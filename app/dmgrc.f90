! dmgrc.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmgrc
    !! Leica GeoCOM return code inspector. This program checks the GeoCOM return
    !! code (GRC) of observations from robotic total stations and creates a log
    !! message if an error is detected. The GRC response must be of type
    !! `RESPONSE_TYPE_INT32`.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmgrc'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 8

    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: app_level_type
        integer, allocatable :: codes(:) !! Array of GeoCOM return codes.
    end type app_level_type

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)            :: name      = APP_NAME   !! Instance and configuration name (required).
        character(len=FILE_PATH_LEN)     :: config    = ' '        !! Path to configuration file (required).
        character(len=LOGGER_NAME_LEN)   :: logger    = ' '        !! Name of logger.
        character(len=NODE_ID_LEN)       :: node_id   = ' '        !! Node id (required).
        character(len=RESPONSE_NAME_LEN) :: response  = 'grc'      !! Response name of GeoCOM return code.
        integer                          :: level     = LL_WARNING !! Default log level for return codes other than `GRC_OK`.
        logical                          :: debug     = .false.    !! Forward debug messages via IPC.
        logical                          :: verbose   = .false.    !! Print debug messages to stderr.
        type(app_level_type)             :: levels(LL_LAST)        !! Custom log levels of GeoCOM return codes.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer           :: rc     ! Return code.
    type(app_type)    :: app    ! App settings.
    type(mqueue_type) :: mqueue ! POSIX message queue.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read settings from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,  & ! Name of logger process.
                          node_id = app%node_id, & ! Node id.
                          source  = app%name,    & ! Log source.
                          debug   = app%debug,   & ! Forward debug messages via IPC.
                          ipc     = .true.,      & ! Enable IPC (if logger is set).
                          verbose = app%verbose)   ! Print logs to standard error.

    init_block: block
        ! Open observation message queue for reading.
        rc = dm_mqueue_open(mqueue, type=TYPE_OBSERV, name=app%name, access=MQUEUE_RDONLY)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to open mqueue /' // trim(app%name) // ': ' // dm_system_error_message())
            exit init_block
        end if

        ! Register signal handlers and run the IPC loop.
        call dm_signal_register(signal_callback)
        call run(app, mqueue)
    end block init_block

    ! Clean up and exit.
    call halt(rc)
contains
    subroutine find_level(levels, grc, level, default)
        !! Returns log level associated with GeoCOM return code in `level`.
        type(app_level_type), intent(inout) :: levels(:) !! Log levels of GeoCOM return codes.
        integer,              intent(in)    :: grc       !! GeoCOM return code to search for.
        integer,              intent(out)   :: level     !! Associated log level or default.
        integer,              intent(in)    :: default   !! Default log level.

        integer :: i
        logical :: has

        level = default

        do i = 1, size(levels)
            has = dm_array_has(levels(i)%codes, grc)
            if (.not. has) cycle
            level = i
            return
        end do
    end subroutine find_level

    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: rc, stat

        stat = dm_btoi(dm_is_error(error), STOP_FAILURE, STOP_SUCCESS)

        call dm_mqueue_close(mqueue, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to close mqueue /' // app%name, error=rc)

        call dm_mqueue_unlink(mqueue, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to unlink mqueue /' // app%name, error=rc)

        call dm_stop(stat)
    end subroutine halt

    subroutine run(app, mqueue)
        type(app_type),    intent(inout) :: app    !! App type.
        type(mqueue_type), intent(inout) :: mqueue !! Message queue type.

        integer           :: rc
        type(observ_type) :: observ

        call logger%info('started ' // APP_NAME)

        ipc_loop: do
            ! Blocking read from POSIX message queue.
            call logger%debug('waiting for observ on mqueue /' // app%name)
            rc = dm_mqueue_read(mqueue, observ)

            if (dm_is_error(rc)) then
                call logger%error('failed to read observ from mqueue /' // app%name, error=rc)
                call dm_sleep(1)
                cycle ipc_loop
            end if

            grc_block: block
                integer :: grc, grc_error, grc_type
                integer :: i, level
                logical :: found

                if (observ%nrequests == 0) then
                    call logger%debug('no requests in observ ' // observ%name, observ=observ, error=rc)
                    exit grc_block
                end if

                ! Search all requests for GeoCOM return code.
                found = .false.

                do i = 1, observ%nrequests
                    ! Find GeoCOM return code response in request.
                    call dm_request_get(observ%requests(i), app%response, grc, type=grc_type, error=grc_error, status=rc)
                    if (dm_is_error(rc)) cycle

                    ! Validate response type.
                    if (grc_type /= RESPONSE_TYPE_INT32) then
                        call logger%warning('GeoCOM return code response ' // trim(app%response) // ' of invalid type in request ' // &
                                            trim(observ%requests(i)%name) // ' (' // dm_itoa(i) // ') of observ ' // observ%name, &
                                            observ=observ, error=E_TYPE)
                        cycle
                    end if

                    ! Validate response error.
                    if (dm_is_error(grc_error)) then
                        call logger%warning('invalid GeoCOM return code response ' // trim(app%response) // ' in request ' // &
                                            trim(observ%requests(i)%name) // ' (' // dm_itoa(i) // ') of observ ' // &
                                            observ%name, observ=observ, error=grc_error)
                        cycle
                    end if

                    ! Validate response value.
                    if (dm_geocom_is_ok(grc)) then
                        call logger%debug('GeoCOM return code response ' // trim(app%response) // ' in request ' // &
                                          trim(observ%requests(i)%name) // ' (' // dm_itoa(i) // ') of observ ' // &
                                          trim(observ%name) // ' not an error', observ=observ, error=E_NONE)
                        cycle
                    end if

                    ! Find associated log level of GeoCOM return code, and create log message.
                    call find_level(app%levels, grc, level, default=app%level)
                    call logger%log(level, dm_geocom_error_message(grc), observ=observ, error=E_GEOCOM)

                    found = .true.
                end do

                if (.not. found) then
                    call logger%debug('no GeoCOM return code response ' // trim(app%response) // ' found in observ ' // &
                                      observ%name, observ=observ, error=E_NOT_FOUND)
                end if
            end block grc_block

            ! Forward observation.
            call logger%debug('finished observ ' // observ%name, observ=observ)
            rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
        end do ipc_loop
    end subroutine run

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app !! App type.

        integer        :: i
        type(arg_type) :: args(8)

        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <id>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),      & ! -l, --logger <id>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('response', short='R', type=ARG_TYPE_ID, max_len=RESPONSE_NAME_LEN), & ! -R, --response <id>
            arg_type('level',    short='L', type=ARG_TYPE_LEVEL),   & ! -L, --level <level>
            arg_type('debug',    short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('verbose',  short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%name)
        call dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call dm_arg_get(args(3), app%logger)
        call dm_arg_get(args(4), app%node_id)
        call dm_arg_get(args(5), app%response)
        call dm_arg_get(args(6), app%level)
        call dm_arg_get(args(7), app%debug)
        call dm_arg_get(args(8), app%verbose)

        ! Allocate return code arrays.
        do i = 1, size(app%levels)
            if (.not. allocated(app%levels(i)%codes)) allocate (app%levels(i)%codes(0))
        end do

        ! Validate options.
        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_type) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = dm_config_open(config, app%config, app%name, geocom=.true.)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'logger',   app%logger)
            call dm_config_get(config, 'node',     app%node_id)
            call dm_config_get(config, 'response', app%response)
            call dm_config_get(config, 'level',    app%level)
            call dm_config_get(config, 'debug',    app%debug)
            call dm_config_get(config, 'verbose',  app%verbose)

            if (dm_is_ok(dm_config_field(config, 'codes'))) then
                call dm_config_get(config, 'debug',    app%levels(LL_DEBUG   )%codes)
                call dm_config_get(config, 'info',     app%levels(LL_INFO    )%codes)
                call dm_config_get(config, 'warning',  app%levels(LL_WARNING )%codes)
                call dm_config_get(config, 'error',    app%levels(LL_ERROR   )%codes)
                call dm_config_get(config, 'critical', app%levels(LL_CRITICAL)%codes)
                call dm_config_get(config, 'user',     app%levels(LL_USER    )%codes)

                call dm_config_remove(config)
            end if
        end if

        call dm_config_close(config)
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger')
            return
        end if

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (.not. dm_id_is_valid(app%response, max_len=RESPONSE_NAME_LEN)) then
            call dm_error_out(rc, 'invalid response name')
            return
        end if

        if (.not. dm_log_level_is_valid(app%level)) then
            call dm_error_out(rc, 'invalid log level')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine signal_callback(signum) bind(c)
        !! Default POSIX signal handler of the program.
        integer(kind=c_int), intent(in), value :: signum

        call logger%info('exit on signal ' // dm_signal_name(signum))
        call halt(E_NONE)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmgrc
