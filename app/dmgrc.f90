! dmgrc.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmgrc
    !! Leica GeoCOM return code inspector. This program checks the GeoCOM return
    !! code (GRC) of observations from robotic total stations and creates a log
    !! message if an error is detected.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmgrc'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 0

    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: grc_level_type
        integer, allocatable :: codes(:) !! Array of GeoCOM return codes.
    end type grc_level_type

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)            :: name      = APP_NAME    !! Instance and configuration name (required).
        character(len=FILE_PATH_LEN)     :: config    = ' '         !! Path to configuration file (required).
        character(len=LOGGER_NAME_LEN)   :: logger    = ' '         !! Name of logger.
        character(len=NODE_ID_LEN)       :: node      = ' '         !! Node id (required).
        character(len=RESPONSE_NAME_LEN) :: response  = 'grc'       !! Response name of GeoCOM return code.
        integer                          :: level     = LVL_WARNING !! Default log level for return codes other than `GRC_OK`.
        logical                          :: debug     = .false.     !! Forward debug messages via IPC.
        logical                          :: verbose   = .false.     !! Print debug messages to stderr.
        type(grc_level_type)             :: levels(LVL_LAST)        !! Custom log levels of GeoCOM return codes.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer           :: rc     ! Return code.
    type(app_type)    :: app    ! App configuration.
    type(mqueue_type) :: mqueue ! Message queue.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read settings from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Initialise logger.
    logger => dm_logger_get()
    call logger%configure(name    = app%logger, &
                          node_id = app%node, &
                          source  = app%name, &
                          debug   = app%debug, &
                          ipc     = (len_trim(app%logger) > 0), &
                          verbose = app%verbose)

    init_block: block
        ! Open observation message queue for reading.
        rc = dm_mqueue_open(mqueue = mqueue, &
                            type   = TYPE_OBSERV, &
                            name   = app%name, &
                            access = MQUEUE_RDONLY)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to open mqueue /' // trim(app%name) // ': ' // dm_system_error_message())
            exit init_block
        end if

        ! Register signal handlers and run the IPC loop.
        call dm_signal_register(signal_handler)
        call run(app, mqueue)
    end block init_block

    ! Clean up and exit.
    call halt(rc)
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app
        type(arg_type)              :: args(8)

        rc = E_NONE

        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <id>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),      & ! -l, --logger <id>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('response', short='R', type=ARG_TYPE_ID, max_len=RESPONSE_NAME_LEN), & ! -R, --response <id>
            arg_type('level',    short='L', type=ARG_TYPE_LEVEL),   & ! -L, --level <n>
            arg_type('debug',    short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('verbose',  short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        rc = dm_arg_get(args(3), app%logger)
        rc = dm_arg_get(args(4), app%node)
        rc = dm_arg_get(args(5), app%response)
        rc = dm_arg_get(args(6), app%level)
        rc = dm_arg_get(args(7), app%debug)
        rc = dm_arg_get(args(8), app%verbose)

        ! Validate options.
        rc = E_INVALID

        if (.not. dm_id_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (len_trim(app%logger) > 0 .and. .not. dm_id_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger')
            return
        end if

        if (.not. dm_id_valid(app%node)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (.not. dm_id_valid(app%response, max_len=RESPONSE_NAME_LEN)) then
            call dm_error_out(rc, 'invalid response name')
            return
        end if

        if (.not. dm_log_valid(app%level)) then
            call dm_error_out(rc, 'invalid log level')
            return
        end if

        ! Allocate return code arrays.
        if (.not. allocated(app%levels(LVL_DEBUG   )%codes)) allocate (app%levels(LVL_DEBUG   )%codes(0))
        if (.not. allocated(app%levels(LVL_INFO    )%codes)) allocate (app%levels(LVL_INFO    )%codes(0))
        if (.not. allocated(app%levels(LVL_WARNING )%codes)) allocate (app%levels(LVL_WARNING )%codes(0))
        if (.not. allocated(app%levels(LVL_ERROR   )%codes)) allocate (app%levels(LVL_ERROR   )%codes(0))
        if (.not. allocated(app%levels(LVL_CRITICAL)%codes)) allocate (app%levels(LVL_CRITICAL)%codes(0))

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.
        type(config_type)             :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name, geocom=.true.)

        if (dm_is_ok(rc)) then
            rc = dm_config_get(config, 'logger',   app%logger)
            rc = dm_config_get(config, 'node',     app%node)
            rc = dm_config_get(config, 'response', app%response)
            rc = dm_config_get(config, 'level',    app%level)
            rc = dm_config_get(config, 'debug',    app%debug)
            rc = dm_config_get(config, 'verbose',  app%verbose)

            rc = dm_config_get(config, 'levels')

            if (dm_is_ok(rc)) then
                rc = dm_config_get(config, 'debug',    app%levels(LVL_DEBUG   )%codes)
                rc = dm_config_get(config, 'info',     app%levels(LVL_INFO    )%codes)
                rc = dm_config_get(config, 'warning',  app%levels(LVL_WARNING )%codes)
                rc = dm_config_get(config, 'error',    app%levels(LVL_ERROR   )%codes)
                rc = dm_config_get(config, 'critical', app%levels(LVL_CRITICAL)%codes)

                call dm_config_remove(config)
            end if

            rc = E_NONE
        end if

        call dm_config_close(config)
    end function read_config

    subroutine find_level(grc_levels, grc, level, default)
        !! Returns log level associated with GeoCOM return code in `level`.
        type(grc_level_type), intent(inout) :: grc_levels(:) !! Log levels of GeoCOM return codes.
        integer,              intent(in)    :: grc           !! GeoCOM return code to search for.
        integer,              intent(out)   :: level         !! Associated log level or default.
        integer,              intent(in)    :: default       !! Default log level.

        integer :: i

        level = default

        do i = 1, size(grc_levels)
            if (dm_array_has(grc_levels(i)%codes, grc)) then
                level = i
                return
            end if
        end do
    end subroutine find_level

    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: rc, stat

        stat = 0
        if (dm_is_error(error)) stat = 1

        rc = dm_mqueue_close(mqueue)
        rc = dm_mqueue_unlink(mqueue)

        call dm_stop(stat)
    end subroutine halt

    subroutine run(app, mqueue)
        type(app_type),    intent(inout) :: app    !! App type.
        type(mqueue_type), intent(inout) :: mqueue !! Message queue type.

        integer           :: rc
        type(observ_type) :: observ

        call logger%info('started ' // app%name)

        ipc_loop: do
            ! Blocking read from POSIX message queue.
            rc = dm_mqueue_read(mqueue, observ)

            if (dm_is_error(rc)) then
                call logger%error('failed to read from mqueue /' // app%name, error=rc)
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
                        call logger%warning('GeoCOM return code response ' // trim(app%response) // &
                                            ' of invalid type in request ' // trim(observ%requests(i)%name) // &
                                            ' (' // dm_itoa(i) // ') of observ ' // observ%name, observ=observ, error=E_TYPE)
                        cycle
                    end if

                    ! Validate response error.
                    if (dm_is_error(grc_error)) then
                        call logger%warning('invalid GeoCOM return code response ' // trim(app%response) // &
                                            ' in request ' // trim(observ%requests(i)%name) // ' (' // dm_itoa(i) // &
                                            ') of observ ' // observ%name, observ=observ, error=grc_error)
                        cycle
                    end if

                    ! Validate response value.
                    if (dm_geocom_is_ok(grc)) then
                        call logger%debug('GeoCOM return code response ' // trim(app%response) // &
                                          ' in request ' // trim(observ%requests(i)%name) // ' (' // dm_itoa(i) // &
                                          ') of observ ' // trim(observ%name) // ' not an error', observ=observ, error=E_NONE)
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

    subroutine signal_handler(signum) bind(c)
        !! Default POSIX signal handler of the program.
        use, intrinsic :: iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: signum

        select case (signum)
            case default
                call logger%info('exit on signal ' // dm_itoa(signum))
                call halt(E_NONE)
        end select
    end subroutine signal_handler
end program dmgrc
