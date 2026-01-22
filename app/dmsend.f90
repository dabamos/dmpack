! dmsend.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmsend
    !! Reads logs or observations in CSV or Fortran 95 Namelist format from
    !! file or standard input, then sends all records to a POSIX message queue.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmsend'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Application settings.
        character(ID_LEN)              :: name        = APP_NAME    !! Name of process and POSIX message queue.
        character(FILE_PATH_LEN)       :: config      = ' '         !! Path to configuration file.
        character(LOGGER_NAME_LEN)     :: logger      = ' '         !! Name of logger (name implies IPC).
        character(NODE_ID_LEN)         :: node_id     = ' '         !! Optional node id.
        character(FILE_PATH_LEN)       :: input       = ' '         !! Path to input file (stdin if empty or `-`).
        character(FORMAT_NAME_LEN)     :: format_name = ' '         !! Format name.
        character(TYPE_NAME_LEN)       :: type_name   = ' '         !! Type name.
        character(OBSERV_RECEIVER_LEN) :: receiver    = ' '         !! Name of receiver's message queue (without leading `/`).
        integer                        :: format      = FORMAT_NONE !! Input format.
        integer                        :: type        = TYPE_NONE   !! Data type.
        logical                        :: debug       = .false.     !! Forward debug messages via IPC.
        logical                        :: forward     = .false.     !! Enable observation forwarding.
        logical                        :: verbose     = .false.     !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments and options from configuration file.
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
    call logger%info('started ' // APP_NAME)

    rc = run(app)
    call shutdown(rc)
contains
    integer function run(app) result(rc)
        !! Reads logs or observations from file/standard input, and then sends
        !! all records to one or more POSIX messages queues.
        type(app_type), intent(inout) :: app !! App type.

        integer                 :: file_unit, stat
        integer(i8)             :: nrecords
        logical                 :: is_file
        type(log_type)          :: log
        type(observ_type)       :: observ
        type(posix_mqueue_type) :: mqueue

        nrecords  = 0
        file_unit = stdin
        is_file   = .false.

        if (dm_string_has(app%input) .and. app%input /= '-') is_file = .true.

        ! Open message queue of receiver for writing.
        if (.not. app%forward) then
            call logger%debug('opening mqueue /' // app%receiver)
            rc = dm_posix_mqueue_open(mqueue, type=app%type, name=app%receiver, access=POSIX_MQUEUE_WRONLY, blocking=.true.)

            if (dm_is_error(rc)) then
                call logger%error('failed to open mqueue /' // app%receiver, error=rc)
                return
            end if
        end if

        read_block: block
            if (is_file) then
                ! Open input file.
                call logger%debug('opening input file ' // app%input)
                open (action='read', file=trim(app%input), iostat=stat, newunit=file_unit, status='old')

                if (stat /= 0) then
                    rc = E_IO
                    call logger%error('failed to open input file ' // app%input, error=rc)
                    exit read_block
                end if
            end if

            ! Read serialised log or observation from file/stdin.
            ipc_loop: do
                ! **************************************************************
                ! OBSERVATION TYPE.
                ! **************************************************************
                if (app%type == TYPE_OBSERV) then
                   ! Read observation in CSV or Namelist format.
                    select case (app%format)
                        case (FORMAT_CSV); rc = dm_csv_read(observ, unit=file_unit)
                        case (FORMAT_NML); rc = dm_nml_read(observ, unit=file_unit)
                    end select

                    ! End of file reached.
                    if (rc == E_EOF) then
                        rc = E_NONE
                        call logger%debug('end of file reached')
                        exit ipc_loop
                    end if

                    if (dm_is_error(rc)) then
                        call logger%error('failed to read observation', error=rc)
                        exit ipc_loop
                    end if

                    select case (app%format)
                        case (FORMAT_CSV); call logger%debug('read observation in CSV format')
                        case (FORMAT_NML); call logger%debug('read observation in NML format')
                    end select

                    ! Validate input.
                    if (.not. dm_observ_is_valid(observ)) then
                        call logger%debug('invalid input observation ' // observ%id, error=E_INVALID)
                    end if

                    ! Forward observation to next receiver, or send it to message queue.
                    if (app%forward) then
                        rc = dm_posix_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
                    else
                        rc = dm_posix_mqueue_write(mqueue, observ)
                    end if
                ! **************************************************************
                ! LOG TYPE.
                ! **************************************************************
                else if (app%type == TYPE_LOG) then
                    ! Read log in CSV or Namelist format.
                    select case (app%format)
                        case (FORMAT_CSV); rc = dm_csv_read(log, unit=file_unit)
                        case (FORMAT_NML); rc = dm_nml_read(log, unit=file_unit)
                    end select

                    ! End of file reached.
                    if (rc == E_EOF) then
                        rc = E_NONE
                        call logger%debug('end of file reached')
                        exit ipc_loop
                    end if

                    if (dm_is_error(rc)) then
                        call logger%error('failed to read log', error=rc)
                        exit ipc_loop
                    end if

                    select case (app%format)
                        case (FORMAT_CSV); call logger%debug('read log in CSV format')
                        case (FORMAT_NML); call logger%debug('read log in NML format')
                    end select

                    ! Validate input.
                    if (.not. dm_log_is_valid(log)) then
                        call logger%error('invalid input log ' // log%id, error=E_INVALID)
                        cycle ipc_loop
                    end if

                    ! Send log to message queue.
                    rc = dm_posix_mqueue_write(mqueue, log)
                end if

                ! Handle message queue error.
                if (dm_is_error(rc)) then
                    call logger%error('failed to write to mqueue', error=rc)
                    call dm_sleep(1)
                    cycle ipc_loop
                end if

                nrecords = max(0_i8, nrecords + 1)
            end do ipc_loop

            if (is_file) then
                call logger%debug('closing input file ' // app%input)
                close (file_unit)
            end if
        end block read_block

        ! Close message queue.
        if (.not. app%forward) then
            call dm_posix_mqueue_close(mqueue, error=stat)
            call logger%debug('closed mqueue /' // app%receiver, error=stat)
        end if

        call logger%debug('finished transmission of ' // dm_itoa(nrecords) // ' records')
    end function run

    subroutine shutdown(error)
        !! Stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: stat

        stat = merge(STOP_FAILURE, STOP_SUCCESS, dm_is_error(error))
        call logger%info('stopped ' // APP_NAME, error=error)
        call dm_stop(stat)
    end subroutine shutdown

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app

        type(arg_parser_class) :: parser

        ! Required and optional command-line arguments.
        call parser%add('name',     short='n', type=ARG_TYPE_ID)      ! -n, --name <string>
        call parser%add('config',   short='c', type=ARG_TYPE_FILE)    ! -c, --config <path>
        call parser%add('logger',   short='l', type=ARG_TYPE_ID)      ! -l, --logger <string>
        call parser%add('node',     short='N', type=ARG_TYPE_ID)      ! -N, --node <string>
        call parser%add('input',    short='i', type=ARG_TYPE_FILE)    ! -i, --input <path>
        call parser%add('format',   short='f', type=ARG_TYPE_STRING)  ! -f, --format <string>
        call parser%add('type',     short='t', type=ARG_TYPE_STRING)  ! -t, --type <string>
        call parser%add('receiver', short='r', type=ARG_TYPE_ID, max_len=OBSERV_RECEIVER_LEN) ! -r, --receiver <string>
        call parser%add('debug',    short='D', type=ARG_TYPE_LOGICAL) ! -D, --debug
        call parser%add('forward',  short='F', type=ARG_TYPE_LOGICAL) ! -F, --forward
        call parser%add('verbose',  short='V', type=ARG_TYPE_LOGICAL) ! -V, --verbose

        ! Read all command-line arguments.
        rc = parser%read(version_callback)
        if (dm_is_error(rc)) return

        call parser%get('name',   app%name)
        call parser%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        call parser%get('logger',   app%logger)
        call parser%get('node',     app%node_id)
        call parser%get('input',    app%input)
        call parser%get('format',   app%format_name)
        call parser%get('type',     app%type_name)
        call parser%get('receiver', app%receiver)
        call parser%get('debug',    app%debug)
        call parser%get('forward',  app%forward)
        call parser%get('verbose',  app%verbose)

        app%format = dm_format_from_name(app%format_name)
        app%type   = dm_type_from_name(app%type_name)

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file if path is not emty.
        type(app_type), intent(inout) :: app !! App type.
        type(config_class)             :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('logger',   app%logger)
            call config%get('node',     app%node_id)
            call config%get('input',    app%input)
            call config%get('format',   app%format_name)
            call config%get('receiver', app%receiver)
            call config%get('type',     app%type_name)
            call config%get('debug',    app%debug)
            call config%get('forward',  app%forward)
            call config%get('verbose',  app%verbose)
        end if

        call config%close()
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (dm_string_has(app%node_id) .and. .not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger name')
            return
        end if

        if (app%type /= TYPE_OBSERV .and. app%type /= TYPE_LOG) then
            call dm_error_out(rc, 'invalid type')
            return
        end if

        if (app%format /= FORMAT_CSV .and. app%format /= FORMAT_NML) then
            call dm_error_out(rc, 'invalid format')
            return
        end if

        if (app%forward .and. app%type /= TYPE_OBSERV) then
            call dm_error_out(rc, '--forward requires type observation')
            return
        else if (.not. app%forward .and. .not. dm_id_is_valid(app%receiver)) then
            call dm_error_out(rc, 'invalid receiver')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmsend
