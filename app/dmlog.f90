! dmlog.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmlog
    !! Logging utility that sends log messages to a dmlogger instance if
    !! a valid logger is set through argument `--logger` or environment
    !! variable `DM_LOGGER`. The log message is printed to standard error if
    !! argument `--verbose` is passed. The default log level is `E_INFO`.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmlog'
    integer,      parameter :: APP_MAJOR = 0
    integer,      parameter :: APP_MINOR = 9
    integer,      parameter :: APP_PATCH = 9

    type :: app_type
        !! Command-line arguments.
        character(LOGGER_NAME_LEN) :: logger  = ' '     !! Name of logger instance (optional).
        logical                    :: debug   = .false. !! Forward debug messages via message queue (optional).
        logical                    :: verbose = .false. !! Print debug messages to stderr (optional).
    end type app_type

    class(logger_class), pointer :: logger

    integer        :: rc
    type(app_type) :: app
    type(log_type) :: log

    call dm_init()

    rc = read_args(app, log)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger, & ! Name of logger process.
                          source  = APP_NAME,   & ! Default log source.
                          debug   = app%debug,  & ! Forward DEBUG messages via IPC.
                          ipc     = .true.,     & ! Enable IPC (if logger is set).
                          verbose = app%verbose)  ! Print logs to standard error.

    call dm_log_set(log, id=dm_uuid4(), timestamp=dm_time_now())
    call logger%log(log)
contains
    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS.
    ! **************************************************************************
    integer function read_args(app, log) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app !! App type.
        type(log_type), intent(out) :: log !! Log type.

        type(arg_class) :: arg

        ! Required and optional command-line arguments.
        call arg%add('logger',  short='l', type=ARG_TYPE_ID)      ! -l, --logger <id>
        call arg%add('level',   short='L', type=ARG_TYPE_LEVEL)   ! -L, --level <n>
        call arg%add('error',   short='e', type=ARG_TYPE_INTEGER) ! -e, --error <n>
        call arg%add('node',    short='N', type=ARG_TYPE_ID)      ! -N, --node <id>
        call arg%add('sensor',  short='S', type=ARG_TYPE_ID)      ! -S, --sensor <id>
        call arg%add('target',  short='T', type=ARG_TYPE_ID)      ! -T, --target <id>
        call arg%add('observ',  short='O', type=ARG_TYPE_UUID)    ! -O, --observ <uuid>
        call arg%add('source',  short='Z', type=ARG_TYPE_ID,     max_len=LOG_SOURCE_LEN)                   ! -Z, --source <id>
        call arg%add('message', short='m', type=ARG_TYPE_STRING, max_len=LOG_MESSAGE_LEN, required=.true.) ! -m, --message <string>
        call arg%add('debug',   short='D', type=ARG_TYPE_LOGICAL) ! -D, --debug
        call arg%add('verbose', short='V', type=ARG_TYPE_LOGICAL) ! -V, --verbose

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('logger',  app%logger)
        call arg%get('level',   log%level, LL_INFO)
        call arg%get('error',   log%error)
        call arg%get('node',    log%node_id)
        call arg%get('sensor',  log%sensor_id)
        call arg%get('target',  log%target_id)
        call arg%get('observ',  log%observ_id)
        call arg%get('source',  log%source, APP_NAME)
        call arg%get('message', log%message)
        call arg%get('debug',   app%debug)
        call arg%get('verbose', app%verbose)

        rc = validate(log)
    end function read_args

    integer function validate(log) result(rc)
        !! Validates options and prints error messages.
        type(log_type), intent(inout) :: log !! Log type.

        rc = E_INVALID

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'argument --logger is invalid')
            return
        end if

        if (.not. dm_error_is_valid(log%error)) then
            call dm_error_out(rc, 'argument --error is invalid')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
    end subroutine version_callback
end program dmlog
