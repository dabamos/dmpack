! dmlog.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmlog
    !! Logging utility that sends log messages to a dmlogger instance if
    !! a valid logger is set through argument `--logger` or environment
    !! variable `DMLOGGER`. The log message is printed to standard error if
    !! argument `--verbose` is passed. The default log level is `E_INFO`.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmlog'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 8

    type :: app_type
        !! Command-line arguments.
        character(len=LOGGER_NAME_LEN) :: logger  = ' '      !! Name of logger instance (optional).
        logical                        :: debug   = .false.  !! Forward debug messages via message queue (optional).
        logical                        :: verbose = .false.  !! Print debug messages to stderr (optional).
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

        type(arg_type) :: args(11)

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('logger',  short='l', type=ARG_TYPE_ID),      & ! -l, --logger <id>
            arg_type('level',   short='L', type=ARG_TYPE_LEVEL),   & ! -L, --level <n>
            arg_type('error',   short='e', type=ARG_TYPE_INTEGER), & ! -e, --error <n>
            arg_type('node',    short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('sensor',  short='S', type=ARG_TYPE_ID),      & ! -S, --sensor <id>
            arg_type('target',  short='T', type=ARG_TYPE_ID),      & ! -T, --target <id>
            arg_type('observ',  short='O', type=ARG_TYPE_UUID),    & ! -O, --observ <uuid>
            arg_type('source',  short='Z', type=ARG_TYPE_ID,     max_len=LOG_SOURCE_LEN),                   & ! -Z, --source <id>
            arg_type('message', short='m', type=ARG_TYPE_STRING, max_len=LOG_MESSAGE_LEN, required=.true.), & ! -m, --message <string>
            arg_type('debug',   short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('verbose', short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args( 1), app%logger)
        call dm_arg_get(args( 2), log%level, LL_INFO)
        call dm_arg_get(args( 3), log%error)
        call dm_arg_get(args( 4), log%node_id)
        call dm_arg_get(args( 5), log%sensor_id)
        call dm_arg_get(args( 6), log%target_id)
        call dm_arg_get(args( 7), log%observ_id)
        call dm_arg_get(args( 8), log%source, APP_NAME)
        call dm_arg_get(args( 9), log%message)
        call dm_arg_get(args(10), app%debug)
        call dm_arg_get(args(11), app%verbose)

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
