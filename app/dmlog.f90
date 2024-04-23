! dmlog.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmlog
    !! Logging utility that sends log messages to a dmlogger instance.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmlog'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 1

    type :: app_type
        !! Command-line arguments.
        character(len=LOGGER_NAME_LEN) :: logger  = ' '     !! Name of logger instance (optional).
        logical                        :: verbose = .false. !! Print debug messages to stderr (optional).
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App configuration.
    type(log_type) :: log ! Log to send.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments.
    rc = read_args(app, log)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get()
    call logger%configure(name=app%logger, ipc=.true., verbose=app%verbose)

    ! Prepare and send log.
    log%id = dm_uuid4()
    log%timestamp = dm_time_now()

    call logger%log(log)
contains
    integer function read_args(app, log) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app
        type(log_type), intent(out) :: log
        type(arg_type)              :: args(10)

        rc = E_NONE

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('logger',  short='l', type=ARG_TYPE_ID),      & ! -l, --logger <id>
            arg_type('verbose', short='V', type=ARG_TYPE_LOGICAL), & ! -V, --verbose
            arg_type('level',   short='L', type=ARG_TYPE_LEVEL),   & ! -L, --level <n>
            arg_type('error',   short='e', type=ARG_TYPE_INTEGER), & ! -e, --error <n>
            arg_type('node',    short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('sensor',  short='S', type=ARG_TYPE_ID),      & ! -S, --sensor <id>
            arg_type('target',  short='T', type=ARG_TYPE_ID),      & ! -T, --target <id>
            arg_type('observ',  short='O', type=ARG_TYPE_UUID),    & ! -O, --observ <uuid>
            arg_type('source',  short='Z', type=ARG_TYPE_ID,     max_len=LOG_SOURCE_LEN), & ! -Z, --source <id>
            arg_type('message', short='m', type=ARG_TYPE_STRING, max_len=LOG_MESSAGE_LEN, required=.true.) &  ! -m, --message <string>
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args( 1), app%logger)
        rc = dm_arg_get(args( 2), app%verbose)
        rc = dm_arg_get(args( 3), log%level, LL_INFO)
        rc = dm_arg_get(args( 4), log%error)
        rc = dm_arg_get(args( 5), log%node_id)
        rc = dm_arg_get(args( 6), log%sensor_id)
        rc = dm_arg_get(args( 7), log%target_id)
        rc = dm_arg_get(args( 8), log%observ_id)
        rc = dm_arg_get(args( 9), log%source, APP_NAME)
        rc = dm_arg_get(args(10), log%message)

        rc = E_INVALID

        if (.not. dm_error_valid(log%error)) then
            call dm_error_out(rc, 'invalid error code')
            return
        end if

        rc = E_NONE
    end function read_args
end program dmlog
