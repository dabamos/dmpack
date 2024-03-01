! Author:  Philipp Engel
! Licence: ISC
module dm_logger
    !! This module provides functions and subroutines to send logs to or read
    !! logs from a POSIX message queue. Each process may use only one logger: the
    !! global `LOGGER`. The `LOGGER` may only be changed once, and by one thread
    !! only.
    !!
    !! Be aware that only a single receiver is allowed (but multiple senders).
    !! Otherwise, the messages are passed in round-robin fashion to the receivers.
    use :: dm_ansi
    use :: dm_ascii
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_log
    use :: dm_node
    use :: dm_observ
    use :: dm_sensor
    use :: dm_target
    use :: dm_time
    use :: dm_type
    use :: dm_uuid
    implicit none (type, external)
    private

    ! Logger parameters.
    integer,          parameter, public :: LOGGER_NAME_LEN     = ID_LEN     !! Maximum length of logger name.
    character(len=*), parameter, public :: LOGGER_NAME_DEFAULT = 'dmlogger' !! Default name of logger process.

    ! ANSI colours of log level.
    integer, parameter :: LOGGER_COLORS(0:5) = [ &
        COLOR_RESET, COLOR_GREEN, COLOR_BLUE, COLOR_YELLOW, COLOR_RED, COLOR_RED &
    ]

    type, public :: logger_type
        !! Opaque logger type.
        private
        character(len=LOGGER_NAME_LEN) :: name     = LOGGER_NAME_DEFAULT !! Logger and message queue name.
        character(len=NODE_ID_LEN)     :: node_id  = ' '                 !! Optional node id.
        character(len=LOG_SOURCE_LEN)  :: source   = ' '                 !! Default source of each log message.
        logical                        :: blocking = .true.              !! Blocking message queue access.
        logical                        :: debug    = .false.             !! Forward debug messages via IPC.
        logical                        :: no_color = .false.             !! Disable ANSI colour output.
        logical                        :: ipc      = .false.             !! Send logs to POSIX message queue.
        logical                        :: verbose  = .true.              !! Print to standard error.
    end type logger_type

    type(logger_type), save, public :: LOGGER !! Global logger.

    interface dm_logger_log
        !! Generic interface to logging routines.
        procedure :: dm_logger_log_args
        procedure :: dm_logger_log_type
    end interface dm_logger_log

    interface dm_log_critical
        !! Alias for subroutine.
        module procedure :: dm_logger_log_critical
    end interface

    interface dm_log_debug
        !! Alias for subroutine.
        module procedure :: dm_logger_log_debug
    end interface

    interface dm_log_error
        !! Alias for subroutine.
        module procedure :: dm_logger_log_error
    end interface

    interface dm_log_info
        !! Alias for subroutine.
        module procedure :: dm_logger_log_info
    end interface

    interface dm_log_warning
        !! Alias for subroutine.
        module procedure :: dm_logger_log_warning
    end interface

    public :: dm_log_debug
    public :: dm_log_info
    public :: dm_log_warning
    public :: dm_log_error
    public :: dm_log_critical

    public :: dm_logger_fail
    public :: dm_logger_init
    public :: dm_logger_log
    public :: dm_logger_log_critical
    public :: dm_logger_log_debug
    public :: dm_logger_log_error
    public :: dm_logger_log_info
    public :: dm_logger_log_warning
    public :: dm_logger_out
    public :: dm_logger_send

    private :: dm_logger_log_args
    private :: dm_logger_log_type
contains
    subroutine dm_logger_fail(message, error, source)
        !! Prints critical error message to standard error, with optional
        !! DMPACK error code.
        character(len=*), intent(in)           :: message !! Error message.
        integer,          intent(in), optional :: error   !! Optional error code.
        character(len=*), intent(in), optional :: source  !! Optional source of log.

        type(log_type) :: log

        log = log_type(timestamp = dm_time_now(), &
                       level     = LVL_ERROR, &
                       message   = message, &
                       source    = LOGGER%source)

        if (present(error))  log%error  = error
        if (present(source)) log%source = source

        call dm_logger_out(log)
    end subroutine dm_logger_fail

    subroutine dm_logger_init(name, node_id, source, debug, ipc, blocking, no_color, verbose)
        !! Initialises the global logger.
        character(len=*), intent(in), optional :: name     !! Logger name.
        character(len=*), intent(in), optional :: node_id  !! Node id.
        character(len=*), intent(in), optional :: source   !! Source (name of calling program).
        logical,          intent(in), optional :: debug    !! Forward debug messages via IPC.
        logical,          intent(in), optional :: ipc      !! IPC through POSIX message queues.
        logical,          intent(in), optional :: blocking !! Blocking IPC.
        logical,          intent(in), optional :: no_color !! Disable ANSI colours.
        logical,          intent(in), optional :: verbose  !! Verbose output.

        if (present(name)) then
            if (dm_id_valid(name)) LOGGER%name = name
        end if

        if (present(node_id))  LOGGER%node_id  = node_id
        if (present(source))   LOGGER%source   = source
        if (present(debug))    LOGGER%debug    = debug
        if (present(ipc))      LOGGER%ipc      = ipc
        if (present(blocking)) LOGGER%blocking = blocking
        if (present(no_color)) LOGGER%no_color = no_color
        if (present(verbose))  LOGGER%verbose  = verbose
    end subroutine dm_logger_init

    subroutine dm_logger_log_args(level, message, source, observ, timestamp, error)
        !! Sends a log message to the message queue (fire & forget). Only the
        !! log level is validated.
        integer,           intent(in)              :: level     !! Log level.
        character(len=*),  intent(in)              :: message   !! Log message.
        character(len=*),  intent(in),    optional :: source    !! Optional source of log.
        type(observ_type), intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),  intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,           intent(in),    optional :: error     !! Optional error code.

        type(log_type) :: log

        ! Ignore debug messages if forwarding and output are both disabled.
        if (level == LVL_DEBUG .and. .not. LOGGER%debug .and. .not. LOGGER%verbose) return

        ! Replace invalid log level with `LVL_ERROR`.
        log%level = LVL_ERROR
        if (dm_log_valid(level)) log%level = level

        ! Set log data.
        log%id      = dm_uuid4()
        log%message = dm_ascii_escape(message)

        if (present(source)) then
            log%source = source
        else
            log%source = LOGGER%source
        end if

        if (present(observ)) then
            log%node_id   = observ%node_id
            log%sensor_id = observ%sensor_id
            log%target_id = observ%target_id
            log%observ_id = observ%id
        else
            log%node_id = LOGGER%node_id
        end if

        if (present(timestamp)) then
            log%timestamp = timestamp
        else
            log%timestamp = dm_time_now()
        end if

        if (present(error)) log%error = error

        ! Output and send log.
        if (LOGGER%verbose) call dm_logger_out(log)
        if (LOGGER%ipc)     call dm_logger_send(log)
    end subroutine dm_logger_log_args

    subroutine dm_logger_log_critical(message, source, observ, timestamp, error)
        !! Sends a debug log message to the message queue.
        character(len=*),  intent(in)              :: message   !! Log message.
        character(len=*),  intent(in),    optional :: source    !! Optional source of log.
        type(observ_type), intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),  intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,           intent(in),    optional :: error     !! Optional error code.

        call dm_logger_log(LVL_CRITICAL, message, source, observ, timestamp, error)
    end subroutine dm_logger_log_critical

    subroutine dm_logger_log_debug(message, source, observ, timestamp, error)
        !! Sends a debug log message to the message queue.
        character(len=*),  intent(in)              :: message   !! Log message.
        character(len=*),  intent(in),    optional :: source    !! Optional source of log.
        type(observ_type), intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),  intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,           intent(in),    optional :: error     !! Optional error code.

        call dm_logger_log(LVL_DEBUG, message, source, observ, timestamp, error)
    end subroutine dm_logger_log_debug

    subroutine dm_logger_log_error(message, source, observ, timestamp, error)
        !! Sends a error log message to the message queue.
        character(len=*),  intent(in)              :: message   !! Log message.
        character(len=*),  intent(in),    optional :: source    !! Optional source of log.
        type(observ_type), intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),  intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,           intent(in),    optional :: error     !! Optional error code.

        call dm_logger_log(LVL_ERROR, message, source, observ, timestamp, error)
    end subroutine dm_logger_log_error

    subroutine dm_logger_log_info(message, source, observ, timestamp, error)
        !! Sends a info log message to the message queue.
        character(len=*),  intent(in)              :: message   !! Log message.
        character(len=*),  intent(in),    optional :: source    !! Optional source of log.
        type(observ_type), intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),  intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,           intent(in),    optional :: error     !! Optional error code.

        call dm_logger_log(LVL_INFO, message, source, observ, timestamp, error)
    end subroutine dm_logger_log_info

    subroutine dm_logger_log_warning(message, source, observ, timestamp, error)
        !! Sends a warning log message to the message queue.
        character(len=*),  intent(in)              :: message   !! Log message.
        character(len=*),  intent(in),    optional :: source    !! Optional source of log.
        type(observ_type), intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),  intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,           intent(in),    optional :: error     !! Optional error code.

        call dm_logger_log(LVL_WARNING, message, source, observ, timestamp, error)
    end subroutine dm_logger_log_warning

    subroutine dm_logger_log_type(log)
        !! Sends a log data type to the message queue (send & forget). The
        !! passed log is not validated and must have id, node id, and
        !! timestamp. The receiver may decline the log if the data is
        !! invalid.
        type(log_type), intent(inout) :: log !! Log type.

        if (LOGGER%verbose) call dm_logger_out(log)
        if (LOGGER%ipc)     call dm_logger_send(log)
    end subroutine dm_logger_log_type

    subroutine dm_logger_out(log, unit)
        !! Prints log message to standard error.
        character(len=*), parameter :: FMT_ERROR = '(a, " [", a, "] ", a, " - ", a, " (", i0, ")")'
        character(len=*), parameter :: FMT_NONE  = '(a, " [", a, "] ", a, " - ", a)'

        type(log_type), intent(inout)        :: log  !! Log to output.
        integer,        intent(in), optional :: unit !! File unit.

        integer :: level, unit_

        level = LVL_ERROR
        if (dm_log_valid(log%level)) level = log%level

        unit_ = stderr
        if (present(unit)) unit_ = unit

        if (.not. LOGGER%no_color) call dm_ansi_color(LOGGER_COLORS(level))

        if (log%error /= E_NONE) then
            write (unit_, FMT_ERROR) log%timestamp, &
                                     trim(LOG_LEVEL_NAMES(level)), &
                                     trim(log%source), &
                                     trim(log%message), &
                                     log%error
        else
            write (unit_, FMT_NONE) log%timestamp, &
                                    trim(LOG_LEVEL_NAMES(level)), &
                                    trim(log%source), &
                                    trim(log%message)
        end if

        if (.not. LOGGER%no_color) call dm_ansi_color(COLOR_RESET)
    end subroutine dm_logger_out

    subroutine dm_logger_send(log)
        !! Sends log message to default log message queue (fire & forget)
        !! if `LOGGER%ipc` is true. Prints message to standard output if
        !! `LOGGER%verbose` is true.
        use :: dm_mqueue
        type(log_type), intent(inout) :: log !! Log type.

        integer           :: rc
        type(mqueue_type) :: mqueue

        if (.not. LOGGER%ipc) return
        if (.not. LOGGER%debug .and. log%level <= LVL_DEBUG) return

        ! Open message queue for writing.
        rc = dm_mqueue_open(mqueue   = mqueue, &
                            type     = TYPE_LOG, &
                            name     = LOGGER%name, &
                            access   = MQUEUE_WRONLY, &
                            blocking = logger%blocking)

        if (dm_is_error(rc)) then
            call dm_logger_fail('failed to open mqueue /' // LOGGER%name, rc)
            return
        end if

        ! Write log message to queue.
        rc = dm_mqueue_write(mqueue, log)

        if (dm_is_error(rc)) then
            call dm_logger_fail('failed to write to mqueue /' // LOGGER%name, rc)
        end if

        ! Close message queue.
        rc = dm_mqueue_close(mqueue)

        if (dm_is_error(rc)) then
            call dm_logger_fail('failed to close mqueue /' // LOGGER%name, rc)
        end if
    end subroutine dm_logger_send
end module dm_logger
