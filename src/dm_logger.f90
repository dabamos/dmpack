! Author:  Philipp Engel
! Licence: ISC
module dm_logger
    !! Object-oriented logger that outputs logs and optionally forwards them
    !! via POSIX message queue.
    !!
    !! Only a single receiver is allowed (but multiple senders). Otherwise,
    !! the log messages are passed in round-robin fashion to the receivers.
    !!
    !! Get a pointer to the logger before configuration and invocation:
    !!
    !! ```fortran
    !! class(logger_class), pointer :: logger
    !!
    !! logger => dm_logger_get_default()
    !! call logger%configure(name='dmlogger', ipc=.true., verbose=.true.)
    !! call logger%error('log message')
    !! ```
    !!
    !! The log message is sent do the _dmlogger(1)_ instance of name
    !! `dmlogger`, i.e., to POSIX message queue `/dmlogger`.
    use :: dm_ansi
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_log
    use :: dm_node
    use :: dm_observ
    use :: dm_type
    implicit none (type, external)
    private

    ! Logger parameters.
    integer,          parameter, public :: LOGGER_NAME_LEN = ID_LEN     !! Maximum length of logger name.
    character(len=*), parameter         :: LOGGER_NAME     = 'dmlogger' !! Default name of logger process.

    ! ANSI colours of log level.
    integer, parameter :: LOGGER_COLORS(LL_NONE:LL_LAST) = [ &
        COLOR_RESET, COLOR_GREEN, COLOR_BLUE, COLOR_YELLOW, COLOR_RED, COLOR_RED, COLOR_CYAN &
    ] !! Colours associated with log level.

    type, public :: logger_class
        !! Opaque logger class.
        private
        character(len=LOGGER_NAME_LEN) :: name      = LOGGER_NAME !! Logger and message queue name.
        character(len=NODE_ID_LEN)     :: node_id   = ' '         !! Optional node id.
        character(len=LOG_SOURCE_LEN)  :: source    = ' '         !! Default source of each log message.
        integer                        :: min_level = LL_INFO     !! Minimum level of logs to be forwarded via message queue.
        logical                        :: blocking  = .true.      !! Blocking message queue access.
        logical                        :: ipc       = .false.     !! Send logs to POSIX message queue.
        logical                        :: no_color  = .false.     !! Disable ANSI colour output.
        logical                        :: verbose   = .true.      !! Print to standard error.
    contains
        private
        ! Private methods.
        procedure :: fail     => logger_fail
        procedure :: log_args => logger_log_args
        procedure :: log_type => logger_log_type
        procedure :: send     => logger_send
        ! Public methods.
        procedure, public :: configure => logger_configure
        procedure, public :: get_name  => logger_get_name
        procedure, public :: is_ipc    => logger_is_ipc
        procedure, public :: out       => logger_out
        ! Public logging methods.
        generic,   public :: log       => log_args, log_type
        procedure, public :: critical  => logger_log_critical
        procedure, public :: debug     => logger_log_debug
        procedure, public :: error     => logger_log_error
        procedure, public :: info      => logger_log_info
        procedure, public :: user      => logger_log_user
        procedure, public :: warning   => logger_log_warning
    end type logger_class

    ! Global logger instance.
    class(logger_class), allocatable, target, save :: MODULE_LOGGER

    ! Public procedures.
    public :: dm_logger_get_default

    ! Private procedures.
    private :: logger_configure
    private :: logger_fail
    private :: logger_get_name
    private :: logger_is_ipc
    private :: logger_log_args
    private :: logger_log_critical
    private :: logger_log_debug
    private :: logger_log_error
    private :: logger_log_info
    private :: logger_log_type
    private :: logger_log_user
    private :: logger_log_warning
    private :: logger_out
    private :: logger_send
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    function dm_logger_get_default() result(logger)
        !! Returns pointer to global logger. The function allocates the logger
        !! if it does not exist yet.
        class(logger_class), pointer :: logger !! Pointer to logger object.

        integer :: stat

        logger => null()

        if (.not. allocated(MODULE_LOGGER)) then
            allocate (MODULE_LOGGER, stat=stat)
            if (stat /= 0) return
        end if

        logger => MODULE_LOGGER
    end function dm_logger_get_default

    ! ******************************************************************
    ! PRIVATE CLASS METHODS.
    ! ******************************************************************
    subroutine logger_configure(this, name, node_id, source, debug, ipc, blocking, no_color, verbose)
        !! Configures the (global) logger. The argument `name` is set only if
        !! it is a valid id of character set `[-0-9A-Z_a-z]`. The argument
        !! `source` shall be the name of the log source, usually the name of
        !! the calling program.
        !!
        !! If `debug` is passed and `.true.`, log messages of level `LL_DEBUG`
        !! are forwarded via message queue. Otherwise, the minimum level for
        !! logs to be transmitted is `LL_INFO`. If `ipc` is passed and
        !! `.true.`, logs are sent to the POSIX message queue of logger `name`.
        !! The name of the message queue is therefore `/<name>`. Writing to
        !! the message queue is blocking, unless `blocking` is passed and
        !! `.false.`.
        !!
        !! The dummy argument `no_color` disables coloured output through ANSI
        !! escape sequences if `.true.`. Log messages are printed to standard
        !! error, unless `verbose` is passed and `.false.`.
        class(logger_class), intent(inout)        :: this     !! Logger object.
        character(len=*),    intent(in), optional :: name     !! Logger name.
        character(len=*),    intent(in), optional :: node_id  !! Node id.
        character(len=*),    intent(in), optional :: source   !! Source (name of calling program).
        logical,             intent(in), optional :: debug    !! Forward debugging messages via IPC.
        logical,             intent(in), optional :: ipc      !! IPC through POSIX message queues.
        logical,             intent(in), optional :: blocking !! Blocking IPC.
        logical,             intent(in), optional :: no_color !! Disable ANSI colours.
        logical,             intent(in), optional :: verbose  !! Verbose output.

        if (present(name)) then
            if (dm_id_is_valid(name)) this%name = name
        end if

        if (present(node_id))  this%node_id   = node_id
        if (present(source))   this%source    = source
        if (present(debug))    this%min_level = LL_DEBUG
        if (present(ipc))      this%ipc       = ipc
        if (present(blocking)) this%blocking  = blocking
        if (present(no_color)) this%no_color  = no_color
        if (present(verbose))  this%verbose   = verbose
    end subroutine logger_configure

    subroutine logger_fail(this, message, error, source)
        !! Prints critical error message to standard error, with optional
        !! DMPACK error code. The length of argument `message` is limited to
        !! `LOG_MESSAGE_LEN`. The error messages is not forwarded and does not
        !! require IPC.
        use :: dm_time, only: dm_time_now

        class(logger_class), intent(inout)        :: this    !! Logger object.
        character(len=*),    intent(in)           :: message !! Error message.
        integer,             intent(in), optional :: error   !! Optional error code.
        character(len=*),    intent(in), optional :: source  !! Optional source of log.

        type(log_type) :: log

        log = log_type(timestamp = dm_time_now(), &
                       level     = LL_ERROR, &
                       message   = message, &
                       source    = this%source)

        if (present(error))  log%error  = error
        if (present(source)) log%source = source

        call this%out(log)
    end subroutine logger_fail

    function logger_get_name(this) result(name)
        !! Returns name of logger as allocatable string.
        class(logger_class), intent(inout) :: this !! Logger object.
        character(len=:), allocatable      :: name !! Logger name.

        name = trim(this%name)
    end function logger_get_name

    logical function logger_is_ipc(this) result(is)
        !! Returns `.true.` if IPC is enabled.
        class(logger_class), intent(inout) :: this !! Logger object.

        is = this%ipc
    end function logger_is_ipc

    subroutine logger_log_args(this, level, message, source, observ, timestamp, error, escape, verbose)
        !! Sends a log message to the message queue (fire & forget). Only the
        !! log level is validated. An invalid level is set to `LL_ERROR`.
        !!
        !! If `error` is passed and `E_NONE`, no log is created, unless
        !! `verbose` is set to `.true.`. The passed log message is not
        !! validated, but non-printable characters are escaped, unless `escape`
        !! is passed and `.false.`.
        !!
        !! The length of argument `message` is limited to `LOG_MESSAGE_LEN`.
        use :: dm_ascii, only: dm_ascii_escape
        use :: dm_time,  only: dm_time_now
        use :: dm_uuid,  only: dm_uuid4

        class(logger_class), intent(inout)           :: this      !! Logger object.
        integer,             intent(in)              :: level     !! Log level.
        character(len=*),    intent(in)              :: message   !! Log message.
        character(len=*),    intent(in),    optional :: source    !! Optional source of log.
        type(observ_type),   intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),    intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,             intent(in),    optional :: error     !! Optional error code.
        logical,             intent(in),    optional :: escape    !! Escape non-printable characters in message (`.true.` by default).
        logical,             intent(in),    optional :: verbose   !! Create log if `error` is passed and `E_NONE` (`.false.` by default).

        logical        :: escape_, verbose_
        type(log_type) :: log

        escape_  = .true.
        verbose_ = .false.

        if (present(escape))  escape_  = escape
        if (present(verbose)) verbose_ = verbose

        ! Ignore debugging messages if forwarding and output are both disabled.
        if (level == LL_DEBUG .and. this%min_level > LL_DEBUG .and. .not. this%verbose) return

        ! Ignore error code `E_NONE` if not verbose.
        if (present(error)) then
            if (dm_is_ok(error) .and. .not. verbose_) return
        end if

        ! Replace invalid log level with `LL_ERROR`.
        log%level = LL_ERROR
        if (dm_log_is_valid(level)) log%level = level

        ! Create log id.
        log%id = dm_uuid4()

        ! Set log message.
        if (escape_) then
            log%message = dm_ascii_escape(adjustl(message))
        else
            log%message = adjustl(message)
        end if

        if (present(source)) then
            log%source = source
        else
            log%source = this%source
        end if

        if (present(observ)) then
            log%node_id   = observ%node_id
            log%sensor_id = observ%sensor_id
            log%target_id = observ%target_id
            log%observ_id = observ%id
        else
            log%node_id = this%node_id
        end if

        if (present(timestamp)) then
            log%timestamp = timestamp
        else
            log%timestamp = dm_time_now()
        end if

        if (present(error)) log%error = error

        ! Output and send log.
        if (this%verbose) call this%out(log)
        if (this%ipc)     call this%send(log)
    end subroutine logger_log_args

    subroutine logger_log_critical(this, message, source, observ, timestamp, error, escape, verbose)
        !! Sends a critical log message to the message queue.
        class(logger_class), intent(inout)           :: this      !! Logger object.
        character(len=*),    intent(in)              :: message   !! Log message.
        character(len=*),    intent(in),    optional :: source    !! Optional source of log.
        type(observ_type),   intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),    intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,             intent(in),    optional :: error     !! Optional error code.
        logical,             intent(in),    optional :: escape    !! Escape non-printable characters in message.
        logical,             intent(in),    optional :: verbose   !! Create log if `error` is `E_NONE`.

        call this%log(LL_CRITICAL, message, source, observ, timestamp, error, escape, verbose)
    end subroutine logger_log_critical

    subroutine logger_log_debug(this, message, source, observ, timestamp, error, escape, verbose)
        !! Sends a debugging log message to the message queue.
        class(logger_class), intent(inout)           :: this      !! Logger object.
        character(len=*),    intent(in)              :: message   !! Log message.
        character(len=*),    intent(in),    optional :: source    !! Optional source of log.
        type(observ_type),   intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),    intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,             intent(in),    optional :: error     !! Optional error code.
        logical,             intent(in),    optional :: escape    !! Escape non-printable characters in message.
        logical,             intent(in),    optional :: verbose   !! Create log if `error` is `E_NONE`.

        call this%log(LL_DEBUG, message, source, observ, timestamp, error, escape, verbose)
    end subroutine logger_log_debug

    subroutine logger_log_error(this, message, source, observ, timestamp, error, escape, verbose)
        !! Sends a error log message to the message queue.
        class(logger_class), intent(inout)           :: this      !! Logger object.
        character(len=*),    intent(in)              :: message   !! Log message.
        character(len=*),    intent(in),    optional :: source    !! Optional source of log.
        type(observ_type),   intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),    intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,             intent(in),    optional :: error     !! Optional error code.
        logical,             intent(in),    optional :: escape    !! Escape non-printable characters in message.
        logical,             intent(in),    optional :: verbose   !! Create log if `error` is `E_NONE`.

        call this%log(LL_ERROR, message, source, observ, timestamp, error, escape, verbose)
    end subroutine logger_log_error

    subroutine logger_log_info(this, message, source, observ, timestamp, error, escape, verbose)
        !! Sends a info log message to the message queue.
        class(logger_class), intent(inout)           :: this      !! Logger object.
        character(len=*),    intent(in)              :: message   !! Log message.
        character(len=*),    intent(in),    optional :: source    !! Optional source of log.
        type(observ_type),   intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),    intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,             intent(in),    optional :: error     !! Optional error code.
        logical,             intent(in),    optional :: escape    !! Escape non-printable characters in message.
        logical,             intent(in),    optional :: verbose   !! Create log if `error` is `E_NONE`.

        call this%log(LL_INFO, message, source, observ, timestamp, error, escape, verbose)
    end subroutine logger_log_info

    subroutine logger_log_type(this, log)
        !! Sends a log data type to the message queue (send & forget). The
        !! passed log is not validated and must have id, node id, and
        !! timestamp. The receiver may decline the log if the data is
        !! invalid. The log is forwarded even if the error code is `E_NONE`.
        class(logger_class), intent(inout) :: this !! Logger object.
        type(log_type),      intent(inout) :: log  !! Log type.

        if (this%verbose) call this%out(log)
        if (this%ipc)     call this%send(log)
    end subroutine logger_log_type

    subroutine logger_log_user(this, message, source, observ, timestamp, error, escape, verbose)
        !! Sends a user-defined log message to the message queue.
        class(logger_class), intent(inout)           :: this      !! Logger object.
        character(len=*),    intent(in)              :: message   !! Log message.
        character(len=*),    intent(in),    optional :: source    !! Optional source of log.
        type(observ_type),   intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),    intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,             intent(in),    optional :: error     !! Optional error code.
        logical,             intent(in),    optional :: escape    !! Escape non-printable characters in message.
        logical,             intent(in),    optional :: verbose   !! Create log if `error` is `E_NONE`.

        call this%log(LL_USER, message, source, observ, timestamp, error, escape, verbose)
    end subroutine logger_log_user

    subroutine logger_log_warning(this, message, source, observ, timestamp, error, escape, verbose)
        !! Sends a warning log message to the message queue.
        class(logger_class), intent(inout)           :: this      !! Logger object.
        character(len=*),    intent(in)              :: message   !! Log message.
        character(len=*),    intent(in),    optional :: source    !! Optional source of log.
        type(observ_type),   intent(inout), optional :: observ    !! Optional observation data.
        character(len=*),    intent(in),    optional :: timestamp !! Optional timestamp of log.
        integer,             intent(in),    optional :: error     !! Optional error code.
        logical,             intent(in),    optional :: escape    !! Escape non-printable characters in message.
        logical,             intent(in),    optional :: verbose   !! Create log if `error` is `E_NONE`.

        call this%log(LL_WARNING, message, source, observ, timestamp, error, escape, verbose)
    end subroutine logger_log_warning

    subroutine logger_out(this, log, unit)
        !! Prints log message to standard error.
        character(len=*), parameter :: FMT_ERROR = '(a, " [", a, "] ", a, " - ", a, " (", i0, ")")'
        character(len=*), parameter :: FMT_NONE  = '(a, " [", a, "] ", a, " - ", a)'

        class(logger_class), intent(inout)        :: this !! Logger object.
        type(log_type),      intent(inout)        :: log  !! Log to output.
        integer,             intent(in), optional :: unit !! File unit.

        integer :: level, unit_

        level = LL_ERROR
        if (dm_log_is_valid(log%level)) level = log%level

        unit_ = stderr
        if (present(unit)) unit_ = unit

        if (.not. this%no_color) call dm_ansi_color(LOGGER_COLORS(level))

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

        if (.not. this%no_color) call dm_ansi_color(COLOR_RESET)
    end subroutine logger_out

    subroutine logger_send(this, log)
        !! Sends log message to default log message queue (fire & forget)
        !! if logger attribute `ipc` is `.true.`. Prints message to standard
        !! output if attribute `verbose` is `.true.`.
        use :: dm_mqueue

        class(logger_class), intent(inout) :: this !! Logger object.
        type(log_type),      intent(inout) :: log  !! Log type.

        integer           :: rc
        type(mqueue_type) :: mqueue

        if (.not. this%ipc) return
        if (this%min_level > LL_DEBUG .and. log%level <= LL_DEBUG) return

        ! Open message queue for writing.
        rc = dm_mqueue_open(mqueue   = mqueue, &
                            type     = TYPE_LOG, &
                            name     = this%name, &
                            access   = MQUEUE_WRONLY, &
                            blocking = this%blocking)

        if (dm_is_error(rc)) then
            call this%fail('failed to open mqueue /' // this%name, rc)
            return
        end if

        ! Write log message to queue.
        rc = dm_mqueue_write(mqueue, log)

        if (dm_is_error(rc)) then
            call this%fail('failed to write to mqueue /' // this%name, rc)
        end if

        ! Close message queue.
        rc = dm_mqueue_close(mqueue)

        if (dm_is_error(rc)) then
            call this%fail('failed to close mqueue /' // this%name, rc)
        end if
    end subroutine logger_send
end module dm_logger
