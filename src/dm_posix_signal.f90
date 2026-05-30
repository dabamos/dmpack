! Author:  Philipp Engel
! Licence: ISC
module dm_posix_signal
    !! Auxiliary interfaces and routines for signal handling on Unix.
    !!
    !! ## Examples
    !!
    !! Example program to catch signal `SIGINT` (Ctrl-C):
    !!
    !! ``` fortran
    !! program main
    !!     use :: dmpack
    !!     implicit none (type, external)
    !!
    !!     integer                 :: n, rc
    !!     integer                 :: numbers(8)
    !!     type(posix_signal_type) :: signal
    !!
    !!     call dm_init()
    !!
    !!     rc = dm_posix_signal_create(signal)
    !!     rc = dm_posix_signal_register(SIGNAL_SIGINT,  signal_callback)
    !!     rc = dm_posix_signal_register(SIGNAL_SIGABRT, signal_callback)
    !!
    !!     do
    !!         ! Poll for signals.
    !!         rc = dm_posix_signal_poll(signal, timeout=0)
    !!         if (rc == E_INTERRUPT) cycle
    !!         if (dm_is_error(rc)) exit
    !!
    !!         ! Run arbitrary task.
    !!         print '("Working ...")'
    !!         call dm_posix_sleep(1)
    !!
    !!         ! Read catched signals.
    !!         rc = dm_posix_signal_read(signal, numbers, n)
    !!         if (n == 0) cycle
    !!
    !!         if (maxval(numbers) > 0) then
    !!             print '("Terminating ...")'
    !!             exit
    !!         end if
    !!     end do
    !!
    !!     call dm_posix_signal_destroy(signal)
    !! contains
    !!     ! subroutine dm_posix_signal_callback(number) bind(c)
    !!     subroutine signal_callback(number) bind(c)
    !!         integer(c_int), intent(in), value :: number
    !!
    !!         call dm_posix_signal_write(signal, number)
    !!     end subroutine signal_callback
    !! end program main
    !! ```
    use :: unix
    use :: dm_error
    use :: dm_posix
    implicit none (type, external)
    private

    ! DMPACK signal numbers.
    integer, parameter, public :: SIGNAL_NONE      = 0
    integer, parameter, public :: SIGNAL_SIGHUP    = SIGHUP
    integer, parameter, public :: SIGNAL_SIGINT    = SIGINT
    integer, parameter, public :: SIGNAL_SIGQUIT   = SIGQUIT
    integer, parameter, public :: SIGNAL_SIGILL    = SIGILL
    integer, parameter, public :: SIGNAL_SIGTRAP   = SIGTRAP
    integer, parameter, public :: SIGNAL_SIGABRT   = SIGABRT
    integer, parameter, public :: SIGNAL_SIGBUS    = SIGBUS
    integer, parameter, public :: SIGNAL_SIGFPE    = SIGFPE
    integer, parameter, public :: SIGNAL_SIGKILL   = SIGKILL
    integer, parameter, public :: SIGNAL_SIGUSR1   = SIGUSR1
    integer, parameter, public :: SIGNAL_SIGSEGV   = SIGSEGV
    integer, parameter, public :: SIGNAL_SIGUSR2   = SIGUSR2
    integer, parameter, public :: SIGNAL_SIGPIPE   = SIGPIPE
    integer, parameter, public :: SIGNAL_SIGALRM   = SIGALRM
    integer, parameter, public :: SIGNAL_SIGTERM   = SIGTERM
    integer, parameter, public :: SIGNAL_SIGCHLD   = SIGCHLD
    integer, parameter, public :: SIGNAL_SIGCONT   = SIGCONT
    integer, parameter, public :: SIGNAL_SIGSTOP   = SIGSTOP
    integer, parameter, public :: SIGNAL_SIGTSTP   = SIGTSTP
    integer, parameter, public :: SIGNAL_SIGTTIN   = SIGTTIN
    integer, parameter, public :: SIGNAL_SIGTTOU   = SIGTTOU
    integer, parameter, public :: SIGNAL_SIGURG    = SIGURG
    integer, parameter, public :: SIGNAL_SIGXCPU   = SIGXCPU
    integer, parameter, public :: SIGNAL_SIGXFSZ   = SIGXFSZ
    integer, parameter, public :: SIGNAL_SIGVTALRM = SIGVTALRM
    integer, parameter, public :: SIGNAL_SIGPROF   = SIGPROF
    integer, parameter, public :: SIGNAL_SIGWINCH  = SIGWINCH
    integer, parameter, public :: SIGNAL_SIGIO     = SIGIO
    integer, parameter, public :: SIGNAL_SIGSYS    = SIGSYS

    ! Pipe ends.
    integer, parameter :: SIGNAL_PIPE_READ  = 1
    integer, parameter :: SIGNAL_PIPE_WRITE = 2

    type, public :: posix_signal_type
        !! Opaque derived type that stores the file descriptors of the self-pipe.
        private
        integer        :: pipe(2) = -1         !! File descriptors.
        type(c_pollfd) :: fds(1)  = c_pollfd() !! Poll file descriptor.
    end type posix_signal_type

    public :: dm_posix_signal_callback

    abstract interface
        subroutine dm_posix_signal_callback(number) bind(c)
            !! C-interoperable signal callback routine interface, to be passed as
            !! argument to `dm_posix_signal_register()`.
            import :: c_int
            implicit none
            integer(c_int), intent(in), value :: number !! Signal number.
        end subroutine dm_posix_signal_callback
    end interface

    public :: dm_posix_signal_create
    public :: dm_posix_signal_destroy
    public :: dm_posix_signal_name
    public :: dm_posix_signal_poll
    public :: dm_posix_signal_read
    public :: dm_posix_signal_register
    public :: dm_posix_signal_should_terminate
    public :: dm_posix_signal_write
contains
    integer function dm_posix_signal_create(signal) result(rc)
        !! Creates self-pipe for signal.
        type(posix_signal_type), intent(out) :: signal !! Created signal.

        rc = E_SYSTEM
        if (c_pipe(signal%pipe) == -1) return

        rc = E_NONE
        signal%fds(1)%fd      = signal%pipe(SIGNAL_PIPE_READ)
        signal%fds(1)%events  = POLLIN
        signal%fds(1)%revents = 0
    end function dm_posix_signal_create

    subroutine dm_posix_signal_destroy(signal)
        !! Destroys self-pipe of signal.
        type(posix_signal_type), intent(inout) :: signal !! Signal to detroy.

        integer :: stat

        stat = c_close(signal%pipe(SIGNAL_PIPE_READ))
        stat = c_close(signal%pipe(SIGNAL_PIPE_WRITE))
    end subroutine dm_posix_signal_destroy

    pure function dm_posix_signal_name(number) result(name)
        !! Returns name of signal as allocatable string. If the signal number is
        !! unknown, the numeric value is returned instead.
        use :: dm_util, only: dm_itoa

        integer, intent(in)       :: number !! Signal number.
        character(:), allocatable :: name   !! Signal name.

        select case (number)
            case (SIGNAL_SIGHUP);    name = 'SIGHUP'
            case (SIGNAL_SIGINT);    name = 'SIGINT'
            case (SIGNAL_SIGQUIT);   name = 'SIGQUIT'
            case (SIGNAL_SIGILL);    name = 'SIGILL'
            case (SIGNAL_SIGTRAP);   name = 'SIGTRAP'
            case (SIGNAL_SIGABRT);   name = 'SIGABRT' ! SIGIOT
            case (SIGNAL_SIGBUS);    name = 'SIGBUS'
            case (SIGNAL_SIGFPE);    name = 'SIGFPE'
            case (SIGNAL_SIGKILL);   name = 'SIGKILL'
            case (SIGNAL_SIGUSR1);   name = 'SIGUSR1'
            case (SIGNAL_SIGSEGV);   name = 'SIGSEGV'
            case (SIGNAL_SIGUSR2);   name = 'SIGUSR2'
            case (SIGNAL_SIGPIPE);   name = 'SIGPIPE'
            case (SIGNAL_SIGALRM);   name = 'SIGALRM'
            case (SIGNAL_SIGTERM);   name = 'SIGTERM'
            case (SIGNAL_SIGCHLD);   name = 'SIGCHLD'
            case (SIGNAL_SIGCONT);   name = 'SIGCONT'
            case (SIGNAL_SIGSTOP);   name = 'SIGSTOP'
            case (SIGNAL_SIGTSTP);   name = 'SIGTSTP'
            case (SIGNAL_SIGTTIN);   name = 'SIGTTIN'
            case (SIGNAL_SIGTTOU);   name = 'SIGTTOU'
            case (SIGNAL_SIGURG);    name = 'SIGURG'
            case (SIGNAL_SIGXCPU);   name = 'SIGXCPU'
            case (SIGNAL_SIGXFSZ);   name = 'SIGXFSZ'
            case (SIGNAL_SIGVTALRM); name = 'SIGVTALRM'
            case (SIGNAL_SIGPROF);   name = 'SIGPROF'
            case (SIGNAL_SIGWINCH);  name = 'SIGWINCH'
            case (SIGNAL_SIGIO);     name = 'SIGIO'
            case (SIGNAL_SIGSYS);    name = 'SIGSYS'
            case default;            name = dm_itoa(number)
        end select
    end function dm_posix_signal_name

    integer function dm_posix_signal_poll(signal, timeout) result(rc)
        !! Polls the file descriptors of the signal pipe. If argument `timeout`
        !! is -1, this function waits forever. If `timeout` is 0, polling is
        !! non-blocking.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INTERRUPT` if an interrupt occured.
        !! * `E_SYSTEM` if system call failed.
        !!
        type(posix_signal_type), intent(inout) :: signal  !! Signal.
        integer,                 intent(in)    :: timeout !! Timeout [ms].

        integer :: stat

        stat = c_poll(signal%fds, size(signal%fds, kind=c_nfds_t), timeout)

        if (stat == -1) then
            rc = E_SYSTEM
            if (dm_posix_error() == EINTR) rc = E_INTERRUPT
        end if

        rc = E_NONE
    end function dm_posix_signal_poll

    integer function dm_posix_signal_read(signal, numbers, n) result(rc)
        !! Reads signal from self-pipe.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INTERRUPT` if an interrupt occured.
        !! * `E_SYSTEM` if system call failed.
        !!
        type(posix_signal_type), intent(inout) :: signal     !! Signal.
        integer,                 intent(inout) :: numbers(:) !! Array of signal numbers.
        integer,                 intent(out)   :: n          !! Number of signals.

        character(size(numbers)), target :: buffer
        integer                          :: i, nbytes
        logical                          :: has_event

        rc = E_NONE

        numbers = 0
        n       = 0

        ! Any signal notification available?
        has_event = (iand(int(signal%fds(1)%revents), POLLIN) == 1)
        if (.not. has_event) return

        ! Drain the self-pipe.
        buffer = repeat(char(0), len(buffer))
        nbytes = int(c_read(signal%pipe(SIGNAL_PIPE_READ), c_loc(buffer), len(buffer, c_size_t)))

        if (nbytes == -1) then
            rc = E_SYSTEM
            if (dm_posix_error() == EINTR) rc = E_INTERRUPT
            return
        end if

        n = nbytes

        ! Fill the signal numbers array.
        do i = 1, n
            numbers(i) = ichar(buffer(i:i))

            if (numbers(i) == 0) then
                n = i
                return
            end if
        end do
    end function dm_posix_signal_read

    integer function dm_posix_signal_register(number, callback) result(rc)
        !! Registers signal handler. The function returns `E_SYSTEM` on error.
        integer, intent(in)                 :: number   !! Signal number.
        procedure(dm_posix_signal_callback) :: callback !! Subroutine to register.

        type(c_sigaction_t) :: sa

        rc = E_NONE

        sa%sa_handler = c_funloc(callback)
        sa%sa_flags   = 0 ! No SA_RESTART: allow poll() to wake promptly.

        if (c_sigaction(number, sa) == -1) rc = E_SYSTEM
    end function dm_posix_signal_register

    logical function dm_posix_signal_should_terminate(signal, number) result(should)
        !! Reads catched signals (if any) and returns `.true.` if `SIGINT`,
        !! `SIGQUIT`, `SIGABRT`, or `SIGTERM` has been received. The signal
        !! number is returned in optional argument `number`. If no signal has
        !! been received, `number` is set to 0.
        !!
        !! The function inspects the last 32 signals (or less).
        integer, parameter :: NSIGNALS = 32

        type(posix_signal_type), intent(inout)         :: signal !! Self-pipe.
        integer,                 intent(out), optional :: number !! Signal number.

        integer :: i, n, rc, s
        integer :: signals(NSIGNALS)

        should = .false.
        if (present(number)) number = SIGNAL_NONE

        rc = dm_posix_signal_read(signal, signals, n)
        if (n == 0) return ! No events.

        do i = 1, n
            s = signals(i)

            select case (s)
                case (SIGNAL_NONE)
                    return

                case (SIGNAL_SIGINT, SIGNAL_SIGQUIT, SIGNAL_SIGABRT, SIGNAL_SIGTERM)
                    should = .true.
                    if (present(number)) number = s
                    return
            end select
        end do
    end function dm_posix_signal_should_terminate

    subroutine dm_posix_signal_write(signal, number)
        !! Writes signal number to self-pipe.
        type(posix_signal_type), intent(inout) :: signal !! Signal.
        integer,                 intent(in)    :: number !! Signal number to write.

        character, target :: a
        integer           :: n

        ! Ignore errors intentionally. The call to write() is async-signal-safe.
        a = char(number)
        n = int(c_write(signal%pipe(SIGNAL_PIPE_WRITE), c_loc(a), len(a, c_size_t)))
    end subroutine dm_posix_signal_write
end module dm_posix_signal
