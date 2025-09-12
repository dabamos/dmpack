! Author:  Philipp Engel
! Licence: ISC
module dm_signal
    !! Auxiliary interfaces and routines for signal handling on Unix.
    use :: unix
    implicit none (type, external)
    private

    abstract interface
        subroutine dm_signal_callback(signum) bind(c)
            !! C-interoperable signal callback routine interface, to be passed as
            !! argument to `dm_signal_register()`.
            import :: c_int
            implicit none
            integer(c_int), intent(in), value :: signum !! Signal number.
        end subroutine dm_signal_callback
    end interface

    public :: dm_signal_callback
    public :: dm_signal_name
    public :: dm_signal_register
contains
    function dm_signal_name(signum) result(name)
        !! Returns name of signal as allocatable string. If the signal number is
        !! unknown, the numeric value is returned instead.
        use :: dm_util, only: dm_itoa

        integer, intent(in)       :: signum !! Signal number.
        character(:), allocatable :: name   !! Signal name.

        select case (signum)
            case (SIGHUP);    name = 'SIGHUP'
            case (SIGINT);    name = 'SIGINT'
            case (SIGQUIT);   name = 'SIGQUIT'
            case (SIGILL);    name = 'SIGILL'
            case (SIGTRAP);   name = 'SIGTRAP'
            case (SIGABRT);   name = 'SIGABRT' ! SIGIOT
            case (SIGBUS);    name = 'SIGBUS'
            case (SIGFPE);    name = 'SIGFPE'
            case (SIGKILL);   name = 'SIGKILL'
            case (SIGUSR1);   name = 'SIGUSR1'
            case (SIGSEGV);   name = 'SIGSEGV'
            case (SIGUSR2);   name = 'SIGUSR2'
            case (SIGPIPE);   name = 'SIGPIPE'
            case (SIGALRM);   name = 'SIGALRM'
            case (SIGTERM);   name = 'SIGTERM'
            case (SIGCHLD);   name = 'SIGCHLD'
            case (SIGCONT);   name = 'SIGCONT'
            case (SIGSTOP);   name = 'SIGSTOP'
            case (SIGTSTP);   name = 'SIGTSTP'
            case (SIGTTIN);   name = 'SIGTTIN'
            case (SIGTTOU);   name = 'SIGTTOU'
            case (SIGURG);    name = 'SIGURG'
            case (SIGXCPU);   name = 'SIGXCPU'
            case (SIGXFSZ);   name = 'SIGXFSZ'
            case (SIGVTALRM); name = 'SIGVTALRM'
            case (SIGPROF);   name = 'SIGPROF'
            case (SIGWINCH);  name = 'SIGWINCH'
            case (SIGIO);     name = 'SIGIO'
            case (SIGSYS);    name = 'SIGSYS'
            case default;     name = dm_itoa(signum)
        end select
    end function dm_signal_name

    subroutine dm_signal_register(handler)
        !! Registers passed C-interoperable POSIX signal callback routine for
        !! `SIGINT`, `SIGQUIT`, `SIGABRT`, `SIGKILL`, and `SIGTERM`.
        procedure(dm_signal_callback) :: handler !! Signal callback routine.

        type(c_funptr) :: ptr

        ptr = c_signal(SIGINT,  c_funloc(handler))
        ptr = c_signal(SIGQUIT, c_funloc(handler))
        ptr = c_signal(SIGABRT, c_funloc(handler))
        ptr = c_signal(SIGKILL, c_funloc(handler))
        ptr = c_signal(SIGTERM, c_funloc(handler))
    end subroutine dm_signal_register
end module dm_signal
