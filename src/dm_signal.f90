! Author:  Philipp Engel
! Licence: ISC
module dm_signal
    !! Auxiliary interfaces and routines for signal handling on Unix.
    use :: unix
    implicit none (type, external)
    private

    abstract interface
        subroutine dm_signal_handler(signum) bind(c)
            !! C-interoperable signal handler routine interface, to be passed as
            !! argument to `dm_signal_register()`.
            import :: c_int
            implicit none
            integer(kind=c_int), intent(in), value :: signum !! Signal number.
        end subroutine dm_signal_handler
    end interface

    public :: dm_signal_handler
    public :: dm_signal_register
contains
    subroutine dm_signal_register(handler)
        !! Registers passed C-interoperable POSIX signal handler routine for
        !! `SIGINT`, `SIGQUIT`, `SIGABRT`, `SIGKILL`, and `SIGTERM`.
        procedure(dm_signal_handler) :: handler !! Signal handler routine.

        type(c_funptr) :: ptr

        ptr = c_signal(SIGINT,  c_funloc(handler))
        ptr = c_signal(SIGQUIT, c_funloc(handler))
        ptr = c_signal(SIGABRT, c_funloc(handler))
        ptr = c_signal(SIGKILL, c_funloc(handler))
        ptr = c_signal(SIGTERM, c_funloc(handler))
    end subroutine dm_signal_register
end module dm_signal
