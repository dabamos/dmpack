! Author:  Philipp Engel
! Licence: ISC
module dm_thread
    !! Abstraction layer of POSIX threads. Has to be linked with `-lpthread`.
    use :: unix
    use :: dm_error
    implicit none (type, external)
    private

    abstract interface
        subroutine dm_thread_routine(arg) bind(c)
            !! C-interoperable POSIX thread routine.
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: arg
        end subroutine dm_thread_routine
    end interface

    type, public :: thread_type
        !! Opaque POSIX thread type.
        private
        type(c_pthread_t) :: state
    end type thread_type

    public :: dm_thread_create
    public :: dm_thread_join
    public :: dm_thread_routine
contains
    integer function dm_thread_create(thread, routine, arg) result(rc)
        !! Creates POSIX thread. The function returns `E_SYSTEM` on error.
        type(thread_type), intent(out)   :: thread  !! Thread type.
        procedure(dm_thread_routine)     :: routine !! Callback procedure of POSIX thread.
        type(*), target,   intent(inout) :: arg     !! Thread argument.

        integer :: stat

        rc = E_SYSTEM
        stat = c_pthread_create(thread%state, c_null_ptr, c_funloc(routine), c_loc(arg))
        if (stat /= 0) return
        rc = E_NONE
    end function dm_thread_create

    integer function dm_thread_join(thread, value) result(rc)
        !! Join POSIX thread, and optionally returns value as C pointer.
        type(thread_type), intent(inout)         :: thread !! Thread type.
        type(c_ptr),       intent(out), optional :: value  !! Returned thread value.

        integer     :: stat
        type(c_ptr) :: ptr

        rc = E_SYSTEM
        if (present(value)) then
            stat = c_pthread_join(thread%state, value)
        else
            stat = c_pthread_join(thread%state, ptr)
        end if
        if (stat /= 0) return
        rc = E_NONE
    end function dm_thread_join
end module dm_thread
