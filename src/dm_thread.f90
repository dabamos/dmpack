! Author:  Philipp Engel
! Licence: ISC
module dm_thread
    !! Abstraction layer of POSIX threads. Has to be linked with `-lpthread`.
    !!
    !! The thread routine must match the C-interoperable abstract interface
    !! `dm_thread_routine(arg)`, for example:
    !!
    !! ```fortran
    !! subroutine thread_routine(arg) bind(c)
    !!     !! C-interoperable POSIX thread routine.
    !!     use, intrinsic :: iso_c_binding
    !!     type(c_ptr), intent(in), value :: arg !! C pointer to client data.
    !!     integer, pointer               :: i   !! Fortran pointer to client data.
    !!
    !!     if (.not. c_associated(arg)) return
    !!     call c_f_pointer(arg, i)
    !!     print '("value: ", i0)', i
    !! end subroutine thread_routine
    !! ```
    !!
    !! The dummy argument `arg` can be of any type. The thread routine and the
    !! argument have to be passed to the create function:
    !!
    !! ```fortran
    !! integer, target   :: arg
    !! integer           :: rc
    !! type(thread_type) :: thread
    !!
    !! arg = 123
    !!
    !! rc = dm_thread_create(thread, thread_routine, arg)
    !! rc = dm_thread_join(thread)
    !! ```
    !!
    !! The functions return `E_SYSTEM` on error.
    use :: unix
    use :: dm_error
    implicit none (type, external)
    private

    abstract interface
        subroutine dm_thread_routine(arg) bind(c)
            !! C-interoperable POSIX thread routine.
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: arg !! Client data as C pointer.
        end subroutine dm_thread_routine
    end interface

    type, public :: thread_type
        !! Opaque POSIX thread type.
        private
        type(c_pthread_t) :: state !! POSIX thread handle.
    end type thread_type

    public :: dm_thread_create
    public :: dm_thread_join
    public :: dm_thread_routine
contains
    integer function dm_thread_create(thread, routine, arg) result(rc)
        !! Creates POSIX thread. The function returns `E_SYSTEM` on error.
        type(thread_type), intent(out)   :: thread  !! Thread type.
        procedure(dm_thread_routine)     :: routine !! Callback procedure of POSIX thread.
        type(*), target,   intent(inout) :: arg     !! Client data to be passed to thread procedure.

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
