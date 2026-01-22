! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_thread
    !! Abstraction layer over NNG threads.
    !!
    !! Thread routines must have the `bind(c)` attribute:
    !!
    !! ```fortran
    !! subroutine thread_callback(arg) bind(c)
    !!     !! C-interoperable NNG thread routine.
    !!     use, intrinsic :: iso_c_binding
    !!     type(c_ptr), intent(in), value :: arg !! C pointer to client data.
    !!     integer, pointer               :: i   !! Fortran pointer to client data.
    !!
    !!     if (.not. c_associated(arg)) return
    !!     call c_f_pointer(arg, i)
    !!     print '("value: ", i0)', i
    !! end subroutine thread_callback
    !! ```
    !!
    !! The dummy argument `arg` can be of any type. The thread routine and the
    !! argument have to be passed to the create function:
    !!
    !! ```fortran
    !! integer, target       :: arg
    !! integer               :: rc
    !! type(ipc_thread_type) :: thread
    !!
    !! arg = 123
    !!
    !! rc = dm_ipc_thread_create(thread, thread_callback, arg)
    !! call dm_ipc_thread_join(thread)
    !! ```
    !!
    !! In contrast to the POSIX thread interface of module `dm_posix_thread`,
    !! the NNG routines are cross-platform.
    use :: dm_c
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    abstract interface
        subroutine dm_ipc_thread_callback(ptr) bind(c)
            !! C-interoperable NNG thread routine.
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr !! Thread argument.
        end subroutine dm_ipc_thread_callback
    end interface

    type, public :: ipc_thread_type
        !! Opaque NNG thread type.
        private
        type(c_ptr) :: ctx = c_null_ptr !! NNG thread context.
    end type ipc_thread_type

    public :: dm_ipc_thread_create
    public :: dm_ipc_thread_join
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_ipc_thread_create(thread, callback, argument) result(rc)
        !! The function creates a single thread of execution, running func with
        !! the argument arg. The thread is started immediately. A pointer to
        !! the NNG thread object is returned in dummy argument `thread`.
        !!
        !! The intention of this module is to facilitate writing parallel
        !! programs. Threads created by this module will be based upon the
        !! underlying threading mechanism of the system that NNG is running on.
        !! This may include use of coroutines.
        !!
        !! Using threads created by this function can make it easy to write
        !! programs that use simple sequential execution, using functions in
        !! the NNG suite that would otherwise normally wait synchronously for
        !! completion.
        !!
        !! When the thread is no longer needed, the `dm_ipc_thread_join()`
        !! routine should be used to reap it. (This function will block waiting
        !! for `callback` to return.)
        use :: nng,    only: nng_thread_create
        use :: dm_ipc, only: dm_ipc_error

        type(ipc_thread_type), intent(out)   :: thread   !! IPC thread.
        procedure(dm_ipc_thread_callback)    :: callback !! Thread routine.
        type(*), target,       intent(inout) :: argument !! Client data to be passed to thread procedure.

        integer :: stat

        stat = nng_thread_create(thread%ctx, c_funloc(callback), c_loc(argument))
        rc = dm_ipc_error(stat)
    end function dm_ipc_thread_create

    subroutine dm_ipc_thread_join(thread)
        !! Joins and destroys NNG thread. This routine is blocking.
        use :: nng, only: nng_thread_destroy

        type(ipc_thread_type), intent(inout) :: thread !! IPC thread.

        if (.not. c_associated(thread%ctx)) return
        call nng_thread_destroy(thread%ctx)
    end subroutine dm_ipc_thread_join
end module dm_ipc_thread
