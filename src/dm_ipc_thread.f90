! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_thread
    !! Thin abstraction layer over ZeroMQ threads.
    use :: dm_c
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    type, public :: ipc_thread_type
        !! Opaque ZeroMQ thread context.
        private
        type(c_ptr) :: context = c_null_ptr
    end type ipc_thread_type

    public :: dm_ipc_thread_callback

    abstract interface
        subroutine dm_ipc_thread_callback(argument) bind(c)
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: argument
        end subroutine dm_ipc_thread_callback
    end interface

    public :: dm_ipc_thread_create
    public :: dm_ipc_thread_join
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS
    ! **************************************************************************
    integer function dm_ipc_thread_create(thread, callback, argument) result(rc)
        !! Creates IPC thread. The function returns `E_IPC` on error.
        use :: zmq, only: zmq_threadstart

        type(ipc_thread_type), intent(out)   :: thread   !! IPC thread.
        procedure(dm_ipc_thread_callback)    :: callback !! IPC callback procedure.
        type(*), target,       intent(inout) :: argument !! Client data to be passed to thread procedure.

        rc = E_IPC
        thread%context = zmq_threadstart(callback, c_loc(argument))
        if (c_associated(thread%context)) rc = E_NONE
    end function dm_ipc_thread_create

    integer function dm_ipc_thread_join(thread) result(rc)
        !! Closes given IPC thread. The function returns `E_NULL` if the thread
        !! pointer is not associated.
        use :: zmq, only: zmq_threadclose

        type(ipc_thread_type), intent(inout) :: thread !! IPC thread.

        rc = E_NULL
        if (.not. c_associated(thread%context)) return

        rc = E_NONE
        call zmq_threadclose(thread%context)
    end function dm_ipc_thread_join
end module dm_ipc_thread
