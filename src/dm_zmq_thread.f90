! Author:  Philipp Engel
! Licence: ISC
module dm_zmq_thread
    !! Abstraction layer over ZeroMQ threads.
    use :: dm_c
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    type, public :: zmq_thread_type
        type(c_ptr) :: context = c_null_ptr
    end type zmq_thread_type

    abstract interface
        subroutine dm_zmq_thread_callback(argument) bind(c)
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: argument
        end subroutine dm_zmq_thread_callback
    end interface

    public :: dm_zmq_thread_callback

    public :: dm_zmq_thread_create
    public :: dm_zmq_thread_join
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_zmq_thread_create(thread, callback, argument) result(rc)
        !! Creates ZMQ thread. The function returns `E_ZMQ` on error.
        use :: zeromq, only: zmq_threadstart

        type(zmq_thread_type), intent(out)   :: thread   !! ZMQ thread.
        procedure(dm_zmq_thread_callback)    :: callback !! ZMQ callback procedure.
        type(*), target,       intent(inout) :: argument !! Client data to be passed to thread procedure.

        rc = E_ZMQ
        thread%context = zmq_threadstart(callback, c_loc(argument))
        if (c_associated(thread%context)) rc = E_NONE
    end function dm_zmq_thread_create

    integer function dm_zmq_thread_join(thread) result(rc)
        !! Closes given ZMQ thread. The function returns `E_NULL` if the thread
        !! pointer is not associated.
        use :: zeromq, only: zmq_threadclose

        type(zmq_thread_type), intent(inout) :: thread !! ZMQ thread.

        rc = E_NULL
        if (.not. c_associated(thread%context)) return

        rc = E_NONE
        call zmq_threadclose(thread%context)
    end function dm_zmq_thread_join
end module dm_zmq_thread
