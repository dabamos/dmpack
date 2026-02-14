! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_async
    !! IPC async task module.
    use :: dm_c
    use :: dm_error
    use :: dm_ipc
    use :: dm_ipc_message
    use :: dm_ipc_type
    implicit none (type, external)
    private

    abstract interface
        subroutine dm_ipc_async_callback(client_data) bind(c)
            !! C-interoperable callback routine for `nng_aio_alloc()`.
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: client_data !! Passed client data.
        end subroutine dm_ipc_async_callback
    end interface

    type, public :: ipc_async_type
        !! NNG aio container.
        integer     :: error_nng = 0          !! Last NNG return code.
        type(c_ptr) :: context   = c_null_ptr !! C pointer to NNG aio.
    end type ipc_async_type

    type, public :: ipc_async_task_type
        !! Asynchronous task type.
        integer                :: id      = 0                         !! Task id.
        integer                :: state   = IPC_ASYNC_TASK_STATE_INIT !! IPC async task state.
        integer                :: error   = E_NONE                    !! DMPACK error code.
        type(ipc_async_type)   :: async   = ipc_async_type()          !! IPC async context.
        type(ipc_context_type) :: context = ipc_context_type()        !! IPC context.
        type(ipc_message_type) :: message = ipc_message_type()        !! IPC message.
    end type ipc_async_task_type

    public :: dm_ipc_async_cancel
    public :: dm_ipc_async_destroy
    public :: dm_ipc_async_init
    public :: dm_ipc_async_get_message
    public :: dm_ipc_async_msleep
    public :: dm_ipc_async_receive
    public :: dm_ipc_async_result
    public :: dm_ipc_async_send
    public :: dm_ipc_async_set_id
    public :: dm_ipc_async_set_message
    public :: dm_ipc_async_set_timeout
    public :: dm_ipc_async_sleep
    public :: dm_ipc_async_wait
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ipc_async_init(task, callback) result(rc)
        use :: nng, only: nng_aio_alloc

        type(ipc_async_task_type), target, intent(inout) :: task     !! IPC async task.
        procedure(dm_ipc_async_callback)                 :: callback !! Message handling subroutine.

        task%async%error_nng = nng_aio_alloc(task%async%context, c_funloc(callback), c_loc(task))
        rc = dm_ipc_error(task%async%error_nng)
    end function dm_ipc_async_init

    integer function dm_ipc_async_result(task) result(rc)
        use :: nng, only: nng_aio_result

        type(ipc_async_task_type), intent(inout) :: task !! IPC async task.

        task%async%error_nng = nng_aio_result(task%async%context)
        rc = dm_ipc_error(task%async%error_nng)
    end function dm_ipc_async_result

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_ipc_async_cancel(task)
        use :: nng, only: nng_aio_cancel

        type(ipc_async_task_type), intent(inout) :: task !! IPC async task.

        call nng_aio_cancel(task%async%context)
    end subroutine dm_ipc_async_cancel

    impure elemental subroutine dm_ipc_async_destroy(task)
        use :: nng, only: nng_aio_free, nng_aio_stop

        type(ipc_async_task_type), intent(inout) :: task !! IPC async task.

        call nng_aio_stop(task%async%context)
        call nng_aio_free(task%async%context)
    end subroutine dm_ipc_async_destroy

    subroutine dm_ipc_async_get_message(task, message, error)
        use :: nng, only: nng_aio_get_msg

        type(ipc_async_task_type), intent(inout)         :: task    !! IPC async task.
        type(ipc_message_type),    intent(out), optional :: message !! IPC message.
        integer,                   intent(out), optional :: error   !! Error code.

        integer :: rc

        if (present(message)) then
            message%context = nng_aio_get_msg(task%async%context)
            rc = dm_ipc_message_header(message)
        else
            task%message%context = nng_aio_get_msg(task%async%context)
            rc = dm_ipc_message_header(task%message)
        end if

        if (present(error)) error = rc
    end subroutine dm_ipc_async_get_message

    subroutine dm_ipc_async_msleep(async, msec)
        use :: nng, only: c_uint32_t, nng_sleep_aio

        type(ipc_async_type), intent(inout) :: async !! IPC async context.
        integer,              intent(in)    :: msec  !! Delay [msec].

        call nng_sleep_aio(int(msec, c_uint32_t), async%context)
    end subroutine dm_ipc_async_msleep

    subroutine dm_ipc_async_receive(task)
        use :: nng, only: nng_ctx_recv

        type(ipc_async_task_type), intent(inout) :: task !! IPC async task.

        call nng_ctx_recv(task%context%context, task%async%context)
    end subroutine dm_ipc_async_receive

    subroutine dm_ipc_async_send(task, timeout)
        !! Sends message using NNG context asynchronously.
        !!
        !! The function assumes ownership of the NNG message. If the message was
        !! successfully queued for delivery to the socket, then the NNG aio will
        !! be completed, and `dm_ipc_async_result()` will return `E_NONE`.
        !!
        !! If the operation fails for any reason (including cancellation or
        !! timeout), then the callback will be executed and
        !! `dm_ipc_async_result()` will return a non-zero error status. In this
        !! case, the callback has a responsibility to retrieve the message from
        !! the aio with `dm_ipc_async_get_message()` and dispose of it
        !! appropriately. (This may include retrying the send operation on the
        !! same or a different socket, or deallocating the message with
        !! `dm_ipc_message_destroy()`.)
        !!
        !! The semantics of what sending a message means varies from protocol to
        !! protocol, so examination of the NNG protocol documentation is
        !! encouraged.
        !!
        !! Context send operations are asynchronous. If a synchronous operation
        !! is needed, one can be constructed by using a `NULL` callback on the
        !! NNG aio and then waiting for the operation using
        !! `dm_ipc_async_wait()`.
        !!
        !! If `timeout` is passed, sets the duration in milliseconds as a send
        !! timeout. This causes a timer to be started when the operation is
        !! actually started. If the timer expires before the operation is
        !! completed, then it is aborted with an error of `E_TIMEOUT`. The
        !! timeout is specified as a relative number of milliseconds.
        !!
        !! If the timeout is `IPC_TIMEOUT_INFINITE`, then no timeout is used. If
        !! the timeout is `IPC_TIMEOUT_DEFAULT`, then a default or
        !! socket-specific timeout is used. (This is frequently the same as
        !! `IPC_TIMEOUT_INFINITE`.)
        use :: nng, only: nng_ctx_send

        type(ipc_async_task_type), intent(inout)        :: task    !! IPC async task.
        integer,                   intent(in), optional :: timeout !! Timeout [msec].

        if (present(timeout)) call dm_ipc_async_set_timeout(task, timeout)
        call nng_ctx_send(task%context%context, task%async%context)
    end subroutine dm_ipc_async_send

    pure elemental subroutine dm_ipc_async_set_id(task, id)
        !! Sets the task id.
        type(ipc_async_task_type), intent(inout) :: task !! IPC async task.
        integer,                   intent(in)    :: id   !! IPC async task id.

        task%id = id
    end subroutine dm_ipc_async_set_id

    subroutine dm_ipc_async_set_message(task, message)
        !! Sets message for asynchronous send. The `nng_aio_type` of the task
        !! must not have an operation in progress.
        use :: nng, only: nng_aio_set_msg

        type(ipc_async_task_type), intent(inout)           :: task    !! IPC async task.
        type(ipc_message_type),    intent(inout), optional :: message !! IPC message.

        if (present(message)) then
            call nng_aio_set_msg(task%async%context, message%context)
        else
            call nng_aio_set_msg(task%async%context, task%message%context)
        end if
    end subroutine dm_ipc_async_set_message

    subroutine dm_ipc_async_set_timeout(task, timeout)
        !! Sets a timeout for the asynchronous operation associated with the NNG
        !! aio object. This causes a timer to be started when the operation is
        !! actually started. If the timer expires before the operation is
        !! completed, then it is aborted with an error of `E_TIMEOUT`. The
        !! timeout is specified as a relative number of milliseconds.
        !!
        !! If the timeout is `IPC_TIMEOUT_INFINITE`, then no timeout is used. If
        !! the timeout is `IPC_TIMEOUT_DEFAULT`, then a default or
        !! socket-specific timeout is used. (This is frequently the same as
        !! `IPC_TIMEOUT_INFINITE`.)
        use :: nng, only: nng_aio_set_timeout, nng_duration

        type(ipc_async_task_type), intent(inout) :: task    !! IPC async task.
        integer,                   intent(in)    :: timeout !! Timeout [msec].

        call nng_aio_set_timeout(task%async%context, int(timeout, nng_duration))
    end subroutine dm_ipc_async_set_timeout

    subroutine dm_ipc_async_sleep(async, sec)
        use :: nng, only: c_uint32_t, nng_sleep_aio

        type(ipc_async_type), intent(inout) :: async !! IPC async context.
        integer,              intent(in)    :: sec   !! Delay [sec].

        call nng_sleep_aio(int(sec * 1000, c_uint32_t), async%context)
    end subroutine dm_ipc_async_sleep

    subroutine dm_ipc_async_wait(task)
        !! Waits for the asynchronous operation to finish.
        use :: nng, only: nng_aio_wait

        type(ipc_async_task_type), intent(inout) :: task  !! IPC async task.

        call nng_aio_wait(task%async%context)
    end subroutine dm_ipc_async_wait
end module dm_ipc_async
