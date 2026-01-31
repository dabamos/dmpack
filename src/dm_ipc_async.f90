! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_async
    !! IPC async worker.
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
        type(ipc_async_type)   :: async   = ipc_async_type()          !! IPC async context.
        type(ipc_context_type) :: context = ipc_context_type()        !! IPC context.
        type(ipc_message_type) :: message = ipc_message_type()        !! IPC message.
    end type ipc_async_task_type

    public :: dm_ipc_async_destroy
    public :: dm_ipc_async_init
    public :: dm_ipc_async_get_message
    public :: dm_ipc_async_msleep
    public :: dm_ipc_async_receive
    public :: dm_ipc_async_result
    public :: dm_ipc_async_send
    public :: dm_ipc_async_set_message
    public :: dm_ipc_async_set_id
    public :: dm_ipc_async_set_state
    public :: dm_ipc_async_wait
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ipc_async_init(task, callback, id) result(rc)
        use :: nng, only: nng_aio_alloc

        type(ipc_async_task_type), target, intent(inout)        :: task     !! IPC async task.
        procedure(dm_ipc_async_callback)                        :: callback !! Message handling subroutine.
        integer,                           intent(in), optional :: id       !! IPC async task id.

        if (present(id)) call dm_ipc_async_set_id(task, id)

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
    impure elemental subroutine dm_ipc_async_destroy(task)
        use :: nng, only: nng_aio_free

        type(ipc_async_task_type), intent(inout) :: task !! IPC async task.

        call dm_ipc_close(task%context)
        call nng_aio_free(task%async%context)
    end subroutine dm_ipc_async_destroy

    subroutine dm_ipc_async_get_message(task, message, error)
        use :: nng, only: nng_aio_get_msg

        type(ipc_async_task_type), intent(inout)         :: task    !! IPC async task.
        type(ipc_message_type),    intent(out), optional :: message !! IPC message.
        integer,                   intent(out), optional :: error   !! Error code.

        integer :: rc

        rc = E_NULL

        if (present(message)) then
            message%context = nng_aio_get_msg(task%async%context)
            if (c_associated(message%context)) rc = E_NONE
        else
            task%message%context = nng_aio_get_msg(task%async%context)
            if (c_associated(task%message%context)) rc = E_NONE
        end if

        if (present(error)) error = rc
    end subroutine dm_ipc_async_get_message

    subroutine dm_ipc_async_msleep(async, msec)
        use :: nng, only: c_uint32_t, nng_sleep_aio

        type(ipc_async_type), intent(inout) :: async !! IPC async context.
        integer,              intent(in)    :: msec  !! Delay [msec].

        call nng_sleep_aio(int(msec, c_uint32_t), async%context)
    end subroutine dm_ipc_async_msleep

    recursive subroutine dm_ipc_async_receive(task)
        use :: nng, only: nng_ctx_recv

        type(ipc_async_task_type), intent(inout) :: task !! IPC async task.

        call nng_ctx_recv(task%context%context, task%async%context)
    end subroutine dm_ipc_async_receive

    subroutine dm_ipc_async_send(task)
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
        use :: nng, only: nng_ctx_send

        type(ipc_async_task_type), intent(inout) :: task !! IPC async task.

        call nng_ctx_send(task%context%context, task%async%context)
    end subroutine dm_ipc_async_send

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

    pure elemental subroutine dm_ipc_async_set_id(task, id)
        type(ipc_async_task_type), intent(inout) :: task !! IPC async task.
        integer,                   intent(in)    :: id   !! IPC async task id.

        task%id = id
    end subroutine dm_ipc_async_set_id

    pure elemental subroutine dm_ipc_async_set_state(task, state, error)
        type(ipc_async_task_type), intent(inout)         :: task  !! IPC async task.
        integer,                   intent(in)            :: state !! IPC async task state.
        integer,                   intent(out), optional :: error !! Error code.

        if (.not. dm_ipc_async_task_state_is_valid(state)) then
            if (present(error)) error = E_INVALID
            return
        end if

        task%state = state
        if (present(error)) error = E_NONE
    end subroutine dm_ipc_async_set_state

    subroutine dm_ipc_async_wait(task)
        use :: nng, only: nng_aio_wait

        type(ipc_async_task_type), intent(inout) :: task  !! IPC async task.

        call nng_aio_wait(task%async%context)
    end subroutine dm_ipc_async_wait
end module dm_ipc_async
