! Author:  Philipp Engel
! Licence: ISC
module dm_zmq_message
    !! Abstraction layer over ZeroMQ messages.
    use :: zmq
    use :: dm_c
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    use :: dm_zmq
    implicit none (type, external)
    private

    type, public :: zmq_message_type
        type(zmq_msg_t) :: context
    end type zmq_message_type

    public :: dm_zmq_message_create
    public :: dm_zmq_message_destroy
    public :: dm_zmq_message_get_data
    public :: dm_zmq_message_has_more
    public :: dm_zmq_message_receive
    public :: dm_zmq_message_send
    public :: dm_zmq_message_size
contains
    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    integer function dm_zmq_message_create(message, nbytes) result(rc)
        !! Creates new ZMQ message of optional size `nbytes`.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_INVALID` if argument `nbyte` is invalid.
        !! * `E_MEMORY` if insufficient storage space is available.
        !!
        type(zmq_message_type), intent(out)          :: message !! ZMQ message.
        integer(i8),            intent(in), optional :: nbytes  !! Message size [byte].

        integer :: stat

        if (present(nbytes)) then
            rc = E_INVALID
            if (nbytes < 0) return

            rc = E_NONE
            if (zmq_msg_init_size(message%context, int(nbytes, c_size_t)) < 0) rc = dm_zmq_error()
            return
        end if

        rc = E_NONE
        stat = zmq_msg_init(message%context)
    end function dm_zmq_message_create

    integer function dm_zmq_message_destroy(message) result(rc)
        !! Closes ZMQ message and releases resources. The function return
        !! `E_CORRUPT` on error.
        type(zmq_message_type), intent(inout) :: message !! ZMQ message.

        rc = E_NONE
        if (zmq_msg_close(message%context) < 0) rc = dm_zmq_error()
    end function dm_zmq_message_destroy

    integer function dm_zmq_message_get_data(message, data) result(rc)
        !! The function return `E_NULL` if data pointer is not associated.
        type(zmq_message_type), intent(inout) :: message !! ZMQ message.
        type(c_ptr),            intent(out)   :: data    !! C pointer to data.

        rc = E_NULL
        data = zmq_msg_data(message%context)
        if (c_associated(data)) rc = E_NONE
    end function dm_zmq_message_get_data

    logical function dm_zmq_message_has_more(message) result(has)
        !! Returns `.true.` if there are more message frames to follow after
        !! `message`.
        type(zmq_message_type), intent(inout) :: message !! ZMQ message.

        has = zmq_msg_more(message%context)
    end function dm_zmq_message_has_more

    integer function dm_zmq_message_receive(message, socket, blocking) result(rc)
        !! The function receives a message part from the socket and store it in
        !! the message. Any content previously stored in the message is properly
        !! deallocated. If there are no message parts available on the specified
        !! socket the function blocks until the request can be satisfied.
        !!
        !! If argument `blocking` is `.false.`, the operation is performed in
        !! non-blocking mode. If there are no messages available on the
        !! specified socket, the function fails with error `E_ZMQ_AGAIN`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if the message is invalid.
        !! * `E_NOT_SUPPORTED` if operation is not supported by this socket type.
        !! * `E_NULL` if socket context is not associated.
        !! * `E_ZMQ_AGAIN` if non-blocking mode was requested and no messages
        !!   are available at the moment.
        !! * `E_ZMQ_CLOSED` if the context associated with the specified socket
        !!   was terminated or the provided socket was invalid.
        !! * `E_ZMQ_INTERRUPTED` if the operation was interrupted by delivery of
        !!   a signal before the message was available.
        !! * `E_ZMQ_STATE` if operation cannot be performed on this socket at
        !!   the moment due to the socket not being in the appropriate state.
        !!
        type(zmq_message_type), intent(inout)        :: message  !! ZMQ message.
        type(zmq_socket_type),  intent(inout)        :: socket   !! ZMQ socket.
        logical,                intent(in), optional :: blocking !! Blocking mode (default: `.true.`).

        integer :: flags

        flags = 0
        if (dm_present(blocking, .true.)) flags = ior(flags, ZMQ_DONTWAIT)

        rc = E_NULL
        if (.not. c_associated(socket%context)) return

        rc = E_NONE
        if (zmq_msg_recv(message%context, socket%context, flags) < 0) rc = dm_zmq_error()
    end function dm_zmq_message_receive

    integer function dm_zmq_message_send(message, socket, blocking, more) result(rc)
        !! The function queues the message to be sent to the socket (blocking).
        !!
        !! If argument `blocking` is `.false.`, the operation is performed in
        !! non-blocking mode (only for `DEALER` and `PUSH` socket types).
        !!
        !! If argument `more` is `.true.`, the message being sent is specified
        !! to be a multi-part message, and that further message parts are to
        !! follow.
        !!
        !! Sending a multi-part message:
        !!
        !! ``` fortran
        !! rc = dm_zmq_message_send(part1, socket, more=.true.)
        !! rc = dm_zmq_message_send(part2, socket, more=.true.)
        !! rc = dm_zmq_message_send(part3, socket)
        !! ```
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if the message is invalid.
        !! * `E_NOT_SUPPORTED` if operation is not supported by this socket type.
        !! * `E_NULL` if socket context is not associated.
        !! * `E_ZMQ_AGAIN` if non-blocking mode was requested and the message
        !!   cannot be sent at the moment.
        !! * `E_ZMQ_CLOSED` if the context associated with the specified socket
        !!   was terminated or the provided socket was invalid.
        !! * `E_ZMQ_INTERRUPTED` if the operation was interrupted by delivery of
        !!   a signal before the message was sent.
        !! * `E_ZMQ_UNREACHABLE` if the message cannot be routed.
        !!
        type(zmq_message_type), intent(inout)        :: message  !! ZMQ message.
        type(zmq_socket_type),  intent(inout)        :: socket   !! ZMQ socket.
        logical,                intent(in), optional :: blocking !! Blocking mode (default: `.true.`).
        logical,                intent(in), optional :: more     !! Send more (default: `.false.`).

        integer :: flags

        flags = 0
        if (dm_present(blocking, .true.))  flags = ior(flags, ZMQ_DONTWAIT)
        if (dm_present(more,     .false.)) flags = ior(flags, ZMQ_SNDMORE)

        rc = E_NULL
        if (.not. c_associated(socket%context)) return

        rc = E_NONE
        if (zmq_msg_send(message%context, socket%context, flags) < 0) rc = dm_zmq_error()
    end function dm_zmq_message_send

    integer(i8) function dm_zmq_message_size(message) result(nbytes)
        type(zmq_message_type), intent(inout) :: message !! ZMQ message.

        nbytes = zmq_msg_size(message%context)
    end function dm_zmq_message_size
end module dm_zmq_message
