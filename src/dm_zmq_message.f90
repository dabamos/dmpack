! Author:  Philipp Engel
! Licence: ISC
module dm_zmq_message
    !! Module for sending and receiving derived types in MessagePack format via
    !! ZeroMQ.
    !!
    !! ## Examples
    !!
    !! Serialise and send an observation:
    !!
    !! ``` fortran
    !! integer                :: rc
    !! type(buffer_type)      :: buffer
    !! type(observ_type)      :: observ
    !! type(zmq_message_type) :: message
    !!
    !! ! Serialise observation in MessagePack format.
    !! call dm_buffer_init(buffer, 1024_i8)
    !! call dm_msgpack_pack_message(buffer, dm_message_header_observ(from='dmdummy1', to='dmdummy2'), observ)
    !!
    !! ! Send MessagePack bytes to ZeroMQ socket.
    !! rc = dm_zmq_message_create(message, buffer, free=.true.)
    !! rc = dm_zmq_message_send(message, socket)
    !! rc = dm_zmq_message_destroy(message)
    !! ```
    !!
    !! Deserialise the received message:
    !!
    !! ``` fortran
    !! integer                   :: rc
    !! type(buffer_type)         :: buffer
    !! type(message_header_type) :: header
    !! type(observ_type)         :: observ
    !! type(zmq_message_type)    :: message
    !!
    !! rc = dm_zmq_message_create(message)
    !! rc = dm_zmq_message_receive(message, socket)
    !! rc = dm_zmq_message_data(message, buffer)
    !! rc = dm_zmq_message_destroy(message)
    !!
    !! call dm_msgpack_unpack_message(buffer, header, observ)
    !! call dm_buffer_destroy(buffer)
    !! ```
    use :: dm_buffer
    use :: dm_c
    use :: dm_error
    use :: dm_kind
    use :: dm_zmq
    use :: zmq
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PUBLIC DERIVED TYPES
    ! **************************************************************************
    type, public :: zmq_message_type
        !! ZeroMQ message context.
        type(zmq_msg_t) :: context = zmq_msg_t()
    end type zmq_message_type

    ! **************************************************************************
    ! PUBLIC ABSTRACT INTERFACES
    ! **************************************************************************
    public :: dm_zmq_message_free_callback

    abstract interface
        ! void zmq_free_fn(void *data, void *hint)
        subroutine dm_zmq_message_free_callback(data, hint) bind(c)
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: data
            type(c_ptr), intent(in), value :: hint
        end subroutine dm_zmq_message_free_callback
    end interface

    ! **************************************************************************
    ! PUBLIC INTERFACES
    ! **************************************************************************
    public :: dm_zmq_message_create
    public :: dm_zmq_message_data

    interface dm_zmq_message_create
        module procedure :: zmq_message_create_buffer
        module procedure :: zmq_message_create_size
    end interface dm_zmq_message_create

    interface dm_zmq_message_data
        module procedure :: zmq_message_data_buffer
        module procedure :: zmq_message_data_bytes
    end interface dm_zmq_message_data

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_zmq_message_destroy
    public :: dm_zmq_message_free_buffer
    public :: dm_zmq_message_has_more
    public :: dm_zmq_message_receive
    public :: dm_zmq_message_send
    public :: dm_zmq_message_size

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    private :: zmq_message_data_buffer
    private :: zmq_message_data_bytes
    private :: zmq_message_create_buffer
    private :: zmq_message_create_size
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    integer function dm_zmq_message_destroy(message) result(rc)
        !! Closes ZeroMQ message and releases resources. The function returns
        !! `E_CORRUPT` on error.
        type(zmq_message_type), intent(inout) :: message !! ZeroMQ message.

        rc = E_NONE
        if (zmq_msg_close(message%context) < 0) rc = dm_zmq_error()
    end function dm_zmq_message_destroy

    logical function dm_zmq_message_has_more(message) result(has)
        !! Returns `.true.` if there are more message frames to follow after
        !! `message`.
        type(zmq_message_type), intent(inout) :: message !! ZeroMQ message.

        has = zmq_msg_more(message%context)
    end function dm_zmq_message_has_more

    integer function dm_zmq_message_receive(message, socket, blocking, nbytes) result(rc)
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
        use :: dm_util, only: dm_present, dm_present_set

        type(zmq_message_type), intent(inout)         :: message  !! ZeroMQ message.
        type(zmq_socket_type),  intent(inout)         :: socket   !! ZeroMQ socket.
        logical,                intent(in),  optional :: blocking !! Blocking mode (default: `.true.`).
        integer,                intent(out), optional :: nbytes   !! Number of bytes received.

        integer :: flags, nbytes_

        flags = 0
        if (.not. dm_present(blocking, .true.)) flags = ior(flags, ZMQ_DONTWAIT)

        zmq_block: block
            rc = E_NULL
            if (.not. c_associated(socket%context)) exit zmq_block

            rc = E_NONE
            nbytes_ = zmq_msg_recv(message%context, socket%context, flags)
            if (nbytes_ >= 0) exit zmq_block

            rc = dm_zmq_error()
            nbytes_ = 0
        end block zmq_block

        call dm_present_set(nbytes, nbytes_)
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
        use :: dm_util, only: dm_present

        type(zmq_message_type), intent(inout)        :: message  !! ZeroMQ message.
        type(zmq_socket_type),  intent(inout)        :: socket   !! ZeroMQ socket.
        logical,                intent(in), optional :: blocking !! Blocking mode (default: `.true.`).
        logical,                intent(in), optional :: more     !! Send more (default: `.false.`).

        integer :: flags

        flags = 0
        if (.not. dm_present(blocking, .true.)) flags = ior(flags, ZMQ_DONTWAIT)
        if (dm_present(more, .false.))          flags = ior(flags, ZMQ_SNDMORE)

        rc = E_NULL
        if (.not. c_associated(socket%context)) return

        rc = E_NONE
        if (zmq_msg_send(message%context, socket%context, flags) < 0) rc = dm_zmq_error()
    end function dm_zmq_message_send

    integer(i8) function dm_zmq_message_size(message) result(nbytes)
        type(zmq_message_type), intent(inout) :: message !! ZeroMQ message.

        nbytes = zmq_msg_size(message%context)
    end function dm_zmq_message_size

    ! **************************************************************************
    ! PUBLIC CALLBACKS
    ! **************************************************************************
    subroutine dm_zmq_message_free_buffer(data, hint) bind(c)
        !! Do not call this subroutine directly!
        type(c_ptr), intent(in), value :: data !! `void *`
        type(c_ptr), intent(in), value :: hint !! `void *`

        type(buffer_type), pointer :: buffer

        if (.not. c_associated(hint)) return
        call c_f_pointer(hint, buffer)
        call dm_buffer_destroy(buffer)
    end subroutine dm_zmq_message_free_buffer

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    integer function zmq_message_create_buffer(message, buffer, nbytes, free) result(rc)
        !! Creates new ZeroMQ message of given buffer. If `free` is `.true.` the
        !! buffer is destroyed after the message has been sent. In this case, do
        !! not access the buffer after passing it to this function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if argument `nbyte` is invalid.
        !! * `E_MEMORY` if insufficient storage space is available.
        !!
        use :: dm_util, only: dm_present

        type(zmq_message_type), intent(inout)        :: message !! ZeroMQ message.
        type(buffer_type),      intent(inout)        :: buffer  !! Message data.
        integer(i8),            intent(in), optional :: nbytes  !! Number of bytes.
        logical,                intent(in), optional :: free    !! Free buffer once done.

        integer           :: zrc
        integer(c_size_t) :: nbytes_

        rc = E_NONE

        if (present(nbytes)) then
            nbytes_ = int(nbytes, c_size_t)
        else
            nbytes_ = int(dm_buffer_size(buffer), c_size_t)
        end if

        if (dm_present(free, .false.)) then
            zrc = zmq_msg_init_data(message%context, dm_buffer_bytes(buffer), nbytes_, dm_zmq_message_free_buffer, buffer)
        else
            zrc = zmq_msg_init_data(message%context, dm_buffer_bytes(buffer), nbytes_)
        end if

        if (zrc < 0) rc = dm_zmq_error()
    end function zmq_message_create_buffer

    integer function zmq_message_create_size(message, nbytes) result(rc)
        !! Creates new ZeroMQ message of optional size `nbytes`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if argument `nbyte` is invalid.
        !! * `E_MEMORY` if insufficient storage space is available.
        !!
        type(zmq_message_type), intent(inout)        :: message !! ZeroMQ message.
        integer(i8),            intent(in), optional :: nbytes  !! Message size [byte].

        rc = E_NONE

        if (present(nbytes)) then
            rc = E_INVALID
            if (nbytes < 0) return

            rc = E_NONE
            if (zmq_msg_init_size(message%context, int(nbytes, c_size_t)) < 0) rc = dm_zmq_error()
            return
        end if

        if (zmq_msg_init(message%context) < 0) rc = dm_zmq_error()
    end function zmq_message_create_size

    integer function zmq_message_data_buffer(message, buffer) result(rc)
        !! Copies message data to buffer. The function returns `E_NULL` if data
        !! pointer is not associated.
        type(zmq_message_type), intent(inout) :: message !! ZeroMQ message.
        type(buffer_type),      intent(out)   :: buffer  !! Message data.

        integer(i8) :: nbytes
        type(c_ptr) :: ptr

        nbytes = zmq_msg_size(message%context)
        ptr    = zmq_msg_data(message%context)

        rc = E_NULL
        if (.not. c_associated(ptr)) return
        call dm_buffer_init(buffer, ptr, nbytes, error=rc)
    end function zmq_message_data_buffer

    integer function zmq_message_data_bytes(message, bytes) result(rc)
        !! Copies message data to allocatable character string. The function
        !! returns `E_NULL` if data pointer is not associated.
        type(zmq_message_type),    intent(inout) :: message !! ZeroMQ message.
        character(:), allocatable, intent(out)   :: bytes   !! Message data.

        integer(i8) :: nbytes
        type(c_ptr) :: ptr

        nbytes = zmq_msg_size(message%context)
        ptr    = zmq_msg_data(message%context)

        rc = E_NULL
        if (.not. c_associated(ptr)) return

        rc = E_NONE
        call dm_c_f_string_pointer(ptr, bytes, nbytes)
    end function zmq_message_data_bytes
end module dm_zmq_message
