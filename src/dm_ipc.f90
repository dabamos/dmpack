! Author:  Philipp Engel
! Licence: ISC
module dm_ipc
    !! Abstraction layer over NNG sockets.
    use :: nng
    use :: dm_c
    use :: dm_error
    use :: dm_ipc_type
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: IPC_TIMEOUT_DEFAULT  = NNG_DURATION_DEFAULT  !! Default timeout.
    integer, parameter, public :: IPC_TIMEOUT_INFINITE = NNG_DURATION_INFINITE !! Infinite timeout.
    integer, parameter, public :: IPC_TIMEOUT_ZERO     = NNG_DURATION_ZERO     !! No timeout.

    type, public :: ipc_context_type
        !! IPC context type.
        integer       :: error_nng = 0         !! Last NNG return code.
        type(nng_ctx) :: context   = nng_ctx() !! NNG context.
    end type ipc_context_type

    type, public :: ipc_socket_type
        !! IPC socket type.
        integer            :: error_nng = 0              !! Last NNG return code.
        type(nng_dialer)   :: dialer    = nng_dialer()   !! NNG dialer.
        type(nng_listener) :: listener  = nng_listener() !! NNG listener.
        type(nng_socket)   :: socket    = nng_socket()   !! NNG socket.
    end type ipc_socket_type

    interface dm_ipc_close
        module procedure :: dm_ipc_context_close
        module procedure :: dm_ipc_socket_close
    end interface dm_ipc_close

    interface dm_ipc_last_error
        module procedure :: dm_ipc_context_last_error
        module procedure :: dm_ipc_socket_last_error
    end interface dm_ipc_last_error

    public :: dm_ipc_close
    public :: dm_ipc_context_open
    public :: dm_ipc_context_close
    public :: dm_ipc_context_last_error
    public :: dm_ipc_dial
    public :: dm_ipc_error
    public :: dm_ipc_error_message
    public :: dm_ipc_init
    public :: dm_ipc_last_error
    public :: dm_ipc_listen
    public :: dm_ipc_open_pair
    public :: dm_ipc_open_pull
    public :: dm_ipc_open_push
    public :: dm_ipc_open_reply
    public :: dm_ipc_open_request
    public :: dm_ipc_receive
    public :: dm_ipc_send
    public :: dm_ipc_set_max_message_size
    public :: dm_ipc_set_receive_timeout
    public :: dm_ipc_set_send_timeout
    public :: dm_ipc_shutdown
    public :: dm_ipc_socket_close
    public :: dm_ipc_socket_last_error
    public :: dm_ipc_version
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ipc_context_last_error(context) result(error)
        !! Returns last NNG return code from context. Pass the code to
        !! `dm_ipc_error()` to convert it to a DMPACK return code.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        error = context%error_nng
    end function dm_ipc_context_last_error

    integer function dm_ipc_context_open(context, socket) result(rc)
        !! Opens socket context.
        type(ipc_context_type), intent(inout) :: context !! IPC context.
        type(ipc_socket_type),  intent(inout) :: socket  !! IPC socket.

        context%error_nng = nng_ctx_open(context%context, socket%socket)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_context_open

    integer function dm_ipc_dial(socket, url, async) result(rc)
        !! Creates and starts dialer for given URL.
        !!
        !! The argument `url` may be of the form:
        !!
        !! * `ipc:///tmp/socket.ipc`
        !! * `tcp://127.0.0.1:3327`
        !!
        use :: dm_util, only: dm_present

        type(ipc_socket_type), intent(inout)        :: socket !! IPC socket.
        character(*),          intent(in)           :: url    !! URL.
        logical,               intent(in), optional :: async  !! Run asynchronously (non-blocking).

        integer(c_int) :: flags

        flags = 0
        if (dm_present(async, .false.)) flags = NNG_FLAG_NONBLOCK

        socket%error_nng = nng_dial(socket%socket, dm_f_c_string(url), socket%dialer, flags)
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_dial

    pure elemental integer function dm_ipc_error(error_nng) result(rc)
        !! Returns DMPACK error code associated with NNG return code. The
        !! default error code is `E_NNG`.
        integer, intent(in) :: error_nng !! NNG return code.

        select case (error_nng)
            case (0);                rc = E_NONE
            case (NNG_EINTR);        rc = E_IPC_INTERRUPTED
            case (NNG_ENOMEM);       rc = E_MEMORY
            case (NNG_EINVAL);       rc = E_INVALID
            case (NNG_EBUSY);        rc = E_IPC_BUSY
            case (NNG_ETIMEDOUT);    rc = E_IPC_TIMEOUT
            case (NNG_ECONNREFUSED); rc = E_IPC_REFUSED
            case (NNG_ECLOSED);      rc = E_IPC_CLOSED
            case (NNG_EAGAIN);       rc = E_IPC_AGAIN
            case (NNG_ENOTSUP);      rc = E_IPC_NOT_SUPPORTED
            case (NNG_EADDRINUSE);   rc = E_IPC_IN_USE
            case (NNG_ESTATE);       rc = E_IPC_STATE
            case (NNG_ENOENT);       rc = E_NNG
            case (NNG_EPROTO);       rc = E_IPC_PROTOCOL
            case (NNG_EUNREACHABLE); rc = E_IPC_UNREACHABLE
            case (NNG_EADDRINVAL);   rc = E_INVALID
            case (NNG_EPERM);        rc = E_ACCESS
            case (NNG_EMSGSIZE);     rc = E_IPC_SIZE
            case (NNG_ECONNABORTED); rc = E_IPC_ABORTED
            case (NNG_ECONNRESET);   rc = E_IPC_RESET
            case (NNG_ECANCELED);    rc = E_IPC_CANCELED
            case (NNG_ENOFILES);     rc = E_LIMIT
            case (NNG_ENOSPC);       rc = E_FULL
            case (NNG_EEXIST);       rc = E_IPC_EXIST
            case (NNG_EREADONLY);    rc = E_READ_ONLY
            case (NNG_EWRITEONLY);   rc = E_WRITE_ONLY
            case (NNG_ECRYPTO);      rc = E_CRYPTO
            case (NNG_EPEERAUTH);    rc = E_AUTH
            case (NNG_ENOARG);       rc = E_INCOMPLETE
            case (NNG_EAMBIGUOUS);   rc = E_AMBIGUOUS
            case (NNG_EBADTYPE);     rc = E_TYPE
            case (NNG_ECONNSHUT);    rc = E_NNG
            case (NNG_EINTERNAL);    rc = E_NNG
            case (NNG_ESYSERR);      rc = E_SYSTEM
            case (NNG_ETRANERR);     rc = E_NNG
            case default;            rc = E_ERROR
        end select
    end function dm_ipc_error

    function dm_ipc_error_message(socket) result(message)
        !! Returns NNG error message associated with last status code.
        type(ipc_socket_type), intent(inout) :: socket  !! IPC socket.
        character(:), allocatable            :: message !! NNG error message.

        message = nng_strerror(socket%error_nng)
    end function dm_ipc_error_message

    integer function dm_ipc_init() result(rc)
        !! Initialises NNG v2.
        rc = E_NONE
        ! rc = dm_ipc_error(nng_init())
    end function dm_ipc_init

    integer function dm_ipc_socket_last_error(socket) result(error)
        !! Returns last NNG return code from socket. Pass the code to
        !! `dm_ipc_error()` to convert it to a DMPACK return code.
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.

        error = socket%error_nng
    end function dm_ipc_socket_last_error

    integer function dm_ipc_listen(socket, url) result(rc)
        !! Creates and starts listener for URL.
        !!
        !! The argument `url` may be of the form:
        !!
        !! * `ipc:///tmp/socket.ipc`
        !! * `tcp://127.0.0.1:3327`
        !!
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.
        character(*),          intent(in)    :: url    !! URL.

        socket%error_nng = nng_listen(socket%socket, dm_f_c_string(url), socket%listener, 0)
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_listen

    integer function dm_ipc_open_pair(socket) result(rc)
        !! Opens pair socket (to pair socket).
        use :: nng_pair0
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.

        socket%error_nng = nng_pair0_open(socket%socket)
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_open_pair

    integer function dm_ipc_open_pull(socket) result(rc)
        !! Opens pipeline pull socket (to push socket).
        use :: nng_pipeline0
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.

        socket%error_nng = nng_pull0_open(socket%socket)
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_open_pull

    integer function dm_ipc_open_push(socket) result(rc)
        !! Opens pipeline push socket (to pull socket).
        use :: nng_pipeline0
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.

        socket%error_nng = nng_push0_open(socket%socket)
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_open_push

    integer function dm_ipc_open_request(socket) result(rc)
        !! Opens request socket (to reply socket).
        use :: nng_reqrep0
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.

        socket%error_nng = nng_req0_open(socket%socket)
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_open_request

    integer function dm_ipc_open_reply(socket) result(rc)
        !! Opens reply socket (to request socket).
        use :: nng_reqrep0
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.

        socket%error_nng = nng_rep0_open(socket%socket)
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_open_reply

    integer function dm_ipc_receive(socket, bytes, nbyte, timeout) result(rc)
        !! Receives data from socket.
        type(ipc_socket_type), intent(inout)         :: socket  !! IPC socket.
        character(*), target,  intent(inout)         :: bytes   !! Received bytes.
        integer(i8),           intent(out), optional :: nbyte   !! Number of received bytes.
        integer,               intent(in),  optional :: timeout !! Timeout [msec].

        integer(c_size_t) :: sz

        if (present(nbyte)) nbyte = 0

        if (present(timeout)) then
            rc = dm_ipc_set_receive_timeout(socket, timeout)
            if (dm_is_error(rc)) return
        end if

        sz = len(bytes, c_size_t)
        socket%error_nng = nng_recv(socket%socket, c_loc(bytes), sz, 0)
        rc = dm_ipc_error(socket%error_nng)
        if (present(nbyte)) nbyte = sz
    end function dm_ipc_receive

    integer function dm_ipc_send(socket, bytes, nbyte, timeout, async) result(rc)
        !! Sends data in `bytes` to socket.
        use :: dm_util, only: dm_present

        character(*), target,  intent(inout)        :: bytes   !! Bytes to send.
        type(ipc_socket_type), intent(inout)        :: socket  !! IPC socket.
        integer(i8),           intent(in), optional :: nbyte   !! Number of bytes to send.
        integer,               intent(in), optional :: timeout !! Timeout [msec].
        logical,               intent(in), optional :: async   !! Run asynchronously (non-blocking).

        integer(c_int)    :: flags
        integer(c_size_t) :: sz

        if (present(nbyte)) then
            sz = int(nbyte, c_size_t)
        else
            sz = len(bytes, c_size_t)
        end if

        if (present(timeout)) then
            rc = dm_ipc_set_send_timeout(socket, timeout)
            if (dm_is_error(rc)) return
        end if

        flags = 0
        if (dm_present(async, .false.)) flags = NNG_FLAG_NONBLOCK

        socket%error_nng = nng_send(socket%socket, c_loc(bytes), sz, flags)
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_send

    integer function dm_ipc_set_max_message_size(socket, size) result(rc)
        !!  Sets the maximum message size that the will be accepted from a
        !! remote peer. If a peer attempts to send a message larger than this,
        !! then the message will be discarded. If the value of this is zero,
        !! then no limit on message sizes is enforced.
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.
        integer,               intent(in)    :: size   !! Max. size [byte].

        socket%error_nng = nng_socket_set_size(socket%socket, NNG_OPT_RECVMAXSZ, int(size, c_size_t))
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_set_max_message_size

    integer function dm_ipc_set_receive_timeout(socket, msec) result(rc)
        !! Sets receive timeout of socket.
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.
        integer,               intent(in)    :: msec   !! Timeout [msec].

        socket%error_nng = nng_socket_set_ms(socket%socket, NNG_OPT_RECVTIMEO, msec)
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_set_receive_timeout

    integer function dm_ipc_set_send_timeout(socket, msec) result(rc)
        !! Sets send timeout of socket.
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.
        integer,               intent(in)    :: msec   !! Timeout [msec].

        socket%error_nng = nng_socket_set_ms(socket%socket, NNG_OPT_SENDTIMEO, msec)
        rc = dm_ipc_error(socket%error_nng)
    end function dm_ipc_set_send_timeout

    function dm_ipc_version(name) result(version)
        !! Returns NNG library version as allocatable string.
        use :: dm_util, only: dm_present

        logical, intent(in), optional :: name    !! Add prefix `nng/'.
        character(:), allocatable     :: version !! Version string.

        if (dm_present(name, .false.)) then
            version = 'nng/' // nng_version()
        else
            version = nng_version()
        end if
    end function dm_ipc_version

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    impure elemental subroutine dm_ipc_context_close(context)
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        context%error_nng = nng_ctx_close(context%context)
    end subroutine dm_ipc_context_close

    impure elemental subroutine dm_ipc_socket_close(socket)
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.

        socket%error_nng = nng_dialer_close(socket%dialer)
        socket%error_nng = nng_listener_close(socket%listener)
        socket%error_nng = nng_socket_close(socket%socket)
    end subroutine dm_ipc_socket_close

    subroutine dm_ipc_shutdown()
        call nng_fini()
    end subroutine dm_ipc_shutdown
end module dm_ipc
