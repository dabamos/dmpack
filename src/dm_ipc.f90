! Author:  Philipp Engel
! Licence: ISC
module dm_ipc
    !! Abstraction layer over NNG sockets.
    use :: nng
    use :: dm_c
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    type, public :: ipc_context_type
        !! IPC context type.
        integer            :: error_nng = 0    !! NNG return code.
        type(nng_ctx)      :: ctx              !! NNG context.
        type(nng_dialer)   :: dialer           !! NNG dialer.
        type(nng_listener) :: listener         !! NNG listener.
        type(nng_socket)   :: socket           !! NNG socket.
        type(c_ptr)        :: aio = c_null_ptr !! NNG aio.
    end type ipc_context_type

    public :: dm_ipc_dial
    public :: dm_ipc_close
    public :: dm_ipc_error
    public :: dm_ipc_error_message
    public :: dm_ipc_last_error
    public :: dm_ipc_listen
    public :: dm_ipc_open_pair
    public :: dm_ipc_open_pull
    public :: dm_ipc_open_push
    public :: dm_ipc_open_request
    public :: dm_ipc_open_response
    public :: dm_ipc_receive
    public :: dm_ipc_send
    public :: dm_ipc_set_max_message_size
    public :: dm_ipc_set_receive_timeout
    public :: dm_ipc_set_send_timeout
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ipc_dial(context, url, non_blocking) result(rc)
        !! Creates and starts dialer for URL (UNIX Domain Socket or TCP address).
        !!
        !! The argument `url` may be of the form:
        !!
        !! * `ipc:///tmp/socket.ipc`
        !! * `tcp://127.0.0.1:3327`
        !!
        use :: dm_util, only: dm_present

        type(ipc_context_type), intent(inout)        :: context      !! IPC context.
        character(*),           intent(in)           :: url          !! URL.
        logical,                intent(in), optional :: non_blocking !! Run asynchronously.

        integer(c_int) :: flags

        flags = 0
        if (dm_present(non_blocking, .false.)) flags = NNG_FLAG_NONBLOCK

        context%error_nng = nng_dial(context%socket, dm_f_c_string(url), context%dialer, flags)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_dial

    pure elemental integer function dm_ipc_error(error_nng) result(rc)
        !! Returns DMPACK error code associated with NNG return code. The
        !! default error code is `E_NNG`.
        integer, intent(in) :: error_nng !! NNG return code.

        select case (error_nng)
            case (0);                rc = E_NONE
            case (NNG_EINTR);        rc = E_NNG
            case (NNG_ENOMEM);       rc = E_MEMORY
            case (NNG_EINVAL);       rc = E_INVALID
            case (NNG_EBUSY);        rc = E_BUSY
            case (NNG_ETIMEDOUT);    rc = E_TIMEOUT
            case (NNG_ECONNREFUSED); rc = E_NNG
            case (NNG_ECLOSED);      rc = E_NNG
            case (NNG_EAGAIN);       rc = E_AGAIN
            case (NNG_ENOTSUP);      rc = E_NOT_SUPPORTED
            case (NNG_EADDRINUSE);   rc = E_NNG
            case (NNG_ESTATE);       rc = E_NNG
            case (NNG_ENOENT);       rc = E_NNG
            case (NNG_EPROTO);       rc = E_NNG
            case (NNG_EUNREACHABLE); rc = E_NNG
            case (NNG_EADDRINVAL);   rc = E_INVALID
            case (NNG_EPERM);        rc = E_PERM
            case (NNG_EMSGSIZE);     rc = E_BOUNDS
            case (NNG_ECONNABORTED); rc = E_NNG
            case (NNG_ECONNRESET);   rc = E_NNG
            case (NNG_ECANCELED);    rc = E_CANCELED
            case (NNG_ENOFILES);     rc = E_NNG
            case (NNG_ENOSPC);       rc = E_FULL
            case (NNG_EEXIST);       rc = E_EXIST
            case (NNG_EREADONLY);    rc = E_READ_ONLY
            case (NNG_EWRITEONLY);   rc = E_WRITE_ONLY
            case (NNG_ECRYPTO);      rc = E_NNG
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

    function dm_ipc_error_message(context) result(message)
        !! Returns NNG error message associated with last status code.
        type(ipc_context_type), intent(inout) :: context !! IPC context.
        character(:), allocatable             :: message !! NNG error message.

        message = nng_strerror(context%error_nng)
    end function dm_ipc_error_message

    integer function dm_ipc_last_error(context) result(error)
        !! Returns last NNG return code from context. Pass the code to
        !! `dm_ipc_error()` to convert it to a DMPACK return code.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        error = context%error_nng
    end function dm_ipc_last_error

    integer function dm_ipc_listen(context, url) result(rc)
        !! Creates and starts listener for URL (UNIX Domain Socket or TCP address).
        !!
        !! The argument `url` may be of the form:
        !!
        !! * `ipc:///tmp/socket.ipc`
        !! * `tcp://127.0.0.1:3327`
        !!
        type(ipc_context_type), intent(inout) :: context !! IPC context.
        character(*),           intent(in)    :: url     !! URL.

        context%error_nng = nng_listen(context%socket, dm_f_c_string(url), context%listener, 0)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_listen

    integer function dm_ipc_open_pair(context) result(rc)
        !! Opens pair connection (to pair connection).
        use :: nng_pair0, only: nng_pair0_open

        type(ipc_context_type), intent(inout) :: context !! IPC context.

        context%error_nng = nng_pair0_open(context%socket)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_open_pair

    integer function dm_ipc_open_pull(context) result(rc)
        !! Opens pipeline pull connection (to push connection).
        use :: nng_pipeline0, only: nng_pull0_open

        type(ipc_context_type), intent(inout) :: context !! IPC context.

        context%error_nng = nng_pull0_open(context%socket)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_open_pull

    integer function dm_ipc_open_push(context) result(rc)
        !! Opens pipeline push connection (to pull connection).
        use :: nng_pipeline0, only: nng_push0_open

        type(ipc_context_type), intent(inout) :: context !! IPC context.

        context%error_nng = nng_push0_open(context%socket)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_open_push

    integer function dm_ipc_open_request(context) result(rc)
        !! Opens request connection (to response connection).
        use :: nng_reqrep0, only: nng_req0_open

        type(ipc_context_type), intent(inout) :: context !! IPC context.

        context%error_nng = nng_req0_open(context%socket)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_open_request

    integer function dm_ipc_open_response(context) result(rc)
        !! Opens response connection (to request connection).
        use :: nng_reqrep0, only: nng_rep0_open

        type(ipc_context_type), intent(inout) :: context !! IPC context.

        context%error_nng = nng_rep0_open(context%socket)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_open_response

    integer function dm_ipc_receive(context, bytes, nbyte, timeout) result(rc)
        !! Receives data from socket.
        type(ipc_context_type), intent(inout)         :: context !! IPC context.
        character(*), target,   intent(inout)         :: bytes   !! Received bytes.
        integer(i8),            intent(out), optional :: nbyte   !! Number of received bytes.
        integer,                intent(in),  optional :: timeout !! Timeout [msec].

        integer(c_size_t) :: sz

        if (present(nbyte)) nbyte = 0

        if (present(timeout)) then
            rc = dm_ipc_set_receive_timeout(context, timeout)
            if (dm_is_error(rc)) return
        end if

        sz = len(bytes, c_size_t)
        context%error_nng = nng_recv(context%socket, c_loc(bytes), sz, 0)
        rc = dm_ipc_error(context%error_nng)
        if (present(nbyte)) nbyte = sz
    end function dm_ipc_receive

    integer function dm_ipc_send(context, bytes, nbyte, timeout, non_blocking) result(rc)
        !! Sends data `bytes` to socket.
        use :: dm_util, only: dm_present

        type(ipc_context_type), intent(inout)        :: context      !! IPC context.
        character(*), target,   intent(inout)        :: bytes        !! Bytes to send.
        integer(i8),            intent(in), optional :: nbyte        !! Number of bytes to send.
        integer,                intent(in), optional :: timeout      !! Timeout [msec].
        logical,                intent(in), optional :: non_blocking !! Run asynchronously.

        integer(c_int)    :: flags
        integer(c_size_t) :: sz

        if (present(nbyte)) then
            sz = int(nbyte, c_size_t)
        else
            sz = len(bytes, c_size_t)
        end if

        if (present(timeout)) then
            rc = dm_ipc_set_send_timeout(context, timeout)
            if (dm_is_error(rc)) return
        end if

        flags = 0
        if (dm_present(non_blocking, .false.)) flags = NNG_FLAG_NONBLOCK

        context%error_nng = nng_send(context%socket, c_loc(bytes), sz, flags)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_send

    integer function dm_ipc_set_max_message_size(context, size) result(rc)
        !!  Sets the maximum message size that the will be accepted from a
        !! remote peer. If a peer attempts to send a message larger than this,
        !! then the message will be discarded. If the value of this is zero,
        !! then no limit on message sizes is enforced.
        type(ipc_context_type), intent(inout) :: context !! IPC context.
        integer(i8),            intent(in)    :: size    !! Max. size [byte].

        context%error_nng = nng_socket_set_size(context%socket, NNG_OPT_RECVMAXSZ, int(size, c_size_t))
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_set_max_message_size

    integer function dm_ipc_set_receive_timeout(context, msec) result(rc)
        !! Sets receive timeout of socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.
        integer,                intent(in)    :: msec    !! Timeout [msec].

        context%error_nng = nng_socket_set_ms(context%socket, NNG_OPT_RECVTIMEO, msec)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_set_receive_timeout

    integer function dm_ipc_set_send_timeout(context, msec) result(rc)
        !! Sets send timeout of socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.
        integer,                intent(in)    :: msec    !! Timeout [msec].

        context%error_nng = nng_socket_set_ms(context%socket, NNG_OPT_SENDTIMEO, msec)
        rc = dm_ipc_error(context%error_nng)
    end function dm_ipc_set_send_timeout

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_ipc_close(context)
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        if (c_associated(context%aio)) call nng_aio_free(context%aio)

        context%error_nng = nng_ctx_close(context%ctx)
        context%error_nng = nng_dialer_close(context%dialer)
        context%error_nng = nng_listener_close(context%listener)
        context%error_nng = nng_socket_close(context%socket)
    end subroutine dm_ipc_close
end module dm_ipc
