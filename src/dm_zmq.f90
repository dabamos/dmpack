! Author:  Philipp Engel
! Licence: ISC
module dm_zmq
    !! Abstraction layer over ZeroMQ.
    use :: zmq
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    type, public :: zmq_context_type
        !! ZeroMQ context type.
        type(c_ptr) :: context = c_null_ptr
    end type zmq_context_type

    type, public :: zmq_socket_type
        !! ZeroMQ socket type.
        type(c_ptr) :: context = c_null_ptr
    end type zmq_socket_type

    public :: dm_zmq_context_create
    public :: dm_zmq_context_destroy
    public :: dm_zmq_context_set_max_sockets
    public :: dm_zmq_context_set_max_threads
    public :: dm_zmq_error
    public :: dm_zmq_error_message
    public :: dm_zmq_receive
    public :: dm_zmq_send
    public :: dm_zmq_sleep
    public :: dm_zmq_socket_bind
    public :: dm_zmq_socket_connect
    public :: dm_zmq_socket_open_dealer
    public :: dm_zmq_socket_open_pair
    public :: dm_zmq_socket_open_pub
    public :: dm_zmq_socket_open_pull
    public :: dm_zmq_socket_open_push
    public :: dm_zmq_socket_open_req
    public :: dm_zmq_socket_open_rep
    public :: dm_zmq_socket_open_router
    public :: dm_zmq_socket_open_sub
    public :: dm_zmq_socket_open_stream
    public :: dm_zmq_socket_open_xpub
    public :: dm_zmq_socket_open_xsub
    public :: dm_zmq_socket_close
    public :: dm_zmq_version

    private :: zmq_socket_open
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_zmq_context_create(context) result(rc)
        type(zmq_context_type), intent(out) :: context !! ZeroMQ context.

        rc = E_ZMQ
        context%context = zmq_ctx_new()
        if (c_associated(context%context)) rc = E_NONE
    end function dm_zmq_context_create

    integer function dm_zmq_context_destroy(context) result(rc)
        type(zmq_context_type), intent(inout) :: context !! ZeroMQ context.

        rc = E_NONE
        if (zmq_ctx_destroy(context%context) < 0) rc = dm_zmq_error()
    end function dm_zmq_context_destroy

    integer function dm_zmq_context_set_max_sockets(context, n) result(rc)
        type(zmq_context_type), intent(inout) :: context !! ZeroMQ context.
        integer,                intent(in)    :: n       !! Number of sockets.

        rc = E_INVALID
        if (n < 1) return

        rc = E_NONE
        if (zmq_ctx_set(context%context, ZMQ_MAX_SOCKETS, n) < 0) rc = dm_zmq_error()
    end function dm_zmq_context_set_max_sockets

    integer function dm_zmq_context_set_max_threads(context, n) result(rc)
        type(zmq_context_type), intent(inout) :: context !! ZeroMQ context.
        integer,                intent(in)    :: n       !! Number of I/O threads.

        rc = E_INVALID
        if (n < 1) return

        rc = E_NONE
        if (zmq_ctx_set(context%context, ZMQ_IO_THREADS, n) < 0) rc = dm_zmq_error()
    end function dm_zmq_context_set_max_threads

    integer function dm_zmq_error(zmq_error) result(rc)
        !! Returns DMPACK error code of last ZMQ error and optionally the ZMQ
        !! error in `zmq_error`.
        use :: unix, only: EAGAIN, EFAULT, EINTR, EINVAL, EMFILE, ENOMEM
        integer, intent(out), optional :: zmq_error !! ZMQ error number.

        integer :: error

        error = zmq_errno()
        if (present(zmq_error)) zmq_error = error

        select case (error)
            ! POSIX:
            case (0);               rc = E_NONE
            case (EINTR);           rc = E_ZMQ_INTERRUPTED
            case (EAGAIN);          rc = E_ZMQ_AGAIN
            case (ENOMEM);          rc = E_MEMORY
            case (EFAULT);          rc = E_CORRUPT
            case (EINVAL);          rc = E_INVALID
            case (EMFILE);          rc = E_LIMIT
            ! ZeroMQ:
            case (ENOTSUP);         rc = E_ZMQ_NOT_SUPPORTED
            case (EPROTONOSUPPORT); rc = E_ZMQ_PROTOCOL
            case (ENETDOWN);        rc = E_ZMQ_UNREACHABLE
            case (EADDRINUSE);      rc = E_ZMQ_IN_USE
            case (EADDRNOTAVAIL);   rc = E_ZMQ_NOT_AVAILABLE
            case (ECONNREFUSED);    rc = E_ZMQ_REFUSED
            case (EINPROGRESS);     rc = E_ZMQ_EXIST
            case (ENOTSOCK);        rc = E_ZMQ_CLOSED
            case (EMSGSIZE);        rc = E_ZMQ_SIZE
            case (EAFNOSUPPORT);    rc = E_ZMQ_NOT_SUPPORTED
            case (ENETUNREACH);     rc = E_ZMQ_UNREACHABLE
            case (ECONNABORTED);    rc = E_ZMQ_ABORTED
            case (ECONNRESET);      rc = E_ZMQ_RESET
            case (ENOTCONN);        rc = E_ZMQ_CLOSED
            case (ETIMEDOUT);       rc = E_ZMQ_TIMEOUT
            case (EHOSTUNREACH);    rc = E_ZMQ_UNREACHABLE
            case (ENETRESET);       rc = E_ZMQ_RESET
            case (EFSM);            rc = E_ZMQ_STATE
            case (ENOCOMPATPROTO);  rc = E_ZMQ_PROTOCOL
            case (ETERM);           rc = E_ZMQ_CLOSED
            case default;           rc = E_ZMQ
        end select
    end function dm_zmq_error

    function dm_zmq_error_message(zmq_error) result(message)
        !! Returns error message of last ZMQ error or `zmq_error` if passed.
        integer, intent(in), optional :: zmq_error !! ZMQ error number.
        character(:), allocatable     :: message   !! Error message.

        if (present(zmq_error)) then
            message = zmq_strerror(zmq_error)
        else
            message = zmq_strerror(zmq_errno())
        end if
    end function dm_zmq_error_message

    integer function dm_zmq_socket_bind(socket, endpoint) result(rc)
        !! Binds socket to given endpoint.
        type(zmq_socket_type), intent(inout) :: socket   !! ZMQ socket.
        character(*),          intent(in)    :: endpoint !! Endpoint.

        rc = E_NONE
        if (zmq_bind(socket%context, endpoint) < 0) rc = dm_zmq_error()
    end function dm_zmq_socket_bind

    integer function dm_zmq_socket_connect(socket, endpoint) result(rc)
        !! Connects socket to endpoint and accepts incoming connections on that
        !! endpoint.
        type(zmq_socket_type), intent(inout) :: socket   !! ZMQ socket.
        character(*),          intent(in)    :: endpoint !! Endpoint.

        rc = E_NONE
        if (zmq_connect(socket%context, endpoint) < 0) rc = dm_zmq_error()
    end function dm_zmq_socket_connect

    integer function dm_zmq_socket_open_dealer(socket, context) result(rc)
        !! Opens `DEALER` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_DEALER)
    end function dm_zmq_socket_open_dealer

    integer function dm_zmq_socket_open_pair(socket, context) result(rc)
        !! Opens `PAIR` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_PAIR)
    end function dm_zmq_socket_open_pair

    integer function dm_zmq_socket_open_pub(socket, context) result(rc)
        !! Opens `PUB` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_PUB)
    end function dm_zmq_socket_open_pub

    integer function dm_zmq_socket_open_pull(socket, context) result(rc)
        !! Opens `PULL` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_PULL)
    end function dm_zmq_socket_open_pull

    integer function dm_zmq_socket_open_push(socket, context) result(rc)
        !! Opens `PUSH` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_PUSH)
    end function dm_zmq_socket_open_push

    integer function dm_zmq_socket_open_req(socket, context) result(rc)
        !! Opens `REQ` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_REQ)
    end function dm_zmq_socket_open_req

    integer function dm_zmq_socket_open_rep(socket, context) result(rc)
        !! Opens `REP` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_REP)
    end function dm_zmq_socket_open_rep

    integer function dm_zmq_socket_open_router(socket, context) result(rc)
        !! Opens `ROUTER` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_ROUTER)
    end function dm_zmq_socket_open_router

    integer function dm_zmq_socket_open_sub(socket, context) result(rc)
        !! Opens `SUB` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_SUB)
    end function dm_zmq_socket_open_sub

    integer function dm_zmq_socket_open_stream(socket, context) result(rc)
        !! Opens `STREAM` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_STREAM)
    end function dm_zmq_socket_open_stream

    integer function dm_zmq_socket_open_xpub(socket, context) result(rc)
        !! Opens `XPUB` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_XPUB)
    end function dm_zmq_socket_open_xpub

    integer function dm_zmq_socket_open_xsub(socket, context) result(rc)
        !! Opens `XSUB` socket on given ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.

        rc = zmq_socket_open(socket, context, ZMQ_XSUB)
    end function dm_zmq_socket_open_xsub

    integer function dm_zmq_receive(socket, bytes, nbytes) result(rc)
        !! Receives data from socket.
        type(zmq_socket_type), intent(inout)         :: socket !! ZeroMQ socket.
        character(*), target,  intent(inout)         :: bytes  !! Received bytes.
        integer(i8),           intent(out), optional :: nbytes !! Number of received bytes.

    end function dm_zmq_receive

    integer function dm_zmq_send(socket, bytes, nbytes) result(rc)
        !! Sends data in `bytes` to socket.
        type(zmq_socket_type), intent(inout)        :: socket !! ZeroMQ socket.
        character(*), target,  intent(inout)        :: bytes  !! Bytes to send.
        integer(i8),           intent(in), optional :: nbytes !! Number of bytes to send.

        integer           :: flags
        integer(c_size_t) :: sz

        rc = E_NONE

        if (present(nbytes)) then
            sz = int(nbytes, c_size_t)
        else
            sz = len(bytes, c_size_t)
        end if

        flags = 0
        if (zmq_send(socket%context, c_loc(bytes), sz, flags) < 0) rc = dm_zmq_error()
    end function dm_zmq_send

    subroutine dm_zmq_sleep(sec)
        !! Pauses program execution for given time in seconds.
        integer, intent(in) :: sec !! Delay [sec].

        call zmq_sleep(sec)
    end subroutine dm_zmq_sleep

    integer function dm_zmq_socket_close(socket) result(rc)
        !! Closes ZMQ socket.
        type(zmq_socket_type), intent(inout) :: socket !! ZeroMQ socket.

        rc = E_NONE
        if (zmq_close(socket%context) < 0) rc = dm_zmq_error()
    end function dm_zmq_socket_close

    function dm_zmq_version(name) result(version)
        !! Returns ZMQ library version as allocatable string.
        use :: dm_util, only: dm_present

        logical, intent(in), optional :: name    !! Add prefix `libzmq/'.
        character(:), allocatable     :: version !! Version string.

        character(8) :: v
        integer      :: major, minor, patch

        call zmq_version(major, minor, patch)
        write (v, '(i0, 2(".", i0))') major, minor, patch

        if (dm_present(name, .false.)) then
            version = 'libzmq/' // trim(v)
        else
            version = trim(v)
        end if
    end function dm_zmq_version

    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function zmq_socket_open(socket, context, type) result(rc)
        !! Opens socket of given type on ZMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been
        !!   reached.
        !! * `E_ZMQ_CLOSED` if context was terminated.
        !!
        type(zmq_socket_type),  intent(out)   :: socket  !! ZMQ socket.
        type(zmq_context_type), intent(inout) :: context !! ZMQ context.
        integer,                intent(in)    :: type    !! ZMQ socket type.

        rc = E_NULL
        if (.not. c_associated(context%context)) return

        rc = E_NONE
        socket%context = zmq_socket(context%context, type)
        if (.not. c_associated(socket%context)) rc = dm_zmq_error()
    end function zmq_socket_open
end module dm_zmq
