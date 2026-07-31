! Author:  Philipp Engel
! Licence: ISC
module dm_ipc
    !! Thin abstraction layer over ZeroMQ.
    use :: dm_error
    use :: dm_kind
    use :: zmq
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PUBLIC PARAMETERS
    ! **************************************************************************
    integer, parameter, public :: IPC_ADDRESS_LEN = 256

    ! **************************************************************************
    ! PUBLIC DERIVED TYPES
    ! **************************************************************************
    type, public :: ipc_context_type
        !! IPC context type.
        type(c_ptr) :: context = c_null_ptr
    end type ipc_context_type

    type, public :: ipc_socket_type
        !! IPC socket type.
        character(IPC_ADDRESS_LEN) :: address = ' '
        type(c_ptr)                :: context = c_null_ptr
    end type ipc_socket_type

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_ipc_context_create
    public :: dm_ipc_context_destroy
    public :: dm_ipc_context_set_max_sockets
    public :: dm_ipc_context_set_max_threads
    public :: dm_ipc_error
    public :: dm_ipc_error_message
    public :: dm_ipc_is_valid_address
    public :: dm_ipc_sleep
    public :: dm_ipc_socket_address
    public :: dm_ipc_socket_bind
    public :: dm_ipc_socket_close
    public :: dm_ipc_socket_connect
    public :: dm_ipc_socket_open_dealer
    public :: dm_ipc_socket_open_pair
    public :: dm_ipc_socket_open_pub
    public :: dm_ipc_socket_open_pull
    public :: dm_ipc_socket_open_push
    public :: dm_ipc_socket_open_rep
    public :: dm_ipc_socket_open_req
    public :: dm_ipc_socket_open_router
    public :: dm_ipc_socket_open_stream
    public :: dm_ipc_socket_open_sub
    public :: dm_ipc_socket_open_xpub
    public :: dm_ipc_socket_open_xsub
    public :: dm_ipc_socket_proxy
    public :: dm_ipc_socket_receive
    public :: dm_ipc_socket_send
    public :: dm_ipc_socket_subscribe
    public :: dm_ipc_socket_unsubscribe
    public :: dm_ipc_version

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    private :: ipc_socket_open
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    integer function dm_ipc_context_create(context) result(rc)
        !! Creates ZeroMQ context.
        type(ipc_context_type), intent(out) :: context !! IPC context.

        rc = E_IPC
        context%context = zmq_ctx_new()
        if (c_associated(context%context)) rc = E_NONE
    end function dm_ipc_context_create

    subroutine dm_ipc_context_destroy(context, error)
        !! Destroys ZeroMQ context.
        use :: dm_util, only: dm_present_set

        type(ipc_context_type), intent(inout)         :: context !! IPC context.
        integer,                intent(out), optional :: error   !! Error code.

        integer :: rc

        rc = E_NONE
        if (zmq_ctx_destroy(context%context) < 0) rc = dm_ipc_error()
        call dm_present_set(error, rc)
    end subroutine dm_ipc_context_destroy

    integer function dm_ipc_context_set_max_sockets(context, n) result(rc)
        !! Sets maximum number of sockets to use.
        type(ipc_context_type), intent(inout) :: context !! IPC context.
        integer,                intent(in)    :: n       !! Number of sockets.

        rc = E_INVALID
        if (n < 1) return

        rc = E_NONE
        if (zmq_ctx_set(context%context, ZMQ_MAX_SOCKETS, n) < 0) rc = dm_ipc_error()
    end function dm_ipc_context_set_max_sockets

    integer function dm_ipc_context_set_max_threads(context, n) result(rc)
        !! Sets maximum number of I/O threads to create.
        type(ipc_context_type), intent(inout) :: context !! IPC context.
        integer,                intent(in)    :: n       !! Number of I/O threads.

        rc = E_INVALID
        if (n < 1) return

        rc = E_NONE
        if (zmq_ctx_set(context%context, ZMQ_IO_THREADS, n) < 0) rc = dm_ipc_error()
    end function dm_ipc_context_set_max_threads

    integer function dm_ipc_error(zmq_error) result(rc)
        !! Returns DMPACK error code of last ZeroMQ error and optionally the ZMQ
        !! error in `zmq_error`.
        use :: unix, only: EAGAIN, EFAULT, EINTR, EINVAL, EMFILE, ENOMEM

        integer, intent(out), optional :: zmq_error !! IPC error number.

        integer :: error

        error = zmq_errno()
        if (present(zmq_error)) zmq_error = error

        select case (error)
            case (0);               rc = E_NONE
            ! POSIX:
            case (EINTR);           rc = E_INTERRUPT
            case (EAGAIN);          rc = E_AGAIN
            case (ENOMEM);          rc = E_MEMORY
            case (EFAULT);          rc = E_CORRUPT
            case (EINVAL);          rc = E_INVALID
            case (EMFILE);          rc = E_LIMIT
            ! ZeroMQ:
            case (ENOTSUP);         rc = E_IPC_NOT_SUPPORTED
            case (EPROTONOSUPPORT); rc = E_IPC_PROTOCOL
            case (ENETDOWN);        rc = E_IPC_UNREACHABLE
            case (EADDRINUSE);      rc = E_IPC_IN_USE
            case (EADDRNOTAVAIL);   rc = E_IPC_NOT_AVAILABLE
            case (ECONNREFUSED);    rc = E_IPC_REFUSED
            case (EINPROGRESS);     rc = E_IPC_EXIST
            case (ENOTSOCK);        rc = E_IPC_CLOSED
            case (EMSGSIZE);        rc = E_IPC_SIZE
            case (EAFNOSUPPORT);    rc = E_IPC_NOT_SUPPORTED
            case (ENETUNREACH);     rc = E_IPC_UNREACHABLE
            case (ECONNABORTED);    rc = E_IPC_ABORTED
            case (ECONNRESET);      rc = E_IPC_RESET
            case (ENOTCONN);        rc = E_IPC_CLOSED
            case (ETIMEDOUT);       rc = E_IPC_TIMEOUT
            case (EHOSTUNREACH);    rc = E_IPC_UNREACHABLE
            case (ENETRESET);       rc = E_IPC_RESET
            case (EFSM);            rc = E_IPC_STATE
            case (ENOCOMPATPROTO);  rc = E_IPC_PROTOCOL
            case (ETERM);           rc = E_IPC_CLOSED
            case default;           rc = E_IPC
        end select
    end function dm_ipc_error

    function dm_ipc_error_message(zmq_error) result(message)
        !! Returns error message of last ZeroMQ error or `zmq_error` if passed.
        integer, intent(in), optional :: zmq_error !! IPC error number.
        character(:), allocatable     :: message   !! Error message.

        if (present(zmq_error)) then
            message = zmq_strerror(zmq_error)
        else
            message = zmq_strerror(zmq_errno())
        end if
    end function dm_ipc_error_message

    logical function dm_ipc_is_valid_address(address) result(valid)
        !! Returns `.true.` if passed address is (more or less) a valid ZeroMQ
        !! socket address. Uses POSIX regular expressions (extended syntax) for
        !! matching, which is why the result may be wrong.
        use :: dm_posix_regex

        character(*), parameter :: PATTERN = &
            '^(inproc://[^:[:space:]]+:[1-9][0-9]*|ipc:///[^[:space:]]+|tcp://((25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])' // &
            '(\.(25[0-5]|2[0-4][0-9]|1?[0-9]?[0-9])){3}|\*|[[:alnum:].-]+):[1-9][0-9]*)$'

        character(*), intent(in) :: address !! IPC socket address.

        integer                :: rc
        type(posix_regex_type) :: regex

        rc = dm_posix_regex_create(regex, PATTERN, extended=.true.)

        if (dm_is_error(rc)) then
            valid = .false.
        else
            valid = dm_posix_regex_match(regex, trim(address))
        end if

        call dm_posix_regex_destroy(regex)
    end function dm_ipc_is_valid_address

    subroutine dm_ipc_sleep(sec)
        !! Pauses program execution for given time in seconds.
        integer, intent(in) :: sec !! Delay [sec].

        call zmq_sleep(sec)
    end subroutine dm_ipc_sleep

    function dm_ipc_socket_address(socket) result(address)
        type(ipc_socket_type), intent(inout) :: socket  !! IPC socket.
        character(:), allocatable            :: address !! Address.

        address = trim(socket%address)
    end function dm_ipc_socket_address

    integer function dm_ipc_socket_bind(socket, address) result(rc)
        !! Binds socket to given address.
        type(ipc_socket_type), intent(inout) :: socket  !! IPC socket.
        character(*),          intent(in)    :: address !! Address.

        if (zmq_bind(socket%context, address) < 0) then
            rc = dm_ipc_error()
            return
        end if

        rc = E_NONE
        socket%address = address
    end function dm_ipc_socket_bind

    subroutine dm_ipc_socket_close(socket, error)
        !! Closes ZeroMQ socket.
        use :: dm_c,    only: c_associated
        use :: dm_util, only: dm_present_set

        type(ipc_socket_type), intent(inout)         :: socket !! IPC socket.
        integer,               intent(out), optional :: error   !! Error code.

        integer :: rc

        rc = E_NONE

        ipc_block: block
            if (.not. c_associated(socket%context)) exit ipc_block
            if (zmq_close(socket%context) < 0) rc = dm_ipc_error()
        end block ipc_block

        call dm_present_set(error, rc)
    end subroutine dm_ipc_socket_close

    integer function dm_ipc_socket_connect(socket, address) result(rc)
        !! Connects socket to address and accepts incoming connections on that
        !! address.
        type(ipc_socket_type), intent(inout) :: socket  !! IPC socket.
        character(*),          intent(in)    :: address !! Address.

        if (zmq_connect(socket%context, address) < 0) then
            rc = dm_ipc_error()
            return
        end if

        rc = E_NONE
        socket%address = address
    end function dm_ipc_socket_connect

    integer function dm_ipc_socket_open_dealer(socket, context) result(rc)
        !! Opens `DEALER` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_DEALER)
    end function dm_ipc_socket_open_dealer

    integer function dm_ipc_socket_open_pair(socket, context) result(rc)
        !! Opens `PAIR` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_PAIR)
    end function dm_ipc_socket_open_pair

    integer function dm_ipc_socket_open_pub(socket, context) result(rc)
        !! Opens `PUB` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_PUB)
    end function dm_ipc_socket_open_pub

    integer function dm_ipc_socket_open_pull(socket, context) result(rc)
        !! Opens `PULL` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_PULL)
    end function dm_ipc_socket_open_pull

    integer function dm_ipc_socket_open_push(socket, context) result(rc)
        !! Opens `PUSH` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_PUSH)
    end function dm_ipc_socket_open_push

    integer function dm_ipc_socket_open_req(socket, context) result(rc)
        !! Opens `REQ` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_REQ)
    end function dm_ipc_socket_open_req

    integer function dm_ipc_socket_open_rep(socket, context) result(rc)
        !! Opens `REP` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_REP)
    end function dm_ipc_socket_open_rep

    integer function dm_ipc_socket_open_router(socket, context) result(rc)
        !! Opens `ROUTER` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_ROUTER)
    end function dm_ipc_socket_open_router

    integer function dm_ipc_socket_open_sub(socket, context) result(rc)
        !! Opens `SUB` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_SUB)
    end function dm_ipc_socket_open_sub

    integer function dm_ipc_socket_open_stream(socket, context) result(rc)
        !! Opens `STREAM` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_STREAM)
    end function dm_ipc_socket_open_stream

    integer function dm_ipc_socket_open_xpub(socket, context) result(rc)
        !! Opens `XPUB` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_XPUB)
    end function dm_ipc_socket_open_xpub

    integer function dm_ipc_socket_open_xsub(socket, context) result(rc)
        !! Opens `XSUB` socket on given ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_NULL` if context is not associated.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        rc = ipc_socket_open(socket, context, ZMQ_XSUB)
    end function dm_ipc_socket_open_xsub

    integer function dm_ipc_socket_proxy(frontend, backend, capture) result(rc)
        !! Creates proxy between front-end and back-end.
        type(ipc_socket_type), intent(inout)           :: frontend !! IPC front-end socket.
        type(ipc_socket_type), intent(inout)           :: backend  !! IPC back-end socket.
        type(ipc_socket_type), intent(inout), optional :: capture  !! IPC capture socket.

        integer :: zrc

        if (present(capture)) then
            zrc = zmq_proxy(frontend%context, backend%context, capture%context)
        else
            zrc = zmq_proxy(frontend%context, backend%context, c_null_ptr)
        end if

        rc = E_NONE
        if (zrc < 0) rc = dm_ipc_error()
    end function dm_ipc_socket_proxy

    integer function dm_ipc_socket_receive(socket, bytes, nbytes, topic, blocking) result(rc)
        !! Receives data from socket. Pass argument `topic` to read a pub-sub
        !! envelope message.
        use :: dm_util, only: dm_present

        type(ipc_socket_type), intent(inout)           :: socket   !! IPC socket.
        character(*), target,  intent(inout)           :: bytes    !! Received bytes.
        integer(i8),           intent(inout), optional :: nbytes   !! Buffer size/number of bytes received.
        character(*),          intent(inout), optional :: topic    !! Topic of received pub/sub message.
        logical,               intent(in),    optional :: blocking !! Don’t wait if `.false.`.

        character(256), target :: topic_
        integer                :: flags, n
        integer(c_size_t)      :: nbytes_

        if (present(nbytes)) then
            nbytes_ = int(max(0_i8, nbytes), c_size_t)
            nbytes  = 0_i8
        else
            nbytes_ = len(bytes, c_size_t)
        end if

        flags = 0
        if (.not. dm_present(blocking, .true.)) flags = ior(flags, ZMQ_DONTWAIT)

        rc = E_NONE
        zmq_block: block
            if (present(topic)) then
                ! Topic envelope.
                topic_ = ' '
                n = zmq_recv(socket%context, c_loc(topic_), len(topic_, c_size_t), flags)
                topic = topic_
                if (n < 0) exit zmq_block
            end if

            ! Message data envelope.
            n = zmq_recv(socket%context, c_loc(bytes), nbytes_, flags)
        end block zmq_block

        if (n < 0) rc = dm_ipc_error()
        if (present(nbytes) .and. n > 0) nbytes = n
    end function dm_ipc_socket_receive

    integer function dm_ipc_socket_send(socket, bytes, nbytes, topic, blocking, more) result(rc)
        !! Sends data in `bytes` to socket. Pass a non-empty topic to send
        !! message to pub-sub message queue.
        use :: dm_string, only: dm_string_is_present
        use :: dm_util,   only: dm_present

        type(ipc_socket_type), intent(inout)        :: socket   !! IPC socket.
        character(*), target,  intent(inout)        :: bytes    !! Bytes to send.
        integer(i8),           intent(in), optional :: nbytes   !! Number of bytes to send.
        character(*), target,  intent(in), optional :: topic    !! Topic of pub/sub socket.
        logical,               intent(in), optional :: blocking !! Don’t wait if `.false.`.
        logical,               intent(in), optional :: more     !! Send more.

        integer           :: flags, n
        integer(c_size_t) :: nbytes_

        if (present(nbytes)) then
            nbytes_ = int(max(0_i8, nbytes), c_size_t)
        else
            nbytes_ = len(bytes, c_size_t)
        end if

        flags = 0
        if (.not. dm_present(blocking, .true. )) flags = ior(flags, ZMQ_DONTWAIT)
        if (      dm_present(more,     .false.)) flags = ior(flags, ZMQ_SNDMORE)

        rc = E_NONE
        zmq_block: block
            if (dm_string_is_present(topic)) then
                n = zmq_send(socket%context, c_loc(topic), len_trim(topic, c_size_t), ZMQ_SNDMORE)
                if (n < 0) exit zmq_block
            end if

            n = zmq_send(socket%context, c_loc(bytes), nbytes_, flags)
        end block zmq_block

        if (n < 0) rc = dm_ipc_error()
    end function dm_ipc_socket_send

    integer function dm_ipc_socket_subscribe(socket, topic) result(rc)
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.
        character(*), target,  intent(in)    :: topic  !! Topic to subscribe

        rc = E_NONE
        if (zmq_setsockopt(socket%context, ZMQ_SUBSCRIBE, c_loc(topic), len_trim(topic, c_size_t)) < 0) then
            rc = dm_ipc_error()
        end if
    end function dm_ipc_socket_subscribe

    integer function dm_ipc_socket_unsubscribe(socket, topic) result(rc)
        type(ipc_socket_type), intent(inout) :: socket !! IPC socket.
        character(*), target,  intent(in)    :: topic  !! Topic to unsubscribe

        rc = E_NONE
        if (zmq_setsockopt(socket%context, ZMQ_UNSUBSCRIBE, c_loc(topic), len_trim(topic, c_size_t)) < 0) then
            rc = dm_ipc_error()
        end if
    end function dm_ipc_socket_unsubscribe

    function dm_ipc_version(name) result(version)
        !! Returns ZeroMQ library version as allocatable string.
        use :: dm_util, only: dm_present

        logical, intent(in), optional :: name    !! Add prefix `libzmq/'.
        character(:), allocatable     :: version !! Version string.

        character(8) :: v
        integer      :: major, minor, patch, stat

        call zmq_version(major, minor, patch)
        write (v, '(i0, 2(".", i0))', iostat=stat) major, minor, patch

        if (dm_present(name, .false.)) then
            version = 'libzmq/' // trim(v)
        else
            version = trim(v)
        end if
    end function dm_ipc_version

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    integer function ipc_socket_open(socket, context, type) result(rc)
        !! Opens socket of given type on ZeroMQ context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if context is not associated.
        !! * `E_CORRUPT` if context is invalid.
        !! * `E_LIMIT` if limit on the total number of open sockets has been reached.
        !! * `E_IPC_CLOSED` if context was terminated.
        !!
        type(ipc_socket_type),  intent(out)   :: socket  !! IPC socket.
        type(ipc_context_type), intent(inout) :: context !! IPC context.
        integer,                intent(in)    :: type    !! IPC socket type.

        rc = E_NULL
        if (.not. c_associated(context%context)) return

        rc = E_NONE
        socket%context = zmq_socket(context%context, type)
        if (.not. c_associated(socket%context)) rc = dm_ipc_error()
    end function ipc_socket_open
end module dm_ipc
