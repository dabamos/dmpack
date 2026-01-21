! nng.F90
!
! Author:  Philipp Engel
! Licence: ISC
module nng
    !! Auto-generated Fortran 2018 interface bindings to nanomsg-ng (nng) v1.11.0.
    use, intrinsic :: iso_c_binding, only: c_bool, c_char, c_funptr, c_int, c_ptr, c_size_t, &
                                           c_int8_t, c_int16_t, c_int32_t, c_int64_t, &
                                           c_null_char, c_null_funptr, c_null_ptr, &
                                           c_associated, c_f_pointer, c_funloc, c_loc
#if HAS_UNSIGNED

    use, intrinsic :: iso_c_binding, only: c_uint8_t, c_uint16_t, c_uint32_t, c_uint64_t, c_unsigned

#endif
    use :: nng_util
    implicit none (type, external)
    private

    public :: c_bool
    public :: c_char
    public :: c_funptr
    public :: c_int
    public :: c_null_char
    public :: c_null_funptr
    public :: c_null_ptr
    public :: c_ptr
    public :: c_size_t

    public :: c_associated
    public :: c_f_pointer
    public :: c_funloc
    public :: c_loc

#if HAS_UNSIGNED

    public :: c_uint8_t
    public :: c_uint16_t
    public :: c_uint32_t
    public :: c_uint64_t
    public :: c_unsigned

#else

    integer, parameter, public :: c_uint8_t  = c_int8_t
    integer, parameter, public :: c_uint16_t = c_int16_t
    integer, parameter, public :: c_uint32_t = c_int32_t
    integer, parameter, public :: c_uint64_t = c_int64_t
    integer, parameter, public :: c_unsigned = c_int

#endif

    integer, parameter, public :: nng_duration       = c_int32_t
    integer, parameter, public :: nng_init_parameter = c_int
    integer, parameter, public :: nng_time           = c_uint64_t

    integer(c_int), parameter, public :: NNG_MAXADDRLEN = 128

    ! enum nng_sockaddr_family
    integer(c_int), parameter, public :: NNG_AF_UNSPEC   = 0
    integer(c_int), parameter, public :: NNG_AF_INPROC   = 1
    integer(c_int), parameter, public :: NNG_AF_IPC      = 2
    integer(c_int), parameter, public :: NNG_AF_INET     = 3
    integer(c_int), parameter, public :: NNG_AF_INET6    = 4
    integer(c_int), parameter, public :: NNG_AF_ZT       = 5 ! ZeroTier
    integer(c_int), parameter, public :: NNG_AF_ABSTRACT = 6

    integer(c_int), parameter, public :: NNG_DURATION_INFINITE = -1
    integer(c_int), parameter, public :: NNG_DURATION_DEFAULT  = -2
    integer(c_int), parameter, public :: NNG_DURATION_ZERO     = 0

    integer(c_int), parameter, public :: NNG_INIT_PARAMETER_NONE       = 0
    integer(c_int), parameter, public :: NNG_INIT_NUM_TASK_THREADS     = 1
    integer(c_int), parameter, public :: NNG_INIT_NUM_EXPIRE_THREADS   = 2
    integer(c_int), parameter, public :: NNG_INIT_NUM_POLLER_THREADS   = 3
    integer(c_int), parameter, public :: NNG_INIT_NUM_RESOLVER_THREADS = 4
    integer(c_int), parameter, public :: NNG_INIT_MAX_TASK_THREADS     = 5
    integer(c_int), parameter, public :: NNG_INIT_MAX_EXPIRE_THREADS   = 6
    integer(c_int), parameter, public :: NNG_INIT_MAX_POLLER_THREADS   = 7

    ! enum nng_pipe_ev
    integer(c_int), parameter, public :: NNG_PIPE_EV_ADD_PRE  = 0
    integer(c_int), parameter, public :: NNG_PIPE_EV_ADD_POST = 1
    integer(c_int), parameter, public :: NNG_PIPE_EV_REM_POST = 2

    ! enum nng_log_level
    integer(c_int), parameter, public :: NNG_LOG_LEVEL_NONE   = 0 ! used for filters only, NNG suppresses these
    integer(c_int), parameter, public :: NNG_LOG_LEVEL_ERR    = 3
    integer(c_int), parameter, public :: NNG_LOG_LEVEL_WARN   = 4
    integer(c_int), parameter, public :: NNG_LOG_LEVEL_NOTICE = 5
    integer(c_int), parameter, public :: NNG_LOG_LEVEL_INFO   = 6
    integer(c_int), parameter, public :: NNG_LOG_LEVEL_DEBUG  = 7

    ! enum nng_log_facility
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_USER   = 1
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_DAEMON = 3
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_AUTH   = 10 ! actually AUTHPRIV for sensitive logs
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_LOCAL0 = 16
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_LOCAL1 = 17
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_LOCAL2 = 18
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_LOCAL3 = 19
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_LOCAL4 = 20
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_LOCAL5 = 21
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_LOCAL6 = 22
    integer(c_int), parameter, public :: NNG_LOG_FACILITY_LOCAL7 = 23

    ! enum nng_errno_enum
    integer(c_int), parameter, public :: NNG_EINTR        = 1
    integer(c_int), parameter, public :: NNG_ENOMEM       = 2
    integer(c_int), parameter, public :: NNG_EINVAL       = 3
    integer(c_int), parameter, public :: NNG_EBUSY        = 4
    integer(c_int), parameter, public :: NNG_ETIMEDOUT    = 5
    integer(c_int), parameter, public :: NNG_ECONNREFUSED = 6
    integer(c_int), parameter, public :: NNG_ECLOSED      = 7
    integer(c_int), parameter, public :: NNG_EAGAIN       = 8
    integer(c_int), parameter, public :: NNG_ENOTSUP      = 9
    integer(c_int), parameter, public :: NNG_EADDRINUSE   = 10
    integer(c_int), parameter, public :: NNG_ESTATE       = 11
    integer(c_int), parameter, public :: NNG_ENOENT       = 12
    integer(c_int), parameter, public :: NNG_EPROTO       = 13
    integer(c_int), parameter, public :: NNG_EUNREACHABLE = 14
    integer(c_int), parameter, public :: NNG_EADDRINVAL   = 15
    integer(c_int), parameter, public :: NNG_EPERM        = 16
    integer(c_int), parameter, public :: NNG_EMSGSIZE     = 17
    integer(c_int), parameter, public :: NNG_ECONNABORTED = 18
    integer(c_int), parameter, public :: NNG_ECONNRESET   = 19
    integer(c_int), parameter, public :: NNG_ECANCELED    = 20
    integer(c_int), parameter, public :: NNG_ENOFILES     = 21
    integer(c_int), parameter, public :: NNG_ENOSPC       = 22
    integer(c_int), parameter, public :: NNG_EEXIST       = 23
    integer(c_int), parameter, public :: NNG_EREADONLY    = 24
    integer(c_int), parameter, public :: NNG_EWRITEONLY   = 25
    integer(c_int), parameter, public :: NNG_ECRYPTO      = 26
    integer(c_int), parameter, public :: NNG_EPEERAUTH    = 27
    integer(c_int), parameter, public :: NNG_ENOARG       = 28
    integer(c_int), parameter, public :: NNG_EAMBIGUOUS   = 29
    integer(c_int), parameter, public :: NNG_EBADTYPE     = 30
    integer(c_int), parameter, public :: NNG_ECONNSHUT    = 31
    integer(c_int), parameter, public :: NNG_EINTERNAL    = 1000
    integer(c_int), parameter, public :: NNG_ESYSERR      = int(z'10000000')
    integer(c_int), parameter, public :: NNG_ETRANERR     = int(z'20000000')

    ! enum nng_unit_enum
    integer(c_int), parameter, public :: NNG_UNIT_NONE     = 0 ! No special units
    integer(c_int), parameter, public :: NNG_UNIT_BYTES    = 1 ! Bytes, e.g. bytes sent, etc.
    integer(c_int), parameter, public :: NNG_UNIT_MESSAGES = 2 ! Messages, one per message
    integer(c_int), parameter, public :: NNG_UNIT_MILLIS   = 3 ! Milliseconds
    integer(c_int), parameter, public :: NNG_UNIT_EVENTS   = 4 ! Some other type of event

    integer(c_unsigned), parameter, public :: NNG_FLAG_ALLOC    = 1 ! Recv to allocate receive buffer
    integer(c_unsigned), parameter, public :: NNG_FLAG_NONBLOCK = 2 ! Non-blocking operations

    character(*), parameter, public :: NNG_OPT_SOCKNAME                = 'socket-name' // c_null_char
    character(*), parameter, public :: NNG_OPT_RAW                     = 'raw' // c_null_char
    character(*), parameter, public :: NNG_OPT_PROTO                   = 'protocol' // c_null_char
    character(*), parameter, public :: NNG_OPT_PROTONAME               = 'protocol-name' // c_null_char
    character(*), parameter, public :: NNG_OPT_PEER                    = 'peer' // c_null_char
    character(*), parameter, public :: NNG_OPT_PEERNAME                = 'peer-name' // c_null_char
    character(*), parameter, public :: NNG_OPT_RECVBUF                 = 'recv-buffer' // c_null_char
    character(*), parameter, public :: NNG_OPT_SENDBUF                 = 'send-buffer' // c_null_char
    character(*), parameter, public :: NNG_OPT_RECVFD                  = 'recv-fd' // c_null_char
    character(*), parameter, public :: NNG_OPT_SENDFD                  = 'send-fd' // c_null_char
    character(*), parameter, public :: NNG_OPT_RECVTIMEO               = 'recv-timeout' // c_null_char
    character(*), parameter, public :: NNG_OPT_SENDTIMEO               = 'send-timeout' // c_null_char
    character(*), parameter, public :: NNG_OPT_LOCADDR                 = 'local-address' // c_null_char
    character(*), parameter, public :: NNG_OPT_REMADDR                 = 'remote-address' // c_null_char
    character(*), parameter, public :: NNG_OPT_URL                     = 'url' // c_null_char
    character(*), parameter, public :: NNG_OPT_MAXTTL                  = 'ttl-max' // c_null_char
    character(*), parameter, public :: NNG_OPT_RECVMAXSZ               = 'recv-size-max' // c_null_char
    character(*), parameter, public :: NNG_OPT_RECONNMINT              = 'reconnect-time-min' // c_null_char
    character(*), parameter, public :: NNG_OPT_RECONNMAXT              = 'reconnect-time-max' // c_null_char
    character(*), parameter, public :: NNG_OPT_TLS_CONFIG              = 'tls-config' // c_null_char
    character(*), parameter, public :: NNG_OPT_TLS_AUTH_MODE           = 'tls-authmode' // c_null_char
    character(*), parameter, public :: NNG_OPT_TLS_CERT_KEY_FILE       = 'tls-cert-key-file' // c_null_char
    character(*), parameter, public :: NNG_OPT_TLS_CA_FILE             = 'tls-ca-file' // c_null_char
    character(*), parameter, public :: NNG_OPT_TLS_SERVER_NAME         = 'tls-server-name' // c_null_char
    character(*), parameter, public :: NNG_OPT_TLS_VERIFIED            = 'tls-verified' // c_null_char
    character(*), parameter, public :: NNG_OPT_TLS_PEER_CN             = 'tls-peer-cn' // c_null_char
    character(*), parameter, public :: NNG_OPT_TLS_PEER_ALT_NAMES      = 'tls-peer-alt-names' // c_null_char
    character(*), parameter, public :: NNG_OPT_TCP_NODELAY             = 'tcp-nodelay' // c_null_char
    character(*), parameter, public :: NNG_OPT_TCP_KEEPALIVE           = 'tcp-keepalive' // c_null_char
    character(*), parameter, public :: NNG_OPT_TCP_BOUND_PORT          = 'tcp-bound-port' // c_null_char
    character(*), parameter, public :: NNG_OPT_IPC_SECURITY_DESCRIPTOR = 'ipc:security-descriptor' // c_null_char
    character(*), parameter, public :: NNG_OPT_IPC_PERMISSIONS         = 'ipc:permissions' // c_null_char
    character(*), parameter, public :: NNG_OPT_PEER_UID                = 'ipc:peer-uid' // c_null_char
    character(*), parameter, public :: NNG_OPT_IPC_PEER_UID            = NNG_OPT_PEER_UID
    character(*), parameter, public :: NNG_OPT_PEER_GID                = 'ipc:peer-gid' // c_null_char
    character(*), parameter, public :: NNG_OPT_IPC_PEER_GID            = NNG_OPT_PEER_GID
    character(*), parameter, public :: NNG_OPT_PEER_PID                = 'ipc:peer-pid' // c_null_char
    character(*), parameter, public :: NNG_OPT_IPC_PEER_PID            = NNG_OPT_PEER_PID
    character(*), parameter, public :: NNG_OPT_PEER_ZONEID             = 'ipc:peer-zoneid' // c_null_char
    character(*), parameter, public :: NNG_OPT_IPC_PEER_ZONEID         = NNG_OPT_PEER_ZONEID
    character(*), parameter, public :: NNG_OPT_WS_REQUEST_HEADERS      = 'ws:request-headers' // c_null_char
    character(*), parameter, public :: NNG_OPT_WS_RESPONSE_HEADERS     = 'ws:response-headers' // c_null_char
    character(*), parameter, public :: NNG_OPT_WS_RESPONSE_HEADER      = 'ws:response-header:' // c_null_char
    character(*), parameter, public :: NNG_OPT_WS_REQUEST_HEADER       = 'ws:request-header:' // c_null_char
    character(*), parameter, public :: NNG_OPT_WS_REQUEST_URI          = 'ws:request-uri' // c_null_char
    character(*), parameter, public :: NNG_OPT_WS_SENDMAXFRAME         = 'ws:txframe-max' // c_null_char
    character(*), parameter, public :: NNG_OPT_WS_RECVMAXFRAME         = 'ws:rxframe-max' // c_null_char
    character(*), parameter, public :: NNG_OPT_WS_PROTOCOL             = 'ws:protocol' // c_null_char
    character(*), parameter, public :: NNG_OPT_WS_SEND_TEXT            = 'ws:send-text' // c_null_char
    character(*), parameter, public :: NNG_OPT_WS_RECV_TEXT            = 'ws:recv-text' // c_null_char
    character(*), parameter, public :: NNG_OPT_SOCKET_FD               = 'socket:fd' // c_null_char

    ! struct nng_ctx
    type, bind(c), public :: nng_ctx
        integer(c_uint32_t) :: id = 0
    end type nng_ctx

    ! struct nng_dialer
    type, bind(c), public :: nng_dialer
        integer(c_uint32_t) :: id = 0
    end type nng_dialer

    ! struct nng_listener
    type, bind(c), public :: nng_listener
        integer(c_uint32_t) :: id = 0
    end type nng_listener

    ! struct nng_pipe
    type, bind(c), public :: nng_pipe
        integer(c_uint32_t) :: id = 0
    end type nng_pipe

    ! struct nng_socket
    type, bind(c), public :: nng_socket
        integer(c_uint32_t) :: id = 0
    end type nng_socket

    ! struct nng_iov
    type, bind(c), public :: nng_iov
        type(c_ptr)       :: iov_buf = c_null_ptr
        integer(c_size_t) :: iov_len = 0
    end type nng_iov

    public :: nng_aio_cancelfn
    public :: nng_logger
    public :: nng_pipe_cb

    abstract interface
        ! void (*nng_aio_cancelfn)(nng_aio *aio, void *arg, int error)
        subroutine nng_aio_cancelfn(aio, arg, error) bind(c)
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: aio
            type(c_ptr),    intent(in), value :: arg
            integer(c_int), intent(in), value :: error
        end subroutine nng_aio_cancelfn

        ! void (*nng_logger)(nng_log_level level, nng_log_facility facility, const char *msgid, const char *msg)
        subroutine nng_logger(level, facility, msgid, msg) bind(c)
            import :: c_char, c_int
            implicit none
            integer(c_int),    intent(in), value :: level
            integer(c_int),    intent(in), value :: facility
            character(c_char), intent(in)        :: msgid
            character(c_char), intent(in)        :: msg
        end subroutine nng_logger

        ! void (*nng_pipe_cb)(nng_pipe pipe, nng_pipe_ev ev, void *arg)
        subroutine nng_pipe_cb(pipe, ev, arg) bind(c)
            import :: c_int, c_ptr, nng_pipe
            implicit none
            type(nng_pipe), intent(in), value :: pipe
            integer(c_int), intent(in), value :: ev
            type(c_ptr),    intent(in), value :: arg
        end subroutine nng_pipe_cb
    end interface

    public :: nng_aio_abort
    public :: nng_aio_alloc
    public :: nng_aio_begin
    public :: nng_aio_busy
    public :: nng_aio_cancel
    public :: nng_aio_defer
    public :: nng_aio_finish
    public :: nng_aio_free
    public :: nng_aio_free_
    public :: nng_aio_get_input
    public :: nng_aio_get_msg
    public :: nng_aio_get_output
    public :: nng_aio_reap
    public :: nng_aio_result
    public :: nng_aio_set_expire
    public :: nng_aio_set_input
    public :: nng_aio_set_iov
    public :: nng_aio_set_msg
    public :: nng_aio_set_output
    public :: nng_aio_set_timeout
    public :: nng_aio_stop
    public :: nng_aio_wait
    public :: nng_alloc
    public :: nng_clock
    public :: nng_close
    public :: nng_ctx_close
    public :: nng_ctx_get
    public :: nng_ctx_get_bool
    public :: nng_ctx_get_int
    public :: nng_ctx_get_ms
    public :: nng_ctx_get_ptr
    public :: nng_ctx_get_size
    public :: nng_ctx_get_string
    public :: nng_ctx_get_uint64
    public :: nng_ctx_id
    public :: nng_ctx_open
    public :: nng_ctx_recv
    public :: nng_ctx_recvmsg
    public :: nng_ctx_send
    public :: nng_ctx_sendmsg
    public :: nng_ctx_set
    public :: nng_ctx_set_bool
    public :: nng_ctx_set_int
    public :: nng_ctx_set_ms
    public :: nng_ctx_set_ptr
    public :: nng_ctx_set_size
    public :: nng_ctx_set_string
    public :: nng_ctx_set_uint64
    public :: nng_cv_alloc
    public :: nng_cv_free
    public :: nng_cv_free_
    public :: nng_cv_until
    public :: nng_cv_wait
    public :: nng_cv_wake
    public :: nng_cv_wake1
    public :: nng_device
    public :: nng_device_aio
    public :: nng_dial
    public :: nng_dialer_close
    public :: nng_dialer_create
    public :: nng_dialer_get
    public :: nng_dialer_get_addr
    public :: nng_dialer_get_bool
    public :: nng_dialer_get_int
    public :: nng_dialer_get_ms
    public :: nng_dialer_get_ptr
    public :: nng_dialer_get_size
    public :: nng_dialer_get_string
    public :: nng_dialer_get_uint64
    public :: nng_dialer_get_url
    public :: nng_dialer_id
    public :: nng_dialer_set
    public :: nng_dialer_set_addr
    public :: nng_dialer_set_bool
    public :: nng_dialer_set_int
    public :: nng_dialer_set_ms
    public :: nng_dialer_set_ptr
    public :: nng_dialer_set_size
    public :: nng_dialer_set_string
    public :: nng_dialer_set_uint64
    public :: nng_dialer_start
    public :: nng_fini
    public :: nng_free
    public :: nng_free_
    public :: nng_init_set_parameter
    public :: nng_listen
    public :: nng_listener_close
    public :: nng_listener_create
    public :: nng_listener_get
    public :: nng_listener_get_addr
    public :: nng_listener_get_bool
    public :: nng_listener_get_int
    public :: nng_listener_get_ms
    public :: nng_listener_get_ptr
    public :: nng_listener_get_size
    public :: nng_listener_get_string
    public :: nng_listener_get_uint64
    public :: nng_listener_get_url
    public :: nng_listener_id
    public :: nng_listener_set
    public :: nng_listener_set_addr
    public :: nng_listener_set_bool
    public :: nng_listener_set_int
    public :: nng_listener_set_ms
    public :: nng_listener_set_ptr
    public :: nng_listener_set_size
    public :: nng_listener_set_string
    public :: nng_listener_set_uint64
    public :: nng_listener_start
    public :: nng_log_auth
    public :: nng_log_debug
    public :: nng_log_err
    public :: nng_log_get_level
    public :: nng_log_info
    public :: nng_log_notice
    public :: nng_log_set_facility
    public :: nng_log_set_level
    public :: nng_log_set_logger
    public :: nng_log_warn
    public :: nng_msg_alloc
    public :: nng_msg_append
    public :: nng_msg_append_u16
    public :: nng_msg_append_u32
    public :: nng_msg_append_u64
    public :: nng_msg_body
    public :: nng_msg_capacity
    public :: nng_msg_chop
    public :: nng_msg_chop_u16
    public :: nng_msg_chop_u32
    public :: nng_msg_chop_u64
    public :: nng_msg_clear
    public :: nng_msg_dup
    public :: nng_msg_free
    public :: nng_msg_free_
    public :: nng_msg_get_pipe
    public :: nng_msg_header
    public :: nng_msg_header_append
    public :: nng_msg_header_append_u16
    public :: nng_msg_header_append_u32
    public :: nng_msg_header_append_u64
    public :: nng_msg_header_chop
    public :: nng_msg_header_chop_u16
    public :: nng_msg_header_chop_u32
    public :: nng_msg_header_chop_u64
    public :: nng_msg_header_clear
    public :: nng_msg_header_insert
    public :: nng_msg_header_insert_u16
    public :: nng_msg_header_insert_u32
    public :: nng_msg_header_insert_u64
    public :: nng_msg_header_len
    public :: nng_msg_header_trim
    public :: nng_msg_header_trim_u16
    public :: nng_msg_header_trim_u32
    public :: nng_msg_header_trim_u64
    public :: nng_msg_insert
    public :: nng_msg_insert_u16
    public :: nng_msg_insert_u32
    public :: nng_msg_insert_u64
    public :: nng_msg_len
    public :: nng_msg_realloc
    public :: nng_msg_reserve
    public :: nng_msg_set_pipe
    public :: nng_msg_trim
    public :: nng_msg_trim_u16
    public :: nng_msg_trim_u32
    public :: nng_msg_trim_u64
    public :: nng_msleep
    public :: nng_mtx_alloc
    public :: nng_mtx_free
    public :: nng_mtx_free_
    public :: nng_mtx_lock
    public :: nng_mtx_unlock
    public :: nng_null_logger
    public :: nng_pipe_close
    public :: nng_pipe_dialer
    public :: nng_pipe_get
    public :: nng_pipe_get_addr
    public :: nng_pipe_get_bool
    public :: nng_pipe_get_int
    public :: nng_pipe_get_ms
    public :: nng_pipe_get_ptr
    public :: nng_pipe_get_size
    public :: nng_pipe_get_string
    public :: nng_pipe_get_uint64
    public :: nng_pipe_id
    public :: nng_pipe_listener
    public :: nng_pipe_notify
    public :: nng_pipe_socket
    public :: nng_random
    public :: nng_recv
    public :: nng_recv_aio
    public :: nng_recvmsg
    public :: nng_send
    public :: nng_send_aio
    public :: nng_sendmsg
    public :: nng_sleep_aio
    public :: nng_sock_recv
    public :: nng_sock_send
    public :: nng_socket_close
    public :: nng_socket_get
    public :: nng_socket_get_addr
    public :: nng_socket_get_bool
    public :: nng_socket_get_int
    public :: nng_socket_get_ms
    public :: nng_socket_get_ptr
    public :: nng_socket_get_size
    public :: nng_socket_get_string
    public :: nng_socket_get_uint64
    public :: nng_socket_id
    public :: nng_socket_pair
    public :: nng_socket_peer_id
    public :: nng_socket_peer_name
    public :: nng_socket_proto_id
    public :: nng_socket_proto_name
    public :: nng_socket_raw
    public :: nng_socket_set
    public :: nng_socket_set_addr
    public :: nng_socket_set_bool
    public :: nng_socket_set_int
    public :: nng_socket_set_ms
    public :: nng_socket_set_ptr
    public :: nng_socket_set_size
    public :: nng_socket_set_string
    public :: nng_socket_set_uint64
    public :: nng_stat_bool
    public :: nng_stat_child
    public :: nng_stat_desc
    public :: nng_stat_desc_
    public :: nng_stat_find
    public :: nng_stat_find_dialer
    public :: nng_stat_find_listener
    public :: nng_stat_find_socket
    public :: nng_stat_name
    public :: nng_stat_name_
    public :: nng_stat_next
    public :: nng_stat_string
    public :: nng_stat_string_
    public :: nng_stat_timestamp
    public :: nng_stat_type
    public :: nng_stat_unit
    public :: nng_stat_value
    public :: nng_stats_dump
    public :: nng_stats_free
    public :: nng_stats_free_
    public :: nng_stats_get
    public :: nng_stderr_logger
    public :: nng_str_sockaddr
    public :: nng_str_sockaddr_
    public :: nng_stream_close
    public :: nng_stream_dialer_alloc
    public :: nng_stream_dialer_alloc_url
    public :: nng_stream_dialer_close
    public :: nng_stream_dialer_dial
    public :: nng_stream_dialer_free
    public :: nng_stream_dialer_free_
    public :: nng_stream_dialer_get
    public :: nng_stream_dialer_get_addr
    public :: nng_stream_dialer_get_bool
    public :: nng_stream_dialer_get_int
    public :: nng_stream_dialer_get_ms
    public :: nng_stream_dialer_get_ptr
    public :: nng_stream_dialer_get_size
    public :: nng_stream_dialer_get_string
    public :: nng_stream_dialer_get_uint64
    public :: nng_stream_dialer_set
    public :: nng_stream_dialer_set_addr
    public :: nng_stream_dialer_set_bool
    public :: nng_stream_dialer_set_int
    public :: nng_stream_dialer_set_ms
    public :: nng_stream_dialer_set_ptr
    public :: nng_stream_dialer_set_size
    public :: nng_stream_dialer_set_string
    public :: nng_stream_dialer_set_uint64
    public :: nng_stream_free
    public :: nng_stream_free_
    public :: nng_stream_get
    public :: nng_stream_get_addr
    public :: nng_stream_get_bool
    public :: nng_stream_get_int
    public :: nng_stream_get_ms
    public :: nng_stream_get_ptr
    public :: nng_stream_get_size
    public :: nng_stream_get_string
    public :: nng_stream_get_uint64
    public :: nng_stream_listener_accept
    public :: nng_stream_listener_alloc
    public :: nng_stream_listener_alloc_url
    public :: nng_stream_listener_close
    public :: nng_stream_listener_free
    public :: nng_stream_listener_free_
    public :: nng_stream_listener_get
    public :: nng_stream_listener_get_addr
    public :: nng_stream_listener_get_bool
    public :: nng_stream_listener_get_int
    public :: nng_stream_listener_get_ms
    public :: nng_stream_listener_get_ptr
    public :: nng_stream_listener_get_size
    public :: nng_stream_listener_get_string
    public :: nng_stream_listener_get_uint64
    public :: nng_stream_listener_listen
    public :: nng_stream_listener_set
    public :: nng_stream_listener_set_addr
    public :: nng_stream_listener_set_bool
    public :: nng_stream_listener_set_int
    public :: nng_stream_listener_set_ms
    public :: nng_stream_listener_set_ptr
    public :: nng_stream_listener_set_size
    public :: nng_stream_listener_set_string
    public :: nng_stream_listener_set_uint64
    public :: nng_stream_recv
    public :: nng_stream_send
    public :: nng_stream_set
    public :: nng_stream_set_bool
    public :: nng_stream_set_int
    public :: nng_stream_set_ms
    public :: nng_stream_set_ptr
    public :: nng_stream_set_size
    public :: nng_stream_set_string
    public :: nng_stream_set_uint64
    public :: nng_strerror
    public :: nng_strerror_
    public :: nng_strfree
    public :: nng_strfree_
    public :: nng_system_logger
    public :: nng_thread_create
    public :: nng_thread_destroy
    public :: nng_thread_set_name
    public :: nng_udp_close
    public :: nng_udp_multicast_membership
    public :: nng_udp_open
    public :: nng_udp_recv
    public :: nng_udp_send
    public :: nng_udp_sockname
    public :: nng_url_clone
    public :: nng_url_free
    public :: nng_url_free_
    public :: nng_url_parse
    public :: nng_version
    public :: nng_version_

    interface
        ! void nng_aio_abort(nng_aio *aio, int err)
        subroutine nng_aio_abort(aio, err) bind(c, name='nng_aio_abort')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: aio
            integer(c_int), intent(in), value :: err
        end subroutine nng_aio_abort

        ! int nng_aio_alloc(nng_aio **aiop, void (*cb)(void *), void *arg)
        function nng_aio_alloc(aiop, cb, arg) bind(c, name='nng_aio_alloc')
            import :: c_int, c_funptr, c_ptr
            implicit none
            type(c_ptr),    intent(out)       :: aiop
            type(c_funptr), intent(in), value :: cb
            type(c_ptr),    intent(in), value :: arg
            integer(c_int)                    :: nng_aio_alloc
        end function nng_aio_alloc

        ! bool nng_aio_begin(nng_aio *aio)
        function nng_aio_begin(aio) bind(c, name='nng_aio_begin')
            import :: c_bool, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: aio
            logical(c_bool)                :: nng_aio_begin
        end function nng_aio_begin

        ! bool nng_aio_busy(nng_aio *aio)
        function nng_aio_busy(aio) bind(c, name='nng_aio_busy')
            import :: c_bool, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: aio
            logical(c_bool)                :: nng_aio_busy
        end function nng_aio_busy

        ! void nng_aio_cancel(nng_aio *aio)
        subroutine nng_aio_cancel(aio) bind(c, name='nng_aio_cancel')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_aio_cancel

        ! void nng_aio_defer(nng_aio *aio, nng_aio_cancelfn fn, void *arg)
        subroutine nng_aio_defer(aio, fn, arg) bind(c, name='nng_aio_defer')
            import :: c_ptr, nng_aio_cancelfn
            implicit none
            type(c_ptr), intent(in), value :: aio
            procedure(nng_aio_cancelfn)    :: fn
            type(c_ptr), intent(in), value :: arg
        end subroutine nng_aio_defer

        ! void nng_aio_finish(nng_aio *aio, int err)
        subroutine nng_aio_finish(aio, err) bind(c, name='nng_aio_finish')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: aio
            integer(c_int), intent(in), value :: err
        end subroutine nng_aio_finish

        ! void nng_aio_free(nng_aio *aio)
        subroutine nng_aio_free_(aio) bind(c, name='nng_aio_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_aio_free_

        ! void *nng_aio_get_input(nng_aio *aio, unsigned index)
        function nng_aio_get_input(aio, index) bind(c, name='nng_aio_get_input')
            import :: c_ptr, c_unsigned
            implicit none
            type(c_ptr),         intent(in), value :: aio
            integer(c_unsigned), intent(in), value :: index
            type(c_ptr)                            :: nng_aio_get_input
        end function nng_aio_get_input

        ! nng_msg *nng_aio_get_msg(nng_aio *aio)
        function nng_aio_get_msg(aio) bind(c, name='nng_aio_get_msg')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: aio
            type(c_ptr)                    :: nng_aio_get_msg
        end function nng_aio_get_msg

        ! void *nng_aio_get_output(nng_aio *aio, unsigned index)
        function nng_aio_get_output(aio, index) bind(c, name='nng_aio_get_output')
            import :: c_ptr, c_unsigned
            implicit none
            type(c_ptr),         intent(in), value :: aio
            integer(c_unsigned), intent(in), value :: index
            type(c_ptr)                            :: nng_aio_get_output
        end function nng_aio_get_output

        ! void nng_aio_reap(nng_aio *aio)
        subroutine nng_aio_reap(aio) bind(c, name='nng_aio_reap')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_aio_reap

        ! int nng_aio_result(nng_aio *aio)
        function nng_aio_result(aio) bind(c, name='nng_aio_result')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: aio
            integer(c_int)                 :: nng_aio_result
        end function nng_aio_result

        ! void nng_aio_set_expire(nng_aio *aio, nng_time expiration)
        subroutine nng_aio_set_expire(aio, expiration) bind(c, name='nng_aio_set_expire')
            import :: c_ptr, nng_time
            implicit none
            type(c_ptr),       intent(in), value :: aio
            integer(nng_time), intent(in), value :: expiration
        end subroutine nng_aio_set_expire

        ! int nng_aio_set_input(nng_aio *aio, unsigned index, void *param)
        function nng_aio_set_input(aio, index, param) bind(c, name='nng_aio_set_input')
            import :: c_int, c_ptr, c_unsigned
            implicit none
            type(c_ptr),         intent(in), value :: aio
            integer(c_unsigned), intent(in), value :: index
            type(c_ptr),         intent(in), value :: param
            integer(c_int)                         :: nng_aio_set_input
        end function nng_aio_set_input

        ! int nng_aio_set_iov(nng_aio *aio, unsigned niov, const nng_iov *iov)
        function nng_aio_set_iov(aio, niov, iov) bind(c, name='nng_aio_set_iov')
            import :: c_int, c_ptr, c_unsigned, nng_iov
            implicit none
            type(c_ptr),         intent(in), value :: aio
            integer(c_unsigned), intent(in), value :: niov
            type(nng_iov),       intent(in)        :: iov(*)
            integer(c_int)                         :: nng_aio_set_iov
        end function nng_aio_set_iov

        ! void nng_aio_set_msg(nng_aio *aio, nng_msg *msg)
        subroutine nng_aio_set_msg(aio, msg) bind(c, name='nng_aio_set_msg')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: aio
            type(c_ptr), intent(in), value :: msg
        end subroutine nng_aio_set_msg

        ! int nng_aio_set_output(nng_aio *aio, unsigned index, void *result)
        function nng_aio_set_output(aio, index, result) bind(c, name='nng_aio_set_output')
            import :: c_int, c_ptr, c_unsigned
            implicit none
            type(c_ptr),         intent(in), value :: aio
            integer(c_unsigned), intent(in), value :: index
            type(c_ptr),         intent(in), value :: result
            integer(c_int)                         :: nng_aio_set_output
        end function nng_aio_set_output

        ! void nng_aio_set_timeout(nng_aio *aio, nng_duration msec)
        subroutine nng_aio_set_timeout(aio, msec) bind(c, name='nng_aio_set_timeout')
            import :: c_ptr, nng_duration
            implicit none
            type(c_ptr),           intent(in), value :: aio
            integer(nng_duration), intent(in), value :: msec
        end subroutine nng_aio_set_timeout

        ! void nng_aio_stop(nng_aio *aio)
        subroutine nng_aio_stop(aio) bind(c, name='nng_aio_stop')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_aio_stop

        ! void nng_aio_wait(nng_aio *aio)
        subroutine nng_aio_wait(aio) bind(c, name='nng_aio_wait')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_aio_wait

        ! void *nng_alloc(size_t size)
        function nng_alloc(size) bind(c, name='nng_alloc')
            import :: c_ptr, c_size_t
            implicit none
            integer(c_size_t), intent(in), value :: size
            type(c_ptr)                          :: nng_alloc
        end function nng_alloc

        ! nng_time nng_clock(void)
        function nng_clock() bind(c, name='nng_clock')
            import :: nng_time
            implicit none
            integer(nng_time) :: nng_clock
        end function nng_clock

        ! int nng_close(nng_socket socket)
        function nng_close(socket) bind(c, name='nng_close')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            integer(c_int)                      :: nng_close
        end function nng_close

        ! int nng_ctx_close(nng_ctx ctx)
        function nng_ctx_close(ctx) bind(c, name='nng_ctx_close')
            import :: c_int, nng_ctx
            implicit none
            type(nng_ctx), intent(in), value :: ctx
            integer(c_int)                   :: nng_ctx_close
        end function nng_ctx_close

        ! int nng_ctx_get(nng_ctx ctx, const char *opt, void *val, size_t *valszp)
        function nng_ctx_get(ctx, opt, val, valszp) bind(c, name='nng_ctx_get')
            import :: c_char, c_int, c_ptr, c_size_t, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(out)       :: valszp
            integer(c_int)                       :: nng_ctx_get
        end function nng_ctx_get

        ! int nng_ctx_get_bool(nng_ctx ctx, const char *opt, bool *bvalp)
        function nng_ctx_get_bool(ctx, opt, bvalp) bind(c, name='nng_ctx_get_bool')
            import :: c_bool, c_char, c_int, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(out)       :: bvalp
            integer(c_int)                       :: nng_ctx_get_bool
        end function nng_ctx_get_bool

        ! int nng_ctx_get_int(nng_ctx ctx, const char *opt, int *ivalp)
        function nng_ctx_get_int(ctx, opt, ivalp) bind(c, name='nng_ctx_get_int')
            import :: c_char, c_int, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(out)       :: ivalp
            integer(c_int)                       :: nng_ctx_get_int
        end function nng_ctx_get_int

        ! int nng_ctx_get_ms(nng_ctx ctx, const char *opt, nng_duration *durp)
        function nng_ctx_get_ms(ctx, opt, durp) bind(c, name='nng_ctx_get_ms')
            import :: c_char, c_int, nng_ctx, nng_duration
            implicit none
            type(nng_ctx),         intent(in), value :: ctx
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(out)       :: durp
            integer(c_int)                           :: nng_ctx_get_ms
        end function nng_ctx_get_ms

        ! int nng_ctx_get_ptr(nng_ctx ctx, const char *opt, void **ptr)
        function nng_ctx_get_ptr(ctx, opt, ptr) bind(c, name='nng_ctx_get_ptr')
            import :: c_char, c_int, c_ptr, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: ptr
            integer(c_int)                       :: nng_ctx_get_ptr
        end function nng_ctx_get_ptr

        ! int nng_ctx_get_size(nng_ctx ctx, const char *opt, size_t *zp)
        function nng_ctx_get_size(ctx, opt, zp) bind(c, name='nng_ctx_get_size')
            import :: c_char, c_int, c_size_t, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(out)       :: zp
            integer(c_int)                       :: nng_ctx_get_size
        end function nng_ctx_get_size

        ! int nng_ctx_get_string(nng_ctx ctx, const char *opt, char **strp)
        function nng_ctx_get_string(ctx, opt, strp) bind(c, name='nng_ctx_get_string')
            import :: c_char, c_int, c_ptr, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: strp
            integer(c_int)                       :: nng_ctx_get_string
        end function nng_ctx_get_string

        ! int nng_ctx_get_uint64(nng_ctx ctx, const char *opt, uint64_t *u64p)
        function nng_ctx_get_uint64(ctx, opt, u64p) bind(c, name='nng_ctx_get_uint64')
            import :: c_char, c_int, c_uint64_t, nng_ctx
            implicit none
            type(nng_ctx),       intent(in), value :: ctx
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(out)       :: u64p
            integer(c_int)                         :: nng_ctx_get_uint64
        end function nng_ctx_get_uint64

        ! int nng_ctx_id(nng_ctx ctx)
        function nng_ctx_id(ctx) bind(c, name='nng_ctx_id')
            import :: c_int, nng_ctx
            implicit none
            type(nng_ctx), intent(in), value :: ctx
            integer(c_int)                   :: nng_ctx_id
        end function nng_ctx_id

        ! int nng_ctx_open(nng_ctx *ctx, nng_socket socket)
        function nng_ctx_open(ctx, socket) bind(c, name='nng_ctx_open')
            import :: c_int, nng_ctx, nng_socket
            implicit none
            type(nng_ctx),    intent(out)       :: ctx
            type(nng_socket), intent(in), value :: socket
            integer(c_int)                      :: nng_ctx_open
        end function nng_ctx_open

        ! void nng_ctx_recv(nng_ctx ctx, nng_aio *aio)
        subroutine nng_ctx_recv(ctx, aio) bind(c, name='nng_ctx_recv')
            import :: c_ptr, nng_ctx
            implicit none
            type(nng_ctx), intent(in), value :: ctx
            type(c_ptr),   intent(in), value :: aio
        end subroutine nng_ctx_recv

        ! int nng_ctx_recvmsg(nng_ctx ctx, nng_msg **msgp, int flags)
        function nng_ctx_recvmsg(ctx, msgp, flags) bind(c, name='nng_ctx_recvmsg')
            import :: c_int, c_ptr, nng_ctx
            implicit none
            type(nng_ctx),  intent(in), value :: ctx
            type(c_ptr),    intent(out)       :: msgp
            integer(c_int), intent(in), value :: flags
            integer(c_int)                    :: nng_ctx_recvmsg
        end function nng_ctx_recvmsg

        ! void nng_ctx_send(nng_ctx ctx, nng_aio *aio)
        subroutine nng_ctx_send(ctx, aio) bind(c, name='nng_ctx_send')
            import :: c_ptr, nng_ctx
            implicit none
            type(nng_ctx), intent(in), value :: ctx
            type(c_ptr),   intent(in), value :: aio
        end subroutine nng_ctx_send

        ! int nng_ctx_sendmsg(nng_ctx ctx, nng_msg *msg, int flags)
        function nng_ctx_sendmsg(ctx, msg, flags) bind(c, name='nng_ctx_sendmsg')
            import :: c_int, c_ptr, nng_ctx
            implicit none
            type(nng_ctx),  intent(in), value :: ctx
            type(c_ptr),    intent(in), value :: msg
            integer(c_int), intent(in), value :: flags
            integer(c_int)                    :: nng_ctx_sendmsg
        end function nng_ctx_sendmsg

        ! int nng_ctx_set(nng_ctx ctx, const char *opt, const void *val, size_t valsz)
        function nng_ctx_set(ctx, opt, val, valsz) bind(c, name='nng_ctx_set')
            import :: c_char, c_int, c_ptr, c_size_t, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(in), value :: valsz
            integer(c_int)                       :: nng_ctx_set
        end function nng_ctx_set

        ! int nng_ctx_set_bool(nng_ctx ctx, const char *opt, bool bval)
        function nng_ctx_set_bool(ctx, opt, bval) bind(c, name='nng_ctx_set_bool')
            import :: c_bool, c_char, c_int, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(in), value :: bval
            integer(c_int)                       :: nng_ctx_set_bool
        end function nng_ctx_set_bool

        ! int nng_ctx_set_int(nng_ctx ctx, const char *opt, int ival)
        function nng_ctx_set_int(ctx, opt, ival) bind(c, name='nng_ctx_set_int')
            import :: c_char, c_int, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(in), value :: ival
            integer(c_int)                       :: nng_ctx_set_int
        end function nng_ctx_set_int

        ! int nng_ctx_set_ms(nng_ctx ctx, const char *opt, nng_duration dur)
        function nng_ctx_set_ms(ctx, opt, dur) bind(c, name='nng_ctx_set_ms')
            import :: c_char, c_int, nng_ctx, nng_duration
            implicit none
            type(nng_ctx),         intent(in), value :: ctx
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(in), value :: dur
            integer(c_int)                           :: nng_ctx_set_ms
        end function nng_ctx_set_ms

        ! int nng_ctx_set_ptr(nng_ctx ctx, const char *opt, void *ptr)
        function nng_ctx_set_ptr(ctx, opt, ptr) bind(c, name='nng_ctx_set_ptr')
            import :: c_char, c_int, c_ptr, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: ptr
            integer(c_int)                       :: nng_ctx_set_ptr
        end function nng_ctx_set_ptr

        ! int nng_ctx_set_size(nng_ctx ctx, const char *opt, size_t z)
        function nng_ctx_set_size(ctx, opt, z) bind(c, name='nng_ctx_set_size')
            import :: c_char, c_int, c_size_t, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(in), value :: z
            integer(c_int)                       :: nng_ctx_set_size
        end function nng_ctx_set_size

        ! int nng_ctx_set_string(nng_ctx ctx, const char *opt, const char *str)
        function nng_ctx_set_string(ctx, opt, str) bind(c, name='nng_ctx_set_string')
            import :: c_char, c_int, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: ctx
            character(c_char), intent(in)        :: opt
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: nng_ctx_set_string
        end function nng_ctx_set_string

        ! int nng_ctx_set_uint64(nng_ctx ctx, const char *opt, uint64_t u64)
        function nng_ctx_set_uint64(ctx, opt, u64) bind(c, name='nng_ctx_set_uint64')
            import :: c_char, c_int, c_uint64_t, nng_ctx
            implicit none
            type(nng_ctx),       intent(in), value :: ctx
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(in), value :: u64
            integer(c_int)                         :: nng_ctx_set_uint64
        end function nng_ctx_set_uint64

        ! int nng_cv_alloc(nng_cv **cvp, nng_mtx *mtx)
        function nng_cv_alloc(cvp, mtx) bind(c, name='nng_cv_alloc')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out)       :: cvp
            type(c_ptr), intent(in), value :: mtx
            integer(c_int)                 :: nng_cv_alloc
        end function nng_cv_alloc

        ! void nng_cv_free(nng_cv *cv)
        subroutine nng_cv_free_(cv) bind(c, name='nng_cv_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cv
        end subroutine nng_cv_free_

        ! int nng_cv_until(nng_cv *cv, nng_time when)
        function nng_cv_until(cv, when) bind(c, name='nng_cv_until')
            import :: c_int, c_ptr, nng_time
            implicit none
            type(c_ptr),       intent(in), value :: cv
            integer(nng_time), intent(in), value :: when
            integer(c_int)                       :: nng_cv_until
        end function nng_cv_until

        ! void nng_cv_wait(nng_cv *cv)
        subroutine nng_cv_wait(cv) bind(c, name='nng_cv_wait')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cv
        end subroutine nng_cv_wait

        ! void nng_cv_wake(nng_cv *cv)
        subroutine nng_cv_wake(cv) bind(c, name='nng_cv_wake')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cv
        end subroutine nng_cv_wake

        ! void nng_cv_wake1(nng_cv *cv)
        subroutine nng_cv_wake1(cv) bind(c, name='nng_cv_wake1')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cv
        end subroutine nng_cv_wake1

        ! int nng_device(nng_socket s1, nng_socket s2)
        function nng_device(s1, s2) bind(c, name='nng_device')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: s1
            type(nng_socket), intent(in), value :: s2
            integer(c_int)                      :: nng_device
        end function nng_device

        ! void nng_device_aio(nng_aio *aio, nng_socket s1, nng_socket s1)
        subroutine nng_device_aio(aio, s1, s2) bind(c, name='nng_device_aio')
            import :: c_ptr, nng_socket
            implicit none
            type(c_ptr),      intent(in), value :: aio
            type(nng_socket), intent(in), value :: s1
            type(nng_socket), intent(in), value :: s2
        end subroutine nng_device_aio

        ! int nng_dial(nng_socket socket, const char *url, nng_dialer *dialerp, int flags)
        function nng_dial(socket, url, dialerp, flags) bind(c, name='nng_dial')
            import :: c_char, c_int, nng_dialer, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: url
            type(nng_dialer),  intent(out)       :: dialerp
            integer(c_int),    intent(in), value :: flags
            integer(c_int)                       :: nng_dial
        end function nng_dial

        ! int nng_dialer_close(nng_dialer dialer)
        function nng_dialer_close(dialer) bind(c, name='nng_dialer_close')
            import :: c_int, nng_dialer
            implicit none
            type(nng_dialer), intent(in), value :: dialer
            integer(c_int)                      :: nng_dialer_close
        end function nng_dialer_close

        ! int nng_dialer_create(nng_dialer *dialerp, nng_socket socket, const char *url)
        function nng_dialer_create(dialerp, socket, url) bind(c, name='nng_dialer_create')
            import :: c_char, c_int, nng_dialer, nng_socket
            implicit none
            type(nng_dialer),  intent(out)       :: dialerp
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: url
            integer(c_int)                       :: nng_dialer_create
        end function nng_dialer_create

        ! int nng_dialer_get(nng_dialer dialer, const char *opt, void *val, size_t *valszp)
        function nng_dialer_get(dialer, opt, val, valszp) bind(c, name='nng_dialer_get')
            import :: c_char, c_int, c_ptr, c_size_t, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(out)       :: valszp
            integer(c_int)                       :: nng_dialer_get
        end function nng_dialer_get

        ! int nng_dialer_get_addr(nng_dialer dialer, const char *opt, nng_sockaddr *sap)
        function nng_dialer_get_addr(dialer, opt, sap) bind(c, name='nng_dialer_get_addr')
            import :: c_char, c_int, c_ptr, nng_dialer
            implicit none
            type(nng_dialer),   intent(in), value :: dialer
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(out)       :: sap
            integer(c_int)                        :: nng_dialer_get_addr
        end function nng_dialer_get_addr

        ! int nng_dialer_get_bool(nng_dialer dialer, const char *opt, bool *bvalp)
        function nng_dialer_get_bool(dialer, opt, bvalp) bind(c, name='nng_dialer_get_bool')
            import :: c_bool, c_char, c_int, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(out)       :: bvalp
            integer(c_int)                       :: nng_dialer_get_bool
        end function nng_dialer_get_bool

        ! int nng_dialer_get_int(nng_dialer dialer, const char *opt, int *ivalp)
        function nng_dialer_get_int(dialer, opt, ivalp) bind(c, name='nng_dialer_get_int')
            import :: c_char, c_int, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(out)       :: ivalp
            integer(c_int)                       :: nng_dialer_get_int
        end function nng_dialer_get_int

        ! int nng_dialer_get_ms(nng_dialer dialer, const char *opt, nng_duration *durp)
        function nng_dialer_get_ms(dialer, opt, durp) bind(c, name='nng_dialer_get_ms')
            import :: c_char, c_int, nng_dialer, nng_duration
            implicit none
            type(nng_dialer),      intent(in), value :: dialer
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(out)       :: durp
            integer(c_int)                           :: nng_dialer_get_ms
        end function nng_dialer_get_ms

        ! int nng_dialer_get_ptr(nng_dialer dialer, const char *opt, void **ptr)
        function nng_dialer_get_ptr(dialer, opt, ptr) bind(c, name='nng_dialer_get_ptr')
            import :: c_char, c_int, c_ptr, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: ptr
            integer(c_int)                       :: nng_dialer_get_ptr
        end function nng_dialer_get_ptr

        ! int nng_dialer_get_size(nng_dialer dialer, const char *opt, size_t *zp)
        function nng_dialer_get_size(dialer, opt, zp) bind(c, name='nng_dialer_get_size')
            import :: c_char, c_int, c_size_t, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(out)       :: zp
            integer(c_int)                       :: nng_dialer_get_size
        end function nng_dialer_get_size

        ! int nng_dialer_get_string(nng_dialer dialer, const char *opt, char **strp)
        function nng_dialer_get_string(dialer, opt, strp) bind(c, name='nng_dialer_get_string')
            import :: c_char, c_int, c_ptr, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: strp
            integer(c_int)                       :: nng_dialer_get_string
        end function nng_dialer_get_string

        ! int nng_dialer_get_uint64(nng_dialer dialer, const char *opt, uint64_t *u64p)
        function nng_dialer_get_uint64(dialer, opt, u64p) bind(c, name='nng_dialer_get_uint64')
            import :: c_char, c_int, c_uint64_t, nng_dialer
            implicit none
            type(nng_dialer),    intent(in), value :: dialer
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(out)       :: u64p
            integer(c_int)                         :: nng_dialer_get_uint64
        end function nng_dialer_get_uint64

        ! int nng_dialer_get_url(nng_dialer dialer, const nng_url **urlp)
        function nng_dialer_get_url(dialer, urlp) bind(c, name='nng_dialer_get_url')
            import :: c_int, c_ptr, nng_dialer
            implicit none
            type(nng_dialer), intent(in), value :: dialer
            type(c_ptr),      intent(out)       :: urlp
            integer(c_int)                      :: nng_dialer_get_url
        end function nng_dialer_get_url

        ! int nng_dialer_id(nng_dialer dialer)
        function nng_dialer_id(dialer) bind(c, name='nng_dialer_id')
            import :: c_int, nng_dialer
            implicit none
            type(nng_dialer), intent(in), value :: dialer
            integer(c_int)                      :: nng_dialer_id
        end function nng_dialer_id

        ! int nng_dialer_set(nng_dialer dialer, const char *opt, const void *val, size_t valsz)
        function nng_dialer_set(dialer, opt, val, valsz) bind(c, name='nng_dialer_set')
            import :: c_char, c_int, c_ptr, c_size_t, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(in), value :: valsz
            integer(c_int)                       :: nng_dialer_set
        end function nng_dialer_set

        ! int nng_dialer_set_addr(nng_dialer dialer, const char *opt, const nng_sockaddr *sa)
        function nng_dialer_set_addr(dialer, opt, sa) bind(c, name='nng_dialer_set_addr')
            import :: c_char, c_int, c_ptr, nng_dialer
            implicit none
            type(nng_dialer),   intent(in), value :: dialer
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(in), value :: sa
            integer(c_int)                        :: nng_dialer_set_addr
        end function nng_dialer_set_addr

        ! int nng_dialer_set_bool(nng_dialer dialer, const char *opt, bool bval)
        function nng_dialer_set_bool(dialer, opt, bval) bind(c, name='nng_dialer_set_bool')
            import :: c_bool, c_char, c_int, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(in), value :: bval
            integer(c_int)                       :: nng_dialer_set_bool
        end function nng_dialer_set_bool

        ! int nng_dialer_set_int(nng_dialer dialer, const char *opt, int ival)
        function nng_dialer_set_int(dialer, opt, ival) bind(c, name='nng_dialer_set_int')
            import :: c_char, c_int, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(in), value :: ival
            integer(c_int)                       :: nng_dialer_set_int
        end function nng_dialer_set_int

        ! int nng_dialer_set_ms(nng_dialer dialer, const char *opt, nng_duration dur)
        function nng_dialer_set_ms(dialer, opt, dur) bind(c, name='nng_dialer_set_ms')
            import :: c_char, c_int, nng_dialer, nng_duration
            implicit none
            type(nng_dialer),      intent(in), value :: dialer
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(in), value :: dur
            integer(c_int)                           :: nng_dialer_set_ms
        end function nng_dialer_set_ms

        ! int nng_dialer_set_ptr(nng_dialer dialer, const char *opt, void *ptr)
        function nng_dialer_set_ptr(dialer, opt, ptr) bind(c, name='nng_dialer_set_ptr')
            import :: c_char, c_int, c_ptr, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: ptr
            integer(c_int)                       :: nng_dialer_set_ptr
        end function nng_dialer_set_ptr

        ! int nng_dialer_set_size(nng_dialer dialer, const char *opt, size_t z)
        function nng_dialer_set_size(dialer, opt, z) bind(c, name='nng_dialer_set_size')
            import :: c_char, c_int, c_size_t, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(in), value :: z
            integer(c_int)                       :: nng_dialer_set_size
        end function nng_dialer_set_size

        ! int nng_dialer_set_string(nng_dialer dialer, const char *opt, const char *str)
        function nng_dialer_set_string(dialer, opt, str) bind(c, name='nng_dialer_set_string')
            import :: c_char, c_int, nng_dialer
            implicit none
            type(nng_dialer),  intent(in), value :: dialer
            character(c_char), intent(in)        :: opt
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: nng_dialer_set_string
        end function nng_dialer_set_string

        ! int nng_dialer_set_uint64(nng_dialer dialer, const char *opt, uint64_t u64)
        function nng_dialer_set_uint64(dialer, opt, u64) bind(c, name='nng_dialer_set_uint64')
            import :: c_char, c_int, c_uint64_t, nng_dialer
            implicit none
            type(nng_dialer),    intent(in), value :: dialer
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(in), value :: u64
            integer(c_int)                         :: nng_dialer_set_uint64
        end function nng_dialer_set_uint64

        ! int nng_dialer_start(nng_dialer dialer, int flags)
        function nng_dialer_start(dialer, flags) bind(c, name='nng_dialer_start')
            import :: c_int, nng_dialer
            implicit none
            type(nng_dialer), intent(in), value :: dialer
            integer(c_int),   intent(in), value :: flags
            integer(c_int)                      :: nng_dialer_start
        end function nng_dialer_start

        ! void nng_fini(void)
        subroutine nng_fini() bind(c, name='nng_fini')
        end subroutine nng_fini

        ! void nng_free(void *ptr, size_t size)
        subroutine nng_free_(ptr, size) bind(c, name='nng_free')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: ptr
            integer(c_size_t), intent(in), value :: size
        end subroutine nng_free_

        ! void nng_init_set_parameter(nng_init_parameter p, uint64_t u64)
        subroutine nng_init_set_parameter(p, u64) bind(c, name='nng_init_set_parameter')
            import :: nng_init_parameter, c_uint64_t
            implicit none
            integer(nng_init_parameter), intent(in), value :: p
            integer(c_uint64_t),         intent(in), value :: u64
        end subroutine nng_init_set_parameter

        ! int nng_listen(nng_socket socket, const char *url, nng_listener *lp, int flags)
        function nng_listen(socket, url, lp, flags) bind(c, name='nng_listen')
            import :: c_char, c_int, nng_listener, nng_socket
            implicit none
            type(nng_socket),   intent(in), value :: socket
            character(c_char),  intent(in)        :: url
            type(nng_listener), intent(out)       :: lp
            integer(c_int),     intent(in), value :: flags
            integer(c_int)                        :: nng_listen
        end function nng_listen

        ! int nng_listener_close(nng_listener l)
        function nng_listener_close(l) bind(c, name='nng_listener_close')
            import :: c_int, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            integer(c_int)                        :: nng_listener_close
        end function nng_listener_close

        ! int nng_listener_create(nng_listener *lp, nng_socket socket, const char *opt)
        function nng_listener_create(lp, socket, opt) bind(c, name='nng_listener_create')
            import :: c_char, c_int, nng_listener, nng_socket
            implicit none
            type(nng_listener), intent(out)       :: lp
            type(nng_socket),   intent(in), value :: socket
            character(c_char) , intent(in)        :: opt
            integer(c_int)                        :: nng_listener_create
        end function nng_listener_create

        ! int nng_listener_get(nng_listener l, const char *opt, void *val, size_t *valszp)
        function nng_listener_get(l, opt, val, valszp) bind(c, name='nng_listener_get')
            import :: c_char, c_int, c_ptr, c_size_t, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(in), value :: val
            integer(c_size_t),  intent(out)       :: valszp
            integer(c_int)                        :: nng_listener_get
        end function nng_listener_get

        ! int nng_listener_get_addr(nng_listener l, const char *opt, nng_sockaddr *sap)
        function nng_listener_get_addr(l, opt, sap) bind(c, name='nng_listener_get_addr')
            import :: c_char, c_int, c_ptr, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(out)       :: sap
            integer(c_int)                        :: nng_listener_get_addr
        end function nng_listener_get_addr

        ! int nng_listener_get_bool(nng_listener l, const char *opt, bool *bvalp)
        function nng_listener_get_bool(l, opt, bvalp) bind(c, name='nng_listener_get_bool')
            import :: c_bool, c_char, c_int, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            logical(c_bool),    intent(out)       :: bvalp
            integer(c_int)                        :: nng_listener_get_bool
        end function nng_listener_get_bool

        ! int nng_listener_get_int(nng_listener l, const char *opt, int *ivalp)
        function nng_listener_get_int(l, opt, ivalp) bind(c, name='nng_listener_get_int')
            import :: c_char, c_int, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            integer(c_int),     intent(out)       :: ivalp
            integer(c_int)                        :: nng_listener_get_int
        end function nng_listener_get_int

        ! int nng_listener_get_ms(nng_listener l, const char *opt, nng_duration *durp)
        function nng_listener_get_ms(l, opt, durp) bind(c, name='nng_listener_get_ms')
            import :: c_char, c_int, nng_duration, nng_listener
            implicit none
            type(nng_listener),    intent(in), value :: l
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(out)       :: durp
            integer(c_int)                           :: nng_listener_get_ms
        end function nng_listener_get_ms

        ! int nng_listener_get_ptr(nng_listener l, const char *opt, void **ptr)
        function nng_listener_get_ptr(l, opt, ptr) bind(c, name='nng_listener_get_ptr')
            import :: c_char, c_int, c_ptr, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(out)       :: ptr
            integer(c_int)                        :: nng_listener_get_ptr
        end function nng_listener_get_ptr

        ! int nng_listener_get_size(nng_listener l, const char *opt, size_t *zp)
        function nng_listener_get_size(l, opt, zp) bind(c, name='nng_listener_get_size')
            import :: c_char, c_int, c_size_t, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            integer(c_size_t),  intent(out)       :: zp
            integer(c_int)                        :: nng_listener_get_size
        end function nng_listener_get_size

        ! int nng_listener_get_string(nng_listener l, const char *opt, char **strp)
        function nng_listener_get_string(l, opt, strp) bind(c, name='nng_listener_get_string')
            import :: c_char, c_int, c_ptr, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(out)       :: strp
            integer(c_int)                        :: nng_listener_get_string
        end function nng_listener_get_string

        ! int nng_listener_get_uint64(nng_listener l, const char *opt, uint64_t *u64p)
        function nng_listener_get_uint64(l, opt, u64p) bind(c, name='nng_listener_get_uint64')
            import :: c_char, c_int, c_uint64_t, nng_listener
            implicit none
            type(nng_listener),  intent(in), value :: l
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(out)       :: u64p
            integer(c_int)                         :: nng_listener_get_uint64
        end function nng_listener_get_uint64

        ! int nng_listener_get_url(nng_listener l, const nng_url **urlp)
        function nng_listener_get_url(l, urlp) bind(c, name='nng_listener_get_url')
            import :: c_int, c_ptr, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            type(c_ptr),        intent(out)       :: urlp
            integer(c_int)                        :: nng_listener_get_url
        end function nng_listener_get_url

        ! int nng_listener_id(nng_listener l)
        function nng_listener_id(l) bind(c, name='nng_listener_id')
            import :: c_int, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            integer(c_int)                        :: nng_listener_id
        end function nng_listener_id

        ! int nng_listener_set(nng_listener l, const char *opt, const void *val, size_t valsz)
        function nng_listener_set(l, opt, val, valsz) bind(c, name='nng_listener_set')
            import :: c_char, c_int, c_ptr, c_size_t, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(in), value :: val
            integer(c_size_t),  intent(in), value :: valsz
            integer(c_int)                        :: nng_listener_set
        end function nng_listener_set

        ! int nng_listener_set_addr(nng_listener l, const char *opt, const nng_sockaddr *sa)
        function nng_listener_set_addr(l, opt, sa) bind(c, name='nng_listener_set_addr')
            import :: c_char, c_int, c_ptr, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(in), value :: sa
            integer(c_int)                        :: nng_listener_set_addr
        end function nng_listener_set_addr

        ! int nng_listener_set_bool(nng_listener l, const char *opt, bool bval)
        function nng_listener_set_bool(l, opt, bval) bind(c, name='nng_listener_set_bool')
            import :: c_bool, c_char, c_int, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            logical(c_bool),    intent(in), value :: bval
            integer(c_int)                        :: nng_listener_set_bool
        end function nng_listener_set_bool

        ! int nng_listener_set_int(nng_listener l, const char *opt, int ival)
        function nng_listener_set_int(l, opt, ival) bind(c, name='nng_listener_set_int')
            import :: c_char, c_int, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            integer(c_int),     intent(in), value :: ival
            integer(c_int)                        :: nng_listener_set_int
        end function nng_listener_set_int

        ! int nng_listener_set_ms(nng_listener l, const char *opt, nng_duration dur)
        function nng_listener_set_ms(l, opt, dur) bind(c, name='nng_listener_set_ms')
            import :: c_char, c_int, nng_duration, nng_listener
            implicit none
            type(nng_listener),    intent(in), value :: l
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(in), value :: dur
            integer(c_int)                           :: nng_listener_set_ms
        end function nng_listener_set_ms

        ! int nng_listener_set_ptr(nng_listener l, const char *opt, void *ptr)
        function nng_listener_set_ptr(l, opt, ptr) bind(c, name='nng_listener_set_ptr')
            import :: c_char, c_int, c_ptr, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(in), value :: ptr
            integer(c_int)                        :: nng_listener_set_ptr
        end function nng_listener_set_ptr

        ! int nng_listener_set_size(nng_listener l, const char *opt, size_t z)
        function nng_listener_set_size(l, opt, z) bind(c, name='nng_listener_set_size')
            import :: c_char, c_int, c_size_t, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            integer(c_size_t),  intent(in), value :: z
            integer(c_int)                        :: nng_listener_set_size
        end function nng_listener_set_size

        ! int nng_listener_set_string(nng_listener l, const char *opt, const char *str)
        function nng_listener_set_string(l, opt, str) bind(c, name='nng_listener_set_string')
            import :: c_char, c_int, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            character(c_char),  intent(in)        :: str
            integer(c_int)                        :: nng_listener_set_string
        end function nng_listener_set_string

        ! int nng_listener_set_uint64(nng_listener l, const char *opt, uint64_t u64)
        function nng_listener_set_uint64(l, opt, u64) bind(c, name='nng_listener_set_uint64')
            import :: c_char, c_int, c_uint64_t, nng_listener
            implicit none
            type(nng_listener),  intent(in), value :: l
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(in), value :: u64
            integer(c_int)                         :: nng_listener_set_uint64
        end function nng_listener_set_uint64

        ! int nng_listener_start(nng_listener l, int flags)
        function nng_listener_start(l, flags) bind(c, name='nng_listener_start')
            import :: c_int, nng_listener
            implicit none
            type(nng_listener), intent(in), value :: l
            integer(c_int),     intent(in), value :: flags
            integer(c_int)                        :: nng_listener_start
        end function nng_listener_start

        ! void nng_log_auth_(nng_log_level level, const char *msgid, const char *msg)
        subroutine nng_log_auth(level, msgid, msg) bind(c, name='nng_log_auth_')
            import :: c_char, c_int
            implicit none
            integer(c_int),    intent(in), value :: level
            character(c_char), intent(in)        :: msgid
            character(c_char), intent(in)        :: msg
        end subroutine nng_log_auth

        ! void nng_log_debug_(const char *msgid, const char *msg)
        subroutine nng_log_debug(msgid, msg) bind(c, name='nng_log_debug_')
            import :: c_char
            implicit none
            character(c_char), intent(in) :: msgid
            character(c_char), intent(in) :: msg
        end subroutine nng_log_debug

        ! void nng_log_err_(const char *msgid, const char *msg)
        subroutine nng_log_err(msgid, msg) bind(c, name='nng_log_err_')
            import :: c_char
            implicit none
            character(c_char), intent(in) :: msgid
            character(c_char), intent(in) :: msg
        end subroutine nng_log_err

        ! nng_log_level nng_log_get_level(void)
        function nng_log_get_level() bind(c, name='nng_log_get_level')
            import :: c_int
            implicit none
            integer(c_int) :: nng_log_get_level
        end function nng_log_get_level

        ! void nng_log_info_(const char *msgid, const char *msg)
        subroutine nng_log_info(msgid, msg) bind(c, name='nng_log_info_')
            import :: c_char
            implicit none
            character(c_char), intent(in) :: msgid
            character(c_char), intent(in) :: msg
        end subroutine nng_log_info

        ! void nng_log_notice_(const char *msgid, const char *msg)
        subroutine nng_log_notice(msgid, msg) bind(c, name='nng_log_notice_')
            import :: c_char
            implicit none
            character(c_char), intent(in) :: msgid
            character(c_char), intent(in) :: msg
        end subroutine nng_log_notice

        ! void nng_log_set_facility(nng_log_facility facility)
        subroutine nng_log_set_facility(facility) bind(c, name='nng_log_set_facility')
            import :: c_int
            implicit none
            integer(c_int), intent(in), value :: facility
        end subroutine nng_log_set_facility

        ! void nng_log_set_level(nng_log_level level)
        subroutine nng_log_set_level(level) bind(c, name='nng_log_set_level')
            import :: c_int
            implicit none
            integer(c_int), intent(in), value :: level
        end subroutine nng_log_set_level

        ! void nng_log_set_logger(nng_logger logger)
        subroutine nng_log_set_logger(logger) bind(c, name='nng_log_set_logger')
            import :: nng_logger
            implicit none
            procedure(nng_logger) :: logger
        end subroutine nng_log_set_logger

        ! void nng_log_warn_(const char *msgid, const char *msg)
        subroutine nng_log_warn(msgid, msg) bind(c, name='nng_log_warn_')
            import :: c_char
            implicit none
            character(c_char), intent(in) :: msgid
            character(c_char), intent(in) :: msg
        end subroutine nng_log_warn

        ! int nng_msg_alloc(nng_msg **msgp, size_t size)
        function nng_msg_alloc(msgp, size) bind(c, name='nng_msg_alloc')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(out)       :: msgp
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_alloc
        end function nng_msg_alloc

        ! int nng_msg_append(nng_msg *msg, const void *val, size_t size)
        function nng_msg_append(msg, val, size) bind(c, name='nng_msg_append')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: msg
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_append
        end function nng_msg_append

        ! int nng_msg_append_u16(nng_msg *msg, uint16_t val16)
        function nng_msg_append_u16(msg, val16) bind(c, name='nng_msg_append_u16')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint16_t), intent(in), value :: val16
            integer(c_int)                         :: nng_msg_append_u16
        end function nng_msg_append_u16

        ! int nng_msg_append_u32(nng_msg *msg, uint32_t val32)
        function nng_msg_append_u32(msg, val32) bind(c, name='nng_msg_append_u32')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint32_t), intent(in), value :: val32
            integer(c_int)                         :: nng_msg_append_u32
        end function nng_msg_append_u32

        ! int nng_msg_append_u64(nng_msg *msg, uint64_t val64)
        function nng_msg_append_u64(msg, val64) bind(c, name='nng_msg_append_u64')
            import :: c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint64_t), intent(in), value :: val64
            integer(c_int)                         :: nng_msg_append_u64
        end function nng_msg_append_u64

        ! void *nng_msg_body(nng_msg *msg)
        function nng_msg_body(msg) bind(c, name='nng_msg_body')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: msg
            type(c_ptr)                    :: nng_msg_body
        end function nng_msg_body

        ! size_t nng_msg_capacity(nng_msg *msg)
        function nng_msg_capacity(msg) bind(c, name='nng_msg_capacity')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: msg
            integer(c_size_t)              :: nng_msg_capacity
        end function nng_msg_capacity

        ! int nng_msg_chop(nng_msg *msg, size_t size)
        function nng_msg_chop(msg, size) bind(c, name='nng_msg_chop')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: msg
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_chop
        end function nng_msg_chop

        ! int nng_msg_chop_u16(nng_msg *msg, uint16_t *val16)
        function nng_msg_chop_u16(msg, val16) bind(c, name='nng_msg_chop_u16')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint16_t), intent(out)       :: val16
            integer(c_int)                         :: nng_msg_chop_u16
        end function nng_msg_chop_u16

        ! int nng_msg_chop_u32(nng_msg *msg, uint32_t *val32)
        function nng_msg_chop_u32(msg, val32) bind(c, name='nng_msg_chop_u32')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint32_t), intent(out)       :: val32
            integer(c_int)                         :: nng_msg_chop_u32
        end function nng_msg_chop_u32

        ! int nng_msg_chop_u64(nng_msg *msg, uint64_t *val64)
        function nng_msg_chop_u64(msg, val64) bind(c, name='nng_msg_chop_u64')
            import :: c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint64_t), intent(out)       :: val64
            integer(c_int)                         :: nng_msg_chop_u64
        end function nng_msg_chop_u64

        ! void nng_msg_clear(nng_msg *msg)
        subroutine nng_msg_clear(msg) bind(c, name='nng_msg_clear')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: msg
        end subroutine nng_msg_clear

        ! int nng_msg_dup(nng_msg **dup, const nng_msg *orig)
        function nng_msg_dup(dup, orig) bind(c, name='nng_msg_dup')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out)       :: dup
            type(c_ptr), intent(in), value :: orig
            integer(c_int)                 :: nng_msg_dup
        end function nng_msg_dup

        ! void nng_msg_free(nng_msg *msg)
        subroutine nng_msg_free_(msg) bind(c, name='nng_msg_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: msg
        end subroutine nng_msg_free_

        ! nng_pipe nng_msg_get_pipe(const nng_msg *msg)
        function nng_msg_get_pipe(msg) bind(c, name='nng_msg_get_pipe')
            import :: c_ptr, nng_pipe
            implicit none
            type(c_ptr), intent(in), value :: msg
            type(nng_pipe)                 :: nng_msg_get_pipe
        end function nng_msg_get_pipe

        ! void *nng_msg_header(nng_msg *msg)
        function nng_msg_header(msg) bind(c, name='nng_msg_header')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: msg
            type(c_ptr)                    :: nng_msg_header
        end function nng_msg_header

        ! int nng_msg_header_append(nng_msg *msg, const void *val, size_t size)
        function nng_msg_header_append(msg, val, size) bind(c, name='nng_msg_header_append')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: msg
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_header_append
        end function nng_msg_header_append

        ! int nng_msg_header_append_u16(nng_msg *msg, uint16_t val16)
        function nng_msg_header_append_u16(msg, val16) bind(c, name='nng_msg_header_append_u16')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint16_t), intent(in), value :: val16
            integer(c_int)                         :: nng_msg_header_append_u16
        end function nng_msg_header_append_u16

        ! int nng_msg_header_append_u32(nng_msg *msg, uint32_t val32)
        function nng_msg_header_append_u32(msg, val32) bind(c, name='nng_msg_header_append_u32')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint32_t), intent(in), value :: val32
            integer(c_int)                         :: nng_msg_header_append_u32
        end function nng_msg_header_append_u32

        ! int nng_msg_header_append_u64(nng_msg *msg, uint64_t val64)
        function nng_msg_header_append_u64(msg, val64) bind(c, name='nng_msg_header_append_u64')
            import :: c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint64_t), intent(in), value :: val64
            integer(c_int)                         :: nng_msg_header_append_u64
        end function nng_msg_header_append_u64

        ! int nng_msg_header_chop(nng_msg *msg, size_t size)
        function nng_msg_header_chop(msg, size) bind(c, name='nng_msg_header_chop')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: msg
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_header_chop
        end function nng_msg_header_chop

        ! int nng_msg_header_chop_u16(nng_msg *msg, uint16_t *val16)
        function nng_msg_header_chop_u16(msg, val16) bind(c, name='nng_msg_header_chop_u16')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint16_t), intent(out)       :: val16
            integer(c_int)                         :: nng_msg_header_chop_u16
        end function nng_msg_header_chop_u16

        ! int nng_msg_header_chop_u32(nng_msg *msg, uint32_t *val32)
        function nng_msg_header_chop_u32(msg, val32) bind(c, name='nng_msg_header_chop_u32')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint32_t), intent(out)       :: val32
            integer(c_int)                         :: nng_msg_header_chop_u32
        end function nng_msg_header_chop_u32

        ! int nng_msg_header_chop_u64(nng_msg *msg, uint64_t *val64)
        function nng_msg_header_chop_u64(msg, val64) bind(c, name='nng_msg_header_chop_u64')
            import :: c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint64_t), intent(out)       :: val64
            integer(c_int)                         :: nng_msg_header_chop_u64
        end function nng_msg_header_chop_u64

        ! void nng_msg_header_clear(nng_msg *msg)
        subroutine nng_msg_header_clear(msg) bind(c, name='nng_msg_header_clear')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: msg
        end subroutine nng_msg_header_clear

        ! int nng_msg_header_insert(nng_msg *msg, const void *val, size_t size)
        function nng_msg_header_insert(msg, val, size) bind(c, name='nng_msg_header_insert')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: msg
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_header_insert
        end function nng_msg_header_insert

        ! int nng_msg_header_insert_u16(nng_msg *msg, uint16_t val16)
        function nng_msg_header_insert_u16(msg, val16) bind(c, name='nng_msg_header_insert_u16')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint16_t), intent(in), value :: val16
            integer(c_int)                         :: nng_msg_header_insert_u16
        end function nng_msg_header_insert_u16

        ! int nng_msg_header_insert_u32(nng_msg *msg, uint32_t val32)
        function nng_msg_header_insert_u32(msg, val32) bind(c, name='nng_msg_header_insert_u32')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint32_t), intent(in), value :: val32
            integer(c_int)                         :: nng_msg_header_insert_u32
        end function nng_msg_header_insert_u32

        ! int nng_msg_header_insert_u64(nng_msg *msg, uint64_t val64)
        function nng_msg_header_insert_u64(msg, val64) bind(c, name='nng_msg_header_insert_u64')
            import :: c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint64_t), intent(in), value :: val64
            integer(c_int)                         :: nng_msg_header_insert_u64
        end function nng_msg_header_insert_u64

        ! size_t nng_msg_header_len(const nng_msg *msg)
        function nng_msg_header_len(msg) bind(c, name='nng_msg_header_len')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: msg
            integer(c_size_t)              :: nng_msg_header_len
        end function nng_msg_header_len

        ! int nng_msg_header_trim(nng_msg *msg, size_t size)
        function nng_msg_header_trim(msg, size) bind(c, name='nng_msg_header_trim')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: msg
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_header_trim
        end function nng_msg_header_trim

        ! int nng_msg_header_trim_u16(nng_msg *msg, uint16_t *val16)
        function nng_msg_header_trim_u16(msg, val16) bind(c, name='nng_msg_header_trim_u16')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint16_t), intent(out)       :: val16
            integer(c_int)                         :: nng_msg_header_trim_u16
        end function nng_msg_header_trim_u16

        ! int nng_msg_header_trim_u32(nng_msg *msg, uint32_t *val32)
        function nng_msg_header_trim_u32(msg, val32) bind(c, name='nng_msg_header_trim_u32')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint32_t), intent(out)       :: val32
            integer(c_int)                         :: nng_msg_header_trim_u32
        end function nng_msg_header_trim_u32

        ! int nng_msg_header_trim_u64(nng_msg *msg, uint64_t *val64)
        function nng_msg_header_trim_u64(msg, val64) bind(c, name='nng_msg_header_trim_u64')
            import :: c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint64_t), intent(out)       :: val64
            integer(c_int)                         :: nng_msg_header_trim_u64
        end function nng_msg_header_trim_u64

        ! int nng_msg_insert(nng_msg *msg, const void *ptr, size_t size)
        function nng_msg_insert(msg, ptr, size) bind(c, name='nng_msg_insert')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: msg
            type(c_ptr),       intent(in), value :: ptr
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_insert
        end function nng_msg_insert

        ! int nng_msg_insert_u16(nng_msg *msg, uint16_t val16)
        function nng_msg_insert_u16(msg, val16) bind(c, name='nng_msg_insert_u16')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint16_t), intent(in), value :: val16
            integer(c_int)                         :: nng_msg_insert_u16
        end function nng_msg_insert_u16

        ! int nng_msg_insert_u32(nng_msg *msg, uint32_t val32)
        function nng_msg_insert_u32(msg, val32) bind(c, name='nng_msg_insert_u32')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint32_t), intent(in), value :: val32
            integer(c_int)                         :: nng_msg_insert_u32
        end function nng_msg_insert_u32

        ! int nng_msg_insert_u64(nng_msg *msg, uint64_t val64)
        function nng_msg_insert_u64(msg, val64) bind(c, name='nng_msg_insert_u64')
            import :: c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint64_t), intent(in), value :: val64
            integer(c_int)                         :: nng_msg_insert_u64
        end function nng_msg_insert_u64

        ! size_t nng_msg_len(const nng_msg *msg)
        function nng_msg_len(msg) bind(c, name='nng_msg_len')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: msg
            integer(c_size_t)              :: nng_msg_len
        end function nng_msg_len

        ! int nng_msg_realloc(nng_msg *msg, size_t size)
        function nng_msg_realloc(msg, size) bind(c, name='nng_msg_realloc')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: msg
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_realloc
        end function nng_msg_realloc

        ! int nng_msg_reserve(nng_msg *msg, size_t size)
        function nng_msg_reserve(msg, size) bind(c, name='nng_msg_reserve')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: msg
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_reserve
        end function nng_msg_reserve

        ! void nng_msg_set_pipe(nng_msg *msg, nng_pipe pipe)
        subroutine nng_msg_set_pipe(msg, pipe) bind(c, name='nng_msg_set_pipe')
            import :: c_ptr, nng_pipe
            implicit none
            type(c_ptr),    intent(in), value :: msg
            type(nng_pipe), intent(in), value :: pipe
        end subroutine nng_msg_set_pipe

        ! int nng_msg_trim(nng_msg *msg, size_t size)
        function nng_msg_trim(msg, size) bind(c, name='nng_msg_trim')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: msg
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_msg_trim
        end function nng_msg_trim

        ! int nng_msg_trim_u16(nng_msg *msg, uint16_t *val16)
        function nng_msg_trim_u16(msg, val16) bind(c, name='nng_msg_trim_u16')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint16_t), intent(out)       :: val16
            integer(c_int)                         :: nng_msg_trim_u16
        end function nng_msg_trim_u16

        ! int nng_msg_trim_u32(nng_msg *msg, uint32_t *val32)
        function nng_msg_trim_u32(msg, val32) bind(c, name='nng_msg_trim_u32')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint32_t), intent(out)       :: val32
            integer(c_int)                         :: nng_msg_trim_u32
        end function nng_msg_trim_u32

        ! int nng_msg_trim_u64(nng_msg *msg, uint64_t *val64)
        function nng_msg_trim_u64(msg, val64) bind(c, name='nng_msg_trim_u64')
            import :: c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: msg
            integer(c_uint64_t), intent(out)       :: val64
            integer(c_int)                         :: nng_msg_trim_u64
        end function nng_msg_trim_u64

        ! void nng_msleep(nng_duration msec)
        subroutine nng_msleep(msec) bind(c, name='nng_msleep')
            import :: nng_duration
            implicit none
            integer(nng_duration), intent(in), value :: msec
        end subroutine nng_msleep

        ! int nng_mtx_alloc(nng_mtx **mtxp)
        function nng_mtx_alloc(mtxp) bind(c, name='nng_mtx_alloc')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out) :: mtxp
            integer(c_int)           :: nng_mtx_alloc
        end function nng_mtx_alloc

        ! void nng_mtx_free(nng_mtx *mtx)
        subroutine nng_mtx_free_(mtx) bind(c, name='nng_mtx_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: mtx
        end subroutine nng_mtx_free_

        ! void nng_mtx_lock(nng_mtx *mtx)
        subroutine nng_mtx_lock(mtx) bind(c, name='nng_mtx_lock')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: mtx
        end subroutine nng_mtx_lock

        ! void nng_mtx_unlock(nng_mtx *mtx)
        subroutine nng_mtx_unlock(mtx) bind(c, name='nng_mtx_unlock')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: mtx
        end subroutine nng_mtx_unlock

        ! void nng_null_logger(nng_log_level level, nng_log_facility facility, const char *msgid, const char *msg)
        subroutine nng_null_logger(level, facility, msgid, msg) bind(c, name='nng_null_logger')
            import :: c_char, c_int
            implicit none
            integer(c_int),    intent(in), value :: level
            integer(c_int),    intent(in), value :: facility
            character(c_char), intent(in)        :: msgid
            character(c_char), intent(in)        :: msg
        end subroutine nng_null_logger

        ! int nng_pipe_close(nng_pipe pipe)
        function nng_pipe_close(pipe) bind(c, name='nng_pipe_close')
            import :: c_int, nng_pipe
            implicit none
            type(nng_pipe), intent(in), value :: pipe
            integer(c_int)                    :: nng_pipe_close
        end function nng_pipe_close

        ! nng_dialer nng_pipe_dialer(nng_pipe pipe)
        function nng_pipe_dialer(pipe) bind(c, name='nng_pipe_dialer')
            import :: nng_dialer, nng_pipe
            implicit none
            type(nng_pipe), intent(in), value :: pipe
            type(nng_dialer)                  :: nng_pipe_dialer
        end function nng_pipe_dialer

        ! int nng_pipe_get(nng_pipe pipe, const char *opt, void *val, size_t *valszp)
        function nng_pipe_get(pipe, opt, val, valszp) bind(c, name='nng_pipe_get')
            import :: c_char, c_int, c_ptr, c_size_t, nng_pipe
            implicit none
            type(nng_pipe),    intent(in), value :: pipe
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(out)       :: valszp
            integer(c_int)                       :: nng_pipe_get
        end function nng_pipe_get

        ! int nng_pipe_get_addr(nng_pipe pipe, const char *opt, nng_sockaddr *sap)
        function nng_pipe_get_addr(pipe, opt, sap) bind(c, name='nng_pipe_get_addr')
            import :: c_char, c_int, c_ptr, nng_pipe
            implicit none
            type(nng_pipe),     intent(in), value :: pipe
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(out)       :: sap
            integer(c_int)                        :: nng_pipe_get_addr
        end function nng_pipe_get_addr

        ! int nng_pipe_get_bool(nng_pipe pipe, const char *opt, bool *bvalp)
        function nng_pipe_get_bool(pipe, opt, bvalp) bind(c, name='nng_pipe_get_bool')
            import :: c_bool, c_char, c_int, nng_pipe
            implicit none
            type(nng_pipe),    intent(in), value :: pipe
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(out)       :: bvalp
            integer(c_int)                       :: nng_pipe_get_bool
        end function nng_pipe_get_bool

        ! int nng_pipe_get_int(nng_pipe pipe, const char *opt, int *ivalp)
        function nng_pipe_get_int(pipe, opt, ivalp) bind(c, name='nng_pipe_get_int')
            import :: c_char, c_int, nng_pipe
            implicit none
            type(nng_pipe),    intent(in), value :: pipe
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(out)       :: ivalp
            integer(c_int)                       :: nng_pipe_get_int
        end function nng_pipe_get_int

        ! int nng_pipe_get_ms(nng_pipe pipe, const char *opt, nng_duration *durp)
        function nng_pipe_get_ms(pipe, opt, durp) bind(c, name='nng_pipe_get_ms')
            import :: c_char, c_int, nng_duration, nng_pipe
            implicit none
            type(nng_pipe),        intent(in), value :: pipe
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(out)       :: durp
            integer(c_int)                           :: nng_pipe_get_ms
        end function nng_pipe_get_ms

        ! int nng_pipe_get_ptr(nng_pipe pipe, const char *opt, void **ptr)
        function nng_pipe_get_ptr(pipe, opt, ptr) bind(c, name='nng_pipe_get_ptr')
            import :: c_char, c_int, c_ptr, nng_pipe
            implicit none
            type(nng_pipe),    intent(in), value :: pipe
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: ptr
            integer(c_int)                       :: nng_pipe_get_ptr
        end function nng_pipe_get_ptr

        ! int nng_pipe_get_size(nng_pipe pipe, const char *opt, size_t *zp)
        function nng_pipe_get_size(pipe, opt, zp) bind(c, name='nng_pipe_get_size')
            import :: c_char, c_int, c_size_t, nng_pipe
            implicit none
            type(nng_pipe),    intent(in), value :: pipe
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(out)       :: zp
            integer(c_int)                       :: nng_pipe_get_size
        end function nng_pipe_get_size

        ! int nng_pipe_get_string(nng_pipe pipe, const char *opt, char **strp)
        function nng_pipe_get_string(pipe, opt, strp) bind(c, name='nng_pipe_get_string')
            import :: c_char, c_int, c_ptr, nng_pipe
            implicit none
            type(nng_pipe),    intent(in), value :: pipe
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: strp
            integer(c_int)                       :: nng_pipe_get_string
        end function nng_pipe_get_string

        ! int nng_pipe_get_uint64(nng_pipe pipe, const char *opt, uint64_t *u64p)
        function nng_pipe_get_uint64(pipe, opt, u64p) bind(c, name='nng_pipe_get_uint64')
            import :: c_char, c_int, c_uint64_t, nng_pipe
            implicit none
            type(nng_pipe),      intent(in), value :: pipe
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(out)       :: u64p
            integer(c_int)                         :: nng_pipe_get_uint64
        end function nng_pipe_get_uint64

        ! int nng_pipe_id(nng_pipe pipe)
        function nng_pipe_id(pipe) bind(c, name='nng_pipe_id')
            import :: c_int, nng_pipe
            implicit none
            type(nng_pipe), intent(in), value :: pipe
            integer(c_int)                    :: nng_pipe_id
        end function nng_pipe_id

        ! nng_listener nng_pipe_listener(nng_pipe pipe)
        function nng_pipe_listener(pipe) bind(c, name='nng_pipe_listener')
            import :: nng_listener, nng_pipe
            implicit none
            type(nng_pipe), intent(in), value :: pipe
            type(nng_listener)                :: nng_pipe_listener
        end function nng_pipe_listener

        ! int nng_pipe_notify(nng_socket socket, nng_pipe_ev ev, nng_pipe_cb cb, void *arg)
        function nng_pipe_notify(socket, ev, cb, arg) bind(c, name='nng_pipe_notify')
            import :: c_funptr, c_int, c_ptr, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            integer(c_int),   intent(in), value :: ev
            type(c_funptr),   intent(in), value :: cb
            type(c_ptr),      intent(in), value :: arg
            integer(c_int)                      :: nng_pipe_notify
        end function nng_pipe_notify

        ! nng_socket nng_pipe_socket(nng_pipe pipe)
        function nng_pipe_socket(pipe) bind(c, name='nng_pipe_socket')
            import :: nng_pipe, nng_socket
            implicit none
            type(nng_pipe), intent(in), value :: pipe
            type(nng_socket)                  :: nng_pipe_socket
        end function nng_pipe_socket

        ! uint32_t nng_random(void)
        function nng_random() bind(c, name='nng_random')
            import :: c_uint32_t
            implicit none
            integer(c_uint32_t) :: nng_random
        end function nng_random

        ! int nng_recv(nng_socket socket, void *data, size_t *sizep, int flags)
        function nng_recv(socket, data, sizep, flags) bind(c, name='nng_recv')
            import :: c_int, c_ptr, c_size_t, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            type(c_ptr),       intent(in), value :: data
            integer(c_size_t), intent(inout)     :: sizep
            integer(c_int),    intent(in), value :: flags
            integer(c_int)                       :: nng_recv
        end function nng_recv

        ! void nng_recv_aio(nng_socket socket, nng_aio *aio)
        subroutine nng_recv_aio(socket, aio) bind(c, name='nng_recv_aio')
            import :: c_ptr, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            type(c_ptr),      intent(in), value :: aio
        end subroutine nng_recv_aio

        ! int nng_recvmsg(nng_socket socket, nng_msg **msgp, int flags)
        function nng_recvmsg(socket, msgp, flags) bind(c, name='nng_recvmsg')
            import :: c_int, c_ptr, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            type(c_ptr),      intent(out)       :: msgp
            integer(c_int),   intent(in), value :: flags
            integer(c_int)                      :: nng_recvmsg
        end function nng_recvmsg

        ! int nng_send(nng_socket socket, void *data, size_t size, int flags)
        function nng_send(socket, data, size, flags) bind(c, name='nng_send')
            import :: c_int, c_ptr, c_size_t, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            type(c_ptr),       intent(in), value :: data
            integer(c_size_t), intent(in), value :: size
            integer(c_int),    intent(in), value :: flags
            integer(c_int)                       :: nng_send
        end function nng_send

        ! void nng_send_aio(nng_socket socket, nng_aio *aio)
        subroutine nng_send_aio(socket, aio) bind(c, name='nng_send_aio')
            import :: c_ptr, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            type(c_ptr),      intent(in), value :: aio
        end subroutine nng_send_aio

        ! int nng_sendmsg(nng_socket socket, nng_msg *msg, int flags)
        function nng_sendmsg(socket, msg, flags) bind(c, name='nng_sendmsg')
            import :: c_int, c_ptr, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            type(c_ptr),      intent(in), value :: msg
            integer(c_int),   intent(in), value :: flags
            integer(c_int)                      :: nng_sendmsg
        end function nng_sendmsg

        ! void nng_sleep_aio(nng_duration msec, nng_aio *aio)
        subroutine nng_sleep_aio(msec, aio) bind(c, name='nng_sleep_aio')
            import :: c_ptr, nng_duration
            implicit none
            integer(nng_duration), intent(in), value :: msec
            type(c_ptr),           intent(in), value :: aio
        end subroutine nng_sleep_aio

        ! void nng_sock_recv(nng_socket socket, nng_aio *aio)
        subroutine nng_sock_recv(socket, aio) bind(c, name='nng_sock_recv')
            import :: c_ptr, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            type(c_ptr),      intent(in), value :: aio
        end subroutine nng_sock_recv

        ! void nng_sock_send(nng_socket socket, nng_aio *aio)
        subroutine nng_sock_send(socket, aio) bind(c, name='nng_sock_send')
            import :: c_ptr, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            type(c_ptr),      intent(in), value :: aio
        end subroutine nng_sock_send

        ! int nng_socket_close(nng_socket socket)
        function nng_socket_close(socket) bind(c, name='nng_socket_close')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            integer(c_int)                      :: nng_socket_close
        end function nng_socket_close

        ! int nng_socket_get(nng_socket socket, const char *opt, void *val, size_t *valszp)
        function nng_socket_get(socket, opt, val, valszp) bind(c, name='nng_socket_get')
            import :: c_char, c_int, c_ptr, c_size_t, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(out)       :: valszp
            integer(c_int)                       :: nng_socket_get
        end function nng_socket_get

        ! int nng_socket_get_addr(nng_socket socket, const char *opt, nng_sockaddr *sap)
        function nng_socket_get_addr(socket, opt, sap) bind(c, name='nng_socket_get_addr')
            import :: c_char, c_int, c_ptr, nng_socket
            implicit none
            type(nng_socket),   intent(in), value :: socket
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(out)       :: sap
            integer(c_int)                        :: nng_socket_get_addr
        end function nng_socket_get_addr

        ! int nng_socket_get_bool(nng_socket socket, const char *opt, bool *bvalp)
        function nng_socket_get_bool(socket, opt, bvalp) bind(c, name='nng_socket_get_bool')
            import :: c_bool, c_char, c_int, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(out)       :: bvalp
            integer(c_int)                       :: nng_socket_get_bool
        end function nng_socket_get_bool

        ! int nng_socket_get_int(nng_socket socket, const char *opt, int *ivalp)
        function nng_socket_get_int(socket, opt, ivalp) bind(c, name='nng_socket_get_int')
            import :: c_char, c_int, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(out)       :: ivalp
            integer(c_int)                       :: nng_socket_get_int
        end function nng_socket_get_int

        ! int nng_socket_get_ms(nng_socket socket, const char *opt, nng_duration *durp)
        function nng_socket_get_ms(socket, opt, durp) bind(c, name='nng_socket_get_ms')
            import :: c_char, c_int, nng_duration, nng_socket
            implicit none
            type(nng_socket),      intent(in), value :: socket
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(out)       :: durp
            integer(c_int)                           :: nng_socket_get_ms
        end function nng_socket_get_ms

        ! int nng_socket_get_ptr(nng_socket socket, const char *opt, void **ptr)
        function nng_socket_get_ptr(socket, opt, ptr) bind(c, name='nng_socket_get_ptr')
            import :: c_char, c_int, c_ptr, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: ptr
            integer(c_int)                       :: nng_socket_get_ptr
        end function nng_socket_get_ptr

        ! int nng_socket_get_size(nng_socket socket, const char *opt, size_t *zp)
        function nng_socket_get_size(socket, opt, zp) bind(c, name='nng_socket_get_size')
            import :: c_char, c_int, c_size_t, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(out)       :: zp
            integer(c_int)                       :: nng_socket_get_size
        end function nng_socket_get_size

        ! int nng_socket_get_string(nng_socket socket, const char *opt, char **strp)
        function nng_socket_get_string(socket, opt, strp) bind(c, name='nng_socket_get_string')
            import :: c_char, c_int, c_ptr, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: strp
            integer(c_int)                       :: nng_socket_get_string
        end function nng_socket_get_string

        ! int nng_socket_get_uint64(nng_socket socket, const char *opt, uint64_t *u64p)
        function nng_socket_get_uint64(socket, opt, u64p) bind(c, name='nng_socket_get_uint64')
            import :: c_char, c_int, c_uint64_t, nng_socket
            implicit none
            type(nng_socket),    intent(in), value :: socket
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(out)       :: u64p
            integer(c_int)                         :: nng_socket_get_uint64
        end function nng_socket_get_uint64

        ! int nng_socket_id(nng_socket socket)
        function nng_socket_id(socket) bind(c, name='nng_socket_id')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            integer(c_int)                      :: nng_socket_id
        end function nng_socket_id

        ! int nng_socket_pair(int fds[2])
        function nng_socket_pair(fds) bind(c, name='nng_socket_pair')
            import :: c_int
            implicit none
            integer(c_int), intent(in), value :: fds
            integer(c_int)                    :: nng_socket_pair
        end function nng_socket_pair

        ! int nng_socket_peer_id(nng_socket id, uint16_t *idp)
        function nng_socket_peer_id(id, idp) bind(c, name='nng_socket_peer_id')
            import :: c_int, c_uint16_t, nng_socket
            implicit none
            type(nng_socket),    intent(in), value :: id
            integer(c_uint16_t), intent(out)       :: idp
            integer(c_int)                         :: nng_socket_peer_id
        end function nng_socket_peer_id

        ! int nng_socket_peer_name(nng_socket id, const char **namep)
        function nng_socket_peer_name(id, namep) bind(c, name='nng_socket_peer_name')
            import :: c_int, c_ptr, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: id
            type(c_ptr),      intent(out)       :: namep
            integer(c_int)                      :: nng_socket_peer_name
        end function nng_socket_peer_name

        ! int nng_socket_proto_id(nng_socket id, uint16_t *idp)
        function nng_socket_proto_id(id, idp) bind(c, name='nng_socket_proto_id')
            import :: c_int, c_uint16_t, nng_socket
            implicit none
            type(nng_socket),    intent(in), value :: id
            integer(c_uint16_t), intent(out)       :: idp
            integer(c_int)                         :: nng_socket_proto_id
        end function nng_socket_proto_id

        ! int nng_socket_proto_name(nng_socket id, const char **namep)
        function nng_socket_proto_name(id, namep) bind(c, name='nng_socket_proto_name')
            import :: c_int, c_ptr, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: id
            type(c_ptr),      intent(out)       :: namep
            integer(c_int)                      :: nng_socket_proto_name
        end function nng_socket_proto_name

        ! int nng_socket_raw(nng_socket socket, bool *rawp)
        function nng_socket_raw(socket, rawp) bind(c, name='nng_socket_raw')
            import :: c_bool, c_int, nng_socket
            implicit none
            type(nng_socket), intent(in), value :: socket
            logical(c_bool),  intent(out)       :: rawp
            integer(c_int)                      :: nng_socket_raw
        end function nng_socket_raw

        ! int nng_socket_set(nng_socket socket, const char *opt, const void *val, size_t valsz)
        function nng_socket_set(socket, opt, val, valsz) bind(c, name='nng_socket_set')
            import :: c_char, c_int, c_ptr, c_size_t, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(in), value :: valsz
            integer(c_int)                       :: nng_socket_set
        end function nng_socket_set

        ! int nng_socket_set_addr(nng_socket socket, const char *opt, const nng_sockaddr *sa)
        function nng_socket_set_addr(socket, opt, sa) bind(c, name='nng_socket_set_addr')
            import :: c_char, c_int, c_ptr, nng_socket
            implicit none
            type(nng_socket),   intent(in), value :: socket
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(in), value :: sa
            integer(c_int)                        :: nng_socket_set_addr
        end function nng_socket_set_addr

        ! int nng_socket_set_bool(nng_socket socket, const char *opt, bool bval)
        function nng_socket_set_bool(socket, opt, bval) bind(c, name='nng_socket_set_bool')
            import :: c_bool, c_char, c_int, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(in), value :: bval
            integer(c_int)                       :: nng_socket_set_bool
        end function nng_socket_set_bool

        ! int nng_socket_set_int(nng_socket socket, const char *opt, int ival)
        function nng_socket_set_int(socket, opt, ival) bind(c, name='nng_socket_set_int')
            import :: c_char, c_int, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(in), value :: ival
            integer(c_int)                       :: nng_socket_set_int
        end function nng_socket_set_int

        ! int nng_socket_set_ms(nng_socket socket, const char *opt, nng_duration dur)
        function nng_socket_set_ms(socket, opt, dur) bind(c, name='nng_socket_set_ms')
            import :: c_char, c_int, nng_duration, nng_socket
            implicit none
            type(nng_socket),      intent(in), value :: socket
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(in), value :: dur
            integer(c_int)                           :: nng_socket_set_ms
        end function nng_socket_set_ms

        ! int nng_socket_set_ptr(nng_socket socket, const char *opt, void *ptr)
        function nng_socket_set_ptr(socket, opt, ptr) bind(c, name='nng_socket_set_ptr')
            import :: c_char, c_int, c_ptr, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: ptr
            integer(c_int)                       :: nng_socket_set_ptr
        end function nng_socket_set_ptr

        ! int nng_socket_set_size(nng_socket socket, const char *opt, size_t z)
        function nng_socket_set_size(socket, opt, z) bind(c, name='nng_socket_set_size')
            import :: c_char, c_int, c_size_t, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(in), value :: z
            integer(c_int)                       :: nng_socket_set_size
        end function nng_socket_set_size

        ! int nng_socket_set_string(nng_socket socket, const char *opt, const char *str)
        function nng_socket_set_string(socket, opt, str) bind(c, name='nng_socket_set_string')
            import :: c_char, c_int, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: socket
            character(c_char), intent(in)        :: opt
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: nng_socket_set_string
        end function nng_socket_set_string

        ! int nng_socket_set_uint64(nng_socket socket, const char *opt, uint64_t u64)
        function nng_socket_set_uint64(socket, opt, u64) bind(c, name='nng_socket_set_uint64')
            import :: c_char, c_int, c_uint64_t, nng_socket
            implicit none
            type(nng_socket),    intent(in), value :: socket
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(in), value :: u64
            integer(c_int)                         :: nng_socket_set_uint64
        end function nng_socket_set_uint64

        ! bool nng_stat_bool(nng_stat *stat)
        function nng_stat_bool(stat) bind(c, name='nng_stat_bool')
            import :: c_bool, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stat
            logical(c_bool)                :: nng_stat_bool
        end function nng_stat_bool

        ! nng_stat *nng_stat_child(nng_stat *stat)
        function nng_stat_child(stat) bind(c, name='nng_stat_child')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stat
            type(c_ptr)                    :: nng_stat_child
        end function nng_stat_child

        ! const char *nng_stat_desc(nng_stat *stat)
        function nng_stat_desc_(stat) bind(c, name='nng_stat_desc')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stat
            type(c_ptr)                    :: nng_stat_desc_
        end function nng_stat_desc_

        ! nng_stat *nng_stat_find(nng_stat *stat, const char *name)
        function nng_stat_find(stat, name) bind(c, name='nng_stat_find')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: stat
            character(c_char), intent(in)        :: name
            type(c_ptr)                          :: nng_stat_find
        end function nng_stat_find

        ! nng_stat *nng_stat_find_dialer(nng_stat *stat, nng_dialer dialer)
        function nng_stat_find_dialer(stat, dialer) bind(c, name='nng_stat_find_dialer')
            import :: c_ptr, nng_dialer
            implicit none
            type(c_ptr),      intent(in), value :: stat
            type(nng_dialer), intent(in), value :: dialer
            type(c_ptr)                         :: nng_stat_find_dialer
        end function nng_stat_find_dialer

        ! nng_stat *nng_stat_find_listener(nng_stat *stat, nng_listener l)
        function nng_stat_find_listener(stat, l) bind(c, name='nng_stat_find_listener')
            import :: c_ptr, nng_listener
            implicit none
            type(c_ptr),        intent(in), value :: stat
            type(nng_listener), intent(in), value :: l
            type(c_ptr)                           :: nng_stat_find_listener
        end function nng_stat_find_listener

        ! nng_stat *nng_stat_find_socket(nng_stat *stat, nng_socket socket)
        function nng_stat_find_socket(stat, socket) bind(c, name='nng_stat_find_socket')
            import :: c_ptr, nng_socket
            implicit none
            type(c_ptr),      intent(in), value :: stat
            type(nng_socket), intent(in), value :: socket
            type(c_ptr)                         :: nng_stat_find_socket
        end function nng_stat_find_socket

        ! const char *nng_stat_name(nng_stat *stat)
        function nng_stat_name_(stat) bind(c, name='nng_stat_name')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stat
            type(c_ptr)                    :: nng_stat_name_
        end function nng_stat_name_

        ! nng_stat *nng_stat_next(nng_stat *stat)
        function nng_stat_next(stat) bind(c, name='nng_stat_next')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stat
            type(c_ptr)                    :: nng_stat_next
        end function nng_stat_next

        ! const char *nng_stat_string(nng_stat *stat)
        function nng_stat_string_(stat) bind(c, name='nng_stat_string')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stat
            type(c_ptr)                    :: nng_stat_string_
        end function nng_stat_string_

        ! uint64_t nng_stat_timestamp(nng_stat *stat)
        function nng_stat_timestamp(stat) bind(c, name='nng_stat_timestamp')
            import :: c_ptr, c_uint64_t
            implicit none
            type(c_ptr), intent(in), value :: stat
            integer(c_uint64_t)            :: nng_stat_timestamp
        end function nng_stat_timestamp

        ! int nng_stat_type(nng_stat *stat)
        function nng_stat_type(stat) bind(c, name='nng_stat_type')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stat
            integer(c_int)                 :: nng_stat_type
        end function nng_stat_type

        ! int nng_stat_unit(nng_stat *stat)
        function nng_stat_unit(stat) bind(c, name='nng_stat_unit')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stat
            integer(c_int)                 :: nng_stat_unit
        end function nng_stat_unit

        ! uint64_t nng_stat_value(nng_stat *stat)
        function nng_stat_value(stat) bind(c, name='nng_stat_value')
            import :: c_ptr, c_uint64_t
            implicit none
            type(c_ptr), intent(in), value :: stat
            integer(c_uint64_t)            :: nng_stat_value
        end function nng_stat_value

        ! void nng_stats_dump(nng_stat *stat)
        subroutine nng_stats_dump(stat) bind(c, name='nng_stats_dump')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stat
        end subroutine nng_stats_dump

        ! void nng_stats_free(nng_stat *stat)
        subroutine nng_stats_free_(stat) bind(c, name='nng_stats_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stat
        end subroutine nng_stats_free_

        ! int nng_stats_get(nng_stat **statsp)
        function nng_stats_get(statsp) bind(c, name='nng_stats_get')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out) :: statsp
            integer(c_int)           :: nng_stats_get
        end function nng_stats_get

        ! void nng_stderr_logger(nng_log_level level, nng_log_facility facility, const char *msgid, const char *msg)
        subroutine nng_stderr_logger(level, facility, msgid, msg) bind(c, name='nng_stderr_logger')
            import :: c_char, c_int
            implicit none
            integer(c_int),    intent(in), value :: level
            integer(c_int),    intent(in), value :: facility
            character(c_char), intent(in)        :: msgid
            character(c_char), intent(in)        :: msg
        end subroutine nng_stderr_logger

        ! const char *nng_str_sockaddr(const nng_sockaddr *sa, char *buf, size_t bufsz)
        function nng_str_sockaddr_(sa, buf, bufsz) bind(c, name='nng_str_sockaddr')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),        intent(in), value :: sa
            character(c_char),  intent(in)        :: buf
            integer(c_size_t),  intent(in), value :: bufsz
            type(c_ptr)                           :: nng_str_sockaddr_
        end function nng_str_sockaddr_

        ! void nng_stream_close(nng_stream *stream)
        subroutine nng_stream_close(stream) bind(c, name='nng_stream_close')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stream
        end subroutine nng_stream_close

        ! int nng_stream_dialer_alloc(nng_stream_dialer **dp, const char *addr)
        function nng_stream_dialer_alloc(dp, addr) bind(c, name='nng_stream_dialer_alloc')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(out) :: dp
            character(c_char), intent(in)  :: addr
            integer(c_int)                 :: nng_stream_dialer_alloc
        end function nng_stream_dialer_alloc

        ! int nng_stream_dialer_alloc_url(nng_stream_dialer **dp, const nng_url *url)
        function nng_stream_dialer_alloc_url(dp, url) bind(c, name='nng_stream_dialer_alloc_url')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out)       :: dp
            type(c_ptr), intent(in), value :: url
            integer(c_int)                 :: nng_stream_dialer_alloc_url
        end function nng_stream_dialer_alloc_url

        ! void nng_stream_dialer_close(nng_stream_dialer *d)
        subroutine nng_stream_dialer_close(d) bind(c, name='nng_stream_dialer_close')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: d
        end subroutine nng_stream_dialer_close

        ! void nng_stream_dialer_dial(nng_stream_dialer *d, nng_aio *aio)
        subroutine nng_stream_dialer_dial(d, aio) bind(c, name='nng_stream_dialer_dial')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: d
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_stream_dialer_dial

        ! void nng_stream_dialer_free(nng_stream_dialer *d)
        subroutine nng_stream_dialer_free_(d) bind(c, name='nng_stream_dialer_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: d
        end subroutine nng_stream_dialer_free_

        ! int nng_stream_dialer_get(nng_stream_dialer *d, const char *opt, void *val, size_t *valszp)
        function nng_stream_dialer_get(d, opt, val, valszp) bind(c, name='nng_stream_dialer_get')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(out)       :: valszp
            integer(c_int)                       :: nng_stream_dialer_get
        end function nng_stream_dialer_get

        ! int nng_stream_dialer_get_addr(nng_stream_dialer *d, const char *opt, nng_sockaddr *sap)
        function nng_stream_dialer_get_addr(d, opt, sap) bind(c, name='nng_stream_dialer_get_addr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),        intent(in), value :: d
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(out)       :: sap
            integer(c_int)                        :: nng_stream_dialer_get_addr
        end function nng_stream_dialer_get_addr

        ! int nng_stream_dialer_get_bool(nng_stream_dialer *d, const char *opt, bool *bvalp)
        function nng_stream_dialer_get_bool(d, opt, bvalp) bind(c, name='nng_stream_dialer_get_bool')
            import :: c_bool, c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(out)       :: bvalp
            integer(c_int)                       :: nng_stream_dialer_get_bool
        end function nng_stream_dialer_get_bool

        ! int nng_stream_dialer_get_int(nng_stream_dialer *d, const char *opt, int *ivalp)
        function nng_stream_dialer_get_int(d, opt, ivalp) bind(c, name='nng_stream_dialer_get_int')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(out)       :: ivalp
            integer(c_int)                       :: nng_stream_dialer_get_int
        end function nng_stream_dialer_get_int

        ! int nng_stream_dialer_get_ms(nng_stream_dialer *d, const char *opt, nng_duration *durp)
        function nng_stream_dialer_get_ms(d, opt, durp) bind(c, name='nng_stream_dialer_get_ms')
            import :: c_char, c_int, c_ptr, nng_duration
            implicit none
            type(c_ptr),           intent(in), value :: d
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(out)       :: durp
            integer(c_int)                           :: nng_stream_dialer_get_ms
        end function nng_stream_dialer_get_ms

        ! int nng_stream_dialer_get_ptr(nng_stream_dialer *d, const char *opt, void **ptr)
        function nng_stream_dialer_get_ptr(d, opt, ptr) bind(c, name='nng_stream_dialer_get_ptr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: ptr
            integer(c_int)                       :: nng_stream_dialer_get_ptr
        end function nng_stream_dialer_get_ptr

        ! int nng_stream_dialer_get_size(nng_stream_dialer *d, const char *opt, size_t *zp)
        function nng_stream_dialer_get_size(d, opt, zp) bind(c, name='nng_stream_dialer_get_size')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(out)       :: zp
            integer(c_int)                       :: nng_stream_dialer_get_size
        end function nng_stream_dialer_get_size

        ! int nng_stream_dialer_get_string(nng_stream_dialer *d, const char *opt, char **strp)
        function nng_stream_dialer_get_string(d, opt, strp) bind(c, name='nng_stream_dialer_get_string')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: strp
            integer(c_int)                       :: nng_stream_dialer_get_string
        end function nng_stream_dialer_get_string

        ! int nng_stream_dialer_get_uint64(nng_stream_dialer *d, const char *opt, uint64_t *u64p)
        function nng_stream_dialer_get_uint64(d, opt, u64p) bind(c, name='nng_stream_dialer_get_uint64')
            import :: c_char, c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: d
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(out)       :: u64p
            integer(c_int)                         :: nng_stream_dialer_get_uint64
        end function nng_stream_dialer_get_uint64

        ! int nng_stream_dialer_set(nng_stream_dialer *d, const char *opt, const void *data, size_t size)
        function nng_stream_dialer_set(d, opt, data, size) bind(c, name='nng_stream_dialer_set')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: data
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_stream_dialer_set
        end function nng_stream_dialer_set

        ! int nng_stream_dialer_set_addr(nng_stream_dialer *d, const char *opt, const nng_sockaddr *sa)
        function nng_stream_dialer_set_addr(d, opt, sa) bind(c, name='nng_stream_dialer_set_addr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),        intent(in), value :: d
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(in), value :: sa
            integer(c_int)                        :: nng_stream_dialer_set_addr
        end function nng_stream_dialer_set_addr

        ! int nng_stream_dialer_set_bool(nng_stream_dialer *d, const char *opt, bool bval)
        function nng_stream_dialer_set_bool(d, opt, bval) bind(c, name='nng_stream_dialer_set_bool')
            import :: c_bool, c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(in), value :: bval
            integer(c_int)                       :: nng_stream_dialer_set_bool
        end function nng_stream_dialer_set_bool

        ! int nng_stream_dialer_set_int(nng_stream_dialer *d, const char *opt, int ival)
        function nng_stream_dialer_set_int(d, opt, ival) bind(c, name='nng_stream_dialer_set_int')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(in), value :: ival
            integer(c_int)                       :: nng_stream_dialer_set_int
        end function nng_stream_dialer_set_int

        ! int nng_stream_dialer_set_ms(nng_stream_dialer *d, const char *opt, nng_duration dur)
        function nng_stream_dialer_set_ms(d, opt, dur) bind(c, name='nng_stream_dialer_set_ms')
            import :: c_char, c_int, c_ptr, nng_duration
            implicit none
            type(c_ptr),           intent(in), value :: d
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(in), value :: dur
            integer(c_int)                           :: nng_stream_dialer_set_ms
        end function nng_stream_dialer_set_ms

        ! int nng_stream_dialer_set_ptr(nng_stream_dialer *d, const char *opt, void *ptr)
        function nng_stream_dialer_set_ptr(d, opt, ptr) bind(c, name='nng_stream_dialer_set_ptr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: ptr
            integer(c_int)                       :: nng_stream_dialer_set_ptr
        end function nng_stream_dialer_set_ptr

        ! int nng_stream_dialer_set_size(nng_stream_dialer *d, const char *opt, size_t z)
        function nng_stream_dialer_set_size(d, opt, z) bind(c, name='nng_stream_dialer_set_size')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(in), value :: z
            integer(c_int)                       :: nng_stream_dialer_set_size
        end function nng_stream_dialer_set_size

        ! int nng_stream_dialer_set_string(nng_stream_dialer *d, const char *opt, const char *str)
        function nng_stream_dialer_set_string(d, opt, str) bind(c, name='nng_stream_dialer_set_string')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: d
            character(c_char), intent(in)        :: opt
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: nng_stream_dialer_set_string
        end function nng_stream_dialer_set_string

        ! int nng_stream_dialer_set_uint64(nng_stream_dialer *d, const char *opt, uint64_t u64)
        function nng_stream_dialer_set_uint64(d, opt, u64) bind(c, name='nng_stream_dialer_set_uint64')
            import :: c_char, c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: d
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(in), value :: u64
            integer(c_int)                         :: nng_stream_dialer_set_uint64
        end function nng_stream_dialer_set_uint64

        ! void nng_stream_free(nng_stream *stream)
        subroutine nng_stream_free_(stream) bind(c, name='nng_stream_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stream
        end subroutine nng_stream_free_

        ! int nng_stream_get(nng_stream *stream, const char *opt, void *val, size_t *valszp)
        function nng_stream_get(stream, opt, val, valszp) bind(c, name='nng_stream_get')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(out)       :: valszp
            integer(c_int)                       :: nng_stream_get
        end function nng_stream_get

        ! int nng_stream_get_addr(nng_stream *stream, const char *opt, nng_sockaddr *sap)
        function nng_stream_get_addr(stream, opt, sap) bind(c, name='nng_stream_get_addr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),        intent(in), value :: stream
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(out)       :: sap
            integer(c_int)                        :: nng_stream_get_addr
        end function nng_stream_get_addr

        ! int nng_stream_get_bool(nng_stream *stream, const char *opt, bool *bvalp)
        function nng_stream_get_bool(stream, opt, bvalp) bind(c, name='nng_stream_get_bool')
            import :: c_bool, c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(out)       :: bvalp
            integer(c_int)                       :: nng_stream_get_bool
        end function nng_stream_get_bool

        ! int nng_stream_get_int(nng_stream *stream, const char *opt, int *ivalp)
        function nng_stream_get_int(stream, opt, ivalp) bind(c, name='nng_stream_get_int')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(out)       :: ivalp
            integer(c_int)                       :: nng_stream_get_int
        end function nng_stream_get_int

        ! int nng_stream_get_ms(nng_stream *stream, const char *opt, nng_duration *durp)
        function nng_stream_get_ms(stream, opt, durp) bind(c, name='nng_stream_get_ms')
            import :: c_char, c_int, c_ptr, nng_duration
            implicit none
            type(c_ptr),           intent(in), value :: stream
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(out)       :: durp
            integer(c_int)                           :: nng_stream_get_ms
        end function nng_stream_get_ms

        ! int nng_stream_get_ptr(nng_stream *stream, const char *opt, void **ptr)
        function nng_stream_get_ptr(stream, opt, ptr) bind(c, name='nng_stream_get_ptr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: ptr
            integer(c_int)                       :: nng_stream_get_ptr
        end function nng_stream_get_ptr

        ! int nng_stream_get_size(nng_stream *stream, const char *opt, size_t *zp)
        function nng_stream_get_size(stream, opt, zp) bind(c, name='nng_stream_get_size')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(out)       :: zp
            integer(c_int)                       :: nng_stream_get_size
        end function nng_stream_get_size

        ! int nng_stream_get_string(nng_stream *stream, const char *opt, char **strp)
        function nng_stream_get_string(stream, opt, strp) bind(c, name='nng_stream_get_string')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: strp
            integer(c_int)                       :: nng_stream_get_string
        end function nng_stream_get_string

        ! int nng_stream_get_uint64(nng_stream *stream, const char *opt, uint64_t *u64p)
        function nng_stream_get_uint64(stream, opt, u64p) bind(c, name='nng_stream_get_uint64')
            import :: c_char, c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: stream
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(out)       :: u64p
            integer(c_int)                         :: nng_stream_get_uint64
        end function nng_stream_get_uint64

        ! void nng_stream_listener_accept(nng_stream_listener *l, nng_aio *aio)
        subroutine nng_stream_listener_accept(l, aio) bind(c, name='nng_stream_listener_accept')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_stream_listener_accept

        ! int nng_stream_listener_alloc(nng_stream_listener **lp, const char *addr)
        function nng_stream_listener_alloc(lp, addr) bind(c, name='nng_stream_listener_alloc')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(out) :: lp
            character(c_char), intent(in)  :: addr
            integer(c_int)                 :: nng_stream_listener_alloc
        end function nng_stream_listener_alloc

        ! int nng_stream_listener_alloc_url(nng_stream_listener **lp, const nng_url *url)
        function nng_stream_listener_alloc_url(lp, url) bind(c, name='nng_stream_listener_alloc_url')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out)       :: lp
            type(c_ptr), intent(in), value :: url
            integer(c_int)                 :: nng_stream_listener_alloc_url
        end function nng_stream_listener_alloc_url

        ! void nng_stream_listener_close(nng_stream_listener *l)
        subroutine nng_stream_listener_close(l) bind(c, name='nng_stream_listener_close')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
        end subroutine nng_stream_listener_close

        ! void nng_stream_listener_free(nng_stream_listener *l)
        subroutine nng_stream_listener_free_(l) bind(c, name='nng_stream_listener_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
        end subroutine nng_stream_listener_free_

        ! int nng_stream_listener_get(nng_stream_listener *l, const char *opt, void *val, size_t *valszp)
        function nng_stream_listener_get(l, opt, val, valszp) bind(c, name='nng_stream_listener_get')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(out)       :: valszp
            integer(c_int)                       :: nng_stream_listener_get
        end function nng_stream_listener_get

        ! int nng_stream_listener_get_addr(nng_stream_listener *l, const char *opt, nng_sockaddr *sap)
        function nng_stream_listener_get_addr(l, opt, sap) bind(c, name='nng_stream_listener_get_addr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),        intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(out)       :: sap
            integer(c_int)                        :: nng_stream_listener_get_addr
        end function nng_stream_listener_get_addr

        ! int nng_stream_listener_get_bool(nng_stream_listener *l, const char *opt, bool *bvalp)
        function nng_stream_listener_get_bool(l, opt, bvalp) bind(c, name='nng_stream_listener_get_bool')
            import :: c_bool, c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(out)       :: bvalp
            integer(c_int)                       :: nng_stream_listener_get_bool
        end function nng_stream_listener_get_bool

        ! int nng_stream_listener_get_int(nng_stream_listener *l, const char *opt, int *ivalp)
        function nng_stream_listener_get_int(l, opt, ivalp) bind(c, name='nng_stream_listener_get_int')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(out)       :: ivalp
            integer(c_int)                       :: nng_stream_listener_get_int
        end function nng_stream_listener_get_int

        ! int nng_stream_listener_get_ms(nng_stream_listener *l, const char *opt, nng_duration *durp)
        function nng_stream_listener_get_ms(l, opt, durp) bind(c, name='nng_stream_listener_get_ms')
            import :: c_char, c_int, c_ptr, nng_duration
            implicit none
            type(c_ptr),           intent(in), value :: l
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(out)       :: durp
            integer(c_int)                           :: nng_stream_listener_get_ms
        end function nng_stream_listener_get_ms

        ! int nng_stream_listener_get_ptr(nng_stream_listener *l, const char *opt, void **ptr)
        function nng_stream_listener_get_ptr(l, opt, ptr) bind(c, name='nng_stream_listener_get_ptr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: ptr
            integer(c_int)                       :: nng_stream_listener_get_ptr
        end function nng_stream_listener_get_ptr

        ! int nng_stream_listener_get_size(nng_stream_listener *l, const char *opt, size_t *zp)
        function nng_stream_listener_get_size(l, opt, zp) bind(c, name='nng_stream_listener_get_size')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(out)       :: zp
            integer(c_int)                       :: nng_stream_listener_get_size
        end function nng_stream_listener_get_size

        ! int nng_stream_listener_get_string(nng_stream_listener *l, const char *opt, char **strp)
        function nng_stream_listener_get_string(l, opt, strp) bind(c, name='nng_stream_listener_get_string')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(out)       :: strp
            integer(c_int)                       :: nng_stream_listener_get_string
        end function nng_stream_listener_get_string

        ! int nng_stream_listener_get_uint64(nng_stream_listener *l, const char *opt, uint64_t *u64p)
        function nng_stream_listener_get_uint64(l, opt, u64p) bind(c, name='nng_stream_listener_get_uint64')
            import :: c_char, c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: l
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(out)       :: u64p
            integer(c_int)                         :: nng_stream_listener_get_uint64
        end function nng_stream_listener_get_uint64

        ! int nng_stream_listener_listen(nng_stream_listener *l)
        function nng_stream_listener_listen(l) bind(c, name='nng_stream_listener_listen')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
            integer(c_int)                 :: nng_stream_listener_listen
        end function nng_stream_listener_listen

        ! int nng_stream_listener_set(nng_stream_listener *l, const char *opt, const void *val, size_t valsz)
        function nng_stream_listener_set(l, opt, val, valsz) bind(c, name='nng_stream_listener_set')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(in), value :: valsz
            integer(c_int)                       :: nng_stream_listener_set
        end function nng_stream_listener_set

        ! int nng_stream_listener_set_addr(nng_stream_listener *l, const char *opt, const nng_sockaddr *sa)
        function nng_stream_listener_set_addr(l, opt, sa) bind(c, name='nng_stream_listener_set_addr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),        intent(in), value :: l
            character(c_char),  intent(in)        :: opt
            type(c_ptr),        intent(in), value :: sa
            integer(c_int)                        :: nng_stream_listener_set_addr
        end function nng_stream_listener_set_addr

        ! int nng_stream_listener_set_bool(nng_stream_listener *l, const char *opt, bool bval)
        function nng_stream_listener_set_bool(l, opt, bval) bind(c, name='nng_stream_listener_set_bool')
            import :: c_bool, c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(in), value :: bval
            integer(c_int)                       :: nng_stream_listener_set_bool
        end function nng_stream_listener_set_bool

        ! int nng_stream_listener_set_int(nng_stream_listener *l, const char *opt, int ival)
        function nng_stream_listener_set_int(l, opt, ival) bind(c, name='nng_stream_listener_set_int')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(in), value :: ival
            integer(c_int)                       :: nng_stream_listener_set_int
        end function nng_stream_listener_set_int

        ! int nng_stream_listener_set_ms(nng_stream_listener *l, const char *opt, nng_duration dur)
        function nng_stream_listener_set_ms(l, opt, dur) bind(c, name='nng_stream_listener_set_ms')
            import :: c_char, c_int, c_ptr, nng_duration
            implicit none
            type(c_ptr),           intent(in), value :: l
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(in), value :: dur
            integer(c_int)                           :: nng_stream_listener_set_ms
        end function nng_stream_listener_set_ms

        ! int nng_stream_listener_set_ptr(nng_stream_listener *l, const char *opt, void *ptr)
        function nng_stream_listener_set_ptr(l, opt, ptr) bind(c, name='nng_stream_listener_set_ptr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: ptr
            integer(c_int)                       :: nng_stream_listener_set_ptr
        end function nng_stream_listener_set_ptr

        ! int nng_stream_listener_set_size(nng_stream_listener *l, const char *opt, size_t z)
        function nng_stream_listener_set_size(l, opt, z) bind(c, name='nng_stream_listener_set_size')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(in), value :: z
            integer(c_int)                       :: nng_stream_listener_set_size
        end function nng_stream_listener_set_size

        ! int nng_stream_listener_set_string(nng_stream_listener *l, const char *opt, const char *str)
        function nng_stream_listener_set_string(l, opt, str) bind(c, name='nng_stream_listener_set_string')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: l
            character(c_char), intent(in)        :: opt
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: nng_stream_listener_set_string
        end function nng_stream_listener_set_string

        ! int nng_stream_listener_set_uint64(nng_stream_listener *l, const char *opt, uint64_t u64)
        function nng_stream_listener_set_uint64(l, opt, u64) bind(c, name='nng_stream_listener_set_uint64')
            import :: c_char, c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: l
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(in), value :: u64
            integer(c_int)                         :: nng_stream_listener_set_uint64
        end function nng_stream_listener_set_uint64

        ! void nng_stream_recv(nng_stream *stream, nng_aio *aio)
        subroutine nng_stream_recv(stream, aio) bind(c, name='nng_stream_recv')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stream
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_stream_recv

        ! void nng_stream_send(nng_stream *stream, nng_aio *aio)
        subroutine nng_stream_send(stream, aio) bind(c, name='nng_stream_send')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stream
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_stream_send

        ! int nng_stream_set(nng_stream *stream, const char *opt, const void *val, size_t valsz)
        function nng_stream_set(stream, opt, val, valsz) bind(c, name='nng_stream_set')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: val
            integer(c_size_t), intent(in), value :: valsz
            integer(c_int)                       :: nng_stream_set
        end function nng_stream_set

        ! int nng_stream_set_bool(nng_stream *stream, const char *opt, bool bval)
        function nng_stream_set_bool(stream, opt, bval) bind(c, name='nng_stream_set_bool')
            import :: c_bool, c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            logical(c_bool),   intent(in), value :: bval
            integer(c_int)                       :: nng_stream_set_bool
        end function nng_stream_set_bool

        ! int nng_stream_set_int(nng_stream *stream, const char *opt, int ival)
        function nng_stream_set_int(stream, opt, ival) bind(c, name='nng_stream_set_int')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            integer(c_int),    intent(in), value :: ival
            integer(c_int)                       :: nng_stream_set_int
        end function nng_stream_set_int

        ! int nng_stream_set_ms(nng_stream *stream, const char *opt, nng_duration dur)
        function nng_stream_set_ms(stream, opt, dur) bind(c, name='nng_stream_set_ms')
            import :: c_char, c_int, c_ptr, nng_duration
            implicit none
            type(c_ptr),           intent(in), value :: stream
            character(c_char),     intent(in)        :: opt
            integer(nng_duration), intent(in), value :: dur
            integer(c_int)                           :: nng_stream_set_ms
        end function nng_stream_set_ms

        ! int nng_stream_set_ptr(nng_stream *stream, const char *opt, void *ptr)
        function nng_stream_set_ptr(stream, opt, ptr) bind(c, name='nng_stream_set_ptr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            type(c_ptr),       intent(in), value :: ptr
            integer(c_int)                       :: nng_stream_set_ptr
        end function nng_stream_set_ptr

        ! int nng_stream_set_size(nng_stream *stream, const char *opt, size_t z)
        function nng_stream_set_size(stream, opt, z) bind(c, name='nng_stream_set_size')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            integer(c_size_t), intent(in), value :: z
            integer(c_int)                       :: nng_stream_set_size
        end function nng_stream_set_size

        ! int nng_stream_set_string(nng_stream *stream, const char *opt, const char *str)
        function nng_stream_set_string(stream, opt, str) bind(c, name='nng_stream_set_string')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: stream
            character(c_char), intent(in)        :: opt
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: nng_stream_set_string
        end function nng_stream_set_string

        ! int nng_stream_set_uint64(nng_stream *stream, const char *opt, uint64_t u64)
        function nng_stream_set_uint64(stream, opt, u64) bind(c, name='nng_stream_set_uint64')
            import :: c_char, c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: stream
            character(c_char),   intent(in)        :: opt
            integer(c_uint64_t), intent(in), value :: u64
            integer(c_int)                         :: nng_stream_set_uint64
        end function nng_stream_set_uint64

        ! const char *nng_strerror(int err)
        function nng_strerror_(err) bind(c, name='nng_strerror')
            import :: c_int, c_ptr
            implicit none
            integer(c_int), intent(in), value :: err
            type(c_ptr)                       :: nng_strerror_
        end function nng_strerror_

        ! void nng_strfree(char *str)
        subroutine nng_strfree_(str) bind(c, name='nng_strfree')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: str
        end subroutine nng_strfree_

        ! void nng_system_logger(nng_log_level level, nng_log_facility facility, const char *msgid, const char *msg)
        subroutine nng_system_logger(level, facility, msgid, msg) bind(c, name='nng_system_logger')
            import :: c_char, c_int
            implicit none
            integer(c_int),    intent(in), value :: level
            integer(c_int),    intent(in), value :: facility
            character(c_char), intent(in)        :: msgid
            character(c_char), intent(in)        :: msg
        end subroutine nng_system_logger

        ! int nng_thread_create(nng_thread **threadp, void (*fn)(void *), void *arg)
        function nng_thread_create(threadp, fn, arg) bind(c, name='nng_thread_create')
            import :: c_funptr, c_int, c_ptr
            implicit none
            type(c_ptr),    intent(out)       :: threadp
            type(c_funptr), intent(in), value :: fn
            type(c_ptr),    intent(in), value :: arg
            integer(c_int)                    :: nng_thread_create
        end function nng_thread_create

        ! void nng_thread_destroy(nng_thread *thread)
        subroutine nng_thread_destroy(thread) bind(c, name='nng_thread_destroy')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: thread
        end subroutine nng_thread_destroy

        ! void nng_thread_set_name(nng_thread *thread, const char *name)
        subroutine nng_thread_set_name(thread, name) bind(c, name='nng_thread_set_name')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: thread
            character(c_char), intent(in)        :: name
        end subroutine nng_thread_set_name

        ! void nng_udp_close(nng_udp *udp)
        subroutine nng_udp_close(udp) bind(c, name='nng_udp_close')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: udp
        end subroutine nng_udp_close

        ! int nng_udp_multicast_membership(nng_udp *udp, nng_sockaddr *sa, bool join)
        function nng_udp_multicast_membership(udp, sa, join) bind(c, name='nng_udp_multicast_membership')
            import :: c_bool, c_int, c_ptr
            implicit none
            type(c_ptr),     intent(in), value :: udp
            type(c_ptr),     intent(in), value :: sa
            logical(c_bool), intent(in), value :: join
            integer(c_int)                     :: nng_udp_multicast_membership
        end function nng_udp_multicast_membership

        ! int nng_udp_open(nng_udp **udpp, nng_sockaddr *sa)
        function nng_udp_open(udpp, sa) bind(c, name='nng_udp_open')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out)       :: udpp
            type(c_ptr), intent(in), value :: sa
            integer(c_int)                 :: nng_udp_open
        end function nng_udp_open

        ! void nng_udp_recv(nng_udp *udp, nng_aio *aio)
        subroutine nng_udp_recv(udp, aio) bind(c, name='nng_udp_recv')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: udp
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_udp_recv

        ! void nng_udp_send(nng_udp *udp, nng_aio *aio)
        subroutine nng_udp_send(udp, aio) bind(c, name='nng_udp_send')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: udp
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_udp_send

        ! int nng_udp_sockname(nng_udp *udp, nng_sockaddr *sa)
        function nng_udp_sockname(udp, sa) bind(c, name='nng_udp_sockname')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: udp
            type(c_ptr), intent(in), value :: sa
            integer(c_int)                 :: nng_udp_sockname
        end function nng_udp_sockname

        ! int nng_url_clone(nng_url **dup, const nng_url *orig)
        function nng_url_clone(dup, orig) bind(c, name='nng_url_clone')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out)       :: dup
            type(c_ptr), intent(in), value :: orig
            integer(c_int)                 :: nng_url_clone
        end function nng_url_clone

        ! void nng_url_free(nng_url *url)
        subroutine nng_url_free_(url) bind(c, name='nng_url_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: url
        end subroutine nng_url_free_

        ! int nng_url_parse(nng_url **urlp, const char *str)
        function nng_url_parse(urlp, str) bind(c, name='nng_url_parse')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(out) :: urlp
            character(c_char), intent(in)  :: str
            integer(c_int)                 :: nng_url_parse
        end function nng_url_parse

        ! const char *nng_version(void)
        function nng_version_() bind(c, name='nng_version')
            import :: c_ptr
            implicit none
            type(c_ptr) :: nng_version_
        end function nng_version_
    end interface
contains
    ! void nng_aio_free(nng_aio *aio)
    subroutine nng_aio_free(aio)
        type(c_ptr), intent(inout) :: aio

        call nng_aio_free_(aio)
        aio = c_null_ptr
    end subroutine nng_aio_free

    ! void nng_cv_free(nng_cv *cv)
    subroutine nng_cv_free(cv)
        type(c_ptr), intent(inout) :: cv

        call nng_cv_free_(cv)
        cv = c_null_ptr
    end subroutine nng_cv_free

    ! void nng_free(void *ptr, size_t size)
    subroutine nng_free(ptr, size)
        type(c_ptr),       intent(inout) :: ptr
        integer(c_size_t), intent(in)    :: size

        call nng_free_(ptr, size)
        ptr = c_null_ptr
    end subroutine nng_free

    ! void nng_msg_free(nng_msg *msg)
    subroutine nng_msg_free(msg)
        type(c_ptr), intent(inout) :: msg

        call nng_msg_free_(msg)
        msg = c_null_ptr
    end subroutine nng_msg_free

    ! void nng_mtx_free(nng_mtx *mtx)
    subroutine nng_mtx_free(mtx)
        type(c_ptr), intent(inout) :: mtx

        call nng_mtx_free_(mtx)
        mtx = c_null_ptr
    end subroutine nng_mtx_free

    ! const char *nng_stat_desc(nng_stat *stat)
    function nng_stat_desc(stat) result(str)
        type(c_ptr), intent(in)   :: stat
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_stat_desc_(stat)
        call c_f_str_ptr(ptr, str)
    end function nng_stat_desc

    ! const char *nng_stat_name(nng_stat *stat)
    function nng_stat_name(stat) result(str)
        type(c_ptr), intent(in)   :: stat
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_stat_name_(stat)
        call c_f_str_ptr(ptr, str)
    end function nng_stat_name

    ! const char *nng_stat_string(nng_stat *stat)
    function nng_stat_string(stat) result(str)
        type(c_ptr), intent(in)   :: stat
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_stat_string_(stat)
        call c_f_str_ptr(ptr, str)
    end function nng_stat_string

    ! void nng_stats_free(nng_stat *stat)
    subroutine nng_stats_free(stat)
        type(c_ptr), intent(inout) :: stat

        call nng_stats_free_(stat)
        stat = c_null_ptr
    end subroutine nng_stats_free

    ! const char *nng_str_sockaddr(const nng_sockaddr *sa, char *buf, size_t bufsz)
    function nng_str_sockaddr(sa) result(str)
        type(c_ptr), intent(in)   :: sa
        character(:), allocatable :: str

        character(NNG_MAXADDRLEN) :: buf
        type(c_ptr)               :: ptr

        ptr = nng_str_sockaddr_(sa, buf, len(buf, c_size_t))
        call c_f_str_ptr(ptr, str)
    end function nng_str_sockaddr

    ! void nng_stream_dialer_free(nng_stream_dialer *d)
    subroutine nng_stream_dialer_free(d)
        type(c_ptr), intent(inout) :: d

        call nng_stream_dialer_free_(d)
        d = c_null_ptr
    end subroutine nng_stream_dialer_free

    ! void nng_stream_free(nng_stream *stream)
    subroutine nng_stream_free(stream)
        type(c_ptr), intent(inout) :: stream

        call nng_stream_free_(stream)
        stream = c_null_ptr
    end subroutine nng_stream_free

    ! void nng_stream_listener_free(nng_stream_listener *l)
    subroutine nng_stream_listener_free(l)
        type(c_ptr), intent(inout) :: l

        call nng_stream_listener_free_(l)
        l = c_null_ptr
    end subroutine nng_stream_listener_free

    ! const char *nng_strerror(int err)
    function nng_strerror(err) result(str)
        integer, intent(in)       :: err
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_strerror_(err)
        call c_f_str_ptr(ptr, str)
    end function nng_strerror

    ! void nng_strfree(char *str)
    subroutine nng_strfree(str)
        type(c_ptr), intent(inout) :: str

        call nng_strfree_(str)
        str = c_null_ptr
    end subroutine nng_strfree

    ! void nng_url_free(nng_url *url)
    subroutine nng_url_free(url)
        type(c_ptr), intent(inout) :: url

        call nng_url_free_(url)
        url = c_null_ptr
    end subroutine nng_url_free

    ! const char *nng_version(void)
    function nng_version() result(str)
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_version_()
        call c_f_str_ptr(ptr, str)
    end function nng_version
end module nng
