! zmq.f90
!
! Author:  Philipp Engel
! Licence: ISC
module zmq
    !! Auto-generated Fortran 2018 interface bindings to libzmq 4.
    use :: zmq_util
    implicit none (type, external)
    public

    type, bind(c), public :: zmq_msg_t
        private
        integer(c_signed_char) :: data(64) = 0_c_signed_char
    end type zmq_msg_t

    type, bind(c), public :: zmq_pollitem_t
        type(c_ptr)       :: socket  = c_null_ptr
        integer(zmq_fd_t) :: fd      = 0_zmq_fd_t
        integer(c_short)  :: events  = 0_c_short
        integer(c_short)  :: revents = 0_c_short
    end type zmq_pollitem_t

    type, bind(c), public :: zmq_poller_event_t
        type(c_ptr)       :: socket    = c_null_ptr
        integer(zmq_fd_t) :: fd        = 0_zmq_fd_t
        type(c_ptr)       :: user_data = c_null_ptr
        integer(c_short)  :: events    = 0_c_short
    end type zmq_poller_event_t

    ! A number random enough not to collide with different errno ranges on
    ! different OSes. The assumption is that error_t is at least 32-bit type.
    integer(c_int), parameter, public :: ZMQ_HAUSNUMERO = 156384712

    ! On Windows platform some of the standard POSIX errnos are not defined.
    integer(c_int), parameter, public :: ENOTSUP         = ZMQ_HAUSNUMERO + 1
    integer(c_int), parameter, public :: EPROTONOSUPPORT = ZMQ_HAUSNUMERO + 2
    integer(c_int), parameter, public :: ENOBUFS         = ZMQ_HAUSNUMERO + 3
    integer(c_int), parameter, public :: ENETDOWN        = ZMQ_HAUSNUMERO + 4
    integer(c_int), parameter, public :: EADDRINUSE      = ZMQ_HAUSNUMERO + 5
    integer(c_int), parameter, public :: EADDRNOTAVAIL   = ZMQ_HAUSNUMERO + 6
    integer(c_int), parameter, public :: ECONNREFUSED    = ZMQ_HAUSNUMERO + 7
    integer(c_int), parameter, public :: EINPROGRESS     = ZMQ_HAUSNUMERO + 8
    integer(c_int), parameter, public :: ENOTSOCK        = ZMQ_HAUSNUMERO + 9
    integer(c_int), parameter, public :: EMSGSIZE        = ZMQ_HAUSNUMERO + 10
    integer(c_int), parameter, public :: EAFNOSUPPORT    = ZMQ_HAUSNUMERO + 11
    integer(c_int), parameter, public :: ENETUNREACH     = ZMQ_HAUSNUMERO + 12
    integer(c_int), parameter, public :: ECONNABORTED    = ZMQ_HAUSNUMERO + 13
    integer(c_int), parameter, public :: ECONNRESET      = ZMQ_HAUSNUMERO + 14
    integer(c_int), parameter, public :: ENOTCONN        = ZMQ_HAUSNUMERO + 15
    integer(c_int), parameter, public :: ETIMEDOUT       = ZMQ_HAUSNUMERO + 16
    integer(c_int), parameter, public :: EHOSTUNREACH    = ZMQ_HAUSNUMERO + 17
    integer(c_int), parameter, public :: ENETRESET       = ZMQ_HAUSNUMERO + 18

    ! Native 0MQ error codes.
    integer(c_int), parameter, public :: EFSM            = ZMQ_HAUSNUMERO + 51
    integer(c_int), parameter, public :: ENOCOMPATPROTO  = ZMQ_HAUSNUMERO + 52
    integer(c_int), parameter, public :: ETERM           = ZMQ_HAUSNUMERO + 53
    integer(c_int), parameter, public :: EMTHREAD        = ZMQ_HAUSNUMERO + 54

    ! Context options.
    integer(c_int), parameter, public :: ZMQ_IO_THREADS                 = 1
    integer(c_int), parameter, public :: ZMQ_MAX_SOCKETS                = 2
    integer(c_int), parameter, public :: ZMQ_SOCKET_LIMIT               = 3
    integer(c_int), parameter, public :: ZMQ_THREAD_PRIORITY            = 3
    integer(c_int), parameter, public :: ZMQ_THREAD_SCHED_POLICY        = 4
    integer(c_int), parameter, public :: ZMQ_MAX_MSGSZ                  = 5
    integer(c_int), parameter, public :: ZMQ_MSG_T_SIZE                 = 6
    integer(c_int), parameter, public :: ZMQ_THREAD_AFFINITY_CPU_ADD    = 7
    integer(c_int), parameter, public :: ZMQ_THREAD_AFFINITY_CPU_REMOVE = 8
    integer(c_int), parameter, public :: ZMQ_THREAD_NAME_PREFIX         = 9

    ! Default for new contexts.
    integer(c_int), parameter, public :: ZMQ_IO_THREADS_DFLT          = 1
    integer(c_int), parameter, public :: ZMQ_MAX_SOCKETS_DFLT         = 1023
    integer(c_int), parameter, public :: ZMQ_THREAD_PRIORITY_DFLT     = -1
    integer(c_int), parameter, public :: ZMQ_THREAD_SCHED_POLICY_DFLT = -1

    ! Socket types.
    integer(c_int), parameter, public :: ZMQ_PAIR    = 0
    integer(c_int), parameter, public :: ZMQ_PUB     = 1
    integer(c_int), parameter, public :: ZMQ_SUB     = 2
    integer(c_int), parameter, public :: ZMQ_REQ     = 3
    integer(c_int), parameter, public :: ZMQ_REP     = 4
    integer(c_int), parameter, public :: ZMQ_DEALER  = 5
    integer(c_int), parameter, public :: ZMQ_ROUTER  = 6
    integer(c_int), parameter, public :: ZMQ_PULL    = 7
    integer(c_int), parameter, public :: ZMQ_PUSH    = 8
    integer(c_int), parameter, public :: ZMQ_XPUB    = 9
    integer(c_int), parameter, public :: ZMQ_XSUB    = 10
    integer(c_int), parameter, public :: ZMQ_STREAM  = 11

    ! Socket options.
    integer(c_int), parameter, public :: ZMQ_AFFINITY                          = 4
    integer(c_int), parameter, public :: ZMQ_ROUTING_ID                        = 5
    integer(c_int), parameter, public :: ZMQ_SUBSCRIBE                         = 6
    integer(c_int), parameter, public :: ZMQ_UNSUBSCRIBE                       = 7
    integer(c_int), parameter, public :: ZMQ_RATE                              = 8
    integer(c_int), parameter, public :: ZMQ_RECOVERY_IVL                      = 9
    integer(c_int), parameter, public :: ZMQ_SNDBUF                            = 11
    integer(c_int), parameter, public :: ZMQ_RCVBUF                            = 12
    integer(c_int), parameter, public :: ZMQ_RCVMORE                           = 13
    integer(c_int), parameter, public :: ZMQ_FD                                = 14
    integer(c_int), parameter, public :: ZMQ_EVENTS                            = 15
    integer(c_int), parameter, public :: ZMQ_TYPE                              = 16
    integer(c_int), parameter, public :: ZMQ_LINGER                            = 17
    integer(c_int), parameter, public :: ZMQ_RECONNECT_IVL                     = 18
    integer(c_int), parameter, public :: ZMQ_BACKLOG                           = 19
    integer(c_int), parameter, public :: ZMQ_RECONNECT_IVL_MAX                 = 21
    integer(c_int), parameter, public :: ZMQ_MAXMSGSIZE                        = 22
    integer(c_int), parameter, public :: ZMQ_SNDHWM                            = 23
    integer(c_int), parameter, public :: ZMQ_RCVHWM                            = 24
    integer(c_int), parameter, public :: ZMQ_MULTICAST_HOPS                    = 25
    integer(c_int), parameter, public :: ZMQ_RCVTIMEO                          = 27
    integer(c_int), parameter, public :: ZMQ_SNDTIMEO                          = 28
    integer(c_int), parameter, public :: ZMQ_LAST_ENDPOINT                     = 32
    integer(c_int), parameter, public :: ZMQ_ROUTER_MANDATORY                  = 33
    integer(c_int), parameter, public :: ZMQ_TCP_KEEPALIVE                     = 34
    integer(c_int), parameter, public :: ZMQ_TCP_KEEPALIVE_CNT                 = 35
    integer(c_int), parameter, public :: ZMQ_TCP_KEEPALIVE_IDLE                = 36
    integer(c_int), parameter, public :: ZMQ_TCP_KEEPALIVE_INTVL               = 37
    integer(c_int), parameter, public :: ZMQ_IMMEDIATE                         = 39
    integer(c_int), parameter, public :: ZMQ_XPUB_VERBOSE                      = 40
    integer(c_int), parameter, public :: ZMQ_ROUTER_RAW                        = 41
    integer(c_int), parameter, public :: ZMQ_IPV6                              = 42
    integer(c_int), parameter, public :: ZMQ_MECHANISM                         = 43
    integer(c_int), parameter, public :: ZMQ_PLAIN_SERVER                      = 44
    integer(c_int), parameter, public :: ZMQ_PLAIN_USERNAME                    = 45
    integer(c_int), parameter, public :: ZMQ_PLAIN_PASSWORD                    = 46
    integer(c_int), parameter, public :: ZMQ_CURVE_SERVER                      = 47
    integer(c_int), parameter, public :: ZMQ_CURVE_PUBLICKEY                   = 48
    integer(c_int), parameter, public :: ZMQ_CURVE_SECRETKEY                   = 49
    integer(c_int), parameter, public :: ZMQ_CURVE_SERVERKEY                   = 50
    integer(c_int), parameter, public :: ZMQ_PROBE_ROUTER                      = 51
    integer(c_int), parameter, public :: ZMQ_REQ_CORRELATE                     = 52
    integer(c_int), parameter, public :: ZMQ_REQ_RELAXED                       = 53
    integer(c_int), parameter, public :: ZMQ_CONFLATE                          = 54
    integer(c_int), parameter, public :: ZMQ_ZAP_DOMAIN                        = 55
    integer(c_int), parameter, public :: ZMQ_ROUTER_HANDOVER                   = 56
    integer(c_int), parameter, public :: ZMQ_TOS                               = 57
    integer(c_int), parameter, public :: ZMQ_CONNECT_ROUTING_ID                = 61
    integer(c_int), parameter, public :: ZMQ_GSSAPI_SERVER                     = 62
    integer(c_int), parameter, public :: ZMQ_GSSAPI_PRINCIPAL                  = 63
    integer(c_int), parameter, public :: ZMQ_GSSAPI_SERVICE_PRINCIPAL          = 64
    integer(c_int), parameter, public :: ZMQ_GSSAPI_PLAINTEXT                  = 65
    integer(c_int), parameter, public :: ZMQ_HANDSHAKE_IVL                     = 66
    integer(c_int), parameter, public :: ZMQ_SOCKS_PROXY                       = 68
    integer(c_int), parameter, public :: ZMQ_XPUB_NODROP                       = 69
    integer(c_int), parameter, public :: ZMQ_BLOCKY                            = 70
    integer(c_int), parameter, public :: ZMQ_XPUB_MANUAL                       = 71
    integer(c_int), parameter, public :: ZMQ_XPUB_WELCOME_MSG                  = 72
    integer(c_int), parameter, public :: ZMQ_STREAM_NOTIFY                     = 73
    integer(c_int), parameter, public :: ZMQ_INVERT_MATCHING                   = 74
    integer(c_int), parameter, public :: ZMQ_HEARTBEAT_IVL                     = 75
    integer(c_int), parameter, public :: ZMQ_HEARTBEAT_TTL                     = 76
    integer(c_int), parameter, public :: ZMQ_HEARTBEAT_TIMEOUT                 = 77
    integer(c_int), parameter, public :: ZMQ_XPUB_VERBOSER                     = 78
    integer(c_int), parameter, public :: ZMQ_CONNECT_TIMEOUT                   = 79
    integer(c_int), parameter, public :: ZMQ_TCP_MAXRT                         = 80
    integer(c_int), parameter, public :: ZMQ_THREAD_SAFE                       = 81
    integer(c_int), parameter, public :: ZMQ_MULTICAST_MAXTPDU                 = 84
    integer(c_int), parameter, public :: ZMQ_VMCI_BUFFER_SIZE                  = 85
    integer(c_int), parameter, public :: ZMQ_VMCI_BUFFER_MIN_SIZE              = 86
    integer(c_int), parameter, public :: ZMQ_VMCI_BUFFER_MAX_SIZE              = 87
    integer(c_int), parameter, public :: ZMQ_VMCI_CONNECT_TIMEOUT              = 88
    integer(c_int), parameter, public :: ZMQ_USE_FD                            = 89
    integer(c_int), parameter, public :: ZMQ_GSSAPI_PRINCIPAL_NAMETYPE         = 90
    integer(c_int), parameter, public :: ZMQ_GSSAPI_SERVICE_PRINCIPAL_NAMETYPE = 91
    integer(c_int), parameter, public :: ZMQ_BINDTODEVICE                      = 92

    ! Message options.
    integer(c_int), parameter, public :: ZMQ_MORE   = 1
    integer(c_int), parameter, public :: ZMQ_SHARED = 3

    ! Send/recv options.
    integer(c_int), parameter, public :: ZMQ_DONTWAIT = 1
    integer(c_int), parameter, public :: ZMQ_SNDMORE  = 2

    ! Security mechanisms.
    integer(c_int), parameter, public :: ZMQ_NULL   = 0
    integer(c_int), parameter, public :: ZMQ_PLAIN  = 1
    integer(c_int), parameter, public :: ZMQ_CURVE  = 2
    integer(c_int), parameter, public :: ZMQ_GSSAPI = 3

    integer(c_int), parameter, public :: ZMQ_GSSAPI_NT_HOSTBASED      = 0
    integer(c_int), parameter, public :: ZMQ_GSSAPI_NT_USER_NAME      = 1
    integer(c_int), parameter, public :: ZMQ_GSSAPI_NT_KRB5_PRINCIPAL = 2

    integer(c_int), parameter, public :: ZMQ_EVENT_CONNECTED                  = int(z'0001')
    integer(c_int), parameter, public :: ZMQ_EVENT_CONNECT_DELAYED            = int(z'0002')
    integer(c_int), parameter, public :: ZMQ_EVENT_CONNECT_RETRIED            = int(z'0004')
    integer(c_int), parameter, public :: ZMQ_EVENT_LISTENING                  = int(z'0008')
    integer(c_int), parameter, public :: ZMQ_EVENT_BIND_FAILED                = int(z'0010')
    integer(c_int), parameter, public :: ZMQ_EVENT_ACCEPTED                   = int(z'0020')
    integer(c_int), parameter, public :: ZMQ_EVENT_ACCEPT_FAILED              = int(z'0040')
    integer(c_int), parameter, public :: ZMQ_EVENT_CLOSED                     = int(z'0080')
    integer(c_int), parameter, public :: ZMQ_EVENT_CLOSE_FAILED               = int(z'0100')
    integer(c_int), parameter, public :: ZMQ_EVENT_DISCONNECTED               = int(z'0200')
    integer(c_int), parameter, public :: ZMQ_EVENT_MONITOR_STOPPED            = int(z'0400')
    integer(c_int), parameter, public :: ZMQ_EVENT_ALL                        = int(z'FFFF')
    integer(c_int), parameter, public :: ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL = int(z'0800')
    integer(c_int), parameter, public :: ZMQ_EVENT_HANDSHAKE_SUCCEEDED        = int(z'1000')
    integer(c_int), parameter, public :: ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL  = int(z'2000')

    integer(c_int), parameter, public :: ZMQ_EVENT_HANDSHAKE_FAILED_AUTH                       = int(z'4000')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_UNSPECIFIED                   = int(z'10000000')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_UNEXPECTED_COMMAND            = int(z'10000001')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_SEQUENCE              = int(z'10000002')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_KEY_EXCHANGE                  = int(z'10000003')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_UNSPECIFIED = int(z'10000011')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_MESSAGE     = int(z'10000012')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_HELLO       = int(z'10000013')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_INITIATE    = int(z'10000014')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_ERROR       = int(z'10000015')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_READY       = int(z'10000016')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_WELCOME     = int(z'10000017')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_METADATA              = int(z'10000018')

    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_CRYPTOGRAPHIC      = int(z'11000001')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZMTP_MECHANISM_MISMATCH = int(z'11000002')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZAP_UNSPECIFIED         = int(z'20000000')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZAP_MALFORMED_REPLY     = int(z'20000001')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZAP_BAD_REQUEST_ID      = int(z'20000002')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZAP_BAD_VERSION         = int(z'20000003')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZAP_INVALID_STATUS_CODE = int(z'20000004')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_ZAP_INVALID_METADATA    = int(z'20000005')
    integer(c_int), parameter, public :: ZMQ_PROTOCOL_ERROR_WS_UNSPECIFIED          = int(z'30000000')

    ! Deprecated I/O multiplexing.
    integer(c_int), parameter, public :: ZMQ_POLLIN  = 1
    integer(c_int), parameter, public :: ZMQ_POLLOUT = 2
    integer(c_int), parameter, public :: ZMQ_POLLERR = 4
    integer(c_int), parameter, public :: ZMQ_POLLPRI = 8

    ! Deprecated aliases
    integer(c_int), parameter, public :: ZMQ_STREAMER  = 1
    integer(c_int), parameter, public :: ZMQ_FORWARDER = 2
    integer(c_int), parameter, public :: ZMQ_QUEUE     = 3

    ! DRAFT: socket types.
    integer(c_int), parameter, public :: ZMQ_SERVER  = 12
    integer(c_int), parameter, public :: ZMQ_CLIENT  = 13
    integer(c_int), parameter, public :: ZMQ_RADIO   = 14
    integer(c_int), parameter, public :: ZMQ_DISH    = 15
    integer(c_int), parameter, public :: ZMQ_GATHER  = 16
    integer(c_int), parameter, public :: ZMQ_SCATTER = 17
    integer(c_int), parameter, public :: ZMQ_DGRAM   = 18
    integer(c_int), parameter, public :: ZMQ_PEER    = 19
    integer(c_int), parameter, public :: ZMQ_CHANNEL = 20

    ! DRAFT: socket options.
    integer(c_int), parameter, public :: ZMQ_ZAP_ENFORCE_DOMAIN       = 93
    integer(c_int), parameter, public :: ZMQ_LOOPBACK_FASTPATH        = 94
    integer(c_int), parameter, public :: ZMQ_METADATA                 = 95
    integer(c_int), parameter, public :: ZMQ_MULTICAST_LOOP           = 96
    integer(c_int), parameter, public :: ZMQ_ROUTER_NOTIFY            = 97
    integer(c_int), parameter, public :: ZMQ_XPUB_MANUAL_LAST_VALUE   = 98
    integer(c_int), parameter, public :: ZMQ_SOCKS_USERNAME           = 99
    integer(c_int), parameter, public :: ZMQ_SOCKS_PASSWORD           = 100
    integer(c_int), parameter, public :: ZMQ_IN_BATCH_SIZE            = 101
    integer(c_int), parameter, public :: ZMQ_OUT_BATCH_SIZE           = 102
    integer(c_int), parameter, public :: ZMQ_WSS_KEY_PEM              = 103
    integer(c_int), parameter, public :: ZMQ_WSS_CERT_PEM             = 104
    integer(c_int), parameter, public :: ZMQ_WSS_TRUST_PEM            = 105
    integer(c_int), parameter, public :: ZMQ_WSS_HOSTNAME             = 106
    integer(c_int), parameter, public :: ZMQ_WSS_TRUST_SYSTEM         = 107
    integer(c_int), parameter, public :: ZMQ_ONLY_FIRST_SUBSCRIBE     = 108
    integer(c_int), parameter, public :: ZMQ_RECONNECT_STOP           = 109
    integer(c_int), parameter, public :: ZMQ_HELLO_MSG                = 110
    integer(c_int), parameter, public :: ZMQ_DISCONNECT_MSG           = 111
    integer(c_int), parameter, public :: ZMQ_PRIORITY                 = 112
    integer(c_int), parameter, public :: ZMQ_BUSY_POLL                = 113
    integer(c_int), parameter, public :: ZMQ_HICCUP_MSG               = 114
    integer(c_int), parameter, public :: ZMQ_XSUB_VERBOSE_UNSUBSCRIBE = 115
    integer(c_int), parameter, public :: ZMQ_TOPICS_COUNT             = 116
    integer(c_int), parameter, public :: ZMQ_NORM_MODE                = 117
    integer(c_int), parameter, public :: ZMQ_NORM_UNICAST_NACK        = 118
    integer(c_int), parameter, public :: ZMQ_NORM_BUFFER_SIZE         = 119
    integer(c_int), parameter, public :: ZMQ_NORM_SEGMENT_SIZE        = 120
    integer(c_int), parameter, public :: ZMQ_NORM_BLOCK_SIZE          = 121
    integer(c_int), parameter, public :: ZMQ_NORM_NUM_PARITY          = 122
    integer(c_int), parameter, public :: ZMQ_NORM_NUM_AUTOPARITY      = 123
    integer(c_int), parameter, public :: ZMQ_NORM_PUSH                = 124

    ! DRAFT: ZMQ_NORM_MODE options.
    integer(c_int), parameter, public :: ZMQ_NORM_FIXED       = 0
    integer(c_int), parameter, public :: ZMQ_NORM_CC          = 1
    integer(c_int), parameter, public :: ZMQ_NORM_CCL         = 2
    integer(c_int), parameter, public :: ZMQ_NORM_CCE         = 3
    integer(c_int), parameter, public :: ZMQ_NORM_CCE_ECNONLY = 4

    ! DRAFT: ZMQ_RECONNECT_STOP options.
    integer(c_int), parameter, public :: ZMQ_RECONNECT_STOP_CONN_REFUSED     = int(z'1')
    integer(c_int), parameter, public :: ZMQ_RECONNECT_STOP_HANDSHAKE_FAILED = int(z'2')
    integer(c_int), parameter, public :: ZMQ_RECONNECT_STOP_AFTER_DISCONNECT = int(z'4')

    ! DRAFT: context options.
    integer(c_int), parameter, public :: ZMQ_ZERO_COPY_RECV = 10

    ! DRAFT: msg property names.
    character(*), parameter, public :: ZMQ_MSG_PROPERTY_ROUTING_ID   = 'Routing-Id'
    character(*), parameter, public :: ZMQ_MSG_PROPERTY_SOCKET_TYPE  = 'Socket-Type'
    character(*), parameter, public :: ZMQ_MSG_PROPERTY_USER_ID      = 'User-Id'
    character(*), parameter, public :: ZMQ_MSG_PROPERTY_PEER_ADDRESS = 'Peer-Address'

    ! Router notify options.
    integer(c_int), parameter, public :: ZMQ_NOTIFY_CONNECT    = 1
    integer(c_int), parameter, public :: ZMQ_NOTIFY_DISCONNECT = 2

    ! DRAFT: Socket monitoring events.
    integer(c_int), parameter, public :: ZMQ_EVENT_PIPES_STATS = int(z'10000')

    integer(c_int), parameter, public :: ZMQ_CURRENT_EVENT_VERSION       = 1
    integer(c_int), parameter, public :: ZMQ_CURRENT_EVENT_VERSION_DRAFT = 2

    integer(c_int), parameter, public :: ZMQ_EVENT_ALL_V1 = ZMQ_EVENT_ALL
    integer(c_int), parameter, public :: ZMQ_EVENT_ALL_V2 = ior(ZMQ_EVENT_ALL_V1, ZMQ_EVENT_PIPES_STATS)

    public :: zmq_free_fn
    public :: zmq_thread_fn
    public :: zmq_timer_fn

    abstract interface
        ! void zmq_free_fn(void *data, void *hint)
        subroutine zmq_free_fn(data, hint) bind(c)
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: data
            type(c_ptr), intent(in), value :: hint
        end subroutine zmq_free_fn

        ! void zmq_thread_fn(void *arg)
        subroutine zmq_thread_fn(arg) bind(c)
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: arg
        end subroutine zmq_thread_fn

        ! void zmq_timer_fn(int timer_id, void *arg)
        subroutine zmq_timer_fn(timer_id, arg) bind(c)
            import :: c_int, c_ptr
            implicit none
            integer(c_int), intent(in), value :: timer_id
            type(c_ptr),    intent(in), value :: arg
        end subroutine zmq_timer_fn
    end interface

    public :: zmq_atomic_counter_dec
    public :: zmq_atomic_counter_destroy
    public :: zmq_atomic_counter_inc
    public :: zmq_atomic_counter_new
    public :: zmq_atomic_counter_set
    public :: zmq_atomic_counter_value
    public :: zmq_bind
    public :: zmq_bind_
    public :: zmq_close
    public :: zmq_close_
    public :: zmq_connect
    public :: zmq_connect_
    public :: zmq_connect_peer
    public :: zmq_connect_peer_
    public :: zmq_ctx_destroy
    public :: zmq_ctx_get
    public :: zmq_ctx_get_ext
    public :: zmq_ctx_new
    public :: zmq_ctx_set
    public :: zmq_ctx_set_ext
    public :: zmq_ctx_shutdown
    public :: zmq_ctx_term
    public :: zmq_curve_keypair
    public :: zmq_curve_keypair_
    public :: zmq_curve_public
    public :: zmq_curve_public_
    public :: zmq_device
    public :: zmq_disconnect
    public :: zmq_disconnect_
    public :: zmq_errno
    public :: zmq_getsockopt
    public :: zmq_has
    public :: zmq_has_
    public :: zmq_init
    public :: zmq_join
    public :: zmq_join_
    public :: zmq_leave
    public :: zmq_leave_
    public :: zmq_msg_close
    public :: zmq_msg_copy
    public :: zmq_msg_data
    public :: zmq_msg_get
    public :: zmq_msg_gets
    public :: zmq_msg_gets_
    public :: zmq_msg_group
    public :: zmq_msg_group_
    public :: zmq_msg_init
    public :: zmq_msg_init_buffer
    public :: zmq_msg_init_data
    public :: zmq_msg_init_data_
    public :: zmq_msg_init_size
    public :: zmq_msg_more
    public :: zmq_msg_more_
    public :: zmq_msg_move
    public :: zmq_msg_recv
    public :: zmq_msg_routing_id
    public :: zmq_msg_send
    public :: zmq_msg_set
    public :: zmq_msg_set_group
    public :: zmq_msg_set_group_
    public :: zmq_msg_set_routing_id
    public :: zmq_msg_size
    public :: zmq_poll
    public :: zmq_poller_add
    public :: zmq_poller_add_fd
    public :: zmq_poller_destroy
    public :: zmq_poller_fd
    public :: zmq_poller_modify
    public :: zmq_poller_modify_fd
    public :: zmq_poller_new
    public :: zmq_poller_remove
    public :: zmq_poller_remove_fd
    public :: zmq_poller_size
    public :: zmq_poller_wait
    public :: zmq_poller_wait_all
    public :: zmq_ppoll
    public :: zmq_proxy
    public :: zmq_proxy_steerable
    public :: zmq_recv
    public :: zmq_recviov
    public :: zmq_recvmsg
    public :: zmq_send
    public :: zmq_send_const
    public :: zmq_sendiov
    public :: zmq_sendmsg
    public :: zmq_setsockopt
    public :: zmq_sleep
    public :: zmq_socket
    public :: zmq_socket_get_peer_state
    public :: zmq_socket_monitor
    public :: zmq_socket_monitor_
    public :: zmq_socket_monitor_pipes_stats
    public :: zmq_socket_monitor_versioned
    public :: zmq_socket_monitor_versioned_
    public :: zmq_stopwatch_intermediate
    public :: zmq_stopwatch_start
    public :: zmq_stopwatch_stop
    public :: zmq_strerror
    public :: zmq_strerror_
    public :: zmq_term
    public :: zmq_threadclose
    public :: zmq_threadstart
    public :: zmq_timers_add
    public :: zmq_timers_cancel
    public :: zmq_timers_destroy
    public :: zmq_timers_execute
    public :: zmq_timers_new
    public :: zmq_timers_reset
    public :: zmq_timers_set_interval
    public :: zmq_timers_timeout
    public :: zmq_unbind
    public :: zmq_unbind_
    public :: zmq_version
    public :: zmq_z85_decode
    public :: zmq_z85_decode_
    public :: zmq_z85_encode
    public :: zmq_z85_encode_

    interface
        ! int zmq_atomic_counter_dec(void *counter)
        function zmq_atomic_counter_dec(counter) bind(c, name='zmq_atomic_counter_dec')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: counter
            integer(c_int)                 :: zmq_atomic_counter_dec
        end function zmq_atomic_counter_dec

        ! void zmq_atomic_counter_destroy(void **counter_p)
        subroutine zmq_atomic_counter_destroy(counter_p) bind(c, name='zmq_atomic_counter_destroy')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(inout) :: counter_p
        end subroutine zmq_atomic_counter_destroy

        ! int zmq_atomic_counter_inc(void *counter)
        function zmq_atomic_counter_inc(counter) bind(c, name='zmq_atomic_counter_inc')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: counter
            integer(c_int)                 :: zmq_atomic_counter_inc
        end function zmq_atomic_counter_inc

        ! void *zmq_atomic_counter_new(void)
        function zmq_atomic_counter_new() bind(c, name='zmq_atomic_counter_new')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zmq_atomic_counter_new
        end function zmq_atomic_counter_new

        ! void zmq_atomic_counter_set(void *counter, int value)
        subroutine zmq_atomic_counter_set(counter, value) bind(c, name='zmq_atomic_counter_set')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: counter
            integer(c_int), intent(in), value :: value
        end subroutine zmq_atomic_counter_set

        ! int zmq_atomic_counter_value(void *counter)
        function zmq_atomic_counter_value(counter) bind(c, name='zmq_atomic_counter_value')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: counter
            integer(c_int)                 :: zmq_atomic_counter_value
        end function zmq_atomic_counter_value

        ! int zmq_bind(void *s, const char *addr)
        function zmq_bind_(s, addr) bind(c, name='zmq_bind')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: s
            character(c_char), intent(in)        :: addr
            integer(c_int)                       :: zmq_bind_
        end function zmq_bind_

        ! int zmq_close(void *s)
        function zmq_close_(s) bind(c, name='zmq_close')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: s
            integer(c_int)                 :: zmq_close_
        end function zmq_close_

        ! int zmq_connect(void *s, const char *addr)
        function zmq_connect_(s, addr) bind(c, name='zmq_connect')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: s
            character(c_char), intent(in)        :: addr
            integer(c_int)                       :: zmq_connect_
        end function zmq_connect_

        ! uint32_t zmq_connect_peer(void *s, const char *addr)
        function zmq_connect_peer_(s, addr) bind(c, name='zmq_connect_peer')
            import :: c_char, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),       intent(in), value :: s
            character(c_char), intent(in)        :: addr
            integer(c_uint32_t)                  :: zmq_connect_peer_
        end function zmq_connect_peer_

        ! int zmq_ctx_destroy(void *context)
        function zmq_ctx_destroy(context) bind(c, name='zmq_ctx_destroy')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: context
            integer(c_int)                 :: zmq_ctx_destroy
        end function zmq_ctx_destroy

        ! int zmq_ctx_get(void *context, int option)
        function zmq_ctx_get(context, option) bind(c, name='zmq_ctx_get')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: context
            integer(c_int), intent(in), value :: option
            integer(c_int)                    :: zmq_ctx_get
        end function zmq_ctx_get

        ! int zmq_ctx_get_ext(void *context, int option, void *optval, size_t *optval_len)
        function zmq_ctx_get_ext(context, option, optval, optval_len) bind(c, name='zmq_ctx_get_ext')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: context
            integer(c_int),    intent(in), value :: option
            type(c_ptr),       intent(in), value :: optval
            integer(c_size_t), intent(out)       :: optval_len
            integer(c_int)                       :: zmq_ctx_get_ext
        end function zmq_ctx_get_ext

        ! void *zmq_ctx_new(void)
        function zmq_ctx_new() bind(c, name='zmq_ctx_new')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zmq_ctx_new
        end function zmq_ctx_new

        ! int zmq_ctx_set(void *context, int option, int optval)
        function zmq_ctx_set(context, option, optval) bind(c, name='zmq_ctx_set')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: context
            integer(c_int), intent(in), value :: option
            integer(c_int), intent(in), value :: optval
            integer(c_int)                    :: zmq_ctx_set
        end function zmq_ctx_set

        ! int zmq_ctx_set_ext(void *context, int option, const void *optval, size_t optval_len)
        function zmq_ctx_set_ext(context, option, optval, optval_len) bind(c, name='zmq_ctx_set_ext')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: context
            integer(c_int),    intent(in), value :: option
            type(c_ptr),       intent(in), value :: optval
            integer(c_size_t), intent(in), value :: optval_len
            integer(c_int)                       :: zmq_ctx_set_ext
        end function zmq_ctx_set_ext

        ! int zmq_ctx_shutdown(void *context)
        function zmq_ctx_shutdown(context) bind(c, name='zmq_ctx_shutdown')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: context
            integer(c_int)                 :: zmq_ctx_shutdown
        end function zmq_ctx_shutdown

        ! int zmq_ctx_term(void *context)
        function zmq_ctx_term(context) bind(c, name='zmq_ctx_term')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: context
            integer(c_int)                 :: zmq_ctx_term
        end function zmq_ctx_term

        ! int zmq_curve_keypair(char *z85_public_key, char *z85_secret_key)
        function zmq_curve_keypair_(z85_public_key, z85_secret_key) bind(c, name='zmq_curve_keypair')
            import :: c_char, c_int
            implicit none
            character(c_char), intent(in) :: z85_public_key
            character(c_char), intent(in) :: z85_secret_key
            integer(c_int)                :: zmq_curve_keypair_
        end function zmq_curve_keypair_

        ! int zmq_curve_public(char *z85_public_key, const char *z85_secret_key)
        function zmq_curve_public_(z85_public_key, z85_secret_key) bind(c, name='zmq_curve_public')
            import :: c_char, c_int
            implicit none
            character(c_char), intent(in) :: z85_public_key
            character(c_char), intent(in) :: z85_secret_key
            integer(c_int)                :: zmq_curve_public_
        end function zmq_curve_public_

        ! int zmq_device(int type, void *frontend, void *backend)
        function zmq_device(type, frontend, backend) bind(c, name='zmq_device')
            import :: c_int, c_ptr
            implicit none
            integer(c_int), intent(in), value :: type
            type(c_ptr),    intent(in), value :: frontend
            type(c_ptr),    intent(in), value :: backend
            integer(c_int)                    :: zmq_device
        end function zmq_device

        ! int zmq_disconnect(void *s, const char *addr)
        function zmq_disconnect_(s, addr) bind(c, name='zmq_disconnect')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: s
            character(c_char), intent(in)        :: addr
            integer(c_int)                       :: zmq_disconnect_
        end function zmq_disconnect_

        ! int zmq_errno(void)
        function zmq_errno() bind(c, name='zmq_errno')
            import :: c_int
            implicit none
            integer(c_int) :: zmq_errno
        end function zmq_errno

        ! int zmq_getsockopt(void *s, int option, void *optval, size_t *optval_len)
        function zmq_getsockopt(s, option, optval, optval_len) bind(c, name='zmq_getsockopt')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: s
            integer(c_int),    intent(in), value :: option
            type(c_ptr),       intent(in), value :: optval
            integer(c_size_t), intent(out)       :: optval_len
            integer(c_int)                       :: zmq_getsockopt
        end function zmq_getsockopt

        ! int zmq_has(const char *capability)
        function zmq_has_(capability) bind(c, name='zmq_has')
            import :: c_char, c_int
            implicit none
            character(c_char), intent(in) :: capability
            integer(c_int)                :: zmq_has_
        end function zmq_has_

        ! void *zmq_init(int io_threads)
        function zmq_init(io_threads) bind(c, name='zmq_init')
            import :: c_int, c_ptr
            implicit none
            integer(c_int), intent(in), value :: io_threads
            type(c_ptr)                       :: zmq_init
        end function zmq_init

        ! int zmq_join(void *s, const char *group)
        function zmq_join_(s, group) bind(c, name='zmq_join')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: s
            character(c_char), intent(in)        :: group
            integer(c_int)                       :: zmq_join_
        end function zmq_join_

        ! int zmq_leave(void *s, const char *group)
        function zmq_leave_(s, group) bind(c, name='zmq_leave')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: s
            character(c_char), intent(in)        :: group
            integer(c_int)                       :: zmq_leave_
        end function zmq_leave_

        ! int zmq_msg_close(zmq_msg_t *msg)
        function zmq_msg_close(msg) bind(c, name='zmq_msg_close')
            import :: c_int, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout) :: msg
            integer(c_int)                 :: zmq_msg_close
        end function zmq_msg_close

        ! int zmq_msg_copy(zmq_msg_t *dest, zmq_msg_t *src)
        function zmq_msg_copy(dest, src) bind(c, name='zmq_msg_copy')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: dest
            type(c_ptr), intent(in), value :: src
            integer(c_int)                 :: zmq_msg_copy
        end function zmq_msg_copy

        ! void *zmq_msg_data(zmq_msg_t *msg)
        function zmq_msg_data(msg) bind(c, name='zmq_msg_data')
            import :: c_ptr, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout) :: msg
            type(c_ptr)                    :: zmq_msg_data
        end function zmq_msg_data

        ! int zmq_msg_get(const zmq_msg_t *msg, int property)
        function zmq_msg_get(msg, property) bind(c, name='zmq_msg_get')
            import :: c_int, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout)     :: msg
            integer(c_int),  intent(in), value :: property
            integer(c_int)                     :: zmq_msg_get
        end function zmq_msg_get

        ! const char *zmq_msg_gets(const zmq_msg_t *msg, const char *property)
        function zmq_msg_gets_(msg, property) bind(c, name='zmq_msg_gets')
            import :: c_char, c_ptr, zmq_msg_t
            implicit none
            type(zmq_msg_t),   intent(inout) :: msg
            character(c_char), intent(in)    :: property
            type(c_ptr)                      :: zmq_msg_gets_
        end function zmq_msg_gets_

        ! const char *zmq_msg_group(zmq_msg_t *msg)
        function zmq_msg_group_(msg) bind(c, name='zmq_msg_group')
            import :: c_ptr, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout) :: msg
            type(c_ptr)                    :: zmq_msg_group_
        end function zmq_msg_group_

        ! int zmq_msg_init(zmq_msg_t *msg)
        function zmq_msg_init(msg) bind(c, name='zmq_msg_init')
            import :: c_int, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout) :: msg
            integer(c_int)                 :: zmq_msg_init
        end function zmq_msg_init

        ! int zmq_msg_init_buffer(zmq_msg_t *msg, const void *buf, size_t size)
        function zmq_msg_init_buffer(msg, buf, size) bind(c, name='zmq_msg_init_buffer')
            import :: c_int, c_ptr, c_size_t, zmq_msg_t
            implicit none
            type(zmq_msg_t),   intent(inout)     :: msg
            type(c_ptr),       intent(in), value :: buf
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: zmq_msg_init_buffer
        end function zmq_msg_init_buffer

        ! int zmq_msg_init_data(zmq_msg_t *msg, void *data, size_t size, zmq_free_fn *ffn, void *hint)
        function zmq_msg_init_data_(msg, data, size, ffn, hint) bind(c, name='zmq_msg_init_data')
            import :: c_funptr, c_int, c_ptr, c_size_t, zmq_msg_t
            implicit none
            type(zmq_msg_t),   intent(inout)     :: msg
            type(c_ptr),       intent(in), value :: data
            integer(c_size_t), intent(in), value :: size
            type(c_funptr),    intent(in), value :: ffn
            type(c_ptr),       intent(in), value :: hint
            integer(c_int)                       :: zmq_msg_init_data_
        end function zmq_msg_init_data_

        ! int zmq_msg_init_size(zmq_msg_t *msg, size_t size)
        function zmq_msg_init_size(msg, size) bind(c, name='zmq_msg_init_size')
            import :: c_int, c_size_t, zmq_msg_t
            implicit none
            type(zmq_msg_t),   intent(inout)     :: msg
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: zmq_msg_init_size
        end function zmq_msg_init_size

        ! int zmq_msg_more(const zmq_msg_t *msg)
        function zmq_msg_more_(msg) bind(c, name='zmq_msg_more')
            import :: c_int, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout) :: msg
            integer(c_int)                 :: zmq_msg_more_
        end function zmq_msg_more_

        ! int zmq_msg_move(zmq_msg_t *dest, zmq_msg_t *src)
        function zmq_msg_move(dest, src) bind(c, name='zmq_msg_move')
            import :: c_int, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout) :: dest
            type(zmq_msg_t), intent(inout) :: src
            integer(c_int)                 :: zmq_msg_move
        end function zmq_msg_move

        ! int zmq_msg_recv(zmq_msg_t *msg, void *s, int flags)
        function zmq_msg_recv(msg, s, flags) bind(c, name='zmq_msg_recv')
            import :: c_int, c_ptr, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout)     :: msg
            type(c_ptr),     intent(in), value :: s
            integer(c_int),  intent(in), value :: flags
            integer(c_int)                     :: zmq_msg_recv
        end function zmq_msg_recv

        ! uint32_t zmq_msg_routing_id(zmq_msg_t *msg)
        function zmq_msg_routing_id(msg) bind(c, name='zmq_msg_routing_id')
            import :: c_uint32_t, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout) :: msg
            integer(c_uint32_t)            :: zmq_msg_routing_id
        end function zmq_msg_routing_id

        ! int zmq_msg_send(zmq_msg_t *msg, void *s, int flags)
        function zmq_msg_send(msg, s, flags) bind(c, name='zmq_msg_send')
            import :: c_int, c_ptr, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout)     :: msg
            type(c_ptr),     intent(in), value :: s
            integer(c_int),  intent(in), value :: flags
            integer(c_int)                     :: zmq_msg_send
        end function zmq_msg_send

        ! int zmq_msg_set(zmq_msg_t *msg, int property, int optval)
        function zmq_msg_set(msg, property, optval) bind(c, name='zmq_msg_set')
            import :: c_int, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout)     :: msg
            integer(c_int),  intent(in), value :: property
            integer(c_int),  intent(in), value :: optval
            integer(c_int)                     :: zmq_msg_set
        end function zmq_msg_set

        ! int zmq_msg_set_group(zmq_msg_t *msg, const char *group)
        function zmq_msg_set_group_(msg, group) bind(c, name='zmq_msg_set_group')
            import :: c_char, c_int, zmq_msg_t
            implicit none
            type(zmq_msg_t),   intent(inout) :: msg
            character(c_char), intent(in)    :: group
            integer(c_int)                   :: zmq_msg_set_group_
        end function zmq_msg_set_group_

        ! int zmq_msg_set_routing_id(zmq_msg_t *msg, uint32_t routing_id)
        function zmq_msg_set_routing_id(msg, routing_id) bind(c, name='zmq_msg_set_routing_id')
            import :: c_int, c_uint32_t, zmq_msg_t
            implicit none
            type(zmq_msg_t),     intent(inout)     :: msg
            integer(c_uint32_t), intent(in), value :: routing_id
            integer(c_int)                         :: zmq_msg_set_routing_id
        end function zmq_msg_set_routing_id

        ! size_t zmq_msg_size(const zmq_msg_t *msg)
        function zmq_msg_size(msg) bind(c, name='zmq_msg_size')
            import :: c_size_t, zmq_msg_t
            implicit none
            type(zmq_msg_t), intent(inout) :: msg
            integer(c_size_t)              :: zmq_msg_size
        end function zmq_msg_size

        ! int zmq_poll(zmq_pollitem_t *items, int nitems, long timeout)
        function zmq_poll(items, nitems, timeout) bind(c, name='zmq_poll')
            import :: c_int, c_long, zmq_pollitem_t
            implicit none
            type(zmq_pollitem_t), intent(inout)     :: items(*)
            integer(c_int),       intent(in), value :: nitems
            integer(c_long),      intent(in), value :: timeout
            integer(c_int)                          :: zmq_poll
        end function zmq_poll

        ! int zmq_poller_add(void *poller, void *socket, void *user_data, short events)
        function zmq_poller_add(poller, socket, user_data, events) bind(c, name='zmq_poller_add')
            import :: c_int, c_ptr, c_short
            implicit none
            type(c_ptr),      intent(in), value :: poller
            type(c_ptr),      intent(in), value :: socket
            type(c_ptr),      intent(in), value :: user_data
            integer(c_short), intent(in), value :: events
            integer(c_int)                      :: zmq_poller_add
        end function zmq_poller_add

        ! int zmq_poller_add_fd(void *poller, zmq_fd_t fd, void *user_data, short events)
        function zmq_poller_add_fd(poller, fd, user_data, events) bind(c, name='zmq_poller_add_fd')
            import :: c_int, c_ptr, c_short
            implicit none
            type(c_ptr),      intent(in), value :: poller
            integer(c_int),   intent(in), value :: fd
            type(c_ptr),      intent(in), value :: user_data
            integer(c_short), intent(in), value :: events
            integer(c_int)                      :: zmq_poller_add_fd
        end function zmq_poller_add_fd

        ! int zmq_poller_destroy(void **poller_p)
        function zmq_poller_destroy(poller_p) bind(c, name='zmq_poller_destroy')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out) :: poller_p
            integer(c_int)           :: zmq_poller_destroy
        end function zmq_poller_destroy

        ! int zmq_poller_fd(void *poller, zmq_fd_t *fd)
        function zmq_poller_fd(poller, fd) bind(c, name='zmq_poller_fd')
            import :: c_int, c_ptr, zmq_fd_t
            implicit none
            type(c_ptr),       intent(in), value :: poller
            integer(zmq_fd_t), intent(out)       :: fd
            integer(c_int)                       :: zmq_poller_fd
        end function zmq_poller_fd

        ! int zmq_poller_modify(void *poller, void *socket, short events)
        function zmq_poller_modify(poller, socket, events) bind(c, name='zmq_poller_modify')
            import :: c_int, c_ptr, c_short
            implicit none
            type(c_ptr),      intent(in), value :: poller
            type(c_ptr),      intent(in), value :: socket
            integer(c_short), intent(in), value :: events
            integer(c_int)                      :: zmq_poller_modify
        end function zmq_poller_modify

        ! int zmq_poller_modify_fd(void *poller, zmq_fd_t fd, short events)
        function zmq_poller_modify_fd(poller, fd, events) bind(c, name='zmq_poller_modify_fd')
            import :: c_int, c_ptr, c_short, zmq_fd_t
            implicit none
            type(c_ptr),       intent(in), value :: poller
            integer(zmq_fd_t), intent(in), value :: fd
            integer(c_short),  intent(in), value :: events
            integer(c_int)                       :: zmq_poller_modify_fd
        end function zmq_poller_modify_fd

        ! void *zmq_poller_new(void)
        function zmq_poller_new() bind(c, name='zmq_poller_new')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zmq_poller_new
        end function zmq_poller_new

        ! int zmq_poller_remove(void *poller, void *socket)
        function zmq_poller_remove(poller, socket) bind(c, name='zmq_poller_remove')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: poller
            type(c_ptr), intent(in), value :: socket
            integer(c_int)                 :: zmq_poller_remove
        end function zmq_poller_remove

        ! int zmq_poller_remove_fd(void *poller, zmq_fd_t fd)
        function zmq_poller_remove_fd(poller, fd) bind(c, name='zmq_poller_remove_fd')
            import :: c_int, c_ptr, zmq_fd_t
            implicit none
            type(c_ptr),       intent(in), value :: poller
            integer(zmq_fd_t), intent(in), value :: fd
            integer(c_int)                       :: zmq_poller_remove_fd
        end function zmq_poller_remove_fd

        ! int zmq_poller_size(void *poller)
        function zmq_poller_size(poller) bind(c, name='zmq_poller_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: poller
            integer(c_int)                 :: zmq_poller_size
        end function zmq_poller_size

        ! int zmq_poller_wait(void *poller, zmq_poller_event_t *event, long timeout)
        function zmq_poller_wait(poller, event, timeout) bind(c, name='zmq_poller_wait')
            import :: c_int, c_long, c_ptr
            implicit none
            type(c_ptr),     intent(in), value :: poller
            type(c_ptr),     intent(in), value :: event
            integer(c_long), intent(in), value :: timeout
            integer(c_int)                     :: zmq_poller_wait
        end function zmq_poller_wait

        ! int zmq_poller_wait_all(void *poller, zmq_poller_event_t *events, int n_events, long timeout)
        function zmq_poller_wait_all(poller, events, n_events, timeout) bind(c, name='zmq_poller_wait_all')
            import :: c_int, c_long, c_ptr
            implicit none
            type(c_ptr),     intent(in), value :: poller
            type(c_ptr),     intent(in), value :: events
            integer(c_int),  intent(in), value :: n_events
            integer(c_long), intent(in), value :: timeout
            integer(c_int)                     :: zmq_poller_wait_all
        end function zmq_poller_wait_all

        ! int zmq_ppoll(zmq_pollitem_t *items, int nitems, long timeout, const void *sigmask)
        function zmq_ppoll(items, nitems, timeout, sigmask) bind(c, name='zmq_ppoll')
            import :: c_int, c_long, c_ptr
            implicit none
            type(c_ptr),     intent(in), value :: items
            integer(c_int),  intent(in), value :: nitems
            integer(c_long), intent(in), value :: timeout
            type(c_ptr),     intent(in), value :: sigmask
            integer(c_int)                     :: zmq_ppoll
        end function zmq_ppoll

        ! int zmq_proxy(void *frontend, void *backend, void *capture)
        function zmq_proxy(frontend, backend, capture) bind(c, name='zmq_proxy')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: frontend
            type(c_ptr), intent(in), value :: backend
            type(c_ptr), intent(in), value :: capture
            integer(c_int)                 :: zmq_proxy
        end function zmq_proxy

        ! int zmq_proxy_steerable(void *frontend, void *backend, void *capture, void *control)
        function zmq_proxy_steerable(frontend, backend, capture, control) bind(c, name='zmq_proxy_steerable')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: frontend
            type(c_ptr), intent(in), value :: backend
            type(c_ptr), intent(in), value :: capture
            type(c_ptr), intent(in), value :: control
            integer(c_int)                 :: zmq_proxy_steerable
        end function zmq_proxy_steerable

        ! int zmq_recv(void *s, void *buf, size_t len, int flags)
        function zmq_recv(s, buf, len, flags) bind(c, name='zmq_recv')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: s
            type(c_ptr),       intent(in), value :: buf
            integer(c_size_t), intent(in), value :: len
            integer(c_int),    intent(in), value :: flags
            integer(c_int)                       :: zmq_recv
        end function zmq_recv

        ! int zmq_recviov(void *s, struct iovec *iov, size_t *count, int flags)
        function zmq_recviov(s, iov, count, flags) bind(c, name='zmq_recviov')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: s
            type(c_ptr),       intent(in), value :: iov
            integer(c_size_t), intent(out)       :: count
            integer(c_int),    intent(in), value :: flags
            integer(c_int)                       :: zmq_recviov
        end function zmq_recviov

        ! int zmq_recvmsg(void *s, zmq_msg_t *msg, int flags)
        function zmq_recvmsg(s, msg, flags) bind(c, name='zmq_recvmsg')
            import :: c_int, c_ptr, zmq_msg_t
            implicit none
            type(c_ptr),     intent(in), value :: s
            type(zmq_msg_t), intent(inout)     :: msg
            integer(c_int),  intent(in), value :: flags
            integer(c_int)                     :: zmq_recvmsg
        end function zmq_recvmsg

        ! int zmq_send(void *s, const void *buf, size_t len, int flags)
        function zmq_send(s, buf, len, flags) bind(c, name='zmq_send')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: s
            type(c_ptr),       intent(in), value :: buf
            integer(c_size_t), intent(in), value :: len
            integer(c_int),    intent(in), value :: flags
            integer(c_int)                       :: zmq_send
        end function zmq_send

        ! int zmq_send_const(void *s, const void *buf, size_t len, int flags)
        function zmq_send_const(s, buf, len, flags) bind(c, name='zmq_send_const')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: s
            type(c_ptr),       intent(in), value :: buf
            integer(c_size_t), intent(in), value :: len
            integer(c_int),    intent(in), value :: flags
            integer(c_int)                       :: zmq_send_const
        end function zmq_send_const

        ! int zmq_sendiov(void *s, struct iovec *iov, size_t count, int flags)
        function zmq_sendiov(s, iov, count, flags) bind(c, name='zmq_sendiov')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: s
            type(c_ptr),       intent(in), value :: iov
            integer(c_size_t), intent(in), value :: count
            integer(c_int),    intent(in), value :: flags
            integer(c_int)                       :: zmq_sendiov
        end function zmq_sendiov

        ! int zmq_sendmsg(void *s, zmq_msg_t *msg, int flags)
        function zmq_sendmsg(s, msg, flags) bind(c, name='zmq_sendmsg')
            import :: c_int, c_ptr, zmq_msg_t
            implicit none
            type(c_ptr),     intent(in), value :: s
            type(zmq_msg_t), intent(inout)     :: msg
            integer(c_int),  intent(in), value :: flags
            integer(c_int)                     :: zmq_sendmsg
        end function zmq_sendmsg

        ! int zmq_setsockopt(void *s, int option, const void *optval, size_t optval_len)
        function zmq_setsockopt(s, option, optval, optval_len) bind(c, name='zmq_setsockopt')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: s
            integer(c_int),    intent(in), value :: option
            type(*),           intent(in)        :: optval
            integer(c_size_t), intent(in), value :: optval_len
            integer(c_int)                       :: zmq_setsockopt
        end function zmq_setsockopt

        ! void zmq_sleep(int seconds)
        subroutine zmq_sleep(seconds) bind(c, name='zmq_sleep')
            import :: c_int
            implicit none
            integer(c_int), intent(in), value :: seconds
        end subroutine zmq_sleep

        ! void *zmq_socket(void *s, int type)
        function zmq_socket(s, type) bind(c, name='zmq_socket')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: s
            integer(c_int), intent(in), value :: type
            type(c_ptr)                       :: zmq_socket
        end function zmq_socket

        ! int zmq_socket_get_peer_state(void *s, const void *routing_id, size_t routing_id_size)
        function zmq_socket_get_peer_state(s, routing_id, routing_id_size) bind(c, name='zmq_socket_get_peer_state')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: s
            type(c_ptr),       intent(in), value :: routing_id
            integer(c_size_t), intent(in), value :: routing_id_size
            integer(c_int)                       :: zmq_socket_get_peer_state
        end function zmq_socket_get_peer_state

        ! int zmq_socket_monitor(void *s, const char *addr, int events)
        function zmq_socket_monitor_(s, addr, events) bind(c, name='zmq_socket_monitor')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: s
            character(c_char), intent(in)        :: addr
            integer(c_int),    intent(in), value :: events
            integer(c_int)                       :: zmq_socket_monitor_
        end function zmq_socket_monitor_

        ! int zmq_socket_monitor_pipes_stats(void *s)
        function zmq_socket_monitor_pipes_stats(s) bind(c, name='zmq_socket_monitor_pipes_stats')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: s
            integer(c_int)                 :: zmq_socket_monitor_pipes_stats
        end function zmq_socket_monitor_pipes_stats

        ! int zmq_socket_monitor_versioned(void *s, const char *addr, uint64_t events, int event_version, int type)
        function zmq_socket_monitor_versioned_(s, addr, events, event_version, type) bind(c, name='zmq_socket_monitor_versioned')
            import :: c_char, c_int, c_ptr, c_uint64_t
            implicit none
            type(c_ptr),         intent(in), value :: s
            character(c_char),   intent(in)        :: addr
            integer(c_uint64_t), intent(in), value :: events
            integer(c_int),      intent(in), value :: event_version
            integer(c_int),      intent(in), value :: type
            integer(c_int)                         :: zmq_socket_monitor_versioned_
        end function zmq_socket_monitor_versioned_

        ! unsigned long zmq_stopwatch_intermediate(void *watch)
        function zmq_stopwatch_intermediate(watch) bind(c, name='zmq_stopwatch_intermediate')
            import :: c_ptr, c_unsigned_long
            implicit none
            type(c_ptr), intent(in), value :: watch
            integer(c_unsigned_long)       :: zmq_stopwatch_intermediate
        end function zmq_stopwatch_intermediate

        ! void *zmq_stopwatch_start(void)
        function zmq_stopwatch_start() bind(c, name='zmq_stopwatch_start')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zmq_stopwatch_start
        end function zmq_stopwatch_start

        ! unsigned long zmq_stopwatch_stop(void *watch)
        function zmq_stopwatch_stop(watch) bind(c, name='zmq_stopwatch_stop')
            import :: c_ptr, c_unsigned_long
            implicit none
            type(c_ptr), intent(in), value :: watch
            integer(c_unsigned_long)       :: zmq_stopwatch_stop
        end function zmq_stopwatch_stop

        ! const char *zmq_strerror(int errnum)
        function zmq_strerror_(errnum) bind(c, name='zmq_strerror')
            import :: c_int, c_ptr
            implicit none
            integer(c_int), intent(in), value :: errnum
            type(c_ptr)                       :: zmq_strerror_
        end function zmq_strerror_

        ! int zmq_term(void *context)
        function zmq_term(context) bind(c, name='zmq_term')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: context
            integer(c_int)                 :: zmq_term
        end function zmq_term

        ! void zmq_threadclose(void *thread)
        subroutine zmq_threadclose(thread) bind(c, name='zmq_threadclose')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: thread
        end subroutine zmq_threadclose

        ! void *zmq_threadstart(zmq_thread_fn *func, void *arg)
        function zmq_threadstart(func, arg) bind(c, name='zmq_threadstart')
            import :: c_ptr, zmq_thread_fn
            implicit none
            procedure(zmq_thread_fn)       :: func
            type(c_ptr), intent(in), value :: arg
            type(c_ptr)                    :: zmq_threadstart
        end function zmq_threadstart

        ! int zmq_timers_add(void *timers, size_t interval, zmq_timer_fn *handler, void *arg)
        function zmq_timers_add(timers, interval, handler, arg) bind(c, name='zmq_timers_add')
            import :: c_int, c_ptr, c_size_t, zmq_timer_fn
            implicit none
            type(c_ptr),       intent(in), value :: timers
            integer(c_size_t), intent(in), value :: interval
            procedure(zmq_timer_fn)              :: handler
            type(c_ptr),       intent(in), value :: arg
            integer(c_int)                       :: zmq_timers_add
        end function zmq_timers_add

        ! int zmq_timers_cancel(void *timers, int timer_id)
        function zmq_timers_cancel(timers, timer_id) bind(c, name='zmq_timers_cancel')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: timers
            integer(c_int), intent(in), value :: timer_id
            integer(c_int)                    :: zmq_timers_cancel
        end function zmq_timers_cancel

        ! int zmq_timers_destroy(void **timers_p)
        function zmq_timers_destroy(timers_p) bind(c, name='zmq_timers_destroy')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(inout) :: timers_p
            integer(c_int)             :: zmq_timers_destroy
        end function zmq_timers_destroy

        ! int zmq_timers_execute(void *timers)
        function zmq_timers_execute(timers) bind(c, name='zmq_timers_execute')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: timers
            integer(c_int)                 :: zmq_timers_execute
        end function zmq_timers_execute

        ! void *zmq_timers_new(void)
        function zmq_timers_new() bind(c, name='zmq_timers_new')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zmq_timers_new
        end function zmq_timers_new

        ! int zmq_timers_reset(void *timers, int timer_id)
        function zmq_timers_reset(timers, timer_id) bind(c, name='zmq_timers_reset')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: timers
            integer(c_int), intent(in), value :: timer_id
            integer(c_int)                    :: zmq_timers_reset
        end function zmq_timers_reset

        ! int zmq_timers_set_interval(void *timers, int timer_id, size_t interval)
        function zmq_timers_set_interval(timers, timer_id, interval) bind(c, name='zmq_timers_set_interval')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: timers
            integer(c_int),    intent(in), value :: timer_id
            integer(c_size_t), intent(in), value :: interval
            integer(c_int)                       :: zmq_timers_set_interval
        end function zmq_timers_set_interval

        ! long zmq_timers_timeout(void *timers)
        function zmq_timers_timeout(timers) bind(c, name='zmq_timers_timeout')
            import :: c_long, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: timers
            integer(c_long)                :: zmq_timers_timeout
        end function zmq_timers_timeout

        ! int zmq_unbind(void *s, const char *addr)
        function zmq_unbind_(s, addr) bind(c, name='zmq_unbind')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: s
            character(c_char), intent(in)        :: addr
            integer(c_int)                       :: zmq_unbind_
        end function zmq_unbind_

        ! void zmq_version(int *major, int *minor, int *patch)
        subroutine zmq_version(major, minor, patch) bind(c, name='zmq_version')
            import :: c_int
            implicit none
            integer(c_int), intent(out) :: major
            integer(c_int), intent(out) :: minor
            integer(c_int), intent(out) :: patch
        end subroutine zmq_version

        ! uint8_t *zmq_z85_decode(uint8_t *dest, const char *string)
        function zmq_z85_decode_(dest, string) bind(c, name='zmq_z85_decode')
            import :: c_char, c_ptr, c_uint8_t
            implicit none
            integer(c_uint8_t), intent(inout) :: dest(*)
            character(c_char),  intent(in)    :: string
            type(c_ptr)                       :: zmq_z85_decode_
        end function zmq_z85_decode_

        ! char *zmq_z85_encode(char *dest, const uint8_t *data, size_t size)
        function zmq_z85_encode_(dest, data, size) bind(c, name='zmq_z85_encode')
            import :: c_ptr, c_size_t, c_uint8_t
            implicit none
            type(c_ptr),        intent(in), value :: dest
            integer(c_uint8_t), intent(inout)     :: data(*)
            integer(c_size_t),  intent(in), value :: size
            type(c_ptr)                           :: zmq_z85_encode_
        end function zmq_z85_encode_
    end interface
contains
    ! int zmq_bind(void *s, const char *addr)
    integer function zmq_bind(s, addr) result(rc)
        type(c_ptr),  intent(in) :: s
        character(*), intent(in) :: addr

        rc = zmq_bind_(s, f_c_str(addr))
    end function zmq_bind

    ! int zmq_close(void *s)
    integer function zmq_close(s) result(rc)
        type(c_ptr), intent(inout) :: s

        rc = zmq_close_(s)
        if (rc == 0) s = c_null_ptr
    end function zmq_close

    ! int zmq_connect(void *s, const char *addr)
    integer function zmq_connect(s, addr) result(rc)
        type(c_ptr),  intent(in) :: s
        character(*), intent(in) :: addr

        rc = zmq_connect_(s, f_c_str(addr))
    end function zmq_connect

    ! uint32_t zmq_connect_peer(void *s, const char *addr)
    integer(c_uint32_t) function zmq_connect_peer(s, addr) result(id)
        type(c_ptr),  intent(in) :: s
        character(*), intent(in) :: addr

        id = zmq_connect_peer_(s, f_c_str(addr))
    end function zmq_connect_peer

    ! int zmq_curve_keypair(char *z85_public_key, char *z85_secret_key)
    integer function zmq_curve_keypair(z85_public_key, z85_secret_key) result(rc)
        character(*), intent(in) :: z85_public_key
        character(*), intent(in) :: z85_secret_key

        rc = zmq_curve_keypair_(f_c_str(z85_public_key), f_c_str(z85_secret_key))
    end function zmq_curve_keypair

    ! int zmq_curve_public(char *z85_public_key, const char *z85_secret_key)
    integer function zmq_curve_public(z85_public_key, z85_secret_key) result(rc)
        character(*), intent(in) :: z85_public_key
        character(*), intent(in) :: z85_secret_key

        rc = zmq_curve_public_(f_c_str(z85_public_key), f_c_str(z85_secret_key))
    end function zmq_curve_public

    ! int zmq_disconnect(void *s, const char *addr)
    integer function zmq_disconnect(s, addr) result(rc)
        type(c_ptr),  intent(in) :: s
        character(*), intent(in) :: addr

        rc = zmq_disconnect_(s, f_c_str(addr))
    end function zmq_disconnect

    ! int zmq_has(const char *capability)
    logical function zmq_has(capability) result(has)
        character(*), intent(in) :: capability

        has = (zmq_has_(f_c_str(capability)) == 1)
    end function zmq_has

    ! int zmq_join(void *s, const char *group)
    integer function zmq_join(s, group) result(rc)
        type(c_ptr),  intent(in) :: s
        character(*), intent(in) :: group

        rc = zmq_join_(s, f_c_str(group))
    end function zmq_join

    ! int zmq_leave(void *s, const char *group)
    integer function zmq_leave(s, group) result(rc)
        type(c_ptr),  intent(in) :: s
        character(*), intent(in) :: group

        rc = zmq_leave_(s, f_c_str(group))
    end function zmq_leave

    ! const char *zmq_msg_gets(const zmq_msg_t *msg, const char *property)
    function zmq_msg_gets(msg, property) result(str)
        type(zmq_msg_t), intent(inout) :: msg
        character(*),    intent(in)    :: property
        character(:), allocatable      :: str

        type(c_ptr) :: ptr

        ptr = zmq_msg_gets_(msg, f_c_str(property))
        call c_f_str_ptr(ptr, str)
    end function zmq_msg_gets

    ! const char *zmq_msg_group(zmq_msg_t *msg)
    function zmq_msg_group(msg) result(str)
        type(zmq_msg_t), intent(inout) :: msg
        character(:), allocatable      :: str

        type(c_ptr) :: ptr

        ptr = zmq_msg_group_(msg)
        call c_f_str_ptr(ptr, str)
    end function zmq_msg_group

    ! int zmq_msg_init_data(zmq_msg_t *msg, void *data, size_t size, zmq_free_fn *ffn, void *hint)
    integer function zmq_msg_init_data(msg, data, size, ffn, hint) result(rc)
        type(zmq_msg_t),   intent(inout)           :: msg
        type(*), target,   intent(inout)           :: data
        integer(c_size_t), intent(in)              :: size
        procedure(zmq_free_fn),           optional :: ffn
        type(*), target,   intent(inout), optional :: hint

        type(c_funptr) :: ffn_
        type(c_ptr)    :: hint_

        if (present(ffn)) then
            ffn_ = c_funloc(ffn)
        else
            ffn_ = c_null_funptr
        end if

        if (present(hint)) then
            hint_ = c_loc(hint)
        else
            hint_ = c_null_ptr
        end if

        rc = zmq_msg_init_data_(msg, c_loc(data), size, ffn_, hint_)
    end function zmq_msg_init_data

    ! int zmq_msg_more(const zmq_msg_t *msg)
    logical function zmq_msg_more(msg) result(more)
        type(zmq_msg_t), intent(inout) :: msg

        more = (zmq_msg_more_(msg) == 1)
    end function zmq_msg_more

    ! int zmq_msg_set_group(zmq_msg_t *msg, const char *group)
    integer function zmq_msg_set_group(msg, group) result(rc)
        type(zmq_msg_t), intent(inout) :: msg
        character(*),    intent(in)    :: group

        rc = zmq_msg_set_group_(msg, f_c_str(group))
    end function zmq_msg_set_group

    ! int zmq_socket_monitor(void *s, const char *addr, int events)
    integer function zmq_socket_monitor(s, addr, events) result(rc)
        type(c_ptr),  intent(in) :: s
        character(*), intent(in) :: addr
        integer,      intent(in) :: events

        rc = zmq_socket_monitor_(s, f_c_str(addr), events)
    end function zmq_socket_monitor

    ! int zmq_socket_monitor_versioned(void *s, const char *addr, uint64_t events, int event_version, int type)
    integer function zmq_socket_monitor_versioned(s, addr, events, event_version, type) result(rc)
        type(c_ptr),         intent(in) :: s
        character(*),        intent(in) :: addr
        integer(c_uint64_t), intent(in) :: events
        integer,             intent(in) :: event_version
        integer,             intent(in) :: type

        rc = zmq_socket_monitor_versioned_(s, f_c_str(addr), events, event_version, type)
    end function zmq_socket_monitor_versioned

    ! const char *zmq_strerror(int errnum)
    function zmq_strerror(errnum) result(str)
        integer, intent(in)       :: errnum
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zmq_strerror_(errnum)
        call c_f_str_ptr(ptr, str)
    end function zmq_strerror

    ! int zmq_unbind(void *s, const char *addr)
    integer function zmq_unbind(s, addr) result(rc)
        type(c_ptr),  intent(in) :: s
        character(*), intent(in) :: addr

        rc = zmq_unbind_(s, f_c_str(addr))
    end function zmq_unbind

    ! uint8_t *zmq_z85_decode(uint8_t *dest, const char *string)
    subroutine zmq_z85_decode(dest, string)
        !! The `zmq_z85_decode()` subroutine shall decode `string` into `dest`.
        !! The length of `string` shall be divisible by 5. `dest` must be large
        !! enough for the decoded value (0.8 x `len(string)`).
        !!
        !! The encoding shall follow the ZMQ RFC 32 specification.
        integer(c_uint8_t), intent(inout) :: dest(:)
        character(*),       intent(in)    :: string

        type(c_ptr) :: ptr

        ptr = zmq_z85_decode_(dest, f_c_str(string))
    end subroutine zmq_z85_decode

    ! char *zmq_z85_encode(char *dest, const uint8_t *data, size_t size)
    subroutine zmq_z85_encode(dest, data)
        !! The `zmq_z85_encode()` function shall encode the binary block
        !! specified by `data` into a string in `dest`. The size of the binary
        !! block must be divisible by 4. The `dest` must have sufficient
        !! space for size * 1.25 plus 1 for a null terminator. A 32-byte CURVE
        !! key is encoded as 40 ASCII characters plus a null terminator.
        !!
        !! The encoding shall follow the ZMQ RFC 32 specification.
        character(*), target, intent(inout) :: dest
        integer(c_uint8_t),   intent(inout) :: data(:)

        type(c_ptr) :: ptr

        ptr = zmq_z85_encode_(c_loc(dest), data, size(data, kind=c_size_t))
    end subroutine zmq_z85_encode
end module zmq
