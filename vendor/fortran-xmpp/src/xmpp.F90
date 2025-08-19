! xmpp.F90
!
! Auto-generated Fortran 2018 interface bindings to libstrophe.
!
! Author:  Philipp Engel
! Licence: ISC
module xmpp
    use, intrinsic :: iso_c_binding
    use :: xmpp_util
    implicit none (type, external)
    private

#if defined (__flang__) || (defined (__GFORTRAN__) && __GNUC__ > 15) || (defined (__GFORTRAN__) && __GNUC__ == 15 && __GNUC_MINOR__ >= 2)

    public :: c_unsigned
    public :: c_unsigned_char
    public :: c_unsigned_long
    public :: c_unsigned_short

#else

    integer, parameter, public :: c_unsigned       = c_int
    integer, parameter, public :: c_unsigned_char  = c_signed_char
    integer, parameter, public :: c_unsigned_long  = c_long
    integer, parameter, public :: c_unsigned_short = c_short

#endif

    ! namespace defines
    character(len=*), parameter, public :: XMPP_NS_CLIENT              = 'jabber:client'
    character(len=*), parameter, public :: XMPP_NS_COMPONENT           = 'jabber:component:accept'
    character(len=*), parameter, public :: XMPP_NS_STREAMS             = 'http://etherx.jabber.org/streams'
    character(len=*), parameter, public :: XMPP_NS_STREAMS_IETF        = 'urn:ietf:params:xml:ns:xmpp-streams'
    character(len=*), parameter, public :: XMPP_NS_STANZAS_IETF        = 'urn:ietf:params:xml:ns:xmpp-stanzas'
    character(len=*), parameter, public :: XMPP_NS_TLS                 = 'urn:ietf:params:xml:ns:xmpp-tls'
    character(len=*), parameter, public :: XMPP_NS_SASL                = 'urn:ietf:params:xml:ns:xmpp-sasl'
    character(len=*), parameter, public :: XMPP_NS_BIND                = 'urn:ietf:params:xml:ns:xmpp-bind'
    character(len=*), parameter, public :: XMPP_NS_SESSION             = 'urn:ietf:params:xml:ns:xmpp-session'
    character(len=*), parameter, public :: XMPP_NS_AUTH                = 'jabber:iq:auth'
    character(len=*), parameter, public :: XMPP_NS_DISCO_INFO          = 'http://jabber.org/protocol/disco#info'
    character(len=*), parameter, public :: XMPP_NS_DISCO_ITEMS         = 'http://jabber.org/protocol/disco#items'
    character(len=*), parameter, public :: XMPP_NS_ROSTER              = 'jabber:iq:roster'
    character(len=*), parameter, public :: XMPP_NS_REGISTER            = 'jabber:iq:register'
    character(len=*), parameter, public :: XMPP_NS_SM                  = 'urn:xmpp:sm:3'
    character(len=*), parameter, public :: XMPP_NS_COMPRESSION         = 'http://jabber.org/protocol/compress'
    character(len=*), parameter, public :: XMPP_NS_FEATURE_COMPRESSION = 'http://jabber.org/features/compress'

    ! error defines
    integer(kind=c_int), parameter, public :: XMPP_EOK    =  0
    integer(kind=c_int), parameter, public :: XMPP_EMEM   = -1
    integer(kind=c_int), parameter, public :: XMPP_EINVOP = -2
    integer(kind=c_int), parameter, public :: XMPP_EINT   = -3

    ! xmpp_log_level_t;
    integer(kind=c_int), parameter, public :: XMPP_LEVEL_DEBUG = 0
    integer(kind=c_int), parameter, public :: XMPP_LEVEL_INFO  = 1
    integer(kind=c_int), parameter, public :: XMPP_LEVEL_WARN  = 2
    integer(kind=c_int), parameter, public :: XMPP_LEVEL_ERROR = 3

    ! xmpp_conn_type_t
    integer(kind=c_int), parameter, public :: XMPP_UNKNOWN   = 0
    integer(kind=c_int), parameter, public :: XMPP_CLIENT    = 1
    integer(kind=c_int), parameter, public :: XMPP_COMPONENT = 2

    ! connection flags
    integer(kind=c_long), parameter, public :: XMPP_CONN_FLAG_DISABLE_TLS            = shiftl(1, 0)
    integer(kind=c_long), parameter, public :: XMPP_CONN_FLAG_MANDATORY_TLS          = shiftl(1, 1)
    integer(kind=c_long), parameter, public :: XMPP_CONN_FLAG_LEGACY_SSL             = shiftl(1, 2)
    integer(kind=c_long), parameter, public :: XMPP_CONN_FLAG_TRUST_TLS              = shiftl(1, 3)
    integer(kind=c_long), parameter, public :: XMPP_CONN_FLAG_LEGACY_AUTH            = shiftl(1, 4)
    integer(kind=c_long), parameter, public :: XMPP_CONN_FLAG_DISABLE_SM             = shiftl(1, 5)
    integer(kind=c_long), parameter, public :: XMPP_CONN_FLAG_ENABLE_COMPRESSION     = shiftl(1, 6)
    integer(kind=c_long), parameter, public :: XMPP_CONN_FLAG_COMPRESSION_DONT_RESET = shiftl(1, 7)

    ! xmpp_conn_event_t
    integer(kind=c_int), parameter, public :: XMPP_CONN_CONNECT     = 0
    integer(kind=c_int), parameter, public :: XMPP_CONN_RAW_CONNECT = 1
    integer(kind=c_int), parameter, public :: XMPP_CONN_DISCONNECT  = 2
    integer(kind=c_int), parameter, public :: XMPP_CONN_FAIL        = 3

    ! xmpp_queue_element_t
    integer(kind=c_int), parameter, public :: XMPP_QUEUE_OLDEST   = -1
    integer(kind=c_int), parameter, public :: XMPP_QUEUE_YOUNGEST = -2

    ! xmpp_error_type_t
    integer(kind=c_int), parameter, public :: XMPP_SE_BAD_FORMAT              = 0
    integer(kind=c_int), parameter, public :: XMPP_SE_BAD_NS_PREFIX           = 1
    integer(kind=c_int), parameter, public :: XMPP_SE_CONFLICT                = 2
    integer(kind=c_int), parameter, public :: XMPP_SE_CONN_TIMEOUT            = 3
    integer(kind=c_int), parameter, public :: XMPP_SE_HOST_GONE               = 4
    integer(kind=c_int), parameter, public :: XMPP_SE_HOST_UNKNOWN            = 5
    integer(kind=c_int), parameter, public :: XMPP_SE_IMPROPER_ADDR           = 6
    integer(kind=c_int), parameter, public :: XMPP_SE_INTERNAL_SERVER_ERROR   = 7
    integer(kind=c_int), parameter, public :: XMPP_SE_INVALID_FROM            = 8
    integer(kind=c_int), parameter, public :: XMPP_SE_INVALID_ID              = 9
    integer(kind=c_int), parameter, public :: XMPP_SE_INVALID_NS              = 10
    integer(kind=c_int), parameter, public :: XMPP_SE_INVALID_XML             = 11
    integer(kind=c_int), parameter, public :: XMPP_SE_NOT_AUTHORIZED          = 12
    integer(kind=c_int), parameter, public :: XMPP_SE_POLICY_VIOLATION        = 13
    integer(kind=c_int), parameter, public :: XMPP_SE_REMOTE_CONN_FAILED      = 14
    integer(kind=c_int), parameter, public :: XMPP_SE_RESOURCE_CONSTRAINT     = 15
    integer(kind=c_int), parameter, public :: XMPP_SE_RESTRICTED_XML          = 16
    integer(kind=c_int), parameter, public :: XMPP_SE_SEE_OTHER_HOST          = 17
    integer(kind=c_int), parameter, public :: XMPP_SE_SYSTEM_SHUTDOWN         = 18
    integer(kind=c_int), parameter, public :: XMPP_SE_UNDEFINED_CONDITION     = 19
    integer(kind=c_int), parameter, public :: XMPP_SE_UNSUPPORTED_ENCODING    = 20
    integer(kind=c_int), parameter, public :: XMPP_SE_UNSUPPORTED_STANZA_TYPE = 21
    integer(kind=c_int), parameter, public :: XMPP_SE_UNSUPPORTED_VERSION     = 22
    integer(kind=c_int), parameter, public :: XMPP_SE_XML_NOT_WELL_FORMED     = 23

    integer(kind=c_int), parameter, public :: XMPP_SHA1_DIGEST_SIZE = 20

    ! xmpp_log_t
    type, bind(c), public :: xmpp_log_t
        type(c_funptr) :: handler   = c_null_funptr
        type(c_ptr)    :: user_data = c_null_ptr
    end type xmpp_log_t

    ! xmpp_stream_error_t
    type, bind(c), public :: xmpp_stream_error_t
        integer(kind=c_int) :: type   = 0
        type(c_ptr)         :: text   = c_null_ptr
        type(c_ptr)         :: stanza = c_null_ptr
    end type xmpp_stream_error_t

    public :: xmpp_certfail_handler
    public :: xmpp_conn_handler
    public :: xmpp_global_timed_handler
    public :: xmpp_handler
    public :: xmpp_log_handler
    public :: xmpp_password_callback
    public :: xmpp_sockopt_callback
    public :: xmpp_timed_handler

    public :: xmpp_base64_decode_bin
    public :: xmpp_base64_decode_bin_
    public :: xmpp_base64_decode_str
    public :: xmpp_base64_decode_str_
    public :: xmpp_base64_encode
    public :: xmpp_base64_encode_
    public :: xmpp_conn_cert_xmppaddr
    public :: xmpp_conn_cert_xmppaddr_
    public :: xmpp_conn_cert_xmppaddr_num
    public :: xmpp_conn_clone
    public :: xmpp_conn_get_bound_jid
    public :: xmpp_conn_get_bound_jid_
    public :: xmpp_conn_get_context
    public :: xmpp_conn_get_flags
    public :: xmpp_conn_get_jid
    public :: xmpp_conn_get_jid_
    public :: xmpp_conn_get_keyfile
    public :: xmpp_conn_get_keyfile_
    public :: xmpp_conn_get_pass
    public :: xmpp_conn_get_pass_
    public :: xmpp_conn_get_peer_cert
    public :: xmpp_conn_get_sm_state
    public :: xmpp_conn_is_connected
    public :: xmpp_conn_is_connecting
    public :: xmpp_conn_is_disconnected
    public :: xmpp_conn_is_secured
    public :: xmpp_conn_new
    public :: xmpp_conn_open_stream
    public :: xmpp_conn_open_stream_
    public :: xmpp_conn_open_stream_default
    public :: xmpp_conn_release
    public :: xmpp_conn_send_queue_drop_element
    public :: xmpp_conn_send_queue_drop_element_
    public :: xmpp_conn_send_queue_len
    public :: xmpp_conn_set_cafile
    public :: xmpp_conn_set_cafile_
    public :: xmpp_conn_set_capath
    public :: xmpp_conn_set_capath_
    public :: xmpp_conn_set_certfail_handler
    public :: xmpp_conn_set_client_cert
    public :: xmpp_conn_set_client_cert_
    public :: xmpp_conn_set_flags
    public :: xmpp_conn_set_jid
    public :: xmpp_conn_set_jid_
    public :: xmpp_conn_set_pass
    public :: xmpp_conn_set_pass_
    public :: xmpp_conn_set_password_callback
    public :: xmpp_conn_set_password_retries
    public :: xmpp_conn_set_sm_state
    public :: xmpp_conn_set_sockopt_callback
    public :: xmpp_conn_tls_start
    public :: xmpp_connect_client
    public :: xmpp_connect_client_
    public :: xmpp_connect_component
    public :: xmpp_connect_component_
    public :: xmpp_connect_raw
    public :: xmpp_connect_raw_
    public :: xmpp_ctx_free
    public :: xmpp_ctx_free_
    public :: xmpp_ctx_new
    public :: xmpp_ctx_set_timeout
    public :: xmpp_disconnect
    public :: xmpp_error_new
    public :: xmpp_error_new_
    public :: xmpp_free
    public :: xmpp_free_sm_state
    public :: xmpp_free_sm_state_
    public :: xmpp_get_default_logger
    public :: xmpp_global_timed_handler_add
    public :: xmpp_global_timed_handler_delete
    public :: xmpp_handler_add
    public :: xmpp_handler_add_
    public :: xmpp_handler_delete
    public :: xmpp_id_handler_add
    public :: xmpp_id_handler_add_
    public :: xmpp_id_handler_delete
    public :: xmpp_id_handler_delete_
    public :: xmpp_initialize
    public :: xmpp_iq_new
    public :: xmpp_iq_new_
    public :: xmpp_jid_bare
    public :: xmpp_jid_bare_
    public :: xmpp_jid_domain
    public :: xmpp_jid_domain_
    public :: xmpp_jid_new
    public :: xmpp_jid_new_
    public :: xmpp_jid_node
    public :: xmpp_jid_node_
    public :: xmpp_jid_resource
    public :: xmpp_jid_resource_
    public :: xmpp_message_get_body
    public :: xmpp_message_get_body_
    public :: xmpp_message_new
    public :: xmpp_message_new_
    public :: xmpp_message_set_body
    public :: xmpp_message_set_body_
    public :: xmpp_presence_new
    public :: xmpp_rand
    public :: xmpp_rand_bytes
    public :: xmpp_rand_free
    public :: xmpp_rand_free_
    public :: xmpp_rand_new
    public :: xmpp_rand_nonce
    public :: xmpp_run
    public :: xmpp_run_once
    public :: xmpp_send
    public :: xmpp_send_error
    public :: xmpp_send_error_
    public :: xmpp_send_raw
    public :: xmpp_send_raw_string
    public :: xmpp_send_raw_string_
    public :: xmpp_sha1
    public :: xmpp_sha1_
    public :: xmpp_sha1_digest
    public :: xmpp_sha1_final
    public :: xmpp_sha1_free
    public :: xmpp_sha1_free_
    public :: xmpp_sha1_new
    public :: xmpp_sha1_to_digest
    public :: xmpp_sha1_to_string
    public :: xmpp_sha1_to_string_
    public :: xmpp_sha1_update
    public :: xmpp_sha1_update_
    public :: xmpp_shutdown
    public :: xmpp_sockopt_cb_keepalive
    public :: xmpp_stanza_add_child
    public :: xmpp_stanza_add_child_ex
    public :: xmpp_stanza_clone
    public :: xmpp_stanza_copy
    public :: xmpp_stanza_del_attribute
    public :: xmpp_stanza_del_attribute_
    public :: xmpp_stanza_get_attribute
    public :: xmpp_stanza_get_attribute_
    public :: xmpp_stanza_get_attribute_count
    public :: xmpp_stanza_get_attributes
    public :: xmpp_stanza_get_attributes_
    public :: xmpp_stanza_get_child_by_name
    public :: xmpp_stanza_get_child_by_name_
    public :: xmpp_stanza_get_child_by_name_and_ns
    public :: xmpp_stanza_get_child_by_name_and_ns_
    public :: xmpp_stanza_get_child_by_ns
    public :: xmpp_stanza_get_child_by_ns_
    public :: xmpp_stanza_get_children
    public :: xmpp_stanza_get_context
    public :: xmpp_stanza_get_from
    public :: xmpp_stanza_get_from_
    public :: xmpp_stanza_get_id
    public :: xmpp_stanza_get_id_
    public :: xmpp_stanza_get_name
    public :: xmpp_stanza_get_name_
    public :: xmpp_stanza_get_next
    public :: xmpp_stanza_get_ns
    public :: xmpp_stanza_get_ns_
    public :: xmpp_stanza_get_text
    public :: xmpp_stanza_get_text_
    public :: xmpp_stanza_get_text_ptr
    public :: xmpp_stanza_get_text_ptr_
    public :: xmpp_stanza_get_to
    public :: xmpp_stanza_get_to_
    public :: xmpp_stanza_get_type
    public :: xmpp_stanza_get_type_
    public :: xmpp_stanza_is_tag
    public :: xmpp_stanza_is_text
    public :: xmpp_stanza_new
    public :: xmpp_stanza_new_from_string
    public :: xmpp_stanza_new_from_string_
    public :: xmpp_stanza_release
    public :: xmpp_stanza_reply
    public :: xmpp_stanza_reply_error
    public :: xmpp_stanza_reply_error_
    public :: xmpp_stanza_set_attribute
    public :: xmpp_stanza_set_attribute_
    public :: xmpp_stanza_set_from
    public :: xmpp_stanza_set_from_
    public :: xmpp_stanza_set_id
    public :: xmpp_stanza_set_id_
    public :: xmpp_stanza_set_name
    public :: xmpp_stanza_set_name_
    public :: xmpp_stanza_set_ns
    public :: xmpp_stanza_set_ns_
    public :: xmpp_stanza_set_text
    public :: xmpp_stanza_set_text_
    public :: xmpp_stanza_set_text_with_size
    public :: xmpp_stanza_set_to
    public :: xmpp_stanza_set_to_
    public :: xmpp_stanza_set_type
    public :: xmpp_stanza_set_type_
    public :: xmpp_stop
    public :: xmpp_timed_handler_add
    public :: xmpp_timed_handler_delete
    public :: xmpp_tlscert_free
    public :: xmpp_tlscert_free_
    public :: xmpp_tlscert_get_conn
    public :: xmpp_tlscert_get_ctx
    public :: xmpp_tlscert_get_description
    public :: xmpp_tlscert_get_description_
    public :: xmpp_tlscert_get_dnsname
    public :: xmpp_tlscert_get_dnsname_
    public :: xmpp_tlscert_get_pem
    public :: xmpp_tlscert_get_pem_
    public :: xmpp_tlscert_get_string
    public :: xmpp_tlscert_get_string_
    public :: xmpp_uuid_gen
    public :: xmpp_uuid_gen_
    public :: xmpp_version_check

    abstract interface
        ! int xmpp_certfail_handler(const xmpp_tlscert_t *cert, const char *errormsg)
        function xmpp_certfail_handler(cert, error_msg) bind(c)
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cert
            type(c_ptr), intent(in)        :: error_msg
            integer(kind=c_int)            :: xmpp_certfail_handler
        end function xmpp_certfail_handler

        ! void xmpp_conn_handler(xmpp_conn_t *conn, xmpp_conn_event_t event, int error, xmpp_stream_error_t *stream_error, void *userdata)
        subroutine xmpp_conn_handler(conn, event, error, stream_error, user_data) bind(c)
            import :: c_int, c_ptr, xmpp_stream_error_t
            implicit none
            type(c_ptr),               intent(in), value :: conn
            integer(kind=c_int),       intent(in), value :: event
            integer(kind=c_int),       intent(in), value :: error
            type(xmpp_stream_error_t), intent(in)        :: stream_error
            type(c_ptr),               intent(in), value :: user_data
        end subroutine xmpp_conn_handler

        ! int xmpp_global_timed_handler(xmpp_ctx_t *ctx, void *userdata)
        function xmpp_global_timed_handler(ctx, user_data) bind(c)
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            type(c_ptr), intent(in), value :: user_data
            integer(kind=c_int)            :: xmpp_global_timed_handler
        end function xmpp_global_timed_handler

        ! int xmpp_handler(xmpp_conn_t *conn, xmpp_stanza_t *stanza, void *userdata)
        function xmpp_handler(conn, stanza, user_data) bind(c)
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr), intent(in), value :: user_data
            integer(kind=c_int)            :: xmpp_handler
        end function xmpp_handler

        ! void xmpp_log_handler(void *userdata, xmpp_log_level_t level, const char *area, const char *msg)
        subroutine xmpp_log_handler(user_data, level, area, msg) bind(c)
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: user_data
            integer(kind=c_int),    intent(in), value :: level
            character(kind=c_char), intent(in)        :: area
            character(kind=c_char), intent(in)        :: msg
        end subroutine xmpp_log_handler

        ! int xmpp_password_callback(char *pw, size_t pw_max, xmpp_conn_t *conn, void *userdata)
        function xmpp_password_callback(pw, pw_max, conn, user_data) bind(c)
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: pw
            integer(kind=c_size_t), intent(in), value :: pw_max
            type(c_ptr),            intent(in), value :: conn
            type(c_ptr),            intent(in), value :: user_data
            integer(kind=c_int)                       :: xmpp_password_callback
        end function xmpp_password_callback

        ! int xmpp_sockopt_callback(xmpp_conn_t *conn, void *sock)
        function xmpp_sockopt_callback(conn, sock) bind(c)
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: sock
            integer(kind=c_int)            :: xmpp_sockopt_callback
        end function xmpp_sockopt_callback

        ! int xmpp_timed_handler(xmpp_conn_t *conn, void *userdata)
        function xmpp_timed_handler(conn, user_data) bind(c)
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: user_data
            integer(kind=c_int)            :: xmpp_timed_handler
        end function xmpp_timed_handler
    end interface

    interface
        ! void xmpp_base64_decode_bin(xmpp_ctx_t *ctx, const char *base64, size_t len, unsigned char **out, size_t *outlen)
        subroutine xmpp_base64_decode_bin_(ctx, base64, len, out, out_len) bind(c, name='xmpp_base64_decode_bin')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: base64
            integer(kind=c_size_t), intent(in), value :: len
            type(c_ptr),            intent(out)       :: out
            integer(kind=c_size_t), intent(out)       :: out_len
        end subroutine xmpp_base64_decode_bin_

        ! char *xmpp_base64_decode_str(xmpp_ctx_t *ctx, const char *base64, size_t len)
        function xmpp_base64_decode_str_(ctx, base64, len) bind(c, name='xmpp_base64_decode_str')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: base64
            integer(kind=c_size_t), intent(in), value :: len
            type(c_ptr)                               :: xmpp_base64_decode_str_
        end function xmpp_base64_decode_str_

        ! char *xmpp_base64_encode(xmpp_ctx_t *ctx, const unsigned char *data, size_t len)
        function xmpp_base64_encode_(ctx, data, len) bind(c, name='xmpp_base64_encode')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: data
            integer(kind=c_size_t), intent(in), value :: len
            type(c_ptr)                               :: xmpp_base64_encode_
        end function xmpp_base64_encode_

        ! char *xmpp_conn_cert_xmppaddr(xmpp_conn_t *conn, unsigned int n)
        function xmpp_conn_cert_xmppaddr_(conn, n) bind(c, name='xmpp_conn_cert_xmppaddr')
            import :: c_ptr, c_unsigned
            implicit none
            type(c_ptr),              intent(in), value :: conn
            integer(kind=c_unsigned), intent(in), value :: n
            type(c_ptr)                                 :: xmpp_conn_cert_xmppaddr_
        end function xmpp_conn_cert_xmppaddr_

        ! unsigned int xmpp_conn_cert_xmppaddr_num(xmpp_conn_t *conn)
        function xmpp_conn_cert_xmppaddr_num(conn) bind(c, name='xmpp_conn_cert_xmppaddr_num')
            import :: c_ptr, c_unsigned
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(kind=c_unsigned)       :: xmpp_conn_cert_xmppaddr_num
        end function xmpp_conn_cert_xmppaddr_num

        ! xmpp_conn_t *xmpp_conn_clone(xmpp_conn_t *conn)
        function xmpp_conn_clone(conn) bind(c, name='xmpp_conn_clone')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr)                    :: xmpp_conn_clone
        end function xmpp_conn_clone

        ! const char *xmpp_conn_get_bound_jid(const xmpp_conn_t *conn)
        function xmpp_conn_get_bound_jid_(conn) bind(c, name='xmpp_conn_get_bound_jid')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr)                    :: xmpp_conn_get_bound_jid_
        end function xmpp_conn_get_bound_jid_

        ! xmpp_ctx_t *xmpp_conn_get_context(xmpp_conn_t *conn)
        function xmpp_conn_get_context(conn) bind(c, name='xmpp_conn_get_context')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr)                    :: xmpp_conn_get_context
        end function xmpp_conn_get_context

        ! long xmpp_conn_get_flags(const xmpp_conn_t *conn)
        function xmpp_conn_get_flags(conn) bind(c, name='xmpp_conn_get_flags')
            import :: c_long, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(kind=c_long)           :: xmpp_conn_get_flags
        end function xmpp_conn_get_flags

        ! const char *xmpp_conn_get_jid(const xmpp_conn_t *conn)
        function xmpp_conn_get_jid_(conn) bind(c, name='xmpp_conn_get_jid')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr)                    :: xmpp_conn_get_jid_
        end function xmpp_conn_get_jid_

        ! const char *xmpp_conn_get_keyfile(const xmpp_conn_t *conn)
        function xmpp_conn_get_keyfile_(conn) bind(c, name='xmpp_conn_get_keyfile')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr)                    :: xmpp_conn_get_keyfile_
        end function xmpp_conn_get_keyfile_

        ! const char *xmpp_conn_get_pass(const xmpp_conn_t *conn)
        function xmpp_conn_get_pass_(conn) bind(c, name='xmpp_conn_get_pass')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr)                    :: xmpp_conn_get_pass_
        end function xmpp_conn_get_pass_

        ! xmpp_tlscert_t *xmpp_conn_get_peer_cert(xmpp_conn_t *const conn)
        function xmpp_conn_get_peer_cert() bind(c, name='xmpp_conn_get_peer_cert')
            import :: c_ptr
            implicit none
            type(c_ptr) :: xmpp_conn_get_peer_cert
        end function xmpp_conn_get_peer_cert

        ! xmpp_sm_state_t *xmpp_conn_get_sm_state(xmpp_conn_t *conn)
        function xmpp_conn_get_sm_state(conn) bind(c, name='xmpp_conn_get_sm_state')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr)                    :: xmpp_conn_get_sm_state
        end function xmpp_conn_get_sm_state

        ! int xmpp_conn_is_connected(xmpp_conn_t *conn)
        function xmpp_conn_is_connected(conn) bind(c, name='xmpp_conn_is_connected')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(kind=c_int)            :: xmpp_conn_is_connected
        end function xmpp_conn_is_connected

        ! int xmpp_conn_is_connecting(xmpp_conn_t *conn)
        function xmpp_conn_is_connecting(conn) bind(c, name='xmpp_conn_is_connecting')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(kind=c_int)            :: xmpp_conn_is_connecting
        end function xmpp_conn_is_connecting

        ! int xmpp_conn_is_disconnected(xmpp_conn_t *conn)
        function xmpp_conn_is_disconnected(conn) bind(c, name='xmpp_conn_is_disconnected')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(kind=c_int)            :: xmpp_conn_is_disconnected
        end function xmpp_conn_is_disconnected

        ! int xmpp_conn_is_secured(xmpp_conn_t *conn)
        function xmpp_conn_is_secured(conn) bind(c, name='xmpp_conn_is_secured')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(kind=c_int)            :: xmpp_conn_is_secured
        end function xmpp_conn_is_secured

        ! xmpp_conn_t *xmpp_conn_new(xmpp_ctx_t *ctx)
        function xmpp_conn_new(ctx) bind(c, name='xmpp_conn_new')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            type(c_ptr)                    :: xmpp_conn_new
        end function xmpp_conn_new

        ! int xmpp_conn_open_stream(xmpp_conn_t *conn, char **attributes, size_t attributes_len)
        function xmpp_conn_open_stream_(conn, attributes, attributes_len) bind(c, name='xmpp_conn_open_stream')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: conn
            type(c_ptr),            intent(in)        :: attributes(*)
            integer(kind=c_size_t), intent(in), value :: attributes_len
            integer(kind=c_int)                       :: xmpp_conn_open_stream_
        end function xmpp_conn_open_stream_

        ! int xmpp_conn_open_stream_default(xmpp_conn_t *conn)
        function xmpp_conn_open_stream_default(conn) bind(c, name='xmpp_conn_open_stream_default')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(kind=c_int)            :: xmpp_conn_open_stream_default
        end function xmpp_conn_open_stream_default

        ! int xmpp_conn_release(xmpp_conn_t *conn)
        function xmpp_conn_release(conn) bind(c, name='xmpp_conn_release')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(kind=c_int)            :: xmpp_conn_release
        end function xmpp_conn_release

        ! char *xmpp_conn_send_queue_drop_element(xmpp_conn_t *conn, xmpp_queue_element_t which)
        function xmpp_conn_send_queue_drop_element_(conn, which) bind(c, name='xmpp_conn_send_queue_drop_element')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: conn
            integer(kind=c_int), intent(in), value :: which
            type(c_ptr)                            :: xmpp_conn_send_queue_drop_element_
        end function xmpp_conn_send_queue_drop_element_

        ! int xmpp_conn_send_queue_len(const xmpp_conn_t *conn)
        function xmpp_conn_send_queue_len(conn) bind(c, name='xmpp_conn_send_queue_len')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(kind=c_int)            :: xmpp_conn_send_queue_len
        end function xmpp_conn_send_queue_len

        ! void xmpp_conn_set_cafile(const xmpp_conn_t *conn, const char *path)
        subroutine xmpp_conn_set_cafile_(conn, path) bind(c, name='xmpp_conn_set_cafile')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: conn
            character(kind=c_char), intent(in)        :: path
        end subroutine xmpp_conn_set_cafile_

        ! void xmpp_conn_set_capath(const xmpp_conn_t *conn, const char *path)
        subroutine xmpp_conn_set_capath_(conn, path) bind(c, name='xmpp_conn_set_capath')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: conn
            character(kind=c_char), intent(in)        :: path
        end subroutine xmpp_conn_set_capath_

        ! void xmpp_conn_set_certfail_handler(const xmpp_conn_t *conn, xmpp_certfail_handler hndl)
        subroutine xmpp_conn_set_certfail_handler(conn, handler) bind(c, name='xmpp_conn_set_certfail_handler')
            import :: c_ptr, xmpp_certfail_handler
            implicit none
            type(c_ptr), intent(in), value   :: conn
            procedure(xmpp_certfail_handler) :: handler
        end subroutine xmpp_conn_set_certfail_handler

        ! void xmpp_conn_set_client_cert(xmpp_conn_t *conn, const char *cert, const char *key)
        subroutine xmpp_conn_set_client_cert_(conn, cert, key) bind(c, name='xmpp_conn_set_client_cert')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: conn
            character(kind=c_char), intent(in)        :: cert
            character(kind=c_char), intent(in)        :: key
        end subroutine xmpp_conn_set_client_cert_

        ! int xmpp_conn_set_flags(xmpp_conn_t *conn, long flags)
        function xmpp_conn_set_flags(conn, flags) bind(c, name='xmpp_conn_set_flags')
            import :: c_int, c_long, c_ptr
            implicit none
            type(c_ptr),          intent(in), value :: conn
            integer(kind=c_long), intent(in), value :: flags
            integer(kind=c_int)                     :: xmpp_conn_set_flags
        end function xmpp_conn_set_flags

        ! void xmpp_conn_set_jid(xmpp_conn_t *conn, const char *jid)
        subroutine xmpp_conn_set_jid_(conn, jid) bind(c, name='xmpp_conn_set_jid')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: conn
            character(kind=c_char), intent(in)        :: jid
        end subroutine xmpp_conn_set_jid_

        ! void xmpp_conn_set_pass(xmpp_conn_t *conn, const char *pass)
        subroutine xmpp_conn_set_pass_(conn, pass) bind(c, name='xmpp_conn_set_pass')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: conn
            character(kind=c_char), intent(in)        :: pass
        end subroutine xmpp_conn_set_pass_

        ! void xmpp_conn_set_password_callback(xmpp_conn_t *conn, xmpp_password_callback cb, void *userdata)
        subroutine xmpp_conn_set_password_callback(conn, callback, user_data) bind(c, name='xmpp_conn_set_password_callback')
            import :: c_ptr, xmpp_password_callback
            implicit none
            type(c_ptr),    intent(in), value :: conn
            procedure(xmpp_password_callback) :: callback
            type(c_ptr),    intent(in), value :: user_data
        end subroutine xmpp_conn_set_password_callback

        ! void xmpp_conn_set_password_retries(xmpp_conn_t *conn, unsigned int retries)
        subroutine xmpp_conn_set_password_retries(conn, retries) bind(c, name='xmpp_conn_set_password_retries')
            import :: c_ptr, c_unsigned
            implicit none
            type(c_ptr),              intent(in), value :: conn
            integer(kind=c_unsigned), intent(in), value :: retries
        end subroutine xmpp_conn_set_password_retries

        ! int xmpp_conn_set_sm_state(xmpp_conn_t *conn, xmpp_sm_state_t *sm_state)
        function xmpp_conn_set_sm_state(conn, sm_state) bind(c, name='xmpp_conn_set_sm_state')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: sm_state
            integer(kind=c_int)            :: xmpp_conn_set_sm_state
        end function xmpp_conn_set_sm_state

        ! void xmpp_conn_set_sockopt_callback(xmpp_conn_t *conn, xmpp_sockopt_callback callback)
        subroutine xmpp_conn_set_sockopt_callback(conn, callback) bind(c, name='xmpp_conn_set_sockopt_callback')
            import :: c_ptr, xmpp_sockopt_callback
            implicit none
            type(c_ptr),    intent(in), value :: conn
            procedure(xmpp_sockopt_callback)  :: callback
        end subroutine xmpp_conn_set_sockopt_callback

        ! int xmpp_conn_tls_start(xmpp_conn_t *conn)
        function xmpp_conn_tls_start(conn) bind(c, name='xmpp_conn_tls_start')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(kind=c_int)            :: xmpp_conn_tls_start
        end function xmpp_conn_tls_start

        ! int xmpp_connect_client(xmpp_conn_t *conn, const char *altdomain, unsigned short altport, xmpp_conn_handler callback, void *userdata)
        function xmpp_connect_client_(conn, alt_domain, alt_port, callback, user_data) bind(c, name='xmpp_connect_client')
            import :: c_char, c_int, c_ptr, c_unsigned_short
            implicit none
            type(c_ptr),                    intent(in), value :: conn
            character(kind=c_char),         intent(in)        :: alt_domain
            integer(kind=c_unsigned_short), intent(in), value :: alt_port
            procedure(xmpp_conn_handler)                      :: callback
            type(c_ptr),                    intent(in), value :: user_data
            integer(kind=c_int)                               :: xmpp_connect_client_
        end function xmpp_connect_client_

        ! int xmpp_connect_component(xmpp_conn_t *conn, const char *server, unsigned short port, xmpp_conn_handler callback, void *userdata)
        function xmpp_connect_component_(conn, server, port, callback, user_data) bind(c, name='xmpp_connect_component')
            import :: c_char, c_int, c_ptr, c_unsigned_short, xmpp_conn_handler
            implicit none
            type(c_ptr),                    intent(in), value :: conn
            character(kind=c_char),         intent(in)        :: server
            integer(kind=c_unsigned_short), intent(in), value :: port
            procedure(xmpp_conn_handler)                      :: callback
            type(c_ptr),                    intent(in), value :: user_data
            integer(kind=c_int)                               :: xmpp_connect_component_
        end function xmpp_connect_component_

        ! int xmpp_connect_raw(xmpp_conn_t *conn, const char *altdomain, unsigned short altport, xmpp_conn_handler callback, void *userdata)
        function xmpp_connect_raw_(conn, alt_domain, alt_port, callback, user_data) bind(c, name='xmpp_connect_raw')
            import :: c_char, c_int, c_ptr, c_unsigned_short, xmpp_conn_handler
            implicit none
            type(c_ptr),                    intent(in), value :: conn
            character(kind=c_char),         intent(in)        :: alt_domain
            integer(kind=c_unsigned_short), intent(in), value :: alt_port
            procedure(xmpp_conn_handler)                      :: callback
            type(c_ptr),                    intent(in), value :: user_data
            integer(kind=c_int)                               :: xmpp_connect_raw_
        end function xmpp_connect_raw_

        ! void xmpp_ctx_free(xmpp_ctx_t *ctx)
        subroutine xmpp_ctx_free_(ctx) bind(c, name='xmpp_ctx_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
        end subroutine xmpp_ctx_free_

        ! xmpp_ctx_t *xmpp_ctx_new(const xmpp_mem_t *mem, const xmpp_log_t *log)
        function xmpp_ctx_new(mem, log) bind(c, name='xmpp_ctx_new')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: mem
            type(c_ptr), intent(in), value :: log
            type(c_ptr)                    :: xmpp_ctx_new
        end function xmpp_ctx_new

        ! void xmpp_ctx_set_timeout(xmpp_ctx_t *ctx, unsigned long timeout)
        subroutine xmpp_ctx_set_timeout(ctx, timeout) bind(c, name='xmpp_ctx_set_timeout')
            import :: c_ptr, c_unsigned_long
            implicit none
            type(c_ptr),                   intent(in), value :: ctx
            integer(kind=c_unsigned_long), intent(in), value :: timeout
        end subroutine xmpp_ctx_set_timeout

        ! void xmpp_disconnect(xmpp_conn_t *conn)
        subroutine xmpp_disconnect(conn) bind(c, name='xmpp_disconnect')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
        end subroutine xmpp_disconnect

        ! xmpp_stanza_t *xmpp_error_new(xmpp_ctx_t *ctx, xmpp_error_type_t type, const char *text)
        function xmpp_error_new_(ctx, type, text) bind(c, name='xmpp_error_new')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            integer(kind=c_int),    intent(in), value :: type
            character(kind=c_char), intent(in)        :: text
            type(c_ptr)                               :: xmpp_error_new_
        end function xmpp_error_new_

        ! void xmpp_free(const xmpp_ctx_t *ctx, void *p)
        subroutine xmpp_free(ctx, ptr) bind(c, name='xmpp_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            type(c_ptr), intent(in), value :: ptr
        end subroutine xmpp_free

        ! void xmpp_free_sm_state(xmpp_sm_state_t *sm_state)
        subroutine xmpp_free_sm_state_(sm_state) bind(c, name='xmpp_free_sm_state')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: sm_state
        end subroutine xmpp_free_sm_state_

        ! xmpp_log_t *xmpp_get_default_logger(xmpp_log_level_t level)
        function xmpp_get_default_logger(level) bind(c, name='xmpp_get_default_logger')
            import :: c_int, c_ptr
            implicit none
            integer(kind=c_int), intent(in), value :: level
            type(c_ptr)                            :: xmpp_get_default_logger
        end function xmpp_get_default_logger

        ! void xmpp_global_timed_handler_add(xmpp_ctx_t *ctx, xmpp_global_timed_handler handler, unsigned long period, void *userdata)
        subroutine xmpp_global_timed_handler_add(ctx, handler, period, user_data) bind(c, name='xmpp_global_timed_handler_add')
            import :: c_ptr, c_unsigned_long, xmpp_global_timed_handler
            implicit none
            type(c_ptr),                   intent(in), value :: ctx
            procedure(xmpp_global_timed_handler)             :: handler
            integer(kind=c_unsigned_long), intent(in), value :: period
            type(c_ptr),                   intent(in), value :: user_data
        end subroutine xmpp_global_timed_handler_add

        ! void xmpp_global_timed_handler_delete(xmpp_ctx_t *ctx, xmpp_global_timed_handler handler)
        subroutine xmpp_global_timed_handler_delete(ctx, handler) bind(c, name='xmpp_global_timed_handler_delete')
            import :: c_ptr, xmpp_global_timed_handler
            implicit none
            type(c_ptr), intent(in), value       :: ctx
            procedure(xmpp_global_timed_handler) :: handler
        end subroutine xmpp_global_timed_handler_delete

        ! void xmpp_handler_add(xmpp_conn_t *conn, xmpp_handler handler, const char *ns, const char *name, const char *type, void *userdata)
        subroutine xmpp_handler_add_(conn, handler, ns, name, type, user_data) bind(c, name='xmpp_handler_add')
            import :: c_ptr, xmpp_handler
            implicit none
            type(c_ptr), intent(in), value :: conn
            procedure(xmpp_handler)        :: handler
            type(c_ptr), intent(in), value :: ns
            type(c_ptr), intent(in), value :: name
            type(c_ptr), intent(in), value :: type
            type(c_ptr), intent(in), value :: user_data
        end subroutine xmpp_handler_add_

        ! void xmpp_handler_delete(xmpp_conn_t *conn, xmpp_handler handler)
        subroutine xmpp_handler_delete(conn, handler) bind(c, name='xmpp_handler_delete')
            import :: c_ptr, xmpp_handler
            implicit none
            type(c_ptr),    intent(in), value :: conn
            procedure(xmpp_handler)           :: handler
        end subroutine xmpp_handler_delete

        ! void xmpp_id_handler_add(xmpp_conn_t *conn, xmpp_handler handler, const char *id, void *userdata)
        subroutine xmpp_id_handler_add_(conn, handler, id, user_data) bind(c, name='xmpp_id_handler_add')
            import :: c_char, c_ptr, xmpp_handler
            implicit none
            type(c_ptr),            intent(in), value :: conn
            procedure(xmpp_handler)                   :: handler
            character(kind=c_char), intent(in)        :: id
            type(c_ptr),            intent(in), value :: user_data
        end subroutine xmpp_id_handler_add_

        ! void xmpp_id_handler_delete(xmpp_conn_t *conn, xmpp_handler handler, const char *id)
        subroutine xmpp_id_handler_delete_(conn, handler, id) bind(c, name='xmpp_id_handler_delete')
            import :: c_char, c_ptr, xmpp_handler
            implicit none
            type(c_ptr),            intent(in), value :: conn
            procedure(xmpp_handler)                   :: handler
            character(kind=c_char), intent(in)        :: id
        end subroutine xmpp_id_handler_delete_

        ! void xmpp_initialize(void)
        subroutine xmpp_initialize() bind(c, name='xmpp_initialize')
        end subroutine xmpp_initialize

        ! xmpp_stanza_t *xmpp_iq_new(xmpp_ctx_t *ctx, const char *type, const char *id)
        function xmpp_iq_new_(ctx, type, id) bind(c, name='xmpp_iq_new')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: type
            character(kind=c_char), intent(in)        :: id
            type(c_ptr)                               :: xmpp_iq_new_
        end function xmpp_iq_new_

        ! char *xmpp_jid_bare(xmpp_ctx_t *ctx, const char *jid)
        function xmpp_jid_bare_(ctx, jid) bind(c, name='xmpp_jid_bare')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: jid
            type(c_ptr)                               :: xmpp_jid_bare_
        end function xmpp_jid_bare_

        ! char *xmpp_jid_domain(xmpp_ctx_t *ctx, const char *jid)
        function xmpp_jid_domain_(ctx, jid) bind(c, name='xmpp_jid_domain')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: jid
            type(c_ptr)                               :: xmpp_jid_domain_
        end function xmpp_jid_domain_

        ! char *xmpp_jid_new(xmpp_ctx_t *ctx, const char *node, const char *domain, const char *resource)
        function xmpp_jid_new_(ctx, node, domain, resource) bind(c, name='xmpp_jid_new')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: node
            character(kind=c_char), intent(in)        :: domain
            character(kind=c_char), intent(in)        :: resource
            type(c_ptr)                               :: xmpp_jid_new_
        end function xmpp_jid_new_

        ! char *xmpp_jid_node(xmpp_ctx_t *ctx, const char *jid)
        function xmpp_jid_node_(ctx, jid) bind(c, name='xmpp_jid_node')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: jid
            type(c_ptr)                               :: xmpp_jid_node_
        end function xmpp_jid_node_

        ! char *xmpp_jid_resource(xmpp_ctx_t *ctx, const char *jid)
        function xmpp_jid_resource_(ctx, jid) bind(c, name='xmpp_jid_resource')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: jid
            type(c_ptr)                               :: xmpp_jid_resource_
        end function xmpp_jid_resource_

        ! char *xmpp_message_get_body(xmpp_stanza_t *msg)
        function xmpp_message_get_body_(msg) bind(c, name='xmpp_message_get_body')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: msg
            type(c_ptr)                    :: xmpp_message_get_body_
        end function xmpp_message_get_body_

        ! xmpp_stanza_t *xmpp_message_new(xmpp_ctx_t *ctx, const char *type, const char *to, const char *id)
        function xmpp_message_new_(ctx, type, to, id) bind(c, name='xmpp_message_new')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: type
            character(kind=c_char), intent(in)        :: to
            character(kind=c_char), intent(in)        :: id
            type(c_ptr)                               :: xmpp_message_new_
        end function xmpp_message_new_

        ! int xmpp_message_set_body(xmpp_stanza_t *msg, const char *text)
        function xmpp_message_set_body_(msg, text) bind(c, name='xmpp_message_set_body')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: msg
            character(kind=c_char), intent(in)        :: text
            integer(kind=c_int)                       :: xmpp_message_set_body_
        end function xmpp_message_set_body_

        ! xmpp_stanza_t *xmpp_presence_new(xmpp_ctx_t *ctx)
        function xmpp_presence_new(ctx) bind(c, name='xmpp_presence_new')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            type(c_ptr)                    :: xmpp_presence_new
        end function xmpp_presence_new

        ! int xmpp_rand(xmpp_rand_t *rand)
        function xmpp_rand(rand) bind(c, name='xmpp_rand')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: rand
            integer(kind=c_int)            :: xmpp_rand
        end function xmpp_rand

        ! void xmpp_rand_bytes(xmpp_rand_t *rand, unsigned char *output, size_t len)
        subroutine xmpp_rand_bytes(rand, output, len) bind(c, name='xmpp_rand_bytes')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: rand
            character(kind=c_char), intent(inout)     :: output
            integer(kind=c_size_t), intent(in), value :: len
        end subroutine xmpp_rand_bytes

        ! void xmpp_rand_free(xmpp_ctx_t *ctx, xmpp_rand_t *rand)
        subroutine xmpp_rand_free_(ctx, rand) bind(c, name='xmpp_rand_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            type(c_ptr), intent(in), value :: rand
        end subroutine xmpp_rand_free_

        ! xmpp_rand_t *xmpp_rand_new(xmpp_ctx_t *ctx)
        function xmpp_rand_new(ctx) bind(c, name='xmpp_rand_new')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            type(c_ptr)                    :: xmpp_rand_new
        end function xmpp_rand_new

        ! void xmpp_rand_nonce(xmpp_rand_t *rand, char *output, size_t len)
        subroutine xmpp_rand_nonce(rand, output, len) bind(c, name='xmpp_rand_nonce')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: rand
            character(kind=c_char), intent(inout)     :: output
            integer(kind=c_size_t), intent(in), value :: len
        end subroutine xmpp_rand_nonce

        ! void xmpp_run(xmpp_ctx_t *ctx)
        subroutine xmpp_run(ctx) bind(c, name='xmpp_run')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
        end subroutine xmpp_run

        ! void xmpp_run_once(xmpp_ctx_t *ctx, unsigned long timeout)
        subroutine xmpp_run_once(ctx, timeout) bind(c, name='xmpp_run_once')
            import :: c_ptr, c_unsigned_long
            implicit none
            type(c_ptr),                   intent(in), value :: ctx
            integer(kind=c_unsigned_long), intent(in), value :: timeout
        end subroutine xmpp_run_once

        ! void xmpp_send(xmpp_conn_t *conn, xmpp_stanza_t *stanza)
        subroutine xmpp_send(conn, stanza) bind(c, name='xmpp_send')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: stanza
        end subroutine xmpp_send

        ! void xmpp_send_error(xmpp_conn_t *conn, xmpp_error_type_t type, char *text)
        subroutine xmpp_send_error_(conn, type, text) bind(c, name='xmpp_send_error')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: conn
            integer(kind=c_int),    intent(in), value :: type
            character(kind=c_char), intent(in)        :: text
        end subroutine xmpp_send_error_

        ! void xmpp_send_raw(xmpp_conn_t *conn, const char *data, size_t len)
        subroutine xmpp_send_raw(conn, data, len) bind(c, name='xmpp_send_raw')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: conn
            character(kind=c_char), intent(in)        :: data
            integer(kind=c_size_t), intent(in), value :: len
        end subroutine xmpp_send_raw

        ! void xmpp_send_raw_string_(xmpp_conn_t *conn, const char *str)
        subroutine xmpp_send_raw_string_(conn, str) bind(c, name='xmpp_send_raw_string_')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: conn
            character(kind=c_char), intent(in)        :: str
        end subroutine xmpp_send_raw_string_

        ! char *xmpp_sha1(xmpp_ctx_t *ctx, const unsigned char *data, size_t len)
        function xmpp_sha1_(ctx, data, data_len) bind(c, name='xmpp_sha1')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: data
            integer(kind=c_size_t), intent(in), value :: data_len
            type(c_ptr)                               :: xmpp_sha1_
        end function xmpp_sha1_

        ! void xmpp_sha1_digest(const unsigned char *data, size_t len, unsigned char *digest)
        subroutine xmpp_sha1_digest(data, len, digest) bind(c, name='xmpp_sha1_digest')
            import :: c_char, c_size_t
            implicit none
            character(kind=c_char), intent(in)        :: data
            integer(kind=c_size_t), intent(in), value :: len
            character(kind=c_char), intent(in)        :: digest
        end subroutine xmpp_sha1_digest

        ! void xmpp_sha1_final(xmpp_sha1_t *sha1)
        subroutine xmpp_sha1_final(sha1) bind(c, name='xmpp_sha1_final')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: sha1
        end subroutine xmpp_sha1_final

        ! void xmpp_sha1_free(xmpp_sha1_t *sha1)
        subroutine xmpp_sha1_free_(sha1) bind(c, name='xmpp_sha1_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: sha1
        end subroutine xmpp_sha1_free_

        ! xmpp_sha1_t *xmpp_sha1_new(xmpp_ctx_t *ctx)
        function xmpp_sha1_new(ctx) bind(c, name='xmpp_sha1_new')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            type(c_ptr)                    :: xmpp_sha1_new
        end function xmpp_sha1_new

        ! void xmpp_sha1_to_digest(xmpp_sha1_t *sha1, unsigned char *digest)
        subroutine xmpp_sha1_to_digest(sha1, digest) bind(c, name='xmpp_sha1_to_digest')
            import :: XMPP_SHA1_DIGEST_SIZE, c_ptr, c_unsigned_char
            implicit none
            type(c_ptr),                   intent(in), value :: sha1
            integer(kind=c_unsigned_char), intent(inout)     :: digest(XMPP_SHA1_DIGEST_SIZE)
        end subroutine xmpp_sha1_to_digest

        ! char *xmpp_sha1_to_string(xmpp_sha1_t *sha1, char *s, size_t slen)
        function xmpp_sha1_to_string_(sha1, str, str_len) bind(c, name='xmpp_sha1_to_string')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: sha1
            character(kind=c_char), intent(inout)     :: str
            integer(kind=c_size_t), intent(in), value :: str_len
            type(c_ptr)                               :: xmpp_sha1_to_string_
        end function xmpp_sha1_to_string_

        ! void xmpp_sha1_update(xmpp_sha1_t *sha1, const unsigned char *data, size_t len)
        subroutine xmpp_sha1_update_(sha1, data, data_len) bind(c, name='xmpp_sha1_update')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: sha1
            character(kind=c_char), intent(in)        :: data
            integer(kind=c_size_t), intent(in), value :: data_len
        end subroutine xmpp_sha1_update_

        ! void xmpp_shutdown(void)
        subroutine xmpp_shutdown() bind(c, name='xmpp_shutdown')
        end subroutine xmpp_shutdown

        ! int xmpp_sockopt_cb_keepalive(xmpp_conn_t *conn, void *sock)
        function xmpp_sockopt_cb_keepalive(conn, sock) bind(c, name='xmpp_sockopt_cb_keepalive')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: sock
            integer(kind=c_int)            :: xmpp_sockopt_cb_keepalive
        end function xmpp_sockopt_cb_keepalive

        ! int xmpp_stanza_add_child(xmpp_stanza_t *stanza, xmpp_stanza_t *child)
        function xmpp_stanza_add_child(stanza, child) bind(c, name='xmpp_stanza_add_child')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr), intent(in), value :: child
            integer(kind=c_int)            :: xmpp_stanza_add_child
        end function xmpp_stanza_add_child

        ! int xmpp_stanza_add_child_ex(xmpp_stanza_t *stanza, xmpp_stanza_t *child, int do_clone)
        function xmpp_stanza_add_child_ex(stanza, child, do_clone) bind(c, name='xmpp_stanza_add_child_ex')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stanza
            type(c_ptr),         intent(in), value :: child
            integer(kind=c_int), intent(in), value :: do_clone
            integer(kind=c_int)                    :: xmpp_stanza_add_child_ex
        end function xmpp_stanza_add_child_ex

        ! xmpp_stanza_t *xmpp_stanza_clone(xmpp_stanza_t *stanza)
        function xmpp_stanza_clone(stanza) bind(c, name='xmpp_stanza_clone')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_clone
        end function xmpp_stanza_clone

        ! xmpp_stanza_t *xmpp_stanza_copy(const xmpp_stanza_t *stanza)
        function xmpp_stanza_copy(stanza) bind(c, name='xmpp_stanza_copy')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_copy
        end function xmpp_stanza_copy

        ! int xmpp_stanza_del_attribute(xmpp_stanza_t *stanza, const char *name)
        function xmpp_stanza_del_attribute_(stanza, name) bind(c, name='xmpp_stanza_del_attribute')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: name
            integer(kind=c_int)                       :: xmpp_stanza_del_attribute_
        end function xmpp_stanza_del_attribute_

        ! const char *xmpp_stanza_get_attribute(xmpp_stanza_t *stanza, const char *name)
        function xmpp_stanza_get_attribute_(stanza, name) bind(c, name='xmpp_stanza_get_attribute')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: name
            type(c_ptr)                               :: xmpp_stanza_get_attribute_
        end function xmpp_stanza_get_attribute_

        ! int xmpp_stanza_get_attribute_count(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_attribute_count(stanza) bind(c, name='xmpp_stanza_get_attribute_count')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            integer(kind=c_int)            :: xmpp_stanza_get_attribute_count
        end function xmpp_stanza_get_attribute_count

        ! int xmpp_stanza_get_attributes(xmpp_stanza_t *stanza, const char **attr, int attrlen)
        function xmpp_stanza_get_attributes_(stanza, attr, attr_len) bind(c, name='xmpp_stanza_get_attributes')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stanza
            type(c_ptr),         intent(out)       :: attr(*)
            integer(kind=c_int), intent(in), value :: attr_len
            integer(kind=c_int)                    :: xmpp_stanza_get_attributes_
        end function xmpp_stanza_get_attributes_

        ! xmpp_stanza_t *xmpp_stanza_get_child_by_name(xmpp_stanza_t *stanza, const char *name)
        function xmpp_stanza_get_child_by_name_(stanza, name) bind(c, name='xmpp_stanza_get_child_by_name')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: name
            type(c_ptr)                               :: xmpp_stanza_get_child_by_name_
        end function xmpp_stanza_get_child_by_name_

        ! xmpp_stanza_t *xmpp_stanza_get_child_by_name_and_ns(xmpp_stanza_t *stanza, const char *name, const char *ns)
        function xmpp_stanza_get_child_by_name_and_ns_(stanza, name, ns) bind(c, name='xmpp_stanza_get_child_by_name_and_ns')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: name
            character(kind=c_char), intent(in)        :: ns
            type(c_ptr)                               :: xmpp_stanza_get_child_by_name_and_ns_
        end function xmpp_stanza_get_child_by_name_and_ns_

        ! xmpp_stanza_t *xmpp_stanza_get_child_by_ns(xmpp_stanza_t *stanza, const char *ns)
        function xmpp_stanza_get_child_by_ns_(stanza, ns) bind(c, name='xmpp_stanza_get_child_by_ns')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: ns
            type(c_ptr)                               :: xmpp_stanza_get_child_by_ns_
        end function xmpp_stanza_get_child_by_ns_

        ! xmpp_stanza_t *xmpp_stanza_get_children(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_children(stanza) bind(c, name='xmpp_stanza_get_children')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_children
        end function xmpp_stanza_get_children

        ! xmpp_ctx_t *xmpp_stanza_get_context(const xmpp_stanza_t *stanza)
        function xmpp_stanza_get_context(stanza) bind(c, name='xmpp_stanza_get_context')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_context
        end function xmpp_stanza_get_context

        ! const char *xmpp_stanza_get_from(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_from_(stanza) bind(c, name='xmpp_stanza_get_from')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_from_
        end function xmpp_stanza_get_from_

        ! const char *xmpp_stanza_get_id(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_id_(stanza) bind(c, name='xmpp_stanza_get_id')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_id_
        end function xmpp_stanza_get_id_

        ! const char *xmpp_stanza_get_name(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_name_(stanza) bind(c, name='xmpp_stanza_get_name')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_name_
        end function xmpp_stanza_get_name_

        ! xmpp_stanza_t *xmpp_stanza_get_next(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_next(stanza) bind(c, name='xmpp_stanza_get_next')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_next
        end function xmpp_stanza_get_next

        ! const char *xmpp_stanza_get_ns(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_ns_(stanza) bind(c, name='xmpp_stanza_get_ns')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_ns_
        end function xmpp_stanza_get_ns_

        ! char *xmpp_stanza_get_text(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_text_(stanza) bind(c, name='xmpp_stanza_get_text')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_text_
        end function xmpp_stanza_get_text_

        ! const char *xmpp_stanza_get_text_ptr(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_text_ptr_(stanza) bind(c, name='xmpp_stanza_get_text_ptr')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_text_ptr_
        end function xmpp_stanza_get_text_ptr_

        ! const char *xmpp_stanza_get_to(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_to_(stanza) bind(c, name='xmpp_stanza_get_to')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_to_
        end function xmpp_stanza_get_to_

        ! const char *xmpp_stanza_get_type(xmpp_stanza_t *stanza)
        function xmpp_stanza_get_type_(stanza) bind(c, name='xmpp_stanza_get_type')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_get_type_
        end function xmpp_stanza_get_type_

        ! int xmpp_stanza_is_tag(xmpp_stanza_t *stanza)
        function xmpp_stanza_is_tag(stanza) bind(c, name='xmpp_stanza_is_tag')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            integer(kind=c_int)            :: xmpp_stanza_is_tag
        end function xmpp_stanza_is_tag

        ! int xmpp_stanza_is_text(xmpp_stanza_t *stanza)
        function xmpp_stanza_is_text(stanza) bind(c, name='xmpp_stanza_is_text')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            integer(kind=c_int)            :: xmpp_stanza_is_text
        end function xmpp_stanza_is_text

        ! xmpp_stanza_t *xmpp_stanza_new(xmpp_ctx_t *ctx)
        function xmpp_stanza_new(ctx) bind(c, name='xmpp_stanza_new')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            type(c_ptr)                    :: xmpp_stanza_new
        end function xmpp_stanza_new

        ! xmpp_stanza_t *xmpp_stanza_new_from_string(xmpp_ctx_t *ctx, const char *str)
        function xmpp_stanza_new_from_string_(ctx, str) bind(c, name='xmpp_stanza_new_from_string')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: ctx
            character(kind=c_char), intent(in)        :: str
            type(c_ptr)                               :: xmpp_stanza_new_from_string_
        end function xmpp_stanza_new_from_string_

        ! int xmpp_stanza_release(xmpp_stanza_t *stanza)
        function xmpp_stanza_release(stanza) bind(c, name='xmpp_stanza_release')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            integer(kind=c_int)            :: xmpp_stanza_release
        end function xmpp_stanza_release

        ! xmpp_stanza_t *xmpp_stanza_reply(xmpp_stanza_t *stanza)
        function xmpp_stanza_reply(stanza) bind(c, name='xmpp_stanza_reply')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: stanza
            type(c_ptr)                    :: xmpp_stanza_reply
        end function xmpp_stanza_reply

        ! xmpp_stanza_t *xmpp_stanza_reply_error(xmpp_stanza_t *stanza, const char *error_type, const char *condition, const char *text)
        function xmpp_stanza_reply_error_(stanza, error_type, condition, text) bind(c, name='xmpp_stanza_reply_error')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: error_type
            character(kind=c_char), intent(in)        :: condition
            character(kind=c_char), intent(in)        :: text
            type(c_ptr)                               :: xmpp_stanza_reply_error_
        end function xmpp_stanza_reply_error_

        ! int xmpp_stanza_set_attribute(xmpp_stanza_t *stanza, const char *key, const char *value)
        function xmpp_stanza_set_attribute_(stanza, key, value) bind(c, name='xmpp_stanza_set_attribute')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: key
            character(kind=c_char), intent(in)        :: value
            integer(kind=c_int)                       :: xmpp_stanza_set_attribute_
        end function xmpp_stanza_set_attribute_

        ! int xmpp_stanza_set_from(xmpp_stanza_t *stanza, const char *from)
        function xmpp_stanza_set_from_(stanza, from) bind(c, name='xmpp_stanza_set_from')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: from
            integer(kind=c_int)                       :: xmpp_stanza_set_from_
        end function xmpp_stanza_set_from_

        ! int xmpp_stanza_set_id(xmpp_stanza_t *stanza, const char *id)
        function xmpp_stanza_set_id_(stanza, id) bind(c, name='xmpp_stanza_set_id')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: id
            integer(kind=c_int)                       :: xmpp_stanza_set_id_
        end function xmpp_stanza_set_id_

        ! int xmpp_stanza_set_name(xmpp_stanza_t *stanza, const char *name)
        function xmpp_stanza_set_name_(stanza, name) bind(c, name='xmpp_stanza_set_name')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: name
            integer(kind=c_int)                       :: xmpp_stanza_set_name_
        end function xmpp_stanza_set_name_

        ! int xmpp_stanza_set_ns(xmpp_stanza_t *stanza, const char *ns)
        function xmpp_stanza_set_ns_(stanza, ns) bind(c, name='xmpp_stanza_set_ns')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: ns
            integer(kind=c_int)                       :: xmpp_stanza_set_ns_
        end function xmpp_stanza_set_ns_

        ! int xmpp_stanza_set_text(xmpp_stanza_t *stanza, const char *text)
        function xmpp_stanza_set_text_(stanza, text) bind(c, name='xmpp_stanza_set_text')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: text
            integer(kind=c_int)                       :: xmpp_stanza_set_text_
        end function xmpp_stanza_set_text_

        ! int xmpp_stanza_set_text_with_size(xmpp_stanza_t *stanza, const char *text, size_t size)
        function xmpp_stanza_set_text_with_size(stanza, text, size) bind(c, name='xmpp_stanza_set_text_with_size')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: text
            integer(kind=c_size_t), intent(in), value :: size
            integer(kind=c_int)                       :: xmpp_stanza_set_text_with_size
        end function xmpp_stanza_set_text_with_size

        ! int xmpp_stanza_set_to(xmpp_stanza_t *stanza, const char *to)
        function xmpp_stanza_set_to_(stanza, to) bind(c, name='xmpp_stanza_set_to')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: to
            integer(kind=c_int)                       :: xmpp_stanza_set_to_
        end function xmpp_stanza_set_to_

        ! int xmpp_stanza_set_type(xmpp_stanza_t *stanza, const char *type)
        function xmpp_stanza_set_type_(stanza, type) bind(c, name='xmpp_stanza_set_type')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: stanza
            character(kind=c_char), intent(in)        :: type
            integer(kind=c_int)                       :: xmpp_stanza_set_type_
        end function xmpp_stanza_set_type_

        ! void xmpp_stop(xmpp_ctx_t *ctx)
        subroutine xmpp_stop(ctx) bind(c, name='xmpp_stop')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
        end subroutine xmpp_stop

        ! void xmpp_timed_handler_add(xmpp_conn_t *conn, xmpp_timed_handler handler, unsigned long period, void *userdata)
        subroutine xmpp_timed_handler_add(conn, handler, period, user_data) bind(c, name='xmpp_timed_handler_add')
            import :: c_ptr, c_unsigned_long, xmpp_timed_handler
            implicit none
            type(c_ptr),                   intent(in), value :: conn
            procedure(xmpp_timed_handler)                    :: handler
            integer(kind=c_unsigned_long), intent(in), value :: period
            type(c_ptr),                   intent(in), value :: user_data
        end subroutine xmpp_timed_handler_add

        ! void xmpp_timed_handler_delete(xmpp_conn_t *conn, xmpp_timed_handler handler)
        subroutine xmpp_timed_handler_delete(conn, handler) bind(c, name='xmpp_timed_handler_delete')
            import :: c_ptr, xmpp_timed_handler
            implicit none
            type(c_ptr), intent(in), value :: conn
            procedure(xmpp_timed_handler)  :: handler
        end subroutine xmpp_timed_handler_delete

        ! void xmpp_tlscert_free(xmpp_tlscert_t *cert)
        subroutine xmpp_tlscert_free_(cert) bind(c, name='xmpp_tlscert_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cert
        end subroutine xmpp_tlscert_free_

        ! xmpp_conn_t *xmpp_tlscert_get_conn(const xmpp_tlscert_t *cert)
        function xmpp_tlscert_get_conn(cert) bind(c, name='xmpp_tlscert_get_conn')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cert
            type(c_ptr)                    :: xmpp_tlscert_get_conn
        end function xmpp_tlscert_get_conn

        ! xmpp_ctx_t *xmpp_tlscert_get_ctx(const xmpp_tlscert_t *cert)
        function xmpp_tlscert_get_ctx(cert) bind(c, name='xmpp_tlscert_get_ctx')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cert
            type(c_ptr)                    :: xmpp_tlscert_get_ctx
        end function xmpp_tlscert_get_ctx

        ! const char *xmpp_tlscert_get_description(xmpp_cert_element_t elmnt)
        function xmpp_tlscert_get_description_(element) bind(c, name='xmpp_tlscert_get_description')
            import :: c_int, c_ptr
            implicit none
            integer(kind=c_int), intent(in), value :: element
            type(c_ptr)                            :: xmpp_tlscert_get_description_
        end function xmpp_tlscert_get_description_

        ! const char *xmpp_tlscert_get_dnsname(const xmpp_tlscert_t *cert, size_t n)
        function xmpp_tlscert_get_dnsname_(cert, n) bind(c, name='xmpp_tlscert_get_dnsname')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: cert
            integer(kind=c_size_t), intent(in), value :: n
            type(c_ptr)                               :: xmpp_tlscert_get_dnsname_
        end function xmpp_tlscert_get_dnsname_

        ! const char *xmpp_tlscert_get_pem(const xmpp_tlscert_t *cert)
        function xmpp_tlscert_get_pem_(cert) bind(c, name='xmpp_tlscert_get_pem')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: cert
            type(c_ptr)                    :: xmpp_tlscert_get_pem_
        end function xmpp_tlscert_get_pem_

        ! const char *xmpp_tlscert_get_string(const xmpp_tlscert_t *cert, xmpp_cert_element_t elmnt)
        function xmpp_tlscert_get_string_(cert, element) bind(c, name='xmpp_tlscert_get_string')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: cert
            integer(kind=c_int), intent(in), value :: element
            type(c_ptr)                            :: xmpp_tlscert_get_string_
        end function xmpp_tlscert_get_string_

        ! char *xmpp_uuid_gen(xmpp_ctx_t *ctx)
        function xmpp_uuid_gen_(ctx) bind(c, name='xmpp_uuid_gen')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            type(c_ptr)                    :: xmpp_uuid_gen_
        end function xmpp_uuid_gen_

        ! int xmpp_version_check(int major, int minor)
        function xmpp_version_check(major, minor) bind(c, name='xmpp_version_check')
            import :: c_int
            implicit none
            integer(kind=c_int), intent(in), value :: major
            integer(kind=c_int), intent(in), value :: minor
            integer(kind=c_int)                    :: xmpp_version_check
        end function xmpp_version_check
    end interface
contains
    subroutine xmpp_base64_decode_bin(ctx, base64, len, out, out_len)
        type(c_ptr),                   intent(in)  :: ctx
        character(len=*),              intent(in)  :: base64
        integer(kind=c_size_t),        intent(in)  :: len
        character(len=:), allocatable, intent(out) :: out
        integer(kind=c_size_t),        intent(out) :: out_len

        type(c_ptr) :: ptr

        call xmpp_base64_decode_bin_(ctx, base64, len, ptr, out_len)
        call c_f_str_ptr(ptr, out, out_len)
        call xmpp_free(ctx, ptr)
    end subroutine xmpp_base64_decode_bin

    function xmpp_base64_decode_str(ctx, base64, len)
        type(c_ptr),            intent(in) :: ctx
        character(len=*),       intent(in) :: base64
        integer(kind=c_size_t), intent(in) :: len
        character(len=:), allocatable      :: xmpp_base64_decode_str

        type(c_ptr) :: ptr

        ptr = xmpp_base64_decode_str_(ctx, base64, len)
        call c_f_str_ptr(ptr, xmpp_base64_decode_str)
        call xmpp_free(ctx, ptr)
    end function xmpp_base64_decode_str

    function xmpp_base64_encode(ctx, data, len)
        type(c_ptr),            intent(in) :: ctx
        character(len=*),       intent(in) :: data
        integer(kind=c_size_t), intent(in) :: len
        character(len=:), allocatable      :: xmpp_base64_encode

        type(c_ptr) :: ptr

        ptr = xmpp_base64_encode_(ctx, data, len)
        call c_f_str_ptr(ptr, xmpp_base64_encode)
        call xmpp_free(ctx, ptr)
    end function xmpp_base64_encode

    function xmpp_conn_cert_xmppaddr(conn, n)
        type(c_ptr),              intent(in) :: conn
        integer(kind=c_unsigned), intent(in) :: n
        character(len=:), allocatable        :: xmpp_conn_cert_xmppaddr

        type(c_ptr) :: ptr

        ptr = xmpp_conn_cert_xmppaddr_(conn, n)
        call c_f_str_ptr(ptr, xmpp_conn_cert_xmppaddr)
    end function xmpp_conn_cert_xmppaddr

    function xmpp_conn_get_bound_jid(conn)
        type(c_ptr), intent(in)       :: conn
        character(len=:), allocatable :: xmpp_conn_get_bound_jid

        type(c_ptr) :: ptr

        ptr = xmpp_conn_get_bound_jid_(conn)
        call c_f_str_ptr(ptr, xmpp_conn_get_bound_jid)
    end function xmpp_conn_get_bound_jid

    function xmpp_conn_get_jid(conn)
        type(c_ptr), intent(in)       :: conn
        character(len=:), allocatable :: xmpp_conn_get_jid

        type(c_ptr) :: ptr

        ptr = xmpp_conn_get_jid_(conn)
        call c_f_str_ptr(ptr, xmpp_conn_get_jid)
    end function xmpp_conn_get_jid

    function xmpp_conn_get_keyfile(conn)
        type(c_ptr), intent(in)       :: conn
        character(len=:), allocatable :: xmpp_conn_get_keyfile

        type(c_ptr) :: ptr

        ptr = xmpp_conn_get_keyfile_(conn)
        call c_f_str_ptr(ptr, xmpp_conn_get_keyfile)
    end function xmpp_conn_get_keyfile

    function xmpp_conn_get_pass(conn)
        type(c_ptr), intent(in)       :: conn
        character(len=:), allocatable :: xmpp_conn_get_pass

        type(c_ptr) :: ptr

        ptr = xmpp_conn_get_pass_(conn)
        call c_f_str_ptr(ptr, xmpp_conn_get_pass)
    end function xmpp_conn_get_pass

    function xmpp_conn_open_stream(conn, attributes, attributes_len)
        !! Attributes must be an array of null-terminated character strings:
        !! Even index points to an attribute name and odd index points to its
        !! value.
        type(c_ptr),              intent(in)    :: conn
        character(len=*), target, intent(inout) :: attributes(:)
        integer(kind=c_size_t),   intent(in)    :: attributes_len
        integer(kind=c_int)                     :: xmpp_conn_open_stream

        integer(kind=c_size_t) :: i
        type(c_ptr)            :: ptrs(attributes_len)

        ptrs = c_null_ptr

        do i = 1, attributes_len
            ptrs(i) = c_loc(attributes(i))
        end do

        xmpp_conn_open_stream = xmpp_conn_open_stream_(conn, ptrs, attributes_len)
    end function xmpp_conn_open_stream

    function xmpp_conn_send_queue_drop_element(conn, which)
        type(c_ptr), intent(in)       :: conn
        integer,     intent(in)       :: which
        character(len=:), allocatable :: xmpp_conn_send_queue_drop_element

        type(c_ptr) :: ptr

        ptr = xmpp_conn_send_queue_drop_element_(conn, which)
        call c_f_str_ptr(ptr, xmpp_conn_send_queue_drop_element)
        call xmpp_free(xmpp_conn_get_context(conn), ptr)
    end function xmpp_conn_send_queue_drop_element

    subroutine xmpp_conn_set_cafile(conn, path)
        type(c_ptr),      intent(in) :: conn
        character(len=*), intent(in) :: path

        call xmpp_conn_set_cafile_(conn, path // c_null_char)
    end subroutine xmpp_conn_set_cafile

    subroutine xmpp_conn_set_capath(conn, path)
        type(c_ptr),      intent(in) :: conn
        character(len=*), intent(in) :: path

        call xmpp_conn_set_capath_(conn, path // c_null_char)
    end subroutine xmpp_conn_set_capath

    subroutine xmpp_conn_set_client_cert(conn, cert, key)
        type(c_ptr),      intent(in) :: conn
        character(len=*), intent(in) :: cert
        character(len=*), intent(in) :: key

        call xmpp_conn_set_client_cert_(conn, cert // c_null_char, key // c_null_char)
    end subroutine xmpp_conn_set_client_cert

    subroutine xmpp_conn_set_jid(conn, jid)
        type(c_ptr),      intent(in) :: conn
        character(len=*), intent(in) :: jid

        call xmpp_conn_set_jid_(conn, jid // c_null_char)
    end subroutine xmpp_conn_set_jid

    subroutine xmpp_conn_set_pass(conn, pass)
        type(c_ptr),      intent(in) :: conn
        character(len=*), intent(in) :: pass

        call xmpp_conn_set_pass_(conn, pass // c_null_char)
    end subroutine xmpp_conn_set_pass

    function xmpp_connect_client(conn, alt_domain, alt_port, callback, user_data)
        type(c_ptr),      intent(in) :: conn
        character(len=*), intent(in) :: alt_domain
        integer,          intent(in) :: alt_port
        procedure(xmpp_conn_handler) :: callback
        type(c_ptr),      intent(in) :: user_data
        integer                      :: xmpp_connect_client

        xmpp_connect_client = xmpp_connect_client_(conn, &
                                                   alt_domain // c_null_char, &
                                                   int(alt_port, c_unsigned_short), &
                                                   callback, &
                                                   user_data)
    end function xmpp_connect_client

    function xmpp_connect_component(conn, server, port, callback, user_data)
        type(c_ptr),      intent(in) :: conn
        character(len=*), intent(in) :: server
        integer,          intent(in) :: port
        procedure(xmpp_conn_handler) :: callback
        type(c_ptr),      intent(in) :: user_data
        integer                      :: xmpp_connect_component

        xmpp_connect_component = xmpp_connect_component_(conn, &
                                                         server // c_null_char, &
                                                         int(port, c_unsigned_short), &
                                                         callback, &
                                                         user_data)
    end function xmpp_connect_component

    function xmpp_connect_raw(conn, alt_domain, alt_port, callback, user_data)
        type(c_ptr),      intent(in) :: conn
        character(len=*), intent(in) :: alt_domain
        integer,          intent(in) :: alt_port
        procedure(xmpp_conn_handler) :: callback
        type(c_ptr),      intent(in) :: user_data
        integer                      :: xmpp_connect_raw

        xmpp_connect_raw = xmpp_connect_raw_(conn, &
                                             alt_domain // c_null_char, &
                                             int(alt_port, c_unsigned_short), &
                                             callback, &
                                             user_data)
    end function xmpp_connect_raw

    subroutine xmpp_ctx_free(ctx)
        type(c_ptr), intent(inout) :: ctx

        call xmpp_ctx_free_(ctx)
        ctx = c_null_ptr
    end subroutine xmpp_ctx_free

    subroutine xmpp_free_sm_state(sm_state)
        type(c_ptr), intent(inout) :: sm_state

        call xmpp_free_sm_state_(sm_state)
        sm_state = c_null_ptr
    end subroutine xmpp_free_sm_state

    function xmpp_error_new(ctx, type, text)
        type(c_ptr),      intent(in) :: ctx
        integer,          intent(in) :: type
        character(len=*), intent(in) :: text
        type(c_ptr)                  :: xmpp_error_new

        xmpp_error_new = xmpp_error_new_(ctx, type, text // c_null_char)
    end function xmpp_error_new

    subroutine xmpp_handler_add(conn, handler, ns, name, type, user_data)
        !! Wrapper routine that passed null-terminated strings or `NULL` to
        !! interface.
        type(c_ptr),      intent(in) :: conn
        procedure(xmpp_handler)      :: handler
        character(len=*), intent(in) :: ns
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: type
        type(c_ptr),      intent(in) :: user_data

        character(len=len(ns)   + 1), target :: ns_str
        character(len=len(name) + 1), target :: name_str
        character(len=len(type) + 1), target :: type_str

        type(c_ptr) :: ns_ptr, name_ptr, type_ptr

        ns_ptr   = c_null_ptr
        name_ptr = c_null_ptr
        type_ptr = c_null_ptr

        if (len_trim(ns) > 0) then
            ns_str = ns // c_null_char
            ns_ptr = c_loc(ns_str)
        end if

        if (len_trim(name) > 0) then
            name_str = name // c_null_char
            name_ptr = c_loc(name_str)
        end if

        if (len_trim(type) > 0) then
            type_str = type // c_null_char
            type_ptr = c_loc(type_str)
        end if

        call xmpp_handler_add_(conn, handler, ns_ptr, name_ptr, type_ptr, user_data)
    end subroutine xmpp_handler_add

    subroutine xmpp_id_handler_add(conn, handler, id, user_data)
        type(c_ptr),      intent(in) :: conn
        procedure(xmpp_handler)      :: handler
        character(len=*), intent(in) :: id
        type(c_ptr),      intent(in) :: user_data

        call xmpp_id_handler_add_(conn, handler, id // c_null_char, user_data)
    end subroutine xmpp_id_handler_add

    subroutine xmpp_id_handler_delete(conn, handler, id)
        type(c_ptr),      intent(in) :: conn
        procedure(xmpp_handler)      :: handler
        character(len=*), intent(in) :: id

        call xmpp_id_handler_delete_(conn, handler, id // c_null_char)
    end subroutine xmpp_id_handler_delete

    function xmpp_iq_new(ctx, type, id)
        type(c_ptr),      intent(in) :: ctx
        character(len=*), intent(in) :: type
        character(len=*), intent(in) :: id
        type(c_ptr)                  :: xmpp_iq_new

        xmpp_iq_new = xmpp_iq_new_(ctx, type // c_null_char, id // c_null_char)
    end function xmpp_iq_new

    function xmpp_jid_bare(ctx, jid)
        type(c_ptr),      intent(in)  :: ctx
        character(len=*), intent(in)  :: jid
        character(len=:), allocatable :: xmpp_jid_bare

        type(c_ptr) :: ptr

        ptr = xmpp_jid_bare_(ctx, jid // c_null_char)
        call c_f_str_ptr(ptr, xmpp_jid_bare)
        call xmpp_free(ctx, ptr)
    end function xmpp_jid_bare

    function xmpp_jid_domain(ctx, jid)
        type(c_ptr),      intent(in)  :: ctx
        character(len=*), intent(in)  :: jid
        character(len=:), allocatable :: xmpp_jid_domain

        type(c_ptr) :: ptr

        ptr = xmpp_jid_domain_(ctx, jid // c_null_char)
        call c_f_str_ptr(ptr, xmpp_jid_domain)
        call xmpp_free(ctx, ptr)
    end function xmpp_jid_domain

    function xmpp_jid_new(ctx, node, domain, resource)
        type(c_ptr),      intent(in)  :: ctx
        character(len=*), intent(in)  :: node
        character(len=*), intent(in)  :: domain
        character(len=*), intent(in)  :: resource
        character(len=:), allocatable :: xmpp_jid_new

        type(c_ptr) :: ptr

        ptr = xmpp_jid_new_(ctx, node // c_null_char, domain // c_null_char, resource // c_null_char)
        call c_f_str_ptr(ptr, xmpp_jid_new)
        call xmpp_free(ctx, ptr)
    end function xmpp_jid_new

    function xmpp_jid_node(ctx, jid)
        type(c_ptr),      intent(in)  :: ctx
        character(len=*), intent(in)  :: jid
        character(len=:), allocatable :: xmpp_jid_node

        type(c_ptr) :: ptr

        ptr = xmpp_jid_node_(ctx, jid // c_null_char)
        call c_f_str_ptr(ptr, xmpp_jid_node)
        call xmpp_free(ctx, ptr)
    end function xmpp_jid_node

    function xmpp_jid_resource(ctx, jid)
        type(c_ptr),      intent(in)  :: ctx
        character(len=*), intent(in)  :: jid
        character(len=:), allocatable :: xmpp_jid_resource

        type(c_ptr) :: ptr

        ptr = xmpp_jid_resource_(ctx, jid // c_null_char)
        call c_f_str_ptr(ptr, xmpp_jid_resource)
        call xmpp_free(ctx, ptr)
    end function xmpp_jid_resource

    function xmpp_message_get_body(ctx, msg)
        !! In contrast to the libstrophe API, this wrapper functions requires
        !! the context to be passed to free the C string.
        type(c_ptr), intent(in)       :: ctx
        type(c_ptr), intent(in)       :: msg
        character(len=:), allocatable :: xmpp_message_get_body

        type(c_ptr) :: ptr

        ptr = xmpp_message_get_body_(msg)
        call c_f_str_ptr(ptr, xmpp_message_get_body)
        call xmpp_free(ctx, ptr)
    end function xmpp_message_get_body

    function xmpp_message_new(ctx, type, to, id)
        type(c_ptr),      intent(in) :: ctx
        character(len=*), intent(in) :: type
        character(len=*), intent(in) :: to
        character(len=*), intent(in) :: id
        type(c_ptr)                  :: xmpp_message_new

        xmpp_message_new = xmpp_message_new_(ctx, type // c_null_char, to // c_null_char, id // c_null_char)
    end function xmpp_message_new

    function xmpp_message_set_body(msg, text)
        type(c_ptr),      intent(in) :: msg
        character(len=*), intent(in) :: text
        integer                      :: xmpp_message_set_body

        xmpp_message_set_body = xmpp_message_set_body_(msg, text // c_null_char)
    end function xmpp_message_set_body

    subroutine xmpp_rand_free(ctx, rand)
        type(c_ptr), intent(inout) :: ctx
        type(c_ptr), intent(inout) :: rand

        call xmpp_rand_free_(ctx, rand)
        rand = c_null_ptr
    end subroutine xmpp_rand_free

    subroutine xmpp_send_error(conn, type, text)
        type(c_ptr),      intent(in) :: conn
        integer,          intent(in) :: type
        character(len=*), intent(in) :: text

        call xmpp_send_error_(conn, type, text // c_null_char)
    end subroutine xmpp_send_error

    subroutine xmpp_send_raw_string(conn, str)
        type(c_ptr),      intent(in) :: conn
        character(len=*), intent(in) :: str

        call xmpp_send_raw_string_(conn, str // c_null_char)
    end subroutine xmpp_send_raw_string

    function xmpp_sha1(ctx, data, data_len)
        type(c_ptr),            intent(in)           :: ctx
        character(len=*),       intent(in)           :: data
        integer(kind=c_size_t), intent(in), optional :: data_len
        character(len=:), allocatable                :: xmpp_sha1

        integer(kind=c_size_t) :: n
        type(c_ptr)            :: ptr

        if (present(data_len)) then
            n = data_len
        else
            n = len(data, kind=c_size_t)
        end if

        ptr = xmpp_sha1_(ctx, data, n)
        call c_f_str_ptr(ptr, xmpp_sha1)
        call xmpp_free(ctx, ptr)
    end function xmpp_sha1

    subroutine xmpp_sha1_free(sha1)
        type(c_ptr), intent(inout) :: sha1

        call xmpp_sha1_free_(sha1)
        sha1 = c_null_ptr
    end subroutine xmpp_sha1_free

    function xmpp_sha1_to_string(sha1, str, str_len)
        type(c_ptr),            intent(in)           :: sha1
        character(len=*),       intent(inout)        :: str
        integer(kind=c_size_t), intent(in), optional :: str_len
        character(len=:), allocatable                :: xmpp_sha1_to_string

        integer(kind=c_size_t) :: n
        type(c_ptr)            :: ptr

        if (present(str_len)) then
            n = str_len
        else
            n = len(str, kind=c_size_t)
        end if

        ptr = xmpp_sha1_to_string_(sha1, str, n)
        call c_f_str_ptr(ptr, xmpp_sha1_to_string)
    end function xmpp_sha1_to_string

    subroutine xmpp_sha1_update(sha1, data, data_len)
        type(c_ptr),            intent(in)           :: sha1
        character(len=*),       intent(in)           :: data
        integer(kind=c_size_t), intent(in), optional :: data_len

        integer(kind=c_size_t) :: n

        if (present(data_len)) then
            n = data_len
        else
            n = len(data, kind=c_size_t)
        end if

        call xmpp_sha1_update_(sha1, data, n)
    end subroutine xmpp_sha1_update

    function xmpp_stanza_del_attribute(stanza, name)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: name
        integer                      :: xmpp_stanza_del_attribute

        xmpp_stanza_del_attribute = xmpp_stanza_del_attribute_(stanza, name // c_null_char)
    end function xmpp_stanza_del_attribute

    function xmpp_stanza_get_attribute(stanza, name)
        type(c_ptr),      intent(in)  :: stanza
        character(len=*), intent(in)  :: name
        character(len=:), allocatable :: xmpp_stanza_get_attribute

        type(c_ptr) :: ptr

        ptr = xmpp_stanza_get_attribute_(stanza, name // c_null_char)
        call c_f_str_ptr(ptr, xmpp_stanza_get_attribute)
    end function xmpp_stanza_get_attribute

    function xmpp_stanza_get_attributes(stanza, attr, attr_len)
        !! This function populates the array with attributes from the stanza.
        !! The `attr` array will be in the format: `attr(i)` = attribute name,
        !! `attr(i+1)` = attribute value.
        type(c_ptr),      intent(in)    :: stanza
        character(len=*), intent(inout) :: attr(:)
        integer,          intent(in)    :: attr_len
        integer                         :: xmpp_stanza_get_attributes

        character(len=:), allocatable :: str
        integer                       :: i
        type(c_ptr)                   :: ptrs(attr_len)

        xmpp_stanza_get_attributes = xmpp_stanza_get_attributes_(stanza, ptrs, attr_len)

        do i = 1, xmpp_stanza_get_attributes
            if (i > size(attr)) exit
            call c_f_str_ptr(ptrs(i), str)
            attr(i) = str
        end do
    end function xmpp_stanza_get_attributes

    function xmpp_stanza_get_child_by_name(stanza, name)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: name
        type(c_ptr)                  :: xmpp_stanza_get_child_by_name

        xmpp_stanza_get_child_by_name = xmpp_stanza_get_child_by_name_(stanza, name // c_null_char)
    end function xmpp_stanza_get_child_by_name

    function xmpp_stanza_get_child_by_name_and_ns(stanza, name, ns)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: ns
        type(c_ptr)                  :: xmpp_stanza_get_child_by_name_and_ns

        xmpp_stanza_get_child_by_name_and_ns = xmpp_stanza_get_child_by_name_and_ns_(stanza, name // c_null_char, ns // c_null_char)
    end function xmpp_stanza_get_child_by_name_and_ns

    function xmpp_stanza_get_child_by_ns(stanza, ns)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: ns
        type(c_ptr)                  :: xmpp_stanza_get_child_by_ns

        xmpp_stanza_get_child_by_ns = xmpp_stanza_get_child_by_ns_(stanza, ns // c_null_char)
    end function xmpp_stanza_get_child_by_ns

    function xmpp_stanza_get_from(stanza)
        type(c_ptr), intent(in)       :: stanza
        character(len=:), allocatable :: xmpp_stanza_get_from

        type(c_ptr) :: ptr

        ptr = xmpp_stanza_get_from_(stanza)
        call c_f_str_ptr(ptr, xmpp_stanza_get_from)
    end function xmpp_stanza_get_from

    function xmpp_stanza_get_id(stanza)
        type(c_ptr), intent(in)       :: stanza
        character(len=:), allocatable :: xmpp_stanza_get_id

        type(c_ptr) :: ptr

        ptr = xmpp_stanza_get_id_(stanza)
        call c_f_str_ptr(ptr, xmpp_stanza_get_id)
    end function xmpp_stanza_get_id

    function xmpp_stanza_get_name(stanza)
        type(c_ptr), intent(in)       :: stanza
        character(len=:), allocatable :: xmpp_stanza_get_name

        type(c_ptr) :: ptr

        ptr = xmpp_stanza_get_name_(stanza)
        call c_f_str_ptr(ptr, xmpp_stanza_get_name)
    end function xmpp_stanza_get_name

    function xmpp_stanza_get_ns(stanza)
        type(c_ptr), intent(in)       :: stanza
        character(len=:), allocatable :: xmpp_stanza_get_ns

        type(c_ptr) :: ptr

        ptr = xmpp_stanza_get_ns_(stanza)
        call c_f_str_ptr(ptr, xmpp_stanza_get_ns)
    end function xmpp_stanza_get_ns

    function xmpp_stanza_get_text(stanza)
        type(c_ptr), intent(in)       :: stanza
        character(len=:), allocatable :: xmpp_stanza_get_text

        type(c_ptr) :: ptr

        ptr = xmpp_stanza_get_text_(stanza)
        call c_f_str_ptr(ptr, xmpp_stanza_get_text)
        call xmpp_free(xmpp_stanza_get_context(stanza), ptr)
    end function xmpp_stanza_get_text

    function xmpp_stanza_get_text_ptr(stanza)
        type(c_ptr), intent(in)       :: stanza
        character(len=:), allocatable :: xmpp_stanza_get_text_ptr

        type(c_ptr) :: ptr

        ptr = xmpp_stanza_get_text_ptr_(stanza)
        call c_f_str_ptr(ptr, xmpp_stanza_get_text_ptr)
    end function xmpp_stanza_get_text_ptr

    function xmpp_stanza_get_to(stanza)
        type(c_ptr), intent(in)       :: stanza
        character(len=:), allocatable :: xmpp_stanza_get_to

        type(c_ptr) :: ptr

        ptr = xmpp_stanza_get_to_(stanza)
        call c_f_str_ptr(ptr, xmpp_stanza_get_to)
    end function xmpp_stanza_get_to

    function xmpp_stanza_get_type(stanza)
        type(c_ptr), intent(in)       :: stanza
        character(len=:), allocatable :: xmpp_stanza_get_type

        type(c_ptr) :: ptr

        ptr = xmpp_stanza_get_type_(stanza)
        call c_f_str_ptr(ptr, xmpp_stanza_get_type)
    end function xmpp_stanza_get_type

    function xmpp_stanza_new_from_string(ctx, str)
        type(c_ptr),      intent(in) :: ctx
        character(len=*), intent(in) :: str
        type(c_ptr)                  :: xmpp_stanza_new_from_string

        xmpp_stanza_new_from_string = xmpp_stanza_new_from_string_(ctx, str // c_null_char)
    end function xmpp_stanza_new_from_string

    function xmpp_stanza_reply_error(stanza, error_type, condition, text)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: error_type
        character(len=*), intent(in) :: condition
        character(len=*), intent(in) :: text
        type(c_ptr)                  :: xmpp_stanza_reply_error

       xmpp_stanza_reply_error = xmpp_stanza_reply_error_(stanza, &
                                                          error_type // c_null_char, &
                                                          condition // c_null_char, &
                                                          text // c_null_char)
    end function xmpp_stanza_reply_error

    function xmpp_stanza_set_attribute(stanza, key, value)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        integer                      :: xmpp_stanza_set_attribute

        xmpp_stanza_set_attribute = xmpp_stanza_set_attribute_(stanza, key // c_null_char, value // c_null_char)
    end function xmpp_stanza_set_attribute

    function xmpp_stanza_set_from(stanza, from)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: from
        integer                      :: xmpp_stanza_set_from

        xmpp_stanza_set_from = xmpp_stanza_set_from_(stanza, from // c_null_char)
    end function xmpp_stanza_set_from

    function xmpp_stanza_set_id(stanza, id)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: id
        integer                      :: xmpp_stanza_set_id

        xmpp_stanza_set_id = xmpp_stanza_set_id_(stanza, id // c_null_char)
    end function xmpp_stanza_set_id

    function xmpp_stanza_set_name(stanza, name)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: name
        integer                      :: xmpp_stanza_set_name

        xmpp_stanza_set_name = xmpp_stanza_set_name_(stanza, name // c_null_char)
    end function xmpp_stanza_set_name

    function xmpp_stanza_set_ns(stanza, ns)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: ns
        integer                      :: xmpp_stanza_set_ns

        xmpp_stanza_set_ns = xmpp_stanza_set_ns_(stanza, ns // c_null_char)
    end function xmpp_stanza_set_ns

    function xmpp_stanza_set_text(stanza, text)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: text
        integer                      :: xmpp_stanza_set_text

        xmpp_stanza_set_text = xmpp_stanza_set_text_(stanza, text // c_null_char)
    end function xmpp_stanza_set_text

    function xmpp_stanza_set_to(stanza, to)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: to
        integer                      :: xmpp_stanza_set_to

        xmpp_stanza_set_to = xmpp_stanza_set_to_(stanza, to // c_null_char)
    end function xmpp_stanza_set_to

    function xmpp_stanza_set_type(stanza, type)
        type(c_ptr),      intent(in) :: stanza
        character(len=*), intent(in) :: type
        integer                      :: xmpp_stanza_set_type

        xmpp_stanza_set_type = xmpp_stanza_set_type_(stanza, type // c_null_char)
    end function xmpp_stanza_set_type

    subroutine xmpp_tlscert_free(cert)
        type(c_ptr), intent(inout) :: cert

        call xmpp_tlscert_free_(cert)
        cert = c_null_ptr
    end subroutine xmpp_tlscert_free

    function xmpp_tlscert_get_description(element)
        integer, intent(in)           :: element
        character(len=:), allocatable :: xmpp_tlscert_get_description

        type(c_ptr) :: ptr

        ptr = xmpp_tlscert_get_description_(element)
        call c_f_str_ptr(ptr, xmpp_tlscert_get_description)
    end function xmpp_tlscert_get_description

    function xmpp_tlscert_get_dnsname(cert, n)
        type(c_ptr),            intent(in) :: cert
        integer(kind=c_size_t), intent(in) :: n
        character(len=:), allocatable      :: xmpp_tlscert_get_dnsname

        type(c_ptr) :: ptr

        ptr = xmpp_tlscert_get_dnsname_(cert, n)
        call c_f_str_ptr(ptr, xmpp_tlscert_get_dnsname)
    end function xmpp_tlscert_get_dnsname

    function xmpp_tlscert_get_pem(cert)
        type(c_ptr), intent(in)       :: cert
        character(len=:), allocatable :: xmpp_tlscert_get_pem

        type(c_ptr) :: ptr

        ptr = xmpp_tlscert_get_pem_(cert)
        call c_f_str_ptr(ptr, xmpp_tlscert_get_pem)
    end function xmpp_tlscert_get_pem

    function xmpp_tlscert_get_string(cert, element)
        type(c_ptr), intent(in)       :: cert
        integer,     intent(in)       :: element
        character(len=:), allocatable :: xmpp_tlscert_get_string

        type(c_ptr) :: ptr

        ptr = xmpp_tlscert_get_string_(cert, element)
        call c_f_str_ptr(ptr, xmpp_tlscert_get_string)
    end function xmpp_tlscert_get_string

    function xmpp_uuid_gen(ctx)
        type(c_ptr), intent(in)       :: ctx
        character(len=:), allocatable :: xmpp_uuid_gen

        type(c_ptr) :: ptr

        ptr = xmpp_uuid_gen_(ctx)
        call c_f_str_ptr(ptr, xmpp_uuid_gen)
        call xmpp_free(ctx, ptr)
    end function xmpp_uuid_gen
end module xmpp
