! czmq_zsock.f90
!
! Author:  Philipp Engel
! Licence: ISC
module czmq_zsock
    !! Auto-generated Fortran 2018 interface bindings to libczmq 4.
    use :: zmq_util
    implicit none (type, external)
    private

    public :: zsock_affinity
    public :: zsock_attach
    public :: zsock_attach_
    public :: zsock_backlog
    public :: zsock_bind
    public :: zsock_bind_
    public :: zsock_bindtodevice
    public :: zsock_bindtodevice_
    public :: zsock_brecv
    public :: zsock_brecv_
    public :: zsock_bsend
    public :: zsock_bsend_
    public :: zsock_connect
    public :: zsock_connect_
    public :: zsock_connect_timeout
    public :: zsock_curve_publickey
    public :: zsock_curve_publickey_
    public :: zsock_curve_secretkey
    public :: zsock_curve_secretkey_
    public :: zsock_curve_server
    public :: zsock_curve_serverkey
    public :: zsock_curve_serverkey_
    public :: zsock_destroy
    public :: zsock_disconnect
    public :: zsock_endpoint
    public :: zsock_endpoint_
    public :: zsock_events
    public :: zsock_fd
    public :: zsock_flush
    public :: zsock_gssapi_plaintext
    public :: zsock_gssapi_principal
    public :: zsock_gssapi_principal_
    public :: zsock_gssapi_principal_nametype
    public :: zsock_gssapi_server
    public :: zsock_gssapi_service_principal
    public :: zsock_gssapi_service_principal_
    public :: zsock_gssapi_service_principal_nametype
    public :: zsock_handshake_ivl
    public :: zsock_heartbeat_ivl
    public :: zsock_heartbeat_timeout
    public :: zsock_heartbeat_ttl
    public :: zsock_hwm
    public :: zsock_identity
    public :: zsock_identity_
    public :: zsock_immediate
    public :: zsock_in_batch_size
    public :: zsock_invert_matching
    public :: zsock_ipv4only
    public :: zsock_ipv6
    public :: zsock_is
    public :: zsock_last_endpoint
    public :: zsock_last_endpoint_
    public :: zsock_linger
    public :: zsock_loopback_fastpath
    public :: zsock_maxmsgsize
    public :: zsock_mcast_loop
    public :: zsock_mechanism
    public :: zsock_metadata
    public :: zsock_metadata_
    public :: zsock_multicast_hops
    public :: zsock_multicast_loop
    public :: zsock_multicast_maxtpdu
    public :: zsock_new
    public :: zsock_new_dealer
    public :: zsock_new_dealer_
    public :: zsock_new_pair
    public :: zsock_new_pair_
    public :: zsock_new_pub
    public :: zsock_new_pub_
    public :: zsock_new_pull
    public :: zsock_new_pull_
    public :: zsock_new_push
    public :: zsock_new_push_
    public :: zsock_new_rep
    public :: zsock_new_rep_
    public :: zsock_new_req
    public :: zsock_new_req_
    public :: zsock_new_router
    public :: zsock_new_router_
    public :: zsock_new_stream
    public :: zsock_new_stream_
    public :: zsock_new_sub
    public :: zsock_new_sub_
    public :: zsock_new_xpub
    public :: zsock_new_xpub_
    public :: zsock_new_xsub
    public :: zsock_new_xsub_
    public :: zsock_out_batch_size
    public :: zsock_plain_password
    public :: zsock_plain_password_
    public :: zsock_plain_username
    public :: zsock_plain_username_
    public :: zsock_priority
    public :: zsock_rate
    public :: zsock_rcvbuf
    public :: zsock_rcvhwm
    public :: zsock_rcvmore
    public :: zsock_rcvtimeo
    public :: zsock_reconnect_ivl
    public :: zsock_reconnect_ivl_max
    public :: zsock_reconnect_stop
    public :: zsock_recovery_ivl
    public :: zsock_recovery_ivl_msec
    public :: zsock_recv
    public :: zsock_resolve
    public :: zsock_router_notify
    public :: zsock_send
    public :: zsock_set_affinity
    public :: zsock_set_backlog
    public :: zsock_set_bindtodevice
    public :: zsock_set_bindtodevice_
    public :: zsock_set_conflate
    public :: zsock_set_connect_rid
    public :: zsock_set_connect_rid_
    public :: zsock_set_connect_rid_bin
    public :: zsock_set_connect_timeout
    public :: zsock_set_curve_publickey
    public :: zsock_set_curve_publickey_
    public :: zsock_set_curve_publickey_bin
    public :: zsock_set_curve_secretkey
    public :: zsock_set_curve_secretkey_
    public :: zsock_set_curve_secretkey_bin
    public :: zsock_set_curve_server
    public :: zsock_set_curve_serverkey
    public :: zsock_set_curve_serverkey_
    public :: zsock_set_curve_serverkey_bin
    public :: zsock_set_delay_attach_on_connect
    public :: zsock_set_disconnect_msg
    public :: zsock_set_gssapi_plaintext
    public :: zsock_set_gssapi_principal
    public :: zsock_set_gssapi_principal_
    public :: zsock_set_gssapi_principal_nametype
    public :: zsock_set_gssapi_server
    public :: zsock_set_gssapi_service_principal
    public :: zsock_set_gssapi_service_principal_
    public :: zsock_set_gssapi_service_principal_nametype
    public :: zsock_set_handshake_ivl
    public :: zsock_set_heartbeat_ivl
    public :: zsock_set_heartbeat_timeout
    public :: zsock_set_heartbeat_ttl
    public :: zsock_set_hello_msg
    public :: zsock_set_hwm
    public :: zsock_set_identity
    public :: zsock_set_identity_
    public :: zsock_set_immediate
    public :: zsock_set_in_batch_size
    public :: zsock_set_invert_matching
    public :: zsock_set_ipv4only
    public :: zsock_set_ipv6
    public :: zsock_set_linger
    public :: zsock_set_loopback_fastpath
    public :: zsock_set_maxmsgsize
    public :: zsock_set_mcast_loop
    public :: zsock_set_metadata
    public :: zsock_set_metadata_
    public :: zsock_set_multicast_hops
    public :: zsock_set_multicast_loop
    public :: zsock_set_multicast_maxtpdu
    public :: zsock_set_only_first_subscribe
    public :: zsock_set_out_batch_size
    public :: zsock_set_plain_password
    public :: zsock_set_plain_password_
    public :: zsock_set_plain_server
    public :: zsock_set_plain_username
    public :: zsock_set_plain_username_
    public :: zsock_set_priority
    public :: zsock_set_probe_router
    public :: zsock_set_rate
    public :: zsock_set_rcvbuf
    public :: zsock_set_rcvhwm
    public :: zsock_set_rcvtimeo
    public :: zsock_set_reconnect_ivl
    public :: zsock_set_reconnect_ivl_max
    public :: zsock_set_reconnect_stop
    public :: zsock_set_recovery_ivl
    public :: zsock_set_recovery_ivl_msec
    public :: zsock_set_req_correlate
    public :: zsock_set_req_relaxed
    public :: zsock_set_router_handover
    public :: zsock_set_router_mandatory
    public :: zsock_set_router_notify
    public :: zsock_set_router_raw
    public :: zsock_set_sndbuf
    public :: zsock_set_sndhwm
    public :: zsock_set_sndtimeo
    public :: zsock_set_socks_password
    public :: zsock_set_socks_password_
    public :: zsock_set_socks_proxy
    public :: zsock_set_socks_proxy_
    public :: zsock_set_socks_username
    public :: zsock_set_socks_username_
    public :: zsock_set_stream_notify
    public :: zsock_set_subscribe
    public :: zsock_set_subscribe_
    public :: zsock_set_swap
    public :: zsock_set_tcp_accept_filter
    public :: zsock_set_tcp_accept_filter_
    public :: zsock_set_tcp_keepalive
    public :: zsock_set_tcp_keepalive_cnt
    public :: zsock_set_tcp_keepalive_idle
    public :: zsock_set_tcp_keepalive_intvl
    public :: zsock_set_tcp_maxrt
    public :: zsock_set_tos
    public :: zsock_set_unbounded
    public :: zsock_set_unsubscribe
    public :: zsock_set_unsubscribe_
    public :: zsock_set_use_fd
    public :: zsock_set_vmci_buffer_max_size
    public :: zsock_set_vmci_buffer_min_size
    public :: zsock_set_vmci_buffer_size
    public :: zsock_set_vmci_connect_timeout
    public :: zsock_set_wss_cert_pem
    public :: zsock_set_wss_cert_pem_
    public :: zsock_set_wss_hostname
    public :: zsock_set_wss_hostname_
    public :: zsock_set_wss_trust_pem
    public :: zsock_set_wss_trust_pem_
    public :: zsock_set_wss_trust_system
    public :: zsock_set_xpub_manual
    public :: zsock_set_xpub_manual_last_value
    public :: zsock_set_xpub_nodrop
    public :: zsock_set_xpub_verbose
    public :: zsock_set_xpub_verboser
    public :: zsock_set_xpub_welcome_msg
    public :: zsock_set_xpub_welcome_msg_
    public :: zsock_set_zap_domain
    public :: zsock_set_zap_domain_
    public :: zsock_set_zap_enforce_domain
    public :: zsock_signal
    public :: zsock_sndbuf
    public :: zsock_sndhwm
    public :: zsock_sndtimeo
    public :: zsock_socks_password
    public :: zsock_socks_password_
    public :: zsock_socks_proxy
    public :: zsock_socks_proxy_
    public :: zsock_socks_username
    public :: zsock_socks_username_
    public :: zsock_swap
    public :: zsock_tcp_accept_filter
    public :: zsock_tcp_accept_filter_
    public :: zsock_tcp_keepalive
    public :: zsock_tcp_keepalive_cnt
    public :: zsock_tcp_keepalive_idle
    public :: zsock_tcp_keepalive_intvl
    public :: zsock_tcp_maxrt
    public :: zsock_test
    public :: zsock_thread_safe
    public :: zsock_tos
    public :: zsock_type
    public :: zsock_type_str
    public :: zsock_type_str_
    public :: zsock_use_fd
    public :: zsock_vmci_buffer_max_size
    public :: zsock_vmci_buffer_min_size
    public :: zsock_vmci_buffer_size
    public :: zsock_vmci_connect_timeout
    public :: zsock_wait
    public :: zsock_zap_domain
    public :: zsock_zap_domain_
    public :: zsock_zap_enforce_domain

    interface
        ! int zsock_affinity(void *self)
        function zsock_affinity(self) bind(c, name='zsock_affinity')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_affinity
        end function zsock_affinity

        ! int zsock_attach(zsock_t *self, const char *endpoints, bool serverish)
        function zsock_attach_(self, endpoints, serverish) bind(c, name='zsock_attach')
            import :: c_bool, c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: endpoints
            logical(c_bool),   intent(in), value :: serverish
            integer(c_int)                       :: zsock_attach_
        end function zsock_attach_

        ! int zsock_backlog(void *self)
        function zsock_backlog(self) bind(c, name='zsock_backlog')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_backlog
        end function zsock_backlog

        ! int zsock_bind_(zsock_t *self, const char *str)
        function zsock_bind_(self, str) bind(c, name='zsock_bind_')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: zsock_bind_
        end function zsock_bind_

        ! char *zsock_bindtodevice(void *self)
        function zsock_bindtodevice_(self) bind(c, name='zsock_bindtodevice')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_bindtodevice_
        end function zsock_bindtodevice_

        ! int zsock_brecv_(void *self, const char *str)
        function zsock_brecv_(self, str) bind(c, name='zsock_brecv_')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: zsock_brecv_
        end function zsock_brecv_

        ! int zsock_bsend_(void *self, const char *str)
        function zsock_bsend_(self, str) bind(c, name='zsock_bsend_')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: zsock_bsend_
        end function zsock_bsend_

        ! int zsock_connect_(zsock_t *self, const char *str)
        function zsock_connect_(self, str) bind(c, name='zsock_connect_')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: zsock_connect_
        end function zsock_connect_

        ! int zsock_connect_timeout(void *self)
        function zsock_connect_timeout(self) bind(c, name='zsock_connect_timeout')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_connect_timeout
        end function zsock_connect_timeout

        ! char *zsock_curve_publickey(void *self)
        function zsock_curve_publickey_(self) bind(c, name='zsock_curve_publickey')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_curve_publickey_
        end function zsock_curve_publickey_

        ! char *zsock_curve_secretkey(void *self)
        function zsock_curve_secretkey_(self) bind(c, name='zsock_curve_secretkey')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_curve_secretkey_
        end function zsock_curve_secretkey_

        ! int zsock_curve_server(void *self)
        function zsock_curve_server(self) bind(c, name='zsock_curve_server')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_curve_server
        end function zsock_curve_server

        ! char *zsock_curve_serverkey(void *self)
        function zsock_curve_serverkey_(self) bind(c, name='zsock_curve_serverkey')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_curve_serverkey_
        end function zsock_curve_serverkey_

        ! void zsock_destroy(zsock_t **self_p)
        subroutine zsock_destroy(self_p) bind(c, name='zsock_destroy')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(inout) :: self_p
        end subroutine zsock_destroy

        ! int zsock_disconnect_(zsock_t *self, const char *str)
        function zsock_disconnect(self, str) bind(c, name='zsock_disconnect_')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: zsock_disconnect
        end function zsock_disconnect

        ! const char *zsock_endpoint(zsock_t *self)
        function zsock_endpoint_(self) bind(c, name='zsock_endpoint')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_endpoint_
        end function zsock_endpoint_

        ! int zsock_events(void *self)
        function zsock_events(self) bind(c, name='zsock_events')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_events
        end function zsock_events

        ! SOCKET zsock_fd(void *self)
        function zsock_fd(self) bind(c, name='zsock_fd')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_fd
        end function zsock_fd

        ! void zsock_flush(void *self)
        subroutine zsock_flush(self) bind(c, name='zsock_flush')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
        end subroutine zsock_flush

        ! int zsock_gssapi_plaintext(void *self)
        function zsock_gssapi_plaintext(self) bind(c, name='zsock_gssapi_plaintext')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_gssapi_plaintext
        end function zsock_gssapi_plaintext

        ! char *zsock_gssapi_principal(void *self)
        function zsock_gssapi_principal_(self) bind(c, name='zsock_gssapi_principal')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_gssapi_principal_
        end function zsock_gssapi_principal_

        ! int zsock_gssapi_principal_nametype(void *self)
        function zsock_gssapi_principal_nametype(self) bind(c, name='zsock_gssapi_principal_nametype')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_gssapi_principal_nametype
        end function zsock_gssapi_principal_nametype

        ! int zsock_gssapi_server(void *self)
        function zsock_gssapi_server(self) bind(c, name='zsock_gssapi_server')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_gssapi_server
        end function zsock_gssapi_server

        ! char *zsock_gssapi_service_principal(void *self)
        function zsock_gssapi_service_principal_(self) bind(c, name='zsock_gssapi_service_principal')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_gssapi_service_principal_
        end function zsock_gssapi_service_principal_

        ! int zsock_gssapi_service_principal_nametype(void *self)
        function zsock_gssapi_service_principal_nametype(self) bind(c, name='zsock_gssapi_service_principal_nametype')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_gssapi_service_principal_nametype
        end function zsock_gssapi_service_principal_nametype

        ! int zsock_handshake_ivl(void *self)
        function zsock_handshake_ivl(self) bind(c, name='zsock_handshake_ivl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_handshake_ivl
        end function zsock_handshake_ivl

        ! int zsock_heartbeat_ivl(void *self)
        function zsock_heartbeat_ivl(self) bind(c, name='zsock_heartbeat_ivl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_heartbeat_ivl
        end function zsock_heartbeat_ivl

        ! int zsock_heartbeat_timeout(void *self)
        function zsock_heartbeat_timeout(self) bind(c, name='zsock_heartbeat_timeout')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_heartbeat_timeout
        end function zsock_heartbeat_timeout

        ! int zsock_heartbeat_ttl(void *self)
        function zsock_heartbeat_ttl(self) bind(c, name='zsock_heartbeat_ttl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_heartbeat_ttl
        end function zsock_heartbeat_ttl

        ! int zsock_hwm(void *self)
        function zsock_hwm(self) bind(c, name='zsock_hwm')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_hwm
        end function zsock_hwm

        ! char *zsock_identity(void *self)
        function zsock_identity_(self) bind(c, name='zsock_identity')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_identity_
        end function zsock_identity_

        ! int zsock_immediate(void *self)
        function zsock_immediate(self) bind(c, name='zsock_immediate')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_immediate
        end function zsock_immediate

        ! int zsock_in_batch_size(void *self)
        function zsock_in_batch_size(self) bind(c, name='zsock_in_batch_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_in_batch_size
        end function zsock_in_batch_size

        ! int zsock_invert_matching(void *self)
        function zsock_invert_matching(self) bind(c, name='zsock_invert_matching')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_invert_matching
        end function zsock_invert_matching

        ! int zsock_ipv4only(void *self)
        function zsock_ipv4only(self) bind(c, name='zsock_ipv4only')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_ipv4only
        end function zsock_ipv4only

        ! int zsock_ipv6(void *self)
        function zsock_ipv6(self) bind(c, name='zsock_ipv6')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_ipv6
        end function zsock_ipv6

        ! bool zsock_is(void *self)
        function zsock_is(self) bind(c, name='zsock_is')
            import :: c_bool, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            logical(c_bool)                :: zsock_is
        end function zsock_is

        ! char *zsock_last_endpoint(void *self)
        function zsock_last_endpoint_(self) bind(c, name='zsock_last_endpoint')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_last_endpoint_
        end function zsock_last_endpoint_

        ! int zsock_linger(void *self)
        function zsock_linger(self) bind(c, name='zsock_linger')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_linger
        end function zsock_linger

        ! int zsock_loopback_fastpath(void *self)
        function zsock_loopback_fastpath(self) bind(c, name='zsock_loopback_fastpath')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_loopback_fastpath
        end function zsock_loopback_fastpath

        ! int zsock_maxmsgsize(void *self)
        function zsock_maxmsgsize(self) bind(c, name='zsock_maxmsgsize')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_maxmsgsize
        end function zsock_maxmsgsize

        ! int zsock_mcast_loop(void *self)
        function zsock_mcast_loop(self) bind(c, name='zsock_mcast_loop')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_mcast_loop
        end function zsock_mcast_loop

        ! int zsock_mechanism(void *self)
        function zsock_mechanism(self) bind(c, name='zsock_mechanism')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_mechanism
        end function zsock_mechanism

        ! char *zsock_metadata(void *self)
        function zsock_metadata_(self) bind(c, name='zsock_metadata')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_metadata_
        end function zsock_metadata_

        ! int zsock_multicast_hops(void *self)
        function zsock_multicast_hops(self) bind(c, name='zsock_multicast_hops')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_multicast_hops
        end function zsock_multicast_hops

        ! int zsock_multicast_loop(void *self)
        function zsock_multicast_loop(self) bind(c, name='zsock_multicast_loop')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_multicast_loop
        end function zsock_multicast_loop

        ! int zsock_multicast_maxtpdu(void *self)
        function zsock_multicast_maxtpdu(self) bind(c, name='zsock_multicast_maxtpdu')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_multicast_maxtpdu
        end function zsock_multicast_maxtpdu

        ! zsock_t *zsock_new(int type)
        function zsock_new(type) bind(c, name='zsock_new')
            import :: c_int, c_ptr
            implicit none
            integer(c_int), intent(in), value :: type
            type(c_ptr)                       :: zsock_new
        end function zsock_new

        ! zsock_t *zsock_new_dealer(const char *endpoint)
        function zsock_new_dealer_(endpoint) bind(c, name='zsock_new_dealer')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_dealer_
        end function zsock_new_dealer_

        ! zsock_t *zsock_new_pair(const char *endpoint)
        function zsock_new_pair_(endpoint) bind(c, name='zsock_new_pair')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_pair_
        end function zsock_new_pair_

        ! zsock_t *zsock_new_pub(const char *endpoint)
        function zsock_new_pub_(endpoint) bind(c, name='zsock_new_pub')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_pub_
        end function zsock_new_pub_

        ! zsock_t *zsock_new_pull(const char *endpoint)
        function zsock_new_pull_(endpoint) bind(c, name='zsock_new_pull')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_pull_
        end function zsock_new_pull_

        ! zsock_t *zsock_new_push(const char *endpoint)
        function zsock_new_push_(endpoint) bind(c, name='zsock_new_push')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_push_
        end function zsock_new_push_

        ! zsock_t *zsock_new_rep(const char *endpoint)
        function zsock_new_rep_(endpoint) bind(c, name='zsock_new_rep')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_rep_
        end function zsock_new_rep_

        ! zsock_t *zsock_new_req(const char *endpoint)
        function zsock_new_req_(endpoint) bind(c, name='zsock_new_req')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_req_
        end function zsock_new_req_

        ! zsock_t *zsock_new_router(const char *endpoint)
        function zsock_new_router_(endpoint) bind(c, name='zsock_new_router')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_router_
        end function zsock_new_router_

        ! zsock_t *zsock_new_stream(const char *endpoint)
        function zsock_new_stream_(endpoint) bind(c, name='zsock_new_stream')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_stream_
        end function zsock_new_stream_

        ! zsock_t *zsock_new_sub(const char *endpoint, const char *subscribe)
        function zsock_new_sub_(endpoint, subscribe) bind(c, name='zsock_new_sub')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            character(c_char), intent(in) :: subscribe
            type(c_ptr)                   :: zsock_new_sub_
        end function zsock_new_sub_

        ! zsock_t *zsock_new_xpub(const char *endpoint)
        function zsock_new_xpub_(endpoint) bind(c, name='zsock_new_xpub')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_xpub_
        end function zsock_new_xpub_

        ! zsock_t *zsock_new_xsub(const char *endpoint)
        function zsock_new_xsub_(endpoint) bind(c, name='zsock_new_xsub')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: endpoint
            type(c_ptr)                   :: zsock_new_xsub_
        end function zsock_new_xsub_

        ! int zsock_out_batch_size(void *self)
        function zsock_out_batch_size(self) bind(c, name='zsock_out_batch_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_out_batch_size
        end function zsock_out_batch_size

        ! char *zsock_plain_password(void *self)
        function zsock_plain_password_(self) bind(c, name='zsock_plain_password')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_plain_password_
        end function zsock_plain_password_

        ! int zsock_plain_server(void *self)
        function zsock_plain_server(self) bind(c, name='zsock_plain_server')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_plain_server
        end function zsock_plain_server

        ! char *zsock_plain_username(void *self)
        function zsock_plain_username_(self) bind(c, name='zsock_plain_username')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_plain_username_
        end function zsock_plain_username_

        ! int zsock_priority(void *self)
        function zsock_priority(self) bind(c, name='zsock_priority')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_priority
        end function zsock_priority

        ! int zsock_rate(void *self)
        function zsock_rate(self) bind(c, name='zsock_rate')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_rate
        end function zsock_rate

        ! int zsock_rcvbuf(void *self)
        function zsock_rcvbuf(self) bind(c, name='zsock_rcvbuf')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_rcvbuf
        end function zsock_rcvbuf

        ! int zsock_rcvhwm(void *self)
        function zsock_rcvhwm(self) bind(c, name='zsock_rcvhwm')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_rcvhwm
        end function zsock_rcvhwm

        ! int zsock_rcvmore(void *self)
        function zsock_rcvmore(self) bind(c, name='zsock_rcvmore')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_rcvmore
        end function zsock_rcvmore

        ! int zsock_rcvtimeo(void *self)
        function zsock_rcvtimeo(self) bind(c, name='zsock_rcvtimeo')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_rcvtimeo
        end function zsock_rcvtimeo

       ! int zsock_recv_(void *self, const char *str)
        function zsock_recv(self, str) bind(c, name='zsock_recv_')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: zsock_recv
        end function zsock_recv

        ! int zsock_reconnect_ivl(void *self)
        function zsock_reconnect_ivl(self) bind(c, name='zsock_reconnect_ivl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_reconnect_ivl
        end function zsock_reconnect_ivl

        ! int zsock_reconnect_ivl_max(void *self)
        function zsock_reconnect_ivl_max(self) bind(c, name='zsock_reconnect_ivl_max')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_reconnect_ivl_max
        end function zsock_reconnect_ivl_max

        ! int zsock_reconnect_stop(void *self)
        function zsock_reconnect_stop(self) bind(c, name='zsock_reconnect_stop')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_reconnect_stop
        end function zsock_reconnect_stop

        ! int zsock_recovery_ivl(void *self)
        function zsock_recovery_ivl(self) bind(c, name='zsock_recovery_ivl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_recovery_ivl
        end function zsock_recovery_ivl

        ! int zsock_recovery_ivl_msec(void *self)
        function zsock_recovery_ivl_msec(self) bind(c, name='zsock_recovery_ivl_msec')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_recovery_ivl_msec
        end function zsock_recovery_ivl_msec

        ! void *zsock_resolve(void *self)
        function zsock_resolve(self) bind(c, name='zsock_resolve')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_resolve
        end function zsock_resolve

        ! int zsock_router_notify(void *self)
        function zsock_router_notify(self) bind(c, name='zsock_router_notify')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_router_notify
        end function zsock_router_notify

        ! int zsock_send_(void *self, const char *str)
        function zsock_send(self, str) bind(c, name='zsock_send_')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: str
            integer(c_int)                       :: zsock_send
        end function zsock_send

        ! void zsock_set_affinity(void *self, int affinity)
        subroutine zsock_set_affinity(self, affinity) bind(c, name='zsock_set_affinity')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: affinity
        end subroutine zsock_set_affinity

        ! void zsock_set_backlog(void *self, int backlog)
        subroutine zsock_set_backlog(self, backlog) bind(c, name='zsock_set_backlog')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: backlog
        end subroutine zsock_set_backlog

        ! void zsock_set_bindtodevice(void *self, const char *bindtodevice)
        subroutine zsock_set_bindtodevice_(self, bindtodevice) bind(c, name='zsock_set_bindtodevice')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: bindtodevice
        end subroutine zsock_set_bindtodevice_

        ! void zsock_set_conflate(void *self, int conflate)
        subroutine zsock_set_conflate(self, conflate) bind(c, name='zsock_set_conflate')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: conflate
        end subroutine zsock_set_conflate

        ! void zsock_set_connect_rid(void *self, const char *connect_rid)
        subroutine zsock_set_connect_rid_(self, connect_rid) bind(c, name='zsock_set_connect_rid')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: connect_rid
        end subroutine zsock_set_connect_rid_

        ! void zsock_set_connect_rid_bin(void *self, const byte *connect_rid)
        subroutine zsock_set_connect_rid_bin(self, connect_rid) bind(c, name='zsock_set_connect_rid_bin')
            import :: c_byte, c_ptr
            implicit none
            type(c_ptr),     intent(in), value :: self
            integer(c_byte), intent(out)       :: connect_rid
        end subroutine zsock_set_connect_rid_bin

        ! void zsock_set_connect_timeout(void *self, int connect_timeout)
        subroutine zsock_set_connect_timeout(self, connect_timeout) bind(c, name='zsock_set_connect_timeout')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: connect_timeout
        end subroutine zsock_set_connect_timeout

        ! void zsock_set_curve_publickey(void *self, const char *curve_publickey)
        subroutine zsock_set_curve_publickey_(self, curve_publickey) bind(c, name='zsock_set_curve_publickey')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: curve_publickey
        end subroutine zsock_set_curve_publickey_

        ! void zsock_set_curve_publickey_bin(void *self, const byte *curve_publickey)
        subroutine zsock_set_curve_publickey_bin(self, curve_publickey) bind(c, name='zsock_set_curve_publickey_bin')
            import :: c_byte, c_ptr
            implicit none
            type(c_ptr),     intent(in), value :: self
            integer(c_byte), intent(out)       :: curve_publickey
        end subroutine zsock_set_curve_publickey_bin

        ! void zsock_set_curve_secretkey(void *self, const char *curve_secretkey)
        subroutine zsock_set_curve_secretkey_(self, curve_secretkey) bind(c, name='zsock_set_curve_secretkey')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: curve_secretkey
        end subroutine zsock_set_curve_secretkey_

        ! void zsock_set_curve_secretkey_bin(void *self, const byte *curve_secretkey)
        subroutine zsock_set_curve_secretkey_bin(self, curve_secretkey) bind(c, name='zsock_set_curve_secretkey_bin')
            import :: c_byte, c_ptr
            implicit none
            type(c_ptr),     intent(in), value :: self
            integer(c_byte), intent(out)       :: curve_secretkey
        end subroutine zsock_set_curve_secretkey_bin

        ! void zsock_set_curve_server(void *self, int curve_server)
        subroutine zsock_set_curve_server(self, curve_server) bind(c, name='zsock_set_curve_server')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: curve_server
        end subroutine zsock_set_curve_server

        ! void zsock_set_curve_serverkey(void *self, const char *curve_serverkey)
        subroutine zsock_set_curve_serverkey_(self, curve_serverkey) bind(c, name='zsock_set_curve_serverkey')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: curve_serverkey
        end subroutine zsock_set_curve_serverkey_

        ! void zsock_set_curve_serverkey_bin(void *self, const byte *curve_serverkey)
        subroutine zsock_set_curve_serverkey_bin(self, curve_serverkey) bind(c, name='zsock_set_curve_serverkey_bin')
            import :: c_byte, c_ptr
            implicit none
            type(c_ptr),     intent(in), value :: self
            integer(c_byte), intent(out)       :: curve_serverkey
        end subroutine zsock_set_curve_serverkey_bin

        ! void zsock_set_delay_attach_on_connect(void *self, int delay_attach_on_connect)
        subroutine zsock_set_delay_attach_on_connect(self, delay_attach_on_connect) &
                bind(c, name='zsock_set_delay_attach_on_connect')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: delay_attach_on_connect
        end subroutine zsock_set_delay_attach_on_connect

        ! void zsock_set_disconnect_msg(void *self, zframe_t *disconnect_msg)
        subroutine zsock_set_disconnect_msg(self, disconnect_msg) bind(c, name='zsock_set_disconnect_msg')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(in), value :: disconnect_msg
        end subroutine zsock_set_disconnect_msg

        ! void zsock_set_gssapi_plaintext(void *self, int gssapi_plaintext)
        subroutine zsock_set_gssapi_plaintext(self, gssapi_plaintext) bind(c, name='zsock_set_gssapi_plaintext')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: gssapi_plaintext
        end subroutine zsock_set_gssapi_plaintext

        ! void zsock_set_gssapi_principal(void *self, const char *gssapi_principal)
        subroutine zsock_set_gssapi_principal_(self, gssapi_principal) bind(c, name='zsock_set_gssapi_principal')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: gssapi_principal
        end subroutine zsock_set_gssapi_principal_

        ! void zsock_set_gssapi_principal_nametype(void *self, int gssapi_principal_nametype)
        subroutine zsock_set_gssapi_principal_nametype(self, gssapi_principal_nametype) &
                bind(c, name='zsock_set_gssapi_principal_nametype')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: gssapi_principal_nametype
        end subroutine zsock_set_gssapi_principal_nametype

        ! void zsock_set_gssapi_server(void *self, int gssapi_server)
        subroutine zsock_set_gssapi_server(self, gssapi_server) bind(c, name='zsock_set_gssapi_server')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: gssapi_server
        end subroutine zsock_set_gssapi_server

        ! void zsock_set_gssapi_service_principal(void *self, const char *gssapi_service_principal)
        subroutine zsock_set_gssapi_service_principal_(self, gssapi_service_principal) &
                bind(c, name='zsock_set_gssapi_service_principal')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: gssapi_service_principal
        end subroutine zsock_set_gssapi_service_principal_

        ! void zsock_set_gssapi_service_principal_nametype(void *self, int gssapi_service_principal_nametype)
        subroutine zsock_set_gssapi_service_principal_nametype(self, gssapi_service_principal_nametype) &
                bind(c, name='zsock_set_gssapi_service_principal_nametype')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: gssapi_service_principal_nametype
        end subroutine zsock_set_gssapi_service_principal_nametype

        ! void zsock_set_handshake_ivl(void *self, int handshake_ivl)
        subroutine zsock_set_handshake_ivl(self, handshake_ivl) bind(c, name='zsock_set_handshake_ivl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: handshake_ivl
        end subroutine zsock_set_handshake_ivl

        ! void zsock_set_heartbeat_ivl(void *self, int heartbeat_ivl)
        subroutine zsock_set_heartbeat_ivl(self, heartbeat_ivl) bind(c, name='zsock_set_heartbeat_ivl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: heartbeat_ivl
        end subroutine zsock_set_heartbeat_ivl

        ! void zsock_set_heartbeat_timeout(void *self, int heartbeat_timeout)
        subroutine zsock_set_heartbeat_timeout(self, heartbeat_timeout) bind(c, name='zsock_set_heartbeat_timeout')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: heartbeat_timeout
        end subroutine zsock_set_heartbeat_timeout

        ! void zsock_set_heartbeat_ttl(void *self, int heartbeat_ttl)
        subroutine zsock_set_heartbeat_ttl(self, heartbeat_ttl) bind(c, name='zsock_set_heartbeat_ttl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: heartbeat_ttl
        end subroutine zsock_set_heartbeat_ttl

        ! void zsock_set_hello_msg(void *self, zframe_t *hello_msg)
        subroutine zsock_set_hello_msg(self, hello_msg) bind(c, name='zsock_set_hello_msg')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(in), value :: hello_msg
        end subroutine zsock_set_hello_msg

        ! void zsock_set_hwm(void *self, int hwm)
        subroutine zsock_set_hwm(self, hwm) bind(c, name='zsock_set_hwm')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: hwm
        end subroutine zsock_set_hwm

        ! void zsock_set_identity(void *self, const char *identity)
        subroutine zsock_set_identity_(self, identity) bind(c, name='zsock_set_identity')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: identity
        end subroutine zsock_set_identity_

        ! void zsock_set_immediate(void *self, int immediate)
        subroutine zsock_set_immediate(self, immediate) bind(c, name='zsock_set_immediate')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: immediate
        end subroutine zsock_set_immediate

        ! void zsock_set_in_batch_size(void *self, int in_batch_size)
        subroutine zsock_set_in_batch_size(self, in_batch_size) bind(c, name='zsock_set_in_batch_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: in_batch_size
        end subroutine zsock_set_in_batch_size

        ! void zsock_set_invert_matching(void *self, int invert_matching)
        subroutine zsock_set_invert_matching(self, invert_matching) bind(c, name='zsock_set_invert_matching')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: invert_matching
        end subroutine zsock_set_invert_matching

        ! void zsock_set_ipv4only(void *self, int ipv4only)
        subroutine zsock_set_ipv4only(self, ipv4only) bind(c, name='zsock_set_ipv4only')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: ipv4only
        end subroutine zsock_set_ipv4only

        ! void zsock_set_ipv6(void *self, int ipv6)
        subroutine zsock_set_ipv6(self, ipv6) bind(c, name='zsock_set_ipv6')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: ipv6
        end subroutine zsock_set_ipv6

        ! void zsock_set_linger(void *self, int linger)
        subroutine zsock_set_linger(self, linger) bind(c, name='zsock_set_linger')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: linger
        end subroutine zsock_set_linger

        ! void zsock_set_loopback_fastpath(void *self, int loopback_fastpath)
        subroutine zsock_set_loopback_fastpath(self, loopback_fastpath) bind(c, name='zsock_set_loopback_fastpath')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: loopback_fastpath
        end subroutine zsock_set_loopback_fastpath

        ! void zsock_set_maxmsgsize(void *self, int maxmsgsize)
        subroutine zsock_set_maxmsgsize(self, maxmsgsize) bind(c, name='zsock_set_maxmsgsize')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: maxmsgsize
        end subroutine zsock_set_maxmsgsize

        ! void zsock_set_mcast_loop(void *self, int mcast_loop)
        subroutine zsock_set_mcast_loop(self, mcast_loop) bind(c, name='zsock_set_mcast_loop')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: mcast_loop
        end subroutine zsock_set_mcast_loop

        ! void zsock_set_metadata(void *self, const char *metadata)
        subroutine zsock_set_metadata_(self, metadata) bind(c, name='zsock_set_metadata')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: metadata
        end subroutine zsock_set_metadata_

        ! void zsock_set_multicast_hops(void *self, int multicast_hops)
        subroutine zsock_set_multicast_hops(self, multicast_hops) bind(c, name='zsock_set_multicast_hops')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: multicast_hops
        end subroutine zsock_set_multicast_hops

        ! void zsock_set_multicast_loop(void *self, int multicast_loop)
        subroutine zsock_set_multicast_loop(self, multicast_loop) bind(c, name='zsock_set_multicast_loop')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: multicast_loop
        end subroutine zsock_set_multicast_loop

        ! void zsock_set_multicast_maxtpdu(void *self, int multicast_maxtpdu)
        subroutine zsock_set_multicast_maxtpdu(self, multicast_maxtpdu) bind(c, name='zsock_set_multicast_maxtpdu')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: multicast_maxtpdu
        end subroutine zsock_set_multicast_maxtpdu

        ! void zsock_set_only_first_subscribe(void *self, int only_first_subscribe)
        subroutine zsock_set_only_first_subscribe(self, only_first_subscribe) bind(c, name='zsock_set_only_first_subscribe')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: only_first_subscribe
        end subroutine zsock_set_only_first_subscribe

        ! void zsock_set_out_batch_size(void *self, int out_batch_size)
        subroutine zsock_set_out_batch_size(self, out_batch_size) bind(c, name='zsock_set_out_batch_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: out_batch_size
        end subroutine zsock_set_out_batch_size

        ! void zsock_set_plain_password(void *self, const char *plain_password)
        subroutine zsock_set_plain_password_(self, plain_password) bind(c, name='zsock_set_plain_password')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: plain_password
        end subroutine zsock_set_plain_password_

        ! void zsock_set_plain_server(void *self, int plain_server)
        subroutine zsock_set_plain_server(self, plain_server) bind(c, name='zsock_set_plain_server')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: plain_server
        end subroutine zsock_set_plain_server

        ! void zsock_set_plain_username(void *self, const char *plain_username)
        subroutine zsock_set_plain_username_(self, plain_username) bind(c, name='zsock_set_plain_username')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: plain_username
        end subroutine zsock_set_plain_username_

        ! void zsock_set_priority(void *self, int priority)
        subroutine zsock_set_priority(self, priority) bind(c, name='zsock_set_priority')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: priority
        end subroutine zsock_set_priority

        ! void zsock_set_probe_router(void *self, int probe_router)
        subroutine zsock_set_probe_router(self, probe_router) bind(c, name='zsock_set_probe_router')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: probe_router
        end subroutine zsock_set_probe_router

        ! void zsock_set_rate(void *self, int rate)
        subroutine zsock_set_rate(self, rate) bind(c, name='zsock_set_rate')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: rate
        end subroutine zsock_set_rate

        ! void zsock_set_rcvbuf(void *self, int rcvbuf)
        subroutine zsock_set_rcvbuf(self, rcvbuf) bind(c, name='zsock_set_rcvbuf')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: rcvbuf
        end subroutine zsock_set_rcvbuf

        ! void zsock_set_rcvhwm(void *self, int rcvhwm)
        subroutine zsock_set_rcvhwm(self, rcvhwm) bind(c, name='zsock_set_rcvhwm')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: rcvhwm
        end subroutine zsock_set_rcvhwm

        ! void zsock_set_rcvtimeo(void *self, int rcvtimeo)
        subroutine zsock_set_rcvtimeo(self, rcvtimeo) bind(c, name='zsock_set_rcvtimeo')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: rcvtimeo
        end subroutine zsock_set_rcvtimeo

        ! void zsock_set_reconnect_ivl(void *self, int reconnect_ivl)
        subroutine zsock_set_reconnect_ivl(self, reconnect_ivl) bind(c, name='zsock_set_reconnect_ivl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: reconnect_ivl
        end subroutine zsock_set_reconnect_ivl

        ! void zsock_set_reconnect_ivl_max(void *self, int reconnect_ivl_max)
        subroutine zsock_set_reconnect_ivl_max(self, reconnect_ivl_max) bind(c, name='zsock_set_reconnect_ivl_max')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: reconnect_ivl_max
        end subroutine zsock_set_reconnect_ivl_max

        ! void zsock_set_reconnect_stop(void *self, int reconnect_stop)
        subroutine zsock_set_reconnect_stop(self, reconnect_stop) bind(c, name='zsock_set_reconnect_stop')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: reconnect_stop
        end subroutine zsock_set_reconnect_stop

        ! void zsock_set_recovery_ivl(void *self, int recovery_ivl)
        subroutine zsock_set_recovery_ivl(self, recovery_ivl) bind(c, name='zsock_set_recovery_ivl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: recovery_ivl
        end subroutine zsock_set_recovery_ivl

        ! void zsock_set_recovery_ivl_msec(void *self, int recovery_ivl_msec)
        subroutine zsock_set_recovery_ivl_msec(self, recovery_ivl_msec) bind(c, name='zsock_set_recovery_ivl_msec')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: recovery_ivl_msec
        end subroutine zsock_set_recovery_ivl_msec

        ! void zsock_set_req_correlate(void *self, int req_correlate)
        subroutine zsock_set_req_correlate(self, req_correlate) bind(c, name='zsock_set_req_correlate')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: req_correlate
        end subroutine zsock_set_req_correlate

        ! void zsock_set_req_relaxed(void *self, int req_relaxed)
        subroutine zsock_set_req_relaxed(self, req_relaxed) bind(c, name='zsock_set_req_relaxed')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: req_relaxed
        end subroutine zsock_set_req_relaxed

        ! void zsock_set_router_handover(void *self, int router_handover)
        subroutine zsock_set_router_handover(self, router_handover) bind(c, name='zsock_set_router_handover')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: router_handover
        end subroutine zsock_set_router_handover

        ! void zsock_set_router_mandatory(void *self, int router_mandatory)
        subroutine zsock_set_router_mandatory(self, router_mandatory) bind(c, name='zsock_set_router_mandatory')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: router_mandatory
        end subroutine zsock_set_router_mandatory

        ! void zsock_set_router_notify(void *self, int router_notify)
        subroutine zsock_set_router_notify(self, router_notify) bind(c, name='zsock_set_router_notify')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: router_notify
        end subroutine zsock_set_router_notify

        ! void zsock_set_router_raw(void *self, int router_raw)
        subroutine zsock_set_router_raw(self, router_raw) bind(c, name='zsock_set_router_raw')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: router_raw
        end subroutine zsock_set_router_raw

        ! void zsock_set_sndbuf(void *self, int sndbuf)
        subroutine zsock_set_sndbuf(self, sndbuf) bind(c, name='zsock_set_sndbuf')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: sndbuf
        end subroutine zsock_set_sndbuf

        ! void zsock_set_sndhwm(void *self, int sndhwm)
        subroutine zsock_set_sndhwm(self, sndhwm) bind(c, name='zsock_set_sndhwm')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: sndhwm
        end subroutine zsock_set_sndhwm

        ! void zsock_set_sndtimeo(void *self, int sndtimeo)
        subroutine zsock_set_sndtimeo(self, sndtimeo) bind(c, name='zsock_set_sndtimeo')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: sndtimeo
        end subroutine zsock_set_sndtimeo

        ! void zsock_set_socks_password(void *self, const char *socks_password)
        subroutine zsock_set_socks_password_(self, socks_password) bind(c, name='zsock_set_socks_password')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: socks_password
        end subroutine zsock_set_socks_password_

        ! void zsock_set_socks_proxy(void *self, const char *socks_proxy)
        subroutine zsock_set_socks_proxy_(self, socks_proxy) bind(c, name='zsock_set_socks_proxy')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: socks_proxy
        end subroutine zsock_set_socks_proxy_

        ! void zsock_set_socks_username(void *self, const char *socks_username)
        subroutine zsock_set_socks_username_(self, socks_username) bind(c, name='zsock_set_socks_username')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: socks_username
        end subroutine zsock_set_socks_username_

        ! void zsock_set_stream_notify(void *self, int stream_notify)
        subroutine zsock_set_stream_notify(self, stream_notify) bind(c, name='zsock_set_stream_notify')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: stream_notify
        end subroutine zsock_set_stream_notify

        ! void zsock_set_subscribe(void *self, const char *subscribe)
        subroutine zsock_set_subscribe_(self, subscribe) bind(c, name='zsock_set_subscribe')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: subscribe
        end subroutine zsock_set_subscribe_

        ! void zsock_set_swap(void *self, int swap)
        subroutine zsock_set_swap(self, swap) bind(c, name='zsock_set_swap')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: swap
        end subroutine zsock_set_swap

        ! void zsock_set_tcp_accept_filter(void *self, const char *tcp_accept_filter)
        subroutine zsock_set_tcp_accept_filter_(self, tcp_accept_filter) bind(c, name='zsock_set_tcp_accept_filter')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: tcp_accept_filter
        end subroutine zsock_set_tcp_accept_filter_

        ! void zsock_set_tcp_keepalive(void *self, int tcp_keepalive)
        subroutine zsock_set_tcp_keepalive(self, tcp_keepalive) bind(c, name='zsock_set_tcp_keepalive')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: tcp_keepalive
        end subroutine zsock_set_tcp_keepalive

        ! void zsock_set_tcp_keepalive_cnt(void *self, int tcp_keepalive_cnt)
        subroutine zsock_set_tcp_keepalive_cnt(self, tcp_keepalive_cnt) bind(c, name='zsock_set_tcp_keepalive_cnt')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: tcp_keepalive_cnt
        end subroutine zsock_set_tcp_keepalive_cnt

        ! void zsock_set_tcp_keepalive_idle(void *self, int tcp_keepalive_idle)
        subroutine zsock_set_tcp_keepalive_idle(self, tcp_keepalive_idle) bind(c, name='zsock_set_tcp_keepalive_idle')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: tcp_keepalive_idle
        end subroutine zsock_set_tcp_keepalive_idle

        ! void zsock_set_tcp_keepalive_intvl(void *self, int tcp_keepalive_intvl)
        subroutine zsock_set_tcp_keepalive_intvl(self, tcp_keepalive_intvl) bind(c, name='zsock_set_tcp_keepalive_intvl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: tcp_keepalive_intvl
        end subroutine zsock_set_tcp_keepalive_intvl

        ! void zsock_set_tcp_maxrt(void *self, int tcp_maxrt)
        subroutine zsock_set_tcp_maxrt(self, tcp_maxrt) bind(c, name='zsock_set_tcp_maxrt')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: tcp_maxrt
        end subroutine zsock_set_tcp_maxrt

        ! void zsock_set_tos(void *self, int tos)
        subroutine zsock_set_tos(self, tos) bind(c, name='zsock_set_tos')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: tos
        end subroutine zsock_set_tos

        ! void zsock_set_unbounded(void *self)
        subroutine zsock_set_unbounded(self) bind(c, name='zsock_set_unbounded')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
        end subroutine zsock_set_unbounded

        ! void zsock_set_unsubscribe(void *self, const char *unsubscribe)
        subroutine zsock_set_unsubscribe_(self, unsubscribe) bind(c, name='zsock_set_unsubscribe')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: unsubscribe
        end subroutine zsock_set_unsubscribe_

        ! void zsock_set_use_fd(void *self, int use_fd)
        subroutine zsock_set_use_fd(self, use_fd) bind(c, name='zsock_set_use_fd')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: use_fd
        end subroutine zsock_set_use_fd

        ! void zsock_set_vmci_buffer_max_size(void *self, int vmci_buffer_max_size)
        subroutine zsock_set_vmci_buffer_max_size(self, vmci_buffer_max_size) bind(c, name='zsock_set_vmci_buffer_max_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: vmci_buffer_max_size
        end subroutine zsock_set_vmci_buffer_max_size

        ! void zsock_set_vmci_buffer_min_size(void *self, int vmci_buffer_min_size)
        subroutine zsock_set_vmci_buffer_min_size(self, vmci_buffer_min_size) bind(c, name='zsock_set_vmci_buffer_min_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: vmci_buffer_min_size
        end subroutine zsock_set_vmci_buffer_min_size

        ! void zsock_set_vmci_buffer_size(void *self, int vmci_buffer_size)
        subroutine zsock_set_vmci_buffer_size(self, vmci_buffer_size) bind(c, name='zsock_set_vmci_buffer_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: vmci_buffer_size
        end subroutine zsock_set_vmci_buffer_size

        ! void zsock_set_vmci_connect_timeout(void *self, int vmci_connect_timeout)
        subroutine zsock_set_vmci_connect_timeout(self, vmci_connect_timeout) bind(c, name='zsock_set_vmci_connect_timeout')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: vmci_connect_timeout
        end subroutine zsock_set_vmci_connect_timeout

        ! void zsock_set_wss_cert_pem(void *self, const char *wss_cert_pem)
        subroutine zsock_set_wss_cert_pem_(self, wss_cert_pem) bind(c, name='zsock_set_wss_cert_pem')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: wss_cert_pem
        end subroutine zsock_set_wss_cert_pem_

        ! void zsock_set_wss_hostname(void *self, const char *wss_hostname)
        subroutine zsock_set_wss_hostname_(self, wss_hostname) bind(c, name='zsock_set_wss_hostname')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: wss_hostname
        end subroutine zsock_set_wss_hostname_

        ! void zsock_set_wss_trust_pem(void *self, const char *wss_trust_pem)
        subroutine zsock_set_wss_trust_pem_(self, wss_trust_pem) bind(c, name='zsock_set_wss_trust_pem')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: wss_trust_pem
        end subroutine zsock_set_wss_trust_pem_

        ! void zsock_set_wss_trust_system(void *self, int wss_trust_system)
        subroutine zsock_set_wss_trust_system(self, wss_trust_system) bind(c, name='zsock_set_wss_trust_system')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: wss_trust_system
        end subroutine zsock_set_wss_trust_system

        ! void zsock_set_xpub_manual(void *self, int xpub_manual)
        subroutine zsock_set_xpub_manual(self, xpub_manual) bind(c, name='zsock_set_xpub_manual')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: xpub_manual
        end subroutine zsock_set_xpub_manual

        ! void zsock_set_xpub_manual_last_value(void *self, int xpub_manual_last_value)
        subroutine zsock_set_xpub_manual_last_value(self, xpub_manual_last_value) bind(c, name='zsock_set_xpub_manual_last_value')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: xpub_manual_last_value
        end subroutine zsock_set_xpub_manual_last_value

        ! void zsock_set_xpub_nodrop(void *self, int xpub_nodrop)
        subroutine zsock_set_xpub_nodrop(self, xpub_nodrop) bind(c, name='zsock_set_xpub_nodrop')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: xpub_nodrop
        end subroutine zsock_set_xpub_nodrop

        ! void zsock_set_xpub_verbose(void *self, int xpub_verbose)
        subroutine zsock_set_xpub_verbose(self, xpub_verbose) bind(c, name='zsock_set_xpub_verbose')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: xpub_verbose
        end subroutine zsock_set_xpub_verbose

        ! void zsock_set_xpub_verboser(void *self, int xpub_verboser)
        subroutine zsock_set_xpub_verboser(self, xpub_verboser) bind(c, name='zsock_set_xpub_verboser')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: xpub_verboser
        end subroutine zsock_set_xpub_verboser

        ! void zsock_set_xpub_welcome_msg(void *self, const char *xpub_welcome_msg)
        subroutine zsock_set_xpub_welcome_msg_(self, xpub_welcome_msg) bind(c, name='zsock_set_xpub_welcome_msg')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: xpub_welcome_msg
        end subroutine zsock_set_xpub_welcome_msg_

        ! void zsock_set_zap_domain(void *self, const char *zap_domain)
        subroutine zsock_set_zap_domain_(self, zap_domain) bind(c, name='zsock_set_zap_domain')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: zap_domain
        end subroutine zsock_set_zap_domain_

        ! void zsock_set_zap_enforce_domain(void *self, int zap_enforce_domain)
        subroutine zsock_set_zap_enforce_domain(self, zap_enforce_domain) bind(c, name='zsock_set_zap_enforce_domain')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: zap_enforce_domain
        end subroutine zsock_set_zap_enforce_domain

        ! int zsock_signal(void *self, byte status)
        function zsock_signal(self, status) bind(c, name='zsock_signal')
            import :: c_byte, c_int, c_ptr
            implicit none
            type(c_ptr),     intent(in), value :: self
            integer(c_byte), intent(in), value :: status
            integer(c_int)                     :: zsock_signal
        end function zsock_signal

        ! int zsock_sndbuf(void *self)
        function zsock_sndbuf(self) bind(c, name='zsock_sndbuf')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_sndbuf
        end function zsock_sndbuf

        ! int zsock_sndhwm(void *self)
        function zsock_sndhwm(self) bind(c, name='zsock_sndhwm')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_sndhwm
        end function zsock_sndhwm

        ! int zsock_sndtimeo(void *self)
        function zsock_sndtimeo(self) bind(c, name='zsock_sndtimeo')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_sndtimeo
        end function zsock_sndtimeo

        ! char *zsock_socks_password(void *self)
        function zsock_socks_password_(self) bind(c, name='zsock_socks_password')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_socks_password_
        end function zsock_socks_password_

        ! char *zsock_socks_proxy(void *self)
        function zsock_socks_proxy_(self) bind(c, name='zsock_socks_proxy')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_socks_proxy_
        end function zsock_socks_proxy_

        ! char *zsock_socks_username(void *self)
        function zsock_socks_username_(self) bind(c, name='zsock_socks_username')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_socks_username_
        end function zsock_socks_username_

        ! int zsock_swap(void *self)
        function zsock_swap(self) bind(c, name='zsock_swap')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_swap
        end function zsock_swap

        ! char *zsock_tcp_accept_filter(void *self)
        function zsock_tcp_accept_filter_(self) bind(c, name='zsock_tcp_accept_filter')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_tcp_accept_filter_
        end function zsock_tcp_accept_filter_

        ! int zsock_tcp_keepalive(void *self)
        function zsock_tcp_keepalive(self) bind(c, name='zsock_tcp_keepalive')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_tcp_keepalive
        end function zsock_tcp_keepalive

        ! int zsock_tcp_keepalive_cnt(void *self)
        function zsock_tcp_keepalive_cnt(self) bind(c, name='zsock_tcp_keepalive_cnt')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_tcp_keepalive_cnt
        end function zsock_tcp_keepalive_cnt

        ! int zsock_tcp_keepalive_idle(void *self)
        function zsock_tcp_keepalive_idle(self) bind(c, name='zsock_tcp_keepalive_idle')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_tcp_keepalive_idle
        end function zsock_tcp_keepalive_idle

        ! int zsock_tcp_keepalive_intvl(void *self)
        function zsock_tcp_keepalive_intvl(self) bind(c, name='zsock_tcp_keepalive_intvl')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_tcp_keepalive_intvl
        end function zsock_tcp_keepalive_intvl

        ! int zsock_tcp_maxrt(void *self)
        function zsock_tcp_maxrt(self) bind(c, name='zsock_tcp_maxrt')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_tcp_maxrt
        end function zsock_tcp_maxrt

        ! void zsock_test(bool verbose)
        subroutine zsock_test(verbose) bind(c, name='zsock_test')
            import :: c_bool
            implicit none
            logical(c_bool), intent(in), value :: verbose
        end subroutine zsock_test

        ! int zsock_thread_safe(void *self)
        function zsock_thread_safe(self) bind(c, name='zsock_thread_safe')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_thread_safe
        end function zsock_thread_safe

        ! int zsock_tos(void *self)
        function zsock_tos(self) bind(c, name='zsock_tos')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_tos
        end function zsock_tos

        ! int zsock_type(void *self)
        function zsock_type(self) bind(c, name='zsock_type')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_type
        end function zsock_type

        ! const char *zsock_type_str(zsock_t *self)
        function zsock_type_str_(self) bind(c, name='zsock_type_str')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_type_str_
        end function zsock_type_str_

        ! int zsock_use_fd(void *self)
        function zsock_use_fd(self) bind(c, name='zsock_use_fd')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_use_fd
        end function zsock_use_fd

        ! int zsock_vmci_buffer_max_size(void *self)
        function zsock_vmci_buffer_max_size(self) bind(c, name='zsock_vmci_buffer_max_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_vmci_buffer_max_size
        end function zsock_vmci_buffer_max_size

        ! int zsock_vmci_buffer_min_size(void *self)
        function zsock_vmci_buffer_min_size(self) bind(c, name='zsock_vmci_buffer_min_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_vmci_buffer_min_size
        end function zsock_vmci_buffer_min_size

        ! int zsock_vmci_buffer_size(void *self)
        function zsock_vmci_buffer_size(self) bind(c, name='zsock_vmci_buffer_size')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_vmci_buffer_size
        end function zsock_vmci_buffer_size

        ! int zsock_vmci_connect_timeout(void *self)
        function zsock_vmci_connect_timeout(self) bind(c, name='zsock_vmci_connect_timeout')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_vmci_connect_timeout
        end function zsock_vmci_connect_timeout

        ! int zsock_wait(void *self)
        function zsock_wait(self) bind(c, name='zsock_wait')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_wait
        end function zsock_wait

        ! char *zsock_zap_domain(void *self)
        function zsock_zap_domain_(self) bind(c, name='zsock_zap_domain')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zsock_zap_domain_
        end function zsock_zap_domain_

        ! int zsock_zap_enforce_domain(void *self)
        function zsock_zap_enforce_domain(self) bind(c, name='zsock_zap_enforce_domain')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zsock_zap_enforce_domain
        end function zsock_zap_enforce_domain
    end interface
contains
     ! int zsock_attach(zsock_t *self, const char *endpoints, bool serverish)
     integer function zsock_attach(self, endpoints, serverish) result(rc)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: endpoints
        logical,      intent(in) :: serverish

        rc = zsock_attach_(self, f_c_str(endpoints), logical(serverish, c_bool))
    end function zsock_attach

    ! int zsock_bind_(zsock_t *self, const char *str)
    integer function zsock_bind(self, str) result(rc)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: str

        rc = zsock_bind_(self, f_c_str(str))
    end function zsock_bind

    ! int zsock_brecv_(void *self, const char *str)
    integer function zsock_brecv(self, str) result(rc)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: str

        rc = zsock_brecv_(self, f_c_str(str))
    end function zsock_brecv

    ! int zsock_bsend_(void *self, const char *str)
    integer function zsock_bsend(self, str) result(rc)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: str

        rc = zsock_bsend_(self, f_c_str(str))
    end function zsock_bsend

    ! int zsock_connect_(zsock_t *self, const char *str)
    integer function zsock_connect(self, str) result(rc)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: str

        rc = zsock_connect_(self, f_c_str(str))
    end function zsock_connect

    ! char *zsock_bindtodevice(void *self)
    function zsock_bindtodevice(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_bindtodevice_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_bindtodevice

    ! char *zsock_curve_publickey(void *self)
    function zsock_curve_publickey(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_curve_publickey_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_curve_publickey

    ! char *zsock_curve_secretkey(void *self)
    function zsock_curve_secretkey(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_curve_secretkey_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_curve_secretkey

    ! char *zsock_curve_serverkey(void *self)
    function zsock_curve_serverkey(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_curve_serverkey_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_curve_serverkey

    ! const char *zsock_endpoint(zsock_t *self)
    function zsock_endpoint(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_endpoint_(self)
        call c_f_str_ptr(ptr, str)
    end function zsock_endpoint

    ! char *zsock_gssapi_principal(void *self)
    function zsock_gssapi_principal(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_gssapi_principal_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_gssapi_principal

    ! char *zsock_gssapi_service_principal(void *self)
    function zsock_gssapi_service_principal(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_gssapi_service_principal_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_gssapi_service_principal

    ! char *zsock_identity(void *self)
    function zsock_identity(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_identity_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_identity

    ! char *zsock_last_endpoint(void *self)
    function zsock_last_endpoint(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_last_endpoint_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_last_endpoint

    ! char *zsock_metadata(void *self)
    function zsock_metadata(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_metadata_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_metadata

    ! zsock_t *zsock_new_dealer(const char *endpoint)
    function zsock_new_dealer(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_dealer_(f_c_str(endpoint))
    end function zsock_new_dealer

    ! zsock_t *zsock_new_pair(const char *endpoint)
    function zsock_new_pair(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_pair_(f_c_str(endpoint))
    end function zsock_new_pair

    ! zsock_t *zsock_new_pub(const char *endpoint)
    function zsock_new_pub(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_pub_(f_c_str(endpoint))
    end function zsock_new_pub

    ! zsock_t *zsock_new_pull(const char *endpoint)
    function zsock_new_pull(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_pull_(f_c_str(endpoint))
    end function zsock_new_pull

    ! zsock_t *zsock_new_push(const char *endpoint)
    function zsock_new_push(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_push_(f_c_str(endpoint))
    end function zsock_new_push

    ! zsock_t *zsock_new_rep(const char *endpoint)
    function zsock_new_rep(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_rep_(f_c_str(endpoint))
    end function zsock_new_rep

    ! zsock_t *zsock_new_req(const char *endpoint)
    function zsock_new_req(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_req_(f_c_str(endpoint))
    end function zsock_new_req

    ! zsock_t *zsock_new_router(const char *endpoint)
    function zsock_new_router(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_router_(f_c_str(endpoint))
    end function zsock_new_router

    ! zsock_t *zsock_new_stream(const char *endpoint)
    function zsock_new_stream(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_stream_(f_c_str(endpoint))
    end function zsock_new_stream

    ! zsock_t *zsock_new_sub(const char *endpoint, const char *subscribe)
    function zsock_new_sub(endpoint, subscribe) result(ptr)
        character(*), intent(in) :: endpoint
        character(*), intent(in) :: subscribe
        type(c_ptr)              :: ptr

        ptr = zsock_new_sub_(f_c_str(endpoint), f_c_str(subscribe))
    end function zsock_new_sub

    ! zsock_t *zsock_new_xpub(const char *endpoint)
    function zsock_new_xpub(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_xpub_(f_c_str(endpoint))
    end function zsock_new_xpub

    ! zsock_t *zsock_new_xsub(const char *endpoint)
    function zsock_new_xsub(endpoint) result(ptr)
        character(*), intent(in) :: endpoint
        type(c_ptr)              :: ptr

        ptr = zsock_new_xsub_(f_c_str(endpoint))
    end function zsock_new_xsub

    ! char *zsock_plain_password(void *self)
    function zsock_plain_password(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_plain_password_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_plain_password

    ! char *zsock_plain_username(void *self)
    function zsock_plain_username(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_plain_username_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_plain_username

    ! void zsock_set_bindtodevice(void *self, const char *bindtodevice)
    subroutine zsock_set_bindtodevice(self, bindtodevice)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: bindtodevice

        call zsock_set_bindtodevice_(self, f_c_str(bindtodevice))
    end subroutine zsock_set_bindtodevice

    ! void zsock_set_connect_rid(void *self, const char *connect_rid)
    subroutine zsock_set_connect_rid(self, connect_rid)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: connect_rid

        call zsock_set_connect_rid_(self, f_c_str(connect_rid))
    end subroutine zsock_set_connect_rid

    ! void zsock_set_curve_publickey(void *self, const char *curve_publickey)
    subroutine zsock_set_curve_publickey(self, curve_publickey)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: curve_publickey

        call zsock_set_curve_publickey_(self, f_c_str(curve_publickey))
    end subroutine zsock_set_curve_publickey

    ! void zsock_set_curve_secretkey(void *self, const char *curve_secretkey)
    subroutine zsock_set_curve_secretkey(self, curve_secretkey)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: curve_secretkey

        call zsock_set_curve_secretkey_(self, f_c_str(curve_secretkey))
    end subroutine zsock_set_curve_secretkey

    ! void zsock_set_curve_serverkey(void *self, const char *curve_serverkey)
    subroutine zsock_set_curve_serverkey(self, curve_serverkey)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: curve_serverkey

        call zsock_set_curve_serverkey_(self, f_c_str(curve_serverkey))
    end subroutine zsock_set_curve_serverkey

    ! void zsock_set_gssapi_principal(void *self, const char *gssapi_principal)
    subroutine zsock_set_gssapi_principal(self, gssapi_principal)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: gssapi_principal

        call zsock_set_gssapi_principal_(self, f_c_str(gssapi_principal))
    end subroutine zsock_set_gssapi_principal

    ! void zsock_set_gssapi_service_principal(void *self, const char *gssapi_service_principal)
    subroutine zsock_set_gssapi_service_principal(self, gssapi_service_principal)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: gssapi_service_principal

        call zsock_set_gssapi_service_principal_(self, f_c_str(gssapi_service_principal))
    end subroutine zsock_set_gssapi_service_principal

    ! void zsock_set_identity(void *self, const char *identity)
    subroutine zsock_set_identity(self, identity)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: identity

        call zsock_set_identity_(self, f_c_str(identity))
    end subroutine zsock_set_identity

    ! void zsock_set_metadata(void *self, const char *metadata)
    subroutine zsock_set_metadata(self, metadata)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: metadata

        call zsock_set_metadata_(self, f_c_str(metadata))
    end subroutine zsock_set_metadata

    ! void zsock_set_plain_password(void *self, const char *plain_password)
    subroutine zsock_set_plain_password(self, plain_password)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: plain_password

        call zsock_set_plain_password_(self, f_c_str(plain_password))
    end subroutine zsock_set_plain_password

    ! void zsock_set_plain_username(void *self, const char *plain_username)
    subroutine zsock_set_plain_username(self, plain_username)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: plain_username

        call zsock_set_plain_username_(self, f_c_str(plain_username))
    end subroutine zsock_set_plain_username

    ! void zsock_set_socks_password(void *self, const char *socks_password)
    subroutine zsock_set_socks_password(self, socks_password)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: socks_password

        call zsock_set_socks_password_(self, f_c_str(socks_password))
    end subroutine zsock_set_socks_password

    ! void zsock_set_socks_proxy(void *self, const char *socks_proxy)
    subroutine zsock_set_socks_proxy(self, socks_proxy)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: socks_proxy

        call zsock_set_socks_proxy_(self, f_c_str(socks_proxy))
    end subroutine zsock_set_socks_proxy

    ! void zsock_set_socks_username(void *self, const char *socks_username)
    subroutine zsock_set_socks_username(self, socks_username)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: socks_username

        call zsock_set_socks_username_(self, f_c_str(socks_username))
    end subroutine zsock_set_socks_username

    ! void zsock_set_subscribe(void *self, const char *subscribe)
    subroutine zsock_set_subscribe(self, subscribe)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: subscribe

        call zsock_set_subscribe_(self, f_c_str(subscribe))
    end subroutine zsock_set_subscribe

    ! void zsock_set_tcp_accept_filter(void *self, const char *tcp_accept_filter)
    subroutine zsock_set_tcp_accept_filter(self, tcp_accept_filter)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: tcp_accept_filter

        call zsock_set_tcp_accept_filter_(self, f_c_str(tcp_accept_filter))
    end subroutine zsock_set_tcp_accept_filter

    ! void zsock_set_unsubscribe(void *self, const char *unsubscribe)
    subroutine zsock_set_unsubscribe(self, unsubscribe)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: unsubscribe

        call zsock_set_unsubscribe_(self, f_c_str(unsubscribe))
    end subroutine zsock_set_unsubscribe

    ! void zsock_set_wss_cert_pem(void *self, const char *wss_cert_pem)
    subroutine zsock_set_wss_cert_pem(self, wss_cert_pem)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: wss_cert_pem

        call zsock_set_wss_cert_pem_(self, f_c_str(wss_cert_pem))
    end subroutine zsock_set_wss_cert_pem

    ! void zsock_set_wss_hostname(void *self, const char *wss_hostname)
    subroutine zsock_set_wss_hostname(self, wss_hostname)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: wss_hostname

        call zsock_set_wss_hostname_(self, f_c_str(wss_hostname))
    end subroutine zsock_set_wss_hostname

    ! void zsock_set_wss_trust_pem(void *self, const char *wss_trust_pem)
    subroutine zsock_set_wss_trust_pem(self, wss_trust_pem)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: wss_trust_pem

        call zsock_set_wss_trust_pem_(self, f_c_str(wss_trust_pem))
    end subroutine zsock_set_wss_trust_pem

    ! void zsock_set_xpub_welcome_msg(void *self, const char *xpub_welcome_msg)
    subroutine zsock_set_xpub_welcome_msg(self, xpub_welcome_msg)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: xpub_welcome_msg

        call zsock_set_xpub_welcome_msg_(self, f_c_str(xpub_welcome_msg))
    end subroutine zsock_set_xpub_welcome_msg

    ! void zsock_set_zap_domain(void *self, const char *zap_domain)
    subroutine zsock_set_zap_domain(self, zap_domain)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: zap_domain

        call zsock_set_zap_domain_(self, f_c_str(zap_domain))
    end subroutine zsock_set_zap_domain

    ! char *zsock_socks_password(void *self)
    function zsock_socks_password(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_socks_password_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_socks_password

    ! char *zsock_socks_proxy(void *self)
    function zsock_socks_proxy(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_socks_proxy_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_socks_proxy

    ! char *zsock_socks_username(void *self)
    function zsock_socks_username(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_socks_username_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_socks_username

    ! char *zsock_tcp_accept_filter(void *self)
    function zsock_tcp_accept_filter(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_tcp_accept_filter_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_tcp_accept_filter

    ! const char *zsock_type_str(zsock_t *self)
    function zsock_type_str(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_type_str_(self)
        call c_f_str_ptr(ptr, str)
    end function zsock_type_str

    ! char *zsock_zap_domain(void *self)
    function zsock_zap_domain(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zsock_zap_domain_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zsock_zap_domain
end module czmq_zsock
