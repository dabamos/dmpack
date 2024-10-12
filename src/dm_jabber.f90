! Author:  Philipp Engel
! Licence: ISC
module dm_jabber
    !! Jabber/XMPP abstraction module, based on _libstrophe_.
    !!
    !! Link this module against `pkg-config --libs libstrophe expat openssl zlib`.
    !!
    !! The following example just connects to an XMPP server on port 5222 and
    !! sends a presence stanza:
    !!
    !! ```fortran
    !! integer                   :: rc     !! Return code.
    !! type(jabber_type), target :: jabber !! Jabber context type.
    !!
    !! call dm_jabber_init()
    !! call dm_jabber_create(jabber)
    !!
    !! rc = dm_jabber_connect(jabber     = jabber, &
    !!                        host       = 'example.com', &
    !!                        port       = JABBER_PORT, &
    !!                        jid        = 'user@example.com', &
    !!                        password   = 'secret', &
    !!                        callback   = connection_callback, &
    !!                        user_data  = c_loc(jabber), &
    !!                        keep_alive = .true.)
    !! if (dm_is_ok(rc)) call dm_jabber_run(jabber)
    !!
    !! call dm_jabber_disconnect(jabber)
    !! call dm_jabber_destroy(jabber)
    !! call dm_jabber_shutdown()
    !! ```
    !!
    !! The callback passed to the connect function must be of C-interoperable
    !! abstract interface `dm_jabber_connection_callback()`, which is an alias for
    !! `xmpp_conn_handler()` from module `xmpp`, for example:
    !!
    !! ```fortran
    !! subroutine connection_callback(connection, event, error, stream_error, user_data) bind(c)
    !!     use :: xmpp
    !!
    !!     type(c_ptr),               intent(in), value :: connection   !! xmpp_conn_t *
    !!     integer(kind=c_int),       intent(in), value :: event        !! xmpp_conn_event_t
    !!     integer(kind=c_int),       intent(in), value :: error        !! int
    !!     type(xmpp_stream_error_t), intent(in)        :: stream_error !! xmpp_stream_error_t *
    !!     type(c_ptr),               intent(in), value :: user_data    !! void *
    !!
    !!     integer                    :: stat
    !!     type(c_ptr)                :: presence
    !!     type(jabber_type), pointer :: jabber
    !!
    !!     if (.not. c_associated(user_data)) return
    !!     call c_f_pointer(user_data, jabber)
    !!
    !!     if (event == XMPP_CONN_CONNECT) then
    !!         print '("connected")'
    !!
    !!         presence = xmpp_presence_new(jabber%ctx)
    !!         call xmpp_send(connection, presence)
    !!         stat = xmpp_stanza_release(presence)
    !!     else
    !!         print '("disconnected")'
    !!         call dm_jabber_stop(jabber)
    !!     end if
    !! end subroutine connection_callback
    !! ```
    !!
    !! XMPP protocol handling is covered by module `xmpp` from library
    !! `vendor/fortran-xmpp`.
    use, intrinsic :: iso_c_binding
    use :: xmpp, dm_jabber_callback              => xmpp_handler,              &
                 dm_jabber_certfail_callback     => xmpp_certfail_handler,     &
                 dm_jabber_connection_callback   => xmpp_conn_handler,         &
                 dm_jabber_global_timed_callback => xmpp_global_timed_handler, &
                 dm_jabber_log_callback          => xmpp_log_handler,          &
                 dm_jabber_password_callback     => xmpp_password_callback,    &
                 dm_jabber_timed_callback        => xmpp_timed_handler
    use :: dm_error
    implicit none (type, external)
    private

    integer, parameter, public :: JABBER_HOST_LEN     = 256 !! Max. length of host.
    integer, parameter, public :: JABBER_JID_LEN      = 64  !! Max. length of Jabber id.
    integer, parameter, public :: JABBER_JID_FULL_LEN = 128 !! Max. length of JID with additional resource.
    integer, parameter, public :: JABBER_PASSWORD_LEN = 64  !! Max. length of password.
    integer, parameter, public :: JABBER_PING_ID_LEN  = 32  !! Max. length of ping id.

    integer, parameter, public :: JABBER_PORT     = 5222 !! Default XMPP port (StartTLS).
    integer, parameter, public :: JABBER_PORT_TLS = 5223 !! Secondary XMPP port (TLS).

    ! Log level of libstrophe.
    integer, parameter, public :: JABBER_LL_NONE    = -1 !! Logging disabled.
    integer, parameter, public :: JABBER_LL_DEBUG   = XMPP_LEVEL_DEBUG
    integer, parameter, public :: JABBER_LL_INFO    = XMPP_LEVEL_INFO
    integer, parameter, public :: JABBER_LL_WARNING = XMPP_LEVEL_WARN
    integer, parameter, public :: JABBER_LL_ERROR   = XMPP_LEVEL_ERROR

    ! Stanza names.
    character(len=*), parameter, public :: JABBER_STANZA_NAME_ACTOR               = 'actor'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_AFTER               = 'after'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_BEFORE              = 'before'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_BLOCK               = 'block'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_BLOCKLIST           = 'blocklist'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_BODY                = 'body'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_C                   = 'c'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_COMMAND             = 'command'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_CONFERENCE          = 'conference'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_CONFIGURE           = 'configure'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_CONTENT_TYPE        = 'content-type'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_DATA                = 'data'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_DELAY               = 'delay'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_DESTROY             = 'destroy'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_DISABLE             = 'disable'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_ENABLE              = 'enable'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_ERROR               = 'error'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_EVENT               = 'event'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_FEATURE             = 'feature'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_FIELD               = 'field'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_FILENAME            = 'filename'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_FIN                 = 'fin'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_FIRST               = 'first'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_GET                 = 'get'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_GROUP               = 'group'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_HEADER              = 'header'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_IDENTITY            = 'identity'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_INFO                = 'info'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_INVITE              = 'invite'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_IQ                  = 'iq'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_ITEM                = 'item'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_ITEMS               = 'items'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_LAST                = 'last'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_MAX                 = 'max'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_MESSAGE             = 'message'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_METADATA            = 'metadata'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_MINIMIZE            = 'minimize'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_MOOD                = 'mood'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_NICK                = 'nick'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_OPENPGP             = 'openpgp'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_ORIGIN_ID           = 'origin-id'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PASSWORD            = 'password'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PING                = 'ping'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PRESENCE            = 'presence'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PRIORITY            = 'priority'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PROPOSE             = 'propose'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PUBKEY_METADATA     = 'pubkey-metadata'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PUBLIC_KEYS_LIST    = 'public-keys-list'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PUBLISH             = 'publish'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PUBLISH_OPTIONS     = 'publish-options'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PUBSUB              = 'pubsub'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PUPKEY              = 'pubkey'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_PUT                 = 'put'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_QUERY               = 'query'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_REASON              = 'reason'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_RECEIVED            = 'received'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_REPORT              = 'report'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_REQUEST             = 'request'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_RESULT              = 'result'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_SENT                = 'sent'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_SERVICE_UNAVAILABLE = 'service-unavailable'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_SHOW                = 'show'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_SIZE                = 'size'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_SLOT                = 'slot'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_STANZA_ID           = 'stanza-id'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_STATUS              = 'status'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_STORAGE             = 'storage'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_SUBJECT             = 'subject'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_SUBSCRIBE           = 'subscribe'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_TEXT                = 'text'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_UNBLOCK             = 'unblock'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_URL                 = 'url'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_USERNAME            = 'username'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_VALUE               = 'value'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_VCARD               = 'vCard'
    character(len=*), parameter, public :: JABBER_STANZA_NAME_X                   = 'x'

    ! Stanze default name spaces.
    character(len=*), parameter, public :: JABBER_STANZA_NS_AUTH                = XMPP_NS_AUTH
    character(len=*), parameter, public :: JABBER_STANZA_NS_BIND                = XMPP_NS_BIND
    character(len=*), parameter, public :: JABBER_STANZA_NS_CLIENT              = XMPP_NS_CLIENT
    character(len=*), parameter, public :: JABBER_STANZA_NS_COMPONENT           = XMPP_NS_COMPONENT
    character(len=*), parameter, public :: JABBER_STANZA_NS_COMPRESSION         = XMPP_NS_COMPRESSION
    character(len=*), parameter, public :: JABBER_STANZA_NS_DISCO_INFO          = XMPP_NS_DISCO_INFO
    character(len=*), parameter, public :: JABBER_STANZA_NS_DISCO_ITEMS         = XMPP_NS_DISCO_ITEMS
    character(len=*), parameter, public :: JABBER_STANZA_NS_FEATURE_COMPRESSION = XMPP_NS_FEATURE_COMPRESSION
    character(len=*), parameter, public :: JABBER_STANZA_NS_PING                = 'urn:xmpp:ping'
    character(len=*), parameter, public :: JABBER_STANZA_NS_REGISTER            = XMPP_NS_REGISTER
    character(len=*), parameter, public :: JABBER_STANZA_NS_ROSTER              = XMPP_NS_ROSTER
    character(len=*), parameter, public :: JABBER_STANZA_NS_SASL                = XMPP_NS_SASL
    character(len=*), parameter, public :: JABBER_STANZA_NS_SESSION             = XMPP_NS_SESSION
    character(len=*), parameter, public :: JABBER_STANZA_NS_SM                  = XMPP_NS_SM
    character(len=*), parameter, public :: JABBER_STANZA_NS_STANZAS_IETF        = XMPP_NS_STANZAS_IETF
    character(len=*), parameter, public :: JABBER_STANZA_NS_STREAMS             = XMPP_NS_STREAMS
    character(len=*), parameter, public :: JABBER_STANZA_NS_STREAMS_IETF        = XMPP_NS_STREAMS_IETF
    character(len=*), parameter, public :: JABBER_STANZA_NS_TLS                 = XMPP_NS_TLS

    ! Stanza default attributes.
    character(len=*), parameter, public :: JABBER_STANZA_ATTR_JID = 'jid'

    ! Stanza default texts.
    character(len=*), parameter, public :: JABBER_STANZA_TEXT_AWAY   = 'away'
    character(len=*), parameter, public :: JABBER_STANZA_TEXT_CHAT   = 'chat'
    character(len=*), parameter, public :: JABBER_STANZA_TEXT_DND    = 'dnd'
    character(len=*), parameter, public :: JABBER_STANZA_TEXT_ONLINE = 'online'
    character(len=*), parameter, public :: JABBER_STANZA_TEXT_XA     = 'xa'

    ! Stanza default types.
    character(len=*), parameter, public :: JABBER_STANZA_TYPE_CANCEL      = 'cancel'
    character(len=*), parameter, public :: JABBER_STANZA_TYPE_CHAT        = 'chat'
    character(len=*), parameter, public :: JABBER_STANZA_TYPE_ERROR       = 'error'
    character(len=*), parameter, public :: JABBER_STANZA_TYPE_GET         = 'get'
    character(len=*), parameter, public :: JABBER_STANZA_TYPE_MODIFY      = 'modify'
    character(len=*), parameter, public :: JABBER_STANZA_TYPE_NORMAL      = 'normal'
    character(len=*), parameter, public :: JABBER_STANZA_TYPE_RESULT      = 'result'
    character(len=*), parameter, public :: JABBER_STANZA_TYPE_SET         = 'set'
    character(len=*), parameter, public :: JABBER_STANZA_TYPE_SUBMIT      = 'submit'
    character(len=*), parameter, public :: JABBER_STANZA_TYPE_UNAVAILABLE = 'unavailable'

    type, public :: jabber_type
        !! Jabber/XMPP context type.
        type(c_ptr)                        :: ctx        = c_null_ptr  !! libstrophe context.
        type(c_ptr)                        :: connection = c_null_ptr  !! libstrophe connection.
        type(c_ptr)                        :: sm_state   = c_null_ptr  !! libstrophe stream management state.
        character(len=JABBER_HOST_LEN)     :: host       = ' '         !! XMPP server host.
        integer                            :: port       = JABBER_PORT !! XMPP server port.
        character(len=JABBER_JID_LEN)      :: jid        = ' '         !! XMPP id of account.
        character(len=JABBER_JID_FULL_LEN) :: jid_full   = ' '         !! XMPP id with resource.
        character(len=JABBER_PASSWORD_LEN) :: password   = ' '         !! XMPP password of account.
    end type jabber_type

    type, public :: roster_type
        !! Jabber roster type.
        character(len=JABBER_JID_LEN), allocatable :: jids(:) !! JID array.
    end type roster_type

    ! Imported abstract interfaces.
    public :: dm_jabber_callback
    public :: dm_jabber_certfail_callback
    public :: dm_jabber_connection_callback
    public :: dm_jabber_global_timed_callback
    public :: dm_jabber_log_callback
    public :: dm_jabber_password_callback
    public :: dm_jabber_timed_callback

    ! Public procedures.
    public :: dm_jabber_connect
    public :: dm_jabber_create
    public :: dm_jabber_create_iq_error
    public :: dm_jabber_create_iq_ping
    public :: dm_jabber_create_iq_result
    public :: dm_jabber_create_iq_roster
    public :: dm_jabber_destroy
    public :: dm_jabber_disconnect
    public :: dm_jabber_init
    public :: dm_jabber_is_connected
    public :: dm_jabber_preserve_stream_management_state
    public :: dm_jabber_roster_add
    public :: dm_jabber_roster_has
    public :: dm_jabber_run
    public :: dm_jabber_send_presence
    public :: dm_jabber_send_stanza
    public :: dm_jabber_shutdown
    public :: dm_jabber_stop
contains
    ! ******************************************************************
    ! PUBLIC FUNCTIONS.
    ! ******************************************************************
    integer function dm_jabber_connect(jabber, host, port, jid, password, callback, user_data, &
                                       resource, keep_alive, tls_required, tls_trusted) result(rc)
        !! Connects to XMPP server.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if `host`, `jid`, or `password` are empty, or `port` is invalid.
        !! * `E_IO` if the connection attempt failed.
        !! * `E_NULL` if the XMPP context is not associated.
        !! * `E_XMPP` if a connection context could not be created.
        !!
        type(jabber_type), intent(inout)         :: jabber       !! Jabber context type.
        character(len=*),  intent(in)            :: host         !! XMPP server (IP address or FQDN).
        integer,           intent(in)            :: port         !! XMPP server port.
        character(len=*),  intent(in)            :: jid          !! Jabber ID (JID).
        character(len=*),  intent(in)            :: password     !! JID account password.
        procedure(dm_jabber_connection_callback) :: callback     !! Jabber connection handler.
        type(c_ptr),       intent(in), optional  :: user_data    !! C pointer to user data.
        character(len=*),  intent(in), optional  :: resource     !! Optional resource (`<jid>@<domain>/<resource>`).
        logical,           intent(in), optional  :: keep_alive   !! Enable TCP Keep Alive.
        logical,           intent(in), optional  :: tls_required !! TLS is mandatory.
        logical,           intent(in), optional  :: tls_trusted  !! Trust TLS certificate.

        integer              :: stat
        integer(kind=c_long) :: flags
        logical              :: keep_alive_, tls_required_, tls_trusted_
        type(c_ptr)          :: user_data_

        rc = E_NULL
        if (.not. c_associated(jabber%ctx)) return

        rc = E_INVALID
        if (len_trim(host) == 0) return
        if (port <= 0) return
        if (len_trim(jid) == 0 .or. len_trim(password) == 0) return

        ! Optional arguments.
        keep_alive_   = .false.
        tls_required_ = .false.
        tls_trusted_  = .false.

        if (present(keep_alive))   keep_alive_   = keep_alive
        if (present(tls_required)) tls_required_ = tls_required
        if (present(tls_trusted))  tls_trusted_  = tls_trusted

        ! Create new connection.
        rc = E_XMPP
        jabber%connection = xmpp_conn_new(jabber%ctx)
        if (.not. c_associated(jabber%connection)) return

        ! Set flags.
        flags = 0
        if (tls_required_) flags = ior(flags, XMPP_CONN_FLAG_MANDATORY_TLS)
        if (tls_trusted_)  flags = ior(flags, XMPP_CONN_FLAG_TRUST_TLS)

        stat = xmpp_conn_set_flags(jabber%connection, flags)
        if (stat /= XMPP_EOK) return

        ! Set context data.
        jabber%host     = host
        jabber%port     = port
        jabber%jid      = jid
        jabber%jid_full = jid
        jabber%password = password

        if (present(resource)) jabber%jid_full = trim(jabber%jid_full) // '/' // resource

        ! Set credentials.
        call xmpp_conn_set_jid (jabber%connection, trim(jabber%jid_full))
        call xmpp_conn_set_pass(jabber%connection, trim(password))

        ! Set TCP Keep Alive using default callback.
        if (keep_alive_) call xmpp_conn_set_sockopt_callback(jabber%connection, xmpp_sockopt_cb_keepalive)

        ! Set stream management state if available.
        if (c_associated(jabber%sm_state)) then
            stat = xmpp_conn_set_sm_state(jabber%connection, jabber%sm_state)
            jabber%sm_state = c_null_ptr
        end if

        ! C pointer to user data.
        user_data_ = c_null_ptr
        if (present(user_data)) user_data_ = user_data

        rc = E_IO
        stat = xmpp_connect_client(conn       = jabber%connection, &
                                   alt_domain = trim(host), &
                                   alt_port   = port, &
                                   callback   = callback, &
                                   user_data  = user_data_)
        if (stat /= XMPP_EOK) return

        rc = E_NONE
    end function dm_jabber_connect

    integer function dm_jabber_create(jabber, log_level) result(rc)
        !! Creates Jabber context. Logging to standard error is disabled by
        !! default or if `log_level` is `JABBER_LL_NONE`. Make sure that argument
        !! `jabber` is not created already.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if `log_level` is invalid.
        !! * `E_XMPP` if the XMPP context could not be created.
        !!
        type(jabber_type), intent(out)          :: jabber    !! Jabber context type.
        integer,           intent(in), optional :: log_level !! Log level of libstrophe (`JABBER_LL_*`).

        integer     :: ll
        type(c_ptr) :: log

        rc = E_INVALID
        log = c_null_ptr

        ! Set log level.
        ll = JABBER_LL_NONE
        if (present(log_level)) ll = log_level
        if (ll < JABBER_LL_NONE .or. ll > JABBER_LL_ERROR) return

        ! Enable logging.
        if (ll /= JABBER_LL_NONE) log = xmpp_get_default_logger(ll)

        ! Create libstrophe context.
        rc = E_XMPP
        jabber%ctx = xmpp_ctx_new(c_null_ptr, log)
        if (.not. c_associated(jabber%ctx)) return

        rc = E_NONE
    end function dm_jabber_create

    type(c_ptr) function dm_jabber_create_iq_error(jabber, id, type, condition) result(iq_stanza)
        !! Returns C pointer to new error iq stanza.
        type(jabber_type), intent(inout)        :: jabber    !! Jabber context type.
        character(len=*),  intent(in)           :: id        !! Stanza id.
        character(len=*),  intent(in)           :: type      !! Stanza type.
        character(len=*),  intent(in), optional :: condition !! Condition stanza name.

        integer     :: stat
        type(c_ptr) :: condition_stanza, error_stanza

        iq_stanza    = xmpp_iq_new(jabber%ctx, JABBER_STANZA_TYPE_ERROR, id)
        error_stanza = xmpp_stanza_new(jabber%ctx)

        stat = xmpp_stanza_set_name(iq_stanza, JABBER_STANZA_NAME_ERROR)
        stat = xmpp_stanza_set_type(error_stanza, type)

        if (present(condition)) then
            condition_stanza = xmpp_stanza_new(jabber%ctx)
            stat = xmpp_stanza_set_name(condition_stanza, condition)
            stat = xmpp_stanza_set_ns(condition_stanza, JABBER_STANZA_NS_STANZAS_IETF)
            stat = xmpp_stanza_add_child(error_stanza, condition_stanza)
        end if

        stat = xmpp_stanza_add_child(iq_stanza, error_stanza)
        if (present(condition)) stat = xmpp_stanza_release(condition_stanza)
        stat = xmpp_stanza_release(error_stanza)
    end function dm_jabber_create_iq_error

    type(c_ptr) function dm_jabber_create_iq_ping(jabber, id, to) result(iq_stanza)
        !! Returns C pointer to new ping iq stanza.
        type(jabber_type), intent(inout)        :: jabber !! Jabber context type.
        character(len=*),  intent(in)           :: id     !! Stanza id.
        character(len=*),  intent(in), optional :: to     !! Target.

        integer     :: stat
        type(c_ptr) :: ping_stanza

        iq_stanza   = xmpp_iq_new(jabber%ctx, JABBER_STANZA_TYPE_GET, id)
        ping_stanza = xmpp_stanza_new(jabber%ctx)

        if (present(to)) stat = xmpp_stanza_set_to(iq_stanza, to)

        stat = xmpp_stanza_set_name(ping_stanza, JABBER_STANZA_NAME_PING)
        stat = xmpp_stanza_set_ns(ping_stanza, JABBER_STANZA_NS_PING)
        stat = xmpp_stanza_add_child(iq_stanza, ping_stanza)
        stat = xmpp_stanza_release(ping_stanza)
    end function dm_jabber_create_iq_ping

    type(c_ptr) function dm_jabber_create_iq_result(jabber, id) result(iq_stanza)
        !! Returns C pointer to new result iq stanza.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.
        character(len=*),  intent(in)    :: id     !! Stanza id.

        iq_stanza = xmpp_iq_new(jabber%ctx, JABBER_STANZA_TYPE_RESULT, id)
    end function dm_jabber_create_iq_result

    type(c_ptr) function dm_jabber_create_iq_roster(jabber, id) result(iq_stanza)
        !! Returns C pointer to new roster iq stanza.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.
        character(len=*),  intent(in)    :: id     !! Stanza id.

        integer     :: stat
        type(c_ptr) :: query_stanza

        iq_stanza    = xmpp_iq_new(jabber%ctx, JABBER_STANZA_TYPE_GET, id)
        query_stanza = xmpp_stanza_new(jabber%ctx)

        stat = xmpp_stanza_set_name(query_stanza, JABBER_STANZA_NAME_QUERY)
        stat = xmpp_stanza_set_ns(query_stanza, JABBER_STANZA_NS_ROSTER)
        stat = xmpp_stanza_add_child(iq_stanza, query_stanza)
        stat = xmpp_stanza_release(query_stanza)
    end function dm_jabber_create_iq_roster

    logical function dm_jabber_is_connected(jabber) result(is)
        !! Returns `.true.` if connection is open.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        is = c_associated(jabber%connection)
        if (is) is = (xmpp_conn_is_connected(jabber%connection) == 1)
    end function dm_jabber_is_connected

    integer function dm_jabber_roster_add(roster, jid) result(rc)
        type(roster_type), intent(inout) :: roster !! Roster type.
        character(len=*),  intent(in)    :: jid    !! JID to add.

        integer                                    :: n, stat
        character(len=JABBER_JID_LEN), allocatable :: buffer(:)

        rc = E_BOUNDS
        if (len_trim(jid) > JABBER_JID_LEN) return

        rc = E_ALLOC
        if (.not. allocated(roster%jids)) then
            allocate (roster%jids(0), stat=stat)
            if (stat /= 0) return
        end if

        n = size(roster%jids)

        allocate (buffer(n + 1), stat=stat)
        if (stat /= 0) return

        buffer(:n)    = roster%jids
        buffer(n + 1) = jid
        call move_alloc(buffer, roster%jids)

        rc = E_NONE
    end function dm_jabber_roster_add

    logical function dm_jabber_roster_has(roster, jid) result(has)
        !! Returns `.true.` if roster contains JID. Any resource appended to
        !! the JID is ignored.
        type(roster_type), intent(inout) :: roster !! Roster type.
        character(len=*),  intent(in)    :: jid    !! JID to search.

        integer :: i, n

        has = .false.
        if (.not. allocated(roster%jids)) return

        do i = 1, size(roster%jids)
            n = len_trim(roster%jids(i))
            if (jid(:n) /= roster%jids(i)) cycle
            has = .true.
            exit
        end do
    end function dm_jabber_roster_has

    ! ******************************************************************
    ! PUBLIC SUBROUTINES.
    ! ******************************************************************
    subroutine dm_jabber_destroy(jabber)
        !! Destroys XMPP context and an closes the connection if still open.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        if (dm_jabber_is_connected(jabber)) call dm_jabber_disconnect(jabber)
        call xmpp_ctx_free(jabber%ctx)
    end subroutine dm_jabber_destroy

    subroutine dm_jabber_disconnect(jabber)
        !! Disconnects from server and releases connection.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        integer :: stat

        if (.not. dm_jabber_is_connected(jabber)) return
        call xmpp_disconnect(jabber%connection)
        stat = xmpp_conn_release(jabber%connection)
    end subroutine dm_jabber_disconnect

    subroutine dm_jabber_init()
        !! Initialises XMPP backend (libstrophe).
        call xmpp_initialize()
    end subroutine dm_jabber_init

    subroutine dm_jabber_preserve_stream_management_state(jabber)
        !! Saves current XMPP stream management state to Jabber context.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        jabber%sm_state = xmpp_conn_get_sm_state(jabber%connection)
    end subroutine dm_jabber_preserve_stream_management_state

    subroutine dm_jabber_run(jabber)
        !! Starts XMPP event loop.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        if (.not. c_associated(jabber%ctx)) return
        call xmpp_run(jabber%ctx)
    end subroutine dm_jabber_run

    subroutine dm_jabber_send_presence(jabber, show, status)
        !! Sends presence to XMPP server.
        use :: dm_string

        type(jabber_type), intent(inout)        :: jabber !! Jabber context type.
        character(len=*),  intent(in), optional :: show   !! Availability (`online`, `offline`, `dnd`, …).
        character(len=*),  intent(in), optional :: status !! Human-readable status description text.

        integer     :: stat
        type(c_ptr) :: attr, pres, text

        if (.not. dm_jabber_is_connected(jabber)) return

        pres = xmpp_presence_new(jabber%ctx)

        if (dm_string_is_present(show)) then
            attr = xmpp_stanza_new(jabber%ctx)
            stat = xmpp_stanza_set_name(attr, JABBER_STANZA_NAME_SHOW)
            text = xmpp_stanza_new(jabber%ctx)
            stat = xmpp_stanza_set_text(text, trim(show))
            stat = xmpp_stanza_add_child(attr, text)
            stat = xmpp_stanza_add_child(pres, attr)
            stat = xmpp_stanza_release(text)
            stat = xmpp_stanza_release(attr)
        end if

        if (dm_string_is_present(status)) then
            attr = xmpp_stanza_new(jabber%ctx)
            stat = xmpp_stanza_set_name(attr, JABBER_STANZA_NAME_STATUS)
            text = xmpp_stanza_new(jabber%ctx)
            stat = xmpp_stanza_set_text(text, trim(status))
            stat = xmpp_stanza_add_child(attr, text)
            stat = xmpp_stanza_add_child(pres, attr)
            stat = xmpp_stanza_release(text)
            stat = xmpp_stanza_release(attr)
        end if

        call xmpp_send(jabber%connection, pres)
        stat = xmpp_stanza_release(pres)
    end subroutine dm_jabber_send_presence

    subroutine dm_jabber_send_stanza(jabber, stanza, release)
        type(jabber_type), intent(inout)        :: jabber  !! Jabber context type.
        type(c_ptr),       intent(in)           :: stanza  !! Stanza to send.
        logical,           intent(in), optional :: release !! Release stanza afterwards.

        integer :: stat
        logical :: release_

        release_ = .true.
        if (present(release)) release_ = release

        call xmpp_send(jabber%connection, stanza)
        if (release_) stat = xmpp_stanza_release(stanza)
    end subroutine dm_jabber_send_stanza

    subroutine dm_jabber_shutdown()
        !! Shuts down XMPP backend (libstrophe).
        call xmpp_shutdown()
    end subroutine dm_jabber_shutdown

    subroutine dm_jabber_stop(jabber)
        !! Stops XMPP context (libstrophe).
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        call xmpp_stop(jabber%ctx)
    end subroutine dm_jabber_stop
end module dm_jabber
