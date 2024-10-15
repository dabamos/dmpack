! Author:  Philipp Engel
! Licence: ISC
module dm_im
    !! Instant messaging module for Jabber/XMPP connectivity, based on
    !! _libstrophe_.
    !!
    !! Link this module against `pkg-config --libs libstrophe expat openssl zlib`.
    !!
    !! The following example just connects to an XMPP server on port 5222 and
    !! sends a presence stanza:
    !!
    !! ```fortran
    !! integer               :: rc !! Return code.
    !! type(im_type), target :: im !! IM context type.
    !!
    !! call dm_im_init()
    !! call dm_im_create(im)
    !!
    !! rc = dm_im_connect(im         = im, &
    !!                    host       = 'example.com', &
    !!                    port       = IM_PORT, &
    !!                    jid        = 'user@example.com', &
    !!                    password   = 'secret', &
    !!                    callback   = connection_callback, &
    !!                    user_data  = c_loc(im), &
    !!                    keep_alive = .true.)
    !! if (dm_is_ok(rc)) call dm_im_run(im)
    !!
    !! call dm_im_destroy(im)
    !! call dm_im_shutdown()
    !! ```
    !!
    !! The callback passed to the connect function must be of C-interoperable
    !! abstract interface `dm_im_connection_callback()`, which is an alias for
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
    !!     type(im_type), pointer :: im
    !!
    !!     if (.not. c_associated(user_data)) return
    !!     call c_f_pointer(user_data, im)
    !!
    !!     if (event == XMPP_CONN_CONNECT) then
    !!         call dm_im_send_presence(im, IM_STANZA_TEXT_ONLINE)
    !!     else
    !!         call dm_im_stop(im)
    !!     end if
    !! end subroutine connection_callback
    !! ```
    !!
    !! XMPP protocol handling is covered by module `xmpp` from library
    !! `vendor/fortran-xmpp`.
    use, intrinsic :: iso_c_binding
    use :: xmpp, dm_im_callback              => xmpp_handler,              &
                 dm_im_certfail_callback     => xmpp_certfail_handler,     &
                 dm_im_connection_callback   => xmpp_conn_handler,         &
                 dm_im_global_timed_callback => xmpp_global_timed_handler, &
                 dm_im_log_callback          => xmpp_log_handler,          &
                 dm_im_password_callback     => xmpp_password_callback,    &
                 dm_im_timed_callback        => xmpp_timed_handler
    use :: dm_error
    use :: dm_file
    use :: dm_id
    use :: dm_kind
    use :: dm_mime
    use :: dm_sem
    use :: dm_uuid
    implicit none (type, external)
    private

    integer, parameter, public :: IM_HOST_LEN     = 255    !! Max. length of host (XEP-0029).
    integer, parameter, public :: IM_JID_LEN      = 256    !! Max. length of Jabber id (XEP-0029).
    integer, parameter, public :: IM_JID_FULL_LEN = 768    !! Max. length of JID with additional resource (XEP-0029).
    integer, parameter, public :: IM_PASSWORD_LEN = 256    !! Max. length of password.
    integer, parameter, public :: IM_ID_LEN       = ID_LEN !! Max. length of id.
    integer, parameter, public :: IM_URL_LEN      = 2048   !! Max. length of URL.

    integer, parameter, public :: IM_PORT     = 5222 !! Default XMPP port (StartTLS).
    integer, parameter, public :: IM_PORT_TLS = 5223 !! Secondary XMPP port (TLS).

    ! Log level of libstrophe.
    integer, parameter, public :: IM_LL_NONE    = -1 !! Logging disabled.
    integer, parameter, public :: IM_LL_DEBUG   = XMPP_LEVEL_DEBUG
    integer, parameter, public :: IM_LL_INFO    = XMPP_LEVEL_INFO
    integer, parameter, public :: IM_LL_WARNING = XMPP_LEVEL_WARN
    integer, parameter, public :: IM_LL_ERROR   = XMPP_LEVEL_ERROR

    ! Stanza default attributes.
    character(len=*), parameter, public :: IM_STANZA_ATTR_ASK            = 'ask'
    character(len=*), parameter, public :: IM_STANZA_ATTR_AUTOJOIN       = 'autojoin'
    character(len=*), parameter, public :: IM_STANZA_ATTR_CATEGORY       = 'category'
    character(len=*), parameter, public :: IM_STANZA_ATTR_CODE           = 'code'
    character(len=*), parameter, public :: IM_STANZA_ATTR_CONTENT_TYPE   = 'content-type'
    character(len=*), parameter, public :: IM_STANZA_ATTR_DATE           = 'date'
    character(len=*), parameter, public :: IM_STANZA_ATTR_FILENAME       = 'filename'
    character(len=*), parameter, public :: IM_STANZA_ATTR_FROM           = 'from'
    character(len=*), parameter, public :: IM_STANZA_ATTR_HASH           = 'hash'
    character(len=*), parameter, public :: IM_STANZA_ATTR_ID             = 'id'
    character(len=*), parameter, public :: IM_STANZA_ATTR_JID            = 'jid'
    character(len=*), parameter, public :: IM_STANZA_ATTR_LABEL          = 'label'
    character(len=*), parameter, public :: IM_STANZA_ATTR_NAME           = 'name'
    character(len=*), parameter, public :: IM_STANZA_ATTR_NICK           = 'nick'
    character(len=*), parameter, public :: IM_STANZA_ATTR_NODE           = 'node'
    character(len=*), parameter, public :: IM_STANZA_ATTR_PASSWORD       = 'password'
    character(len=*), parameter, public :: IM_STANZA_ATTR_REASON         = 'reason'
    character(len=*), parameter, public :: IM_STANZA_ATTR_SECONDS        = 'seconds'
    character(len=*), parameter, public :: IM_STANZA_ATTR_SIZE           = 'size'
    character(len=*), parameter, public :: IM_STANZA_ATTR_STAMP          = 'stamp'
    character(len=*), parameter, public :: IM_STANZA_ATTR_STATUS         = 'status'
    character(len=*), parameter, public :: IM_STANZA_ATTR_SUBSCRIPTION   = 'subscription'
    character(len=*), parameter, public :: IM_STANZA_ATTR_TO             = 'to'
    character(len=*), parameter, public :: IM_STANZA_ATTR_TYPE           = 'type'
    character(len=*), parameter, public :: IM_STANZA_ATTR_URL            = 'url'
    character(len=*), parameter, public :: IM_STANZA_ATTR_V4_FINGERPRINT = 'v4-fingerprint'
    character(len=*), parameter, public :: IM_STANZA_ATTR_VAR            = 'var'
    character(len=*), parameter, public :: IM_STANZA_ATTR_VER            = 'ver'
    character(len=*), parameter, public :: IM_STANZA_ATTR_XMLNS          = 'xmlns'

    ! Stanza default headers.
    character(len=*), parameter, public :: IM_STANZA_HEADER_AUTHORIZATION = 'Authorization'
    character(len=*), parameter, public :: IM_STANZA_HEADER_COOKIE        = 'Cookie'
    character(len=*), parameter, public :: IM_STANZA_HEADER_EXPIRES       = 'Expires'

    ! Stanza default names.
    character(len=*), parameter, public :: IM_STANZA_NAME_ACTOR               = 'actor'
    character(len=*), parameter, public :: IM_STANZA_NAME_AFTER               = 'after'
    character(len=*), parameter, public :: IM_STANZA_NAME_BEFORE              = 'before'
    character(len=*), parameter, public :: IM_STANZA_NAME_BLOCK               = 'block'
    character(len=*), parameter, public :: IM_STANZA_NAME_BLOCKLIST           = 'blocklist'
    character(len=*), parameter, public :: IM_STANZA_NAME_BODY                = 'body'
    character(len=*), parameter, public :: IM_STANZA_NAME_C                   = 'c'
    character(len=*), parameter, public :: IM_STANZA_NAME_COMMAND             = 'command'
    character(len=*), parameter, public :: IM_STANZA_NAME_CONFERENCE          = 'conference'
    character(len=*), parameter, public :: IM_STANZA_NAME_CONFIGURE           = 'configure'
    character(len=*), parameter, public :: IM_STANZA_NAME_CONTENT_TYPE        = 'content-type'
    character(len=*), parameter, public :: IM_STANZA_NAME_DATA                = 'data'
    character(len=*), parameter, public :: IM_STANZA_NAME_DELAY               = 'delay'
    character(len=*), parameter, public :: IM_STANZA_NAME_DESTROY             = 'destroy'
    character(len=*), parameter, public :: IM_STANZA_NAME_DISABLE             = 'disable'
    character(len=*), parameter, public :: IM_STANZA_NAME_ENABLE              = 'enable'
    character(len=*), parameter, public :: IM_STANZA_NAME_ERROR               = 'error'
    character(len=*), parameter, public :: IM_STANZA_NAME_EVENT               = 'event'
    character(len=*), parameter, public :: IM_STANZA_NAME_FEATURE             = 'feature'
    character(len=*), parameter, public :: IM_STANZA_NAME_FIELD               = 'field'
    character(len=*), parameter, public :: IM_STANZA_NAME_FILENAME            = 'filename'
    character(len=*), parameter, public :: IM_STANZA_NAME_FIN                 = 'fin'
    character(len=*), parameter, public :: IM_STANZA_NAME_FIRST               = 'first'
    character(len=*), parameter, public :: IM_STANZA_NAME_GET                 = 'get'
    character(len=*), parameter, public :: IM_STANZA_NAME_GROUP               = 'group'
    character(len=*), parameter, public :: IM_STANZA_NAME_HEADER              = 'header'
    character(len=*), parameter, public :: IM_STANZA_NAME_IDENTITY            = 'identity'
    character(len=*), parameter, public :: IM_STANZA_NAME_INFO                = 'info'
    character(len=*), parameter, public :: IM_STANZA_NAME_INVITE              = 'invite'
    character(len=*), parameter, public :: IM_STANZA_NAME_IQ                  = 'iq'
    character(len=*), parameter, public :: IM_STANZA_NAME_ITEM                = 'item'
    character(len=*), parameter, public :: IM_STANZA_NAME_ITEMS               = 'items'
    character(len=*), parameter, public :: IM_STANZA_NAME_LAST                = 'last'
    character(len=*), parameter, public :: IM_STANZA_NAME_MAX                 = 'max'
    character(len=*), parameter, public :: IM_STANZA_NAME_MESSAGE             = 'message'
    character(len=*), parameter, public :: IM_STANZA_NAME_METADATA            = 'metadata'
    character(len=*), parameter, public :: IM_STANZA_NAME_MINIMIZE            = 'minimize'
    character(len=*), parameter, public :: IM_STANZA_NAME_MOOD                = 'mood'
    character(len=*), parameter, public :: IM_STANZA_NAME_NICK                = 'nick'
    character(len=*), parameter, public :: IM_STANZA_NAME_OPENPGP             = 'openpgp'
    character(len=*), parameter, public :: IM_STANZA_NAME_ORIGIN_ID           = 'origin-id'
    character(len=*), parameter, public :: IM_STANZA_NAME_PASSWORD            = 'password'
    character(len=*), parameter, public :: IM_STANZA_NAME_PING                = 'ping'
    character(len=*), parameter, public :: IM_STANZA_NAME_PRESENCE            = 'presence'
    character(len=*), parameter, public :: IM_STANZA_NAME_PRIORITY            = 'priority'
    character(len=*), parameter, public :: IM_STANZA_NAME_PROPOSE             = 'propose'
    character(len=*), parameter, public :: IM_STANZA_NAME_PUBKEY_METADATA     = 'pubkey-metadata'
    character(len=*), parameter, public :: IM_STANZA_NAME_PUBLIC_KEYS_LIST    = 'public-keys-list'
    character(len=*), parameter, public :: IM_STANZA_NAME_PUBLISH             = 'publish'
    character(len=*), parameter, public :: IM_STANZA_NAME_PUBLISH_OPTIONS     = 'publish-options'
    character(len=*), parameter, public :: IM_STANZA_NAME_PUBSUB              = 'pubsub'
    character(len=*), parameter, public :: IM_STANZA_NAME_PUPKEY              = 'pubkey'
    character(len=*), parameter, public :: IM_STANZA_NAME_PUT                 = 'put'
    character(len=*), parameter, public :: IM_STANZA_NAME_QUERY               = 'query'
    character(len=*), parameter, public :: IM_STANZA_NAME_REASON              = 'reason'
    character(len=*), parameter, public :: IM_STANZA_NAME_RECEIVED            = 'received'
    character(len=*), parameter, public :: IM_STANZA_NAME_REPORT              = 'report'
    character(len=*), parameter, public :: IM_STANZA_NAME_REQUEST             = 'request'
    character(len=*), parameter, public :: IM_STANZA_NAME_RESULT              = 'result'
    character(len=*), parameter, public :: IM_STANZA_NAME_SENT                = 'sent'
    character(len=*), parameter, public :: IM_STANZA_NAME_SHOW                = 'show'
    character(len=*), parameter, public :: IM_STANZA_NAME_SIZE                = 'size'
    character(len=*), parameter, public :: IM_STANZA_NAME_SLOT                = 'slot'
    character(len=*), parameter, public :: IM_STANZA_NAME_STANZA_ID           = 'stanza-id'
    character(len=*), parameter, public :: IM_STANZA_NAME_STATUS              = 'status'
    character(len=*), parameter, public :: IM_STANZA_NAME_STORAGE             = 'storage'
    character(len=*), parameter, public :: IM_STANZA_NAME_SUBJECT             = 'subject'
    character(len=*), parameter, public :: IM_STANZA_NAME_SUBSCRIBE           = 'subscribe'
    character(len=*), parameter, public :: IM_STANZA_NAME_TEXT                = 'text'
    character(len=*), parameter, public :: IM_STANZA_NAME_UNBLOCK             = 'unblock'
    character(len=*), parameter, public :: IM_STANZA_NAME_URL                 = 'url'
    character(len=*), parameter, public :: IM_STANZA_NAME_USERNAME            = 'username'
    character(len=*), parameter, public :: IM_STANZA_NAME_VALUE               = 'value'
    character(len=*), parameter, public :: IM_STANZA_NAME_VCARD               = 'vCard'
    character(len=*), parameter, public :: IM_STANZA_NAME_X                   = 'x'

    ! Stanza default error conditions.
    character(len=*), parameter, public :: IM_STANZA_NAME_BAD_REQUEST             = 'bad-request'
    character(len=*), parameter, public :: IM_STANZA_NAME_CONFLICT                = 'conflict'
    character(len=*), parameter, public :: IM_STANZA_NAME_FEATURE_NOT_IMPLEMENTED = 'feature-not-implemented'
    character(len=*), parameter, public :: IM_STANZA_NAME_FORBIDDEN               = 'forbidden'
    character(len=*), parameter, public :: IM_STANZA_NAME_GONE                    = 'gone'
    character(len=*), parameter, public :: IM_STANZA_NAME_INTERNAL_SERVER_ERROR   = 'internal-server-error'
    character(len=*), parameter, public :: IM_STANZA_NAME_ITEM_NOT_FOUND          = 'item-not-found'
    character(len=*), parameter, public :: IM_STANZA_NAME_JID_MALFORMED           = 'jid-malformed'
    character(len=*), parameter, public :: IM_STANZA_NAME_NOT_ACCEPTABLE          = 'not-acceptable'
    character(len=*), parameter, public :: IM_STANZA_NAME_NOT_ALLOWED             = 'not-allowed'
    character(len=*), parameter, public :: IM_STANZA_NAME_NOT_AUTHORISED          = 'not-authorised'
    character(len=*), parameter, public :: IM_STANZA_NAME_POLICY_VIOLATION        = 'policy-violation'
    character(len=*), parameter, public :: IM_STANZA_NAME_RECIPIENT_UNAVAILABLE   = 'recipient-unavailable'
    character(len=*), parameter, public :: IM_STANZA_NAME_REDIRECT                = 'redirect'
    character(len=*), parameter, public :: IM_STANZA_NAME_REGISTRATION_REQUIRED   = 'registration-required'
    character(len=*), parameter, public :: IM_STANZA_NAME_REMOTE_SERVER_NOT_FOUND = 'remote-server-not-found'
    character(len=*), parameter, public :: IM_STANZA_NAME_REMOTE_SERVER_TIMEOUT   = 'remote-server-timeout'
    character(len=*), parameter, public :: IM_STANZA_NAME_RESOURCE_CONSTRAINT     = 'resource-constraint'
    character(len=*), parameter, public :: IM_STANZA_NAME_SERVICE_UNAVAILABLE     = 'service-unavailable'
    character(len=*), parameter, public :: IM_STANZA_NAME_SUBSCRIPTION_REQUIRED   = 'subscription-required'
    character(len=*), parameter, public :: IM_STANZA_NAME_UNDEFINED_CONDITION     = 'undefined-condition'
    character(len=*), parameter, public :: IM_STANZA_NAME_UNEXPECTED_REQUEST      = 'unexpected-request'

    ! Stanza default name spaces.
    character(len=*), parameter, public :: IM_STANZA_NS_AUTH                = XMPP_NS_AUTH
    character(len=*), parameter, public :: IM_STANZA_NS_BIND                = XMPP_NS_BIND
    character(len=*), parameter, public :: IM_STANZA_NS_CLIENT              = XMPP_NS_CLIENT
    character(len=*), parameter, public :: IM_STANZA_NS_COMPONENT           = XMPP_NS_COMPONENT
    character(len=*), parameter, public :: IM_STANZA_NS_COMPRESSION         = XMPP_NS_COMPRESSION
    character(len=*), parameter, public :: IM_STANZA_NS_DATA                = 'im:x:data'
    character(len=*), parameter, public :: IM_STANZA_NS_DISCO_INFO          = XMPP_NS_DISCO_INFO
    character(len=*), parameter, public :: IM_STANZA_NS_DISCO_ITEMS         = XMPP_NS_DISCO_ITEMS
    character(len=*), parameter, public :: IM_STANZA_NS_FEATURE_COMPRESSION = XMPP_NS_FEATURE_COMPRESSION
    character(len=*), parameter, public :: IM_STANZA_NS_HTTP_UPLOAD         = 'urn:xmpp:http:upload:0'
    character(len=*), parameter, public :: IM_STANZA_NS_PING                = 'urn:xmpp:ping'
    character(len=*), parameter, public :: IM_STANZA_NS_REGISTER            = XMPP_NS_REGISTER
    character(len=*), parameter, public :: IM_STANZA_NS_ROSTER              = XMPP_NS_ROSTER
    character(len=*), parameter, public :: IM_STANZA_NS_SASL                = XMPP_NS_SASL
    character(len=*), parameter, public :: IM_STANZA_NS_SESSION             = XMPP_NS_SESSION
    character(len=*), parameter, public :: IM_STANZA_NS_SM                  = XMPP_NS_SM
    character(len=*), parameter, public :: IM_STANZA_NS_STANZAS_IETF        = XMPP_NS_STANZAS_IETF
    character(len=*), parameter, public :: IM_STANZA_NS_STREAMS             = XMPP_NS_STREAMS
    character(len=*), parameter, public :: IM_STANZA_NS_STREAMS_IETF        = XMPP_NS_STREAMS_IETF
    character(len=*), parameter, public :: IM_STANZA_NS_TLS                 = XMPP_NS_TLS

    ! Stanza default texts.
    character(len=*), parameter, public :: IM_STANZA_TEXT_AWAY   = 'away'
    character(len=*), parameter, public :: IM_STANZA_TEXT_CHAT   = 'chat'
    character(len=*), parameter, public :: IM_STANZA_TEXT_DND    = 'dnd'
    character(len=*), parameter, public :: IM_STANZA_TEXT_ONLINE = 'online'
    character(len=*), parameter, public :: IM_STANZA_TEXT_XA     = 'xa'

    ! Stanza default types.
    character(len=*), parameter, public :: IM_STANZA_TYPE_CANCEL      = 'cancel'
    character(len=*), parameter, public :: IM_STANZA_TYPE_CHAT        = 'chat'
    character(len=*), parameter, public :: IM_STANZA_TYPE_ERROR       = 'error'
    character(len=*), parameter, public :: IM_STANZA_TYPE_GET         = 'get'
    character(len=*), parameter, public :: IM_STANZA_TYPE_MODIFY      = 'modify'
    character(len=*), parameter, public :: IM_STANZA_TYPE_NORMAL      = 'normal'
    character(len=*), parameter, public :: IM_STANZA_TYPE_RESULT      = 'result'
    character(len=*), parameter, public :: IM_STANZA_TYPE_SET         = 'set'
    character(len=*), parameter, public :: IM_STANZA_TYPE_SUBMIT      = 'submit'
    character(len=*), parameter, public :: IM_STANZA_TYPE_UNAVAILABLE = 'unavailable'

    type, public :: im_type
        !! IM/XMPP context type.
        type(c_ptr)                    :: ctx        = c_null_ptr  !! libstrophe context.
        type(c_ptr)                    :: connection = c_null_ptr  !! libstrophe connection.
        type(c_ptr)                    :: sm_state   = c_null_ptr  !! libstrophe stream management state.
        character(len=IM_HOST_LEN)     :: host       = ' '         !! XMPP server host.
        integer                        :: port       = IM_PORT     !! XMPP server port.
        character(len=IM_JID_LEN)      :: jid        = ' '         !! XMPP id of account.
        character(len=IM_JID_FULL_LEN) :: jid_full   = ' '         !! XMPP id with resource.
        character(len=IM_PASSWORD_LEN) :: password   = ' '         !! XMPP password of account.
    end type im_type

    type, public :: im_upload_type
        !! IM HTTP upload context type
        character(len=FILE_PATH_LEN) :: file_path     = ' '
        character(len=FILE_PATH_LEN) :: file_name     = ' '
        integer(kind=i8)             :: file_size     = 0_i8
        character(len=IM_URL_LEN)    :: url_get       = ' '
        character(len=IM_URL_LEN)    :: url_put       = ' '
        character(len=MIME_LEN)      :: content_type  = ' '
        character(len=32)            :: auth          = ' '
        character(len=1024)          :: cookie        = ' '
        character(len=32)            :: expires       = ' '
        integer                      :: error         = E_NONE
        character(len=512)           :: error_message = ' '
    end type im_upload_type

    ! Imported abstract interfaces.
    public :: dm_im_callback
    public :: dm_im_certfail_callback
    public :: dm_im_connection_callback
    public :: dm_im_global_timed_callback
    public :: dm_im_log_callback
    public :: dm_im_password_callback
    public :: dm_im_timed_callback

    ! Public procedures.
    public :: dm_im_connect
    public :: dm_im_create
    public :: dm_im_create_iq_error
    public :: dm_im_create_iq_http_upload
    public :: dm_im_create_iq_ping
    public :: dm_im_create_iq_result
    public :: dm_im_destroy
    public :: dm_im_disconnect
    public :: dm_im_init
    public :: dm_im_is_connected
    public :: dm_im_preserve_stream_management_state
    public :: dm_im_run
    public :: dm_im_send_presence
    public :: dm_im_send_stanza
    public :: dm_im_shutdown
    public :: dm_im_stop

    ! Private procedures.
    private :: im_stanza_get_error_message

    ! Private callbacks.
    private :: im_http_upload_response_callback
contains
    ! ******************************************************************
    ! PUBLIC FUNCTIONS.
    ! ******************************************************************
    integer function dm_im_connect(im, host, port, jid, password, callback, user_data, &
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
        type(im_type), intent(inout)            :: im           !! IM context type.
        character(len=*),  intent(in)           :: host         !! XMPP server (IP address or FQDN).
        integer,           intent(in)           :: port         !! XMPP server port.
        character(len=*),  intent(in)           :: jid          !! IM ID (JID).
        character(len=*),  intent(in)           :: password     !! JID account password.
        procedure(dm_im_connection_callback)    :: callback     !! IM connection handler.
        type(c_ptr),       intent(in), optional :: user_data    !! C pointer to user data.
        character(len=*),  intent(in), optional :: resource     !! Optional resource (`<jid>@<domain>/<resource>`).
        logical,           intent(in), optional :: keep_alive   !! Enable TCP Keep Alive.
        logical,           intent(in), optional :: tls_required !! TLS is mandatory.
        logical,           intent(in), optional :: tls_trusted  !! Trust TLS certificate.

        integer              :: stat
        integer(kind=c_long) :: flags
        logical              :: keep_alive_, tls_required_, tls_trusted_
        type(c_ptr)          :: user_data_

        rc = E_NULL
        if (.not. c_associated(im%ctx)) return

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
        im%connection = xmpp_conn_new(im%ctx)
        if (.not. c_associated(im%connection)) return

        ! Set flags.
        flags = 0
        if (tls_required_) flags = ior(flags, XMPP_CONN_FLAG_MANDATORY_TLS)
        if (tls_trusted_)  flags = ior(flags, XMPP_CONN_FLAG_TRUST_TLS)

        stat = xmpp_conn_set_flags(im%connection, flags)
        if (stat /= XMPP_EOK) return

        ! Set context data.
        im%host     = host
        im%port     = port
        im%jid      = jid
        im%jid_full = jid
        im%password = password

        if (present(resource)) im%jid_full = trim(im%jid_full) // '/' // resource

        ! Set credentials.
        call xmpp_conn_set_jid (im%connection, trim(im%jid_full))
        call xmpp_conn_set_pass(im%connection, trim(password))

        ! Set TCP Keep Alive using default callback.
        if (keep_alive_) call xmpp_conn_set_sockopt_callback(im%connection, xmpp_sockopt_cb_keepalive)

        ! Set stream management state if available.
        if (c_associated(im%sm_state)) then
            stat = xmpp_conn_set_sm_state(im%connection, im%sm_state)
            im%sm_state = c_null_ptr
        end if

        ! C pointer to user data.
        user_data_ = c_null_ptr
        if (present(user_data)) user_data_ = user_data

        rc = E_IO
        stat = xmpp_connect_client(conn       = im%connection, &
                                   alt_domain = trim(host), &
                                   alt_port   = port, &
                                   callback   = callback, &
                                   user_data  = user_data_)
        if (stat /= XMPP_EOK) return

        rc = E_NONE
    end function dm_im_connect

    integer function dm_im_create(im, log_level) result(rc)
        !! Creates IM context. Logging to standard error is disabled by default
        !! or if `log_level` is `IM_LL_NONE`. Make sure that argument `im` is not
        !! created already.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if `log_level` is invalid.
        !! * `E_XMPP` if the XMPP context could not be created.
        !!
        type(im_type), intent(out)          :: im        !! IM context type.
        integer,       intent(in), optional :: log_level !! Log level of libstrophe (`IM_LL_*`).

        integer     :: ll
        type(c_ptr) :: log

        rc = E_INVALID
        log = c_null_ptr

        ! Set log level.
        ll = IM_LL_NONE
        if (present(log_level)) ll = log_level
        if (ll < IM_LL_NONE .or. ll > IM_LL_ERROR) return

        ! Enable logging.
        if (ll /= IM_LL_NONE) log = xmpp_get_default_logger(ll)

        ! Create libstrophe context.
        rc = E_XMPP
        im%ctx = xmpp_ctx_new(c_null_ptr, log)
        if (.not. c_associated(im%ctx)) return

        rc = E_NONE
    end function dm_im_create

    type(c_ptr) function dm_im_create_iq_error(im, id, type, condition) result(iq_stanza)
        !! Returns C pointer to new error iq stanza.
        type(im_type),    intent(inout)        :: im        !! IM context type.
        character(len=*), intent(in)           :: id        !! Stanza id.
        character(len=*), intent(in)           :: type      !! Stanza type.
        character(len=*), intent(in), optional :: condition !! Condition stanza name.

        integer     :: stat
        type(c_ptr) :: condition_stanza, error_stanza

        iq_stanza    = xmpp_iq_new(im%ctx, IM_STANZA_TYPE_ERROR, id)
        error_stanza = xmpp_stanza_new(im%ctx)

        stat = xmpp_stanza_set_name(iq_stanza, IM_STANZA_NAME_ERROR)
        stat = xmpp_stanza_set_type(error_stanza, type)

        if (present(condition)) then
            condition_stanza = xmpp_stanza_new(im%ctx)
            stat = xmpp_stanza_set_name(condition_stanza, condition)
            stat = xmpp_stanza_set_ns(condition_stanza, IM_STANZA_NS_STANZAS_IETF)
            stat = xmpp_stanza_add_child(error_stanza, condition_stanza)
        end if

        stat = xmpp_stanza_add_child(iq_stanza, error_stanza)
        if (present(condition)) stat = xmpp_stanza_release(condition_stanza)
        stat = xmpp_stanza_release(error_stanza)
    end function dm_im_create_iq_error

    type(c_ptr) function dm_im_create_iq_http_upload(im, id, file_name, file_size, content_type) result(iq_stanza)
        !! Returns C pointer to new http upload iq stanza.
        use :: dm_util, only: dm_itoa

        type(im_type),    intent(inout) :: im           !! IM context type.
        character(len=*), intent(in)    :: id           !! Stanza id.
        character(len=*), intent(in)    :: file_name    !! File name.
        integer(kind=i8), intent(in)    :: file_size    !! File size in bytes.
        character(len=*), intent(in)    :: content_type !! MIME type.

        integer     :: stat
        type(c_ptr) :: request_stanza

        iq_stanza      = xmpp_iq_new(im%ctx, IM_STANZA_TYPE_GET, id)
        request_stanza = xmpp_stanza_new(im%ctx)

        stat = xmpp_stanza_set_name(request_stanza, IM_STANZA_NAME_REQUEST)
        stat = xmpp_stanza_set_ns(request_stanza, IM_STANZA_NS_HTTP_UPLOAD)

        stat = xmpp_stanza_set_attribute(request_stanza, IM_STANZA_ATTR_FILENAME,     trim(file_name))
        stat = xmpp_stanza_set_attribute(request_stanza, IM_STANZA_ATTR_SIZE,         dm_itoa(file_size))
        stat = xmpp_stanza_set_attribute(request_stanza, IM_STANZA_ATTR_CONTENT_TYPE, trim(content_type))

        stat = xmpp_stanza_add_child(iq_stanza, request_stanza)
        stat = xmpp_stanza_release(request_stanza)
    end function dm_im_create_iq_http_upload

    type(c_ptr) function dm_im_create_iq_ping(im, id, to) result(iq_stanza)
        !! Returns C pointer to new ping iq stanza.
        type(im_type),    intent(inout)        :: im !! IM context type.
        character(len=*), intent(in)           :: id !! Stanza id.
        character(len=*), intent(in), optional :: to !! Target.

        integer     :: stat
        type(c_ptr) :: ping_stanza

        iq_stanza   = xmpp_iq_new(im%ctx, IM_STANZA_TYPE_GET, id)
        ping_stanza = xmpp_stanza_new(im%ctx)

        if (present(to)) stat = xmpp_stanza_set_to(iq_stanza, to)

        stat = xmpp_stanza_set_name(ping_stanza, IM_STANZA_NAME_PING)
        stat = xmpp_stanza_set_ns(ping_stanza, IM_STANZA_NS_PING)
        stat = xmpp_stanza_add_child(iq_stanza, ping_stanza)
        stat = xmpp_stanza_release(ping_stanza)
    end function dm_im_create_iq_ping

    type(c_ptr) function dm_im_create_iq_result(im, id) result(iq_stanza)
        !! Returns C pointer to new result iq stanza.
        type(im_type),    intent(inout) :: im !! IM context type.
        character(len=*), intent(in)    :: id !! Stanza id.

        iq_stanza = xmpp_iq_new(im%ctx, IM_STANZA_TYPE_RESULT, id)
    end function dm_im_create_iq_result

    logical function dm_im_is_connected(im) result(is)
        !! Returns `.true.` if connection is open.
        type(im_type), intent(inout) :: im !! IM context type.

        is = c_associated(im%connection)
        if (is) is = (xmpp_conn_is_connected(im%connection) == 1)
    end function dm_im_is_connected

    ! ******************************************************************
    ! PUBLIC SUBROUTINES.
    ! ******************************************************************
    subroutine dm_im_destroy(im)
        !! Destroys XMPP context and an closes the connection if still open.
        type(im_type), intent(inout) :: im !! IM context type.

        if (dm_im_is_connected(im)) call dm_im_disconnect(im)
        call xmpp_ctx_free(im%ctx)
    end subroutine dm_im_destroy

    subroutine dm_im_disconnect(im)
        !! Disconnects from server and releases connection.
        type(im_type), intent(inout) :: im !! IM context type.

        integer :: stat

        if (.not. dm_im_is_connected(im)) return
        call xmpp_disconnect(im%connection)
        stat = xmpp_conn_release(im%connection)
    end subroutine dm_im_disconnect

    subroutine dm_im_init()
        !! Initialises XMPP backend (libstrophe).
        call xmpp_initialize()
    end subroutine dm_im_init

    subroutine dm_im_preserve_stream_management_state(im)
        !! Saves current XMPP stream management state to Jabber context.
        type(im_type), intent(inout) :: im !! IM context type.

        im%sm_state = xmpp_conn_get_sm_state(im%connection)
    end subroutine dm_im_preserve_stream_management_state

    subroutine dm_im_run(im)
        !! Starts XMPP event loop.
        type(im_type), intent(inout) :: im !! IM context type.

        if (.not. c_associated(im%ctx)) return
        call xmpp_run(im%ctx)
    end subroutine dm_im_run

    subroutine dm_im_send_presence(im, show, status)
        !! Creates and sends presence to XMPP server.
        use :: dm_string

        type(im_type),    intent(inout)        :: im     !! IM context type.
        character(len=*), intent(in), optional :: show   !! Availability (`away`, `dnd`, `online`, …).
        character(len=*), intent(in), optional :: status !! Human-readable status description text.

        integer     :: stat
        type(c_ptr) :: attr, pres, text

        if (.not. dm_im_is_connected(im)) return

        pres = xmpp_presence_new(im%ctx)

        if (dm_string_is_present(show)) then
            attr = xmpp_stanza_new(im%ctx)
            stat = xmpp_stanza_set_name(attr, IM_STANZA_NAME_SHOW)
            text = xmpp_stanza_new(im%ctx)
            stat = xmpp_stanza_set_text(text, trim(show))
            stat = xmpp_stanza_add_child(attr, text)
            stat = xmpp_stanza_add_child(pres, attr)
            stat = xmpp_stanza_release(text)
            stat = xmpp_stanza_release(attr)
        end if

        if (dm_string_is_present(status)) then
            attr = xmpp_stanza_new(im%ctx)
            stat = xmpp_stanza_set_name(attr, IM_STANZA_NAME_STATUS)
            text = xmpp_stanza_new(im%ctx)
            stat = xmpp_stanza_set_text(text, trim(status))
            stat = xmpp_stanza_add_child(attr, text)
            stat = xmpp_stanza_add_child(pres, attr)
            stat = xmpp_stanza_release(text)
            stat = xmpp_stanza_release(attr)
        end if

        call xmpp_send(im%connection, pres)
        stat = xmpp_stanza_release(pres)
    end subroutine dm_im_send_presence

    subroutine dm_im_send_stanza(im, stanza, release)
        !! Sends stanza to server and releases it by default.
        type(im_type), intent(inout)        :: im      !! IM context type.
        type(c_ptr),   intent(in)           :: stanza  !! Stanza to send.
        logical,       intent(in), optional :: release !! Release stanza afterwards.

        integer :: stat
        logical :: release_

        release_ = .true.
        if (present(release)) release_ = release

        call xmpp_send(im%connection, stanza)
        if (release_) stat = xmpp_stanza_release(stanza)
    end subroutine dm_im_send_stanza

    subroutine dm_im_shutdown()
        !! Shuts down XMPP backend (libstrophe).
        call xmpp_shutdown()
    end subroutine dm_im_shutdown

    subroutine dm_im_stop(im)
        !! Stops event loop (libstrophe).
        type(im_type), intent(inout) :: im !! IM context type.

        call xmpp_stop(im%ctx)
    end subroutine dm_im_stop

    ! ******************************************************************
    ! PRIVATE FUNCTIONS.
    ! ******************************************************************
    function im_stanza_get_error_message(stanza) result(message)
        !! Returns error message of stanza as allocatable string.
        type(c_ptr), intent(in)       :: stanza  !! XMPP stanza pointer.
        character(len=:), allocatable :: message !! Error message.

        stanza_block: block
            character(len=:), allocatable :: text
            type(c_ptr)                   :: error_stanza, text_stanza

            if (.not. c_associated(stanza)) exit stanza_block
            error_stanza = xmpp_stanza_get_child_by_name(stanza, IM_STANZA_NAME_ERROR)
            if (.not. c_associated(error_stanza)) exit stanza_block
            text_stanza = xmpp_stanza_get_child_by_name(error_stanza, IM_STANZA_NAME_TEXT)
            if (.not. c_associated(text_stanza)) exit stanza_block
            text = xmpp_stanza_get_text(error_stanza)
            if (len(text) == 0) exit stanza_block
            message = text
        end block stanza_block

        if (.not. allocated(message)) message = 'unknown'
    end function im_stanza_get_error_message

    ! ******************************************************************
    ! PRIVATE CALLBACKS.
    ! ******************************************************************
    function im_http_upload_response_callback(stanza, user_data) bind(c)
        !! C-interoperable HTTP upload response callback.
        type(c_ptr), intent(in), value :: stanza                           !! xmpp_stanza_t *
        type(c_ptr), intent(in), value :: user_data                        !! void *
        integer(kind=c_int)            :: im_http_upload_response_callback !! int

        character(len=:), allocatable :: from, header_name, type
        type(c_ptr)                   :: get_stanza, header_stanza, put_stanza, slot_stanza
        type(im_upload_type), pointer :: upload

        im_http_upload_response_callback = 0

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, upload)

        from = xmpp_stanza_get_from(stanza)
        type = xmpp_stanza_get_type(stanza)

        if (type == IM_STANZA_TYPE_ERROR) then
            upload%error         = E_IO
            upload%error_message = im_stanza_get_error_message(stanza)
            return
        end if

        slot_stanza = xmpp_stanza_get_child_by_name(stanza, IM_STANZA_NAME_SLOT)

        if (xmpp_stanza_get_ns(slot_stanza) == IM_STANZA_NS_HTTP_UPLOAD) then
            get_stanza = xmpp_stanza_get_child_by_name(slot_stanza, IM_STANZA_NAME_GET)
            put_stanza = xmpp_stanza_get_child_by_name(slot_stanza, IM_STANZA_NAME_PUT)

            if (c_associated(get_stanza) .and. c_associated(put_stanza)) then
                upload%url_get = xmpp_stanza_get_attribute(get_stanza, IM_STANZA_ATTR_URL)
                upload%url_put = xmpp_stanza_get_attribute(put_stanza, IM_STANZA_ATTR_URL)

                header_stanza = xmpp_stanza_get_children(put_stanza)
                header_stanza = xmpp_stanza_get_next(header_stanza)

                do while (c_associated(header_stanza))
                    if (xmpp_stanza_get_name(header_stanza) == IM_STANZA_NAME_HEADER) then
                        header_name = xmpp_stanza_get_attribute(header_stanza, IM_STANZA_ATTR_NAME)

                        select case (header_name)
                            case (IM_STANZA_HEADER_AUTHORIZATION)
                                upload%auth = xmpp_stanza_get_text(header_stanza)

                            case (IM_STANZA_HEADER_COOKIE)
                                upload%cookie = xmpp_stanza_get_text(header_stanza)

                            case (IM_STANZA_HEADER_EXPIRES)
                                upload%expires = xmpp_stanza_get_text(header_stanza)

                            case default
                                upload%error = E_TYPE
                        end select
                    end if

                    header_stanza = xmpp_stanza_get_next(header_stanza)
                end do

                ! start HTTP upload here ...
            else
                upload%error = E_INVALID
                im_http_upload_response_callback = 1
            end if
        end if
    end function im_http_upload_response_callback
end module dm_im
