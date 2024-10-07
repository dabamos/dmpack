! Author:  Philipp Engel
! Licence: ISC
module dm_jabber
    !! Jabber/XMPP abstraction module, based on _libstrophe_.
    !!
    !! Link this module against `pkg-config --libs libstrophe expat openssl zlib`.
    !!
    !! The following example just connects to an XMPP server and sends a
    !! presence stanza:
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
    !!                        jid        = 'user@example.com',
    !!                        password   = 'secret', &
    !!                        callback   = connection_callback, &
    !!                        user_data  = c_loc(jabber), &
    !!                        keep_alive = .true.)
    !! if (dm_is_ok(rc)) call dm_jabber_run(jabber)
    !!
    !! rc = dm_jabber_disconnect(jabber)
    !! call dm_jabber_destroy(jabber)
    !! call dm_jabber_shutdown()
    !! ```
    !!
    !! The connection callback must be of C-interoperable abstract interface
    !! `dm_jabber_connection_handler()`, which is an alias for
    !! `xmpp_conn_handler()` from module `xmpp`, for example:
    !!
    !! ```fortran
    !! subroutine connection_callback(connection, event, error, stream_error, user_data) bind(c)
    !!     use, intrinsic :: iso_c_binding
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
    !!         call xmpp_stop(jabber%ctx)
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

    integer, parameter, public :: JABBER_PORT     = 5222 !! Default XMPP port (StartTLS).
    integer, parameter, public :: JABBER_PORT_TLS = 5223 !! Secondary XMPP port (TLS).

    ! Log level of libstrophe.
    integer, parameter, public :: JABBER_LL_NONE    = -1 !! Logging disabled.
    integer, parameter, public :: JABBER_LL_DEBUG   = XMPP_LEVEL_DEBUG
    integer, parameter, public :: JABBER_LL_INFO    = XMPP_LEVEL_INFO
    integer, parameter, public :: JABBER_LL_WARNING = XMPP_LEVEL_WARN
    integer, parameter, public :: JABBER_LL_ERROR   = XMPP_LEVEL_ERROR

    type, public :: jabber_type
        !! Jabber/XMPP context type.
        type(c_ptr) :: ctx        = c_null_ptr !! libstrophe context.
        type(c_ptr) :: connection = c_null_ptr !! libstrophe connection.
        type(c_ptr) :: sm_state   = c_null_ptr !! libstrophe stream management state.
    end type jabber_type

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
    public :: dm_jabber_destroy
    public :: dm_jabber_disconnect
    public :: dm_jabber_init
    public :: dm_jabber_is_connected
    public :: dm_jabber_run
    public :: dm_jabber_set_stream_management_state
    public :: dm_jabber_shutdown
contains
    integer function dm_jabber_connect(jabber, host, port, jid, password, callback, user_data, &
                                       keep_alive, tls_required, tls_trusted) result(rc)
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
        jabber%connection = xmpp_conn_new(jabber%ctx)

        rc = E_XMPP
        if (.not. c_associated(jabber%connection)) return

        ! Set flags.
        flags = 0
        if (tls_required_) flags = ior(flags, XMPP_CONN_FLAG_MANDATORY_TLS)
        if (tls_trusted_)  flags = ior(flags, XMPP_CONN_FLAG_TRUST_TLS)

        stat = xmpp_conn_set_flags(jabber%connection, flags)
        if (stat /= XMPP_EOK) return

        ! Set credentials.
        call xmpp_conn_set_jid (jabber%connection, trim(jid))
        call xmpp_conn_set_pass(jabber%connection, trim(password))

        ! Set TCP Keep Alive.
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

        integer     :: level
        type(c_ptr) :: log

        rc = E_INVALID
        log = c_null_ptr

        ! Set log level.
        level = JABBER_LL_NONE
        if (present(log_level)) level = log_level
        if (level < JABBER_LL_NONE .or. level > JABBER_LL_ERROR) return

        ! Enable logging.
        if (level /= JABBER_LL_NONE) then
            log = xmpp_get_default_logger(level)
        end if

        ! Create libstrophe context.
        rc = E_XMPP
        jabber%ctx = xmpp_ctx_new(c_null_ptr, log)
        if (.not. c_associated(jabber%ctx)) return

        rc = E_NONE
    end function dm_jabber_create

    integer function dm_jabber_disconnect(jabber) result(rc)
        !! Disconnects from server and releases connection.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        integer :: stat

        rc = E_XMPP
        call xmpp_disconnect(jabber%connection)
        stat = xmpp_conn_release(jabber%connection)
        if (stat /= XMPP_EOK) return
        rc = E_NONE
    end function dm_jabber_disconnect

    logical function dm_jabber_is_connected(jabber) result(is)
        !! Returns `.true.` if connection is open.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        is = (xmpp_conn_is_connected(jabber%connection) == 1)
    end function dm_jabber_is_connected

    subroutine dm_jabber_destroy(jabber)
        !! Destroys XMPP context and an closes the connection if still open.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        integer :: rc

        if (dm_jabber_is_connected(jabber)) then
            rc = dm_jabber_disconnect(jabber)
        end if

        call xmpp_ctx_free(jabber%ctx)
    end subroutine dm_jabber_destroy

    subroutine dm_jabber_init()
        !! Initialises XMPP backend (libstrophe).
        call xmpp_initialize()
    end subroutine dm_jabber_init

    subroutine dm_jabber_run(jabber)
        !! Starts XMPP event loop.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        if (.not. c_associated(jabber%ctx)) return
        call xmpp_run(jabber%ctx)
    end subroutine dm_jabber_run

    subroutine dm_jabber_set_stream_management_state(jabber)
        !! Saves current XMPP stream management state to Jabber context.
        type(jabber_type), intent(inout) :: jabber !! Jabber context type.

        jabber%sm_state = xmpp_conn_get_sm_state(jabber%connection)
    end subroutine dm_jabber_set_stream_management_state

    subroutine dm_jabber_shutdown()
        !! Shuts down XMPP backend (libstrophe).
        call xmpp_shutdown()
    end subroutine dm_jabber_shutdown
end module dm_jabber
