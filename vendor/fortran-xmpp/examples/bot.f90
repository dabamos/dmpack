! bot.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! Simple XMPP bot, based on the official example `bot.c`.
    use, intrinsic :: iso_c_binding
    use :: xmpp
    implicit none (type, external)

    character(len=*), parameter :: JID      = 'user@example.com'
    character(len=*), parameter :: PASSWORD = 'secret'
    character(len=*), parameter :: HOST     = 'example.com'
    integer,          parameter :: PORT     = 5222

    logical, parameter :: TCP_KEEP_ALIVE = .false.
    logical, parameter :: TLS_REQUIRED   = .true.
    logical, parameter :: TLS_TRUSTED    = .true.

    type :: user_type
        character(len=:), allocatable :: jid
        character(len=:), allocatable :: password
    end type user_type

    integer                 :: stat
    integer(kind=c_long)    :: flags
    logical                 :: reconnect
    type(user_type), target :: user

    type(c_ptr) :: ctx
    type(c_ptr) :: conn
    type(c_ptr) :: log
    type(c_ptr) :: sm_state

    sm_state = c_null_ptr

    ! Set user data.
    user = user_type(JID, PASSWORD)

    ! Set connection flags.
    flags = 0
    if (TLS_REQUIRED) flags = ior(flags, XMPP_CONN_FLAG_MANDATORY_TLS)
    if (TLS_TRUSTED)  flags = ior(flags, XMPP_CONN_FLAG_TRUST_TLS)

    ! Initialise libstrophe.
    call xmpp_initialize()

    ! Set log level and create context.
    log = xmpp_get_default_logger(XMPP_LEVEL_DEBUG)
    ctx = xmpp_ctx_new(c_null_ptr, log)

    do
        ! Reset reconnection flag.
        reconnect = .false.

        ! Create connection and set flags.
        conn = xmpp_conn_new(ctx)
        stat = xmpp_conn_set_flags(conn, flags)

        ! Set key password to user password if key is protected.
        call xmpp_conn_set_password_callback(conn, password_handler, c_loc(user))

        ! Try password retrieval only once.
        call xmpp_conn_set_password_retries(conn, 1)

        ! Set credentials.
        call xmpp_conn_set_jid(conn, user%jid)
        call xmpp_conn_set_pass(conn, user%password)

        if (TCP_KEEP_ALIVE) then
            call xmpp_conn_set_sockopt_callback(conn, xmpp_sockopt_cb_keepalive)
        end if

        ! Set Stream-Management state if available.
        if (c_associated(sm_state)) then
            stat = xmpp_conn_set_sm_state(conn, sm_state)
            sm_state = c_null_ptr
        end if

        ! Connect to XMPP server.
        stat = xmpp_connect_client(conn, HOST, PORT, conn_handler, ctx)
        if (stat == XMPP_EOK) call xmpp_run(ctx)

        if (reconnect) sm_state = xmpp_conn_get_sm_state(conn)
        stat = xmpp_conn_release(conn)

        if (reconnect) cycle
        exit
    end do

    ! Clean-up.
    call xmpp_ctx_free(ctx)
    call xmpp_shutdown()
contains
    subroutine conn_handler(conn, event, error, stream_error, user_data) bind(c)
        !! C-interoperable connection handler of abstract interface `xmpp_conn_handler()`.
        type(c_ptr),               intent(in), value :: conn         !! xmpp_conn_t *
        integer(kind=c_int),       intent(in), value :: event        !! xmpp_conn_event_t
        integer(kind=c_int),       intent(in), value :: error        !! int
        type(xmpp_stream_error_t), intent(in)        :: stream_error !! xmpp_stream_error_t *
        type(c_ptr),               intent(in), value :: user_data    !! void *

        integer     :: stat
        type(c_ptr) :: ctx
        type(c_ptr) :: pres

        ctx = user_data

        if (event == XMPP_CONN_CONNECT) then
            print '(">>>> DEBUG Connected.")'

            call xmpp_handler_add(conn, version_handler, 'jabber:iq:version', 'iq', '', ctx)
            call xmpp_handler_add(conn, message_handler, '', 'message', '', ctx)

            pres = xmpp_presence_new(ctx)
            call xmpp_send(conn, pres)
            stat = xmpp_stanza_release(pres)
        else
            print '(">>>> DEBUG Disconnected.")'
            call xmpp_stop(ctx)
        end if
    end subroutine conn_handler

    function message_handler(conn, stanza, user_data) bind(c)
        type(c_ptr), intent(in), value :: conn      !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: stanza    !! xmpp_stanza_t *
        type(c_ptr), intent(in), value :: user_data !! void *
        integer(kind=c_int)            :: message_handler

        character(len=:), allocatable :: from, reply_text, text, type
        integer                       :: stat
        type(c_ptr)                   :: body, ctx, reply

        message_handler = 1

        ctx = user_data

        body = xmpp_stanza_get_child_by_name(stanza, 'body')
        if (.not. c_associated(body)) return

        type = xmpp_stanza_get_type(stanza)
        if (type == 'error') return

        text = xmpp_stanza_get_text(body)
        from = xmpp_stanza_get_from(stanza)

        print '(">>>> DEBUG Incoming message from ", a, ": ", a)', from, text

        reply = xmpp_stanza_reply(stanza)

        if (.not. c_associated(reply)) then
            stat = xmpp_stanza_set_type(reply, 'chat')
        end if

        if (text == 'quit') then
            reply_text = 'bye!'
            call xmpp_timed_handler_add(conn, quit_handler, 500_c_unsigned_long, c_null_ptr)
        else if (text == 'reconnect') then
            reply_text = 'alright, let''s see what happens!'
            reconnect  = .true.
            call xmpp_timed_handler_add(conn, quit_handler, 500_c_unsigned_long, c_null_ptr)
        else
            reply_text = trim(text) // ' to you too!'
        end if

        stat = xmpp_message_set_body(reply, reply_text)
        call xmpp_send(conn, reply)
    end function message_handler

    function password_handler(pw, pw_max, conn, user_data) bind(c)
        use :: xmpp_util, only: c_memcpy

        type(c_ptr),            intent(in), value :: pw        !! char *
        integer(kind=c_size_t), intent(in), value :: pw_max    !! size_t
        type(c_ptr),            intent(in), value :: conn      !! xmpp_conn_t *
        type(c_ptr),            intent(in), value :: user_data !! void *
        integer(kind=c_int)                       :: password_handler

        type(user_type), pointer :: user

        password_handler = -1

        print '(">>>> DEBUG Trying to unlock ", a, ".")', xmpp_conn_get_keyfile(conn)

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, user)

        if (.not. allocated(user%password)) return
        if (len(user%password, kind=c_size_t) >= pw_max) return

        call c_memcpy(pw, c_loc(user%password), len(user%password, kind=c_size_t))
        password_handler = len(user%password)
    end function password_handler

    function quit_handler(conn, user_data) bind(c)
        type(c_ptr), intent(in), value :: conn      !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: user_data !! void *
        integer(kind=c_int)            :: quit_handler

        call xmpp_disconnect(conn)
        quit_handler = 0
    end function quit_handler

    function version_handler(conn, stanza, user_data) bind(c)
        type(c_ptr), intent(in), value :: conn      !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: stanza    !! xmpp_stanza_t *
        type(c_ptr), intent(in), value :: user_data !! void *
        integer(kind=c_int)            :: version_handler

        character(len=:), allocatable :: ns
        integer                       :: stat
        type(c_ptr)                   :: ctx
        type(c_ptr)                   :: name, query
        type(c_ptr)                   :: reply, text, version

        version_handler = 1

        ctx = user_data

        print '(">>>> DEBUG Received version request from ", a, ".")', xmpp_stanza_get_from(stanza)

        reply = xmpp_stanza_reply(stanza)
        stat  = xmpp_stanza_set_type(reply, 'result')

        query = xmpp_stanza_new(ctx)
        stat  = xmpp_stanza_set_name(query, 'query')

        ns = xmpp_stanza_get_ns(xmpp_stanza_get_children(stanza))
        if (len(ns) > 0) stat = xmpp_stanza_set_ns(query, ns)

        name = xmpp_stanza_new(ctx)
        stat = xmpp_stanza_set_name(name, 'name')
        stat = xmpp_stanza_add_child(query, name)
        stat = xmpp_stanza_release(name)

        text = xmpp_stanza_new(ctx)
        stat = xmpp_stanza_set_text(text, 'Fortran bot')
        stat = xmpp_stanza_add_child(name, text)
        stat = xmpp_stanza_release(text)

        version = xmpp_stanza_new(ctx)
        stat = xmpp_stanza_set_name(version, 'version')
        stat = xmpp_stanza_add_child(query, version)
        stat = xmpp_stanza_release(version)

        text = xmpp_stanza_new(ctx)
        stat = xmpp_stanza_set_text(text, '1.0')
        stat = xmpp_stanza_add_child(version, text)
        stat = xmpp_stanza_release(text)

        stat = xmpp_stanza_add_child(reply, query)
        stat = xmpp_stanza_release(query)

        call xmpp_send(conn, reply)
        stat = xmpp_stanza_release(reply)
    end function version_handler
end program main
