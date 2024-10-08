! dmbot.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmbot
    !! This program is an XMPP bot for remote control of sensor nodes.
    use, intrinsic :: iso_c_binding
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmbot'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 6

    logical, parameter :: APP_TCP_KEEP_ALIVE = .true.  !! Enable TCP Keep Alive.
    logical, parameter :: APP_TLS_TRUSTED    = .false. !! Trust unknown TLS certificate.

    integer, parameter :: HOST_LEN     = 256 !! Max. length of XMPP host.
    integer, parameter :: PASSWORD_LEN = 256 !! Max. length of XMPP password.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)          :: name      = APP_NAME    !! Name of instance/configuration.
        character(len=FILE_PATH_LEN)   :: config    = ' '         !! Path to config file.
        character(len=LOGGER_NAME_LEN) :: logger    = ' '         !! Name of logger.
        character(len=NODE_ID_LEN)     :: node      = ' '         !! Node id.
        character(len=HOST_LEN)        :: host      = ' '         !! IP or FQDN of XMPP server.
        integer                        :: port      = JABBER_PORT !! Port of XMPP server.
        logical                        :: tls       = .true.      !! TLS is mandatory.
        character(len=JABBER_JID_LEN)  :: jid       = ' '         !! HTTP Basic Auth user name.
        character(len=PASSWORD_LEN)    :: password  = ' '         !! HTTP Basic Auth password.
        logical                        :: debug     = .false.     !! Force writing of output file.
        logical                        :: verbose   = .false.     !! Force writing of output file.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer                   :: rc     ! Return code.
    type(app_type)            :: app    ! App settings.
    type(jabber_type), target :: jabber ! Jabber context.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments and configuration from file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get()
    call logger%configure(name    = app%logger, &                 ! Name of logger process.
                          node_id = app%node, &                   ! Node id.
                          source  = app%name, &                   ! Log source.
                          debug   = app%debug, &                  ! Forward DEBUG messages via IPC.
                          ipc     = (len_trim(app%logger) > 0), & ! Enable IPC.
                          verbose = app%verbose)                  ! Print logs to standard error.

    ! Initialise environment.
    init_block: block
        ! Initialise XMPP backend.
        call dm_jabber_init()
        rc = dm_jabber_create(jabber)

        if (dm_is_error(rc)) then
            call dm_error_out(rc)
            exit init_block
        end if

        ! Connect to XMPP server.
        rc = dm_jabber_connect(jabber       = jabber, &
                               host         = app%host, &
                               port         = app%port, &
                               jid          = app%jid, &
                               password     = app%password, &
                               callback     = connect_callback, &
                               user_data    = c_loc(jabber), &
                               resource     = app%name, &
                               keep_alive   = APP_TCP_KEEP_ALIVE, &
                               tls_required = app%tls, &
                               tls_trusted  = APP_TLS_TRUSTED)

        if (dm_is_error(rc)) then
            call dm_error_out(rc)
            exit init_block
        end if

        ! Register signal handler.
        call dm_signal_register(signal_callback)

        ! Run event loop of bot.
        call dm_jabber_run(jabber)
    end block init_block

    call halt(rc)
contains
    ! ******************************************************************
    ! FUNCTIONS.
    ! ******************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and configuration from file (if
        !! `--config` is passed).
        type(app_type), intent(out) :: app !! App type.

        character(len=:), allocatable :: version
        type(arg_type)                :: args(11)

        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <id>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),      & ! -l, --logger <string>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('host',     short='H', type=ARG_TYPE_STRING),  & ! -H, --host <string>
            arg_type('port',     short='q', type=ARG_TYPE_INTEGER), & ! -q, --port <n>
            arg_type('tls',      short='E', type=ARG_TYPE_LOGICAL), & ! -E, --tls
            arg_type('jid',      short='J', type=ARG_TYPE_STRING),  & ! -J, --jid <string>
            arg_type('password', short='P', type=ARG_TYPE_STRING),  & ! -P, --password <string>
            arg_type('debug',    short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('verbose',  short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        version = dm_lua_version(.true.) // ' ' // dm_db_version(.true.)
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, version)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%name)
        call dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call dm_arg_get(args( 3), app%logger)
        call dm_arg_get(args( 4), app%node)
        call dm_arg_get(args( 5), app%host)
        call dm_arg_get(args( 6), app%port)
        call dm_arg_get(args( 7), app%tls)
        call dm_arg_get(args( 8), app%jid)
        call dm_arg_get(args( 9), app%password)
        call dm_arg_get(args(10), app%debug)
        call dm_arg_get(args(11), app%verbose)

        ! Validate passed options.
        rc = E_INVALID

        if (.not. dm_id_is_valid(app%node)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (len_trim(app%host) == 0) then
            call dm_error_out(rc, 'missing host')
            return
        end if

        if (app%port < 0) then
            call dm_error_out(rc, 'invalid port')
            return
        end if

        if (len_trim(app%jid) == 0) then
            call dm_error_out(rc, 'missing jabber id')
            return
        end if

        if (len_trim(app%password) == 0) then
            call dm_error_out(rc, 'missing password')
            return
        end if

        if (app%port == 0) app%port = JABBER_PORT

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.
        type(config_type)             :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'logger',   app%logger)
            call dm_config_get(config, 'node',     app%node)
            call dm_config_get(config, 'host',     app%host)
            call dm_config_get(config, 'port',     app%port)
            call dm_config_get(config, 'tls',      app%tls)
            call dm_config_get(config, 'jid',      app%jid)
            call dm_config_get(config, 'password', app%password)
            call dm_config_get(config, 'debug',    app%debug)
            call dm_config_get(config, 'verbose',  app%verbose)
        end if

        call dm_config_close(config)
    end function read_config

    ! ******************************************************************
    ! SUBROUTINES.
    ! ******************************************************************
    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: rc, stat

        stat = STOP_SUCCESS
        if (dm_is_error(error)) stat = STOP_FAILURE

        rc = dm_jabber_disconnect(jabber)
        call dm_jabber_destroy(jabber)
        call dm_jabber_shutdown()

        call dm_stop(stat)
    end subroutine halt

    ! ******************************************************************
    ! CALLBACK PROCEDURES.
    ! ******************************************************************
    function disconnect_callback(connection, user_data) bind(c)
        use :: xmpp

        type(c_ptr), intent(in), value :: connection !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: user_data  !! void *
        integer(kind=c_int)            :: disconnect_callback

        disconnect_callback = 0
        call xmpp_disconnect(connection)
    end function disconnect_callback

    function message_callback(connection, stanza, user_data) bind(c)
        use :: xmpp

        type(c_ptr), intent(in), value :: connection !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: stanza     !! xmpp_stanza_t *
        type(c_ptr), intent(in), value :: user_data  !! void *
        integer(kind=c_int)            :: message_callback

        character(len=:), allocatable :: from, reply_text, text, type
        integer                       :: stat
        type(c_ptr)                   :: body, reply

        message_callback = 1

        body = xmpp_stanza_get_child_by_name(stanza, 'body')
        if (.not. c_associated(body)) return

        type = xmpp_stanza_get_type(stanza)
        if (type == 'error') return

        text = xmpp_stanza_get_text(body)
        from = xmpp_stanza_get_from(stanza)

        call logger%debug('incoming message from ' // from)

        reply = xmpp_stanza_reply(stanza)

        if (.not. c_associated(reply)) then
            stat = xmpp_stanza_set_type(reply, 'chat')
        end if

        if (text == '!quit') then
            reply_text = 'bye!'
            call xmpp_timed_handler_add(connection, disconnect_callback, int(500, kind=c_unsigned_long), c_null_ptr)
        else
            reply_text = trim(text) // ' to you too!'
        end if

        stat = xmpp_message_set_body(reply, reply_text)
        call xmpp_send(jabber%connection, reply)
    end function message_callback

    subroutine connect_callback(connection, event, error, stream_error, user_data) bind(c)
        use :: xmpp

        type(c_ptr),               intent(in), value :: connection   !! xmpp_conn_t *
        integer(kind=c_int),       intent(in), value :: event        !! xmpp_conn_event_t
        integer(kind=c_int),       intent(in), value :: error        !! int
        type(xmpp_stream_error_t), intent(in)        :: stream_error !! xmpp_stream_error_t *
        type(c_ptr),               intent(in), value :: user_data    !! void *

        type(jabber_type), pointer :: jabber

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, jabber)

        if (event == XMPP_CONN_CONNECT) then
            call logger%debug('connected')

            call xmpp_handler_add(connection, message_callback, '', 'message', '', jabber%ctx)

            call dm_jabber_send_presence(jabber, JABBER_STANZA_TEXT_ONLINE)
        else
            call logger%debug('disconnected')

            call xmpp_handler_delete(connection, message_callback)

            call xmpp_stop(jabber%ctx)
        end if
    end subroutine connect_callback

    subroutine signal_callback(signum) bind(c)
        !! Default POSIX signal handler of the program.
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        select case (signum)
            case default
                call logger%info('exit on signal ' // dm_itoa(signum))
                call halt(E_NONE)
        end select
    end subroutine signal_callback
end program dmbot
