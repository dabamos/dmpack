! dmbot.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmbot
    !! This program is an XMPP bot for remote control of sensor nodes.
    use, intrinsic :: iso_c_binding
    use :: dmpack
    use :: xmpp
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmbot'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 6

    ! Application parameters.
    integer, parameter :: APP_PING_ID_LEN    = 32      !! Max. ping id length.
    integer, parameter :: APP_PING_INTERVAL  = 60      !! XMPP ping interval in seconds.
    logical, parameter :: APP_TCP_KEEP_ALIVE = .true.  !! Enable TCP Keep Alive.
    logical, parameter :: APP_TLS_TRUSTED    = .false. !! Trust unknown TLS certificate.

    ! Bot commands.
    integer, parameter :: BOT_COMMAND_NONE      = 0    !! Invalid command.
    integer, parameter :: BOT_COMMAND_BEATS     = 1    !! Return time in Swatch Internet Time (.beats).
    integer, parameter :: BOT_COMMAND_DATE      = 2    !! Return date and time.
    integer, parameter :: BOT_COMMAND_HELP      = 3    !! Return help text.
    integer, parameter :: BOT_COMMAND_JID       = 4    !! Return log message to logger.
    integer, parameter :: BOT_COMMAND_LOG       = 5    !! Return log message to logger.
    integer, parameter :: BOT_COMMAND_NODE      = 6    !! Return node id.
    integer, parameter :: BOT_COMMAND_POKE      = 7    !! Return bot response if online.
    integer, parameter :: BOT_COMMAND_RECONNECT = 8    !! Reconnect and reload roster.
    integer, parameter :: BOT_COMMAND_ROSTER    = 9    !! Return roster.
    integer, parameter :: BOT_COMMAND_UNAME     = 10   !! Return system name and version.
    integer, parameter :: BOT_COMMAND_UPTIME    = 11   !! Return system uptime.
    integer, parameter :: BOT_COMMAND_VERSION   = 12   !! Return bot version.
    integer, parameter :: BOT_NCOMMANDS         = 12   !! Number of commands.

    integer, parameter :: BOT_COMMAND_PREFIX_LEN = 1   !! Command prefix length.
    integer, parameter :: BOT_COMMAND_NAME_LEN   = 9   !! Max. command name length.
    integer, parameter :: BOT_COMMAND_LEN        = BOT_COMMAND_PREFIX_LEN + BOT_COMMAND_NAME_LEN

    character(len=BOT_COMMAND_PREFIX_LEN), parameter :: BOT_COMMAND_PREFIX = '!' !! Command prefix.
    character(len=BOT_COMMAND_NAME_LEN),   parameter :: BOT_COMMAND_NAMES(BOT_NCOMMANDS) = [ &
        character(len=BOT_COMMAND_NAME_LEN) :: &
        'beats', 'date', 'help', 'jid', 'log', 'node', 'poke', 'reconnect', 'roster', &
        'uname', 'uptime', 'version' &
    ] !! Command names.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)              :: name      = APP_NAME    !! Name of instance/configuration/resource.
        character(len=FILE_PATH_LEN)       :: config    = ' '         !! Path to config file.
        character(len=LOGGER_NAME_LEN)     :: logger    = ' '         !! Name of logger.
        character(len=NODE_ID_LEN)         :: node      = ' '         !! Node id.
        character(len=FILE_PATH_LEN)       :: log_db    = ' '         !! Path to log database.
        character(len=FILE_PATH_LEN)       :: observ_db = ' '         !! Path to observ database.
        character(len=JABBER_HOST_LEN)     :: host      = ' '         !! Domain of XMPP server.
        integer                            :: port      = JABBER_PORT !! Port of XMPP server.
        logical                            :: tls       = .true.      !! Force TLS encryption.
        character(len=JABBER_JID_LEN)      :: jid       = ' '         !! JID of bot account.
        character(len=JABBER_PASSWORD_LEN) :: password  = ' '         !! Password of bot account.
        logical                            :: reconnect = .false.     !! Reconnect on error.
        logical                            :: debug     = .false.     !! Forward debug messages via IPC.
        logical                            :: verbose   = .false.     !! Print debug messages to stderr.
    end type app_type

    type :: bot_type
        !! User data to be passed to libstrophe callbacks.
        character(len=ID_LEN)          :: name      = APP_NAME !! Name of instance/configuration.
        character(len=NODE_ID_LEN)     :: node_id   = ' '      !! Node id.
        character(len=APP_PING_ID_LEN) :: ping_id   = ' '      !! XMPP ping id (XEP-0199).
        character(len=FILE_PATH_LEN)   :: log_db    = ' '      !! Path to log database.
        character(len=FILE_PATH_LEN)   :: observ_db = ' '      !! Path to observ database.
        logical                        :: reconnect = .false.  !! Reconnection flag.
        type(jabber_type)              :: jabber               !! Jabber context type.
        type(roster_type)              :: roster               !! JID roster.
    end type bot_type

    class(logger_class), pointer :: logger ! Logger object.

    integer                :: rc  ! Return code.
    type(app_type)         :: app ! App settings.
    type(bot_type), target :: bot ! Bot type.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments and configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,                 & ! Name of logger process.
                          node_id = app%node,                   & ! Node id.
                          source  = app%name,                   & ! Log source.
                          debug   = app%debug,                  & ! Forward DEBUG messages via IPC.
                          ipc     = (len_trim(app%logger) > 0), & ! Enable IPC.
                          verbose = app%verbose)                  ! Print logs to standard error.

    ! Initialise environment.
    init_block: block
        call dm_jabber_init()
        call logger%info('started ' // APP_NAME)

        do
            ! Initialise bot context.
            call bot_init(bot, app)

            ! Create libstrophe context.
            rc = dm_jabber_create(bot%jabber)

            if (dm_is_error(rc)) then
                call logger%error('failed to create libstrophe context', error=rc)
                exit init_block
            end if

            ! Connect to XMPP server.
            rc = dm_jabber_connect(jabber       = bot%jabber, &
                                   host         = app%host, &
                                   port         = app%port, &
                                   jid          = app%jid, &
                                   password     = app%password, &
                                   callback     = connection_callback, &
                                   user_data    = c_loc(bot), &
                                   resource     = app%name, &
                                   keep_alive   = APP_TCP_KEEP_ALIVE, &
                                   tls_required = app%tls, &
                                   tls_trusted  = APP_TLS_TRUSTED)

            if (dm_is_error(rc)) then
                call logger%error('failed to connect to ' // trim(app%host) // ':' // &
                                  dm_itoa(app%port), error=rc)
                exit init_block
            end if

            ! Register signal handler.
            call dm_signal_register(signal_callback)

            ! Run event loop of bot.
            call dm_jabber_run(bot%jabber)

            ! Preserve stream management state for reconnection.
            if (bot%reconnect) call dm_jabber_preserve_stream_management_state(bot%jabber)

            ! Disconnect and reconnect.
            call dm_jabber_disconnect(bot%jabber)
            if (.not. bot%reconnect) exit
        end do
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
        type(arg_type)                :: args(14)

        args = [ &
            arg_type('name',      short='n', type=ARG_TYPE_ID),       & ! -n, --name <id>
            arg_type('config',    short='c', type=ARG_TYPE_FILE),     & ! -c, --config <path>
            arg_type('logger',    short='l', type=ARG_TYPE_ID),       & ! -l, --logger <string>
            arg_type('node',      short='N', type=ARG_TYPE_ID),       & ! -N, --node <id>
            arg_type('log-db',    short='L', type=ARG_TYPE_DATABASE), & ! -L, --log-db <path>
            arg_type('observ-db', short='O', type=ARG_TYPE_DATABASE), & ! -O, --observ-db <path>
            arg_type('host',      short='H', type=ARG_TYPE_STRING),   & ! -H, --host <string>
            arg_type('port',      short='q', type=ARG_TYPE_INTEGER),  & ! -q, --port <n>
            arg_type('tls',       short='E', type=ARG_TYPE_LOGICAL),  & ! -E, --tls
            arg_type('jid',       short='J', type=ARG_TYPE_STRING),   & ! -J, --jid <string>
            arg_type('password',  short='P', type=ARG_TYPE_STRING),   & ! -P, --password <string>
            arg_type('reconnect', short='R', type=ARG_TYPE_LOGICAL),  & ! -R, --reconnect
            arg_type('debug',     short='D', type=ARG_TYPE_LOGICAL),  & ! -D, --debug
            arg_type('verbose',   short='V', type=ARG_TYPE_LOGICAL)   & ! -V, --verbose
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
        call dm_arg_get(args( 5), app%log_db)
        call dm_arg_get(args( 6), app%observ_db)
        call dm_arg_get(args( 7), app%host)
        call dm_arg_get(args( 8), app%port)
        call dm_arg_get(args( 9), app%tls)
        call dm_arg_get(args(10), app%jid)
        call dm_arg_get(args(11), app%password)
        call dm_arg_get(args(12), app%reconnect)
        call dm_arg_get(args(13), app%debug)
        call dm_arg_get(args(14), app%verbose)

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
            call dm_error_out(rc, 'missing jid')
            return
        end if

        if (len_trim(app%password) == 0) then
            call dm_error_out(rc, 'missing password')
            return
        end if

        if (len_trim(app%log_db) > 0 .and. .not. dm_file_exists(app%log_db)) then
            call dm_error_out(rc, 'log database does not exist')
            return
        end if

        if (len_trim(app%observ_db) > 0 .and. .not. dm_file_exists(app%observ_db)) then
            call dm_error_out(rc, 'observ database does not exist')
            return
        end if

        if (app%port == 0) app%port = JABBER_PORT

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_type) :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'logger',    app%logger)
            call dm_config_get(config, 'node',      app%node)
            call dm_config_get(config, 'log_db',    app%log_db)
            call dm_config_get(config, 'observ_db', app%observ_db)
            call dm_config_get(config, 'host',      app%host)
            call dm_config_get(config, 'port',      app%port)
            call dm_config_get(config, 'tls',       app%tls)
            call dm_config_get(config, 'jid',       app%jid)
            call dm_config_get(config, 'password',  app%password)
            call dm_config_get(config, 'reconnect', app%reconnect)
            call dm_config_get(config, 'debug',     app%debug)
            call dm_config_get(config, 'verbose',   app%verbose)
        end if

        call dm_config_close(config)
    end function read_config

    ! ******************************************************************
    ! SUBROUTINES.
    ! ******************************************************************
    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: stat

        stat = STOP_SUCCESS
        if (dm_is_error(error)) stat = STOP_FAILURE

        if (dm_jabber_is_connected(bot%jabber)) then
            call dm_jabber_send_presence(bot%jabber, JABBER_STANZA_TEXT_AWAY)
            call logger%debug('set presence to ' // JABBER_STANZA_TEXT_AWAY)

            call dm_jabber_disconnect(bot%jabber)
        end if

        call dm_jabber_destroy(bot%jabber)
        call dm_jabber_shutdown()
        call dm_stop(stat)
    end subroutine halt

    ! ******************************************************************
    ! CALLBACK PROCEDURES.
    ! ******************************************************************
    subroutine connection_callback(connection, event, error, stream_error, user_data) bind(c)
        !! C-interoperable connection handler called on connect and disconnect
        !! events. Must be passed to `dm_jabber_connect()`.
        type(c_ptr),               intent(in), value :: connection   !! xmpp_conn_t *
        integer(kind=c_int),       intent(in), value :: event        !! xmpp_conn_event_t
        integer(kind=c_int),       intent(in), value :: error        !! int
        type(xmpp_stream_error_t), intent(in)        :: stream_error !! xmpp_stream_error_t *
        type(c_ptr),               intent(in), value :: user_data    !! void *

        character(len=ID_LEN)      :: id
        integer                    :: stat
        type(c_ptr)                :: iq_stanza
        type(bot_type),    pointer :: bot
        type(jabber_type), pointer :: jabber

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, bot)
        jabber => bot%jabber

        if (event == XMPP_CONN_CONNECT) then
            ! Connected to server.
            call logger%debug('connected as ' // trim(jabber%jid_full) // ' to server ' // &
                              trim(jabber%host) // ':' // dm_itoa(jabber%port))

            ! Add handlers.
            call xmpp_handler_add(connection, iq_callback, '', 'iq', '', user_data)
            call xmpp_handler_add(connection, message_callback, '', 'message', '', user_data)
            call xmpp_timed_handler_add(connection, ping_callback, int(APP_PING_INTERVAL * 1000, kind=c_long), user_data)

            ! Set presence to online.
            call dm_jabber_send_presence(jabber, JABBER_STANZA_TEXT_ONLINE)
            call logger%debug('set presence to ' // JABBER_STANZA_TEXT_ONLINE)

            ! Get roster from server.
            id = dm_uuid4()
            iq_stanza = dm_jabber_create_iq_roster(jabber, id)
            call xmpp_id_handler_add(connection, roster_callback, id, user_data)
            call xmpp_send(connection, iq_stanza)
            stat = xmpp_stanza_release(iq_stanza)
        else
            ! Disconnected from server.
            call logger%debug('disconnected from ' // trim(jabber%host) // ':' // dm_itoa(jabber%port))
            call xmpp_timed_handler_delete(connection, ping_callback)
            call xmpp_handler_delete(connection, message_callback)
            call xmpp_handler_delete(connection, iq_callback)
            call dm_jabber_stop(jabber)
        end if
    end subroutine connection_callback

    function disconnect_callback(connection, user_data) bind(c)
        type(c_ptr), intent(in), value :: connection          !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: user_data           !! void *
        integer(kind=c_int)            :: disconnect_callback !! int

        disconnect_callback = 0
        call xmpp_disconnect(connection)
    end function disconnect_callback

    function iq_callback(connection, iq_stanza, user_data) bind(c)
        !! C-interoperable iq stanza handler for ping processing.
        type(c_ptr), intent(in), value :: connection  !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: iq_stanza   !! xmpp_stanza_t *
        type(c_ptr), intent(in), value :: user_data   !! void *
        integer(kind=c_int)            :: iq_callback !! int

        character(len=:), allocatable :: from, id, type
        integer                       :: stat
        type(c_ptr)                   :: ping_stanza, result_stanza
        type(bot_type), pointer       :: bot

        iq_callback = 0

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, bot)

        ! Get stanza attributes.
        from = xmpp_stanza_get_from(iq_stanza)
        id   = xmpp_stanza_get_id(iq_stanza)
        type = xmpp_stanza_get_type(iq_stanza)

        if (len(type) == 0 .or. len(id) == 0) return

        select case (type)
            case (JABBER_STANZA_TYPE_RESULT)
                if (id == bot%ping_id) bot%ping_id = ' '

            case (JABBER_STANZA_TYPE_GET)
                ping_stanza = xmpp_stanza_get_child_by_ns(iq_stanza, JABBER_STANZA_NS_PING)

                if (c_associated(ping_stanza)) then
                    call logger%debug('received ping from ' // from)
                    result_stanza = dm_jabber_create_iq_result(bot%jabber, id=id)
                else
                    result_stanza = dm_jabber_create_iq_error(bot%jabber, id=id, type=JABBER_STANZA_TYPE_CANCEL, &
                                                              condition=JABBER_STANZA_NAME_SERVICE_UNAVAILABLE)
                end if

                stat = xmpp_stanza_set_to(result_stanza, from)
                call xmpp_send(connection, result_stanza)
                stat = xmpp_stanza_release(result_stanza)

            case (JABBER_STANZA_TYPE_ERROR)
                ping_stanza = xmpp_stanza_get_child_by_ns(iq_stanza, JABBER_STANZA_NS_PING)

                if (c_associated(ping_stanza) .and. id == bot%ping_id) then
                    call xmpp_timed_handler_delete(connection, ping_callback)
                    bot%ping_id = ' '
                end if
        end select
    end function iq_callback

    function message_callback(connection, stanza, user_data) bind(c)
        !! C-interoperable message handler. Must be registered in
        !! `connection_callback()`.
        type(c_ptr), intent(in), value :: connection       !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: stanza           !! xmpp_stanza_t *
        type(c_ptr), intent(in), value :: user_data        !! void *
        integer(kind=c_int)            :: message_callback !! int

        character(len=:), allocatable :: from, reply, text, type
        integer                       :: stat
        type(c_ptr)                   :: body_stanza, reply_stanza
        type(bot_type), pointer       :: bot

        message_callback = 1

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, bot)

        ! Get stanza body.
        body_stanza = xmpp_stanza_get_child_by_name(stanza, JABBER_STANZA_NAME_BODY)
        if (.not. c_associated(body_stanza)) return

        ! Ignore error messages.
        type = xmpp_stanza_get_type(stanza)

        if (type == JABBER_STANZA_TYPE_ERROR) then
            call logger%warning('received error message', error=E_IO)
            return
        end if

        ! Get stanza attributes.
        text = xmpp_stanza_get_text(body_stanza)
        from = xmpp_stanza_get_from(stanza)

        ! Check if client is authorised.
        if (dm_jabber_roster_has(bot%roster, from)) then
            ! Dispatch message.
            reply = bot_dispatch(bot, from, text)
        else
            ! Deny access.
            reply = 'unauthorized'
            call logger%warning('unauthorized access by ' // from, error=E_PERM)
        end if

        ! Don't send empty reply.
        if (len(reply) == 0) return

        ! Create and send reply stanza.
        reply_stanza = xmpp_stanza_reply(stanza)

        if (.not. c_associated(reply_stanza)) then
            stat = xmpp_stanza_set_type(reply_stanza, JABBER_STANZA_TYPE_CHAT)
        end if

        stat = xmpp_message_set_body(reply_stanza, reply)
        call xmpp_send(connection, reply_stanza)
        call logger%debug('sent reply to ' // from)
    end function message_callback

    function ping_callback(connection, user_data) bind(c)
        !! https://xmpp.org/extensions/xep-0199.html
        type(c_ptr), intent(in), value :: connection    !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: user_data     !! void *
        integer(kind=c_int)            :: ping_callback !! int

        integer                 :: stat
        type(c_ptr)             :: iq_stanza
        type(bot_type), pointer :: bot

        ping_callback = 0

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, bot)

        if (len_trim(bot%ping_id) > 0) then
            ! Already sent (lost).
            call xmpp_disconnect(connection)
            return
        end if

        ! Send ping.
        bot%ping_id = dm_uuid4()
        iq_stanza   = dm_jabber_create_iq_ping(bot%jabber, bot%ping_id)

        call xmpp_send(connection, iq_stanza)
        stat = xmpp_stanza_release(iq_stanza)
    end function ping_callback

    function roster_callback(connection, stanza, user_data) bind(c)
        !! C-interoperable roster request handler.
        type(c_ptr), intent(in), value :: connection      !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: stanza          !! xmpp_stanza_t *
        type(c_ptr), intent(in), value :: user_data       !! void *
        integer(kind=c_int)            :: roster_callback !! int

        character(len=:), allocatable :: jid
        integer                       :: rc
        type(c_ptr)                   :: item_stanza, query_stanza
        type(bot_type), pointer       :: bot

        roster_callback = 0

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, bot)

        ! Check stanza type.
        if (xmpp_stanza_get_type(stanza) == JABBER_STANZA_TYPE_ERROR) then
            call logger%error('roster query failed', error=E_IO)
            return
        end if

        call logger%debug('received roster from server')

        ! Deallocate old roster.
        call dm_jabber_roster_destroy(bot%roster)

        ! Add all JIDs in stanza to roster.
        query_stanza = xmpp_stanza_get_child_by_name(stanza, JABBER_STANZA_NAME_QUERY)
        item_stanza  = xmpp_stanza_get_children(query_stanza)

        do while (c_associated(item_stanza))
            jid = xmpp_stanza_get_attribute(item_stanza, JABBER_STANZA_ATTR_JID)
            rc  = dm_jabber_roster_add(bot%roster, jid)

            item_stanza = xmpp_stanza_get_next(item_stanza)
        end do
    end function roster_callback

    subroutine signal_callback(signum) bind(c)
        !! Default POSIX signal handler of the program.
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        select case (signum)
            case default
                call logger%info('exit on signal ' // dm_signal_name(signum))
                call halt(E_NONE)
        end select
    end subroutine signal_callback

    ! ******************************************************************
    ! BOT PROCEDURES.
    ! ******************************************************************
    function bot_dispatch(bot, from, message) result(reply)
        !! Parses message string and returns the reply for the requested
        !! command.
        integer, parameter :: ARGUMENT_LEN = 512
        integer, parameter :: COMMAND_LEN  = BOT_COMMAND_PREFIX_LEN + BOT_COMMAND_NAME_LEN

        type(bot_type),   intent(inout) :: bot     !! Bot type.
        character(len=*), intent(in)    :: from    !! Client JID.
        character(len=*), intent(in)    :: message !! Message received from JID.
        character(len=:), allocatable   :: reply   !! Reply string.

        character(len=:), allocatable :: response
        character(len=ARGUMENT_LEN)   :: argument
        character(len=COMMAND_LEN)    :: command
        integer                       :: command_type
        integer                       :: i, j

        command_type = BOT_COMMAND_NONE

        if (len_trim(message) == 0) then
            reply = ''
            return
        end if

        ! Split message into command and argument.
        i = index(message, ' ')
        if (i == 0) i = len(message)
        j = i + 1

        command = dm_to_lower(message(:i))

        if (j < len(message)) then
            argument = trim(message(j:))
        else
            argument = ''
        end if

        ! Search roster for JID.
        if (command(:BOT_COMMAND_PREFIX_LEN) == BOT_COMMAND_PREFIX) then
            i = BOT_COMMAND_PREFIX_LEN + 1

            do j = 1, BOT_NCOMMANDS
                if (BOT_COMMAND_NAMES(j) /= command(i:)) cycle
                command_type = j
                exit
            end do
        end if

        if (command_type == BOT_COMMAND_NONE) then
            reply = 'unrecognized command (send !help for a list of all commands)'
            call logger%debug('received invalid command from ' // from)
            return
        end if

        call logger%debug('received command ' // trim(command) // ' from ' // from)

        select case (command_type)
            case (BOT_COMMAND_BEATS)
                response = bot_handle_beats()
            case (BOT_COMMAND_DATE)
                response = bot_handle_date()
            case (BOT_COMMAND_HELP)
                response = bot_handle_help()
            case (BOT_COMMAND_JID)
                response = bot_handle_jid(bot)
            case (BOT_COMMAND_LOG)
                response = bot_handle_log(bot, argument)
            case (BOT_COMMAND_NODE)
                response = bot_handle_node(bot)
            case (BOT_COMMAND_POKE)
                response = bot_handle_poke(bot)
            case (BOT_COMMAND_RECONNECT)
                response = bot_handle_reconnect(bot)
            case (BOT_COMMAND_ROSTER)
                response = bot_handle_roster(bot)
            case (BOT_COMMAND_UNAME)
                response = bot_handle_uname()
            case (BOT_COMMAND_UPTIME)
                response = bot_handle_uptime()
            case (BOT_COMMAND_VERSION)
                response = bot_handle_version()
        end select

        reply = trim(BOT_COMMAND_NAMES(command_type)) // ': ' // response
    end function bot_dispatch

    subroutine bot_init(bot, app)
        !! Initialises bot.
        type(bot_type), intent(inout) :: bot !! Bot type.
        type(app_type), intent(inout) :: app !! App type.

        bot%name      = app%name
        bot%node_id   = app%node
        bot%log_db    = app%log_db
        bot%observ_db = app%observ_db
        bot%reconnect = app%reconnect
    end subroutine bot_init

    ! ******************************************************************
    ! BOT COMMAND HANDLING FUNCTIONS.
    ! ******************************************************************
    function bot_handle_beats() result(response)
        !! Returns current time in Swatch Internet Time (.beats).
        character(len=:), allocatable :: response !! Response string.

        character(len=TIME_BEATS_LEN) :: beats
        integer                       :: rc

        rc = dm_time_to_beats(dm_time_now(), beats)
        response = trim(beats)
    end function bot_handle_beats

    function bot_handle_date() result(response)
        !! Returns current date and time in ISO 8601.
        character(len=:), allocatable :: response !! Response string.

        response = dm_time_now()
    end function bot_handle_date

    function bot_handle_help() result(response)
        !! Returns help text.
        character(len=*), parameter :: NL  = ASCII_LF
        character(len=*), parameter :: NL2 = ASCII_LF // ASCII_LF

        character(len=:), allocatable :: response !! Response string.

        response = 'you may enter one of the following commands'      // NL // &
                   '!beats     - return time in Swatch Internet Time' // NL // &
                   '!date      - return date and time in ISO 8601'    // NL // &
                   '!help      - return this help text'               // NL // &
                   '!jid       - return bot JID'                      // NL // &
                   '!log       - send log message to logger'          // NL // &
                   '!node      - return node id'                      // NL // &
                   '!poke      - return message if bot is online'     // NL // &
                   '!reconnect - reconnect to server'                 // NL // &
                   '!roster    - return list of authorized users'     // NL // &
                   '!uname     - return system name'                  // NL // &
                   '!uptime    - return system uptime'                // NL // &
                   '!version   - return bot version'
    end function bot_handle_help

    function bot_handle_jid(bot) result(response)
        !! Returns full JID of bot.
        type(bot_type), intent(inout) :: bot      !! Bot type.
        character(len=:), allocatable :: response !! Response string.

        response = '<' // trim(bot%jabber%jid_full) // '>'
    end function bot_handle_jid

    function bot_handle_log(bot, argument) result(response)
        !! Sends log message to logger.
        type(bot_type),   intent(inout) :: bot      !! Bot type.
        character(len=*), intent(in)    :: argument !! Command arguments.
        character(len=:), allocatable   :: response !! Response string.

        character(len=LOG_LEVEL_NAME_LEN) :: level
        character(len=LOG_MESSAGE_LEN)    :: message

        integer :: lvl, stat

        read (argument, *, iostat=stat) level, message

        if (stat /= 0) then
            response = 'missing arguments <level> "<message>"'
            return
        end if

        lvl = dm_log_level_from_string(level)

        if (.not. dm_log_is_valid(lvl)) then
            response = 'invalid level'
            return
        end if

        call logger%log(lvl, message, source=bot%name)
        response = 'sent ' // trim(LOG_LEVEL_NAMES_LOWER(lvl)) // ' message to ' // logger%get_name()
    end function bot_handle_log

    function bot_handle_node(bot) result(response)
        !! Returns node id.
        type(bot_type), intent(inout) :: bot      !! Bot type.
        character(len=:), allocatable :: response !! Response string.

        if (len_trim(bot%node_id) > 0) then
            response = trim(bot%node_id)
        else
            response = 'n/a'
        end if
    end function bot_handle_node

    function bot_handle_poke(bot) result(response)
        !! Returns awake message.
        type(bot_type), intent(inout) :: bot      !! Bot type.
        character(len=:), allocatable :: response !! Response string.

        if (len_trim(bot%name) > 0) then
            response = trim(bot%name)
        else
            response = APP_NAME
        end if

        response = response // ' is online'
    end function bot_handle_poke

    function bot_handle_reconnect(bot) result(response)
        !! Reconnects bot.
        type(bot_type), intent(inout) :: bot      !! Bot type.
        character(len=:), allocatable :: response !! Response string.

        bot%reconnect = .true.
        call xmpp_timed_handler_add(bot%jabber%connection, disconnect_callback, 500_c_long, c_null_ptr)
        response = 'bye'
   end function bot_handle_reconnect

    function bot_handle_roster(bot) result(response)
        !! Returns bot roster.
        type(bot_type), intent(inout) :: bot      !! Bot type.
        character(len=:), allocatable :: response !! Response string.

        integer :: i, n

        if (.not. allocated(bot%roster%jids)) then
            response = 'unavailable'
            return
        end if

        n = size(bot%roster%jids)

        if (n == 0) then
            response = 'empty'
            return
        end if

        response = '<' // trim(bot%roster%jids(1)) // '>'

        do i = 2, n
            response = response // ', <' // trim(bot%roster%jids(i)) // '>'
        end do
    end function bot_handle_roster

    function bot_handle_uname() result(response)
        !! Returns Unix name.
        character(len=:), allocatable :: response !! Response string.

        type(uname_type) :: uname

        call dm_system_uname(uname)
        response = trim(uname%system_name) // ' ' // &
                   trim(uname%node_name)   // ' ' // &
                   trim(uname%release)     // ' ' // &
                   trim(uname%version)     // ' ' // &
                   trim(uname%machine)
    end function bot_handle_uname

    function bot_handle_uptime() result(response)
        !! Returns system uptime.
        character(len=:), allocatable :: response !! Response string.

        integer(kind=r8)      :: seconds
        type(time_delta_type) :: uptime

        call dm_system_uptime(seconds)
        call dm_time_delta_from_seconds(uptime, seconds)
        response = dm_time_delta_to_string(uptime)
    end function bot_handle_uptime

    function bot_handle_version() result(response)
        !! Returns bot version.
        character(len=:), allocatable :: response !! Response string.

        response = dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
    end function bot_handle_version
end program dmbot
