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
    integer, parameter :: APP_PING_INTERVAL  = 60      !! XMPP ping interval in seconds.
    logical, parameter :: APP_TCP_KEEP_ALIVE = .true.  !! Enable TCP Keep Alive.
    logical, parameter :: APP_TLS_TRUSTED    = .false. !! Trust unknown TLS certificate.

    ! Bot commands.
    integer, parameter :: BOT_COMMAND_NONE      = 0    !! Invalid command.
    integer, parameter :: BOT_COMMAND_BEATS     = 1    !! Return time in Swatch Internet Time (.beats).
    integer, parameter :: BOT_COMMAND_CAMERA    = 2    !! Send camera image.
    integer, parameter :: BOT_COMMAND_DATE      = 3    !! Return date and time.
    integer, parameter :: BOT_COMMAND_HELP      = 4    !! Return help text.
    integer, parameter :: BOT_COMMAND_JID       = 5    !! Return JID of bot.
    integer, parameter :: BOT_COMMAND_LOG       = 6    !! Return log message to logger.
    integer, parameter :: BOT_COMMAND_NODE      = 7    !! Return node id.
    integer, parameter :: BOT_COMMAND_POKE      = 8    !! Return bot response if online.
    integer, parameter :: BOT_COMMAND_RECONNECT = 9    !! Reconnect.
    integer, parameter :: BOT_COMMAND_UNAME     = 10   !! Return system name and version.
    integer, parameter :: BOT_COMMAND_UPTIME    = 11   !! Return system uptime.
    integer, parameter :: BOT_COMMAND_VERSION   = 12   !! Return bot version.
    integer, parameter :: BOT_NCOMMANDS         = 12   !! Number of commands.

    integer, parameter :: BOT_COMMAND_NAME_LEN  = 9                        !! Max. command name length.
    integer, parameter :: BOT_COMMAND_LEN       = 1 + BOT_COMMAND_NAME_LEN !! Max. command length with prefix.

    character,                           parameter :: BOT_COMMAND_PREFIX = '!' !! Command prefix.
    character(len=BOT_COMMAND_NAME_LEN), parameter :: BOT_COMMAND_NAMES(BOT_NCOMMANDS) = [ &
        character(len=BOT_COMMAND_NAME_LEN) :: &
        'beats', 'camera', 'date', 'help', 'jid', 'log', 'node', 'poke', 'reconnect', &
        'uname', 'uptime', 'version' &
    ] !! Command names.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)          :: name      = APP_NAME !! Name of instance/configuration/resource.
        character(len=FILE_PATH_LEN)   :: config    = ' '      !! Path to config file.
        character(len=LOGGER_NAME_LEN) :: logger    = ' '      !! Name of logger.
        character(len=NODE_ID_LEN)     :: node_id   = ' '      !! Node id.
        logical                        :: debug     = .false.  !! Forward debug messages via IPC.
        logical                        :: verbose   = .false.  !! Print debug messages to stderr.
    end type app_type

    type :: bot_type
        !! User data to be passed to libstrophe callbacks.
        type(im_type)                               :: im                         !! IM context type.
        character(len=ID_LEN)                       :: name            = APP_NAME !! Bot name.
        character(len=NODE_ID_LEN)                  :: node_id         = ' '      !! Node id.
        character(len=IM_JID_LEN)                   :: jid             = ' '      !! JID of bot account.
        character(len=IM_PASSWORD_LEN)              :: password        = ' '      !! Password of bot account.
        character(len=IM_HOST_LEN)                  :: host            = ' '      !! Domain of XMPP server.
        integer                                     :: port            = IM_PORT  !! Port of XMPP server.
        logical                                     :: tls             = .true.   !! Force TLS encryption.
        logical                                     :: reconnect       = .false.  !! Reconnect on error.
        character(len=IM_ID_LEN)                    :: ping_id         = ' '      !! XMPP ping id (XEP-0199).
        character(len=FILE_PATH_LEN)                :: database_log    = ' '      !! Path to log database.
        character(len=FILE_PATH_LEN)                :: database_observ = ' '      !! Path to observation database.
        character(len=IM_JID_FULL_LEN), allocatable :: group(:)                   !! Authorised JIDs.
    end type bot_type

    type :: bot_upload_type
        !! HTTP upload type
        character(len=FILE_PATH_LEN) :: file_path    = ' '
        character(len=FILE_PATH_LEN) :: file_name    = ' '
        integer(kind=i8)             :: file_size    = 0_i8
        character(len=IM_URL_LEN)    :: url_get      = ' '
        character(len=IM_URL_LEN)    :: url_put      = ' '
        character(len=MIME_LEN)      :: content_type = ' '
        character(len=32)            :: auth         = ' '
        character(len=1024)          :: cookie       = ' '
        character(len=32)            :: expires      = ' '
    end type bot_upload_type

    class(logger_class), pointer :: logger ! Logger object.

    integer                :: rc  ! Return code.
    type(app_type)         :: app ! App settings.
    type(bot_type), target :: bot ! Bot type.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments and configuration file.
    rc = read_args(app, bot)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,                 & ! Name of logger process.
                          node_id = app%node_id,                & ! Node id.
                          source  = app%name,                   & ! Log source.
                          debug   = app%debug,                  & ! Forward debug messages via IPC.
                          ipc     = (len_trim(app%logger) > 0), & ! Enable IPC.
                          verbose = app%verbose)                  ! Print logs to standard error.

    ! Initialise environment.
    init_block: block
        call dm_im_init()
        call logger%info('started ' // APP_NAME)

        do
            ! Register signal handler.
            call dm_signal_register(signal_callback)

            ! Create libstrophe context.
            rc = dm_im_create(bot%im)

            if (dm_is_error(rc)) then
                call logger%error('failed to create libstrophe context', error=rc)
                exit init_block
            end if

            ! Connect to XMPP server.
            rc = dm_im_connect(im           = bot%im, &
                               host         = bot%host, &
                               port         = bot%port, &
                               jid          = bot%jid, &
                               password     = bot%password, &
                               callback     = connection_callback, &
                               user_data    = c_loc(bot), &
                               resource     = bot%name, &
                               keep_alive   = APP_TCP_KEEP_ALIVE, &
                               tls_required = bot%tls, &
                               tls_trusted  = APP_TLS_TRUSTED)

            if (dm_is_error(rc)) then
                call logger%error('failed to connect to ' // trim(bot%host) // ':' // &
                                  dm_itoa(bot%port), error=rc)
                exit init_block
            end if

            ! Check if authorisation is enabled.
            if (size(bot%group) == 0) then
                call logger%info('bot accepts requests from all clients (authorization is disabled)')
            end if

            ! Run event loop of bot.
            call dm_im_run(bot%im)

            ! Preserve stream management state for reconnection.
            if (bot%reconnect) call dm_im_preserve_stream_management_state(bot%im)

            ! Disconnect and reconnect.
            call dm_im_disconnect(bot%im)
            if (.not. bot%reconnect) exit
        end do
    end block init_block

    call halt(rc)
contains
    ! **************************************************************************
    ! FUNCTIONS.
    ! **************************************************************************
    integer function read_args(app, bot) result(rc)
        !! Reads command-line arguments and configuration from file (if
        !! `--config` is passed).
        type(app_type), intent(out) :: app !! App type.
        type(bot_type), intent(out) :: bot !! Bot type.

        type(arg_type) :: args(12)

        args = [ &
            arg_type('name',      short='n', type=ARG_TYPE_ID),       & ! -n, --name <id>
            arg_type('config',    short='c', type=ARG_TYPE_FILE),     & ! -c, --config <path>
            arg_type('logger',    short='l', type=ARG_TYPE_ID),       & ! -l, --logger <string>
            arg_type('node',      short='N', type=ARG_TYPE_ID),       & ! -N, --node <id>
            arg_type('jid',       short='J', type=ARG_TYPE_STRING),   & ! -J, --jid <string>
            arg_type('password',  short='P', type=ARG_TYPE_STRING),   & ! -P, --password <string>
            arg_type('host',      short='H', type=ARG_TYPE_STRING),   & ! -H, --host <string>
            arg_type('port',      short='q', type=ARG_TYPE_INTEGER),  & ! -q, --port <n>
            arg_type('tls',       short='E', type=ARG_TYPE_LOGICAL),  & ! -E, --tls
            arg_type('reconnect', short='R', type=ARG_TYPE_LOGICAL),  & ! -R, --reconnect
            arg_type('debug',     short='D', type=ARG_TYPE_LOGICAL),  & ! -D, --debug
            arg_type('verbose',   short='V', type=ARG_TYPE_LOGICAL)   & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%name)
        call dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app, bot)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call dm_arg_get(args( 3), app%logger)
        call dm_arg_get(args( 4), app%node_id)
        call dm_arg_get(args( 5), bot%jid)
        call dm_arg_get(args( 6), bot%password)
        call dm_arg_get(args( 7), bot%host)
        call dm_arg_get(args( 8), bot%port)
        call dm_arg_get(args( 9), bot%tls)
        call dm_arg_get(args(10), bot%reconnect)
        call dm_arg_get(args(11), app%debug)
        call dm_arg_get(args(12), app%verbose)

        ! Validate passed options.
        rc = E_INVALID

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (len_trim(bot%host) == 0) then
            call dm_error_out(rc, 'missing host')
            return
        end if

        if (bot%port < 0) then
            call dm_error_out(rc, 'invalid port')
            return
        end if

        if (len_trim(bot%jid) == 0) then
            call dm_error_out(rc, 'missing jid')
            return
        end if

        if (len_trim(bot%password) == 0) then
            call dm_error_out(rc, 'missing password')
            return
        end if

        if (len_trim(bot%database_log) > 0 .and. .not. dm_file_exists(bot%database_log)) then
            call dm_error_out(rc, 'log database does not exist')
            return
        end if

        if (len_trim(bot%database_observ) > 0 .and. .not. dm_file_exists(bot%database_observ)) then
            call dm_error_out(rc, 'observ database does not exist')
            return
        end if

        ! Additional bot settings.
        bot%node_id = app%node_id
        if (bot%port == 0) bot%port = IM_PORT
        if (.not. allocated(bot%group)) allocate (bot%group(0))

        rc = E_NONE
    end function read_args

    integer function read_config(app, bot) result(rc)
        !! Reads configuration from file.
        type(app_type), intent(inout) :: app !! App type.
        type(bot_type), intent(inout) :: bot !! Bot type.

        type(config_type) :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'logger',    app%logger)
            call dm_config_get(config, 'node',      app%node_id)
            call dm_config_get(config, 'jid',       bot%jid)
            call dm_config_get(config, 'password',  bot%password)
            call dm_config_get(config, 'host',      bot%host)
            call dm_config_get(config, 'port',      bot%port)
            call dm_config_get(config, 'tls',       bot%tls)
            call dm_config_get(config, 'reconnect', bot%reconnect)
            call dm_config_get(config, 'group',     bot%group)
            call dm_config_get(config, 'debug',     app%debug)
            call dm_config_get(config, 'verbose',   app%verbose)

            if (dm_is_ok(dm_config_field(config, 'db'))) then
                call dm_config_get(config, 'log',    bot%database_log)
                call dm_config_get(config, 'observ', bot%database_observ)
                call dm_config_remove(config)
            end if
        end if

        call dm_config_close(config)
    end function read_config

    ! **************************************************************************
    ! SUBROUTINES.
    ! **************************************************************************
    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: stat

        stat = STOP_SUCCESS
        if (dm_is_error(error)) stat = STOP_FAILURE

        if (dm_im_is_connected(bot%im)) then
            call dm_im_send_presence(bot%im, IM_STANZA_TEXT_AWAY)
            call logger%debug('set presence to ' // IM_STANZA_TEXT_AWAY)

            call dm_im_disconnect(bot%im)
        end if

        call dm_im_destroy(bot%im)
        call dm_im_shutdown()
        call dm_stop(stat)
    end subroutine halt

    ! **************************************************************************
    ! BOT PROCEDURES.
    ! **************************************************************************
    function bot_dispatch(bot, from, message) result(reply)
        !! Parses message string and returns the reply for the requested
        !! command.
        type(bot_type),   intent(inout) :: bot     !! Bot type.
        character(len=*), intent(in)    :: from    !! Client JID.
        character(len=*), intent(in)    :: message !! Message received from JID.
        character(len=:), allocatable   :: reply   !! Reply string.

        character(len=:), allocatable :: argument, command, output
        integer                       :: c

        ! Do not answer to empty messages.
        if (len_trim(message) == 0) then
            reply = ''
            return
        end if

        c = bot_parse_message(message, command, argument)

        if (c == BOT_COMMAND_NONE) then
            reply = 'unrecognized command (send !help for a list of all commands)'
            call logger%debug('received invalid command from ' // from)
            return
        end if

        call logger%debug('received command ' // command // ' from ' // from)

        select case (c)
            case (BOT_COMMAND_BEATS);     output = bot_response_beats()
            case (BOT_COMMAND_CAMERA);    output = bot_response_camera(bot)
            case (BOT_COMMAND_DATE);      output = bot_response_date()
            case (BOT_COMMAND_HELP);      output = bot_response_help()
            case (BOT_COMMAND_JID);       output = bot_response_jid(bot)
            case (BOT_COMMAND_LOG);       output = bot_response_log(bot, argument)
            case (BOT_COMMAND_NODE);      output = bot_response_node(bot)
            case (BOT_COMMAND_POKE);      output = bot_response_poke(bot)
            case (BOT_COMMAND_RECONNECT); output = bot_response_reconnect(bot)
            case (BOT_COMMAND_UNAME);     output = bot_response_uname()
            case (BOT_COMMAND_UPTIME);    output = bot_response_uptime()
            case (BOT_COMMAND_VERSION);   output = bot_response_version()
        end select

        reply = command // ': ' // output
    end function bot_dispatch

    logical function bot_is_authorized(group, jid) result(is)
        !! Returns `.true.` if JID is in group. If the group is empty, all JIDs
        !! are authorised!
        character(len=IM_JID_FULL_LEN), intent(inout) :: group(:) !! Group of authorised JIDs.
        character(len=*),               intent(in)    :: jid      !! JIDs to validate.

        integer :: i, j, n

        is = .false.

        ! All JIDs are authorised if the group is empty.
        if (size(group) == 0) then
            is = .true.
            return
        end if

        n = len_trim(jid)
        if (n == 0) return
        j = index(jid, '/') - 1
        if (j < 1) j = n

        do i = 1, size(group)
            if (jid(:j) == group(i)) then
                is = .true.
                exit
            end if
        end do
    end function bot_is_authorized

    integer function bot_parse_message(message, command, argument) result(c)
        !! Return bot command parsed from string or `BOT_COMMAND_NONE` on error.
        !! Optionally returns the command string in `command` and the argument
        !! string in `argument`.
        character(len=*),              intent(in)            :: message  !! Message to parse.
        character(len=:), allocatable, intent(out), optional :: command  !! Command string.
        character(len=:), allocatable, intent(out), optional :: argument !! Argument string.

        character(len=BOT_COMMAND_NAME_LEN) :: name
        integer                             :: i, j, n

        c = BOT_COMMAND_NONE

        parse_block: block
            ! Split message into command and argument.
            i = index(message, BOT_COMMAND_PREFIX)
            j = index(message, ' ')
            n = len(message)

            if (j == 0) j = n
            if (i == 0 .or. i >= j .or. i == n) exit parse_block

            name = dm_to_lower(message(i + 1:j))
            if (present(command)) command = trim(name)
            if (present(argument) .and. j < n) argument = adjustl(trim(message(j:)))

            ! Search for command name.
            do i = 1, BOT_NCOMMANDS
                if (name == BOT_COMMAND_NAMES(i)) then
                    c = i
                    exit parse_block
                end if
            end do
        end block parse_block

        if (present(command)) then
            if (.not. allocated(command)) command = ''
        end if

        if (present(argument)) then
            if (.not. allocated(argument)) argument = ''
        end if
    end function bot_parse_message

    ! **************************************************************************
    ! BOT COMMAND HANDLING FUNCTIONS.
    ! **************************************************************************
    function bot_response_beats() result(output)
        !! Returns current time in Swatch Internet Time (.beats).
        character(len=:), allocatable :: output !! Response string.

        character(len=TIME_BEATS_LEN) :: beats
        integer                       :: rc

        rc = dm_time_to_beats(dm_time_now(), beats)
        output = trim(beats)
    end function bot_response_beats

    function bot_response_camera(bot) result(output)
        !! Sends camera image.
        type(bot_type), intent(inout) :: bot    !! Bot type.
        character(len=:), allocatable :: output !! Response string.

        ! character(len=:), allocatable :: content_type, file_name
        ! character(len=ID_LEN)         :: id
        ! integer(kind=i8)              :: file_size
        ! type(c_ptr)                   :: iq_stanza

        output = ''

        ! id = dm_uuid4()
        ! iq_stanza = dm_im_create_iq_http_upload(bot%im, id, file_name, file_size, content_type)
    end function bot_response_camera

    function bot_response_date() result(output)
        !! Returns current date and time in ISO 8601.
        character(len=:), allocatable :: output !! Response string.

        output = dm_time_now()
    end function bot_response_date

    function bot_response_help() result(output)
        !! Returns help text.
        character(len=:), allocatable :: output !! Response string.

        output = 'you may enter one of the following commands:'     // ASCII_LF // &
                 '!beats     - return time in Swatch Internet Time' // ASCII_LF // &
                 '!date      - return date and time in ISO 8601'    // ASCII_LF // &
                 '!help      - return this help text'               // ASCII_LF // &
                 '!jid       - return bot JID'                      // ASCII_LF // &
                 '!log       - send log message to logger'          // ASCII_LF // &
                 '!node      - return node id'                      // ASCII_LF // &
                 '!poke      - return message if bot is online'     // ASCII_LF // &
                 '!reconnect - reconnect to server'                 // ASCII_LF // &
                 '!uname     - return system name'                  // ASCII_LF // &
                 '!uptime    - return system uptime'                // ASCII_LF // &
                 '!version   - return bot version'
    end function bot_response_help

    function bot_response_jid(bot) result(output)
        !! Returns full JID of bot.
        type(bot_type), intent(inout) :: bot    !! Bot type.
        character(len=:), allocatable :: output !! Response string.

        output = '<' // trim(bot%im%jid_full) // '>'
    end function bot_response_jid

    function bot_response_log(bot, argument) result(output)
        !! Sends log message to logger.
        type(bot_type),   intent(inout) :: bot      !! Bot type.
        character(len=*), intent(in)    :: argument !! Command arguments.
        character(len=:), allocatable   :: output   !! Response string.

        character(len=LOG_LEVEL_NAME_LEN) :: level
        character(len=LOG_MESSAGE_LEN)    :: message

        integer :: lvl, stat

        read (argument, *, iostat=stat) level, message

        if (stat /= 0) then
            output = 'missing arguments <level> "<message>"'
            return
        end if

        lvl = dm_log_level_from_string(level)

        if (.not. dm_log_level_is_valid(lvl)) then
            output = 'invalid level'
            return
        end if

        call logger%log(lvl, message, source=bot%name)
        output = 'sent ' // trim(LOG_LEVEL_NAMES_LOWER(lvl)) // ' message to ' // logger%get_name()
    end function bot_response_log

    function bot_response_node(bot) result(output)
        !! Returns node id.
        type(bot_type), intent(inout) :: bot    !! Bot type.
        character(len=:), allocatable :: output !! Response string.

        integer :: n

        n = len_trim(bot%node_id)

        if (n > 0) then
            output = bot%node_id(:n)
        else
            output = 'n/a'
        end if
    end function bot_response_node

    function bot_response_poke(bot) result(output)
        !! Returns awake message.
        type(bot_type), intent(inout) :: bot    !! Bot type.
        character(len=:), allocatable :: output !! Response string.

        integer :: n

        n = len_trim(bot%name)

        if (n > 0) then
            output = bot%name(:n)
        else
            output = APP_NAME
        end if

        output = output // ' is online'
    end function bot_response_poke

    function bot_response_reconnect(bot) result(output)
        !! Reconnects bot.
        type(bot_type), intent(inout) :: bot    !! Bot type.
        character(len=:), allocatable :: output !! Response string.

        bot%reconnect = .true.
        call xmpp_timed_handler_add(bot%im%connection, disconnect_callback, 500_c_long, c_null_ptr)
        output = 'bye'
   end function bot_response_reconnect

    function bot_response_uname() result(output)
        !! Returns Unix name.
        character(len=:), allocatable :: output !! Response string.

        type(uname_type) :: uname

        call dm_system_uname(uname)
        output = trim(uname%system_name) // ' ' // &
                 trim(uname%node_name)   // ' ' // &
                 trim(uname%release)     // ' ' // &
                 trim(uname%version)     // ' ' // &
                 trim(uname%machine)
    end function bot_response_uname

    function bot_response_uptime() result(output)
        !! Returns system uptime.
        character(len=:), allocatable :: output !! Response string.

        integer(kind=r8)      :: seconds
        type(time_delta_type) :: uptime

        call dm_system_uptime(seconds)
        call dm_time_delta_from_seconds(uptime, seconds)
        output = dm_time_delta_to_string(uptime)
    end function bot_response_uptime

    function bot_response_version() result(output)
        !! Returns bot version.
        character(len=:), allocatable :: output !! Response string.

        output = dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, library=.true.)
    end function bot_response_version

    subroutine http_upload(upload)
        use :: curl
        use :: unix

        type(bot_upload_type), intent(inout) :: upload

        integer     :: stat
        type(c_ptr) :: curl_ctx, list_ctx
        type(c_ptr) :: fh

        fh = c_fopen(trim(upload%file_path) // c_null_char, 'rb' // c_null_char)
        if (.not. c_associated(fh)) return

        stat = curl_global_init(CURL_GLOBAL_ALL)

        curl_ctx = curl_easy_init()
        list_ctx = c_null_ptr

        stat = curl_easy_setopt(curl_ctx, CURLOPT_URL,              trim(upload%url_put))
        stat = curl_easy_setopt(curl_ctx, CURLOPT_CUSTOMREQUEST,    'PUT')
        stat = curl_easy_setopt(curl_ctx, CURLOPT_UPLOAD,           1)
        stat = curl_easy_setopt(curl_ctx, CURLOPT_READDATA,         fh)
        stat = curl_easy_setopt(curl_ctx, CURLOPT_INFILESIZE_LARGE, upload%file_size)

        list_ctx = curl_slist_append(list_ctx, 'Content-Type: ' // trim(upload%content_type))

        if (len_trim(upload%auth) > 0) then
            list_ctx = curl_slist_append(list_ctx, 'Authorization: ' // trim(upload%auth))
        end if

        if (len_trim(upload%cookie) > 0) then
            list_ctx = curl_slist_append(list_ctx, 'Cookie: ' // trim(upload%cookie))
        end if

        if (len_trim(upload%expires) > 0) then
            list_ctx = curl_slist_append(list_ctx, 'Expires: ' // trim(upload%expires))
        end if

        stat = curl_easy_setopt(curl_ctx, CURLOPT_HTTPHEADER, list_ctx)
        stat = curl_easy_setopt(curl_ctx, CURLOPT_NOSIGNAL,   1)
        stat = curl_easy_setopt(curl_ctx, CURLOPT_VERBOSE,    0)
        stat = curl_easy_setopt(curl_ctx, CURLOPT_USERAGENT,  dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, library=.true.))

        stat = curl_easy_perform(curl_ctx)

        call curl_slist_free_all(list_ctx)
        call curl_easy_cleanup(curl_ctx)
        call curl_global_cleanup()

        if (c_associated(fh)) stat = c_fclose(fh)
    end subroutine http_upload

    ! **************************************************************************
    ! CALLBACK PROCEDURES.
    ! **************************************************************************
    recursive subroutine connection_callback(connection, event, error, stream_error, user_data) bind(c)
        !! C-interoperable connection handler called on connect and disconnect
        !! events. Must be passed to `dm_im_connect()`.
        type(c_ptr),               intent(in), value :: connection   !! xmpp_conn_t *
        integer(kind=c_int),       intent(in), value :: event        !! xmpp_conn_event_t
        integer(kind=c_int),       intent(in), value :: error        !! int
        type(xmpp_stream_error_t), intent(in)        :: stream_error !! xmpp_stream_error_t *
        type(c_ptr),               intent(in), value :: user_data    !! void *

        type(bot_type), pointer :: bot
        type(im_type),  pointer :: im

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, bot)

        im => bot%im

        if (event == XMPP_CONN_CONNECT) then
            ! Connected to server.
            call logger%debug('connected as ' // trim(im%jid_full) // ' to server ' // &
                              trim(im%host) // ':' // dm_itoa(im%port))

            ! Add handlers.
            call xmpp_handler_add(connection, ping_response_callback, IM_STANZA_NS_PING, IM_STANZA_NAME_IQ,      '', user_data)
            call xmpp_handler_add(connection, message_callback,       '',                IM_STANZA_NAME_MESSAGE, '', user_data)

            ! Add timed handlers.
            call xmpp_timed_handler_add(connection, ping_callback, int(APP_PING_INTERVAL * 1000, kind=c_long), user_data)

            ! Set presence to online.
            call dm_im_send_presence(im, IM_STANZA_TEXT_ONLINE)
            call logger%debug('set presence to ' // IM_STANZA_TEXT_ONLINE)
        else
            ! Disconnected from server.
            call logger%debug('disconnected from ' // trim(im%host) // ':' // dm_itoa(im%port))
            call xmpp_timed_handler_delete(connection, ping_callback)
            call xmpp_handler_delete(connection, message_callback)
            call xmpp_handler_delete(connection, ping_response_callback)
            call dm_im_stop(im)
        end if
    end subroutine connection_callback

    function disconnect_callback(connection, user_data) bind(c)
        type(c_ptr), intent(in), value :: connection          !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: user_data           !! void *
        integer(kind=c_int)            :: disconnect_callback !! int

        disconnect_callback = 0
        call xmpp_disconnect(connection)
    end function disconnect_callback

    function http_upload_response_callback(stanza, user_data) bind(c)
        !! C-interoperable HTTP upload response callback.
        type(c_ptr), intent(in), value :: stanza                        !! xmpp_stanza_t *
        type(c_ptr), intent(in), value :: user_data                     !! void *
        integer(kind=c_int)            :: http_upload_response_callback !! int

        character(len=:), allocatable  :: from, header_name, type
        type(c_ptr)                    :: get_stanza, header_stanza, put_stanza, slot_stanza
        type(bot_upload_type), pointer :: upload

        http_upload_response_callback = 0

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, upload)

        from = xmpp_stanza_get_from(stanza)
        type = xmpp_stanza_get_type(stanza)

        if (type == IM_STANZA_TYPE_ERROR) return

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
                            case (IM_STANZA_HEADER_AUTHORIZATION); upload%auth    = xmpp_stanza_get_text(header_stanza)
                            case (IM_STANZA_HEADER_COOKIE);        upload%cookie  = xmpp_stanza_get_text(header_stanza)
                            case (IM_STANZA_HEADER_EXPIRES);       upload%expires = xmpp_stanza_get_text(header_stanza)
                        end select
                    end if

                    header_stanza = xmpp_stanza_get_next(header_stanza)
                end do

                ! start HTTP upload here ...
            else
                http_upload_response_callback = 1
            end if
        end if
    end function http_upload_response_callback

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
        body_stanza = xmpp_stanza_get_child_by_name(stanza, IM_STANZA_NAME_BODY)
        if (.not. c_associated(body_stanza)) return

        ! Ignore error messages.
        type = xmpp_stanza_get_type(stanza)

        if (type == IM_STANZA_TYPE_ERROR) then
            call logger%warning('received error stanza', error=E_IO)
            return
        end if

        ! Get stanza attributes.
        text = xmpp_stanza_get_text(body_stanza)
        from = xmpp_stanza_get_from(stanza)

        ! Check if client is authorised.
        if (bot_is_authorized(bot%group, from)) then
            reply = bot_dispatch(bot, from, text)
        else
            reply = 'unauthorized'
            call logger%warning('unauthorized access by ' // from, error=E_PERM)
        end if

        ! Don't send empty reply.
        if (len(reply) == 0) return

        ! Create and send reply stanza.
        reply_stanza = xmpp_stanza_reply(stanza)

        if (.not. c_associated(reply_stanza)) then
            stat = xmpp_stanza_set_type(reply_stanza, IM_STANZA_TYPE_CHAT)
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
        iq_stanza   = dm_im_create_iq_ping(bot%im, bot%ping_id)

        call xmpp_send(connection, iq_stanza)
        stat = xmpp_stanza_release(iq_stanza)
    end function ping_callback

    function ping_response_callback(connection, iq_stanza, user_data) bind(c)
        !! C-interoperable iq stanza handler for ping processing.
        type(c_ptr), intent(in), value :: connection             !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: iq_stanza              !! xmpp_stanza_t *
        type(c_ptr), intent(in), value :: user_data              !! void *
        integer(kind=c_int)            :: ping_response_callback !! int

        character(len=:), allocatable :: from, id, type
        integer                       :: stat
        type(c_ptr)                   :: result_stanza
        type(bot_type), pointer       :: bot

        ping_response_callback = 0

        if (.not. c_associated(user_data)) return
        call c_f_pointer(user_data, bot)

        ! Get stanza attributes.
        from = xmpp_stanza_get_from(iq_stanza)
        id   = xmpp_stanza_get_id(iq_stanza)
        type = xmpp_stanza_get_type(iq_stanza)

        if (len(type) == 0 .or. len(id) == 0) return

        select case (type)
            case (IM_STANZA_TYPE_RESULT)
                if (id == bot%ping_id) bot%ping_id = ' '

            case (IM_STANZA_TYPE_GET)
                call logger%debug('received ping from ' // from)
                result_stanza = dm_im_create_iq_result(bot%im, id=id)

                stat = xmpp_stanza_set_to(result_stanza, from)
                call xmpp_send(connection, result_stanza)
                stat = xmpp_stanza_release(result_stanza)

            case (IM_STANZA_TYPE_ERROR)
                if (id == bot%ping_id) then
                    call xmpp_timed_handler_delete(connection, ping_callback)
                    bot%ping_id = ' '
                end if
        end select
    end function ping_response_callback

    subroutine signal_callback(signum) bind(c)
        !! Default POSIX signal handler of the program.
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        select case (signum)
            case default
                call logger%info('exit on signal ' // dm_signal_name(signum))
                call halt(E_NONE)
        end select
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 1x, a)', dm_lua_version(.true.), dm_db_version(.true.)
    end subroutine version_callback
end program dmbot
