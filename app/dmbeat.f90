! dmbeat.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmbeat
    !! Heartbeat emitter, sends status messages to the RPC API.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmbeat'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 8

    integer, parameter :: APP_HOST_LEN     = 256 !! Max. length of host name.
    integer, parameter :: APP_USERNAME_LEN = 256 !! Max. length of user name.
    integer, parameter :: APP_PASSWORD_LEN = 256 !! Max. length of password.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)           :: name             = APP_NAME    !! Name of instance/configuration.
        character(len=FILE_PATH_LEN)    :: config           = ' '         !! Path to configuration file.
        character(len=LOGGER_NAME_LEN)  :: logger           = ' '         !! Name of logger (name implies IPC).
        character(len=NODE_ID_LEN)      :: node_id          = ' '         !! Sensor node id (required).
        character(len=APP_HOST_LEN)     :: host             = ' '         !! IP or FQDN of API (`127.0.0.1`, `example.com`).
        integer                         :: port             = 0           !! API port (set to 0 for protocol default).
        logical                         :: tls              = .false.     !! TLS encryption.
        character(len=APP_USERNAME_LEN) :: username         = ' '         !! HTTP Basic Auth user name.
        character(len=APP_PASSWORD_LEN) :: password         = ' '         !! HTTP Basic Auth password.
        character(len=Z_TYPE_NAME_LEN)  :: compression_name = 'zstd'      !! Compression library (`none`, `zlib`, `zstd`).
        integer                         :: compression      = Z_TYPE_NONE !! Compression type (`Z_TYPE_*`).
        integer                         :: count            = 0           !! Maximum number of heartbeats to send (0 means unlimited).
        integer                         :: interval         = 60          !! Emit interval in seconds (>= 0).
        logical                         :: debug            = .false.     !! Forward debug messages via IPC.
        logical                         :: verbose          = .false.     !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    call dm_init()

    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,                & ! Name of logger process.
                          node_id = app%node_id,               & ! Node id.
                          source  = app%name,                  & ! Log source.
                          debug   = app%debug,                 & ! Forward DEBUG messages via IPC.
                          ipc     = dm_string_has(app%logger), & ! Enable IPC.
                          verbose = app%verbose)                 ! Print logs to standard error.

    init_block: block
        rc = dm_rpc_init()

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize libcurl', error=rc)
            exit init_block
        end if

        call dm_signal_register(signal_callback)
        call run(app, error=rc)
    end block init_block

    call halt(rc)
contains
    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: stat

        stat = dm_btoi(dm_is_error(error), STOP_FAILURE, STOP_SUCCESS)
        call dm_rpc_shutdown()
        call dm_stop(stat)
    end subroutine halt

    subroutine run(app, error)
        !! Runs main loop to emit heartbeats.
        type(app_type), intent(inout)         :: app   !! App type.
        integer,        intent(out), optional :: error !! Error code.

        character(len=BEAT_CLIENT_LEN) :: client
        character(len=LOG_MESSAGE_LEN) :: message
        character(len=:), allocatable  :: url

        integer          :: iter, rc, rc_last, stat
        integer          :: msec, sec
        integer(kind=i8) :: uptime
        logical          :: has_api_status

        type(api_status_type)   :: api_status
        type(beat_type)         :: beat
        type(rpc_request_type)  :: request
        type(rpc_response_type) :: response
        type(timer_type)        :: timer

        if (present(error)) error = E_INVALID

        call logger%info('started ' // APP_NAME)
        call logger%debug('beat transmission interval: ' // dm_itoa(app%interval))

        if (app%compression == Z_TYPE_NONE) then
            call logger%debug('compression is disabled')
        else
            call logger%debug(dm_z_type_name(app%compression) // ' compression is enabled')
        end if

        client  = dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, library=.true.)
        rc_last = E_NONE

        ! Initialise heartbeat.
        call dm_beat_set(beat, node_id=app%node_id, client=client, interval=app%interval)

        ! Create URL of RPC service.
        url = dm_rpc_url(host=app%host, port=app%port, endpoint=RPC_ROUTE_BEAT, tls=app%tls)
        if (.not. dm_string_has(url)) return

        call logger%debug('sending beats to API endpoint ' // url)
        iter = 1

        emit_loop: do
            call dm_timer_start(timer)
            if (app%count > 0) call logger%debug('starting transmission ' // dm_itoa(iter) // '/' // dm_itoa(app%count))

            ! Update heartbeat attributes.
            call dm_system_uptime(uptime, stat)
            call dm_beat_set(beat, time_sent=dm_time_now(), error=rc_last, uptime=int(uptime))

            ! Send RPC request to API, use compression if available.
            rc = dm_rpc_post(request     = request,      &
                             response    = response,     &
                             type        = beat,         &
                             url         = url,          &
                             username    = app%username, &
                             password    = app%password, &
                             user_agent  = client,       &
                             compression = app%compression)

            if (dm_is_error(rc)) call logger%debug('failed to send beat to host ' // app%host, error=rc)

            ! Read API status response from payload.
            has_api_status = .false.

            if (response%content_type == MIME_TEXT) then
                stat = dm_api_status_from_string(response%payload, api_status)
                has_api_status = dm_is_ok(stat)
            end if

            if (response%code > 0) then
                call logger%debug('server answered with status HTTP ' // dm_itoa(response%code) // ' ' // dm_http_status_string(response%code))
            end if

            ! Log the HTTP response code.
            select case (response%code)
                case (HTTP_NONE)
                    rc = E_RPC_CONNECT
                    call logger%warning('connection to host ' // trim(app%host) // ' failed: ' // response%error_message, error=rc)

                case (HTTP_CREATED)
                    rc = E_NONE
                    call logger%debug('beat accepted by host ' // app%host, error=rc)

                case (HTTP_UNAUTHORIZED)
                    rc = E_RPC_AUTH
                    message = 'unauthorized access on host ' // app%host
                    if (has_api_status) message = trim(message) //  ': ' // api_status%message
                    call logger%error(message, error=rc)

                case (HTTP_INTERNAL_SERVER_ERROR)
                    rc = E_RPC_SERVER
                    call logger%error('internal server error on host ' // app%host, error=rc)

                case (HTTP_BAD_GATEWAY)
                    rc = E_RPC_CONNECT
                    call logger%error('bad gateway on host ' // app%host, error=rc)

                case default
                    rc = E_RPC_API
                    write (message, '("API call to host ", a, " failed (HTTP ", i0, ")")') trim(app%host), response%code

                    if (has_api_status) then
                        rc = api_status%error
                        message = trim(message) // ': ' // api_status%message
                    end if

                    call logger%warning(message, error=rc)
            end select

            rc_last = rc

            if (app%count > 0) then
                call logger%debug('finished transmission ' // dm_itoa(iter) // '/' // dm_itoa(app%count))
                iter = iter + 1
                if (iter > app%count) exit emit_loop
            end if

            call dm_timer_stop(timer)
            msec = max(0, int(1000 * (app%interval - dm_timer_result(timer))))
            sec  = dm_msec_to_sec(msec)
            call logger%debug('next beat in ' // dm_itoa(sec) // ' sec')
            call dm_msleep(msec)
        end do emit_loop

        call logger%debug('finished transmission')
        if (present(error)) error = E_NONE
    end subroutine run

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app !! App type.

        type(arg_type) :: args(14)

        args = [ &
            arg_type('name',        short='n', type=ARG_TYPE_ID),      & ! -n, --name <id>
            arg_type('config',      short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',      short='l', type=ARG_TYPE_ID),      & ! -l, --logger <id>
            arg_type('node',        short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('host',        short='H', type=ARG_TYPE_STRING),  & ! -H, --host <string>
            arg_type('port',        short='q', type=ARG_TYPE_INTEGER), & ! -q, --port <n>
            arg_type('tls',         short='E', type=ARG_TYPE_LOGICAL), & ! -E, --tls
            arg_type('username',    short='U', type=ARG_TYPE_STRING),  & ! -U, --username <string>
            arg_type('password',    short='P', type=ARG_TYPE_STRING),  & ! -P, --password <string>
            arg_type('compression', short='x', type=ARG_TYPE_STRING),  & ! -x, --compression <name>
            arg_type('count',       short='C', type=ARG_TYPE_INTEGER), & ! -C, --count <n>
            arg_type('interval',    short='I', type=ARG_TYPE_INTEGER), & ! -I, --interval <n>
            arg_type('debug',       short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('verbose',     short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%name)
        call dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call dm_arg_get(args( 3), app%logger)
        call dm_arg_get(args( 4), app%node_id)
        call dm_arg_get(args( 5), app%host)
        call dm_arg_get(args( 6), app%port)
        call dm_arg_get(args( 7), app%tls)
        call dm_arg_get(args( 8), app%username)
        call dm_arg_get(args( 9), app%password)
        call dm_arg_get(args(10), app%compression_name)
        call dm_arg_get(args(11), app%count)
        call dm_arg_get(args(12), app%interval)
        call dm_arg_get(args(13), app%debug)
        call dm_arg_get(args(14), app%verbose)

        if (dm_string_has(app%compression_name)) app%compression = dm_z_type_from_name(app%compression_name)

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_type) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'logger',      app%logger)
            call dm_config_get(config, 'node',        app%node_id)
            call dm_config_get(config, 'host',        app%host)
            call dm_config_get(config, 'port',        app%port)
            call dm_config_get(config, 'tls',         app%tls)
            call dm_config_get(config, 'username',    app%username)
            call dm_config_get(config, 'password',    app%password)
            call dm_config_get(config, 'compression', app%compression_name)
            call dm_config_get(config, 'count',       app%count)
            call dm_config_get(config, 'interval',    app%interval)
            call dm_config_get(config, 'debug',       app%debug)
            call dm_config_get(config, 'verbose',     app%verbose)
        end if

        call dm_config_close(config)
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger name')
            return
        end if

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (.not. dm_string_has(app%host)) then
            call dm_error_out(rc, 'invalid or missing host')
            return
        end if

        if (app%count < 0) then
            call dm_error_out(rc, 'invalid count')
            return
        end if

        if (app%interval < 0) then
            call dm_error_out(rc, 'invalid interval')
            return
        end if

        if (.not. dm_z_is_valid(app%compression)) then
            call dm_error_out(rc, 'invalid compression')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine signal_callback(signum) bind(c)
        !! C-interoperable signal handler that stops the program.
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        call logger%info('exit on signal ' // dm_signal_name(signum))
        call halt(E_NONE)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 2(1x, a))', dm_rpc_version(), dm_lua_version(.true.), dm_zstd_version(.true.)
    end subroutine version_callback
end program dmbeat
