! dmbeat.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmbeat
    !! Heartbeat emitter, sends status messages to the RPC API.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmbeat'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    integer, parameter :: APP_HOST_LEN     = 256 !! Max. length of host name.
    integer, parameter :: APP_USERNAME_LEN = 256 !! Max. length of user name.
    integer, parameter :: APP_PASSWORD_LEN = 256 !! Max. length of password.

    type :: app_type
        !! Application settings.
        character(ID_LEN)           :: name             = APP_NAME    !! Name of instance/configuration.
        character(FILE_PATH_LEN)    :: config           = ' '         !! Path to configuration file.
        character(LOGGER_NAME_LEN)  :: logger           = ' '         !! Name of logger (name implies IPC).
        character(NODE_ID_LEN)      :: node_id          = ' '         !! Sensor node id (required).
        character(APP_HOST_LEN)     :: host             = ' '         !! IP or FQDN of API (`127.0.0.1`, `example.com`).
        integer                     :: port             = 0           !! API port (set to 0 for protocol default).
        logical                     :: tls              = .false.     !! TLS encryption.
        character(APP_USERNAME_LEN) :: username         = ' '         !! HTTP Basic Auth user name.
        character(APP_PASSWORD_LEN) :: password         = ' '         !! HTTP Basic Auth password.
        character(Z_TYPE_NAME_LEN)  :: compression_name = 'zstd'      !! Compression library (`none`, `zlib`, `zstd`).
        integer                     :: compression      = Z_TYPE_NONE !! Compression type (`Z_TYPE_*`).
        integer                     :: count            = 0           !! Maximum number of heartbeats to send (0 means unlimited).
        integer                     :: interval         = 60          !! Emit interval in seconds (>= 0).
        logical                     :: debug            = .false.     !! Forward debug messages via IPC.
        logical                     :: verbose          = .false.     !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer                 :: rc     ! Return code.
    type(app_type)          :: app    ! App settings.
    type(posix_signal_type) :: signal ! Self-pipe.

    call dm_init()

    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,  & ! Name of logger process.
                          node_id = app%node_id, & ! Node id.
                          source  = app%name,    & ! Log source.
                          debug   = app%debug,   & ! Forward DEBUG messages via IPC.
                          ipc     = .true.,      & ! Enable IPC (if logger is set).
                          verbose = app%verbose)   ! Print logs to standard error.

    rc = init(signal)
    if (dm_is_error(rc)) call shutdown(rc)

    rc = run(app, signal)
    call shutdown(rc)
contains
    integer function init(signal) result(rc)
        type(posix_signal_type), intent(out) :: signal !! Self-pipe.

        ! Create self-pipe and register signal handler.
        signal_block: block
            rc = dm_posix_signal_create(signal)
            if (dm_is_error(rc)) exit signal_block

            rc = dm_posix_signal_register_all(signal_callback)
        end block signal_block

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize signal handler', error=rc)
            return
        end if

        call logger%debug('initialized signal handler')
    end function init

    integer function run(app, signal) result(rc)
        !! Runs main loop to emit heartbeats.
        type(app_type),          intent(in)    :: app    !! App settings.
        type(posix_signal_type), intent(inout) :: signal !! Self-pipe.

        character(BEAT_CLIENT_LEN) :: client
        character(LOG_MESSAGE_LEN) :: message
        character(:), allocatable  :: url

        integer     :: iter, number, rc_last, stat
        integer     :: msec, sec
        integer(i8) :: uptime
        logical     :: has_api_status

        type(api_status_type)   :: api_status
        type(beat_type)         :: beat
        type(rpc_request_type)  :: request
        type(rpc_response_type) :: response
        type(timer_type)        :: timer

        rc = dm_rpc_init()

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize RPC backend', error=rc)
            return
        end if

        call logger%status('started ' // APP_NAME)
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

        rc = E_INVALID
        if (.not. dm_string_has(url)) return

        call logger%debug('sending beats to API endpoint ' // url)
        iter = 1

        main_loop: do
            call dm_timer_start(timer)

            ! Poll for signals.
            rc = dm_posix_signal_poll(signal, timeout=0)

            if (rc == E_INTERRUPT) cycle main_loop

            if (dm_is_error(rc)) then
                call logger%error('failed to poll signal pipe', error=rc)
                exit main_loop
            end if

            if (dm_posix_signal_should_stop(signal, number)) then
                call logger%debug('exit on signal ' // dm_posix_signal_name(number))
                exit main_loop
            end if

            if (app%count > 0) call logger%debug('starting transmission ' // dm_itoa(iter) // '/' // dm_itoa(app%count))

            ! Update heartbeat attributes.
            call dm_posix_uptime(uptime)
            call dm_beat_set(beat, time_sent=dm_time_now(), error=rc_last, uptime=int(uptime))

            ! Send RPC request to API, use compression if available.
            rc = dm_rpc_post(request, response, beat, url=url, username=app%username, password=app%password, user_agent=client, compression=app%compression)
            if (dm_is_error(rc)) call logger%debug('failed to send beat to host ' // app%host, error=rc)

            ! Read API status response from payload.
            has_api_status = .false.

            if (response%content_type == MIME_TEXT) then
                call dm_api_status_from_string(response%payload, api_status, error=stat)
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
                    write (message, '("API call to host ", a, " failed (HTTP ", i0, ")")', iostat=stat) trim(app%host), response%code

                    if (has_api_status) then
                        rc = api_status%error
                        message = trim(message) // ': ' // api_status%message
                    end if

                    call logger%warning(message, error=rc)
            end select

            rc_last = rc
            call dm_rpc_reset(response)

            if (app%count > 0) then
                call logger%debug('finished transmission ' // dm_itoa(iter) // '/' // dm_itoa(app%count))
                iter = iter + 1
                if (iter > app%count) exit main_loop
            end if

            ! Wait before next iteration.
            call dm_timer_stop(timer)
            msec = max(0, int(1000 * (app%interval - dm_timer_result(timer))))
            sec  = dm_msec_to_sec(msec)

            call logger%debug('next beat in ' // dm_itoa(sec) // ' sec')
            call dm_posix_msleep(msec)
        end do main_loop

        call dm_rpc_destroy(request)
        call dm_rpc_destroy(response)
        call dm_rpc_shutdown()

        rc = E_NONE
        call logger%debug('finished transmission')
    end function run

    subroutine shutdown(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        call dm_posix_signal_destroy(signal)
        call logger%status('stopped ' // APP_NAME, error=error)
        call dm_stop(merge(STOP_FAILURE, STOP_SUCCESS, dm_is_error(error)))
    end subroutine shutdown

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app !! App type.

        type(arg_parser_class) :: parser

        call parser%add('name',        short='n', type=ARG_TYPE_ID)      ! -n, --name <id>
        call parser%add('config',      short='c', type=ARG_TYPE_FILE)    ! -c, --config <path>
        call parser%add('logger',      short='l', type=ARG_TYPE_ID)      ! -l, --logger <id>
        call parser%add('node',        short='N', type=ARG_TYPE_ID)      ! -N, --node <id>
        call parser%add('host',        short='H', type=ARG_TYPE_STRING)  ! -H, --host <string>
        call parser%add('port',        short='q', type=ARG_TYPE_INTEGER) ! -q, --port <n>
        call parser%add('tls',         short='E', type=ARG_TYPE_LOGICAL) ! -E, --tls
        call parser%add('username',    short='U', type=ARG_TYPE_STRING)  ! -U, --username <string>
        call parser%add('password',    short='P', type=ARG_TYPE_STRING)  ! -P, --password <string>
        call parser%add('compression', short='x', type=ARG_TYPE_STRING)  ! -x, --compression <name>
        call parser%add('count',       short='C', type=ARG_TYPE_INTEGER) ! -C, --count <n>
        call parser%add('interval',    short='I', type=ARG_TYPE_INTEGER) ! -I, --interval <n>
        call parser%add('debug',       short='D', type=ARG_TYPE_LOGICAL) ! -D, --debug
        call parser%add('verbose',     short='V', type=ARG_TYPE_LOGICAL) ! -V, --verbose

        ! Read all command-line arguments.
        rc = parser%read(version_callback)
        if (dm_is_error(rc)) return

        call parser%get('name',   app%name)
        call parser%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call parser%get('logger',      app%logger)
        call parser%get('node',        app%node_id)
        call parser%get('host',        app%host)
        call parser%get('port',        app%port)
        call parser%get('tls',         app%tls)
        call parser%get('username',    app%username)
        call parser%get('password',    app%password)
        call parser%get('compression', app%compression_name)
        call parser%get('count',       app%count)
        call parser%get('interval',    app%interval)
        call parser%get('debug',       app%debug)
        call parser%get('verbose',     app%verbose)

        if (dm_string_has(app%compression_name)) app%compression = dm_z_type_from_name(app%compression_name)

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('logger',      app%logger)
            call config%get('node',        app%node_id)
            call config%get('host',        app%host)
            call config%get('port',        app%port)
            call config%get('tls',         app%tls)
            call config%get('username',    app%username)
            call config%get('password',    app%password)
            call config%get('compression', app%compression_name)
            call config%get('count',       app%count)
            call config%get('interval',    app%interval)
            call config%get('debug',       app%debug)
            call config%get('verbose',     app%verbose)
        end if

        call config%close()
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(in) :: app !! App type.

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
    ! CALLBACKS
    ! **************************************************************************
    subroutine signal_callback(number) bind(c)
        integer(c_int), intent(in), value :: number

        call dm_posix_signal_write(signal, number)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 2(1x, a))', dm_rpc_version(), dm_lua_version(.true.), dm_zstd_version(.true.)
    end subroutine version_callback
end program dmbeat
