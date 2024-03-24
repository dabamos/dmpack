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
    integer,          parameter :: APP_PATCH = 3

    logical, parameter :: APP_RPC_DEFLATE = .true. !! Compress RPC data.

    integer, parameter :: HOST_LEN     = 256 !! Max. length of host name.
    integer, parameter :: USERNAME_LEN = 256 !! Max. length of user name.
    integer, parameter :: PASSWORD_LEN = 256 !! Max. length of password.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)          :: name     = APP_NAME !! Name of instance/configuration.
        character(len=FILE_PATH_LEN)   :: config   = ' '      !! Path to configuration file.
        character(len=LOGGER_NAME_LEN) :: logger   = ' '      !! Name of logger (name implies IPC).
        character(len=NODE_ID_LEN)     :: node     = ' '      !! Sensor node id (required).
        character(len=HOST_LEN)        :: host     = ' '      !! IP or FQDN of API (`127.0.0.1`, `example.com`).
        integer                        :: port     = 0        !! API port (set to 0 for protocol default).
        logical                        :: tls      = .false.  !! TLS encryption.
        character(len=USERNAME_LEN)    :: username = ' '      !! HTTP Basic Auth user name.
        character(len=PASSWORD_LEN)    :: password = ' '      !! HTTP Basic Auth password.
        integer                        :: count    = 0        !! Maximum number of heartbeats to send (0 means unlimited).
        integer                        :: interval = 60       !! Emit interval in seconds (>= 0).
        logical                        :: debug    = .false.  !! Forward debug messages via IPC.
        logical                        :: ipc      = .false.  !! Send logs via IPC (requires logger name to be set).
        logical                        :: verbose  = .false.  !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App configuration.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line options and configuration from file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Initialise logger.
    logger => dm_logger_get()
    call logger%configure(name    = app%logger, &
                          node_id = app%node, &
                          source  = app%name, &
                          debug   = app%debug, &
                          ipc     = app%ipc, &
                          verbose = app%verbose)

    ! Initialise RPC backend.
    rc = dm_rpc_init()

    if (dm_is_error(rc)) then
        call logger%error('failed to initialize libcurl', error=rc)
        call dm_stop(1)
    end if

    ! Run main loop.
    call dm_signal_register(signal_handler)
    call run(app)

    ! Clean-up.
    call dm_rpc_destroy()
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(inout) :: app !! App type.
        type(arg_type)                :: args(13)

        rc = E_NONE

        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <id>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),      & ! -l, --logger <id>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('host',     short='H', type=ARG_TYPE_STRING),  & ! -H, --host <string>
            arg_type('port',     short='p', type=ARG_TYPE_INTEGER), & ! -p, --port <n>
            arg_type('tls',      short='X', type=ARG_TYPE_LOGICAL), & ! -X, --tls
            arg_type('username', short='U', type=ARG_TYPE_STRING),  & ! -U, --username <string>
            arg_type('password', short='P', type=ARG_TYPE_STRING),  & ! -P, --password <string>
            arg_type('count',    short='C', type=ARG_TYPE_INTEGER), & ! -C, --count <n>
            arg_type('interval', short='I', type=ARG_TYPE_INTEGER), & ! -I, --interval <n>
            arg_type('debug',    short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('verbose',  short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        rc = dm_arg_get(args( 3), app%logger)
        rc = dm_arg_get(args( 4), app%node)
        rc = dm_arg_get(args( 5), app%host)
        rc = dm_arg_get(args( 6), app%port)
        rc = dm_arg_get(args( 7), app%tls)
        rc = dm_arg_get(args( 8), app%username)
        rc = dm_arg_get(args( 9), app%password)
        rc = dm_arg_get(args(10), app%count)
        rc = dm_arg_get(args(11), app%interval)
        rc = dm_arg_get(args(12), app%debug)
        rc = dm_arg_get(args(13), app%verbose)

        rc = E_INVALID

        if (len_trim(app%logger) > 0) then
            if (.not. dm_id_valid(app%logger)) then
                call dm_error_out(rc, 'invalid logger name')
                return
            end if

            app%ipc = .true.
        end if

        if (.not. dm_id_valid(app%node)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (len_trim(app%host) == 0) then
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
            rc = dm_config_get(config, 'logger',   app%logger)
            rc = dm_config_get(config, 'node',     app%node)
            rc = dm_config_get(config, 'host',     app%host)
            rc = dm_config_get(config, 'port',     app%port)
            rc = dm_config_get(config, 'tls',      app%tls)
            rc = dm_config_get(config, 'username', app%username)
            rc = dm_config_get(config, 'password', app%password)
            rc = dm_config_get(config, 'count',    app%count)
            rc = dm_config_get(config, 'interval', app%interval)
            rc = dm_config_get(config, 'debug',    app%debug)
            rc = dm_config_get(config, 'verbose',  app%verbose)
            rc = E_NONE
        end if

        call dm_config_close(config)
    end function read_config

    subroutine run(app)
        !! Runs main loop to emit heartbeats.
        type(app_type), intent(inout) :: app !! App type.

        character(len=BEAT_CLIENT_LEN) :: client ! Client name and version.
        character(len=:), allocatable  :: url    ! URL of HTTP-RPC API endpoint.

        integer          :: last_error
        integer          :: i, rc, stat, t
        integer(kind=i8) :: uptime
        logical          :: has_api_status

        type(api_status_type)   :: api_status
        type(beat_type)         :: beat
        type(rpc_request_type)  :: request
        type(rpc_response_type) :: response
        type(timer_type)        :: timer

        call logger%info('started ' // app%name)

        ! Client and library version.
        client = dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, library=.true.)

        ! Create URL of RPC service.
        url = dm_rpc_url(host     = app%host, &
                         port     = app%port, &
                         endpoint = RPC_ROUTE_BEAT, &
                         tls      = app%tls)

        last_error = E_NONE
        i = 0

        emit_loop: do
            call dm_timer_start(timer)
            call logger%debug('emitting beat for node ' // trim(app%node) // ' to host ' // app%host)

            ! Create new heartbeat.
            beat = beat_type(node_id   = app%node, &
                             client    = client, &
                             time_sent = dm_time_now(), &
                             interval  = app%interval, &
                             error     = last_error)

            ! Get system uptime.
            call dm_system_uptime(uptime, rc)
            beat%uptime = int(uptime, kind=i4)

            ! Send RPC request to API.
            rc = dm_rpc_send(request  = request, &
                             response = response, &
                             type     = beat, &
                             username = app%username, &
                             password = app%password, &
                             deflate  = APP_RPC_DEFLATE, &
                             url      = url)

            if (dm_is_error(rc)) call logger%debug('failed to send beat to host ' // app%host, error=rc)
            has_api_status = .false.

            if (response%content_type == MIME_TEXT) then
                stat = dm_api_status_from_string(response%payload, api_status)
                has_api_status = dm_is_ok(stat)
            end if

            select case (response%code)
                case (0)
                    rc = E_RPC_CONNECT
                    call logger%warning('connection to host ' // trim(app%host) // ' failed: ' // &
                                        response%error_message, error=rc)

                case (HTTP_CREATED)
                    rc = E_NONE
                    call logger%debug('beat accepted by host ' // app%host, error=rc)

                case (HTTP_UNAUTHORIZED)
                    rc = E_RPC_AUTH
                    if (has_api_status) then
                        call logger%error('unauthorized access on host ' // trim(app%host) // ': ' // &
                                          api_status%message, error=rc)
                    else
                        call logger%error('unauthorized access on host ' // app%host, error=rc)
                    end if

                case (HTTP_INTERNAL_SERVER_ERROR)
                    rc = E_RPC_SERVER
                    call logger%error('internal server error on host ' // app%host, error=rc)

                case (HTTP_BAD_GATEWAY)
                    rc = E_RPC_CONNECT
                    call logger%error('bad gateway on host ' // app%host, error=rc)

                case default
                    ! Log response from API status if available.
                    if (has_api_status) then
                        rc = api_status%error
                        call logger%warning('server error on host ' // trim(app%host) // ' (HTTP ' // &
                                            dm_itoa(response%code) // '): ' // api_status%message, error=rc)
                    else
                        rc = E_RPC_API
                        call logger%warning('API call to host ' // trim(app%host) // ' failed (HTTP ' // &
                                            dm_itoa(response%code) // ')', error=rc)
                    end if
            end select

            last_error = rc

            if (app%count > 0) then
                i = i + 1
                if (i >= app%count) exit emit_loop
            end if

            call dm_timer_stop(timer)
            t = max(0, int(app%interval - dm_timer_result(timer)))
            call logger%debug('next beat in ' // dm_itoa(t) // ' sec')
            call dm_sleep(t)
        end do emit_loop

        call logger%debug('finished transmission')
    end subroutine run

    subroutine signal_handler(signum) bind(c)
        !! C-interoperable signal handler that closes database, removes message
        !! queue, and stops program.
        use, intrinsic :: iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        select case (signum)
            case default
                call logger%info('exit on signal ' // dm_itoa(signum))
                call dm_rpc_destroy()
                call dm_stop(0)
        end select
    end subroutine signal_handler
end program dmbeat
