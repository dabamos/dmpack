! dmsync.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmsync
    !! Observation and log database synchronisation program.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmsync'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 8

    integer, parameter :: APP_DB_MAX_NATTEMPTS = 10                 !! Max. number of database insert attempts.
    integer, parameter :: APP_DB_TIMEOUT       = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout [msec].
    integer, parameter :: APP_SYNC_LIMIT       = 10                 !! Max. number of records to sync at once.

    integer, parameter :: HOST_LEN     = 256 !! Max. length of host.
    integer, parameter :: USERNAME_LEN = 256 !! Max. length of user name.
    integer, parameter :: PASSWORD_LEN = 256 !! Max. length of password.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)          :: name             = APP_NAME       !! Name of instance/configuration.
        character(len=FILE_PATH_LEN)   :: config           = ' '            !! Path to configuration file.
        character(len=LOGGER_NAME_LEN) :: logger           = ' '            !! Name of logger.
        character(len=SEM_NAME_LEN)    :: wait             = ' '            !! Name of POSIX semaphore to wait for (without leading `/`).
        character(len=NODE_ID_LEN)     :: node_id          = ' '            !! Node id.
        character(len=FILE_PATH_LEN)   :: database         = ' '            !! Path to database.
        character(len=HOST_LEN)        :: host             = ' '            !! IP or FQDN of API.
        integer                        :: port             = 0              !! HTTP port of API (0 selects port automatically).
        character(len=USERNAME_LEN)    :: username         = ' '            !! HTTP Basic Auth user name.
        character(len=PASSWORD_LEN)    :: password         = ' '            !! HTTP Basic Auth password.
        character(len=Z_TYPE_NAME_LEN) :: compression_name = 'zstd'         !! Compression library (`none`, `zlib`, `zstd`).
        character(len=TYPE_NAME_LEN)   :: type_name        = ' '            !! Database record type string.
        integer                        :: compression      = Z_TYPE_NONE    !! Compression type (`Z_TYPE_*`).
        integer                        :: type             = SYNC_TYPE_NONE !! Database record type.
        integer                        :: interval         = 0              !! Sync interval in seconds.
        logical                        :: create           = .false.        !! Create synchronisation tables.
        logical                        :: debug            = .false.        !! Forward debug messages via IPC.
        logical                        :: ipc              = .false.        !! Watch named semaphore for synchronisation.
        logical                        :: tls              = .false.        !! Use TLS encryption.
        logical                        :: verbose          = .false.        !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer              :: rc  ! Return code.
    type(app_type)       :: app ! App settings.
    type(db_type)        :: db  ! Database type.
    type(sem_named_type) :: sem ! POSIX semaphore type.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line options and configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,  & ! Name of logger process.
                          node_id = app%node_id, & ! Node id.
                          source  = app%name,    & ! Log source.
                          debug   = app%debug,   & ! Forward DEBUG messages via IPC.
                          ipc     = .true.,      & ! Enable IPC (if logger is set).
                          verbose = app%verbose)   ! Print logs to standard error.

    ! Initialise environment.
    rc = init(app, db, sem)
    if (dm_is_error(rc)) call halt(rc)

    ! Start synchronisation.
    rc = run(app, db, sem)
    call halt(rc)
contains
    integer function init(app, db, sem) result(rc)
        !! Initialises environment.
        type(app_type),       intent(inout) :: app !! App type.
        type(db_type),        intent(inout) :: db  !! Database type.
        type(sem_named_type), intent(inout) :: sem !! Semaphore type.

        ! Open SQLite database.
        rc = dm_db_open(db, app%database, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call logger%error('failed to open database', error=rc)
            return
        end if

        ! Create synchronisation tables.
        if (app%create) then
            select case (app%type)
                case (SYNC_TYPE_LOG)
                    rc = dm_db_table_create_sync_logs(db)
                case (SYNC_TYPE_NODE, SYNC_TYPE_OBSERV, SYNC_TYPE_SENSOR, SYNC_TYPE_TARGET)
                    rc = dm_db_table_create_sync_observs(db)
            end select

            if (dm_is_error(rc)) then
                call logger%error('failed to create database table', error=rc)
                return
            end if
        end if

        ! Check if tables exist.
        select case (app%type)
            case (SYNC_TYPE_LOG)
                if (.not. dm_db_table_has_logs(db) .or. &
                    .not. dm_db_table_has_sync_logs(db)) rc = E_NOT_FOUND
            case (SYNC_TYPE_NODE, SYNC_TYPE_OBSERV, SYNC_TYPE_SENSOR, SYNC_TYPE_TARGET)
                if (.not. dm_db_table_has_observs(db) .or. &
                    .not. dm_db_table_has_sync_observs(db)) rc = E_NOT_FOUND
        end select

        if (dm_is_error(rc)) then
            call logger%error('database tables not found', error=rc)
            return
        end if

        ! Open semaphore.
        if (app%ipc) then
            rc = dm_sem_open(sem, app%wait)

            if (dm_is_error(rc)) then
                call logger%error('failed to open semaphore /' // app%wait, error=rc)
                return
            end if
        end if

        ! Initialise RPC backend.
        rc = dm_rpc_init()

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize libcurl', error=rc)
            return
        end if

        ! Register signal handler.
        call dm_signal_register(signal_callback)
    end function init

    integer function run(app, db, sem) result(rc)
        !! Synchronises logs database via RPC API.
        type(app_type),       intent(inout) :: app !! App configuration type.
        type(db_type),        intent(inout) :: db  !! Database type.
        type(sem_named_type), intent(inout) :: sem !! Semaphore type.

        character(len=LOG_MESSAGE_LEN) :: message
        character(len=:), allocatable  :: name, url

        integer          :: i, j, n, stat
        integer          :: msec, sec
        integer(kind=i8) :: limit, nsyncs
        logical          :: debug, has_auth
        real(kind=r8)    :: dt
        type(timer_type) :: sync_timer
        type(timer_type) :: rpc_timer

        character(len=ID_LEN)   :: ids(APP_SYNC_LIMIT)       ! Derived type ids.
        type(rpc_request_type)  :: requests(APP_SYNC_LIMIT)  ! HTTP-RPC requests.
        type(rpc_response_type) :: responses(APP_SYNC_LIMIT) ! HTTP-RPC responses.

        type(log_type),    allocatable :: logs(:)
        type(observ_type), allocatable :: observs(:)
        type(node_type),   allocatable :: nodes(:)
        type(sensor_type), allocatable :: sensors(:)
        type(target_type), allocatable :: targets(:)
        type(sync_type),   allocatable :: syncs(:)

        name     = dm_sync_name(app%type)
        limit    = APP_SYNC_LIMIT
        has_auth = (dm_string_has(app%username) .and. dm_string_has(app%password))
        debug    = (app%debug .or. app%verbose)

        call logger%info('started ' // APP_NAME)

        if (app%compression == Z_TYPE_NONE) then
            call logger%debug('compression of ' // name // ' data disabled')
        else
            call logger%debug(dm_z_type_name(app%compression) // ' compression of ' // name // ' data enabled')
        end if

        ! Allocate type arrays.
        rc = E_ALLOC
        select case (app%type)
            case (SYNC_TYPE_LOG);    allocate (logs   (APP_SYNC_LIMIT), stat=stat)
            case (SYNC_TYPE_NODE);   allocate (nodes  (APP_SYNC_LIMIT), stat=stat)
            case (SYNC_TYPE_OBSERV); allocate (observs(APP_SYNC_LIMIT), stat=stat)
            case (SYNC_TYPE_SENSOR); allocate (sensors(APP_SYNC_LIMIT), stat=stat)
            case (SYNC_TYPE_TARGET); allocate (targets(APP_SYNC_LIMIT), stat=stat)
        end select

        if (stat /= 0) return

        ! Generate URL of HTTP-RPC API endpoint.
        rc = E_CORRUPT
        select case (app%type)
            case (SYNC_TYPE_LOG);    url = dm_rpc_url(app%host, app%port, endpoint=RPC_ROUTE_LOG,    tls=app%tls)
            case (SYNC_TYPE_NODE);   url = dm_rpc_url(app%host, app%port, endpoint=RPC_ROUTE_NODE,   tls=app%tls)
            case (SYNC_TYPE_OBSERV); url = dm_rpc_url(app%host, app%port, endpoint=RPC_ROUTE_OBSERV, tls=app%tls)
            case (SYNC_TYPE_SENSOR); url = dm_rpc_url(app%host, app%port, endpoint=RPC_ROUTE_SENSOR, tls=app%tls)
            case (SYNC_TYPE_TARGET); url = dm_rpc_url(app%host, app%port, endpoint=RPC_ROUTE_TARGET, tls=app%tls)
        end select

        if (.not. dm_string_has(url)) return

        ! Prepare requests (will be re-used).
        do i = 1, APP_SYNC_LIMIT
            call dm_rpc_request_set(request     = requests(i),     &
                                    compression = app%compression, &
                                    url         = url,             &
                                    user_agent  = dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, library=.true.))
            if (has_auth) call dm_rpc_request_set(requests(i), auth=RPC_AUTH_BASIC, username=app%username, password=app%password)
        end do

        ! Main synchronisation loop.
        sync_loop: do
            if (.not. app%ipc) then
                ! Start interval timer.
                call dm_timer_start(sync_timer)
            else
                ! Wait for semaphore.
                if (debug) call logger%debug('waiting for semaphore ' // app%wait)
                rc = dm_sem_wait(sem)

                if (dm_is_error(rc)) then
                    ! Unrecoverable semaphore error. Stop program.
                    call logger%error('failed to wait for semaphore ' // app%wait, error=rc)
                    exit sync_loop
                end if
            end if

            ! Fetch sync records up to the limit `APP_SYNC_LIMIT`. Argument
            ! `nsyncs` will store the total number of records in database
            ! pending for synchronisation, and may be greater than the limit.
            if (allocated(syncs)) deallocate (syncs)

            select case (app%type)
                case (SYNC_TYPE_LOG);    rc = dm_db_select_sync_logs   (db, syncs, nsyncs, limit)
                case (SYNC_TYPE_NODE);   rc = dm_db_select_sync_nodes  (db, syncs, nsyncs, limit)
                case (SYNC_TYPE_OBSERV); rc = dm_db_select_sync_observs(db, syncs, nsyncs, limit)
                case (SYNC_TYPE_SENSOR); rc = dm_db_select_sync_sensors(db, syncs, nsyncs, limit)
                case (SYNC_TYPE_TARGET); rc = dm_db_select_sync_targets(db, syncs, nsyncs, limit)
            end select

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                ! Unrecoverable database error. Stop program.
                call logger%error('failed to select sync data from database', error=rc)
                exit sync_loop
            end if

            ! Read the data records to synchronise from database.
            if (debug) call logger%debug(dm_itoa(nsyncs) // ' ' // name // 's pending for sync found in database')
            n = size(syncs)

            do i = 1, n
                associate (id => ids(i), sync => syncs(i), log => logs(i), node => nodes(i), &
                           observ => observs(i), sensor => sensors(i), target => targets(i))
                    select case (sync%type)
                        case (SYNC_TYPE_LOG)
                            rc = dm_db_select(db, log, sync%id)
                            id = log%id
                        case (SYNC_TYPE_NODE)
                            rc = dm_db_select(db, node, sync%id)
                            id = node%id
                        case (SYNC_TYPE_OBSERV)
                            rc = dm_db_select(db, observ, sync%id)
                            id = observ%id
                        case (SYNC_TYPE_SENSOR)
                            rc = dm_db_select(db, sensor, sync%id)
                            id = sensor%id
                        case (SYNC_TYPE_TARGET)
                            rc = dm_db_select(db, target, sync%id)
                            id = target%id
                    end select

                    if (dm_is_error(rc)) then
                        call logger%error('failed to select ' // name // ' ' // sync%id // ', next sync attempt in 30 sec', error=rc)
                        call dm_sleep(30)
                        cycle sync_loop
                    end if
                end associate
            end do

            ! Send records concurrently to HTTP-RPC API.
            rpc_block: block
                integer               :: last_error
                logical               :: has_api_status
                type(api_status_type) :: api_status

                last_error = E_NONE
                if (n == 0) exit rpc_block

                if (debug) then
                    write (message, '("syncing ", i0, " of ", i0, " ", a, "s from database ", a, " with host ", a)') n, nsyncs, name, trim(app%database), app%host
                    call logger%debug(message)
                end if

                ! Send data records via HTTP-RPC to the host.
                call dm_timer_start(rpc_timer)

                ! Send log to HTTP-RPC API. Reuse the RPC request.
                select case (app%type)
                    case (SYNC_TYPE_LOG);    rc = dm_rpc_post(requests(1:n), responses(1:n), logs   (1:n))
                    case (SYNC_TYPE_NODE);   rc = dm_rpc_post(requests(1:n), responses(1:n), nodes  (1:n))
                    case (SYNC_TYPE_OBSERV); rc = dm_rpc_post(requests(1:n), responses(1:n), observs(1:n))
                    case (SYNC_TYPE_SENSOR); rc = dm_rpc_post(requests(1:n), responses(1:n), sensors(1:n))
                    case (SYNC_TYPE_TARGET); rc = dm_rpc_post(requests(1:n), responses(1:n), targets(1:n))
                end select

                call dm_timer_stop(rpc_timer, dt)

                if (dm_is_error(rc)) then
                    call logger%warning('failed to sync with host ' // app%host, error=rc)
                else if (debug) then
                    call logger%debug('finished sync in ' // dm_ftoa(dt, 3) // ' sec')
                end if

                update_loop: do i = 1, n
                    associate (response => responses(i), sync => syncs(i))
                        ! Read API status response from payload.
                        has_api_status = .false.

                        if (debug .and. response%content_type == MIME_TEXT) then
                            stat = dm_api_status_from_string(response%payload, api_status)
                            has_api_status = dm_is_ok(stat)
                        end if

                        ! Log the HTTP response code.
                        select case (response%code)
                            case (HTTP_NONE)
                                ! Failed to connect.
                                rc = E_RPC_CONNECT
                                if (debug) call logger%debug('connection to host ' // trim(app%host) // ' failed: ' // response%error_message, error=rc)

                            case (HTTP_CREATED)
                                ! Success.
                                rc = E_NONE
                                if (debug) call logger%debug('synced ' // name // ' with id ' // trim(ids(i)), error=rc)

                            case (HTTP_CONFLICT)
                                ! Record exists in server database.
                                rc = E_EXIST
                                if (debug) call logger%debug(name // ' with id ' // trim(ids(i)) // ' exists', error=rc)

                            case (HTTP_UNAUTHORIZED)
                                ! Missing or wrong API credentials.
                                rc = E_RPC_AUTH
                                if (debug) then
                                    message = 'unauthorized access on host ' // app%host
                                    if (has_api_status .and. dm_string_has(api_status%message)) then
                                        message = trim(message) // ': ' // api_status%message
                                    end if
                                    call logger%debug(message, error=rc)
                                end if

                            case (HTTP_INTERNAL_SERVER_ERROR)
                                ! Server crashed.
                                rc = E_RPC_SERVER
                                if (debug) call logger%debug('internal server error on host ' // app%host, error=rc)

                            case (HTTP_BAD_GATEWAY)
                                ! Reverse proxy of server failed to connect to API.
                                rc = E_RPC_CONNECT
                                if (debug) call logger%debug('bad gateway on host ' // app%host, error=rc)

                            case default
                                ! Any other server error.
                                rc = E_RPC_API
                                if (debug) then
                                    write (message, '("API call to host ", a, " failed (HTTP ", i0, ")")') trim(app%host), response%code
                                    if (has_api_status .and. dm_string_has(api_status%message)) then
                                        rc = api_status%error
                                        message = trim(message) // ': ' // api_status%message
                                    end if
                                    call logger%debug(message, error=rc)
                                end if
                        end select

                        if (dm_is_error(rc)) last_error = rc

                        ! Update sync data.
                        call dm_sync_set(sync, timestamp=dm_time_now(), code=response%code, attempts=sync%attempts + 1)

                        ! Insert or replace the sync data in database. If the database
                        ! is busy, try up to `APP_DB_MAX_NATTEMPTS` times, then abort.
                        db_loop: do j = 1, APP_DB_MAX_NATTEMPTS
                            ! Try to insert sync data.
                            rc = dm_db_insert_sync(db, sync)

                            ! Re-try insert if database is busy.
                            if (rc == E_DB_BUSY) then
                                if (debug) then
                                    write (message, '("database busy (attempt ", i0, " of ", i0, ")")') i, APP_DB_MAX_NATTEMPTS
                                    call logger%debug(message, error=rc)
                                end if

                                if (j < APP_DB_MAX_NATTEMPTS) then
                                    call dm_db_sleep(APP_DB_TIMEOUT)
                                else
                                    call logger%warning('sync database update aborted')
                                end if

                                cycle db_loop
                            end if

                            if (dm_is_error(rc)) then
                                call logger%error('failed to update ' // name // ' sync status: ' // dm_db_error_message(db), error=rc)
                            end if

                            exit db_loop
                        end do db_loop
                    end associate
                end do update_loop

                call dm_rpc_reset(responses)
                if (dm_is_error(last_error)) call logger%warning(dm_error_message(last_error), error=last_error)

                ! Synchronise pending data.
                if (nsyncs > limit) then
                    if (dm_is_error(last_error)) then
                        ! Wait a grace period on error.
                        if (debug) call logger%debug('next ' // name // ' sync attempt in 30 sec')
                        call dm_sleep(30)
                    end if

                    cycle sync_loop
                end if
            end block rpc_block

            ! Sleep for the given sync interval in seconds.
            if (.not. app%ipc) then
                if (app%interval <= 0) exit sync_loop
                call dm_timer_stop(sync_timer)
                msec = max(1, 1000 * int(app%interval - dm_timer_result(sync_timer)))
                sec  = dm_msec_to_sec(msec)
                if (debug) call logger%debug('next ' // name // ' sync in ' // dm_itoa(sec) // ' sec')
                call dm_msleep(msec)
            end if
        end do sync_loop

        call dm_rpc_destroy(requests)
        call dm_rpc_destroy(responses)

        call logger%debug('finished transmission')
    end function run

    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.
        integer :: rc, stat

        stat = dm_btoi(dm_is_error(error), STOP_FAILURE, STOP_SUCCESS)
        call dm_rpc_shutdown()

        if (app%ipc) then
            call dm_sem_close(sem, error=rc)
            if (dm_is_error(rc)) call logger%error('failed to close semaphore /' // app%wait, error=rc)
        end if

        call dm_db_close(db, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to close database', error=rc)

        call dm_stop(stat)
    end subroutine halt

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app
        type(arg_type) :: args(17)

        args = [ &
            arg_type('name',        short='n', type=ARG_TYPE_ID),       & ! -n, --name <string>
            arg_type('config',      short='c', type=ARG_TYPE_FILE),     & ! -c, --config <path>
            arg_type('logger',      short='l', type=ARG_TYPE_ID),       & ! -l, --logger <string>
            arg_type('wait',        short='w', type=ARG_TYPE_STRING),   & ! -w, --wait <string>
            arg_type('node',        short='N', type=ARG_TYPE_ID),       & ! -N, --node <string>
            arg_type('database',    short='d', type=ARG_TYPE_DATABASE), & ! -d, --database <path>
            arg_type('host',        short='H', type=ARG_TYPE_STRING),   & ! -H, --host <string>
            arg_type('port',        short='q', type=ARG_TYPE_INTEGER),  & ! -q, --port <n>
            arg_type('username',    short='U', type=ARG_TYPE_STRING),   & ! -U, --username <string>
            arg_type('password',    short='P', type=ARG_TYPE_STRING),   & ! -P, --password <string>
            arg_type('compression', short='x', type=ARG_TYPE_STRING),   & ! -x, --compression <name>
            arg_type('type',        short='t', type=ARG_TYPE_STRING),   & ! -t, --type log|observ
            arg_type('interval',    short='I', type=ARG_TYPE_INTEGER),  & ! -I, --interval <n>
            arg_type('create',      short='C', type=ARG_TYPE_LOGICAL),  & ! -C, --create
            arg_type('debug',       short='D', type=ARG_TYPE_LOGICAL),  & ! -D, --debug
            arg_type('tls',         short='E', type=ARG_TYPE_LOGICAL),  & ! -E, --tls
            arg_type('verbose',     short='V', type=ARG_TYPE_LOGICAL)   & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%name)
        call dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        call dm_arg_get(args( 3), app%logger)
        call dm_arg_get(args( 4), app%wait)
        call dm_arg_get(args( 5), app%node_id)
        call dm_arg_get(args( 6), app%database)
        call dm_arg_get(args( 7), app%host)
        call dm_arg_get(args( 8), app%port)
        call dm_arg_get(args( 9), app%username)
        call dm_arg_get(args(10), app%password)
        call dm_arg_get(args(11), app%compression_name)
        call dm_arg_get(args(12), app%type_name)
        call dm_arg_get(args(13), app%interval)
        call dm_arg_get(args(14), app%create)
        call dm_arg_get(args(15), app%debug)
        call dm_arg_get(args(16), app%tls)
        call dm_arg_get(args(17), app%verbose)

        app%type = dm_sync_type_from_name(app%type_name)
        app%ipc  = dm_string_has(app%wait)

        if (dm_string_has(app%compression_name)) app%compression = dm_z_type_from_name(app%compression_name)

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.
        type(config_type) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'logger',      app%logger)
            call dm_config_get(config, 'wait',        app%wait)
            call dm_config_get(config, 'node',        app%node_id)
            call dm_config_get(config, 'database',    app%database)
            call dm_config_get(config, 'host',        app%host)
            call dm_config_get(config, 'port',        app%port)
            call dm_config_get(config, 'username',    app%username)
            call dm_config_get(config, 'password',    app%password)
            call dm_config_get(config, 'compression', app%compression_name)
            call dm_config_get(config, 'type',        app%type_name)
            call dm_config_get(config, 'interval',    app%interval)
            call dm_config_get(config, 'create',      app%create)
            call dm_config_get(config, 'debug',       app%debug)
            call dm_config_get(config, 'tls',         app%tls)
            call dm_config_get(config, 'verbose',     app%verbose)
        end if

        call dm_config_close(config)
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger name')
            return
        end if

        if (.not. dm_string_has(app%database)) then
            call dm_error_out(rc, 'missing database')
            return
        end if

        if (.not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database does not exist')
            return
        end if

        select case (app%type)
            case (SYNC_TYPE_NODE,   &
                  SYNC_TYPE_SENSOR, &
                  SYNC_TYPE_TARGET, &
                  SYNC_TYPE_OBSERV, &
                  SYNC_TYPE_LOG)
                continue
            case default
                call dm_error_out(rc, 'invalid sync type')
                return
        end select

        if (.not. dm_string_has(app%host)) then
            call dm_error_out(rc, 'invalid or missing host')
            return
        end if

        if (app%ipc .and. app%interval > 0) then
            call dm_error_out(rc, '--wait is incompatible to --interval')
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
        !! Default POSIX signal handler of the program.
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        call logger%info('exit on signal ' // dm_signal_name(signum))
        call halt(E_NONE)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 3(1x, a))', dm_rpc_version(), dm_lua_version(.true.), dm_db_version(.true.), dm_zstd_version(.true.)
    end subroutine version_callback
end program dmsync
