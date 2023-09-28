! dmsync.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmsync
    !! Observation and log database synchronisation program.
    use :: dmpack, dm_log => dm_logger_log
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmsync'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9

    integer, parameter :: APP_DB_NATTEMPTS = 10                 !! Max. number of database insert attempts.
    integer, parameter :: APP_DB_TIMEOUT   = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.
    integer, parameter :: APP_SYNC_LIMIT   = 10                 !! Max. number of records to sync at once.
    logical, parameter :: APP_RPC_DEFLATE  = .true.             !! Compress RPC payload.

    character(len=*), parameter :: API_ROUTE_LOG    = '/log'    !! Resolves to `/api/v1/log`.
    character(len=*), parameter :: API_ROUTE_OBSERV = '/observ' !! Resolves to `/api/v1/observ`.
    character(len=*), parameter :: API_ROUTE_NODE   = '/node'   !! Resolves to `/api/v1/node`.
    character(len=*), parameter :: API_ROUTE_SENSOR = '/sensor' !! Resolves to `/api/v1/sensor`.
    character(len=*), parameter :: API_ROUTE_TARGET = '/target' !! Resolves to `/api/v1/target`.

    integer, parameter :: HOST_LEN      = 80
    integer, parameter :: USERNAME_LEN  = 32
    integer, parameter :: PASSWORD_LEN  = 32

    type :: app_type
        !! Global application settings.
        character(len=APP_NAME_LEN)    :: name      = APP_NAME       !! Name of instance/configuration.
        character(len=FILE_PATH_LEN)   :: config    = ' '            !! Path to configuration file.
        character(len=LOGGER_NAME_LEN) :: logger    = ' '            !! Name of logger.
        character(len=SEM_NAME_LEN)    :: wait      = ' '            !! Name of POSIX semaphore to wait for (without leading `/`).
        character(len=NODE_ID_LEN)     :: node      = ' '            !! Node id.
        character(len=FILE_PATH_LEN)   :: database  = ' '            !! Path to database.
        character(len=HOST_LEN)        :: host      = ' '            !! IP or FQDN of API.
        integer                        :: port      = 0              !! HTTP port of API (0 selects port automatically).
        character(len=USERNAME_LEN)    :: username  = ' '            !! HTTP Basic Auth user name.
        character(len=PASSWORD_LEN)    :: password  = ' '            !! HTTP Basic Auth password.
        character(len=TYPE_NAME_LEN)   :: type_name = ' '            !! Database record type string.
        integer                        :: type      = SYNC_TYPE_NONE !! Database record type.
        integer                        :: interval  = 0              !! Sync interval in seconds.
        logical                        :: create    = .false.        !! Create synchronisation tables.
        logical                        :: debug     = .false.        !! Forward debug messages via IPC.
        logical                        :: ipc       = .false.        !! Watch named semaphore for synchronisation.
        logical                        :: tls       = .false.        !! Use TLS encryption.
        logical                        :: verbose   = .false.        !! Print debug messages to stderr.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App configuration.
    type(db_type)  :: db  ! Database type.
    type(sem_type) :: sem ! POSIX semaphore type.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line options and configuration from file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Initialise logger.
    call dm_logger_init(name    = app%logger, &                 ! Name of logger process.
                        node_id = app%node, &                   ! Node id.
                        source  = app%name, &                   ! Log source.
                        debug   = app%debug, &                  ! Forward DEBUG messages via IPC.
                        ipc     = (len_trim(app%logger) > 0), & ! Enable IPC.
                        verbose = app%verbose)                  ! Print logs to standard error.

    ! Initialise environment.
    init_block: block
        ! Open SQLite database.
        rc = dm_db_open(db      = db, &
                        path    = app%database, &
                        timeout = APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call dm_log(LOG_ERROR, 'failed to open database', error=rc)
            exit init_block
        end if

        ! Create synchronisation tables.
        if (app%create) then
            rc = dm_db_create_observs(db, sync=.true.)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to create database tables', error=rc)
                exit init_block
            end if
        end if

        ! Open semaphore.
        if (app%ipc) then
            rc = dm_sem_open(sem, app%wait)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to open semaphore /' // app%wait, error=rc)
                exit init_block
            end if
        end if

        ! Initialise RPC backend.
        rc = dm_rpc_init()

        if (dm_is_error(rc)) then
            call dm_log(LOG_ERROR, 'failed to initialize libcurl', error=rc)
            exit init_block
        end if

        ! Register signal handler.
        call dm_signal_register(signal_handler)

        ! Start synchronisation.
        rc = run(app, db, sem)
    end block init_block

    call halt(min(1, rc))
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(inout) :: app
        type(arg_type)                :: args(16)

        rc = E_NONE

        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <string>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),      & ! -l, --logger <string>
            arg_type('wait',     short='w', type=ARG_TYPE_CHAR),    & ! -w, --wait <string>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <string>
            arg_type('database', short='d', type=ARG_TYPE_DB),      & ! -d, --database <path>
            arg_type('type',     short='t', type=ARG_TYPE_CHAR),    & ! -t, --type log|observ
            arg_type('host',     short='H', type=ARG_TYPE_CHAR),    & ! -H, --host <string>
            arg_type('port',     short='p', type=ARG_TYPE_INTEGER), & ! -p, --port <n>
            arg_type('username', short='U', type=ARG_TYPE_CHAR),    & ! -U, --username <string>
            arg_type('password', short='P', type=ARG_TYPE_CHAR),    & ! -P, --password <string>
            arg_type('interval', short='I', type=ARG_TYPE_INTEGER), & ! -I, --interval <n>
            arg_type('create',   short='C', type=ARG_TYPE_BOOL),    & ! -C, --create
            arg_type('debug',    short='D', type=ARG_TYPE_BOOL),    & ! -D, --debug
            arg_type('tls',      short='X', type=ARG_TYPE_BOOL),    & ! -X, --tls
            arg_type('verbose',  short='V', type=ARG_TYPE_BOOL)     & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        rc = dm_arg_get(args( 3), app%logger)
        rc = dm_arg_get(args( 4), app%wait)
        rc = dm_arg_get(args( 5), app%node)
        rc = dm_arg_get(args( 6), app%database)
        rc = dm_arg_get(args( 7), app%type_name)
        rc = dm_arg_get(args( 8), app%host)
        rc = dm_arg_get(args( 9), app%port)
        rc = dm_arg_get(args(10), app%username)
        rc = dm_arg_get(args(11), app%password)
        rc = dm_arg_get(args(12), app%interval)
        rc = dm_arg_get(args(13), app%create)
        rc = dm_arg_get(args(14), app%debug)
        rc = dm_arg_get(args(15), app%tls)
        rc = dm_arg_get(args(16), app%verbose)

        app%type = dm_sync_type_from_name(app%type_name)

        rc = E_INVALID

        if (.not. dm_id_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
        end if

        if (len_trim(app%logger) > 0 .and. .not. dm_id_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger name')
            return
        end if

        if (len_trim(app%database) == 0) then
            call dm_error_out(rc, 'invalid or missing database')
            return
        end if

        if (.not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database does not exist')
            return
        end if

        select case (app%type)
            case (SYNC_TYPE_NODE)
            case (SYNC_TYPE_SENSOR)
            case (SYNC_TYPE_TARGET)
            case (SYNC_TYPE_OBSERV)
            case (SYNC_TYPE_LOG)
                continue
            case default
                call dm_error_out(rc, 'invalid sync type')
                return
        end select

        if (len_trim(app%host) == 0) then
            call dm_error_out(rc, 'invalid or missing host')
            return
        end if

        if (len_trim(app%wait) > 0) app%ipc = .true.

        if (app%ipc .and. app%interval > 0) then
            call dm_error_out(rc, '--wait is incompatible to --interval')
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

        if_block: if (dm_is_ok(rc)) then
            rc = dm_config_get(config, 'logger',   app%logger)
            rc = dm_config_get(config, 'wait',     app%wait)
            rc = dm_config_get(config, 'node',     app%node)
            rc = dm_config_get(config, 'database', app%database)
            rc = dm_config_get(config, 'type',     app%type_name)
            rc = dm_config_get(config, 'host',     app%host)
            rc = dm_config_get(config, 'port',     app%port)
            rc = dm_config_get(config, 'username', app%username)
            rc = dm_config_get(config, 'password', app%password)
            rc = dm_config_get(config, 'interval', app%interval)
            rc = dm_config_get(config, 'create',   app%create)
            rc = dm_config_get(config, 'debug',    app%debug)
            rc = dm_config_get(config, 'tls',      app%tls)
            rc = dm_config_get(config, 'verbose',  app%verbose)
            rc = E_NONE
        end if if_block

        call dm_config_close(config)
    end function read_config

    integer function run(app, db, sem) result(rc)
        !! Synchronises logs database via RPC API.
        type(app_type), intent(inout) :: app !! App configuration type.
        type(db_type),  intent(inout) :: db  !! Database type.
        type(sem_type), intent(inout) :: sem !! Semaphore type.

        character(len=:), allocatable :: name, url
        integer                       :: delay, i, j, n, stat
        integer(kind=i8)              :: limit, nsyncs
        real(kind=r8)                 :: dt

        type(api_status_type) :: api
        type(timer_type)      :: sync_timer
        type(timer_type)      :: rpc_timer

        character(len=ID_LEN),   allocatable :: ids(:)       ! Derived type ids.
        type(rpc_request_type),  allocatable :: requests(:)  ! HTTP-RPC requests.
        type(rpc_response_type), allocatable :: responses(:) ! HTTP-RPC responses.
        type(sync_type),         allocatable :: syncs(:)     ! Sync data.

        type(log_type),    allocatable :: logs(:)
        type(observ_type), allocatable :: observs(:)
        type(node_type),   allocatable :: nodes(:)
        type(sensor_type), allocatable :: sensors(:)
        type(target_type), allocatable :: targets(:)

        call dm_log(LOG_INFO, 'starting ' // app%name)

        name  = dm_sync_name(app%type)
        limit = APP_SYNC_LIMIT

        ! Allocate type array, and generate URL of HTTP-RPC API endpoint.
        rc = E_ALLOC

        select case (app%type)
            case (SYNC_TYPE_LOG)
                allocate (logs(APP_SYNC_LIMIT), stat=stat)
                url = dm_rpc_url(app%host, port=app%port, endpoint=API_ROUTE_LOG, tls=app%tls)
            case (SYNC_TYPE_NODE)
                allocate (nodes(APP_SYNC_LIMIT), stat=stat)
                url = dm_rpc_url(app%host, port=app%port, endpoint=API_ROUTE_NODE, tls=app%tls)
            case (SYNC_TYPE_OBSERV)
                allocate (observs(APP_SYNC_LIMIT), stat=stat)
                url = dm_rpc_url(app%host, port=app%port, endpoint=API_ROUTE_OBSERV, tls=app%tls)
            case (SYNC_TYPE_SENSOR)
                allocate (sensors(APP_SYNC_LIMIT), stat=stat)
                url = dm_rpc_url(app%host, port=app%port, endpoint=API_ROUTE_SENSOR, tls=app%tls)
            case (SYNC_TYPE_TARGET)
                allocate (targets(APP_SYNC_LIMIT), stat=stat)
                url = dm_rpc_url(app%host, port=app%port, endpoint=API_ROUTE_TARGET, tls=app%tls)
            case default
                ! Fail-safe, should never occur.
                rc = E_TYPE
                call dm_log(LOG_ERROR, 'invalid sync type', error=rc)
                return
        end select

        if (stat /= 0) return

        ! Allocate request array.
        allocate (requests(APP_SYNC_LIMIT), stat=stat)
        if (stat /= 0) return

        ! Allocate id array.
        allocate (ids(APP_SYNC_LIMIT), stat=stat)
        if (stat /= 0) return

        ! Prepare requests (they will be re-used).
        do i = 1, APP_SYNC_LIMIT
            requests(i)%url     = url
            requests(i)%deflate = APP_RPC_DEFLATE

            if (len_trim(app%username) > 0 .and. len_trim(app%password) > 0) then
                requests(i)%auth     = RPC_AUTH_BASIC
                requests(i)%username = trim(app%username)
                requests(i)%password = trim(app%password)
            end if
        end do

        ! Main synchronisation loop.
        sync_loop: &
        do
            if (.not. app%ipc) then
                ! Start interval timer.
                call dm_timer_start(sync_timer)
            else
                ! Wait for semaphore.
                call dm_log(LOG_DEBUG, 'waiting for signal from ' // app%wait)
                rc = dm_sem_wait(sem)

                if (dm_is_error(rc)) then
                    ! Unrecoverable semaphore error. Stop program.
                    call dm_log(LOG_ERROR, 'semaphore error', error=rc)
                    exit sync_loop
                end if
            end if

            ! Fetch sync records up to the limit `APP_SYNC_LIMIT`. Argument
            ! `nsyncs` will store the total number of records in database
            ! pending for synchronisation, and may be greater than the limit.
            if (allocated(syncs)) deallocate (syncs)

            select case (app%type)
                case (SYNC_TYPE_LOG)
                    rc = dm_db_select_sync_logs(db, syncs, nsyncs, limit)
                case (SYNC_TYPE_NODE)
                    rc = dm_db_select_sync_nodes(db, syncs, nsyncs, limit)
                case (SYNC_TYPE_OBSERV)
                    rc = dm_db_select_sync_observs(db, syncs, nsyncs, limit)
                case (SYNC_TYPE_SENSOR)
                    rc = dm_db_select_sync_sensors(db, syncs, nsyncs, limit)
                case (SYNC_TYPE_TARGET)
                    rc = dm_db_select_sync_targets(db, syncs, nsyncs, limit)
            end select

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                ! Unrecoverable database error. Stop program.
                call dm_log(LOG_ERROR, 'failed to select sync data', error=rc)
                exit sync_loop
            end if

            ! Read data records to synchronise from database.
            n = size(syncs)

            do i = 1, n
                select case (syncs(i)%type)
                    case (SYNC_TYPE_LOG)
                        rc = dm_db_select(db, logs(i), syncs(i)%id)
                        ids(i) = logs(i)%id
                    case (SYNC_TYPE_NODE)
                        rc = dm_db_select(db, nodes(i), syncs(i)%id)
                        ids(i) = nodes(i)%id
                    case (SYNC_TYPE_OBSERV)
                        rc = dm_db_select(db, observs(i), syncs(i)%id)
                        ids(i) = observs(i)%id
                    case (SYNC_TYPE_SENSOR)
                        rc = dm_db_select(db, sensors(i), syncs(i)%id)
                        ids(i) = sensors(i)%id
                    case (SYNC_TYPE_TARGET)
                        rc = dm_db_select(db, targets(i), syncs(i)%id)
                        ids(i) = targets(i)%id
                end select

                if (dm_is_error(rc)) then
                    call dm_log(LOG_ERROR, 'failed to select ' // name // ' ' // syncs(i)%id, error=rc)
                    call dm_sleep(1)
                    cycle sync_loop
                end if
            end do

            ! Send records concurrently to HTTP-RPC API.
            rpc_block: &
            block
                if (n == 0) then
                    call dm_log(LOG_DEBUG, 'no ' // name // 's to sync found')
                    exit rpc_block
                end if

                call dm_log(LOG_DEBUG, 'syncing ' // dm_itoa(n) // ' of ' // &
                            dm_itoa(nsyncs) // ' ' // name // 's from database ' // &
                            trim(app%database) // ' with host ' // app%host)

                ! Send data records via HTTP-RPC to the host.
                call dm_timer_start(rpc_timer)

                ! Send log to HTTP-RPC API. Reuse the RPC request.
                select case (app%type)
                    case (SYNC_TYPE_LOG)
                        rc = dm_rpc_send(requests(1:n), responses, logs(1:n))
                    case (SYNC_TYPE_NODE)
                        rc = dm_rpc_send(requests(1:n), responses, nodes(1:n))
                    case (SYNC_TYPE_OBSERV)
                        rc = dm_rpc_send(requests(1:n), responses, observs(1:n))
                    case (SYNC_TYPE_SENSOR)
                        rc = dm_rpc_send(requests(1:n), responses, sensors(1:n))
                    case (SYNC_TYPE_TARGET)
                        rc = dm_rpc_send(requests(1:n), responses, targets(1:n))
                end select

                dt = dm_timer_stop(rpc_timer)

                if (dm_is_error(rc)) then
                    call dm_log(LOG_DEBUG, 'failed to sync with host ' // app%host, error=rc)
                else
                    call dm_log(LOG_DEBUG, 'finished sync in ' // dm_ftoa(dt) // ' seconds', error=rc)
                end if

                update_loop: &
                do i = 1, n
                    ! Log the HTTP response code.
                    code_select: &
                    select case (responses(i)%code)
                        case (0)
                            ! Failed to connect.
                            call dm_log(LOG_WARNING, 'connection to host ' // trim(app%host) // ' failed: ' // &
                                        responses(i)%error_message, error=E_RPC_CONNECT)
                        case (HTTP_CREATED)
                            ! Success.
                            call dm_log(LOG_DEBUG, 'synced ' // name // ' ' // trim(ids(i)))
                        case (HTTP_CONFLICT)
                            ! Record exists in server database.
                            call dm_log(LOG_INFO, name // ' ' // trim(ids(i)) // ' exists', error=E_EXIST)
                        case (HTTP_UNAUTHORIZED)
                            ! Missing or wrong API credentials.
                            call dm_log(LOG_ERROR, 'unauthorized access on host ' // app%host, error=E_RPC_AUTH)
                        case (HTTP_INTERNAL_SERVER_ERROR)
                            ! Server crashed.
                            call dm_log(LOG_ERROR, 'internal server error on host ' // app%host, error=E_RPC_SERVER)
                        case (HTTP_BAD_GATEWAY)
                            ! Reverse proxy of server failed to connect to API.
                            call dm_log(LOG_ERROR, 'bad gateway on host ' // app%host, error=E_RPC_CONNECT)
                        case default
                            ! MIME type `text/plain` indicates API status response.
                            if (responses(i)%content_type == MIME_TEXT) then
                                ! Convert response text to API type.
                                stat = dm_api_status_from_string(api, responses(i)%payload)

                                if (dm_is_ok(stat)) then
                                    call dm_log(LOG_ERROR, 'server error on host ' // trim(app%host) // &
                                                ' (HTTP ' // dm_itoa(responses(i)%code) // '): ' // &
                                                api%message, error=api%error)
                                    exit code_select
                                end if
                            end if

                            ! Otherwise, output generic log message.
                            call dm_log(LOG_WARNING, 'API call to host ' // trim(app%host) // ' failed (HTTP ' // &
                                        dm_itoa(responses(i)%code) // ')', error=E_RPC_API)
                    end select code_select

                    ! Update sync data.
                    syncs(i)%timestamp = dm_time_now()          ! Last sync attempt.
                    syncs(i)%code      = responses(i)%code      ! Server status code.
                    syncs(i)%nattempts = syncs(i)%nattempts + 1 ! Number of sync attempts.

                    ! Insert or replace the sync data in database. If the database
                    ! is busy, try up to `APP_DB_NATTEMPTS` times, then abort.
                    db_loop: &
                    do j = 1, APP_DB_NATTEMPTS
                        ! Try to insert sync data.
                        rc = dm_db_insert_sync(db, syncs(i))

                        ! Re-try insert if database is busy.
                        if (rc == E_DB_BUSY) then
                            call dm_log(LOG_WARNING, 'database busy (attempt ' // dm_itoa(i) // &
                                        ' of ' // dm_itoa(APP_DB_NATTEMPTS) // ')', error=rc)

                            if (j < APP_DB_NATTEMPTS) then
                                call dm_db_sleep(APP_DB_TIMEOUT)
                            else
                                call dm_log(LOG_INFO, 'sync database update aborted')
                            end if

                            cycle db_loop
                        end if

                        if (dm_is_error(rc)) then
                            call dm_log(LOG_ERROR, 'failed to update sync status: ' // &
                                        dm_db_error_message(db), error=rc)
                        end if

                        exit
                    end do db_loop
                end do update_loop

                ! Synchronise pending data.
                if (nsyncs > limit) then
                    if (dm_is_error(rc)) then
                        ! Wait a grace period on error.
                        call dm_log(LOG_DEBUG, 'waiting 10 seconds before next sync attempt')
                        call dm_sleep(10)
                    end if

                    cycle sync_loop
                end if
            end block rpc_block

            ! Sleep for the given sync interval in seconds.
            if (.not. app%ipc) then
                if (app%interval <= 0) exit sync_loop
                delay = max(1, nint(app%interval - dm_timer_stop(sync_timer)))
                call dm_log(LOG_DEBUG, 'next sync in ' // dm_itoa(delay) // ' seconds')
                call dm_sleep(delay)
            end if
        end do sync_loop

        call dm_log(LOG_DEBUG, 'exiting ...')
    end function run

    subroutine halt(stat)
        !! Cleans up and stops program.
        integer, intent(in), optional :: stat !! Exit status.

        integer :: rc, stat_

        stat_ = 0
        if (present(stat)) stat_ = stat

        call dm_rpc_destroy()
        if (app%ipc) rc = dm_sem_close(sem)
        rc = dm_db_close(db)
        call dm_stop(stat_)
    end subroutine halt

    subroutine signal_handler(signum) bind(c)
        !! Default POSIX signal handler of the program.
        use, intrinsic :: iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        select case (signum)
            case default
                call dm_log(LOG_INFO, 'exit on signal ' // dm_itoa(signum))
                call halt(0)
        end select
    end subroutine signal_handler
end program dmsync
