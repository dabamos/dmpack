! dmupload.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmupload
    !! Uploads camera images to RPC server.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmupload'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    ! Program parameters.
    integer, parameter :: APP_DB_MAX_NATTEMPTS = 10                 !! Max. number of database insert attempts.
    integer, parameter :: APP_DB_TIMEOUT       = DB_TIMEOUT_DEFAULT !! Database busy timeout [msec].

    integer, parameter :: APP_HOST_LEN     = 256 !! Max. length of host.
    integer, parameter :: APP_USERNAME_LEN = 256 !! Max. length of user name.
    integer, parameter :: APP_PASSWORD_LEN = 256 !! Max. length of password.

    type :: app_type
        !! Application settings.
        character(ID_LEN)           :: name             = APP_NAME    !! Name of database instance.
        character(FILE_PATH_LEN)    :: config           = ' '         !! Path to configuration file.
        character(LOGGER_NAME_LEN)  :: logger           = ' '         !! Name of logger (name implies IPC).
        character(SEM_NAME_LEN)     :: wait             = ' '         !! Name of POSIX semaphore to wait for (without leading `/`).
        character(NODE_ID_LEN)      :: node_id          = ' '         !! Node id.
        character(FILE_PATH_LEN)    :: database         = ' '         !! Path to SQLite image database file.
        character(FILE_PATH_LEN)    :: directory        = ' '         !! Path to camera image directory.
        character(APP_HOST_LEN)     :: host             = ' '         !! IP or FQDN of API (`127.0.0.1`, `example.com`).
        character(APP_USERNAME_LEN) :: username         = ' '         !! HTTP Basic Auth user name.
        character(APP_PASSWORD_LEN) :: password         = ' '         !! HTTP Basic Auth password.
        character(Z_TYPE_NAME_LEN)  :: compression_name = 'zstd'      !! Compression library (`none`, `zlib`, `zstd`).
        integer                     :: compression      = Z_TYPE_NONE !! Compression type (`Z_TYPE_*`).
        integer                     :: interval         = 0           !! Upload interval [sec].
        integer                     :: port             = 0           !! API port (set to 0 for protocol default).
        logical                     :: create           = .false.     !! Create synchronisation table.
        logical                     :: debug            = .false.     !! Forward debug messages via IPC.
        logical                     :: ipc              = .false.     !! Open semaphore if attribute wait is set.
        logical                     :: tls              = .false.     !! TLS encryption.
        logical                     :: verbose          = .false.     !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer              :: rc  ! Return code.
    type(app_type)       :: app ! App settings.
    type(db_type)        :: db  ! Database type.
    type(sem_named_type) :: sem ! POSIX semaphore type.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,  & ! Name of logger process.
                          node_id = app%node_id, & ! Node id.
                          source  = app%name,    & ! Log source.
                          debug   = app%debug,   & ! Forward debug messages via IPC.
                          ipc     = .true.,      & ! Enable IPC (if logger is set).
                          verbose = app%verbose)   ! Print logs to standard error.
    call logger%info('started ' // APP_NAME)

    rc = init(app, db, sem)
    if (dm_is_error(rc)) call shutdown(rc)

    rc = run(app, db, sem)
    call shutdown(rc)
contains
    integer function init(app, db, sem) result(rc)
        !! Initialises program.
        type(app_type),       intent(inout) :: app !! App type.
        type(db_type),        intent(out)   :: db  !! Database type.
        type(sem_named_type), intent(out)   :: sem !! POSIX semaphore type.

        ! Open SQLite database.
        rc = dm_db_open(db, path=app%database, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call logger%error('failed to open database ' // app%database, error=rc)
            return
        end if

        call logger%debug('opened database ' // app%database)

        ! Create synchronisation tables.
        if (app%create .and. dm_db_table_has_sync_images(db)) then
            call logger%debug('synchronization table already exists in database ' // app%database)
            app%create = .false.
        end if

        if (app%create) then
            rc = dm_db_table_create_sync_images(db)

            if (dm_is_error(rc)) then
                call logger%error('failed to create tables in database ' // app%database, error=rc)
                return
            end if

            call logger%debug('created database tables in database ' // app%database)
        end if

        ! Find database tables.
        if (.not. dm_db_table_has_images(db) .or. .not. dm_db_table_has_sync_images(db)) then
            rc = E_INVALID
            call logger%error('missing tables in database ' // app%database, error=rc)
            return
        end if

        ! Open named semaphore for IPC.
        if (app%ipc) then
            rc = dm_sem_open(sem, name=app%wait)

            if (dm_is_error(rc)) then
                call logger%error('failed to open semaphore /' // app%wait, error=rc)
                return
            end if

            call logger%debug('opened semaphore /' // app%wait)
        end if

        ! Initialise libcurl.
        rc = dm_rpc_init()

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize libcurl', error=rc)
            return
        end if

        call dm_signal_register(signal_callback)
    end function init

    integer function response_error(response, host, debug) result(rc)
        !! Logs HTTP response error and returns associated error code.
        type(rpc_response_type), intent(inout) :: response !! RPC API response.
        character(*),            intent(in)    :: host     !! Host name.
        logical,                 intent(in)    :: debug    !! Output debug messages.

        character(LOG_MESSAGE_LEN) :: message
        logical                    :: has_api_status
        type(api_status_type)      :: api_status

        ! Read API status response from payload.
        has_api_status = .false.

        if (response%content_type == MIME_TEXT) then
            rc = dm_api_status_from_string(response%payload, api_status)
            has_api_status = dm_is_ok(rc)
        end if

        if (debug .and. response%code > 0) then
            call logger%debug('server answered with status HTTP ' // dm_itoa(response%code) // ' ' // dm_http_status_string(response%code))
        end if

        ! Log the HTTP response code.
        select case (response%code)
            case (HTTP_NONE)
                rc = E_RPC_CONNECT
                message = 'connection to host ' // trim(host) // ' failed: ' // response%error_message

            case (HTTP_CREATED)
                rc = E_NONE
                if (debug) call logger%debug('finished upload to host ' // trim(host))

            case (HTTP_ACCEPTED)
                rc = E_NONE
                if (debug) call logger%debug('upload request accepted by host ' // host)

            case (HTTP_UNAUTHORIZED)
                rc = E_RPC_AUTH
                message = 'unauthorized access on host ' // host
                if (has_api_status) message = trim(message) //  ': ' // api_status%message

            case (HTTP_INTERNAL_SERVER_ERROR)
                rc = E_RPC_SERVER
                message = 'internal server error on host ' // host

            case (HTTP_BAD_GATEWAY)
                rc = E_RPC_CONNECT
                message = 'bad gateway on host ' // host

            case default
                rc = E_RPC_API
                message = 'API call to host ' // trim(app%host) // ' failed (HTTP ' // dm_itoa(response%code) // ')'

                if (has_api_status) then
                    rc = api_status%error
                    if (dm_string_has(api_status%message)) message = trim(message) // ': ' // api_status%message
                end if
        end select

        if (dm_is_error(rc)) call logger%error(message, error=rc)
    end function response_error

    integer function run(app, db, sem) result(rc)
        !! Uploads images.
        type(app_type),       intent(inout) :: app !! App settings.
        type(db_type),        intent(inout) :: db  !! Database type.
        type(sem_named_type), intent(inout) :: sem !! Semaphore type.

        character(:), allocatable :: url, user_agent

        integer     :: sec
        integer(i8) :: nsyncs
        logical     :: debug, has_auth
        real(r8)    :: dt

        type(rpc_request_type)  :: request
        type(rpc_response_type) :: response
        type(timer_type)        :: sync_timer

        nsyncs   = 0
        debug    = (app%debug .or. app%verbose)
        has_auth = (dm_string_has(app%username) .and. dm_string_has(app%password))

        if (app%compression == Z_TYPE_NONE) then
            call logger%debug('compression of RPC data disabled')
        else
            call logger%debug(dm_z_type_name(app%compression) // ' compression of RPC data enabled')
        end if

        if (app%interval == 0) then
            call logger%debug('no upload interval set')
        else
            call logger%debug('upload interval set to ' // dm_itoa(app%interval) // ' sec')
        end if

        ! Create URL of HTTP-RPC API endpoint.
        url = dm_rpc_url(app%host, port=app%port, endpoint=RPC_ROUTE_IMAGE, tls=app%tls)

        if (.not. dm_string_has(url)) then
            rc = E_INVALID
            call logger%error('failed to create URL of host ' // app%host, error=rc)
            return
        end if

        ! Prepare request and response (will be reused).
        user_agent = dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, library=.true.)
        call dm_rpc_request_set(request, compression=app%compression, url=url, user_agent=user_agent)
        if (has_auth) call dm_rpc_request_set(request, auth=RPC_AUTH_BASIC, username=app%username, password=app%password)

        rc = dm_rpc_header_create(request, max_size=1)

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize HTTP request headers', error=rc)
            return
        end if

        rc = dm_rpc_header_create(response, max_size=1)

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize HTTP response headers', error=rc)
            return
        end if

        main_loop: do
            ! Wait for semaphore if IPC is enabled.
            if (app%ipc .and. nsyncs <= 1) then
                if (debug) call logger%debug('waiting for semaphore /' // app%wait)

                rc = dm_sem_wait(sem)

                if (dm_is_error(rc)) then
                    call logger%error('failed to wait for semaphore /' // app%wait, error=rc)
                    exit main_loop
                end if
            end if

            call dm_timer_start(sync_timer)

            sync_block: block
                character(:), allocatable :: image_path, transfer_id

                type(sync_type)  :: sync
                type(timer_type) :: rpc_timer
                type(image_type) :: image

                select_block: block
                    ! Select next image to sync from database.
                    rc = dm_db_select_sync_image(db, sync, nsyncs)

                    if (dm_is_error(rc)) then
                        call logger%error('failed to select upload status from database', error=rc)
                        exit main_loop
                    end if

                    if (nsyncs == 0) then
                        if (debug) call logger%debug('no image to upload found in database')
                        exit sync_block
                    end if

                    if (debug) call logger%debug(dm_itoa(nsyncs) // ' images to upload found in database')

                    ! Select image meta type from database.
                    rc = dm_db_select(db, image, sync%id)

                    if (dm_is_error(rc)) then
                        call logger%error('failed to select image ' // sync%id // ' from database', error=rc)
                        exit main_loop
                    end if

                    ! Generate image file path.
                    image_path = dm_image_path(image, app%directory)

                    if (.not. dm_file_exists(image_path)) then
                        rc = E_NOT_FOUND
                        call logger%error('image file ' // image_path // ' not found', error=rc)
                        exit sync_block
                    end if

                    if (.not. dm_file_is_readable(image_path)) then
                        rc = E_PERM
                        call logger%error('no permission to read image file ' // image_path, error=rc)
                        exit sync_block
                    end if
                end block select_block

                call dm_timer_start(rpc_timer)

                rpc_block: block
                    ! Prepare HTTP response header.
                    rc = dm_rpc_header_add(response, RPC_HEADER_TRANSFER_ID)

                    if (dm_is_error(rc)) then
                        call logger%error('failed to prepare HTTP POST response header of image ' // image%id, error=rc)
                        exit rpc_block
                    end if

                    ! Post serialised image type to HTTP-RPC API.
                    rc = dm_rpc_post(request, response, image)

                    if (dm_is_error(rc)) then
                        call logger%error('failed to send HTTP POST request of image ' // image%id // ' to ' // url, error=rc)
                        exit rpc_block
                    end if

                    if (debug) call logger%debug('sent HTTP POST request of image ' // image%id // ' to ' // url)

                    ! Handle response code.
                    rc = response_error(response, app%host, debug)
                    if (dm_is_error(rc) .and. response%code /= HTTP_CONFLICT) exit rpc_block
                    if (debug .and. rc == HTTP_CONFLICT) call logger%debug('image ' // image%id // ' already exists on server')

                    ! Extract transfer id from response.
                    rc = dm_rpc_header_get(response, RPC_HEADER_TRANSFER_ID, transfer_id)

                    if (dm_is_error(rc) .or. len(transfer_id) == 0) then
                        call logger%error('missing transfer id for image ' // image%id // ' in HTTP POST response header', error=rc)
                        exit rpc_block
                    end if

                    if (debug) call logger%debug('received transfer id ' // transfer_id // ' of image ' // image%id)

                    call dm_rpc_reset(request)
                    call dm_rpc_reset(response)

                    ! Prepare sending transfer id in HTTP request header.
                    rc = dm_rpc_header_add(request, RPC_HEADER_TRANSFER_ID, transfer_id)

                    if (dm_is_error(rc)) then
                        call logger%error('failed to prepare HTTP PUT request header of image ' // image%id, error=rc)
                        exit rpc_block
                    end if

                    ! Upload image data to server.
                    rc = dm_rpc_put(request, response, payload_path=image_path, content_type=image%mime)

                    if (dm_is_error(rc)) then
                        call logger%error('failed to send HTTP PUT request of image ' // image%id // ' to ' // url, error=rc)
                        exit rpc_block
                    end if

                    if (debug) call logger%debug('sent HTTP PUT request of image ' // image%id // ' to ' // url)

                    ! Handle response code.
                    rc = response_error(response, app%host, debug)
                end block rpc_block

                call dm_timer_stop(rpc_timer, duration=dt)

                if (dm_is_error(rc)) then
                    call logger%warning('failed to upload image to host ' // app%host, error=rc)
                else if (debug) then
                    call logger%debug('finished image upload in ' // dm_ftoa(dt, 3) // ' sec')
                end if

                ! Update sync status in database.
                update_block: block
                    integer :: i

                    call dm_sync_set(sync, timestamp=dm_time_now(), code=response%code, attempts=sync%attempts + 1)

                    ! Insert or replace the sync status in database. If the database
                    ! is busy, try up to `APP_DB_MAX_NATTEMPTS` times, then abort.
                    db_loop: do i = 1, APP_DB_MAX_NATTEMPTS
                        rc = dm_db_insert_sync(db, sync)

                        ! Re-try insert if database is busy.
                        if (rc == E_DB_BUSY) then
                            if (debug) call logger%debug('database busy (attempt ' // dm_itoa(i) // ' of ' // dm_itoa(APP_DB_MAX_NATTEMPTS) // ')', error=rc)

                            if (i < APP_DB_MAX_NATTEMPTS) then
                                call dm_db_sleep(APP_DB_TIMEOUT)
                            else
                                call logger%warning('aborted database update')
                            end if

                            cycle db_loop
                        end if

                        if (dm_is_error(rc)) then
                            call logger%error('failed to update upload status of image ' // image%id // ': ' // dm_db_error_message(db), error=rc)
                            exit update_block
                        end if

                        exit db_loop
                    end do db_loop

                    if (debug) call logger%debug('updated upload status of image ' // image%id)
                end block update_block
            end block sync_block

            call dm_rpc_reset(request)
            call dm_rpc_reset(response)

            ! Upload pending images.
            if (nsyncs > 1) cycle main_loop

            ! Sleep for the given sync interval in seconds.
            if (.not. app%ipc) then
                if (app%interval <= 0) exit main_loop
                call dm_timer_stop(sync_timer, duration=dt)
                sec = max(0, int(app%interval - dt))
                if (debug) call logger%debug('next upload attempt in ' // dm_itoa(sec) // ' sec')
                call dm_sleep(sec)
            end if
        end do main_loop

        call dm_rpc_destroy(request)
        call dm_rpc_destroy(response)

        call logger%debug('finished upload')
    end function run

    subroutine shutdown(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: rc, stat

        stat = merge(STOP_FAILURE, STOP_SUCCESS, dm_is_error(error))

        call dm_rpc_shutdown()

        if (app%ipc) then
            call dm_sem_close(sem, error=rc)
            if (dm_is_error(rc)) call logger%error('failed to close semaphore /' // app%wait, error=rc)
        end if

        if (dm_db_is_connected(db)) then
            call dm_db_close(db, error=rc)
            if (dm_is_error(rc)) call logger%error('failed to close database ' // app%database, error=rc)
        end if

        call logger%info('stopped ' // APP_NAME, error=error)
        call dm_stop(stat)
    end subroutine shutdown

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app !! App type.

        type(arg_parser_class) :: parser

        ! Required and optional command-line arguments.
        call parser%add('name',        short='n', type=ARG_TYPE_ID)       ! -n, --name <id>
        call parser%add('config',      short='c', type=ARG_TYPE_FILE)     ! -c, --config <path>
        call parser%add('logger',      short='l', type=ARG_TYPE_ID)       ! -l, --logger <id>
        call parser%add('wait',        short='w', type=ARG_TYPE_ID)       ! -w, --wait <string>
        call parser%add('node',        short='N', type=ARG_TYPE_ID)       ! -N, --node <id>
        call parser%add('database',    short='d', type=ARG_TYPE_DATABASE) ! -d, --database <path>
        call parser%add('directory',   short='p', type=ARG_TYPE_FILE)     ! -p, --directory <path>
        call parser%add('host',        short='H', type=ARG_TYPE_STRING)   ! -H, --host <string>
        call parser%add('username',    short='U', type=ARG_TYPE_STRING)   ! -U, --username <string>
        call parser%add('password',    short='P', type=ARG_TYPE_STRING)   ! -P, --password <string>
        call parser%add('compression', short='x', type=ARG_TYPE_STRING)   ! -x, --compression <name>
        call parser%add('interval',    short='I', type=ARG_TYPE_INTEGER)  ! -I, --interval <sec>
        call parser%add('port',        short='q', type=ARG_TYPE_INTEGER)  ! -q, --port <n>
        call parser%add('create',      short='C', type=ARG_TYPE_LOGICAL)  ! -C, --create
        call parser%add('debug',       short='D', type=ARG_TYPE_LOGICAL)  ! -D, --debug
        call parser%add('tls',         short='E', type=ARG_TYPE_LOGICAL)  ! -E, --tls
        call parser%add('verbose',     short='V', type=ARG_TYPE_LOGICAL)  ! -V, --verbose

        ! Read all command-line arguments.
        rc = parser%read(version_callback)
        if (dm_is_error(rc)) return

        call parser%get('name',   app%name)
        call parser%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite configuration.
        call parser%get('logger',      app%logger)
        call parser%get('wait',        app%wait)
        call parser%get('node',        app%node_id)
        call parser%get('database',    app%database)
        call parser%get('directory',   app%directory)
        call parser%get('host',        app%host)
        call parser%get('username',    app%username)
        call parser%get('password',    app%password)
        call parser%get('compression', app%compression)
        call parser%get('interval',    app%interval)
        call parser%get('port',        app%port)
        call parser%get('create',      app%create)
        call parser%get('debug',       app%debug)
        call parser%get('tls',         app%tls)
        call parser%get('verbose',     app%verbose)

        app%ipc = dm_string_has(app%wait)

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file if path is not emty.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('logger',    app%logger)
            call config%get('node',      app%node_id)
            call config%get('database',  app%database)
            call config%get('directory', app%directory)
            call config%get('wait',      app%wait)
            call config%get('interval',  app%interval)
            call config%get('create',    app%create)
            call config%get('debug',     app%debug)
            call config%get('verbose',   app%verbose)
        end if

        call config%close()
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

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (.not. dm_string_has(app%database)) then
            rc = E_NOT_FOUND
            call dm_error_out(rc, 'database required')
            return
        end if

        if (.not. dm_file_exists(app%database)) then
            rc = E_NOT_FOUND
            call dm_error_out(rc, 'database ' // trim(app%database) // ' does not exist')
            return
        end if

        if (.not. dm_string_has(app%directory)) then
            call dm_error_out(rc, 'missing image directory')
            return
        end if

        if (.not. dm_file_exists(app%directory)) then
            rc = E_NOT_FOUND
            call dm_error_out(rc, 'image directory' // trim(app%directory) // ' does not exist')
            return
        end if

        if (.not. dm_file_is_directory(app%directory)) then
            call dm_error_out(rc, 'file ' // trim(app%directory) // ' is not a directory')
            return
        end if

        if (.not. dm_file_is_readable(app%directory)) then
            rc = E_PERM
            call dm_error_out(rc, 'no read access to image directory' // trim(app%directory))
            return
        end if

        if (app%ipc) then
            if (.not. dm_id_is_valid(app%wait)) then
                call dm_error_out(rc, 'invalid wait name')
                return
            end if

            if (app%interval > 0) then
                call dm_error_out(rc, 'option interval conflicts with option wait')
                return
            end if
        end if

        if (app%interval < 0) then
            call dm_error_out(rc, 'invalid interval')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine signal_callback(signum) bind(c)
        !! C-interoperable signal handler that closes database, removes message
        !! queue, and stops program.
        integer(c_int), intent(in), value :: signum

        call logger%debug('exit on on signal ' // dm_signal_name(signum))
        call shutdown(E_NONE)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_db_version(.true.)
    end subroutine version_callback
end program dmupload
