! dmapi.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmapi
    !! An HTTP-RPC service for DMPACK database access. A FastCGI-compatible web
    !! server, such as lighttpd, is required to run this web app.
    !!
    !! Observations and log messages sent via HTTP POST are expected to be in
    !! Fortran 95 Namelist format, with optional deflate or zstd compression.
    !! The server returns data in CSV format with optional header by default.
    !! The client has to set an HTTP Accept header to request JSON or JSON
    !! Lines format. Error and status messages are returned as plain-text
    !! (key-value pairs).
    !!
    !! Configure the web app through FastCGI environment variables:
    !!
    !! | Variable       | Description                                  |
    !! |----------------|----------------------------------------------|
    !! | `DM_BEAT_DB`   | Path to beat database.                       |
    !! | `DM_IMAGE_DB`  | Path to image database.                      |
    !! | `DM_LOG_DB`    | Path to log database.                        |
    !! | `DM_OBSERV_DB` | Path to observation database.                |
    !! | `DM_READ_ONLY` | Open databases in read-only mode (optional). |
    !!
    !! If HTTP Basic Auth is enabled, the sensor id of each beat, log, node,
    !! sensor, and observation sent to the RPC service must match the name of
    !! the authenticated user. For example, to store an observation of the node
    !! with id `node-1`, the HTTP Basic Auth user name must be `node-1`. If the
    !! observation is sent by any other user, it will be rejected (HTTP 401).
    use :: dmpack, NL => ASCII_LF
    implicit none (type, external)

    ! Program version.
    integer, parameter :: APP_MAJOR = 0
    integer, parameter :: APP_MINOR = 9
    integer, parameter :: APP_PATCH = 8

    ! Program parameters.
    integer, parameter :: APP_DB_TIMEOUT   = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.
    integer, parameter :: APP_MAX_NLOGS    = 10000              !! Max. number of logs per request.
    integer, parameter :: APP_MAX_NOBSERVS = 10000              !! Max. number of observations per request.
    integer, parameter :: APP_NROUTES      = 16                 !! Total number of routes.
    logical, parameter :: APP_CSV_HEADER   = .false.            !! Add CSV header by default.
    logical, parameter :: APP_READ_ONLY    = .false.            !! Default database access mode.

    ! Global settings.
    character(len=FILE_PATH_LEN) :: beat_db     = ' '           ! Path to beat database.
    character(len=FILE_PATH_LEN) :: image_db    = ' '           ! Path to image database.
    character(len=FILE_PATH_LEN) :: log_db      = ' '           ! Path to log database.
    character(len=FILE_PATH_LEN) :: observ_db   = ' '           ! Path to observation database.
    logical                      :: read_only   = APP_READ_ONLY ! Read-only flag for databases.

    integer               :: n, rc, status
    type(cgi_env_type)    :: env
    type(cgi_route_type)  :: routes(APP_NROUTES)
    type(cgi_router_type) :: router

    ! Initialise DMPACK.
    call dm_init()

    ! Add routes.
    routes = [ &
        cgi_route_type('',            route_root),      &
        cgi_route_type('/',           route_root),      &
        cgi_route_type('/beat',       route_beat),      &
        cgi_route_type('/beats',      route_beats),     &
        cgi_route_type('/image',      route_image),     &
        cgi_route_type('/log',        route_log),       &
        cgi_route_type('/logs',       route_logs),      &
        cgi_route_type('/node',       route_node),      &
        cgi_route_type('/nodes',      route_nodes),     &
        cgi_route_type('/observ',     route_observ),    &
        cgi_route_type('/observs',    route_observs),   &
        cgi_route_type('/sensor',     route_sensor),    &
        cgi_route_type('/sensors',    route_sensors),   &
        cgi_route_type('/target',     route_target),    &
        cgi_route_type('/targets',    route_targets),   &
        cgi_route_type('/timeseries', route_timeseries) &
    ]

    ! Read environment variables.
    rc = dm_env_get('DM_BEAT_DB',   beat_db,   n)
    rc = dm_env_get('DM_IMAGE_DB',  image_db,  n)
    rc = dm_env_get('DM_LOG_DB',    log_db,    n)
    rc = dm_env_get('DM_OBSERV_DB', observ_db, n)
    rc = dm_env_get('DM_READ_ONLY', read_only, APP_READ_ONLY)

    ! Set API routes.
    rc = dm_cgi_router_set(router, routes)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Run event loop.
    do while (dm_fcgi_accept())
        call dm_cgi_env(env)
        call dm_cgi_router_dispatch(router, env, status)
        if (status == HTTP_OK) cycle
        call api_error(status, dm_error_message(E_NOT_FOUND), E_NOT_FOUND)
    end do

    ! Clean up.
    call dm_cgi_router_destroy(router)
contains
    ! **************************************************************************
    ! ENDPOINTS.
    ! **************************************************************************
    subroutine route_beat(env)
        !! Accepts beat in Namelist format via HTTP POST. Returns beat of
        !! a given node id in CSV, JSON or Namelist format to GET requests.
        !!
        !! ## Path
        !!
        !! * `/api/v1/beat`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        !! ## GET Parameters
        !!
        !! * `node_id` - Node id (required).
        !! * `header`  - CSV header (0 or 1).
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Heartbeat is returned.
        !! * `400` - Invalid request.
        !! * `404` - Heartbeat was not found.
        !! * `503` - Database error.
        !!
        !! ## POST Request Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## POST Responses
        !!
        !! * `201` - Heartbeat was accepted.
        !! * `400` - Invalid request or payload.
        !! * `401` - Unauthorised.
        !! * `415` - Invalid payload format.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, beat_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=MIME_LEN)       :: mime
            character(len=NML_NODE_LEN)   :: buffer
            character(len=NODE_ID_LEN)    :: node_id
            integer                       :: format, z
            logical                       :: header
            type(cgi_param_type)          :: param
            type(beat_type)               :: beat

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                if (env%content_type /= MIME_NML) then
                    call api_error(HTTP_UNSUPPORTED_MEDIA_TYPE, 'invalid content type', E_INVALID)
                    exit response_block
                end if

                ! Read request content.
                rc = dm_fcgi_read(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                z = dm_z_type_from_encoding(env%http_content_encoding)

                if (z == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, z, beat)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'corrupted payload', rc)
                    exit response_block
                end if

                ! Validate beat.
                if (.not. dm_beat_is_valid(beat)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid beat data', E_INVALID)
                    exit response_block
                end if

                ! Validate node id.
                if (dm_cgi_auth_basic(env) .and. env%remote_user /= beat%node_id) then
                    call api_error(HTTP_UNAUTHORIZED, 'node id does not match user name', E_RPC_AUTH)
                    exit response_block
                end if

                ! Set remote IP address and time received.
                call dm_beat_set(beat, address=env%remote_addr, time_recv=dm_time_now())

                ! Insert beat into database.
                rc = dm_db_insert(db, beat, validate=.false.)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_SERVICE_UNAVAILABLE, 'database insert failed', rc)
                    exit response_block
                end if

                ! Empty response on success.
                call dm_fcgi_header(MIME_TEXT, HTTP_CREATED)
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call dm_cgi_query(env, param)

            ! Mandatory GET parameters.
            if (dm_cgi_get(param, 'node_id', node_id) /= E_NONE) then
                call api_error(HTTP_BAD_REQUEST, 'missing parameter node_id', E_INVALID)
                exit response_block
            end if

            ! Validate parameters.
            if (.not. dm_id_is_valid(node_id)) then
                call api_error(HTTP_BAD_REQUEST, 'invalid parameter node_id', E_INVALID)
                exit response_block
            end if

            ! Optional GET parameters.
            rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)

            ! Get beat from database.
            rc = dm_db_select(db, beat, node_id)

            if (rc == E_DB_NO_ROWS) then
                call api_error(HTTP_NOT_FOUND, 'beat not found', rc)
                exit response_block
            end if

            if (dm_is_error(rc)) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime)

            select case (format)
                case (FORMAT_CSV)
                    if (header) call dm_fcgi_write(dm_csv_header_beat())
                    call dm_fcgi_write(dm_csv_from(beat))
                case (FORMAT_JSON)
                    call dm_fcgi_write(dm_json_from(beat))
                case (FORMAT_NML)
                    rc = dm_nml_from(beat, buffer)
                    call dm_fcgi_write(trim(buffer))
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_beat

    subroutine route_beats(env)
        !! Returns list of all beats in database in CSV, JSON, or JSON Lines
        !! format. If no `Accept` header is set, beats are returned in CSV
        !! format.
        !!
        !! ## Path
        !!
        !! * `/api/v1/beats`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `header` - CSV header (0 or 1).
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/jsonl`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Beats are returned.
        !! * `404` - No beats found.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, beat_db, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=MIME_LEN) :: mime
            integer                 :: format
            integer(kind=i8)        :: i, n
            logical                 :: header
            type(cgi_param_type)    :: param
            type(db_stmt_type)      :: db_stmt
            type(beat_type)         :: beat

            ! Get query parameters.
            call dm_cgi_query(env, param)
            rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)

            ! Get number of beats in database.
            rc = dm_db_count_beats(db, n)

            if (dm_is_error(rc)) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            ! Get MIME type from HTTP Accept header.
            call content_type(env, mime, MIME_CSV, format)

            ! Return empty response if no beats have been found.
            if (n == 0) then
                call dm_fcgi_header(mime, HTTP_NOT_FOUND)

                select case (format)
                    case (FORMAT_CSV);  if (header) call dm_fcgi_write(dm_csv_header_beat())
                    case (FORMAT_JSON); call dm_fcgi_write('[]')
                end select

                exit response_block
            end if

            ! Output serialised beats.
            call dm_fcgi_header(mime, HTTP_OK)

            do i = 1, n
                rc = dm_db_select_beats(db, db_stmt, beat, validate=(n == 1))
                if (rc /= E_DB_ROW) exit

                select case (format)
                    case (FORMAT_CSV);   call csv_iter(TYPE_BEAT, i, header, dm_csv_from(beat))
                    case (FORMAT_JSON);  call json_iter(i, n, dm_json_from(beat))
                    case (FORMAT_JSONL); call jsonl_iter(dm_json_from(beat))
                end select
            end do

            call dm_db_finalize(db_stmt)
        end block response_block

        call dm_db_close(db)
    end subroutine route_beats

    subroutine route_image(env)
        !! On POST, tries to create new image transfer and returns a new
        !! transfer token in HTTP response header `dmpack-transfer-id`.
        !!
        !! On PUT, searches database for transfer token passed in HTTP request
        !! header `dmpack-transfer-id` and stores payload. The payload size
        !! must match the image size accepted in the POST request.
        !!
        !! ## Path
        !!
        !! * `/api/v1/image`
        !!
        !! ## Methods
        !!
        !! * POST
        !! * PUT
        !!
        !! ## POST Request Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## POST Response Headers
        !!
        !! * `dmpack-transfer-id` - Transfer token for image upload (UUIDv4).
        !!
        !! ## POST Responses
        !!
        !! * `202` - Image transfer was accepted.
        !! * `400` - Invalid request or payload.
        !! * `401` - Unauthorised.
        !! * `405` - Invalid HTTP method.
        !! * `409` - Image exists in database.
        !! * `415` - Invalid payload format.
        !! * `503` - Database error.
        !!
        !! ## PUT Request Headers
        !!
        !! * `Content-Length`     - Image size, must match image size passed in POST request.
        !! * `Content-Type`       - `image/jpeg`, `image/png`
        !! * `dmpack-transfer-id` - Transfer token (UUIDv4).
        !!
        type(cgi_env_type), intent(inout) :: env

        character(len=:), allocatable :: content
        character(len=UUID_LEN)       :: headers(2)
        integer                       :: rc, z
        type(db_type)                 :: db
        type(image_type)              :: image
        type(transfer_type)           :: transfer

        ! Open image database.
        rc = dm_db_open(db, image_db, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        method_select: select case (env%request_method)
            case ('POST')
                ! Payload MIME type.
                if (env%content_type /= MIME_NML) then
                    call api_error(HTTP_UNSUPPORTED_MEDIA_TYPE, 'invalid content type', E_INVALID)
                    exit method_select
                end if

                ! Payload compression type.
                z = dm_z_type_from_encoding(env%http_content_encoding)

                if (z == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit method_select
                end if

                ! Read request content.
                rc = dm_fcgi_read(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit method_select
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, z, image)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'corrupted payload', rc)
                    exit method_select
                end if

                ! Validate node id.
                if (dm_cgi_auth_basic(env) .and. env%remote_user /= image%node_id) then
                    call api_error(HTTP_UNAUTHORIZED, 'node id does not match user name', E_RPC_AUTH)
                    exit method_select
                end if

                ! Validate image data.
                if (.not. dm_image_is_valid(image)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid image data', E_INVALID)
                    exit method_select
                end if

                ! Validate uniqueness.
                rc = dm_db_select_transfer(db, transfer, type_id=image%id)

                if (dm_is_ok(rc)) then
                    call api_error(HTTP_CONFLICT, 'image exists', E_EXIST)
                    exit method_select
                end if

                ! Insert image into database.
               !rc = dm_db_insert(db, image, validate=.false.)
               !
               !if (dm_is_error(rc)) then
               !    call api_error(HTTP_SERVICE_UNAVAILABLE, 'image insert failed', E_EXIST)
               !    exit method_select
               !end if

                ! Create transfer.
                rc = dm_transfer_create(transfer, image%node_id, image%id, TRANSFER_TYPE_IMAGE, image%size)

                ! Insert transfer into database.
               !rc = dm_db_insert(db, transfer)
               !
               !if (dm_is_error(rc)) then
               !    call api_error(HTTP_SERVICE_UNAVAILABLE, 'transfer insert failed', rc)
               !    exit response_block
               !end if

                ! Return token in HTTP header.
                headers = [ character(len=TRANSFER_ID_LEN) :: HTTP_HEADER_TRANSFER_ID, transfer%id ]
                call dm_fcgi_header(MIME_TEXT, HTTP_ACCEPTED, headers)

            case ('PUT')
                ! TODO
                call api_error(HTTP_METHOD_NOT_ALLOWED, 'invalid request method', E_INVALID)

            case default
                call api_error(HTTP_METHOD_NOT_ALLOWED, 'invalid request method', E_INVALID)
        end select method_select

        call dm_db_close(db)
    end subroutine route_image

    subroutine route_log(env)
        !! Accepts log in Namelist format via HTTP POST. Returns single log of
        !! passed log id in CSV, JSON, or Namelist format to GET requests.
        !!
        !! ## Path
        !!
        !! * `/api/v1/log`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        !! ## GET Parameters
        !!
        !! * `id` - Log id (UUID).
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Log is returned.
        !! * `400` - Invalid request.
        !! * `404` - Log was not found.
        !! * `503` - Database error.
        !!
        !! ## POST Request Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## POST Responses
        !!
        !! * `201` - Log was accepted.
        !! * `400` - Invalid request or payload.
        !! * `401` - Unauthorised.
        !! * `409` - Log exists in database.
        !! * `415` - Invalid payload format.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, log_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=MIME_LEN)       :: mime
            character(len=NML_NODE_LEN)   :: buffer
            character(len=LOG_ID_LEN)     :: id
            integer                       :: format, z
            type(cgi_param_type)          :: param
            type(log_type)                :: log

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                if (env%content_type /= MIME_NML) then
                    call api_error(HTTP_UNSUPPORTED_MEDIA_TYPE, 'invalid content type', E_INVALID)
                    exit response_block
                end if

                ! Read request content.
                rc = dm_fcgi_read(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                z = dm_z_type_from_encoding(env%http_content_encoding)

                if (z == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, z, log)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'corrupted payload', rc)
                    exit response_block
                end if

                ! Validate log.
                if (.not. dm_log_is_valid(log)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid log data', E_INVALID)
                    exit response_block
                end if

                ! Validate node id.
                if (dm_cgi_auth_basic(env)) then
                    if (env%remote_user /= log%node_id) then
                        call api_error(HTTP_UNAUTHORIZED, 'node id does not match user name', E_RPC_AUTH)
                        exit response_block
                    end if
                end if

                ! Validate uniqueness.
                if (dm_db_has_log(db, log%id)) then
                    call api_error(HTTP_CONFLICT, 'log exists', E_EXIST)
                    exit response_block
                end if

                ! Insert log into database.
                rc = dm_db_insert(db, log, validate=.false.)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_SERVICE_UNAVAILABLE, 'database insert failed', rc)
                    exit response_block
                end if

                ! Empty response on success.
                call dm_fcgi_header(MIME_TEXT, HTTP_CREATED)
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call dm_cgi_query(env, param)

            ! Mandatory GET parameters.
            if (dm_cgi_get(param, 'id', id) /= E_NONE) then
                call api_error(HTTP_BAD_REQUEST, 'missing parameter id', E_INVALID)
                exit response_block
            end if

            ! Validate parameters.
            if (.not. dm_uuid4_is_valid(id)) then
                call api_error(HTTP_BAD_REQUEST, 'invalid parameter id', E_INVALID)
                exit response_block
            end if

            ! Get log from database.
            rc = dm_db_select(db, log, id)

            if (rc == E_DB_NO_ROWS) then
                call api_error(HTTP_NOT_FOUND, 'log not found', rc)
                exit response_block
            end if

            if (dm_is_error(rc)) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime, HTTP_OK)

            select case (format)
                case (FORMAT_CSV)
                    call dm_fcgi_write(dm_csv_from(log))
                case (FORMAT_JSON)
                    call dm_fcgi_write(dm_json_from(log))
                case (FORMAT_NML)
                    rc = dm_nml_from(log, buffer)
                    call dm_fcgi_write(trim(buffer))
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_log

    subroutine route_logs(env)
        !! Returns logs of a given time range in CSV, JSON, or JSON Lines format
        !! from database.
        !!
        !! ## Path
        !!
        !! * `/api/v1/logs`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `node_id` - Node id.
        !! * `from`    - Start timestamp (ISO 8601).
        !! * `to`      - End timestamp (ISO 8601).
        !! * `header`  - CSV header (0 or 1).
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/jsonl`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Logs are returned.
        !! * `400` - Invalid request.
        !! * `404` - No logs found.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, log_db, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=MIME_LEN)     :: mime
            character(len=NODE_ID_LEN)  :: node_id
            character(len=TIME_LEN)     :: from, to
            integer                     :: code, format, i, limit_
            integer(kind=i8)            :: limit
            logical                     :: header
            type(cgi_param_type)        :: param
            type(log_type), allocatable :: logs(:)

            ! GET request.
            call dm_cgi_query(env, param)
            rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)

            rc   = E_INVALID
            code = HTTP_BAD_REQUEST

            ! Mandatory GET parameters.
            if (dm_cgi_get(param, 'node_id', node_id) /= E_NONE) then
                call api_error(code, 'missing parameter node_id', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'from', from) /= E_NONE) then
                call api_error(code, 'missing parameter from', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'to', to) /= E_NONE) then
                call api_error(code, 'missing parameter to', rc)
                exit response_block
            end if

            ! Validate parameters.
            if (.not. dm_id_is_valid(node_id)) then
                call api_error(code, 'invalid parameter node_id', rc)
                exit response_block
            end if

            if (.not. dm_time_is_valid(from)) then
                call api_error(code, 'invalid parameter from', rc)
                exit response_block
            end if

            if (.not. dm_time_is_valid(to)) then
                call api_error(code, 'invalid parameter to', rc)
                exit response_block
            end if

            ! Optional parameters.
            limit = APP_MAX_NLOGS

            if (dm_cgi_get(param, 'limit', limit_) == E_NONE) then
                if (limit_ < 1 .or. limit_ > APP_MAX_NLOGS) then
                    call api_error(code, 'invalid parameter limit', rc)
                    exit response_block
                end if
                limit = limit_
            end if

            ! Select logs from database.
            rc = dm_db_select_logs(db, logs, node_id=node_id, from=from, to=to, limit=limit)

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            code = HTTP_OK
            if (size(logs) == 0) code = HTTP_NOT_FOUND

            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime, code)

            select case (format)
                case (FORMAT_CSV)
                    if (header) call dm_fcgi_write(dm_csv_header_log())

                    do i = 1, size(logs)
                        call dm_fcgi_write(dm_csv_from(logs(i)) // NL)
                    end do

                case (FORMAT_JSON)
                    if (size(logs) == 0) then
                        call dm_fcgi_write('[]')
                        exit response_block
                    end if

                    do i = 1, size(logs)
                        call json_iter(int(i, kind=i8), size(logs, kind=i8), dm_json_from(logs(i)))
                    end do

                case (FORMAT_JSONL)
                    do i = 1, size(logs)
                        call jsonl_iter(dm_json_from(logs(i)))
                    end do
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_logs

    subroutine route_node(env)
        !! Returns node of given node id in CSV, JSON, or Namelist format from
        !! database. On POST, adds node to database.
        !!
        !! ## Path
        !!
        !! * `/api/v1/node`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        !! ## GET Parameters
        !!
        !! * `id` - Node id.
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Node is returned.
        !! * `400` - Invalid request.
        !! * `404` - Node was not found.
        !! * `503` - Database error.
        !!
        !! ## POST Request Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## POST Responses
        !!
        !! * `201` - Node was accepted.
        !! * `400` - Invalid request or payload.
        !! * `401` - Unauthorised.
        !! * `409` - Node exists in database.
        !! * `415` - Invalid payload format.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, observ_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=MIME_LEN)       :: mime
            character(len=NML_NODE_LEN)   :: buffer
            character(len=NODE_ID_LEN)    :: id
            integer                       :: format, z
            type(cgi_param_type)          :: param
            type(node_type)               :: node

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                if (env%content_type /= MIME_NML) then
                    call api_error(HTTP_UNSUPPORTED_MEDIA_TYPE, 'invalid content type', E_INVALID)
                    exit response_block
                end if

                ! Read request content.
                rc = dm_fcgi_read(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                z = dm_z_type_from_encoding(env%http_content_encoding)

                if (z == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, z, node)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'corrupted payload', rc)
                    exit response_block
                end if

                ! Validate node data.
                if (.not. dm_node_is_valid(node)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid node data', E_INVALID)
                    exit response_block
                end if

                ! Validate node id.
                if (dm_cgi_auth_basic(env)) then
                    if (env%remote_user /= node%id) then
                        call api_error(HTTP_UNAUTHORIZED, 'node id does not match user name', E_RPC_AUTH)
                        exit response_block
                    end if
                end if

                ! Validate uniqueness.
                if (dm_db_has_node(db, node%id)) then
                    call api_error(HTTP_CONFLICT, 'node exists', E_EXIST)
                    exit response_block
                end if

                ! Insert node into database.
                rc = dm_db_insert(db, node, validate=.false.)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_SERVICE_UNAVAILABLE, 'database insert failed', rc)
                    exit response_block
                end if

                ! Empty response on success.
                call dm_fcgi_header(MIME_TEXT, HTTP_CREATED)
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call dm_cgi_query(env, param)

            ! Mandatory GET parameters.
            if (dm_cgi_get(param, 'id', id) /= E_NONE) then
                call api_error(HTTP_BAD_REQUEST, 'missing parameter id', E_INVALID)
                exit response_block
            end if

            ! Validate parameters.
            if (.not. dm_id_is_valid(id)) then
                call api_error(HTTP_BAD_REQUEST, 'invalid parameter id', E_INVALID)
                exit response_block
            end if

            ! Get node from database.
            rc = dm_db_select(db, node, id)

            if (rc == E_DB_NO_ROWS) then
                call api_error(HTTP_NOT_FOUND, 'node not found', rc)
                exit response_block
            end if

            if (dm_is_error(rc)) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            ! Select MIME type from HTTP Accept header.
            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime, HTTP_OK)

            select case (format)
                case (FORMAT_CSV)
                    call dm_fcgi_write(dm_csv_from(node))
                case (FORMAT_JSON)
                    call dm_fcgi_write(dm_json_from(node))
                case (FORMAT_NML)
                    rc = dm_nml_from(node, buffer)
                    call dm_fcgi_write(trim(buffer))
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_node

    subroutine route_nodes(env)
        !! Returns all nodes in CSV, JSON, JSON Lines format from database.
        !!
        !! ## Path
        !!
        !! * `/api/v1/nodes`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `header`  - CSV header (0 or 1).
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/jsonl`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Nodes are returned.
        !! * `404` - No nodes found.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=MIME_LEN)      :: mime
            integer                      :: code, format
            logical                      :: header
            type(cgi_param_type)         :: param
            type(node_type), allocatable :: nodes(:)

            ! Select all nodes from database.
            rc = dm_db_select_nodes(db, nodes)

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            code = HTTP_OK
            if (size(nodes) == 0) code = HTTP_NOT_FOUND

            ! Select MIME type from HTTP Accept header.
            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime, code)

            select case (format)
                case (FORMAT_CSV)
                    call dm_cgi_query(env, param)
                    rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)
                    call dm_fcgi_write(dm_csv_from(nodes, header=header))
                case (FORMAT_JSON)
                    call dm_fcgi_write(dm_json_from(nodes))
                case (FORMAT_JSONL)
                    call dm_fcgi_write(dm_jsonl_from(nodes))
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_nodes

    subroutine route_observ(env)
        !! Returns observation of given id in CSV, JSON, or Namelist format
        !! from database. On POST, adds observation to database.
        !!
        !! ## Path
        !!
        !! * `/api/v1/observ`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        !! ## GET Parameters
        !!
        !! * `id` - Observation id (UUID).
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Observation is returned.
        !! * `400` - Invalid request.
        !! * `404` - Observation was not found.
        !! * `503` - Database error.
        !!
        !! ## POST Request Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## POST Responses
        !!
        !! * `201` - Observation was accepted.
        !! * `400` - Invalid request or payload.
        !! * `401` - Unauthorised.
        !! * `409` - Observation exists in database.
        !! * `415` - Invalid payload format.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, observ_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=MIME_LEN)       :: mime
            character(len=NML_OBSERV_LEN) :: buffer
            character(len=OBSERV_ID_LEN)  :: id
            integer                       :: format, z
            type(cgi_param_type)          :: param
            type(observ_type)             :: observ

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                if (env%content_type /= MIME_NML) then
                    call api_error(HTTP_UNSUPPORTED_MEDIA_TYPE, 'invalid content type', E_INVALID)
                    exit response_block
                end if

                ! Read request content.
                rc = dm_fcgi_read(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                z = dm_z_type_from_encoding(env%http_content_encoding)

                if (z == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, z, observ)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'corrupted payload', rc)
                    exit response_block
                end if

                ! Validate observation data.
                if (.not. dm_observ_is_valid(observ)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid observ data', E_INVALID)
                    exit response_block
                end if

                ! Validate node id.
                if (dm_cgi_auth_basic(env)) then
                    if (env%remote_user /= observ%node_id) then
                        call api_error(HTTP_UNAUTHORIZED, 'node id does not match user name', E_RPC_AUTH)
                        exit response_block
                    end if
                end if

                ! Validate uniqueness.
                if (dm_db_has_observ(db, observ%id)) then
                    call api_error(HTTP_CONFLICT, 'observation exists', E_EXIST)
                    exit response_block
                end if

                ! Insert observation into database.
                rc = dm_db_insert(db, observ, validate=.false.)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_SERVICE_UNAVAILABLE, 'database insert failed', rc)
                    exit response_block
                end if

                ! Empty response on success.
                call dm_fcgi_header(MIME_TEXT, HTTP_CREATED)
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call dm_cgi_query(env, param)

            ! Mandatory GET parameters.
            if (dm_cgi_get(param, 'id', id) /= E_NONE) then
                call api_error(HTTP_BAD_REQUEST, 'missing parameter id', E_INVALID)
                exit response_block
            end if

            ! Validate parameters.
            if (.not. dm_uuid4_is_valid(id)) then
                call api_error(HTTP_BAD_REQUEST, 'invalid parameter id', E_INVALID)
                exit response_block
            end if

            ! Get observation from database.
            rc = dm_db_select(db, observ, id)

            if (rc == E_DB_NO_ROWS) then
                call api_error(HTTP_NOT_FOUND, 'observation not found', rc)
                exit response_block
            end if

            if (dm_is_error(rc)) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            ! Select MIME type from HTTP Accept header.
            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime, HTTP_OK)

            select case (format)
                case (FORMAT_CSV)
                    call dm_fcgi_write(dm_csv_from(observ))
                case (FORMAT_JSON)
                    call dm_fcgi_write(dm_json_from(observ))
                case (FORMAT_NML)
                    rc = dm_nml_from(observ, buffer)
                    call dm_fcgi_write(trim(buffer))
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_observ

    subroutine route_observs(env)
        !! Returns observations of given node, sensor, target, and time range
        !! in CSV, JSON, or JSON Lines format from database.
        !!
        !! ## Path
        !!
        !! * `/api/v1/observs`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `node_id`   - Node id.
        !! * `sensor_id` - Sensor id.
        !! * `target_id` - Target id.
        !! * `from`      - Start timestamp (ISO 8601).
        !! * `to`        - End timestamp (ISO 8601).
        !! * `limit`     - Max. number of results (optional).
        !! * `header`    - CSV header (0 or 1).
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/jsonl`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Observations are returned.
        !! * `400` - Invalid request.
        !! * `404` - No observations found.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=MIME_LEN)        :: mime
            character(len=NODE_ID_LEN)     :: node_id
            character(len=SENSOR_ID_LEN)   :: sensor_id
            character(len=TARGET_ID_LEN)   :: target_id
            character(len=TIME_LEN)        :: from, to
            integer                        :: i, code, format
            integer                        :: limit, limit_
            logical                        :: header
            type(cgi_param_type)           :: param
            type(observ_type), allocatable :: observs(:)

            ! GET request.
            call dm_cgi_query(env, param)

            ! Mandatory parameters.
            rc   = E_INVALID
            code = HTTP_BAD_REQUEST

            if (dm_cgi_get(param, 'node_id', node_id) /= E_NONE) then
                call api_error(code, 'missing parameter node_id', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'sensor_id', sensor_id) /= E_NONE) then
                call api_error(code, 'missing parameter sensor_id', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'target_id', target_id) /= E_NONE) then
                call api_error(code, 'missing parameter target_id', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'from', from) /= E_NONE) then
                call api_error(code, 'missing parameter from', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'to', to) /= E_NONE) then
                call api_error(code, 'missing parameter to', rc)
                exit response_block
            end if

            ! Validate parameters.
            if (.not. dm_id_is_valid(node_id)) then
                call api_error(code, 'invalid parameter node_id', rc)
                exit response_block
            end if

            if (.not. dm_id_is_valid(sensor_id)) then
                call api_error(code, 'invalid parameter sensor_id', rc)
                exit response_block
            end if

            if (.not. dm_id_is_valid(target_id)) then
                call api_error(code, 'invalid parameter target_id', rc)
                exit response_block
            end if

            if (.not. dm_time_is_valid(from)) then
                call api_error(code, 'invalid parameter from', rc)
                exit response_block
            end if

            if (.not. dm_time_is_valid(to)) then
                call api_error(code, 'invalid parameter to', rc)
                exit response_block
            end if

            ! Optional parameters.
            limit = APP_MAX_NOBSERVS

            if (dm_cgi_get(param, 'limit', limit_) == E_NONE) then
                if (limit_ < 1 .or. limit_ > APP_MAX_NOBSERVS) then
                    call api_error(code, 'invalid parameter limit', rc)
                    exit response_block
                end if
                limit = limit_
            end if

            ! Select observations from database.
            rc = dm_db_select_observs(db, observs, node_id=node_id, sensor_id=sensor_id, target_id=target_id, &
                                      from=from, to=to, limit=int(limit, kind=i8))

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            code = HTTP_OK
            if (size(observs) == 0) code = HTTP_NOT_FOUND

            ! Select MIME type from HTTP Accept header.
            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime, code)

            select case (format)
                case (FORMAT_CSV)
                    rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)
                    if (header) call dm_fcgi_write(dm_csv_header_observ())

                    do i = 1, size(observs)
                        call dm_fcgi_write(dm_csv_from(observs(i)))
                    end do

                case (FORMAT_JSON)
                    if (size(observs) == 0) then
                        call dm_fcgi_write('[]')
                        exit response_block
                    end if

                    do i = 1, size(observs)
                        call json_iter(int(i, kind=i8), size(observs, kind=i8), dm_json_from(observs(i)))
                    end do

                case (FORMAT_JSONL)
                    do i = 1, size(observs)
                        call jsonl_iter(dm_json_from(observs(i)))
                    end do
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_observs

    subroutine route_root(env)
        !! Returns service status in API status format.
        !!
        !! ## Path
        !!
        !! * `/api/v1/`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Responses
        !!
        !! * `200` - Always.
        !!
        type(cgi_env_type), intent(inout) :: env

        type(api_status_type) :: status

        call dm_api_status_set(status    = status, &
                               version   = dm_version_to_string(APP_MAJOR, APP_MINOR, APP_PATCH), &
                               dmpack    = DM_VERSION_STRING, &
                               host      = env%server_name, &
                               server    = env%server_software, &
                               timestamp = dm_time_now())

        ! Check database availability.
        if (.not. dm_file_exists(beat_db)) then
            status%error   = E_NOT_FOUND
            status%message = 'beat database not found'
        else if (.not. dm_file_exists(log_db)) then
            status%error   = E_NOT_FOUND
            status%message = 'log database not found'
        else if (.not. dm_file_exists(observ_db)) then
            status%error   = E_NOT_FOUND
            status%message = 'observation database not found'
        else
            status%error   = E_NONE
            status%message = 'online'
        end if

        call dm_fcgi_header(MIME_TEXT, HTTP_OK)
        call dm_fcgi_write(dm_api_status_to_string(status))
    end subroutine route_root

    subroutine route_sensor(env)
        !! Returns sensor of given sensor id in CSV, JSON, or Namelist format
        !! from database. On POST, adds node to database.
        !!
        !! ## Path
        !!
        !! * `/api/v1/sensor`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        !! ## GET Parameters
        !!
        !! * `id` - Sensor id.
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Sensor is returned.
        !! * `400` - Invalid request.
        !! * `404` - Sensor was not found.
        !! * `503` - Database error.
        !!
        !! ## POST Request Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## POST Responses
        !!
        !! * `201` - Sensor was accepted.
        !! * `400` - Invalid request or payload.
        !! * `401` - Unauthorised.
        !! * `409` - Sensor exists in database.
        !! * `415` - Invalid payload format.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, observ_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=MIME_LEN)       :: mime
            character(len=NML_SENSOR_LEN) :: buffer
            character(len=SENSOR_ID_LEN)  :: id
            integer                       :: format, z
            type(cgi_param_type)          :: param
            type(sensor_type)             :: sensor

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                if (env%content_type /= MIME_NML) then
                    call api_error(HTTP_UNSUPPORTED_MEDIA_TYPE, 'invalid content type', E_INVALID)
                    exit response_block
                end if

                ! Read request content.
                rc = dm_fcgi_read(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                z = dm_z_type_from_encoding(env%http_content_encoding)

                if (z == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, z, sensor)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'corrupted payload', rc)
                    exit response_block
                end if

                ! Validate sensor data.
                if (.not. dm_sensor_is_valid(sensor)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid sensor data', E_INVALID)
                    exit response_block
                end if

                ! Validate node id.
                if (dm_cgi_auth_basic(env)) then
                    if (env%remote_user /= sensor%node_id) then
                        call api_error(HTTP_UNAUTHORIZED, 'node id does not match user name', E_RPC_AUTH)
                        exit response_block
                    end if
                end if

                ! Validate uniqueness.
                if (dm_db_has_sensor(db, sensor%id)) then
                    call api_error(HTTP_CONFLICT, 'sensor exists', E_EXIST)
                    exit response_block
                end if

                ! Insert sensor into database.
                rc = dm_db_insert(db, sensor, validate=.false.)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_SERVICE_UNAVAILABLE, 'database insert failed', rc)
                    exit response_block
                end if

                ! Empty response on success.
                call dm_fcgi_header(MIME_TEXT, HTTP_CREATED)
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call dm_cgi_query(env, param)

            ! Mandatory GET parameters.
            if (dm_cgi_get(param, 'id', id) /= E_NONE) then
                call api_error(HTTP_BAD_REQUEST, 'missing parameter id', E_INVALID)
                exit response_block
            end if

            ! Validate parameters.
            if (.not. dm_id_is_valid(id)) then
                call api_error(HTTP_BAD_REQUEST, 'invalid parameter id', E_INVALID)
                exit response_block
            end if

            ! Get sensor from database.
            rc = dm_db_select(db, sensor, id)

            if (rc == E_DB_NO_ROWS) then
                call api_error(HTTP_NOT_FOUND, 'sensor not found', rc)
                exit response_block
            end if

            if (dm_is_error(rc)) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            ! Select MIME type from HTTP Accept header.
            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime, HTTP_OK)

            select case (format)
                case (FORMAT_CSV)
                    call dm_fcgi_write(dm_csv_from(sensor))
                case (FORMAT_JSON)
                    call dm_fcgi_write(dm_json_from(sensor))
                case (FORMAT_NML)
                    rc = dm_nml_from(sensor, buffer)
                    call dm_fcgi_write(trim(buffer))
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_sensor

    subroutine route_sensors(env)
        !! Returns all sensor in database in CSV, JSON, or JSON Lines format.
        !!
        !! ## Path
        !!
        !! * `/api/v1/sensors`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `header` - CSV header (0 or 1).
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/jsonl`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Sensors are returned.
        !! * `400` - Invalid request.
        !! * `404` - No sensors found.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=MIME_LEN)        :: mime
            integer                        :: code, format
            logical                        :: header
            type(cgi_param_type)           :: param
            type(sensor_type), allocatable :: sensors(:)

            ! Select all sensors from database.
            rc = dm_db_select_sensors(db, sensors)

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            code = HTTP_OK
            if (size(sensors) == 0) code = HTTP_NOT_FOUND

            ! Select MIME type from HTTP Accept header.
            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime, code)

            select case (format)
                case (FORMAT_CSV)
                    call dm_cgi_query(env, param)
                    rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)
                    call dm_fcgi_write(dm_csv_from(sensors, header=header))
                case (FORMAT_JSON)
                    call dm_fcgi_write(dm_json_from(sensors))
                case (FORMAT_JSONL)
                    call dm_fcgi_write(dm_jsonl_from(sensors))
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_sensors

    subroutine route_target(env)
        !! Returns target of given target id in CSV, JSON, or Namelist format
        !! from database. On POST, adds target to database.
        !!
        !! ## Path
        !!
        !! * `/api/v1/target`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        !! ## GET Parameters
        !!
        !! * `id` - Target id.
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Target is returned.
        !! * `400` - Invalid request.
        !! * `404` - Target was not found.
        !! * `503` - Database error.
        !!
        !! ## POST Request Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## POST Responses
        !!
        !! * `201` - Target was accepted.
        !! * `400` - Invalid request or payload.
        !! * `401` - Unauthorised.
        !! * `409` - Target exists in database.
        !! * `415` - Invalid payload format.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, observ_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=MIME_LEN)       :: mime
            character(len=NML_TARGET_LEN) :: buffer
            character(len=TARGET_ID_LEN)  :: id
            integer                       :: format, z
            type(cgi_param_type)          :: param
            type(target_type)             :: target

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                if (env%content_type /= MIME_NML) then
                    call api_error(HTTP_UNSUPPORTED_MEDIA_TYPE, 'invalid content type', E_INVALID)
                    exit response_block
                end if

                ! Read request content.
                rc = dm_fcgi_read(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                z = dm_z_type_from_encoding(env%http_content_encoding)

                if (z == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, z, target)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'corrupted payload', rc)
                    exit response_block
                end if

                ! Validate target data.
                if (.not. dm_target_is_valid(target)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid target data', E_INVALID)
                    exit response_block
                end if

                ! Validate uniqueness.
                if (dm_db_has_target(db, target%id)) then
                    call api_error(HTTP_CONFLICT, 'target exists', E_EXIST)
                    exit response_block
                end if

                ! Insert target into database.
                rc = dm_db_insert(db, target, validate=.false.)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_SERVICE_UNAVAILABLE, 'database insert failed', rc)
                    exit response_block
                end if

                ! Empty response on success.
                call dm_fcgi_header(MIME_TEXT, HTTP_CREATED)
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call dm_cgi_query(env, param)

            ! Mandatory GET parameters.
            if (dm_cgi_get(param, 'id', id) /= E_NONE) then
                call api_error(HTTP_BAD_REQUEST, 'missing parameter id', E_INVALID)
                exit response_block
            end if

            ! Validate parameters.
            if (.not. dm_id_is_valid(id)) then
                call api_error(HTTP_BAD_REQUEST, 'invalid parameter id', E_INVALID)
                exit response_block
            end if

            ! Get target from database.
            rc = dm_db_select(db, target, id)

            if (rc == E_DB_NO_ROWS) then
                call api_error(HTTP_NOT_FOUND, 'target not found', rc)
                exit response_block
            end if

            if (dm_is_error(rc)) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            ! Select MIME type from HTTP Accept header.
            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime, HTTP_OK)

            select case (format)
                case (FORMAT_CSV)
                    call dm_fcgi_write(dm_csv_from(target))
                case (FORMAT_JSON)
                    call dm_fcgi_write(dm_json_from(target))
                case (FORMAT_NML)
                    rc = dm_nml_from(target, buffer)
                    call dm_fcgi_write(trim(buffer))
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_target

    subroutine route_targets(env)
        !! Returns all targets in CSV, JSON, or JSON Lines format from database.
        !!
        !! ## Path
        !!
        !! * `/api/v1/targets`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `header` - CSV header (0 or 1).
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `application/json`, `application/jsonl`, `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Targets are returned.
        !! * `404` - No targets found.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=MIME_LEN)        :: mime
            integer                        :: code, format
            logical                        :: header
            type(cgi_param_type)           :: param
            type(target_type), allocatable :: targets(:)

            ! Select all targets from database.
            rc = dm_db_select_targets(db, targets)

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            code = HTTP_OK
            if (size(targets) == 0) code = HTTP_NOT_FOUND

            ! Select MIME type from HTTP Accept header.
            call content_type(env, mime, MIME_CSV, format)
            call dm_fcgi_header(mime, code)

            select case (format)
                case (FORMAT_CSV)
                    call dm_cgi_query(env, param)
                    rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)
                    call dm_fcgi_write(dm_csv_from(targets, header=header))
                case (FORMAT_JSON)
                    call dm_fcgi_write(dm_json_from(targets))
                case (FORMAT_JSONL)
                    call dm_fcgi_write(dm_jsonl_from(targets))
            end select
        end block response_block

        call dm_db_close(db)
    end subroutine route_targets

    subroutine route_timeseries(env)
        !! Returns observations as observation views or data points (X/Y
        !! records) in CSV format from database.
        !!
        !! ## Path
        !!
        !! * `/api/v1/timeseries`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `node_id`   - Node id.
        !! * `sensor_id` - Sensor id.
        !! * `target_id` - Target id.
        !! * `response`  - Response name.
        !! * `from`      - Start timestamp (ISO 8601).
        !! * `to`        - End timestamp (ISO 8601).
        !! * `limit`     - Max. number of results (optional).
        !! * `header`    - CSV header (0 or 1).
        !! * `view`      - Returns observation views (0 or 1).
        !!
        !! ## GET Request Headers
        !!
        !! * `Accept` - `text/comma-separated-values`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Observations are returned.
        !! * `400` - Invalid request.
        !! * `404` - No observations found.
        !! * `503` - Database error.
        !!
        type(cgi_env_type), intent(inout) :: env

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=NODE_ID_LEN)       :: node_id
            character(len=SENSOR_ID_LEN)     :: sensor_id
            character(len=TARGET_ID_LEN)     :: target_id
            character(len=RESPONSE_NAME_LEN) :: response
            character(len=TIME_LEN)          :: from, to
            integer                          :: i, code, limit, limit_
            logical                          :: header, view
            type(cgi_param_type)             :: param

            type(observ_view_type), allocatable :: views(:)
            type(dp_type),          allocatable :: dps(:)

            ! GET request.
            call dm_cgi_query(env, param)

            ! Mandatory GET parameters.
            rc   = E_INVALID
            code = HTTP_BAD_REQUEST

            if (dm_cgi_get(param, 'node_id', node_id) /= E_NONE) then
                call api_error(code, 'missing parameter node_id', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'sensor_id', sensor_id) /= E_NONE) then
                call api_error(code, 'missing parameter sensor_id', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'target_id', target_id) /= E_NONE) then
                call api_error(code, 'missing parameter target_id', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'response', response) /= E_NONE) then
                call api_error(code, 'missing parameter response', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'from', from) /= E_NONE) then
                call api_error(code, 'missing parameter from', rc)
                exit response_block
            end if

            if (dm_cgi_get(param, 'to', to) /= E_NONE) then
                call api_error(code, 'missing parameter to', rc)
                exit response_block
            end if

            ! Validate parameters.
            if (.not. dm_id_is_valid(node_id)) then
                call api_error(code, 'invalid parameter node_id', rc)
                exit response_block
            end if

            if (.not. dm_id_is_valid(sensor_id)) then
                call api_error(code, 'invalid parameter sensor_id', rc)
                exit response_block
            end if

            if (.not. dm_id_is_valid(target_id)) then
                call api_error(code, 'invalid parameter target_id', rc)
                exit response_block
            end if

            if (.not. dm_id_is_valid(response)) then
                call api_error(code, 'invalid parameter response', rc)
                exit response_block
            end if

            if (.not. dm_time_is_valid(from)) then
                call api_error(code, 'invalid parameter from', rc)
                exit response_block
            end if

            if (.not. dm_time_is_valid(to)) then
                call api_error(code, 'invalid parameter to', rc)
                exit response_block
            end if

            ! Optional parameters.
            limit = APP_MAX_NOBSERVS

            if (dm_cgi_get(param, 'limit', limit_) == E_NONE) then
                if (limit_ < 1 .or. limit_ > APP_MAX_NOBSERVS) then
                    call api_error(code, 'invalid parameter limit', rc)
                    exit response_block
                end if
                limit = limit_
            end if

            rc = dm_cgi_get(param, 'header', header, default=APP_CSV_HEADER)
            rc = dm_cgi_get(param, 'view',   view,   default=.false.)

            if (view) then
                ! Select observation views from database.
                rc = dm_db_select_observ_views(db, views, node_id=node_id, sensor_id=sensor_id, target_id=target_id, &
                                               response_name=response, from=from, to=to, limit=int(limit, kind=i8))
            else
                ! Select data points from database.
                rc = dm_db_select_data_points(db, dps, node_id=node_id, sensor_id=sensor_id, target_id=target_id, &
                                              response_name=response, from=from, to=to, limit=int(limit, kind=i8))
            end if

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            ! Return observation views in CSV format.
            call dm_fcgi_header(MIME_CSV, HTTP_OK)

            if (view) then
                if (header) call dm_fcgi_write(dm_csv_header_observ_view())

                do i = 1, size(views)
                    call dm_fcgi_write(dm_csv_from(views(i)) // NL)
                end do
            else
                if (header) call dm_fcgi_write(dm_csv_header_data_point())

                do i = 1, size(dps)
                    call dm_fcgi_write(dm_csv_from(dps(i)) // NL)
                end do
            end if
        end block response_block

        call dm_db_close(db)
    end subroutine route_timeseries

    ! **************************************************************************
    ! UTILITY ROUTINES.
    ! **************************************************************************
    integer function format_from_mime(mime) result(format)
        !! Returns format type from MIME (CSV, JSON, JSONL).
        character(len=*), intent(in) :: mime !! MIME type string.

        select case (mime)
            case (MIME_CSV);   format = FORMAT_CSV
            case (MIME_JSON);  format = FORMAT_JSON
            case (MIME_JSONL); format = FORMAT_JSONL
            case (MIME_NML);   format = FORMAT_NML
            case default;      format = FORMAT_CSV
        end select
    end function format_from_mime

    subroutine api_error(status, message, error)
        !! Outputs error response in stub `api_status_type` format as `text/plain`.
        integer,          intent(in), optional :: status  !! HTTP status code.
        character(len=*), intent(in), optional :: message !! Error message.
        integer,          intent(in), optional :: error   !! DMPACK error code.

        if (present(status)) then
            call dm_fcgi_header(MIME_TEXT, status)
        else
            call dm_fcgi_header(MIME_TEXT, HTTP_OK)
        end if

        if (present(message)) call dm_fcgi_write('message=' // trim(message))
        if (present(error))   call dm_fcgi_write('error='   // dm_itoa(error))
    end subroutine api_error

    subroutine content_type(env, mime, default, format)
        !! Returns the content type first found in CGI environment variable
        !! `HTTP_ACCEPT`, either CSV, JSON Lines, JSON, or NML (in this order).
        !! If none of them is found, the passed default is returned.
        type(cgi_env_type),      intent(inout)         :: env     !! CGI environment type.
        character(len=MIME_LEN), intent(out)           :: mime    !! Content type (MIME).
        character(len=*),        intent(in)            :: default !! Default content type (MIME).
        integer,                 intent(out), optional :: format  !! Format enumerator (`FORMAT_*`).

        if (index(env%http_accept, MIME_CSV) > 0) then
            mime = MIME_CSV
            if (present(format)) format = FORMAT_CSV
            return
        end if

        ! Look for JSONL before JSON, or JSONL will never be returned.
        if (index(env%http_accept, MIME_JSONL) > 0) then
            mime = MIME_JSONL
            if (present(format)) format = FORMAT_JSONL
            return
        end if

        if (index(env%http_accept, MIME_JSON) > 0) then
            mime = MIME_JSON
            if (present(format)) format = FORMAT_JSON
            return
        end if

        if (index(env%http_accept, MIME_NML) > 0) then
            mime = MIME_NML
            if (present(format)) format = FORMAT_NML
            return
        end if

        mime = default
        if (present(format)) format = format_from_mime(default)
    end subroutine content_type

    subroutine csv_iter(type, index, header, csv)
        integer,          intent(in) :: type   !! Data type.
        integer(kind=i8), intent(in) :: index  !! Current array index.
        logical,          intent(in) :: header !! Output CSV header.
        character(len=*), intent(in) :: csv    !! CSV string

        if (index == 1 .and. header) then
            select case (type)
                case (TYPE_BEAT);   call dm_fcgi_write(dm_csv_header_beat())
                case (TYPE_LOG);    call dm_fcgi_write(dm_csv_header_log())
                case (TYPE_OBSERV); call dm_fcgi_write(dm_csv_header_observ())
                case (TYPE_SENSOR); call dm_fcgi_write(dm_csv_header_sensor())
                case (TYPE_TARGET); call dm_fcgi_write(dm_csv_header_target())
            end select
        end if

        call dm_fcgi_write(csv // NL)
    end subroutine csv_iter

    subroutine json_iter(index, size, json)
        !! Outputs JSON array element.
        integer(kind=i8), intent(in) :: index !! Current array index.
        integer(kind=i8), intent(in) :: size  !! Array size.
        character(len=*), intent(in) :: json  !! JSON string

        if (index == 1) then
            call dm_fcgi_write('[' // json)
        else if (index < size) then
            call dm_fcgi_write(json // ',')
        else
            call dm_fcgi_write(json // ']')
        end if
    end subroutine json_iter

    subroutine jsonl_iter(json)
        !! Outputs JSONL array element.
        character(len=*), intent(in) :: json !! JSON string.

        call dm_fcgi_write(json // NL)
    end subroutine jsonl_iter
end program dmapi
