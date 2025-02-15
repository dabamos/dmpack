! dmapi.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmapi
    !! An HTTP-RPC service for DMPACK database access. A FastCGI-compatible web
    !! server, such as lighttpd, is required to run this web app.
    !!
    !! Observations and log messages sent via HTTP POST are expected to be in
    !! Fortran 95 Namelist format, with optional deflate compression
    !! (`Content-Encoding: deflate`). The server returns data in CSV format
    !! with optional header by default. The client has to set an HTTP Accept
    !! header to request JSON or JSON Lines format. Error and status messages
    !! are returned as plain-text (key-value pairs).
    !!
    !! Configure the web app through FastCGI environment variables:
    !!
    !! | Environment Variable | Description                                  |
    !! |----------------------|----------------------------------------------|
    !! | `DM_DB_BEAT`         | Path to beat database.                       |
    !! | `DM_DB_LOG`          | Path to log database.                        |
    !! | `DM_DB_OBSERV`       | Path to observation database.                |
    !! | `DM_READ_ONLY`       | Open databases in read-only mode (optional). |
    !!
    !! If HTTP Basic Auth is enabled, the sensor id of each beat, log, node,
    !! sensor, and observation sent to the RPC service must match the name of
    !! the authenticated user. For example, to store an observation of the node
    !! with id `node-1`, the HTTP Basic Auth user name must be `node-1`. If the
    !! observation is sent by any other user, it will be rejected (HTTP 401).
    use :: dmpack
    implicit none (type, external)

    ! Program version.
    integer, parameter :: APP_MAJOR = 0
    integer, parameter :: APP_MINOR = 9
    integer, parameter :: APP_PATCH = 3

    ! Program parameters.
    integer, parameter :: APP_DB_TIMEOUT   = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.
    integer, parameter :: APP_MAX_NLOGS    = 10000              !! Max. number of logs per request.
    integer, parameter :: APP_MAX_NOBSERVS = 10000              !! Max. number of observations per request.
    integer, parameter :: APP_NROUTES      = 15                 !! Total number of routes.
    logical, parameter :: APP_CSV_HEADER   = .false.            !! Add CSV header by default.
    logical, parameter :: APP_READ_ONLY    = .false.            !! Default database access mode.

    ! Global settings.
    character(len=FILE_PATH_LEN) :: db_beat   = ' '           ! Path to beat database.
    character(len=FILE_PATH_LEN) :: db_log    = ' '           ! Path to log database.
    character(len=FILE_PATH_LEN) :: db_observ = ' '           ! Path to observation database.
    logical                      :: read_only = APP_READ_ONLY ! Read-only flag for databases.

    integer               :: code
    integer               :: n, rc
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
    rc = dm_env_get('DM_DB_BEAT',   db_beat,   n)
    rc = dm_env_get('DM_DB_LOG',    db_log,    n)
    rc = dm_env_get('DM_DB_OBSERV', db_observ, n)
    rc = dm_env_get('DM_READ_ONLY', read_only, APP_READ_ONLY)

    ! Set API routes.
    rc = dm_cgi_router_set(router, routes)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Run event loop.
    do while (dm_fcgi_accept())
        call dm_cgi_env(env)
        call dm_cgi_router_dispatch(router, env, code)
        if (code == HTTP_OK) cycle
        call api_error(code, dm_error_message(E_NOT_FOUND), E_NOT_FOUND)
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
        !! * `node_id` - Node id.
        !!
        !! ## GET Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## POST Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Heartbeat is returned.
        !! * `400` - Invalid request.
        !! * `404` - Heartbeat was not found.
        !! * `503` - Database error.
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

        rc = dm_db_open(db, db_beat, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=NML_NODE_LEN)   :: buffer
            character(len=NODE_ID_LEN)    :: node_id
            integer                       :: encoding
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
                rc = dm_fcgi_content(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                encoding = dm_z_type_from_encoding(env%http_content_encoding)

                if (encoding == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, encoding, beat)

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
                if (dm_cgi_auth_basic(env)) then
                    if (env%remote_user /= beat%node_id) then
                        call api_error(HTTP_UNAUTHORIZED, 'node id does not match user name', E_RPC_AUTH)
                        exit response_block
                    end if
                end if

                ! Set time stamp and remote IP address.
                beat%time_recv = dm_time_now()
                beat%address   = env%remote_addr

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
                call api_error(code, 'invalid parameter node_id', E_INVALID)
                exit response_block
            end if

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

            ! Select MIME type from HTTP Accept header.
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV)
                    call dm_fcgi_out(dm_csv_from(beat))
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON)
                    call dm_fcgi_out(dm_json_from(beat))
                case (MIME_NML)
                    ! Return Namelist.
                    rc = dm_nml_from(beat, buffer)
                    call dm_fcgi_header(MIME_NML)
                    call dm_fcgi_out(trim(buffer))
            end select
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_beat

    subroutine route_beats(env)
        !! Returns list of all beats in database in CSV, JSON, or JSON Lines
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
        !! ## GET Headers
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

        rc = dm_db_open(db, db_beat, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            integer                      :: code
            logical                      :: header
            type(cgi_param_type)         :: param
            type(beat_type), allocatable :: beats(:)

            ! Optional GET parameters.
            call dm_cgi_query(env, param)
            rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)

            ! Select all beats from database.
            rc = dm_db_select_beats(db, beats)

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                call api_error(HTTP_SERVICE_UNAVAILABLE, 'database query failed', rc)
                exit response_block
            end if

            code = HTTP_OK
            if (size(beats) == 0) code = HTTP_NOT_FOUND

            ! Select MIME type from HTTP Accept header.
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV, code)
                    call dm_fcgi_out(dm_csv_from(beats, header=header))
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON, code)
                    call dm_fcgi_out(dm_json_from(beats))
                case (MIME_JSONL)
                    ! Return JSON Lines.
                    call dm_fcgi_header(MIME_JSONL, code)
                    call dm_fcgi_out(dm_jsonl_from(beats))
            end select
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_beats

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
        !! ## GET Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## POST Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Log is returned.
        !! * `400` - Invalid request.
        !! * `404` - Log was not found.
        !! * `503` - Database error.
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

        rc = dm_db_open(db, db_log, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=NML_NODE_LEN)   :: buffer
            character(len=LOG_ID_LEN)     :: id
            integer                       :: encoding
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
                rc = dm_fcgi_content(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                encoding = dm_z_type_from_encoding(env%http_content_encoding)

                if (encoding == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, encoding, log)

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
                call api_error(code, 'invalid parameter id', E_INVALID)
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

            ! Select MIME type from HTTP Accept header.
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV)
                    call dm_fcgi_out(dm_csv_from(log))
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON)
                    call dm_fcgi_out(dm_json_from(log))
                case (MIME_NML)
                    ! Return Namelist.
                    rc = dm_nml_from(log, buffer)
                    call dm_fcgi_header(MIME_NML)
                    call dm_fcgi_out(trim(buffer))
            end select
        end block response_block

        rc = dm_db_close(db)
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
        !! ## GET Headers
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

        rc = dm_db_open(db, db_log, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=NODE_ID_LEN)  :: node_id
            character(len=TIME_LEN)     :: from, to
            integer                     :: code, i, limit_
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

            ! Select MIME type from HTTP Accept header.
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV, code)
                    if (header) call dm_fcgi_out(dm_csv_header_log())

                    do i = 1, size(logs)
                        call dm_fcgi_out(dm_csv_from(logs(i)))
                    end do
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON, code)

                    if (size(logs) == 0) then
                        call dm_fcgi_out('[]')
                        exit response_block
                    end if

                    do i = 1, size(logs)
                        if (i == 1) then
                            call dm_fcgi_out('[ ' // dm_json_from(logs(i)))
                        else if (i < size(logs)) then
                            call dm_fcgi_out(dm_json_from(logs(i)) // ',')
                        else
                            call dm_fcgi_out(dm_json_from(logs(i)) // ' ]')
                        end if
                    end do
                case (MIME_JSONL)
                    ! Return JSON Lines.
                    call dm_fcgi_header(MIME_JSONL, code)
                    if (size(logs) == 0)  exit response_block

                    do i = 1, size(logs)
                        call dm_fcgi_out(dm_json_from(logs(i)))
                    end do
            end select
        end block response_block

        rc = dm_db_close(db)
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
        !! ## GET Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## POST Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Node is returned.
        !! * `400` - Invalid request.
        !! * `404` - Node was not found.
        !! * `503` - Database error.
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

        rc = dm_db_open(db, db_observ, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=NML_NODE_LEN)   :: buffer
            character(len=NODE_ID_LEN)    :: id
            integer                       :: encoding
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
                rc = dm_fcgi_content(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                encoding = dm_z_type_from_encoding(env%http_content_encoding)

                if (encoding == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, encoding, node)

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
                call api_error(code, 'invalid parameter id', E_INVALID)
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
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV)
                    call dm_fcgi_out(dm_csv_from(node))
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON)
                    call dm_fcgi_out(dm_json_from(node))
                case (MIME_NML)
                    ! Return Namelist.
                    rc = dm_nml_from(node, buffer)
                    call dm_fcgi_header(MIME_NML)
                    call dm_fcgi_out(trim(buffer))
            end select
        end block response_block

        rc = dm_db_close(db)
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
        !! ## GET Headers
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

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            integer                      :: code
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
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Optional GET parameters.
                    call dm_cgi_query(env, param)
                    rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV, code)
                    call dm_fcgi_out(dm_csv_from(nodes, header=header))
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON, code)
                    call dm_fcgi_out(dm_json_from(nodes))
                case (MIME_JSONL)
                    ! Return JSON Lines.
                    call dm_fcgi_header(MIME_JSONL, code)
                    call dm_fcgi_out(dm_jsonl_from(nodes))
            end select
        end block response_block

        rc = dm_db_close(db)
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
        !! ## GET Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## POST Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Observation is returned.
        !! * `400` - Invalid request.
        !! * `404` - Observation was not found.
        !! * `503` - Database error.
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

        rc = dm_db_open(db, db_observ, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=NML_OBSERV_LEN) :: buffer
            character(len=OBSERV_ID_LEN)  :: id
            integer                       :: encoding
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
                rc = dm_fcgi_content(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                encoding = dm_z_type_from_encoding(env%http_content_encoding)

                if (encoding == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, encoding, observ)

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
                call api_error(code, 'invalid parameter id', E_INVALID)
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
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV)
                    call dm_fcgi_out(dm_csv_from(observ))
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON)
                    call dm_fcgi_out(dm_json_from(observ))
                case (MIME_NML)
                    ! Return Namelist.
                    rc = dm_nml_from(observ, buffer)
                    call dm_fcgi_header(MIME_NML)
                    call dm_fcgi_out(trim(buffer))
            end select
        end block response_block

        rc = dm_db_close(db)
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
        !! ## GET Headers
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

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=NODE_ID_LEN)     :: node_id
            character(len=SENSOR_ID_LEN)   :: sensor_id
            character(len=TARGET_ID_LEN)   :: target_id
            character(len=TIME_LEN)        :: from, to
            integer                        :: i, code, limit, limit_
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
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV, code)

                    ! Add optional CSV header.
                    rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)
                    if (header) call dm_fcgi_out(dm_csv_header_observ())

                    do i = 1, size(observs)
                        call dm_fcgi_out(dm_csv_from(observs(i)))
                    end do
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON, code)

                    if (size(observs) == 0) then
                        call dm_fcgi_out('[]')
                        exit response_block
                    end if

                    do i = 1, size(observs)
                        if (i == 1) then
                            call dm_fcgi_out('[ ' // dm_json_from(observs(i)) // ',')
                        else if (i < size(observs)) then
                            call dm_fcgi_out(dm_json_from(observs(i)) // ',')
                        else
                            call dm_fcgi_out(dm_json_from(observs(i)) // ' ]')
                        end if
                    end do
                case (MIME_JSONL)
                    ! Return JSON Lines.
                    call dm_fcgi_header(MIME_JSONL, code)
                    if (size(observs) == 0) exit response_block

                    do i = 1, size(observs)
                        call dm_fcgi_out(dm_json_from(observs(i)))
                    end do
            end select
        end block response_block

        rc = dm_db_close(db)
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

        character(len=API_STATUS_LEN) :: message
        integer                       :: rc
        type(api_status_type)         :: api

        rc = E_NONE

        ! Check database availability.
        if (.not. dm_file_exists(db_beat)) then
            rc = E_NOT_FOUND
            message = 'beat database not found'
        else if (.not. dm_file_exists(db_log)) then
            rc = E_NOT_FOUND
            message = 'log database not found'
        else if (.not. dm_file_exists(db_observ)) then
            rc = E_NOT_FOUND
            message = 'observation database not found'
        else
            message = 'online'
        end if

        api = api_status_type(version   = dm_version_to_string(APP_MAJOR, APP_MINOR, APP_PATCH), &
                              dmpack    = DM_VERSION_STRING, &
                              host      = env%server_name, &
                              server    = env%server_software, &
                              timestamp = dm_time_now(), &
                              message   = message, &
                              error     = rc)

        call dm_fcgi_header(MIME_TEXT, HTTP_OK)
        call dm_fcgi_out(dm_api_status_to_string(api))
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
        !! ## GET Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## POST Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Sensor is returned.
        !! * `400` - Invalid request.
        !! * `404` - Sensor was not found.
        !! * `503` - Database error.
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

        rc = dm_db_open(db, db_observ, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=NML_SENSOR_LEN) :: buffer
            character(len=SENSOR_ID_LEN)  :: id
            integer                       :: encoding
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
                rc = dm_fcgi_content(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                encoding = dm_z_type_from_encoding(env%http_content_encoding)

                if (encoding == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, encoding, sensor)

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
                call api_error(code, 'invalid parameter id', E_INVALID)
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
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV)
                    call dm_fcgi_out(dm_csv_from(sensor))
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON)
                    call dm_fcgi_out(dm_json_from(sensor))
                case (MIME_NML)
                    ! Return Namelist.
                    rc = dm_nml_from(sensor, buffer)
                    call dm_fcgi_header(MIME_NML)
                    call dm_fcgi_out(trim(buffer))
            end select
        end block response_block

        rc = dm_db_close(db)
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
        !! ## GET Headers
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

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            integer                        :: code
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
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Optional GET parameters.
                    call dm_cgi_query(env, param)
                    rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV, code)
                    call dm_fcgi_out(dm_csv_from(sensors, header=header))
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON, code)
                    call dm_fcgi_out(dm_json_from(sensors))
                case (MIME_JSONL)
                    ! Return JSON Lines.
                    call dm_fcgi_header(MIME_JSONL, code)
                    call dm_fcgi_out(dm_jsonl_from(sensors))
            end select
        end block response_block

        rc = dm_db_close(db)
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
        !! ## GET Headers
        !!
        !! * `Accept` - `application/json`, `application/namelist`, `text/comma-separated-values`
        !!
        !! ## POST Headers
        !!
        !! * `Content-Encoding` - `deflate`, `zstd` (optional)
        !! * `Content-Type`     - `application/namelist`
        !!
        !! ## GET Responses
        !!
        !! * `200` - Target is returned.
        !! * `400` - Invalid request.
        !! * `404` - Target was not found.
        !! * `503` - Database error.
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

        rc = dm_db_open(db, db_observ, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            character(len=:), allocatable :: content
            character(len=NML_TARGET_LEN) :: buffer
            character(len=TARGET_ID_LEN)  :: id
            integer                       :: encoding
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
                rc = dm_fcgi_content(env, content)

                if (dm_is_error(rc)) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid payload', rc)
                    exit response_block
                end if

                ! Select payload compression type.
                encoding = dm_z_type_from_encoding(env%http_content_encoding)

                if (encoding == Z_TYPE_INVALID) then
                    call api_error(HTTP_BAD_REQUEST, 'invalid content encoding', E_INVALID)
                    exit response_block
                end if

                ! Uncompress payload.
                rc = dm_z_uncompress(content, encoding, target)

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
                call api_error(code, 'invalid parameter id', E_INVALID)
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
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV)
                    call dm_fcgi_out(dm_csv_from(target))
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON)
                    call dm_fcgi_out(dm_json_from(target))
                case (MIME_NML)
                    ! Return Namelist.
                    rc = dm_nml_from(target, buffer)
                    call dm_fcgi_header(MIME_NML)
                    call dm_fcgi_out(trim(buffer))
            end select
        end block response_block

        rc = dm_db_close(db)
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
        !! ## GET Headers
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

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call api_error(HTTP_SERVICE_UNAVAILABLE, 'database connection failed', rc)
            return
        end if

        response_block: block
            integer                        :: code
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
            select case (content_type(env, MIME_CSV))
                case (MIME_CSV)
                    ! Optional GET parameters.
                    call dm_cgi_query(env, param)
                    rc = dm_cgi_get(param, 'header', header, APP_CSV_HEADER)
                    ! Return CSV.
                    call dm_fcgi_header(MIME_CSV, code)
                    call dm_fcgi_out(dm_csv_from(targets, header=header))
                case (MIME_JSON)
                    ! Return JSON.
                    call dm_fcgi_header(MIME_JSON, code)
                    call dm_fcgi_out(dm_json_from(targets))
                case (MIME_JSONL)
                    ! Return JSON Lines.
                    call dm_fcgi_header(MIME_JSONL, code)
                    call dm_fcgi_out(dm_jsonl_from(targets))
            end select
        end block response_block

        rc = dm_db_close(db)
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
        !! ## GET Headers
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

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

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
            call dm_fcgi_header(MIME_CSV)

            if (view) then
                if (header) call dm_fcgi_out(dm_csv_header_observ_view())

                do i = 1, size(views)
                    call dm_fcgi_out(dm_csv_from(views(i)))
                end do
            else
                if (header) call dm_fcgi_out(dm_csv_header_data_point())

                do i = 1, size(dps)
                    call dm_fcgi_out(dm_csv_from(dps(i)))
                end do
            end if
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_timeseries

    ! **************************************************************************
    ! UTILITY ROUTINES.
    ! **************************************************************************
    function content_type(env, default)
        !! Returns the content type first found in CGI environment variable
        !! `HTTP_ACCEPT`, either CSV, JSON Lines, JSON, or NML (in this order).
        !! If none of them is found, the passed default is returned.
        type(cgi_env_type), intent(inout) :: env          !! CGI environment type.
        character(len=*),   intent(in)    :: default      !! Default content type.
        character(len=:), allocatable     :: content_type !! String of content type.

        if (index(env%http_accept, MIME_CSV) > 0) then
            content_type = MIME_CSV
            return
        end if

        ! Look for JSONL before JSON, or JSONL will never be returned.
        if (index(env%http_accept, MIME_JSONL) > 0) then
            content_type = MIME_JSONL
            return
        end if

        if (index(env%http_accept, MIME_JSON) > 0) then
            content_type = MIME_JSON
            return
        end if

        if (index(env%http_accept, MIME_NML) > 0) then
            content_type = MIME_NML
            return
        end if

        content_type = default
    end function content_type

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

        if (present(message)) call dm_fcgi_out('message=' // trim(message))
        if (present(error))   call dm_fcgi_out('error='   // dm_itoa(error))
    end subroutine api_error
end program dmapi
