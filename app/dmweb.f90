! dmweb.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmweb
    !! This program is a server-side web application for DMPACK database access
    !! that has to be executed by a CGI-compatible web server, such as
    !! _lighttpd(1)_. If served locally, access the web interface at
    !! `http://127.0.0.1/dmpack/`.
    !!
    !! Make sure that the path `/dmpack` is redirected to the CGI program. For
    !! example, in the _lighttpd(1)_ configuration file `lighttpd.conf`, load
    !! the required modules first and then add an alias:
    !!
    !! ```lighttpd
    !! # Load additional modules.
    !! server.modules += ( "mod_alias", "mod_cgi", "mod_setenv" )
    !!
    !! $HTTP["url"] =^ "/dmpack/" {
    !!   # Map URL to CGI executable.
    !!   alias.url += ( "/dmpack" => "/usr/local/bin/dmweb"
    !!
    !!   # Enable CGI.
    !!   cgi.assign = ( "" => "" )
    !! }
    !! ```
    !!
    !! In this particular case, the web interface is installed to
    !! `/usr/local/bin/`. Configure the application through CGI environment
    !! variables:
    !!
    !! | Environment Variable | Description                                  |
    !! |----------------------|----------------------------------------------|
    !! | `DM_DB_BEAT`         | Path to beat database.                       |
    !! | `DM_DB_LOG`          | Path to log database.                        |
    !! | `DM_DB_OBSERV`       | Path to observation database.                |
    !! | `DM_READ_ONLY`       | Open databases in read-only mode (optional). |
    !!
    !! The databases have to exist at start-up. Add the variables to the
    !! configuration file of your web server. In _lighttpd(1)_, for instance:
    !!
    !! ```lighttpd
    !!  # Pass the database paths through environment variables.
    !!  setenv.add-environment = (
    !!    "DM_DB_BEAT"   => "/var/dmpack/beat.sqlite",
    !!    "DM_DB_LOG"    => "/var/dmpack/log.sqlite",
    !!    "DM_DB_OBSERV" => "/var/dmpack/observ.sqlite",
    !!    "DM_TILE_URL"  => "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    !!    "DM_READ_ONLY" => "0"
    !!  )
    !! ```
    !!
    !! The module `mod_sentenv` must be loaded (see above).
    !!
    !! Copy the CSS file `share/dmpack.min.css` to the document root path of the
    !! web server (for example, `/var/www/`), or create a symlink. Other
    !! classless style sheets may work as well.
    use :: dmpack
    implicit none (type, external)

    ! Program version number and patch level.
    integer, parameter :: APP_MAJOR = 0
    integer, parameter :: APP_MINOR = 9
    integer, parameter :: APP_PATCH = 5

    ! Program parameters.
    character(len=*), parameter :: APP_BASE_PATH  = '/dmpack'          !! URI base path.
    character(len=*), parameter :: APP_CSS_PATH   = '/dmweb'           !! Path to CSS directory.
    character(len=*), parameter :: APP_JS_PATH    = APP_CSS_PATH       !! Path to JavaScript directory.
    character(len=*), parameter :: APP_TITLE      = 'DMPACK'           !! HTML title and heading.
    integer,          parameter :: APP_DB_TIMEOUT = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.
    integer,          parameter :: APP_PLOT_TERM  = PLOT_TERM_SVG      !! Plotting backend.
    logical,          parameter :: APP_READ_ONLY  = .false.            !! Default database access mode.
    real(kind=r8),    parameter :: APP_MAP_LON    = 10.4541194_r8      !! Default map view longitude.
    real(kind=r8),    parameter :: APP_MAP_LAT    = 51.1642292_r8      !! Default map view latitude.

    ! Global settings.
    character(len=FILE_PATH_LEN) :: db_beat   = ' ' ! Path to beat database.
    character(len=FILE_PATH_LEN) :: db_log    = ' ' ! Path to log database.
    character(len=FILE_PATH_LEN) :: db_observ = ' ' ! Path to observation database.
    character(len=FILE_PATH_LEN) :: tile_url  = ' ' ! URL of map tile server.

    logical :: has_db_beat   = .false.              ! Beat database passed.
    logical :: has_db_log    = .false.              ! Log database passed.
    logical :: has_db_observ = .false.              ! Observation database passed.
    logical :: has_tile_url  = .false.              ! Map tile URL passed.
    logical :: read_only     = APP_READ_ONLY        ! Open databases in read-only mode.

    type(cgi_route_type)  :: routes(19)
    type(cgi_router_type) :: router

    ! Initialise DMPACK.
    call dm_init()

    ! Routes to dynamic pages.
    routes = [ &
        cgi_route_type('',         route_dashboard), &
        cgi_route_type('/',        route_dashboard), &
        cgi_route_type('/beat',    route_beat),      &
        cgi_route_type('/beats',   route_beats),     &
        cgi_route_type('/env',     route_env),       &
        cgi_route_type('/licence', route_licence),   &
        cgi_route_type('/log',     route_log),       &
        cgi_route_type('/logs',    route_logs),      &
        cgi_route_type('/map',     route_map),       &
        cgi_route_type('/node',    route_node),      &
        cgi_route_type('/nodes',   route_nodes),     &
        cgi_route_type('/observ',  route_observ),    &
        cgi_route_type('/observs', route_observs),   &
        cgi_route_type('/plots',   route_plots),     &
        cgi_route_type('/sensor',  route_sensor),    &
        cgi_route_type('/sensors', route_sensors),   &
        cgi_route_type('/status',  route_status),    &
        cgi_route_type('/target',  route_target),    &
        cgi_route_type('/targets', route_targets)    &
    ]

    ! Dispatch request and output response.
    route_block: block
        integer            :: code, n, rc
        type(cgi_env_type) :: env

        ! Read environment variables.
        rc = dm_env_get('DM_DB_BEAT',   db_beat,   n, exists=has_db_beat)
        rc = dm_env_get('DM_DB_LOG',    db_log,    n, exists=has_db_log)
        rc = dm_env_get('DM_DB_OBSERV', db_observ, n, exists=has_db_observ)
        rc = dm_env_get('DM_TILE_URL',  tile_url,  n, exists=has_tile_url)
        rc = dm_env_get('DM_READ_ONLY', read_only, APP_READ_ONLY)

        ! Set-up router.
        rc = dm_cgi_router_set(router, routes)
        if (dm_is_error(rc)) exit route_block

        ! Get CGI environment variables, dispatch request, and
        ! return the response.
        call dm_cgi_env(env)
        call dm_cgi_router_dispatch(router, env, code)
        if (code /= HTTP_OK) call html_error(status=code)
    end block route_block

    call dm_cgi_router_destroy(router)
contains
    ! ******************************************************************
    ! ENDPOINTS.
    ! ******************************************************************
    subroutine route_beat(env)
        !! Beat page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/beat`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `node_id` – Node id (string).
        !!
        character(len=*), parameter :: TITLE = 'Beat' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, db_beat, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        response_block: block
            character(len=NODE_ID_LEN) :: node_id
            integer(kind=i8)           :: delta
            type(cgi_param_type)       :: param
            type(beat_type)            :: beat

            call dm_cgi_query(env, param)
            rc = dm_cgi_get(param, 'node_id', node_id)

            if (dm_is_error(rc)) then
                call html_error('Missing Parameter', error=rc)
                exit response_block
            end if

            rc = dm_db_select(db, beat, node_id)

            if (dm_is_error(rc)) then
                call html_error('Beat Not Found', error=rc)
                exit response_block
            end if

            delta = huge(0_i8)
            rc = dm_time_diff(beat%time_recv, dm_time_now(), delta)

            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))
            call dm_cgi_out(dm_html_beat(beat, delta, prefix=APP_BASE_PATH // '/node?id='))
            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_beat

    subroutine route_beats(env)
        !! Beats page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/beats`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        character(len=*), parameter :: TITLE = 'Beats' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, db_beat, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        response_block: block
            character(len=TIME_LEN)       :: now
            integer(kind=i8)              :: i, n
            type(beat_type),  allocatable :: beats(:)
            integer(kind=i8), allocatable :: deltas(:)

            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))

            rc = dm_db_select_beats(db, beats, nbeats=n)

            if (n > 0) then
                allocate (deltas(n), source=huge(0_i8))
                now = dm_time_now()

                do i = 1, n
                    rc = dm_time_diff(beats(i)%time_recv, now, deltas(i))
                end do

                call dm_cgi_out(dm_html_beats(beats, deltas=deltas, prefix=APP_BASE_PATH // '/beat?node_id='))
            else
                call dm_cgi_out(dm_html_p('No beats found.'))
            end if

            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_beats

    subroutine route_dashboard(env)
        !! Dashboard page, shows last observations, logs, and heartbeats.
        !!
        !! ## Path
        !!
        !! * `/dmpack/`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        character(len=*), parameter :: TITLE = 'Dashboard' !! Page title.

        integer(kind=i8), parameter :: NBEATS   = 10 !! Max. number of beats to show.
        integer(kind=i8), parameter :: NLOGS    = 10 !! Max. number of logs to show.
        integer(kind=i8), parameter :: NOBSERVS = 10 !! Max. number of observations to show.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        character(len=TIME_LEN) :: now
        integer                 :: rc
        integer(kind=i8)        :: i, n
        type(db_type)           :: db

        integer(kind=i8),  allocatable :: deltas(:)
        type(beat_type),   allocatable :: beats(:)
        type(log_type),    allocatable :: logs(:)
        type(observ_type), allocatable :: observs(:)

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        call html_header(TITLE)
        call dm_cgi_out(dm_html_heading(1, TITLE))
        call dm_cgi_out(dm_html_p('The dashboard lists heartbeats, logs, and observations ' // &
                                  'most recently added to the databases.'))

        if (.not. has_db_beat .and. .not. has_db_log .and. .not. has_db_observ) then
            call dm_cgi_out(dm_html_p('No databases configured.'))
            call html_footer()
            return
        end if

        ! ------------------------------------------------------------------
        ! Heatbeats.
        ! ------------------------------------------------------------------
        beat_if: &
        if (has_db_beat) then
            call dm_cgi_out(dm_html_heading(2, 'Beats', small='Last ' // dm_itoa(NBEATS) // ' Beats'))
            rc = dm_db_open(db, db_beat, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call dm_cgi_out(dm_html_p('Database connection failed.'))
                exit beat_if
            end if

            rc = dm_db_select_beats(db, beats, limit=NBEATS, nbeats=n)

            if (dm_is_error(rc)) then
                call dm_cgi_out(dm_html_p('No beats found.'))
                exit beat_if
            end if

            allocate (deltas(n), source=huge(0_i8))
            now = dm_time_now()

            do i = 1, n
                rc = dm_time_diff(beats(i)%time_recv, now, deltas(i))
            end do

            call dm_cgi_out(dm_html_beats(beats, deltas=deltas, prefix=APP_BASE_PATH // '/beat?node_id='))
        end if beat_if

        if (has_db_beat) rc = dm_db_close(db)

        ! ------------------------------------------------------------------
        ! Logs.
        ! ------------------------------------------------------------------
        log_if: &
        if (has_db_log) then
            call dm_cgi_out(dm_html_heading(2, 'Logs', small='Last ' // dm_itoa(NLOGS) // ' Logs'))
            rc = dm_db_open(db, db_log, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call dm_cgi_out(dm_html_p('Database connection failed.'))
                exit log_if
            end if

            rc = dm_db_select_logs(db, logs, limit=NLOGS, desc=.true.)

            if (dm_is_error(rc)) then
                call dm_cgi_out(dm_html_p('No logs found.'))
                exit log_if
            end if

            call dm_cgi_out(dm_html_logs(logs, prefix=APP_BASE_PATH // '/log?id=', max_len=32))
        end if log_if

        if (has_db_log) rc = dm_db_close(db)

        ! ------------------------------------------------------------------
        ! Observations.
        ! ------------------------------------------------------------------
        observ_if: &
        if (has_db_observ) then
            call dm_cgi_out(dm_html_heading(2, 'Observations', small='Last ' // dm_itoa(NOBSERVS) // ' Observations'))
            rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call dm_cgi_out(dm_html_p('Database connection failed.'))
                exit observ_if
            end if

            rc = dm_db_select_observs(db, observs, desc=.true., limit=NOBSERVS, stub=.true.)

            if (dm_is_error(rc)) then
                call dm_cgi_out(dm_html_p('No observations found.'))
                exit observ_if
            end if

            call dm_cgi_out(dm_html_observs(observs, prefix=APP_BASE_PATH // '/observ?id=', &
                                            node_id=.true., sensor_id=.true., target_id=.true., &
                                            name=.true., source=.true., error=.true.))
        end if observ_if

        if (has_db_observ) rc = dm_db_close(db)

        call html_footer()
    end subroutine route_dashboard

    subroutine route_env(env)
        !! CGI environment variables page. This page is intentionally hidden
        !! (not linked in the navigation), and only implemented for testing.
        !!
        !! ## Path
        !!
        !! * `/dmpack/env`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        character(len=*), parameter :: TITLE = 'CGI Environment Variables' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        call html_header(TITLE)
        call dm_cgi_out(dm_html_heading(1, TITLE))
        call dm_cgi_out(dm_html_cgi_env(env))
        call html_footer()
    end subroutine route_env

    subroutine route_licence(env)
        !! Licence page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/licence`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        character(len=*), parameter :: TITLE = 'Licence' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        call html_header(TITLE)
        call dm_cgi_out(dm_html_heading(1, TITLE))
        call dm_cgi_out(H_BLOCKQUOTE)
        call dm_cgi_out(dm_html_p(DM_COPYRIGHT, encode=.true.))
        call dm_cgi_out(H_P // 'Permission to use, copy, modify, and/or distribute this ' // &
                        'software for any purpose with or without fee is hereby '         // &
                        'granted, provided that the above copyright notice and this '     // &
                        'permission notice appear in all copies.' // H_P_END)
        call dm_cgi_out(H_P // 'THE SOFTWARE IS PROVIDED &quot;AS IS&quot; AND THE AUTHOR ' // &
                        'DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING '  // &
                        'ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO '     // &
                        'EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, '        // &
                        'INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER '     // &
                        'RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN '       // &
                        'ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, '         // &
                        'ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE '      // &
                        'OF THIS SOFTWARE.' // H_P_END)
        call dm_cgi_out(H_BLOCKQUOTE_END)
        call html_footer()
    end subroutine route_licence

    subroutine route_log(env)
        !! Log page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/log`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `id` – Log id (UUID).
        !!
        character(len=*), parameter :: TITLE = 'Log' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        character(len=LOG_ID_LEN) :: id
        integer                   :: rc
        type(cgi_param_type)      :: param
        type(db_type)             :: db
        type(log_type)            :: log

        rc = dm_db_open(db, db_log, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        response_block: block
            call dm_cgi_query(env, param)
            rc = dm_cgi_get(param, 'id', id)

            if (dm_is_error(rc)) then
                call html_error('Missing Parameter', error=rc)
                exit response_block
            end if

            if (.not. dm_uuid4_valid(id)) then
                call html_error('Invalid Parameter', error=E_INVALID)
                exit response_block
            end if

            rc = dm_db_select(db, log, id)

            if (dm_is_error(rc)) then
                call html_error('Log Not Found', error=rc)
                exit response_block
            end if

            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))
            call dm_cgi_out(dm_html_log(log, prefix_node   = APP_BASE_PATH // '/node?id=', &
                                             prefix_sensor = APP_BASE_PATH // '/sensor?id=', &
                                             prefix_target = APP_BASE_PATH // '/target?id=', &
                                             prefix_observ = APP_BASE_PATH // '/observ?id='))
            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_log

    subroutine route_logs(env)
        !! Logs page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/logs`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        !! ## POST Parameters
        !!
        !! * `node_id`     – Node id (string).
        !! * `sensor_id`   – Sensor id (string).
        !! * `target_id`   – Target id (string).
        !! * `source`      – Log source (string).
        !! * `from`        – Time range start (ISO 8601).
        !! * `to`          – Time range end (ISO 8601).
        !! * `level`       – Log level (integer).
        !! * `max_results` – Maximum number of logs (integer).
        !!
        character(len=*), parameter :: TITLE = 'Logs' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer                     :: rc
        integer(kind=i8)            :: nlogs
        type(db_type)               :: db
        type(log_type), allocatable :: logs(:)

        response_block: block
            character(len=NODE_ID_LEN)    :: node_id
            character(len=SENSOR_ID_LEN)  :: sensor_id
            character(len=TARGET_ID_LEN)  :: target_id
            character(len=LOG_SOURCE_LEN) :: source
            character(len=TIME_LEN)       :: from, to
            integer                       :: level, max_results(5), nresults
            logical                       :: has_level, valid
            type(cgi_param_type)          :: param

            type(node_type),   allocatable  :: nodes(:)
            type(sensor_type), allocatable  :: sensors(:)
            type(target_type), allocatable  :: targets(:)

            max_results = [ 25, 50, 100, 250, 500 ]

            rc = dm_db_open(db, db_observ, read_only=read_only, timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call html_error('Database Connection Failed', error=rc)
                return
            end if

            rc = dm_db_select_nodes(db, nodes)
            rc = dm_db_select_sensors(db, sensors)
            rc = dm_db_select_targets(db, targets)

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                ! Validate content type.
                if (env%content_type /= MIME_FORM) then
                    call html_error(status=HTTP_BAD_REQUEST)
                    exit response_block
                end if

                ! Read form data from request body.
                call dm_cgi_form(env, param)

                ! Read and validate parameters.
                if (dm_is_error(dm_cgi_get(param, 'from', from)) .or. &
                    dm_is_error(dm_cgi_get(param, 'to', to)) .or. &
                    dm_is_error(dm_cgi_get(param, 'max_results', nresults))) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                valid = .true.

                ! Timestamps.
                if (.not. dm_time_valid(from)) valid = .false.
                if (.not. dm_time_valid(to))   valid = .false.

                ! Node id.
                if (dm_is_ok(dm_cgi_get(param, 'node_id', node_id))) then
                    if (.not. dm_id_valid(node_id)) valid = .false.
                end if

                ! Sensor id.
                if (dm_is_ok(dm_cgi_get(param, 'sensor_id', sensor_id))) then
                    if (.not. dm_id_valid(sensor_id)) valid = .false.
                end if

                ! Target id.
                if (dm_is_ok(dm_cgi_get(param, 'target_id', target_id))) then
                    if (.not. dm_id_valid(target_id)) valid = .false.
                end if

                ! Number of results.
                if (.not. dm_array_has(max_results, nresults)) valid = .false.

                if (.not. valid) then
                    call html_error('Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Log level.
                has_level = dm_is_ok(dm_cgi_get(param, 'level', level))

                ! Log source.
                rc = dm_cgi_get(param, 'source', source)

                ! Open log database.
                rc = dm_db_close(db)
                rc = dm_db_open(db, db_log, read_only=read_only, timeout=APP_DB_TIMEOUT)

                if (dm_is_error(rc)) then
                    call html_error('Database Connection Failed', error=rc)
                    return
                end if

                if (has_level) then
                    rc = dm_db_select_logs(db, logs, node_id, sensor_id, target_id, source, from, to, &
                                           min_level=level, max_level=level, limit=int(nresults, kind=i8), &
                                           nlogs=nlogs)
                else
                    rc = dm_db_select_logs(db, logs, node_id, sensor_id, target_id, source, from, to, &
                                           limit=int(nresults, kind=i8), nlogs=nlogs)
                end if

                if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                    call html_error('Database Query Failed', error=rc, extra=dm_db_error_message(db))
                    exit response_block
                end if

                call html_header(TITLE)
                call dm_cgi_out(dm_html_heading(1, TITLE))

                if (has_level) then
                    call dm_cgi_out(html_form_logs(nodes, sensors, targets, max_results, &
                                                   node_id, sensor_id, target_id, source, &
                                                   from, to, level, nresults=nresults))
                else
                    call dm_cgi_out(html_form_logs(nodes, sensors, targets, max_results, &
                                                   node_id, sensor_id, target_id, source, &
                                                   from, to, nresults=nresults))
                end if

                if (nlogs > 0) then
                    call dm_cgi_out(dm_html_logs(logs, prefix=APP_BASE_PATH // '/log?id=', max_len=32))
                else
                    call dm_cgi_out(dm_html_p('No logs found.'))
                end if

                call html_footer()
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))
            call dm_cgi_out(html_form_logs(nodes, sensors, targets, max_results))
            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_logs

    subroutine route_map(env)
        !! Shows map of sensor and target positions. The environment variable
        !! `TILE_URL` must be set.
        !!
        !! ## Path
        !!
        !! * `/dmpack/map`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        character(len=*), parameter :: JS_DMPACK  = APP_JS_PATH  // '/dmpack.js'       !! DMPACK JS.
        character(len=*), parameter :: JS_LEAFLET = APP_JS_PATH  // '/leaflet.js'      !! Leaflet JS.
        character(len=*), parameter :: STYLE      = APP_CSS_PATH // '/leaflet.min.css' !! Additional CSS file.
        character(len=*), parameter :: MAP_ID     = 'map'                              !! HTML element id of map.
        character(len=*), parameter :: TITLE      = 'Map'                              !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: i, rc
        integer       :: nn, ns, nt
        real(kind=r8) :: lon, lat
        type(db_type) :: db

        type(node_type),   allocatable :: nodes(:)
        type(sensor_type), allocatable :: sensors(:)
        type(target_type), allocatable :: targets(:)

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        if (len_trim(tile_url) == 0) then
            call html_error('No tile map URL provided in environment variable DM_TILE_URL.', error=E_EMPTY)
            return
        end if

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        rc = dm_db_select_nodes(db, nodes)
        rc = dm_db_select_sensors(db, sensors)
        rc = dm_db_select_targets(db, targets)
        rc = dm_db_close(db)

        ! Map view coordinates.
        lon = 0.0_r8
        lat = 0.0_r8

        ! Select view point.
        if (size(nodes) > 0) then
            lon = nodes(1)%lon
            lat = nodes(1)%lat
        else if (size(sensors) > 0) then
            lon = sensors(1)%lon
            lat = sensors(1)%lat
        else if (size(targets) > 0) then
            lon = targets(1)%lon
            lat = targets(1)%lat
        end if

        ! Use default coordinates for view point.
        if (dm_equals(lon, 0.0_r8) .and. dm_equals(lat, 0.0_r8)) then
            lon = APP_MAP_LON
            lat = APP_MAP_LAT
        end if

        ! Output page header.
        call html_header(TITLE, style=STYLE)
        call dm_cgi_out(dm_html_heading(1, TITLE))

        ! Output map element and scripts.
        call dm_cgi_out('<div id="' // MAP_ID // '"></div>')
        call dm_cgi_out(dm_html_script(JS_LEAFLET))
        call dm_cgi_out(dm_html_script(JS_DMPACK))

        ! Output inline script to create Leaflet map.
        call dm_cgi_out(H_SCRIPT)
        call dm_cgi_out('const id = '''  // MAP_ID          // ''';')
        call dm_cgi_out('const url = ''' // trim(tile_url)  // ''';')
        call dm_cgi_out('const lon = '   // dm_ftoa(lon)    // ';')
        call dm_cgi_out('const lat = '   // dm_ftoa(lat)    // ';')
        call dm_cgi_out('const zoom = 5;')
        call dm_cgi_out('const features = [')

        nn = size(nodes)
        ns = size(sensors)
        nt = size(targets)

        do i = 1, nn
            if (i < nn .or. ns > 0 .or. nt > 0) then
                call dm_cgi_out(dm_geojson_from(nodes(i)) // ',')
            else
                call dm_cgi_out(dm_geojson_from(nodes(i)))
            end if
        end do

        do i = 1, ns
            if (i < nn .or. nt > 0) then
                call dm_cgi_out(dm_geojson_from(sensors(i)) // ',')
            else
                call dm_cgi_out(dm_geojson_from(sensors(i)))
            end if
        end do

        do i = 1, nt
            if (i < nt) then
                call dm_cgi_out(dm_geojson_from(targets(i)) // ',')
            else
                call dm_cgi_out(dm_geojson_from(targets(i)))
            end if
        end do

        call dm_cgi_out('];')
        call dm_cgi_out('createMap(id, url, lon, lat, zoom, features);')
        call dm_cgi_out(H_SCRIPT_END)

        ! Output page footer.
        call html_footer()
    end subroutine route_map

    subroutine route_node(env)
        !! Node page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/node?id=<id>`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `id` – Node id (string).
        !!
        character(len=*), parameter :: TITLE = 'Node' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        response_block: block
            character(len=NODE_ID_LEN) :: id
            type(cgi_param_type)       :: param
            type(node_type)            :: node

            call dm_cgi_query(env, param)
            rc = dm_cgi_get(param, 'id', id)

            if (dm_is_error(rc)) then
                call html_error('Missing Parameter', error=rc)
                exit response_block
            end if

            rc = dm_db_select(db, node, id)

            if (dm_is_error(rc)) then
                call html_error('Node Not Found', error=rc)
                exit response_block
            end if

            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))
            call dm_cgi_out(dm_html_node(node))
            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_node

    subroutine route_nodes(env)
        !! Nodes page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/nodes`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        !! ## POST Parameters
        !!
        !! * `id`   – Node id (string).
        !! * `name` – Node name (string).
        !! * `meta` – Node meta description (string).
        !!
        character(len=*), parameter :: TITLE = 'Nodes' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, db_observ, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        response_block: block
            type(cgi_param_type)         :: param
            type(node_type)              :: node
            type(node_type), allocatable :: nodes(:)

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                ! Validate content type.
                if (env%content_type /= MIME_FORM) then
                    call html_error(status=HTTP_BAD_REQUEST)
                    exit response_block
                end if

                ! Read form data from request body.
                call dm_cgi_form(env, param)

                ! Read and validate parameters.
                if (dm_is_error(dm_cgi_get(param, 'id', node%id)) .or. &
                    dm_is_error(dm_cgi_get(param, 'name', node%name))) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Optional parameters.
                rc = dm_cgi_get(param, 'meta', node%meta)
                rc = dm_cgi_get(param, 'x',    node%x)
                rc = dm_cgi_get(param, 'y',    node%y)
                rc = dm_cgi_get(param, 'z',    node%z)
                rc = dm_cgi_get(param, 'lon',  node%lon)
                rc = dm_cgi_get(param, 'lat',  node%lat)
                rc = dm_cgi_get(param, 'alt',  node%alt)

                ! Validate node data.
                if (.not. dm_node_valid(node)) then
                    call html_error('Invalid Node', error=E_INVALID)
                    exit response_block
                end if

                ! Add node to database.
                rc = dm_db_insert(db, node)

                if (dm_is_error(rc)) then
                    ! Catch file permission error.
                    if (rc /= E_READ_ONLY) rc = dm_db_error(db)
                    call html_error('Database Operation Failed', error=rc)
                    exit response_block
                end if
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))

            rc = dm_db_select_nodes(db, nodes)

            if (size(nodes) > 0) then
                call dm_cgi_out(dm_html_nodes(nodes, prefix=APP_BASE_PATH // '/node?id='))
            else
                call dm_cgi_out(dm_html_p('No nodes found.'))
            end if

            call dm_cgi_out(html_form_nodes(disabled=read_only))
            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_nodes

    subroutine route_observ(env)
        !! Observation page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/observ`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `id` – Observation id (UUID).
        !!
        character(len=*), parameter :: TITLE = 'Observation' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        character(len=OBSERV_ID_LEN) :: id
        integer                      :: rc
        type(cgi_param_type)         :: param
        type(db_type)                :: db

        call dm_cgi_query(env, param)
        rc = dm_cgi_get(param, 'id', id)

        if (dm_is_error(rc)) then
            call html_error('Missing or Invalid Parameter', error=rc)
            return
        end if

        if (.not. dm_uuid4_valid(id)) then
            call html_error('Invalid Parameter', error=E_INVALID)
            return
        end if

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        response_block: block
            integer(kind=i8)            :: nlogs
            type(log_type), allocatable :: logs(:)
            type(observ_type)           :: observ

            ! Get observation from database.
            rc = dm_db_select(db, observ, id)

            if (dm_is_error(rc)) then
                call html_error('Observation Not Found', error=rc)
                exit response_block
            end if

            ! Get associated logs from database.
            rc = dm_db_close(db)
            rc = dm_db_open(db, db_log, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call html_error('Database Connection Failed', error=rc)
                return
            end if

            rc = dm_db_select_logs_by_observ(db, logs, id, nlogs)

            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))
            call dm_cgi_out(dm_html_observ(observ, prefix_node   = APP_BASE_PATH // '/node?id=', &
                                                   prefix_sensor = APP_BASE_PATH // '/sensor?id=', &
                                                   prefix_target = APP_BASE_PATH // '/target?id='))
            call dm_cgi_out(dm_html_heading(2, 'Logs'))

            if (nlogs > 0) then
                call dm_cgi_out(dm_html_logs(logs, prefix=APP_BASE_PATH // '/log?id=', max_len=32))
            else
                call dm_cgi_out(dm_html_p('No associated logs found.'))
            end if

            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_observ

    subroutine route_observs(env)
        !! Observations page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/observs`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        !! ## POST Parameters
        !!
        !! * `node_id`     – Node id (string).
        !! * `sensor_id`   – Sensor id (string).
        !! * `target_id`   – Target id (string).
        !! * `from`        – Time range start (ISO 8601).
        !! * `to`          – Time range end (ISO 8601).
        !! * `max_results` – Maximum number of points per plot (integer).
        !!
        character(len=*), parameter :: TITLE = 'Observations' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, db_observ, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        response_block: block
            character(len=NODE_ID_LEN)   :: node_id
            character(len=SENSOR_ID_LEN) :: sensor_id
            character(len=TARGET_ID_LEN) :: target_id
            character(len=TIME_LEN)      :: from, to
            integer                      :: max_results(6), nresults
            integer(kind=i8)             :: nobservs
            type(cgi_param_type)         :: param

            type(node_type),   allocatable :: nodes(:)
            type(observ_type), allocatable :: observs(:)
            type(sensor_type), allocatable :: sensors(:)
            type(target_type), allocatable :: targets(:)

            max_results = [ 25, 50, 100, 250, 500, 1000 ]

            rc = dm_db_select_nodes(db, nodes)
            rc = dm_db_select_sensors(db, sensors)
            rc = dm_db_select_targets(db, targets)

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                ! Validate content type.
                if (env%content_type /= MIME_FORM) then
                    call html_error(status=HTTP_BAD_REQUEST)
                    exit response_block
                end if

                ! Read form data from request body.
                call dm_cgi_form(env, param)

                ! Get parameters.
                if (dm_is_error(dm_cgi_get(param, 'node_id',     node_id))   .or. &
                    dm_is_error(dm_cgi_get(param, 'sensor_id',   sensor_id)) .or. &
                    dm_is_error(dm_cgi_get(param, 'target_id',   target_id)) .or. &
                    dm_is_error(dm_cgi_get(param, 'from',        from))      .or. &
                    dm_is_error(dm_cgi_get(param, 'to',          to))        .or. &
                    dm_is_error(dm_cgi_get(param, 'max_results', nresults) )) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Validate parameters.
                if (.not. dm_id_valid(node_id)   .or. &
                    .not. dm_id_valid(sensor_id) .or. &
                    .not. dm_id_valid(target_id) .or. &
                    .not. dm_time_valid(from)    .or. &
                    .not. dm_time_valid(to)) then
                    call html_error('Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                if (.not. dm_array_has(MAX_RESULTS, nresults)) then
                    call html_error('Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Get observation stubs.
                rc = dm_db_select_observs(db, observs, node_id, sensor_id, target_id, from, to, &
                                          limit=int(nresults, kind=i8), stub=.true., nobservs=nobservs)

                if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                    call html_error('Database Query Failed', error=rc, extra=dm_db_error_message(db))
                    exit response_block
                end if

                ! Output table.
                call html_header(TITLE)
                call dm_cgi_out(dm_html_heading(1, TITLE))
                call dm_cgi_out(html_form_observs(nodes, sensors, targets, max_results, node_id, &
                                                  sensor_id, target_id, from, to, nresults))

                if (nobservs == 0) then
                    call dm_cgi_out(dm_html_p('No observations found.'))
                else
                    call dm_cgi_out(dm_html_observs(observs, prefix=APP_BASE_PATH // '/observ?id=', &
                                                    id=.true., name=.true., source=.true., error=.true.))
                end if

                call html_footer()
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))
            call dm_cgi_out(html_form_observs(nodes, sensors, targets, MAX_RESULTS))

            if (size(nodes) == 0) then
                call dm_cgi_out(dm_html_p('No nodes found.'))
            else if (size(sensors) == 0) then
                call dm_cgi_out(dm_html_p('No sensors found.'))
            else if (size(targets) == 0) then
                call dm_cgi_out(dm_html_p('No targets found.'))
            end if

            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_observs

    subroutine route_plots(env)
        !! Plotting page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/plots`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        !! ## POST Parameters
        !!
        !! * `node_id`       – Node id (string).
        !! * `sensor_id`     – Sensor id (string).
        !! * `target_id`     – Target id (string).
        !! * `response_name` – Observation response name (string).
        !! * `from`          – Time range start (ISO 8601).
        !! * `to`            – Time range end (ISO 8601).
        !! * `max_results`   – Maximum number of data points (integer).
        !!
        character(len=*), parameter :: TITLE       = 'Plots' !! Page title.
        integer,          parameter :: PLOT_WIDTH  = 1050    !! Default plot width.
        integer,          parameter :: PLOT_HEIGHT = 400     !! Default plot height.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        response_block: block
            character(len=:), allocatable    :: str_err, str_out
            character(len=NODE_ID_LEN)       :: node_id
            character(len=SENSOR_ID_LEN)     :: sensor_id
            character(len=TARGET_ID_LEN)     :: target_id
            character(len=RESPONSE_NAME_LEN) :: response_name
            character(len=TIME_LEN)          :: from, to
            integer                          :: max_results(7), nresults
            integer(kind=i8)                 :: npoints
            type(cgi_param_type)             :: param
            type(plot_type)                  :: plot

            type(dp_type),     allocatable :: data_points(:)
            type(node_type),   allocatable :: nodes(:)
            type(sensor_type), allocatable :: sensors(:)
            type(target_type), allocatable :: targets(:)

            max_results = [ 5, 25, 50, 100, 250, 500, 1000 ]

            rc = dm_db_select_nodes(db, nodes)
            rc = dm_db_select_sensors(db, sensors)
            rc = dm_db_select_targets(db, targets)

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                ! Validate content type.
                if (env%content_type /= MIME_FORM) then
                    call html_error(status=HTTP_BAD_REQUEST)
                    exit response_block
                end if

                ! Read form data from request body.
                call dm_cgi_form(env, param)

                ! Get request parameters.
                if (dm_is_error(dm_cgi_get(param, 'node_id',       node_id))       .or. &
                    dm_is_error(dm_cgi_get(param, 'sensor_id',     sensor_id))     .or. &
                    dm_is_error(dm_cgi_get(param, 'target_id',     target_id))     .or. &
                    dm_is_error(dm_cgi_get(param, 'response_name', response_name)) .or. &
                    dm_is_error(dm_cgi_get(param, 'from',          from))          .or. &
                    dm_is_error(dm_cgi_get(param, 'to',            to))            .or. &
                    dm_is_error(dm_cgi_get(param, 'max_results',   nresults))) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Validate parameters.
                if (.not. dm_id_valid(node_id)       .or. &
                    .not. dm_id_valid(sensor_id)     .or. &
                    .not. dm_id_valid(target_id)     .or. &
                    .not. dm_id_valid(response_name) .or. &
                    .not. dm_time_valid(from)        .or. &
                    .not. dm_time_valid(to)) then
                    call html_error('Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                if (.not. dm_array_has(max_results, nresults)) then
                    call html_error('Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Output plot.
                call html_header(TITLE)
                call dm_cgi_out(dm_html_heading(1, TITLE))
                call dm_cgi_out(html_form_plots(nodes, sensors, targets, max_results, node_id, sensor_id, &
                                                target_id, response_name, from, to, nresults))

                ! Get time series.
                rc = dm_db_select_data_points(db, data_points, node_id, sensor_id, target_id, response_name, &
                                              from, to, limit=int(nresults, kind=i8), npoints=npoints)

                if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                    call dm_cgi_out(dm_html_p('Database query failed.'))
                    call html_footer()
                    exit response_block
                end if

                if (rc == E_DB_NO_ROWS .or. npoints == 0) then
                    call dm_cgi_out(dm_html_p('No observations found.'))
                    call html_footer()
                    exit response_block
                end if

                ! Plotting via Gnuplot.
                plot%term     = APP_PLOT_TERM
                plot%font     = 'sans'
                plot%graph    = '#ffffff'
                plot%bidirect = .true.
                plot%width    = PLOT_WIDTH
                plot%height   = PLOT_HEIGHT
                plot%xlabel   = 'Time'
                plot%ylabel   = response_name

                rc = dm_plot_lines(plot, data_points)

                if (dm_plot_error(plot, str_err) > 0) then
                    call dm_cgi_out(dm_html_pre(dm_html_encode(str_err), code=.true.))
                end if

                if (dm_plot_read(plot, str_out) == 0) then
                    call dm_cgi_out(dm_html_p('Failed to execute plotting backend.'))
                    call html_footer()
                    exit response_block
                end if

                ! Output HTML image with base64-encoded data URI.
                call dm_cgi_out(H_FIGURE)
                call dm_cgi_out(dm_html_image(src=dm_html_data_uri(str_out, MIME_SVG), alt='SVG'))
                call dm_cgi_out(H_FIGURE_END)

                call html_footer()
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))
            call dm_cgi_out(html_form_plots(nodes, sensors, targets, max_results))

            if (size(nodes) == 0) then
                call dm_cgi_out(dm_html_p('No nodes found.'))
            else if (size(sensors) == 0) then
                call dm_cgi_out(dm_html_p('No sensors found.'))
            else if (size(targets) == 0) then
                call dm_cgi_out(dm_html_p('No targets found.'))
            end if

            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_plots

    subroutine route_sensor(env)
        !! Sensor page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/sensor`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `id` – Sensor id (string).
        !!
        character(len=*), parameter :: TITLE = 'Sensor' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        response_block: block
            character(len=SENSOR_ID_LEN) :: id
            type(cgi_param_type)         :: param
            type(sensor_type)            :: sensor

            call dm_cgi_query(env, param)
            rc = dm_cgi_get(param, 'id', id)

            if (dm_is_error(rc)) then
                call html_error('Missing Parameter', error=rc)
                exit response_block
            end if

            if (.not. dm_id_valid(id)) then
                call html_error('Invalid Parameter', error=E_INVALID)
                exit response_block
            end if

            rc = dm_db_select(db, sensor, id)

            if (dm_is_error(rc)) then
                call html_error('Sensor Not Found', error=rc)
                exit response_block
            end if

            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))
            call dm_cgi_out(dm_html_sensor(sensor))
            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_sensor

    subroutine route_sensors(env)
        !! Sensors page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/sensors`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        character(len=*), parameter :: TITLE = 'Sensors' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, db_observ, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        response_block: block
            type(cgi_param_type)           :: param
            type(node_type),   allocatable :: nodes(:)
            type(sensor_type)              :: sensor
            type(sensor_type), allocatable :: sensors(:)

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                ! Validate content type.
                if (env%content_type /= MIME_FORM) then
                    call html_error(status=HTTP_BAD_REQUEST)
                    exit response_block
                end if

                ! Read form data from request body.
                call dm_cgi_form(env, param)

                ! Read and validate parameters.
                if (dm_is_error(dm_cgi_get(param, 'id',      sensor%id))      .or. &
                    dm_is_error(dm_cgi_get(param, 'node_id', sensor%node_id)) .or. &
                    dm_is_error(dm_cgi_get(param, 'type',    sensor%type))    .or. &
                    dm_is_error(dm_cgi_get(param, 'name',    sensor%name))) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                rc = dm_cgi_get(param, 'sn',   sensor%sn)
                rc = dm_cgi_get(param, 'meta', sensor%meta)
                rc = dm_cgi_get(param, 'x',    sensor%x)
                rc = dm_cgi_get(param, 'y',    sensor%y)
                rc = dm_cgi_get(param, 'z',    sensor%z)
                rc = dm_cgi_get(param, 'lon',  sensor%lon)
                rc = dm_cgi_get(param, 'lat',  sensor%lat)
                rc = dm_cgi_get(param, 'alt',  sensor%alt)

                ! Validate sensor data.
                if (.not. dm_sensor_valid(sensor)) then
                    call html_error('Invalid Sensor', error=E_INVALID)
                    exit response_block
                end if

                ! Add sensor to database.
                rc = dm_db_insert(db, sensor)

                if (dm_is_error(rc)) then
                    ! Catch file permission error.
                    if (rc /= E_READ_ONLY) rc = dm_db_error(db)
                    call html_error('Database Operation Failed', error=rc)
                    exit response_block
                end if
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))

            rc = dm_db_select_sensors(db, sensors)

            if (size(sensors) > 0) then
                call dm_cgi_out(dm_html_sensors(sensors, prefix=APP_BASE_PATH // '/sensor?id='))
            else
                call dm_cgi_out(dm_html_p('No sensors found.'))
            end if

            rc = dm_db_select_nodes(db, nodes)

            if (size(nodes) > 0) then
                call dm_cgi_out(html_form_sensors(nodes, disabled=read_only))
            else
                call dm_cgi_out(dm_html_p('At least one node is required to add a sensor.'))
            end if

            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_sensors

    subroutine route_status(env)
        !! Status page. Shows system status (time, uptime, host name, ...) and
        !! database status. The database table includes paths, sizes, and access
        !! mode.
        !!
        !! ## Path
        !!
        !! * `/dmpack/status`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version

        character(len=*), parameter :: TITLE = 'Status' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        call html_header(TITLE)
        call dm_cgi_out(dm_html_heading(1, TITLE))

        ! System information.
        system_block: block
            character(len=:), allocatable :: content
            character(len=FILE_PATH_LEN)  :: path
            integer(kind=i8)              :: seconds
            type(uname_type)              :: uname
            type(time_delta_type)         :: uptime

            call dm_system_path(path)
            call dm_system_uname(uname)
            call dm_system_uptime(seconds)
            call dm_time_delta_from_seconds(uptime, seconds)

            content = H_TABLE // H_TBODY // &
                      H_TR // H_TH // 'Hostname' // H_TH_END // &
                              H_TD // dm_html_encode(uname%node_name) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Server Time' // H_TH_END // &
                              H_TD // dm_html_time(dm_time_now(), human=.true.) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Server Uptime' // H_TH_END // &
                              H_TD // dm_time_delta_to_string(uptime) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'OS Name' // H_TH_END // &
                              H_TD // dm_html_encode(uname%system_name) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'OS Release' // H_TH_END // &
                              H_TD // dm_html_encode(uname%release) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'OS Version' // H_TH_END // &
                              H_TD // dm_html_encode(uname%version) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'OS Platform' // H_TH_END // &
                              H_TD // dm_html_encode(uname%machine) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Remote Address' // H_TH_END // &
                              H_TD // dm_html_encode(env%remote_addr) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Remote User' // H_TH_END // &
                              H_TD // dm_html_encode(env%remote_user) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Executable Path' // H_TH_END // &
                              H_TD // dm_html_encode(path) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Executable Version' // H_TH_END // &
                              H_TD // dm_version_to_string(APP_MAJOR, APP_MINOR, APP_PATCH) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'DMPACK Version' // H_TH_END // &
                              H_TD // DM_VERSION_STRING // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'SQLite Version' // H_TH_END // &
                              H_TD // dm_db_version() // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Compiler' // H_TH_END // &
                              H_TD // dm_html_encode(compiler_version()) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Compiler Options' // H_TH_END // &
                              H_TD // dm_html_encode(compiler_options()) // H_TD_END // H_TR_END // &
                      H_TBODY_END // H_TABLE_END

            call dm_cgi_out(dm_html_heading(2, 'System'))
            call dm_cgi_out(content)
        end block system_block

        ! Database information.
        db_block: block
            integer(kind=i8), parameter :: FSIZE = 1024_i8**2 !! Bytes to MiB factor.

            character(len=:), allocatable :: content
            character(len=:), allocatable :: mode
            integer(kind=i8)              :: sz
            logical                       :: db_beat_exists, db_log_exists, db_observ_exists

            db_beat_exists   = dm_file_exists(db_beat)
            db_log_exists    = dm_file_exists(db_log)
            db_observ_exists = dm_file_exists(db_observ)

            if (.not. db_beat_exists .and. .not. db_log_exists .and. db_observ_exists) exit db_block

            mode = dm_btoa(read_only, 'yes', 'no')

            content = H_TABLE // H_THEAD // &
                      H_TR // H_TH // 'Type'      // H_TH_END // &
                              H_TH // 'Path'      // H_TH_END // &
                              H_TH // 'Size'      // H_TH_END // &
                              H_TH // 'Read-Only' // H_TH_END // H_TR_END // &
                      H_THEAD_END // H_TBODY

            ! The sizes will be at least 1 MiB, even if a file is actually smaller.
            ! This way, it is easier to distinguish between non-existing and small
            ! databases, as non-existing ones will always be of size zero.
            if (db_beat_exists) then
                sz = max(1_i8, dm_file_size(db_beat) / FSIZE)
                content = content // H_TR // &
                                     H_TD // 'Beat'                           // H_TD_END // &
                                     H_TD // dm_html_encode(db_beat)          // H_TD_END // &
                                     H_TD // dm_itoa(sz) // ' MiB'            // H_TD_END // &
                                     H_TD // dm_html_mark(mode, class='info') // H_TD_END // &
                                     H_TR_END
            end if

            if (db_log_exists) then
                sz = max(1_i8, dm_file_size(db_log) / FSIZE)
                content = content // H_TR // &
                                     H_TD // 'Log'                            // H_TD_END // &
                                     H_TD // dm_html_encode(db_log)           // H_TD_END // &
                                     H_TD // dm_itoa(sz) // ' MiB'            // H_TD_END // &
                                     H_TD // dm_html_mark(mode, class='info') // H_TD_END // &
                                     H_TR_END
            end if

            if (db_observ_exists) then
                sz = max(1_i8, dm_file_size(db_observ) / FSIZE)
                content = content // H_TR // &
                                     H_TD // 'Observation'                    // H_TD_END // &
                                     H_TD // dm_html_encode(db_observ)        // H_TD_END // &
                                     H_TD // dm_itoa(sz) // ' MiB'            // H_TD_END // &
                                     H_TD // dm_html_mark(mode, class='info') // H_TD_END // &
                                     H_TR_END
            end if

            content = content // H_TBODY_END // H_TABLE_END

            call dm_cgi_out(dm_html_heading(2, 'Databases'))
            call dm_cgi_out(content)
        end block db_block

        call html_footer()
    end subroutine route_status

    subroutine route_target(env)
        !! Target page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/target`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `id` – Target id (string).
        !!
        character(len=*), parameter :: TITLE = 'Target' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, db_observ, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        response_block: block
            character(len=TARGET_ID_LEN) :: id
            type(cgi_param_type)         :: param
            type(target_type)            :: target

            call dm_cgi_query(env, param)
            rc = dm_cgi_get(param, 'id', id)

            if (dm_is_error(rc)) then
                call html_error('Missing Parameter', error=rc)
                exit response_block
            end if

            if (.not. dm_id_valid(id)) then
                call html_error('Invalid Parameter', error=E_INVALID)
                exit response_block
            end if

            rc = dm_db_select(db, target, id)

            if (dm_is_error(rc)) then
                call html_error('Target Not Found', error=rc)
                exit response_block
            end if

            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))
            call dm_cgi_out(dm_html_target(target))
            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_target

    subroutine route_targets(env)
        !! Targets page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/targets`
        !!
        !! ## Methods
        !!
        !! * GET
        !! * POST
        !!
        character(len=*), parameter :: TITLE = 'Targets' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, db_observ, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        response_block: block
            type(cgi_param_type)           :: param
            type(target_type)              :: target
            type(target_type), allocatable :: targets(:)

            ! ------------------------------------------------------------------
            ! POST REQUEST.
            ! ------------------------------------------------------------------
            if (env%request_method == 'POST') then
                ! Validate content type.
                if (env%content_type /= MIME_FORM) then
                    call html_error(status=HTTP_BAD_REQUEST)
                    exit response_block
                end if

                ! Read form data from request body.
                call dm_cgi_form(env, param)

                ! Read and validate parameters.
                if (dm_is_error(dm_cgi_get(param, 'id', target%id)) .or. &
                    dm_is_error(dm_cgi_get(param, 'name', target%name))) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Invalid state, x, y, and z are replaced with the default values.
                rc = dm_cgi_get(param, 'meta',  target%meta)
                rc = dm_cgi_get(param, 'state', target%state)
                rc = dm_cgi_get(param, 'x',     target%x)
                rc = dm_cgi_get(param, 'y',     target%y)
                rc = dm_cgi_get(param, 'z',     target%z)
                rc = dm_cgi_get(param, 'lon',   target%lon)
                rc = dm_cgi_get(param, 'lat',   target%lat)
                rc = dm_cgi_get(param, 'alt',   target%alt)

                ! Validate target data.
                if (.not. dm_target_valid(target)) then
                    call html_error('Invalid Target', error=E_INVALID)
                    exit response_block
                end if

                ! Add target to database.
                rc = dm_db_insert(db, target)

                if (dm_is_error(rc)) then
                    ! Catch file permission error.
                    if (rc /= E_READ_ONLY) rc = dm_db_error(db)
                    call html_error('Database Operation Failed', error=rc)
                    exit response_block
                end if
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call html_header(TITLE)
            call dm_cgi_out(dm_html_heading(1, TITLE))

            rc = dm_db_select_targets(db, targets)

            if (size(targets) > 0) then
                call dm_cgi_out(dm_html_targets(targets, prefix=APP_BASE_PATH // '/target?id='))
            else
                call dm_cgi_out(dm_html_p('No targets found.'))
            end if

            call dm_cgi_out(html_form_targets(disabled=read_only))
            call html_footer()
        end block response_block

        rc = dm_db_close(db)
    end subroutine route_targets

    ! ******************************************************************
    ! HTML FORM GENERATORS.
    ! ******************************************************************
    function html_form_logs(nodes, sensors, targets, max_results, node_id, sensor_id, target_id, &
                            source, from, to, level, nresults) result(html)
        !! Returns HTML form for log selection.
        type(node_type),   intent(inout)        :: nodes(:)       !! Node types.
        type(sensor_type), intent(inout)        :: sensors(:)     !! Sensor types.
        type(target_type), intent(inout)        :: targets(:)     !! Target types.
        integer,           intent(inout)        :: max_results(:) !! Max. results to show.
        character(len=*),  intent(in), optional :: node_id        !! Selected node.
        character(len=*),  intent(in), optional :: sensor_id      !! Selected sensor.
        character(len=*),  intent(in), optional :: target_id      !! Selected target.
        character(len=*),  intent(in), optional :: source         !! Log source.
        character(len=*),  intent(in), optional :: from           !! Start time.
        character(len=*),  intent(in), optional :: to             !! End time.
        integer,           intent(in), optional :: level          !! Log level.
        integer,           intent(in), optional :: nresults       !! Selected number of results.
        character(len=:), allocatable           :: html           !! HTML form.

        character(len=NODE_ID_LEN)    :: node_id_
        character(len=SENSOR_ID_LEN)  :: sensor_id_
        character(len=TARGET_ID_LEN)  :: target_id_
        character                     :: level_
        character(len=LOG_SOURCE_LEN) :: source_
        character(len=TIME_LEN)       :: from_
        character(len=TIME_LEN)       :: to_
        integer                       :: nresults_
        integer                       :: i

        character(len=4) :: year
        character(len=2) :: month
        character(len=2) :: day

        type(select_type) :: select_node
        type(select_type) :: select_sensor
        type(select_type) :: select_target
        type(select_type) :: select_level
        type(select_type) :: select_result

        call dm_time_strings(year, month, day)

        node_id_   = ' '
        sensor_id_ = ' '
        target_id_ = ' '
        level_     = ' '
        source_    = ' '
        from_      = year // '-' // month // '-' // day // 'T00:00:00'
        to_        = '2100-01-01T00:00:00'
        nresults_  = 0

        if (present(node_id))   node_id_   = dm_html_encode(node_id)
        if (present(sensor_id)) sensor_id_ = dm_html_encode(sensor_id)
        if (present(target_id)) target_id_ = dm_html_encode(target_id)
        if (present(source))    source_    = dm_html_encode(source)
        if (present(level))     level_     = dm_itoa(level)
        if (present(from))      from_      = dm_html_encode(from)
        if (present(to))        to_        = dm_html_encode(to)
        if (present(nresults))  nresults_  = nresults

        ! Create HTML select elements for form. Add 1 due to empty element.
        call dm_html_select_create(select_node,   1 + size(nodes))
        call dm_html_select_create(select_sensor, 1 + size(sensors))
        call dm_html_select_create(select_target, 1 + size(targets))
        call dm_html_select_create(select_level,  1 + LOG_NLEVEL)
        call dm_html_select_create(select_result, size(max_results))

        ! Add empty select elements.
        call dm_html_select_set(select_node,   1, '', '')
        call dm_html_select_set(select_sensor, 1, '', '')
        call dm_html_select_set(select_target, 1, '', '')
        call dm_html_select_set(select_level,  1, '', '')

        ! Add all other select elements.
        do i = 1, size(nodes)
            call dm_html_select_set(select_node, i + 1, nodes(i)%name, nodes(i)%id)
        end do

        do i = 1, size(sensors)
            call dm_html_select_set(select_sensor, i + 1, sensors(i)%name, sensors(i)%id)
        end do

        do i = 1, size(targets)
            call dm_html_select_set(select_target,  i + 1, targets(i)%name, targets(i)%id)
        end do

        do i = 1, LOG_NLEVEL
            call dm_html_select_set(select_level, i + 1, LOG_LEVEL_NAMES(i - 1), dm_itoa(i - 1))
        end do

        do i = 1, size(max_results)
            call dm_html_select_set(select_result, i, dm_itoa(max_results(i)), dm_itoa(max_results(i)))
        end do

        ! Create HTML.
        html = H_FORM_POST // H_FIELDSET // &
               H_DIV_ROW // & ! row 1
               H_DIV_COL // & ! column 1
               dm_html_label('Node Name', for='node_id') // &
               dm_html_select(select_node, 'node_id', 'node_id', node_id_) // &
               dm_html_label('Sensor Name', for='sensor_id') // &
               dm_html_select(select_sensor, 'sensor_id', 'sensor_id', sensor_id_) // &
               dm_html_label('Target Name', for='target_id') // &
               dm_html_select(select_target, 'target_id', 'target_id', target_id_) // &
               H_DIV_END // & ! end column 1
               H_DIV_COL // & ! column 2
               dm_html_label('Source', for='source') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, id='source', name='source', &
                             max_length=LOG_SOURCE_LEN, pattern='[\-0-9A-Z_a-z]+', &
                             placeholder='Enter log source (optional)', value=source_) // &
               dm_html_label('From', for='from') // &
               dm_html_input(HTML_INPUT_TYPE_DATETIME_LOCAL, id='from', name='from', required=.true., value=from_) // &
               dm_html_label('To', for='to') // &
               dm_html_input(HTML_INPUT_TYPE_DATETIME_LOCAL, id='to', name='to', required=.true., value=to_) // &
               H_DIV_END // & ! end column 2
               H_DIV_COL // & ! column 3
               dm_html_label('Log Level', for='level') // &
               dm_html_select(select_level, 'level', 'level', level_) // &
               dm_html_label('Max. Results', for='max_results') // &
               dm_html_select(select_result, 'max_results', 'max_results', dm_itoa(nresults_)) // &
               H_DIV_END // & ! end column 3
               H_DIV_END // & ! end row 1
               dm_html_input(HTML_INPUT_TYPE_SUBMIT, name='submit', value='Search') // &
               H_FIELDSET_END // H_FORM_END

        call dm_html_select_destroy(select_node)
        call dm_html_select_destroy(select_sensor)
        call dm_html_select_destroy(select_target)
        call dm_html_select_destroy(select_level)
        call dm_html_select_destroy(select_result)
    end function html_form_logs

    function html_form_nodes(disabled) result(html)
        !! Returns HTML form for node creation.
        logical, intent(in), optional :: disabled !! Form elements are disabled.
        character(len=:), allocatable :: html     !! HTML form.

        logical :: disabled_

        disabled_ = .false.
        if (present(disabled)) disabled_ = disabled

        ! Create HTML.
        html = H_DETAILS // H_SUMMARY // 'Add Node' // H_SUMMARY_END // &
               H_P // 'Add a new sensor node to the database.' // H_P_END // &
               H_FORM_POST // H_FIELDSET // &
               H_DIV_ROW // & ! row 1
               H_DIV_COL // & ! column 1
               dm_html_label('ID', for='id') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='id', name='id', &
                             max_length=NODE_ID_LEN, pattern='[\-0-9A-Z_a-z]+', &
                             placeholder='Enter unique id', required=.true.) // &
               dm_html_label('Name', for='name') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='name', name='name', &
                             max_length=NODE_NAME_LEN, placeholder='Enter name', &
                             required=.true.) // &
               dm_html_label('Meta', for='meta') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='meta', name='meta', &
                             max_length=NODE_META_LEN, placeholder='Enter description (optional)') // &
               H_DIV_END // & ! end column 1
               H_DIV_COL // & ! column 2
               dm_html_label('X', for='x') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='x', name='x', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter X or easting (optional)') // &
               dm_html_label('Y', for='y') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='y', name='y', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter Y or northing (optional)') // &
               dm_html_label('Z', for='z') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='z', name='z', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter Z or elevation (optional)') // &
               H_DIV_END // & ! end column 2
               H_DIV_COL // & ! column 3
               dm_html_label('Longitude', for='lon') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='lon', name='lon', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter longitude (optional)') // &
               dm_html_label('Latitude', for='lat') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='lat', name='lat', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter latitude (optional)') // &
               dm_html_label('Altitude', for='alt') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='alt', name='alt', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter altitude (optional)') // &
               H_DIV_END // & ! end column 3
               H_DIV_END // & ! end row 1
               dm_html_input(HTML_INPUT_TYPE_SUBMIT, disabled=disabled_, name='submit', value='Submit') // &
               H_FIELDSET_END // H_FORM_END // H_DETAILS_END
    end function html_form_nodes

    function html_form_observs(nodes, sensors, targets, max_results, node_id, sensor_id, &
                               target_id, from, to, nresults) result(html)
        !! Returns HTML form for time series selection.
        type(node_type),   intent(inout)        :: nodes(:)       !! Node types.
        type(sensor_type), intent(inout)        :: sensors(:)     !! Sensor types.
        type(target_type), intent(inout)        :: targets(:)     !! Target types.
        integer,           intent(inout)        :: max_results(:) !! Max. results to show.
        character(len=*),  intent(in), optional :: node_id        !! Selected node.
        character(len=*),  intent(in), optional :: sensor_id      !! Selected sensor.
        character(len=*),  intent(in), optional :: target_id      !! Selected target.
        character(len=*),  intent(in), optional :: from           !! Start time.
        character(len=*),  intent(in), optional :: to             !! End time.
        integer,           intent(in), optional :: nresults       !! Selected number of results.
        character(len=:), allocatable           :: html           !! HTML form.

        character(len=NODE_ID_LEN)   :: node_id_
        character(len=SENSOR_ID_LEN) :: sensor_id_
        character(len=TARGET_ID_LEN) :: target_id_
        character(len=TIME_LEN)      :: from_
        character(len=TIME_LEN)      :: to_
        integer                      :: nresults_
        integer                      :: i
        logical                      :: disabled

        character(len=4) :: year
        character(len=2) :: month
        character(len=2) :: day

        type(select_type) :: select_node
        type(select_type) :: select_result
        type(select_type) :: select_sensor
        type(select_type) :: select_target

        call dm_time_strings(year, month, day)

        from_     = year // '-' // month // '-' // day // 'T00:00:00'
        to_       = '2100-01-01T00:00:00'
        nresults_ = 0

        if (present(node_id))   node_id_   = dm_html_encode(node_id)
        if (present(sensor_id)) sensor_id_ = dm_html_encode(sensor_id)
        if (present(target_id)) target_id_ = dm_html_encode(target_id)
        if (present(from))      from_      = dm_html_encode(from)
        if (present(to))        to_        = dm_html_encode(to)
        if (present(nresults))  nresults_  = nresults

        ! Create HTML select elements for form.
        call dm_html_select_create(select_node,   size(nodes))
        call dm_html_select_create(select_sensor, size(sensors))
        call dm_html_select_create(select_target, size(targets))
        call dm_html_select_create(select_result, size(max_results))

        ! Fill select elements.
        do i = 1, size(nodes)
            call dm_html_select_set(select_node, i, nodes(i)%name, nodes(i)%id)
        end do

        do i = 1, size(sensors)
            call dm_html_select_set(select_sensor, i, sensors(i)%name, sensors(i)%id)
        end do

        do i = 1, size(targets)
            call dm_html_select_set(select_target, i, targets(i)%name, targets(i)%id)
        end do

        do i = 1, size(max_results)
            call dm_html_select_set(select_result, i, dm_itoa(max_results(i)), dm_itoa(max_results(i)))
        end do

        disabled = (size(nodes) == 0)

        ! Create HTML.
        html = H_FORM_POST // H_FIELDSET // &
               H_DIV_ROW // & ! row 1
               H_DIV_COL // & ! column 1
               dm_html_label('Node Name', for='node_id') // &
               dm_html_select(select_node, 'node_id', 'node_id', node_id_) // &
               dm_html_label('Sensor Name', for='sensor_id') // &
               dm_html_select(select_sensor, 'sensor_id', 'sensor_id', sensor_id_) // &
               dm_html_label('Target Name', for='target_id') // &
               dm_html_select(select_target, 'target_id', 'target_id', target_id_) // &
               H_DIV_END // & ! end column 1
               H_DIV_COL // & ! column 2
               dm_html_label('From', for='from') // &
               dm_html_input(HTML_INPUT_TYPE_DATETIME_LOCAL, id='from', name='from', required=.true., value=from_) // &
               dm_html_label('To', for='to') // &
               dm_html_input(HTML_INPUT_TYPE_DATETIME_LOCAL, id='to', name='to', required=.true., value=to_) // &
               H_DIV_END // & ! end column 2
               H_DIV_COL // & ! column 3
               dm_html_label('Max. Results', for='max_results') // &
               dm_html_select(select_result, 'max_results', 'max_results', dm_itoa(nresults_)) // &
               H_DIV_END // & ! end column 3
               H_DIV_END // & ! end row 1
               dm_html_input(HTML_INPUT_TYPE_SUBMIT, disabled=disabled, name='submit', value='Search') // &
               H_FIELDSET_END // H_FORM_END

        call dm_html_select_destroy(select_node)
        call dm_html_select_destroy(select_sensor)
        call dm_html_select_destroy(select_target)
        call dm_html_select_destroy(select_result)
    end function html_form_observs

    function html_form_plots(nodes, sensors, targets, max_results, node_id, sensor_id, &
                             target_id, response_name, from, to, nresults) result(html)
        !! Returns HTML form for plot selection.
        type(node_type),   intent(inout)        :: nodes(:)       !! Node types.
        type(sensor_type), intent(inout)        :: sensors(:)     !! Sensor types.
        type(target_type), intent(inout)        :: targets(:)     !! Target types.
        integer,           intent(inout)        :: max_results(:) !! Max. results to show.
        character(len=*),  intent(in), optional :: node_id        !! Selected node.
        character(len=*),  intent(in), optional :: sensor_id      !! Selected sensor.
        character(len=*),  intent(in), optional :: target_id      !! Selected target.
        character(len=*),  intent(in), optional :: response_name  !! Selected response name.
        character(len=*),  intent(in), optional :: from           !! Start time.
        character(len=*),  intent(in), optional :: to             !! End time.
        integer,           intent(in), optional :: nresults       !! Selected number of results.
        character(len=:), allocatable           :: html           !! HTML form.

        character(len=NODE_ID_LEN)       :: node_id_
        character(len=SENSOR_ID_LEN)     :: sensor_id_
        character(len=TARGET_ID_LEN)     :: target_id_
        character(len=RESPONSE_NAME_LEN) :: response_name_
        character(len=TIME_LEN)          :: from_
        character(len=TIME_LEN)          :: to_
        integer                          :: nresults_
        integer                          :: i
        logical                          :: disabled

        character(len=4) :: year
        character(len=2) :: month
        character(len=2) :: day

        type(select_type) :: select_node
        type(select_type) :: select_result
        type(select_type) :: select_sensor
        type(select_type) :: select_target

        call dm_time_strings(year, month, day)

        node_id_       = ' '
        sensor_id_     = ' '
        target_id_     = ' '
        response_name_ = ' '
        from_          = year // '-' // month // '-' // day // 'T00:00:00'
        to_            = '2100-01-01T00:00:00'
        nresults_      = 0

        if (present(node_id))       node_id_       = dm_html_encode(node_id)
        if (present(sensor_id))     sensor_id_     = dm_html_encode(sensor_id)
        if (present(target_id))     target_id_     = dm_html_encode(target_id)
        if (present(response_name)) response_name_ = dm_html_encode(response_name)
        if (present(from))          from_          = dm_html_encode(from)
        if (present(to))            to_            = dm_html_encode(to)
        if (present(nresults))      nresults_      = nresults

        ! Create HTML select elements for form.
        call dm_html_select_create(select_node,   size(nodes))
        call dm_html_select_create(select_sensor, size(sensors))
        call dm_html_select_create(select_target, size(targets))
        call dm_html_select_create(select_result, size(max_results))

        ! Fill select elements.
        do i = 1, size(nodes)
            call dm_html_select_set(select_node, i, nodes(i)%name, nodes(i)%id)
        end do

        do i = 1, size(sensors)
            call dm_html_select_set(select_sensor, i, sensors(i)%name, sensors(i)%id)
        end do

        do i = 1, size(targets)
            call dm_html_select_set(select_target, i, targets(i)%name, targets(i)%id)
        end do

        do i = 1, size(max_results)
            call dm_html_select_set(select_result, i, dm_itoa(max_results(i)), dm_itoa(max_results(i)))
        end do

        disabled = (size(nodes) == 0)

        ! Create HTML.
        html = H_FORM_POST // H_FIELDSET // &
               H_DIV_ROW // & ! row 1
               H_DIV_COL // & ! column 1
               dm_html_label('Node Name', for='node_id') // &
               dm_html_select(select_node, 'node_id', 'node_id', node_id_) // &
               dm_html_label('Sensor Name', for='sensor_id') // &
               dm_html_select(select_sensor, 'sensor_id', 'sensor_id', sensor_id_) // &
               dm_html_label('Target Name', for='target_id') // &
               dm_html_select(select_target, 'target_id', 'target_id', target_id_) // &
               H_DIV_END // & ! end column 1
               H_DIV_COL // & ! column 2
               dm_html_label('Response Name', for='response_name') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, id='response_name', name='response_name', &
                             max_length=RESPONSE_NAME_LEN, pattern='[\-0-9A-Z_a-z]+', &
                             placeholder='Enter response name', required=.true., &
                             value=response_name_) // &
               dm_html_label('From', for='from') // &
               dm_html_input(HTML_INPUT_TYPE_DATETIME_LOCAL, id='from', name='from', required=.true., value=from_) // &
               dm_html_label('To', for='to') // &
               dm_html_input(HTML_INPUT_TYPE_DATETIME_LOCAL, id='to', name='to', required=.true., value=to_) // &
               H_DIV_END // & ! end column 2
               H_DIV_COL // & ! column 3
               dm_html_label('Max. Results', for='max_results') // &
               dm_html_select(select_result, 'max_results', 'max_results', dm_itoa(nresults_)) // &
               H_DIV_END // & ! end column 3
               H_DIV_END // & ! end row 1
               dm_html_input(HTML_INPUT_TYPE_SUBMIT, disabled=disabled, name='submit', value='Plot') // &
               H_FIELDSET_END // H_FORM_END

        call dm_html_select_destroy(select_node)
        call dm_html_select_destroy(select_sensor)
        call dm_html_select_destroy(select_target)
        call dm_html_select_destroy(select_result)
    end function html_form_plots

    function html_form_sensors(nodes, disabled) result(html)
        !! Returns HTML form for sensor creation.
        type(node_type), intent(inout)        :: nodes(:) !! Node types.
        logical,         intent(in), optional :: disabled !! Form elements are disabled.
        character(len=:), allocatable         :: html     !! HTML form.

        integer           :: i
        logical           :: disabled_
        type(select_type) :: select_node
        type(select_type) :: select_sensor_type

        disabled_ = .false.
        if (present(disabled)) disabled_ = disabled

        call dm_html_select_create(select_node, size(nodes))
        call dm_html_select_create(select_sensor_type, SENSOR_TYPE_LAST + 1)

        do i = 1, size(nodes)
            call dm_html_select_set(select_node, i, nodes(i)%name, nodes(i)%id)
        end do

        do i = 0, SENSOR_TYPE_LAST
            call dm_html_select_set(select_sensor_type, i + 1, SENSOR_TYPE_NAMES(i), dm_itoa(i))
        end do

        ! Create HTML.
        html = H_DETAILS // H_SUMMARY // 'Add Sensor' // H_SUMMARY_END // &
               H_P // 'Add a new sensor to the database.' // H_P_END // &
               H_FORM_POST // H_FIELDSET // &
               H_DIV_ROW // & ! row 1
               H_DIV_COL // & ! column 1
               dm_html_label('Node Name', for='node_id') // &
               dm_html_select(select_node, 'node_id', 'node_id', '', disabled=disabled_) // &
               dm_html_label('ID', for='id') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='id', name='id', &
                             max_length=SENSOR_ID_LEN, pattern='[\-0-9A-Z_a-z]+', &
                             placeholder='Enter unique id', required=.true.) // &
               dm_html_label('Name', for='name') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='name', name='name', &
                             max_length=SENSOR_NAME_LEN, placeholder='Enter name', &
                             required=.true.) // &
               H_DIV_END // & ! end column 1
               H_DIV_COL // & ! column 2
               dm_html_label('Type', for='type') // &
               dm_html_select(select_sensor_type, 'type', 'type', dm_itoa(SENSOR_TYPE_NONE), disabled=disabled_) // &
               dm_html_label('Serial Number', for='sn') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='sn', name='sn', &
                             max_length=SENSOR_SN_LEN, placeholder='Enter serial number (optional)') // &
               dm_html_label('Meta', for='meta') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='meta', name='meta', &
                             max_length=SENSOR_META_LEN, placeholder='Enter description (optional)') // &
               H_DIV_END // & ! end column 2
               H_DIV_COL // & ! column 3
               dm_html_label('X', for='x') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='x', name='x', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter X or easting (optional)') // &
               dm_html_label('Y', for='y') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='y', name='y', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter Y or northing (optional)') // &
               dm_html_label('Z', for='z') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='z', name='z', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter Z or elevation (optional)') // &
               H_DIV_END // & ! end column 3
               H_DIV_COL // & ! column 4
               dm_html_label('Longitude', for='lon') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='lon', name='lon', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter longitude (optional)') // &
               dm_html_label('Latitude', for='lat') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='lat', name='lat', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter latitude (optional)') // &
               dm_html_label('Altitude', for='alt') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='alt', name='alt', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter altitude (optional)') // &
               H_DIV_END // & ! end column 4
               H_DIV_END // & ! end row 1
               dm_html_input(HTML_INPUT_TYPE_SUBMIT, disabled=disabled_, name='submit', value='Submit') // &
               H_FIELDSET_END // H_FORM_END // H_DETAILS_END

        call dm_html_select_destroy(select_node)
        call dm_html_select_destroy(select_sensor_type)
    end function html_form_sensors

    function html_form_targets(disabled) result(html)
        !! Returns HTML form for target creation.
        logical, intent(in), optional :: disabled !! Form elements are disabled.
        character(len=:), allocatable :: html     !! HTML form.

        integer           :: i
        logical           :: disabled_
        type(select_type) :: select_target_state

        disabled_ = .false.
        if (present(disabled)) disabled_ = disabled

        call dm_html_select_create(select_target_state, TARGET_STATE_LAST + 1)

        do i = 0, TARGET_STATE_LAST
            call dm_html_select_set(select_target_state, i + 1, TARGET_STATE_NAMES(i), dm_itoa(i))
        end do

        ! Create HTML.
        html = H_DETAILS // H_SUMMARY // 'Add Target' // H_SUMMARY_END // &
               H_P // 'Add a new target to the database.' // H_P_END // &
               H_FORM_POST // H_FIELDSET // &
               H_DIV_ROW // & ! row 1
               H_DIV_COL // & ! column 1
               dm_html_label('ID', for='id') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='id', name='id', &
                             max_length=TARGET_ID_LEN, pattern='[\-0-9A-Z_a-z]+', &
                             placeholder='Enter unique id', required=.true.) // &
               dm_html_label('Name', for='name') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='name', name='name', &
                             max_length=TARGET_NAME_LEN, placeholder='Enter name', &
                             required=.true.) // &
               dm_html_label('Meta', for='meta') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='meta', name='meta', &
                             max_length=TARGET_META_LEN, placeholder='Enter description (optional)') // &
               dm_html_label('State', for='state') // &
               dm_html_select(select_target_state, 'state', 'state', dm_itoa(TARGET_STATE_NONE), disabled=disabled_) // &
               H_DIV_END // & ! end column 1
               H_DIV_COL // & ! column 2
               dm_html_label('X', for='x') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='x', name='x', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter X or easting (optional)') // &
               dm_html_label('Y', for='y') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='y', name='y', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter Y or northing (optional)') // &
               dm_html_label('Z', for='z') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='z', name='z', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter Z or elevation (optional)') // &
               H_DIV_END // & ! end column 2
               H_DIV_COL // & ! column 3
               dm_html_label('Longitude', for='lon') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='lon', name='lon', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter longitude (optional)') // &
               dm_html_label('Latitude', for='lat') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='lat', name='lat', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter latitude (optional)') // &
               dm_html_label('Altitude', for='alt') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='alt', name='alt', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter altitude (optional)') // &
               H_DIV_END // & ! end column 3
               H_DIV_END // & ! end row 1
               dm_html_input(HTML_INPUT_TYPE_SUBMIT, disabled=disabled_, name='submit', value='Submit') // &
               H_FIELDSET_END // H_FORM_END // H_DETAILS_END
    end function html_form_targets

    ! ******************************************************************
    ! UTILITY PROCEDURES.
    ! ******************************************************************
    subroutine html_error(heading, error, status, title, extra)
        !! Outputs error page (with header and footer).
        character(len=*), intent(in), optional :: heading !! Page heading.
        integer,          intent(in), optional :: error   !! DMPACK error code.
        integer,          intent(in), optional :: status  !! HTTP status code.
        character(len=*), intent(in), optional :: title   !! Page title.
        character(len=*), intent(in), optional :: extra   !! Extra message, pre-formatted.

        if (present(title)) then
            call html_header(title)
        else
            call html_header()
        end if

        if (present(status)) then
            call dm_cgi_out(dm_html_heading(1, dm_itoa(status) // ' ' // dm_http_status_string(status)))

            select case (status)
                case (HTTP_BAD_REQUEST)
                    call dm_cgi_out(dm_html_p('Malformed request or invalid request header.'))
                case (HTTP_NOT_FOUND)
                    call dm_cgi_out(dm_html_p('The requested resource could not be found.'))
                case (HTTP_INTERNAL_SERVER_ERROR, HTTP_SERVICE_UNAVAILABLE)
                    call dm_cgi_out(dm_html_p('An internal server error occured.'))
                case default
                    call dm_cgi_out(dm_html_p('An error occured.'))
            end select
        else
            if (present(heading)) then
                call dm_cgi_out(dm_html_heading(1, dm_html_encode(heading)))
            else
                call dm_cgi_out(dm_html_heading(1, 'Error'))
            end if
        end if

        if (present(error)) then
            call dm_cgi_out(dm_html_error(error))
        end if

        if (present(extra)) then
            call dm_cgi_out(dm_html_pre(extra, code=.true.))
        end if

        call html_footer()
    end subroutine html_error

    subroutine html_footer()
        !! Outputs HTML footer.
        character(len=*), parameter :: CONTENT = &
            H_P // H_SMALL // &
            '<a href="https://www.dabamos.de/">DMPACK</a> ' // DM_VERSION_STRING // &
            ' | <a href="' // APP_BASE_PATH // '/licence">Licence</a>' // &
            ' | <a href="' // APP_BASE_PATH // '/status">Status</a>' // &
            H_SMALL_END // H_P_END

        call dm_cgi_out(dm_html_footer(CONTENT))
    end subroutine html_footer

    subroutine html_header(title, inline_style, style)
        !! Outputs HTTP header, HTML header, and navigation.
        integer, parameter :: NANCHORS = 9 !! Number of elements in navigation.

        character(len=*), intent(in), optional :: title        !! Page title.
        character(len=*), intent(in), optional :: inline_style !! Additional inline CSS.
        character(len=*), intent(in), optional :: style        !! Additional CSS path.

        character(len=:), allocatable :: title_
        logical                       :: mask(NANCHORS)
        type(anchor_type)             :: nav(NANCHORS)
        type(string_type)             :: styles(2)

        ! HTML anchors for sidebar navigation.
        nav = [ &
            anchor_type(APP_BASE_PATH // '/',        'Dashboard'),    &
            anchor_type(APP_BASE_PATH // '/nodes',   'Nodes'),        &
            anchor_type(APP_BASE_PATH // '/sensors', 'Sensors'),      &
            anchor_type(APP_BASE_PATH // '/targets', 'Targets'),      &
            anchor_type(APP_BASE_PATH // '/observs', 'Observations'), &
            anchor_type(APP_BASE_PATH // '/plots',   'Plots'),        &
            anchor_type(APP_BASE_PATH // '/logs',    'Logs'),         &
            anchor_type(APP_BASE_PATH // '/beats',   'Beats'),        &
            anchor_type(APP_BASE_PATH // '/map',     'Map')           &
        ]

        mask = [ &
            .true.,        & ! Dashboard.
            has_db_observ, & ! Nodes.
            has_db_observ, & ! Sensors.
            has_db_observ, & ! Targets.
            has_db_observ, & ! Observations.
            has_db_observ, & ! Plots.
            has_db_log,    & ! Logs.
            has_db_beat,   & ! Beats.
            has_tile_url   & ! Map.
        ]

        ! HTML document header.
        call dm_cgi_header(MIME_HTML, HTTP_OK)

        ! Page title.
        if (present(title)) then
            title_ = title // ' | ' // APP_TITLE
        else
            title_ = APP_TITLE
        end if

        ! Style sheet files.
        styles(1) = string_type(APP_CSS_PATH // '/dmpack.min.css')
        if (present(style)) styles(2) = string_type(style)

        ! Output header.
        call dm_cgi_out(dm_html_header(title        = title_,       &
                                       brand        = APP_TITLE,    &
                                       inline_style = inline_style, &
                                       styles       = styles,       &
                                       nav          = nav,          &
                                       nav_mask     = mask))
    end subroutine html_header
end program dmweb
