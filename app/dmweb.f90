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
    !! $HTTP["url"] =~ "^/dmpack" {
    !!   # Map URL to CGI executable.
    !!   alias.url += ( "/dmpack" => "/usr/local/bin/dmweb" )
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
    !! | Variable       | Description                                  |
    !! |----------------|----------------------------------------------|
    !! | `DM_BEAT_DB`   | Path to beat database.                       |
    !! | `DM_IMAGE_DB`  | Path to image database.                      |
    !! | `DM_IMAGE_DIR` | Path to images from WWW root directory.      |
    !! | `DM_LOG_DB`    | Path to log database.                        |
    !! | `DM_OBSERV_DB` | Path to observation database.                |
    !! | `DM_READ_ONLY` | Open databases in read-only mode (optional). |
    !! | `DM_TILE_URL`  | URL of map tiles.                            |
    !!
    !! The databases have to exist at start-up. Add the variables to the
    !! configuration file of your web server. In _lighttpd(1)_, for instance:
    !!
    !! ```lighttpd
    !!  # Pass the database paths through environment variables.
    !!  setenv.add-environment = (
    !!    "DM_BEAT_DB"   => "/var/dmpack/beat.sqlite",
    !!    "DM_IMAGE_DB"  => "/var/dmpack/image.sqlite",
    !!    "DM_IMAGE_DIR" => "/images",
    !!    "DM_LOG_DB"    => "/var/dmpack/log.sqlite",
    !!    "DM_OBSERV_DB" => "/var/dmpack/observ.sqlite",
    !!    "DM_READ_ONLY" => "0",
    !!    "DM_TILE_URL"  => "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
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
    integer, parameter :: APP_PATCH = 8

    ! Program parameters.
    character(len=*), parameter :: APP_BASE_PATH     = '/dmpack'          !! URI base path.
    character(len=*), parameter :: APP_CSS_PATH      = '/dmweb'           !! Path to CSS directory.
    character(len=*), parameter :: APP_JS_PATH       = APP_CSS_PATH       !! Path to JavaScript directory.
    character(len=*), parameter :: APP_TITLE         = 'DMPACK'           !! HTML title and heading.
    integer,          parameter :: APP_DB_TIMEOUT    = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.
    integer,          parameter :: APP_NROUTES       = 21                 !! Total number of routes.
    integer,          parameter :: APP_PLOT_TERMINAL = PLOT_TERMINAL_SVG  !! Plotting backend.
    logical,          parameter :: APP_READ_ONLY     = .false.            !! Default database access mode.
    real(kind=r8),    parameter :: APP_MAP_LAT       = 51.1642292_r8      !! Default map view latitude.
    real(kind=r8),    parameter :: APP_MAP_LON       = 10.4541194_r8      !! Default map view longitude.

    ! Global settings.
    character(len=FILE_PATH_LEN) :: beat_db   = ' ' ! Path to beat database.
    character(len=FILE_PATH_LEN) :: image_db  = ' ' ! Path to image database.
    character(len=FILE_PATH_LEN) :: image_dir = ' ' ! Path to image directory.
    character(len=FILE_PATH_LEN) :: log_db    = ' ' ! Path to log database.
    character(len=FILE_PATH_LEN) :: observ_db = ' ' ! Path to observation database.
    character(len=FILE_PATH_LEN) :: tile_url  = ' ' ! URL of map tile server.

    logical :: has_beat_db   = .false.              ! Beat database passed.
    logical :: has_image_db  = .false.              ! Image database passed.
    logical :: has_image_dir = .false.              ! Image directory passed.
    logical :: has_log_db    = .false.              ! Log database passed.
    logical :: has_observ_db = .false.              ! Observation database passed.
    logical :: has_tile_url  = .false.              ! Map tile URL passed.
    logical :: read_only     = APP_READ_ONLY        ! Open databases in read-only mode.

    type(cgi_route_type)  :: routes(APP_NROUTES)
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
        cgi_route_type('/image',   route_image),     &
        cgi_route_type('/images',  route_images),    &
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
        rc = dm_env_get('DM_BEAT_DB',   beat_db,   n, exists=has_beat_db)
        rc = dm_env_get('DM_IMAGE_DB',  image_db,  n, exists=has_image_db)
        rc = dm_env_get('DM_IMAGE_DIR', image_dir, n, exists=has_image_dir)
        rc = dm_env_get('DM_LOG_DB',    log_db,    n, exists=has_log_db)
        rc = dm_env_get('DM_OBSERV_DB', observ_db, n, exists=has_observ_db)
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
    ! **************************************************************************
    ! ENDPOINTS.
    ! **************************************************************************
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

        rc = dm_db_open(db, beat_db, read_only=.true., timeout=APP_DB_TIMEOUT)

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
                call html_error('Database Query Failed', error=rc)
                exit response_block
            end if

            if (rc == E_DB_DONE) then
                call html_error('Beat Not Found', error=E_NOT_FOUND)
                exit response_block
            end if

            delta = huge(0_i8)
            rc = dm_time_diff(beat%time_recv, dm_time_now(), delta)

            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))
            call dm_cgi_write(dm_html_beat(beat, delta, prefix=APP_BASE_PATH // '/node?id='))
            call html_footer()
        end block response_block

        call dm_db_close(db)
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

        rc = dm_db_open(db, beat_db, read_only=.true., timeout=APP_DB_TIMEOUT)

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
            call dm_cgi_write(dm_html_heading(1, TITLE))

            rc = dm_db_select_beats(db, beats, nbeats=n)

            if (n > 0) then
                allocate (deltas(n), source=huge(0_i8))
                now = dm_time_now()

                do i = 1, n
                    rc = dm_time_diff(beats(i)%time_recv, now, deltas(i))
                end do

                call dm_cgi_write(dm_html_beats(beats, deltas=deltas, prefix=APP_BASE_PATH // '/beat?node_id='))
            else
                call dm_cgi_write(dm_html_p('No beats found.'))
            end if

            call html_footer()
        end block response_block

        call dm_db_close(db)
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

        integer(kind=i8), parameter :: NBEATS   = 15 !! Max. number of beats to show.
        integer(kind=i8), parameter :: NIMAGES  = 15 !! Max. number of images to show.
        integer(kind=i8), parameter :: NLOGS    = 15 !! Max. number of logs to show.
        integer(kind=i8), parameter :: NOBSERVS = 15 !! Max. number of observations to show.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        call html_header(TITLE)
        call dm_cgi_write(dm_html_heading(1, TITLE))

        if (.not. has_beat_db .and. .not. has_log_db .and. .not. has_observ_db) then
            call dm_cgi_write(dm_html_p('No databases configured.'))
            call html_footer()
            return
        end if

        ! ------------------------------------------------------------------
        ! Heatbeats.
        ! ------------------------------------------------------------------
        beat_block: block
            character(len=TIME_LEN)       :: now
            integer(kind=i8)              :: i, n
            integer(kind=i8), allocatable :: deltas(:)
            type(beat_type),  allocatable :: beats(:)

            if (.not. has_beat_db) exit beat_block

            call dm_cgi_write(dm_html_heading(2, 'Beats', small='Last ' // dm_itoa(NBEATS) // ' Beats'))
            rc = dm_db_open(db, beat_db, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call dm_cgi_write(dm_html_p('Database connection failed.'))
                exit beat_block
            end if

            if (.not. dm_db_table_has_beats(db)) then
                call dm_cgi_write(dm_html_p('Database tables not found.'))
                exit beat_block
            end if

            rc = dm_db_select_beats(db, beats, limit=NBEATS, nbeats=n)

            if (dm_is_error(rc)) then
                call dm_cgi_write(dm_html_p('No beats found.'))
                exit beat_block
            end if

            allocate (deltas(n), source=huge(0_i8))
            now = dm_time_now()

            do i = 1, n
                rc = dm_time_diff(beats(i)%time_recv, now, deltas(i))
            end do

            call dm_cgi_write(dm_html_beats(beats, deltas=deltas, prefix=APP_BASE_PATH // '/beat?node_id='))
        end block beat_block

        call dm_db_close(db)

        ! ------------------------------------------------------------------
        ! Logs.
        ! ------------------------------------------------------------------
        log_block: block
            type(log_type), allocatable :: logs(:)

            if (.not. has_log_db) exit log_block

            call dm_cgi_write(dm_html_heading(2, 'Logs', small='Last ' // dm_itoa(NLOGS) // ' Logs'))
            rc = dm_db_open(db, log_db, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call dm_cgi_write(dm_html_p('Database connection failed.'))
                exit log_block
            end if

            if (.not. dm_db_table_has_logs(db)) then
                call dm_cgi_write(dm_html_p('Database tables not found.'))
                exit log_block
            end if

            rc = dm_db_select_logs(db, logs, limit=NLOGS, desc=.true.)

            if (dm_is_error(rc)) then
                call dm_cgi_write(dm_html_p('No logs found.'))
                exit log_block
            end if

            call dm_cgi_write(dm_html_logs(logs, prefix=APP_BASE_PATH // '/log?id=', max_len=32))
        end block log_block

        call dm_db_close(db)

        ! ------------------------------------------------------------------
        ! Observations.
        ! ------------------------------------------------------------------
        observ_block: block
            type(observ_type), allocatable :: observs(:)

            if (.not. has_observ_db) exit observ_block

            call dm_cgi_write(dm_html_heading(2, 'Observations', small='Last ' // dm_itoa(NOBSERVS) // ' Observations'))
            rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call dm_cgi_write(dm_html_p('Database connection failed.'))
                exit observ_block
            end if

            if (.not. dm_db_table_has_observs(db)) then
                call dm_cgi_write(dm_html_p('Database tables not found.'))
                exit observ_block
            end if

            rc = dm_db_select_observs(db, observs, desc=.true., limit=NOBSERVS, stub=.true.)

            if (dm_is_error(rc)) then
                call dm_cgi_write(dm_html_p('No observations found.'))
                exit observ_block
            end if

            call dm_cgi_write(dm_html_observs(observs, prefix=APP_BASE_PATH // '/observ?id=', &
                                              node_id=.true., sensor_id=.true., target_id=.true., &
                                              name=.true., source=.true., error=.true.))
        end block observ_block

        call dm_db_close(db)

        ! ------------------------------------------------------------------
        ! Images.
        ! ------------------------------------------------------------------
        image_block: block
            type(image_type), allocatable :: images(:)

            if (.not. has_image_db) exit image_block

            call dm_cgi_write(dm_html_heading(2, 'Images', small='Last ' // dm_itoa(NIMAGES) // ' Images'))
            rc = dm_db_open(db, image_db, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call dm_cgi_write(dm_html_p('Database connection failed.'))
                exit image_block
            end if

            if (.not. dm_db_table_has_images(db)) then
                call dm_cgi_write(dm_html_p('Database tables not found.'))
                exit image_block
            end if

            rc = dm_db_select_images(db, images, limit=NIMAGES, desc=.true.)

            if (dm_is_error(rc)) then
                call dm_cgi_write(dm_html_p('No images found.'))
                exit image_block
            end if

            call dm_cgi_write(dm_html_images(images, prefix=APP_BASE_PATH // '/image?id='))
        end block image_block

        call dm_db_close(db)

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
        call dm_cgi_write(dm_html_heading(1, TITLE))
        call dm_cgi_write(dm_html_cgi_env(env))
        call html_footer()
    end subroutine route_env

    subroutine route_image(env)
        !! Image page. The routine does not check whether the image file
        !! actually exists, as the environment variable `DOCUMENT_ROOT` does
        !! not include the path to the WWW root directory, at least on
        !! _lighttpd(1)_.
        !!
        !! ## Path
        !!
        !! * `/dmpack/image`
        !!
        !! ## Methods
        !!
        !! * GET
        !!
        !! ## GET Parameters
        !!
        !! * `id` – Image id (UUID).
        !!
        character(len=*), parameter :: TITLE = 'Image' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        type(db_type) :: db

        if (.not. has_image_db .or. .not. has_image_dir) then
            call html_error('no image database or directory configured', error=E_NOT_FOUND)
            return
        end if

        rc = dm_db_open(db, image_db, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        response_block: block
            character(len=IMAGE_ID_LEN) :: id
            type(cgi_param_type)        :: param
            type(image_type)            :: image
            type(transfer_type)         :: transfer

            call dm_cgi_query(env, param)
            rc = dm_cgi_get(param, 'id', id)

            if (dm_is_error(rc)) then
                call html_error('Missing Parameter', error=rc)
                exit response_block
            end if

            if (.not. dm_uuid4_is_valid(id)) then
                call html_error('Invalid Parameter', error=E_INVALID)
                exit response_block
            end if

            rc = dm_db_select(db, image, id)

            if (dm_is_error(rc)) then
                call html_error('Database Query Failed', error=rc)
                exit response_block
            end if

            if (rc == E_DB_DONE) then
                call html_error('Image Not Found', error=E_NOT_FOUND)
                exit response_block
            end if

            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))

            call dm_cgi_write(H_FIGURE)
            call dm_cgi_write(dm_html_img(src=dm_image_path(image, image_dir), alt=image%id))
            call dm_cgi_write(H_FIGURE_END)

            call dm_cgi_write(dm_html_image(image, prefix_node  =APP_BASE_PATH // '/node?id=', &
                                                   prefix_sensor=APP_BASE_PATH // '/sensor?id=', &
                                                   prefix_target=APP_BASE_PATH // '/target?id='))

            if (dm_db_table_has_transfers(db)) then
                rc = dm_db_select_transfer(db, transfer, type_id=image%id)

                if (rc == E_NONE) then
                    call dm_cgi_write(H_DETAILS // H_SUMMARY // 'Transfer' // H_SUMMARY_END)
                    call dm_cgi_write(dm_html_transfer(transfer, prefix_node=APP_BASE_PATH // '/node?id='))
                    call dm_cgi_write(H_DETAILS_END)
                end if
            end if

            call html_footer()
        end block response_block

        call dm_db_close(db)
    end subroutine route_image

    subroutine route_images(env)
        !! Images page.
        !!
        !! ## Path
        !!
        !! * `/dmpack/images`
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
        !! * `max_results` – Maximum number of images (integer).
        !!
        character(len=*), parameter :: TITLE = 'Images' !! Page title.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        type(db_type) :: db

        response_block: block
            character(len=NODE_ID_LEN)   :: node_id
            character(len=SENSOR_ID_LEN) :: sensor_id
            character(len=TARGET_ID_LEN) :: target_id
            character(len=TIME_LEN)      :: from, to

            integer          :: max_results(5), nresults, rc
            integer(kind=i8) :: nimages
            logical          :: valid

            type(cgi_param_type)           :: param
            type(image_type),  allocatable :: images(:)
            type(node_type),   allocatable :: nodes(:)
            type(sensor_type), allocatable :: sensors(:)
            type(target_type), allocatable :: targets(:)

            max_results = [ 25, 50, 100, 250, 500 ]

            rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call html_error('Database Connection Failed', error=rc)
                return
            end if

            rc = dm_db_select_nodes  (db, nodes)
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
                valid = .false.

                validate_block: block
                    integer :: rcs(3)

                    ! Mandatory parameters.
                    rcs(1) = dm_cgi_get(param, 'from',        from)
                    rcs(2) = dm_cgi_get(param, 'to',          to)
                    rcs(3) = dm_cgi_get(param, 'max_results', nresults)

                    if (any(dm_is_error(rcs))) exit validate_block

                    ! Timestamps.
                    if (.not. dm_time_is_valid(from) .or. .not. dm_time_is_valid(to)) exit validate_block

                    ! Optional parameters.
                    rcs(1) = dm_cgi_get(param, 'node_id',   node_id)
                    rcs(2) = dm_cgi_get(param, 'sensor_id', sensor_id)
                    rcs(3) = dm_cgi_get(param, 'target_id', target_id)

                    if (dm_is_ok(rcs(1)) .and. .not. dm_id_is_valid(node_id))   exit validate_block
                    if (dm_is_ok(rcs(2)) .and. .not. dm_id_is_valid(sensor_id)) exit validate_block
                    if (dm_is_ok(rcs(3)) .and. .not. dm_id_is_valid(target_id)) exit validate_block

                    ! Number of results.
                    if (.not. dm_array_has(max_results, nresults)) exit validate_block

                    valid = .true.
                end block validate_block

                if (.not. valid) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                call dm_db_close(db)

                ! Open image database.
                rc = dm_db_open(db, image_db, read_only=.true., timeout=APP_DB_TIMEOUT)

                if (dm_is_error(rc)) then
                    call html_error('Database Connection Failed', error=rc)
                    return
                end if

                rc = dm_db_select_images(db, images, node_id=node_id, sensor_id=sensor_id, target_id=target_id, &
                                         from=from, to=to, limit=int(nresults, kind=i8), nimages=nimages)

                if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                    call html_error('Database Query Failed', error=rc, extra=dm_db_error_message(db))
                    exit response_block
                end if

                call html_header(TITLE)
                call dm_cgi_write(dm_html_heading(1, TITLE))
                call dm_cgi_write(html_form_images(nodes, sensors, targets, max_results, node_id, sensor_id, target_id, from, to, nresults))

                if (nimages > 0) then
                    call dm_cgi_write(dm_html_images(images, prefix=APP_BASE_PATH // '/image?id='))
                else
                    call dm_cgi_write(dm_html_p('No images found.'))
                end if

                call html_footer()
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))
            call dm_cgi_write(html_form_images(nodes, sensors, targets, max_results))
            call html_footer()
        end block response_block

        call dm_db_close(db)
    end subroutine route_images

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
        character(len=*), parameter :: TITLE   = 'Licence' !! Page title.
        character(len=*), parameter :: LICENCE = &
            H_P // 'Permission to use, copy, modify, and/or distribute this '   // &
            'software for any purpose with or without fee is hereby '           // &
            'granted, provided that the above copyright notice and this '       // &
            'permission notice appear in all copies.' // H_P_END                // &
            H_P // 'THE SOFTWARE IS PROVIDED &quot;AS IS&quot; AND THE AUTHOR ' // &
            'DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING '  // &
            'ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO '     // &
            'EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, '        // &
            'INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER '     // &
            'RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN '       // &
            'ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, '         // &
            'ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE '      // &
            'OF THIS SOFTWARE.' // H_P_END

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        call html_header(TITLE)
        call dm_cgi_write(dm_html_heading(1, TITLE))
        call dm_cgi_write(H_BLOCKQUOTE)
        call dm_cgi_write(dm_html_p(DM_COPYRIGHT, encode=.true.))
        call dm_cgi_write(LICENCE)
        call dm_cgi_write(H_BLOCKQUOTE_END)
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

        integer       :: rc
        type(db_type) :: db

        rc = dm_db_open(db, log_db, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        response_block: block
            character(len=LOG_ID_LEN) :: id
            type(log_type)            :: log
            type(cgi_param_type)      :: param

            call dm_cgi_query(env, param)
            rc = dm_cgi_get(param, 'id', id)

            if (dm_is_error(rc)) then
                call html_error('Missing Parameter', error=rc)
                exit response_block
            end if

            if (.not. dm_uuid4_is_valid(id)) then
                call html_error('Invalid Parameter', error=E_INVALID)
                exit response_block
            end if

            rc = dm_db_select(db, log, id)

            if (dm_is_error(rc)) then
                call html_error('Database Query Failed', error=rc)
                exit response_block
            end if

            if (rc == E_DB_DONE) then
                call html_error('Log Not Found', error=E_NOT_FOUND)
                exit response_block
            end if

            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))
            call dm_cgi_write(dm_html_log(log, prefix_node   = APP_BASE_PATH // '/node?id=', &
                                               prefix_sensor = APP_BASE_PATH // '/sensor?id=', &
                                               prefix_target = APP_BASE_PATH // '/target?id=', &
                                               prefix_observ = APP_BASE_PATH // '/observ?id='))
            call html_footer()
        end block response_block

        call dm_db_close(db)
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

        integer       :: rc
        type(db_type) :: db

        response_block: block
            character(len=NODE_ID_LEN)    :: node_id
            character(len=SENSOR_ID_LEN)  :: sensor_id
            character(len=TARGET_ID_LEN)  :: target_id
            character(len=LOG_SOURCE_LEN) :: source
            character(len=TIME_LEN)       :: from, to

            integer          :: level, max_results(5), nresults
            logical          :: has_level, valid
            integer(kind=i8) :: nlogs

            type(cgi_param_type)           :: param
            type(log_type),    allocatable :: logs(:)
            type(node_type),   allocatable :: nodes(:)
            type(sensor_type), allocatable :: sensors(:)
            type(target_type), allocatable :: targets(:)

            max_results = [ 25, 50, 100, 250, 500 ]

            rc = dm_db_open(db, observ_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call html_error('Database Connection Failed', error=rc)
                return
            end if

            rc = dm_db_select_nodes  (db, nodes)
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
                valid = .false.

                validate_block: block
                    integer :: rcs(3)

                    ! Mandatory parameters.
                    rcs(1) = dm_cgi_get(param, 'from',        from)
                    rcs(2) = dm_cgi_get(param, 'to',          to)
                    rcs(3) = dm_cgi_get(param, 'max_results', nresults)

                    if (any(dm_is_error(rcs))) exit validate_block

                    ! Timestamps.
                    if (.not. dm_time_is_valid(from) .or. .not. dm_time_is_valid(to)) exit validate_block

                    ! Optional parameters.
                    rcs(1) = dm_cgi_get(param, 'node_id',   node_id)
                    rcs(2) = dm_cgi_get(param, 'sensor_id', sensor_id)
                    rcs(3) = dm_cgi_get(param, 'target_id', target_id)

                    if (dm_is_ok(rcs(1)) .and. .not. dm_id_is_valid(node_id))   exit validate_block
                    if (dm_is_ok(rcs(2)) .and. .not. dm_id_is_valid(sensor_id)) exit validate_block
                    if (dm_is_ok(rcs(3)) .and. .not. dm_id_is_valid(target_id)) exit validate_block

                    ! Number of results.
                    if (.not. dm_array_has(max_results, nresults)) exit validate_block

                    valid = .true.
                end block validate_block

                if (.not. valid) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Log level.
                has_level = dm_is_ok(dm_cgi_get(param, 'level', level))

                ! Log source.
                rc = dm_cgi_get(param, 'source', source)

                ! Open log database.
                call dm_db_close(db)
                rc = dm_db_open(db, log_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

                if (dm_is_error(rc)) then
                    call html_error('Database Connection Failed', error=rc)
                    return
                end if

                if (has_level) then
                    rc = dm_db_select_logs(db, logs, node_id=node_id, sensor_id=sensor_id, target_id=target_id, &
                                           source=source, from=from, to=to, min_level=level, max_level=level, &
                                           limit=int(nresults, kind=i8), nlogs=nlogs)
                else
                    rc = dm_db_select_logs(db, logs, node_id=node_id, sensor_id=sensor_id, target_id=target_id, &
                                           source=source, from=from, to=to, limit=int(nresults, kind=i8), nlogs=nlogs)
                end if

                if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                    call html_error('Database Query Failed', error=rc, extra=dm_db_error_message(db))
                    exit response_block
                end if

                call html_header(TITLE)
                call dm_cgi_write(dm_html_heading(1, TITLE))

                if (has_level) then
                    call dm_cgi_write(html_form_logs(nodes, sensors, targets, max_results, &
                                                     node_id, sensor_id, target_id, source, &
                                                     from, to, level, nresults=nresults))
                else
                    call dm_cgi_write(html_form_logs(nodes, sensors, targets, max_results, &
                                                     node_id, sensor_id, target_id, source, &
                                                     from, to, nresults=nresults))
                end if

                if (nlogs > 0) then
                    call dm_cgi_write(dm_html_logs(logs, prefix=APP_BASE_PATH // '/log?id=', max_len=32))
                else
                    call dm_cgi_write(dm_html_p('No logs found.'))
                end if

                call html_footer()
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))
            call dm_cgi_write(html_form_logs(nodes, sensors, targets, max_results))
            call html_footer()
        end block response_block

        call dm_db_close(db)
    end subroutine route_logs

    subroutine route_map(env)
        !! Shows map of node, sensor, and target positions. The environment
        !! variable `DM_TILE_URL` must be set.
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
        integer,          parameter :: ZOOM_LEVEL = 5                                  !! Map zoom level.

        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        integer       :: rc
        real(kind=r8) :: lon, lat

        character(len=:),  allocatable :: geojson
        type(node_type),   allocatable :: nodes(:)
        type(sensor_type), allocatable :: sensors(:)
        type(target_type), allocatable :: targets(:)

        ! ------------------------------------------------------------------
        ! GET REQUEST.
        ! ------------------------------------------------------------------
        if (.not. dm_string_has(tile_url)) then
            call html_error('Missing Environment Variable', error=E_EMPTY)
            return
        end if

        ! Read nodes, sensors, targets from database.
        db_block: block
            type(db_type) :: db

            rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call html_error('Database Connection Failed', error=rc)
                return
            end if

            rc = dm_db_select_nodes  (db, nodes)
            rc = dm_db_select_sensors(db, sensors)
            rc = dm_db_select_targets(db, targets)

            call dm_db_close(db)
        end block db_block

        ! Map view coordinates.
        lon = 0.0_r8
        lat = 0.0_r8

        ! Select view point.
        if (size(nodes) > 0) then
            lon = nodes(1)%longitude
            lat = nodes(1)%latitude
        else if (size(sensors) > 0) then
            lon = sensors(1)%longitude
            lat = sensors(1)%latitude
        else if (size(targets) > 0) then
            lon = targets(1)%longitude
            lat = targets(1)%latitude
        end if

        ! Use default coordinates for view point.
        if (dm_equals(lon, 0.0_r8) .and. dm_equals(lat, 0.0_r8)) then
            lon = APP_MAP_LON
            lat = APP_MAP_LAT
        end if

        ! Create GeoJSON feature collection.
        geojson_block: block
            integer                               :: i, n
            type(geojson_feature_collection_type) :: collection

            n  = size(nodes) + size(sensors) + size(targets)
            rc = dm_geojson_feature_collection_create(collection, n)

            do i = 1, size(nodes)
                rc = dm_geojson_feature_collection_add(collection, nodes(i))
            end do

            do i = 1, size(sensors)
                rc = dm_geojson_feature_collection_add(collection, sensors(i))
            end do

            do i = 1, size(targets)
                rc = dm_geojson_feature_collection_add(collection, targets(i))
            end do

            geojson = dm_geojson_feature_collection(collection)
            call dm_geojson_feature_collection_destroy(collection)
        end block geojson_block

        ! Output page header.
        call html_header(TITLE, style=STYLE)
        call dm_cgi_write(dm_html_heading(1, TITLE))

        ! Output map element and scripts.
        call dm_cgi_write(dm_html_div(id=MAP_ID, close=.true.))
        call dm_cgi_write(dm_html_script(JS_LEAFLET))
        call dm_cgi_write(dm_html_script(JS_DMPACK))

        ! Output inline script to create Leaflet map.
        call dm_cgi_write(H_SCRIPT)
        call dm_cgi_write(dm_js_const('id', MAP_ID))
        call dm_cgi_write(dm_js_const('url', tile_url))
        call dm_cgi_write(dm_js_const('lon', lon))
        call dm_cgi_write(dm_js_const('lat', lat))
        call dm_cgi_write(dm_js_const('zoom', ZOOM_LEVEL))
        call dm_cgi_write(dm_js_const('geoJson', geojson, quote=.false.))
        call dm_cgi_write('createMap(id, url, lon, lat, zoom, geoJson);')
        call dm_cgi_write(H_SCRIPT_END)

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

        rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

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
                call html_error('Database Query Failed', error=rc)
                exit response_block
            end if

            if (rc == E_DB_DONE) then
                call html_error('Node Not Found', error=E_NOT_FOUND)
                exit response_block
            end if

            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))
            call dm_cgi_write(dm_html_node(node))
            call html_footer()
        end block response_block

        call dm_db_close(db)
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

        rc = dm_db_open(db, observ_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        response_block: block
            integer                      :: rcs(2)
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
                rcs(1) = dm_cgi_get(param, 'id',   node%id)
                rcs(2) = dm_cgi_get(param, 'name', node%name)

                if (any(dm_is_error(rcs))) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Optional parameters.
                rc = dm_cgi_get(param, 'meta',      node%meta)
                rc = dm_cgi_get(param, 'x',         node%x)
                rc = dm_cgi_get(param, 'y',         node%y)
                rc = dm_cgi_get(param, 'z',         node%z)
                rc = dm_cgi_get(param, 'longitude', node%longitude)
                rc = dm_cgi_get(param, 'latitude',  node%latitude)
                rc = dm_cgi_get(param, 'elevation', node%elevation)

                ! Validate node data.
                if (.not. dm_node_is_valid(node)) then
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
            call dm_cgi_write(dm_html_heading(1, TITLE))

            rc = dm_db_select_nodes(db, nodes)

            if (size(nodes) > 0) then
                call dm_cgi_write(dm_html_nodes(nodes, prefix=APP_BASE_PATH // '/node?id='))
            else
                call dm_cgi_write(dm_html_p('No nodes found.'))
            end if

            call dm_cgi_write(html_form_nodes(disabled=read_only))
            call html_footer()
        end block response_block

        call dm_db_close(db)
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

        if (.not. dm_uuid4_is_valid(id)) then
            call html_error('Invalid Parameter', error=E_INVALID)
            return
        end if

        rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

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
                call html_error('Database Query Failed', error=rc)
                exit response_block
            end if

            if (rc == E_DB_DONE) then
                call html_error('Observation Not Found', error=E_NOT_FOUND)
                exit response_block
            end if

            ! Get associated logs from database.
            call dm_db_close(db)
            rc = dm_db_open(db, log_db, read_only=.true., timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call html_error('Database Connection Failed', error=rc)
                return
            end if

            rc = dm_db_select_logs(db, logs, observ_id=id, nlogs=nlogs)

            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))
            call dm_cgi_write(dm_html_observ(observ, prefix_node  =APP_BASE_PATH // '/node?id=', &
                                                     prefix_sensor=APP_BASE_PATH // '/sensor?id=', &
                                                     prefix_target=APP_BASE_PATH // '/target?id='))
            call dm_cgi_write(dm_html_heading(2, 'Logs'))

            if (nlogs > 0) then
                call dm_cgi_write(dm_html_logs(logs, prefix=APP_BASE_PATH // '/log?id=', max_len=32))
            else
                call dm_cgi_write(dm_html_p('No associated logs found.'))
            end if

            call html_footer()
        end block response_block

        call dm_db_close(db)
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

        rc = dm_db_open(db, observ_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        response_block: block
            character(len=NODE_ID_LEN)   :: node_id
            character(len=SENSOR_ID_LEN) :: sensor_id
            character(len=TARGET_ID_LEN) :: target_id
            character(len=TIME_LEN)      :: from, to
            integer                      :: max_results(6), nresults, rcs(6)
            integer(kind=i8)             :: nobservs
            logical                      :: valid
            type(cgi_param_type)         :: param

            type(node_type),   allocatable :: nodes(:)
            type(observ_type), allocatable :: observs(:)
            type(sensor_type), allocatable :: sensors(:)
            type(target_type), allocatable :: targets(:)

            max_results = [ 25, 50, 100, 250, 500, 1000 ]

            rc = dm_db_select_nodes  (db, nodes)
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
                rcs(1) = dm_cgi_get(param, 'node_id',     node_id)
                rcs(2) = dm_cgi_get(param, 'sensor_id',   sensor_id)
                rcs(3) = dm_cgi_get(param, 'target_id',   target_id)
                rcs(4) = dm_cgi_get(param, 'from',        from)
                rcs(5) = dm_cgi_get(param, 'to',          to)
                rcs(6) = dm_cgi_get(param, 'max_results', nresults)

                if (any(dm_is_error(rcs))) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Validate parameters.
                valid = (dm_id_is_valid(node_id)   .and. &
                         dm_id_is_valid(sensor_id) .and. &
                         dm_id_is_valid(target_id) .and. &
                         dm_time_is_valid(from)    .and. &
                         dm_time_is_valid(to)      .and. &
                         dm_array_has(MAX_RESULTS, nresults))

                if (.not. valid) then
                    call html_error('Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Get observation stubs.
                rc = dm_db_select_observs(db, observs, node_id=node_id, sensor_id=sensor_id, target_id=target_id, &
                                          from=from, to=to, limit=int(nresults, kind=i8), stub=.true., nobservs=nobservs)

                if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                    call html_error('Database Query Failed', error=rc, extra=dm_db_error_message(db))
                    exit response_block
                end if

                ! Output table.
                call html_header(TITLE)
                call dm_cgi_write(dm_html_heading(1, TITLE))
                call dm_cgi_write(html_form_observs(nodes, sensors, targets, max_results, node_id, &
                                                    sensor_id, target_id, from, to, nresults))

                if (nobservs == 0) then
                    call dm_cgi_write(dm_html_p('No observations found.'))
                else
                    call dm_cgi_write(dm_html_observs(observs, prefix=APP_BASE_PATH // '/observ?id=', &
                                                      id=.true., name=.true., source=.true., error=.true.))
                end if

                call html_footer()
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))
            call dm_cgi_write(html_form_observs(nodes, sensors, targets, MAX_RESULTS))

            if (size(nodes) == 0) then
                call dm_cgi_write(dm_html_p('No nodes found.'))
            else if (size(sensors) == 0) then
                call dm_cgi_write(dm_html_p('No sensors found.'))
            else if (size(targets) == 0) then
                call dm_cgi_write(dm_html_p('No targets found.'))
            end if

            call html_footer()
        end block response_block

        call dm_db_close(db)
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

        rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        response_block: block
            character(len=:), allocatable    :: error, output
            character(len=NODE_ID_LEN)       :: node_id
            character(len=SENSOR_ID_LEN)     :: sensor_id
            character(len=TARGET_ID_LEN)     :: target_id
            character(len=RESPONSE_NAME_LEN) :: response_name
            character(len=TIME_LEN)          :: from, to
            integer                          :: max_results(7), nresults, rcs(7)
            integer(kind=i8)                 :: n, ndps
            logical                          :: valid
            type(cgi_param_type)             :: param
            type(plot_type)                  :: plot

            type(dp_type),     allocatable :: data_points(:)
            type(node_type),   allocatable :: nodes(:)
            type(sensor_type), allocatable :: sensors(:)
            type(target_type), allocatable :: targets(:)

            max_results = [ 5, 25, 50, 100, 250, 500, 1000 ]

            rc = dm_db_select_nodes  (db, nodes)
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
                rcs(1) = dm_cgi_get(param, 'node_id',       node_id)
                rcs(2) = dm_cgi_get(param, 'sensor_id',     sensor_id)
                rcs(3) = dm_cgi_get(param, 'target_id',     target_id)
                rcs(4) = dm_cgi_get(param, 'response_name', response_name)
                rcs(5) = dm_cgi_get(param, 'from',          from)
                rcs(6) = dm_cgi_get(param, 'to',            to)
                rcs(7) = dm_cgi_get(param, 'max_results',   nresults)

                if (any(dm_is_error(rcs))) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Validate parameters.
                valid = (dm_id_is_valid(node_id)       .and. &
                         dm_id_is_valid(sensor_id)     .and. &
                         dm_id_is_valid(target_id)     .and. &
                         dm_id_is_valid(response_name) .and. &
                         dm_time_is_valid(from)        .and. &
                         dm_time_is_valid(to)          .and. &
                         dm_array_has(max_results, nresults))

                if (.not. valid) then
                    call html_error('Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Output plot.
                call html_header(TITLE)
                call dm_cgi_write(dm_html_heading(1, TITLE))
                call dm_cgi_write(html_form_plots(nodes, sensors, targets, max_results, node_id, sensor_id, &
                                                  target_id, response_name, from, to, nresults))

                ! Get time series.
                rc = dm_db_select_data_points(db, data_points, node_id, sensor_id, target_id, response_name, &
                                              from, to, limit=int(nresults, kind=i8), ndps=ndps)

                if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                    call dm_cgi_write(dm_html_p('Database query failed.'))
                    call html_footer()
                    exit response_block
                end if

                if (rc == E_DB_NO_ROWS .or. ndps == 0) then
                    call dm_cgi_write(dm_html_p('No observations found.'))
                    call html_footer()
                    exit response_block
                end if

                ! Plotting via Gnuplot.
                call dm_plot_set(plot    = plot,               &
                                 terminal = APP_PLOT_TERMINAL, &
                                 font     = 'sans',            &
                                 graph    = '#ffffff',         &
                                 bidirect = .true.,            &
                                 width    = PLOT_WIDTH,        &
                                 height   = PLOT_HEIGHT,       &
                                 xlabel   = 'Time',            &
                                 ylabel   = response_name)

                rc = dm_plot_lines(plot, data_points)
                rc = dm_plot_error(plot, error, n)

                if (n > 0) call dm_cgi_write(dm_html_pre(dm_html_encode(error), code=.true.))

                rc = dm_plot_read(plot, output)

                if (dm_is_error(rc)) then
                    call dm_cgi_write(dm_html_p('Failed to execute plotting backend.'))
                    call html_footer()
                    exit response_block
                end if

                ! Output HTML image with base64-encoded data URI.
                call dm_cgi_write(H_FIGURE)
                call dm_cgi_write(dm_html_img(src=dm_html_data_uri(output, MIME_SVG), alt='SVG'))
                call dm_cgi_write(H_FIGURE_END)

                call html_footer()
                exit response_block
            end if

            ! ------------------------------------------------------------------
            ! GET REQUEST.
            ! ------------------------------------------------------------------
            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))
            call dm_cgi_write(html_form_plots(nodes, sensors, targets, max_results))

            if (size(nodes) == 0) then
                call dm_cgi_write(dm_html_p('No nodes found.'))
            else if (size(sensors) == 0) then
                call dm_cgi_write(dm_html_p('No sensors found.'))
            else if (size(targets) == 0) then
                call dm_cgi_write(dm_html_p('No targets found.'))
            end if

            call html_footer()
        end block response_block

        call dm_db_close(db)
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

        rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

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

            if (.not. dm_id_is_valid(id)) then
                call html_error('Invalid Parameter', error=E_INVALID)
                exit response_block
            end if

            rc = dm_db_select(db, sensor, id)

            if (dm_is_error(rc)) then
                call html_error('Database Query Failed', error=rc)
                exit response_block
            end if

            if (rc == E_DB_DONE) then
                call html_error('Sensor Not Found', error=E_NOT_FOUND)
                exit response_block
            end if

            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))
            call dm_cgi_write(dm_html_sensor(sensor))
            call html_footer()
        end block response_block

        call dm_db_close(db)
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

        rc = dm_db_open(db, observ_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        response_block: block
            integer                        :: rcs(4)
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
                rcs(1) = dm_cgi_get(param, 'id',      sensor%id)
                rcs(2) = dm_cgi_get(param, 'node_id', sensor%node_id)
                rcs(3) = dm_cgi_get(param, 'type',    sensor%type)
                rcs(4) = dm_cgi_get(param, 'name',    sensor%name)

                if (any(dm_is_error(rcs))) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                rc = dm_cgi_get(param, 'sn',        sensor%sn)
                rc = dm_cgi_get(param, 'meta',      sensor%meta)
                rc = dm_cgi_get(param, 'x',         sensor%x)
                rc = dm_cgi_get(param, 'y',         sensor%y)
                rc = dm_cgi_get(param, 'z',         sensor%z)
                rc = dm_cgi_get(param, 'longitude', sensor%longitude)
                rc = dm_cgi_get(param, 'latitude',  sensor%latitude)
                rc = dm_cgi_get(param, 'elevation', sensor%elevation)

                ! Validate sensor data.
                if (.not. dm_sensor_is_valid(sensor)) then
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
            call dm_cgi_write(dm_html_heading(1, TITLE))

            rc = dm_db_select_sensors(db, sensors)

            if (size(sensors) > 0) then
                call dm_cgi_write(dm_html_sensors(sensors, prefix=APP_BASE_PATH // '/sensor?id='))
            else
                call dm_cgi_write(dm_html_p('No sensors found.'))
            end if

            rc = dm_db_select_nodes(db, nodes)

            if (size(nodes) > 0) then
                call dm_cgi_write(html_form_sensors(nodes, disabled=read_only))
            else
                call dm_cgi_write(dm_html_p('At least one node is required to add a sensor.'))
            end if

            call html_footer()
        end block response_block

        call dm_db_close(db)
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
        call dm_cgi_write(dm_html_heading(1, TITLE))

        ! System status.
        system_block: block
            character(len=:), allocatable :: content
            integer(kind=i8)              :: seconds
            type(uname_type)              :: uname
            type(time_delta_type)         :: uptime

            call dm_system_uname(uname)
            call dm_system_uptime(seconds)
            call dm_time_delta_from_seconds(uptime, seconds)

            content = H_TABLE // H_TBODY // &
                      H_TR // H_TH // 'Host Name'                               // H_TH_END // &
                              H_TD // dm_html_encode(uname%node_name)           // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Server Time'                             // H_TH_END // &
                              H_TD // dm_html_time(dm_time_now(), human=.true.) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Server Uptime'                           // H_TH_END // &
                              H_TD // dm_time_delta_to_string(uptime)           // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'OS Name'                                 // H_TH_END // &
                              H_TD // dm_html_encode(uname%system_name)         // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'OS Release'                              // H_TH_END // &
                              H_TD // dm_html_encode(uname%release)             // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'OS Version'                              // H_TH_END // &
                              H_TD // dm_html_encode(uname%version)             // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'OS Platform'                             // H_TH_END // &
                              H_TD // dm_html_encode(uname%machine)             // H_TD_END // H_TR_END // &
                      H_TBODY_END // H_TABLE_END

            call dm_cgi_write(dm_html_heading(2, 'System'))
            call dm_cgi_write(content)
        end block system_block

        ! DMPACK status.
        dmpack_block: block
            character(len=:), allocatable :: content
            character(len=FILE_PATH_LEN)  :: path

            call dm_system_path(path)

            content = H_TABLE // H_TBODY // &
                      H_TR // H_TH // 'Executable Path'                                     // H_TH_END // &
                              H_TD // dm_html_encode(path)                                  // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Executable Version'                                  // H_TH_END // &
                              H_TD // dm_version_to_string(APP_MAJOR, APP_MINOR, APP_PATCH) // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'DMPACK Version'                                      // H_TH_END // &
                              H_TD // DM_VERSION_STRING // ' (' // DM_BUILD_DATE // ')'     // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'SQLite Version'                                      // H_TH_END // &
                              H_TD // dm_db_version()                                       // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Compiler'                                            // H_TH_END // &
                              H_TD // dm_html_encode(compiler_version())                    // H_TD_END // H_TR_END // &
                      H_TR // H_TH // 'Compiler Options'                                    // H_TH_END // &
                              H_TD // dm_html_encode(compiler_options())                    // H_TD_END // H_TR_END // &
                      H_TBODY_END // H_TABLE_END

            call dm_cgi_write(dm_html_heading(2, 'DMPACK'))
            call dm_cgi_write(content)
        end block dmpack_block

        ! Database status.
        db_block: block
            character(len=:), allocatable :: content
            character(len=:), allocatable :: mode

            integer(kind=i8) :: nbyte
            logical          :: beat_db_exists, image_db_exists, log_db_exists, observ_db_exists

            beat_db_exists   = dm_file_exists(beat_db)
            image_db_exists  = dm_file_exists(image_db)
            log_db_exists    = dm_file_exists(log_db)
            observ_db_exists = dm_file_exists(observ_db)

            if (.not. beat_db_exists  .and. &
                .not. image_db_exists .and. &
                .not. log_db_exists   .and. &
                .not. observ_db_exists) exit db_block

            mode = dm_btoa(read_only, 'yes', 'no')

            content = H_TABLE // H_THEAD // &
                      H_TR // &
                      H_TH // 'Type'      // H_TH_END // &
                      H_TH // 'Path'      // H_TH_END // &
                      H_TH // 'Size'      // H_TH_END // &
                      H_TH // 'Read-Only' // H_TH_END // &
                      H_TR_END // &
                      H_THEAD_END // H_TBODY

            if (beat_db_exists) then
                nbyte = dm_file_size(beat_db)
                content = content // H_TR // &
                                     H_TD // 'Beat'                           // H_TD_END // &
                                     H_TD // dm_html_encode(beat_db)          // H_TD_END // &
                                     H_TD // dm_size_to_human(nbyte)          // H_TD_END // &
                                     H_TD // dm_html_mark(mode, class='info') // H_TD_END // &
                                     H_TR_END
            end if

            if (image_db_exists) then
                nbyte = dm_file_size(image_db)
                content = content // H_TR // &
                                     H_TD // 'Image'                          // H_TD_END // &
                                     H_TD // dm_html_encode(image_db)         // H_TD_END // &
                                     H_TD // dm_size_to_human(nbyte)          // H_TD_END // &
                                     H_TD // dm_html_mark(mode, class='info') // H_TD_END // &
                                     H_TR_END
            end if

            if (log_db_exists) then
                nbyte = dm_file_size(log_db)
                content = content // H_TR // &
                                     H_TD // 'Log'                            // H_TD_END // &
                                     H_TD // dm_html_encode(log_db)           // H_TD_END // &
                                     H_TD // dm_size_to_human(nbyte)          // H_TD_END // &
                                     H_TD // dm_html_mark(mode, class='info') // H_TD_END // &
                                     H_TR_END
            end if

            if (observ_db_exists) then
                nbyte = dm_file_size(observ_db)
                content = content // H_TR // &
                                     H_TD // 'Observation'                    // H_TD_END // &
                                     H_TD // dm_html_encode(observ_db)        // H_TD_END // &
                                     H_TD // dm_size_to_human(nbyte)          // H_TD_END // &
                                     H_TD // dm_html_mark(mode, class='info') // H_TD_END // &
                                     H_TR_END
            end if

            content = content // H_TBODY_END // H_TABLE_END

            call dm_cgi_write(dm_html_heading(2, 'Databases'))
            call dm_cgi_write(content)
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

        rc = dm_db_open(db, observ_db, read_only=.true., timeout=APP_DB_TIMEOUT)

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

            if (.not. dm_id_is_valid(id)) then
                call html_error('Invalid Parameter', error=E_INVALID)
                exit response_block
            end if

            rc = dm_db_select(db, target, id)

            if (dm_is_error(rc)) then
                call html_error('Database Query Failed', error=rc)
                exit response_block
            end if

            if (rc == E_DB_DONE) then
                call html_error('Target Not Found', error=E_NOT_FOUND)
                exit response_block
            end if

            call html_header(TITLE)
            call dm_cgi_write(dm_html_heading(1, TITLE))
            call dm_cgi_write(dm_html_target(target))
            call html_footer()
        end block response_block

        call dm_db_close(db)
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

        rc = dm_db_open(db, observ_db, read_only=read_only, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call html_error('Database Connection Failed', error=rc)
            return
        end if

        response_block: block
            integer                        :: rcs(2)
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
                rcs(1) = dm_cgi_get(param, 'id',   target%id)
                rcs(2) = dm_cgi_get(param, 'name', target%name)

                if (any(dm_is_error(rcs))) then
                    call html_error('Missing or Invalid Parameters', error=E_INVALID)
                    exit response_block
                end if

                ! Invalid state, x, y, and z are replaced with the default values.
                rc = dm_cgi_get(param, 'meta',      target%meta)
                rc = dm_cgi_get(param, 'state',     target%state)
                rc = dm_cgi_get(param, 'x',         target%x)
                rc = dm_cgi_get(param, 'y',         target%y)
                rc = dm_cgi_get(param, 'z',         target%z)
                rc = dm_cgi_get(param, 'longitude', target%longitude)
                rc = dm_cgi_get(param, 'latitude',  target%latitude)
                rc = dm_cgi_get(param, 'elevation', target%elevation)

                ! Validate target data.
                if (.not. dm_target_is_valid(target)) then
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
            call dm_cgi_write(dm_html_heading(1, TITLE))

            rc = dm_db_select_targets(db, targets)

            if (size(targets) > 0) then
                call dm_cgi_write(dm_html_targets(targets, prefix=APP_BASE_PATH // '/target?id='))
            else
                call dm_cgi_write(dm_html_p('No targets found.'))
            end if

            call dm_cgi_write(html_form_targets(disabled=read_only))
            call html_footer()
        end block response_block

        call dm_db_close(db)
    end subroutine route_targets

    ! **************************************************************************
    ! HTML FORM GENERATORS.
    ! **************************************************************************
    function html_form_images(nodes, sensors, targets, max_results, node_id, sensor_id, target_id, &
                              from, to, nresults) result(html)
        !! Returns HTML form for image selection.
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

        integer :: nresults_
        integer :: i

        character(len=4) :: year
        character(len=2) :: month
        character(len=2) :: day

        type(select_type) :: select_node
        type(select_type) :: select_sensor
        type(select_type) :: select_target
        type(select_type) :: select_result

        call dm_time_strings(year, month, day)

        node_id_   = ' '
        sensor_id_ = ' '
        target_id_ = ' '
        from_      = year // '-' // month // '-' // day // 'T00:00:00'
        to_        = '2100-01-01T00:00:00'
        nresults_  = 0

        if (present(node_id))   node_id_   = dm_html_encode(node_id)
        if (present(sensor_id)) sensor_id_ = dm_html_encode(sensor_id)
        if (present(target_id)) target_id_ = dm_html_encode(target_id)
        if (present(from))      from_      = dm_html_encode(from)
        if (present(to))        to_        = dm_html_encode(to)
        if (present(nresults))  nresults_  = nresults

        ! Create HTML select elements for form. Add 1 due to empty element.
        call dm_html_select_create(select_node,   1 + size(nodes))
        call dm_html_select_create(select_sensor, 1 + size(sensors))
        call dm_html_select_create(select_target, 1 + size(targets))
        call dm_html_select_create(select_result, size(max_results))

        ! Add empty select elements.
        call dm_html_select_set(select_node,   1, '', '')
        call dm_html_select_set(select_sensor, 1, '', '')
        call dm_html_select_set(select_target, 1, '', '')

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
               H_DIV_END // & ! end column 1
               H_DIV_COL // & ! column 2
               dm_html_label('Target Name', for='target_id') // &
               dm_html_select(select_target, 'target_id', 'target_id', target_id_) // &
               dm_html_label('Max. Results', for='max_results') // &
               dm_html_select(select_result, 'max_results', 'max_results', dm_itoa(nresults_)) // &
               H_DIV_END // & ! end column 2
               H_DIV_COL // & ! column 3
               dm_html_label('From', for='from') // &
               dm_html_input(HTML_INPUT_TYPE_DATETIME_LOCAL, id='from', name='from', required=.true., value=from_) // &
               dm_html_label('To', for='to') // &
               dm_html_input(HTML_INPUT_TYPE_DATETIME_LOCAL, id='to', name='to', required=.true., value=to_) // &
               H_DIV_END // & ! end column 3
               H_DIV_END // & ! end row 1
               dm_html_input(HTML_INPUT_TYPE_SUBMIT, name='submit', value='Search') // &
               H_FIELDSET_END // H_FORM_END

        call dm_html_select_destroy(select_node)
        call dm_html_select_destroy(select_sensor)
        call dm_html_select_destroy(select_target)
        call dm_html_select_destroy(select_result)
    end function html_form_images

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

        integer :: nresults_
        integer :: i

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

        disabled_ = dm_present(disabled, .false.)

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
               dm_html_label('Longitude', for='longitude') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='longitude', name='longitude', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter longitude (optional)') // &
               dm_html_label('Latitude', for='latitude') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='latitude', name='latitude', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter latitude (optional)') // &
               dm_html_label('Elevation', for='elevation') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='elevation', name='elevation', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter elevation (optional)') // &
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

        disabled_ = dm_present(disabled, .false.)

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
               dm_html_label('Longitude', for='longitude') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='longitude', name='longitude', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter longitude (optional)') // &
               dm_html_label('Latitude', for='latitude') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='latitude', name='latitude', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter latitude (optional)') // &
               dm_html_label('Elevation', for='elevation') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='elevation', name='elevation', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter elevation (optional)') // &
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

        disabled_ = dm_present(disabled, .false.)

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
               dm_html_label('Longitude', for='longitude') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='longitude', name='longitude', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter longitude (optional)') // &
               dm_html_label('Latitude', for='latitude') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='latitude', name='latitude', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter latitude (optional)') // &
               dm_html_label('Elevation', for='elevation') // &
               dm_html_input(HTML_INPUT_TYPE_TEXT, disabled=disabled_, id='elevation', name='elevation', &
                             pattern='[\+\-\.0-9]+', placeholder='Enter elevation (optional)') // &
               H_DIV_END // & ! end column 3
               H_DIV_END // & ! end row 1
               dm_html_input(HTML_INPUT_TYPE_SUBMIT, disabled=disabled_, name='submit', value='Submit') // &
               H_FIELDSET_END // H_FORM_END // H_DETAILS_END
    end function html_form_targets

    ! **************************************************************************
    ! UTILITY PROCEDURES.
    ! **************************************************************************
    subroutine html_error(heading, error, status, title, extra)
        !! Outputs error page (with header and footer).
        character(len=*), intent(in), optional :: heading !! Page heading.
        integer,          intent(in), optional :: error   !! DMPACK error code.
        integer,          intent(in), optional :: status  !! HTTP status code.
        character(len=*), intent(in), optional :: title   !! Page title.
        character(len=*), intent(in), optional :: extra   !! Extra message, pre-formatted.

        call html_header(title)

        if (present(status)) then
            call dm_cgi_write(dm_html_heading(1, dm_itoa(status) // ' ' // dm_http_status_string(status)))

            select case (status)
                case (HTTP_BAD_REQUEST)
                    call dm_cgi_write(dm_html_p('Malformed request or invalid request header.'))
                case (HTTP_NOT_FOUND)
                    call dm_cgi_write(dm_html_p('The requested resource could not be found.'))
                case (HTTP_INTERNAL_SERVER_ERROR, HTTP_SERVICE_UNAVAILABLE)
                    call dm_cgi_write(dm_html_p('An internal server error occured.'))
                case default
                    call dm_cgi_write(dm_html_p('An error occured.'))
            end select
        else
            if (present(heading)) then
                call dm_cgi_write(dm_html_heading(1, dm_html_encode(heading)))
            else
                call dm_cgi_write(dm_html_heading(1, 'Error'))
            end if
        end if

        if (present(error)) then
            call dm_cgi_write(dm_html_error(error))
        end if

        if (present(extra)) then
            call dm_cgi_write(dm_html_pre(extra, code=.true.))
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

        call dm_cgi_write(dm_html_footer(CONTENT))
    end subroutine html_footer

    subroutine html_header(title, inline_style, style)
        !! Outputs HTTP header, HTML header, and navigation.
        integer, parameter :: NANCHORS = 10 !! Number of elements in navigation.

        character(len=*), intent(in), optional :: title        !! Page title.
        character(len=*), intent(in), optional :: inline_style !! Additional inline CSS.
        character(len=*), intent(in), optional :: style        !! Additional CSS path.

        character(len=:), allocatable :: html, title_
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
            anchor_type(APP_BASE_PATH // '/images',  'Images'),       &
            anchor_type(APP_BASE_PATH // '/map',     'Map')           &
        ]

        mask = [ &
            .true.,                           & ! Dashboard.
            has_observ_db,                    & ! Nodes.
            has_observ_db,                    & ! Sensors.
            has_observ_db,                    & ! Targets.
            has_observ_db,                    & ! Observations.
            has_observ_db,                    & ! Plots.
            has_log_db,                       & ! Logs.
            has_beat_db,                      & ! Beats.
            has_image_db .and. has_image_dir, & ! Images.
            has_tile_url                      & ! Map.
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
        html = dm_html_header(title=title_, brand=APP_TITLE, inline_style=inline_style, styles=styles, nav=nav, nav_mask=mask)
        call dm_cgi_write(html)
    end subroutine html_header
end program dmweb
