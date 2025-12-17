! dmdwd.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmdwd
    !! Fetches and forwards weather reports from Deutscher Wetterdienst (DWD).
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmdwd'
    integer,      parameter :: APP_MAJOR = 0
    integer,      parameter :: APP_MINOR = 9
    integer,      parameter :: APP_PATCH = 9

    character(*), parameter :: APP_OBSERV_NAME  = 'dwd_weather_report'
    character(*), parameter :: APP_REQUEST_NAME = 'report'

    logical, parameter :: APP_MQ_BLOCKING = .true.  !! Observation forwarding is blocking.
    logical, parameter :: APP_RPC_TLS     = .false. !! Use TLS-encrypted connection.

    integer, parameter :: APP_READ_TYPE_NONE = 0 !! Invalid.
    integer, parameter :: APP_READ_TYPE_ALL  = 1 !! Read all weather report records.
    integer, parameter :: APP_READ_TYPE_LAST = 2 !! Read only the last weather report record.
    integer, parameter :: APP_READ_TYPE_NEXT = 3 !! Read only from the next weather report record on.

    integer, parameter :: APP_READ_TYPE_NAME_LEN = 4 !! Max. read type name length.

    type :: app_type
        !! Application settings.
        character(ID_LEN)                    :: name       = APP_NAME           !! Instance and configuration name (required).
        character(FILE_PATH_LEN)             :: config     = ' '                !! Path to configuration file (required).
        character(LOGGER_NAME_LEN)           :: logger     = ' '                !! Name of logger.
        character(NODE_ID_LEN)               :: node_id    = ' '                !! Node id (required).
        character(SENSOR_ID_LEN)             :: sensor_id  = ' '                !! Sensor id (required).
        character(TARGET_ID_LEN)             :: target_id  = ' '                !! Target id (required).
        character(FILE_PATH_LEN)             :: catalog    = ' '                !! Path to MOSMIX station catalog.
        character(DWD_MOSMIX_STATION_ID_LEN) :: station_id = ' '                !! MOSMIX station id.
        character(OBSERV_RECEIVER_LEN)       :: receiver   = ' '                !! Name of receiver's message queue (without leading `/`).
        character(APP_READ_TYPE_NAME_LEN)    :: read_name  = ' '                !! Read type name (required).
        integer                              :: read       = APP_READ_TYPE_LAST !! Read type.
        integer                              :: interval   = 0                  !! Read interval in seconds (>= 0).
        logical                              :: debug      = .false.            !! Forward debug messages via IPC.
        logical                              :: verbose    = .false.            !! Print debug messages to stdout (optional).
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App configuration.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments.
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

    ! Register signal handler.
    call dm_signal_register(signal_callback)

    ! Run main loop.
    rc = run(app)

    call logger%info('stopped ' // APP_NAME, error=rc)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function fetch_weather_reports(reports, station_id, last_modified) result(rc)
        !! Downloads weather reports file from DWD API.
        type(dwd_weather_report_type), allocatable, intent(out)   :: reports(:)    !! DWD weather reports.
        character(*),                               intent(in)    :: station_id    !! MOSMIX station id.
        integer(i8),                                intent(inout) :: last_modified !! Last updated time [Epoch].

        type(rpc_request_type)  :: request
        type(rpc_response_type) :: response

        rpc_block: block
            character(TIME_LEN)       :: timestamp
            character(:), allocatable :: url

            integer          :: stat
            real(r8)         :: duration
            type(timer_type) :: timer

            ! Open scratch file to store the response in.
            open (action='readwrite', form='formatted', iostat=stat, newunit=response%unit, status='scratch')

            if (stat /= 0) then
                rc = E_IO
                call logger%debug('failed to open temporary scratch file', error=rc)
                exit rpc_block
            end if

            url = dm_dwd_api_weather_report_url(app%station_id, tls=APP_RPC_TLS)

            if (len(url) == 0) then
                rc  = E_INVALID
                call logger%error('failed to create weather report URL', error=rc)
                exit rpc_block
            end if

            ! Send HTTP request to DWD server.
            call logger%debug('fetching weather reports of station ' // trim(station_id) // ' from ' // url)
            call dm_timer_start(timer)
            rc = dm_rpc_get(request, response, url, modified_since=last_modified, callback=dm_dwd_api_callback)
            call dm_timer_stop(timer, duration=duration)

            ! Handle server response.
            select case (response%code)
                case (HTTP_NONE)
                    call logger%debug('failed to fetch weather reports: ' // response%error_message, error=rc)

                case (HTTP_OK)
                    call logger%debug('fetched weather reports of station ' // trim(station_id) // ' in ' // dm_ftoa(duration, 3) // ' sec')

                    if (response%last_modified > 0) then
                        last_modified = response%last_modified
                        stat = dm_time_from_unix(response%last_modified, timestamp)
                        call logger%debug('weather reports of station ' // trim(station_id) // ' last updated ' // timestamp)
                    end if

                    rewind (response%unit)

                    rc = dm_dwd_weather_report_read(reports, response%unit)

                    if (dm_is_error(rc)) then
                        call logger%debug('failed to read weather reports into derived types', error=rc)
                        exit rpc_block
                    end if

                    call logger%debug('received ' // dm_itoa(size(reports)) // ' weather reports for station ' // trim(station_id))

                case (HTTP_NOT_MODIFIED)
                    rc = E_LIMIT
                    stat = dm_time_from_unix(last_modified, timestamp)
                    call logger%debug('no new weather reports since ' // timestamp // ' (HTTP ' // dm_itoa(response%code) // ')')

                case (HTTP_NOT_FOUND)
                    rc = E_NOT_FOUND
                    call logger%debug('no weather reports found for station ' // trim(station_id) // ' (HTTP ' // dm_itoa(response%code) // ')', error=rc)

                case default
                    call logger%debug('failed to fetch weather reports: ' // response%error_message // ' (HTTP ' // dm_itoa(response%code) // ')', error=rc)
            end select
        end block rpc_block

        close (response%unit)

        call dm_rpc_destroy(request)
        call dm_rpc_destroy(response)

        if (.not. allocated(reports)) allocate (reports(0))
    end function fetch_weather_reports

    integer function read_type_from_name(name) result(type)
        !! Returns read type from string. Returns `APP_READ_TYPE_NONE` on
        !! error.
        character(*), intent(in) :: name !! Read type name.

        character(APP_READ_TYPE_NAME_LEN) :: name_

        name_ = dm_to_lower(name)

        select case (name_)
            case ('all');  type = APP_READ_TYPE_ALL
            case ('last'); type = APP_READ_TYPE_LAST
            case ('next'); type = APP_READ_TYPE_NEXT
            case default;  type = APP_READ_TYPE_NONE
        end select
    end function read_type_from_name

    integer function run(app) result(rc)
        !! Fetches weather reports and forwards observations.
        type(app_type), intent(inout) :: app !! App type.

        integer     :: i, n, read_type
        integer(i8) :: first_report, last_modified

        type(dwd_weather_report_type), allocatable :: reports(:)
        type(observ_type)                          :: observ

        call find_station(app%catalog, app%station_id)

        rc = dm_rpc_init()

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize RPC backend', error=rc)
            return
        end if

        read_type     = app%read
        first_report  = 0_i8
        last_modified = 0_i8

        report_loop: do
            rpc_block: block
                rc = fetch_weather_reports(reports, app%station_id, last_modified)

                if (rc == E_LIMIT) then
                    call logger%debug('skipped reading of stale weather reports', error=rc)
                    exit rpc_block
                end if

                if (dm_is_error(rc)) then
                    call logger%error('failed to fetch weather reports from DWD API', error=rc)
                    exit rpc_block
                end if

                if (size(reports) == 0) then
                    call logger%error('no weather reports returned', error=E_EMPTY)
                    exit rpc_block
                end if

                if (.not. all(dm_dwd_is_weather_report_valid(reports))) then
                    call logger%error('invalid weather reports received', error=E_INVALID)
                    exit rpc_block
                end if

                if (first_report == 0) first_report = last_modified

                select case (read_type)
                    case (APP_READ_TYPE_ALL)
                        ! Read all weather reports.
                        n = size(reports)

                        do i = 1, n
                            call create_observ(observ, app, reports(i))
                            rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
                            call logger%debug('finished observ ' // observ%name)
                        end do

                        read_type = APP_READ_TYPE_LAST

                    case (APP_READ_TYPE_LAST)
                        ! Read only last weather report.
                        call create_observ(observ, app, reports(1))
                        rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
                        call logger%debug('finished observ ' // observ%name)

                    case (APP_READ_TYPE_NEXT)
                        ! Wait for next weather report.
                        if (last_modified > first_report) then
                            call create_observ(observ, app, reports(1))
                            rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
                            call logger%debug('finished observ ' // observ%name)
                        else
                            call logger%debug('waiting for next weather report')
                        end if
                end select
            end block rpc_block

            if (app%interval <= 0) then
                call logger%debug('no cycle interval set')
                exit report_loop
            end if

            call logger%debug('next DWD API call in ' // dm_itoa(app%interval) // ' sec')
            call dm_sleep(app%interval)
        end do report_loop

        call logger%debug('finished fetching of weather reports')
        call dm_rpc_shutdown()
    end function run

    subroutine create_observ(observ, app, report)
        !! Creates observation from weather report.
        type(observ_type),             intent(out)   :: observ
        type(app_type),                intent(inout) :: app
        type(dwd_weather_report_type), intent(inout) :: report

        integer            :: i, n
        type(request_type) :: requests(3)

        ! Initialise observation.
        call dm_observ_set(observ    = observ,           &
                           id        = dm_uuid4(),       &
                           node_id   = app%node_id,      &
                           sensor_id = app%sensor_id,    &
                           target_id = app%target_id,    &
                           name      = APP_OBSERV_NAME,  &
                           timestamp = report%timestamp, &
                           source    = app%name)

        ! Add observation receiver if available.
        rc = dm_observ_add_receiver(observ, app%receiver)
        call logger%debug('created observ ' // observ%name)

        ! Initialise requests.
        call dm_request_set(requests, name=APP_REQUEST_NAME, timestamp=report%timestamp, raw_request=app%station_id)

        ! Add responses to requests (if they exist).
        i = 1

        call add_response_real32(i, requests, 'cloud_cover',                    '%',     report%cloud_cover)
        call add_response_real32(i, requests, 'temperature_mean_prev_day',      'degC',  report%temperature_mean_prev_day)
        call add_response_real32(i, requests, 'depth_new_snow',                 'cm',    report%depth_new_snow)
        call add_response_real32(i, requests, 'dew_point_temperature_2m',       'degC',  report%dew_point_temperature_2m)
        call add_response_real32(i, requests, 'diffuse_radiation_last_hour',    'W/m^2', report%diffuse_radiation_last_hour)
        call add_response_real32(i, requests, 'direct_radiation_last_24h',      'W/m^2', report%direct_radiation_last_24h)
        call add_response_real32(i, requests, 'direct_radiation_last_hour',     'W/m^2', report%direct_radiation_last_hour)
        call add_response_real32(i, requests, 'dry_bulb_temperature_2m',        'degC',  report%dry_bulb_temperature_2m)
        call add_response_real32(i, requests, 'evaporation_last_24h',           'mm',    report%evaporation_last_24h)
        call add_response_real32(i, requests, 'global_radiation_last_hour',     'W/m^2', report%global_radiation_last_hour)
        call add_response_real32(i, requests, 'global_radiation_last_24h',      'W/m^2', report%global_radiation_last_24h)
        call add_response_real32(i, requests, 'lowest_cloud_above_station',     'm',     report%lowest_cloud_above_station)
        call add_response_real32(i, requests, 'horizontal_visibility',          'km',    report%horizontal_visibility)
        call add_response_real32(i, requests, 'max_wind_speed_mean_prev_day',   'km/h',  report%max_wind_speed_mean_prev_day)
        call add_response_real32(i, requests, 'max_temperature_prev_day',       'degC',  report%max_temperature_prev_day)
        call add_response_real32(i, requests, 'max_temperature_last_12h_2m',    'degC',  report%max_temperature_last_12h_2m)
        call add_response_real32(i, requests, 'max_wind_speed_mean_last_hour',  'km/h',  report%max_wind_speed_mean_last_hour)
        call add_response_real32(i, requests, 'max_wind_speed_last_6h',         'km/h',  report%max_wind_speed_last_6h)
        call add_response_real32(i, requests, 'max_wind_speed_prev_day',        'km/h',  report%max_wind_speed_prev_day)
        call add_response_real32(i, requests, 'max_wind_speed_last_hour',       'km/h',  report%max_wind_speed_last_hour)
        call add_response_real32(i, requests, 'wind_dir_mean_last_10min_10m',   'deg',   report%wind_dir_mean_last_10min_10m)
        call add_response_real32(i, requests, 'wind_speed_mean_last_10min_10m', 'km/h',  report%wind_speed_mean_last_10min_10m)
        call add_response_real32(i, requests, 'min_temperature_prev_day_5cm',   'degC',  report%min_temperature_prev_day_5cm)
        call add_response_real32(i, requests, 'min_temperature_prev_day',       'degC',  report%min_temperature_prev_day)
        call add_response_real32(i, requests, 'min_temperature_last_12h_2m',    'degC',  report%min_temperature_last_12h_2m)
        call add_response_real32(i, requests, 'min_temperature_last_12h_5cm',   'degC',  report%min_temperature_last_12h_5cm)
        call add_response_int32 (i, requests, 'last_weather1',                  'code',  report%last_weather1)
        call add_response_int32 (i, requests, 'last_weather2',                  'code',  report%last_weather2)
        call add_response_real32(i, requests, 'precipitation_last_24h',         'mm',    report%precipitation_last_24h)
        call add_response_real32(i, requests, 'precipitation_last_3h',          'mm',    report%precipitation_last_3h)
        call add_response_real32(i, requests, 'precipitation_last_6h',          'mm',    report%precipitation_last_6h)
        call add_response_real32(i, requests, 'precipitation_last_hour',        'mm',    report%precipitation_last_hour)
        call add_response_real32(i, requests, 'precipitation_last_12h',         'mm',    report%precipitation_last_12h)
        call add_response_int32 (i, requests, 'present_weather',                'code',  report%present_weather)
        call add_response_real32(i, requests, 'pressure_mean_sea_level',        'hPa',   report%pressure_mean_sea_level)
        call add_response_real32(i, requests, 'relative_humidity',              '%',     report%relative_humidity)
        call add_response_real32(i, requests, 'water_temperature',              'degC',  report%water_temperature)
        call add_response_real32(i, requests, 'temperature_5cm',                'degC',  report%temperature_5cm)
        call add_response_real32(i, requests, 'total_snow_depth',               'cm',    report%total_snow_depth)
        call add_response_real32(i, requests, 'total_time_sunshine_last_hour',  'min',   report%total_time_sunshine_last_hour)
        call add_response_real32(i, requests, 'total_time_sunshine_last_day',   'h',     report%total_time_sunshine_last_day)

        n = 1 + ((i - 1) / REQUEST_MAX_NRESPONSES)

        do i = 1, n
            if (requests(i)%nresponses == 0) cycle
            rc = dm_observ_add_request(observ, requests(i))

            if (dm_is_error(rc)) then
                call logger%error('failed to add request ' // trim(requests(i)%name) // ' to observ ' // observ%name, observ=observ, error=rc)
            else
                call logger%debug('added request ' // trim(requests(i)%name) // ' to observ ' // observ%name, observ=observ)
            end if
        end do
    end subroutine create_observ

    subroutine find_station(catalog, station_id)
        !! Tries to find MOSMIX station in station catalog. This routine only
        !! outputs log messages.
        character(*), intent(in) :: catalog    !! Path to station catalog.
        character(*), intent(in) :: station_id !! Station id.

        integer                                    :: rc, stat, unit
        logical                                    :: found
        type(dwd_mosmix_station_type)              :: station
        type(dwd_mosmix_station_type), allocatable :: stations(:)

        if (.not. dm_string_has(catalog)) then
            call logger%debug('no station catalog provided')
            return
        end if

        if (.not. dm_file_exists(catalog)) then
            call logger%warning('station catalog ' // trim(catalog) // ' not found', error=E_NOT_FOUND)
            return
        end if

        open (action='read', file=trim(catalog), iostat=stat, newunit=unit, status='old')

        if (stat /= 0) then
            call logger%warning('failed to open station catalog ' // catalog, error=E_IO)
            return
        end if

        io_block: block
            rc = dm_dwd_mosmix_station_catalog_read(stations, unit)

            if (dm_is_error(rc)) then
                call logger%warning('failed to read station catalog ' // catalog, error=rc)
                exit io_block
            end if

            call logger%debug('read ' // dm_itoa(size(stations)) // ' stations from catalog file ' // catalog)
            rc = dm_dwd_mosmix_station_find(stations, station_id, station, found)

            if (.not. found) then
                call logger%warning('station ' // trim(station_id) // ' not found in catalog', error=rc)
                exit io_block
            end if

            call logger%debug('found station ' // trim(station_id) // ' in catalog: ' // station%name)
        end block io_block

        close (unit)
    end subroutine find_station

    ! **************************************************************************
    ! RESPONSE ROUTINES.
    ! **************************************************************************
    subroutine add_response_int32(index, requests, name, unit, value)
        integer,            intent(inout) :: index
        type(request_type), intent(inout) :: requests(:)
        character(*),       intent(in)    :: name
        character(*),       intent(in)    :: unit
        integer(i4),        intent(in)    :: value

        integer :: rc

        request_block: block
            character(80) :: message
            integer       :: i, stat

            rc = E_NONE
            if (.not. dm_dwd_weather_report_has_value(value)) exit request_block

            rc = E_BOUNDS
            i = 1 + ((index - 1) / REQUEST_MAX_NRESPONSES)
            if (i > size(requests)) exit request_block

            rc = dm_request_add(requests(i), name=name, unit=unit, value=value)
            if (dm_is_error(rc)) exit request_block

            write (message, '("added value ", a, ": ", i0, 1x, a)', iostat=stat) trim(name), value, unit
            call logger%debug(message)

            rc = E_NONE
            index = index + 1
        end block request_block

        if (dm_is_error(rc)) call logger%error('failed to add response ' // trim(name) // ' (' // dm_itoa(index) // ') to request', error=rc)
    end subroutine add_response_int32

    subroutine add_response_real32(index, requests, name, unit, value)
        integer,            intent(inout) :: index
        type(request_type), intent(inout) :: requests(:)
        character(*),       intent(in)    :: name
        character(*),       intent(in)    :: unit
        real(r4),           intent(in)    :: value

        integer :: rc

        request_block: block
            character(80) :: message
            integer       :: i, stat

            rc = E_NONE
            if (.not. dm_dwd_weather_report_has_value(value)) exit request_block

            rc = E_BOUNDS
            i = 1 + ((index - 1) / REQUEST_MAX_NRESPONSES)
            if (i > size(requests)) exit request_block

            rc = dm_request_add(requests(i), name=name, unit=unit, value=value)
            if (dm_is_error(rc)) exit request_block

            write (message, '("added value ", a, ": ", f0.2, 1x, a)', iostat=stat) trim(name), value, unit
            call logger%debug(message)

            rc = E_NONE
            index = index + 1
        end block request_block

        if (dm_is_error(rc)) call logger%error('failed to add response ' // trim(name) // ' (' // dm_itoa(index) // ') to request', error=rc)
    end subroutine add_response_real32

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app !! App type.

        type(arg_class) :: arg

        ! Required and optional command-line arguments.
        call arg%create()
        call arg%add('name',     short='n', type=ARG_TYPE_ID)      ! -n, --name <string>
        call arg%add('config',   short='c', type=ARG_TYPE_FILE)    ! -c, --config <path>
        call arg%add('logger',   short='l', type=ARG_TYPE_ID)      ! -l, --logger <string>
        call arg%add('node',     short='N', type=ARG_TYPE_ID)      ! -N, --node <string>
        call arg%add('sensor',   short='S', type=ARG_TYPE_ID)      ! -S, --sensor <string>
        call arg%add('target',   short='T', type=ARG_TYPE_ID)      ! -T, --target <string>
        call arg%add('catalog',  short='C', type=ARG_TYPE_FILE)    ! -C, --catalog <path>
        call arg%add('station',  short='m', type=ARG_TYPE_STRING, max_len=DWD_MOSMIX_STATION_ID_LEN) ! -m, --station <id>
        call arg%add('receiver', short='r', type=ARG_TYPE_ID,     max_len=OBSERV_RECEIVER_LEN)       ! -r, --receiver <string>
        call arg%add('read',     short='R', type=ARG_TYPE_STRING)  ! -R, --read <string>
        call arg%add('interval', short='I', type=ARG_TYPE_INTEGER) ! -I, --interval <n>
        call arg%add('debug',    short='D', type=ARG_TYPE_LOGICAL) ! -D, --debug
        call arg%add('verbose',  short='V', type=ARG_TYPE_LOGICAL) ! -V, --verbose

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('name',   app%name)
        call arg%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call arg%get('logger',   app%logger)
        call arg%get('node',     app%node_id)
        call arg%get('sensor',   app%sensor_id)
        call arg%get('target',   app%target_id)
        call arg%get('catalog',  app%catalog)
        call arg%get('station',  app%station_id)
        call arg%get('receiver', app%receiver)
        call arg%get('read',     app%read_name)
        call arg%get('interval', app%interval)
        call arg%get('debug',    app%debug)
        call arg%get('verbose',  app%verbose)
        call arg%destroy()

        app%read = read_type_from_name(app%read_name)

        ! Validate options.
        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('logger',   app%logger)
            call config%get('node',     app%node_id)
            call config%get('sensor',   app%sensor_id)
            call config%get('target',   app%target_id)
            call config%get('catalog',  app%catalog)
            call config%get('station',  app%station_id)
            call config%get('receiver', app%receiver)
            call config%get('read',     app%read_name)
            call config%get('interval', app%interval)
            call config%get('debug',    app%debug)
            call config%get('verbose',  app%verbose)
        end if

        call config%close()
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        integer :: n

        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger')
            return
        end if

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (.not. dm_id_is_valid(app%sensor_id)) then
            call dm_error_out(rc, 'invalid or missing sensor id')
            return
        end if

        if (.not. dm_id_is_valid(app%target_id)) then
            call dm_error_out(rc, 'invalid or missing target id')
            return
        end if

        n = len_trim(app%station_id)

        if (n == 0 .or. n > DWD_MOSMIX_STATION_ID_LEN) then
            call dm_error_out(rc, 'invalid or missing station id')
            return
        end if

        if (dm_string_has(app%receiver) .and. .not. dm_id_is_valid(app%receiver)) then
            call dm_error_out(rc, 'invalid receiver')
            return
        end if

        if (app%read == APP_READ_TYPE_NONE) then
            call dm_error_out(rc, 'invalid read type')
            return
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
        !! Default POSIX signal handler of the program.
        integer(c_int), intent(in), value :: signum !! Signal number.

        call logger%debug('exit on on signal ' // dm_signal_name(signum))
        call logger%info('stopped ' // APP_NAME)
        call dm_stop(STOP_SUCCESS)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 1x, a)', dm_rpc_version(), dm_lua_version(.true.)
    end subroutine version_callback
end program dmdwd
