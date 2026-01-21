! dmdwd.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmdwd
    !! Fetches and forwards weather reports from Deutscher Wetterdienst (DWD).
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmdwd'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    character(*), parameter :: APP_OBSERV_NAME  = 'dwd_weather_report'

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
                            call logger%debug('finished observation ' // observ%name)
                        end do

                        read_type = APP_READ_TYPE_LAST

                    case (APP_READ_TYPE_LAST)
                        ! Read only last weather report.
                        call create_observ(observ, app, reports(1))
                        rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
                        call logger%debug('finished observation ' // observ%name)

                    case (APP_READ_TYPE_NEXT)
                        ! Wait for next weather report.
                        if (last_modified > first_report) then
                            call create_observ(observ, app, reports(1))
                            rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
                            call logger%debug('finished observation ' // observ%name)
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
        use :: dmpack, only: add_response => dm_observ_add_response, &
                             has_value    => dm_dwd_weather_report_has_value

        type(observ_type),             intent(out)   :: observ
        type(app_type),                intent(inout) :: app
        type(dwd_weather_report_type), intent(inout) :: report

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

        ! Add responses to requests (if they exist).
        if (has_value(report%cloud_cover                   )) rc = add_response(observ, 'cloud_cover',                    '%',     report%cloud_cover)
        if (has_value(report%temperature_mean_prev_day     )) rc = add_response(observ, 'temperature_mean_prev_day',      'degC',  report%temperature_mean_prev_day)
        if (has_value(report%depth_new_snow                )) rc = add_response(observ, 'depth_new_snow',                 'cm',    report%depth_new_snow)
        if (has_value(report%dew_point_temperature_2m      )) rc = add_response(observ, 'dew_point_temperature_2m',       'degC',  report%dew_point_temperature_2m)
        if (has_value(report%diffuse_radiation_last_hour   )) rc = add_response(observ, 'diffuse_radiation_last_hour',    'W/m^2', report%diffuse_radiation_last_hour)
        if (has_value(report%direct_radiation_last_24h     )) rc = add_response(observ, 'direct_radiation_last_24h',      'W/m^2', report%direct_radiation_last_24h)
        if (has_value(report%direct_radiation_last_hour    )) rc = add_response(observ, 'direct_radiation_last_hour',     'W/m^2', report%direct_radiation_last_hour)
        if (has_value(report%dry_bulb_temperature_2m       )) rc = add_response(observ, 'dry_bulb_temperature_2m',        'degC',  report%dry_bulb_temperature_2m)
        if (has_value(report%evaporation_last_24h          )) rc = add_response(observ, 'evaporation_last_24h',           'mm',    report%evaporation_last_24h)
        if (has_value(report%global_radiation_last_hour    )) rc = add_response(observ, 'global_radiation_last_hour',     'W/m^2', report%global_radiation_last_hour)
        if (has_value(report%global_radiation_last_24h     )) rc = add_response(observ, 'global_radiation_last_24h',      'W/m^2', report%global_radiation_last_24h)
        if (has_value(report%lowest_cloud_above_station    )) rc = add_response(observ, 'lowest_cloud_above_station',     'm',     report%lowest_cloud_above_station)
        if (has_value(report%horizontal_visibility         )) rc = add_response(observ, 'horizontal_visibility',          'km',    report%horizontal_visibility)
        if (has_value(report%max_wind_speed_mean_prev_day  )) rc = add_response(observ, 'max_wind_speed_mean_prev_day',   'km/h',  report%max_wind_speed_mean_prev_day)
        if (has_value(report%max_temperature_prev_day      )) rc = add_response(observ, 'max_temperature_prev_day',       'degC',  report%max_temperature_prev_day)
        if (has_value(report%max_temperature_last_12h_2m   )) rc = add_response(observ, 'max_temperature_last_12h_2m',    'degC',  report%max_temperature_last_12h_2m)
        if (has_value(report%max_wind_speed_mean_last_hour )) rc = add_response(observ, 'max_wind_speed_mean_last_hour',  'km/h',  report%max_wind_speed_mean_last_hour)
        if (has_value(report%max_wind_speed_last_6h        )) rc = add_response(observ, 'max_wind_speed_last_6h',         'km/h',  report%max_wind_speed_last_6h)
        if (has_value(report%max_wind_speed_prev_day       )) rc = add_response(observ, 'max_wind_speed_prev_day',        'km/h',  report%max_wind_speed_prev_day)
        if (has_value(report%max_wind_speed_last_hour      )) rc = add_response(observ, 'max_wind_speed_last_hour',       'km/h',  report%max_wind_speed_last_hour)
        if (has_value(report%wind_dir_mean_last_10min_10m  )) rc = add_response(observ, 'wind_dir_mean_last_10min_10m',   'deg',   report%wind_dir_mean_last_10min_10m)
        if (has_value(report%wind_speed_mean_last_10min_10m)) rc = add_response(observ, 'wind_speed_mean_last_10min_10m', 'km/h',  report%wind_speed_mean_last_10min_10m)
        if (has_value(report%min_temperature_prev_day_5cm  )) rc = add_response(observ, 'min_temperature_prev_day_5cm',   'degC',  report%min_temperature_prev_day_5cm)
        if (has_value(report%min_temperature_prev_day      )) rc = add_response(observ, 'min_temperature_prev_day',       'degC',  report%min_temperature_prev_day)
        if (has_value(report%min_temperature_last_12h_2m   )) rc = add_response(observ, 'min_temperature_last_12h_2m',    'degC',  report%min_temperature_last_12h_2m)
        if (has_value(report%min_temperature_last_12h_5cm  )) rc = add_response(observ, 'min_temperature_last_12h_5cm',   'degC',  report%min_temperature_last_12h_5cm)
        if (has_value(report%last_weather1                 )) rc = add_response(observ, 'last_weather1',                  'code',  report%last_weather1)
        if (has_value(report%last_weather2                 )) rc = add_response(observ, 'last_weather2',                  'code',  report%last_weather2)
        if (has_value(report%precipitation_last_24h        )) rc = add_response(observ, 'precipitation_last_24h',         'mm',    report%precipitation_last_24h)
        if (has_value(report%precipitation_last_3h         )) rc = add_response(observ, 'precipitation_last_3h',          'mm',    report%precipitation_last_3h)
        if (has_value(report%precipitation_last_6h         )) rc = add_response(observ, 'precipitation_last_6h',          'mm',    report%precipitation_last_6h)
        if (has_value(report%precipitation_last_hour       )) rc = add_response(observ, 'precipitation_last_hour',        'mm',    report%precipitation_last_hour)
        if (has_value(report%precipitation_last_12h        )) rc = add_response(observ, 'precipitation_last_12h',         'mm',    report%precipitation_last_12h)
        if (has_value(report%present_weather               )) rc = add_response(observ, 'present_weather',                'code',  report%present_weather)
        if (has_value(report%pressure_mean_sea_level       )) rc = add_response(observ, 'pressure_mean_sea_level',        'hPa',   report%pressure_mean_sea_level)
        if (has_value(report%relative_humidity             )) rc = add_response(observ, 'relative_humidity',              '%',     report%relative_humidity)
        if (has_value(report%water_temperature             )) rc = add_response(observ, 'water_temperature',              'degC',  report%water_temperature)
        if (has_value(report%temperature_5cm               )) rc = add_response(observ, 'temperature_5cm',                'degC',  report%temperature_5cm)
        if (has_value(report%total_snow_depth              )) rc = add_response(observ, 'total_snow_depth',               'cm',    report%total_snow_depth)
        if (has_value(report%total_time_sunshine_last_hour )) rc = add_response(observ, 'total_time_sunshine_last_hour',  'min',   report%total_time_sunshine_last_hour)
        if (has_value(report%total_time_sunshine_last_day  )) rc = add_response(observ, 'total_time_sunshine_last_day',   'h',     report%total_time_sunshine_last_day)

        call logger%debug('created observation ' // observ%name)
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
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app !! App type.

        type(arg_parser_class) :: parser

        ! Required and optional command-line arguments.
        call parser%add('name',     short='n', type=ARG_TYPE_ID)                   ! -n, --name <string>
        call parser%add('config',   short='c', type=ARG_TYPE_FILE)                 ! -c, --config <path>
        call parser%add('logger',   short='l', type=ARG_TYPE_ID)                   ! -l, --logger <string>
        call parser%add('node',     short='N', type=ARG_TYPE_ID)                   ! -N, --node <string>
        call parser%add('sensor',   short='S', type=ARG_TYPE_ID)                   ! -S, --sensor <string>
        call parser%add('target',   short='T', type=ARG_TYPE_ID)                   ! -T, --target <string>
        call parser%add('catalog',  short='C', type=ARG_TYPE_FILE,   exist=.true.) ! -C, --catalog <path>
        call parser%add('station',  short='m', type=ARG_TYPE_STRING, max_len=DWD_MOSMIX_STATION_ID_LEN) ! -m, --station <id>
        call parser%add('receiver', short='r', type=ARG_TYPE_ID,     max_len=OBSERV_RECEIVER_LEN)       ! -r, --receiver <string>
        call parser%add('read',     short='R', type=ARG_TYPE_STRING)               ! -R, --read <string>
        call parser%add('interval', short='I', type=ARG_TYPE_INTEGER)              ! -I, --interval <n>
        call parser%add('debug',    short='D', type=ARG_TYPE_LOGICAL)              ! -D, --debug
        call parser%add('verbose',  short='V', type=ARG_TYPE_LOGICAL)              ! -V, --verbose

        ! Read all command-line arguments.
        rc = parser%read(version_callback)
        if (dm_is_error(rc)) return

        call parser%get('name',   app%name)
        call parser%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call parser%get('logger',   app%logger)
        call parser%get('node',     app%node_id)
        call parser%get('sensor',   app%sensor_id)
        call parser%get('target',   app%target_id)
        call parser%get('catalog',  app%catalog)
        call parser%get('station',  app%station_id)
        call parser%get('receiver', app%receiver)
        call parser%get('read',     app%read_name)
        call parser%get('interval', app%interval)
        call parser%get('debug',    app%debug)
        call parser%get('verbose',  app%verbose)

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
