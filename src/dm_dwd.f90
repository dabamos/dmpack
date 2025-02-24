! Author:  Philipp Engel
! Licence: ISC
module dm_dwd
    !! I/O module for meteorological data of Deutsche Wetterdienst (DWD).
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_time
    use :: dm_util, only: dm_present
    implicit none (type, external)
    private

    character(len=*), parameter :: DWD_MOSMIX_STATION_FMT  = '(a5, 1x, a4, 1x, a20, 1x, f6.2, 1x, f7.2, 1x, i5)'

    integer, parameter, public :: DWD_MOSMIX_STATION_ID_LEN   = 5
    integer, parameter, public :: DWD_MOSMIX_STATION_ICAO_LEN = 4
    integer, parameter, public :: DWD_MOSMIX_STATION_NAME_LEN = 20

    type, public :: dwd_mosmix_station_type
        !! MOSMIX weather station.
        character(len=DWD_MOSMIX_STATION_ID_LEN)   :: id   = ' '    !! Station id.
        character(len=DWD_MOSMIX_STATION_ICAO_LEN) :: icao = '----' !! ICAO code.
        character(len=DWD_MOSMIX_STATION_NAME_LEN) :: name = ' '    !! Station name.
        real                                       :: lat  = 0.0    !! Latitude.
        real                                       :: lon  = 0.0    !! Longitude.
        integer                                    :: elev = 0      !! Elevation.
    end type dwd_mosmix_station_type

    type, public :: dwd_weather_report_type
        !! Weather report data (POI).
        character(len=TIME_LEN) :: timestamp                            = TIME_DEFAULT !! Date and time [UTC].
        real                    :: cloud_cover                          = huge(0.0)    !! Cloud cover total [%].
        real                    :: temperature_mean_prev_day            = huge(0.0)    !! Daily mean of temperature previous day [°C].
        real                    :: depth_new_snow                       = huge(0.0)    !! Depth of new snow [cm].
        real                    :: dew_point_temperature_2_m            = huge(0.0)    !! Dew point temperature at 2 meters above ground [°C].
        real                    :: diffuse_solar_radiation_last_hour    = huge(0.0)    !! Diffuse solar radiation last hour [W/m^2].
        real                    :: direct_solar_radiation_last_24_hours = huge(0.0)    !! Direct solar radiation last 24 hours [W/m^2].
        real                    :: direct_solar_radiation_last_hour     = huge(0.0)    !! Direct solar radiation last hour [W/m^2].
        real                    :: dry_bulb_temperature_2_m             = huge(0.0)    !! Dry bulb temperature at 2 meters above ground [°C].
        real                    :: evaporation_last_24_hours            = huge(0.0)    !! Evaporation/evapotranspiration last 24 hours [mm].
        real                    :: global_radiation_last_hour           = huge(0.0)    !! Global radiation last hour [W/m^2].
        real                    :: global_radiation_last_24_hours       = huge(0.0)    !! Global radiation last 24 hours [W/m^2].
        real                    :: height_lowest_cloud_above_station    = huge(0.0)    !! Height of base of lowest cloud above station [m].
        real                    :: horizontal_visibility                = huge(0.0)    !! Horizontal visibility [km].
        real                    :: max_wind_speed_mean_prev_day         = huge(0.0)    !! Maximum of 10 minutes mean of wind speed for previous day [km/h].
        real                    :: max_temperature_prev_day             = huge(0.0)    !! Maximum of temperature for previous day [°C].
        real                    :: max_temperature_last_12_hours_2_m    = huge(0.0)    !! Maximum temperature last 12 hours 2 meters above ground [°C].
        real                    :: max_wind_speed_mean_last_hour        = huge(0.0)    !! Maximum wind speed as 10 minutes mean during last hour [km/h].
        real                    :: max_wind_speed_last_6_hours          = huge(0.0)    !! Maximum wind speed during last 6 hours [km/h].
        real                    :: max_wind_speed_prev_day              = huge(0.0)    !! Maximum wind speed for previous day [km/h].
        real                    :: max_wind_speed_last_hour             = huge(0.0)    !! Maximum wind speed last hour [km/h].
        real                    :: wind_direction_mean_last_10_min_10_m = huge(0.0)    !! Mean wind direction during last 10 min at 10 meters above ground [°].
        real                    :: wind_speed_mean_last_10_min_10_m     = huge(0.0)    !! Mean wind speed during last 10 min at 10 meters above ground [km/h].
        real                    :: min_temperature_prev_day_5_cm        = huge(0.0)    !! Minimum of temperature at 5 cm above ground for previous day [°C].
        real                    :: min_temperature_prev_day             = huge(0.0)    !! Minimum of temperature for previous day [°C].
        real                    :: min_temperature_last_12_hours_2_m    = huge(0.0)    !! Minimum temperature last 12 hours 2 meters above ground [°C].
        real                    :: min_temperature_last_12_hours_5_cm   = huge(0.0)    !! Minimum temperature last 12 hours 5 cm above ground [°C].
        integer                 :: last_weather_1                       = 0            !! Past weather 1 [code].
        integer                 :: last_weather_2                       = 0            !! Past weather 2 [code].
        real                    :: precipitation_last_24_hours          = huge(0.0)    !! Precipitation amount last 24 hours [mm].
        real                    :: precipitation_last_3_hours           = huge(0.0)    !! Precipitation amount last 3 hours [mm].
        real                    :: precipitation_last_6_hours           = huge(0.0)    !! Precipitation amount last 6 hours [mm].
        real                    :: precipitation_last_hour              = huge(0.0)    !! Precipitation amount last hour [mm].
        real                    :: precipitation_last_12_hours          = huge(0.0)    !! Precipitation last 12 hours [mm].
        integer                 :: present_weather                      = 0            !! Present weather [code].
        real                    :: pressure_mean_sea_level              = huge(0.0)    !! Pressure reduced to mean sea level [hPa].
        real                    :: relative_humidity                    = huge(0.0)    !! Relative humidity [%].
        real                    :: water_temperature                    = huge(0.0)    !! Sea/water temperature [°C].
        real                    :: temperature_5_cm                     = huge(0.0)    !! Temperature at 5 cm above ground [°C].
        real                    :: total_snow_depth                     = huge(0.0)    !! Total snow depth [cm].
        real                    :: total_time_sunshine_last_hour        = huge(0.0)    !! Total time of sunshine during last hour [min].
        real                    :: total_time_sunshine_last_day         = huge(0.0)    !! Total time of sunshine last day [h].
    end type dwd_weather_report_type

    interface dm_dwd_weather_report_has_value
        module procedure :: dwd_weather_report_has_value_int32
        module procedure :: dwd_weather_report_has_value_real32
    end interface dm_dwd_weather_report_has_value

    public :: dm_dwd_mosmix_station_catalog_read
    public :: dm_dwd_mosmix_station_catalog_write
    public :: dm_dwd_mosmix_station_find
    public :: dm_dwd_mosmix_station_out
    public :: dm_dwd_weather_report_has_value
    public :: dm_dwd_weather_report_out
    public :: dm_dwd_weather_report_read

    private :: dwd_weather_report_has_value_int32
    private :: dwd_weather_report_has_value_real32
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_dwd_mosmix_station_catalog_read(stations, unit, header) result(rc)
        !! Reads MOSMIX stations from CFG catalog file. On error, the array is
        !! allocated but of size 0. Pass the file unit to the function:
        !!
        !! ```fortran
        !! integer :: stat, unit
        !! type(dwd_mosmix_station_type), allocatable :: stations(:)
        !!
        !! open (action='read', file='catalog.cfg', iostat=stat, newunit=unit, status='old')
        !! if (stat /= 0) error stop
        !! rc = dm_dwd_mosmix_station_catalog_read(stations, unit)
        !! close (unit)
        !! ```
        !!
        !! The MOSMIX station catalog has the following format:
        !!
        !! ```text
        !! ID    ICAO NAME                 LAT    LON     ELEV
        !! ----- ---- -------------------- -----  ------- -----
        !! 01001 ENJA JAN MAYEN             70.56   -8.40    10
        !! 01008 ENSB SVALBARD              78.15   15.28    29
        !! 01025 ---- TROMSOE               69.41   18.55    10
        !! ```
        !!
        !! The catalog file can be downloaded from:
        !!
        !! * https://www.dwd.de/DE/leistungen/met_verfahren_mosmix/mosmix_stationskatalog.cfg?view=nasPublication&nn=16102
        !!
        !! Or, use local file `share/dmdwd/catalog.cfg`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_EMPTY` if no records have been read.
        !! * `E_FORMAT` if line format is invalid.
        !! * `E_READ` if file reading failed.
        !!
        type(dwd_mosmix_station_type), allocatable, intent(out)          :: stations(:) !! Stations read from file.
        integer,                                    intent(in)           :: unit        !! File unit.
        logical,                                    intent(in), optional :: header      !! File contains header.

        character(len=52) :: line
        integer           :: nlines, nstations, stat
        logical           :: header_

        type(dwd_mosmix_station_type), allocatable :: buffer(:)

        nlines    = 0
        nstations = 0
        header_   = dm_present(header, .true.)

        rc = E_ALLOC
        allocate (stations(0), stat=stat)
        if (stat /= 0) return

        read_loop: do
            nlines = nlines + 1

            rc = E_READ
            read (unit, '(a)', iostat=stat) line

            if (is_iostat_end(stat)) exit read_loop
            if (stat /= 0)           return

            if (header_ .and. nlines < 3) cycle read_loop

            nstations = nstations + 1

            rc = E_ALLOC
            if (allocated(buffer)) deallocate (buffer)
            allocate (buffer(nstations), stat=stat)
            if (stat /= 0) return

            buffer(1:size(stations)) = stations

            rc = E_FORMAT
            read (line, DWD_MOSMIX_STATION_FMT, iostat=stat) buffer(nstations)
            if (stat /= 0) return

            call move_alloc(from=buffer, to=stations)
        end do read_loop

        rc = E_NONE
        if (size(stations) == 0) rc = E_EMPTY
    end function dm_dwd_mosmix_station_catalog_read

    integer function dm_dwd_mosmix_station_find(stations, id, station, found) result(rc)
        !! Returns station of `id` from array `stations` in argument `station`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if array is empty.
        !! * `E_NOT_FOUND` if station id was not found.
        !!
        type(dwd_mosmix_station_type),            intent(inout)         :: stations(:) !! MOSMIX stations.
        character(len=DWD_MOSMIX_STATION_ID_LEN), intent(in)            :: id          !! Station id.
        type(dwd_mosmix_station_type),            intent(out)           :: station     !! MOSMIX station of id.
        logical,                                  intent(out), optional :: found       !! Station found.

        integer :: i, loc

        if (present(found)) found = .false.

        rc = E_EMPTY
        if (size(stations) == 0) return

        loc = 0

        do i = 1, size(stations)
            if (stations(i)%id == id) then
                loc = i
                exit
            end if
        end do

        rc = E_NOT_FOUND
        if (loc == 0) return

        rc = E_NONE
        station = stations(loc)

        if (present(found)) found = .true.
    end function dm_dwd_mosmix_station_find

    integer function dm_dwd_weather_report_read(reports, unit, header) result(rc)
        !! Reads DWD weather report records from file unit. The reports are
        !! available as open data from:
        !!
        !! * https://opendata.dwd.de/weather/weather_reports/poi/
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_EMPTY` if no records have been read.
        !! * `E_FORMAT` if line format is invalid.
        !! * `E_READ` if file reading failed.
        !!
        use :: dm_string, only: dm_string_split

        type(dwd_weather_report_type), allocatable, intent(out)          :: reports(:) !! Weather report records.
        integer,                                    intent(in)           :: unit       !! File unit.
        logical,                                    intent(in), optional :: header     !! Write header.

        character(len=512) :: line
        character(len=8)   :: fields(43)
        integer            :: nfields, nlines, nreports, stat
        logical            :: header_

        type(dwd_weather_report_type), allocatable :: buffer(:)

        nlines   = 0
        nreports = 0
        header_  = dm_present(header, .true.)
        line     = ' '

        rc = E_ALLOC
        allocate (reports(0), stat=stat)
        if (stat /= 0) return

        read_loop: do
            nlines = nlines + 1

            rc = E_READ
            read (unit, '(a)', iostat=stat) line

            if (is_iostat_end(stat)) exit read_loop
            if (stat /= 0)           return

            if (header_ .and. nlines < 4) cycle read_loop
            if (len_trim(line) == 0)      exit read_loop

            nreports = nreports + 1

            rc = E_ALLOC
            if (allocated(buffer)) deallocate (buffer)
            allocate (buffer(nreports), stat=stat)
            if (stat /= 0) return

            buffer(1:size(reports)) = reports

            rc = E_FORMAT
            call dm_string_split(line, fields, del=';', n=nfields)
            if (nfields /= 43) return

            associate (report => buffer(nreports), date => fields(1), time => fields(2))
                ! Date and time to ISO 8601 timestamp.
                write (report%timestamp, '("20", a2, "-", a2, "-", a2, "T", a5, ":00.000000+00:00")') date(7:8), date(4:5), date(1:2), time(1:5)
                if (.not. dm_time_is_valid(report%timestamp)) return

                ! Weather report data.
                report%cloud_cover                          = read_real   (fields( 3))
                report%temperature_mean_prev_day            = read_real   (fields( 4))
                report%depth_new_snow                       = read_real   (fields( 5))
                report%dew_point_temperature_2_m            = read_real   (fields( 6))
                report%diffuse_solar_radiation_last_hour    = read_real   (fields( 7))
                report%direct_solar_radiation_last_24_hours = read_real   (fields( 8))
                report%direct_solar_radiation_last_hour     = read_real   (fields( 9))
                report%dry_bulb_temperature_2_m             = read_real   (fields(10))
                report%evaporation_last_24_hours            = read_real   (fields(11))
                report%global_radiation_last_hour           = read_real   (fields(12))
                report%global_radiation_last_24_hours       = read_real   (fields(13))
                report%height_lowest_cloud_above_station    = read_real   (fields(14))
                report%horizontal_visibility                = read_real   (fields(15))
                report%max_wind_speed_mean_prev_day         = read_real   (fields(16))
                report%max_temperature_prev_day             = read_real   (fields(17))
                report%max_temperature_last_12_hours_2_m    = read_real   (fields(18))
                report%max_wind_speed_mean_last_hour        = read_real   (fields(19))
                report%max_wind_speed_last_6_hours          = read_real   (fields(20))
                report%max_wind_speed_prev_day              = read_real   (fields(21))
                report%max_wind_speed_last_hour             = read_real   (fields(22))
                report%wind_direction_mean_last_10_min_10_m = read_real   (fields(23))
                report%wind_speed_mean_last_10_min_10_m     = read_real   (fields(24))
                report%min_temperature_prev_day_5_cm        = read_real   (fields(25))
                report%min_temperature_prev_day             = read_real   (fields(26))
                report%min_temperature_last_12_hours_2_m    = read_real   (fields(27))
                report%min_temperature_last_12_hours_5_cm   = read_real   (fields(28))
                report%last_weather_1                       = read_integer(fields(29))
                report%last_weather_2                       = read_integer(fields(30))
                report%precipitation_last_24_hours          = read_real   (fields(31))
                report%precipitation_last_3_hours           = read_real   (fields(32))
                report%precipitation_last_6_hours           = read_real   (fields(33))
                report%precipitation_last_hour              = read_real   (fields(34))
                report%precipitation_last_12_hours          = read_real   (fields(35))
                report%present_weather                      = read_integer(fields(36))
                report%pressure_mean_sea_level              = read_real   (fields(37))
                report%relative_humidity                    = read_real   (fields(38))
                report%water_temperature                    = read_real   (fields(39))
                report%temperature_5_cm                     = read_real   (fields(40))
                report%total_snow_depth                     = read_real   (fields(41))
                report%total_time_sunshine_last_hour        = read_real   (fields(42))
                report%total_time_sunshine_last_day         = read_real   (fields(43))
            end associate

            call move_alloc(from=buffer, to=reports)
        end do read_loop

        rc = E_NONE
        if (size(reports) == 0) rc = E_EMPTY
    contains
        pure integer function read_integer(field) result(value)
            character(len=*), intent(in) :: field !! Data field.

            integer :: stat

            value = 0
            if (field == '---') return
            read (field, *, iostat=stat) value
        end function read_integer

        pure real function read_real(field) result(value)
            character(len=*), intent(in) :: field !! Data field.

            integer :: stat

            value = huge(0.0)
            if (field == '---') return
            read (field, *, decimal='comma', iostat=stat) value
        end function read_real
    end function dm_dwd_weather_report_read

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_dwd_mosmix_station_catalog_write(stations, unit, header)
        !! Writes MOSMIX station catalog to standard output or file unit.
        type(dwd_mosmix_station_type), intent(inout)        :: stations(:) !! MOSMIX station types.
        integer,                       intent(in), optional :: unit        !! File unit.
        logical,                       intent(in), optional :: header      !! Write header.

        integer :: i, unit_
        logical :: header_

        unit_   = dm_present(unit, stdout)
        header_ = dm_present(header, .true.)

        if (header_) then
            write (unit_, '("ID", 4x, "ICAO", 1x, "NAME", 17x, "LAT", 4x, "LON", 5x, "ELEV")')
            write (unit_, '(5("-"), 1x, 4("-"), 1x, 20("-"), 1x, 5("-"), 2x, 7("-"), 1x, 5("-"))')
        end if

        write (unit_, DWD_MOSMIX_STATION_FMT) [ (stations(i), i = 1, size(stations)) ]
    end subroutine dm_dwd_mosmix_station_catalog_write

    subroutine dm_dwd_mosmix_station_out(station, unit)
        !! Prints MOSMIX station to standard output or given file unit.
        type(dwd_mosmix_station_type), intent(in)           :: station !! MOSMIX station type.
        integer,                       intent(in), optional :: unit    !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, stdout)

        write (unit_, '("dwd_mosmix_station.id: ", a)')     trim(station%id)
        write (unit_, '("dwd_mosmix_station.icao: ", a)')   trim(station%icao)
        write (unit_, '("dwd_mosmix_station.name: ", a)')   trim(station%name)
        write (unit_, '("dwd_mosmix_station.lat: ", f0.2)') station%lat
        write (unit_, '("dwd_mosmix_station.lon: ", f0.2)') station%lon
        write (unit_, '("dwd_mosmix_station.elev: ", i0)')  station%elev
    end subroutine dm_dwd_mosmix_station_out

    subroutine dm_dwd_weather_report_out(report, unit)
        !! Prints DWD weather report to standard output or given file unit.
        type(dwd_weather_report_type), intent(in)           :: report !! Weather report type.
        integer,                       intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, stdout)

        write (unit_, '("dwd_weather_report.timestamp: ", a)') report%timestamp

        if (report%cloud_cover                           < huge(0.0)) write (unit_, '("dwd_weather_report.cloud_cover: ", f0.1)')                          report%cloud_cover
        if (report%temperature_mean_prev_day             < huge(0.0)) write (unit_, '("dwd_weather_report.temperature_mean_prev_day: ", f0.1)')            report%temperature_mean_prev_day
        if (report%depth_new_snow                        < huge(0.0)) write (unit_, '("dwd_weather_report.depth_new_snow: ", f0.1)')                       report%depth_new_snow
        if (report%dew_point_temperature_2_m             < huge(0.0)) write (unit_, '("dwd_weather_report.dew_point_temperature_2_m: ", f0.1)')            report%dew_point_temperature_2_m
        if (report%diffuse_solar_radiation_last_hour     < huge(0.0)) write (unit_, '("dwd_weather_report.diffuse_solar_radiation_last_hour: ", f0.1)')    report%diffuse_solar_radiation_last_hour
        if (report%direct_solar_radiation_last_24_hours  < huge(0.0)) write (unit_, '("dwd_weather_report.direct_solar_radiation_last_24_hours: ", f0.1)') report%direct_solar_radiation_last_24_hours
        if (report%direct_solar_radiation_last_hour      < huge(0.0)) write (unit_, '("dwd_weather_report.direct_solar_radiation_last_hour: ", f0.1)')     report%direct_solar_radiation_last_hour
        if (report%dry_bulb_temperature_2_m              < huge(0.0)) write (unit_, '("dwd_weather_report.dry_bulb_temperature_2_m: ", f0.1)')             report%dry_bulb_temperature_2_m
        if (report%evaporation_last_24_hours             < huge(0.0)) write (unit_, '("dwd_weather_report.evaporation_last_24_hours: ", f0.1)')            report%evaporation_last_24_hours
        if (report%global_radiation_last_hour            < huge(0.0)) write (unit_, '("dwd_weather_report.global_radiation_last_hour: ", f0.1)')           report%global_radiation_last_hour
        if (report%global_radiation_last_24_hours        < huge(0.0)) write (unit_, '("dwd_weather_report.global_radiation_last_24_hours: ", f0.1)')       report%global_radiation_last_24_hours
        if (report%height_lowest_cloud_above_station     < huge(0.0)) write (unit_, '("dwd_weather_report.height_lowest_cloud_above_station: ", f0.1)')    report%height_lowest_cloud_above_station
        if (report%horizontal_visibility                 < huge(0.0)) write (unit_, '("dwd_weather_report.horizontal_visibility: ", f0.1)')                report%horizontal_visibility
        if (report%max_wind_speed_mean_prev_day          < huge(0.0)) write (unit_, '("dwd_weather_report.max_wind_speed_mean_prev_day: ", f0.1)')         report%max_wind_speed_mean_prev_day
        if (report%max_temperature_prev_day              < huge(0.0)) write (unit_, '("dwd_weather_report.max_temperature_prev_day: ", f0.1)')             report%max_temperature_prev_day
        if (report%max_temperature_last_12_hours_2_m     < huge(0.0)) write (unit_, '("dwd_weather_report.max_temperature_last_12_hours_2_m: ", f0.1)')    report%max_temperature_last_12_hours_2_m
        if (report%max_wind_speed_mean_last_hour         < huge(0.0)) write (unit_, '("dwd_weather_report.max_wind_speed_mean_last_hour: ", f0.1)')        report%max_wind_speed_mean_last_hour
        if (report%max_wind_speed_last_6_hours           < huge(0.0)) write (unit_, '("dwd_weather_report.max_wind_speed_last_6_hours: ", f0.1)')          report%max_wind_speed_last_6_hours
        if (report%max_wind_speed_prev_day               < huge(0.0)) write (unit_, '("dwd_weather_report.max_wind_speed_prev_day: ", f0.1)')              report%max_wind_speed_prev_day
        if (report%max_wind_speed_last_hour              < huge(0.0)) write (unit_, '("dwd_weather_report.max_wind_speed_last_hour: ", f0.1)')             report%max_wind_speed_last_hour
        if (report%wind_direction_mean_last_10_min_10_m  < huge(0.0)) write (unit_, '("dwd_weather_report.wind_direction_mean_last_10_min_10_m: ", f0.1)') report%wind_direction_mean_last_10_min_10_m
        if (report%wind_speed_mean_last_10_min_10_m      < huge(0.0)) write (unit_, '("dwd_weather_report.wind_speed_mean_last_10_min_10_m: ", f0.1)')     report%wind_speed_mean_last_10_min_10_m
        if (report%min_temperature_prev_day_5_cm         < huge(0.0)) write (unit_, '("dwd_weather_report.min_temperature_prev_day_5_cm: ", f0.1)')        report%min_temperature_prev_day_5_cm
        if (report%min_temperature_prev_day              < huge(0.0)) write (unit_, '("dwd_weather_report.min_temperature_prev_day: ", f0.1)')             report%min_temperature_prev_day
        if (report%min_temperature_last_12_hours_2_m     < huge(0.0)) write (unit_, '("dwd_weather_report.min_temperature_last_12_hours_2_m: ", f0.1)')    report%min_temperature_last_12_hours_2_m
        if (report%min_temperature_last_12_hours_5_cm    < huge(0.0)) write (unit_, '("dwd_weather_report.min_temperature_last_12_hours_5_cm: ", f0.1)')   report%min_temperature_last_12_hours_5_cm
        if (report%last_weather_1                        > 0)         write (unit_, '("dwd_weather_report.last_weather_1: ", i0)')                         report%last_weather_1
        if (report%last_weather_2                        > 0)         write (unit_, '("dwd_weather_report.last_weather_2: ", i0)')                         report%last_weather_2
        if (report%precipitation_last_24_hours           < huge(0.0)) write (unit_, '("dwd_weather_report.precipitation_last_24_hours: ", f0.1)')          report%precipitation_last_24_hours
        if (report%precipitation_last_3_hours            < huge(0.0)) write (unit_, '("dwd_weather_report.precipitation_last_3_hours: ", f0.1)')           report%precipitation_last_3_hours
        if (report%precipitation_last_6_hours            < huge(0.0)) write (unit_, '("dwd_weather_report.precipitation_last_6_hours: ", f0.1)')           report%precipitation_last_6_hours
        if (report%precipitation_last_hour               < huge(0.0)) write (unit_, '("dwd_weather_report.precipitation_last_hour: ", f0.1)')              report%precipitation_last_hour
        if (report%precipitation_last_12_hours           < huge(0.0)) write (unit_, '("dwd_weather_report.precipitation_last_12_hours: ", f0.1)')          report%precipitation_last_12_hours
        if (report%present_weather                       > 0)         write (unit_, '("dwd_weather_report.present_weather: ", i0)')                        report%present_weather
        if (report%pressure_mean_sea_level               < huge(0.0)) write (unit_, '("dwd_weather_report.pressure_mean_sea_level: ", f0.1)')              report%pressure_mean_sea_level
        if (report%relative_humidity                     < huge(0.0)) write (unit_, '("dwd_weather_report.relative_humidity: ", f0.1)')                    report%relative_humidity
        if (report%water_temperature                     < huge(0.0)) write (unit_, '("dwd_weather_report.water_temperature: ", f0.1)')                    report%water_temperature
        if (report%temperature_5_cm                      < huge(0.0)) write (unit_, '("dwd_weather_report.temperature_5_cm: ", f0.1)')                     report%temperature_5_cm
        if (report%total_snow_depth                      < huge(0.0)) write (unit_, '("dwd_weather_report.total_snow_depth: ", f0.1)')                     report%total_snow_depth
        if (report%total_time_sunshine_last_hour         < huge(0.0)) write (unit_, '("dwd_weather_report.total_time_sunshine_last_hour: ", f0.1)')        report%total_time_sunshine_last_hour
        if (report%total_time_sunshine_last_day          < huge(0.0)) write (unit_, '("dwd_weather_report.total_time_sunshine_last_day: ", f0.1)')         report%total_time_sunshine_last_day
    end subroutine dm_dwd_weather_report_out

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    logical function dwd_weather_report_has_value_int32(attribute) result(has)
        !! Returns `.true.` if attribute is a valid value.
        integer, intent(in) :: attribute !! DWD weather report attribute.

        has = (attribute > 0)
    end function dwd_weather_report_has_value_int32

    logical function dwd_weather_report_has_value_real32(attribute) result(has)
        !! Returns `.true.` if attribute is a valid value.
        real, intent(in) :: attribute !! DWD weather report attribute.

        has = (attribute < huge(0.0))
    end function dwd_weather_report_has_value_real32
end module dm_dwd
