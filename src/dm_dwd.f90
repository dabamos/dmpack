! Author:  Philipp Engel
! Licence: ISC
module dm_dwd
    !! I/O module for meteorological data of Deutscher Wetterdienst (DWD).
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_time
    use :: dm_util, only: dm_present
    implicit none (type, external)
    private

    character(len=*), parameter :: DWD_MOSMIX_STATION_FMT = '(a5, 1x, a4, 1x, a20, 1x, f6.2, 1x, f7.2, 1x, i5)'

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
        character(len=TIME_LEN) :: timestamp                      = TIME_DEFAULT !! Date and time [UTC].
        integer                 :: last_weather1                  = 0            !! Past weather 1 [code].
        integer                 :: last_weather2                  = 0            !! Past weather 2 [code].
        integer                 :: present_weather                = 0            !! Present weather [code].
        real                    :: cloud_cover                    = huge(0.0)    !! Cloud cover total [%].
        real                    :: lowest_cloud_above_station     = huge(0.0)    !! Height of base of lowest cloud above station [m].
        real                    :: diffuse_radiation_last_hour    = huge(0.0)    !! Diffuse solar radiation last hour [W/m^2].
        real                    :: direct_radiation_last_hour     = huge(0.0)    !! Direct solar radiation last hour [W/m^2].
        real                    :: direct_radiation_last_24h      = huge(0.0)    !! Direct solar radiation last 24 hours [W/m^2].
        real                    :: global_radiation_last_hour     = huge(0.0)    !! Global radiation last hour [W/m^2].
        real                    :: global_radiation_last_24h      = huge(0.0)    !! Global radiation last 24 hours [W/m^2].
        real                    :: evaporation_last_24h           = huge(0.0)    !! Evaporation/evapotranspiration last 24 hours [mm].
        real                    :: horizontal_visibility          = huge(0.0)    !! Horizontal visibility [km].
        real                    :: precipitation_last_hour        = huge(0.0)    !! Precipitation amount last hour [mm].
        real                    :: precipitation_last_3h          = huge(0.0)    !! Precipitation amount last 3 hours [mm].
        real                    :: precipitation_last_6h          = huge(0.0)    !! Precipitation amount last 6 hours [mm].
        real                    :: precipitation_last_12h         = huge(0.0)    !! Precipitation last 12 hours [mm].
        real                    :: precipitation_last_24h         = huge(0.0)    !! Precipitation amount last 24 hours [mm].
        real                    :: pressure_mean_sea_level        = huge(0.0)    !! Pressure reduced to mean sea level [hPa].
        real                    :: relative_humidity              = huge(0.0)    !! Relative humidity [%].
        real                    :: temperature_5cm                = huge(0.0)    !! Temperature at 5 cm above ground [°C].
        real                    :: temperature_mean_prev_day      = huge(0.0)    !! Daily mean of temperature previous day [°C].
        real                    :: min_temperature_last_12h_5cm   = huge(0.0)    !! Minimum temperature last 12 hours 5 cm above ground [°C].
        real                    :: min_temperature_last_12h_2m    = huge(0.0)    !! Minimum temperature last 12 hours 2 meters above ground [°C].
        real                    :: min_temperature_prev_day       = huge(0.0)    !! Minimum of temperature for previous day [°C].
        real                    :: min_temperature_prev_day_5cm   = huge(0.0)    !! Minimum of temperature at 5 cm above ground for previous day [°C].
        real                    :: max_temperature_last_12h_2m    = huge(0.0)    !! Maximum temperature last 12 hours 2 meters above ground [°C].
        real                    :: max_temperature_prev_day       = huge(0.0)    !! Maximum of temperature for previous day [°C].
        real                    :: dew_point_temperature_2m       = huge(0.0)    !! Dew point temperature at 2 meters above ground [°C].
        real                    :: dry_bulb_temperature_2m        = huge(0.0)    !! Dry bulb temperature at 2 meters above ground [°C].
        real                    :: depth_new_snow                 = huge(0.0)    !! Depth of new snow [cm].
        real                    :: total_snow_depth               = huge(0.0)    !! Total snow depth [cm].
        real                    :: total_time_sunshine_last_day   = huge(0.0)    !! Total time of sunshine last day [h].
        real                    :: total_time_sunshine_last_hour  = huge(0.0)    !! Total time of sunshine during last hour [min].
        real                    :: water_temperature              = huge(0.0)    !! Sea/water temperature [°C].
        real                    :: wind_dir_mean_last_10min_10m   = huge(0.0)    !! Mean wind direction during last 10 min at 10 meters above ground [°].
        real                    :: wind_speed_mean_last_10min_10m = huge(0.0)    !! Mean wind speed during last 10 min at 10 meters above ground [km/h].
        real                    :: max_wind_speed_last_hour       = huge(0.0)    !! Maximum wind speed last hour [km/h].
        real                    :: max_wind_speed_last_6h         = huge(0.0)    !! Maximum wind speed during last 6 hours [km/h].
        real                    :: max_wind_speed_mean_last_hour  = huge(0.0)    !! Maximum wind speed as 10 minutes mean during last hour [km/h].
        real                    :: max_wind_speed_mean_prev_day   = huge(0.0)    !! Maximum of 10 minutes mean of wind speed for previous day [km/h].
        real                    :: max_wind_speed_prev_day        = huge(0.0)    !! Maximum wind speed for previous day [km/h].
    end type dwd_weather_report_type

    interface dwd_read_value
        module procedure :: dwd_read_value_integer
        module procedure :: dwd_read_value_real
    end interface dwd_read_value

    interface dm_dwd_weather_report_has_value
        module procedure :: dwd_weather_report_has_value_int32
        module procedure :: dwd_weather_report_has_value_real32
    end interface dm_dwd_weather_report_has_value

    public :: dm_dwd_is_weather_report_valid
    public :: dm_dwd_mosmix_station_catalog_read
    public :: dm_dwd_mosmix_station_catalog_write
    public :: dm_dwd_mosmix_station_find
    public :: dm_dwd_mosmix_station_out
    public :: dm_dwd_weather_report_has_value
    public :: dm_dwd_weather_report_out
    public :: dm_dwd_weather_report_read

    private :: dwd_read_value
    private :: dwd_read_value_integer
    private :: dwd_read_value_real
    private :: dwd_weather_report_has_value_int32
    private :: dwd_weather_report_has_value_real32
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental logical function dm_dwd_is_weather_report_valid(report) result(valid)
        !! Returns `.true.` if weather report has a valid timestamp that is not
        !! `TIME_DEFAULT`.
        type(dwd_weather_report_type), intent(in) :: report !! Weather report type.

        valid = (report%timestamp /= TIME_DEFAULT .and. dm_time_is_valid(report%timestamp))
    end function dm_dwd_is_weather_report_valid

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
        !! Or, use local catalog file `share/dmdwd/catalog.cfg`.
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
            if (stat /= 0) return

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
        use :: dm_string, only: dm_to_upper

        type(dwd_mosmix_station_type), intent(inout)         :: stations(:) !! MOSMIX stations.
        character(len=*),              intent(in)            :: id          !! Station id.
        type(dwd_mosmix_station_type), intent(out), optional :: station     !! MOSMIX station of id.
        logical,                       intent(out), optional :: found       !! Station found.

        character(len=DWD_MOSMIX_STATION_ID_LEN) :: id_
        integer                                  :: i, loc

        if (present(found)) found = .false.

        rc = E_EMPTY
        if (size(stations) == 0) return

        loc = 0
        id_ = dm_to_upper(id)

        do i = 1, size(stations)
            if (id_ /= stations(i)%id) cycle
            loc = i
            exit
        end do

        rc = E_NOT_FOUND
        if (loc == 0) return

        rc = E_NONE
        if (present(station)) station = stations(loc)
        if (present(found))   found   = .true.
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
            if (stat /= 0) return

            if (header_ .and. nlines < 4) cycle read_loop
            if (len_trim(line) == 0) exit read_loop

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

                ! Read weather report record data.
                call dwd_read_value(fields( 3), report%cloud_cover)
                call dwd_read_value(fields( 4), report%temperature_mean_prev_day)
                call dwd_read_value(fields( 5), report%depth_new_snow)
                call dwd_read_value(fields( 6), report%dew_point_temperature_2m)
                call dwd_read_value(fields( 7), report%diffuse_radiation_last_hour)
                call dwd_read_value(fields( 8), report%direct_radiation_last_24h)
                call dwd_read_value(fields( 9), report%direct_radiation_last_hour)
                call dwd_read_value(fields(10), report%dry_bulb_temperature_2m)
                call dwd_read_value(fields(11), report%evaporation_last_24h)
                call dwd_read_value(fields(12), report%global_radiation_last_hour)
                call dwd_read_value(fields(13), report%global_radiation_last_24h)
                call dwd_read_value(fields(14), report%lowest_cloud_above_station)
                call dwd_read_value(fields(15), report%horizontal_visibility)
                call dwd_read_value(fields(16), report%max_wind_speed_mean_prev_day)
                call dwd_read_value(fields(17), report%max_temperature_prev_day)
                call dwd_read_value(fields(18), report%max_temperature_last_12h_2m)
                call dwd_read_value(fields(19), report%max_wind_speed_mean_last_hour)
                call dwd_read_value(fields(20), report%max_wind_speed_last_6h)
                call dwd_read_value(fields(21), report%max_wind_speed_prev_day)
                call dwd_read_value(fields(22), report%max_wind_speed_last_hour)
                call dwd_read_value(fields(23), report%wind_dir_mean_last_10min_10m)
                call dwd_read_value(fields(24), report%wind_speed_mean_last_10min_10m)
                call dwd_read_value(fields(25), report%min_temperature_prev_day_5cm)
                call dwd_read_value(fields(26), report%min_temperature_prev_day)
                call dwd_read_value(fields(27), report%min_temperature_last_12h_2m)
                call dwd_read_value(fields(28), report%min_temperature_last_12h_5cm)
                call dwd_read_value(fields(29), report%last_weather1)
                call dwd_read_value(fields(30), report%last_weather2)
                call dwd_read_value(fields(31), report%precipitation_last_24h)
                call dwd_read_value(fields(32), report%precipitation_last_3h)
                call dwd_read_value(fields(33), report%precipitation_last_6h)
                call dwd_read_value(fields(34), report%precipitation_last_hour)
                call dwd_read_value(fields(35), report%precipitation_last_12h)
                call dwd_read_value(fields(36), report%present_weather)
                call dwd_read_value(fields(37), report%pressure_mean_sea_level)
                call dwd_read_value(fields(38), report%relative_humidity)
                call dwd_read_value(fields(39), report%water_temperature)
                call dwd_read_value(fields(40), report%temperature_5cm)
                call dwd_read_value(fields(41), report%total_snow_depth)
                call dwd_read_value(fields(42), report%total_time_sunshine_last_hour)
                call dwd_read_value(fields(43), report%total_time_sunshine_last_day)
            end associate

            call move_alloc(from=buffer, to=reports)
        end do read_loop

        rc = E_NONE
        if (size(reports) == 0) rc = E_EMPTY
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

        if (report%last_weather1                  > 0)         write (unit_, '("dwd_weather_report.last_weather1: ", i0)')                       report%last_weather1
        if (report%last_weather2                  > 0)         write (unit_, '("dwd_weather_report.last_weather2: ", i0)')                       report%last_weather2
        if (report%present_weather                > 0)         write (unit_, '("dwd_weather_report.present_weather: ", i0)')                     report%present_weather
        if (report%cloud_cover                    < huge(0.0)) write (unit_, '("dwd_weather_report.cloud_cover: ", f0.1)')                       report%cloud_cover
        if (report%lowest_cloud_above_station     < huge(0.0)) write (unit_, '("dwd_weather_report.lowest_cloud_above_station: ", f0.1)')        report%lowest_cloud_above_station
        if (report%diffuse_radiation_last_hour    < huge(0.0)) write (unit_, '("dwd_weather_report.diffuse_radiation_last_hour: ", f0.1)')       report%diffuse_radiation_last_hour
        if (report%direct_radiation_last_hour     < huge(0.0)) write (unit_, '("dwd_weather_report.direct_radiation_last_hour: ", f0.1)')        report%direct_radiation_last_hour
        if (report%direct_radiation_last_24h      < huge(0.0)) write (unit_, '("dwd_weather_report.direct_radiation_last_24h: ", f0.1)')         report%direct_radiation_last_24h
        if (report%global_radiation_last_hour     < huge(0.0)) write (unit_, '("dwd_weather_report.global_radiation_last_hour: ", f0.1)')        report%global_radiation_last_hour
        if (report%global_radiation_last_24h      < huge(0.0)) write (unit_, '("dwd_weather_report.global_radiation_last_24h: ", f0.1)')         report%global_radiation_last_24h
        if (report%evaporation_last_24h           < huge(0.0)) write (unit_, '("dwd_weather_report.evaporation_last_24h: ", f0.1)')              report%evaporation_last_24h
        if (report%horizontal_visibility          < huge(0.0)) write (unit_, '("dwd_weather_report.horizontal_visibility: ", f0.1)')             report%horizontal_visibility
        if (report%precipitation_last_hour        < huge(0.0)) write (unit_, '("dwd_weather_report.precipitation_last_hour: ", f0.1)')           report%precipitation_last_hour
        if (report%precipitation_last_3h          < huge(0.0)) write (unit_, '("dwd_weather_report.precipitation_last_3h: ", f0.1)')             report%precipitation_last_3h
        if (report%precipitation_last_6h          < huge(0.0)) write (unit_, '("dwd_weather_report.precipitation_last_6h: ", f0.1)')             report%precipitation_last_6h
        if (report%precipitation_last_12h         < huge(0.0)) write (unit_, '("dwd_weather_report.precipitation_last_12h: ", f0.1)')            report%precipitation_last_12h
        if (report%precipitation_last_24h         < huge(0.0)) write (unit_, '("dwd_weather_report.precipitation_last_24h: ", f0.1)')            report%precipitation_last_24h
        if (report%pressure_mean_sea_level        < huge(0.0)) write (unit_, '("dwd_weather_report.pressure_mean_sea_level: ", f0.1)')           report%pressure_mean_sea_level
        if (report%relative_humidity              < huge(0.0)) write (unit_, '("dwd_weather_report.relative_humidity: ", f0.1)')                 report%relative_humidity
        if (report%temperature_5cm                < huge(0.0)) write (unit_, '("dwd_weather_report.temperature_5cm: ", f0.1)')                   report%temperature_5cm
        if (report%temperature_mean_prev_day      < huge(0.0)) write (unit_, '("dwd_weather_report.temperature_mean_prev_day: ", f0.1)')         report%temperature_mean_prev_day
        if (report%min_temperature_last_12h_5cm   < huge(0.0)) write (unit_, '("dwd_weather_report.min_temperature_last_12h_5cm: ", f0.1)')      report%min_temperature_last_12h_5cm
        if (report%min_temperature_last_12h_2m    < huge(0.0)) write (unit_, '("dwd_weather_report.min_temperature_last_12h_2m: ", f0.1)')       report%min_temperature_last_12h_2m
        if (report%min_temperature_prev_day       < huge(0.0)) write (unit_, '("dwd_weather_report.min_temperature_prev_day: ", f0.1)')          report%min_temperature_prev_day
        if (report%min_temperature_prev_day_5cm   < huge(0.0)) write (unit_, '("dwd_weather_report.min_temperature_prev_day_5cm: ", f0.1)')      report%min_temperature_prev_day_5cm
        if (report%max_temperature_last_12h_2m    < huge(0.0)) write (unit_, '("dwd_weather_report.max_temperature_last_12h_2m: ", f0.1)')       report%max_temperature_last_12h_2m
        if (report%max_temperature_prev_day       < huge(0.0)) write (unit_, '("dwd_weather_report.max_temperature_prev_day: ", f0.1)')          report%max_temperature_prev_day
        if (report%dew_point_temperature_2m       < huge(0.0)) write (unit_, '("dwd_weather_report.dew_point_temperature_2m: ", f0.1)')          report%dew_point_temperature_2m
        if (report%dry_bulb_temperature_2m        < huge(0.0)) write (unit_, '("dwd_weather_report.dry_bulb_temperature_2m: ", f0.1)')           report%dry_bulb_temperature_2m
        if (report%total_snow_depth               < huge(0.0)) write (unit_, '("dwd_weather_report.total_snow_depth: ", f0.1)')                  report%total_snow_depth
        if (report%depth_new_snow                 < huge(0.0)) write (unit_, '("dwd_weather_report.depth_new_snow: ", f0.1)')                    report%depth_new_snow
        if (report%total_time_sunshine_last_day   < huge(0.0)) write (unit_, '("dwd_weather_report.total_time_sunshine_last_day: ", f0.1)')      report%total_time_sunshine_last_day
        if (report%total_time_sunshine_last_hour  < huge(0.0)) write (unit_, '("dwd_weather_report.total_time_sunshine_last_hour: ", f0.1)')     report%total_time_sunshine_last_hour
        if (report%water_temperature              < huge(0.0)) write (unit_, '("dwd_weather_report.water_temperature: ", f0.1)')                 report%water_temperature
        if (report%wind_dir_mean_last_10min_10m   < huge(0.0)) write (unit_, '("dwd_weather_report.wind_dir_mean_last_10min_10m: ", f0.1)')      report%wind_dir_mean_last_10min_10m
        if (report%wind_speed_mean_last_10min_10m < huge(0.0)) write (unit_, '("dwd_weather_report.wind_speed_mean_last_10min_10m: ", f0.1)')    report%wind_speed_mean_last_10min_10m
        if (report%max_wind_speed_last_hour       < huge(0.0)) write (unit_, '("dwd_weather_report.max_wind_speed_last_hour: ", f0.1)')          report%max_wind_speed_last_hour
        if (report%max_wind_speed_last_6h         < huge(0.0)) write (unit_, '("dwd_weather_report.max_wind_speed_last_6h: ", f0.1)')            report%max_wind_speed_last_6h
        if (report%max_wind_speed_mean_last_hour  < huge(0.0)) write (unit_, '("dwd_weather_report.max_wind_speed_mean_last_hour: ", f0.1)')     report%max_wind_speed_mean_last_hour
        if (report%max_wind_speed_mean_prev_day   < huge(0.0)) write (unit_, '("dwd_weather_report.max_wind_speed_mean_prev_day: ", f0.1)')      report%max_wind_speed_mean_prev_day
        if (report%max_wind_speed_prev_day        < huge(0.0)) write (unit_, '("dwd_weather_report.max_wind_speed_prev_day: ", f0.1)')           report%max_wind_speed_prev_day
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

    pure subroutine dwd_read_value_integer(field, value)
        character(len=*), intent(in)  :: field !! Data field.
        integer,          intent(out) :: value !! Field value.

        integer :: stat

        value = 0
        if (field == '---') return
        read (field, *, iostat=stat) value
    end subroutine dwd_read_value_integer

    pure subroutine dwd_read_value_real(field, value)
        character(len=*), intent(in)  :: field !! Data field.
        real,             intent(out) :: value !! Field value.

        integer :: stat

        value = huge(0.0)
        if (field == '---') return
        read (field, *, decimal='comma', iostat=stat) value
    end subroutine dwd_read_value_real
end module dm_dwd
