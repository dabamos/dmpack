! dmtestdwd.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestdwd
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestdwd'
    integer,          parameter :: NTESTS    = 3

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    no_color = dm_env_has('NO_COLOR')

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, no_color)
contains
    logical function test01() result(stat)
        character(len=*), parameter :: CATALOG    = './share/dmdwd/catalog.cfg'
        character(len=*), parameter :: STATION_ID = '10281' ! Airport Trollenhagen.
        integer,          parameter :: NSTATIONS  = 5995    ! Records in catalog.

        integer                                    :: rc, unit
        logical                                    :: found
        real(kind=r8)                              :: dt
        type(dwd_mosmix_station_type)              :: station
        type(dwd_mosmix_station_type), allocatable :: stations(:)
        type(timer_type)                           :: timer

        stat = TEST_FAILED

        if (.not. dm_file_exists(CATALOG)) then
            print *, 'Catalog file ' // CATALOG // ' not found'
            return
        end if

        print *, 'Reading MOSMIX station catalog from ' // CATALOG // ' ...'

        open (action='read', file=CATALOG, iostat=rc, newunit=unit, status='old')
        if (rc /= 0) return

        call dm_timer_start(timer)
        rc = dm_dwd_mosmix_station_catalog_read(stations, unit)
        call dm_timer_stop(timer, duration=dt)

        close (unit)
        print '(" Loaded ", i0, " stations in ", f0.6, " sec")', size(stations), dt

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        if (size(stations) /= NSTATIONS) then
            print '(" Error: expected ", i0, " stations")', NSTATIONS
            return
        end if

        print *, 'Validating ...'
        if (stations(1)%id   /= '01001')             return
        if (stations(1)%icao /= 'ENJA')              return
        if (stations(1)%name /= 'JAN MAYEN')         return
        if (.not. dm_equals(stations(1)%lat, 70.56)) return
        if (.not. dm_equals(stations(1)%lon, -8.40)) return
        if (stations(1)%elev /= 10)                  return

        print *, 'Searching for station ' // STATION_ID // ' ...'

        call dm_timer_start(timer)
        rc = dm_dwd_mosmix_station_find(stations, STATION_ID, station, found)
        call dm_timer_stop(timer, duration=dt)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        if (.not. found) return
        print '(" Found station ", a, " in ", f0.6, " sec")', STATION_ID, dt

        print *, 'Validating ...'
        if (station%id /= STATION_ID) return

        print *, 'Printing station ...'
        print '(72("."))'
        call dm_dwd_mosmix_station_out(station)
        print '(72("."))'

        print *, 'Writing first entries of station catalog ...'
        print '(72("."))'
        call dm_dwd_mosmix_station_catalog_write(stations(1:5), unit=stdout, header=.true.)
        print '(72("."))'

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=*), parameter :: INPUT = 'test/test_poi.csv'

        integer                                    :: iostat, rc, unit
        type(dwd_weather_report_type), allocatable :: reports(:)

        stat = TEST_FAILED

        if (.not. dm_file_exists(INPUT)) then
            print *, 'Test file ' // INPUT // ' not found'
            return
        end if

        print *, 'Reading weather report records from ' // INPUT // ' ...'

        open (action='read', file=INPUT, iostat=iostat, newunit=unit, status='old')
        if (iostat /= 0) return
        rc = dm_dwd_weather_report_read(reports, unit, header=.true.)
        close (unit)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return
        if (size(reports) /= 25) return

        print *, 'Printing report ...'
        print '(72("."))'
        call dm_dwd_weather_report_out(reports(1))
        print '(72("."))'

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        character(len=*), parameter :: STATION_ID = '10385' ! Airport Berlin-Brandenburg.

        integer :: iostat, rc, unit
        logical :: enabled

        stat = TEST_PASSED

        rc = dm_env_get('DM_DWD_API', enabled, default=.false.)

        if (.not. enabled) then
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("> Set environment variable DM_DWD_API to 1. This test will be skipped.")'
            call dm_ansi_reset(no_color)
            return
        end if

        stat = TEST_FAILED

        rc = dm_rpc_init()
        if (dm_is_error(rc)) return

        print *, 'Opening scratch file ...'
        ! open (access='stream', action='readwrite', form='unformatted', iostat=iostat, newunit=unit, status='scratch')
        open (action='readwrite', form='formatted', iostat=iostat, newunit=unit, status='scratch')
        if (iostat /= 0) return

        rpc_block: block
            character(len=TIME_LEN)       :: timestamp
            character(len=:), allocatable :: url

            integer(kind=i8)        :: epoch
            real(kind=r8)           :: dt
            type(rpc_request_type)  :: request
            type(rpc_response_type) :: response
            type(timer_type)        :: timer

            type(dwd_weather_report_type), allocatable :: reports(:)

            rc = E_INVALID
            print *, 'Creating URL to records of Airport Berlin-Brandenburg ...'
            url = dm_dwd_api_weather_report_url(id=STATION_ID, tls=.false.)
            if (len(url) == 0) exit rpc_block

            print *, 'Fetching ' // url // ' ...'
            response%unit = unit

            call dm_timer_start(timer)
            rc = dm_rpc_get(request, response, url, callback=dm_dwd_api_callback)
            call dm_timer_stop(timer, duration=dt)

            if (dm_is_error(rc)) then
                print '(" HTTP ", i0, ": ", a)', response%code, response%error_message
                exit rpc_block
            end if

            print '(" Finished in ", f0.6, " sec")', dt

            if (response%last_modified > 0) then
                rc = dm_time_from_unix(response%last_modified, timestamp)
                print '(" Last modified: ", a)', timestamp
            end if

            rewind (unit)

            print *, 'Reading weather report records ...'
            rc = dm_dwd_weather_report_read(reports, response%unit)
            if (dm_is_error(rc)) exit rpc_block

            print '(" Printing report ", i0, " of ", i0, " ...")', 1, size(reports)
            print '(72("."))'
            call dm_dwd_weather_report_out(reports(1))
            print '(72("."))'

            print *, 'Fetching ' // url // ' ...'
            epoch = 1000 + response%last_modified
            rc = dm_rpc_get(request, response, url, modified_since=epoch, callback=dm_dwd_api_callback)
            print '(" HTTP ", i0)', response%code
            if (response%code /= HTTP_NOT_MODIFIED) return
            rc = dm_time_from_unix(epoch, timestamp)
            print *, 'File has not been modified since ' // timestamp
        end block rpc_block

        close (unit)
        call dm_rpc_shutdown()

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test03
end program dmtestdwd
