! dmtestdwd.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestdwd
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestdwd'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=*), parameter :: CATALOG    = 'share/dmdwd/catalog.cfg'
        character(len=*), parameter :: STATION_ID = 'Y0209'
        integer,          parameter :: NSTATIONS  = 5995

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

        print *, 'Writing (subset of) station catalog ...'
        print '(72("."))'
        call dm_dwd_mosmix_station_catalog_write(stations(1:5), unit=stdout, header=.true.)
        print '(72("."))'

        stat = TEST_PASSED
    end function test01
end program dmtestdwd
