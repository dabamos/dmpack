! dmtesttime.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesttime
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtesttime%dm_test01', dm_test01)
    tests(2) = test_type('dmtesttime%dm_test02', dm_test02)
    tests(3) = test_type('dmtesttime%dm_test03', dm_test03)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
        character(len=TIME_LEN) :: timestamp
        integer                 :: rc
        integer                 :: mseconds, timezone
        integer(kind=i8)        :: seconds

        stat = TEST_FAILED

        timestamp = dm_time_now()
        rc = dm_time_to_seconds(timestamp, seconds, mseconds, timezone)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print '(" ISO 8601: ", a)',  timestamp
        print '(" seconds.: ", i0)', seconds
        print '(" mseconds: ", i0)', mseconds
        print '(" timezone: ", i0)', timezone

        stat = TEST_PASSED
    end function dm_test01

    logical function dm_test02() result(stat)
        integer(kind=i8) :: mseconds

        stat = TEST_FAILED

        mseconds = dm_time_mseconds()
        print '(" mseconds: ", i0)', mseconds

        stat = TEST_PASSED
    end function dm_test02

    logical function dm_test03() result(stat)
        stat = TEST_FAILED

        print *, 'Validating timestamp ...'

        if (.not. dm_time_valid('1970-01-01T00:00:00.000+00:00')) return
        if (.not. dm_time_valid('1970-01-01T00:00:00.000-00:00')) return
        if (.not. dm_time_valid('1970-01-01T00:00:00.000')) return
        if (.not. dm_time_valid('1970-01-01T00:00:00')) return
        if (.not. dm_time_valid('1970-01-01T')) return
        if (.not. dm_time_valid('1970')) return

        if (dm_time_valid('1970-01-01T00:00:00.000+00:00 UTC')) return
        if (dm_time_valid('1970/01/01T00:00:00.000+00:00')) return
        if (dm_time_valid('1970-01-01 00:00:00.000+00:00')) return
        if (dm_time_valid('1970-01-01T00:00:00.000 00:00')) return
        if (dm_time_valid('19')) return

        stat = TEST_PASSED
    end function dm_test03
end program dmtesttime
