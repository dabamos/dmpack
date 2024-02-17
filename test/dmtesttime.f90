! dmtesttime.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesttime
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 6

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtesttime.test01', test01)
    tests(2) = test_type('dmtesttime.test02', test02)
    tests(3) = test_type('dmtesttime.test03', test03)
    tests(4) = test_type('dmtesttime.test04', test04)
    tests(5) = test_type('dmtesttime.test05', test05)
    tests(6) = test_type('dmtesttime.test06', test06)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=TIME_LEN) :: timestamp
        integer                 :: rc
        integer                 :: useconds
        integer(kind=i8)        :: seconds

        stat = TEST_FAILED

        timestamp = dm_time_now()
        rc = dm_time_to_unix(timestamp, seconds, useconds)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print '(" ISO 8601: ", a)',  timestamp
        print '(" seconds.: ", i0)', seconds
        print '(" useconds: ", i0)', useconds

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer(kind=i8) :: mseconds

        stat = TEST_FAILED

        mseconds = dm_time_mseconds()
        print '(" mseconds: ", i0)', mseconds

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        stat = TEST_FAILED

        print *, 'Validating timestamp ...'

        if (.not. dm_time_valid('1970-01-01T00:00:00.000000+00:00')) return
        if (.not. dm_time_valid('1970-01-01T00:00:00.000000-00:00')) return
        if (.not. dm_time_valid('1970-01-01T00:00:00.000')) return
        if (.not. dm_time_valid('1970-01-01T00:00:00')) return
        if (.not. dm_time_valid('1970-01-01T')) return
        if (.not. dm_time_valid('1970')) return

        if (dm_time_valid('1970-01-01T00:00:00.000+00:00')) return
        if (dm_time_valid('1970-01-01T00:00:00.000000+00:00 UTC')) return
        if (dm_time_valid('1970/01/01T00:00:00.000000+00:00')) return
        if (dm_time_valid('1970-01-01 00:00:00.000000+00:00')) return
        if (dm_time_valid('1970-01-01T00:00:00.000000 00:00')) return
        if (dm_time_valid('19')) return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        character(len=8)        :: beats1, beats2
        character(len=TIME_LEN) :: time1, time2
        integer                 :: rc
        integer(kind=i8)        :: unix1, unix2

        stat = TEST_FAILED

        ! Timestamp 1
        print *, 'Time to Epoch ...'
        time1 = '2023-09-10T20:30:30.000000+00:00'

        rc = dm_time_to_unix(time1, unix1)
        if (dm_is_error(rc)) return

        rc = dm_time_to_beats(time1, beats1)
        if (dm_is_error(rc)) return

        print *, time1, unix1, beats1

        ! Timestamp 2
        print *, 'Time to Epoch ...'
        time2 = '2023-09-10T22:30:30.000000+02:00'

        rc = dm_time_to_unix(time2, unix2)
        if (dm_is_error(rc)) return

        rc = dm_time_to_beats(time2, beats2)
        if (dm_is_error(rc)) return

        print *, time2, unix2, beats2

        ! Validation
        print *, 'Validating timestamps ...'
        if (time1 == time2) return
        if (unix1 /= unix2) return
        if (unix1 /= 1694377830_i8) return
        if (beats1 /= beats2) return

        time1 = dm_time_now()
        rc = dm_time_to_beats(time1, beats1)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Now: ', time1, ' (', trim(beats1), ')'

        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        character(len=TIME_LEN) :: time1, time2
        integer                 :: rc
        integer(kind=i8)        :: diff

        stat = TEST_FAILED

        time1 = '2023-09-10T20:30:30.000000+00:00'
        time2 = '2023-09-10T22:30:30.000000+02:00'

        rc = dm_time_diff(time1, time2, diff)

        print *, 'Time 1: ', time1
        print *, 'Time 2: ', time2
        print *, 'Diff:   ', diff

        if (dm_is_error(rc)) return
        if (diff /= 0) return

        time1 = '2023-09-10T20:30:30.000000+00:00'
        time2 = '2023-09-10T20:32:00.000000+00:00'

        rc = dm_time_diff(time1, time2, diff)

        print *, 'Time 1: ', time1
        print *, 'Time 2: ', time2
        print *, 'Diff:   ', diff

        if (dm_is_error(rc)) return
        if (diff /= 90) return

        stat = TEST_PASSED
    end function test05

    logical function test06() result(stat)
        character(len=:), allocatable :: diff_string
        character(len=TIME_LEN)       :: time1, time2
        integer                       :: rc
        integer                       :: u1, u2
        integer(kind=i8)              :: t1, t2
        integer(kind=i8)              :: diff
        type(time_delta_type)         :: time_delta

        stat = TEST_FAILED

        time1 = '2023-09-10T20:30:00.100000+01:00'
        time2 = '2023-09-10T20:32:00.200000+01:00'

        print *, 'Time 1: ', time1
        print *, 'Time 2: ', time2

        rc = dm_time_to_unix(time1, t1, u1)
        if (dm_is_error(rc)) return
        rc = dm_time_to_unix(time2, t2, u2)
        if (dm_is_error(rc)) return

        diff = abs(t2 - t1)

        print *, 'Epoch 1: ', t1, u1
        print *, 'Epoch 2: ', t2, u2
        print *, 'Diff:    ', diff

        if (diff /= 120) return

        rc = dm_time_diff(time1, time2, diff)

        print *, 'Time 1:  ', time1
        print *, 'Time 2:  ', time2
        print *, 'Diff:    ', diff

        if (dm_is_error(rc)) return
        if (diff /= 120) return

        call dm_time_delta_from_seconds(time_delta, diff)
        diff_string = dm_time_delta_to_string(time_delta)

        print *, 'String:  ', diff_string
        if (diff_string /= '0 days 0 hours 2 mins 0 secs') return

        stat = TEST_PASSED
    end function test06
end program dmtesttime
