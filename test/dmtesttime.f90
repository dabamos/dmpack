! dmtesttime.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesttime
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtesttime'
    integer,          parameter :: NTESTS    = 7

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04), &
        test_type('test05', test05), &
        test_type('test06', test06), &
        test_type('test07', test07)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=TIME_LEN) :: timestamp
        integer                 :: rc
        integer                 :: useconds
        integer(kind=i8)        :: seconds

        stat = TEST_FAILED

        timestamp = dm_time_now()
        rc = dm_time_to_unix(timestamp, seconds, useconds)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print '(" ISO 8601: ", a)',  timestamp
        print '(" seconds.: ", i0)', seconds
        print '(" useconds: ", i0)', useconds

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        stat = TEST_FAILED

        print '(" seconds: ", i0)',  dm_time_unix()
        print '(" mseconds: ", i0)', dm_time_unix_mseconds()

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        stat = TEST_FAILED

        print *, 'Validating timestamps ...'

        if (.not. dm_time_is_valid('1970-01-01T00:00:00.000000+00:00')) return
        if (.not. dm_time_is_valid('1970-01-01T00:00:00.000000-00:00')) return
        if (.not. dm_time_is_valid('1970-01-01T00:00:00.000'))          return
        if (.not. dm_time_is_valid('1970-01-01T00:00:00'))              return
        if (.not. dm_time_is_valid('1970-01-01T'))                      return
        if (.not. dm_time_is_valid('1970'))                             return

        if (dm_time_is_valid('1970-01-01T00:00:00.000+00:00'))        return
        if (dm_time_is_valid('1970-01-01T00:00:00.000000Z'))          return
        if (dm_time_is_valid('1970-01-01T00:00:00.000000+00:00 UTC')) return
        if (dm_time_is_valid('1970/01/01T00:00:00.000000+00:00'))     return
        if (dm_time_is_valid('1970-01-01 00:00:00.000000+00:00'))     return
        if (dm_time_is_valid('1970-01-01T00:00:00.000000 00:00'))     return
        if (dm_time_is_valid('19'))                                   return

        if (.not. dm_time_is_valid('1970-01-01T00:00:00.000000+00:00', strict=.true.)) return
        if (.not. dm_time_is_valid('1970-01-01T00:00:00.000000-00:00', strict=.true.)) return

        if (dm_time_is_valid('1970-01-01T00:00:00.000', strict=.true.)) return
        if (dm_time_is_valid('1970-01-01T00:00:00',     strict=.true.)) return

        print *, 'All tests passed'

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

    logical function test07() result(stat)
        character(len=TIME_LEN) :: time1
        character(len=25)       :: time2

        stat = TEST_FAILED

        print *, 'Stripping useconds ...'

        time1 = '2023-09-10T20:30:30.123456+00:00'
        time2 = dm_time_strip_useconds(time1)

        print *, 'Full:  ', time1
        print *, 'Short: ', time2

        if (time2 /= '2023-09-10T20:30:30+00:00') return

        stat = TEST_PASSED
    end function test07
end program dmtesttime
