! dmtestutil.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestutil
    !! Test program for utility procedures.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestutil%dm_test01', dm_test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
        character(len=:), allocatable :: str

        stat = TEST_FAILED

        str = dm_ftoa(123.123)
        print *, 'dm_ftoa() = ', str
        if (str /= '123.12300') return

        str = dm_ftoa(123.123_i8)
        print *, 'dm_ftoa() = ', str
        if (str /= '123.12300') return

        str = dm_ftoa(-123.123)
        print *, 'dm_ftoa() = ', str
        if (str /= '-123.12300') return

        str = dm_ftoa(-123.123_i8)
        print *, 'dm_ftoa() = ', str
        if (str /= '-123.12300') return

        str = dm_ftoa(123.123123123_i8)
        print *, 'dm_ftoa() = ', str
        if (str /= '123.12312') return

        str = dm_ftoa(-123.123123123_i8)
        print *, 'dm_ftoa() = ', str
        if (str /= '-123.12312') return

        str = dm_ftoa(987654321.12345678)
        print *, 'dm_ftoa() = ', str
        if (str /= '9.87654336E+8') return

        str = dm_ftoa(987654321.12345678_i8)
        print *, 'dm_ftoa() = ', str
        if (str /= '9.87654321E+8') return

        str = dm_ftoa(-987654321.12345678)
        print *, 'dm_ftoa() = ', str
        if (str /= '-9.87654336E+8') return

        str = dm_ftoa(-987654321.12345678_i8)
        print *, 'dm_ftoa() = ', str
        if (str /= '-9.87654321E+8') return

        stat = TEST_PASSED
    end function dm_test01
end program dmtestutil
