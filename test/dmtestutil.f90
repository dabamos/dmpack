! dmtestutil.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestutil
    !! Test program for utility procedures.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestutil.test01', test01)
    tests(2) = test_type('dmtestutil.test02', test02)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
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
    end function test01

    logical function test02() result(stat)
        integer(kind=i4) :: ival4
        integer(kind=i8) :: ival8
        logical          :: lval1
        real(kind=r4)    :: rval4

        stat = TEST_FAILED

        print *, 'Converting real64 to types ...'

        call dm_from_real64(1.1_r8, ival4)
        call dm_from_real64(1.1_r8, ival8)
        call dm_from_real64(1.1_r8, lval1)
        call dm_from_real64(1.1_r8, rval4)

        print *, 'Validating ...'
        if (ival4 /= 1_i4) return
        if (ival8 /= 1_i8) return
        if (.not. lval1) return
        if (.not. dm_equals(rval4, 1.1_r4)) return

        stat = TEST_PASSED
    end function test02
end program dmtestutil
