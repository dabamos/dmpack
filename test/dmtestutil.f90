! dmtestutil.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestutil
    !! Test program for utility procedures.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestutil'
    integer,          parameter :: NTESTS    = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=:), allocatable :: str

        stat = TEST_FAILED

        str = dm_ftoa(123.123)
        print *, 'dm_ftoa() = ', str
        if (str /= '123.123001099') return

        str = dm_ftoa(123.123_r8)
        print *, 'dm_ftoa() = ', str
        if (str /= '123.123000000') return

        str = dm_ftoa(-123.123)
        print *, 'dm_ftoa() = ', str
        if (str /= '-123.123001099') return

        str = dm_ftoa(-123.123_r8)
        print *, 'dm_ftoa() = ', str
        if (str /= '-123.123000000') return

        str = dm_ftoa(123.123123123_r8)
        print *, 'dm_ftoa() = ', str
        if (str /= '123.123123123') return

        str = dm_ftoa(-123.123123123_r8)
        print *, 'dm_ftoa() = ', str
        if (str /= '-123.123123123') return

        str = dm_ftoa(987654321.12345678)
        print *, 'dm_ftoa() = ', str
        if (str /= '987654336.000') return

        str = dm_ftoa(987654321.12345678_r8)
        print *, 'dm_ftoa() = ', str
        if (str /= '987654321.123') return

        str = dm_ftoa(-987654321.12345678)
        print *, 'dm_ftoa() = ', str
        if (str /= '-987654336.000') return

        str = dm_ftoa(-987654321.12345678_r8)
        print *, 'dm_ftoa() = ', str
        if (str /= '-987654321.123') return

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

    logical function test03() result(stat)
        integer          :: rc
        integer(kind=i4) :: v4
        integer(kind=i8) :: v8

        stat = TEST_FAILED

        print *, 'Converting hex to integer ...'
        call dm_hex_to_int('0x0',        v4, rc); if (dm_is_error(rc)) return; if (v4 /= 0)        return
        call dm_hex_to_int('0xFF',       v4, rc); if (dm_is_error(rc)) return; if (v4 /= 255)      return
        call dm_hex_to_int('0xA068',     v4, rc); if (dm_is_error(rc)) return; if (v4 /= 41064)    return
        call dm_hex_to_int('0x7FFFFFFF', v4, rc); if (dm_is_error(rc)) return; if (v4 /= huge(v4)) return

        call dm_hex_to_int('0x0',                v8, rc); if (dm_is_error(rc)) return; if (v8 /= 0_i8)     return
        call dm_hex_to_int('0xA068',             v8, rc); if (dm_is_error(rc)) return; if (v8 /= 41064_i8) return
        call dm_hex_to_int('0x7FFFFFFFFFFFFFFF', v8, rc); if (dm_is_error(rc)) return; if (v8 /= huge(v8)) return

        call dm_hex_to_int('0', v4, rc); if (.not. dm_is_error(rc)) return
        call dm_hex_to_int('0', v8, rc); if (.not. dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test03
end program dmtestutil
