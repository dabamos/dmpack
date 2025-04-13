! dmtestutil.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestutil
    !! Test program for utility procedures.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestutil'
    integer,          parameter :: NTESTS    = 4

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
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
        character(len=*), parameter :: FMT = '(1x, "2^", i2, " B = ", i19, " B = ", a)'

        integer          :: b4, i
        integer(kind=i8) :: b8

        stat = TEST_FAILED

        print *, 'Printing sizes in human readable format ...'
        print *, '32-bit signed integers ...'

        do i = 1, 30
            b4 = 2**i
            print FMT, i, b4, dm_size_to_human(b4)
        end do

        print *, '64-bit signed integers ...'

        do i = 1, 62
            b8 = 2_i8**i
            print FMT, i, b8, dm_size_to_human(b8)
        end do

        print *, 'Validating ...'

        if (dm_size_to_human(2_i8**37) /= '128.0 GiB') return
        if (dm_size_to_human(2_i8**62) /= '4.0 EiB')   return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        character(len=*), parameter :: FMT = '(1x, a, " = ", 3(i4))'
        character(len=7) :: hex
        integer          :: r, g, b

        stat = TEST_FAILED

        print *, 'Hex to RGB ...'
        hex = '#FF0000'
        call dm_hex_to_rgb(hex, r, g, b)
        print FMT, hex, r, g, b
        if (r /= 255 .or. g /= 0 .or. b /= 0) return

        print *, 'RGB to hex ...'
        r = 0; g = 0; b = 255
        call dm_rgb_to_hex(r, g, b, hex)
        print FMT, hex, r, g, b
        if (hex /= '#0000FF') return

        stat = TEST_PASSED
    end function test04
end program dmtestutil
