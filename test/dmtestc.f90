! dmtestc.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestc
    !! Test program for C interoperability.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestc'
    integer,          parameter :: NTESTS    = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        stat = TEST_FAILED

        print *, 'Converting logical values ...'
        if (.not. dm_c_f_logical(1))      return
        if (dm_c_f_logical(0))            return
        if (dm_f_c_logical(.true.)  /= 1) return
        if (dm_f_c_logical(.false.) /= 0) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=*), parameter :: FMT = '(" unsigned: ", i12, " signed: ", i12)'

        integer(kind=u2) :: uv2
        integer(kind=u4) :: uv4
        integer(kind=i4) :: sv4
        integer(kind=i8) :: sv8

        stat = TEST_FAILED

        print *, 'Converting unsigned to signed ...'

        uv2 = 0_u2
        sv4 = dm_to_signed(uv2)
        print FMT, uv2, sv4
        if (sv4 /= 0) return

        uv2 = dm_to_unsigned(32767_i4)
        sv4 = dm_to_signed(uv2)
        print FMT, uv2, sv4
        if (sv4 /= 32767_i4) return

        uv2 = dm_to_unsigned(65535_i4)
        sv4 = dm_to_signed(uv2)
        print FMT, uv2, sv4
        if (sv4 /= 65535_i4) return

        uv4 = 0_u4
        sv8 = dm_to_signed(uv4)
        print FMT, uv4, sv8
        if (sv8 /= 0) return

        uv4 = dm_to_unsigned(2147483648_i8)
        sv8 = dm_to_signed(uv4)
        print FMT, uv4, sv8
        if (sv8 /= 2147483648_i8) return

        uv4 = dm_to_unsigned(4294967295_i8)
        sv8 = dm_to_signed(uv4)
        print FMT, uv4, sv8
        if (sv8 /= 4294967295_i8) return

        stat = TEST_PASSED
    end function test02
end program dmtestc
