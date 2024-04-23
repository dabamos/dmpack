! dmteststring.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmteststring
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmteststring'
    integer,          parameter :: NTESTS    = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=:), allocatable :: a, b
        integer                       :: n

        stat = TEST_FAILED

        print *, 'Counting character ...'
        a = repeat('A' // ASCII_LF, 3)
        n = dm_string_count_char(a, ASCII_LF)
        if (n /= 3) return

        print *, 'Counting lines ...'
        n = dm_string_count_lines(a)
        if (n /= 4) return

        print *, 'Counting characters (with quoting) ...'
        b = '100,3.141,"foo,",,"bar"'
        n = dm_string_count_char(b, ',', quote='"')
        if (n /= 4) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer,          parameter :: EXP_I32 = 123456789_i4
        integer(kind=i8), parameter :: EXP_I64 = 123456789123456789_i8
        real,             parameter :: EXP_R32 = 12345.12345_r4
        real(kind=r8),    parameter :: EXP_R64 = 12345.123456789_r8

        character(len=:), allocatable :: a
        integer                       :: rc1, rc2

        integer          :: i32
        integer(kind=i8) :: i64
        real             :: r32
        real(kind=r8)    :: r64

        stat = TEST_FAILED

        print *, 'string from and to int32 ...'
        call dm_string_from(EXP_I32, a, rc1)
        call dm_string_to(a, i32, rc2)
        if (dm_is_error(rc1) .or. dm_is_error(rc2)) return
        if (i32 /= EXP_I32) return

        print *, 'string from and to int64 ...'
        call dm_string_from(EXP_I64, a, rc1)
        call dm_string_to(a, i64, rc2)
        if (dm_is_error(rc1) .or. dm_is_error(rc2)) return
        if (i64 /= EXP_I64) return

        print *, 'string from and to real32 ...'
        call dm_string_from(EXP_R32, a, rc1)
        call dm_string_to(a, r32, rc2)
        if (dm_is_error(rc1) .or. dm_is_error(rc2)) return
        if (.not. dm_equals(r32, EXP_R32)) return

        print *, 'string from and to real64 ...'
        call dm_string_from(EXP_R64, a, rc1)
        call dm_string_to(a, r64, rc2)
        if (dm_is_error(rc1) .or. dm_is_error(rc2)) return
        if (.not. dm_equals(r64, EXP_R64)) return

        stat = TEST_PASSED
    end function test02
end program dmteststring
