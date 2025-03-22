! dmteststring.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmteststring
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmteststring'
    integer,          parameter :: NTESTS    = 5

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04), &
        test_type('test05', test05)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=:), allocatable :: a
        integer                       :: n

        stat = TEST_FAILED

        print *, 'Counting character ...'
        a = repeat('A' // ASCII_LF, 3)
        n = dm_string_count_char(a, ASCII_LF)
        if (n /= 3) return

        print *, 'Counting lines ...'
        n = dm_string_count_lines(a)
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

    logical function test03() result(stat)
        character(len=:), allocatable :: a, b, c, d

        stat = TEST_FAILED

        allocate (character(len=0) :: b)
        allocate (character(len=1) :: c)
        allocate (character(len=1) :: d)

        c = ' '
        d = 'X'

        print *, 'Testing for emptiness ...'

        if (.not. dm_string_is_empty())  return
        if (.not. dm_string_is_empty(a)) return
        if (.not. dm_string_is_empty(b)) return
        if (.not. dm_string_is_empty(c)) return
        if (dm_string_is_empty(d))       return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        stat = TEST_FAILED

        print *, 'Testing starts-with function ...'

        if (.not. dm_string_starts_with('aaa', 'a'))   return
        if (.not. dm_string_starts_with('aaa', 'aaa')) return
        if (dm_string_starts_with('', ''))             return
        if (dm_string_starts_with('aaa', ''))          return
        if (dm_string_starts_with('', 'aaa'))          return
        if (dm_string_starts_with('a', 'aaa'))         return

        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        integer          :: rc
        integer(kind=i4) :: v4
        integer(kind=i8) :: v8

        stat = TEST_FAILED

        print *, 'Converting hex to integer ...'
        call dm_string_hex_to_int('0x0',        v4, rc); if (dm_is_error(rc)) return; if (v4 /= 0)        return
        call dm_string_hex_to_int('0xFF',       v4, rc); if (dm_is_error(rc)) return; if (v4 /= 255)      return
        call dm_string_hex_to_int('0xA068',     v4, rc); if (dm_is_error(rc)) return; if (v4 /= 41064)    return
        call dm_string_hex_to_int('0x7FFFFFFF', v4, rc); if (dm_is_error(rc)) return; if (v4 /= huge(v4)) return

        call dm_string_hex_to_int('0x0',                v8, rc); if (dm_is_error(rc)) return; if (v8 /= 0_i8)     return
        call dm_string_hex_to_int('0xA068',             v8, rc); if (dm_is_error(rc)) return; if (v8 /= 41064_i8) return
        call dm_string_hex_to_int('0x7FFFFFFFFFFFFFFF', v8, rc); if (dm_is_error(rc)) return; if (v8 /= huge(v8)) return

        call dm_string_hex_to_int('0', v4, rc); if (.not. dm_is_error(rc)) return
        call dm_string_hex_to_int('0', v8, rc); if (.not. dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test05
end program dmteststring
