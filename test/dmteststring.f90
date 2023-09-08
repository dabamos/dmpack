! dmteststring.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmteststring
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmteststring.test01', test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
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
end program dmteststring
