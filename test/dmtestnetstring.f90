! dmtestnetstring.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestnetstring
    !! Test program for netstring parsing.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestnetstring'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(*), parameter :: RAW = 'abcdefghi'
        character(*), parameter :: NS  = '9:abcdefghi,'

        character(32) :: input, output
        character(8)  :: buffer
        integer       :: last, n, rc

        stat = TEST_FAILED

        input = NS
        print *, 'Reading netstring ' // trim(input) // ' ...'
        call dm_netstring_read(input(1:12), output, length=n, last=last, error=rc)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return
        if (n /= 9) return
        if (output(1:n) /= RAW) return
        if (last /= 12) return

        input = RAW
        print *, 'Writing netstring ' // trim(input) // ' ...'
        call dm_netstring_write(input(1:9), output, length=n, error=rc)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return
        if (n /= 12) return
        if (output(1:n) /= NS) return

        input = RAW
        print *, 'Validating ...'
        call dm_netstring_write(input(1:9), buffer, error=rc)
        if (rc /= E_BOUNDS) return

        stat = TEST_PASSED
    end function test01
end program dmtestnetstring
