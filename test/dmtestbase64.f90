! dmtestbase64.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestbase64
    !! Test program for Base64 encoding.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestbase64'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'), compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=*), parameter :: INPUT = &
            'Now is the time for all good men to come to the aid of the party.'
        character(len=*), parameter :: ASSERT = &
            'Tm93IGlzIHRoZSB0aW1lIGZvciBhbGwgZ29vZCBtZW4gdG8gY29tZSB0byB0aGUgYWlkIG9mIHRoZSBwYXJ0eS4='

        character(len=:), allocatable :: output

        stat = TEST_FAILED

        call dm_base64_encode(INPUT, output)

        print *, 'Checking encoded string ...'
        if (output /= ASSERT) return

        stat = TEST_PASSED
    end function test01
end program dmtestbase64
