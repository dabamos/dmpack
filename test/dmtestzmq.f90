! dmtestzmq.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestzmq
    !! Test program for ZeroMQ access.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestzmq'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        stat = TEST_FAILED

        print '(" Version: ", a)', dm_zmq_version()
        print '(" Version: ", a)', dm_zmq_version(.true.)

        stat = TEST_PASSED
    end function test01
end program dmtestzmq
