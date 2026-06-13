! dmtestprocess.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestprocess
    !! Test program for network module.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestprocess'
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
        type(process_type) :: process

        stat = TEST_FAILED

        call dm_process_init(process, 'dmdummy', '/opt/bin/dmdummy', '/opt/config/dmdummy.conf')
        call dm_process_out(process)
        if (dm_process_is_running(process)) return
        call dm_process_destroy(process)

        stat = TEST_PASSED
    end function test01
end program dmtestprocess
