! dmtestpath.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestpath
    !! Test program for path routines.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestpath%dm_test01', dm_test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
        character(len=:), allocatable :: path, parsed

        stat = TEST_FAILED

        path = '/tmp/%Y-%M-%DT%h-%m-%s_report.html'

        parsed = dm_path_parsed(path)
        print *, 'Path:   ', path
        print *, 'Parsed: ', parsed

        if (len(path) /= 34) return

        stat = TEST_PASSED
    end function dm_test01
end program dmtestpath
