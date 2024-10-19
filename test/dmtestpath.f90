! dmtestpath.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestpath
    !! Test program for path routines.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestpath'
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
        character(len=:), allocatable :: path, parsed

        stat = TEST_FAILED

        path = '/tmp/%Y-%M-%DT%h-%m-%s_report.html'

        parsed = dm_path_parsed(path)
        print *, 'Path:   ', path
        print *, 'Parsed: ', parsed

        if (len(path) /= 34) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        stat = TEST_FAILED

        print *, 'Joining paths ...'

        if (dm_path_join('a', 'b')  /= 'a/b') return
        if (dm_path_join('a/', 'b') /= 'a/b') return
        if (dm_path_join('a', '/b') /= 'a/b') return
        if (dm_path_join('a', '')   /= 'a')   return
        if (dm_path_join('', 'b')   /= 'b')   return
        if (dm_path_join('', '')    /= '')    return

        stat = TEST_PASSED
    end function test02
end program dmtestpath
