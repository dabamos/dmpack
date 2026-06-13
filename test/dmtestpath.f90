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
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        stat = TEST_FAILED

        print *, 'Joining paths ...'

        if (dm_path_join('a', 'b')  /= 'a/b') return
        if (dm_path_join('a/', 'b') /= 'a/b') return
        if (dm_path_join('a', '/b') /= 'a/b') return
        if (dm_path_join('a', '')   /= 'a')   return
        if (dm_path_join('', 'b')   /= 'b')   return
        if (dm_path_join('', '')    /= '')    return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        stat = TEST_FAILED

        print *, 'Extracting file names from paths ...'

        if (dm_path_name('        ') /= '')   return
        if (dm_path_name('ls      ') /= 'ls') return
        if (dm_path_name('/bin/ls ') /= 'ls') return
        if (dm_path_name('bin/ls  ') /= 'ls') return
        if (dm_path_name('/bin/ls/') /= '')   return

        stat = TEST_PASSED
    end function test02
end program dmtestpath
