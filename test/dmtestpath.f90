! dmtestpath.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestpath
    !! Test program for path routines.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestpath'
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
end program dmtestpath
