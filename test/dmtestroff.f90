! dmtestroff.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestroff
    !! Test program for formatted output with GNU Troff and ms macro package.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestroff'
    integer,          parameter :: NTESTS    = 1

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    no_color = dm_env_has('NO_COLOR')

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        stat = TEST_FAILED

        print '(a)', dm_roff_header(title='Test Report', author='DMPACK', institution='University of Elbonia', &
                                    font_family=ROFF_FONT_HELVETICA)
        print '(a)', dm_roff_lp('The first paragraph.')

        stat = TEST_PASSED
    end function test01
end program dmtestroff
