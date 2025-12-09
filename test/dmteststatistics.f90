! dmteststatistics.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmteststatistics
    !! Test program for statistics module.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmteststatistics'
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
        real(r8) :: mean, x(64)
        integer  :: i

        stat = TEST_FAILED

        do i = 1, size(x)
            x(i) = dble(i)
        end do

        print *, 'Testing mean value ...'
        mean = dm_statistics_mean(x)

        print *, 'Mean: ', mean
        if (.not. dm_equals(mean, 32.5_r8)) return

        stat = TEST_PASSED
    end function test01
end program dmteststatistics
