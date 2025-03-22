! dmtestdp.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestdp
    !! Test program for data points handling.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestdp'
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
        integer, parameter :: N = 10

        integer       :: i
        type(dp_type) :: dps(N)

        stat = TEST_FAILED

        do i = 1, N
            dps(i)%x = dm_time_now()
            dps(i)%y = modulo(i * 0.01 * PI, PI)

            print *, '"' // dm_dp_to_string(dps(i)) // '"'
        end do

        stat = TEST_PASSED
    end function test01
end program dmtestdp
