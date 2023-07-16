! dmtestdp.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestdp
    !! Test program for data points handling.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestdp%dm_test01', dm_test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
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
    end function dm_test01
end program dmtestdp
