! dmtestdp.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestdp
    !! Test program for data points handling.
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
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        integer, parameter :: N = 10

        integer       :: i
        type(dp_type) :: dps(N)

        stat = TEST_FAILED

        do i = 1, N
            dps(i)%x = dm_time_now()
            dps(i)%y = dm_random_get_uniform(-10e18_r8, 0.0_r8)

            print *, '"' // dm_dp_to_string(dps(i)) // '"'
        end do

        print *, len(dm_dp_to_string(dps(1))), len_trim(dm_dp_to_string(dps(1)))

        do i = 1, N
            dps(i)%x = dm_time_now()
            dps(i)%y = dm_random_get_uniform(10000.0_r8, 10000000.0_r8)

            print *, '"' // dm_dp_to_string(dps(i)) // '"'
        end do

        print *, len(dm_dp_to_string(dps(1))), len_trim(dm_dp_to_string(dps(1)))

        stat = TEST_PASSED
    end function test01
end program dmtestdp
