! dmtestfilter.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestfilter
    !! Test program for IIR filters.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestfilter'
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
        integer, parameter :: N   = 100
        integer, parameter :: ORD = 4

        integer  :: i
        real(r8) :: d1(N), d2(N)
        real(r8) :: cutoff, fs, t, ts

        stat = TEST_FAILED

        cutoff = 50.0_r8
        fs = 1000.0_r8
        ts = 1.0_r8 / fs

        ! Generate test signal: 20 Hz sine and 200 Hz sine.
        do i = 1, N
            t     = (i - 1) * ts
            d1(i) = sin(2.0_r8 * PI *  20.0_r8 * t) + &
                    sin(2.0_r8 * PI * 200.0_r8 * t)
        end do

        d2 = d1
        call dm_filter_low_pass(d2, ORD, cutoff, ts, .false.)

        do i = 1, N
            print '(1x, i4, 2(1x, f12.8))', i, d1(i), d2(i)
        end do

        if (.not. dm_equals(d2(N), -0.9291101969195006_r8)) return

        stat = TEST_PASSED
    end function test01
end program dmtestfilter
