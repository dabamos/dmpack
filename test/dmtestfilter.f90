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
        integer, parameter :: N     = 1000
        integer, parameter :: ORDER = 4

        integer  :: i
        real(r8) :: d1(N), d2(N)
        real(r8) :: cutoff, fs, t, ts

        stat = TEST_FAILED
        print *, 'Testing low-pass Butterworth filter ...'

        cutoff = 50.0_r8     ! Cut-off frequency.
        fs     = 1000.0_r8   ! Sampling frequency.
        ts     = 1.0_r8 / fs ! Time step.

        ! Generate test signal: 20 Hz sine + 200 Hz sine.
        do i = 1, N
            t     = (i - 1) * ts
            d1(i) = sin(2.0_r8 * PI * 20.0_r8 * t) + sin(2.0_r8 * PI * 200.0_r8 * t)
        end do

        d2 = d1
        call dm_filter_low_pass(FILTER_BUTTERWORTH, d2, ORDER, cutoff, ts, .false.)

        ! Print input and output data.
        ! do i = 1, N
        !     print '(1x, i4, 2(1x, f12.8))', i, d1(i), d2(i)
        ! end do

        ! Must equal XAPiir output.
        if (.not. dm_equals(d2(N), -0.92910927356616424_r8)) return

        stat = TEST_PASSED
    end function test01
end program dmtestfilter
