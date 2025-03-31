! dmtestplot.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestplot
    !! Disabled the tests of this program by setting the following environment
    !! variable:
    !!
    !!      DM_PIPE_SKIP - Skip all tests.
    !!
    !! For example:
    !!
    !!      $ export DM_PIPE_SKIP=1
    !!      $ ./dmtestplot
    !!
    !! This may be necessary on test platforms where Gnuplot or bi-directional
    !! pipes are not available.
    !!
    !! If _gnuplot(1)_ is not available under the name `gnuplot`, set an alias:
    !!
    !!      $ alias gnuplot="gnuplot-nox"
    !!
    !! Otherwise, the tests in this program will fail.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestplot'
    integer,          parameter :: NTESTS    = 1

    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=:), allocatable :: output
        integer                       :: rc
        integer(kind=i8)              :: n
        type(plot_type)               :: plot
        type(dp_type)                 :: dps(3)

        stat = TEST_PASSED
        if (dm_test_skip('DM_PIPE_SKIP')) return

        stat = TEST_FAILED
        call dm_plot_set(plot       = plot, &
                         terminal   = PLOT_TERMINAL_SVG, &
                         title      = 'Dummy Plot', &
                         bidirect   = .true., &
                         background = 'white')

        dps(1) = dp_type('1970-01-01T00:00:00.000+00:00', 123.123_8)
        dps(2) = dp_type('1970-01-01T00:01:00.000+00:00', 223.123_8)
        dps(3) = dp_type('1970-01-01T00:02:00.000+00:00',  23.123_8)

        print '(" Library: ", a)', dm_plot_version(.true.)
        print *, 'Writing to stdin ...'
        rc = dm_plot_lines(plot, dps)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading from stdout ...'
        rc = dm_plot_read(plot, output, n)
        call dm_plot_close(plot)
        if (dm_is_error(rc)) return

        print '(" nbytes: ", i0)', n
        print '(" nbytes: ", i0)', len(output)
        if (n == 0 .or. n /= len(output)) return

        stat = TEST_PASSED
    end function test01
end program dmtestplot
