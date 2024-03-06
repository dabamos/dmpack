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
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    call dm_init()
    tests = [ test_type('dmtestplot.test01', test01) ]
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=:), allocatable :: bytes
        integer                       :: rc
        integer(kind=i8)              :: sz
        type(plot_type)               :: plot
        type(dp_type)                 :: dps(3)

        stat = TEST_PASSED
        if (dm_test_skip('DM_PIPE_SKIP')) return

        stat = TEST_FAILED
        plot%term = PLOT_TERM_SVG
        ! plot%term = PLOT_TERM_PNG_CAIRO
        ! plot%font = '/usr/local/share/fonts/gnu-unifont-ttf/unifont.ttf'

        plot%title      = 'Dummy Plot'
        plot%bidirect   = .true.
        plot%background = 'white'

        dps(1) = dp_type('1970-01-01T00:00:00.000+00:00', 123.123_8)
        dps(2) = dp_type('1970-01-01T00:01:00.000+00:00', 223.123_8)
        dps(3) = dp_type('1970-01-01T00:02:00.000+00:00',  23.123_8)

        print *, 'Writing to stdin ...'
        rc = dm_plot_lines(plot, dps)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading from stdout ...'
        sz = dm_plot_read(plot, bytes)
        print *, '#size: ', sz
        print *, '#bytes: ', len_trim(bytes)
        if (sz == 0) return

        stat = TEST_PASSED
    end function test01
end program dmtestplot
