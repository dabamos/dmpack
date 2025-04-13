! dmtestroff.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestroff
    !! Test program for formatted output with GNU roff and -ms macro package.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestroff'
    integer,          parameter :: NTESTS    = 4

    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        !! Tests markup generation and output.
        stat = TEST_FAILED

        print '(" Version: ", a)', dm_roff_version(name=.true.)

        print '(" Printing troff markup ...")'
        print '(72("."))'
        print '(a)', dm_roff_ms_header(title='Test Report', author='Sensor Node 1', institution='University of Elbonia', &
                                       font_family=ROFF_FONT_HELVETICA) // &
                     dm_roff_ms_lp('Now is the time for all good men to come to the aid of the party.') // &
                     dm_roff_pspic('/tmp/pic.eps', align='c', width=17.0, height=5.0) // &
                     ROFF_REQUEST_BP
        print '(72("."))'

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! Tests PDF output.
        character(len=*), parameter :: PDF_FILE = 'testroff1.pdf'

        character(len=:), allocatable :: roff
        integer                       :: rc

        stat = TEST_PASSED
        if (dm_test_skip('DM_PIPE_SKIP')) return

        stat = TEST_FAILED
        print *, 'Generating troff markup ...'
        roff = dm_roff_ms_header(title='Test Report', author='Sensor Node 1', institution='University of Elbonia', &
                                 font_family=ROFF_FONT_HELVETICA, left_footer=dm_time_date(), &
                                 right_footer='DMPACK ' // DM_VERSION_STRING) // &
               dm_roff_ms_sh(2, 'Results') // &
               dm_roff_ms_lp('UTF-8: äöüß€')

        if (dm_file_exists(PDF_FILE)) then
            print *, 'Deleting stale PDF file ' // PDF_FILE // ' ...'
            call dm_file_delete(PDF_FILE)
        end if

        print *, 'Creating PDF ...'
        rc = dm_roff_to_pdf(roff, PDF_FILE, preconv=.true.)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Validating ...'
        if (.not. dm_file_exists(PDF_FILE)) return
        if (dm_file_size(PDF_FILE) == 0) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        !! Tests PIC generation and PDF output.
        character(len=*), parameter :: PDF_FILE = 'testroff2.pdf'

        integer :: rc

        stat = TEST_PASSED
        if (dm_test_skip('DM_PIPE_SKIP')) return

        stat = TEST_FAILED
        test_block: block
            character(len=:), allocatable :: pic
            character(len=TIME_LEN)       :: timestamp
            integer                       :: i
            type(plot_type)               :: plot
            type(dp_type)                 :: dps(60)

            print *, 'Generating time series ...'

            do i = 1, size(dps)
                write (timestamp, '("2025-01-01T00:", i0.2, ":00.000000+00:00")') i
                dps(i) = dp_type(timestamp, 10 * sin(i * 0.1_r8))
            end do

            call dm_plot_set(plot, terminal=PLOT_TERMINAL_GPIC, title='Dummy Plot', bidirect=.true., &
                             background='white', foreground='black', xlabel='Time', ylabel=' ')

            print *, 'Plotting ...'
            rc = dm_plot_lines(plot, dps)
            call dm_error_out(rc)

            rc = dm_plot_read(plot, pic)
            call dm_error_out(rc)
            call dm_plot_close(plot)

            if (dm_file_exists(PDF_FILE)) then
                print *, 'Deleting stale PDF file ' // PDF_FILE // ' ...'
                call dm_file_delete(PDF_FILE)
            end if

            print *, 'Creating PDF ...'
            rc = dm_roff_to_pdf(pic, PDF_FILE, macro=ROFF_MACRO_NONE, pic=.true.)
            if (dm_is_error(rc)) exit test_block

            if (dm_file_size(PDF_FILE) == 0) rc = E_EMPTY
        end block test_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        !! Tests adding EPS image to PS document.
        character(len=*), parameter :: PDF_FILE = 'testroff3.pdf'

        character(len=:), allocatable :: eps_file, ps_file
        integer                       :: rc
        real(kind=r8)                 :: duration
        type(timer_type)              :: timer

        stat = TEST_PASSED
        if (dm_test_skip('DM_PIPE_SKIP')) return

        stat = TEST_FAILED
        eps_file = dm_uuid4() // '.eps'
        ps_file  = dm_uuid4() // '.ps'

        call dm_timer_start(timer)

        test_block: block
            character(len=:), allocatable :: roff
            character(len=8)              :: format(4, 1)
            character(len=32)             :: data(4, 2)
            character(len=TIME_LEN)       :: timestamp
            integer                       :: i
            type(plot_type)               :: plot
            type(dp_type)                 :: dps(60)

            print *, 'Generating time series ...'

            do i = 1, size(dps)
                write (timestamp, '("2025-01-01T00:", i0.2, ":00.000000+00:00")') i
                dps(i) = dp_type(timestamp, 10 * sin(i * 0.1_r8))
            end do

            call dm_plot_set(plot, terminal=PLOT_TERMINAL_POSTSCRIPT, width=17, height=5, output=eps_file, title='Dummy Plot', &
                             bidirect=.false., graph=' ', font='Helvetica', xlabel='Time', ylabel='Y')

            print *, 'Plotting to EPS file ' // eps_file // ' ...'
            rc = dm_plot_lines(plot, dps)
            call dm_plot_close(plot)
            if (.not. dm_file_exists(eps_file)) return

            if (dm_file_exists(PDF_FILE)) then
                print *, 'Deleting stale PDF file ' // PDF_FILE // ' ...'
                call dm_file_delete(PDF_FILE)
            end if

            format = reshape([ character(len=8)  :: 'lb', 'l', 'lb', 'l' ], [ 4, 1 ])
            data   = reshape([ character(len=32) :: 'Node ID:',   'dummy-node', 'From:', TIME_DEFAULT, &
                                                    'Node Name:', 'Dummy Node', 'To:',   dm_time_now() ], [ 4, 2 ])

            print *, 'Generating troff markup ...'
            roff = dm_roff_ms_header(title='Test Report', author='Dummy Node', institution='University of Elbonia', &
                                     font_family=ROFF_FONT_HELVETICA, font_size=10, center_header=TEST_NAME, &
                                     left_footer='DMPACK ' // DM_VERSION_STRING, right_footer=dm_time_date(), &
                                     page_one=.true.)
            roff = roff // dm_roff_defcolor('gray', 128, 128, 128)
            roff = roff // dm_roff_tbl(format, data)
            roff = roff // dm_roff_ms_sh(2, 'Results ' // dm_roff_m('gray', dm_roff_s(1, '\|Sensor 1', rel='-')))
            roff = roff // dm_roff_ms_lp('Now is the time for all good men to come to the aid of the party.')
            roff = roff // dm_roff_pspic(eps_file)

            print '(" Printing troff markup ...")'
            print '(72("."))'
            print '(a)', roff
            print '(72("."))'

            print *, 'Creating PS file ' // ps_file // ' ...'
            rc = dm_roff_to_ps(roff, ps_file, macro=ROFF_MACRO_MS, preconv=.true., tbl=.true.)
            if (dm_is_error(rc)) exit test_block

            print *, 'Converting PS file ' // ps_file // ' to PDF file ' // PDF_FILE // ' ...'
            rc = dm_roff_ps_to_pdf(ps_file, PDF_FILE)
            if (dm_is_error(rc)) exit test_block

            if (dm_file_size(PDF_FILE) == 0) rc = E_EMPTY
        end block test_block

        call dm_timer_stop(timer, duration=duration)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        write (*, '(" Generated report ", a, " in ", f0.3, " sec")') PDF_FILE, duration

        print *, 'Deleting EPS file ' // eps_file // ' ...'
        call dm_file_delete(eps_file)

        print *, 'Deleting PS file ' // ps_file // ' ...'
        call dm_file_delete(ps_file)

        stat = TEST_PASSED
    end function test04
end program dmtestroff
