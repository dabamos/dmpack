! dmtestroff.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestroff
    !! Test program for formatted output with GNU roff and -ms macro package.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: PDF_FILE  = 'testreport.pdf'
    character(len=*), parameter :: TEST_NAME = 'dmtestroff'
    integer,          parameter :: NTESTS    = 3

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    no_color = dm_env_has('NO_COLOR')

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        stat = TEST_FAILED

        print '(a)', dm_roff_header(title='Test Report', author='Sensor Node 1', institution='University of Elbonia', &
                                    font_family=ROFF_FONT_HELVETICA) // &
                     dm_roff_lp('The first paragraph.')

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=:), allocatable :: roff
        integer                       :: rc

        stat = TEST_PASSED
        if (dm_test_skip('DM_PIPE_SKIP')) return

        stat = TEST_FAILED
        print *, 'Generating markup ...'
        roff = dm_roff_header(title='Test Report', author='Sensor Node 1', institution='University of Elbonia', &
                              font_family=ROFF_FONT_HELVETICA, left_footer=dm_time_date(), &
                              right_footer='DMPACK ' // DM_VERSION_STRING) // &
               dm_roff_sh(1, 'Results') // &
               dm_roff_lp('UTF-8: äöüß€')

        if (dm_file_exists(PDF_FILE)) then
            print *, 'Deleting stale file ...'
            call dm_file_delete(PDF_FILE)
        end if

        print *, 'Creating PDF ...'
        rc = dm_roff_make_pdf(roff, PDF_FILE)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Validating ...'
        if (.not. dm_file_exists(PDF_FILE)) return
        if (dm_file_size(PDF_FILE) == 0) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        integer :: rc

        stat = TEST_PASSED
        if (dm_test_skip('DM_PIPE_SKIP')) return

        stat = TEST_FAILED
        test_block: block
            character(len=:), allocatable :: pic, roff
            character(len=TIME_LEN)       :: timestamp
            integer                       :: i
            type(plot_type)               :: plot
            type(dp_type)                 :: dps(60)

            print *, 'Generating time series ...'

            do i = 1, size(dps)
                write (timestamp, '("2025-01-01T00:", i0.2, ":00.000000+00:00")') i
                dps(i) = dp_type(timestamp, 10 * sin(i * 0.1_r8))
            end do

            call dm_plot_set(plot       = plot,               &
                             terminal   = PLOT_TERMINAL_GPIC, &
                             title      = 'Dummy Plot',       &
                             bidirect   = .true.,             &
                             background = 'white',            &
                             foreground = 'black',            &
                             xlabel     = 'Time',             &
                             ylabel     = ' ')

            print *, 'Plotting ...'
            rc = dm_plot_lines(plot, dps)
            rc = dm_plot_read(plot, pic)
            call dm_plot_close(plot)
            if (dm_is_error(rc)) exit test_block

            print *, 'Generating markup ...'
            roff = dm_roff_header(title='Test Report', author='Sensor Node 1', institution='University of Elbonia', &
                                  font_family=ROFF_FONT_HELVETICA, left_footer=dm_time_date(), &
                                  right_footer='DMPACK ' // DM_VERSION_STRING) // &
                   dm_roff_sh(1, 'Plot') // &
                   dm_roff_lp('') // &
                   pic

            if (dm_file_exists(PDF_FILE)) then
                print *, 'Deleting stale file ...'
                call dm_file_delete(PDF_FILE)
            end if

            print *, 'Creating PDF ...'
            rc = dm_roff_make_pdf(roff, PDF_FILE, pic=.true.)
            if (dm_is_error(rc)) exit test_block

            rc = E_EMPTY
            if (dm_file_size(PDF_FILE) == 0) exit test_block

            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test03
end program dmtestroff
