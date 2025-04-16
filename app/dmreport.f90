! dmreport.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmreport
    !! Generates reports in HTML, PDF, or PostScript format.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmreport'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 8

    character(len=*), parameter :: APP_SUFFIX_EPS = '.eps'             !! EPS file ending.
    character(len=*), parameter :: APP_SUFFIX_PS  = '.ps'              !! PS file ending.

    character(len=*), parameter :: APP_XLABEL    = 'Time'              !! Plot X label.
    character(len=*), parameter :: APP_TMP_DIR   = '/tmp'              !! Place of temporary files.
    integer,          parameter :: APP_ROFF_FONT = ROFF_FONT_HELVETICA !! GNU roff font name (PDF/PS).

    character(len=*), parameter :: APP_HTML_FONT        = 'Open Sans'  !! Gnuplot font name (HTML).
    integer,          parameter :: APP_HTML_PLOT_WIDTH  = 1000         !! Plot width for HTML [px].
    integer,          parameter :: APP_HTML_PLOT_HEIGHT = 400          !! Plot height for HTML [px].
    character(len=*), parameter :: APP_PS_FONT          = 'Helvetica'  !! Gnuplot font name (PDF/PS).
    integer,          parameter :: APP_PS_PLOT_WIDTH    = 17           !! Plot width for PDF/PS [cm].
    integer,          parameter :: APP_PS_PLOT_HEIGHT   = 6            !! Plot height for PDF/PS [cm].

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)        :: name   = APP_NAME !! Name of instance and configuration table.
        character(len=FILE_PATH_LEN) :: config = ' '      !! Path to configuration file.
        type(report_type)            :: report            !! Report settings.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments and configuration file options.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    select case (app%report%format)
        case (REPORT_FORMAT_HTML); call make_html(app%report, error=rc)
        case (REPORT_FORMAT_PDF);  call make_pdf (app%report, error=rc)
        case (REPORT_FORMAT_PS);   call make_ps  (app%report, error=rc)
        case default;              rc = E_INVALID
    end select

    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    subroutine make_html(report, error)
        !! Generates report in HTML format.
        type(report_type), intent(inout)         :: report !! Report type.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: unit, rc

        html_block: block
            character(len=:), allocatable :: inline_style, path
            integer                       :: i, n, stat
            type(node_type)               :: node

            allocate (character(len=0) :: inline_style)
            allocate (character(len=0) :: path)

            ! Open output file for writing.
            path = dm_path_parsed(report%output)
            open (action='write', file=path, iostat=stat, newunit=unit, status='replace')

            if (stat /= 0) then
                rc = E_IO
                call dm_error_out(rc, 'failed to open output file ' // path)
                exit html_block
            end if

            ! Read CSS from file.
            if (len_trim(report%style) > 0) then
                call dm_file_read(report%style, inline_style, error=rc)

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to read CSS file ' // report%style)
                    exit html_block
                end if
            end if

            ! Add HTML header with optional inline CSS.
            write (unit, '(a)') dm_html_header(report%title, report%subtitle, inline_style=inline_style)

            ! Add report overview table.
            rc = db_read_node(node, report%node, report%plot%database)

            if (dm_is_ok(rc)) then
                write (unit, '(34a)') H_NAV, H_TABLE, H_TBODY, &
                                      H_TR, H_TH, 'Node Name:', H_TH_END, H_TD, dm_html_encode(node%name),               H_TD_END,           &
                                            H_TH, 'From:',      H_TH_END, H_TD, dm_html_time(report%from, human=.true.), H_TD_END, H_TR_END, &
                                      H_TR, H_TH, 'Node ID:',   H_TH_END, H_TD, dm_html_encode(node%id),                 H_TD_END,           &
                                            H_TH, 'To:',        H_TH_END, H_TD, dm_html_time(report%to, human=.true.),   H_TD_END, H_TR_END, &
                                      H_TBODY_END, H_TABLE_END, H_NAV_END
            end if

            if (dm_is_error(rc) .and. report%verbose) write (unit, '(a)') dm_html_error(rc)

            ! Add optional report description.
            if (len_trim(report%meta) > 0) write (unit, '(a)') dm_html_p(dm_html_encode(report%meta))

            ! Add plots to HTML document if enabled.
            plot_block: block
                integer                    :: terminal
                type(dp_type), allocatable :: dps(:)

                if (report%plot%disabled) exit plot_block

                ! Add plot section heading and meta description.
                write (unit, '(a)') dm_html_heading(2, report%plot%title)
                if (len_trim(report%plot%meta) > 0) write (unit, '(a)') dm_html_p(dm_html_encode(report%plot%meta))

                if (.not. allocated(report%plot%observs)) exit plot_block
                n = size(report%plot%observs)

                ! Plot loop.
                do i = 1, n
                    associate (observ => report%plot%observs(i))
                        ! Skip if disabled.
                        if (observ%disabled) cycle

                        ! Read data points from observation database.
                        rc = db_read_dps(dps      = dps,                  &
                                         database = report%plot%database, &
                                         node     = report%node,          &
                                         sensor   = observ%sensor,        &
                                         target   = observ%target,        &
                                         response = observ%response,      &
                                         from     = report%from,          &
                                         to       = report%to)

                        if (dm_is_ok(rc) .or. report%verbose) then
                            write (unit, '(a)') dm_html_heading(3, observ%title, observ%subtitle)
                        end if

                        ! Handle errors.
                        if (rc == E_DB_NO_ROWS) then
                            if (report%verbose) write (unit, '(a)') dm_html_p('No observations found in database.')
                            cycle
                        end if

                        if (dm_is_error(rc)) then
                            if (report%verbose) write (unit, '(a)') dm_html_error(rc)
                            cycle
                        end if

                        ! Scale response values.
                        call dm_dp_scale(dps, observ%scale)

                        ! Get Gnuplot terminal name.
                        terminal = dm_plot_terminal_from_name(observ%format)

                        ! Add HTML plot figure.
                        write (unit, '(a)') html_plot(dps      = dps,             &
                                                      response = observ%response, &
                                                      unit     = observ%unit,     &
                                                      terminal = terminal,        &
                                                      title    = observ%title,    &
                                                      meta     = observ%meta,     &
                                                      color    = observ%color,    &
                                                      width    = observ%width,    &
                                                      height   = observ%height,   &
                                                      verbose  = report%verbose)
                    end associate
                end do
            end block plot_block

            ! Add table of logs to HTML document if enabled.
            log_block: block
                type(log_type), allocatable :: logs(:)

                ! Skip logs if disabled.
                if (report%log%disabled) exit log_block

                ! Read logs from database.
                rc = db_read_logs(logs      = logs,                 &
                                  database  = report%log%database,  &
                                  node      = report%node,          &
                                  from      = report%from,          &
                                  to        = report%to,            &
                                  min_level = report%log%min_level, &
                                  max_level = report%log%max_level)

                ! Add section heading.
                if (dm_is_ok(rc) .or. report%verbose) then
                    write (unit, '(a)') dm_html_heading(2, report%log%title)
                end if

                ! Handle errors.
                if (rc == E_DB_NO_ROWS) then
                    if (report%verbose) write (unit, '(a)') dm_html_p('No logs found in database.')
                    exit log_block
                end if

                if (dm_is_error(rc)) then
                    if (report%verbose) write (unit, '(a)') dm_html_error(rc)
                    exit log_block
                end if

                ! Add meta description.
                if (len_trim(report%log%meta) > 0) write (unit, '(a)') dm_html_p(dm_html_encode(report%log%meta))

                ! Add logs table.
                write (unit, '(a)') dm_html_logs(logs, node=.false.)
            end block log_block

            ! Add HTML footer.
            write (unit, '(12a)') H_FOOTER, H_HR, H_P, H_SMALL, &
                                  'This report was generated ', dm_html_time(dm_time_now(), human=.true.), ' by ', &
                                  dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, library=.true.), &
                                  H_SMALL_END, H_P_END, H_FOOTER_END, &
                                  dm_html_footer()
            rc = E_NONE
        end block html_block

        close (unit)
        if (present(error)) error = rc
    end subroutine make_html

    subroutine make_pdf(report, error)
        !! Generates report in PDF format.
        type(report_type), intent(inout)         :: report !! Report type.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: rc

        pdf_block: block
            character(len=FILE_PATH_LEN) :: pdf_file

            pdf_file = dm_path_parsed(report%output)
            call dm_file_touch(pdf_file, error=rc)
            if (dm_is_error(rc)) exit pdf_block

            report%output = temporary_file(APP_TMP_DIR, APP_SUFFIX_PS)
            call make_ps(report, error=rc)
            if (dm_is_error(rc)) exit pdf_block

            rc = dm_roff_ps_to_pdf(report%output, pdf_file)
            if (dm_is_error(rc)) exit pdf_block
            call dm_file_delete(report%output)
        end block pdf_block

        if (present(error)) error = rc
    end subroutine make_pdf

    subroutine make_ps(report, error)
        !! Generates report in PostScript format.
        type(report_type), intent(inout)         :: report !! Report type.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: rc

        ps_block: block
            character(len=*), parameter :: RULE  = ROFF_REQUEST_BR // ROFF_ESC_MVUP // ROFF_ESC_HR // ASCII_LF
            character(len=*), parameter :: SUB   = 'sub'
            integer,          parameter :: SUB_R = 128, SUB_G = 128, SUB_B = 128

            character(len=:),             allocatable :: path, roff
            character(len=FILE_PATH_LEN), allocatable :: eps_files(:)
            integer                                   :: i, n

            allocate (eps_files(0))

            ! Create output file.
            path = dm_path_parsed(report%output)
            call dm_file_touch(path, error=rc)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to open output file ' // path)
                exit ps_block
            end if

            ! Add document header.
            roff_block: block
                integer, parameter :: NCOL = 4, NFMT = 1, NROW = 2

                character(len=TIME_DATE_LEN) :: date
                character(len=32)            :: data(NCOL, NROW)
                character(len=2)             :: format(NCOL, NFMT)
                type(node_type)              :: node

                ! Read node from database.
                rc = db_read_node(node, report%node, report%plot%database)

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to read ' // trim(report%node) // ' from database ' // report%plot%database)
                    exit ps_block
                end if

                ! Create MS header and define colour.
                date = dm_time_date()
                roff = dm_roff_ms_header(title=report%title, author=report%subtitle, institution=date, font_family=APP_ROFF_FONT, font_size=10, &
                                         left_header=report%title, center_header=report%subtitle, right_header=date, &
                                         left_footer='DMPACK ' // DM_VERSION_STRING, center_footer=ROFF_ESC_ENDASH // ' % ' // ROFF_ESC_ENDASH) // &
                       dm_roff_defcolor(SUB, SUB_R, SUB_G, SUB_B)

                ! Add report overview table.
                format = reshape([ character(len=2)  :: 'lb', 'l', 'lb', 'l' ], [ NCOL, NFMT ])
                data   = reshape([ character(len=32) :: 'Node Name:', node%name, 'From:', dm_time_to_human(report%from), &
                                                        'Node ID:',   node%id,   'To:',   dm_time_to_human(report%to) ], &
                                 [ NCOL, NROW ])

                roff = roff // dm_roff_tbl(format, data) // dm_roff_ms_lp(report%meta)
            end block roff_block

            ! Add plots.
            plot_block: block
                type(plot_type)            :: plot
                type(dp_type), allocatable :: dps(:)

                if (report%plot%disabled)                 exit plot_block
                if (.not. allocated(report%plot%observs)) exit plot_block

                n = size(report%plot%observs)
                if (allocated(eps_files)) deallocate (eps_files)
                allocate (eps_files(n), source=repeat(' ', FILE_PATH_LEN))

                ! Add plot section title and meta description.
                roff = roff // dm_roff_ms_sh(2, report%plot%title) // RULE // &
                               dm_roff_ms_lp(report%plot%meta)

                ! Plot subsection loop.
                do i = 1, n
                    associate (observ => report%plot%observs(i))
                        ! Skip if disabled.
                        if (observ%disabled) cycle

                        ! Read data points from observation database.
                        rc = db_read_dps(dps      = dps,                  &
                                         database = report%plot%database, &
                                         node     = report%node,          &
                                         sensor   = observ%sensor,        &
                                         target   = observ%target,        &
                                         response = observ%response,      &
                                         from     = report%from,          &
                                         to       = report%to)

                        ! Add title and subtitle.
                        if (dm_is_ok(rc) .or. report%verbose) then
                            if (len_trim(observ%subtitle) > 0) then
                                roff = roff // dm_roff_ms_sh(3, trim(observ%title) // ROFF_ESC_NBSP // &
                                               dm_roff_m(SUB, dm_roff_s(1, observ%subtitle, rel='-')))
                            else
                                roff = roff // dm_roff_ms_sh(3, observ%title)
                            end if
                        end if

                        ! Handle errors.
                        if (rc == E_DB_NO_ROWS) then
                            if (report%verbose) roff = roff // dm_roff_ms_lp('No observations found in database.')
                            cycle
                        end if

                        if (dm_is_error(rc)) then
                            if (report%verbose) roff = roff // dm_roff_ms_lp(dm_error_message(rc))
                            cycle
                        end if

                        ! Add meta description.
                        if (len_trim(observ%meta) > 0) roff = roff // dm_roff_ms_lp(observ%meta)

                        ! Scale response values.
                        call dm_dp_scale(dps, observ%scale)

                        ! Plot to EPS file.
                        eps_files(i) = temporary_file(APP_TMP_DIR, APP_SUFFIX_EPS)

                        call dm_plot_set(plot     = plot,                     &
                                         terminal = PLOT_TERMINAL_POSTSCRIPT, &
                                         width    = APP_PS_PLOT_WIDTH,        &
                                         height   = APP_PS_PLOT_HEIGHT,       &
                                         output   = eps_files(i),             &
                                         bidirect = .false.,                  &
                                         graph    = ' ',                      &
                                         font     = APP_PS_FONT,              &
                                         xlabel   = APP_XLABEL,               &
                                         ylabel   = observ%response)

                        if (len_trim(observ%unit)  > 0) plot%ylabel     = trim(plot%ylabel) // ' [' // trim(observ%unit) // ']'
                        if (len_trim(observ%title) > 0) plot%title      = trim(observ%title)
                        if (len_trim(observ%color) > 0) plot%foreground = observ%color

                        if (observ%width  > 0 .and. observ%width  <= APP_PS_PLOT_WIDTH) plot%width  = observ%width
                        if (observ%height > 0 .and. observ%height <= APP_PS_PLOT_WIDTH) plot%height = observ%height

                        rc = dm_plot_lines(plot, dps)
                        call dm_plot_close(plot)

                        ! Add EPS to markup.
                        roff = roff // dm_roff_pspic(eps_files(i))

                        ! Add page break.
                        if (observ%pagebreak) roff = roff // ROFF_REQUEST_BP
                    end associate
                end do
            end block plot_block

            ! Add logs.
            log_block: block
                integer, parameter :: NCOL = 5, NFMT = 3

                character(len=4)                :: format(NCOL, NFMT)
                character(len=520), allocatable :: data(:, :)
                type(log_type),     allocatable :: logs(:)

                ! Skip logs if disabled.
                if (report%log%disabled) exit log_block

                ! Read logs from database.
                rc = db_read_logs(logs      = logs,                 &
                                  database  = report%log%database,  &
                                  node      = report%node,          &
                                  from      = report%from,          &
                                  to        = report%to,            &
                                  min_level = report%log%min_level, &
                                  max_level = report%log%max_level)

                ! Add log section title and meta description.
                if (dm_is_ok(rc) .or. report%verbose) then
                    roff = roff // dm_roff_ms_sh(2, report%log%title) // RULE // &
                                   dm_roff_ms_lp(report%log%meta)
                end if

                ! Handle errors.
                if (rc == E_DB_NO_ROWS) then
                    if (report%verbose) roff = roff // dm_roff_ms_lp('No logs found in database.')
                    exit log_block
                end if

                if (dm_is_error(rc)) then
                    if (report%verbose) roff = roff // dm_roff_ms_lp(dm_error_message(rc))
                    exit log_block
                end if

                ! Add log table.
                n = size(logs) + 1
                allocate (data(NCOL, n))

                ! Set table header.
                format = reshape([ character(len=4) ::               &
                                   'lb', 'lb', 'lb', 'lb',   'lb',   & ! Left aligned, bold.
                                    '-',  '-',  '-',  '-',    '-',   & ! Horizontal rule.
                                    'l',  'l',  'l',  'l', 'lw36' ], & ! Left aligned, with min. width.
                                 [ NCOL, NFMT ])
                data(:, 1) = [ character(len=520) :: 'Timestamp', 'Source', 'Level', 'Error', 'Message' ]

                ! Add table rows.
                do i = 1, n - 1
                    associate (log => logs(i))
                        data(:, i + 1) = [ character(len=520) :: &
                            dm_time_to_human(log%timestamp),     & ! Log timestamp.
                            log%source,                          & ! Log source.
                            LOG_LEVEL_NAMES_LOWER(log%level),    & ! Log level name.
                            dm_itoa(log%error),                  & ! Log error code.
                            dm_roff_tbl_block(log%message)       & ! Log message.
                        ]
                    end associate
                end do

                roff = roff // dm_roff_tbl(format, data, expand=.true.)
            end block log_block

            ! Create PostScript file.
            rc = dm_roff_to_ps(roff, report%output, macro=ROFF_MACRO_MS, preconv=.true., tbl=.true.)

            ! Remove temporary files.
            do i = 1, size(eps_files)
                if (len_trim(eps_files(i)) == 0) cycle
                call dm_file_delete(eps_files(i))
            end do
        end block ps_block

        if (present(error)) error = rc
    end subroutine make_ps

    ! **************************************************************************
    ! DATABASE FUNCTIONS.
    ! **************************************************************************
    integer function db_read_dps(dps, database, node, sensor, target, response, from, to) result(rc)
        !! Returns data points from observations database.
        type(dp_type), allocatable, intent(out) :: dps(:)   !! Returned data points from database.
        character(len=*),           intent(in)  :: database !! Path to database.
        character(len=*),           intent(in)  :: node     !! Node id.
        character(len=*),           intent(in)  :: sensor   !! Sensor id.
        character(len=*),           intent(in)  :: target   !! Target id.
        character(len=*),           intent(in)  :: response !! Response name.
        character(len=*),           intent(in)  :: from     !! Start of time range.
        character(len=*),           intent(in)  :: to       !! End of time range.

        type(db_type) :: db

        db_block: block
            rc = dm_db_open(db, database, read_only=.true.); if (dm_is_error(rc)) exit db_block
            rc = dm_db_select_data_points(db, dps, node, sensor, target, response, from, to)
        end block db_block

        call dm_db_close(db)
    end function db_read_dps

    integer function db_read_logs(logs, database, node, from, to, min_level, max_level) result(rc)
        !! Returns logs from logs database.
        type(log_type), allocatable, intent(out) :: logs(:)   !! Returned logs from database.
        character(len=*),            intent(in)  :: database  !! Path to database.
        character(len=*),            intent(in)  :: node      !! Node id.
        character(len=*),            intent(in)  :: from      !! Start of time range.
        character(len=*),            intent(in)  :: to        !! End of time range.
        integer,                     intent(in)  :: min_level !! Min. log level.
        integer,                     intent(in)  :: max_level !! Max. log level.

        type(db_type) :: db

        db_block: block
            rc = dm_db_open(db, database, read_only=.true.); if (dm_is_error(rc)) exit db_block
            rc = dm_db_select_logs(db, logs, node_id=node, from=from, to=to, min_level=min_level, max_level=max_level)
        end block db_block

        call dm_db_close(db)
    end function db_read_logs

    integer function db_read_node(node, node_id, database) result(rc)
        !! Returns node of given id from observations database.
        type(node_type),  intent(out) :: node     !! Returned node type from database.
        character(len=*), intent(in)  :: node_id  !! Node id.
        character(len=*), intent(in)  :: database !! Path to database.

        type(db_type) :: db

        db_block: block
            rc = dm_db_open(db, database, read_only=.true.); if (dm_is_error(rc)) exit db_block
            rc = dm_db_select(db, node, node_id)
        end block db_block

        call dm_db_close(db)
    end function db_read_node

    ! **************************************************************************
    ! HTML FUNCTIONS.
    ! **************************************************************************
    function html_plot(dps, response, unit, terminal, title, meta, color, width, height, verbose) result(html)
        !! Returns time series plot in HTML format from given data points.
        type(dp_type),    intent(inout)        :: dps(:)   !! Data points to plot.
        character(len=*), intent(in)           :: response !! Response name.
        character(len=*), intent(in)           :: unit     !! Response unit.
        integer,          intent(in)           :: terminal !! Plot terminal.
        character(len=*), intent(in), optional :: title    !! Plot title.
        character(len=*), intent(in), optional :: meta     !! Plot description.
        character(len=*), intent(in), optional :: color    !! Foreground colour.
        integer,          intent(in), optional :: width    !! Plot width [px].
        integer,          intent(in), optional :: height   !! Plot height [px].
        logical,          intent(in), optional :: verbose  !! Output warnings and errors.
        character(len=:), allocatable          :: html     !! Generated HTML.

        logical :: verbose_

        verbose_ = dm_present(verbose, .true.)

        plot_block: block
            character(len=:), allocatable :: error, image, mime, output
            integer                       :: rc
            type(plot_type)               :: plot

            ! Plot settings.
            call dm_plot_set(plot     = plot,                 & ! Plot type.
                             bidirect = .true.,               & ! Bi-directional pipe to Gnuplot.
                             terminal = terminal,             & ! Gnuplot terminal.
                             font     = APP_HTML_FONT,        & ! Font name.
                             width    = APP_HTML_PLOT_WIDTH,  & ! Plot width [px].
                             height   = APP_HTML_PLOT_HEIGHT, & ! Plot height [px].
                             xlabel   = APP_XLABEL,           & ! X axis label.
                             ylabel   = response)               ! Y axis label.

            ! Add unit to Y label of plot.
            if (len_trim(unit) > 0) plot%ylabel = trim(plot%ylabel) // ' [' // trim(unit) // ']'

            ! Set title, colour, width, and height.
            if (dm_string_is_present(title)) plot%title      = title
            if (dm_string_is_present(color)) plot%foreground = color

            if (present(width)) then
                if (width > 0) plot%width = width
            end if

            if (present(height)) then
                if (height > 0) plot%height = height
            end if

            ! Select MIME type according to format.
            select case (plot%terminal)
                case (PLOT_TERMINAL_GIF);      mime = MIME_GIF
                case (PLOT_TERMINAL_PNG, &
                      PLOT_TERMINAL_PNGCAIRO); mime = MIME_PNG
                case (PLOT_TERMINAL_SVG);      mime = MIME_SVG
                case default
                    ! Fail-safe: should never occur.
                    html = dm_html_error(E_INVALID, 'invalid plot format')
                    exit plot_block
            end select

            ! Create lines plot.
            rc = dm_plot_lines(plot, dps)

            if (dm_is_error(rc)) then
                if (verbose_) html = dm_html_error(rc, 'failed to create plot')
                exit plot_block
            end if

            ! Read Gnuplot output from stdout.
            rc = dm_plot_read(plot, output)

            if (dm_is_error(rc)) then
                if (verbose_) html = dm_html_error(rc, 'failed to read from backend')
                call dm_plot_close(plot)
                exit plot_block
            end if

            ! Create HTML figure.
            image = dm_html_image(src=dm_html_data_uri(output, mime), alt=response)
            html  = dm_html_figure(content=image, caption=meta)

            ! Read Gnuplot output from stderr.
            if (verbose_) then
                rc = dm_plot_error(plot, error); if (dm_is_error(rc)) exit plot_block
                html = html // dm_html_pre(dm_html_encode(error), code=.true.)
            end if
        end block plot_block

        if (.not. allocated(html)) html = ''
    end function html_plot

    ! **************************************************************************
    ! UTILITY FUNCTIONS.
    ! **************************************************************************
    function temporary_file(base, suffix) result(path)
        !! Returns path of temporary file.
        character(len=*), intent(in)  :: base   !! Base path.
        character(len=*), intent(in)  :: suffix !! File suffix.
        character(len=:), allocatable :: path   !! File path.

        path = dm_path_join(base, dm_uuid4() // suffix)
    end function temporary_file

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from file.
        type(app_type), target, intent(out) :: app !! App type.

        character(len=REPORT_FORMAT_NAME_LEN) :: format
        integer                               :: i, n, terminal
        logical                               :: has_format
        type(arg_type)                        :: args(8)

        args = [ &
            arg_type('name',   short='n', type=ARG_TYPE_ID),     & ! -n, --name <string>
            arg_type('config', short='c', type=ARG_TYPE_FILE),   & ! -c, --config <path>
            arg_type('node',   short='N', type=ARG_TYPE_ID),     & ! -N, --node <id>
            arg_type('from',   short='B', type=ARG_TYPE_TIME),   & ! -B, --from <timestamp>
            arg_type('to',     short='E', type=ARG_TYPE_TIME),   & ! -E, --to <timestamp>
            arg_type('format', short='F', type=ARG_TYPE_STRING), & ! -F, --format <name>
            arg_type('output', short='o', type=ARG_TYPE_STRING), & ! -o, --output <path>
            arg_type('style',  short='C', type=ARG_TYPE_FILE)    & ! -C, --style <path>
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%name)
        call dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        call dm_arg_get(args(3), app%report%node)
        call dm_arg_get(args(4), app%report%from)
        call dm_arg_get(args(5), app%report%to)
        call dm_arg_get(args(6), format, passed=has_format)
        call dm_arg_get(args(7), app%report%output)
        call dm_arg_get(args(8), app%report%style)

        if (has_format) app%report%format = dm_report_format_from_name(format)

        ! Validate settings.
        rc = E_INVALID

        associate (report => app%report, plot => app%report%plot, log => app%report%log)
            if (.not. dm_id_is_valid(report%node)) then
                call dm_error_out(rc, 'invalid node id')
                return
            end if

            if (.not. dm_time_is_valid(report%from, strict=.false.)) then
                call dm_error_out(rc, 'invalid from timestamp')
                return
            end if

            if (.not. dm_time_is_valid(report%to, strict=.false.)) then
                call dm_error_out(rc, 'invalid to timestamp')
                return
            end if

            if (.not. dm_report_format_is_valid(report%format)) then
                call dm_error_out(rc, 'invalid report format')
                return
            end if

            ! Validate plot settings.
            if (.not. plot%disabled) then
                if (len_trim(plot%database) == 0) then
                    call dm_error_out(rc, 'missing path to observation database')
                    return
                end if

                n = 0
                if (allocated(plot%observs)) n = size(plot%observs)

                do i = 1, n
                    terminal = dm_plot_terminal_from_name(plot%observs(i)%format)

                    select case (report%format)
                        case (REPORT_FORMAT_HTML)
                            if (terminal /= PLOT_TERMINAL_GIF      .and. terminal /= PLOT_TERMINAL_PNG .and. &
                                terminal /= PLOT_TERMINAL_PNGCAIRO .and. terminal /= PLOT_TERMINAL_SVG) then
                                call dm_error_out(rc, 'invalid plot format ' // plot%observs(i)%format)
                                return
                            end if

                        case (REPORT_FORMAT_PDF, REPORT_FORMAT_PS)
                            if (terminal /= PLOT_TERMINAL_POSTSCRIPT) then
                                call dm_error_out(rc, 'invalid plot format ' // trim(plot%observs(i)%format) // ' (PostScript required)')
                                return
                            end if
                    end select

                    if (.not. dm_id_is_valid(plot%observs(i)%sensor)) then
                        call dm_error_out(rc, 'invalid sensor id ' // plot%observs(i)%sensor)
                        return
                    end if

                    if (.not. dm_id_is_valid(plot%observs(i)%target)) then
                        call dm_error_out(rc, 'invalid target id ' // plot%observs(i)%target)
                        return
                    end if

                    if (len_trim(plot%observs(i)%response) == 0) then
                        call dm_error_out(rc, 'invalid response name ' // plot%observs(i)%response)
                        return
                    end if
                end do
            end if

            ! Validate log settings.
            if (.not. log%disabled) then
                if (.not. dm_log_level_is_valid(log%min_level)) then
                    call dm_error_out(rc, 'invalid minimum log level')
                    return
                end if

                if (.not. dm_log_level_is_valid(log%max_level)) then
                    call dm_error_out(rc, 'invalid maximum log level')
                    return
                end if

                if (log%min_level > log%max_level) then
                    call dm_error_out(rc, 'minimum log level must be less than maximum')
                    return
               end if

                if (len_trim(log%database) == 0) then
                    call dm_error_out(rc, 'missing path to log database')
                    return
                end if
            end if

            ! Validate a second time, just to be sure.
            if (.not. dm_report_is_valid(report)) then
                call dm_error_out(rc, 'invalid report settings')
                return
            end if
        end associate

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads app configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.
        type(config_type)             :: config

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            ! Take the table from the top of the Lua stack,
            ! do not load a table field.
            call dm_config_get(config, app%name, app%report, field=.false.)
        end if

        call dm_config_close(config)
    end function read_config

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 3(1x, a))', dm_plot_version(.true.), dm_roff_version(.true.), dm_lua_version(.true.), dm_db_version(.true.)
    end subroutine version_callback
end program dmreport
