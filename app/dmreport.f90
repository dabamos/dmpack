! dmreport.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmreport
    !! Generates HTML reports with plots and logs.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmreport'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 8

    character(len=*), parameter :: APP_FONT        = 'Open Sans' !! Default font name.
    integer,          parameter :: APP_PLOT_WIDTH  = 1000        !! Default plot width.
    integer,          parameter :: APP_PLOT_HEIGHT = 400         !! Default plot height.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)        :: name   = APP_NAME !! Name of instance and POSIX semaphore.
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

    ! Create HTML report.
    call create_report(app%report, rc)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    function html_footer() result(html)
        !! Returns HTML footer with current date and time.
        character(len=:), allocatable :: html

        html = H_FOOTER // H_HR // H_P // H_SMALL // &
               'This report was generated ' // dm_html_time(dm_time_now(), human=.true.) // ' by ' // &
               dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, library=.true.) // &
               H_SMALL_END // H_P_END // H_FOOTER_END // &
               dm_html_footer()
    end function html_footer

    function html_plot(dps, response, unit, format, title, meta, color, width, height, verbose) result(html)
        !! Returns time series plot in HTML format from given data points.
        type(dp_type),    intent(inout)        :: dps(:)   !! Data points to plot.
        character(len=*), intent(in)           :: response !! Response name.
        character(len=*), intent(in)           :: unit     !! Response unit.
        integer,          intent(in)           :: format   !! Plot format.
        character(len=*), intent(in), optional :: title    !! Plot title.
        character(len=*), intent(in), optional :: meta     !! Plot description.
        character(len=*), intent(in), optional :: color    !! Foreground colour.
        integer,          intent(in), optional :: width    !! Plot width.
        integer,          intent(in), optional :: height   !! Plot height (+ x).
        logical,          intent(in), optional :: verbose  !! Output warnings and errors.
        character(len=:), allocatable          :: html     !! Generated HTML.

        logical :: verbose_

        verbose_ = dm_present(verbose, .true.)

        plot_block: block
            character(len=:), allocatable :: error, image, mime, output
            integer                       :: rc
            type(plot_type)               :: plot

            ! Plot settings.
            call dm_plot_set(plot     = plot,            & ! Plot type.
                             bidirect = .true.,          & ! Bi-directional pipe to Gnuplot.
                             terminal = format,          & ! Gnuplot terminal.
                             font     = APP_FONT,        & ! Font name.
                             width    = APP_PLOT_WIDTH,  & ! Plot width.
                             height   = APP_PLOT_HEIGHT, & ! Plot height.
                             xlabel   = 'Time',          & ! X axis label.
                             ylabel   = response)          ! Y axis label.

            ! Add unit to Y label of plot.
            if (len_trim(unit) > 0) then
                plot%ylabel = trim(plot%ylabel) // ' [' // trim(unit) // ']'
            end if

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
                case (PLOT_TERMINAL_GIF)
                    mime = MIME_GIF
                case (PLOT_TERMINAL_PNG, PLOT_TERMINAL_PNGCAIRO)
                    mime = MIME_PNG
                case (PLOT_TERMINAL_SVG)
                    mime = MIME_SVG
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
                rc = dm_plot_error(plot, error)
                if (dm_is_error(rc)) exit plot_block
                html = html // dm_html_pre(dm_html_encode(error), code=.true.)
            end if
        end block plot_block

        if (.not. allocated(html)) html = ''
    end function html_plot

    function html_report_table(node, from, to) result(html)
        !! Returns HTML table of node id, from, to.
        type(node_type),  intent(inout) :: node !! Node type.
        character(len=*), intent(in)    :: from !! Start of time range.
        character(len=*), intent(in)    :: to   !! End of time range.
        character(len=:), allocatable   :: html !! Generated HTML.

        html = H_NAV // H_TABLE // H_TBODY // &
               H_TR // H_TH // 'Node ID:'   // H_TH_END // H_TD // dm_html_encode(node%id)   // H_TD_END // &
                       H_TH // 'From:'      // H_TH_END // H_TD // dm_html_time(from)        // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Node Name:' // H_TH_END // H_TD // dm_html_encode(node%name) // H_TD_END // &
                       H_TH // 'To:'        // H_TH_END // H_TD // dm_html_time(to)          // H_TD_END // H_TR_END // &
               H_TBODY_END // H_TABLE_END // H_NAV_END
    end function html_report_table

    integer function read_dps(dps, database, node, sensor, target, response, from, to) result(rc)
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
            rc = dm_db_open(db, database, read_only=.true.)
            if (dm_is_error(rc)) exit db_block
            rc = dm_db_select_data_points(db, dps, node, sensor, target, response, from, to)
        end block db_block

        call dm_db_close(db)
    end function read_dps

    integer function read_logs(logs, database, node, from, to, min_level, max_level) result(rc)
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
    end function read_logs

    integer function read_node(node, node_id, database) result(rc)
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
    end function read_node

    subroutine create_report(report, error)
        !! Creates report in HTML format.
        type(report_type), intent(inout)         :: report !! Report type.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: unit, rc
        logical :: is_file

        ! By default, print generated HTML to standard output.
        unit = stdout
        is_file = (len_trim(report%output) > 0 .and. report%output /= '-')

        report_block: block
            character(len=:), allocatable :: inline_style, path
            integer                       :: i, n, stat

            type(node_type)             :: node

            allocate (character(len=0) :: inline_style)
            allocate (character(len=0) :: path)

            ! Open output file for writing.
            if (is_file) then
                rc   = E_WRITE
                path = dm_path_parsed(report%output)

                open (action='write', file=path, iostat=stat, newunit=unit, status='replace')

                if (stat /= 0) then
                    call dm_error_out(rc, 'failed to open output file ' // path)
                    exit report_block
                end if
            end if

            ! Read CSS from file.
            if (len_trim(report%style) > 0) then
                call dm_file_read(report%style, inline_style, error=rc)

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to read CSS file ' // report%style)
                    exit report_block
                end if
            end if

            ! Add HTML header with optional inline CSS.
            write (unit, '(a)') dm_html_header(report%title, report%subtitle, inline_style=inline_style)

            ! Add report overview table.
            rc = read_node(node, report%node, report%plot%database)

            if (dm_is_ok(rc)) then
                write (unit, '(a)') html_report_table(node, report%from, report%to)
            else if (report%verbose) then
                write (unit, '(a)') dm_html_error(rc)
            end if

            ! Add optional report description.
            if (len_trim(report%meta) > 0) then
                write (unit, '(a)') dm_html_p(dm_html_encode(report%meta))
            end if

            ! Add plots to HTML document if enabled.
            plot_block: block
                integer                    :: format
                type(dp_type), allocatable :: dps(:)

                if (report%plot%disabled) exit plot_block

                ! Add plot section heading.
                write (unit, '(a)') dm_html_heading(2, report%plot%title)

                ! Add meta description.
                if (len_trim(report%plot%meta) > 0) then
                    write (unit, '(a)') dm_html_p(dm_html_encode(report%plot%meta))
                end if

                if (.not. allocated(report%plot%observs)) exit plot_block
                n = size(report%plot%observs)

                ! Plot loop.
                do i = 1, n
                    associate (observ => report%plot%observs(i))
                        ! Read data points from observation database.
                        rc = read_dps(dps      = dps, &
                                      database = report%plot%database, &
                                      node     = report%node, &
                                      sensor   = observ%sensor, &
                                      target   = observ%target, &
                                      response = observ%response, &
                                      from     = report%from, &
                                      to       = report%to)

                        ! Handle errors.
                        if (rc == E_DB_NO_ROWS) then
                            if (report%verbose) then
                                write (unit, '(a)') dm_html_heading(3, observ%title, observ%subtitle)
                                write (unit, '(a)') dm_html_p('No observations found in database.')
                            end if
                            cycle
                        end if

                        if (dm_is_error(rc)) then
                            if (report%verbose) then
                                write (unit, '(a)') dm_html_heading(3, observ%title, observ%subtitle)
                                write (unit, '(a)') dm_html_error(rc)
                            end if
                            cycle
                        end if

                        ! Scale response values.
                        if (.not. dm_equals(observ%scale, 0.0_r8) .and. .not. dm_equals(observ%scale, 1.0_r8)) then
                            dps%y = dps%y * observ%scale
                        end if

                        ! Get Gnuplot terminal name.
                        format = dm_plot_terminal_from_name(observ%format)

                        ! Add HTML plot figure.
                        write (unit, '(a)') dm_html_heading(3, observ%title, observ%subtitle)
                        write (unit, '(a)') html_plot(dps      = dps, &
                                                      response = observ%response, &
                                                      unit     = observ%unit, &
                                                      format   = format, &
                                                      title    = observ%title, &
                                                      meta     = observ%meta, &
                                                      color    = observ%color, &
                                                      width    = observ%width, &
                                                      height   = observ%height, &
                                                      verbose  = report%verbose)
                    end associate
                end do
            end block plot_block

            ! Add table of logs to HTML document if enabled.
            log_block: block
                type(log_type), allocatable :: logs(:)

                if (report%log%disabled) exit log_block

                ! Add section heading.
                write (unit, '(a)') dm_html_heading(2, report%log%title)

                ! Add meta description.
                if (len_trim(report%log%meta) > 0) then
                    write (unit, '(a)') dm_html_p(dm_html_encode(report%log%meta))
                end if

                ! Read logs from database.
                rc = read_logs(logs      = logs, &
                               database  = report%log%database, &
                               node      = report%node, &
                               from      = report%from, &
                               to        = report%to, &
                               min_level = report%log%min_level, &
                               max_level = report%log%max_level)

                ! Handle errors.
                if (rc == E_DB_NO_ROWS) then
                    if (report%verbose) write (unit, '(a)') dm_html_p('No logs found in database.')
                    exit log_block
                end if

                if (dm_is_error(rc)) then
                    if (report%verbose) write (unit, '(a)') dm_html_error(rc)
                    exit log_block
                end if

                ! Add logs table.
                write (unit, '(a)') dm_html_logs(logs, node=.false.)
            end block log_block

            ! Add HTML footer.
            write (unit, '(a)') html_footer()

            rc = E_NONE
        end block report_block

        if (is_file) close (unit)
        if (present(error)) error = rc
    end subroutine create_report

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from file.
        type(app_type), target, intent(out) :: app !! App type.

        integer        :: format, i, n
        type(arg_type) :: args(7)

        args = [ &
            arg_type('name',   short='n', type=ARG_TYPE_ID),     & ! -n, --name <string>
            arg_type('config', short='c', type=ARG_TYPE_FILE),   & ! -c, --config <path>
            arg_type('node',   short='N', type=ARG_TYPE_ID),     & ! -N, --node <id>
            arg_type('from',   short='B', type=ARG_TYPE_TIME),   & ! -B, --from <timestamp>
            arg_type('to',     short='E', type=ARG_TYPE_TIME),   & ! -E, --to <timestamp>
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
        call dm_arg_get(args(6), app%report%output)
        call dm_arg_get(args(7), app%report%style)

        ! Validate settings.
        rc = E_INVALID

        if (.not. dm_id_is_valid(app%report%node)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (.not. dm_time_is_valid(app%report%from, strict=.false.)) then
            call dm_error_out(rc, 'invalid from timestamp')
            return
        end if

        if (.not. dm_time_is_valid(app%report%to, strict=.false.)) then
            call dm_error_out(rc, 'invalid to timestamp')
            return
        end if

        associate (plot => app%report%plot, log => app%report%log)
            ! Validate plot settings.
            if (.not. plot%disabled) then
                if (len_trim(plot%database) == 0) then
                    call dm_error_out(rc, 'missing path to observation database')
                    return
                end if

                n = 0
                if (allocated(plot%observs)) n = size(plot%observs)

                do i = 1, n
                    format = dm_plot_terminal_from_name(plot%observs(i)%format)

                    if (format /= PLOT_TERMINAL_GIF      .and. format /= PLOT_TERMINAL_PNG .and. &
                        format /= PLOT_TERMINAL_PNGCAIRO .and. format /= PLOT_TERMINAL_SVG) then
                        call dm_error_out(rc, 'invalid plot format ' // plot%observs(i)%format)
                        return
                    end if

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
        end associate

        ! Validate a second time, just to be sure.
        if (.not. dm_report_is_valid(app%report)) then
            call dm_error_out(rc, 'invalid report settings')
            return
        end if

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
        print '(a, 2(1x, a))', dm_plot_version(.true.), dm_lua_version(.true.), dm_db_version(.true.)
    end subroutine version_callback
end program dmreport
