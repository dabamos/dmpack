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

    character(len=*), parameter :: APP_FONT        = 'Open Sans' !! Default font name.
    integer,          parameter :: APP_PLOT_WIDTH  = 1000        !! Default plot width.
    integer,          parameter :: APP_PLOT_HEIGHT = 400         !! Default plot height.

    type :: app_type
        !! Application settings.
        character(len=LOGGER_NAME_LEN) :: name   = APP_NAME !! Name of instance and POSIX semaphore.
        character(len=FILE_PATH_LEN)   :: config = ' '      !! Path to configuration file.
        type(report_type)              :: report            !! Report settings.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App configuration.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments and configuration file options.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Create HTML report.
    call create_report(app%report, rc)
    if (dm_is_error(rc)) call dm_stop(1)
    call dm_stop(0)
contains
    function html_footer() result(html)
        !! Returns HTML footer with current date and time.
        character(len=:), allocatable :: html

        html = H_FOOTER // H_HR // H_P // H_SMALL // &
               'This page was generated ' // dm_time_now() // &
               ' by ' // APP_NAME // ' ' // dm_version_to_string(APP_MAJOR, APP_MINOR) // &
               ' (DMPACK ' // DM_VERSION_STRING // ') ' // &
               H_SMALL_END // H_P_END // H_FOOTER_END // dm_html_footer()
    end function html_footer

    function html_plot(data_points, response, unit, format, title, meta, color, width, height) result(html)
        !! Returns time series plot in HTML format from given data points.
        type(dp_type),    intent(inout)        :: data_points(:) !! Data points to plot.
        character(len=*), intent(in)           :: response       !! Response name.
        character(len=*), intent(in)           :: unit           !! Response unit.
        integer,          intent(in)           :: format         !! Plot format.
        character(len=*), intent(in), optional :: title          !! Plot title.
        character(len=*), intent(in), optional :: meta           !! Plot description.
        character(len=*), intent(in), optional :: color          !! Foreground colour.
        integer,          intent(in), optional :: width          !! Plot width.
        integer,          intent(in), optional :: height         !! Plot height (+ x).
        character(len=:), allocatable          :: html           !! Generated HTML.

        character(len=:), allocatable :: image, mime
        character(len=:), allocatable :: str_err, str_out
        integer                       :: rc
        type(plot_type)               :: plot

        ! Plot settings.
        plot%bidirect = .true.
        plot%term     = format
        plot%font     = APP_FONT
        plot%width    = APP_PLOT_WIDTH
        plot%height   = APP_PLOT_HEIGHT
        plot%xlabel   = 'Time'
        plot%ylabel   = response

        ! Add unit to Y label of plot.
        if (len_trim(unit) > 0) then
            plot%ylabel = trim(plot%ylabel) // ' [' // trim(unit) // ']'
        end if

        ! Set title, meta, colour, width, and height.
        if (present(title)) then
            if (len_trim(title) > 0) plot%title = title
        end if

        if (present(color)) then
            if (len_trim(color) > 0) plot%foreground = color
        end if

        if (present(width)) then
            if (width > 0) plot%width = width
        end if

        if (present(height)) then
            if (height > 0) plot%height = height
        end if

        ! Select MIME type according to format.
        select case (plot%term)
            case (PLOT_TERM_GIF)
                mime = MIME_GIF
            case (PLOT_TERM_PNG, PLOT_TERM_PNG_CAIRO)
                mime = MIME_PNG
            case (PLOT_TERM_SVG)
                mime = MIME_SVG
            case default
                ! Fail-safe: should never occur.
                html = dm_html_error(E_INVALID, 'invalid plot format')
                return
        end select

        ! Create lines plot.
        rc = dm_plot_lines(plot, data_points)

        if (dm_is_error(rc)) then
            html = dm_html_error(rc, 'failed to create plot')
            return
        end if

        ! Read Gnuplot output from stdout.
        if (dm_plot_read(plot, str_out) == 0) then
            html = dm_html_error(E_IO, 'failed to read from backend')
            return
        end if

        ! Create HTML figure.
        image = dm_html_image(src=dm_html_data_uri(str_out, mime), alt=response)
        html  = dm_html_figure(content=image, caption=meta)

        ! Read Gnuplot output from stderr.
        if (dm_plot_error(plot, str_err) > 0) then
            html = html // dm_html_pre(dm_html_encode(str_err), code=.true.)
        end if
    end function html_plot

    function html_report_table(node_id, from, to) result(html)
        !! Returns HTML table of node id, from, to.
        character(len=*), intent(in)  :: node_id !! Node id.
        character(len=*), intent(in)  :: from    !! Start of time range.
        character(len=*), intent(in)  :: to      !! End of time range.
        character(len=:), allocatable :: html    !! Generated HTML.

        html = H_NAV // H_TABLE // H_TBODY // H_TR // &
               H_TH // 'Node ID:' // H_TH_END // H_TD // dm_html_encode(node_id) // H_TD_END // &
               H_TH // 'From:'    // H_TH_END // H_TD // dm_html_encode(from)    // H_TD_END // &
               H_TH // 'To:'      // H_TH_END // H_TD // dm_html_encode(to)      // H_TD_END // &
               H_TR_END // H_TBODY_END // H_TABLE_END // H_NAV_END
    end function html_report_table

    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from file.
        type(app_type), target, intent(inout) :: app !! App type.

        integer                         :: format, i, n
        type(arg_type)                  :: args(7)
        type(report_log_type),  pointer :: log
        type(report_plot_type), pointer :: plot

        rc = E_NONE

        args = [ &
            arg_type('name',   short='n', type=ARG_TYPE_ID),   & ! -n, --name <string>
            arg_type('config', short='c', type=ARG_TYPE_FILE), & ! -c, --config <path>
            arg_type('node',   short='N', type=ARG_TYPE_ID),   & ! -N, --node <id>
            arg_type('from',   short='B', type=ARG_TYPE_TIME), & ! -B, --from <timestamp>
            arg_type('to',     short='E', type=ARG_TYPE_TIME), & ! -E, --to <timestamp>
            arg_type('output', short='o', type=ARG_TYPE_CHAR), & ! -o, --output <path>
            arg_type('style',  short='C', type=ARG_TYPE_FILE)  & ! -C, --style <path>
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        rc = dm_arg_get(args(3), app%report%node)
        rc = dm_arg_get(args(4), app%report%from)
        rc = dm_arg_get(args(5), app%report%to)
        rc = dm_arg_get(args(6), app%report%output)
        rc = dm_arg_get(args(7), app%report%style)

        ! Validate settings.
        rc = E_INVALID

        if (.not. dm_id_valid(app%report%node)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (.not. dm_time_valid(app%report%from)) then
            call dm_error_out(rc, 'invalid from timestamp')
            return
        end if

        if (.not. dm_time_valid(app%report%to)) then
            call dm_error_out(rc, 'invalid to timestamp')
            return
        end if

        ! Create pointers.
        plot => app%report%plot
        log  => app%report%log

        ! Validate plot settings.
        if (.not. plot%disabled) then
            if (len_trim(plot%database) == 0) then
                call dm_error_out(rc, 'missing path to observations database')
                return
            end if

            n = 0
            if (allocated(plot%observs)) n = size(plot%observs)

            do i = 1, n
                format = dm_plot_term_from_name(plot%observs(i)%format)

                if (format /= PLOT_TERM_GIF       .and. format /= PLOT_TERM_PNG .and. &
                    format /= PLOT_TERM_PNG_CAIRO .and. format /= PLOT_TERM_SVG) then
                    call dm_error_out(rc, 'invalid plot format ' // plot%observs(i)%format)
                    return
                end if

                if (.not. dm_id_valid(plot%observs(i)%sensor)) then
                    call dm_error_out(rc, 'invalid sensor id ' // plot%observs(i)%sensor)
                    return
                end if

                if (.not. dm_id_valid(plot%observs(i)%target)) then
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
            if (log%min_level < LOG_NONE .or. log%min_level > LOG_CRITICAL) then
                call dm_error_out(rc, 'invalid minimum log level')
                return
            end if

            if (log%max_level < LOG_NONE .or. log%max_level > LOG_CRITICAL) then
                call dm_error_out(rc, 'invalid maximum log level')
                return
            end if

            if (log%min_level > log%max_level) then
                call dm_error_out(rc, 'minimum log level must be less than maximum')
                return
           end if

            if (len_trim(log%database) == 0) then
                call dm_error_out(rc, 'missing path to logs database')
                return
            end if
        end if

        ! Validate a second time, just to be sure.
        if (.not. dm_report_valid(app%report)) then
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
            rc = dm_config_get(config, app%name, app%report, field=.false.)
        end if

        call dm_config_close(config)
    end function read_config

    integer function read_data_points(data_points, database, node, sensor, target, response, from, to) result(rc)
        !! Returns data points from observations database.
        type(dp_type), allocatable, intent(out) :: data_points(:) !! Returned data points from database.
        character(len=*),           intent(in)  :: database       !! Path to database.
        character(len=*),           intent(in)  :: node           !! Node id.
        character(len=*),           intent(in)  :: sensor         !! Sensor id.
        character(len=*),           intent(in)  :: target         !! Target id.
        character(len=*),           intent(in)  :: response       !! Response name.
        character(len=*),           intent(in)  :: from           !! Start of time range.
        character(len=*),           intent(in)  :: to             !! End of time range.

        type(db_type) :: db

        db_block: block
            rc = dm_db_open(db, database, read_only=.true.)
            if (dm_is_error(rc)) exit db_block
            rc = dm_db_select(db, data_points, node, sensor, target, response, from, to)
        end block db_block

        rc = max(dm_db_close(db), rc)
    end function read_data_points

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
            rc = dm_db_open(db, database, read_only=.true.)
            if (dm_is_error(rc)) exit db_block
            rc = dm_db_select(db, logs, node_id=node, from=from, to=to, &
                              min_level=min_level, max_level=max_level)
        end block db_block

        rc = max(dm_db_close(db), rc)
    end function read_logs

    subroutine create_report(report, error)
        !! Creates report in HTML format.
        type(report_type), intent(inout)         :: report !! Report type.
        integer,           intent(out), optional :: error  !! Error code.

        character(len=:), allocatable :: path, style
        integer                       :: format, fu, i, n, rc, stat
        logical                       :: is_file

        type(dp_type),  allocatable :: data_points(:)
        type(log_type), allocatable :: logs(:)

        ! By default, print generated HTML to standard output.
        fu = stdout
        is_file = (len_trim(report%output) > 0)

        report_block: block
            ! Open output file for writing.
            if (is_file) then
                rc   = E_WRITE
                path = dm_path_parsed(report%output)

                open (action='write', file=path, iostat=stat, newunit=fu, status='replace')

                if (stat /= 0) then
                    call dm_error_out(rc, 'failed to open output file ' // path)
                    exit report_block
                end if
            end if

            ! Read CSS from file.
            if (len_trim(report%style) > 0) then
                call dm_file_read(report%style, style, error=rc)

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to read CSS file ' // report%style)
                    exit report_block
                end if
            end if

            ! Output HTML header with optional inline CSS.
            if (len_trim(style) > 0) then
                write (fu, '(a)') dm_html_header(report%title, report%subtitle, internal_style=style)
            else
                write (fu, '(a)') dm_html_header(report%title, report%subtitle)
            end if

            ! Output report table.
            write (fu, '(a)') html_report_table(report%node, report%from, report%to)

            if (len_trim(report%meta) > 0) then
                write (fu, '(a)') dm_html_p(dm_html_encode(report%meta))
            end if

            ! Output plots.
            plot_if: if (.not. report%plot%disabled) then
                ! Output heading.
                write (fu, '(a)') dm_html_heading(2, report%plot%title)

                ! Output meta description.
                if (len_trim(report%plot%meta) > 0) then
                    write (fu, '(a)') dm_html_p(dm_html_encode(report%plot%meta))
                end if

                if (.not. allocated(report%plot%observs)) exit plot_if
                n = size(report%plot%observs)

                ! Plot loop.
                do i = 1, n
                    ! Output plot heading.
                    write (fu, '(a)') dm_html_heading(3, report%plot%observs(i)%title, &
                                                      report%plot%observs(i)%subtitle)

                    plot_block: block
                        ! Read data points from observations database.
                        rc = read_data_points(data_points, &
                                              database = report%plot%database, &
                                              node     = report%node, &
                                              sensor   = report%plot%observs(i)%sensor, &
                                              target   = report%plot%observs(i)%target, &
                                              response = report%plot%observs(i)%response, &
                                              from     = report%from, &
                                              to       = report%to)

                        if (rc == E_DB_NO_ROWS) then
                            write (fu, '(a)') dm_html_p('No observations found in database.')
                            exit plot_block
                        end if

                        if (dm_is_error(rc)) then
                            write (fu, '(a)') dm_html_error(rc)
                            exit plot_block
                        end if

                        format = dm_plot_term_from_name(report%plot%observs(i)%format)

                        if (format /= PLOT_TERM_GIF       .and. format /= PLOT_TERM_PNG .and. &
                            format /= PLOT_TERM_PNG_CAIRO .and. format /= PLOT_TERM_SVG) then
                            ! Fail safe: should never occur.
                            write (fu, '(a)') dm_html_error(E_INVALID, 'invalid plot format')
                            exit plot_block
                        end if

                        ! Output HTML plot figure.
                        write (fu, '(a)') html_plot(data_points, &
                                                    response = report%plot%observs(i)%response, &
                                                    unit     = report%plot%observs(i)%unit, &
                                                    format   = format, &
                                                    title    = report%plot%observs(i)%title, &
                                                    meta     = report%plot%observs(i)%meta, &
                                                    color    = report%plot%observs(i)%color, &
                                                    width    = report%plot%observs(i)%width, &
                                                    height   = report%plot%observs(i)%height)
                    end block plot_block

                end do
            end if plot_if

            ! Output logs.
            log_if: if (.not. report%log%disabled) then
                ! Output heading.
                write (fu, '(a)') dm_html_heading(2, report%log%title)

                ! Output meta description.
                if (len_trim(report%log%meta) > 0) then
                    write (fu, '(a)') dm_html_p(dm_html_encode(report%log%meta))
                end if

                ! Read logs from database.
                rc = read_logs(logs      = logs, &
                               database  = report%log%database, &
                               node      = report%node, &
                               from      = report%from, &
                               to        = report%to, &
                               min_level = report%log%min_level, &
                               max_level = report%log%max_level)

                if (rc == E_DB_NO_ROWS) then
                    write (fu, '(a)') dm_html_p('No logs found in database.')
                    exit log_if
                end if

                if (dm_is_error(rc)) then
                    write (fu, '(a)') dm_html_error(rc)
                    exit log_if
                end if

                ! Output logs table.
                write (fu, '(a)') dm_html_logs(logs, node=.false.)
            end if log_if

            ! Output HTML footer.
            write (fu, '(a)') html_footer()

            rc = E_NONE
        end block report_block

        if (is_file) close (fu)
        if (present(error)) error = rc
    end subroutine create_report
end program dmreport
