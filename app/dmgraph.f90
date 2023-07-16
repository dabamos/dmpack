! dmgraph.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmgraph
    !! Creates plots from time series.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmgraph'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9

    type :: app_type
        !! Application settings.
        character(len=LOGGER_NAME_LEN)   :: name       = APP_NAME       !! Name of instance and POSIX semaphore.
        character(len=FILE_PATH_LEN)     :: config     = ' '            !! Path to configuration file.
        character(len=FILE_PATH_LEN)     :: database   = ' '            !! Path to observation database.
        character(len=NODE_ID_LEN)       :: node       = ' '            !! Node id.
        character(len=SENSOR_ID_LEN)     :: sensor     = ' '            !! Sensor id.
        character(len=TARGET_ID_LEN)     :: target     = ' '            !! Target id.
        character(len=RESPONSE_NAME_LEN) :: response   = ' '            !! Response name.
        character(len=TIME_LEN)          :: from       = ' '            !! Start of time range (ISO 8601).
        character(len=TIME_LEN)          :: to         = ' '            !! End of time range (ISO 8601).
        integer                          :: terminal   = PLOT_TERM_NONE !! Plot terminal (Gnuplot terminal).
        character(len=FILE_PATH_LEN)     :: output     = ' '            !! Path of plot file.
        character(len=8)                 :: background = ' '            !! Background colour.
        character(len=8)                 :: foreground = ' '            !! Foreground colour (graph).
        character(len=32)                :: font       = ' '            !! Font name.
        character(len=80)                :: title      = ' '            !! Plot title.
        integer                          :: width      = 1000           !! Plot width.
        integer                          :: height     = 400            !! Plot height.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App configuration.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments and configuration file options.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    plot_block: block
        character(len=:), allocatable :: path
        type(dp_type),    allocatable :: dps(:)

        rc = read_data_points(dps, app%database, app%node, app%sensor, &
                              app%target, app%response, app%from, app%to)

        if (rc == E_DB_NO_ROWS) then
            call dm_error_out(rc, 'no observations found in database')
            exit plot_block
        end if

        if (dm_is_error(rc)) then
            call dm_error_out(rc)
            exit plot_block
        end if

        ! Create plot.
        path = dm_path_parsed(app%output)
        rc = create_plot(dps, app%terminal, path, app%background, app%foreground, &
                         app%font, app%title, app%width, app%height, 'Time', app%response)
        call dm_error_out(rc)
    end block plot_block

    if (dm_is_error(rc)) call dm_stop(1)
    call dm_stop(0)
contains
    integer function create_plot(dps, terminal, output, background, foreground, &
                                 font, title, width, height, xlabel, ylabel) result(rc)
        !! Writes plot to file or shows X11 window.
        type(dp_type),    intent(inout)         :: dps(:)     !! Data points array.
        integer,          intent(in)            :: terminal     !! Plot terminal.
        character(len=*), intent(in),  optional :: output     !! Output file.
        character(len=*), intent(in),  optional :: background !! Background colour.
        character(len=*), intent(in),  optional :: foreground !! Foreground colour.
        character(len=*), intent(in),  optional :: font       !! Plot font.
        character(len=*), intent(in),  optional :: title      !! Plot title.
        integer,          intent(in),  optional :: width      !! Plot width.
        integer,          intent(in),  optional :: height     !! Plot height.
        character(len=*), intent(in),  optional :: xlabel     !! X label.
        character(len=*), intent(in),  optional :: ylabel     !! Y label.

        type(plot_type) :: plot

        plot%term = terminal

        ! Make X11 window persistent.
        if (plot%term == PLOT_TERM_X11) plot%persist = .true.

        if (present(output)) then
            if (len_trim(output) > 0) plot%output = output
        end if

        if (present(background)) then
            if (len_trim(background) > 0) plot%background = background
        end if

        if (present(foreground)) then
            if (len_trim(foreground) > 0) plot%foreground = foreground
        end if

        if (present(font)) then
            if (len_trim(font) > 0) plot%font = font
        end if

        if (present(title)) then
            if (len_trim(title) > 0) plot%title = title
        end if

        if (present(width)) then
            if (width > 0) plot%width = width
        end if

        if (present(height)) then
            if (height > 0) plot%height = height
        end if

        if (present(xlabel)) then
            if (len_trim(xlabel) > 0) plot%xlabel = xlabel
        end if

        if (present(ylabel)) then
            if (len_trim(ylabel) > 0) plot%ylabel = ylabel
        end if

        rc = dm_plot_lines(plot, dps)
    end function create_plot

    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from file.
        type(app_type), intent(out) :: app !! App type.

        character(len=PLOT_TERM_NAME_LEN) :: terminal
        type(arg_type)                    :: args(17)

        rc = E_NONE

        args = [ &
            arg_type('name',       short='n', type=ARG_TYPE_ID), &      ! -n, --name <string>
            arg_type('config',     short='c', type=ARG_TYPE_FILE), &    ! -c, --config <path>
            arg_type('database',   short='d', type=ARG_TYPE_DB), &      ! -d, --database <file>
            arg_type('node',       short='N', type=ARG_TYPE_ID), &      ! -N, --node <id>
            arg_type('sensor',     short='S', type=ARG_TYPE_ID), &      ! -S, --sensor <id>
            arg_type('target',     short='T', type=ARG_TYPE_ID), &      ! -T, --target <id>
            arg_type('response',   short='R', type=ARG_TYPE_ID, max_len=RESPONSE_NAME_LEN), & ! -R, --response <name>
            arg_type('from',       short='B', type=ARG_TYPE_TIME), &    ! -B, --from <timestamp>
            arg_type('to',         short='E', type=ARG_TYPE_TIME), &    ! -E, --to <timestamp>
            arg_type('terminal',   short='M', type=ARG_TYPE_CHAR, max_len=PLOT_TERM_NAME_LEN), & ! -M, --terminal <name>
            arg_type('output',     short='o', type=ARG_TYPE_CHAR), &    ! -o, --output <file>
            arg_type('background', short='G', type=ARG_TYPE_CHAR), &    ! -G, --background <color>
            arg_type('foreground', short='P', type=ARG_TYPE_CHAR), &    ! -P, --foreground <color>
            arg_type('font',       short='A', type=ARG_TYPE_CHAR), &    ! -A, --font <name>
            arg_type('title',      short='T', type=ARG_TYPE_CHAR), &    ! -T, --title <title>
            arg_type('width',      short='W', type=ARG_TYPE_INTEGER), & ! -W, --width <n>
            arg_type('height',     short='H', type=ARG_TYPE_INTEGER)  & ! -H, --height <n>
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Read all other options.
        rc = dm_arg_get(args( 3), app%database)
        rc = dm_arg_get(args( 4), app%node)
        rc = dm_arg_get(args( 5), app%sensor)
        rc = dm_arg_get(args( 6), app%target)
        rc = dm_arg_get(args( 7), app%response)
        rc = dm_arg_get(args( 8), app%from)
        rc = dm_arg_get(args( 9), app%to)
        rc = dm_arg_get(args(10), terminal)
        rc = dm_arg_get(args(11), app%output)
        rc = dm_arg_get(args(12), app%background)
        rc = dm_arg_get(args(13), app%foreground)
        rc = dm_arg_get(args(14), app%font)
        rc = dm_arg_get(args(15), app%title)
        rc = dm_arg_get(args(16), app%width)
        rc = dm_arg_get(args(17), app%height)

        app%terminal = dm_plot_term_from_name(terminal)

        ! Validate settings.
        rc = E_INVALID

        if (.not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database ' // trim(app%database) // ' not found')
            return
        end if

        if (.not. dm_id_valid(app%node)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (.not. dm_id_valid(app%sensor)) then
            call dm_error_out(rc, 'invalid sensor id')
            return
        end if

        if (.not. dm_id_valid(app%target)) then
            call dm_error_out(rc, 'invalid target id')
            return
        end if

        if (.not. dm_id_valid(app%response)) then
            call dm_error_out(rc, 'invalid response name')
            return
        end if

        if (.not. dm_time_valid(app%from)) then
            call dm_error_out(rc, 'invalid from timestamp')
            return
        end if

        if (.not. dm_time_valid(app%to)) then
            call dm_error_out(rc, 'invalid to timestamp')
            return
        end if

        if (.not. dm_plot_term_valid(app%terminal)) then
            call dm_error_out(rc, 'invalid plot terminal')
            return
        end if

        if (app%width < 0) then
            call dm_error_out(rc, 'invalid plot width')
            return
        end if

        if (app%height < 0) then
            call dm_error_out(rc, 'invalid plot height')
            return
        end if

        select case (app%terminal)
            case (PLOT_TERM_GIF, PLOT_TERM_PNG, PLOT_TERM_PNG_CAIRO, PLOT_TERM_SVG)
                if (len_trim(app%output) == 0) then
                    call dm_error_out(rc, 'missing output path')
                    return
                end if
            case (PLOT_TERM_ANSI, PLOT_TERM_SIXEL, PLOT_TERM_X11)
                ! Ignore output file path.
                app%output = ' '
        end select

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads app configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.

        character(len=PLOT_TERM_NAME_LEN) :: terminal
        type(config_type)                 :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            rc = dm_config_get(config, 'background', app%background)
            rc = dm_config_get(config, 'database',   app%database)
            rc = dm_config_get(config, 'font',       app%font)
            rc = dm_config_get(config, 'foreground', app%foreground)
            rc = dm_config_get(config, 'terminal',   terminal)
            rc = dm_config_get(config, 'from',       app%from)
            rc = dm_config_get(config, 'height',     app%height)
            rc = dm_config_get(config, 'node',       app%node)
            rc = dm_config_get(config, 'output',     app%output)
            rc = dm_config_get(config, 'response',   app%response)
            rc = dm_config_get(config, 'sensor',     app%sensor)
            rc = dm_config_get(config, 'target',     app%target)
            rc = dm_config_get(config, 'title',      app%title)
            rc = dm_config_get(config, 'to',         app%to)
            rc = dm_config_get(config, 'width',      app%width)

            app%terminal = dm_plot_term_from_name(terminal)
        end if

        call dm_config_close(config)
    end function read_config

    integer function read_data_points(dps, database, node, sensor, target, response, from, to) result(rc)
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
            rc = dm_db_select(db, dps, node, sensor, target, response, from, to)
        end block db_block

        rc = max(dm_db_close(db), rc)
    end function read_data_points
end program dmgraph
