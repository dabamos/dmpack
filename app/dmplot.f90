! dmplot.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmplot
    !! Creates plots from time series.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmplot'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 8

    character(len=*), parameter :: APP_XLABEL = 'Time'

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)            :: name       = APP_NAME           !! Name of instance and POSIX semaphore.
        character(len=FILE_PATH_LEN)     :: config     = ' '                !! Path to configuration file.
        character(len=FILE_PATH_LEN)     :: database   = ' '                !! Path to observation database.
        character(len=NODE_ID_LEN)       :: node_id    = ' '                !! Node id.
        character(len=SENSOR_ID_LEN)     :: sensor_id  = ' '                !! Sensor id.
        character(len=TARGET_ID_LEN)     :: target_id  = ' '                !! Target id.
        character(len=RESPONSE_NAME_LEN) :: response   = ' '                !! Response name.
        character(len=TIME_LEN)          :: from       = ' '                !! Start of time range (ISO 8601).
        character(len=TIME_LEN)          :: to         = ' '                !! End of time range (ISO 8601).
        integer                          :: terminal   = PLOT_TERMINAL_NONE !! Plot terminal backend.
        character(len=FILE_PATH_LEN)     :: output     = ' '                !! Path of plot file.
        character(len=8)                 :: background = ' '                !! Background colour.
        character(len=8)                 :: foreground = ' '                !! Foreground colour (graph).
        character(len=32)                :: font       = ' '                !! Font name.
        character(len=80)                :: title      = ' '                !! Plot title.
        integer                          :: width      = 1000               !! Plot width.
        integer                          :: height     = 400                !! Plot height.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments and configuration file options.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Create plot.
    rc = create_plot(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function create_graph(dps, terminal, output, background, foreground, &
                                  font, title, width, height, xlabel, ylabel) result(rc)
        !! Writes plot to file or shows X11 window.
        type(dp_type),    intent(inout)        :: dps(:)     !! Data points array.
        integer,          intent(in)           :: terminal   !! Plot terminal.
        character(len=*), intent(in), optional :: output     !! Output file.
        character(len=*), intent(in), optional :: background !! Background colour.
        character(len=*), intent(in), optional :: foreground !! Foreground colour.
        character(len=*), intent(in), optional :: font       !! Plot font.
        character(len=*), intent(in), optional :: title      !! Plot title.
        integer,          intent(in), optional :: width      !! Plot width.
        integer,          intent(in), optional :: height     !! Plot height.
        character(len=*), intent(in), optional :: xlabel     !! X label.
        character(len=*), intent(in), optional :: ylabel     !! Y label.

        type(plot_type) :: plot

        plot%terminal = terminal

        if (plot%terminal == PLOT_TERMINAL_X11) plot%persist    = .true.
        if (dm_string_is_present(output))       plot%output     = output
        if (dm_string_is_present(background))   plot%background = background
        if (dm_string_is_present(foreground))   plot%foreground = foreground
        if (dm_string_is_present(font))         plot%font       = font
        if (dm_string_is_present(title))        plot%title      = title
        if (dm_string_is_present(xlabel))       plot%xlabel     = xlabel
        if (dm_string_is_present(ylabel))       plot%ylabel     = ylabel

        if (present(width)) then
            if (width > 0) plot%width = width
        end if

        if (present(height)) then
            if (height > 0) plot%height = height
        end if

        rc = dm_plot_lines(plot, dps)
    end function create_graph

    integer function create_plot(app) result(rc)
        type(app_type), intent(inout) :: app !! App type.

        character(len=:), allocatable :: path
        type(dp_type),    allocatable :: dps(:)

        plot_block: block
            ! Read data points from database.
            rc = read_data_points(dps, app%database, app%node_id, app%sensor_id, app%target_id, &
                                  app%response, app%from, app%to)
            if (dm_is_error(rc)) exit plot_block

            ! Parse output path for format descriptors.
            path = dm_time_parse_string(app%output)

            ! Create plot.
            rc = create_graph(dps, app%terminal, path, app%background, app%foreground, app%font, &
                              app%title, app%width, app%height, APP_XLABEL, app%response)
        end block plot_block

        if (rc == E_DB_NO_ROWS) then
            call dm_error_out(rc, 'no observations found in database ' // app%database)
        else if (dm_is_error(rc)) then
            call dm_error_out(rc)
        end if
    end function create_plot

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
            rc = dm_db_select_data_points(db, dps, node, sensor, target, response, from, to, error=E_NONE)
        end block db_block

        call dm_db_close(db)
    end function read_data_points

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from file.
        type(app_type), intent(out) :: app !! App type.

        character(len=PLOT_TERMINAL_NAME_LEN) :: terminal_name
        type(arg_class)                       :: arg

        call arg%create()
        call arg%add('name',       short='n', type=ARG_TYPE_ID)       ! -n, --name <string>
        call arg%add('config',     short='c', type=ARG_TYPE_FILE)     ! -c, --config <path>
        call arg%add('database',   short='d', type=ARG_TYPE_DATABASE) ! -d, --database <file>
        call arg%add('node',       short='N', type=ARG_TYPE_ID)       ! -N, --node <id>
        call arg%add('sensor',     short='S', type=ARG_TYPE_ID)       ! -S, --sensor <id>
        call arg%add('target',     short='T', type=ARG_TYPE_ID)       ! -T, --target <id>
        call arg%add('response',   short='R', type=ARG_TYPE_ID,     max_len=RESPONSE_NAME_LEN)      ! -R, --response <name>
        call arg%add('from',       short='B', type=ARG_TYPE_TIME)     ! -B, --from <timestamp>
        call arg%add('to',         short='E', type=ARG_TYPE_TIME)     ! -E, --to <timestamp>
        call arg%add('terminal',   short='m', type=ARG_TYPE_STRING, max_len=PLOT_TERMINAL_NAME_LEN) ! -m, --terminal <name>
        call arg%add('output',     short='o', type=ARG_TYPE_STRING)   ! -o, --output <file>
        call arg%add('background', short='G', type=ARG_TYPE_STRING)   ! -G, --background <color>
        call arg%add('foreground', short='P', type=ARG_TYPE_STRING)   ! -P, --foreground <color>
        call arg%add('font',       short='A', type=ARG_TYPE_STRING)   ! -A, --font <name>
        call arg%add('title',      short='C', type=ARG_TYPE_STRING)   ! -C, --title <title>
        call arg%add('width',      short='W', type=ARG_TYPE_INTEGER)  ! -W, --width <n>
        call arg%add('height',     short='H', type=ARG_TYPE_INTEGER)  ! -H, --height <n>

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('name',   app%name)
        call arg%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Read all other options.
        call arg%get('database',   app%database)
        call arg%get('node',       app%node_id)
        call arg%get('sensor',     app%sensor_id)
        call arg%get('target',     app%target_id)
        call arg%get('response',   app%response)
        call arg%get('from',       app%from)
        call arg%get('to',         app%to)
        call arg%get('terminal',   terminal_name)
        call arg%get('output',     app%output)
        call arg%get('background', app%background)
        call arg%get('foreground', app%foreground)
        call arg%get('font',       app%font)
        call arg%get('title',      app%title)
        call arg%get('width',      app%width)
        call arg%get('height',     app%height)
        call arg%destroy()

        app%terminal = dm_plot_terminal_from_name(terminal_name)

        select case (app%terminal)
            case (PLOT_TERMINAL_ANSI, PLOT_TERMINAL_SIXELGD, PLOT_TERMINAL_SIXELTEK, PLOT_TERMINAL_X11)
                app%output = ' ' ! Ignore output file path.
        end select

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads app configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.

        character(len=PLOT_TERMINAL_NAME_LEN) :: terminal
        type(config_class)                    :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('background', app%background)
            call config%get('database',   app%database)
            call config%get('font',       app%font)
            call config%get('foreground', app%foreground)
            call config%get('terminal',   terminal)
            call config%get('from',       app%from)
            call config%get('height',     app%height)
            call config%get('node',       app%node_id)
            call config%get('output',     app%output)
            call config%get('response',   app%response)
            call config%get('sensor',     app%sensor_id)
            call config%get('target',     app%target_id)
            call config%get('title',      app%title)
            call config%get('to',         app%to)
            call config%get('width',      app%width)

            app%terminal = dm_plot_terminal_from_name(terminal)
        end if

        call config%close()
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (.not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database ' // trim(app%database) // ' not found')
            return
        end if

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (.not. dm_id_is_valid(app%sensor_id)) then
            call dm_error_out(rc, 'invalid or missing sensor id')
            return
        end if

        if (.not. dm_id_is_valid(app%target_id)) then
            call dm_error_out(rc, 'invalid or missing target id')
            return
        end if

        if (.not. dm_id_is_valid(app%response)) then
            call dm_error_out(rc, 'invalid or missing response name')
            return
        end if

        if (.not. dm_time_is_valid(app%from)) then
            call dm_error_out(rc, 'invalid or missing timestamp from')
            return
        end if

        if (.not. dm_time_is_valid(app%to)) then
            call dm_error_out(rc, 'invalid or missing timestamp to')
            return
        end if

        if (.not. dm_plot_terminal_is_valid(app%terminal)) then
            call dm_error_out(rc, 'invalid or missing plot terminal')
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
            case (PLOT_TERMINAL_GIF,        &
                  PLOT_TERMINAL_GPIC,       &
                  PLOT_TERMINAL_PNG,        &
                  PLOT_TERMINAL_PNGCAIRO,   &
                  PLOT_TERMINAL_POSTSCRIPT, &
                  PLOT_TERMINAL_SVG)
                if (.not. dm_string_has(app%output)) then
                    call dm_error_out(rc, 'missing output path')
                    return
                end if
        end select

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        logical :: found

        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 2(1x, a))', dm_plot_version(.true., found), dm_lua_version(.true.), dm_db_version(.true.)
        if (.not. found) call dm_error_out(E_NOT_FOUND, 'Gnuplot binary not found')
    end subroutine version_callback
end program dmplot
