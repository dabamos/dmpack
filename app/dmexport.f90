! dmexport.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmexport
    !! Exports nodes, observations, sensors, targets, and logs from database to
    !! file, in ASCII block, CSV, JSON, or JSON Lines format.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmexport'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 1

    type :: app_type
        !! Command-line arguments.
        character(len=FILE_PATH_LEN)     :: database  = ' '         !! Path to database.
        character(len=FILE_PATH_LEN)     :: output    = ' '         !! Output file path, empty or '-' for stdout.
        character(len=NODE_ID_LEN)       :: node      = ' '         !! Node id.
        character(len=SENSOR_ID_LEN)     :: sensor    = ' '         !! Sensor id.
        character(len=TARGET_ID_LEN)     :: target    = ' '         !! Target id.
        character(len=TIME_LEN)          :: from      = ' '         !! Time range start.
        character(len=TIME_LEN)          :: to        = ' '         !! Time range end.
        character(len=RESPONSE_NAME_LEN) :: response  = ' '         !! Response name.
        integer                          :: format    = FORMAT_NONE !! Output format.
        integer                          :: type      = TYPE_NONE   !! Entity type.
        logical                          :: header    = .false.     !! CSV header.
        character                        :: separator = ','         !! CSV separator character.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Create selected database type.
    rc = export(app)
    if (dm_is_error(rc)) call dm_stop(1)
contains
    integer function export(app) result(rc)
        type(app_type), intent(inout) :: app

        integer        :: stat, unit
        logical        :: is_file
        type (db_type) :: db

        ! Arrays to hold the records from database.
        type(beat_type),   allocatable :: beats(:)
        type(dp_type),     allocatable :: data_points(:)
        type(log_type),    allocatable :: logs(:)
        type(node_type),   allocatable :: nodes(:)
        type(observ_type), allocatable :: observs(:)
        type(sensor_type), allocatable :: sensors(:)
        type(target_type), allocatable :: targets(:)

        is_file = .false.
        if (len_trim(app%output) > 0 .and. app%output /= '-') is_file = .true.

        rc = dm_db_open(db, app%database, read_only=.true., validate=.true.)

        if (rc == E_INVALID) then
            call dm_error_out(rc, 'invalid database')
            return
        end if

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to open database')
            return
        end if

        ! Select records from database.
        select case (app%type)
            case (TYPE_NODE)
                rc = dm_db_select(db, nodes)
            case (TYPE_SENSOR)
                rc = dm_db_select(db, sensors)
            case (TYPE_TARGET)
                rc = dm_db_select(db, targets)
            case (TYPE_OBSERV)
                rc = dm_db_select(db, observs, node_id=app%node, sensor_id=app%sensor, &
                                  target_id=app%target, from=app%from, to=app%to)
            case (TYPE_LOG)
                rc = dm_db_select(db, logs, node_id=app%node, sensor_id=app%sensor, &
                                  target_id=app%target, from=app%from, to=app%to)
            case (TYPE_BEAT)
                rc = dm_db_select(db, beats)
            case (TYPE_DP)
                rc = dm_db_select(db, data_points, node_id=app%node, sensor_id=app%sensor, &
                                  target_id=app%target, response_name=app%response, &
                                  from=app%from, to=app%to)
        end select

        if (dm_is_error(rc)) call dm_error_out(rc, 'failed to select from database')
        if (dm_is_error(dm_db_close(db))) rc = E_DB
        if (dm_is_error(rc)) return

        unit = stdout

        ! Open file.
        if (is_file) then
            rc = E_IO
            open (action  = 'write', &
                  file    = trim(app%output), &
                  iostat  = stat, &
                  newunit = unit, &
                  status = 'replace')
            if (stat /= 0) return
        end if

        ! Output records in selected format.
        select case (app%format)
            case (FORMAT_BLOCK)
                rc = E_INVALID
                if (app%type == TYPE_DP) then
                    rc = dm_block_write(data_points, unit)
                end if

            case (FORMAT_CSV)
                select case (app%type)
                    case (TYPE_NODE)
                        rc = dm_csv_write(nodes, unit, app%header, app%separator)
                    case (TYPE_SENSOR)
                        rc = dm_csv_write(sensors, unit, app%header, app%separator)
                    case (TYPE_TARGET)
                        rc = dm_csv_write(targets, unit, app%header, app%separator)
                    case (TYPE_OBSERV)
                        rc = dm_csv_write(observs, unit, app%header, app%separator)
                    case (TYPE_LOG)
                        rc = dm_csv_write(logs, unit, app%header, app%separator)
                    case (TYPE_BEAT)
                        rc = dm_csv_write(beats, unit, app%header, app%separator)
                    case (TYPE_DP)
                        rc = dm_csv_write(data_points, unit, app%header, app%separator)
                end select

            case (FORMAT_JSON)
                select case (app%type)
                    case (TYPE_NODE)
                        rc = dm_json_write(nodes, unit)
                    case (TYPE_SENSOR)
                        rc = dm_json_write(sensors, unit)
                    case (TYPE_TARGET)
                        rc = dm_json_write(targets, unit)
                    case (TYPE_OBSERV)
                        rc = dm_json_write(observs, unit)
                    case (TYPE_LOG)
                        rc = dm_json_write(logs, unit)
                    case (TYPE_BEAT)
                        rc = dm_json_write(beats, unit)
                    case (TYPE_DP)
                        rc = dm_json_write(data_points, unit)
                end select

            case (FORMAT_JSONL)
                select case (app%type)
                    case (TYPE_NODE)
                        rc = dm_jsonl_write(nodes, unit)
                    case (TYPE_SENSOR)
                        rc = dm_jsonl_write(sensors, unit)
                    case (TYPE_TARGET)
                        rc = dm_jsonl_write(targets, unit)
                    case (TYPE_OBSERV)
                        rc = dm_jsonl_write(observs, unit)
                    case (TYPE_LOG)
                        rc = dm_jsonl_write(logs, unit)
                    case (TYPE_BEAT)
                        rc = dm_jsonl_write(beats, unit)
                    case (TYPE_DP)
                        rc = dm_jsonl_write(data_points, unit)
                end select
        end select

        if (is_file) close (unit)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to write data')
            return
        end if
    end function export

    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(inout) :: app
        type(arg_type)                :: args(12)

        character(len=6) :: format, type

        rc = E_NONE

        args = [ &
            arg_type('database',  short='d', type=ARG_TYPE_DB, required=.true.), & ! -d, --database <path>
            arg_type('output',    short='o', type=ARG_TYPE_CHAR), &                ! -o, --output <path>
            arg_type('node',      short='N', type=ARG_TYPE_ID), &                  ! -N, --node <id>
            arg_type('sensor',    short='S', type=ARG_TYPE_ID), &                  ! -S, --sensor <id>
            arg_type('target',    short='T', type=ARG_TYPE_ID), &                  ! -T, --target <id>
            arg_type('from',      short='B', type=ARG_TYPE_TIME), &                ! -F, --from <timestamp>
            arg_type('to',        short='E', type=ARG_TYPE_TIME), &                ! -T, --to <timestamp>
            arg_type('response',  short='R', type=ARG_TYPE_ID,   max_len=RESPONSE_NAME_LEN),                & ! -R, --response <name>
            arg_type('format',    short='f', type=ARG_TYPE_CHAR, max_len=FORMAT_NAME_LEN, required=.true.), & ! -f, --format <string>
            arg_type('type',      short='t', type=ARG_TYPE_CHAR, max_len=TYPE_NAME_LEN,   required=.true.), & ! -t, --type <string>
            arg_type('header',    short='H', type=ARG_TYPE_BOOL), &                ! -H, --header
            arg_type('separator', short='s', type=ARG_TYPE_CHAR, max_len=1) &      ! -a, --separator <char>
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args( 1), app%database)
        rc = dm_arg_get(args( 2), app%output)
        rc = dm_arg_get(args( 3), app%node)
        rc = dm_arg_get(args( 4), app%sensor)
        rc = dm_arg_get(args( 5), app%target)
        rc = dm_arg_get(args( 6), app%from)
        rc = dm_arg_get(args( 7), app%to)
        rc = dm_arg_get(args( 8), app%response)
        rc = dm_arg_get(args( 9), format)
        rc = dm_arg_get(args(10), type)
        rc = dm_arg_get(args(11), app%header)
        rc = dm_arg_get(args(12), app%separator)

        app%format = dm_format_from_name(format)
        app%type   = dm_type_from_name(type)

        rc = E_INVALID

        ! Serialisation format.
        select case (app%format)
            case (FORMAT_BLOCK, FORMAT_CSV, FORMAT_JSON, FORMAT_JSONL)
                continue
            case default
                call dm_error_out(rc, 'invalid format')
                return
        end select

        ! Data type.
        select case (app%type)
            case (TYPE_NODE, TYPE_SENSOR, TYPE_TARGET, TYPE_OBSERV, TYPE_LOG, TYPE_BEAT, TYPE_DP)
                continue
            case default
                call dm_error_out(rc, 'invalid type')
                return
        end select

        ! Log, observation, and data point.
        if (app%type == TYPE_LOG .or. app%type == TYPE_OBSERV .or. app%type == TYPE_DP) then
            if (.not. dm_time_valid(app%from)) then
                call dm_error_out(rc, 'invalid or missing argument --from')
                return
            end if

            if (.not. dm_time_valid(app%to)) then
                call dm_error_out(rc, 'invalid or missing argument --to')
                return
            end if
        end if

        ! Observation and data point.
        if (app%type == TYPE_OBSERV .or. app%type == TYPE_DP) then
            if (.not. dm_id_valid(app%node)) then
                 call dm_error_out(rc, 'invalid or missing argument --node')
                 return
            end if

            if (.not. dm_id_valid(app%sensor)) then
                call dm_error_out(rc, 'invalid or missing argument --sensor')
                return
            end if

            if (.not. dm_id_valid(app%target)) then
                call dm_error_out(rc, 'invalid or missing argument --target')
                return
            end if
        end if

        ! Data point.
        if (app%type == TYPE_DP) then
            if (.not. dm_id_valid(app%response)) then
                 call dm_error_out(rc, 'invalid or missing argument --response')
                 return
            end if
        else
            if (app%format == FORMAT_BLOCK) then
                call dm_error_out(rc, 'block format not supported')
                return
            end if
        end if

        rc = E_NONE
    end function read_args
end program dmexport
