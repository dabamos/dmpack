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
    integer,          parameter :: APP_PATCH = 8

    type :: app_type
        !! Command-line arguments.
        character(len=FILE_PATH_LEN)     :: database  = ' '         !! Path to database.
        character(len=FILE_PATH_LEN)     :: output    = ' '         !! Output file path, empty or '-' for stdout.
        character(len=NODE_ID_LEN)       :: node_id   = ' '         !! Node id.
        character(len=SENSOR_ID_LEN)     :: sensor_id = ' '         !! Sensor id.
        character(len=TARGET_ID_LEN)     :: target_id = ' '         !! Target id.
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
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Create selected database type.
    rc = export(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function export(app) result(rc)
        type(app_type), intent(inout) :: app

        integer :: stat, unit
        logical :: first, header, is_file

        type(db_type)      :: db
        type(db_stmt_type) :: db_stmt

        type(beat_type)   :: beat
        type(dp_type)     :: dp
        type(log_type)    :: log
        type(node_type)   :: node
        type(observ_type) :: observ
        type(sensor_type) :: sensor
        type(target_type) :: target

        is_file = (dm_string_has(app%output) .and. app%output /= '-')

        ! Open file.
        unit = stdout

        if (is_file) then
            rc = E_IO
            open (action='write', file=trim(app%output), iostat=stat, newunit=unit, status='replace')
            if (stat /= 0) return
        end if

        ! Open database.
        rc = dm_db_open(db, app%database, read_only=.true., validate=.true.)

        if (rc == E_INVALID) then
            call dm_error_out(rc, 'invalid database')
            return
        end if

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to open database')
            return
        end if

        ! JSON array start.
        if (app%format == FORMAT_JSON) then
            write (unit, '("[")', advance='no', iostat=stat)
            if (stat /= 0) rc = E_WRITE
        end if

        ! Select and output records.
        first  = .true.
        header = app%header

        do while (dm_is_ok(rc))
            select case (app%type)
                case (TYPE_NODE);   rc = dm_db_select_nodes      (db, db_stmt, node, validate=first)
                case (TYPE_SENSOR); rc = dm_db_select_sensors    (db, db_stmt, sensor, validate=first)
                case (TYPE_TARGET); rc = dm_db_select_targets    (db, db_stmt, target, validate=first)
                case (TYPE_OBSERV); rc = dm_db_select_observs    (db, db_stmt, observ, node_id=app%node_id, sensor_id=app%sensor_id, target_id=app%target_id, from=app%from, to=app%to, validate=first)
                case (TYPE_LOG);    rc = dm_db_select_logs       (db, db_stmt, log, node_id=app%node_id, sensor_id=app%sensor_id, target_id=app%target_id, from=app%from, to=app%to, validate=first)
                case (TYPE_BEAT);   rc = dm_db_select_beats      (db, db_stmt, beat, validate=first)
                case (TYPE_DP);     rc = dm_db_select_data_points(db, db_stmt, dp, node_id=app%node_id, sensor_id=app%sensor_id, target_id=app%target_id, response_name=app%response, from=app%from, to=app%to, validate=first)
            end select

            if (rc == E_DB_DONE) exit

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to select from database')
                exit
            end if

            ! Output records in selected format.
            select case (app%format)
                case (FORMAT_BLOCK)
                    rc = E_INVALID
                    if (app%type == TYPE_DP) rc = dm_block_write(dp, unit)

                case (FORMAT_CSV)
                    select case (app%type)
                        case (TYPE_NODE);   rc = dm_csv_write(node,   unit, header, app%separator)
                        case (TYPE_SENSOR); rc = dm_csv_write(sensor, unit, header, app%separator)
                        case (TYPE_TARGET); rc = dm_csv_write(target, unit, header, app%separator)
                        case (TYPE_OBSERV); rc = dm_csv_write(observ, unit, header, app%separator)
                        case (TYPE_LOG);    rc = dm_csv_write(log,    unit, header, app%separator)
                        case (TYPE_BEAT);   rc = dm_csv_write(beat,   unit, header, app%separator)
                        case (TYPE_DP);     rc = dm_csv_write(dp,     unit, header, app%separator)
                    end select

                    header = .false.

                case (FORMAT_JSON)
                    if (.not. first) write (unit, '(",")', advance='no', iostat=stat)

                    select case (app%type)
                        case (TYPE_NODE);   write (unit, '(a)', advance='no', iostat=stat) dm_json_from(node)
                        case (TYPE_SENSOR); write (unit, '(a)', advance='no', iostat=stat) dm_json_from(sensor)
                        case (TYPE_TARGET); write (unit, '(a)', advance='no', iostat=stat) dm_json_from(target)
                        case (TYPE_OBSERV); write (unit, '(a)', advance='no', iostat=stat) dm_json_from(observ)
                        case (TYPE_LOG);    write (unit, '(a)', advance='no', iostat=stat) dm_json_from(log)
                        case (TYPE_BEAT);   write (unit, '(a)', advance='no', iostat=stat) dm_json_from(beat)
                        case (TYPE_DP);     write (unit, '(a)', advance='no', iostat=stat) dm_json_from(dp)
                    end select

                    if (stat /= 0) rc = E_WRITE

                    first = .false.

                case (FORMAT_JSONL)
                    select case (app%type)
                        case (TYPE_NODE);   rc = dm_json_write(node,   unit)
                        case (TYPE_SENSOR); rc = dm_json_write(sensor, unit)
                        case (TYPE_TARGET); rc = dm_json_write(target, unit)
                        case (TYPE_OBSERV); rc = dm_json_write(observ, unit)
                        case (TYPE_LOG);    rc = dm_json_write(log,    unit)
                        case (TYPE_BEAT);   rc = dm_json_write(beat,   unit)
                        case (TYPE_DP);     rc = dm_json_write(dp,     unit)
                    end select
            end select
        end do

        ! JSON array end.
        if (app%format == FORMAT_JSON) then
            write (unit, '("]")', advance='no', iostat=stat)
            if (stat /= 0) rc = E_WRITE
        end if

        call dm_db_finalize(db_stmt)
        call dm_db_close(db)

        if (is_file) close (unit)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to write data')
            return
        end if
    end function export

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app

        character(len=6) :: format_name, type_name
        type(arg_type)   :: args(12)

        args = [ &
            arg_type('database',  short='d', type=ARG_TYPE_DATABASE, required=.true.), & ! -d, --database <path>
            arg_type('output',    short='o', type=ARG_TYPE_STRING),                    & ! -o, --output <path>
            arg_type('node',      short='N', type=ARG_TYPE_ID),                        & ! -N, --node <id>
            arg_type('sensor',    short='S', type=ARG_TYPE_ID),                        & ! -S, --sensor <id>
            arg_type('target',    short='T', type=ARG_TYPE_ID),                        & ! -T, --target <id>
            arg_type('from',      short='B', type=ARG_TYPE_TIME),                      & ! -F, --from <timestamp>
            arg_type('to',        short='E', type=ARG_TYPE_TIME),                      & ! -T, --to <timestamp>
            arg_type('response',  short='R', type=ARG_TYPE_ID,     max_len=RESPONSE_NAME_LEN),                & ! -R, --response <name>
            arg_type('format',    short='f', type=ARG_TYPE_STRING, max_len=FORMAT_NAME_LEN, required=.true.), & ! -f, --format <string>
            arg_type('type',      short='t', type=ARG_TYPE_STRING, max_len=TYPE_NAME_LEN,   required=.true.), & ! -t, --type <string>
            arg_type('header',    short='H', type=ARG_TYPE_LOGICAL),                   & ! -H, --header
            arg_type('separator', short='s', type=ARG_TYPE_CHAR)                       & ! -a, --separator <char>
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args( 1), app%database)
        call dm_arg_get(args( 2), app%output)
        call dm_arg_get(args( 3), app%node_id)
        call dm_arg_get(args( 4), app%sensor_id)
        call dm_arg_get(args( 5), app%target_id)
        call dm_arg_get(args( 6), app%from)
        call dm_arg_get(args( 7), app%to)
        call dm_arg_get(args( 8), app%response)
        call dm_arg_get(args( 9), format_name)
        call dm_arg_get(args(10), type_name)
        call dm_arg_get(args(11), app%header)
        call dm_arg_get(args(12), app%separator)

        app%format = dm_format_from_name(format_name)
        app%type   = dm_type_from_name(type_name)

        rc = validate(app)
    end function read_args

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

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
            if (.not. dm_time_is_valid(app%from)) then
                call dm_error_out(rc, 'invalid or missing argument --from')
                return
            end if

            if (.not. dm_time_is_valid(app%to)) then
                call dm_error_out(rc, 'invalid or missing argument --to')
                return
            end if
        end if

        ! Observation and data point.
        if (app%type == TYPE_OBSERV .or. app%type == TYPE_DP) then
            if (.not. dm_id_is_valid(app%node_id)) then
                 call dm_error_out(rc, 'invalid or missing argument --node')
                 return
            end if

            if (.not. dm_id_is_valid(app%sensor_id)) then
                call dm_error_out(rc, 'invalid or missing argument --sensor')
                return
            end if

            if (.not. dm_id_is_valid(app%target_id)) then
                call dm_error_out(rc, 'invalid or missing argument --target')
                return
            end if
        end if

        ! Data point.
        if (app%type == TYPE_DP) then
            if (.not. dm_id_is_valid(app%response)) then
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
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_db_version(.true.)
    end subroutine version_callback
end program dmexport
