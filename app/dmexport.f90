! dmexport.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmexport
    !! Exports nodes, observations, sensors, targets, and logs from database to
    !! file, in ASCII block, CSV, JSON, JSON Lines, NML, or TSV format.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmexport'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    type :: app_type
        !! Command-line arguments.
        character(FILE_PATH_LEN)     :: database  = ' '         !! Path to database.
        character(FILE_PATH_LEN)     :: output    = ' '         !! Output file path, empty or '-' for stdout.
        character(NODE_ID_LEN)       :: node_id   = ' '         !! Node id.
        character(SENSOR_ID_LEN)     :: sensor_id = ' '         !! Sensor id.
        character(TARGET_ID_LEN)     :: target_id = ' '         !! Target id.
        character(TIME_LEN)          :: from      = ' '         !! Time range start.
        character(TIME_LEN)          :: to        = ' '         !! Time range end.
        character(RESPONSE_NAME_LEN) :: response  = ' '         !! Response name.
        integer                      :: format    = FORMAT_NONE !! Output format.
        integer                      :: type      = TYPE_NONE   !! Entity type.
        logical                      :: header    = .false.     !! CSV header.
        character                    :: separator = ','         !! CSV separator character.
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
        logical :: empty, first, is_file

        type(db_type)      :: db
        type(db_stmt_type) :: dbs
        type(serial_class) :: serial

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

        ! Export rows.
        first = .true.

        do
            empty = (rc == E_DB_NO_ROWS)

            select case (app%type)
                case (TYPE_NODE)
                    rc = dm_db_select(db, dbs, node, validate=first)
                    if (first) call serial%create(node, app%format, unit=unit, empty=empty, header=app%header, separator=app%separator)
                    if (rc /= E_NONE) exit
                    call serial%next(node)

                case (TYPE_SENSOR)
                    rc = dm_db_select(db, dbs, sensor, validate=first)
                    if (first) call serial%create(sensor, app%format, unit=unit, empty=empty, header=app%header, separator=app%separator)
                    if (rc /= E_NONE) exit
                    call serial%next(sensor)

                case (TYPE_TARGET)
                    rc = dm_db_select(db, dbs, target, validate=first)
                    if (first) call serial%create(target, app%format, unit=unit, empty=empty, header=app%header, separator=app%separator)
                    if (rc /= E_NONE) exit
                    call serial%next(target)

                case (TYPE_OBSERV)
                    rc = dm_db_select(db, dbs, observ, node_id=app%node_id, sensor_id=app%sensor_id, target_id=app%target_id, from=app%from, to=app%to, validate=first)
                    if (first) call serial%create(observ, app%format, unit=unit, empty=empty, header=app%header, separator=app%separator)
                    if (rc /= E_NONE) exit
                    call serial%next(observ)

                case (TYPE_LOG)
                    rc = dm_db_select(db, dbs, log, node_id=app%node_id, sensor_id=app%sensor_id, target_id=app%target_id, from=app%from, to=app%to, validate=first)
                    if (first) call serial%create(log, app%format, unit=unit, empty=empty, header=app%header, separator=app%separator)
                    if (rc /= E_NONE) exit
                    call serial%next(log)

                case (TYPE_BEAT)
                    rc = dm_db_select(db, dbs, beat, validate=first)
                    if (first) call serial%create(beat, app%format, unit=unit, empty=empty, header=app%header, separator=app%separator)
                    if (rc /= E_NONE) exit
                    call serial%next(beat)

                case (TYPE_DP)
                    rc = dm_db_select(db, dbs, dp, node_id=app%node_id, sensor_id=app%sensor_id, target_id=app%target_id, response_name=app%response, from=app%from, to=app%to, validate=first)
                    if (first .and. app%format /= FORMAT_BLOCK) call serial%create(dp, app%format, unit=unit, empty=empty, header=app%header, separator=app%separator)
                    if (rc /= E_NONE) exit

                    if (app%format == FORMAT_BLOCK) then
                        stat = dm_block_write(dp, unit)
                    else
                        call serial%next(dp)
                    end if
            end select

            first = .false.

            if (rc == E_DB_DONE) exit

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to select from database')
                exit
            end if
        end do

        call serial%finalize()
        call dm_db_finalize(dbs)
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

        character(6)           :: format_name, type_name
        type(arg_parser_class) :: parser

        call parser%add('database',  short='d', type=ARG_TYPE_DATABASE, required=.true., exist=.true.)          ! -d, --database <path>
        call parser%add('output',    short='o', type=ARG_TYPE_FILE)                                             ! -o, --output <path>
        call parser%add('node',      short='N', type=ARG_TYPE_ID)                                               ! -N, --node <id>
        call parser%add('sensor',    short='S', type=ARG_TYPE_ID)                                               ! -S, --sensor <id>
        call parser%add('target',    short='T', type=ARG_TYPE_ID)                                               ! -T, --target <id>
        call parser%add('from',      short='B', type=ARG_TYPE_TIME)                                             ! -F, --from <timestamp>
        call parser%add('to',        short='E', type=ARG_TYPE_TIME)                                             ! -T, --to <timestamp>
        call parser%add('response',  short='R', type=ARG_TYPE_ID,     max_len=RESPONSE_NAME_LEN)                ! -R, --response <name>
        call parser%add('format',    short='f', type=ARG_TYPE_STRING, max_len=FORMAT_NAME_LEN, required=.true.) ! -f, --format <string>
        call parser%add('type',      short='t', type=ARG_TYPE_STRING, max_len=TYPE_NAME_LEN,   required=.true.) ! -t, --type <string>
        call parser%add('header',    short='H', type=ARG_TYPE_LOGICAL)                                          ! -H, --header
        call parser%add('separator', short='s', type=ARG_TYPE_CHAR)                                             ! -a, --separator <char>

        ! Read all command-line arguments.
        rc = parser%read(version_callback)
        if (dm_is_error(rc)) return

        call parser%get('database',  app%database)
        call parser%get('output',    app%output)
        call parser%get('node',      app%node_id)
        call parser%get('sensor',    app%sensor_id)
        call parser%get('target',    app%target_id)
        call parser%get('from',      app%from)
        call parser%get('to',        app%to)
        call parser%get('response',  app%response)
        call parser%get('format',    format_name)
        call parser%get('type',      type_name)
        call parser%get('header',    app%header)
        call parser%get('separator', app%separator)

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
            case (FORMAT_BLOCK, FORMAT_CSV, FORMAT_JSON, FORMAT_JSONL, FORMAT_NML, FORMAT_TSV)
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
        if (app%type == TYPE_DP .and. .not. dm_id_is_valid(app%response)) then
            call dm_error_out(rc, 'invalid or missing argument --response')
            return
        end if

        ! Block format.
        if (app%type /= TYPE_DP .and. app%format == FORMAT_BLOCK) then
            call dm_error_out(rc, 'block format not supported')
            return
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
