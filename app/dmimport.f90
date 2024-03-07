! dmimport.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmimport
    !! Imports nodes, observations, sensors, targets, and logs from CSV file
    !! into database.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmimport'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 0

    type :: app_type
        !! Command-line arguments.
        character(len=FILE_PATH_LEN) :: database  = ' '       !! Path to database.
        character(len=FILE_PATH_LEN) :: input     = ' '       !! Input file path.
        integer                      :: type      = TYPE_NONE !! Entity type.
        character                    :: quote     = ASCII_NUL !! CSV quote character (disabled by default).
        character                    :: separator = ','       !! CSV separator character.
        logical                      :: dry       = .false.   !! Dry run.
        logical                      :: verbose   = .false.   !! Print progress to standard output.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App type.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Create selected database type.
    rc = import(app)
    if (dm_is_error(rc)) call dm_stop(1)
contains
    integer function import(app) result(rc)
        integer(kind=i8), parameter :: PROGRESS_STEP_SIZE = 500_i8 !! Import progress step size.

        type(app_type), intent(inout) :: app

        integer          :: er, stat, unit
        integer(kind=i8) :: nrecs, nrows
        logical          :: exists, valid
        real(kind=r8)    :: dt
        type(db_type)    :: db
        type(timer_type) :: timer

        type(log_type)    :: log
        type(node_type)   :: node
        type(observ_type) :: observ
        type(sensor_type) :: sensor
        type(target_type) :: target

        ! Try to open input file.
        rc = E_IO
        open (action='read', file=trim(app%input), iostat=stat, newunit=unit)

        if (stat /= 0) then
            call dm_error_out(rc, 'failed to open file ' // trim(app%input))
            return
        end if

        if (app%verbose) then
            print '("Opened file ", a)', trim(app%input)
        end if

        import_block: block
            if (.not. app%dry) then
                ! Try to open database.
                rc = dm_db_open(db, app%database, validate=.true.)

                if (rc == E_INVALID) then
                    call dm_error_out(rc, 'invalid database ' // trim(app%database))
                    exit import_block
                end if

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to open database ' // trim(app%database))
                    exit import_block
                end if

                if (app%verbose) then
                    print '("Opened database ", a)', trim(app%database)
                end if

                ! Check for appropriate database table.
                select case (app%type)
                    case (TYPE_NODE)
                        rc = dm_db_has_table(db, SQL_TABLE_NODES, exists)
                    case (TYPE_SENSOR)
                        rc = dm_db_has_table(db, SQL_TABLE_SENSORS, exists)
                    case (TYPE_TARGET)
                        rc = dm_db_has_table(db, SQL_TABLE_TARGETS, exists)
                    case (TYPE_OBSERV)
                        rc = dm_db_has_table(db, SQL_TABLE_OBSERVS, exists)
                    case (TYPE_LOG)
                        rc = dm_db_has_table(db, SQL_TABLE_LOGS, exists)
                end select

                if (dm_is_error(rc) .or. .not. exists) then
                    rc = E_INVALID
                    call dm_error_out(rc, 'database table not found')
                    exit import_block
                end if
            end if

            ! Start timer.
            if (app%verbose) call dm_timer_start(timer)

            if (.not. app%dry) then
                ! Start database transaction.
                rc = dm_db_begin(db)

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'database transaction error')
                    exit import_block
                end if
            end if

            nrecs = 0 ! Number of records written to database.
            nrows = 0 ! Number of rows in CSV file.

            ! Read records from file and insert them into database.
            read_loop: do
                nrows = nrows + 1

                ! Read record from file.
                select case (app%type)
                    case (TYPE_NODE)
                        rc = dm_csv_read(node, unit, app%separator, app%quote)
                    case (TYPE_SENSOR)
                        rc = dm_csv_read(sensor, unit, app%separator, app%quote)
                    case (TYPE_TARGET)
                        rc = dm_csv_read(target, unit, app%separator, app%quote)
                    case (TYPE_OBSERV)
                        rc = dm_csv_read(observ, unit, app%separator, app%quote)
                    case (TYPE_LOG)
                        rc = dm_csv_read(log, unit, app%separator, app%quote)
                end select

                ! Ignore comments and empty rows.
                if (rc == E_EOR) then
                    rc = E_NONE
                    cycle read_loop
                end if

                ! End of file reached.
                if (rc == E_EOF) then
                    rc = E_NONE
                    exit read_loop
                end if

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to read record in row ' // dm_itoa(nrows))
                    exit read_loop
                end if

                if (app%dry) then
                    ! Validate record.
                    select case (app%type)
                        case (TYPE_NODE)
                            valid = dm_node_valid(node)
                        case (TYPE_SENSOR)
                            valid = dm_sensor_valid(sensor)
                        case (TYPE_TARGET)
                            valid = dm_target_valid(target)
                        case (TYPE_OBSERV)
                            valid = dm_observ_valid(observ)
                        case (TYPE_LOG)
                            valid = dm_log_valid(log)
                    end select

                    if (.not. valid) then
                        rc = E_INVALID
                        call dm_error_out(rc, 'invalid record in row ' // dm_itoa(nrows))
                        exit read_loop
                    end if

                    ! Skip database insert on dry run.
                    cycle read_loop
                end if

                ! Validate and insert record.
                select case (app%type)
                    case (TYPE_NODE)
                        rc = dm_db_insert(db, node)
                    case (TYPE_SENSOR)
                        rc = dm_db_insert(db, sensor)
                    case (TYPE_TARGET)
                        rc = dm_db_insert(db, target)
                    case (TYPE_OBSERV)
                        rc = dm_db_insert(db, observ)
                    case (TYPE_LOG)
                        rc = dm_db_insert(db, log)
                end select

                ! Handle database result.
                select case (rc)
                    case (E_NONE)
                        nrecs = nrecs + 1
                        if (.not. app%verbose) cycle read_loop
                        if (modulo(nrecs, PROGRESS_STEP_SIZE) == 0) then
                            print '("Imported ", i0, " records")', nrecs
                        end if
                    case (E_INVALID)
                        call dm_error_out(rc, 'invalid record in row ' // dm_itoa(nrows))
                        exit read_loop
                    case (E_DB_CONSTRAINT)
                        call dm_error_out(rc, 'record in row ' // dm_itoa(nrows) // ' already exists in database')
                        exit read_loop
                    case default
                        call dm_error_out(rc, 'failed to insert record in row ' // dm_itoa(nrows))
                        exit read_loop
                end select
            end do read_loop

            if (.not. app%dry) then
                ! Rollback transaction on error.
                if (dm_is_error(rc)) then
                    er = dm_db_rollback(db)

                    if (dm_is_error(er)) then
                        call dm_error_out(er, 'failed to roll back database transaction')
                        exit import_block
                    end if

                    if (app%verbose) print '("All database changes rolled back")'
                    exit import_block
                end if

                ! Commit transaction to database.
                rc = dm_db_commit(db)

                ! Rollback transaction on error.
                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to commit database transaction')
                    er = dm_db_rollback(db)

                    if (dm_is_error(er)) then
                        call dm_error_out(er, 'failed to roll back database transaction')
                        exit import_block
                    end if

                    if (app%verbose) print '("All database changes rolled back")'
                    exit import_block
                end if
            end if

            ! Output statistics.
            if (app%verbose) then
                dt = dm_timer_stop(timer)
                print '("Read ", i0, " rows from file ", a)', nrows, trim(app%input)
                print '("Imported ", i0, " records in ", f0.1, " seconds")', nrecs, dt
            end if
        end block import_block

        ! Close database.
        if (.not. app%dry) then
            if (dm_is_error(dm_db_close(db))) rc = E_DB
            if (app%verbose) print '("Closed database ", a)', trim(app%database)
        end if

        ! Close file.
        close (unit)
        if (app%verbose) print '("Closed file ", a)', trim(app%input)

        if (dm_is_error(rc)) return
        if (app%verbose) print '("Finished")'
    end function import

    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(inout) :: app

        character(len=6) :: type_name
        type(arg_type)   :: args(7)

        type_name = ' '

        args = [ &
            arg_type('type',      short='t', type=ARG_TYPE_CHAR, max_len=TYPE_NAME_LEN, required=.true.), & ! -t, --type <string>
            arg_type('database',  short='d', type=ARG_TYPE_DB), &                    ! -d, --database <path>
            arg_type('input',     short='i', type=ARG_TYPE_CHAR, required=.true.), & ! -i, --input <path>
            arg_type('quote',     short='q', type=ARG_TYPE_CHAR, max_len=1), &       ! -q, --quote <char>
            arg_type('separator', short='s', type=ARG_TYPE_CHAR, max_len=1), &       ! -s, --separator <char>
            arg_type('dry',       short='y', type=ARG_TYPE_BOOL), &                  ! -y, --dry
            arg_type('verbose',   short='V', type=ARG_TYPE_BOOL) &                   ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), type_name)
        rc = dm_arg_get(args(2), app%database)
        rc = dm_arg_get(args(3), app%input)
        rc = dm_arg_get(args(4), app%quote)
        rc = dm_arg_get(args(5), app%separator)
        rc = dm_arg_get(args(6), app%dry)
        rc = dm_arg_get(args(7), app%verbose)

        app%type = dm_type_from_name(type_name)

        rc = E_INVALID

        if (.not. app%dry) then
            if (len_trim(app%database) == 0) then
                call dm_error_out(rc, 'argument --database required')
                return
            end if

            if (.not. dm_file_exists(app%database)) then
                call dm_error_out(rc, 'database ' // trim(app%database) // ' not found')
                return
            end if
        end if

        ! Validate data type.
        select case (app%type)
            case (TYPE_NODE, TYPE_SENSOR, TYPE_TARGET, TYPE_OBSERV, TYPE_LOG)
                continue
            case default
                call dm_error_out(rc, 'invalid type')
                return
        end select

        rc = E_NONE
    end function read_args
end program dmimport
