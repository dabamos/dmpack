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

    type :: app_type
        !! Command-line arguments.
        character(len=FILE_PATH_LEN) :: database  = ' '       !! Path to database.
        character(len=FILE_PATH_LEN) :: input     = ' '       !! Input file path.
        integer                      :: type      = TYPE_NONE !! Entity type.
        character                    :: separator = ','       !! CSV separator character.
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
        type(app_type), intent(inout) :: app

        integer          :: er, fu, stat
        integer(kind=i8) :: nrecs, nrows
        real(kind=r8)    :: dt
        type (db_type)   :: db
        type(timer_type) :: timer

        type(log_type)    :: log
        type(node_type)   :: node
        type(observ_type) :: observ
        type(sensor_type) :: sensor
        type(target_type) :: target

        ! Does the input file exist?
        rc = E_NOT_FOUND

        if (.not. dm_file_exists(app%input)) then
            call dm_error_out(rc, 'input file ' // trim(app%input) // ' not found')
            return
        end if

        ! Try to open input file.
        rc = E_IO

        open (action='read', file=trim(app%input), iostat=stat, newunit=fu)

        if (stat /= 0) then
            call dm_error_out(rc, 'failed to open input file ' // trim(app%input))
            return
        end if

        if (app%verbose) then
            print '("Opened input file ", a)', trim(app%input)
        end if

        db_block: block
            ! Try to open database.
            rc = dm_db_open(db, app%database, validate=.true.)

            if (rc == E_INVALID) then
                call dm_error_out(rc, 'invalid database ' // trim(app%database))
                exit db_block
            end if

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to open database ' // trim(app%database))
                exit db_block
            end if

            if (app%verbose) then
                print '("Opened database ", a)', trim(app%database)
            end if

            ! Start database transaction.
            call dm_timer_start(timer)
            rc = dm_db_begin(db)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'database transaction error')
                exit db_block
            end if

            nrecs = 0
            nrows = 0

            ! Read records from file and insert them into database.
            read_loop: do
                nrows = nrows + 1

                ! Read record from file.
                select case (app%type)
                    case (TYPE_NODE)
                        rc = dm_csv_read(node, fu, app%separator)
                    case (TYPE_SENSOR)
                        rc = dm_csv_read(sensor, fu, app%separator)
                    case (TYPE_TARGET)
                        rc = dm_csv_read(target, fu, app%separator)
                    case (TYPE_OBSERV)
                        rc = dm_csv_read(observ, fu, app%separator)
                    case (TYPE_LOG)
                        rc = dm_csv_read(log, fu, app%separator)
                end select

                ! Ignore comments and empty lines.
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

                ! Insert record into database.
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

                ! Handle return code.
                select case (rc)
                    case (E_NONE)
                        nrecs = nrecs + 1
                        if (app%verbose) then
                            if (modulo(nrecs, 100_i8) == 0) then
                                print '("Imported ", i0, " records")', nrecs
                            end if
                        end if
                    case (E_DB_CONSTRAINT)
                        call dm_error_out(rc, 'record in row ' // dm_itoa(nrows) // ' already exists in database')
                        exit read_loop
                    case default
                        call dm_error_out(rc, 'failed to insert record in line ' // dm_itoa(nrows))
                        exit read_loop
                end select
            end do read_loop

            ! Rollback transaction on error.
            if (dm_is_error(rc)) then
                er = dm_db_rollback(db)

                if (dm_is_error(er)) then
                    call dm_error_out(er, 'failed to roll back database transaction')
                    exit db_block
                end if

                if (app%verbose) print '("All database changes rolled back")'
                exit db_block
            end if

            ! Commit transaction.
            rc = dm_db_commit(db)
            dt = dm_timer_stop(timer)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to commit database transaction')
                er = dm_db_rollback(db)

                if (dm_is_error(er)) then
                    call dm_error_out(er, 'failed to roll back database transaction')
                    exit db_block
                end if

                if (app%verbose) print '("All database changes rolled back")'
                exit db_block
            end if

            if (app%verbose) then
                print '("Read ", i0, " rows from file ", a)', nrows, trim(app%input)
                print '("Imported ", i0, " records into database ", a, " in ", f0.1, " seconds")', &
                    nrecs, trim(app%database), dt
            end if
        end block db_block

        if (dm_is_error(dm_db_close(db))) rc = E_DB
        if (app%verbose) print '("Closed database ", a)', trim(app%database)

        close (fu)
        if (app%verbose) print '("Closed input file ", a)', trim(app%input)

        if (dm_is_error(rc)) return
        if (app%verbose) print '("Finished")'
    end function import

    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(inout) :: app
        type(arg_type)                :: args(5)

        character(len=6) :: type

        args = [ &
            arg_type('database',  short='d', type=ARG_TYPE_DB, required=.true.), & ! -d, --database <path>
            arg_type('input',     short='o', type=ARG_TYPE_CHAR), &                ! -i, --input <path>
            arg_type('type',      short='t', type=ARG_TYPE_CHAR, max_len=TYPE_NAME_LEN, required=.true.), & ! -t, --type <string>
            arg_type('separator', short='a', type=ARG_TYPE_CHAR, max_len=1), &     ! -a, --separator <char>
            arg_type('verbose',   short='V', type=ARG_TYPE_BOOL) &                 ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%database)
        rc = dm_arg_get(args(2), app%input)
        rc = dm_arg_get(args(3), type)
        rc = dm_arg_get(args(4), app%separator)
        rc = dm_arg_get(args(5), app%verbose)

        app%type = dm_type_from_name(type)

        rc = E_INVALID

        ! Data type.
        select case (app%type)
            case (TYPE_NODE)
            case (TYPE_SENSOR)
            case (TYPE_TARGET)
            case (TYPE_OBSERV)
            case (TYPE_LOG)
                continue
            case default
                call dm_error_out(rc, 'invalid type')
                return
        end select

        rc = E_NONE
    end function read_args
end program dmimport
