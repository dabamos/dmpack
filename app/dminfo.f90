! dminfo.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dminfo
    !! Utility program that displays system and database information.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dminfo'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 0

    type :: app_type
        !! Command-line arguments.
        character(len=FILE_PATH_LEN) :: database = ' ' !! Path to database (optional).
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    rc = output_info(app)
    if (dm_is_error(rc)) call dm_stop(1)
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(inout) :: app
        type(arg_type)                :: args(1)

        args = [ &
            arg_type(name='database', short='d', type=ARG_TYPE_DB) & ! -d, --database <path>
        ]

        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        rc = dm_arg_get(args(1), app%database)
        rc = E_NONE
    end function read_args

    integer function output_info(app) result(rc)
        !! Reads system and database information and prints it as key-value
        !! pairs to standard output.
        use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version

        type(app_type), intent(inout) :: app

        character(len=:), allocatable :: mode_name
        integer                       :: app_id, mode
        integer(kind=i8)              :: n, sz
        logical                       :: exists, has_db
        type(db_type)                 :: db
        type(uname_type)              :: uname

        ! Try to open database.
        has_db = (len_trim(app%database) > 0)

        if (has_db) then
            sz = dm_file_size(app%database)
            rc = dm_db_open(db, app%database, read_only=.true.)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to open database ' // app%database)
                return
            end if
        end if

        ! Compiler and build options.
        print '("build.compiler: ", a)', compiler_version()
        print '("build.options: ", a)',  compiler_options()

        ! Database information.
        if (has_db) then
            rc = dm_db_get_application_id(db, app_id)
            print '("db.application_id: ", z0)', app_id

            rc = dm_db_get_foreign_keys(db, exists)
            print '("db.foreign_keys: ", l)', exists

            rc = dm_db_get_journal_mode(db, mode, mode_name)
            print '("db.journal_mode: ", a)', mode_name

            print '("db.path: ", a)', trim(app%database)
            print '("db.size: ", i0)', sz

            rc = dm_db_table_exists(db, SQL_TABLE_BEATS, exists)
            rc = dm_db_count_beats(db, n)
            print '("db.table.beats: ", l)', exists
            print '("db.table.beats.rows: ", i0)', n

            rc = dm_db_table_exists(db, SQL_TABLE_LOGS, exists)
            rc = dm_db_count_logs(db, n)
            print '("db.table.logs: ", l)', exists
            print '("db.table.logs.rows: ", i0)', n

            rc = dm_db_table_exists(db, SQL_TABLE_NODES, exists)
            rc = dm_db_count_nodes(db, n)
            print '("db.table.nodes: ", l)', exists
            print '("db.table.nodes.rows: ", i0)', n

            rc = dm_db_table_exists(db, SQL_TABLE_OBSERVS, exists)
            rc = dm_db_count_observs(db, n)
            print '("db.table.observs: ", l)', exists
            print '("db.table.observs.rows: ", i0)', n

            rc = dm_db_table_exists(db, SQL_TABLE_RECEIVERS, exists)
            rc = dm_db_count_receivers(db, n)
            print '("db.table.receivers: ", l)', exists
            print '("db.table.receivers.rows: ", i0)', n

            rc = dm_db_table_exists(db, SQL_TABLE_REQUESTS, exists)
            rc = dm_db_count_requests(db, n)
            print '("db.table.requests: ", l)', exists
            print '("db.table.requests.rows: ", i0)', n

            rc = dm_db_table_exists(db, SQL_TABLE_RESPONSES, exists)
            rc = dm_db_count_responses(db, n)
            print '("db.table.responses: ", l)', exists
            print '("db.table.responses.rows: ", i0)', n

            rc = dm_db_table_exists(db, SQL_TABLE_SENSORS, exists)
            rc = dm_db_count_sensors(db, n)
            print '("db.table.sensors: ", l)', exists
            print '("db.table.sensors.rows: ", i0)', n

            rc = dm_db_table_exists(db, SQL_TABLE_TARGETS, exists)
            rc = dm_db_count_targets(db, n)
            print '("db.table.targets: ", l)', exists
            print '("db.table.targets.rows: ", i0)', n

            rc = dm_db_close(db)
        end if

        ! DMPACK information.
        print '("dmpack.version: ", a)', DM_VERSION_STRING

        ! System information.
        write (*, '("system.byte_order: ")', advance='no')

        if (LITTLE_ENDIAN) then
            print '("little-endian")'
        else
            print '("big-endian")'
        end if

        call dm_system_uname(uname)

        print '("system.host: ", a)',      trim(uname%node_name)
        print '("system.machine: ", a)',   trim(uname%machine)
        print '("system.name: ", a)',      trim(uname%system_name)
        print '("system.release: ", a)',   trim(uname%release)
        print '("system.time.now: ", a)',  dm_time_now()
        print '("system.time.zone: ", a)', dm_time_zone()
        print '("system.version: ", a)',   trim(uname%version)
    end function output_info
end program dminfo
