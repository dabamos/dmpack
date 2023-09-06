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
    call dm_error_out(rc)
    if (dm_is_error(rc)) call dm_stop(1)

    rc = output_info(app)
    if (dm_is_error(rc)) call dm_stop(1)
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(inout) :: app
        type(arg_type)                :: args(1)

        rc = E_NONE

        args = [ arg_type(name='database', short='d', type=ARG_TYPE_DB) ] ! --database <path>

        ! Read arguments and get database path.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%database)
        if (dm_is_error(rc)) return
    end function read_args

    integer function output_info(app) result(rc)
        !! Reads system and database information and prints it as key-value
        !! pairs to standard output.
        use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
        type(app_type), intent(inout) :: app

        character(len=:), allocatable :: mode_name
        integer                       :: app_id, mode
        integer(kind=i8)              :: n, sz
        logical                       :: has, has_db
        type(db_type)                 :: db
        type(uname_type)              :: uname

        has_db = (len_trim(app%database) > 0)

        if (has_db) then
            sz = dm_file_size(app%database)
            rc = dm_db_open(db, app%database, read_only=.true.)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to open database ' // app%database)
                return
            end if
        end if

        print '("build.compiler = ", a)', compiler_version()
        print '("build.args = ", a)', compiler_options()

        if (has_db) then
            rc = dm_db_get_application_id(db, app_id)
            print '("db.application_id = ", z0)', app_id

            rc = dm_db_get_foreign_keys(db, has)
            print '("db.foreign_keys = ", l)', has

            rc = dm_db_get_journal_mode(db, mode, mode_name)
            print '("db.journal_mode = ", a)', mode_name

            print '("db.path = ", a)', trim(app%database)
            print '("db.size = ", i0)', sz

            rc = dm_db_has_table(db, SQL_TABLE_BEATS, has)
            rc = dm_db_count_beats(db, n)
            print '("db.table.beats = ", l)', has
            print '("db.table.beats.rows = ", i0)', n

            rc = dm_db_has_table(db, SQL_TABLE_LOGS, has)
            rc = dm_db_count_logs(db, n)
            print '("db.table.logs = ", l)', has
            print '("db.table.logs.rows = ", i0)', n

            rc = dm_db_has_table(db, SQL_TABLE_NODES, has)
            rc = dm_db_count_nodes(db, n)
            print '("db.table.nodes = ", l)', has
            print '("db.table.nodes.rows = ", i0)', n

            rc = dm_db_has_table(db, SQL_TABLE_OBSERVS, has)
            rc = dm_db_count_observs(db, n)
            print '("db.table.observs = ", l)', has
            print '("db.table.observs.rows = ", i0)', n

            rc = dm_db_has_table(db, SQL_TABLE_RECEIVERS, has)
            rc = dm_db_count_receivers(db, n)
            print '("db.table.receivers = ", l)', has
            print '("db.table.receivers.rows = ", i0)', n

            rc = dm_db_has_table(db, SQL_TABLE_REQUESTS, has)
            rc = dm_db_count_requests(db, n)
            print '("db.table.requests = ", l)', has
            print '("db.table.requests.rows = ", i0)', n

            rc = dm_db_has_table(db, SQL_TABLE_RESPONSES, has)
            rc = dm_db_count_responses(db, n)
            print '("db.table.responses = ", l)', has
            print '("db.table.responses.rows = ", i0)', n

            rc = dm_db_has_table(db, SQL_TABLE_SENSORS, has)
            rc = dm_db_count_sensors(db, n)
            print '("db.table.sensors = ", l)', has
            print '("db.table.sensors.rows = ", i0)', n

            rc = dm_db_has_table(db, SQL_TABLE_TARGETS, has)
            rc = dm_db_count_targets(db, n)
            print '("db.table.targets = ", l)', has
            print '("db.table.targets.rows = ", i0)', n

            rc = dm_db_close(db)
        end if

        call dm_system_uname(uname)

        print '("dmpack.version = ", a)', DM_VERSION_STRING
        print '("system.host = ", a)', trim(uname%node_name)
        print '("system.machine = ", a)', trim(uname%machine)
        print '("system.name = ", a)', trim(uname%system_name)
        print '("system.release = ", a)', trim(uname%release)
        print '("system.time.now = ", a)', dm_time_now()
        print '("system.time.zone = ", a)', dm_time_zone()
        print '("system.version = ", a)', trim(uname%version)
    end function output_info
end program dminfo
