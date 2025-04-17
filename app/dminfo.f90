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
    integer,          parameter :: APP_PATCH = 8

    type :: app_type
        !! Command-line arguments.
        character(len=FILE_PATH_LEN) :: database = ' ' !! Path to database (optional).
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    call dm_init()

    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    call output_info(app)
contains
    subroutine output_info(app)
        !! Reads system and database information and prints it as key-value
        !! pairs to standard output.
        use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version

        type(app_type), intent(inout) :: app

        character(len=64)             :: file_system, model, mounted_on
        character(len=:), allocatable :: mode_name

        integer          :: app_id, capacity, mode, ncore, rc, schema_version
        integer(kind=i8) :: available, n, nbyte
        logical          :: foreign_keys
        type(db_type)    :: db
        type(uname_type) :: uname

        ! Database information.
        if (dm_string_has(app%database)) then
            rc = dm_db_open(db, app%database, read_only=.true.)
            if (dm_is_error(rc)) call dm_error_out(rc, 'failed to open database ' // app%database, fatal=.true.)

            rc = dm_db_get_application_id(db, app_id)
            rc = dm_db_get_foreign_keys(db, foreign_keys)
            rc = dm_db_get_journal_mode(db, mode, mode_name)
            rc = dm_db_get_schema_version(db, schema_version)
            rc = dm_db_size(db, nbyte)

            ! Available disk space and disk capacity.
            rc = dm_system_disk_free(path        = app%database, &
                                     file_system = file_system,  &
                                     available   = available,    &
                                     capacity    = capacity,     &
                                     mounted_on  = mounted_on)

            print '("db.application_id: ", z0)', app_id
            print '("db.foreign_keys: ", l1)',   dm_btoa(foreign_keys, 'true', 'false')
            print '("db.fs.available: ", i0)',   available
            print '("db.fs.capacity: ", i0)',    capacity
            print '("db.fs.mount_point: ", a)',  trim(mounted_on)
            print '("db.fs.path: ", a)',         trim(file_system)
            print '("db.journal_mode: ", a)',    mode_name
            print '("db.library: ", a)',         dm_db_version(.true.)
            print '("db.path: ", a)',            trim(app%database)
            print '("db.schema_version: ", i0)', schema_version
            print '("db.size: ", i0)',           nbyte

            if (dm_db_table_has(db, SQL_TABLE_BEATS)) then
                rc = dm_db_count_beats(db, n)
                print '("db.table.beats.rows: ", i0)', n
            end if

            if (dm_db_table_has(db, SQL_TABLE_LOGS)) then
                rc = dm_db_count_logs(db, n)
                print '("db.table.logs.rows: ", i0)', n
            end if

            if (dm_db_table_has(db, SQL_TABLE_NODES)) then
                rc = dm_db_count_nodes(db, n)
                print '("db.table.nodes.rows: ", i0)', n
            end if

            if (dm_db_table_has(db, SQL_TABLE_OBSERVS)) then
                rc = dm_db_count_observs(db, n)
                print '("db.table.observs.rows: ", i0)', n
            end if

            if (dm_db_table_has(db, SQL_TABLE_RECEIVERS)) then
                rc = dm_db_count_receivers(db, n)
                print '("db.table.receivers.rows: ", i0)', n
            end if

            if (dm_db_table_has(db, SQL_TABLE_REQUESTS)) then
                rc = dm_db_count_requests(db, n)
                print '("db.table.requests.rows: ", i0)', n
            end if

            if (dm_db_table_has(db, SQL_TABLE_RESPONSES)) then
                rc = dm_db_count_responses(db, n)
                print '("db.table.responses.rows: ", i0)', n
            end if

            if (dm_db_table_has(db, SQL_TABLE_SENSORS)) then
                rc = dm_db_count_sensors(db, n)
                print '("db.table.sensors.rows: ", i0)', n
            end if

            if (dm_db_table_has(db, SQL_TABLE_TARGETS)) then
                rc = dm_db_count_targets(db, n)
                print '("db.table.targets.rows: ", i0)', n
            end if

            call dm_db_close(db)
        end if

        call dm_system_uname(uname)
        rc = dm_system_cpu_cores(ncore)
        rc = dm_system_cpu_model(model)

        print '("dmpack.compiler: ", a)',   compiler_version()
        print '("dmpack.date: ", a)',       DM_BUILD_DATE
        print '("dmpack.options: ", a)',    compiler_options()
        print '("dmpack.version: ", a)',    DM_VERSION_STRING
        print '("system.byte_order: ", a)', dm_btoa(LITTLE_ENDIAN, 'little-endian', 'big-endian')
        print '("system.cpu.cores: ", i0)', ncore
        print '("system.cpu.model: ", a)',  trim(model)
        print '("system.hostname: ", a)',   trim(uname%node_name)
        print '("system.name: ", a)',       trim(uname%system_name)
        print '("system.platform: ", a)',   trim(uname%machine)
        print '("system.release: ", a)',    trim(uname%release)
        print '("system.time.now: ", a)',   dm_time_now()
        print '("system.time.zone: ", a)',  dm_time_zone()
        print '("system.version: ", a)',    trim(uname%version)
    end subroutine output_info

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app
        type(arg_type)              :: args(1)

        args = [ &
            arg_type(name='database', short='d', type=ARG_TYPE_DATABASE) & ! -d, --database <path>
        ]

        rc = dm_arg_read(args, version_callback)
        call dm_arg_get(args(1), app%database)
        rc = E_NONE
    end function read_args

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_db_version(.true.)
    end subroutine version_callback
end program dminfo
