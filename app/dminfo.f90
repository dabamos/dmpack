! dminfo.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dminfo
    !! Utility program that displays system and database information.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dminfo'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    type :: app_type
        !! Command-line arguments.
        character(FILE_PATH_LEN) :: database = ' ' !! Path to database (optional).
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

        character(*), parameter :: FALSE = 'false'
        character(*), parameter :: TRUE  = 'true'

        type(app_type), intent(inout) :: app

        integer     :: app_id, capacity, mode, ncore, rc, schema_version
        integer(i8) :: available, n, nbyte
        logical     :: foreign_keys, has

        character(64)             :: file_system, model, mounted_on
        character(:), allocatable :: mode_name
        type(db_type)             :: db
        type(posix_uname_type)    :: uname

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
            rc = dm_posix_disk_free(path        = app%database, &
                                    file_system = file_system,  &
                                    available   = available,    &
                                    capacity    = capacity,     &
                                    mounted_on  = mounted_on)

            print '("db.application_id: ", z0)', app_id
            print '("db.foreign_keys: ", a)',    dm_btoa(foreign_keys, TRUE, FALSE)
            print '("db.fs.available: ", i0)',   available
            print '("db.fs.capacity: ", i0)',    capacity
            print '("db.fs.mount_point: ", a)',  trim(mounted_on)
            print '("db.fs.path: ", a)',         trim(file_system)
            print '("db.journal_mode: ", a)',    mode_name
            print '("db.library: ", a)',         dm_db_version(.true.)
            print '("db.path: ", a)',            trim(app%database)
            print '("db.schema_version: ", i0)', schema_version
            print '("db.size: ", i0)',           nbyte

            has = dm_db_table_has(db, SQL_TABLE_BEATS)
            rc  = dm_db_count_beats(db, n)

            print '("db.table.beats: ", a)', dm_btoa(has, TRUE, FALSE)
            if (has) print '("db.table.beats.rows: ", i0)', n

            has = dm_db_table_has(db, SQL_TABLE_IMAGES)
            rc  = dm_db_count_images(db, n)

            print '("db.table.images: ", a)', dm_btoa(has, TRUE, FALSE)
            if (has) print '("db.table.images.rows: ", i0)', n

            has = dm_db_table_has(db, SQL_TABLE_LOGS)
            rc  = dm_db_count_logs(db, n)

            print '("db.table.logs: ", a)', dm_btoa(has, TRUE, FALSE)
            if (has) print '("db.table.logs.rows: ", i0)', n

            has = dm_db_table_has(db, SQL_TABLE_NODES)
            rc  = dm_db_count_nodes(db, n)

            print '("db.table.nodes: ", a)', dm_btoa(has, TRUE, FALSE)
            if (has) print '("db.table.nodes.rows: ", i0)', n

            has = dm_db_table_has(db, SQL_TABLE_OBSERVS)
            rc  = dm_db_count_observs(db, n)

            print '("db.table.observs: ", a)', dm_btoa(has, TRUE, FALSE)
            if (has) print '("db.table.observs.rows: ", i0)', n

            has = dm_db_table_has(db, SQL_TABLE_RECEIVERS)
            rc  = dm_db_count_receivers(db, n)

            print '("db.table.receivers: ", a)', dm_btoa(has, TRUE, FALSE)
            if (has) print '("db.table.receivers.rows: ", i0)', n

            has = dm_db_table_has(db, SQL_TABLE_RESPONSES)
            rc  = dm_db_count_responses(db, n)

            print '("db.table.responses: ", a)', dm_btoa(has, TRUE, FALSE)
            if (has) print '("db.table.responses.rows: ", i0)', n

            has = dm_db_table_has(db, SQL_TABLE_SENSORS)
            rc  = dm_db_count_sensors(db, n)

            print '("db.table.sensors: ", a)', dm_btoa(has, TRUE, FALSE)
            if (has) print '("db.table.sensors.rows: ", i0)', n

            has = dm_db_table_has(db, SQL_TABLE_TARGETS)
            rc  = dm_db_count_targets(db, n)

            print '("db.table.targets: ", a)', dm_btoa(has, TRUE, FALSE)
            if (has) print '("db.table.targets.rows: ", i0)', n

            has = dm_db_table_has(db, SQL_TABLE_TRANSFERS)
            rc  = dm_db_count_transfers(db, n)

            print '("db.table.transfers: ", a)', dm_btoa(has, TRUE, FALSE)
            if (has) print '("db.table.transfers.rows: ", i0)', n

            call dm_db_close(db)
        end if

        call dm_posix_uname(uname)
        rc = dm_posix_cpu_cores(ncore)
        rc = dm_posix_cpu_model(model)

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

        type(arg_parser_class) :: parser

        call parser%add(name='database', short='d', type=ARG_TYPE_DATABASE, exist=.true.) ! -d, --database <path>
        rc = parser%read(version_callback)
        if (dm_is_error(rc)) return
        call parser%get('database', app%database)
    end function read_args

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_db_version(.true.)
    end subroutine version_callback
end program dminfo
