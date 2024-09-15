! dminit.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dminit
    !! Utility program that creates a beat, log, or observation database. No
    !! action is performed if the specified database already exists, unless
    !! command-line argument `--force` is passed. WAL mode should be enabled
    !! for databases with multiple readers. A synchronisation table is required
    !! for observation and log synchronisation with an DMPACK server, and can
    !! be omitted otherwise. The program also sets the database application id
    !! and user version.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dminit'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 2

    type :: app_type
        !! Command-line arguments.
        character(len=FILE_PATH_LEN) :: database = ' '       !! Path to database.
        integer                      :: type     = TYPE_NONE !! Type of database.
        logical                      :: force    = .false.   !! Force creation.
        logical                      :: sync     = .false.   !! Sync flag.
        logical                      :: wal      = .false.   !! WAL flag.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Create selected database type.
    rc = create_db(app%type, app%database, app%wal, app%sync)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function create_db(type, path, wal, sync) result(rc)
        !! Creates database schema.
        integer,          intent(in) :: type !! Database type.
        character(len=*), intent(in) :: path !! Path to database.
        logical,          intent(in) :: wal  !! WAL mode.
        logical,          intent(in) :: sync !! Add sync tables.

        type(db_type) :: db

        ! Open and create database.
        rc = dm_db_open(db, path, create=.true., wal=wal)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to create database')
            return
        end if

        ! Create tables.
        db_block: block
            rc = E_INVALID

            select case (type)
                case (TYPE_BEAT)
                    ! Create beat table.
                    rc = dm_db_create_beats(db)
                case (TYPE_LOG)
                    ! Create log table.
                    rc = dm_db_create_logs(db, sync=sync)
                case (TYPE_OBSERV)
                    ! Create observation tables.
                    rc = dm_db_create_observs(db, sync=sync)
                case default
                    ! Should never occur.
                    call dm_error_out(rc, 'invalid database type')
                    exit db_block
            end select

            if (dm_is_error(rc)) call dm_error_out(rc, 'failed to create database')
        end block db_block

        if (dm_is_error(dm_db_close(db))) rc = E_DB
    end function create_db

    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app !! App type.

        character(len=TYPE_NAME_LEN) :: type
        type(arg_type)               :: args(5)

        rc = E_NONE

        args = [ &
            arg_type('type',     short='t', type=ARG_TYPE_STRING, required=.true.), & ! -t, --type [beat|log|observ]
            arg_type('database', short='d', type=ARG_TYPE_STRING, required=.true.), & ! -d, --database <path>
            arg_type('force',    short='F', type=ARG_TYPE_LOGICAL),                 & ! -F, --force
            arg_type('sync',     short='Y', type=ARG_TYPE_LOGICAL),                 & ! -Y, --sync
            arg_type('wal',      short='W', type=ARG_TYPE_LOGICAL)                  & ! -W, --wal
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, dm_db_version(.true.))
        if (dm_is_error(rc)) return

        ! Database type (observ, log, beat).
        call dm_arg_get(args(1), type)
        call dm_arg_get(args(2), app%database)
        call dm_arg_get(args(3), app%force)
        call dm_arg_get(args(4), app%sync)
        call dm_arg_get(args(5), app%wal)

        ! Validate options.
        app%type = dm_type_from_name(type)

        rc = E_INVALID
        select case (app%type)
            case (TYPE_OBSERV, TYPE_LOG, TYPE_BEAT)
                continue
            case default
                call dm_error_out(rc, 'invalid database type ' // trim(type) // ' (must be observ, log, or beat)')
                return
        end select

        rc = E_EXIST
        if (.not. app%force .and. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database ' // trim(app%database) // ' exists')
            return
        end if

        rc = E_NONE
    end function read_args
end program dminit
