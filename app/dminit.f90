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
    integer,          parameter :: APP_PATCH = 8

    type :: app_type
        !! Command-line arguments.
        character(len=FILE_PATH_LEN) :: database = ' '       !! Path to database.
        integer                      :: type     = TYPE_NONE !! Type of database.
        logical                      :: force    = .false.   !! Force creation.
        logical                      :: sync     = .false.   !! Sync flag.
        logical                      :: transfer = .false.   !! Transfer flag.
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
    rc = create_db(app%type, app%database, app%wal, app%sync, app%transfer)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function create_db(type, path, wal, sync, transfer) result(rc)
        !! Creates database schema.
        integer,          intent(in) :: type     !! Database type.
        character(len=*), intent(in) :: path     !! Path to database.
        logical,          intent(in) :: wal      !! WAL mode.
        logical,          intent(in) :: sync     !! Add sync tables.
        logical,          intent(in) :: transfer !! Add transfers table.

        type(db_type) :: db

        ! Open and create database.
        rc = dm_db_open(db, path, create=.true., wal=wal)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to create database')
            return
        end if

        ! Create tables.
        select case (type)
            case (TYPE_BEAT);   rc = dm_db_table_create_beats(db)
            case (TYPE_IMAGE);  rc = dm_db_table_create_images(db, transfer=transfer)
            case (TYPE_LOG);    rc = dm_db_table_create_logs(db, sync=sync)
            case (TYPE_OBSERV); rc = dm_db_table_create_observs(db, sync=sync)
        end select

        call dm_db_close(db)
        if (dm_is_error(rc)) call dm_error_out(rc, 'failed to create database')
    end function create_db

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app !! App type.

        character(len=TYPE_NAME_LEN) :: type_name
        type(arg_class)              :: arg

        call arg%create()
        call arg%add('type',     short='t', type=ARG_TYPE_STRING, required=.true.) ! -t, --type [beat|log|observ]
        call arg%add('database', short='d', type=ARG_TYPE_STRING, required=.true.) ! -d, --database <path>
        call arg%add('force',    short='F', type=ARG_TYPE_LOGICAL)                 ! -F, --force
        call arg%add('sync',     short='s', type=ARG_TYPE_LOGICAL)                 ! -s, --sync
        call arg%add('transfer', short='x', type=ARG_TYPE_LOGICAL)                 ! -x, --transfer
        call arg%add('wal',      short='W', type=ARG_TYPE_LOGICAL)                 ! -W, --wal

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        ! Database type (observ, log, beat).
        call arg%get('type',     type_name)
        call arg%get('database', app%database)
        call arg%get('force',    app%force)
        call arg%get('sync',     app%sync)
        call arg%get('transfer', app%transfer)
        call arg%get('wal',      app%wal)

        app%type = dm_type_from_name(type_name)

        ! Validate options.
        rc = validate(app)
    end function read_args

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        select case (app%type)
            case (TYPE_OBSERV, TYPE_LOG, TYPE_BEAT)
                if (app%transfer) then
                    call dm_error_out(rc, 'argument --transfer is not allowed')
                    return
                end if

            case (TYPE_IMAGE)
                continue

            case default
                call dm_error_out(rc, 'argument --type is invalid (must be observ, log, beat, or image)')
                return
        end select

        rc = E_EXIST

        if (.not. app%force .and. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database ' // trim(app%database) // ' exists')
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
end program dminit
