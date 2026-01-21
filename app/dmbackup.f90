! dmbackup.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmbackup
    !! Creates an online backup of an SQLite database.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmbackup'
    integer,      parameter :: APP_MAJOR = 0
    integer,      parameter :: APP_MINOR = 9
    integer,      parameter :: APP_PATCH = 9

    integer, parameter :: APP_NSTEPS     = 500 !! Step size for backup API.
    integer, parameter :: APP_SLEEP_TIME = 25  !! Sleep time between steps in msec.

    type :: app_type
        !! Command-line arguments.
        character(FILE_PATH_LEN) :: database = ' '     !! Path to database.
        character(FILE_PATH_LEN) :: backup   = ' '     !! Path to backup.
        logical                  :: vacuum   = .false. !! Vacuum flag.
        logical                  :: wal      = .false. !! WAL flag.
        logical                  :: verbose  = .false. !! Verbose flag.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Create selected database type.
    rc = backup(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function backup(app) result(rc)
        !! Creates database backup.
        type(app_type), intent(inout) :: app

        type(db_type) :: db

        ! Open database.
        rc = dm_db_open(db, app%database)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to open database ' // app%database)
            return
        end if

        backup_block: block
            ! Use VACUUM INTO.
            if (app%vacuum) then
                rc = dm_db_vacuum(db, into=app%backup)
                exit backup_block
            end if

            ! Use SQLite backup API.
            if (app%verbose) then
                ! Using callback.
                rc = dm_db_backup(db=db, path=app%backup, wal=app%wal, callback=backup_callback, &
                                  nsteps=APP_NSTEPS, sleep_time=APP_SLEEP_TIME)
                print *
            else
                ! No callback.
                rc = dm_db_backup(db=db, path=app%backup, wal=app%wal, nsteps=APP_NSTEPS, &
                                  sleep_time=APP_SLEEP_TIME)
            end if
        end block backup_block

        call dm_error_out(rc, 'backup failed')
        call dm_db_close(db)
    end function backup

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app

        type(arg_class) :: arg

        call arg%add('database', short='d', type=ARG_TYPE_DATABASE, required=.true., exist=.true.) ! -d, --database <path>
        call arg%add('backup',   short='b', type=ARG_TYPE_DATABASE, required=.true.)               ! -b, --backup <path>
        call arg%add('vacuum',   short='U', type=ARG_TYPE_LOGICAL) ! -U, --vacuum
        call arg%add('wal',      short='W', type=ARG_TYPE_LOGICAL) ! -W, --wal
        call arg%add('verbose',  short='V', type=ARG_TYPE_LOGICAL) ! -V, --verbose

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('database', app%database)
        call arg%get('backup',   app%backup)
        call arg%get('vaccum',   app%vacuum)
        call arg%get('wal',      app%wal)
        call arg%get('verbose',  app%verbose)

        rc = validate(app)
    end function read_args

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (dm_file_exists(app%backup)) then
            call dm_error_out(rc, 'backup database exists')
            return
        end if

        if (app%vacuum .and. app%wal) then
            call dm_error_out(rc, 'argument --wal conflicts with --vacuum')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine backup_callback(remaining, page_count)
        !! Prints progess to standard output of SQLite backup API is selected.
        !! The cursor is reset to the first column of the line on each
        !! invokation.
        integer, intent(in) :: remaining  !! Pages remaining.
        integer, intent(in) :: page_count !! Total count of pages.

        write (*, '(a1, "[0GProgress: ", f5.1, " %")', advance='no') &
            ASCII_ESC, 100.0 * (page_count - remaining) / page_count
    end subroutine backup_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_db_version(.true.)
    end subroutine version_callback
end program dmbackup
