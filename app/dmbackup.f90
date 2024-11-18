! dmbackup.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmbackup
    !! Creates an online backup of a running database.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmbackup'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 2

    integer, parameter :: APP_NSTEPS     = 500 !! Step size for backup API.
    integer, parameter :: APP_SLEEP_TIME = 25  !! Sleep time between steps in msec.

    type :: app_type
        !! Command-line arguments.
        character(len=FILE_PATH_LEN) :: database = ' '     !! Path to database.
        character(len=FILE_PATH_LEN) :: backup   = ' '     !! Path to backup.
        logical                      :: vacuum   = .false. !! Vacuum flag.
        logical                      :: wal      = .false. !! WAL flag.
        logical                      :: verbose  = .false. !! Verbose flag.
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
                rc = dm_db_backup(db=db, path=app%backup, wal=app%wal, callback=backup_handler, &
                                  nsteps=APP_NSTEPS, sleep_time=APP_SLEEP_TIME)
                print *
            else
                ! No callback.
                rc = dm_db_backup(db=db, path=app%backup, wal=app%wal, nsteps=APP_NSTEPS, &
                                  sleep_time=APP_SLEEP_TIME)
            end if
        end block backup_block

        call dm_error_out(rc, 'backup failed')
        rc = dm_db_close(db)
    end function backup

    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app

        type(arg_type) :: args(5)

        args = [ &
            arg_type('database', short='d', type=ARG_TYPE_DATABASE, required=.true.), & ! -d, --database <path>
            arg_type('backup',   short='b', type=ARG_TYPE_STRING,   required=.true.), & ! -b, --backup <path>
            arg_type('vacuum',   short='U', type=ARG_TYPE_LOGICAL), & ! -U, --vacuum
            arg_type('wal',      short='W', type=ARG_TYPE_LOGICAL), & ! -W, --wal
            arg_type('verbose',  short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%database)
        call dm_arg_get(args(2), app%backup)
        call dm_arg_get(args(3), app%vacuum)
        call dm_arg_get(args(4), app%wal)
        call dm_arg_get(args(5), app%verbose)

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
    end function read_args

    subroutine backup_handler(remaining, page_count)
        !! Prints progess to standard output of SQLite backup API is selected.
        !! The cursor is reset to the first column of the line on each
        !! invokation.
        integer, intent(in) :: remaining  !! Pages remaining.
        integer, intent(in) :: page_count !! Total count of pages.

        write (*, '(a1, "[0GProgress: ", f5.1, " %")', advance='no') &
            ASCII_ESC, 100.0 * (page_count - remaining) / page_count
    end subroutine backup_handler

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_db_version(.true.)
    end subroutine version_callback
end program dmbackup
