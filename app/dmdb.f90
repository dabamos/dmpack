! dmdb.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmdb
    !! The database program collects observations from a POSIX message queue and
    !! stores them in a SQLite database.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmdb'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 2

    ! Program parameters.
    integer, parameter :: APP_DB_NSTEPS   = 500                !! Number of steps before database is optimised.
    integer, parameter :: APP_DB_TIMEOUT  = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.
    logical, parameter :: APP_MQ_BLOCKING = .true.             !! Observation forwarding is blocking.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)          :: name     = APP_NAME !! Name of database instance and POSIX semaphore.
        character(len=FILE_PATH_LEN)   :: config   = ' '      !! Path to configuration file.
        character(len=LOGGER_NAME_LEN) :: logger   = ' '      !! Name of logger (name implies IPC).
        character(len=FILE_PATH_LEN)   :: database = ' '      !! Path to SQLite database file.
        character(len=NODE_ID_LEN)     :: node     = ' '      !! Node id.
        logical                        :: debug    = .false.  !! Forward debug messages via IPC.
        logical                        :: ipc      = .false.  !! Use POSIX semaphore for process synchronisation.
        logical                        :: verbose  = .false.  !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer           :: rc     ! Return code.
    type(app_type)    :: app    ! App settings.
    type(db_type)     :: db     ! Database handle.
    type(mqueue_type) :: mqueue ! POSIX message queue handle.
    type(sem_type)    :: sem    ! POSIX semaphore handle.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Initialise logger.
    logger => dm_logger_get()
    call logger%configure(name    = app%logger, &
                          node_id = app%node, &
                          source  = app%name, &
                          debug   = app%debug, &
                          ipc     = (len_trim(app%logger) > 0), &
                          verbose = app%verbose)

    init_block: block
        ! Open SQLite database.
        rc = dm_db_open(db, path=app%database, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call logger%error('failed to open database ' // app%database, error=rc)
            exit init_block
        end if

        ! Open observation message queue for reading.
        rc = dm_mqueue_open(mqueue = mqueue, &
                            type   = TYPE_OBSERV, &
                            name   = app%name, &
                            access = MQUEUE_RDONLY)

        if (dm_is_error(rc)) then
            call logger%error('failed to open mqueue /' // trim(app%name) // ': ' // &
                              dm_system_error_message(), error=rc)
            exit init_block
        end if

        ! Create semaphore for IPC.
        if (app%ipc) then
            rc = dm_sem_open(sem, name=app%name, value=0, create=.true.)

            if (dm_is_error(rc)) then
                call logger%error('failed to open semaphore /' // app%name, error=rc)
                exit init_block
            end if
        end if

        ! Run the IPC loop.
        call dm_signal_register(signal_handler)
        call run(app, db, mqueue, sem)
    end block init_block

    call halt(rc)
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(inout) :: app
        type(arg_type)                :: args(8)

        rc = E_NONE

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),   & ! -n, --name <string>
            arg_type('config',   short='c', type=ARG_TYPE_FILE), & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),   & ! -l, --logger <string>
            arg_type('database', short='d', type=ARG_TYPE_DB),   & ! -d, --database <path>
            arg_type('node',     short='N', type=ARG_TYPE_ID),   & ! -N, --node <string>
            arg_type('debug',    short='D', type=ARG_TYPE_BOOL), & ! -D, --debug
            arg_type('ipc',      short='Q', type=ARG_TYPE_BOOL), & ! -Q, --ipc
            arg_type('verbose',  short='V', type=ARG_TYPE_BOOL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite configuration.
        rc = dm_arg_get(args(3), app%logger)
        rc = dm_arg_get(args(4), app%database)
        rc = dm_arg_get(args(5), app%node)
        rc = dm_arg_get(args(6), app%debug)
        rc = dm_arg_get(args(7), app%ipc)
        rc = dm_arg_get(args(8), app%verbose)

        rc = E_INVALID

        if (.not. dm_id_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
        end if

        if (len_trim(app%logger) > 0 .and. .not. dm_id_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger name')
            return
        end if

        if (len_trim(app%database) == 0) then
            call dm_error_out(rc, 'missing database ' // app%database)
            return
        end if

        if (.not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database ' // trim(app%database) // ' does not exist')
            return
        end if

        if (.not. dm_id_valid(app%node)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file if path is not emty.
        type(app_type), intent(inout) :: app !! App type.
        type(config_type)             :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            rc = dm_config_get(config, 'logger',   app%logger)
            rc = dm_config_get(config, 'database', app%database)
            rc = dm_config_get(config, 'node',     app%node)
            rc = dm_config_get(config, 'debug',    app%debug)
            rc = dm_config_get(config, 'ipc',      app%ipc)
            rc = dm_config_get(config, 'verbose',  app%verbose)
            rc = E_NONE
        end if

        call dm_config_close(config)
    end function read_config

    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in), optional :: error !! DMPACK error code.

        integer :: rc, stat

        stat = 0
        if (present(error)) then
            if (dm_is_error(error)) stat = 1
        end if

        rc = dm_db_close(db)
        rc = dm_mqueue_close(mqueue)
        rc = dm_mqueue_unlink(mqueue)

        if (app%ipc) then
            rc = dm_sem_close(sem)
            rc = dm_sem_unlink(sem)
        end if

        call dm_stop(stat)
    end subroutine halt

    subroutine run(app, db, mqueue, sem)
        !! Opens observation message queue for reading, and stores received
        !! derived types in database.
        type(app_type),    intent(inout) :: app     !! App settings.
        type(db_type),     intent(inout) :: db      !! Database type.
        type(mqueue_type), intent(inout) :: mqueue  !! Message queue type.
        type(sem_type),    intent(inout) :: sem     !! Semaphore type.

        integer           :: rc, steps, value
        type(observ_type) :: observ

        steps = 0
        call logger%info('started ' // app%name)

        ipc_loop: do
            ! Blocking read from POSIX message queue.
            rc = dm_mqueue_read(mqueue, observ)

            if (dm_is_error(rc)) then
                call logger%error('failed to read from mqueue /' // app%name, error=rc)
                call dm_sleep(1)
                cycle ipc_loop
            end if

            if (.not. dm_observ_valid(observ)) then
                call logger%error('invalid observ ' // trim(observ%name), error=E_INVALID)
                cycle ipc_loop
            end if

            if (dm_db_observ_exists(db, observ%id)) then
                call logger%warning('observ ' // trim(observ%id) // ' exists', error=E_EXIST)
                cycle ipc_loop
            end if

            db_loop: do
                ! Add observation to database.
                rc = dm_db_insert(db, observ)

                ! Retry if database is busy.
                if (rc == E_DB_BUSY) then
                    call logger%debug('database busy', error=rc)
                    call dm_db_sleep(APP_DB_TIMEOUT)
                    cycle db_loop
                end if

                ! Get more precise database error.
                if (dm_is_error(rc)) then
                    rc = dm_db_error(db)
                    call logger%error('failed to insert observ ' // observ%name, error=rc)
                    exit db_loop
                end if

                call logger%debug('inserted observ ' // observ%name)

                ! Post semaphore.
                if (app%ipc) then
                    rc = dm_sem_value(sem, value)
                    if (value == 0) rc = dm_sem_post(sem)
                end if

                exit db_loop
            end do db_loop

            ! Forward observation.
            rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)

            if (steps == 0) then
                ! Optimise database.
                call logger%debug('optimizing database')
                rc = dm_db_optimize(db)

                if (dm_is_error(rc)) then
                    call logger%error('failed to optimize database', error=rc)
                end if
            end if

            ! Increase optimise step counter.
            steps = modulo(steps + 1, APP_DB_NSTEPS)
        end do ipc_loop
    end subroutine run

    subroutine signal_handler(signum) bind(c)
        !! C-interoperable signal handler that closes database, removes message
        !! queue, and stops program.
        use, intrinsic :: iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: signum

        select case (signum)
            case default
                call logger%info('exit on signal ' // dm_itoa(signum))
                call halt(E_NONE)
        end select
    end subroutine signal_handler
end program dmdb
