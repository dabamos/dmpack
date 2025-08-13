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
    integer,          parameter :: APP_PATCH = 8

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
        character(len=NODE_ID_LEN)     :: node_id  = ' '      !! Node id.
        logical                        :: debug    = .false.  !! Forward debug messages via IPC.
        logical                        :: ipc      = .false.  !! Use POSIX semaphore for process synchronisation.
        logical                        :: verbose  = .false.  !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer              :: rc     ! Return code.
    type(app_type)       :: app    ! App settings.
    type(db_type)        :: db     ! Database type.
    type(mqueue_type)    :: mqueue ! POSIX message queue type.
    type(sem_named_type) :: sem    ! POSIX semaphore type.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,  & ! Name of logger process.
                          node_id = app%node_id, & ! Node id.
                          source  = app%name,    & ! Log source.
                          debug   = app%debug,   & ! Forward debug messages via IPC.
                          ipc     = .true.,      & ! Enable IPC (if logger is set).
                          verbose = app%verbose)   ! Print logs to standard error.

    call init(app, db, mqueue, sem, error=rc)
    if (dm_is_error(rc)) call halt(rc)

    call run(app, db, mqueue, sem)
    call halt(E_NONE)
contains
    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: rc, stat

        stat = merge(STOP_FAILURE, STOP_SUCCESS, dm_is_error(error))

        call dm_db_close(db, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to close database', error=rc)

        call dm_mqueue_close(mqueue, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to close mqueue /' // app%name, error=rc)

        call dm_mqueue_unlink(mqueue, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to unlink mqueue /' // app%name, error=rc)

        if (app%ipc) then
            call dm_sem_close(sem, error=rc)
            if (dm_is_error(rc)) call logger%error('failed to close semaphore /' // app%name, error=rc)

            call dm_sem_unlink(sem, error=rc)
            if (dm_is_error(rc)) call logger%error('failed to unlink semaphore /' // app%name, error=rc)
        end if

        call dm_stop(stat)
    end subroutine halt

    subroutine init(app, db, mqueue, sem, error)
        !! Initialises program.
        type(app_type),       intent(inout)         :: app    !! App type.
        type(db_type),        intent(out)           :: db     !! Database type.
        type(mqueue_type),    intent(out)           :: mqueue !! POSIX message queue type.
        type(sem_named_type), intent(out)           :: sem    !! POSIX semaphore type.
        integer,              intent(out), optional :: error  !! Error code.

        integer :: rc

        init_block: block
            ! Open SQLite database.
            rc = dm_db_open(db, path=app%database, timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call logger%error('failed to open database ' // app%database, error=rc)
                exit init_block
            end if

            if (.not. dm_db_table_has_observs(db)) then
                call logger%error('database tables not found', error=E_INVALID)
                exit init_block
            end if

            call logger%debug('opened database ' // app%database)

            ! Open observation message queue for reading.
            rc = dm_mqueue_open(mqueue = mqueue,      & ! Message queue type.
                                type   = TYPE_OBSERV, & ! Observation type.
                                name   = app%name,    & ! Name of message queue.
                                access = MQUEUE_RDONLY) ! Read-only access.

            if (dm_is_error(rc)) then
                call logger%error('failed to open mqueue /' // trim(app%name) // ': ' // &
                                  dm_system_error_message(), error=rc)
                exit init_block
            end if

            call logger%debug('opened mqueue /' // app%name)

            ! Create semaphore for IPC.
            if (app%ipc) then
                rc = dm_sem_open(sem, name=app%name, value=0, create=.true.)

                if (dm_is_error(rc)) then
                    call logger%error('failed to open semaphore /' // app%name, error=rc)
                    exit init_block
                end if

                call logger%debug('opened semaphore /' // app%name)
            end if

            call dm_signal_register(signal_callback)
        end block init_block

        if (present(error)) error = rc
    end subroutine init

    subroutine run(app, db, mqueue, sem)
        !! Opens observation message queue for reading, and stores received
        !! derived types in database.
        type(app_type),       intent(inout) :: app    !! App settings.
        type(db_type),        intent(inout) :: db     !! Database type.
        type(mqueue_type),    intent(inout) :: mqueue !! Message queue type.
        type(sem_named_type), intent(inout) :: sem    !! Semaphore type.

        integer           :: rc, steps, value
        type(observ_type) :: observ

        steps = 0
        call logger%info('started ' // APP_NAME)

        ipc_loop: do
            ! Blocking read from POSIX message queue.
            call logger%debug('waiting for observ on mqueue /' // app%name)
            rc = dm_mqueue_read(mqueue, observ)

            if (dm_is_error(rc)) then
                call logger%error('failed to read from mqueue /' // app%name, error=rc)
                call dm_sleep(1)
                cycle ipc_loop
            end if

            call logger%debug('received observ ' // observ%name)

            if (.not. dm_observ_is_valid(observ)) then
                call logger%error('invalid observ ' // trim(observ%name), error=E_INVALID)
                cycle ipc_loop
            end if

            if (dm_db_has_observ(db, observ%id)) then
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

                ! Handle database error.
                if (dm_is_error(rc)) then
                    call logger%error('failed to insert observ ' // observ%name, error=rc)
                    exit db_loop
                end if

                call logger%debug('inserted observ ' // observ%name)

                ! Post semaphore.
                if (app%ipc) then
                    rc = dm_sem_value(sem, value)

                    if (dm_is_error(rc)) then
                        call logger%error('failed to get value of semaphore /' // app%name, error=rc)
                        exit db_loop
                    end if

                    if (value /= 0) exit db_loop

                    rc = dm_sem_post(sem)

                    if (dm_is_error(rc)) then
                        call logger%error('failed to post to semaphore /' // app%name, error=rc)
                        exit db_loop
                    end if
                end if

                exit db_loop
            end do db_loop

            ! Forward observation if any receivers are left.
            rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)

            if (steps == 0) then
                rc = dm_db_optimize(db)

                if (dm_is_error(rc)) then
                    call logger%error('failed to optimize database', error=rc)
                else
                    call logger%debug('optimized database')
                end if
            end if

            ! Increase optimise step counter.
            steps = modulo(steps + 1, APP_DB_NSTEPS)
        end do ipc_loop
    end subroutine run

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app !! App type.

        type(arg_class) :: arg

        ! Required and optional command-line arguments.
        call arg%create()
        call arg%add('name',     short='n', type=ARG_TYPE_ID)       ! -n, --name <id>
        call arg%add('config',   short='c', type=ARG_TYPE_FILE)     ! -c, --config <path>
        call arg%add('logger',   short='l', type=ARG_TYPE_ID)       ! -l, --logger <id>
        call arg%add('database', short='d', type=ARG_TYPE_DATABASE) ! -d, --database <path>
        call arg%add('node',     short='N', type=ARG_TYPE_ID)       ! -N, --node <id>
        call arg%add('debug',    short='D', type=ARG_TYPE_LOGICAL)  ! -D, --debug
        call arg%add('ipc',      short='Q', type=ARG_TYPE_LOGICAL)  ! -Q, --ipc
        call arg%add('verbose',  short='V', type=ARG_TYPE_LOGICAL)  ! -V, --verbose

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('name',   app%name)
        call arg%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite configuration.
        call arg%get('logger',   app%logger)
        call arg%get('database', app%database)
        call arg%get('node',     app%node_id)
        call arg%get('debug',    app%debug)
        call arg%get('ipc',      app%ipc)
        call arg%get('verbose',  app%verbose)
        call arg%destroy()

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file if path is not emty.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('logger',   app%logger)
            call config%get('database', app%database)
            call config%get('node',     app%node_id)
            call config%get('debug',    app%debug)
            call config%get('ipc',      app%ipc)
            call config%get('verbose',  app%verbose)
        end if

        call config%close()
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger name')
            return
        end if

        if (.not. dm_string_has(app%database)) then
            call dm_error_out(rc, 'missing database')
            return
        end if

        if (.not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database ' // trim(app%database) // ' does not exist')
            return
        end if

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine signal_callback(signum) bind(c)
        !! C-interoperable signal handler that closes database, removes message
        !! queue, and stops program.
        integer(kind=c_int), intent(in), value :: signum

        call logger%info('exit on signal ' // dm_signal_name(signum))
        call halt(E_NONE)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 1x, a)', dm_lua_version(.true.), dm_db_version(.true.)
    end subroutine version_callback
end program dmdb
