! dmlogger.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmlogger
    !! The logger program collects log messages from a POSIX message queue and
    !! stores them in a SQLite database.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmlogger'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 8

    integer, parameter :: APP_DB_NSTEPS  = 500                !! Number of steps before database is optimised.
    integer, parameter :: APP_DB_TIMEOUT = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)        :: name      = APP_NAME !! Name of logger instance and POSIX semaphore.
        character(len=FILE_PATH_LEN) :: config    = ' '      !! Path to configuration file.
        character(len=FILE_PATH_LEN) :: database  = ' '      !! Path to SQLite database file.
        character(len=NODE_ID_LEN)   :: node_id   = ' '      !! Node id.
        integer                      :: min_level = LL_INFO  !! Minimum level for a log to be stored in the database.
        logical                      :: ipc       = .false.  !! Use POSIX semaphore for process synchronisation.
        logical                      :: verbose   = .false.  !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer              :: rc     ! Return code.
    type(app_type)       :: app    ! App configuration.
    type(db_type)        :: db     ! Global database handle.
    type(mqueue_type)    :: mqueue ! Global POSIX message queue handle.
    type(sem_named_type) :: sem    ! Global POSIX semaphore handle.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    init_block: block
        ! Initialise logger.
        logger => dm_logger_get_default()
        call logger%configure(name    = app%name,    & ! Name of global logger.
                              node_id = app%node_id, & ! Sensor node id.
                              source  = app%name,    & ! Application name.
                              ipc     = .false.,     & ! Don't send logs via IPC.
                              verbose = app%verbose)   ! Prints logs to terminal.

        ! Open SQLite database.
        rc = dm_db_open(db, path=app%database, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call logger%error('failed to open database ' // app%database, error=rc)
            exit init_block
        end if

        if (.not. dm_db_table_has_logs(db)) then
            call logger%error('database table not found', error=E_INVALID)
            exit init_block
        end if

        ! Open log message queue for reading.
        rc = dm_mqueue_open(mqueue = mqueue,   &    ! Message queue type.
                            type   = TYPE_LOG, &    ! Log type.
                            name   = app%name, &    ! Name of message queue.
                            access = MQUEUE_RDONLY) ! Read-only access.

        if (dm_is_error(rc)) then
            call logger%error('failed to open mqueue /' // trim(app%name) // ': ' // &
                              dm_system_error_message(), error=rc)
            exit init_block
        end if

        ! Create semaphore for IPC.
        if (app%ipc) then
            rc = dm_sem_open(sem, app%name, value=0, create=.true.)

            if (dm_is_error(rc)) then
                call logger%error('failed to open semaphore /' // app%name, error=rc)
                exit init_block
            end if
        end if

        ! Run the IPC loop.
        call dm_signal_register(signal_callback)
        call run(app, db, mqueue, sem)
    end block init_block

    call halt(rc)
contains
    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: rc, stat

        stat = dm_btoi(dm_is_error(error), STOP_FAILURE, STOP_SUCCESS)

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

    subroutine run(app, db, mqueue, sem)
        !! Stores received logs in database. The given message queue has to be
        !! opened already.
        type(app_type),       intent(inout) :: app    !! App settings.
        type(db_type),        intent(inout) :: db     !! Log database.
        type(mqueue_type),    intent(inout) :: mqueue !! Message queue.
        type(sem_named_type), intent(inout) :: sem    !! Semaphore.

        integer        :: rc, steps, value
        type(log_type) :: log

        steps = 0

        call logger%info('started ' // APP_NAME)
        call logger%debug('waiting for log on mqueue /' // trim(app%name) // ' (minimum log level is ' // &
                          trim(LOG_LEVEL_NAMES(app%min_level)) // ')')
        ipc_loop: do
            ! Blocking read from POSIX message queue.
            rc = dm_mqueue_read(mqueue, log)

            if (dm_is_error(rc)) then
                call logger%error('failed to read from mqueue /' // app%name, error=rc)
                call dm_sleep(1)
                cycle ipc_loop
            end if

            ! Replace missing or invalid sensor node id.
            if (.not. dm_id_is_valid(log%node_id)) log%node_id = app%node_id

            ! Print to standard output.
            if (app%verbose) call logger%out(log)

            ! Skip if log level is lower than minimum level.
            if (log%level < app%min_level) cycle ipc_loop

            ! Skip if log already exists.
            if (dm_db_has_log(db, log%id)) then
                call logger%warning('log with id ' // trim(log%id) // ' exists', error=E_EXIST)
                cycle ipc_loop
            end if

            db_loop: do
                ! Add log to database.
                rc = dm_db_insert(db, log)

                if (dm_is_error(rc)) then
                    ! Get more precise database error.
                    rc = dm_db_error(db)

                    if (rc == E_DB_BUSY) then
                        call logger%warning('database busy', error=rc)
                        call dm_db_sleep(APP_DB_TIMEOUT)
                        cycle db_loop
                    end if

                    call logger%error('failed to insert log with id ' // trim(log%id) // ': ' // &
                                      dm_db_error_message(db), error=rc)
                    exit db_loop
                end if

                if (app%ipc) then
                    ! Post semaphore.
                    rc = dm_sem_value(sem, value)
                    if (value == 0) rc = dm_sem_post(sem)
                end if

                exit db_loop
            end do db_loop

            if (steps == 0) then
                rc = dm_db_optimize(db)

                if (dm_is_error(rc)) then
                    call logger%error('failed to optimize database', error=rc)
                else
                    call logger%debug('optimized database')
                end if
            end if

            ! Increase optimise step counter.
            steps = modulo(dm_inc(steps), APP_DB_NSTEPS)
        end do ipc_loop
    end subroutine run

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app

        type(arg_type) :: args(7)

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),       & ! -n, --name <id>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),     & ! -c, --config <path>
            arg_type('database', short='d', type=ARG_TYPE_DATABASE), & ! -d, --database <path>
            arg_type('node',     short='N', type=ARG_TYPE_ID),       & ! -N, --node <id>
            arg_type('minlevel', short='L', type=ARG_TYPE_LEVEL),    & ! -L, --minlevel <n>
            arg_type('ipc',      short='Q', type=ARG_TYPE_LOGICAL),  & ! -Q, --ipc
            arg_type('verbose',  short='V', type=ARG_TYPE_LOGICAL)   & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%name)
        call dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        call dm_arg_get(args(3), app%database)
        call dm_arg_get(args(4), app%node_id)
        call dm_arg_get(args(5), app%min_level)
        call dm_arg_get(args(6), app%ipc)
        call dm_arg_get(args(7), app%verbose)

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file if path is not emty.
        type(app_type), intent(inout) :: app !! App type.
        type(config_type)             :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'database', app%database)
            call dm_config_get(config, 'node',     app%node_id)
            call dm_config_get(config, 'minlevel', app%min_level)
            call dm_config_get(config, 'ipc',      app%ipc)
            call dm_config_get(config, 'verbose',  app%verbose)
        end if

        call dm_config_close(config)
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (.not. dm_string_has(app%database)) then
            call dm_error_out(rc, 'missing database')
            return
        end if

        if (.not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database does not exist')
            return
        end if

        if (.not. dm_log_level_is_valid(app%min_level)) then
            call dm_error_out(rc, 'invalid log level')
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
        call dm_sleep(1)
        call halt(0)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 1x, a)', dm_lua_version(.true.), dm_db_version(.true.)
    end subroutine version_callback
end program dmlogger
