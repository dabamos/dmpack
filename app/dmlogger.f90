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
    integer,          parameter :: APP_PATCH = 2

    integer, parameter :: APP_DB_NSTEPS  = 500                !! Number of steps before database is optimised.
    integer, parameter :: APP_DB_TIMEOUT = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)        :: name     = APP_NAME   !! Name of logger instance and POSIX semaphore.
        character(len=FILE_PATH_LEN) :: config   = ' '        !! Path to configuration file.
        character(len=FILE_PATH_LEN) :: database = ' '        !! Path to SQLite database file.
        character(len=NODE_ID_LEN)   :: node     = ' '        !! Node id.
        integer                      :: minlevel = LL_WARNING !! Minimum level for a log to be stored in the database.
        logical                      :: ipc      = .false.    !! Use POSIX semaphore for process synchronisation.
        logical                      :: verbose  = .false.    !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer           :: rc     ! Return code.
    type(app_type)    :: app    ! App configuration.
    type(db_type)     :: db     ! Global database handle.
    type(mqueue_type) :: mqueue ! Global POSIX message queue handle.
    type(sem_type)    :: sem    ! Global POSIX semaphore handle.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    init_block: block
        ! Initialise logger.
        logger => dm_logger_get()
        call logger%configure(name    = app%name, &  ! Name of global logger.
                              node_id = app%node, &  ! Sensor node id.
                              source  = app%name, &  ! Application name.
                              ipc     = .false., &   ! Don't send logs via IPC.
                              verbose = app%verbose) ! Prints logs to terminal.

        ! Open SQLite database.
        rc = dm_db_open(db, path=app%database, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call logger%error('failed to open database ' // app%database, error=rc)
            exit init_block
        end if

        ! Open log message queue for reading.
        rc = dm_mqueue_open(mqueue = mqueue, &
                            type   = TYPE_LOG, &
                            name   = app%name, &
                            access = MQUEUE_RDONLY)

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
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app

        character(len=:), allocatable :: version
        type(arg_type)                :: args(7)

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <id>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('database', short='d', type=ARG_TYPE_DB),      & ! -d, --database <path>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('minlevel', short='L', type=ARG_TYPE_LEVEL),   & ! -L, --minlevel <n>
            arg_type('ipc',      short='Q', type=ARG_TYPE_LOGICAL), & ! -Q, --ipc
            arg_type('verbose',  short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        version = dm_lua_version(.true.) // ' ' // dm_db_version(.true.)
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, version)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%name)
        call dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        call dm_arg_get(args(3), app%database)
        call dm_arg_get(args(4), app%node)
        call dm_arg_get(args(5), app%minlevel)
        call dm_arg_get(args(6), app%ipc)
        call dm_arg_get(args(7), app%verbose)

        rc = E_INVALID

        if (.not. dm_id_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
        end if

        if (.not. dm_id_valid(app%node)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (len_trim(app%database) == 0) then
            call dm_error_out(rc, 'missing database')
            return
        end if

        if (.not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database does not exist')
            return
        end if

        if (.not. dm_log_valid(app%minlevel)) then
            call dm_error_out(rc, 'invalid log level')
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
            call dm_config_get(config, 'database', app%database)
            call dm_config_get(config, 'node',     app%node)
            call dm_config_get(config, 'minlevel', app%minlevel)
            call dm_config_get(config, 'ipc',      app%ipc)
            call dm_config_get(config, 'verbose',  app%verbose)
        end if

        call dm_config_close(config)
    end function read_config

    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error

        integer :: rc, stat

        stat = STOP_SUCCESS
        if (dm_is_error(error)) stat = STOP_FAILURE

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
        !! Stores received logs in database. The given message queue has to be
        !! opened already.
        type(app_type),    intent(inout) :: app    !! App settings.
        type(db_type),     intent(inout) :: db     !! Log database.
        type(mqueue_type), intent(inout) :: mqueue !! Message queue.
        type(sem_type),    intent(inout) :: sem    !! Semaphore.

        integer        :: rc, steps, value
        type(log_type) :: log

        steps = 0

        call logger%info('started ' // dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH))
        call logger%debug('waiting for log on mqueue /' // trim(app%name) // ' (minimum log level is ' // &
                          trim(LOG_LEVEL_NAMES(app%minlevel)) // ')')
        ipc_loop: do
            ! Blocking read from POSIX message queue.
            rc = dm_mqueue_read(mqueue, log)

            if (dm_is_error(rc)) then
                call logger%error('failed to read from mqueue /' // app%name, error=rc)
                call dm_sleep(1)
                cycle ipc_loop
            end if

            ! Replace missing or invalid sensor node id.
            if (.not. dm_id_valid(log%node_id)) log%node_id = app%node

            ! Print to standard output.
            if (app%verbose) call logger%out(log)

            ! Skip if log level is lower than minimum level.
            if (log%level < app%minlevel) cycle ipc_loop

            ! Skip if log already exists.
            if (dm_db_log_exists(db, log%id)) then
                call logger%warning('log ' // trim(log%id) // ' exists', error=E_EXIST)
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

                    call logger%error('failed to insert log ' // trim(log%id) // ': ' // &
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
                ! Optimise database.
                call logger%debug('optimizing database')
                rc = dm_db_optimize(db)
                if (dm_is_error(rc)) call logger%error('failed to optimize database', error=rc)
            end if

            ! Increase optimise step counter.
            steps = modulo(dm_inc(steps), APP_DB_NSTEPS)
        end do ipc_loop
    end subroutine run

    subroutine signal_callback(signum) bind(c)
        !! C-interoperable signal handler that closes database, removes message
        !! queue, and stops program.
        integer(kind=c_int), intent(in), value :: signum

        select case (signum)
            case default
                call logger%info('exit on signal ' // dm_itoa(signum))
                call dm_sleep(2)
                call halt(0)
        end select
    end subroutine signal_callback
end program dmlogger
