! dmlogger.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmlogger
    !! The logger program collects log messages from a POSIX message queue and
    !! writes them log file or log database.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmlogger'
    integer,      parameter :: APP_MAJOR = 0
    integer,      parameter :: APP_MINOR = 9
    integer,      parameter :: APP_PATCH = 9

    integer, parameter :: APP_DB_NSTEPS  = 500                !! Number of steps before database is optimised.
    integer, parameter :: APP_DB_TIMEOUT = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.
    integer, parameter :: APP_UNIT_NONE  = -99999             !! Default file unit.

    type :: app_type
        !! Application settings.
        character(ID_LEN)        :: name      = APP_NAME !! Name of logger instance and POSIX semaphore.
        character(FILE_PATH_LEN) :: config    = ' '      !! Path to configuration file.
        character(FILE_PATH_LEN) :: database  = ' '      !! Path to SQLite database file.
        character(FILE_PATH_LEN) :: output    = ' '      !! Path to output file.
        character(NODE_ID_LEN)   :: node_id   = ' '      !! Node id.
        integer                  :: min_level = LL_INFO  !! Minimum level for a log to be stored in the database.
        logical                  :: ipc       = .false.  !! Use POSIX semaphore for process synchronisation.
        logical                  :: verbose   = .false.  !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer              :: rc     ! Return code.
    integer              :: unit   ! Log file unit.
    type(app_type)       :: app    ! App configuration.
    type(db_type)        :: db     ! Global database handle.
    type(mqueue_type)    :: mqueue ! Global POSIX message queue handle.
    type(sem_named_type) :: sem    ! Global POSIX semaphore handle.

    ! Disable output to log file.
    unit = APP_UNIT_NONE

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%name,    & ! Name of global logger.
                          node_id = app%node_id, & ! Sensor node id.
                          source  = app%name,    & ! Application name.
                          ipc     = .false.,     & ! Don't send logs via IPC.
                          verbose = app%verbose)   ! Prints logs to terminal.
    call logger%info('started ' // APP_NAME)

    call init(app, db, mqueue, sem, unit, rc)
    if (dm_is_error(rc)) call shutdown(rc)

    call run(app, db, mqueue, sem, unit, rc)
    call shutdown(rc)
contains
    subroutine shutdown(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: rc, stat

        stat = merge(STOP_FAILURE, STOP_SUCCESS, dm_is_error(error))

        if (dm_db_is_connected(db)) then
            call dm_db_close(db, error=rc)
            if (dm_is_error(rc)) call logger%error('failed to close database', error=rc)
        end if

        if (unit /= APP_UNIT_NONE) close (unit)

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

        call logger%info('stopped ' // APP_NAME, error=error)
        call dm_stop(stat)
    end subroutine shutdown

    subroutine init(app, db, mqueue, sem, unit, error)
        !! Opens database, log file, message queue, and semaphore.
        type(app_type),       intent(inout) :: app    !! App settings.
        type(db_type),        intent(inout) :: db     !! Log database.
        type(mqueue_type),    intent(inout) :: mqueue !! Message queue.
        type(sem_named_type), intent(inout) :: sem    !! Semaphore.
        integer,              intent(inout) :: unit   !! File unit.
        integer,              intent(out)   :: error  !! Error code.

        init_block: block
            integer :: stat

            ! Open SQLite database.
            if (dm_string_has(app%database)) then
                rc = dm_db_open(db, path=app%database, timeout=APP_DB_TIMEOUT)

                if (dm_is_error(rc)) then
                    call logger%error('failed to open database ' // app%database, error=rc)
                    exit init_block
                end if

                call logger%debug('opened database ' // app%database)

                if (.not. dm_db_table_has_logs(db)) then
                    call logger%error('missing table in database ' // app%database, error=E_INVALID)
                    exit init_block
                end if
            end if

            ! Open log file for writing.
            if (dm_string_has(app%output) .and. app%output /= '-') then
                if (dm_file_is_fifo(app%output)) then
                    ! Open named pipe.
                    open (access='stream', action='write', file=trim(app%output), form='formatted', iostat=stat, newunit=unit, status='old')
                else
                    ! Open regular file.
                    open (action='write', file=trim(app%output), iostat=stat, newunit=unit, position='append', status='unknown')
                end if

                if (stat /= 0) rc = E_IO

                if (dm_is_error(rc)) then
                    call logger%error('failed to open log file ' // app%output, error=rc)
                    return
                end if
            end if

            ! Open log message queue for reading.
            rc = dm_mqueue_open(mqueue = mqueue,   &    ! Message queue type.
                                type   = TYPE_LOG, &    ! Log type.
                                name   = app%name, &    ! Name of message queue.
                                access = MQUEUE_RDONLY) ! Read-only access.

            if (dm_is_error(rc)) then
                call logger%error('failed to open mqueue /' // trim(app%name) // ': ' // dm_system_error_message(), error=rc)
                exit init_block
            end if

            call logger%debug('opened mqueue ' // app%name)

            ! Create semaphore for IPC.
            if (app%ipc) then
                rc = dm_sem_open(sem, app%name, value=0, create=.true.)

                if (dm_is_error(rc)) then
                    call logger%error('failed to open semaphore /' // app%name, error=rc)
                    exit init_block
                end if

                call logger%debug('opened semaphore ' // app%name)
            end if

            ! Register signal handler.
            call dm_signal_register(signal_callback)
        end block init_block

        error = rc
    end subroutine init

    subroutine output(log, unit)
        !! Append log to file `path`.
        type(log_type), intent(inout) :: log  !! Log to write.
        integer,        intent(in)    :: unit !! File unit.

        integer :: rc

        if (unit == APP_UNIT_NONE) return
        call logger%out(log, unit, error=rc)
        if (dm_is_error(rc)) call dm_error_out(rc, 'failed to write to log file')
    end subroutine output

    subroutine run(app, db, mqueue, sem, unit, error)
        !! Writes received logs to log file and/or log database. The given
        !! message queue has to be opened already.
        type(app_type),       intent(inout) :: app    !! App settings.
        type(db_type),        intent(inout) :: db     !! Log database.
        type(mqueue_type),    intent(inout) :: mqueue !! Message queue.
        type(sem_named_type), intent(inout) :: sem    !! Semaphore.
        integer,              intent(inout) :: unit   !! Log file unit.
        integer,              intent(out)   :: error  !! Error code.

        integer        :: rc, steps, value
        logical        :: has_db, has_file
        type(log_type) :: log

        has_db   = dm_db_is_connected(db)
        has_file = (unit /= APP_UNIT_NONE .and. app%output /= '-')

        if (has_db) then
            call logger%debug('writing logs of level >= ' // trim(LOG_LEVEL_NAMES(app%min_level)) // ' to database ' // app%database)
        else
            call logger%debug('database storage is disabled')
        end if

        if (has_file) then
            call logger%debug('writing logs of level >= ' // trim(LOG_LEVEL_NAMES(app%min_level)) // ' to file ' // app%output)
        else
            call logger%debug('file storage is disabled')
        end if

        call logger%debug('waiting for log on mqueue /' // trim(app%name))

        steps = 0

        ipc_loop: do
            ! Blocking read from POSIX message queue.
            rc = dm_mqueue_read(mqueue, log)

            if (dm_is_error(rc)) then
                call logger%error('failed to read from mqueue /' // trim(app%name) // ', next attempt in 30 sec', error=rc)
                call dm_sleep(30)
                cycle ipc_loop
            end if

            ! Replace missing or invalid sensor node id.
            if (.not. dm_id_is_valid(log%node_id)) log%node_id = app%node_id

            ! Print log to standard error.
            if (app%verbose) call logger%out(log)

            ! Skip if log level is lower than minimum level required for storage.
            if (log%level < app%min_level) cycle ipc_loop

            ! Write log to file.
            if (has_file) call output(log, unit)

            ! Skip if database storage is disabled.
            if (.not. has_db) cycle ipc_loop

            ! Skip if log already exists.
            if (dm_db_has_log(db, log%id)) then
                call logger%warning('log ' // trim(log%id) // ' exists', error=E_EXIST)
                cycle ipc_loop
            end if

            ! Add log to database.
            db_loop: do
                rc = dm_db_insert(db, log)

                if (dm_is_error(rc)) then
                    ! Get more precise database error.
                    rc = dm_db_error(db)

                    if (rc == E_DB_BUSY) then
                        call logger%warning('database busy', error=rc)
                        call dm_db_sleep(APP_DB_TIMEOUT)
                        cycle db_loop
                    end if

                    call logger%error('failed to insert log ' // trim(log%id) // ': ' // dm_db_error_message(db), error=rc)
                    exit db_loop
                end if

                if (app%ipc) then
                    ! Post semaphore.
                    rc = dm_sem_value(sem, value)
                    if (value == 0) rc = dm_sem_post(sem)
                end if

                exit db_loop
            end do db_loop

            ! Optimise database.
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

        error = rc
    end subroutine run

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app

        type(arg_class) :: arg

        ! Required and optional command-line arguments.
        call arg%create()
        call arg%add('name',     short='n', type=ARG_TYPE_ID)      ! -n, --name <id>
        call arg%add('config',   short='c', type=ARG_TYPE_FILE)    ! -c, --config <path>
        call arg%add('database', short='d', type=ARG_TYPE_DATABASE, exist=.true.) ! -d, --database <path>
        call arg%add('output',   short='o', type=ARG_TYPE_FILE)    ! -o, --output <path>
        call arg%add('node',     short='N', type=ARG_TYPE_ID)      ! -N, --node <id>
        call arg%add('minlevel', short='L', type=ARG_TYPE_LEVEL)   ! -L, --minlevel <n>
        call arg%add('ipc',      short='Q', type=ARG_TYPE_LOGICAL) ! -Q, --ipc
        call arg%add('verbose',  short='V', type=ARG_TYPE_LOGICAL) ! -V, --verbose

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('name',   app%name)
        call arg%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        call arg%get('database', app%database)
        call arg%get('output',   app%output)
        call arg%get('node',     app%node_id)
        call arg%get('minlevel', app%min_level)
        call arg%get('ipc',      app%ipc)
        call arg%get('verbose',  app%verbose)
        call arg%destroy()

        app%verbose = (app%verbose .or. app%output == '-')
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
            call config%get('database', app%database)
            call config%get('output',   app%output)
            call config%get('node',     app%node_id)
            call config%get('minlevel', app%min_level)
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

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (dm_string_has(app%database) .and. .not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database does not exist')
            return
        end if

        if (.not. dm_string_has(app%database) .and. .not. dm_string_has(app%output) .and. .not. app%verbose) then
            call dm_error_out(rc, 'database, output, or verbose mode required')
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
        integer(c_int), intent(in), value :: signum

        call logger%debug('exit on on signal ' // dm_signal_name(signum))
        call shutdown(E_NONE)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 1x, a)', dm_lua_version(.true.), dm_db_version(.true.)
    end subroutine version_callback
end program dmlogger
