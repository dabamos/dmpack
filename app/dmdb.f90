! dmdb.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmdb
    !! The database program collects observations from a POSIX message queue and
    !! stores them in a SQLite database.
    use :: dmpack, dm_log => dm_logger_log
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmdb'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 0

    ! Program parameters.
    integer, parameter :: APP_DB_NSTEPS   = 500                !! Number of steps before database is optimised.
    integer, parameter :: APP_DB_TIMEOUT  = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.
    logical, parameter :: APP_MQ_BLOCKING = .true.             !! Observation forwarding is blocking.

    type :: app_type
        !! Application settings.
        character(len=LOGGER_NAME_LEN) :: name     = APP_NAME !! Name of logger instance and POSIX semaphore.
        character(len=FILE_PATH_LEN)   :: config   = ' '      !! Path to configuration file.
        character(len=LOGGER_NAME_LEN) :: logger   = ' '      !! Name of logger (name implies IPC).
        character(len=FILE_PATH_LEN)   :: database = ' '      !! Path to SQLite database file.
        character(len=NODE_ID_LEN)     :: node     = ' '      !! Node id.
        logical                        :: debug    = .false.  !! Forward debug messages via IPC.
        logical                        :: ipc      = .false.  !! Use POSIX semaphore for process synchronisation.
        logical                        :: verbose  = .false.  !! Print debug messages to stderr.
    end type app_type

    integer           :: rc     ! Return code.
    type(app_type)    :: app    ! App configuration.
    type(db_type)     :: db     ! Database handle.
    type(mqueue_type) :: mqueue ! POSIX message queue handle.
    type(sem_type)    :: sem    ! POSIX semaphore handle.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Initialise logger.
    call dm_logger_init(name    = app%logger, &
                        node_id = app%node, &
                        source  = app%name, &
                        debug   = app%debug, &
                        ipc     = (len_trim(app%logger) > 0), &
                        verbose = app%verbose)

    init_block: block
        ! Open SQLite database.
        rc = dm_db_open(db, path=app%database, timeout=APP_DB_TIMEOUT)

        if (dm_is_error(rc)) then
            call dm_log(LOG_ERROR, 'failed to open database ' // app%database, error=rc)
            exit init_block
        end if

        ! Open observation message queue for reading.
        rc = dm_mqueue_open(mqueue = mqueue, &
                            type   = TYPE_OBSERV, &
                            name   = app%name, &
                            access = MQUEUE_RDONLY)

        if (dm_is_error(rc)) then
            call dm_log(LOG_ERROR, 'failed to open mqueue /' // app%name, error=rc)
            exit init_block
        end if

        ! Create semaphore for IPC.
        if (app%ipc) then
            rc = dm_sem_open(sem, name=app%name, value=0, create=.true.)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to open semaphore /' // app%name, error=rc)
                exit init_block
            end if
        end if

        ! Run the IPC loop.
        call dm_signal_register(signal_handler)
        call run(app, db, mqueue, sem)
    end block init_block

    if (dm_is_error(rc)) call halt(1)
    call halt(0)
contains
    integer function forward_observ(observ, name) result(rc)
        !! Forwards given observation to next receiver.
        type(observ_type), intent(inout)        :: observ !! Observation to forward.
        character(len=*),  intent(in), optional :: name   !! App name.

        integer           :: next
        type(mqueue_type) :: mqueue

        rc   = E_NONE
        next = observ%next

        do
            ! Increase the receiver index.
            next = max(0, next) + 1

            ! End of receiver list reached?
            if (next > observ%nreceivers) then
                call dm_log(LOG_DEBUG, 'no receivers left in observ ' // observ%name, observ=observ)
                return
            end if

            ! Invalid receiver name?
            if (.not. dm_id_valid(observ%receivers(next))) then
                rc = E_INVALID
                call dm_log(LOG_ERROR, 'invalid receiver ' // trim(observ%receivers(next)) // &
                            ' in observ ' // observ%name, observ=observ, error=rc)
                return
            end if

            ! Cycle to next + 1 if receiver name equals app name. We don't want
            ! to send the observation to this program instance.
            if (.not. present(name)) exit
            if (observ%receivers(next) /= name) exit
            call dm_log(LOG_DEBUG, 'skipping receiver ' // dm_itoa(next) // ' (' // &
                        trim(observ%receivers(next)) // ') of observ ' // observ%name)
        end do

        mqueue_block: block
            ! Open message queue of receiver for writing.
            rc = dm_mqueue_open(mqueue   = mqueue, &
                                type     = TYPE_OBSERV, &
                                name     = observ%receivers(next), &
                                access   = MQUEUE_WRONLY, &
                                blocking = APP_MQ_BLOCKING)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to open mqueue /' // observ%receivers(next), &
                            observ=observ, error=rc)
                exit mqueue_block
            end if

            ! Send observation to message queue.
            observ%next = next
            rc = dm_mqueue_write(mqueue, observ)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to send observ ' // trim(observ%name) // &
                            ' to mqueue /' // observ%receivers(next), observ=observ, error=rc)
                exit mqueue_block
            end if

            call dm_log(LOG_DEBUG, 'sent observ ' // trim(observ%name) // ' to mqueue /' // &
                        observ%receivers(next), observ=observ)
        end block mqueue_block

        ! Close message queue.
        rc = dm_mqueue_close(mqueue)

        if (dm_is_error(rc)) then
            call dm_log(LOG_WARNING, 'failed to close mqueue /' // observ%receivers(next), &
                        observ=observ, error=rc)
        end if
    end function forward_observ

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

    subroutine halt(stat)
        !! Cleans up and stops program.
        integer, intent(in) :: stat
        integer             :: rc

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
        type(app_type),    intent(inout) :: app
        type(db_type),     intent(inout) :: db
        type(mqueue_type), intent(inout) :: mqueue
        type(sem_type),    intent(inout) :: sem

        integer           :: rc, steps, value
        type(observ_type) :: observ

        steps = 0
        call dm_log(LOG_INFO, 'started ' // app%name)

        ipc_loop: do
            ! Blocking read from POSIX message queue.
            rc = dm_mqueue_read(mqueue, observ)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to read from mqueue /' // app%name, error=rc)
                call dm_sleep(1)
                cycle ipc_loop
            end if

            if (.not. dm_observ_valid(observ)) then
                call dm_log(LOG_ERROR, 'invalid observ ' // trim(observ%name), error=E_INVALID)
                cycle ipc_loop
            end if

            if (dm_db_exists_observ(db, observ%id)) then
                call dm_log(LOG_WARNING, 'observ ' // trim(observ%id) // ' exists', error=E_EXIST)
                cycle ipc_loop
            end if

            db_loop: do
                ! Add observation to database.
                rc = dm_db_insert(db, observ)

                ! Retry if database is busy.
                if (rc == E_DB_BUSY) then
                    call dm_log(LOG_DEBUG, 'database busy', error=rc)
                    call dm_db_sleep(APP_DB_TIMEOUT)
                    cycle db_loop
                end if

                ! Get more precise database error.
                if (dm_is_error(rc)) then
                    rc = dm_db_error(db)
                    call dm_log(LOG_ERROR, 'failed to insert observ ' // observ%name, error=rc)
                    exit db_loop
                end if

                call dm_log(LOG_DEBUG, 'inserted observ ' // observ%name)

                ! Post semaphore.
                if (app%ipc) then
                    rc = dm_sem_value(sem, value)
                    if (value == 0) rc = dm_sem_post(sem)
                end if

                exit db_loop
            end do db_loop

            ! Forward observation.
            rc = forward_observ(observ, app%name)

            if (steps == 0) then
                ! Optimise database.
                call dm_log(LOG_DEBUG, 'optimizing database')
                rc = dm_db_optimize(db)

                if (dm_is_error(rc)) then
                    call dm_log(LOG_ERROR, 'failed to optimize database', error=rc)
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
                call dm_log(LOG_INFO, 'exit on signal ' // dm_itoa(signum))
                call halt(0)
        end select
    end subroutine signal_handler
end program dmdb
