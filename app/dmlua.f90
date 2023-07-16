! dmlua.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmlua
    !! Lua script spawner that reads an observation from POSIX message queue,
    !! passes it to a configured Lua script, and forwards the returned
    !! observation to the next specified receiver.
    use :: dmpack, dm_log => dm_logger_log
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmlua'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9

    integer, parameter :: APP_PROC_LEN    = 32     !! Max. length of Lua function name.
    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Global application settings.
        character(len=APP_NAME_LEN)    :: name      = APP_NAME  !! Instance and configuration name (required).
        character(len=FILE_PATH_LEN)   :: config    = ' '       !! Path to configuration file (required).
        character(len=LOGGER_NAME_LEN) :: logger    = ' '       !! Name of logger.
        character(len=NODE_ID_LEN)     :: node      = ' '       !! Node id (required).
        character(len=APP_PROC_LEN)    :: proc      = 'process' !! Name of Lua function (required).
        character(len=FILE_PATH_LEN)   :: script    = ' '       !! Path to Lua script file (required).
        logical                        :: verbose   = .false.   !! Print debug messages to stderr.
    end type app_type

    integer              :: rc     ! Return code.
    type(app_type)       :: app    ! App configuration.
    type(lua_state_type) :: lua    ! Lua interpreter.
    type(mqueue_type)    :: mqueue ! Message queue.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Initialise logger.
    call dm_logger_init(name    = app%logger, &
                        node_id = app%node, &
                        source  = app%name, &
                        ipc     = (len_trim(app%logger) > 0), &
                        verbose = app%verbose)

    init_block: block
        ! Initialise Lua interpreter.
        rc = dm_lua_init(lua)

        if (dm_is_error(rc)) then
            call dm_log(LOG_ERROR, 'failed to init Lua interpreter', error=rc)
            exit init_block
        end if

        ! Open and run Lua script.
        rc = dm_lua_open(lua, app%script, eval=.true.)

        if (dm_is_error(rc)) then
            call dm_log(LOG_ERROR, 'failed to load Lua script', error=rc)
            exit init_block
        end if

        ! Open observation message queue for reading.
        rc = dm_mqueue_open(mqueue = mqueue, &
                            type   = TYPE_OBSERV, &
                            name   = app%name, &
                            access = MQUEUE_RDONLY)

        if (dm_is_error(rc)) then
            call dm_log(LOG_ERROR, 'failed to open message queue /' // app%name, error=rc)
            exit init_block
        end if

        ! Register signal handlers and run the IPC loop.
        call register_signal_handlers()
        call run(app, lua, mqueue)
    end block init_block

    ! Clean up and exit.
    if (dm_is_error(rc)) call halt(1)
    call halt(0)
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        character(len=*), parameter :: PROC_SET = &
            '-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz'
        type(app_type), intent(inout) :: app
        type(arg_type)                :: args(7)

        rc = E_NONE

        args = [ &
            arg_type('name',      short='n', type=ARG_TYPE_ID),   & ! -n, --name <string>
            arg_type('config',    short='c', type=ARG_TYPE_FILE), & ! -c, --config <path>
            arg_type('logger',    short='l', type=ARG_TYPE_ID),   & ! -l, --logger <string>
            arg_type('node',      short='N', type=ARG_TYPE_ID),   & ! -N, --node <string>
            arg_type('procedure', short='p', type=ARG_TYPE_CHAR), & ! -p, --procedure <string>
            arg_type('script',    short='s', type=ARG_TYPE_FILE), & ! -s, --script <path>
            arg_type('verbose',   short='V', type=ARG_TYPE_BOOL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        rc = dm_arg_get(args(3), app%logger)
        rc = dm_arg_get(args(4), app%node)
        rc = dm_arg_get(args(5), app%proc)
        rc = dm_arg_get(args(6), app%script)
        rc = dm_arg_get(args(7), app%verbose)

        ! Validate options.
        rc = E_INVALID

        if (.not. dm_id_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (len_trim(app%logger) > 0 .and. .not. dm_id_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger')
            return
        end if

        if (.not. dm_id_valid(app%node)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (verify(trim(app%proc), PROC_SET) > 0) then
            call dm_error_out(rc, 'invalid Lua function name ' // app%proc)
            return
        end if

        if (.not. dm_file_exists(app%script)) then
            call dm_error_out(rc, 'Lua script ' // trim(app%script) // ' not found')
            return
        end if

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.
        type(config_type)             :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            rc = dm_config_get(config, 'logger',    app%logger)
            rc = dm_config_get(config, 'node',      app%node)
            rc = dm_config_get(config, 'procedure', app%proc)
            rc = dm_config_get(config, 'script',    app%script)
            rc = dm_config_get(config, 'verbose',   app%verbose)
            rc = E_NONE
        end if

        call dm_config_close(config)
    end function read_config

    integer function send_observ(observ) result(rc)
        !! Sends passed observation to next receiver via POSIX message queue.
        type(observ_type), intent(inout) :: observ

        integer           :: next
        type(mqueue_type) :: mqueue

        next = max(0, observ%next) + 1

        ! Validate receiver.
        if (next > min(observ%nreceivers, OBSERV_MAX_NRECEIVERS)) then
            call dm_log(LOG_DEBUG, 'no receivers left in observation ' // observ%name, observ=observ)
            rc = E_NONE
            return
        end if

        if (.not. dm_id_valid(observ%receivers(next))) then
            call dm_log(LOG_ERROR, 'invalid receiver ' // observ%receivers(next) // &
                        ' in observation ' // observ%name, observ=observ, error=E_INVALID)
            rc = E_INVALID
            return
        end if

        mqueue_block: block
            ! Open message queue of receiver for writing.
            rc = dm_mqueue_open(mqueue   = mqueue, &
                                type     = TYPE_OBSERV, &
                                name     = observ%receivers(next), &
                                access   = MQUEUE_WRONLY, &
                                blocking = APP_MQ_BLOCKING)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to open message queue /' // observ%receivers(next), &
                            observ=observ, error=rc)
                exit mqueue_block
            end if

            ! Send observation to message queue.
            observ%next = next
            rc = dm_mqueue_write(mqueue, observ)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to send observation ' // trim(observ%name) // &
                            ' to message queue /' // observ%receivers(next), observ=observ, error=rc)
                exit mqueue_block
            end if

            call dm_log(LOG_DEBUG, 'sent observation ' // trim(observ%name) // ' to message queue /' // &
                        observ%receivers(next), observ=observ)
        end block mqueue_block

        ! Close message queue.
        rc = dm_mqueue_close(mqueue)

        if (dm_is_error(rc)) then
            call dm_log(LOG_WARNING, 'failed to close message queue /' // observ%receivers(next), &
                        observ=observ, error=rc)
        end if
    end function send_observ

    subroutine halt(stat)
        !! Cleans up and stops program.
        integer, intent(in) :: stat
        integer             :: rc

        rc = dm_mqueue_close(mqueue)
        rc = dm_mqueue_unlink(mqueue)

        call dm_lua_destroy(lua)
        call dm_stop(stat)
    end subroutine halt

    subroutine run(app, lua, mqueue)
        !! Waits for incoming observation, passes derived type as table to Lua
        !! function. The Lua function has to return the (modified) observation
        !! on exit.
        !!
        !! Variable `obs_in` stores the observation type received from message
        !! queue. The observation data returned from the Lua function is stored
        !! in `obs_out` and will be forwarded to the next receiver. On error,
        !! the received observation will be forwarded instead.
        type(app_type),       intent(inout) :: app
        type(lua_state_type), intent(inout) :: lua
        type(mqueue_type),    intent(inout) :: mqueue

        integer           :: rc
        type(observ_type) :: obs_in, obs_out

        call dm_log(LOG_INFO, 'starting ' // app%name)

        ipc_loop: do
            ! Blocking read from POSIX message queue.
            rc = dm_mqueue_read(mqueue, obs_in)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to read from message queue /' // app%name, error=rc)
                call dm_sleep(1)
                cycle ipc_loop
            end if

            ! Validate observation.
            if (.not. dm_observ_valid(obs_in)) then
                call dm_log(LOG_ERROR, 'observation ' // trim(obs_in%id) // ' is invalid', &
                            observ=obs_in, error=E_INVALID)
                cycle ipc_loop
            end if

            call dm_log(LOG_DEBUG, 'processing observation ' // obs_in%name, observ=obs_in)

            ! Pass the observation to the Lua function in read the returned
            ! observation.
            lua_block: block
                ! Load Lua function.
                rc = dm_lua_global(lua, trim(app%proc))

                if (dm_is_error(rc)) then
                    call dm_log(LOG_ERROR, 'failed to load Lua function ' // trim(app%proc) // '()', error=rc)
                    exit lua_block
                end if

                if (.not. dm_lua_is_function(lua)) then
                    rc = E_INVALID
                    call dm_log(LOG_ERROR, 'invalid Lua function ' // trim(app%proc) // '()', error=rc)
                    exit lua_block
                end if

                ! Write derived type to Lua stack.
                rc = dm_lua_from(lua, obs_in)

                if (dm_is_error(rc)) then
                    call dm_lua_pop(lua)
                    call dm_log(LOG_ERROR, 'failed to write observation to Lua stack', observ=obs_in, error=rc)
                    exit lua_block
                end if

                ! Call the Lua function.
                rc = dm_lua_call(lua, nargs=1, nresults=1)

                if (dm_is_error(rc)) then
                    call dm_lua_pop(lua)
                    call dm_log(LOG_ERROR, 'failed to execute Lua function ' // trim(app%proc) // '()', &
                                observ=obs_in, error=rc)
                    exit lua_block
                end if

                ! Read observation from Lua stack into derived type.
                rc = dm_lua_to(lua, obs_out)

                if (dm_is_error(rc)) then
                    call dm_lua_pop(lua)
                    call dm_log(LOG_ERROR, 'failed to read observation from Lua stack', &
                                error=rc, observ=obs_in)
                    exit lua_block
                end if

                ! Validate returned observation.
                if (.not. dm_observ_valid(obs_out)) then
                    rc = E_INVALID
                    call dm_log(LOG_ERROR, 'invalid observation returned from Lua function ' // &
                                trim(app%proc) // '()', error=rc, observ=obs_in)
                    exit lua_block
                end if
            end block lua_block

            ! Forward observation. On error, send the retrieved observation instead.
            if (dm_is_error(rc)) then
                obs_out = obs_in
            else
                call dm_log(LOG_DEBUG, 'finished observation ' // obs_out%name, observ=obs_out)
            end if

            rc = send_observ(obs_out)
        end do ipc_loop
    end subroutine run

    subroutine register_signal_handlers()
        !! Registers POSIX signal handlers.
        use, intrinsic :: iso_c_binding, only: c_funloc, c_funptr
        use :: unix
        type(c_funptr) :: ptr

        ptr = c_signal(SIGINT,  c_funloc(signal_handler))
        ptr = c_signal(SIGQUIT, c_funloc(signal_handler))
        ptr = c_signal(SIGABRT, c_funloc(signal_handler))
        ptr = c_signal(SIGKILL, c_funloc(signal_handler))
        ptr = c_signal(SIGTERM, c_funloc(signal_handler))
    end subroutine register_signal_handlers

    subroutine signal_handler(signum) bind(c)
        !! Default POSIX signal handler of the program.
        use, intrinsic :: iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: signum

        select case (signum)
            case default
                call dm_log(LOG_INFO, 'exit on signal ' // dm_itoa(signum))
                call halt(0)
        end select
    end subroutine signal_handler
end program dmlua
