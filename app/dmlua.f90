! dmlua.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmlua
    !! Lua script spawner that reads an observation from POSIX message queue,
    !! passes it to a configured Lua script, and forwards the returned
    !! observation to the next specified receiver.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmlua'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 7

    integer, parameter :: APP_PROC_LEN    = 32     !! Max. length of Lua function name.
    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Global application settings.
        character(len=ID_LEN)          :: name    = APP_NAME  !! Instance and configuration name (required).
        character(len=FILE_PATH_LEN)   :: config  = ' '       !! Path to configuration file (required).
        character(len=LOGGER_NAME_LEN) :: logger  = ' '       !! Name of logger.
        character(len=NODE_ID_LEN)     :: node_id = ' '       !! Node id (required).
        character(len=APP_PROC_LEN)    :: proc    = 'process' !! Name of Lua function (required).
        character(len=FILE_PATH_LEN)   :: script  = ' '       !! Path to Lua script file (required).
        logical                        :: debug   = .false.   !! Forward debug messages via IPC.
        logical                        :: verbose = .false.   !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer              :: rc     ! Return code.
    type(app_type)       :: app    ! App configuration.
    type(lua_state_type) :: lua    ! Lua interpreter.
    type(mqueue_type)    :: mqueue ! Message queue.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,                 & ! Name of logger process.
                          node_id = app%node_id,                & ! Node id.
                          source  = app%name,                   & ! Log source.
                          debug   = app%debug,                  & ! Forward debug messages via IPC.
                          ipc     = (len_trim(app%logger) > 0), & ! Enable IPC.
                          verbose = app%verbose)                  ! Print logs to standard error.

    init_block: block
        ! Initialise Lua interpreter.
        rc = dm_lua_init(lua)

        if (dm_is_error(rc)) then
            call logger%error('failed to init Lua interpreter', error=rc)
            exit init_block
        end if

        ! Register DMPACK API for Lua.
        rc = dm_lua_api_register(lua)

        if (dm_is_error(rc)) then
            call logger%error('failed to register Lua API', error=rc)
            exit init_block
        end if

        ! Register GeoCOM API for Lua.
        rc = dm_lua_geocom_register(lua, procedures=.false., errors=.true.)

        if (dm_is_error(rc)) then
            call logger%error('failed to register GeoCOM API', error=rc)
            exit init_block
        end if

        ! Open and run Lua script.
        rc = dm_lua_open(lua, app%script, eval=.true.)

        if (dm_is_error(rc)) then
            call logger%error('failed to load Lua script ' // app%script, error=rc)
            exit init_block
        end if

        ! Open observation message queue for reading.
        rc = dm_mqueue_open(mqueue = mqueue,      & ! Message queue type.
                            type   = TYPE_OBSERV, & ! Observation type.
                            name   = app%name,    & ! Name of message queue.
                            access = MQUEUE_RDONLY) ! Read-only access.

        if (dm_is_error(rc)) then
            call logger%error('failed to open mqueue /' // trim(app%name) // ': ' // dm_system_error_message(), error=rc)
            exit init_block
        end if

        ! Register signal handlers and run the IPC loop.
        call dm_signal_register(signal_callback)
        call run(app, lua, mqueue)
    end block init_block

    ! Clean up and exit.
    call halt(rc)
contains
    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error

        integer :: rc, stat

        stat = dm_btoi(dm_is_error(error), STOP_FAILURE, STOP_SUCCESS)

        call dm_mqueue_close(mqueue, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to close mqueue /' // app%name, error=rc)

        call dm_mqueue_unlink(mqueue, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to unlink mqueue /' // app%name, error=rc)

        call dm_lua_destroy(lua)
        call dm_stop(stat)
    end subroutine halt

    subroutine run(app, lua, mqueue)
        !! Waits for incoming observation, passes derived type as table to Lua
        !! function. The Lua function has to return the (modified) observation
        !! on exit.
        !!
        !! Variable `observ_in` stores the observation type received from message
        !! queue. The observation data returned from the Lua function is stored
        !! in `observ_out` and will be forwarded to the next receiver. On error,
        !! the received observation will be forwarded instead.
        type(app_type),       intent(inout) :: app
        type(lua_state_type), intent(inout) :: lua
        type(mqueue_type),    intent(inout) :: mqueue

        integer           :: rc
        type(observ_type) :: observ_in, observ_out

        call logger%info('started ' // APP_NAME)

        ipc_loop: do
            ! Blocking read from POSIX message queue.
            call logger%debug('waiting for observ on mqueue /' // app%name)
            rc = dm_mqueue_read(mqueue, observ_in)

            if (dm_is_error(rc)) then
                call logger%error('failed to read observ from mqueue /' // app%name, error=rc)
                call dm_sleep(1)
                cycle ipc_loop
            end if

            ! Validate observation.
            if (.not. dm_observ_is_valid(observ_in)) then
                call logger%error('received invalid observ ' // trim(observ_in%name), observ=observ_in, error=E_INVALID)
                cycle ipc_loop
            end if

            call logger%debug('passing observ ' // trim(observ_in%name) // ' to Lua function ' // trim(app%proc) // '()', observ=observ_in)

            ! Pass the observation to the Lua function and read the returned observation.
            lua_block: block
                ! Load Lua function.
                rc = dm_lua_read(lua, trim(app%proc))

                if (dm_is_error(rc)) then
                    call logger%error('failed to load Lua function ' // trim(app%proc) // '()', error=rc)
                    exit lua_block
                end if

                if (.not. dm_lua_is_function(lua)) then
                    rc = E_INVALID
                    call logger%error('invalid Lua function ' // trim(app%proc) // '()', error=rc)
                    exit lua_block
                end if

                ! Write derived type to Lua stack and call the Lua function.
                call dm_lua_from(lua, observ_in)
                rc = dm_lua_call(lua, nargs=1, nresults=1)

                if (dm_is_error(rc)) then
                    call dm_lua_pop(lua)
                    call logger%error('failed to execute Lua function ' // trim(app%proc) // '()', observ=observ_in, error=rc)
                    exit lua_block
                end if

                ! Read observation from Lua stack into derived type.
                rc = dm_lua_to(lua, observ_out)

                if (dm_is_error(rc)) then
                    call dm_lua_pop(lua)
                    call logger%error('failed to read observ from Lua stack', error=rc, observ=observ_in)
                    exit lua_block
                end if

                ! Validate returned observation.
                if (.not. dm_observ_is_valid(observ_out)) then
                    rc = E_INVALID
                    call logger%error('invalid observ returned from Lua function ' // trim(app%proc) // '()', error=rc, observ=observ_in)
                    exit lua_block
                end if
            end block lua_block

            ! Forward observation. On error, send the original observation instead.
            if (dm_is_error(rc)) then
                call logger%debug('forwarding observ ' // trim(observ_in%name) // ' unmodified', observ=observ_out)
                rc = dm_mqueue_forward(observ_in, name=app%name, blocking=APP_MQ_BLOCKING)
            else
                call logger%debug('forwarding observ ' // observ_out%name, observ=observ_out)
                rc = dm_mqueue_forward(observ_out, name=app%name, blocking=APP_MQ_BLOCKING)
            end if
        end do ipc_loop
    end subroutine run

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        character(len=*), parameter :: PROC_SET = &
            '-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz' ! Valid procedure name characters.

        type(app_type), intent(out) :: app
        type(arg_type)              :: args(8)

        args = [ &
            arg_type('name',      short='n', type=ARG_TYPE_ID),      & ! -n, --name <id>
            arg_type('config',    short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',    short='l', type=ARG_TYPE_ID),      & ! -l, --logger <id>
            arg_type('node',      short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('procedure', short='p', type=ARG_TYPE_STRING),  & ! -p, --procedure <string>
            arg_type('script',    short='s', type=ARG_TYPE_FILE),    & ! -s, --script <path>
            arg_type('debug',     short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('verbose',   short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%name)
        call dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call dm_arg_get(args(3), app%logger)
        call dm_arg_get(args(4), app%node_id)
        call dm_arg_get(args(5), app%proc)
        call dm_arg_get(args(6), app%script)
        call dm_arg_get(args(7), app%debug)
        call dm_arg_get(args(8), app%verbose)

        ! Validate options.
        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (len_trim(app%logger) > 0 .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger')
            return
        end if

        if (.not. dm_id_is_valid(app%node_id)) then
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
            call dm_config_get(config, 'logger',    app%logger)
            call dm_config_get(config, 'node',      app%node_id)
            call dm_config_get(config, 'procedure', app%proc)
            call dm_config_get(config, 'script',    app%script)
            call dm_config_get(config, 'debug',     app%debug)
            call dm_config_get(config, 'verbose',   app%verbose)
        end if

        call dm_config_close(config)
    end function read_config

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine signal_callback(signum) bind(c)
        !! Default POSIX signal handler of the program.
        integer(kind=c_int), intent(in), value :: signum

        select case (signum)
            case default
                call logger%info('exit on signal ' // dm_signal_name(signum))
                call halt(E_NONE)
        end select
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmlua
