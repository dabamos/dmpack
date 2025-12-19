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

    character(*), parameter :: APP_NAME  = 'dmlua'
    integer,      parameter :: APP_MAJOR = 0
    integer,      parameter :: APP_MINOR = 9
    integer,      parameter :: APP_PATCH = 9

    integer, parameter :: APP_PROCEDURE_LEN = 32     !! Max. length of Lua function name.
    logical, parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Global application settings.
        character(ID_LEN)            :: name      = APP_NAME  !! Instance and configuration name (required).
        character(FILE_PATH_LEN)     :: config    = ' '       !! Path to configuration file (required).
        character(LOGGER_NAME_LEN)   :: logger    = ' '       !! Name of logger.
        character(NODE_ID_LEN)       :: node_id   = ' '       !! Node id (required).
        character(APP_PROCEDURE_LEN) :: procedure = 'process' !! Name of Lua function (required).
        character(FILE_PATH_LEN)     :: script    = ' '       !! Path to Lua script file (required).
        logical                      :: debug     = .false.   !! Forward debug messages via IPC.
        logical                      :: verbose   = .false.   !! Print debug messages to stderr.
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
    call logger%configure(name    = app%logger,  & ! Name of logger process.
                          node_id = app%node_id, & ! Node id.
                          source  = app%name,    & ! Log source.
                          debug   = app%debug,   & ! Forward debug messages via IPC.
                          ipc     = .true.,      & ! Enable IPC (if logger is set).
                          verbose = app%verbose)   ! Print logs to standard error.
    call logger%info('started ' // APP_NAME)

    call init(app, lua, mqueue, error=rc)
    if (dm_is_error(rc)) call shutdown(rc)

    call run(app, lua, mqueue)
    call shutdown(E_NONE)
contains
    subroutine shutdown(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error

        integer :: rc, stat

        stat = merge(STOP_FAILURE, STOP_SUCCESS, dm_is_error(error))

        call dm_mqueue_close(mqueue, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to close mqueue /' // app%name, error=rc)

        call dm_mqueue_unlink(mqueue, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to unlink mqueue /' // app%name, error=rc)

        call dm_lua_destroy(lua)

        call logger%info('stopped ' // APP_NAME, error=error)
        call dm_stop(stat)
    end subroutine shutdown

    subroutine init(app, lua, mqueue, error)
        !! Initialises program.
        type(app_type),       intent(inout)         :: app    !! App type.
        type(lua_state_type), intent(out)           :: lua    !! Lua state type.
        type(mqueue_type),    intent(out)           :: mqueue !! POSIX message queue type.
        integer,              intent(out), optional :: error  !! Error code.

        integer :: rc

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

            ! Open and run Lua script once.
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

            ! Register signal handlers.
            call dm_signal_register(signal_callback)
        end block init_block

        if (present(error)) error = rc
    end subroutine init

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

            call logger%debug('passing observ ' // trim(observ_in%name) // ' to Lua function ' // trim(app%procedure) // '()', observ=observ_in)

            ! Pass the observation to the Lua function and read the returned observation.
            lua_block: block
                ! Load Lua function.
                rc = dm_lua_read(lua, trim(app%procedure))

                if (dm_is_error(rc)) then
                    call logger%error('failed to load Lua function ' // trim(app%procedure) // '()', error=rc)
                    exit lua_block
                end if

                if (.not. dm_lua_is_function(lua)) then
                    rc = E_INVALID
                    call logger%error('invalid Lua function ' // trim(app%procedure) // '()', error=rc)
                    exit lua_block
                end if

                ! Write derived type to Lua stack and call the Lua function.
                call dm_lua_from(lua, observ_in)
                rc = dm_lua_call(lua, nargs=1, nresults=1)

                if (dm_is_error(rc)) then
                    call dm_lua_pop(lua)
                    call logger%error('failed to execute Lua function ' // trim(app%procedure) // '()', observ=observ_in, error=rc)
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
                    call logger%error('invalid observ returned from Lua function ' // trim(app%procedure) // '()', error=rc, observ=observ_in)
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
        type(app_type), intent(out) :: app

        type(arg_class) :: arg

        call arg%create()
        call arg%add('name',      short='n', type=ARG_TYPE_ID)      ! -n, --name <id>
        call arg%add('config',    short='c', type=ARG_TYPE_FILE)    ! -c, --config <path>
        call arg%add('logger',    short='l', type=ARG_TYPE_ID)      ! -l, --logger <id>
        call arg%add('node',      short='N', type=ARG_TYPE_ID)      ! -N, --node <id>
        call arg%add('procedure', short='p', type=ARG_TYPE_STRING)  ! -p, --procedure <string>
        call arg%add('script',    short='s', type=ARG_TYPE_FILE)    ! -s, --script <path>
        call arg%add('debug',     short='D', type=ARG_TYPE_LOGICAL) ! -D, --debug
        call arg%add('verbose',   short='V', type=ARG_TYPE_LOGICAL) ! -V, --verbose

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('name',   app%name)
        call arg%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call arg%get('logger',    app%logger)
        call arg%get('node',      app%node_id)
        call arg%get('procedure', app%procedure)
        call arg%get('script',    app%script)
        call arg%get('debug',     app%debug)
        call arg%get('verbose',   app%verbose)
        call arg%destroy()

        ! Validate options.
        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('logger',    app%logger)
            call config%get('node',      app%node_id)
            call config%get('procedure', app%procedure)
            call config%get('script',    app%script)
            call config%get('debug',     app%debug)
            call config%get('verbose',   app%verbose)
        end if

        call config%close()
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        character(*), parameter :: PROCEDURE_SET = &
            '-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz' ! Valid procedure name characters.

        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger')
            return
        end if

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (verify(trim(app%procedure), PROCEDURE_SET) > 0) then
            call dm_error_out(rc, 'invalid Lua function name ' // app%procedure)
            return
        end if

        rc = E_NOT_FOUND

        if (.not. dm_file_exists(app%script)) then
            call dm_error_out(rc, 'Lua script ' // trim(app%script) // ' not found')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine signal_callback(signum) bind(c)
        !! Default POSIX signal handler of the program.
        integer(c_int), intent(in), value :: signum

        call logger%debug('exit on on signal ' // dm_signal_name(signum))
        call shutdown(E_NONE)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmlua
