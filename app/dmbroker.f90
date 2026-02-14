! dmbroker.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! Message broker.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmbroker'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    ! Program parameters.
    character(*), parameter :: DISCO_URL         = 'tcp://127.0.0.1:5100'
    integer,      parameter :: MAX_DISCO_WORKERS = 2

    ! Type declarations.
    type :: app_disco_worker_type
        !! IPC discovery worker.
        type(ipc_async_task_type)     :: task                        = ipc_async_task_type()     !! Async task context.
        type(ipc_disco_response_type) :: responses(IPC_SERVICE_LAST) = ipc_disco_response_type() !! Response definitions.
        type(ipc_mutex_type)          :: mutex                       = ipc_mutex_type()
    end type app_disco_worker_type

    type :: app_disco_type
        !! IPC discovery socket and workers.
        character(IPC_URL_LEN)      :: url                        = DISCO_URL               !! URL of IPC discovery service.
        type(app_disco_worker_type) :: workers(MAX_DISCO_WORKERS) = app_disco_worker_type() !! IPC discovery workers.
        type(ipc_socket_type)       :: socket                     = ipc_socket_type()       !! IPC discovery socket.
    end type app_disco_type

    type :: app_broker_type
        !! IPC context container.
        type(ipc_trigger_type) :: should_stop = ipc_trigger_type() !! Trigger for shutdown.
        type(app_disco_type)   :: disco       = app_disco_type()   !! IPC discovery context.
    end type app_broker_type

    type :: app_type
        !! Application settings.
        character(ID_LEN)        :: name    = APP_NAME !! Name of database instance and POSIX semaphore.
        character(FILE_PATH_LEN) :: config  = ' '      !! Path to configuration file.
        character(NODE_ID_LEN)   :: node_id = ' '      !! Node id.
        logical                  :: verbose = .false.  !! Print debug messages to stderr.
    end type app_type

    ! Global variables.
    integer                        :: rc     ! Return code.
    type(app_type)                 :: app    ! App settings.
    type(app_broker_type), target  :: broker ! Broker.
    class(logger_class),   pointer :: logger ! Logger object.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%name,    & ! Name of logger process.
                          node_id = app%node_id, & ! Node id.
                          source  = app%name,    & ! Log source.
                          debug   = .false.,     & ! Forward debug messages via IPC.
                          ipc     = .false.,     & ! Enable IPC (if logger is set).
                          verbose = app%verbose)   ! Print logs to standard error.
    call logger%status('started ' // APP_NAME)

    rc = init(broker)
    if (dm_is_ok(rc)) call run(broker)
    call shutdown(broker, rc)
contains
    integer function init(broker) result(rc)
        !! Initialises the broker.
        type(app_broker_type), intent(inout) :: broker !! Broker.

        ! Initialise NNG.
        rc = dm_ipc_init()

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize NNG')
            return
        end if

        ! Create the shutdown trigger.
        rc = dm_ipc_trigger_create(broker%should_stop, .false.)

        if (dm_is_error(rc)) then
            call logger%error('failed to create IPC trigger')
            return
        end if

        ! Initialise the disco workers.
        rc = init_disco(broker%disco%socket, broker%disco%workers, broker%disco%url)

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize IPC discovery service')
            return
        end if

        ! Register signal handler callback.
        call dm_posix_signal_register(signal_callback)
    end function init

    integer function init_disco(socket, workers, url) result(rc)
        !! Initialises the IPC discovery workers.
        type(ipc_socket_type),               intent(inout) :: socket     !! IPC disco socket.
        type(app_disco_worker_type), target, intent(inout) :: workers(:) !! IPC disco workers.
        character(*),                        intent(in)    :: url        !! IPC disco URL.

        integer :: i, n

        ! Open socket.
        rc = dm_ipc_open_reply(socket)
        if (dm_is_error(rc)) return

        call logger%debug('opened IPC discovery socket')
        n = size(workers)

        ! Create worker contexts.
        do i = 1, n
            associate (task => workers(i)%task)
                call dm_ipc_async_set_id(task, i)

                rc = dm_ipc_mutex_create(workers(i)%mutex)
                if (dm_is_error(rc)) return

                rc = dm_ipc_async_init(task, disco_callback)
                if (dm_is_error(rc)) return

                rc = dm_ipc_context_open(task%context, socket)
                if (dm_is_error(rc)) return
            end associate
        end do

        call logger%debug('initialized IPC discovery contexts')

        ! Listen on socket.
        rc = dm_ipc_listen(socket, url)
        if (dm_is_error(rc)) return

        call logger%debug('IPC discovery service is listening on socket ' // url)

        ! Start the workers.
        do i = 1, n
            call disco_callback(c_loc(workers(i)))
        end do

        call logger%debug('started IPC discovery tasks')
    end function init_disco

    recursive subroutine disco_callback(client_data) bind(c)
        !! Async callback routine for single IPC disco task.
        type(c_ptr), intent(in), value :: client_data !! IPC disco worker passed as C pointer.

        integer                              :: rc, rc2, old
        type(app_disco_worker_type), pointer :: worker
        type(ipc_disco_request_type)         :: request
        type(ipc_disco_response_type)        :: response
        type(ipc_message_header_type)        :: header

        if (.not. c_associated(client_data)) return
        call c_f_pointer(client_data, worker)

        call dm_ipc_mutex_lock(worker%mutex)

        associate (async => worker%task%async, message => worker%task%message, &
                   task => worker%task, state => worker%task%state)
            old = state
            state_select: select case (state)
                case (IPC_ASYNC_TASK_STATE_INIT)
                    call dm_ipc_async_receive(task)
                    state = IPC_ASYNC_TASK_STATE_RECV

                case (IPC_ASYNC_TASK_STATE_RECV)
                    rc = dm_ipc_async_result(task)
                    call dm_ipc_async_sleep(async, 0)
                    call dm_ipc_async_get_message(task, error=rc2)

                    if (dm_is_error(rc) .or. dm_is_error(rc2)) then
                        print *, '------>', task%id, rc2, dm_error_message(rc2)
                        call dm_error_out(rc, 'IPC_ASYNC_TASK_STATE_RECV', extra=.true.)
                        call dm_ipc_message_destroy(message)
                        state = IPC_ASYNC_TASK_STATE_INIT
                        exit state_select
                    end if

                    state = IPC_ASYNC_TASK_STATE_WORK

                case (IPC_ASYNC_TASK_STATE_WORK)
                    header = message%header
                    rc = dm_ipc_disco_from_message(request, message)

                    if (dm_is_error(rc)) then
                        print *, '------>', task%id, rc
                        call dm_error_out(rc, 'IPC_ASYNC_TASK_STATE_WORK', extra=.true.)
                        call dm_ipc_message_destroy(message)
                        state = IPC_ASYNC_TASK_STATE_INIT
                        exit state_select
                    end if

                    call disco_response(response, request, worker%responses)
                    !print *, task%id, response%status
                    call dm_error_out(rc, 'IPC_ASYNC_TASK_STATE_WORK', extra=.true.)
                    message%context = c_null_ptr
                    rc = dm_ipc_disco_reply(message, header, response, error=rc)

                    if (dm_is_error(rc)) then
                        print *, '------>', task%id, rc
                        call dm_error_out(rc, 'IPC_ASYNC_TASK_STATE_WORK', extra=.true.)
                        call dm_ipc_message_destroy(message)
                        state = IPC_ASYNC_TASK_STATE_INIT
                        exit state_select
                    end if

                    call dm_ipc_async_set_message(task)
                    if (.not. c_associated(message%context)) call dm_error_out(E_NULL, 'dm_ipc_async_set_message()')
                    call dm_ipc_async_send(task)
                    state = IPC_ASYNC_TASK_STATE_SEND

                case (IPC_ASYNC_TASK_STATE_SEND)
                    rc = dm_ipc_async_result(task)
                    call dm_ipc_async_sleep(async, 0)
                    if (dm_is_error(rc)) then
                        print *, '------>', task%id, rc
                        call dm_error_out(rc, 'IPC_ASYNC_TASK_STATE_SEND', extra=.true.)
                        call dm_ipc_message_destroy(message)
                    end if
                    state = IPC_ASYNC_TASK_STATE_INIT

                case default
                    state = IPC_ASYNC_TASK_STATE_INIT
            end select state_select
            print '("[TASK ", i0, "] ", i0, " -> ", i0)', task%id, old, state
        end associate

        call dm_ipc_mutex_unlock(worker%mutex)
    end subroutine disco_callback

    subroutine disco_response(response, request, responses)
        !! Returns matching disco response to disco request from given responses
        !! array.
        type(ipc_disco_response_type), intent(out)   :: response     !! IPC disco response.
        type(ipc_disco_request_type),  intent(in)    :: request      !! IPC disco request.
        type(ipc_disco_response_type), intent(inout) :: responses(:) !! IPC disco responses.

        ! Prepare disco response.
        response = ipc_disco_response_type(service=request%service, transport=request%transport, status=IPC_STATUS_UNKNOWN)

        associate (i => request%service)
            ! No service requested.
            if (i == IPC_SERVICE_NONE) return

            ! Service is not supported.
            response%status = IPC_STATUS_UNAVAILABLE
            if (i > size(responses)) return
            if (i < IPC_SERVICE_RPC_BLOB .or. i > IPC_SERVICE_RPC_OBSERV) return

            ! Transport is not supported.
            if (request%transport /= IPC_TRANSPORT_ANY .and. &
                request%transport /= responses(i)%protocol) return

            ! Set service attributes.
            response = responses(i)
        end associate
    end subroutine disco_response

    subroutine run(broker)
        type(app_broker_type), intent(inout) :: broker

        call logger%debug('started event loop')

        do while (.not. dm_ipc_trigger_get(broker%should_stop))
            call dm_posix_msleep(1000)
        end do
    end subroutine run

    subroutine shutdown(broker, error)
        !! Stops program.
        type(app_broker_type), intent(inout) :: broker !! Broker.
        integer,               intent(in)    :: error  !! DMPACK error code.

        integer :: stat

        stat = merge(STOP_FAILURE, STOP_SUCCESS, dm_is_error(error))

        call dm_ipc_trigger_destroy(broker%should_stop)
        call logger%debug('destroyed IPC trigger')

        associate (disco => broker%disco, workers => broker%disco%workers)
            call dm_ipc_async_destroy(workers%task)
            call logger%debug('destroyed IPC discovery tasks')

            call dm_posix_msleep(500)

            call dm_ipc_close(workers%task%context)
            call logger%debug('closed IPC discovery socket contexts')

            call dm_posix_msleep(500)

            call dm_ipc_close(disco%socket)
            call logger%debug('closed IPC discovery socket')

            call dm_ipc_mutex_destroy(workers%mutex)
            call logger%debug('destroyed IPC mutexes')
        end associate

        call dm_ipc_shutdown()

        call logger%status('stopped ' // APP_NAME, error=error)
        call dm_stop(stat)
    end subroutine shutdown

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app !! App type.

        type(arg_parser_class) :: parser

        call parser%add('name',    short='n', type=ARG_TYPE_ID)      ! -n, --name <id>
        call parser%add('config',  short='c', type=ARG_TYPE_FILE)    ! -c, --config <path>
        call parser%add('node',    short='N', type=ARG_TYPE_ID)      ! -N, --node <id>
        call parser%add('verbose', short='V', type=ARG_TYPE_LOGICAL) ! -V, --verbose

        ! Read all command-line arguments.
        rc = parser%read(version_callback)
        if (dm_is_error(rc)) return

        call parser%get('name',   app%name)
        call parser%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call parser%get('node',    app%node_id)
        call parser%get('verbose', app%verbose)

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('node',    app%node_id)
            call config%get('verbose', app%verbose)
        end if

        call config%close()
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! MISC. CALLBACKS.
    ! **************************************************************************
    subroutine signal_callback(signum) bind(c)
        !! C-interoperable signal handler that stops the program.
        integer(c_int), intent(in), value :: signum !! Signal number.

        call logger%status('exit on signal ' // dm_posix_signal_name(signum))
        call dm_ipc_trigger_set(broker%should_stop, .true.)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 1x, a)', dm_lua_version(.true.), dm_ipc_version(.true.)
    end subroutine version_callback
end program main
