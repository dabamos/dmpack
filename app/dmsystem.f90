! dmsystem.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmsystem
    !! System monitor. Sends system status as observation to receiver via POSIX
    !! message queue.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmsystem'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 6

    character(len=*), parameter :: APP_OBSERV_NAME  = 'system_status'
    character(len=*), parameter :: APP_REQUEST_NAME = 'status'

    type :: app_options_type
        !! Enabled responses.
        character(len=FILE_PATH_LEN) :: disk_free  = ' '    !! Disk free path (file or directory).
        character(len=FILE_PATH_LEN) :: log_db     = ' '    !! Path of log database.
        character(len=FILE_PATH_LEN) :: observ_db  = ' '    !! Path of log database.
        logical                      :: cpu_temp   = .true. !! Enable CPU temperature.
        logical                      :: load_avg1  = .true. !! Enable system load average, 1 min.
        logical                      :: load_avg5  = .true. !! Enable system load average, 5 min.
        logical                      :: load_avg15 = .true. !! Enable system load average, 15 min.
        logical                      :: uptime     = .true. !! Enable system uptime.
    end type app_options_type

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)              :: name      = APP_NAME !! Name of instance/configuration.
        character(len=FILE_PATH_LEN)       :: config    = ' '      !! Path to configuration file.
        character(len=LOGGER_NAME_LEN)     :: logger    = ' '      !! Name of logger (name implies IPC).
        character(len=NODE_ID_LEN)         :: node_id   = ' '      !! Node id (required).
        character(len=SENSOR_ID_LEN)       :: sensor_id = ' '      !! Sensor id (required).
        character(len=TARGET_ID_LEN)       :: target_id = ' '      !! Target id (required).
        character(len=OBSERV_RECEIVER_LEN) :: receiver  = ' '      !! Observation receiver.
        integer                            :: count     = 0        !! Maximum number of observations to send (0 means unlimited).
        integer                            :: interval  = 600      !! Emit interval in seconds (>= 0).
        logical                            :: debug     = .false.  !! Forward debug messages via IPC.
        logical                            :: verbose   = .false.  !! Print debug messages to stderr.
        type(app_options_type)             :: options              !! Enabled responses.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    call dm_init()

    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,                 & ! Name of logger process.
                          node_id = app%node_id,                & ! Node id.
                          source  = app%name,                   & ! Log source.
                          debug   = app%debug,                  & ! Forward DEBUG messages via IPC.
                          ipc     = (len_trim(app%logger) > 0), & ! Enable IPC.
                          verbose = app%verbose)                  ! Print logs to standard error.

    call dm_signal_register(signal_callback)

    call run(app, error=rc)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app !! App type.

        type(arg_type) :: args(11)

        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <id>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),      & ! -l, --logger <id>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('sensor',   short='S', type=ARG_TYPE_ID),      & ! -S, --sensor <id>
            arg_type('target',   short='T', type=ARG_TYPE_ID),      & ! -T, --target <id>
            arg_type('receiver', short='r', type=ARG_TYPE_ID),      & ! -r, --receiver <id>
            arg_type('count',    short='C', type=ARG_TYPE_INTEGER), & ! -C, --count <n>
            arg_type('interval', short='I', type=ARG_TYPE_INTEGER), & ! -I, --interval <n>
            arg_type('debug',    short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('verbose',  short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
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
        call dm_arg_get(args( 3), app%logger)
        call dm_arg_get(args( 4), app%node_id)
        call dm_arg_get(args( 5), app%sensor_id)
        call dm_arg_get(args( 6), app%target_id)
        call dm_arg_get(args( 7), app%receiver)
        call dm_arg_get(args( 8), app%count)
        call dm_arg_get(args( 9), app%interval)
        call dm_arg_get(args(10), app%debug)
        call dm_arg_get(args(11), app%verbose)

        ! Validate settings.
        rc = E_INVALID

        if (len_trim(app%logger) > 0 .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger name')
            return
        end if

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (.not. dm_id_is_valid(app%sensor_id)) then
            call dm_error_out(rc, 'invalid or missing sensor id')
            return
        end if

        if (.not. dm_id_is_valid(app%target_id)) then
            call dm_error_out(rc, 'invalid or missing target id')
            return
        end if

        if (len_trim(app%receiver) > 0 .and. .not. dm_id_is_valid(app%receiver)) then
            call dm_error_out(rc, 'invalid receiver')
            return
        end if

        if (app%count < 0) then
            call dm_error_out(rc, 'invalid count')
            return
        end if

        if (app%interval < 0) then
            call dm_error_out(rc, 'invalid interval')
            return
        end if

        rc = E_NOT_FOUND

        if (len_trim(app%options%disk_free) > 0 .and. .not. dm_file_exists(app%options%disk_free)) then
            call dm_error_out(rc, 'disk free path does not exist')
            return
        end if

        if (len_trim(app%options%log_db) > 0 .and. .not. dm_file_exists(app%options%log_db)) then
            call dm_error_out(rc, 'log database does not exist')
            return
        end if

        if (len_trim(app%options%observ_db) > 0 .and. .not. dm_file_exists(app%options%observ_db)) then
            call dm_error_out(rc, 'observation database does not exist')
            return
        end if

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_type) :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'logger',   app%logger)
            call dm_config_get(config, 'node',     app%node_id)
            call dm_config_get(config, 'sensor',   app%sensor_id)
            call dm_config_get(config, 'target',   app%target_id)
            call dm_config_get(config, 'receiver', app%receiver)
            call dm_config_get(config, 'count',    app%count)
            call dm_config_get(config, 'interval', app%interval)
            call dm_config_get(config, 'debug',    app%debug)
            call dm_config_get(config, 'verbose',  app%verbose)

            if (dm_is_ok(dm_config_field(config, 'options'))) then
                call dm_config_get(config, 'cpu_temp',   app%options%cpu_temp)
                call dm_config_get(config, 'disk_free',  app%options%disk_free)
                call dm_config_get(config, 'load_avg1',  app%options%load_avg1)
                call dm_config_get(config, 'load_avg5',  app%options%load_avg5)
                call dm_config_get(config, 'load_avg15', app%options%load_avg15)
                call dm_config_get(config, 'log_db',     app%options%log_db)
                call dm_config_get(config, 'observ_db',  app%options%observ_db)
                call dm_config_get(config, 'uptime',     app%options%uptime)
                call dm_config_remove(config)
            end if
        end if

        call dm_config_close(config)
    end function read_config

    subroutine run(app, error)
        !! Run system monitoring and emit observations.
        type(app_type), intent(inout)         :: app   !! App type.
        integer,        intent(out), optional :: error !! Error code.

        character(len=256) :: file_system, message
        integer            :: capacity, iter, sec
        integer(kind=i8)   :: available, n, seconds
        logical            :: debug
        real               :: avg1, avg5, avg15, cpu_temp

        logical :: has_cpu_temp, has_disk_free, has_log_db, has_observ_db, has_uptime
        logical :: has_load_avg, has_load_avg1, has_load_avg5, has_load_avg15

        type(observ_type)     :: observ
        type(request_type)    :: request
        type(time_delta_type) :: uptime
        type(timer_type)      :: timer

        if (present(error)) error = E_ERROR

        has_cpu_temp   = app%options%cpu_temp
        has_load_avg1  = app%options%load_avg1
        has_load_avg5  = app%options%load_avg5
        has_load_avg15 = app%options%load_avg15
        has_load_avg   = (has_load_avg1 .or. has_load_avg5 .or. has_load_avg15)
        has_uptime     = app%options%uptime

        has_disk_free = (len_trim(app%options%disk_free) > 0)
        has_log_db    = (len_trim(app%options%log_db)    > 0)
        has_observ_db = (len_trim(app%options%observ_db) > 0)

        ! Initialise observation.
        call dm_observ_set(observ    = observ,          &
                           node_id   = app%node_id,     &
                           sensor_id = app%sensor_id,   &
                           target_id = app%target_id,   &
                           name      = APP_OBSERV_NAME, &
                           source    = app%name)
        rc = dm_observ_add_receiver(observ, app%receiver) ! Ignore empty receiver error.

        ! Initialise request.
        call dm_request_set(request, name=APP_REQUEST_NAME)
        rc = dm_observ_add_request(observ, request)
        if (dm_is_error(rc)) return

        call logger%info('started ' // APP_NAME)
        debug = (app%debug .or. app%verbose)

        if (has_cpu_temp)   call logger%debug('CPU temperature monitoring enabled')
        if (has_disk_free)  call logger%debug('disk free monitoring enabled')
        if (has_load_avg1)  call logger%debug('load average 1 monitoring enabled')
        if (has_load_avg5)  call logger%debug('load average 5 monitoring enabled')
        if (has_load_avg15) call logger%debug('load average 15 monitoring enabled')
        if (has_log_db)     call logger%debug('log database monitoring enabled')
        if (has_observ_db)  call logger%debug('observ database monitoring enabled')
        if (has_uptime)     call logger%debug('uptime monitoring enabled')

        emit_loop: do
            call dm_timer_start(timer)

            ! Reset observation and request.
            call dm_observ_set(observ, id=dm_uuid4(), timestamp=dm_time_now())
            request = request_type(name=APP_REQUEST_NAME)

            ! CPU temperature.
            if (has_cpu_temp) then
                rc = dm_system_cpu_temperature(cpu_temp)

                if (dm_is_error(rc)) then
                    call logger%error('failed to read CPU temperature', observ=observ, error=rc)
                else if (debug) then
                    write (message, '("CPU temperature is ", f0.1, " C")') cpu_temp
                    call logger%debug(message, observ=observ)
                end if

                rc = dm_request_add(request, name='cpu_temp', unit='degC', value=cpu_temp)
            end if

            ! Available disk space and disk capacity.
            if (has_disk_free) then
                rc = dm_system_disk_free(app%options%disk_free, file_system=file_system, available=available, capacity=capacity)

                if (dm_is_error(rc)) then
                    call logger%error('failed to read free disk space', observ=observ, error=rc)
                else if (debug) then
                    write (message, '("file system ", a, " has ", a, " available (", i0, " % used)")') trim(file_system), dm_size_to_human(available), capacity
                    call logger%debug(message, observ=observ)
                end if

                rc = dm_request_add(request, name='disk_capacity', unit='%', value=capacity,  error=rc)
                rc = dm_request_add(request, name='disk_free',     unit='B', value=available, error=rc)
            end if

            ! Log database size.
            if (has_log_db) then
                n = dm_file_size(app%options%log_db, error=rc)

                if (dm_is_error(rc)) then
                    call logger%error('failed to read log database size', observ=observ, error=rc)
                else if (debug) then
                    call logger%debug('size of log database ' // trim(app%options%log_db) // ' is ' // dm_size_to_human(n), observ=observ)
                end if

                rc = dm_request_add(request, name='log_db', unit='B', value=n, error=rc)
            end if

            ! Observation database size.
            if (has_observ_db) then
                n = dm_file_size(app%options%observ_db, error=rc)

                if (dm_is_error(rc)) then
                    call logger%error('failed to read observ database size', observ=observ, error=rc)
                else if (debug) then
                    call logger%debug('size of observ database ' // trim(app%options%log_db) // ' is ' // dm_size_to_human(n), observ=observ)
                end if

                rc = dm_request_add(request, name='observ_db', unit='B', value=n, error=rc)
            end if

            ! Load average.
            if (has_load_avg) then
                rc = dm_system_load_average(avg1, avg5, avg15)

                if (dm_is_error(rc)) then
                    call logger%error('failed to read load average', observ=observ, error=rc)
                else if (debug) then
                    write (message, '("load average:", 3(1x, f0.1))') avg1, avg5, avg15
                    call logger%debug(message, observ=observ)
                end if

                if (has_load_avg1)  rc = dm_request_add(request, name='load_avg1',  unit='none', value=avg1,  error=rc)
                if (has_load_avg5)  rc = dm_request_add(request, name='load_avg5',  unit='none', value=avg5,  error=rc)
                if (has_load_avg15) rc = dm_request_add(request, name='load_avg15', unit='none', value=avg15, error=rc)
            end if

            ! Uptime.
            if (has_uptime) then
                call dm_system_uptime(seconds, error=rc)

                if (dm_is_error(rc)) then
                    call logger%error('failed to read uptime', observ=observ, error=rc)
                else if (debug) then
                    call dm_time_delta_from_seconds(uptime, seconds)
                    call logger%debug('uptime: ' // dm_time_delta_to_string(uptime), observ=observ)
                end if

                rc = dm_request_add(request, name='uptime', unit='sec', value=seconds, error=rc)
            end if

            ! Finish and replace request.
            call dm_request_set(request, timestamp=dm_time_now())
            observ%requests(1) = request

            if (app%count > 0) then
                iter = iter + 1
                if (iter >= app%count) exit emit_loop
            end if

            call dm_timer_stop(timer)
            sec = max(0, int(app%interval - dm_timer_result(timer)))
            if (debug) call logger%debug('next observation in ' // dm_itoa(sec) // ' sec')
            call dm_sleep(sec)
        end do emit_loop

        if (debug) call logger%debug('finished system monitoring')
        if (present(error)) error = E_NONE
    end subroutine run

    subroutine signal_callback(signum) bind(c)
        !! C-interoperable signal handler that stops the program.
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        call logger%info('exit on signal ' // dm_signal_name(signum))
        call dm_stop(STOP_SUCCESS)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmsystem
