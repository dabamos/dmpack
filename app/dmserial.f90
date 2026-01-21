! dmserial.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmserial
    !! Reads observations from serial port (TTY/PTY).
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmserial'
    integer,      parameter :: APP_MAJOR = 0
    integer,      parameter :: APP_MINOR = 9
    integer,      parameter :: APP_PATCH = 9

    character, parameter :: APP_CSV_SEPARATOR = ','    !! CSV field separator.
    logical,   parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    integer, parameter :: OUTPUT_NONE   = 0 !! No output.
    integer, parameter :: OUTPUT_STDOUT = 1 !! Output to standard output.
    integer, parameter :: OUTPUT_FILE   = 2 !! Output to file.

    type :: app_type
        !! Application settings.
        character(ID_LEN)              :: name        = APP_NAME    !! Instance and configuration name (required).
        character(FILE_PATH_LEN)       :: config      = ' '         !! Path to configuration file (required).
        character(LOGGER_NAME_LEN)     :: logger      = ' '         !! Name of logger.
        character(NODE_ID_LEN)         :: node_id     = ' '         !! Node id (required).
        character(SENSOR_ID_LEN)       :: sensor_id   = ' '         !! Sensor id (required).
        character(FILE_PATH_LEN)       :: output      = ' '         !! Path of output file.
        integer                        :: output_type = OUTPUT_NONE !! Output type.
        character(FORMAT_NAME_LEN)     :: format_name = ' '         !! Output format name.
        integer                        :: format      = FORMAT_NONE !! Output format.
        character(FILE_PATH_LEN)       :: path        = ' '         !! Path of TTY/PTY device (required).
        integer                        :: baud_rate   = 9600        !! Baud rate (required).
        integer                        :: byte_size   = 8           !! Byte size (required).
        character(TTY_PARITY_NAME_LEN) :: parity      = 'none'      !! Parity name (required).
        integer                        :: stop_bits   = 1           !! Stop bits (required).
        integer                        :: timeout     = 0           !! Timeout in seconds.
        logical                        :: dtr         = .false.     !! DTR flag.
        logical                        :: rts         = .false.     !! RTS flag.
        logical                        :: debug       = .false.     !! Forward debug messages via IPC.
        logical                        :: verbose     = .false.     !! Print debug messages to stderr.
        type(job_list_type)            :: jobs                      !! Job list.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.
    type(tty_type) :: tty ! TTY/PTY type.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments and read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Create TTY type.
    rc = create_tty(tty       = tty,           &
                    path      = app%path,      &
                    baud_rate = app%baud_rate, &
                    byte_size = app%byte_size, &
                    parity    = app%parity,    &
                    stop_bits = app%stop_bits, &
                    dtr       = app%dtr,       &
                    rts       = app%rts)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,  &
                          node_id = app%node_id, &
                          source  = app%name,    &
                          debug   = app%debug,   &
                          ipc     = .true.,      &
                          verbose = app%verbose)
    call logger%info('started ' // APP_NAME)

    call dm_signal_register(signal_callback)
    rc = run(app, tty)

    call logger%info('stopped' // APP_NAME, error=rc)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function create_tty(tty, path, baud_rate, byte_size, parity, stop_bits, dtr, rts) result(rc)
        !! Creates TTY type from application settings.
        type(tty_type), intent(out) :: tty       !! TTY type.
        character(*),   intent(in)  :: path      !! Device path.
        integer,        intent(in)  :: baud_rate !! Numeric baud rate.
        integer,        intent(in)  :: byte_size !! Numeric byte size.
        character(*),   intent(in)  :: parity    !! Parity string.
        integer,        intent(in)  :: stop_bits !! Numeric stop bits.
        logical,        intent(in)  :: dtr       !! DTR enabled.
        logical,        intent(in)  :: rts       !! RTS enabled.

        tty_block: block
            tty%path = path

            tty%baud_rate = dm_tty_baud_rate_from_value(baud_rate, error=rc)
            if (dm_is_error(rc)) exit tty_block

            tty%byte_size = dm_tty_byte_size_from_value(byte_size, error=rc)
            if (dm_is_error(rc)) exit tty_block

            tty%parity = dm_tty_parity_from_name(parity, error=rc)
            if (dm_is_error(rc)) exit tty_block

            tty%stop_bits = dm_tty_stop_bits_from_value(stop_bits, error=rc)
            if (dm_is_error(rc)) exit tty_block

            tty%dtr = dtr
            tty%rts = rts
        end block tty_block

        if (dm_is_error(rc)) call dm_error_out(rc, 'invalid TTY parameters')
    end function create_tty

    integer function output_observ(observ, type) result(rc)
        !! Outputs observation to file if `type` is `OUTPUT_FILE`, or to
        !! _stdout_ if `OUTPUT_STDOUT`.
        type(observ_type), intent(inout) :: observ !! Observation type.
        integer,           intent(in)    :: type   !! Output I/O type.

        integer :: stat, unit

        rc = E_NONE

        select case (type)
            case (OUTPUT_NONE)
                ! No output.
                return

            case (OUTPUT_STDOUT)
                ! Output to standard output.
                rc = write_observ(observ, unit=stdout, format=app%format)

                if (dm_is_error(rc)) then
                    call logger%error('failed to write observ', observ=observ, error=rc)
                    return
                end if

            case (OUTPUT_FILE)
                ! Output to file.
                rc = E_IO

                open (action='write', file=trim(app%output), iostat=stat, newunit=unit, position='append', status='unknown')

                if (stat /= 0) then
                    call logger%error('failed to open file ' // app%output, observ=observ, error=rc)
                    return
                end if

                ! Output in CSV or JSON Lines format.
                rc = write_observ(observ, unit=unit, format=app%format)
                if (dm_is_error(rc)) call logger%error('failed to write observ to file ' // app%output, observ=observ, error=rc)

                close (unit)
        end select
    end function output_observ

    integer function read_observ(tty, observ, node_id, sensor_id, source, debug) result(rc)
        !! Sends requests sequentially to sensor and reads responses.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the observation contains no requests.
        !!
        type(tty_type),    intent(inout) :: tty       !! TTY type.
        type(observ_type), intent(inout) :: observ    !! Observation to read.
        character(*),      intent(in)    :: node_id   !! Node id of observation.
        character(*),      intent(in)    :: sensor_id !! Sensor id of observation.
        character(*),      intent(in)    :: source    !! Source of observation.
        logical,           intent(in)    :: debug     !! Output debug messages.

        integer :: msec, sec
        integer :: i, n

        rc = E_EMPTY

        ! Initialise observation.
        call dm_observ_set(observ    = observ,        &
                           id        = dm_uuid4(),    &
                           node_id   = node_id,       &
                           sensor_id = sensor_id,     &
                           source    = source,        &
                           timestamp = dm_time_now(), &
                           device    = trim(tty%path))
        n = observ%nrequests

        if (n == 0) then
            call logger%info('no requests in observ ' // observ%name, observ=observ)
            return
        end if

        ! Send requests sequentially to sensor.
        request_loop: do i = 1, n
            associate (request => observ%requests(i))
                if (debug) call logger%debug('started ' // request_name_string(observ, request) // ' (' // dm_itoa(i) // '/' // dm_itoa(n) // ')', observ=observ)

                rc = read_request(tty, observ, request, debug)
                call dm_request_set(request, error=rc)

                if (debug) call logger%debug('finished ' // request_name_string(observ, request), observ=observ)

                ! Wait the set delay time of the request.
                msec = max(0, request%delay)
                sec  = dm_msec_to_sec(msec)

                if (msec == 0) cycle request_loop

                if (i < n) then
                    if (debug) call logger%debug('next ' // request_name_string(observ, observ%requests(i + 1)) // ' in ' // dm_itoa(sec) // ' sec', observ=observ)
                else
                    if (debug) call logger%debug('next observ in ' // dm_itoa(sec) // ' sec', observ=observ)
                end if

                call dm_msleep(msec)
            end associate
        end do request_loop

        rc = E_NONE
    end function read_observ

    integer function read_request(tty, observ, request, debug) result(rc)
        type(tty_type),     intent(inout) :: tty     !! TTY type.
        type(observ_type),  intent(inout) :: observ  !! Observation type.
        type(request_type), intent(inout) :: request !! Request type.
        logical,            intent(in)    :: debug   !! Output debug messages.

        integer :: i

        rc = E_NONE

        ! Return if request is disabled.
        if (request%state == REQUEST_STATE_DISABLED) then
            if (debug) call logger%debug(request_name_string(observ, request) // ' is disabled', observ=observ)
            return
        end if

        ! Prepare request.
        call dm_request_set(request, timestamp=dm_time_now(), raw_response=' ')
        call dm_request_set_response_error(request, E_INCOMPLETE)

        ! Flush buffers.
        rc = dm_tty_flush(tty)
        if (dm_is_error(rc)) call logger%warning('failed to flush buffers', observ=observ, error=rc)

        ! Send request to sensor.
        if (debug) call logger%debug('sending request to TTY ' // trim(tty%path) // ': ' // request%request, observ=observ, escape=.false.)
        rc = dm_tty_write(tty, request)

        if (dm_is_error(rc)) then
            call logger%error('failed to write ' // request_name_string(observ, request) // ' to TTY ' // tty%path, observ=observ, error=rc)
            return
        end if

        ! Ignore sensor response if no delimiter is set.
        if (.not. dm_string_has(request%delimiter)) then
            rc = E_NONE
            if (debug) call logger%debug('no delimiter in ' // request_name_string(observ, request), observ=observ)
            return
        end if

        ! Read sensor response from TTY.
        rc = dm_tty_read(tty, request)

        if (dm_is_error(rc)) then
            call logger%error('failed to read response of ' // request_name_string(observ, request) // ' from TTY ' // tty%path, observ=observ, error=rc)
            return
        end if

        if (debug) call logger%debug('received response from TTY ' // trim(tty%path) // ': ' // request%response, observ=observ, escape=.false.)

        ! Do not extract responses if no pattern is set.
        if (.not. dm_string_has(request%pattern)) then
            rc = E_NONE
            if (debug) call logger%debug('no pattern in ' // request_name_string(observ, request), observ=observ)
            return
        end if

        ! Try to extract the response values if a regex pattern is given.
        rc = dm_regex_request(request)

        if (dm_is_error(rc)) then
            call logger%warning('response of ' // request_name_string(observ, request) // ' does not match pattern', observ=observ, error=rc)
            return
        end if

        ! Check response groups for errors.
        do i = 1, request%nresponses
            associate (response => request%responses(i))
                if (dm_is_error(response%error)) call logger%warning('failed to extract response ' // trim(response%name) // ' of ' // request_name_string(observ, request), observ=observ, error=response%error)
            end associate
        end do
    end function read_request

    function request_name_string(observ, request) result(string)
        !! Returns string of observation and request name for logging.
        type(observ_type),  intent(inout) :: observ  !! Observation type.
        type(request_type), intent(inout) :: request !! Request type.
        character(:), allocatable         :: string  !! Result.

        string = 'request ' // trim(request%name) // ' of observ ' // trim(observ%name)
    end function request_name_string

    integer function run(app, tty) result(rc)
        !! Performs jobs in job list.
        type(app_type), intent(inout) :: app !! App type.
        type(tty_type), intent(inout) :: tty !! TTY type.

        integer        :: msec, sec
        integer        :: njobs
        logical        :: debug
        type(job_type) :: job

        debug = (app%debug .or. app%verbose)

        ! Try to open TTY/PTY.
        do
            rc = dm_tty_open(tty)
            if (dm_is_ok(rc)) exit

            call logger%error('failed to open TTY ' // trim(app%path) // ', next attempt in 30 sec', error=rc)
            call dm_sleep(30) ! Wait grace period.
        end do

        call logger%debug('opened TTY ' // trim(app%path) // ' to sensor ' // trim(app%sensor_id) // &
                          ' (' // dm_itoa(tty%baud_rate) // ' ' // dm_itoa(app%byte_size) // &
                          dm_to_upper(app%parity(1:1)) // dm_itoa(app%stop_bits) // ')')

        ! Run until no jobs are left.
        job_loop: do
            ! Get number of jobs left.
            njobs = dm_job_list_count(app%jobs)

            if (njobs == 0) then
                rc = E_NONE
                if (debug) call logger%debug('no jobs left')
                exit job_loop
            end if

            if (debug) call logger%debug(dm_itoa(njobs) // dm_btoa((njobs == 1), ' job', ' jobs') // ' left in job queue')

            ! Get next job as deep copy.
            rc = dm_job_list_next(app%jobs, job)

            if (dm_is_error(rc)) then
                call logger%error('failed to fetch next job', error=rc)
                cycle job_loop
            end if

            observ_block: associate (observ => job%observ)
                if (.not. job%valid) exit observ_block
                if (debug) call logger%debug('started observ ' // trim(observ%name) // ' for sensor ' // app%sensor_id, observ=observ)

                ! Read observation from TTY.
                rc = read_observ(tty, observ, app%node_id, app%sensor_id, app%name, debug)
                call dm_observ_set(observ, error=rc)

                ! Forward observation via POSIX message queue.
                rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)

                ! Output observation.
                rc = output_observ(observ, app%output_type)

                if (debug) call logger%debug('finished observ ' // trim(observ%name) // ' for sensor ' // app%sensor_id, observ=observ)
            end associate observ_block

            ! Wait the set delay time of the job (absolute).
            msec = max(0, job%delay)
            sec  = dm_msec_to_sec(msec)

            if (msec == 0) cycle job_loop
            if (debug) call logger%debug('next job in ' // dm_itoa(sec) // ' sec')

            call dm_msleep(msec)
        end do job_loop

        if (dm_tty_is_connected(tty)) then
            call dm_tty_close(tty)
            call logger%debug('closed TTY ' // app%path)
        end if

        rc = E_NONE
    end function run

    integer function write_observ(observ, unit, format) result(rc)
        !! Writes observation to file unit, in CSV or JSON Lines format.
        type(observ_type), intent(inout) :: observ !! Observation type.
        integer,           intent(in)    :: unit   !! File unit.
        integer,           intent(in)    :: format !! Format enumerator (`FORMAT_*`).

        select case (format)
            case (FORMAT_CSV);   rc = dm_csv_write (observ, unit=unit, header=.false., separator=APP_CSV_SEPARATOR)
            case (FORMAT_JSONL); rc = dm_json_write(observ, unit=unit)
            case default;        rc = E_INVALID
        end select
    end function write_observ

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app

        type(arg_class) :: arg

        call arg%add('name',     short='n', type=ARG_TYPE_ID)      ! -n, --name <string>
        call arg%add('config',   short='c', type=ARG_TYPE_FILE, required=.true.) ! -c, --config <path>
        call arg%add('logger',   short='l', type=ARG_TYPE_ID)      ! -l, --logger <string>
        call arg%add('node',     short='N', type=ARG_TYPE_ID)      ! -N, --node <string>
        call arg%add('sensor',   short='S', type=ARG_TYPE_ID)      ! -S, --sensor <string>
        call arg%add('output',   short='o', type=ARG_TYPE_FILE)    ! -o, --output <path>
        call arg%add('format',   short='f', type=ARG_TYPE_STRING)  ! -f, --format <string>
        call arg%add('path',     short='p', type=ARG_TYPE_STRING)  ! -p, --path <string>
        call arg%add('baudrate', short='B', type=ARG_TYPE_INTEGER) ! -B, --baudrate <n>
        call arg%add('bytesize', short='Z', type=ARG_TYPE_INTEGER) ! -Z, --bytesize <n>
        call arg%add('parity',   short='P', type=ARG_TYPE_STRING)  ! -P, --parity <string>
        call arg%add('stopbits', short='O', type=ARG_TYPE_INTEGER) ! -O, --stopbits <n>
        call arg%add('timeout',  short='T', type=ARG_TYPE_INTEGER) ! -T, --timeout <n>
        call arg%add('dtr',      short='Q', type=ARG_TYPE_LOGICAL) ! -Q, --dtr
        call arg%add('rts',      short='R', type=ARG_TYPE_LOGICAL) ! -R, --rts
        call arg%add('debug',    short='D', type=ARG_TYPE_LOGICAL) ! -D, --debug
        call arg%add('verbose',  short='V', type=ARG_TYPE_LOGICAL) ! -V, --verbose

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('name',   app%name)
        call arg%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call arg%get('logger',   app%logger)
        call arg%get('node',     app%node_id)
        call arg%get('sensor',   app%sensor_id)
        call arg%get('output',   app%output)
        call arg%get('format',   app%format_name)
        call arg%get('path',     app%path)
        call arg%get('baudrate', app%baud_rate)
        call arg%get('bytesize', app%byte_size)
        call arg%get('parity',   app%parity)
        call arg%get('stopbits', app%stop_bits)
        call arg%get('timeout',  app%timeout)
        call arg%get('dtr',      app%dtr)
        call arg%get('rts',      app%rts)
        call arg%get('debug',    app%debug)
        call arg%get('verbose',  app%verbose)

        if (dm_string_has(app%output)) then
            app%format = dm_format_from_name(app%format_name)

            if (app%output == '-') then
                app%output_type = OUTPUT_STDOUT
            else
                app%output_type = OUTPUT_FILE
            end if
        end if

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_INVALID
        if (.not. dm_string_has(app%config)) return ! Fail-safe, should never occur.

        ! Enable Leica GeoCOM API in configuration file.
        rc = config%open(app%config, app%name, geocom=.true.)

        if (dm_is_ok(rc)) then
            call config%get('logger',   app%logger)
            call config%get('node',     app%node_id)
            call config%get('sensor',   app%sensor_id)
            call config%get('path',     app%path)
            call config%get('baudrate', app%baud_rate)
            call config%get('bytesize', app%byte_size)
            call config%get('parity',   app%parity)
            call config%get('stopbits', app%stop_bits)
            call config%get('timeout',  app%timeout)
            call config%get('dtr',      app%dtr)
            call config%get('rts',      app%rts)
            call config%get('output',   app%output)
            call config%get('format',   app%format_name)
            call config%get('debug',    app%debug)
            call config%get('verbose',  app%verbose)
            call config%get('jobs',     app%jobs)
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

        if (.not. dm_id_is_valid(app%sensor_id)) then
            call dm_error_out(rc, 'invalid or missing sensor id')
            return
        end if

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger')
            return
        end if

        if (dm_string_has(app%output) .and. (app%format /= FORMAT_CSV .and. app%format /= FORMAT_JSONL)) then
            call dm_error_out(rc, 'invalid or missing output format')
            return
        end if

        ! TTY options.
        if (dm_tty_baud_rate_from_value(app%baud_rate) == 0) then
            call dm_error_out(rc, 'invalid baud rate')
            return
        end if

        if (dm_tty_byte_size_from_value(app%byte_size) == 0) then
            call dm_error_out(rc, 'invalid byte size')
            return
        end if

        if (dm_tty_parity_from_name(app%parity) == 0) then
            call dm_error_out(rc, 'invalid parity')
            return
        end if

        if (dm_tty_stop_bits_from_value(app%stop_bits) == 0) then
            call dm_error_out(rc, 'invalid stop bits')
            return
        end if

        if (.not. dm_tty_timeout_is_valid(app%timeout)) then
            call dm_error_out(rc, 'invalid timeout')
            return
        end if

        rc = E_NOT_FOUND

        if (.not. dm_file_exists(app%path)) then
            call dm_error_out(rc, 'TTY ' // trim(app%path) // ' does not exist')
            return
        end if

        ! Observation jobs.
        rc = E_EMPTY

        if (dm_job_list_count(app%jobs) == 0) then
            call dm_error_out(rc, 'no enabled jobs')
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

        if (dm_tty_is_connected(tty)) then
            call dm_tty_close(tty)
            call logger%debug('closed TTY ' // tty%path)
        end if

        call logger%info('stopped' // APP_NAME)
        call dm_stop(STOP_SUCCESS)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmserial
