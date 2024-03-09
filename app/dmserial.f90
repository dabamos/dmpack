! dmserial.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmserial
    !! Reads observations from serial port (TTY/PTY).
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmserial'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 1

    character, parameter :: APP_CSV_SEPARATOR = ','    !! CSV field separator.
    logical,   parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    integer, parameter :: OUTPUT_NONE   = 0 !! No output.
    integer, parameter :: OUTPUT_STDOUT = 1 !! Output to standard output.
    integer, parameter :: OUTPUT_FILE   = 2 !! Output to file.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)              :: name        = APP_NAME    !! Instance and configuration name (required).
        character(len=FILE_PATH_LEN)       :: config      = ' '         !! Path to configuration file (required).
        character(len=LOGGER_NAME_LEN)     :: logger      = ' '         !! Name of logger.
        character(len=NODE_ID_LEN)         :: node        = ' '         !! Node id (required).
        character(len=SENSOR_ID_LEN)       :: sensor      = ' '         !! Sensor id (required).
        character(len=FILE_PATH_LEN)       :: output      = ' '         !! Path of output file.
        integer                            :: output_type = OUTPUT_NONE !! Output type.
        character(len=FORMAT_NAME_LEN)     :: format_name = ' '         !! Output format name.
        integer                            :: format      = FORMAT_NONE !! Output format.
        character(len=FILE_PATH_LEN)       :: tty         = ' '         !! Path of TTY/PTY device (required).
        integer                            :: baud_rate   = 9600        !! Baud rate (required).
        integer                            :: byte_size   = 8           !! Byte size (required).
        character(len=TTY_PARITY_NAME_LEN) :: parity      = 'none'      !! Parity name (required).
        integer                            :: stop_bits   = 1           !! Stop bits (required).
        integer                            :: timeout     = 0           !! Timeout in seconds.
        logical                            :: dtr         = .false.     !! DTR flag.
        logical                            :: rts         = .false.     !! RTS flag.
        logical                            :: debug       = .false.     !! Forward debug messages via IPC.
        logical                            :: verbose     = .false.     !! Print debug messages to stderr.
        type(job_list_type)                :: jobs                      !! Job list.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.
    type(tty_type) :: tty ! TTY/PTY type.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Create TTY type.
    rc = create_tty(tty       = tty, &
                    path      = app%tty, &
                    baud_rate = app%baud_rate, &
                    byte_size = app%byte_size, &
                    parity    = app%parity, &
                    stop_bits = app%stop_bits, &
                    dtr       = app%dtr, &
                    rts       = app%rts)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Initialise logger.
    call dm_logger_init(name    = app%logger, &
                        node_id = app%node, &
                        source  = app%name, &
                        debug   = app%debug, &
                        ipc     = (len_trim(app%logger) > 0), &
                        verbose = app%verbose)

    ! Register signal handler.
    call dm_signal_register(signal_handler)

    ! Run main loop.
    rc = run(app, tty)
    if (dm_is_error(rc)) call dm_stop(1)

    call dm_stop(0)
contains
    integer function create_tty(tty, path, baud_rate, byte_size, parity, stop_bits, dtr, rts) result(rc)
        !! Creates TTY type from application settings.
        type(tty_type),   intent(out) :: tty       !! TTY type.
        character(len=*), intent(in)  :: path      !! Device path.
        integer,          intent(in)  :: baud_rate !! Numeric baud rate.
        integer,          intent(in)  :: byte_size !! Numeric byte size.
        character(len=*), intent(in)  :: parity    !! Parity string.
        integer,          intent(in)  :: stop_bits !! Numeric stop bits.
        logical,          intent(in)  :: dtr       !! DTR enabled.
        logical,          intent(in)  :: rts       !! RTS enabled.

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
        !! Outputs observation to file or _stdout_ if `type` is `OUTPUT_STDOUT`
        !! or `OUTPUT_FILE`.
        type(observ_type), intent(inout) :: observ !! Observation type.
        integer,           intent(in)    :: type   !! Output I/O type.

        integer :: fu, stat

        rc = E_NONE

        select case (type)
            case (OUTPUT_NONE)
                ! No output.
                return

            case (OUTPUT_STDOUT)
                ! Output to standard output.
                rc = write_observ(observ, unit=stdout, format=app%format)

                if (dm_is_error(rc)) then
                    call dm_log_error('failed to output observ', error=rc)
                    return
                end if

            case (OUTPUT_FILE)
                ! Output to file.
                rc = E_IO

                open (action='write', file=trim(app%output), iostat=stat, &
                      newunit=fu, position='append', status='unknown')

                if (stat /= 0) then
                    call dm_log_error('failed to open file ' // app%output, error=rc)
                    return
                end if

                ! Output in CSV or JSON Lines format.
                rc = write_observ(observ, unit=fu, format=app%format)

                if (dm_is_error(rc)) then
                    call dm_log_error('failed to write observ to file ' // app%output, error=rc)
                end if

                close (fu)
        end select
    end function output_observ

    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(inout) :: app
        type(arg_type)                :: args(17)

        rc = E_NONE

        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),       & ! -n, --name <string>
            arg_type('config',   short='c', type=ARG_TYPE_FILE, required=.true.), & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),       & ! -l, --logger <string>
            arg_type('node',     short='N', type=ARG_TYPE_ID),       & ! -N, --node <string>
            arg_type('sensor',   short='S', type=ARG_TYPE_ID),       & ! -S, --sensor <string>
            arg_type('tty',      short='Y', type=ARG_TYPE_CHAR),     & ! -T, --tty <string>
            arg_type('output',   short='o', type=ARG_TYPE_CHAR),     & ! -o, --output <string>
            arg_type('format',   short='f', type=ARG_TYPE_CHAR),     & ! -f, --format <string>
            arg_type('baudrate', short='B', type=ARG_TYPE_INTEGER),  & ! -B, --baudrate <n>
            arg_type('bytesize', short='Z', type=ARG_TYPE_INTEGER),  & ! -Z, --bytesize <n>
            arg_type('parity',   short='P', type=ARG_TYPE_CHAR),     & ! -P, --parity <string>
            arg_type('stopbits', short='O', type=ARG_TYPE_INTEGER),  & ! -O, --stopbits <n>
            arg_type('timeout',  short='T', type=ARG_TYPE_INTEGER),  & ! -T, --timeout <n>
            arg_type('dtr',      short='Q', type=ARG_TYPE_BOOL),     & ! -Q, --dtr
            arg_type('rts',      short='R', type=ARG_TYPE_BOOL),     & ! -R, --rts
            arg_type('debug',    short='D', type=ARG_TYPE_BOOL),     & ! -D, --debug
            arg_type('verbose',  short='V', type=ARG_TYPE_BOOL)      & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        rc = dm_arg_get(args( 3), app%logger)
        rc = dm_arg_get(args( 4), app%node)
        rc = dm_arg_get(args( 5), app%sensor)
        rc = dm_arg_get(args( 6), app%output)
        rc = dm_arg_get(args( 7), app%format_name)
        rc = dm_arg_get(args( 8), app%tty)
        rc = dm_arg_get(args( 9), app%baud_rate)
        rc = dm_arg_get(args(10), app%byte_size)
        rc = dm_arg_get(args(11), app%parity)
        rc = dm_arg_get(args(12), app%stop_bits)
        rc = dm_arg_get(args(13), app%timeout)
        rc = dm_arg_get(args(14), app%dtr)
        rc = dm_arg_get(args(15), app%rts)
        rc = dm_arg_get(args(16), app%debug)
        rc = dm_arg_get(args(17), app%verbose)

        ! Validate options.
        rc = E_INVALID

        if (.not. dm_id_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (.not. dm_id_valid(app%node)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (.not. dm_id_valid(app%sensor)) then
            call dm_error_out(rc, 'invalid or missing sensor id')
            return
        end if

        if (len_trim(app%logger) > 0 .and. .not. dm_id_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger')
            return
        end if

        if (len_trim(app%output) > 0) then
            app%format = dm_format_from_name(app%format_name)

            select case (app%format)
                case (FORMAT_CSV, FORMAT_JSONL)
                    continue
                case default
                    call dm_error_out(rc, 'invalid or missing output format')
                    return
            end select

            app%output_type = OUTPUT_FILE
            if (trim(app%output) == '-') app%output_type = OUTPUT_STDOUT
        end if

        ! TTY options.
        if (.not. dm_file_exists(app%tty)) then
            call dm_error_out(rc, 'TTY ' // trim(app%tty) // ' does not exist')
            return
        end if

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

        if (.not. dm_tty_valid_timeout(app%timeout)) then
            call dm_error_out(rc, 'invalid timeout')
            return
        end if

        ! Observation jobs.
        if (dm_job_list_count(app%jobs) == 0) then
            call dm_error_out(rc, 'no enabled jobs')
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

        rc = dm_config_open(config, app%config, app%name, geocom=.true.)

        if (dm_is_ok(rc)) then
            rc = dm_config_get(config, 'baudrate', app%baud_rate)
            rc = dm_config_get(config, 'bytesize', app%byte_size)
            rc = dm_config_get(config, 'dtr',      app%dtr)
            rc = dm_config_get(config, 'format',   app%format_name)
            rc = dm_config_get(config, 'logger',   app%logger)
            rc = dm_config_get(config, 'node',     app%node)
            rc = dm_config_get(config, 'output',   app%output)
            rc = dm_config_get(config, 'parity',   app%parity)
            rc = dm_config_get(config, 'rts',      app%rts)
            rc = dm_config_get(config, 'sensor',   app%sensor)
            rc = dm_config_get(config, 'stopbits', app%stop_bits)
            rc = dm_config_get(config, 'timeout',  app%timeout)
            rc = dm_config_get(config, 'tty',      app%tty)
            rc = dm_config_get(config, 'debug',    app%debug)
            rc = dm_config_get(config, 'verbose',  app%verbose)

            rc = dm_config_get(config, 'jobs', app%jobs)
        end if

        call dm_config_close(config)
    end function read_config

    integer function read_observ(tty, observ, node_id, sensor_id, source, debug) result(rc)
        !! Sends requests sequentially to sensor and reads responses.
        type(tty_type),            intent(inout)        :: tty       !! TTY type.
        type(observ_type), target, intent(inout)        :: observ    !! Observation to read.
        character(len=*),          intent(in)           :: node_id   !! Node id of observation.
        character(len=*),          intent(in)           :: sensor_id !! Sensor id of observation.
        character(len=*),          intent(in)           :: source    !! Source of observation.
        logical,                   intent(in), optional :: debug     !! Output debug messages.

        integer                      :: delay
        integer                      :: i, j, n
        logical                      :: debug_   ! Create debug messages only if necessary.
        type(request_type),  pointer :: request  ! Next request to execute.
        type(response_type), pointer :: response ! Single response in request.

        rc = E_EMPTY

        debug_ = .true.
        if (present(debug)) debug_ = debug

        ! Initialise observation.
        observ%id        = dm_uuid4()
        observ%node_id   = node_id
        observ%sensor_id = sensor_id
        observ%source    = source
        observ%timestamp = dm_time_now()
        observ%path      = trim(tty%path)

        n = observ%nrequests

        if (n == 0) then
            observ%error = rc
            call dm_log_info('no requests in observ ' // observ%name, observ=observ)
            return
        end if

        ! Read files in requests sequentially.
        req_loop: do i = 1, n
            request => observ%requests(i)

            if (debug_) then
                call dm_log_debug('starting ' // request_name_string(request%name, i, n, observ%name), observ=observ)
            end if

            ! Prepare request.
            rc = dm_request_set_response_error(request, E_INCOMPLETE)

            request%response  = ' '
            request%timestamp = dm_time_now()

            ! Flush buffers.
            rc = dm_tty_flush(tty)

            if (debug_) then
                if (dm_is_error(rc)) call dm_log_warning('failed to flush buffers', observ=observ, error=rc)
                call dm_log_debug('sending request to TTY ' // trim(app%tty) // ': ' // request%request, observ=observ)
            end if

            ! Send request to sensor.
            rc = dm_tty_write(tty, request)

            if (dm_is_error(rc)) then
                request%error = rc
                call dm_log_error('failed to write ' // request_name_string(request%name, i) // ' to TTY ' // &
                                  app%tty, observ=observ, error=rc)
                cycle req_loop
            end if

            ! Ignore sensor response if no delimiter is set.
            if (len_trim(request%delimiter) == 0) then
                if (debug_) call dm_log_debug('no delimiter in ' // request_name_string(request%name, i), observ=observ)
                cycle req_loop
            end if

            ! Read sensor response from TTY.
            rc = dm_tty_read(tty, request)

            if (dm_is_error(rc)) then
                request%error = rc
                call dm_log_error('failed to read response of ' // request_name_string(request%name, i) // &
                                  ' from TTY ' // app%tty, observ=observ, error=request%error)
                cycle req_loop
            end if

            if (debug_) then
                call dm_log_debug('received response from TTY ' // trim(app%tty) // ': ' // &
                                  request%response, observ=observ)
            end if

            ! Do not extract responses if no pattern is set.
            if (len_trim(request%pattern) == 0) then
                if (debug_) then
                    call dm_log_debug('no pattern in ' // request_name_string(request%name, i), observ=observ)
                end if
                cycle req_loop
            end if

            ! Try to extract the response values if a regex pattern is given.
            rc = dm_regex_request(request)

            if (dm_is_error(rc)) then
                request%error = rc
                call dm_log_warning('response of ' // request_name_string(request%name, i) // ' does not match pattern', &
                                    observ=observ, error=request%error)
                cycle req_loop
            end if

            ! Check response groups for errors.
            do j = 1, request%nresponses
                response => request%responses(j)

                if (dm_is_error(response%error)) then
                    call dm_log_warning('failed to extract response ' // trim(response%name) // ' of ' // &
                                        request_name_string(request%name, i), observ=observ, error=response%error)
                    cycle
                end if
            end do

            if (debug_) call dm_log_debug('finished ' // request_name_string(request%name, i, n, observ%name), observ=observ)

            ! Wait the set delay time of the request.
            delay = max(0, request%delay)
            if (delay <= 0) cycle req_loop

            if (debug_ .and. i < n) then
                call dm_log_debug('next ' // request_name_string(observ%requests(i + 1)%name, i + 1, n, observ%name) // &
                                  ' in ' // dm_itoa(delay / 1000) // ' sec', observ=observ)
            else if (debug_) then
                call dm_log_debug('next observ in ' // dm_itoa(delay / 1000) // ' sec', observ=observ)
            end if

            call dm_usleep(delay * 1000) ! [msec] to [us].
        end do req_loop
    end function read_observ

    pure function request_name_string(request_name, i, n, observ_name) result(str)
        !! Returns string of request name and index for logging.
        character(len=*), intent(in)           :: request_name !! Request name.
        integer,          intent(in)           :: i            !! Request index.
        integer,          intent(in), optional :: n            !! Number of requests in observation.
        character(len=*), intent(in), optional :: observ_name  !! Observation name.
        character(len=:), allocatable          :: str          !! Result.

        if (present(n)) then
            str = 'request ' // trim(request_name) // ' (' // dm_itoa(i) // ' of ' // dm_itoa(n) // ')'
        else
            str = 'request ' // trim(request_name) // ' (' // dm_itoa(i) // ')'
        end if

        if (present(observ_name)) then
            str = str // ' of observ ' // trim(observ_name)
        end if
    end function request_name_string

    integer function run(app, tty) result(rc)
        !! Performs jobs in job list.
        type(app_type), intent(inout) :: app !! App settings.
        type(tty_type), intent(inout) :: tty !! TTY settings.

        integer                    :: delay, njobs
        logical                    :: debug  ! Create debug messages only if necessary.
        type(job_type),    target  :: job    ! Next job to run.
        type(observ_type), pointer :: observ ! Next observation to perform.

        debug = (app%debug .or. app%verbose)

        call dm_log_info('started ' // app%name)

        ! Try to open TTY/PTY.
        call dm_log_debug('opening TTY '  // trim(app%tty) // ' to sensor ' // trim(app%sensor) // &
                          ' (' // dm_itoa(tty%baud_rate) // ' ' // dm_itoa(app%byte_size) // &
                          dm_upper(app%parity(1:1)) // dm_itoa(app%stop_bits) // ')')

        do
            rc = dm_tty_open(tty)
            if (dm_is_ok(rc)) exit
            call dm_log_error('failed to open TTY ' // trim(app%tty) // ', trying again in 5 sec', error=rc)
            call dm_sleep(5)
        end do

        ! Run until no jobs are left.
        job_loop: do
            ! Get number of jobs left.
            njobs = dm_job_list_count(app%jobs)

            if (njobs == 0) then
                rc = E_NONE
                if (debug) call dm_log_debug('no jobs left')
                exit job_loop
            end if

            ! Get next job as deep copy.
            if (debug) call dm_log_debug(dm_itoa(njobs) // ' job(s) left in job queue')
            rc = dm_job_list_next(app%jobs, job)

            if (dm_is_error(rc)) then
                call dm_log_error('failed to fetch next job', error=rc)
                cycle job_loop
            end if

            if (job%valid) then
                ! Get pointer to job observation.
                observ => job%observ

                if (debug) then
                    call dm_log_debug('starting observ ' // trim(observ%name) // ' for sensor ' // &
                                      app%sensor, observ=observ)
                end if

                ! Read observation from TTY.
                rc = read_observ(tty, observ, app%node, app%sensor, app%name, debug=debug)

                ! Forward observation.
                rc = dm_mqueue_forward(observ, app%name, blocking=APP_MQ_BLOCKING)

                ! Output observation.
                rc = output_observ(observ, app%output_type)

                if (debug) then
                    call dm_log_debug('finished observ ' // trim(observ%name) // ' for sensor ' // &
                                      app%sensor, observ=observ)
                end if
            end if

            ! Wait the set delay time of the job (absolute).
            delay = max(0, job%delay)
            if (delay <= 0) cycle job_loop
            if (debug) call dm_log_debug('next job in ' // dm_itoa(delay / 1000) // ' sec', observ=observ)
            call dm_usleep(delay * 1000) ! [msec] to [us].
        end do job_loop

        if (dm_tty_connected(tty)) then
            call dm_log_debug('closing TTY ' // app%tty)
            call dm_tty_close(tty)
        end if

        rc = E_NONE
    end function run

    integer function write_observ(observ, unit, format) result(rc)
        !! Writes observation to file unit, in CSV or JSON Lines format.
        type(observ_type), intent(inout) :: observ
        integer,           intent(in)    :: unit
        integer,           intent(in)    :: format

        rc = E_INVALID

        select case (format)
            case (FORMAT_CSV)
                ! CSV format.
                rc = dm_csv_write(observ, unit=unit, header=.false., &
                                  separator=APP_CSV_SEPARATOR)
            case (FORMAT_JSONL)
                ! JSON Lines format.
                rc = dm_json_write(observ, unit=unit)
            case default
                return
        end select
    end function write_observ

    subroutine signal_handler(signum) bind(c)
        !! Default POSIX signal handler of the program.
        use, intrinsic :: iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: signum

        select case (signum)
            case default
                call dm_log_info('exit on signal ' // dm_itoa(signum))

                if (dm_tty_connected(tty)) then
                    call dm_log_debug('closing TTY ' // tty%path)
                    call dm_tty_close(tty)
                end if

                call dm_stop(0)
        end select
    end subroutine signal_handler
end program dmserial
