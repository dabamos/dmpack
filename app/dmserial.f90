! dmserial.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmserial
    !! Reads observations from serial port (TTY/PTY).
    use :: dmpack, dm_log => dm_logger_log
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmserial'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 0

    character, parameter :: APP_CSV_SEPARATOR = ','    !! CSV seperator character.
    logical,   parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    integer, parameter :: OUTPUT_NONE   = 0
    integer, parameter :: OUTPUT_STDOUT = 1
    integer, parameter :: OUTPUT_FILE   = 2

    type :: app_type
        !! Application settings.
        character(len=APP_NAME_LEN)        :: name        = APP_NAME    !! Instance and configuration name (required).
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
    rc = create_tty(tty, app)
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
    integer function create_tty(tty, app) result(rc)
        !! Creates TTY type from application settings.
        type(tty_type), intent(inout) :: tty !! TTY type.
        type(app_type), intent(inout) :: app !! App type.

        tty_block: block
            tty%path = app%tty

            tty%baud_rate = dm_tty_baud_rate_from_value(app%baud_rate, error=rc)
            if (dm_is_error(rc)) exit tty_block

            tty%byte_size = dm_tty_byte_size_from_value(app%byte_size, error=rc)
            if (dm_is_error(rc)) exit tty_block

            tty%parity = dm_tty_parity_from_name(app%parity, error=rc)
            if (dm_is_error(rc)) exit tty_block

            tty%stop_bits = dm_tty_stop_bits_from_value(app%stop_bits, error=rc)
            if (dm_is_error(rc)) exit tty_block

            tty%dtr = app%dtr
            tty%rts = app%rts
        end block tty_block

        if (dm_is_error(rc)) call dm_error_out(rc, 'invalid TTY parameters')
    end function create_tty

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
            call dm_log(LOG_DEBUG, 'skipped receiver ' // dm_itoa(next) // ' (' // &
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
                call dm_log(LOG_ERROR, 'failed to open mqueue /' // trim(observ%receivers(next)) // ': ' // &
                            dm_system_error_string(), observ=observ, error=rc)
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
                    call dm_log(LOG_ERROR, 'failed to output observ', error=rc)
                    return
                end if

            case (OUTPUT_FILE)
                ! Output to file.
                rc = E_IO

                open (action='write', file=trim(app%output), iostat=stat, &
                      newunit=fu, position='append', status='unknown')

                if (stat /= 0) then
                    call dm_log(LOG_ERROR, 'failed to open file ' // app%output, error=rc)
                    return
                end if

                ! Output in CSV or JSON Lines format.
                rc = write_observ(observ, unit=fu, format=app%format)

                if (dm_is_error(rc)) then
                    call dm_log(LOG_ERROR, 'failed to write observ to file ' // app%output, error=rc)
                end if

                close (fu)
        end select
    end function output_observ

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

        rc = dm_config_open(config, app%config, app%name)

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

    integer function run(app, tty) result(rc)
        !! Performs jobs in job list.
        type(app_type), intent(inout) :: app
        type(tty_type), intent(inout) :: tty

        character(len=REQUEST_REQUEST_LEN)   :: r_request   ! Raw request (unescaped).
        character(len=REQUEST_DELIMITER_LEN) :: r_delimiter ! Raw delimiter (unescaped).

        integer :: delay, njobs
        integer :: i, j

        type(job_type),      target  :: job      ! Next job to run.
        type(observ_type),   pointer :: observ   ! Next observation to perform.
        type(request_type),  pointer :: request  ! Next request to execute.
        type(response_type), pointer :: response ! Response in request.

        call dm_log(LOG_INFO, 'started ' // app%name)
        call dm_log(LOG_DEBUG, 'opening TTY '  // trim(app%tty) // ' to sensor ' // trim(app%sensor) // &
                    ' (' // dm_itoa(tty%baud_rate) // ' ' // dm_itoa(app%byte_size) // &
                    dm_upper(app%parity(1:1)) // dm_itoa(app%stop_bits) // ')')

        ! Open TTY/PTY.
        open_loop: do
            rc = dm_tty_open(tty)
            if (dm_is_ok(rc)) exit open_loop
            call dm_log(LOG_ERROR, 'failed to open TTY ' // app%tty, error=rc)
            call dm_log(LOG_DEBUG, 'trying to open TTY again in 5 seconds', error=rc)
            call dm_sleep(5)
        end do open_loop

        ! Run until no jobs are left.
        job_loop: do
            ! Get number of jobs left.
            njobs = dm_job_list_count(app%jobs)

            if (njobs == 0) then
                rc = E_NONE
                call dm_log(LOG_DEBUG, 'no jobs left')
                exit job_loop
            end if

            ! Get next job as deep copy.
            call dm_log(LOG_DEBUG, dm_itoa(njobs) // ' job(s) left in job queue')
            rc = dm_job_list_next(app%jobs, job)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to fetch next job', error=rc)
                cycle job_loop
            end if

            observ_if: if (job%valid) then
                ! Get pointer to job observation.
                observ => job%observ

                call dm_log(LOG_DEBUG, 'starting observ ' // trim(observ%name) // &
                            ' for sensor ' // app%sensor, observ=observ)

                ! Initialise observation.
                observ%id        = dm_uuid4()
                observ%node_id   = app%node
                observ%sensor_id = app%sensor
                observ%timestamp = dm_time_now()
                observ%path      = trim(app%tty)

                if (observ%nrequests == 0) then
                    call dm_log(LOG_INFO, 'no requests in observ ' // observ%name, observ=observ)
                    observ%error = E_EMPTY
                    exit observ_if
                end if

                ! Read files in requests sequentially.
                req_loop: do i = 1, observ%nrequests
                    ! Get pointer to next request.
                    request => observ%requests(i)

                    call dm_log(LOG_DEBUG, 'starting request ' // dm_itoa(i) // ' of ' // &
                                dm_itoa(observ%nrequests), observ=observ)

                    ! Set raw values.
                    r_request   = dm_ascii_unescape(request%request)
                    r_delimiter = dm_ascii_unescape(request%delimiter)

                    ! Set default error of responses.
                    rc = dm_request_set_response_error(request, E_INCOMPLETE)

                    ! Send request to sensor.
                    call dm_log(LOG_DEBUG, 'sending request: ' // r_request, observ=observ)

                    request%response  = ' '
                    request%timestamp = dm_time_now()

                    request%error = dm_tty_flush(tty, output=.false.)
                    request%error = dm_tty_write(tty, trim(r_request))

                    if (dm_is_error(request%error)) then
                        call dm_log(LOG_ERROR, 'failed to write to TTY ' // app%tty, observ=observ, &
                                    error=request%error)
                        cycle req_loop
                    end if

                    if (len_trim(r_delimiter) == 0) then
                        call dm_log(LOG_DEBUG, 'no delimiter set in request ' // dm_itoa(i), observ=observ)
                        cycle req_loop
                    end if

                    ! Read raw sensor response from TTY.
                    request%error = dm_tty_read(tty, request%response, trim(r_delimiter))

                    if (dm_is_error(request%error)) then
                        call dm_log(LOG_ERROR, 'failed to read from TTY ' // app%tty, &
                                    observ=observ, error=request%error)
                        cycle req_loop
                    end if

                    call dm_log(LOG_DEBUG, 'received raw response: ' // request%response, observ=observ)

                    if (len_trim(request%pattern) == 0) then
                        call dm_log(LOG_DEBUG, 'no pattern in request ' // dm_itoa(i), observ=observ)
                        cycle req_loop
                    end if

                    ! Try to extract the response values if a regex pattern is given.
                    call dm_log(LOG_DEBUG, 'extracting response values', observ=observ)
                    request%error = dm_regex_request(request)

                    ! Unescape raw response.
                    request%response = dm_ascii_escape(request%response)

                    if (dm_is_error(request%error)) then
                        call dm_log(LOG_WARNING, 'response to request ' // dm_itoa(i) // ' does not match pattern', &
                                    observ=observ, error=request%error)
                        cycle req_loop
                    end if

                    ! Check response groups for errors.
                    do j = 1, request%nresponses
                        response => request%responses(j)

                        if (dm_is_error(response%error)) then
                            call dm_log(LOG_WARNING, 'failed to extract response ' // trim(response%name) // &
                                        ' to request ' // dm_itoa(i), observ=observ, error=response%error)
                            cycle
                        end if

                        call dm_log(LOG_DEBUG, 'extracted response ' // trim(response%name) // &
                                    ' of request ' // dm_itoa(i), observ=observ)
                    end do

                    call dm_log(LOG_DEBUG, 'finished request ' // dm_itoa(i) // ' of ' // &
                                dm_itoa(observ%nrequests), observ=observ)

                    ! Wait the set delay time of the request.
                    delay = max(0, request%delay)
                    if (delay <= 0) cycle req_loop
                    call dm_log(LOG_DEBUG, 'next request of observ ' // trim(observ%name) // &
                                ' in ' // dm_itoa(delay / 1000) // ' sec')
                    call dm_usleep(delay * 1000)
                end do req_loop

                ! Forward and output observation.
                call dm_log(LOG_DEBUG, 'finished observ ' // trim(observ%name) // &
                            ' for sensor ' // app%sensor, observ=observ)
                rc = forward_observ(observ, app%name)
                rc = output_observ(observ, app%output_type)
            end if observ_if

            ! Wait the set delay time of the job (absolute).
            delay = max(0, job%delay)
            if (delay <= 0) cycle job_loop
            call dm_log(LOG_DEBUG, 'next job in ' // dm_itoa(delay / 1000) // ' sec')
            call dm_usleep(delay * 1000)
        end do job_loop

        if (dm_tty_connected(tty)) then
            call dm_log(LOG_DEBUG, 'closing TTY ' // app%tty)
            call dm_tty_close(tty)
        end if

        rc = E_NONE
    end function run

    subroutine signal_handler(signum) bind(c)
        !! Default POSIX signal handler of the program.
        use, intrinsic :: iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: signum

        select case (signum)
            case default
                call dm_log(LOG_INFO, 'exit on signal ' // dm_itoa(signum))

                if (dm_tty_connected(tty)) then
                    call dm_log(LOG_DEBUG, 'closing TTY ' // tty%path)
                    call dm_tty_close(tty)
                end if

                call dm_stop(0)
        end select
    end subroutine signal_handler
end program dmserial
