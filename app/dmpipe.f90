! dmpipe.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmpipe
    !! Unix pipe reader to be used to query system information and third-party
    !! applications. Runs a process through an anonymous pipe and forwards
    !! the parsed result as observation to the next specified process via POSIX
    !! message queue.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmpipe'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 0

    character, parameter :: APP_CSV_SEPARATOR = ','    !! CSV seperator character.
    logical,   parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    integer, parameter :: OUTPUT_NONE   = 0 !! No output.
    integer, parameter :: OUTPUT_STDOUT = 1 !! Standard output.
    integer, parameter :: OUTPUT_FILE   = 2 !! File.

    type :: app_type
        !! Global application settings.
        character(len=ID_LEN)          :: name        = APP_NAME    !! Instance and configuration name (required).
        character(len=FILE_PATH_LEN)   :: config      = ' '         !! Path to configuration file (required).
        character(len=LOGGER_NAME_LEN) :: logger      = ' '         !! Name of logger.
        character(len=NODE_ID_LEN)     :: node        = ' '         !! Node id (required).
        character(len=SENSOR_ID_LEN)   :: sensor      = ' '         !! Sensor id (required).
        character(len=FILE_PATH_LEN)   :: output      = ' '         !! Path of output file.
        character(len=FORMAT_NAME_LEN) :: format_name = 'none'      !! Output format name.
        integer                        :: output_type = OUTPUT_NONE !! Output type.
        integer                        :: format      = FORMAT_NONE !! Output format.
        logical                        :: debug       = .false.     !! Forward debug messages via IPC.
        logical                        :: verbose     = .false.     !! Print debug messages to stderr.
        type(job_list_type)            :: jobs                      !! Job list.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App configuration.

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

    ! Run main loop.
    call dm_signal_register(signal_handler)
    call run(app)
    call dm_stop(0)
contains
    integer function output_observ(observ, type) result(rc)
        !! Outputs observation to file or _stdout_ if `type` is not
        !! `OUTPUT_NONE`.
        type(observ_type), intent(inout) :: observ !! Observation type.
        integer,           intent(in)    :: type   !! Output I/O type.

        integer :: fu, stat

        rc = E_NONE

        select case (type)
            case (OUTPUT_NONE)
                return

            case (OUTPUT_STDOUT)
                rc = write_observ(observ, unit=stdout, format=app%format)

                if (dm_is_error(rc)) then
                    call dm_log_error('failed to output observ', error=rc)
                    return
                end if

            case (OUTPUT_FILE)
                rc = E_IO

                open (action='write', file=trim(app%output), iostat=stat, &
                      newunit=fu, position='append', status='unknown')

                if (stat /= 0) then
                    call dm_log_error('failed to open file ' // app%output, error=rc)
                    return
                end if

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
        type(arg_type)                :: args(9)

        rc = E_NONE

        args = [ &
            arg_type('name',    short='n', type=ARG_TYPE_ID),                    & ! -n, --name <string>
            arg_type('config',  short='c', type=ARG_TYPE_FILE, required=.true.), & ! -c, --config <path>
            arg_type('logger',  short='l', type=ARG_TYPE_ID),                    & ! -l, --logger <string>
            arg_type('node',    short='N', type=ARG_TYPE_ID),                    & ! -N, --node <string>
            arg_type('sensor',  short='S', type=ARG_TYPE_ID),                    & ! -S, --sensor <string>
            arg_type('output',  short='o', type=ARG_TYPE_CHAR),                  & ! -o, --output <string>
            arg_type('format',  short='f', type=ARG_TYPE_CHAR),                  & ! -f, --format <string>
            arg_type('debug',   short='D', type=ARG_TYPE_BOOL),                  & ! -D, --debug
            arg_type('verbose', short='V', type=ARG_TYPE_BOOL)                   & ! -V, --verbose
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
        rc = dm_arg_get(args(3), app%logger)
        rc = dm_arg_get(args(4), app%node)
        rc = dm_arg_get(args(5), app%sensor)
        rc = dm_arg_get(args(6), app%output)
        rc = dm_arg_get(args(7), app%format_name)
        rc = dm_arg_get(args(8), app%debug)
        rc = dm_arg_get(args(9), app%verbose)

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
            if (app%output == '-') app%output_type = OUTPUT_STDOUT
        end if

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
            rc = dm_config_get(config, 'format',  app%format_name)
            rc = dm_config_get(config, 'jobs',    app%jobs)
            rc = dm_config_get(config, 'logger',  app%logger)
            rc = dm_config_get(config, 'node',    app%node)
            rc = dm_config_get(config, 'output',  app%output)
            rc = dm_config_get(config, 'sensor',  app%sensor)
            rc = dm_config_get(config, 'debug',   app%debug)
            rc = dm_config_get(config, 'verbose', app%verbose)
            rc = E_NONE
        end if

        call dm_config_close(config)
    end function read_config

    integer function read_observ(pipe, observ, node_id, sensor_id, source, debug) result(rc)
        !! Reads observation from pipe.
        type(pipe_type),           intent(inout)        :: pipe      !! Pipe to read from.
        type(observ_type), target, intent(inout)        :: observ    !! Observation to read.
        character(len=*),          intent(in)           :: node_id   !! Node id of observation.
        character(len=*),          intent(in)           :: sensor_id !! Sensor id of observation.
        character(len=*),          intent(in)           :: source    !! Source of observation.
        logical,                   intent(in), optional :: debug     !! Output debug messages.

        character(len=REQUEST_RESPONSE_LEN)  :: raw ! Raw response (unescaped).

        integer          :: delay
        integer          :: i, j, n
        integer(kind=i8) :: sz
        logical          :: debug_

        type(request_type),  pointer :: request  ! Next request to execute.
        type(response_type), pointer :: response ! Response in request.

        rc = E_EMPTY

        debug_ = .true.
        if (present(debug)) debug_ = debug

        ! Initialise observation.
        observ%id        = dm_uuid4()
        observ%node_id   = node_id
        observ%sensor_id = sensor_id
        observ%source    = source
        observ%timestamp = dm_time_now()

        n = observ%nrequests

        if (n == 0) then
            observ%error = rc
            if (debug_) call dm_log_debug('no requests in observ ' // observ%name, observ=observ)
            return
        end if

        ! Read files in requests sequentially.
        req_loop: do i = 1, n
            ! Get pointer to next request.
            request => observ%requests(i)

            if (debug_) call dm_log_debug('starting ' // request_name_string(request%name, i, n, observ%name), observ=observ)

            ! Initialise request.
            request%timestamp = dm_time_now()
            request%error     = E_IO

            ! Open pipe.
            rc = dm_pipe_open(pipe, request%request, PIPE_RDONLY)

            if (dm_is_error(rc)) then
                call dm_pipe_close(pipe)
                call dm_log_error('failed to open pipe to ' // request%request, observ=observ, error=rc)
                cycle req_loop
            end if

            ! Read until the request pattern matches or end is reached.
            pipe_loop: do
                ! Read from pipe.
                rc = E_READ
                sz = dm_pipe_read(pipe, raw)
                if (sz == 0) exit pipe_loop

                request%response = dm_ascii_escape(raw)

                ! Try to extract the response values.
                if (len_trim(request%pattern) == 0) then
                    rc = E_NONE
                    if (debug_) call dm_log_debug('no pattern in ' // request_name_string(request%name, i), observ=observ)
                    exit pipe_loop
                end if

                rc = dm_regex_request(request)

                if (dm_is_error(rc)) then
                    if (debug_) then
                        call dm_log_debug('response of ' // request_name_string(request%name, i) // &
                                          ' does not match pattern', observ=observ, error=request%error)
                    end if
                    cycle pipe_loop
                end if

                ! Check responses.
                do j = 1, request%nresponses
                    response => request%responses(j)
                    if (dm_is_ok(response%error)) cycle
                    call dm_log_warning('failed to extract response ' // trim(response%name) // &
                                        ' of ' // request_name_string(request%name, i), &
                                        observ=observ, error=response%error)
                end do

                ! Re-read on error.
                if (dm_is_error(rc)) cycle pipe_loop

                exit pipe_loop
            end do pipe_loop

            ! Close pipe.
            call dm_pipe_close(pipe)

            request%error = rc

            if (dm_is_error(rc)) then
                call dm_log_error('failed to read from process ' // request%request, observ=observ, error=rc)
                call dm_sleep(10) ! Wait grace period.
                cycle req_loop
            end if

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

    integer function write_observ(observ, unit, format) result(rc)
        !! Writes observation to file unit, in CSV or JSON Lines format.
        type(observ_type), intent(inout) :: observ
        integer,           intent(in)    :: unit
        integer,           intent(in)    :: format

        rc = E_INVALID

        select case (format)
            case (FORMAT_CSV)
                rc = dm_csv_write(observ, unit=unit, header=.false., separator=APP_CSV_SEPARATOR)
            case (FORMAT_JSONL)
                rc = dm_json_write(observ, unit=unit)
            case default
                return
        end select
    end function write_observ

    subroutine run(app)
        !! Performs jobs in job list.
        type(app_type), intent(inout) :: app

        integer :: delay, njobs
        integer :: rc
        logical :: debug

        type(job_type),    target  :: job    ! Next job to run.
        type(pipe_type)            :: pipe   ! Pipe to process.
        type(observ_type), pointer :: observ ! Next observation to perform.

        debug = (app%debug .or. app%verbose)

        call dm_log_info('started ' // app%name)

        ! Run until no jobs are left.
        job_loop: do
            njobs = dm_job_list_count(app%jobs)

            if (njobs == 0) then
                if (debug) call dm_log_debug('no jobs left')
                exit job_loop
            end if

            if (debug) call dm_log_debug(dm_itoa(njobs) // ' job(s) left in job queue')

            ! Get next job as deep copy.
            rc = dm_job_list_next(app%jobs, job)

            if (dm_is_error(rc)) then
                call dm_log_error('failed to fetch next job', error=rc)
                cycle job_loop
            end if

            observ_if: if (job%valid) then
                ! Get pointer to job observation.
                observ => job%observ
                if (debug) call dm_log_debug('starting observ ' // observ%name, observ=observ)

                ! Read observation.
                rc = read_observ(pipe, observ, app%node, app%sensor, app%name, debug=debug)

                ! Forward observation.
                rc = dm_mqueue_forward(observ, app%name, APP_MQ_BLOCKING)

                ! Output observation.
                rc = output_observ(observ, app%output_type)

                if (debug) call dm_log_debug('finished observ ' // observ%name, observ=observ)
            end if observ_if

            ! Wait the set delay time of the job (absolute).
            delay = max(0, job%delay)
            if (delay <= 0) cycle job_loop
            if (debug) call dm_log_debug('next job in ' // dm_itoa(delay / 1000) // ' sec', observ=observ)
            call dm_usleep(delay * 1000)
        end do job_loop
    end subroutine run

    subroutine signal_handler(signum) bind(c)
        !! Default POSIX signal handler of the program.
        use, intrinsic :: iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: signum

        select case (signum)
            case default
                call dm_log_info('exit on signal ' // dm_itoa(signum))
                call dm_stop(0)
        end select
    end subroutine signal_handler
end program dmpipe
