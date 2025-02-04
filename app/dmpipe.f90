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
    integer,          parameter :: APP_PATCH = 2

    character, parameter :: APP_CSV_SEPARATOR = ','    !! CSV field separator.
    logical,   parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    integer, parameter :: OUTPUT_NONE   = 0 !! No output.
    integer, parameter :: OUTPUT_STDOUT = 1 !! Standard output.
    integer, parameter :: OUTPUT_FILE   = 2 !! File.

    type :: app_type
        !! Global application settings.
        character(len=ID_LEN)          :: name        = APP_NAME    !! Instance and configuration name (required).
        character(len=FILE_PATH_LEN)   :: config      = ' '         !! Path to configuration file (required).
        character(len=LOGGER_NAME_LEN) :: logger      = ' '         !! Name of logger.
        character(len=NODE_ID_LEN)     :: node_id     = ' '         !! Node id (required).
        character(len=SENSOR_ID_LEN)   :: sensor_id   = ' '         !! Sensor id (required).
        character(len=FILE_PATH_LEN)   :: output      = ' '         !! Path of output file.
        character(len=FORMAT_NAME_LEN) :: format_name = 'none'      !! Output format name.
        integer                        :: output_type = OUTPUT_NONE !! Output type.
        integer                        :: format      = FORMAT_NONE !! Output format.
        logical                        :: debug       = .false.     !! Forward debug messages via IPC.
        logical                        :: verbose     = .false.     !! Print debug messages to stderr.
        type(job_list_type)            :: jobs                      !! Job list.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

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

    ! Run main loop.
    call dm_signal_register(signal_callback)
    call run(app)
contains
    integer function output_observ(observ, type) result(rc)
        !! Outputs observation to file or _stdout_ if `type` is not
        !! `OUTPUT_NONE`.
        type(observ_type), intent(inout) :: observ !! Observation type.
        integer,           intent(in)    :: type   !! Output I/O type.

        integer :: stat, unit

        rc = E_NONE

        select case (type)
            case (OUTPUT_NONE)
                return

            case (OUTPUT_STDOUT)
                rc = write_observ(observ, unit=stdout, format=app%format)

                if (dm_is_error(rc)) then
                    call logger%error('failed to output observ', error=rc)
                    return
                end if

            case (OUTPUT_FILE)
                rc = E_IO

                open (action='write', file=trim(app%output), iostat=stat, &
                      newunit=unit, position='append', status='unknown')

                if (stat /= 0) then
                    call logger%error('failed to open file ' // app%output, error=rc)
                    return
                end if

                rc = write_observ(observ, unit=unit, format=app%format)

                if (dm_is_error(rc)) then
                    call logger%error('failed to write observ to file ' // app%output, error=rc)
                end if

                close (unit)
        end select
    end function output_observ

    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app
        type(arg_type)              :: args(9)

        args = [ &
            arg_type('name',    short='n', type=ARG_TYPE_ID),      & ! -n, --name <id>
            arg_type('config',  short='c', type=ARG_TYPE_FILE, required=.true.), & ! -c, --config <path>
            arg_type('logger',  short='l', type=ARG_TYPE_ID),      & ! -l, --logger <id>
            arg_type('node',    short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('sensor',  short='S', type=ARG_TYPE_ID),      & ! -S, --sensor <id>
            arg_type('output',  short='o', type=ARG_TYPE_STRING),  & ! -o, --output <path>
            arg_type('format',  short='f', type=ARG_TYPE_STRING),  & ! -f, --format <string>
            arg_type('debug',   short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('verbose', short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
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
        call dm_arg_get(args(5), app%sensor_id)
        call dm_arg_get(args(6), app%output)
        call dm_arg_get(args(7), app%format_name)
        call dm_arg_get(args(8), app%debug)
        call dm_arg_get(args(9), app%verbose)

        ! Validate options.
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

        if (len_trim(app%logger) > 0 .and. .not. dm_id_is_valid(app%logger)) then
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
            call dm_config_get(config, 'format',  app%format_name)
            call dm_config_get(config, 'jobs',    app%jobs)
            call dm_config_get(config, 'logger',  app%logger)
            call dm_config_get(config, 'node',    app%node_id)
            call dm_config_get(config, 'sensor',  app%sensor_id)
            call dm_config_get(config, 'output',  app%output)
            call dm_config_get(config, 'debug',   app%debug)
            call dm_config_get(config, 'verbose', app%verbose)
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

        integer          :: delay_msec
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
            if (debug_) call logger%debug('no requests in observ ' // observ%name, observ=observ)
            return
        end if

        ! Read pipes in requests sequentially.
        req_loop: do i = 1, n
            ! Get pointer to next request.
            request => observ%requests(i)

            if (request%state == REQUEST_STATE_DISABLED) then
                if (debug_) call logger%debug(request_name_string(request%name, i, n, observ%name) // ' is disabled', observ=observ)
                cycle
            end if

            if (debug_) call logger%debug('starting ' // request_name_string(request%name, i, n, observ%name), observ=observ)

            ! Initialise request.
            request%timestamp = dm_time_now()
            request%error     = E_IO

            ! Open pipe.
            rc = dm_pipe_open(pipe, request%request, PIPE_RDONLY)

            if (dm_is_error(rc)) then
                call dm_pipe_close(pipe)
                call logger%error('failed to open pipe to ' // request%request, observ=observ, error=rc)
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
                    if (debug_) call logger%debug('no pattern in ' // request_name_string(request%name, i), observ=observ)
                    exit pipe_loop
                end if

                rc = dm_regex_request(request)

                if (dm_is_error(rc)) then
                    if (debug_) then
                        call logger%debug('response of ' // request_name_string(request%name, i) // &
                                          ' does not match pattern', observ=observ, error=request%error)
                    end if
                    cycle pipe_loop
                end if

                ! Check responses.
                do j = 1, request%nresponses
                    response => request%responses(j)
                    if (dm_is_ok(response%error)) cycle
                    call logger%warning('failed to extract response ' // trim(response%name) // ' of ' // &
                                        request_name_string(request%name, i), observ=observ, error=response%error)
                end do

                ! Re-read on error.
                if (dm_is_error(rc)) cycle pipe_loop

                exit pipe_loop
            end do pipe_loop

            ! Close pipe.
            call dm_pipe_close(pipe)

            request%error = rc

            if (dm_is_error(rc)) then
                call logger%error('failed to read from process ' // request%request, observ=observ, error=rc)
                call dm_sleep(10) ! Wait grace period.
                cycle req_loop
            end if

            if (debug_) call logger%debug('finished ' // request_name_string(request%name, i, n, observ%name), observ=observ)

            ! Wait the set delay time of the request.
            delay_msec = max(0, request%delay)
            if (delay_msec == 0) cycle req_loop

            if (debug_ .and. i < n) then
                call logger%debug('next ' // request_name_string(observ%requests(i + 1)%name, i + 1, n, observ%name) // &
                                  ' in ' // dm_itoa(dm_msec_to_sec(delay_msec)) // ' sec', observ=observ)
            else if (debug_) then
                call logger%debug('next observ in ' // dm_itoa(dm_msec_to_sec(delay_msec)) // ' sec', observ=observ)
            end if

            call dm_msleep(delay_msec)
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
            str = 'request ' // trim(request_name) // ' (' // dm_itoa(i) // '/' // dm_itoa(n) // ')'
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

        integer :: delay_msec, njobs
        integer :: rc
        logical :: debug

        type(pipe_type)            :: pipe   ! Pipe to process.
        type(job_type),    target  :: job    ! Next job to run.
        type(observ_type), pointer :: observ ! Next observation to perform.

        debug = (app%debug .or. app%verbose)

        call logger%info('started ' // APP_NAME)

        ! Run until no jobs are left.
        job_loop: do
            njobs = dm_job_list_count(app%jobs)

            if (njobs == 0) then
                if (debug) call logger%debug('no jobs left')
                exit job_loop
            end if

            if (debug) call logger%debug(dm_itoa(njobs) // ' job(s) left in job queue')

            ! Get next job as deep copy.
            rc = dm_job_list_next(app%jobs, job)

            if (dm_is_error(rc)) then
                call logger%error('failed to fetch next job', error=rc)
                cycle job_loop
            end if

            observ_if: if (job%valid) then
                ! Get pointer to job observation.
                observ => job%observ
                if (debug) call logger%debug('starting observ ' // observ%name, observ=observ)

                ! Read observation.
                rc = read_observ(pipe, observ, app%node_id, app%sensor_id, app%name, debug=debug)

                ! Forward observation.
                rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)

                ! Output observation.
                rc = output_observ(observ, app%output_type)

                if (debug) call logger%debug('finished observ ' // observ%name, observ=observ)
            end if observ_if

            ! Wait the set (absolute) delay time of the job.
            delay_msec = max(0, job%delay)
            if (delay_msec <= 0) cycle job_loop
            if (debug) call logger%debug('next job in ' // dm_itoa(dm_msec_to_sec(delay_msec)) // ' sec', observ=observ)
            call dm_msleep(delay_msec)
        end do job_loop
    end subroutine run

    subroutine signal_callback(signum) bind(c)
        !! Default POSIX signal handler of the program.
        integer(kind=c_int), intent(in), value :: signum

        select case (signum)
            case default
                call logger%info('exit on signal ' // dm_signal_name(signum))
                call dm_stop(STOP_SUCCESS)
        end select
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmpipe
