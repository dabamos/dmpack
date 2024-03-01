! dmfs.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmfs
    !! Reads observations from file system (file, virtual file, or named pipe).
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmfs'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 1

    character, parameter :: APP_CSV_SEPARATOR = ','    !! CSV separator character.
    logical,   parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    integer, parameter :: OUTPUT_NONE   = 0
    integer, parameter :: OUTPUT_STDOUT = 1
    integer, parameter :: OUTPUT_FILE   = 2

    type :: app_type
        !! Application settings.
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
    type(app_type) :: app ! App settings.

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
                ! No output.
                return

            case (OUTPUT_STDOUT)
                ! Print to standard output.
                rc = write_observ(observ, unit=stdout, format=app%format)

                if (dm_is_error(rc)) then
                    call dm_log_error('failed to output observ', error=rc)
                    return
                end if

            case (OUTPUT_FILE)
                ! Write to file.
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
        type(app_type), intent(inout) :: app !! App settings.
        type(arg_type)                :: args(9)

        rc = E_NONE

        args = [ &
            arg_type('name',    short='n', type=ARG_TYPE_ID),   & ! -n, --name <string>
            arg_type('config',  short='c', type=ARG_TYPE_FILE, required=.true.), & ! -c, --config <path>
            arg_type('logger',  short='l', type=ARG_TYPE_ID),   & ! -l, --logger <string>
            arg_type('node',    short='N', type=ARG_TYPE_ID),   & ! -N, --node <string>
            arg_type('sensor',  short='S', type=ARG_TYPE_ID),   & ! -S, --sensor <string>
            arg_type('output',  short='o', type=ARG_TYPE_CHAR), & ! -o, --output <string>
            arg_type('format',  short='f', type=ARG_TYPE_CHAR), & ! -f, --format <string>
            arg_type('debug',   short='D', type=ARG_TYPE_BOOL), & ! -D, --debug
            arg_type('verbose', short='V', type=ARG_TYPE_BOOL)  & ! -V, --verbose
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
            if (trim(app%output) == '-') app%output_type = OUTPUT_STDOUT
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

    integer function read_observ(observ, node_id, sensor_id, source, debug) result(rc)
        !! Reads observation from file.
        type(observ_type), target, intent(inout)        :: observ    !! Observation to read.
        character(len=*),          intent(in)           :: node_id   !! Node id of observation.
        character(len=*),          intent(in)           :: sensor_id !! Sensor id of observation.
        character(len=*),          intent(in)           :: source    !! Source of observation.
        logical,                   intent(in), optional :: debug     !! Output debug messages.

        integer                      :: delay
        integer                      :: fu, stat
        integer                      :: i, j
        logical                      :: debug_
        type(request_type),  pointer :: request  ! Next request to execute.
        type(response_type), pointer :: response ! Single response in request.

        debug_ = .true.
        if (present(debug)) debug_ = debug

        ! Initialise observation.
        observ%id        = dm_uuid4()
        observ%node_id   = node_id
        observ%sensor_id = sensor_id
        observ%source    = source
        observ%timestamp = dm_time_now()

        if (observ%nrequests == 0) then
            if (debug_) call dm_log_debug('no requests in observ ' // observ%name, observ=observ)
            return
        end if

        ! Read files in requests sequentially.
        req_loop: do i = 1, observ%nrequests
            ! Get pointer to next request.
            request => observ%requests(i)

            ! Initialise request.
            request%timestamp = dm_time_now()
            request%error     = E_IO

            ! Check if file path passed as observation request exists.
            if (.not. dm_file_exists(request%request)) then
                call dm_log_error('file ' // trim(request%request) // ' not found', &
                                  observ=observ, error=request%error)
                cycle req_loop
            end if

            ! Try to open file for reading.
            open (action='read', file=trim(request%request), iostat=stat, newunit=fu)
            if (stat == 0) request%error = E_NONE

            if (dm_is_error(request%error)) then
                call dm_log_error('failed to open ' // trim(request%request), &
                                  observ=observ, error=request%error)
                cycle req_loop
            end if

            ! Read until the request pattern matches.
            read_loop: do
                rc = E_EOF
                read (fu, '(a)', iostat=stat) request%response
                if (is_iostat_end(stat)) exit read_loop
                if (stat /= 0) cycle read_loop

                ! Try to extract the response values.
                rc = dm_regex_request(request)

                if (dm_is_error(rc)) then
                    if (debug_) call dm_log_debug('line does not match pattern', observ=observ, error=rc)
                    cycle read_loop
                end if

                ! Check responses.
                do j = 1, request%nresponses
                    response => request%responses(j)
                    if (dm_is_ok(response%error)) cycle
                    call dm_log_warning('failed to extract response ' // trim(response%name) // &
                                        ' of request ' // dm_itoa(i), observ=observ, error=response%error)
                end do

                ! Cycle on error or exit on success.
                if (dm_is_error(rc)) cycle read_loop
                exit read_loop
            end do read_loop

            ! Close file.
            close (fu)

            ! Save response and return code.
            request%response = dm_ascii_escape(request%response)
            request%error    = rc

            ! Create log message and repeat.
            if (dm_is_error(rc)) then
                call dm_log_error('failed to read from file ' // request%request, observ=observ, error=rc)
                call dm_sleep(10) ! Wait grace period.
                cycle req_loop
            end if

            if (debug_) then
                call dm_log_debug('finished request ' // dm_itoa(i) // ' of ' // &
                                  dm_itoa(observ%nrequests), observ=observ)
            end if

            ! Wait the set delay time of the request.
            delay = max(0, request%delay)
            if (delay <= 0) cycle req_loop

            if (debug_) then
                call dm_log_debug('next request of observ ' // trim(observ%name) // &
                                  ' in ' // dm_itoa(delay / 1000) // ' sec', observ=observ)
            end if

            call dm_usleep(delay * 1000)
        end do req_loop
    end function read_observ

    integer function write_observ(observ, unit, format) result(rc)
        !! Writes observation to file unit, in CSV or JSON Lines format.
        type(observ_type), intent(inout) :: observ !! Observation to write.
        integer,           intent(in)    :: unit   !! File unit.
        integer,           intent(in)    :: format !! Output format (`FORMAT_CSV`, `FORMAT_JSONL`).

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

    subroutine run(app)
        !! Performs jobs in job list.
        type(app_type), intent(inout) :: app !! App settings.

        integer                    :: delay, njobs, rc
        logical                    :: debug
        type(job_type),    target  :: job    ! Next job to run.
        type(observ_type), pointer :: observ ! Observation of job.

        debug = (app%debug .or. app%verbose)
        call dm_log_info('started ' // app%name)

        ! Run until no jobs are left.
        job_loop: do
            njobs = dm_job_list_count(app%jobs)

            if (njobs == 0) then
                call dm_log_debug('no jobs left')
                exit job_loop
            end if

            if (debug) call dm_log_debug(dm_itoa(njobs) // ' job(s) left in job queue')

            ! Get next job as deep copy.
            rc = dm_job_list_next(app%jobs, job)

            if (dm_is_error(rc)) then
                call dm_log_error('failed to fetch next job', error=rc)
                cycle job_loop
            end if

            if (job%valid) then
                observ => job%observ

                if (debug) then
                    call dm_log_debug('starting observ ' // trim(observ%name) // &
                                      ' for sensor ' // app%sensor, observ=observ)
                end if

                ! Read observation from file system.
                rc = read_observ(observ, app%node, app%sensor, app%name, debug=debug)

                if (debug) then
                    call dm_log_debug('finished observ ' // trim(observ%name) // &
                                      ' for sensor ' // app%sensor, observ=observ)
                end if

                ! Forward observation via message queue.
                rc = dm_mqueue_forward(observ, app%name, APP_MQ_BLOCKING)

                ! Output observation.
                rc = output_observ(observ, app%output_type)
            end if

            ! Wait delay time of the job if set (absolute).
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
end program dmfs
