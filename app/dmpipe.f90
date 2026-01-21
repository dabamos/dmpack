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

    character(*), parameter :: APP_NAME  = 'dmpipe'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    character, parameter :: APP_CSV_SEPARATOR = ','    !! CSV field separator.
    logical,   parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    integer, parameter :: OUTPUT_NONE   = 0 !! No output.
    integer, parameter :: OUTPUT_STDOUT = 1 !! Standard output.
    integer, parameter :: OUTPUT_FILE   = 2 !! File.

    type :: app_type
        !! Global application settings.
        character(ID_LEN)          :: name        = APP_NAME    !! Instance and configuration name (required).
        character(FILE_PATH_LEN)   :: config      = ' '         !! Path to configuration file (required).
        character(LOGGER_NAME_LEN) :: logger      = ' '         !! Name of logger.
        character(NODE_ID_LEN)     :: node_id     = ' '         !! Node id (required).
        character(SENSOR_ID_LEN)   :: sensor_id   = ' '         !! Sensor id (required).
        character(FILE_PATH_LEN)   :: output      = ' '         !! Path of output file.
        character(FORMAT_NAME_LEN) :: format_name = 'none'      !! Output format name.
        integer                    :: output_type = OUTPUT_NONE !! Output type.
        integer                    :: format      = FORMAT_NONE !! Output format.
        logical                    :: debug       = .false.     !! Forward debug messages via IPC.
        logical                    :: verbose     = .false.     !! Print debug messages to stderr.
        type(job_list_type)        :: jobs                      !! Job list.
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
    call logger%configure(name    = app%logger,  & ! Name of logger process.
                          node_id = app%node_id, & ! Node id.
                          source  = app%name,    & ! Log source.
                          debug   = app%debug,   & ! Forward debug messages via IPC.
                          ipc     = .true.,      & ! Enable IPC (if logger is set).
                          verbose = app%verbose)   ! Print logs to standard error.
    call logger%info('started ' // APP_NAME)

    ! Register signal handlers and run main loop.
    call dm_signal_register(signal_callback)
    call run(app)

    call logger%info('stopped ' // APP_NAME)
    call dm_stop(STOP_SUCCESS)
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
                    call logger%error('failed to output observation', error=rc)
                    return
                end if

            case (OUTPUT_FILE)
                rc = E_IO

                open (action='write', file=trim(app%output), iostat=stat, newunit=unit, position='append', status='unknown')

                if (stat /= 0) then
                    call logger%error('failed to open file ' // app%output, error=rc)
                    return
                end if

                rc = write_observ(observ, unit=unit, format=app%format)
                if (dm_is_error(rc)) call logger%error('failed to write observation to file ' // app%output, error=rc)

                close (unit)
        end select
    end function output_observ

    integer function read_observ(observ, node_id, sensor_id, source, debug) result(rc)
        !! Reads observation from pipe.
        type(observ_type), intent(inout) :: observ    !! Observation to read.
        character(*),      intent(in)    :: node_id   !! Node id of observation.
        character(*),      intent(in)    :: sensor_id !! Sensor id of observation.
        character(*),      intent(in)    :: source    !! Source of observation.
        logical,           intent(in)    :: debug     !! Output debug messages.

        character(OBSERV_RESPONSE_LEN) :: raw
        integer                        :: i, msec, sec
        type(pipe_type)                :: pipe

        rc = E_NONE

        ! Prepare observation.
        call dm_observ_set(observ    = observ,     &
                           id        = dm_uuid4(), &
                           node_id   = node_id,    &
                           sensor_id = sensor_id,  &
                           source    = source,     &
                           timestamp = dm_time_now())

        if (debug) call logger%debug('started observation ' // observ%name, observ=observ)

        if (dm_observ_is_disabled(observ)) then
            if (debug) call logger%debug('observation ' // trim(observ%name) // ' is disabled', observ=observ)
            return
        end if

        if (.not. dm_string_has(observ%pattern)) then
            if (debug) call logger%debug('no regular expression pattern in observation ' // observ%name, observ=observ)
            return
        end if

        call dm_observ_set_response_error(observ, E_INCOMPLETE)

        pipe_block: block
            ! Open pipe.
            rc = dm_pipe_open(pipe, observ%request, PIPE_RDONLY)

            if (dm_is_error(rc)) then
                call logger%error('failed to open pipe to ' // observ%request, observ=observ, error=rc)
                exit pipe_block
            end if

            ! Read until the pattern matches or end is reached.
            read_loop: do
                rc = dm_pipe_read(pipe, raw)
                if (dm_is_error(rc)) exit read_loop

                observ%response = dm_ascii_escape(raw)
                rc = dm_regex_observ(observ)

                if (dm_is_error(rc)) then
                    if (debug) call logger%debug('response of observation ' // trim(observ%name) // ' does not match pattern', observ=observ, error=observ%error)
                    cycle read_loop
                end if

                ! Log failed or missing responses.
                do i = 1, observ%nresponses
                    associate (response => observ%responses(i))
                        if (dm_is_error(response%error)) then
                            call logger%warning('failed to extract response ' // trim(response%name) // ' of observation ' // observ%name, observ=observ, error=response%error)
                        end if
                    end associate
                end do

                exit read_loop
            end do read_loop
        end block pipe_block

        call dm_pipe_close(pipe)

        if (dm_is_error(rc)) then
            call logger%error('failed to read from process ' // observ%request, observ=observ, error=rc)
            return
        end if

        if (debug) call logger%debug('finished observation ' // observ%name, observ=observ)

        ! Wait the set delay time of the observation.
        msec = max(0, observ%delay)
        sec  = dm_msec_to_sec(msec)

        if (msec == 0) return
        if (debug) call logger%debug('next observation in ' // dm_itoa(sec) // ' sec', observ=observ)
        call dm_msleep(msec)
    end function read_observ

    integer function write_observ(observ, unit, format) result(rc)
        !! Writes observation to file unit, in CSV or JSON Lines format.
        type(observ_type), intent(inout) :: observ
        integer,           intent(in)    :: unit
        integer,           intent(in)    :: format

        select case (format)
            case (FORMAT_CSV);   rc = dm_csv_write(observ, unit=unit, header=.false., separator=APP_CSV_SEPARATOR)
            case (FORMAT_JSONL); rc = dm_json_write(observ, unit=unit)
            case default;        rc = E_INVALID
        end select
    end function write_observ

    subroutine run(app)
        !! Performs jobs in job list.
        type(app_type), intent(inout) :: app

        integer :: msec, sec
        integer :: next, njobs, rc
        logical :: debug

        type(job_type)    :: job
        type(observ_type) :: observ

        debug = (app%debug .or. app%verbose)

        ! Run until no jobs are left.
        job_loop: do
            njobs = dm_job_list_count(app%jobs)

            if (njobs == 0) then
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

            if (dm_job_count(job) == 0) then
                call logger%debug('observation group of job is empty', error=E_EMPTY)
                cycle job_loop
            end if

            if (debug) call logger%debug('started job of observation group ' // dm_job_get_id(job))
            next = 0

            ! Run until no observations are left.
            observ_loop: do
                ! Get next observation of job group.
                rc = dm_job_next(job, next, observ)
                if (dm_is_error(rc)) exit observ_loop

                if (debug) call logger%debug('started observation ' // trim(observ%name) // ' of sensor ' // app%sensor_id, observ=observ)

                ! Read observation.
                rc = read_observ(observ, app%node_id, app%sensor_id, app%name, debug)
                call dm_observ_set(observ, error=rc)

                ! Forward observation.
                rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)

                ! Output observation.
                rc = output_observ(observ, app%output_type)

                if (debug) call logger%debug('finished observation ' // trim(observ%name) // ' of sensor ' // app%sensor_id, observ=observ)
            end do observ_loop

            if (debug) call logger%debug('finished job of observation group ' // dm_job_get_id(job))

            ! Wait the set (absolute) delay time of the job.
            msec = max(0, job%delay)
            sec  = dm_msec_to_sec(msec)

            if (msec <= 0) cycle job_loop
            if (debug) call logger%debug('next job in ' // dm_itoa(sec) // ' sec')

            call dm_msleep(msec)
        end do job_loop
    end subroutine run

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app

        type(arg_parser_class) :: parser

        call parser%add('name',    short='n', type=ARG_TYPE_ID)      ! -n, --name <id>
        call parser%add('config',  short='c', type=ARG_TYPE_FILE)    ! -c, --config <path>
        call parser%add('logger',  short='l', type=ARG_TYPE_ID)      ! -l, --logger <id>
        call parser%add('node',    short='N', type=ARG_TYPE_ID)      ! -N, --node <id>
        call parser%add('sensor',  short='S', type=ARG_TYPE_ID)      ! -S, --sensor <id>
        call parser%add('output',  short='o', type=ARG_TYPE_FILE)    ! -o, --output <path>
        call parser%add('format',  short='f', type=ARG_TYPE_STRING)  ! -f, --format <string>
        call parser%add('debug',   short='D', type=ARG_TYPE_LOGICAL) ! -D, --debug
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
        call parser%get('logger',  app%logger)
        call parser%get('node',    app%node_id)
        call parser%get('sensor',  app%sensor_id)
        call parser%get('output',  app%output)
        call parser%get('format',  app%format_name)
        call parser%get('debug',   app%debug)
        call parser%get('verbose', app%verbose)

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

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('format',  app%format_name)
            call config%get('logger',  app%logger)
            call config%get('node',    app%node_id)
            call config%get('sensor',  app%sensor_id)
            call config%get('output',  app%output)
            call config%get('debug',   app%debug)
            call config%get('verbose', app%verbose)
            call config%get('jobs',    app%jobs, error=rc)
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
        call logger%info('stopped ' // APP_NAME)
        call dm_stop(STOP_SUCCESS)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmpipe
