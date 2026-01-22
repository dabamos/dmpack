! dmfs.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmfs
    !! Reads observations from file system (file, virtual file, or named pipe).
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmfs'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    character, parameter :: APP_CSV_SEPARATOR = ','    !! CSV field separator.
    logical,   parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    integer, parameter :: OUTPUT_NONE   = 0
    integer, parameter :: OUTPUT_STDOUT = 1
    integer, parameter :: OUTPUT_FILE   = 2

    type :: app_type
        !! Application settings.
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

    call dm_posix_signal_register(signal_callback)
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
                ! No output.
                return

            case (OUTPUT_STDOUT)
                ! Print to standard output.
                rc = write_observ(observ, unit=stdout, format=app%format)

                if (dm_is_error(rc)) then
                    call logger%error('failed to output observation', error=rc)
                    return
                end if

            case (OUTPUT_FILE)
                ! Write to file.
                rc = E_IO

                open (action='write', file=trim(app%output), iostat=stat, &
                      newunit=unit, position='append', status='unknown')

                if (stat /= 0) then
                    call logger%error('failed to open file ' // app%output, error=rc)
                    return
                end if

                rc = write_observ(observ, unit=unit, format=app%format)

                if (dm_is_error(rc)) then
                    call logger%error('failed to write observation to file ' // app%output, error=rc)
                end if

                close (unit)
        end select
    end function output_observ

    integer function read_observ(observ, node_id, sensor_id, source, debug) result(rc)
        !! Reads observation from file. By default, sends log messages to
        !! logger.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if reading from file failed.
        !! * `E_NOT_FOUND` if file could not be found.
        !!
        type(observ_type), intent(inout) :: observ    !! Observation to read.
        character(*),      intent(in)    :: node_id   !! Node id of observation.
        character(*),      intent(in)    :: sensor_id !! Sensor id of observation.
        character(*),      intent(in)    :: source    !! Source of observation.
        logical,           intent(in)    :: debug     !! Output debug messages.

        character(OBSERV_RESPONSE_LEN) :: raw
        integer                        :: i, msec, sec, unit, stat

        rc = E_EMPTY

        ! Initialise observation.
        call dm_observ_set(observ, id=dm_uuid4(), node_id=node_id, sensor_id=sensor_id, timestamp=dm_time_now(), source=source)
        if (debug) call logger%debug('started observation ' // observ%name, observ=observ)

        ! Return if observation is disabled.
        if (dm_observ_is_disabled(observ)) then
            if (debug) call logger%debug('observation ' // trim(observ%name) // ' is disabled', observ=observ)
            return
        end if

        ! Prepare observation.
        call dm_observ_set_response_error(observ, E_INCOMPLETE)

        ! Return if file path passed as observation request does not exist.
        if (.not. dm_file_exists(observ%request)) then
            rc = E_NOT_FOUND
            call logger%error('file ' // trim(observ%request) // ' not found', observ=observ, error=rc)
            return
        end if

        ! Try to open file for reading.
        io_block: block
            rc = E_IO
            open (action='read', file=trim(observ%request), iostat=stat, newunit=unit)

            if (stat /= 0) then
                call logger%error('failed to open file ' // trim(observ%request), observ=observ, error=rc)
                exit io_block
            end if

            ! Update time stamp.
            call dm_observ_set(observ, timestamp=dm_time_now())

            ! Read until the response pattern matches or end is reached.
            rc = E_EOF

            read_loop: do
                ! Read single line from unit.
                read (unit, '(a)', iostat=stat) raw
                if (is_iostat_end(stat)) exit read_loop

                rc = E_READ
                if (stat /= 0) exit read_loop

                ! Escape non-printable characters.
                observ%response = dm_ascii_escape(raw)

                ! Look for regular expression pattern.
                if (.not. dm_observ_has_pattern(observ)) then
                    rc = E_EMPTY
                    if (debug) call logger%debug('no regular expression pattern in observation ' // observ%name, observ=observ, error=rc)
                    exit read_loop
                end if

                ! Try to extract the response values.
                rc = dm_regex_observ(observ)

                if (dm_is_error(rc)) then
                    if (debug) call logger%debug('response in observation ' // trim(observ%name) // ' does not match pattern', observ=observ, error=rc)
                    cycle read_loop
                end if

                ! Look for response errors.
                do i = 1, observ%nresponses
                    associate (response => observ%responses(i))
                        if (dm_is_error(response%error)) then
                            call logger%warning('failed to extract response ' // trim(response%name) // ' in observation ' // observ%name, observ=observ, error=response%error)
                        end if
                    end associate
                end do

                exit read_loop
            end do read_loop
        end block io_block

        close (unit)
        if (dm_is_error(rc)) call logger%error('failed to read from file ' // observ%request, observ=observ, error=rc)
        if (debug) call logger%debug('finished observation ' // observ%name, observ=observ)

        msec = max(0, observ%delay)
        sec  = dm_msec_to_sec(msec)
        if (msec == 0) return
        if (debug) call logger%debug('next observation in ' // dm_itoa(msec) // ' sec', observ=observ)
        call dm_msleep(msec)
    end function read_observ

    integer function write_observ(observ, unit, format) result(rc)
        !! Writes observation to file unit, in CSV or JSON Lines format.
        type(observ_type), intent(inout) :: observ !! Observation to write.
        integer,           intent(in)    :: unit   !! File unit.
        integer,           intent(in)    :: format !! Output format (`FORMAT_CSV`, `FORMAT_JSONL`).

        select case (format)
            case (FORMAT_CSV);   rc = dm_csv_write(observ, unit=unit, header=.false., separator=APP_CSV_SEPARATOR)
            case (FORMAT_JSONL); rc = dm_json_write(observ, unit=unit)
            case default;        rc = E_INVALID
        end select
    end function write_observ

    subroutine run(app)
        !! Performs jobs in job list.
        type(app_type), intent(inout) :: app !! App type.

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
                call logger%debug('no jobs left')
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

                ! Read observation from file system.
                rc = read_observ(observ, app%node_id, app%sensor_id, app%name, debug)
                call dm_observ_set(observ, error=rc)

                ! Forward observation via message queue.
                rc = dm_posix_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)

                ! Output observation.
                rc = output_observ(observ, app%output_type)

                if (debug) call logger%debug('finished observation ' // trim(observ%name) // ' of sensor ' // app%sensor_id, observ=observ)
            end do observ_loop

            if (debug) call logger%debug('finished job of observation group ' // dm_job_get_id(job))

            ! Wait delay time of the job if set (absolute).
            msec = max(0, job%delay)
            sec  = dm_msec_to_sec(sec)

            if (msec == 0) cycle
            if (debug) call logger%debug('next job in ' // dm_itoa(sec) // ' sec')
            call dm_msleep(msec)
        end do job_loop
    end subroutine run

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app !! App type.

        type(arg_parser_class) :: parser

        call parser%add('name',    short='n', type=ARG_TYPE_ID)      ! -n, --name <id>
        call parser%add('config',  short='c', type=ARG_TYPE_FILE, required=.true.) ! -c, --config <path>
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

        if (len_trim(app%output) > 0) then
            app%format = dm_format_from_name(app%format_name)

            if (trim(app%output) == '-') then
                app%output_type = OUTPUT_STDOUT
            else
                app%output_type = OUTPUT_FILE
            end if
        end if

        ! Validate options.
        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

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

        if (len_trim(app%logger) > 0 .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger')
            return
        end if

        if (len_trim(app%output) > 0 .and. (app%format /= FORMAT_CSV .and. app%format /= FORMAT_JSONL)) then
            call dm_error_out(rc, 'invalid or missing output format')
            return
        end if

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

        call logger%debug('exit on on signal ' // dm_posix_signal_name(signum))
        call logger%info('stopped ' // APP_NAME)
        call dm_stop(STOP_SUCCESS)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmfs
