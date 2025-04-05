! dmmb.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmmb
    !! Reads observations from device connected via Modbus RTU/TCP.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmmb'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 7

    character, parameter :: APP_CSV_SEPARATOR = ','    !! CSV field separator.
    logical,   parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    integer, parameter :: OUTPUT_NONE   = 0 !! No output.
    integer, parameter :: OUTPUT_STDOUT = 1 !! Output to standard output.
    integer, parameter :: OUTPUT_FILE   = 2 !! Output to file.

    type :: app_rtu_type
        !! Modbus RTU settings.
        character(len=FILE_PATH_LEN) :: path      = ' '             !! Path.
        integer                      :: baud_rate = TTY_B19200      !! Baud rate.
        integer                      :: byte_size = TTY_BYTE_SIZE8  !! Byte size.
        integer                      :: parity    = TTY_PARITY_EVEN !! Parity name.
        integer                      :: stop_bits = TTY_STOP_BITS1  !! Stop bits.
    end type app_rtu_type

    type :: app_tcp_type
        !! Modbus TCP settings.
        character(len=NET_IPV4_LEN) :: address = ' ' !! IPv4 address.
        integer                     :: port    = 0   !! Port.
    end type app_tcp_type

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)          :: name        = APP_NAME         !! Instance and configuration name (required).
        character(len=FILE_PATH_LEN)   :: config      = ' '              !! Path to configuration file (required).
        character(len=LOGGER_NAME_LEN) :: logger      = ' '              !! Name of logger.
        character(len=NODE_ID_LEN)     :: node_id     = ' '              !! Node id (required).
        character(len=SENSOR_ID_LEN)   :: sensor_id   = ' '              !! Sensor id (required).
        character(len=FILE_PATH_LEN)   :: output      = ' '              !! Path of output file.
        integer                        :: output_type = OUTPUT_NONE      !! Output type.
        character(len=FORMAT_NAME_LEN) :: format_name = ' '              !! Output format name.
        integer                        :: format      = FORMAT_NONE      !! Output format.
        integer                        :: mode        = MODBUS_MODE_NONE !! Modbus RTU or TCP.
        logical                        :: debug       = .false.          !! Forward debug messages via IPC.
        logical                        :: verbose     = .false.          !! Print debug messages to stderr.
        type(app_rtu_type)             :: rtu                            !! Modbus RTU settings.
        type(app_tcp_type)             :: tcp                            !! Modbus TCP settings.
        type(job_list_type)            :: jobs                           !! Job list.
    end type app_type

    integer                        :: rc         ! Return code.
    type(app_type)                 :: app        ! App settings.
    type(modbus_rtu_type), target  :: modbus_rtu ! Modbus RTU type.
    type(modbus_tcp_type), target  :: modbus_tcp ! Modbus TCP type.
    class(modbus_type),    pointer :: modbus     ! Modbus pointer.
    class(logger_class),   pointer :: logger     ! Logger object.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments and read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,                 &
                          node_id = app%node_id,                &
                          source  = app%name,                   &
                          debug   = app%debug,                  &
                          ipc     = (len_trim(app%logger) > 0), &
                          verbose = app%verbose)

    init_block: block
        ! Create Modbus context.
        if (app%mode == MODBUS_MODE_RTU) then
            rc = dm_modbus_create(modbus    = modbus_rtu,        &
                                  path      = app%rtu%path,      &
                                  baud_rate = app%rtu%baud_rate, &
                                  byte_size = app%rtu%byte_size, &
                                  parity    = app%rtu%parity,    &
                                  stop_bits = app%rtu%stop_bits)
            modbus => modbus_rtu
        else
            rc = dm_modbus_create(modbus_tcp, app%tcp%address, app%tcp%port)
            modbus => modbus_tcp
        end if

        if (dm_is_error(rc)) then
            call logger%error('failed to create Modbus context: ' // dm_modbus_error_message(), error=rc)
            exit init_block
        end if

        ! Register signal handler.
        call dm_signal_register(signal_callback)

        ! Run main loop.
        rc = run(app, modbus)
    end block init_block

    call halt(rc)
contains
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

    integer function read_observ(app, modbus, observ, debug) result(rc)
        !! Sends requests sequentially to sensor and reads responses.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the observation contains no requests.
        !!
        type(app_type),    intent(inout) :: app    !! App type.
        type(modbus_type), intent(inout) :: modbus !! Modbus context type.
        type(observ_type), intent(inout) :: observ !! Observation to read.
        logical,           intent(in)    :: debug  !! Log debug messages.

        integer :: msec, sec
        integer :: i, n

        rc = E_EMPTY

        ! Initialise observation.
        call dm_observ_set(observ    = observ,        &
                           id        = dm_uuid4(),    &
                           node_id   = app%node_id,   &
                           sensor_id = app%sensor_id, &
                           source    = app%name,      &
                           timestamp = dm_time_now())

        if (app%mode == MODBUS_MODE_RTU) then
            call dm_observ_set(observ, device=app%rtu%path)
        else
            call dm_observ_set(observ, device=trim(app%tcp%address) // ':' // dm_itoa(app%tcp%port))
        end if

        n = observ%nrequests

        if (n == 0) then
            call logger%info('no requests in observ ' // observ%name, observ=observ)
            return
        end if

        ! Send requests sequentially to sensor.
        request_loop: do i = 1, n
            associate (request => observ%requests(i))
                if (debug) call logger%debug('starting ' // request_name_string(observ, request) // ' (' // dm_itoa(i) // '/' // dm_itoa(n) // ')', observ=observ)

                rc = read_request(modbus, observ, request, debug)
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

    integer function read_request(modbus, observ, request, debug) result(rc)
        type(modbus_type),  intent(inout) :: modbus  !! Modbus context type.
        type(observ_type),  intent(inout) :: observ  !! Observation type.
        type(request_type), intent(inout) :: request !! Request type.
        logical,            intent(in)    :: debug   !! Log debug messages.

        type(modbus_register_type) :: register

        rc = E_NONE

        ! Return if request is disabled.
        if (request%state == REQUEST_STATE_DISABLED) then
            if (debug) call logger%debug(request_name_string(observ, request) // ' is disabled', observ=observ)
            return
        end if

        ! Prepare request.
        call dm_request_set(request, timestamp=dm_time_now())
        if (register%access == MODBUS_ACCESS_READ) call dm_response_set(request%responses(1), error=E_INCOMPLETE)

        ! Parse request string for Modbus register.
        call dm_modbus_register_parse(request%request, register, error=rc)

        if (dm_is_error(rc)) then
            call logger%error('failed to parse Modbus parameters in ' // request_name_string(observ, request), observ=observ, error=rc)
            return
        end if

        if (.not. dm_modbus_register_is_valid(register)) then
            rc = E_INVALID
            call logger%error('invalid Modbus parameters in ' // request_name_string(observ, request), observ=observ, error=rc)
            return
        end if

        ! Set slave device.
        rc = dm_modbus_set_slave(modbus, slave=register%slave)

        if (dm_is_error(rc)) then
            call logger%error('failed to set slave id to ' // dm_itoa(register%slave), observ=observ, error=rc)
            return
        end if

        ! Read or write value.
        select case (register%access)
            case (MODBUS_ACCESS_READ)
                if (debug) call logger%debug('reading value from register address ' // dm_itoa(register%address))
                rc = read_value(modbus, register, request, debug)

                if (dm_is_error(rc)) then
                    call logger%error('failed to read value from register address ' // dm_itoa(register%address) // ':' // dm_modbus_error_message(), error=rc)
                    return
                end if

            case (MODBUS_ACCESS_WRITE)
                if (debug) call logger%debug('writing value to register address ' // dm_itoa(register%address))
                rc = write_value(modbus, register)

                if (dm_is_error(rc)) then
                    call logger%error('failed to write value to register address ' // dm_itoa(register%address) // ':' // dm_modbus_error_message(), error=rc)
                    return
                end if

            case (MODBUS_ACCESS_NONE)
                rc = E_INVALID
                call logger%error('invalid Modbus access type in ' // request_name_string(observ, request), error=rc)
                return
        end select
    end function read_request

    integer function read_value(modbus, register, request, debug) result(rc)
        !! Reads value from register and stores it in the first response of the request.
        character(len=*), parameter :: FMT_INT  = '(i0)'
        character(len=*), parameter :: FMT_REAL = '(f0.8)'

        class(modbus_type),         intent(inout) :: modbus   !! Modbus context type.
        type(modbus_register_type), intent(inout) :: register !! Modbus register type.
        type(request_type),         intent(inout) :: request  !! Observation request.
        logical,                    intent(in)    :: debug    !! Log debug messages.

        character(len=REQUEST_RESPONSE_LEN) :: raw
        integer                             :: stat
        real(kind=r8)                       :: value

        integer(kind=i2) :: i16
        integer(kind=i4) :: i32
        integer(kind=i8) :: i64
        real(kind=r4)    :: r32

        type_select: select case (register%type)
            case (MODBUS_TYPE_INT16)
                rc = dm_modbus_read_int16(modbus, register%address, i16)
                if (dm_is_error(rc)) exit type_select
                write (raw, FMT_INT, iostat=stat) i16
                value = dm_to_real64(i16)

            case (MODBUS_TYPE_INT32)
                rc = dm_modbus_read_int32(modbus, register%address, i32)
                if (dm_is_error(rc)) exit type_select
                write (raw, FMT_INT, iostat=stat) i32
                value = dm_to_real64(i32)

            case (MODBUS_TYPE_UINT16)
                rc = dm_modbus_read_uint16(modbus, register%address, i32)
                if (dm_is_error(rc)) exit type_select
                write (raw, FMT_INT, iostat=stat) i32
                value = dm_to_real64(i32)

            case (MODBUS_TYPE_UINT32)
                rc = dm_modbus_read_uint32(modbus, register%address, i64)
                if (dm_is_error(rc)) exit type_select
                write (raw, FMT_INT, iostat=stat) i64
                value = dm_to_real64(i64)

            case (MODBUS_TYPE_FLOAT)
                rc = dm_modbus_read_float(modbus, register%address, r32, register%order)
                if (dm_is_error(rc)) exit type_select
                write (raw, FMT_REAL, iostat=stat) r32
                value = dm_to_real64(r32)

            case default
                rc   = E_INVALID
                stat = 0
        end select type_select

        if (stat /= 0) rc = E_WRITE

        associate (response => request%responses(1))
            call dm_request_set(request, raw_response=raw)
            call dm_response_set(response, error=rc)

            if (dm_is_error(rc)) return

            if (dm_modbus_register_has_scale(register)) then
                call dm_modbus_register_scale(register, value)
                if (debug) call logger%debug('scaled value by ' // dm_itoa(register%scale))
            end if

            call dm_response_set(response, value=value)
        end associate
    end function read_value

    function request_name_string(observ, request) result(string)
        !! Returns string of observation and request name for logging.
        type(observ_type),  intent(inout) :: observ  !! Observation type.
        type(request_type), intent(inout) :: request !! Request type.
        character(len=:), allocatable     :: string  !! Result.

        string = 'request ' // trim(request%name) // ' of observ ' // trim(observ%name)
    end function request_name_string

    integer function run(app, modbus) result(rc)
        !! Performs jobs in job list.
        type(app_type),    intent(inout) :: app    !! App type.
        type(modbus_type), intent(inout) :: modbus !! Modbus context type.

        integer        :: msec, sec
        integer        :: njobs
        logical        :: debug
        type(job_type) :: job

        debug = (app%debug .or. app%verbose)
        call logger%info('started ' // APP_NAME)

        ! Create Modbus connection.
        if (app%mode == MODBUS_MODE_RTU) then
            call logger%debug('connecting to Modbus RTU device ' // trim(app%rtu%path))
        else
            call logger%debug('connecting to Modbus TCP device ' // trim(app%tcp%address) // ':' // dm_itoa(app%tcp%port))
        end if

        rc = dm_modbus_connect(modbus)

        if (dm_is_error(rc)) then
            call logger%error('failed to create Modbus connection', error=rc)
            return
        end if

        job_loop: do
            ! Get number of jobs left.
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

            observ_block: associate (observ => job%observ)
                if (.not. job%valid) exit observ_block
                if (debug) call logger%debug('starting observ ' // trim(observ%name) // ' for sensor ' // app%sensor_id, observ=observ)

                ! Read observation from TTY.
                rc = read_observ(app, modbus, observ, debug)
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

    integer function write_value(modbus, register) result(rc)
        !! Writes value to register.
        class(modbus_type),         intent(inout) :: modbus   !! Modbus context type.
        type(modbus_register_type), intent(inout) :: register !! Modbus register type.

        select case (register%type)
            case (MODBUS_TYPE_INT16);  rc = dm_modbus_write_int16 (modbus, register%address, int(register%value, kind=i2))
            case (MODBUS_TYPE_INT32);  rc = dm_modbus_write_int32 (modbus, register%address, register%value)
            case (MODBUS_TYPE_UINT16); rc = dm_modbus_write_uint16(modbus, register%address, register%value)
            case (MODBUS_TYPE_UINT32); rc = dm_modbus_write_uint32(modbus, register%address, int(register%value, kind=i8))
            case default;              rc = E_INVALID
        end select
    end function write_value

    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: stat

        stat = dm_btoi(dm_is_error(error), STOP_FAILURE, STOP_SUCCESS)

        call dm_modbus_close(modbus)
        call dm_modbus_destroy(modbus)

        call dm_stop(stat)
    end subroutine halt

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app

        type(arg_type) :: args(9)

        args = [ &
            arg_type('name',    short='n', type=ARG_TYPE_ID),                    & ! -n, --name <string>
            arg_type('config',  short='c', type=ARG_TYPE_FILE, required=.true.), & ! -c, --config <path>
            arg_type('logger',  short='l', type=ARG_TYPE_ID),                    & ! -l, --logger <string>
            arg_type('node',    short='N', type=ARG_TYPE_ID),                    & ! -N, --node <string>
            arg_type('sensor',  short='S', type=ARG_TYPE_ID),                    & ! -S, --sensor <string>
            arg_type('output',  short='o', type=ARG_TYPE_STRING),                & ! -o, --output <string>
            arg_type('format',  short='f', type=ARG_TYPE_STRING),                & ! -f, --format <string>
            arg_type('debug',   short='D', type=ARG_TYPE_LOGICAL),               & ! -D, --debug
            arg_type('verbose', short='V', type=ARG_TYPE_LOGICAL)                & ! -V, --verbose
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

            if (app%format /= FORMAT_CSV .and. app%format /= FORMAT_JSONL) then
                call dm_error_out(rc, 'invalid or missing output format')
                return
            end if

            app%output_type = OUTPUT_FILE
            if (trim(app%output) == '-') app%output_type = OUTPUT_STDOUT
        end if

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.

        character(len=MODBUS_MODE_NAME_LEN) :: mode_name
        character(len=TTY_PARITY_NAME_LEN)  :: parity_name
        integer                             :: baud_rate, byte_size, stop_bits
        type(config_type)                   :: config

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'logger',  app%logger)
            call dm_config_get(config, 'node',    app%node_id)
            call dm_config_get(config, 'sensor',  app%sensor_id)
            call dm_config_get(config, 'output',  app%output)
            call dm_config_get(config, 'format',  app%format_name)
            call dm_config_get(config, 'mode',    mode_name)
            call dm_config_get(config, 'debug',   app%debug)
            call dm_config_get(config, 'verbose', app%verbose)
            call dm_config_get(config, 'jobs',    app%jobs)

            ! Modbus RTU.
            if (dm_is_ok(dm_config_field(config, 'rtu'))) then
                call dm_config_get(config, 'path',     app%rtu%path)
                call dm_config_get(config, 'baudrate', baud_rate)
                call dm_config_get(config, 'bytesize', byte_size)
                call dm_config_get(config, 'parity',   parity_name)
                call dm_config_get(config, 'stopbits', stop_bits)
                call dm_config_remove(config)
            end if

            ! Modbus TCP.
            if (dm_is_ok(dm_config_field(config, 'tcp'))) then
                call dm_config_get(config, 'address', app%tcp%address)
                call dm_config_get(config, 'port',    app%tcp%port)
                call dm_config_remove(config)
            end if
        end if

        call dm_config_close(config)
        if (dm_is_error(rc)) return

        ! Validate Modbus settings.
        rc = E_INVALID

        app%mode = dm_modbus_mode_from_name(mode_name)

        if (.not. dm_modbus_mode_is_valid(app%mode)) then
            call dm_error_out(rc, 'invalid Modbus mode ' // mode_name)
            return
        end if

        if (app%mode == MODBUS_MODE_RTU) then
            ! Modbus RTU.
            app%rtu%baud_rate = dm_tty_baud_rate_from_value(baud_rate)
            app%rtu%byte_size = dm_tty_byte_size_from_value(byte_size)
            app%rtu%parity    = dm_tty_parity_from_name(parity_name)
            app%rtu%stop_bits = dm_tty_stop_bits_from_value(stop_bits)

            if (.not. dm_tty_baud_rate_is_valid(app%rtu%baud_rate)) then
                call dm_error_out(rc, 'invalid baud rate')
                return
            end if

            if (.not. dm_tty_byte_size_is_valid(app%rtu%byte_size)) then
                call dm_error_out(rc, 'invalid byte size')
                return
            end if

            if (.not. dm_tty_parity_is_valid(app%rtu%parity)) then
                call dm_error_out(rc, 'invalid parity')
                return
            end if

            if (.not. dm_tty_stop_bits_is_valid(app%rtu%stop_bits)) then
                call dm_error_out(rc, 'invalid stop bits')
                return
            end if

            if (len_trim(app%rtu%path) == 0) then
                call dm_error_out(rc, 'TTY path is required for Modbus RTU')
                return
            end if

            if (.not. dm_file_exists(app%rtu%path)) then
                rc = E_NOT_FOUND
                call dm_error_out(rc, 'TTY ' // trim(app%rtu%path) // ' does not exist')
                return
            end if
        else
            ! Modbus TCP.
            if (len_trim(app%tcp%address) == 0) then
                call dm_error_out(rc, 'IPv4 address is required for Modbus TCP')
                return
            end if

            if (.not. dm_net_ipv4_is_valid(app%tcp%address)) then
                call dm_error_out(rc, 'invalid IPv4 address')
                return
            end if

            if (app%tcp%port < 1) then
                call dm_error_out(rc, 'invalid port')
                return
            end if
        end if

        rc = E_NONE
    end function read_config

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine signal_callback(signum) bind(c)
        !! Default POSIX signal handler of the program.
        integer(kind=c_int), intent(in), value :: signum

        call logger%info('exit on signal ' // dm_signal_name(signum))
        call halt(E_NONE)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmmb
