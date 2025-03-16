! dmved.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmved
    !! VE.Direct protocol logger for MPPT solar chargers and SmartShunt battery
    !! monitors by Victron Energy.
    !!
    !! The following VE.Direct fields are captured:
    !!
    !! | Response | Unit    | Description                               | MPPT | Shunt |
    !! |----------|---------|-------------------------------------------|------|-------|
    !! | `alarm`  | –       | Alarm condition active (on/off).          |      |   ✓   |
    !! | `ar`     | –       | Alarm reason (decimal).                   |      |   ✓   |
    !! | `ce`     | mAh     | Consumed amp hours.                       |      |   ✓   |
    !! | `cs`     | –       | State of operation.                       |  ✓   |       |
    !! | `dm`     | ‰       | Mid-point deviation of the battery bank.  |      |   ✓   |
    !! | `err`    | –       | Error code.                               |  ✓   |       |
    !! | `h1`     | mAh     | Depth of the deepest discharge.           |      |   ✓   |
    !! | `h2`     | mAh     | Depth of the last discharge.              |      |   ✓   |
    !! | `h3`     | mAh     | Depth of the average discharge.           |      |   ✓   |
    !! | `h4`     | –       | Number of charge cycles.                  |      |   ✓   |
    !! | `h5`     | –       | Number of full discharges.                |      |   ✓   |
    !! | `h6`     | mAh     | Cumulative amp hours drawn.               |      |   ✓   |
    !! | `h7`     | mV      | Minimum main (battery) voltage.           |      |   ✓   |
    !! | `h8`     | mV      | Maximum main (battery) voltage.           |      |   ✓   |
    !! | `h9`     | sec     | Number of seconds since last full charge. |      |   ✓   |
    !! | `h10`    | –       | Number of automatic synchronisations.     |      |   ✓   |
    !! | `h11`    | –       | Number of low main voltage alarms.        |      |   ✓   |
    !! | `h12`    | –       | Number of high main voltage alarms.       |      |   ✓   |
    !! | `h15`    | mV      | Minimum auxiliary (battery) voltage.      |      |   ✓   |
    !! | `h16`    | mV      | Maximum auxiliary (battery) voltage.      |      |   ✓   |
    !! | `h17`    | kWh/100 | Amount of produced energy.                |      |   ✓   |
    !! | `h18`    | kWh/100 | Amount of consumed energy.                |      |   ✓   |
    !! | `h19`    | kWh/100 | Yield total (user resettable counter).    |  ✓   |       |
    !! | `h20`    | kWh/100 | Yield today.                              |  ✓   |       |
    !! | `h21`    | W       | Maximum power today.                      |  ✓   |       |
    !! | `h22`    | kWh/100 | Yield yesterday.                          |  ✓   |       |
    !! | `h23`    | W       | Maximum power yesterday.                  |  ✓   |       |
    !! | `hsds`   | –       | Day sequence number (0 to 364).           |  ✓   |       |
    !! | `i`      | mA      | Main or channel 1 battery current.        |  ✓   |   ✓   |
    !! | `il`     | mA      | Load current.                             |  ✓   |       |
    !! | `load`   | –       | Load output state (on/off).               |  ✓   |       |
    !! | `mon`    | –       | DC monitor mode.                          |      |   ✓   |
    !! | `mppt`   | –       | Tracker operation mode.                   |  ✓   |       |
    !! | `or`     | –       | Off reason.                               |  ✓   |       |
    !! | `p`      | W       | Instantaneous power.                      |      |   ✓   |
    !! | `ppv`    | W       | Panel power.                              |  ✓   |       |
    !! | `relay`  | –       | Relay state (on/off).                     |  ✓   |   ✓   |
    !! | `soc`    | ‰       | State-of-charge.                          |      |   ✓   |
    !! | `t`      | °C      | Battery temperature.                      |      |   ✓   |
    !! | `ttg`    | min     | Time-to-go.                               |      |   ✓   |
    !! | `v`      | mV      | Main or channel 1 (battery) voltage.      |  ✓   |   ✓   |
    !! | `vm`     | mV      | Mid-point voltage of the battery bank.    |      |   ✓   |
    !! | `vpv`    | mV      | Panel voltage.                            |  ✓   |       |
    !! | `vs`     | mV      | Auxiliary (starter) voltage.              |      |   ✓   |
    !!
    !! The MPPT observation will contain one request with 16 responses and the
    !! shunt observation two requests with 30 responses.
    !!
    !! The TTY is always configured to 19200 baud (8N1).
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmved'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 6

    integer, parameter :: APP_DUMP_UNIT   = 100    !! Unit of dump file.
    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Command-line arguments.
        character(len=ID_LEN)              :: name        = APP_NAME       !! Instance and configuration name (required).
        character(len=FILE_PATH_LEN)       :: config      = ' '            !! Path to configuration file (required).
        character(len=LOGGER_NAME_LEN)     :: logger      = ' '            !! Name of logger.
        character(len=NODE_ID_LEN)         :: node_id     = ' '            !! Node id (required).
        character(len=SENSOR_ID_LEN)       :: sensor_id   = ' '            !! Sensor id (required).
        character(len=TARGET_ID_LEN)       :: target_id   = ' '            !! Target id (required).
        character(len=FILE_PATH_LEN)       :: path        = ' '            !! Path of TTY/PTY device (required).
        character(len=FILE_PATH_LEN)       :: dump        = ' '            !! Path of file or named pipe to dump VE.Direct raw data to.
        character(len=OBSERV_RECEIVER_LEN) :: receiver    = ' '            !! Name of receiver's message queue (without leading `/`).
        character(len=VE_DEVICE_NAME_LEN)  :: device_name = 'none'         !! Device name (`mppt`, `shunt`).
        integer                            :: device      = VE_DEVICE_NONE !! Device enumerator (`VE_DEVICE_MPPT`, VE_DEVICE_SHUNT`).
        integer                            :: interval    = 60             !! Emit interval in seconds (>= 0).
        logical                            :: debug       = .false.        !! Forward debug messages via IPC.
        logical                            :: verbose     = .false.        !! Print debug messages to stderr (optional).
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App configuration.
    type(tty_type) :: tty ! TTY type.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger, &
                          node_id = app%node_id, &
                          source  = app%name, &
                          debug   = app%debug, &
                          ipc     = (len_trim(app%logger) > 0), &
                          verbose = app%verbose)

    ! Register signal handler.
    call dm_signal_register(signal_callback)

    ! Open dump file.
    rc = open_dump(app%dump)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Run main loop.
    rc = run(app, tty)

    ! Close dump file.
    call close_dump(app%dump)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function open_dump(path) result(rc)
        !! Opens dump file.
        character(len=*), intent(in) :: path !! Path of dump file.

        integer :: stat

        rc = E_NONE
        if (len_trim(path) == 0) return

        call logger%debug('opening dump file ' // path)
        open (action='write', file=trim(path), iostat=stat, position='append', status='unknown', unit=APP_DUMP_UNIT)
        if (stat /= 0) rc = E_IO
    end function open_dump

    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app !! App type.

        type(arg_type) :: args(13)

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <string>
            arg_type('config',   short='c', type=ARG_TYPE_FILE, required=.true.), & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),      & ! -l, --logger <string>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <string>
            arg_type('sensor',   short='S', type=ARG_TYPE_ID),      & ! -S, --sensor <string>
            arg_type('target',   short='T', type=ARG_TYPE_ID),      & ! -T, --target <string>
            arg_type('path',     short='p', type=ARG_TYPE_STRING),  & ! -p, --path <path>
            arg_type('dump',     short='o', type=ARG_TYPE_STRING),  & ! -o, --dump <path>
            arg_type('receiver', short='r', type=ARG_TYPE_ID,     max_len=OBSERV_RECEIVER_LEN), & ! -r, --receiver <string>
            arg_type('device',   short='d', type=ARG_TYPE_STRING, max_len=VE_DEVICE_NAME_LEN),  & ! -r, --receiver <string>
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
        call dm_arg_get(args( 7), app%path)
        call dm_arg_get(args( 8), app%dump)
        call dm_arg_get(args( 9), app%receiver)
        call dm_arg_get(args(10), app%device_name)
        call dm_arg_get(args(11), app%interval)
        call dm_arg_get(args(12), app%debug)
        call dm_arg_get(args(13), app%verbose)

        app%device = dm_ve_device_from_name(app%device_name)

        ! Validate options.
        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (len_trim(app%logger) > 0 .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger')
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

        if (len_trim(app%path) == 0) then
            call dm_error_out(rc, 'missing TTY path')
            return
        end if

        if (len_trim(app%receiver) > 0 .and. .not. dm_id_is_valid(app%receiver)) then
            call dm_error_out(rc, 'invalid receiver')
            return
        end if

        if (.not. dm_ve_is_valid_device(app%device)) then
            call dm_error_out(rc, 'invalid device')
            return
        end if

        if (app%interval < 0) then
            call dm_error_out(rc, 'invalid interval')
            return
        end if

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.
        type(config_type)             :: config

        rc = E_INVALID
        if (len_trim(app%config) == 0) return ! Fail-safe, should never occur.

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'logger',   app%logger)
            call dm_config_get(config, 'node',     app%node_id)
            call dm_config_get(config, 'sensor',   app%sensor_id)
            call dm_config_get(config, 'target',   app%target_id)
            call dm_config_get(config, 'path',     app%path)
            call dm_config_get(config, 'receiver', app%receiver)
            call dm_config_get(config, 'device',   app%device_name)
            call dm_config_get(config, 'dump',     app%dump)
            call dm_config_get(config, 'interval', app%interval)
            call dm_config_get(config, 'debug',    app%debug)
            call dm_config_get(config, 'verbose',  app%verbose)
        end if

        call dm_config_close(config)
    end function read_config

    integer function run(app, tty) result(rc)
        !! Connects to TTY and runs an event loop to read VE.Direct frames to
        !! responses. The observation is then forwarded via message queue to
        !! the specified receiver.
        type(app_type), intent(inout) :: app !! App type.
        type(tty_type), intent(inout) :: tty !! TTY type.

        character(len=VE_PRODUCT_NAME_LEN) :: product

        character        :: byte
        integer          :: errors(VE_NFIELDS)
        integer          :: code, code_last, field_type, pid, stat
        integer(kind=i8) :: epoch_last, epoch_now
        logical          :: debug, dump, eor, finished, valid
        logical          :: has_pid, has_receiver

        type(observ_type)   :: observ
        type(ve_frame_type) :: frame
        type(ve_frame_type) :: frames(VE_NFIELDS)
        type(response_type) :: responses(VE_NFIELDS)

        call logger%info('started ' // APP_NAME)

        ! Set initial values.
        code_last    = 0
        epoch_last   = 0_i8
        debug        = (app%debug .or. app%verbose)
        dump         = (len_trim(app%dump) > 0)
        has_pid      = .false.
        has_receiver = (len_trim(app%receiver) > 0)

        ! Set serial port parameters.
        call dm_tty_set(tty       = tty,              &
                        path      = app%path,         &
                        access    = VE_TTY_ACCESS,    &
                        baud_rate = VE_TTY_BAUD_RATE, &
                        byte_size = VE_TTY_BYTE_SIZE, &
                        parity    = VE_TTY_PARITY,    &
                        stop_bits = VE_TTY_STOP_BITS)

        ! Try to open TTY.
        call logger%debug('opening TTY ' // trim(app%path) // ' to MPPT ' // app%sensor_id)

        do
            rc = dm_tty_open(tty)
            if (dm_is_ok(rc)) exit

            call logger%error('failed to open TTY ' // trim(app%path) // ', next attempt in 5 sec', error=rc)
            call dm_sleep(5)
        end do

        tty_loop: do
            ! Read single byte from TTY.
            rc = dm_tty_read_byte(tty, byte)

            if (dm_is_error(rc)) then
                call logger%error('failed to read byte from TTY ' // app%path, error=rc)
                exit tty_loop
            end if

            ! Dump byte to file.
            if (dump) then
                write (APP_DUMP_UNIT, '(a1)', advance='no', iostat=stat) byte
                if (stat /= 0) call logger%error('failed to write to dump file', error=E_IO)
            end if

            ! Fill VE.Direct protocol frame.
            call dm_ve_frame_next(frame, byte, eor, finished, valid)

            ! VE.Direct block finished.
            if (finished) then
                mqueue_block: block
                    ! Check if block is valid.
                    if (.not. valid) then
                        if (debug) call logger%debug('checksum error detected, discarding block', error=E_CORRUPT)
                        exit mqueue_block
                    end if

                    ! Check if interval time has been reached.
                    epoch_now = dm_time_unix()
                    if (epoch_last + app%interval > epoch_now) exit mqueue_block
                    epoch_last = epoch_now

                    ! Check if message passing is enabled.
                    if (.not. has_receiver) then
                        if (debug) call logger%debug('no receiver specified, skipping observation forwarding')
                        exit mqueue_block
                    end if

                    ! Convert all frames to responses.
                    call dm_ve_frame_read(frames, responses, error=errors)
                    if (any(dm_is_error(errors))) call logger%warning('failed to convert frame to response', error=maxval(errors))

                    ! Create and forward observation.
                    call create_observ(observ, app, responses)
                    rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
                end block mqueue_block

                call dm_ve_frame_reset(frame)
                cycle tty_loop
            end if

            ! VE.Direct frame finished.
            if (eor) then
                if (frame%label == 'BMV')  cycle tty_loop ! Ignore deprecated model description.
                if (frame%label == 'SER#') cycle tty_loop ! Ignore serial number string.

                ! Log VE.Direct device error.
                if (frame%label == 'ERR') then
                    code = dm_atoi(frame%value)

                    ! Create only one log message for the same error code.
                    if (dm_ve_is_error(code) .and. code /= code_last) then
                        call logger%warning(dm_ve_error_message(code), error=E_SENSOR)
                        code_last = code
                    end if
                end if

                ! Output product name of connected VE device from PID.
                if (frame%label == 'PID' .and. .not. has_pid) then
                    call dm_string_hex_to_int(frame%value, pid)
                    rc = dm_ve_product_name(pid, product)

                    if (dm_is_error(rc)) then
                        call logger%warning('connected to unknown Victron Energy device', error=rc)
                    else
                        call logger%info('connected to Victron Energy ' // product)
                    end if

                    has_pid = .true.
                end if

                ! Validate VE.Direct field type.
                field_type = dm_ve_field_type(frame%label)

                if (.not. dm_ve_is_valid_field_type(field_type)) then
                    call logger%warning('received invalid or unsupported field ' // frame%label, error=rc)
                    cycle tty_loop
                end if

                if (debug) call logger%debug('received field ' // trim(frame%label) // ': ' // frame%value)

                ! Save VE.Direct frame.
                frames(field_type) = frame
            end if
        end do tty_loop

        ! Close TTY.
        if (dm_tty_is_connected(tty)) then
            call logger%debug('closing TTY ' // app%path)
            call dm_tty_close(tty)
        end if
    end function run

    subroutine close_dump(path)
        !! Closes dump file.
        character(len=*), intent(in) :: path !! Path of dump file.

        if (len_trim(path) == 0) return
        call logger%debug('closing dump file ' // path)
        close (APP_DUMP_UNIT)
    end subroutine close_dump

    subroutine create_observ(observ, app, responses)
        !! Creates new observation from VE.Direct responses.
        type(observ_type),   intent(out)   :: observ       !! Created observation.
        type(app_type),      intent(inout) :: app          !! App type.
        type(response_type), intent(inout) :: responses(:) !! All captured VE.Direct responses.

        character(len=TIME_LEN) :: timestamp
        integer                 :: rc
        type(request_type)      :: requests(2)

        timestamp = dm_time_now()

        ! Prepare observation.
        call dm_observ_set(observ    = observ,        &
                           id        = dm_uuid4(),    &
                           node_id   = app%node_id,   &
                           sensor_id = app%sensor_id, &
                           target_id = app%target_id, &
                           timestamp = timestamp,     &
                           source    = app%name,      &
                           device    = app%path)

        rc = dm_observ_add_receiver(observ, app%receiver)

        ! Prepare requests.
        call dm_request_set(requests, name='ved', timestamp=timestamp, delay=dm_sec_to_msec(app%interval))

        select case (app%device)
            case (VE_DEVICE_MPPT)
                ! BlueSolar/SmartSolar MPPT.
                observ%name = 'ved_mppt'

                rc = dm_request_add(requests(1), responses(VE_FIELD_CS))    ! 1
                rc = dm_request_add(requests(1), responses(VE_FIELD_ERR))   ! 2
                rc = dm_request_add(requests(1), responses(VE_FIELD_H19))   ! 3
                rc = dm_request_add(requests(1), responses(VE_FIELD_H20))   ! 4
                rc = dm_request_add(requests(1), responses(VE_FIELD_H21))   ! 5
                rc = dm_request_add(requests(1), responses(VE_FIELD_H22))   ! 6
                rc = dm_request_add(requests(1), responses(VE_FIELD_H23))   ! 7
                rc = dm_request_add(requests(1), responses(VE_FIELD_HSDS))  ! 8
                rc = dm_request_add(requests(1), responses(VE_FIELD_I))     ! 9
                rc = dm_request_add(requests(1), responses(VE_FIELD_IL))    ! 10
                rc = dm_request_add(requests(1), responses(VE_FIELD_LOAD))  ! 11
                rc = dm_request_add(requests(1), responses(VE_FIELD_MPPT))  ! 12
                rc = dm_request_add(requests(1), responses(VE_FIELD_OR))    ! 13
                rc = dm_request_add(requests(1), responses(VE_FIELD_PPV))   ! 14
                rc = dm_request_add(requests(1), responses(VE_FIELD_V))     ! 15
                rc = dm_request_add(requests(1), responses(VE_FIELD_VPV))   ! 16

                rc = dm_observ_add_request(observ, requests(1))

            case (VE_DEVICE_SHUNT)
                ! SmartShunt battery monitor.
                observ%name = 'ved_shunt'

                rc = dm_request_add(requests(1), responses(VE_FIELD_ALARM)) ! 1
                rc = dm_request_add(requests(1), responses(VE_FIELD_AR))    ! 2
                rc = dm_request_add(requests(1), responses(VE_FIELD_CE))    ! 3
                rc = dm_request_add(requests(1), responses(VE_FIELD_DM))    ! 4
                rc = dm_request_add(requests(1), responses(VE_FIELD_H1))    ! 5
                rc = dm_request_add(requests(1), responses(VE_FIELD_H2))    ! 6
                rc = dm_request_add(requests(1), responses(VE_FIELD_H3))    ! 7
                rc = dm_request_add(requests(1), responses(VE_FIELD_H4))    ! 8
                rc = dm_request_add(requests(1), responses(VE_FIELD_H5))    ! 9
                rc = dm_request_add(requests(1), responses(VE_FIELD_H6))    ! 10
                rc = dm_request_add(requests(1), responses(VE_FIELD_H7))    ! 11
                rc = dm_request_add(requests(1), responses(VE_FIELD_H8))    ! 12
                rc = dm_request_add(requests(1), responses(VE_FIELD_H9))    ! 13
                rc = dm_request_add(requests(1), responses(VE_FIELD_H10))   ! 14
                rc = dm_request_add(requests(1), responses(VE_FIELD_H11))   ! 15
                rc = dm_request_add(requests(1), responses(VE_FIELD_H12))   ! 16

                rc = dm_request_add(requests(2), responses(VE_FIELD_H15))   ! 1
                rc = dm_request_add(requests(2), responses(VE_FIELD_H16))   ! 2
                rc = dm_request_add(requests(2), responses(VE_FIELD_H17))   ! 3
                rc = dm_request_add(requests(2), responses(VE_FIELD_H18))   ! 4
                rc = dm_request_add(requests(2), responses(VE_FIELD_I))     ! 5
                rc = dm_request_add(requests(2), responses(VE_FIELD_MON))   ! 6
                rc = dm_request_add(requests(2), responses(VE_FIELD_P))     ! 7
                rc = dm_request_add(requests(2), responses(VE_FIELD_RELAY)) ! 8
                rc = dm_request_add(requests(2), responses(VE_FIELD_SOC))   ! 9
                rc = dm_request_add(requests(2), responses(VE_FIELD_T))     ! 10
                rc = dm_request_add(requests(2), responses(VE_FIELD_TTG))   ! 11
                rc = dm_request_add(requests(2), responses(VE_FIELD_V))     ! 12
                rc = dm_request_add(requests(2), responses(VE_FIELD_VM))    ! 13
                rc = dm_request_add(requests(2), responses(VE_FIELD_VS))    ! 14

                rc = dm_observ_add_request(observ, requests(1))
                rc = dm_observ_add_request(observ, requests(2))
        end select
    end subroutine create_observ

    subroutine signal_callback(signum) bind(c)
        !! Default POSIX signal handler of the program.
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        call logger%info('exit on signal ' // dm_signal_name(signum))

        if (dm_tty_is_connected(tty)) then
            call logger%debug('closing TTY ' // app%path)
            call dm_tty_close(tty)
        end if

        call close_dump(app%dump)

        call dm_stop(STOP_SUCCESS)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmved
