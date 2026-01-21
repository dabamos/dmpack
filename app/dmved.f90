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
    !! The MPPT observation will contain 16 responses and the shunt observation
    !! 30 responses.
    !!
    !! The TTY is always configured to 19200 baud (8N1).
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmved'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    integer, parameter :: APP_DUMP_UNIT   = 100    !! Unit of dump file.
    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Command-line arguments.
        character(ID_LEN)              :: name        = APP_NAME       !! Instance and configuration name (required).
        character(FILE_PATH_LEN)       :: config      = ' '            !! Path to configuration file (required).
        character(LOGGER_NAME_LEN)     :: logger      = ' '            !! Name of logger.
        character(NODE_ID_LEN)         :: node_id     = ' '            !! Node id (required).
        character(SENSOR_ID_LEN)       :: sensor_id   = ' '            !! Sensor id (required).
        character(TARGET_ID_LEN)       :: target_id   = ' '            !! Target id (required).
        character(FILE_PATH_LEN)       :: path        = ' '            !! Path of TTY/PTY device (required).
        character(FILE_PATH_LEN)       :: dump        = ' '            !! Path of file or named pipe to dump VE.Direct raw data to.
        character(OBSERV_RECEIVER_LEN) :: receiver    = ' '            !! Name of receiver's message queue (without leading `/`).
        character(VE_DEVICE_NAME_LEN)  :: device_name = 'none'         !! Device name (`mppt`, `shunt`).
        integer                        :: device      = VE_DEVICE_NONE !! Device enumerator (`VE_DEVICE_MPPT`, VE_DEVICE_SHUNT`).
        integer                        :: interval    = 60             !! Emit interval in seconds (>= 0).
        logical                        :: debug       = .false.        !! Forward debug messages via IPC.
        logical                        :: verbose     = .false.        !! Print debug messages to stderr (optional).
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
    call logger%configure(name    = app%logger,  & ! Name of logger process.
                          node_id = app%node_id, & ! Node id.
                          source  = app%name,    & ! Log source.
                          debug   = app%debug,   & ! Forward DEBUG messages via IPC.
                          ipc     = .true.,      & ! Enable IPC (if logger is set).
                          verbose = app%verbose)   ! Print logs to standard error.
    call logger%info('started ' // APP_NAME)

    call dm_signal_register(signal_callback)
    rc = run(app, tty)
    call shutdown(rc)
contains
    integer function open_dump(path) result(rc)
        !! Opens dump file.
        character(*), intent(in) :: path !! Path of dump file.

        integer :: stat

        rc = E_NONE
        if (.not. dm_string_has(path)) return

        open (action='write', file=trim(path), iostat=stat, position='append', status='unknown', unit=APP_DUMP_UNIT)

        if (stat /= 0) then
            rc = E_IO
            call logger%error('failed to open dump file ' // path)
            return
        end if

        call logger%debug('openend dump file ' // path)
    end function open_dump

    integer function run(app, tty) result(rc)
        !! Connects to TTY and runs an event loop to read VE.Direct frames to
        !! responses. The observation is then forwarded via message queue to
        !! the specified receiver.
        type(app_type), intent(inout) :: app !! App type.
        type(tty_type), intent(inout) :: tty !! TTY type.

        character(VE_PRODUCT_NAME_LEN) :: product_name

        character   :: byte
        integer     :: errors(VE_NFIELDS)
        integer     :: code, code_last, field_type, pid, stat
        integer(i8) :: epoch_last, epoch_now
        logical     :: debug, dump, eor, finished, valid
        logical     :: has_pid, has_receiver

        type(observ_type)   :: observ
        type(ve_frame_type) :: frame
        type(ve_frame_type) :: frames(VE_NFIELDS)
        type(response_type) :: responses(VE_NFIELDS)

        ! Open dump file.
        rc = open_dump(app%dump)
        if (dm_is_error(rc)) return

        ! Set initial values.
        code_last    = 0
        epoch_last   = 0_i8
        debug        = (app%debug .or. app%verbose)
        dump         = dm_string_has(app%dump)
        has_pid      = .false.
        has_receiver = dm_string_has(app%receiver)

        ! Set serial port parameters.
        call dm_tty_set(tty       = tty,              &
                        path      = app%path,         &
                        access    = VE_TTY_ACCESS,    &
                        baud_rate = VE_TTY_BAUD_RATE, &
                        byte_size = VE_TTY_BYTE_SIZE, &
                        parity    = VE_TTY_PARITY,    &
                        stop_bits = VE_TTY_STOP_BITS)

        ! Try to open TTY.
        do
            rc = dm_tty_open(tty)
            if (dm_is_ok(rc)) exit

            call logger%error('failed to open TTY ' // trim(app%path) // ', next attempt in 30 sec', error=rc)
            call dm_sleep(30)
        end do

        call logger%debug('opened TTY ' // trim(app%path) // ' connected to ' // app%sensor_id)

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
                    if (any(dm_is_error(errors))) call logger%debug('failed to convert frame to response', error=maxval(errors))

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
                    rc = dm_ve_product_name(pid, product_name)

                    if (dm_is_error(rc)) then
                        call logger%info('connected to unknown Victron Energy device', error=rc)
                    else
                        call logger%info('connected to Victron Energy ' // product_name)
                    end if

                    has_pid = .true.
                end if

                ! Validate VE.Direct field type.
                field_type = dm_ve_field_type(frame%label)

                if (.not. dm_ve_field_type_is_valid(field_type)) then
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
            call dm_tty_close(tty)
            call logger%debug('closed TTY ' // app%path)
        end if
    end function run

    subroutine close_dump(path)
        !! Closes dump file.
        character(*), intent(in) :: path !! Path of dump file.

        if (.not. dm_string_has(path)) return
        close (APP_DUMP_UNIT)
        call logger%debug('closed dump file ' // path)
    end subroutine close_dump

    subroutine create_observ(observ, app, responses)
        !! Creates new observation from VE.Direct responses.
        type(observ_type),   intent(out)   :: observ       !! Created observation.
        type(app_type),      intent(inout) :: app          !! App type.
        type(response_type), intent(inout) :: responses(:) !! All captured VE.Direct responses.

        integer :: rc

        ! Prepare observation.
        call dm_observ_set(observ    = observ,        &
                           id        = dm_uuid4(),    &
                           node_id   = app%node_id,   &
                           sensor_id = app%sensor_id, &
                           target_id = app%target_id, &
                           timestamp = dm_time_now(), &
                           source    = app%name,      &
                           device    = app%path,      &
                           delay     = dm_sec_to_msec(app%interval))
        rc = dm_observ_add_receiver(observ, app%receiver)

        select case (app%device)
            case (VE_DEVICE_MPPT)
                ! BlueSolar/SmartSolar MPPT.
                observ%name = 'ved_mppt'

                rc = dm_observ_add_response(observ, responses(VE_FIELD_CS))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_ERR))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H19))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H20))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H21))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H22))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H23))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_HSDS))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_I))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_IL))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_LOAD))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_MPPT))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_OR))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_PPV))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_V))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_VPV))

            case (VE_DEVICE_SHUNT)
                ! SmartShunt battery monitor.
                observ%name = 'ved_shunt'

                rc = dm_observ_add_response(observ, responses(VE_FIELD_ALARM))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_AR))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_CE))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_DM))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H1))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H2))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H3))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H4))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H5))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H6))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H7))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H8))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H9))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H10))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H11))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H12))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H15))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H16))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H17))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_H18))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_I))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_MON))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_P))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_RELAY))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_SOC))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_T))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_TTG))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_V))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_VM))
                rc = dm_observ_add_response(observ, responses(VE_FIELD_VS))
        end select
    end subroutine create_observ

    subroutine shutdown(error)
        !! Stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: stat

        stat = merge(STOP_FAILURE, STOP_SUCCESS, dm_is_error(error))

        if (dm_tty_is_connected(tty)) then
            call dm_tty_close(tty)
            call logger%debug('closed TTY ' // app%path)
        end if

        call close_dump(app%dump)

        call logger%info('stopped ' // APP_NAME, error=error)
        call dm_stop(stat)
    end subroutine shutdown

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app !! App type.

        type(arg_parser_class) :: parser

        ! Required and optional command-line arguments.
        call parser%add('name',     short='n', type=ARG_TYPE_ID)                    ! -n, --name <string>
        call parser%add('config',   short='c', type=ARG_TYPE_FILE, required=.true.) ! -c, --config <path>
        call parser%add('logger',   short='l', type=ARG_TYPE_ID)                    ! -l, --logger <string>
        call parser%add('node',     short='N', type=ARG_TYPE_ID)                    ! -N, --node <string>
        call parser%add('sensor',   short='S', type=ARG_TYPE_ID)                    ! -S, --sensor <string>
        call parser%add('target',   short='T', type=ARG_TYPE_ID)                    ! -T, --target <string>
        call parser%add('path',     short='p', type=ARG_TYPE_FILE)                  ! -p, --path <path>
        call parser%add('dump',     short='o', type=ARG_TYPE_FILE)                  ! -o, --dump <path>
        call parser%add('receiver', short='r', type=ARG_TYPE_ID,     max_len=OBSERV_RECEIVER_LEN) ! -r, --receiver <string>
        call parser%add('device',   short='d', type=ARG_TYPE_STRING, max_len=VE_DEVICE_NAME_LEN)  ! -r, --receiver <string>
        call parser%add('interval', short='I', type=ARG_TYPE_INTEGER)               ! -I, --interval <n>
        call parser%add('debug',    short='D', type=ARG_TYPE_LOGICAL)               ! -D, --debug
        call parser%add('verbose',  short='V', type=ARG_TYPE_LOGICAL)               ! -V, --verbose

        ! Read all command-line arguments.
        rc = parser%read(version_callback)
        if (dm_is_error(rc)) return

        call parser%get('name',   app%name)
        call parser%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call parser%get('logger',   app%logger)
        call parser%get('node',     app%node_id)
        call parser%get('sensor',   app%sensor_id)
        call parser%get('target',   app%target_id)
        call parser%get('path',     app%path)
        call parser%get('dump',     app%dump)
        call parser%get('receiver', app%receiver)
        call parser%get('device',   app%device_name)
        call parser%get('interval', app%interval)
        call parser%get('debug',    app%debug)
        call parser%get('verbose',  app%verbose)

        app%device = dm_ve_device_from_name(app%device_name)

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_INVALID
        if (.not. dm_string_has(app%config)) return ! Fail-safe, should never occur.

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('logger',   app%logger)
            call config%get('node',     app%node_id)
            call config%get('sensor',   app%sensor_id)
            call config%get('target',   app%target_id)
            call config%get('path',     app%path)
            call config%get('receiver', app%receiver)
            call config%get('device',   app%device_name)
            call config%get('dump',     app%dump)
            call config%get('interval', app%interval)
            call config%get('debug',    app%debug)
            call config%get('verbose',  app%verbose)
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

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
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

        if (.not. dm_string_has(app%path)) then
            call dm_error_out(rc, 'missing TTY path')
            return
        end if

        if (dm_string_has(app%receiver) .and. .not. dm_id_is_valid(app%receiver)) then
            call dm_error_out(rc, 'invalid receiver')
            return
        end if

        if (.not. dm_ve_device_is_valid(app%device)) then
            call dm_error_out(rc, 'invalid device')
            return
        end if

        if (app%interval < 0) then
            call dm_error_out(rc, 'invalid interval')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine signal_callback(signum) bind(c)
        !! Default POSIX signal handler of the program.
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        call logger%debug('exit on on signal ' // dm_signal_name(signum))
        call shutdown(E_NONE)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmved
