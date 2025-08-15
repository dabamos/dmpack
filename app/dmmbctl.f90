! dmmbctl.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmmbctl
    !! Basic command-line tool for Modbus RTU/TCP communication.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmmbctl'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 8

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
        integer            :: mode     = MODBUS_MODE_NONE    !! Modbus mode (RTU, TCP).
        integer            :: slave    = 1                   !! Modbus slave id.
        integer            :: register = 0                   !! Modbus register address.
        integer            :: access   = MODBUS_ACCESS_NONE  !! Read or write operation.
        integer            :: type     = MODBUS_TYPE_DEFAULT !! Number type.
        integer            :: order    = MODBUS_ORDER_NONE   !! Byte order of type float.
        integer            :: value    = 0                   !! Value to write.
        logical            :: debug    = .false.             !! Enable debug mode of libmodbus.
        type(app_rtu_type) :: rtu                            !! Modbus RTU settings.
        type(app_tcp_type) :: tcp                            !! Modbus TCP settings.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments and read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Read from or write to Modbus address.
    rc = run(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function run(app) result(rc)
        type(app_type), intent(inout) :: app !! App type.

        type(modbus_rtu_type), target  :: modbus_rtu ! Modbus RTU type.
        type(modbus_tcp_type), target  :: modbus_tcp ! Modbus TCP type.
        class(modbus_type),    pointer :: modbus     ! Modbus pointer.

        modbus_block: block
            rc = E_INVALID

            ! Create Modbus RTU/TCP context.
            select case (app%mode)
                case (MODBUS_MODE_RTU)
                    rc = dm_modbus_create(modbus    = modbus_rtu,        &
                                          path      = app%rtu%path,      &
                                          baud_rate = app%rtu%baud_rate, &
                                          byte_size = app%rtu%byte_size, &
                                          parity    = app%rtu%parity,    &
                                          stop_bits = app%rtu%stop_bits)
                    modbus => modbus_rtu
                case (MODBUS_MODE_TCP)
                    rc = dm_modbus_create_tcp(modbus_tcp, app%tcp%address, app%tcp%port)
                    modbus => modbus_tcp
            end select

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to create Modbus context: ' // dm_modbus_error_message())
                exit modbus_block
            end if

            ! Debug mode.
            if (app%debug) then
                rc = dm_modbus_set_debug(modbus, .true.)

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to enable debug mode: ' // dm_modbus_error_message())
                    exit modbus_block
                end if
            end if

            ! Create Modbus connection.
            rc = dm_modbus_connect(modbus)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to create Modbus connection: ' // dm_modbus_error_message())
                exit modbus_block
            end if

            ! Set slave device.
            rc = dm_modbus_set_slave(modbus, slave=app%slave)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to set slave id to ' // dm_itoa(app%slave))
                exit modbus_block
            end if

            ! Read or write value.
            select case (app%access)
                case (MODBUS_ACCESS_READ)
                    rc = read_value(modbus, app%register, app%type, app%order)
                    if (dm_is_error(rc)) call dm_error_out(rc, 'failed to read ' // MODBUS_TYPE_NAMES(app%type))
                case (MODBUS_ACCESS_WRITE)
                    rc = write_value(modbus, app%register, app%type, app%value)
                    if (dm_is_error(rc)) call dm_error_out(rc, 'failed to write ' // MODBUS_TYPE_NAMES(app%type))
            end select
        end block modbus_block

        ! Disconnect and clean-up.
        call dm_modbus_close(modbus)
        call dm_modbus_destroy(modbus)
    end function run

    integer function read_value(modbus, address, type, order) result(rc)
        !! Reads and prints value from register.
        character(len=*), parameter :: FMT_INT  = '(i0)'   !! Integer output format.
        character(len=*), parameter :: FMT_REAL = '(f0.8)' !! Real output format.

        class(modbus_type), intent(inout) :: modbus  !! Modbus RTU/TCP type.
        integer,            intent(in)    :: address !! Register address.
        integer,            intent(in)    :: type    !! Type of value.
        integer,            intent(in)    :: order   !! Byte order.

        integer(kind=i2) :: i16
        integer(kind=i4) :: i32
        integer(kind=i8) :: i64
        real(kind=r4)    :: r32

        ! Read value.
        select case (type)
            case (MODBUS_TYPE_INT16);  rc = dm_modbus_read_int16 (modbus, address, i16)
            case (MODBUS_TYPE_INT32);  rc = dm_modbus_read_int32 (modbus, address, i32)
            case (MODBUS_TYPE_UINT16); rc = dm_modbus_read_uint16(modbus, address, i32)
            case (MODBUS_TYPE_UINT32); rc = dm_modbus_read_uint32(modbus, address, i64)
            case (MODBUS_TYPE_FLOAT);  rc = dm_modbus_read_float (modbus, address, r32, order)
            case default;              rc = E_INVALID
        end select

        if (dm_is_error(rc)) return

        ! Print value.
        select case (type)
            case (MODBUS_TYPE_INT16);  print FMT_INT,  i16
            case (MODBUS_TYPE_INT32);  print FMT_INT,  i32
            case (MODBUS_TYPE_UINT16); print FMT_INT,  i32
            case (MODBUS_TYPE_UINT32); print FMT_INT,  i64
            case (MODBUS_TYPE_FLOAT);  print FMT_REAL, r32
        end select
    end function read_value

    integer function write_value(modbus, address, type, value) result(rc)
        !! Writes value to register.
        class(modbus_type), intent(inout) :: modbus  !! Modbus RTU/TCP type.
        integer,            intent(in)    :: address !! Register address.
        integer,            intent(in)    :: type    !! Type of value.
        integer,            intent(in)    :: value   !! Value to write.

        select case (type)
            case (MODBUS_TYPE_INT16);  rc = dm_modbus_write_int16 (modbus, address, int(value, kind=i2))
            case (MODBUS_TYPE_INT32);  rc = dm_modbus_write_int32 (modbus, address, value)
            case (MODBUS_TYPE_UINT16); rc = dm_modbus_write_uint16(modbus, address, value)
            case (MODBUS_TYPE_UINT32); rc = dm_modbus_write_uint32(modbus, address, int(value, kind=i8))
            case default;              rc = E_INVALID
        end select
    end function write_value

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app !! App type.

        logical :: has_read, has_write
        logical :: has_order, has_type, has_value
        logical :: has_baud_rate, has_byte_size, has_parity, has_path, has_stop_bits
        logical :: has_address, has_port

        type(arg_class) :: arg

        call arg%create()
        call arg%add('read',      short='r', type=ARG_TYPE_INTEGER)                                  ! -r, --read <register>
        call arg%add('write',     short='w', type=ARG_TYPE_INTEGER)                                  ! -w, --write <register>
        call arg%add('path',      short='p', type=ARG_TYPE_STRING)                                   ! -p, --path <string>
        call arg%add('baudrate',  short='B', type=ARG_TYPE_INTEGER)                                  ! -B, --baudrate <n>
        call arg%add('bytesize',  short='Z', type=ARG_TYPE_INTEGER)                                  ! -Z, --bytesize <n>
        call arg%add('parity',    short='P', type=ARG_TYPE_STRING)                                   ! -P, --parity <string>
        call arg%add('stopbits',  short='O', type=ARG_TYPE_INTEGER)                                  ! -O, --stopbits <n>
        call arg%add('address',   short='a', type=ARG_TYPE_STRING,  min_len=7, max_len=NET_IPV4_LEN) ! -a, --address <string>
        call arg%add('port',      short='q', type=ARG_TYPE_INTEGER)                                  ! -q, --port <n>
        call arg%add('slave',     short='s', type=ARG_TYPE_INTEGER, required=.true.)                 ! -s, --slave <n>
        call arg%add('type',      short='t', type=ARG_TYPE_STRING,  min_len=5, max_len=6)            ! -t, --type <string>
        call arg%add('order',     short='b', type=ARG_TYPE_STRING,  min_len=4, max_len=4)            ! -b, --order <string>
        call arg%add('value',     short='i', type=ARG_TYPE_INTEGER)                                  ! -i, --value <n>
        call arg%add('debug',     short='D', type=ARG_TYPE_LOGICAL)                                  ! -D, --debug

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        block
            character(len=4) :: order, parity
            character(len=6) :: type
            integer          :: baud_rate, byte_size, stop_bits
            integer          :: read_register, write_register

            call arg%get('read',     read_register,   passed=has_read)
            call arg%get('write',    write_register,  passed=has_write)
            call arg%get('path',     app%rtu%path,    passed=has_path)
            call arg%get('baudrate', baud_rate,       passed=has_baud_rate)
            call arg%get('bytesize', byte_size,       passed=has_byte_size)
            call arg%get('parity',   parity,          passed=has_parity)
            call arg%get('stopbits', stop_bits,       passed=has_stop_bits)
            call arg%get('address',  app%tcp%address, passed=has_address)
            call arg%get('port',     app%tcp%port,    passed=has_port)
            call arg%get('slave',    app%slave)
            call arg%get('type',     type,            passed=has_type)
            call arg%get('order',    order,           passed=has_order)
            call arg%get('value',    app%value,       passed=has_value)
            call arg%get('debug',    app%debug)

            if (has_baud_rate) app%rtu%baud_rate = dm_tty_baud_rate_from_value(baud_rate)
            if (has_byte_size) app%rtu%byte_size = dm_tty_byte_size_from_value(byte_size)
            if (has_parity)    app%rtu%parity    = dm_tty_parity_from_name(parity)
            if (has_stop_bits) app%rtu%stop_bits = dm_tty_stop_bits_from_value(stop_bits)
            if (has_type)      app%type          = dm_modbus_type_from_name(type)
            if (has_order)     app%order         = dm_modbus_order_from_name(order)

            if (has_path) then
                app%mode = MODBUS_MODE_RTU
            else
                app%mode = MODBUS_MODE_TCP
            end if

            if (has_read) then
                ! Modbus register to read from.
                app%access   = MODBUS_ACCESS_READ
                app%register = read_register
            else
                ! Modbus register to write to.
                app%access   = MODBUS_ACCESS_WRITE
                app%register = write_register
            end if
        end block

        call arg%destroy()

        ! Validate settings.
        rc = E_INVALID

        ! Path and address.
        if (.not. has_path .and. .not. has_address) then
            call dm_error_out(rc, 'argument --path or --address required')
            return
        end if

        if (has_path .and. has_address) then
            call dm_error_out(rc, 'argument --path conflicts with --address')
            return
        end if

        ! Modbus RTU.
        if (app%mode == MODBUS_MODE_RTU) then
            if (.not. has_baud_rate) then
                call dm_error_out(rc, 'argument --baudrate is required')
                return
            end if

            if (.not. has_byte_size) then
                call dm_error_out(rc, 'argument --bytesize is required')
                return
            end if

            if (.not. has_parity) then
                call dm_error_out(rc, 'argument --parity is required')
                return
            end if

            if (.not. has_stop_bits) then
                call dm_error_out(rc, 'argument --stopbits is required')
                return
            end if

            ! Invalid arguments.
            if (has_port) then
                call dm_error_out(rc, 'argument --port is not permitted')
                return
            end if

            ! TTY path.
            if (.not. dm_file_exists(app%rtu%path)) then
                rc = E_NOT_FOUND
                call dm_error_out(rc, 'device ' // trim(app%rtu%path) // ' not found')
                return
            end if

            ! TTY baud rate.
            if (.not. dm_tty_baud_rate_is_valid(app%rtu%baud_rate)) then
                call dm_error_out(rc, 'argument --baudrate is invalid')
                return
            end if

            ! TTY byte size.
            if (.not. dm_tty_byte_size_is_valid(app%rtu%byte_size)) then
                call dm_error_out(rc, 'argument --bytesize is invalid')
                return
            end if

            ! TTY parity.
            if (.not. dm_tty_parity_is_valid(app%rtu%parity)) then
                call dm_error_out(rc, 'argument --parity is invalid')
                return
            end if

            ! TTY stop bits.
            if (.not. dm_tty_stop_bits_is_valid(app%rtu%stop_bits)) then
                call dm_error_out(rc, 'argument --stopbits is invalid')
                return
            end if
        end if

        ! Modbus TCP.
        if (app%mode == MODBUS_MODE_TCP) then
            if (.not. has_port) then
                call dm_error_out(rc, 'argument --port is required')
                return
            end if

            ! Invalid arguments.
            if (has_baud_rate) then
                call dm_error_out(rc, 'argument --baudrate is not permitted')
                return
            end if

            if (has_byte_size) then
                call dm_error_out(rc, 'argument --bytesize is not permitted')
                return
            end if

            if (has_parity) then
                call dm_error_out(rc, 'argument --parity is not permitted')
                return
            end if

            if (has_stop_bits) then
                call dm_error_out(rc, 'argument --stopbits is not permitted')
                return
            end if

            ! Modbus TCP options.
            if (.not. dm_net_ipv4_is_valid(app%tcp%address)) then
                call dm_error_out(rc, 'argument --address is not a valid IPv4 address')
                return
            end if

            if (app%tcp%port < 1) then
                call dm_error_out(rc, 'argument --port must be > 0')
                return
            end if
        end if

        ! Slave id.
        if (app%slave < 1) then
            call dm_error_out(rc, 'argument --slave must be > 0')
            return
        end if

        ! Read xor write access.
        if (has_read .and. has_write) then
            call dm_error_out(rc, 'argument --read conflicts with --write')
            return
        else if (.not. has_read .and. .not. has_write) then
            call dm_error_out(rc, 'argument --read or --write is missing')
            return
        end if

        if (has_read) then
            ! Modbus register to read from.
            if (app%register < 0) then
                call dm_error_out(rc, 'argument --read is not a valid Modbus register')
                return
            end if
        else
            ! Modbus register to write to.
            if (app%register < 0) then
                call dm_error_out(rc, 'argument --write is not a valid Modbus register')
                return
            end if

            if (.not. has_value) then
                call dm_error_out(rc, 'argument --write requires --value')
                return
            end if
        end if

        ! Number type (int16, int32, uint16, uint32, float).
        if (has_type) then
            if (.not. dm_modbus_type_is_valid(app%type)) then
                call dm_error_out(rc, 'argument --type is not a valid number type')
                return
            end if

            if (app%type == MODBUS_TYPE_FLOAT .and. app%access == MODBUS_ACCESS_WRITE) then
                call dm_error_out(rc, 'argument --write is not allowed for type float')
                return
            end if
        end if

        ! Byte order.
        if (has_order) then
            if (app%type /= MODBUS_TYPE_FLOAT .and. app%order /= MODBUS_ORDER_NONE) then
                call dm_error_out(rc, 'argument --order is not allowed for type ' // MODBUS_TYPE_NAMES(app%type))
                return
            end if

            if (.not. dm_modbus_order_is_valid(app%order)) then
                call dm_error_out(rc, 'argument --order is not a valid byte order')
                return
            end if
        end if

        rc = E_NONE
    end function read_args

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_modbus_version(.true.)
    end subroutine version_callback
end program dmmbctl
