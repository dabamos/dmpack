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
    integer,          parameter :: APP_PATCH = 6

    integer, parameter :: APP_MODE_NONE = 0 !! Unset mode.
    integer, parameter :: APP_MODE_RTU  = 1 !! Modbus RTU mode.
    integer, parameter :: APP_MODE_TCP  = 2 !! Modbus TCP mode.

    type :: rtu_type
        !! Modbus RTU settings.
        character(len=FILE_PATH_LEN) :: path      = ' '             !! Path (required).
        integer                      :: baud_rate = TTY_B19200      !! Baud rate (required).
        integer                      :: byte_size = TTY_BYTE_SIZE8  !! Byte size (required).
        integer                      :: parity    = TTY_PARITY_EVEN !! Parity name (required).
        integer                      :: stop_bits = TTY_STOP_BITS1  !! Stop bits (required).
    end type rtu_type

    type :: tcp_type
        !! Modbus TCP settings.
        character(len=NET_IPV4_LEN) :: address = ' ' !! IPv4 address.
        integer                     :: port    = 0   !! Port.
    end type tcp_type

    type :: app_type
        !! Application settings.
        integer        :: mode    = APP_MODE_NONE       !! Modbus mode (RTU, TCP).
        integer        :: slave   = 1                   !! Modbus slave id.
        integer        :: address = 0                   !! Modbus address.
        integer        :: access  = MODBUS_ACCESS_NONE  !! Read or write operation.
        integer        :: type    = MODBUS_TYPE_DEFAULT !! Number type.
        integer        :: order   = MODBUS_ORDER_NONE   !! Byte order of type float.
        integer        :: value   = 0                   !! Value to write.
        logical        :: verbose = .false.             !! Print debug messages to stderr.
        type(rtu_type) :: rtu                           !! Modbus RTU settings.
        type(tcp_type) :: tcp                           !! Modbus TCP settings.
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
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        character(len=*), parameter :: IP_ADDR_SET = '.0123456789'

        type(app_type), intent(out) :: app !! App type.

        character(len=4) :: order, parity
        character(len=6) :: type
        integer          :: read_address, write_address
        logical          :: has_address, has_baud_rate, has_byte_size, has_order, has_parity, has_path
        logical          :: has_port, has_read, has_stop_bits, has_type, has_value, has_write
        type(arg_type)   :: args(14)

        args = [ &
            arg_type('read',      short='r', type=ARG_TYPE_INTEGER),                      & ! -r, --read <address>
            arg_type('write',     short='w', type=ARG_TYPE_INTEGER),                      & ! -w, --write <address>
            arg_type('path',      short='p', type=ARG_TYPE_STRING),                       & ! -p, --path <string>
            arg_type('baudrate',  short='B', type=ARG_TYPE_INTEGER),                      & ! -B, --baudrate <n>
            arg_type('bytesize',  short='Z', type=ARG_TYPE_INTEGER),                      & ! -Z, --bytesize <n>
            arg_type('parity',    short='P', type=ARG_TYPE_STRING),                       & ! -P, --parity <string>
            arg_type('stopbits',  short='O', type=ARG_TYPE_INTEGER),                      & ! -O, --stopbits <n>
            arg_type('address',   short='a', type=ARG_TYPE_STRING, min_len=7, max_len=NET_IPV4_LEN), & ! -a, --address <string>
            arg_type('port',      short='q', type=ARG_TYPE_INTEGER),                      & ! -q, --port <n>
            arg_type('slave',     short='s', type=ARG_TYPE_INTEGER, required=.true.),     & ! -s, --slave <n>
            arg_type('type',      short='t', type=ARG_TYPE_STRING, min_len=5, max_len=6), & ! -t, --type <string>
            arg_type('order',     short='b', type=ARG_TYPE_STRING, min_len=4, max_len=4), & ! -b, --order <string>
            arg_type('value',     short='i', type=ARG_TYPE_INTEGER),                      & ! -i, --value <n>
            arg_type('verbose',   short='V', type=ARG_TYPE_LOGICAL)                       & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args( 1), read_address,      passed=has_read)
        call dm_arg_get(args( 2), write_address,     passed=has_write)
        call dm_arg_get(args( 3), app%rtu%path,      passed=has_path)
        call dm_arg_get(args( 4), app%rtu%baud_rate, passed=has_baud_rate)
        call dm_arg_get(args( 5), app%rtu%byte_size, passed=has_byte_size)
        call dm_arg_get(args( 6), parity,            passed=has_parity)
        call dm_arg_get(args( 7), app%rtu%stop_bits, passed=has_stop_bits)
        call dm_arg_get(args( 8), app%tcp%address,   passed=has_address)
        call dm_arg_get(args( 9), app%tcp%port,      passed=has_port)
        call dm_arg_get(args(10), app%slave)
        call dm_arg_get(args(11), type,              passed=has_type)
        call dm_arg_get(args(12), order,             passed=has_order)
        call dm_arg_get(args(13), app%value,         passed=has_value)
        call dm_arg_get(args(14), app%verbose)

        ! Parse and validate settings.
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

        ! Modbus mode (RTU/TCP).
        if (has_path) then
            app%mode = APP_MODE_RTU
        else if (has_address) then
            app%mode = APP_MODE_TCP
        end if

        ! Modbus RTU.
        if (app%mode == APP_MODE_RTU) then
            ! Required arguments.
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
            if (dm_tty_baud_rate_from_value(app%rtu%baud_rate) == 0) then
                call dm_error_out(rc, 'argument --baudrate is invalid')
                return
            end if

            ! TTY byte size.
            if (dm_tty_byte_size_from_value(app%rtu%byte_size) == 0) then
                call dm_error_out(rc, 'argument --bytesize is invalid')
                return
            end if

            ! TTY parity.
            app%rtu%parity = dm_tty_parity_from_name(parity)

            if (app%rtu%parity == 0) then
                call dm_error_out(rc, 'argument --parity is invalid')
                return
            end if

            ! TTY stop bits.
            if (dm_tty_stop_bits_from_value(app%rtu%stop_bits) == 0) then
                call dm_error_out(rc, 'argument --stopbits is invalid')
                return
            end if

        ! Modbus TCP.
        else if (app%mode == APP_MODE_TCP) then
            ! Required arguments.
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
            if (verify(trim(app%tcp%address), IP_ADDR_SET) > 0) then
                call dm_error_out(rc, 'argument --address is not a valid IP address')
                return
            end if

            if (app%tcp%port <= 0) then
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

        ! Modbus address to read from.
        if (has_read) then
            if (read_address < 0) then
                call dm_error_out(rc, 'argument --read is not a valid Modbus address')
                return
            end if

            app%access  = MODBUS_ACCESS_READ
            app%address = read_address

        ! Modbus address to write to.
        else if (has_write) then
            if (write_address < 0) then
                call dm_error_out(rc, 'argument --write is not a valid Modbus address')
                return
            end if

            if (.not. has_value) then
                call dm_error_out(rc, 'argument --write requires --value')
                return
            end if

            app%access  = MODBUS_ACCESS_WRITE
            app%address = write_address
        end if

        ! Number type (int16, int32, uint16, uint32, float).
        if (has_type) then
            app%type = dm_modbus_type_from_name(type)

            if (.not. dm_modbus_type_is_valid(app%order)) then
                call dm_error_out(rc, 'argument --type is not a valid number type')
                return
            end if

            if (app%type   == MODBUS_TYPE_FLOAT .and. &
                app%access == MODBUS_ACCESS_WRITE) then
                call dm_error_out(rc, 'argument --write is not allowed for type float')
                return
            end if
        end if

        ! Byte order.
        if (has_order) then
            app%order = dm_modbus_order_from_name(order)

            if (app%type  /= MODBUS_TYPE_FLOAT .and. &
                app%order /= MODBUS_ORDER_NONE) then
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

    integer function run(app) result(rc)
        type(app_type), intent(inout) :: app !! App type.

        type(modbus_rtu_type), target  :: modbus_rtu ! Modbus RTU type.
        type(modbus_tcp_type), target  :: modbus_tcp ! Modbus TCP type.
        class(modbus_type),    pointer :: modbus     ! Modbus pointer.

        modbus_block: block
            integer(kind=i2) :: i16
            integer(kind=i4) :: i32
            integer(kind=i8) :: i64
            real(kind=r4)    :: r32

            rc = E_INVALID

            ! Create Modbus context.
            if (app%mode == APP_MODE_RTU) then
                rc = dm_modbus_create(modbus    = modbus_rtu,        &
                                      path      = app%rtu%path,      &
                                      baud_rate = app%rtu%baud_rate, &
                                      byte_size = app%rtu%byte_size, &
                                      parity    = app%rtu%parity,    &
                                      stop_bits = app%rtu%stop_bits)
                modbus => modbus_rtu
            else if (app%mode == APP_MODE_TCP) then
                rc = dm_modbus_create_tcp(modbus_tcp, app%tcp%address, app%tcp%port)
                modbus => modbus_tcp
            end if

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to create Modbus context')
                exit modbus_block
            end if

            ! Debug mode.
            if (app%verbose) then
                rc = dm_modbus_set_debug(modbus, .true.)

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to enable debug mode')
                    exit modbus_block
                end if
            end if

            ! Create Modbus connection.
            rc = dm_modbus_connect(modbus)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to create Modbus connection')
                exit modbus_block
            end if

            ! Set slave device.
            rc = dm_modbus_set_slave(modbus, slave=app%slave)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to set slave id to ' // dm_itoa(app%slave))
                exit modbus_block
            end if

            ! Read value.
            if (app%access == MODBUS_ACCESS_READ) then
                read_select: select case (app%type)
                    case (MODBUS_TYPE_INT16)
                        rc = dm_modbus_read_int16(modbus, app%address, i16)
                        if (dm_is_error(rc)) exit read_select
                        print '(i0)', i16

                    case (MODBUS_TYPE_INT32)
                        rc = dm_modbus_read_int32(modbus, app%address, i32)
                        if (dm_is_error(rc)) exit read_select
                        print '(i0)', i32

                    case (MODBUS_TYPE_UINT16)
                        rc = dm_modbus_read_uint16(modbus, app%address, i32)
                        if (dm_is_error(rc)) exit read_select
                        print '(i0)', i32

                    case (MODBUS_TYPE_UINT32)
                        rc = dm_modbus_read_uint32(modbus, app%address, i64)
                        if (dm_is_error(rc)) exit read_select
                        print '(i0)', i64

                    case (MODBUS_TYPE_FLOAT)
                        rc = dm_modbus_read_float(modbus, app%address, app%order, r32)
                        if (dm_is_error(rc)) exit read_select
                        print '(f0.12)', i64
                end select read_select

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to read ' // MODBUS_TYPE_NAMES(app%type))
                    exit modbus_block
                end if

            ! Write value.
            else if (app%access == MODBUS_ACCESS_WRITE) then
                write_select: select case (app%type)
                    case (MODBUS_TYPE_INT16)
                        rc = dm_modbus_write_int16(modbus, app%address, int(app%value, kind=i2))

                    case (MODBUS_TYPE_INT32)
                        rc = dm_modbus_write_int32(modbus, app%address, app%value)

                    case (MODBUS_TYPE_UINT16)
                        rc = dm_modbus_write_uint16(modbus, app%address, app%value)

                    case (MODBUS_TYPE_UINT32)
                        rc = dm_modbus_write_uint32(modbus, app%address, int(app%value, kind=i8))

                    case default
                        rc = E_INVALID
                end select write_select

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'failed to write ' // MODBUS_TYPE_NAMES(app%type))
                    exit modbus_block
                end if
            end if

            rc = E_NONE
        end block modbus_block

        ! Disconnect and clean-up.
        call dm_modbus_close(modbus)
        call dm_modbus_destroy(modbus)
    end function run

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_modbus_version(.true.)
    end subroutine version_callback
end program dmmbctl
