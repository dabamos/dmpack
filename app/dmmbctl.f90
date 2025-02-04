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

    integer, parameter :: ACCESS_READ  = 0 !! Read values.
    integer, parameter :: ACCESS_WRITE = 1 !! Write values.

    integer, parameter :: MODE_NONE = 0    !! Unset mode.
    integer, parameter :: MODE_RTU  = 1    !! Modbus RTU mode.
    integer, parameter :: MODE_TCP  = 2    !! Modbus TCP mode.

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
        integer        :: access    = ACCESS_READ       !! Read or write operation.
        integer        :: address   = 0                 !! Modbus address.
        integer        :: mode      = MODE_NONE         !! Modbus mode (RTU, TCP).
        integer        :: float     = MODBUS_FLOAT_NONE !! Number type and byte order.
        integer        :: registers = 1                 !! Modbus register count to read or write.
        integer        :: slave     = 1                 !! Modbus slave id.
        logical        :: verbose   = .false.           !! Print debug messages to stderr.
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
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        character(len=*), parameter :: IP_ADDR_SET = '.0123456789'

        type(app_type), intent(out) :: app

        character(len=4) :: float, parity
        integer          :: read_address, write_address
        logical          :: has_baud_rate, has_byte_size, has_float, has_path, has_parity, has_stop_bits
        logical          :: has_address, has_port, has_registers
        logical          :: has_read, has_write
        type(arg_type)   :: args(13)

        args = [ &
            arg_type('path',      short='p', type=ARG_TYPE_STRING),                       & ! -p, --path <string>
            arg_type('baudrate',  short='B', type=ARG_TYPE_INTEGER),                      & ! -B, --baudrate <n>
            arg_type('bytesize',  short='Z', type=ARG_TYPE_INTEGER),                      & ! -Z, --bytesize <n>
            arg_type('parity',    short='P', type=ARG_TYPE_STRING),                       & ! -P, --parity <string>
            arg_type('stopbits',  short='O', type=ARG_TYPE_INTEGER),                      & ! -O, --stopbits <n>
            arg_type('address',   short='a', type=ARG_TYPE_STRING, min_len=7, max_len=NET_IPV4_LEN), & ! -a, --address <string>
            arg_type('port',      short='q', type=ARG_TYPE_INTEGER),                      & ! -q, --port <n>
            arg_type('slave',     short='s', type=ARG_TYPE_INTEGER, required=.true.),     & ! -s, --slave <n>
            arg_type('registers', short='n', type=ARG_TYPE_INTEGER),                      & ! -n, --registers <n>
            arg_type('read',      short='r', type=ARG_TYPE_INTEGER),                      & ! -r, --read <address>
            arg_type('write',     short='w', type=ARG_TYPE_INTEGER),                      & ! -w, --write <address>
            arg_type('float',     short='f', type=ARG_TYPE_STRING, min_len=4, max_len=4), & ! -f, --float
            arg_type('verbose',   short='V', type=ARG_TYPE_LOGICAL)                       & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, version_callback)
        if (dm_is_error(rc)) return

        call dm_arg_get(args( 1), app%rtu%path,      passed=has_path)
        call dm_arg_get(args( 2), app%rtu%baud_rate, passed=has_baud_rate)
        call dm_arg_get(args( 3), app%rtu%byte_size, passed=has_byte_size)
        call dm_arg_get(args( 4), parity,            passed=has_parity)
        call dm_arg_get(args( 5), app%rtu%stop_bits, passed=has_stop_bits)
        call dm_arg_get(args( 6), app%tcp%address,   passed=has_address)
        call dm_arg_get(args( 7), app%tcp%port,      passed=has_port)
        call dm_arg_get(args( 8), app%slave)
        call dm_arg_get(args( 9), app%registers,     passed=has_registers)
        call dm_arg_get(args(10), read_address,      passed=has_read)
        call dm_arg_get(args(11), write_address,     passed=has_write)
        call dm_arg_get(args(12), float,             passed=has_float)
        call dm_arg_get(args(13), app%verbose)

        ! Modbus RTU or TCP mode.
        rc = E_INVALID

        if (.not. has_path .and. .not. has_address) then
            call dm_error_out(rc, 'argument --path or --address required')
            return
        end if

        if (has_path .and. has_address) then
            call dm_error_out(rc, 'argument --path conflicts with --address')
            return
        end if

        if (has_path) then
            app%mode = MODE_RTU
        else if (has_address) then
            app%mode = MODE_TCP
        end if

        select case (app%mode)
            case (MODE_RTU)
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

            case (MODE_TCP)
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
        end select

        ! Slave id.
        if (app%slave < 1) then
            call dm_error_out(rc, 'argument --slave must be > 0')
            return
        end if

        ! Number of registers to read or write.
        if (app%registers < 1) then
            call dm_error_out(rc, 'argument --registers must be > 0')
            return
        end if

        ! Read or write operation.
        if (has_read .and. has_write) then
            call dm_error_out(rc, 'argument --read conflicts with --write')
            return
        else if (.not. has_read .and. .not. has_write) then
            call dm_error_out(rc, 'argument --read or --write is missing')
            return
        end if

        ! Modbus address to read from or write to.
        if (has_read) then
            if (read_address < 0) then
                call dm_error_out(rc, 'argument --read is not a valid Modbus address')
                return
            end if

            app%access  = ACCESS_READ
            app%address = read_address
        else if (has_write) then
            if (write_address < 0) then
                call dm_error_out(rc, 'argument --write is not a valid Modbus address')
                return
            end if

            app%access  = ACCESS_WRITE
            app%address = write_address
        end if

        ! Floating-point number.
        if (has_float) then
            app%float = dm_modbus_float_from_name(float)

            if (.not. dm_modbus_is_valid_float(app%float)) then
                call dm_error_out(rc, 'argument --float is not a valid byte order')
                return
            end if

            if (has_registers .and. app%registers /= 2) then
                call dm_error_out(rc, 'argument --registers must be 2')
                return
            end if

            app%registers = 2
        end if

        rc = E_NONE
    end function read_args

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_modbus_version(.true.)
    end subroutine version_callback
end program dmmbctl
