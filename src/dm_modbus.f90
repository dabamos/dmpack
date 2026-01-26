! Author:  Philipp Engel
! Licence: ISC
module dm_modbus
    !! Abstraction layer over _libmodbus_, for Modbus RTU/TCP communication.
    !!
    !! The following types and ranges are used by the Modbus protocol:
    !!
    !! | Code | Range         | Type             | Functions                                       |
    !! |------|---------------|------------------|-------------------------------------------------|
    !! | 0x01 | 00001 – 09999 | coil             | `dm_modbus_read_bit()`, `dm_modbus_read_bits()` |
    !! | 0x02 | 10001 – 19999 | discrete input   | `dm_modbus_read_input_bits()`                   |
    !! | 0x04 | 30001 – 39999 | input register   | `dm_modbus_read_input_registers()`              |
    !! | 0x03 | 40001 – 49999 | holding register | `dm_modbus_read_registers()`                    |
    !!
    !! You may want to use the functions `dm_to_signed()` and
    !! `dm_to_unsigned()` available in module `dm_c` to convert unsigned to
    !! signed integers and vice versa.
    !!
    !! Use Modbus function code `0x03` to read holding registers from a Modbus
    !! RTU connection:
    !!
    !! ``` fortran
    !! integer               :: i, rc, s
    !! integer(u2)           :: data(2)
    !! type(modbus_rtu_type) :: modbus
    !!
    !! ! Create Modbus RTU context and connect to device 10.
    !! rc = dm_modbus_create(modbus    = modbus,                &
    !!                       path      = '/dev/ttyUSB0',        &
    !!                       baud_rate = POSIX_TTY_B19200,      &
    !!                       byte_size = POSIX_TTY_BYTE_SIZE8,  &
    !!                       parity    = POSIX_TTY_PARITY_EVEN, &
    !!                       stop_bits = POSIX_TTY_STOP_BITS1)
    !! rc = dm_modbus_connect(modbus)
    !! rc = dm_modbus_set_slave(modbus, slave=10)
    !!
    !! ! Read and output two registers.
    !! rc = dm_modbus_read_registers(modbus, address=30050, data=data)
    !!
    !! do i = 1, size(data)
    !!     s = dm_to_signed(data(i))
    !!     print '("data(", i0, ") = ", i0, " (0x", z0, ")")', i, s, s
    !! end do
    !!
    !! ! Print the two registers as real in ABCD byte order.
    !! print '(f12.8)', dm_modbus_get_float_abcd(data)
    !!
    !! ! Disconnect and clean-up.
    !! call dm_modbus_close(modbus)
    !! call dm_modbus_destroy(modbus)
    !! ```
    !!
    !! In production, add additional error handling of the return codes.
    !!
    !! ## References
    !!
    !! * [libmodbus Reference Manual](https://libmodbus.org/reference/)
    !!
    use, intrinsic :: iso_c_binding
    use :: modbus
    use :: modbus_rtu
    use :: modbus_tcp
    use :: dm_c
    use :: dm_error
    use :: dm_kind
    use :: dm_modbus_type
    implicit none (type, external)
    private

    character, parameter :: PARITY_NONE = 'N'
    character, parameter :: PARITY_EVEN = 'E'
    character, parameter :: PARITY_ODD  = 'O'

    ! From module `modbus_rtu`.
    public :: MODBUS_RTU_RS232
    public :: MODBUS_RTU_RS485
    public :: MODBUS_RTU_RTS_NONE
    public :: MODBUS_RTU_RTS_UP
    public :: MODBUS_RTU_RTS_DOWN

    type, public :: modbus_type
        !! Opaque Modbus RTU/TCP context type.
        private
        type(c_ptr) :: ctx = c_null_ptr !! C pointer to Modbus RTU/TCP context.
    end type modbus_type

    type, extends(modbus_type), public :: modbus_rtu_type
        !! Opaque Modbus RTU context type.
        private
    end type modbus_rtu_type

    type, extends(modbus_type), public :: modbus_tcp_type
        !! Opaque Modbus TCP context type.
        private
    end type modbus_tcp_type

    interface dm_modbus_create
        !! Generic function to create Modbus RTU or TCP context.
        module procedure :: dm_modbus_create_rtu
        module procedure :: dm_modbus_create_tcp
    end interface dm_modbus_create

    public :: dm_modbus_close
    public :: dm_modbus_connect
    public :: dm_modbus_create
    public :: dm_modbus_create_rtu
    public :: dm_modbus_create_tcp
    public :: dm_modbus_destroy
    public :: dm_modbus_error_message
    public :: dm_modbus_flush
    public :: dm_modbus_get_float
    public :: dm_modbus_get_float_abcd
    public :: dm_modbus_get_float_badc
    public :: dm_modbus_get_float_cdab
    public :: dm_modbus_get_float_dcba
    public :: dm_modbus_get_high_byte
    public :: dm_modbus_get_int32_from_int16
    public :: dm_modbus_get_int64_from_int16
    public :: dm_modbus_get_low_byte
    public :: dm_modbus_get_serial_mode
    public :: dm_modbus_get_slave
    public :: dm_modbus_read_bit
    public :: dm_modbus_read_bits
    public :: dm_modbus_read_float
    public :: dm_modbus_read_input_bits
    public :: dm_modbus_read_input_registers
    public :: dm_modbus_read_int16
    public :: dm_modbus_read_int32
    public :: dm_modbus_read_registers
    public :: dm_modbus_read_uint16
    public :: dm_modbus_read_uint32
    public :: dm_modbus_set_debug
    public :: dm_modbus_set_float
    public :: dm_modbus_set_float_abcd
    public :: dm_modbus_set_float_badc
    public :: dm_modbus_set_float_cdab
    public :: dm_modbus_set_float_dcba
    public :: dm_modbus_set_int32_to_int16
    public :: dm_modbus_set_int64_to_int16
    public :: dm_modbus_set_serial_mode
    public :: dm_modbus_set_slave
    public :: dm_modbus_version
    public :: dm_modbus_write_bit
    public :: dm_modbus_write_int16
    public :: dm_modbus_write_int32
    public :: dm_modbus_write_register
    public :: dm_modbus_write_registers
    public :: dm_modbus_write_uint16
    public :: dm_modbus_write_uint32
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_modbus_connect(modbus) result(rc)
        !! Connects to Modbus RTU/TCP device.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if no connection could be established.
        !! * `E_NULL` if the modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus !! Modbus.

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_IO
        if (modbus_connect(modbus%ctx) == -1) return

        rc = E_NONE
    end function dm_modbus_connect

    integer function dm_modbus_create_rtu(modbus, path, baud_rate, byte_size, parity, stop_bits) result(rc)
        !! Creates a new Modbus RTU context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if a given argument is invalid.
        !! * `E_MODBUS` if no Modbus context could be created.
        !!
        use :: dm_posix_tty

        type(modbus_rtu_type), intent(out) :: modbus    !! Modbus RTU.
        character(*),          intent(in)  :: path      !! Device path.
        integer,               intent(in)  :: baud_rate !! Baud rate enumerator (`POSIX_TTY_B*`).
        integer,               intent(in)  :: byte_size !! Byte size enumerator (`POSIX_TTY_BYTE_SIZE*`).
        integer,               intent(in)  :: parity    !! Parity enumerator (`POSIX_TTY_PARITY_*`).
        integer,               intent(in)  :: stop_bits !! Stop bits enumerator (`POSIX_TTY_STOP_BITS*`).

        character :: parity_
        integer   :: byte_size_, stop_bits_

        rc = E_INVALID
        if (.not. dm_posix_tty_baud_rate_is_valid(baud_rate)) return
        if (.not. dm_posix_tty_byte_size_is_valid(byte_size)) return
        if (.not. dm_posix_tty_parity_is_valid(parity))       return
        if (.not. dm_posix_tty_stop_bits_is_valid(stop_bits)) return

        ! Byte size: 5, 6, 7, 8 (start bits).
        select case (byte_size)
            case (POSIX_TTY_BYTE_SIZE5); byte_size_ = 5
            case (POSIX_TTY_BYTE_SIZE6); byte_size_ = 6
            case (POSIX_TTY_BYTE_SIZE7); byte_size_ = 7
            case (POSIX_TTY_BYTE_SIZE8); byte_size_ = 8
        end select

        ! Parity: none, odd, even.
        select case (parity)
            case (POSIX_TTY_PARITY_NONE); parity_ = PARITY_NONE
            case (POSIX_TTY_PARITY_ODD);  parity_ = PARITY_ODD
            case (POSIX_TTY_PARITY_EVEN); parity_ = PARITY_EVEN
        end select

        ! Stop bits: 1, 2.
        select case (stop_bits)
            case (POSIX_TTY_STOP_BITS1); stop_bits_ = 1
            case (POSIX_TTY_STOP_BITS2); stop_bits_ = 2
        end select

        rc = E_MODBUS
        modbus%ctx = modbus_new_rtu(path, baud_rate, parity_, byte_size_, stop_bits_)
        if (.not. c_associated(modbus%ctx)) return

        rc = E_NONE
    end function dm_modbus_create_rtu

    integer function dm_modbus_create_tcp(modbus, address, port) result(rc)
        !! Creates a new Modbus TCP context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the given arguments are invalid.
        !! * `E_MODBUS` if no Modbus context could be created.
        !!
        type(modbus_tcp_type), intent(out) :: modbus  !! Modbus TCP.
        character(*),          intent(in)  :: address !! IPv4 address.
        integer,               intent(in)  :: port    !! Port number.

        rc = E_INVALID
        if (len_trim(address) == 0) return

        rc = E_MODBUS
        modbus%ctx = modbus_new_tcp(address, port)
        if (.not. c_associated(modbus%ctx)) return

        rc = E_NONE
    end function dm_modbus_create_tcp

    function dm_modbus_error_message() result(message)
        !! Returns error message from libmodbus.
        use :: unix, only: c_errno

        character(:), allocatable :: message

        message = modbus_strerror(c_errno())
    end function dm_modbus_error_message

    integer function dm_modbus_flush(modbus) result(rc)
        !! Flushes non-transmitted data.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if flushing failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus !! Modbus.

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_MODBUS
        if (modbus_flush(modbus%ctx) == -1) return

        rc = E_NONE
    end function dm_modbus_flush

    real function dm_modbus_get_float(data, order, error) result(value)
        !! Returns real value from two registers of given byte order in argument
        !! `value`. The argument byte order must be one of the following:
        !!
        !! * `MODBUS_ORDER_ABCD`
        !! * `MODBUS_ORDER_BADC`
        !! * `MODBUS_ORDER_CDAB`
        !! * `MODBUS_ORDER_DCBA`
        !!
        !! The function sets argument `error' to `E_INVALID` on any other value.
        integer(u2), intent(inout)         :: data(2) !! Registers to convert.
        integer,     intent(in)            :: order   !! Byte order.
        integer,     intent(out), optional :: error   !! Error code.

        value = 0.0
        if (present(error)) error = E_INVALID

        select case (order)
            case (MODBUS_ORDER_ABCD); value = modbus_get_float_abcd(data)
            case (MODBUS_ORDER_BADC); value = modbus_get_float_badc(data)
            case (MODBUS_ORDER_CDAB); value = modbus_get_float_cdab(data)
            case (MODBUS_ORDER_DCBA); value = modbus_get_float_dcba(data)
            case default;             return
        end select

        if (present(error)) error = E_NONE
    end function dm_modbus_get_float

    real function dm_modbus_get_float_abcd(data) result(value)
        !! Returns real value from two registers in ABCD byte order.
        integer(u2), intent(inout) :: data(2) !! Registers to convert.

        value = modbus_get_float_abcd(data)
    end function dm_modbus_get_float_abcd

    real function dm_modbus_get_float_badc(data) result(value)
        !! Returns real value from two registers in BADC byte order.
        integer(u2), intent(inout) :: data(2) !! Registers to convert.

        value = modbus_get_float_badc(data)
    end function dm_modbus_get_float_badc

    real function dm_modbus_get_float_cdab(data) result(value)
        !! Returns real value from two registers in CDAB byte order.
        integer(u2), intent(inout) :: data(2) !! Registers to convert.

        value = modbus_get_float_cdab(data)
    end function dm_modbus_get_float_cdab

    real function dm_modbus_get_float_dcba(data) result(value)
        !! Returns real value from two registers in DCBA byte order.
        integer(u2), intent(inout) :: data(2) !! Registers to convert.

        value = modbus_get_float_dcba(data)
    end function dm_modbus_get_float_dcba

    pure elemental character function dm_modbus_get_high_byte(data) result(byte)
        !! Returns high byte from 2-byte integer.
        integer(u2), intent(in) :: data !! Register data.

        byte = char(iand(shiftr(data, 8), int(z'FF', u2)))
    end function dm_modbus_get_high_byte

    pure integer(i4) function dm_modbus_get_int32_from_int16(data) result(value)
        !! Returns 4-byte integer from two 2-byte registers.
        integer(u2), intent(in) :: data(2) !! Register data.

        integer(i4) :: d(2)

        d = dm_to_signed(data)
        value = ior(shiftl(d(1), 16), d(2))
    end function dm_modbus_get_int32_from_int16

    pure integer(i8) function dm_modbus_get_int64_from_int16(data) result(value)
        !! Returns 8-byte integer from four 2-byte registers.
        integer(u2), intent(in) :: data(4) !! Register data.

        integer(i8) :: d(4)

        d = dm_to_signed(data)
        value = ior(ior(ior(shiftl(d(1), 48), shiftl(d(2), 32)), shiftl(d(3), 16)), d(4))
    end function dm_modbus_get_int64_from_int16

    pure elemental character function dm_modbus_get_low_byte(data) result(byte)
        !! Returns low byte from 2-byte integer.
        integer(u2), intent(in) :: data !! Register data.

        byte = char(iand(data, int(z'FF', u2)))
    end function dm_modbus_get_low_byte

    integer function dm_modbus_get_serial_mode(modbus, mode) result(rc)
        !! Gets the current Modbus RTU serial mode (RS-232 or RS-485).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if getting the serial mode failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        type(modbus_rtu_type), intent(inout) :: modbus !! Modbus RTU.
        integer,               intent(out)   :: mode   !! Modbus RTU mode (`MODBUS_RTU_RS232`, `MODBUS_RTU_RS485`).

        mode = -1

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_MODBUS
        mode = modbus_rtu_get_serial_mode(modbus%ctx)

        rc = E_NONE
    end function dm_modbus_get_serial_mode

    integer function dm_modbus_get_slave(modbus, slave) result(rc)
        !! Gets current slave number in the Modbus context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if getting the slave failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus !! Modbus.
        integer,            intent(out)   :: slave  !! Device id.

        slave = -1

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_MODBUS
        slave = modbus_get_slave(modbus%ctx)
        if (slave == -1) return

        rc = E_NONE
    end function dm_modbus_get_slave

    integer function dm_modbus_read_bit(modbus, address, value) result(rc)
        !! Reads single input bit from `address`. The function uses the Modbus
        !! function code `0x01` (read coil status).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if reading the register failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to read from.
        integer(i4),        intent(out)   :: value   !! Bit read.

        integer(u1) :: data(1)

        rc = dm_modbus_read_bits(modbus, address, data)
        value = data(1)
    end function dm_modbus_read_bit

    integer function dm_modbus_read_bits(modbus, address, data, n) result(rc)
        !! Reads many input bits from `address`. The size of argument `data`
        !! determines the number of bits to read, unless optional argument
        !! `n` is passed. The function uses the Modbus function code `0x01`
        !! (read coil status).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if argument `n` is larger than size of `data`.
        !! * `E_INVALID` if argument `data` is invalid.
        !! * `E_MODBUS` if reading the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout)           :: modbus  !! Modbus.
        integer,            intent(in)              :: address !! Address to read from.
        integer(u1),        intent(inout)           :: data(:) !! Bits read.
        integer,            intent(inout), optional :: n       !! Number of registers to read on input, number of registers read on output.

        integer :: nregisters, stat

        data = 0_u1

        nregisters = size(data)

        if (present(n)) then
            nregisters = n
            n = 0
        end if

        rc = E_BOUNDS
        if (nregisters > size(data)) return

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_INVALID
        if (size(data) == 0 .or. nregisters <= 0) return

        rc = E_MODBUS
        stat = modbus_read_bits(modbus%ctx, address, nregisters, data)
        if (stat == -1) return
        if (present(n)) n = stat

        rc = E_NONE
    end function dm_modbus_read_bits

    integer function dm_modbus_read_float(modbus, address, value, order) result(rc)
        !! Reads 4-byte real from input or holding register, depending on the
        !! address, and returns result in `value`. If the address is not in
        !! input register or holding register range, it is interpreted as a
        !! holding register address.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if argument `n` is larger than size of `data`.
        !! * `E_INVALID` if argument `address` is invalid.
        !! * `E_MODBUS` if reading the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to read from.
        real(r4),           intent(out)   :: value   !! Value read from register.
        integer,            intent(in)    :: order   !! Byte order.

        integer(u2) :: data(2)

        select case (address)
            case (30001:39999); rc = dm_modbus_read_input_registers(modbus, address, data) ! Input registers.
            case (40001:49999); rc = dm_modbus_read_registers      (modbus, address, data) ! Holding registers.
            case default;       rc = dm_modbus_read_registers      (modbus, address, data) ! Holding registers (non-default).
        end select

        if (dm_is_error(rc)) return
        value = dm_modbus_get_float(data, order, error=rc)
    end function dm_modbus_read_float

    integer function dm_modbus_read_input_bits(modbus, address, data, n) result(rc)
        !! Reads many input bits from `address`. The size of argument `data`
        !! determines the number of bits to read, unless optional argument
        !! `n` is passed. The function uses the Modbus function code `0x02`
        !! (read input status).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if argument `n` is larger than size of `data`.
        !! * `E_INVALID` if argument `data` is invalid.
        !! * `E_MODBUS` if reading the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout)           :: modbus  !! Modbus.
        integer,            intent(in)              :: address !! Address to read from.
        integer(u1),        intent(inout)           :: data(:) !! Bits.
        integer,            intent(inout), optional :: n       !! Number of registers to read on input, number of registers read on output.

        integer :: nregisters, stat

        data = 0_u1

        nregisters = size(data)

        if (present(n)) then
            nregisters = n
            n = 0
        end if

        rc = E_BOUNDS
        if (nregisters > size(data)) return

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_INVALID
        if (size(data) == 0 .or. nregisters <= 0) return

        rc = E_MODBUS
        stat = modbus_read_input_bits(modbus%ctx, address, nregisters, data)
        if (stat == -1) return
        if (present(n)) n = stat

        rc = E_NONE
    end function dm_modbus_read_input_bits

    integer function dm_modbus_read_input_registers(modbus, address, data, n) result(rc)
        !! Reads many registers from `address`. The size of argument `data`
        !! determines the number of registers to read, unless optional argument
        !! `n` is passed. The function uses the Modbus function code `0x04`
        !! (read input registers).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if argument `n` is larger than size of `data`.
        !! * `E_INVALID` if argument `data` is invalid.
        !! * `E_MODBUS` if reading the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout)           :: modbus  !! Modbus.
        integer,            intent(in)              :: address !! Address to read from.
        integer(u2),        intent(inout)           :: data(:) !! Register values (unsigned).
        integer,            intent(inout), optional :: n       !! Number of registers to read on input, number of registers read on output.

        integer :: nregisters, stat

        data = 0_u2

        nregisters = size(data)

        if (present(n)) then
            nregisters = n
            n = 0
        end if

        rc = E_BOUNDS
        if (nregisters > size(data)) return

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_INVALID
        if (size(data) == 0 .or. nregisters <= 0) return

        rc = E_MODBUS
        stat = modbus_read_input_registers(modbus%ctx, address, nregisters, data)
        if (stat == -1) return
        if (present(n)) n = stat

        rc = E_NONE
    end function dm_modbus_read_input_registers

    integer function dm_modbus_read_int16(modbus, address, value) result(rc)
        !! Reads 2-byte signed integer from input or holding register, depending
        !! on the address, and returns result in `value`. If the address is not
        !! in input register or holding register range, it is interpreted as a
        !! holding register address.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if argument `n` is larger than size of `data`.
        !! * `E_INVALID` if argument `address` is invalid.
        !! * `E_MODBUS` if reading the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to read from.
        integer(i2),        intent(out)   :: value   !! Value read from register.

        integer(u2) :: data(1)

        value = 0

        select case (address)
            case (30001:39999); rc = dm_modbus_read_input_registers(modbus, address, data) ! Input registers.
            case (40001:49999); rc = dm_modbus_read_registers      (modbus, address, data) ! Holding registers.
            case default;       rc = dm_modbus_read_registers      (modbus, address, data) ! Holding registers (non-default).
        end select

        if (dm_is_error(rc)) return
        value = data(1)
    end function dm_modbus_read_int16

    integer function dm_modbus_read_int32(modbus, address, value) result(rc)
        !! Reads 4-byte signed integer from input or holding register, depending
        !! on the address, and returns result in `value`. If the address is not
        !! in input register or holding register range, it is interpreted as a
        !! holding register address.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if argument `n` is larger than size of `data`.
        !! * `E_INVALID` if argument `address` is invalid.
        !! * `E_MODBUS` if reading the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to read from.
        integer(i4),        intent(out)   :: value   !! Value read from register.

        integer(u2) :: data(2)

        value = 0

        select case (address)
            case (30001:39999); rc = dm_modbus_read_input_registers(modbus, address, data) ! Input registers.
            case (40001:49999); rc = dm_modbus_read_registers      (modbus, address, data) ! Holding registers.
            case default;       rc = dm_modbus_read_registers      (modbus, address, data) ! Holding registers (non-default).
        end select

        if (dm_is_error(rc)) return
        value = dm_modbus_get_int32_from_int16(data)
    end function dm_modbus_read_int32

    integer function dm_modbus_read_registers(modbus, address, data, n) result(rc)
        !! Reads many registers from `address`. The size of argument `data`
        !! determines the number of registers to read, unless optional argument
        !! `n` is passed. The function uses the Modbus function code `0x03`
        !! (read holding registers).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if argument `n` is larger than size of `data`.
        !! * `E_INVALID` if argument `data` is invalid.
        !! * `E_MODBUS` if reading the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout)           :: modbus  !! Modbus.
        integer,            intent(in)              :: address !! Address to read from.
        integer(u2),        intent(inout)           :: data(:) !! Register values (unsigned).
        integer,            intent(inout), optional :: n       !! Number of registers to read on input, number of registers read on output.

        integer :: nregisters, stat

        data = 0_u2

        nregisters = size(data)

        if (present(n)) then
            nregisters = n
            n = 0
        end if

        rc = E_BOUNDS
        if (nregisters > size(data)) return

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_INVALID
        if (size(data) == 0 .or. nregisters <= 0) return

        rc = E_MODBUS
        stat = modbus_read_registers(modbus%ctx, address, nregisters, data)
        if (stat == -1) return
        if (present(n)) n = stat

        rc = E_NONE
    end function dm_modbus_read_registers

    integer function dm_modbus_read_uint16(modbus, address, value) result(rc)
        !! Reads 2-byte unsigned integer from input or holding register,
        !! depending on the address, and returns result in `value`. Stores the
        !! 2-byte unsigned value in a 4-byte signed integer. If the address is
        !! not in input register or holding register range, it is interpreted
        !! as a holding register address.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if argument `n` is larger than size of `data`.
        !! * `E_INVALID` if argument `address` is invalid.
        !! * `E_MODBUS` if reading the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to read from.
        integer(i4),        intent(out)   :: value   !! Value read from register.

        integer(i2) :: u

        value = 0_i4
        rc = dm_modbus_read_int16(modbus, address, u)
        if (dm_is_error(rc)) return
        value = dm_to_signed(u)
    end function dm_modbus_read_uint16

    integer function dm_modbus_read_uint32(modbus, address, value) result(rc)
        !! Reads 4-byte unsigned integer from input or holding register,
        !! depending on the address, and returns result in `value`. Stores the
        !! 4-byte unsigned value in a 8-byte signed integer. If the address is
        !! not in input register or holding register range, it is interpreted
        !! as a holding register address.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if argument `n` is larger than size of `data`.
        !! * `E_INVALID` if argument `address` is invalid.
        !! * `E_MODBUS` if reading the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to read from.
        integer(i8),        intent(out)   :: value   !! Value read from register.

        integer(u4) :: u

        value = 0_i8
        rc = dm_modbus_read_int32(modbus, address, u)
        if (dm_is_error(rc)) return
        value = dm_to_signed(u)
    end function dm_modbus_read_uint32

    integer function dm_modbus_set_debug(modbus, debug) result(rc)
        !! Sets debug flag of the Modbus context. Returns `E_MODBUS` on error.
        class(modbus_type), intent(inout) :: modbus !! Modbus.
        logical,            intent(in)    :: debug  !! Enable debug mode.

        rc = E_MODBUS
        if (modbus_set_debug(modbus%ctx, dm_f_c_logical(debug)) == -1) return
        rc = E_NONE
    end function dm_modbus_set_debug

    integer function dm_modbus_set_serial_mode(modbus, mode) result(rc)
        !! Sets the Modbus RTU serial mode to RS-232 or RS-485. This API
        !! function is only supported on Linux kernels 2.6.28 onwards.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the mode is invalid.
        !! * `E_MODBUS` if setting the serial mode failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        type(modbus_rtu_type), intent(inout) :: modbus !! Modbus RTU.
        integer,               intent(in)    :: mode   !! Modbus RTU mode (`MODBUS_RTU_RS232`, `MODBUS_RTU_RS485`).

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_INVALID
        if (mode /= MODBUS_RTU_RS232 .and. mode /= MODBUS_RTU_RS485) return

        rc = E_MODBUS
        if (modbus_rtu_set_serial_mode(modbus%ctx, mode) == -1) return

        rc = E_NONE
    end function dm_modbus_set_serial_mode

    integer function dm_modbus_set_slave(modbus, slave) result(rc)
        !! Sets slave number in the Modbus context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if setting the slave failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus !! Modbus.
        integer,            intent(in)    :: slave  !! Device id.

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_MODBUS
        if (modbus_set_slave(modbus%ctx, slave) == -1) return

        rc = E_NONE
    end function dm_modbus_set_slave

    function dm_modbus_version(name) result(version)
        !! Returns libmodbus version as allocatable string.
        use :: dm_util, only: dm_present

        logical, intent(in), optional :: name    !! Add prefix `libmodbus/`.
        character(:), allocatable     :: version !! Version string.

        character(8) :: v

        write (v, '(2(i0, "."), i0)') LIBMODBUS_VERSION_MAJOR, &
                                      LIBMODBUS_VERSION_MINOR, &
                                      LIBMODBUS_VERSION_MICRO

        if (dm_present(name, .false.)) then
            version = 'libmodbus/' // trim(v)
        else
            version = trim(v)
        end if
    end function dm_modbus_version

    integer function dm_modbus_write_bit(modbus, address, value) result(rc)
        !! Writes bit `value` to `address` using Modbus function code
        !! `0x05`(force single coil).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if writing the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to write to.
        integer,            intent(in)    :: value   !! Value to write (0 or 1).

        integer :: stat

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_MODBUS
        stat = modbus_write_bit(modbus%ctx, address, min(1, max(0, value)))
        if (stat == -1) return

        rc = E_NONE
    end function dm_modbus_write_bit

    integer function dm_modbus_write_int16(modbus, address, value) result(rc)
        !! Writes 2-byte signed integer to `address`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if writing the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to write to.
        integer(i2),        intent(in)    :: value   !! Value to write.

        rc = dm_modbus_write_register(modbus, address, value)
    end function dm_modbus_write_int16

    integer function dm_modbus_write_int32(modbus, address, value) result(rc)
        !! Writes 4-byte signed integer to `address`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if writing the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to write to.
        integer(i4),        intent(in)    :: value   !! Value to write.

        integer(u2) :: data(2)

        call dm_modbus_set_int32_to_int16(value, data)
        rc = dm_modbus_write_registers(modbus, address, data)
    end function dm_modbus_write_int32

    integer function dm_modbus_write_register(modbus, address, data) result(rc)
        !! Writes register to `address`. The function uses the Modbus function
        !! code `0x06` (preset single register).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if writing the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus   !! Modbus.
        integer,            intent(in)    :: address  !! Address to write to.
        integer(u2),        intent(in)    :: data     !! Register value (unsigned).

        integer :: stat

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_MODBUS
        stat = modbus_write_register(modbus%ctx, address, data)
        if (stat == -1) return

        rc = E_NONE
    end function dm_modbus_write_register

    integer function dm_modbus_write_registers(modbus, address, data, n) result(rc)
        !! Writes many registers to `address`. The size of argument `data`
        !! determines the number of registers to write, unless optional
        !! argument `n` is passed. The function uses the Modbus function code
        !! `0x10` (preset multiple registers).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if argument `n` is larger than size of `data`.
        !! * `E_INVALID` if argument `data` is invalid.
        !! * `E_MODBUS` if writing the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout)           :: modbus  !! Modbus.
        integer,            intent(in)              :: address !! Address to write to.
        integer(u2),        intent(inout)           :: data(:) !! Register values (unsigned).
        integer,            intent(inout), optional :: n       !! Number of registers to write on input, number of registers written on output.

        integer :: nregisters, stat

        nregisters = size(data)

        if (present(n)) then
            nregisters = n
            n = 0
        end if

        rc = E_BOUNDS
        if (nregisters > size(data)) return

        rc = E_NULL
        if (.not. c_associated(modbus%ctx)) return

        rc = E_INVALID
        if (size(data) == 0 .or. nregisters <= 0) return

        rc = E_MODBUS
        stat = modbus_write_registers(modbus%ctx, address, nregisters, data)
        if (stat == -1) return
        if (present(n)) n = stat

        rc = E_NONE
    end function dm_modbus_write_registers

    integer function dm_modbus_write_uint16(modbus, address, value) result(rc)
        !! Writes 2-byte unsigned integer to `address`. The unsigned value must
        !! be passed in a 4-byte signed integer.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if writing the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to write to.
        integer(i4),        intent(in)    :: value   !! Value to write.

        rc = dm_modbus_write_register(modbus, address, dm_to_unsigned(value))
    end function dm_modbus_write_uint16

    integer function dm_modbus_write_uint32(modbus, address, value) result(rc)
        !! Writes 4-byte unsigned integer to `address`. The unsigned value must
        !! be passed in a 8-byte signed integer.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_MODBUS` if writing the registers failed.
        !! * `E_NULL` if the Modbus context is not associated.
        !!
        class(modbus_type), intent(inout) :: modbus  !! Modbus.
        integer,            intent(in)    :: address !! Address to write to.
        integer(i8),        intent(in)    :: value   !! Value to write.

        integer(u2) :: data(2)

        call dm_modbus_set_int32_to_int16(dm_to_unsigned(value), data)
        rc = dm_modbus_write_registers(modbus, address, data)
    end function dm_modbus_write_uint32

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_modbus_close(modbus)
        !! Closes the Modbus RTU/TCP connection.
        class(modbus_type), intent(inout) :: modbus !! Modbus RTU/TCP.

        if (.not. c_associated(modbus%ctx)) return
        call modbus_close(modbus%ctx)
    end subroutine dm_modbus_close

    subroutine dm_modbus_destroy(modbus)
        !! Destroys the Modbus RTU/TCP context.
        class(modbus_type), intent(inout) :: modbus !! Modbus.

        if (.not. c_associated(modbus%ctx)) return
        call modbus_free(modbus%ctx)
    end subroutine dm_modbus_destroy

    pure subroutine dm_modbus_set_int32_to_int16(value, data)
        !! Writes 4-byte integer to two 2-byte registers.
        integer(i4), intent(in)  :: value   !! Value to convert.
        integer(u2), intent(out) :: data(2) !! Returned register data.

        data = [ &
            int(shiftr(value, 16), u2), &
            int(value,             u2)  &
        ]
    end subroutine dm_modbus_set_int32_to_int16

    pure subroutine dm_modbus_set_int64_to_int16(value, data)
        !! Writes 8-byte integer to four 2-byte registers.
        integer(i8), intent(in)  :: value   !! Value to convert.
        integer(u2), intent(out) :: data(4) !! Returned register data.

        data = [ &
            int(shiftr(value, 48), u2), &
            int(shiftr(value, 32), u2), &
            int(shiftr(value, 16), u2), &
            int(value,             u2)  &
        ]
    end subroutine dm_modbus_set_int64_to_int16

    subroutine dm_modbus_set_float(value, data, order, error)
        !! Writes real value to registers of given byte order. The argument
        !! `order` must be one of the following:
        !!
        !! * `MODBUS_ORDER_ABCD`
        !! * `MODBUS_ORDER_BADC`
        !! * `MODBUS_ORDER_CDAB`
        !! * `MODBUS_ORDER_DCBA`
        !!
        !! The routine sets argument `error' to `E_INVALID` on any other value.
        real,        intent(in)            :: value   !! Real value to set.
        integer(u2), intent(out)           :: data(2) !! Registers to write to.
        integer,     intent(in)            :: order   !! Byte order.
        integer,     intent(out), optional :: error   !! Error code.

        data = 0_u2
        if (present(error)) error = E_INVALID

        select case (order)
            case (MODBUS_ORDER_ABCD); call modbus_set_float_abcd(value, data)
            case (MODBUS_ORDER_BADC); call modbus_set_float_badc(value, data)
            case (MODBUS_ORDER_CDAB); call modbus_set_float_cdab(value, data)
            case (MODBUS_ORDER_DCBA); call modbus_set_float_dcba(value, data)
            case default;             return
        end select

        if (present(error)) error = E_NONE
    end subroutine dm_modbus_set_float

    subroutine dm_modbus_set_float_abcd(value, data)
        !! Writes real value to registers in ABCD byte order.
        real,        intent(in)  :: value   !! Real value to set.
        integer(u2), intent(out) :: data(2) !! Registers to write to.

        call modbus_set_float_abcd(value, data)
    end subroutine dm_modbus_set_float_abcd

    subroutine dm_modbus_set_float_badc(value, data)
        !! Writes real value to registers in BADC byte order.
        real,        intent(in)  :: value   !! Real value to set.
        integer(u2), intent(out) :: data(2) !! Registers to write to.

        call modbus_set_float_badc(value, data)
    end subroutine dm_modbus_set_float_badc

    subroutine dm_modbus_set_float_cdab(value, data)
        !! Writes real value to registers in CDAB byte order.
        real,        intent(in)  :: value   !! Real value to set.
        integer(u2), intent(out) :: data(2) !! Registers to write to.

        call modbus_set_float_cdab(value, data)
    end subroutine dm_modbus_set_float_cdab

    subroutine dm_modbus_set_float_dcba(value, data)
        !! Writes real value to registers in DCBA byte order.
        real,        intent(in)  :: value   !! Real value to set.
        integer(u2), intent(out) :: data(2) !! Registers to write to.

        call modbus_set_float_dcba(value, data)
    end subroutine dm_modbus_set_float_dcba
end module dm_modbus
