! Author:  Philipp Engel
! Licence: ISC
module dm_modbus
    !! Abstraction layer over _libmodbus_, for Modbus RTU/TCP communication.
    !!
    !! You may want to use function `dm_to_signed()` available in module
    !! `dm_compat` to convert unsigned to signed integers.
    !!
    !! Use Modbus function code `0x03` to read holding registers from a Modbus
    !! RTU connection:
    !!
    !! ```fortran
    !! integer           :: i, rc, s
    !! integer(kind=u2)  :: regs(2)
    !! type(modbus_type) :: modbus
    !!
    !! ! Create Modbus RTU context and connect to device 10.
    !! rc = dm_modbus_create(modbus    = modbus, &
    !!                       path      = '/dev/ttyUSB0', &
    !!                       baud_rate = TTY_B19200, &
    !!                       byte_size = TTY_BYTE_SIZE8, &
    !!                       parity    = TTY_PARITY_EVEN, &
    !!                       stop_bits = TTY_STOP_BITS1)
    !! rc = dm_modbus_connect(modbus)
    !! rc = dm_modbus_set_slave(modbus, slave=10)
    !!
    !! ! Read and output two registers.
    !! rc = dm_modbus_read_registers(modbus, address=50, registers=regs)
    !!
    !! do i = 1, size(registers)
    !!     s = dm_to_signed(regs(i))
    !!     print '("regs(", i0, ") = ", i0, " (0x", z0, ")")', i, s, s
    !! end do
    !!
    !! ! Print the two registers as real in ABCD byte order.
    !! print '(f12.8)', dm_modbus_get_real_abcd(regs)
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
    use :: dm_compat
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    ! Modbus modes.
    integer, parameter, public :: MODBUS_MODE_NONE = 0 !! None (invalid).
    integer, parameter, public :: MODBUS_MODE_RTU  = 1 !! Modbus RTU (Remote Terminal Unit).
    integer, parameter, public :: MODBUS_MODE_TCP  = 2 !! Modbus TCP (Transmission Control Protocol).

    ! Byte orders of 32-bit real values.
    integer, parameter, public :: MODBUS_REAL_ABCD = 0 !! ABCD byte order.
    integer, parameter, public :: MODBUS_REAL_BADC = 1 !! BADC byte order.
    integer, parameter, public :: MODBUS_REAL_CDAB = 2 !! CDBA byte order.
    integer, parameter, public :: MODBUS_REAL_DCBA = 3 !! DCBA byte order.

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
        integer     :: mode = MODBUS_MODE_NONE !! RTU or TCP.
        type(c_ptr) :: ctx  = c_null_ptr       !! C-pointer to Modbus context.
    end type modbus_type

    interface dm_modbus_create
        !! Generic function to create Modbus RTU or TCP context.
        module procedure :: dm_modbus_create_rtu
        module procedure :: dm_modbus_create_tcp
    end interface

    public :: dm_modbus_close
    public :: dm_modbus_connect
    public :: dm_modbus_destroy
    public :: dm_modbus_error_message
    public :: dm_modbus_flush
    public :: dm_modbus_get_real
    public :: dm_modbus_get_real_abcd
    public :: dm_modbus_get_real_badc
    public :: dm_modbus_get_real_cdab
    public :: dm_modbus_get_real_dcba
    public :: dm_modbus_get_serial_mode
    public :: dm_modbus_get_slave
    public :: dm_modbus_mode
    public :: dm_modbus_read_registers
    public :: dm_modbus_create
    public :: dm_modbus_create_rtu
    public :: dm_modbus_create_tcp
    public :: dm_modbus_set_debug
    public :: dm_modbus_set_serial_mode
    public :: dm_modbus_set_slave
    public :: dm_modbus_version
contains
    integer function dm_modbus_connect(modbus) result(rc)
        !! Connects to Modbus RTU/TCP device.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the modbus context is not associated.
        !! * `E_IO` if no connection could be established.
        !!
        type(modbus_type), intent(inout) :: modbus !! Modbus type.

        rc = E_INVALID
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
        use :: dm_tty

        type(modbus_type), intent(out) :: modbus    !! Modbus type.
        character(len=*),  intent(in)  :: path      !! Device path.
        integer,           intent(in)  :: baud_rate !! Baud rate enumerator (`TTY_B*`).
        integer,           intent(in)  :: byte_size !! Byte size enumerator (`TTY_BYTE_SIZE*`).
        integer,           intent(in)  :: parity    !! Parity enumerator (`TTY_PARITY_*`).
        integer,           intent(in)  :: stop_bits !! Stop bits enumerator (`TTY_STOP_BITS*`).

        character :: parity_
        integer   :: byte_size_, stop_bits_

        rc = E_INVALID
        if (.not. dm_tty_valid_baud_rate(baud_rate)) return
        if (.not. dm_tty_valid_byte_size(byte_size)) return
        if (.not. dm_tty_valid_parity(parity))       return
        if (.not. dm_tty_valid_stop_bits(stop_bits)) return

        ! Byte size: 5, 6, 7, 8 (start bits).
        select case (byte_size)
            case (TTY_BYTE_SIZE5)
                byte_size_ = 5
            case (TTY_BYTE_SIZE6)
                byte_size_ = 6
            case (TTY_BYTE_SIZE7)
                byte_size_ = 7
            case (TTY_BYTE_SIZE8)
                byte_size_ = 8
        end select

        ! Parity: none, odd, even.
        select case (parity)
            case (TTY_PARITY_NONE)
                parity_ = PARITY_NONE
            case (TTY_PARITY_ODD)
                parity_ = PARITY_ODD
            case (TTY_PARITY_EVEN)
                parity_ = PARITY_EVEN
        end select

        ! Stop bits: 1, 2.
        select case (stop_bits)
            case (TTY_STOP_BITS1)
                stop_bits_ = 1
            case (TTY_STOP_BITS2)
                stop_bits_ = 2
        end select

        rc = E_MODBUS
        modbus%mode = MODBUS_MODE_RTU
        modbus%ctx  = modbus_new_rtu(path, baud_rate, parity_, byte_size_, stop_bits_)
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
        type(modbus_type), intent(out) :: modbus  !! Modbus type.
        character(len=*),  intent(in)  :: address !! IPv4 address.
        integer,           intent(in)  :: port    !! Port number.

        rc = E_INVALID
        if (len_trim(address) == 0) return

        rc = E_MODBUS
        modbus%mode = MODBUS_MODE_TCP
        modbus%ctx  = modbus_new_tcp(address, port)
        if (.not. c_associated(modbus%ctx)) return

        rc = E_NONE
    end function dm_modbus_create_tcp

    function dm_modbus_error_message() result(message)
        !! Returns error message from libmodbus.
        use :: unix, only: c_errno

        character(len=:), allocatable :: message

        message = modbus_strerror(c_errno())
    end function dm_modbus_error_message

    integer function dm_modbus_flush(modbus) result(rc)
        !! Flushes non-transmitted data.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the Modbus context is not associated.
        !! * `E_MODBUS` if flushing failed.
        !!
        type(modbus_type), intent(inout) :: modbus !! Modbus type.

        rc = E_INVALID
        if (.not. c_associated(modbus%ctx)) return

        rc = E_MODBUS
        if (modbus_flush(modbus%ctx) == -1) return

        rc = E_NONE
    end function dm_modbus_flush

    integer function dm_modbus_get_real(registers, byte_order, value) result(rc)
        !! Returns real value from two registers of given byte order in argument
        !! `value`. The argument byte order must be one of the following:
        !!
        !! * `MODBUS_REAL_ABCD`
        !! * `MODBUS_REAL_BADC`
        !! * `MODBUS_REAL_CDAB`
        !! * `MODBUS_REAL_DCBA`
        !!
        !! The function returns `E_INVALID` on any other value.
        integer(kind=u2), intent(inout) :: registers(2) !! Registers to convert.
        integer,          intent(in)    :: byte_order   !! Byte order.
        real,             intent(out)   :: value        !! Returned real value.

        rc = E_NONE

        select case (byte_order)
            case (MODBUS_REAL_ABCD)
                value = modbus_get_float_abcd(registers)
            case (MODBUS_REAL_BADC)
                value = modbus_get_float_badc(registers)
            case (MODBUS_REAL_CDAB)
                value = modbus_get_float_cdab(registers)
            case (MODBUS_REAL_DCBA)
                value = modbus_get_float_dcba(registers)
            case default
                rc = E_INVALID
        end select
    end function dm_modbus_get_real

    real function dm_modbus_get_real_abcd(registers) result(value)
        !! Returns real value from two registers in ABCD byte order.
        integer(kind=u2), intent(inout) :: registers(2) !! Registers to convert.

        value = modbus_get_float_abcd(registers)
    end function dm_modbus_get_real_abcd

    real function dm_modbus_get_real_badc(registers) result(value)
        !! Returns real value from two registers in BADC byte order.
        integer(kind=u2), intent(inout) :: registers(2) !! Registers to convert.

        value = modbus_get_float_badc(registers)
    end function dm_modbus_get_real_badc

    real function dm_modbus_get_real_cdab(registers) result(value)
        !! Returns real value from two registers in CDAB byte order.
        integer(kind=u2), intent(inout) :: registers(2) !! Registers to convert.

        value = modbus_get_float_cdab(registers)
    end function dm_modbus_get_real_cdab

    real function dm_modbus_get_real_dcba(registers) result(value)
        !! Returns real value from two registers in DCBA byte order.
        integer(kind=u2), intent(inout) :: registers(2) !! Registers to convert.

        value = modbus_get_float_dcba(registers)
    end function dm_modbus_get_real_dcba

    integer function dm_modbus_get_serial_mode(modbus, mode) result(rc)
        !! Gets the current Modbus RTU serial mode (RS-232 or RS-485).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the Modbus context is not RTU or not associated.
        !! * `E_MODBUS` if getting the serial mode failed.
        !!
        type(modbus_type), intent(inout) :: modbus !! Modbus type.
        integer,           intent(out)   :: mode   !! Modbus RTU mode (`MODBUS_RTU_RS232`, `MODBUS_RTU_RS485`).

        mode = -1

        rc = E_INVALID
        if (.not. c_associated(modbus%ctx)) return
        if (modbus%mode /= MODBUS_MODE_RTU) return

        rc = E_MODBUS
        mode = modbus_rtu_get_serial_mode(modbus%ctx)

        rc = E_NONE
    end function dm_modbus_get_serial_mode

    integer function dm_modbus_get_slave(modbus, slave) result(rc)
        !! Gets current slave number in the Modbus context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the Modbus context is not associated.
        !! * `E_MODBUS` if getting the slave failed.
        !!
        type(modbus_type), intent(inout) :: modbus !! Modbus type.
        integer,           intent(out)   :: slave  !! Device id.

        slave = -1

        rc = E_INVALID
        if (.not. c_associated(modbus%ctx)) return

        rc = E_MODBUS
        slave = modbus_get_slave(modbus%ctx)
        if (slave == -1) return

        rc = E_NONE
    end function dm_modbus_get_slave

    integer function dm_modbus_mode(modbus) result(mode)
        !! Returns Modbus mode (`MODBUS_MODE_NONE`, `MODBUS_MODE_RTU` or
        !! `MODBUS_MODE_TCP`).
        type(modbus_type), intent(inout) :: modbus !! Modbus type.

        mode = modbus%mode
    end function dm_modbus_mode

    integer function dm_modbus_read_registers(modbus, address, registers, n) result(rc)
        !! Reads many registers from `address`. The size of argument
        !! `registers` determines the number of registers to read, unless
        !! optional argument `n` is passed. The function uses the Modbus
        !! function code `0x03` (read holding registers).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if argument `registers` is invalid or if the Modbus
        !!    context is not associated.
        !! * `E_MODBUS` if reading the registers failed.
        !!
        type(modbus_type), intent(inout)           :: modbus       !! Modbus type.
        integer,           intent(in)              :: address      !! Address to read from.
        integer(kind=u2),  intent(inout)           :: registers(:) !! Register values (signed).
        integer,           intent(inout), optional :: n            !! Number of registers to read on input, number of registers read on output.

        integer :: nregisters, stat

        registers = 0_u2

        nregisters = size(registers)
        if (present(n)) nregisters = n
        if (nregisters > size(registers)) nregisters = size(registers)
        if (present(n)) n = 0

        rc = E_INVALID
        if (size(registers) == 0 .or. nregisters <= 0) return
        if (.not. c_associated(modbus%ctx)) return

        rc = E_MODBUS
        stat = modbus_read_registers(modbus%ctx, address, nregisters, registers)
        if (stat == -1) return
        if (present(n)) n = stat

        rc = E_NONE
    end function dm_modbus_read_registers

    integer function dm_modbus_set_debug(modbus, debug) result(rc)
        !! Sets debug flag of the Modbus context. Returns `E_MODBUS` on error.
        type(modbus_type), intent(inout) :: modbus !! Modbus type.
        logical,           intent(in)    :: debug  !! Enable debug mode.

        integer :: debug_

        debug_ = dm_f_c_logical(debug)

        rc = E_MODBUS
        if (modbus_set_debug(modbus%ctx, debug_) == -1) return

        rc = E_NONE
    end function dm_modbus_set_debug

    integer function dm_modbus_set_serial_mode(modbus, mode) result(rc)
        !! Sets the Modbus RTU serial mode to RS-232 or RS-485. This API
        !! function is only supported on Linux kernels 2.6.28 onwards.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the Modbus context is not associated or if the mode
        !!    is invalid.
        !! * `E_MODBUS` if setting the serial mode failed.
        !!
        type(modbus_type), intent(inout) :: modbus !! Modbus type.
        integer,           intent(in)    :: mode   !! Modbus RTU mode (`MODBUS_RTU_RS232`, `MODBUS_RTU_RS485`).

        rc = E_INVALID
        if (.not. c_associated(modbus%ctx)) return
        if (modbus%mode /= MODBUS_MODE_RTU) return
        if (mode /= MODBUS_RTU_RS232 .or. mode /= MODBUS_RTU_RS485) return

        rc = E_MODBUS
        if (modbus_rtu_set_serial_mode(modbus%ctx, mode) == -1) return

        rc = E_NONE
    end function dm_modbus_set_serial_mode

    integer function dm_modbus_set_slave(modbus, slave) result(rc)
        !! Sets slave number in the Modbus context.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the Modbus context is not associated.
        !! * `E_MODBUS` if setting the slave failed.
        !!
        type(modbus_type), intent(inout) :: modbus !! Modbus type.
        integer,           intent(in)    :: slave  !! Device id.

        rc = E_INVALID
        if (.not. c_associated(modbus%ctx)) return

        rc = E_MODBUS
        if (modbus_set_slave(modbus%ctx, slave) == -1) return

        rc = E_NONE
    end function dm_modbus_set_slave

    subroutine dm_modbus_close(modbus)
        !! Closes the Modbus connection.
        type(modbus_type), intent(inout) :: modbus !! Modbus type.

        if (.not. c_associated(modbus%ctx)) return
        call modbus_close(modbus%ctx)
    end subroutine dm_modbus_close

    subroutine dm_modbus_destroy(modbus)
        !! Destroys the Modbus context.
        type(modbus_type), intent(inout) :: modbus !! Modbus type.

        if (.not. c_associated(modbus%ctx)) return
        call modbus_free(modbus%ctx)
    end subroutine dm_modbus_destroy

    subroutine dm_modbus_version(major, minor, patch)
        !! Returns version numbers of libmodbus.
        integer, intent(out) :: major !! Major version.
        integer, intent(out) :: minor !! Minor version.
        integer, intent(out) :: patch !! Micro version.

        major = LIBMODBUS_VERSION_MAJOR
        minor = LIBMODBUS_VERSION_MINOR
        patch = LIBMODBUS_VERSION_MICRO
    end subroutine dm_modbus_version
end module dm_modbus
