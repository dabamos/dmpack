! Author:  Philipp Engel
! Licence: ISC
module dm_tty
    !! Serial port access (TTY/PTY) on Unix.
    use, intrinsic :: iso_c_binding
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_string
    implicit none (type, external)
    private

    ! Access.
    integer, parameter, public :: TTY_RDONLY  = 1 !! Read-only.
    integer, parameter, public :: TTY_WRONLY  = 2 !! Write-only.
    integer, parameter, public :: TTY_RDWR    = 3 !! Read/write.

    ! Baud rate.
    integer, parameter, public :: TTY_B0      = 0
    integer, parameter, public :: TTY_B50     = 50
    integer, parameter, public :: TTY_B75     = 75
    integer, parameter, public :: TTY_B110    = 110
    integer, parameter, public :: TTY_B134    = 134
    integer, parameter, public :: TTY_B150    = 150
    integer, parameter, public :: TTY_B200    = 200
    integer, parameter, public :: TTY_B300    = 300
    integer, parameter, public :: TTY_B600    = 600
    integer, parameter, public :: TTY_B1200   = 1200
    integer, parameter, public :: TTY_B1800   = 1800
    integer, parameter, public :: TTY_B2400   = 2400
    integer, parameter, public :: TTY_B4800   = 4800
    integer, parameter, public :: TTY_B9600   = 9600
    integer, parameter, public :: TTY_B19200  = 19200
    integer, parameter, public :: TTY_B38400  = 38400
    integer, parameter, public :: TTY_B57600  = 57600
    integer, parameter, public :: TTY_B115200 = 115200
    integer, parameter, public :: TTY_B230400 = 230400
    integer, parameter, public :: TTY_B460800 = 460800
    integer, parameter, public :: TTY_B921600 = 921600

    ! Parity.
    integer, parameter, public :: TTY_PARITY_NONE = 1 !! No parity.
    integer, parameter, public :: TTY_PARITY_EVEN = 2 !! Even parity.
    integer, parameter, public :: TTY_PARITY_ODD  = 3 !! Odd parity.

    integer, parameter, public :: TTY_PARITY_NAME_LEN = 4 !! Parity string length.

    ! Byte size.
    integer, parameter, public :: TTY_BYTE_SIZE5  = 1 !! 5 bits.
    integer, parameter, public :: TTY_BYTE_SIZE6  = 2 !! 6 bits.
    integer, parameter, public :: TTY_BYTE_SIZE7  = 3 !! 7 bits.
    integer, parameter, public :: TTY_BYTE_SIZE8  = 4 !! 8 bits.

    ! Stop bits.
    integer, parameter, public :: TTY_STOP_BITS1  = 1 !! 1 stop bit.
    integer, parameter, public :: TTY_STOP_BITS2  = 2 !! 2 stop bits.

    ! Serial port type.
    type, public :: tty_type
        !! TTY/PTY data type that stores serial port settings (default: 9600 baud, 8N1).
        character(len=FILE_PATH_LEN) :: path      = ' '             !! TTY/PTY path.
        integer                      :: access    = TTY_RDWR        !! Access mode (read/write).
        integer                      :: baud_rate = TTY_B9600       !! Baud rate (9600).
        integer                      :: byte_size = TTY_BYTE_SIZE8  !! Byte size (8).
        integer                      :: stop_bits = TTY_STOP_BITS1  !! Stop bits (1).
        integer                      :: parity    = TTY_PARITY_NONE !! Parity (none).
        integer                      :: timeout   = 5               !! Read timeout in seconds.
        logical                      :: dtr       = .false.         !! Data Terminal Ready.
        logical                      :: rts       = .false.         !! Request To Send.
        logical                      :: blocking  = .true.          !! Blocking read.
        integer(kind=c_int), private :: fd        = -1              !! Unix file descriptor.
    end type tty_type

    interface dm_tty_read
        !! Generic TTY read function.
        module procedure :: dm_tty_read_bytes
        module procedure :: dm_tty_read_request
    end interface

    interface dm_tty_write
        !! Generic TTY write function.
        module procedure :: dm_tty_write_bytes
        module procedure :: dm_tty_write_request
    end interface

    ! Public procedures.
    public :: dm_tty_baud_rate_from_value
    public :: dm_tty_byte_size_from_value
    public :: dm_tty_close
    public :: dm_tty_connected
    public :: dm_tty_flush
    public :: dm_tty_open
    public :: dm_tty_parity_from_name
    public :: dm_tty_read
    public :: dm_tty_read_byte
    public :: dm_tty_read_bytes
    public :: dm_tty_read_request
    public :: dm_tty_set_attributes
    public :: dm_tty_set_blocking
    public :: dm_tty_set_timeout
    public :: dm_tty_stop_bits_from_value
    public :: dm_tty_valid_baud_rate
    public :: dm_tty_valid_byte_size
    public :: dm_tty_valid_parity
    public :: dm_tty_valid_stop_bits
    public :: dm_tty_valid_timeout
    public :: dm_tty_write
    public :: dm_tty_write_bytes
    public :: dm_tty_write_request
contains
    integer function dm_tty_baud_rate_from_value(value, error) result(baud_rate)
        !! Returns baud rate enumerator from numeric value. If the value is
        !! invalid, returns 0 by default and sets optional argument `error` to
        !! `E_INVALID`.
        integer, intent(in)            :: value !! Numeric baud rate value.
        integer, intent(out), optional :: error !! Error code.

        baud_rate = 0
        if (present(error)) error = E_INVALID

        select case (value)
            case (0)
                baud_rate = TTY_B0
            case (50)
                baud_rate = TTY_B50
            case (75)
                baud_rate = TTY_B75
            case (110)
                baud_rate = TTY_B110
            case (134)
                baud_rate = TTY_B134
            case (150)
                baud_rate = TTY_B150
            case (200)
                baud_rate = TTY_B200
            case (300)
                baud_rate = TTY_B300
            case (600)
                baud_rate = TTY_B600
            case (1200)
                baud_rate = TTY_B1200
            case (1800)
                baud_rate = TTY_B1800
            case (2400)
                baud_rate = TTY_B2400
            case (4800)
                baud_rate = TTY_B4800
            case (9600)
                baud_rate = TTY_B9600
            case (19200)
                baud_rate = TTY_B19200
            case (38400)
                baud_rate = TTY_B38400
            case (57600)
                baud_rate = TTY_B57600
            case (115200)
                baud_rate = TTY_B115200
            case (230400)
                baud_rate = TTY_B230400
            case (460800)
                baud_rate = TTY_B460800
            case (921600)
                baud_rate = TTY_B921600
            case default
                return
        end select

        if (present(error)) error = E_NONE
    end function dm_tty_baud_rate_from_value

    integer function dm_tty_byte_size_from_value(value, error) result(byte_size)
        !! Returns byte size enumerator from numeric value. If the value is
        !! invalid, returns 0 by default and sets optional argument `error`
        !! to `E_INVALID`.
        integer, intent(in)            :: value !! Numeric byte size value.
        integer, intent(out), optional :: error !! Error code.

        byte_size = 0
        if (present(error)) error = E_INVALID

        select case (value)
            case (5)
                byte_size = TTY_BYTE_SIZE5
            case (6)
                byte_size = TTY_BYTE_SIZE6
            case (7)
                byte_size = TTY_BYTE_SIZE7
            case (8)
                byte_size = TTY_BYTE_SIZE8
            case default
                return
        end select

        if (present(error)) error = E_NONE
    end function dm_tty_byte_size_from_value

    logical function dm_tty_connected(tty) result(connected)
        !! Return `.true.` if TTY is connected, else `.false.`.
        type(tty_type), intent(inout) :: tty !! TTY type.

        connected = .false.
        if (tty%fd /= -1) connected = .true.
    end function dm_tty_connected

    integer function dm_tty_flush(tty, input, output) result(rc)
        !! Flushes TTY input and output buffer. Returns `E_INVALID` if the
        !! passed `tty` type is invalid, or `E_SYSTEM` if the system call
        !! failed.
        use :: unix, only: c_tcflush, TCIFLUSH, TCIOFLUSH, TCOFLUSH

        type(tty_type), intent(inout)        :: tty    !! TTY type.
        logical,        intent(in), optional :: input  !! Flush input buffer.
        logical,        intent(in), optional :: output !! Flush output buffer.

        integer(kind=c_int) :: n
        logical             :: input_, output_

        input_  = .true.
        output_ = .true.

        if (present(input))  input_  = input
        if (present(output)) output_ = output

        rc = E_INVALID
        if (tty%fd < 0) return

        if (input_ .and. output_) then
            ! Flush input and output.
            n = TCIOFLUSH
        else if (input_) then
            ! Flush input.
            n = TCIFLUSH
        else if (output_) then
            ! Flush output.
            n = TCOFLUSH
        else
            ! Nothing to do.
            rc = E_NONE
            return
        end if

        rc = E_SYSTEM
        if (c_tcflush(tty%fd, n) /= 0) return

        rc = E_NONE
    end function dm_tty_flush

    integer function dm_tty_open(tty, path, baud_rate, byte_size, parity, stop_bits) result(rc)
        !! Opens TTY/PTS device in set access mode and applies serial port
        !! attributes. The arguments `baud_rate`, `byte_size`, `parity`, and
        !! `stop_bits` must be valid enumerators.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if the TTY is already connected.
        !! * `E_INVALID` if the TTY parameters or flags are invalid.
        !! * `E_IO` if opening the TTY failed.
        !! * `E_SYSTEM` if setting the TTY attributes or flushing the buffers failed.
        use :: unix

        type(tty_type),   intent(inout)        :: tty       !! TTY type.
        character(len=*), intent(in), optional :: path      !! Device path.
        integer,          intent(in), optional :: baud_rate !! Baud rate enumerator (`TTY_B_*`).
        integer,          intent(in), optional :: byte_size !! Byte size enumerator (`TTY_BYTE_SIZE*`).
        integer,          intent(in), optional :: parity    !! Parity enumerator (`TTY_PARITY_*`).
        integer,          intent(in), optional :: stop_bits !! Stop bits enumerator (`TTY_STOP_BITS*`).

        integer(kind=c_int) :: flags

        rc = E_EXIST
        if (dm_tty_connected(tty)) return

        ! Set arguments.
        rc = E_INVALID
        if (present(path)) tty%path = path

        if (present(baud_rate)) then
            if (.not. dm_tty_valid_baud_rate(baud_rate)) return
            tty%baud_rate = baud_rate
        end if

        if (present(byte_size)) then
            if (.not. dm_tty_valid_byte_size(byte_size)) return
            tty%byte_size = byte_size
        end if

        if (present(parity)) then
            if (.not. dm_tty_valid_parity(parity)) return
            tty%parity = parity
        end if

        if (present(stop_bits)) then
            if (.not. dm_tty_valid_stop_bits(stop_bits)) return
            tty%stop_bits = stop_bits
        end if

        ! Set flags.
        flags = ior(O_NOCTTY, O_SYNC)
        flags = ior(flags, O_NONBLOCK)
        flags = ior(flags, O_NDELAY)

        select case (tty%access)
            case (TTY_RDONLY)
                flags = ior(flags, O_RDONLY)
            case (TTY_WRONLY)
                flags = ior(flags, O_WRONLY)
            case (TTY_RDWR)
                flags = ior(flags, O_RDWR)
            case default
                return
        end select

        ! Open TTY.
        rc = E_IO
        tty%fd = c_open(trim(tty%path) // c_null_char, flags, 0_c_mode_t)
        if (tty%fd < 0) return

        rc = dm_tty_set_attributes(tty)
        if (dm_is_error(rc)) return

        rc = dm_tty_flush(tty)
    end function dm_tty_open

    integer function dm_tty_parity_from_name(name, error) result(parity)
        !! Returns parity from character string (`none`, `even`, `odd`). If the
        !! parity is not recognised, returns 0 by default and sets optional
        !! argument `error` to `E_INVALID`.
        character(len=*), intent(in)            :: name  !! Parity name.
        integer,          intent(out), optional :: error !! Error code.

        parity = 0
        if (present(error)) error = E_INVALID

        select case (dm_lower(name))
            case ('none')
                parity = TTY_PARITY_NONE
            case ('even')
                parity = TTY_PARITY_EVEN
            case ('odd')
                parity = TTY_PARITY_ODD
            case default
                return
        end select

        if (present(error)) error = E_NONE
    end function dm_tty_parity_from_name

    integer function dm_tty_read_byte(tty, byte) result(rc)
        !! Reads single byte from file descriptor.
        use :: unix

        type(tty_type),    intent(inout) :: tty  !! TTY type.
        character, target, intent(out)   :: byte !! Byte read.

        integer(kind=c_size_t) :: sz

        rc = E_READ
        sz = c_read(tty%fd, c_loc(byte), 1_c_size_t)
        if (sz <= 0) return
        rc = E_NONE
    end function dm_tty_read_byte

    integer function dm_tty_read_bytes(tty, bytes, del, nbytes) result(rc)
        !! Reads from TTY into `buf` until delimiter `del` occurs. The
        !! number of bytes read is returned in `n`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if end of buffer is reached.
        !! * `E_READ` if the read operation failed.
        type(tty_type),   intent(inout)         :: tty    !! TTY type.
        character(len=*), intent(inout)         :: bytes  !! Input buffer.
        character(len=*), intent(in)            :: del    !! Delimiter.
        integer(kind=i8), intent(out), optional :: nbytes !! Number of bytes read.

        character        :: a
        integer          :: i, j, k
        integer(kind=i8) :: n

        i = 1
        j = len(bytes)
        k = len(del)
        n = 0_i8

        do
            rc = E_BOUNDS
            if (i > j) exit

            rc = dm_tty_read_byte(tty, a)
            if (dm_is_error(rc)) exit

            bytes(i:i) = a
            i = i + 1
            n = n + 1

            rc = E_NONE
            if (bytes(i - k:i) == del) exit
            cycle
        end do

        if (present(nbytes)) nbytes = n
    end function dm_tty_read_bytes

    integer function dm_tty_read_request(tty, request) result(rc)
        !! Reads TTY response into request. The request delimiter is unescaped.
        !! The response is escaped before being stored in the request.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if the response is longer than `REQUEST_RESPONSE_LEN`.
        !! * `E_READ` if reading from TTY failed.
        use :: dm_ascii, only: dm_ascii_escape, dm_ascii_unescape
        use :: dm_request

        type(tty_type),     intent(inout) :: tty     !! TTY type.
        type(request_type), intent(inout) :: request !! Request type.

        character(len=REQUEST_RESPONSE_LEN)  :: raw ! Raw response (unescaped).
        character(len=REQUEST_DELIMITER_LEN) :: del ! Raw delimiter (unescaped).

        del = dm_ascii_unescape(request%delimiter)
        raw = ' '

        rc = dm_tty_read(tty, raw, trim(del))

        request%error    = rc
        request%response = dm_ascii_escape(raw)
    end function dm_tty_read_request

    integer function dm_tty_set_attributes(tty) result(rc)
        !! Sets terminal attributes.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if TTY is not connected.
        !! * `E_SYSTEM` if system calls failed.
        use :: unix

        type(tty_type), intent(inout) :: tty !! TTY type.

        integer(kind=c_tcflag_t) :: c_cflag
        integer(kind=c_tcflag_t) :: c_iflag
        integer(kind=c_tcflag_t) :: c_lflag
        integer(kind=c_tcflag_t) :: c_oflag

        integer(kind=c_speed_t) :: baud_rate
        integer(kind=c_int)     :: byte_size
        integer(kind=c_int)     :: parity
        integer(kind=c_int)     :: stop_bits

        integer(kind=c_int), target :: stat
        type(c_termios)             :: termios

        rc = E_INVALID
        if (tty%fd < 0) return

        ! Byte size (start bits).
        select case (tty%byte_size)
            case (TTY_BYTE_SIZE5)
                byte_size = CS5
            case (TTY_BYTE_SIZE6)
                byte_size = CS6
            case (TTY_BYTE_SIZE7)
                byte_size = CS7
            case (TTY_BYTE_SIZE8)
                byte_size = CS8
            case default
                return
        end select

        ! Stop bits.
        select case (tty%stop_bits)
            case (TTY_STOP_BITS1)
                stop_bits = 0
            case (TTY_STOP_BITS2)
                stop_bits = CSTOPB
            case default
                return
        end select

        ! Parity.
        select case (tty%parity)
            case (TTY_PARITY_NONE)
                parity = 0
            case (TTY_PARITY_EVEN)
                parity = PARENB
            case (TTY_PARITY_ODD)
                parity = ior(PARENB, PARODD)
            case default
                return
        end select

        ! Baud rate.
        select case (tty%baud_rate)
            case (TTY_B0)
                baud_rate = B0
            case (TTY_B50)
                baud_rate = B50
            case (TTY_B75)
                baud_rate = B75
            case (TTY_B110)
                baud_rate = B110
            case (TTY_B134)
                baud_rate = B134
            case (TTY_B150)
                baud_rate = B150
            case (TTY_B200)
                baud_rate = B200
            case (TTY_B300)
                baud_rate = B300
            case (TTY_B600)
                baud_rate = B600
            case (TTY_B1200)
                baud_rate = B1200
            case (TTY_B1800)
                baud_rate = B1800
            case (TTY_B2400)
                baud_rate = B2400
            case (TTY_B4800)
                baud_rate = B4800
            case (TTY_B9600)
                baud_rate = B9600
            case (TTY_B19200)
                baud_rate = B19200
            case (TTY_B38400)
                baud_rate = B38400
            case (TTY_B57600)
                baud_rate = B57600
            case (TTY_B115200)
                baud_rate = B115200
            case (TTY_B230400)
                baud_rate = B230400
            case (TTY_B460800)
                baud_rate = B460800
            case (TTY_B921600)
                baud_rate = B921600
            case default
                return
        end select

        rc = E_SYSTEM

        ! Get current attributes.
        stat = c_tcgetattr(tty%fd, termios)
        if (stat /= 0) return

        ! Set baud rate (I/O).
        stat = c_cfsetispeed(termios, baud_rate); if (stat /= 0) return
        stat = c_cfsetospeed(termios, baud_rate); if (stat /= 0) return

        ! Modes.
        c_iflag = int(c_uint_to_int(termios%c_iflag), kind=c_tcflag_t)
        c_oflag = int(c_uint_to_int(termios%c_oflag), kind=c_tcflag_t)
        c_cflag = int(c_uint_to_int(termios%c_cflag), kind=c_tcflag_t)
        c_lflag = int(c_uint_to_int(termios%c_lflag), kind=c_tcflag_t)

        ! Input modes.
        c_iflag = iand(c_iflag, not(IGNBRK + BRKINT + PARMRK + ISTRIP + INLCR + IGNCR + ICRNL)) ! No special handling of received bytes.
        c_iflag = iand(c_iflag, not(IXON + IXOFF + IXANY))  ! Turn XON/XOFF control off.

        ! Output modes.
        c_oflag = iand(c_oflag, not(OPOST))                 ! No special interpretation of output bytes.

        ! Control modes.
        c_cflag = iand(c_cflag, not(CSIZE))                 ! Unset byte size.
        c_cflag = iand(c_cflag, not(CSTOPB))                ! Unset stop bits.
        c_cflag = iand(c_cflag, not(ior(PARENB, PARODD)))   ! Unset parity.
        c_cflag = ior (c_cflag, byte_size)                  ! Set byte size.
        c_cflag = ior (c_cflag, stop_bits)                  ! Set stop bits.
        c_cflag = ior (c_cflag, parity)                     ! Set parity.
        c_cflag = ior (c_cflag, ior(CLOCAL, CREAD))         ! Ignore modem controls, enable reading.

        ! Local modes.
        c_lflag = iand(c_lflag, not(ECHO + ECHOE + ECHONL)) ! No echo.
        c_lflag = iand(c_lflag, not(IEXTEN))                ! No implementation-defined input processing.
        c_lflag = iand(c_lflag, not(ICANON))                ! No canonical processing.
        c_lflag = iand(c_lflag, not(ISIG))                  ! No signal chars.

        termios%c_iflag = c_iflag
        termios%c_oflag = c_oflag
        termios%c_cflag = c_cflag
        termios%c_lflag = c_lflag

        if (tty%blocking) then
            ! Minimum number of characters for non-canonical read.
            termios%c_cc(VMIN)  = 1_c_cc_t
            termios%c_cc(VTIME) = 0_c_cc_t
        else
            ! Timeout in deciseconds for non-canonical read.
            termios%c_cc(VMIN)  = 0_c_cc_t
            termios%c_cc(VTIME) = int(max(0, min(255, tty%timeout * 10)), kind=c_cc_t)
        end if

        ! Set attributes.
        stat = c_tcsetattr(tty%fd, TCSANOW, termios)
        if (stat /= 0) return

        ! Set RTS, DTR.
        if (tty%rts .or. tty%dtr) then
            stat = 0
            if (c_ioctl(tty%fd, int(TIOCMGET, kind=c_unsigned_long), c_loc(stat)) /= 0) return
            if (tty%rts) stat = ior(stat, TIOCM_RTS)
            if (tty%dtr) stat = ior(stat, TIOCM_DTR)
            if (c_ioctl(tty%fd, int(TIOCMSET, kind=c_unsigned_long), c_loc(stat)) /= 0) return
        end if

        ! Set blocking read.
        rc = dm_tty_set_blocking(tty, tty%blocking)
    end function dm_tty_set_attributes

    integer function dm_tty_set_blocking(tty, blocking) result(rc)
        !! Sets TTY to blocking or non-blocking.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if TTY is not connected.
        !! * `E_SYSTEM` if system calls failed.
        use :: unix

        type(tty_type), intent(inout) :: tty      !! TTY type.
        logical,        intent(in)    :: blocking !! Blocking mode.

        integer(kind=c_int) :: flags

        rc = E_INVALID
        if (tty%fd < 0) return

        rc = E_SYSTEM
        flags = c_fcntl(tty%fd, F_GETFL, 0)

        if (blocking) then
            flags = iand(flags, not(O_NONBLOCK))
        else
            flags = ior(flags, O_NONBLOCK)
        end if

        if (c_fcntl(tty%fd, F_SETFL, flags) /= 0) return
        tty%blocking = blocking
        rc = E_NONE
    end function dm_tty_set_blocking

    integer function dm_tty_set_timeout(tty, timeout) result(rc)
        !! Sets timeout of given TTY. A timeout of 0 results in blocking read
        !! without timeout. The minimum timeout is 0 seconds, the maximum is 25
        !! seconds.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if TTY is not connected.
        !! * `E_SYSTEM` if system calls failed.
        use :: unix

        type(tty_type), intent(inout) :: tty     !! TTY type.
        integer,        intent(in)    :: timeout !! Timeout in seconds.

        type(c_termios) :: termios

        rc = E_INVALID
        if (tty%fd < 0) return

        rc = E_SYSTEM
        if (c_tcgetattr(tty%fd, termios) /= 0) return
        termios%c_cc(VTIME) = int(max(0, min(255, timeout * 10)), kind=c_cc_t)
        if (c_tcsetattr(tty%fd, TCSANOW, termios) /= 0) return

        tty%timeout = timeout
    end function dm_tty_set_timeout

    integer function dm_tty_stop_bits_from_value(value, error) result(stop_bits)
        !! Returns stop bits enumerator from numeric value. If the value is
        !! invalid, returns 0 by default and sets optional argument `error`
        !! to `E_INVALID`.
        integer, intent(in)            :: value !! Numeric byte size value.
        integer, intent(out), optional :: error !! Error code.

        stop_bits = 0
        if (present(error)) error = E_INVALID

        select case (value)
            case (1)
                stop_bits = TTY_STOP_BITS1
            case (2)
                stop_bits = TTY_STOP_BITS2
            case default
                return
        end select

        if (present(error)) error = E_NONE
    end function dm_tty_stop_bits_from_value

    pure elemental logical function dm_tty_valid_baud_rate(baud_rate) result(valid)
        !! Returns `.true.` if given baud rate value is valid, else `.false.`.
        integer, intent(in) :: baud_rate !! Baud rate.

        valid = .false.

        select case (baud_rate)
            case (TTY_B0,      &
                  TTY_B50,     &
                  TTY_B75,     &
                  TTY_B110,    &
                  TTY_B134,    &
                  TTY_B150,    &
                  TTY_B200,    &
                  TTY_B300,    &
                  TTY_B600,    &
                  TTY_B1200,   &
                  TTY_B1800,   &
                  TTY_B2400,   &
                  TTY_B4800,   &
                  TTY_B9600,   &
                  TTY_B19200,  &
                  TTY_B38400,  &
                  TTY_B57600,  &
                  TTY_B115200, &
                  TTY_B230400, &
                  TTY_B460800, &
                  TTY_B921600)
                valid = .true.
        end select
    end function dm_tty_valid_baud_rate

    pure elemental logical function dm_tty_valid_byte_size(byte_size) result(valid)
        !! Returns `.true.` if given byte size value is valid, else `.false.`.
        integer, intent(in) :: byte_size !! Byte size.

        valid = .false.

        select case (byte_size)
            case (TTY_BYTE_SIZE5, TTY_BYTE_SIZE6, TTY_BYTE_SIZE7, TTY_BYTE_SIZE8)
                valid = .true.
        end select
    end function dm_tty_valid_byte_size

    pure elemental logical function dm_tty_valid_parity(parity) result(valid)
        !! Returns `.true.` if given parity value is valid, else `.false.`.
        integer, intent(in) :: parity !! Parity.

        valid = .false.

        select case (parity)
            case (TTY_PARITY_NONE, TTY_PARITY_EVEN, TTY_PARITY_ODD)
                valid = .true.
        end select
    end function dm_tty_valid_parity

    pure elemental logical function dm_tty_valid_stop_bits(stop_bits) result(valid)
        !! Returns `.true.` if given stop bits value is valid, else `.false.`.
        integer, intent(in) :: stop_bits !! Stop bits.

        valid = .false.

        select case (stop_bits)
            case (TTY_STOP_BITS1, TTY_STOP_BITS2)
                valid = .true.
        end select
    end function dm_tty_valid_stop_bits

    pure elemental logical function dm_tty_valid_timeout(timeout) result(valid)
        !! Returns `.true.` if given timeout value is valid, else `.false.`.
        integer, intent(in) :: timeout !! Timeout.

        valid = .false.
        if (timeout >= 0) valid = .true.
    end function dm_tty_valid_timeout

    integer function dm_tty_write_bytes(tty, bytes, nbytes, flush) result(rc)
        !! Writes given string to TTY. Returns `E_WRITE` on error. The function
        !! may cause an access violation if `nbytes` is greater than the length
        !! of `bytes`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_SYSTEM` if flushing the input buffer failed.
        !! * `E_WRITE` if writing to TTY failed.
        use :: unix, only: c_write

        type(tty_type),           intent(inout)        :: tty    !! TTY type.
        character(len=*), target, intent(in)           :: bytes  !! Bytes to send.
        integer,                  intent(in), optional :: nbytes !! Number of bytes to send.
        logical,                  intent(in), optional :: flush  !! Flush input buffer.

        integer(kind=c_size_t) :: n, sz

        if (present(nbytes)) then
            n = int(nbytes, kind=c_size_t)
        else
            n = len(bytes, kind=c_size_t)
        end if

        if (present(flush)) then
            rc = dm_tty_flush(tty, input=flush, output=.false.)
            if (dm_is_error(rc)) return
        end if

        rc = E_NONE
        if (n == 0) return

        rc = E_WRITE
        sz = c_write(tty%fd, c_loc(bytes), n)
        if (sz /= n) return

        rc = E_NONE
    end function dm_tty_write_bytes

    integer function dm_tty_write_request(tty, request, flush) result(rc)
        !! Writes given request to TTY. The function unescapes the request
        !! string. If `flush` is `.true.`, the input buffer is flushed first.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_SYSTEM` if flushing the input buffer failed.
        !! * `E_WRITE` if writing to TTY failed.
        use :: dm_ascii, only: dm_ascii_unescape
        use :: dm_request

        type(tty_type),     intent(inout)        :: tty     !! TTY type.
        type(request_type), intent(inout)        :: request !! Request type
        logical,            intent(in), optional :: flush   !! Flush input buffer.

        character(len=REQUEST_REQUEST_LEN) :: raw ! Raw request (unescaped).

        if (present(flush)) then
            rc = dm_tty_flush(tty, input=flush, output=.false.)
            if (dm_is_error(rc)) return
        end if

        raw = dm_ascii_unescape(request%request)
        rc  = dm_tty_write(tty, raw, nbytes=len_trim(raw))
    end function dm_tty_write_request

    subroutine dm_tty_close(tty)
        !! Closes file descriptor.
        use :: unix, only: c_close

        type(tty_type), intent(inout) :: tty !! TTY type.

        if (c_close(tty%fd) == 0) tty%fd = -1
    end subroutine dm_tty_close
end module dm_tty
