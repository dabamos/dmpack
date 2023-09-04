! Author:  Philipp Engel
! Licence: ISC
module dm_tty
    !! Serial port access (TTY/PTY) on Unix.
    use, intrinsic :: iso_c_binding
    use :: dm_error
    use :: dm_file
    use :: dm_type
    implicit none (type, external)
    private

    integer, parameter, public :: TTY_RDONLY  = 1
    integer, parameter, public :: TTY_WRONLY  = 2
    integer, parameter, public :: TTY_RDWR    = 3

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

    integer, parameter, public :: TTY_PARITY_NONE = 1
    integer, parameter, public :: TTY_PARITY_EVEN = 2
    integer, parameter, public :: TTY_PARITY_ODD  = 3

    integer, parameter, public :: TTY_PARITY_NAME_LEN = 4

    integer, parameter, public :: TTY_BYTE_SIZE5  = 1
    integer, parameter, public :: TTY_BYTE_SIZE6  = 2
    integer, parameter, public :: TTY_BYTE_SIZE7  = 3
    integer, parameter, public :: TTY_BYTE_SIZE8  = 4

    integer, parameter, public :: TTY_STOP_BITS1  = 1
    integer, parameter, public :: TTY_STOP_BITS2  = 2

    ! Serial port type (default: 9600 baud, 8N1).
    type, public :: tty_type
        !! TTY/PTY data type.
        integer(kind=c_int), private :: fd        = -1              !! Unix file descriptor.
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
    end type tty_type

    public :: dm_tty_baud_rate_from_value
    public :: dm_tty_byte_size_from_value
    public :: dm_tty_close
    public :: dm_tty_connected
    public :: dm_tty_flush
    public :: dm_tty_open
    public :: dm_tty_parity_from_name
    public :: dm_tty_read
    public :: dm_tty_read_raw
    public :: dm_tty_set_attributes
    public :: dm_tty_stop_bits_from_value
    public :: dm_tty_valid_baud_rate
    public :: dm_tty_valid_byte_size
    public :: dm_tty_valid_parity
    public :: dm_tty_valid_stop_bits
    public :: dm_tty_valid_timeout
    public :: dm_tty_write
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

    integer function dm_tty_flush(tty, input, output) result(rc)
        !! Flushes TTY input and output buffer.
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

    integer function dm_tty_open(tty) result(rc)
        !! Opens TTY/PTS device in set access mode and applies serial port
        !! attributes.
        use :: unix
        type(tty_type), intent(inout) :: tty !! TTY type.

        integer(kind=c_int) :: flags

        rc = E_EXIST
        if (dm_tty_connected(tty)) return

        ! Set flags.
        rc = E_INVALID
        flags = ior(O_NOCTTY, O_SYNC)
        flags = ior(flags, O_NONBLOCK)

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
        tty%fd = c_open(trim(tty%path) // c_null_char, flags, 0)
        if (tty%fd < 0) return

        rc = dm_tty_set_attributes(tty)
        if (dm_is_error(rc)) return

        rc = dm_tty_flush(tty)
    end function dm_tty_open

    logical function dm_tty_connected(tty) result(connected)
        !! Return `.true.` if TTY is connected, else `.false.`.
        type(tty_type), intent(inout) :: tty

        connected = .false.
        if (tty%fd /= -1) connected = .true.
    end function dm_tty_connected

    integer function dm_tty_parity_from_name(name, error) result(parity)
        !! Returns parity from character string (`none`, `even`, `odd`). If the
        !! parity is not recognised, returns 0 by default and sets optional
        !! argument `error` to `E_INVALID`.
        character(len=*), intent(in)            :: name  !! Parity name.
        integer,          intent(out), optional :: error !! Error code.

        parity = 0
        if (present(error)) error = E_INVALID

        select case (name)
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

    integer function dm_tty_read(tty, buffer, del, nbytes) result(rc)
        !! Reads from TTY into `buf` until delimiter `del` occurs. The
        !! number of bytes read is returned in `n`.
        type(tty_type),   intent(inout)         :: tty    !! TTY type.
        character(len=*), intent(inout)         :: buffer !! Input buffer.
        character(len=*), intent(in)            :: del    !! Delimiter.
        integer(kind=i8), intent(out), optional :: nbytes !! Number of bytes read.

        character        :: a
        integer          :: i, j, k
        integer(kind=i8) :: n, sz

        rc = E_READ

        i = 1
        j = len(buffer)
        k = len(del)
        n = 0_i8

        do
            rc = E_BOUNDS
            if (i > j) exit

            rc = E_READ
            sz = dm_tty_read_raw(tty, a)

            if (sz > 0) then
                buffer(i:i) = a
                i = i + 1
                n = n + 1

                rc = E_NONE
                if (buffer(i - k:i) == del) exit
                cycle
            end if

            exit
        end do

        if (present(nbytes)) nbytes = n
    end function dm_tty_read

    integer(kind=i8) function dm_tty_read_raw(tty, byte) result(n)
        !! Reads single byte from file descriptor.
        use :: unix, only: c_read
        type(tty_type),    intent(inout) :: tty  !! TTY type.
        character, target, intent(out)   :: byte !! Byte read.

        n = int(c_read(tty%fd, c_loc(byte), int(1, kind=c_size_t)), kind=i8)
    end function dm_tty_read_raw

    integer function dm_tty_set_attributes(tty) result(rc)
        !! Sets terminal attributes.
        use :: unix
        type(tty_type), intent(inout) :: tty !! TTY type.

        integer(kind=c_speed_t)     :: baud_rate
        integer(kind=c_int)         :: byte_size
        integer(kind=c_int)         :: parity
        integer(kind=c_int)         :: stop_bits
        integer(kind=c_int), target :: flags
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
        if (c_tcgetattr(tty%fd, termios) /= 0) return

        ! Set baud rate (I/O).
        if (c_cfsetispeed(termios, baud_rate) /= 0) return
        if (c_cfsetospeed(termios, baud_rate) /= 0) return

        ! Set byte size.
        termios%c_cflag = iand(termios%c_cflag, not(CSIZE))
        termios%c_cflag = ior(termios%c_cflag, byte_size)

        ! Set stop bits.
        termios%c_cflag = iand(termios%c_cflag, not(CSTOPB))
        termios%c_cflag = ior(termios%c_cflag, stop_bits)

        ! Set parity.
        termios%c_cflag = iand(termios%c_cflag, not(PARENB + PARODD))
        termios%c_cflag = ior(termios%c_cflag, parity)

        ! Ignore modem controls, enable reading.
        termios%c_cflag = ior(termios%c_cflag, ior(CLOCAL, CREAD))

        ! No special handling of received bytes.
        termios%c_iflag = iand(termios%c_iflag, not(IGNBRK + BRKINT + PARMRK + ISTRIP + INLCR + IGNCR + ICRNL))

        ! Turn XON/XOFF control off.
        termios%c_iflag = iand(termios%c_iflag, not(IXON + IXOFF + IXANY))

        ! No echo.
        termios%c_lflag = iand(termios%c_lflag, not(ECHO + ECHOE + ECHONL))

        ! No canonical processing.
        termios%c_lflag = iand(termios%c_lflag, not(ICANON))

        ! No signal chars.
        termios%c_lflag = iand(termios%c_lflag, not(ISIG))

        ! No special interpretation of output bytes.
        termios%c_oflag = iand(termios%c_oflag, not(OPOST + ONLCR))

        ! Blocking read with timeout in 1/10 seconds.
        termios%c_cc(VMIN)  = 0
        termios%c_cc(VTIME) = tty%timeout * 10

        ! Set attributes.
        if (c_tcsetattr(tty%fd, TCSANOW, termios) /= 0) return

        ! Set RTS, DTR.
        if (c_ioctl(tty%fd, TIOCMGET, c_loc(stat)) /= 0) return
        if (tty%rts) stat = ior(stat, TIOCM_RTS)
        if (tty%dtr) stat = ior(stat, TIOCM_DTR)
        if (c_ioctl(tty%fd, TIOCMSET, c_loc(stat)) /= 0) return

        ! Set blocking read.
        if (tty%blocking) then
            flags = c_fcntl(tty%fd, F_GETFL, c_null_ptr)
            flags = iand(flags, not(O_NONBLOCK))
            if (c_fcntl(tty%fd, F_SETFL, c_loc(flags)) /= 0) return
        end if

        rc = E_NONE
    end function dm_tty_set_attributes

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
            case (TTY_B0)
            case (TTY_B50)
            case (TTY_B75)
            case (TTY_B110)
            case (TTY_B134)
            case (TTY_B150)
            case (TTY_B200)
            case (TTY_B300)
            case (TTY_B600)
            case (TTY_B1200)
            case (TTY_B1800)
            case (TTY_B2400)
            case (TTY_B4800)
            case (TTY_B9600)
            case (TTY_B19200)
            case (TTY_B38400)
            case (TTY_B57600)
            case (TTY_B115200)
            case (TTY_B230400)
            case (TTY_B460800)
            case (TTY_B921600)
                valid = .true.
        end select
    end function dm_tty_valid_baud_rate

    pure elemental logical function dm_tty_valid_byte_size(byte_size) result(valid)
        !! Returns `.true.` if given byte size value is valid, else `.false.`.
        integer, intent(in) :: byte_size !! Byte size.

        valid = .false.

        select case (byte_size)
            case (TTY_BYTE_SIZE5)
            case (TTY_BYTE_SIZE6)
            case (TTY_BYTE_SIZE7)
            case (TTY_BYTE_SIZE8)
                valid = .true.
        end select
    end function dm_tty_valid_byte_size

    pure elemental logical function dm_tty_valid_parity(parity) result(valid)
        !! Returns `.true.` if given parity value is valid, else `.false.`.
        integer, intent(in) :: parity !! Parity.

        valid = .false.

        select case (parity)
            case (TTY_PARITY_NONE)
            case (TTY_PARITY_EVEN)
            case (TTY_PARITY_ODD)
                valid = .true.
        end select
    end function dm_tty_valid_parity

    pure elemental logical function dm_tty_valid_stop_bits(stop_bits) result(valid)
        !! Returns `.true.` if given stop bits value is valid, else `.false.`.
        integer, intent(in) :: stop_bits !! Stop bits.

        valid = .false.

        select case (stop_bits)
            case (TTY_STOP_BITS1)
            case (TTY_STOP_BITS2)
                valid = .true.
        end select
    end function dm_tty_valid_stop_bits

    pure elemental logical function dm_tty_valid_timeout(timeout) result(valid)
        !! Returns `.true.` if given timeout value is valid, else `.false.`.
        integer, intent(in) :: timeout !! Timeout.

        valid = .false.
        if (timeout >= 0) valid = .true.
    end function dm_tty_valid_timeout

    integer function dm_tty_write(tty, bytes) result(rc)
        !! Writes given string to TTY.
        use :: unix, only: c_write
        type(tty_type),   intent(inout) :: tty   !! TTY type.
        character(len=*), intent(in)    :: bytes !! Bytes to send.

        character(kind=c_char), target :: a
        integer                        :: i
        integer(kind=c_size_t)         :: n

        rc = E_WRITE

        do i = 1, len(bytes)
            a = bytes(i:i)
            n = c_write(tty%fd, c_loc(a), int(1, kind=c_size_t))
            if (n /= 1) return
        end do

        rc = E_NONE
    end function dm_tty_write

    subroutine dm_tty_close(tty)
        !! Closes file descriptor.
        use :: unix, only: c_close
        type(tty_type), intent(inout) :: tty !! TTY type.

        if (c_close(tty%fd) == 0) tty%fd = -1
    end subroutine dm_tty_close
end module dm_tty
