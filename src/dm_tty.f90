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

    integer, parameter, public :: TTY_BYTE_SIZE5  = 1
    integer, parameter, public :: TTY_BYTE_SIZE6  = 2
    integer, parameter, public :: TTY_BYTE_SIZE7  = 3
    integer, parameter, public :: TTY_BYTE_SIZE8  = 4

    integer, parameter, public :: TTY_STOP_BITS1  = 1
    integer, parameter, public :: TTY_STOP_BITS2  = 2

    ! Serial port type (default: 9600 baud, 8N1).
    type, public :: tty_type
        !! TTY/PTY data type.
        character(len=FILE_PATH_LEN) :: path      = ' '             !! TTY/PTY path.
        integer                      :: access    = TTY_RDWR        !! Access mode (read/write).
        integer                      :: baud      = TTY_B9600       !! Baud rate (9600).
        integer                      :: byte_size = TTY_BYTE_SIZE8  !! Byte size (8).
        integer                      :: stop_bits = TTY_STOP_BITS1  !! Stop bits (1).
        integer                      :: parity    = TTY_PARITY_NONE !! Parity (none).
        integer                      :: timeout   = 5               !! Read timeout in seconds.
        logical                      :: rts       = .false.         !! RTS level.
        logical                      :: dtr       = .false.         !! DTR level.
        logical                      :: blocking  = .true.          !! Blocking read.
        integer                      :: fd        = -1              !! Unix file descriptor.
    end type tty_type

    public :: dm_tty_close
    public :: dm_tty_open
    public :: dm_tty_read
    public :: dm_tty_read_raw
    public :: dm_tty_set_attributes
    public :: dm_tty_write
contains
    integer function dm_tty_open(tty) result(rc)
        use :: unix
        type(tty_type), intent(inout) :: tty !! TTY type.
        integer                       :: flags

        rc = E_INVALID

        ! Set flags.
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

        rc = E_IO

        ! Open TTY.
        tty%fd = c_open(trim(tty%path) // c_null_char, flags, 0)
        if (tty%fd < 0) return

        rc = dm_tty_set_attributes(tty)
        if (c_tcflush(tty%fd, TCIFLUSH) /= 0) return
        if (c_tcflush(tty%fd, TCOFLUSH) /= 0) return

        rc = E_NONE
    end function dm_tty_open

    integer function dm_tty_read(tty, buf, del, n) result(rc)
        !! Reads from TTY into `buf` until delimiter `del` occurs. The
        !! number of bytes read is returned in `n`.
        type(tty_type),    intent(inout) :: tty !! TTY type.
        character(len=*),  intent(inout) :: buf !! Buffer.
        character(len=*),  intent(in)    :: del !! Delimiter.
        integer(kind=i8),  intent(out)   :: n   !! Number of bytes read.

        character        :: a
        integer          :: i, j, k
        integer(kind=i8) :: sz

        rc = E_IO

        i = 1
        j = len(buf)
        k = len(del)
        n = int(0, kind=i8)

        do
            if (i > j) then
                rc = E_BOUNDS
                return
            end if

            sz = dm_tty_read_raw(tty, a)

            if (sz > 0) then
                buf(i:i) = a
                i = i + 1
                n = n + 1
                if (buf(i - k:i) == del) exit
            else
                if (sz == 0) rc = E_TIMEOUT
                return
            end if
        end do

        rc = E_NONE
    end function dm_tty_read

    integer(kind=i8) function dm_tty_read_raw(tty, byte) result(n)
        !! Reads single byte from file descriptor.
        use :: unix, only: c_read
        type(tty_type),    intent(inout) :: tty  !! TTY type.
        character, target, intent(out)   :: byte !! Read byte.

        n = c_read(tty%fd, c_loc(byte), len(byte, kind=c_size_t))
    end function dm_tty_read_raw

    integer function dm_tty_set_attributes(tty) result(rc)
        !! Sets terminal attributes.
        use :: unix
        type(tty_type), intent(inout) :: tty !! TTY type.

        integer         :: baud
        integer         :: byte_size
        integer         :: parity
        integer         :: stop_bits
        integer, target :: flags
        integer, target :: stat
        type(c_termios) :: term

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
        select case (tty%baud)
            case (TTY_B0)
                baud = B0
            case (TTY_B50)
                baud = B50
            case (TTY_B75)
                baud = B75
            case (TTY_B110)
                baud = B110
            case (TTY_B134)
                baud = B134
            case (TTY_B150)
                baud = B150
            case (TTY_B200)
                baud = B200
            case (TTY_B300)
                baud = B300
            case (TTY_B600)
                baud = B600
            case (TTY_B1200)
                baud = B1200
            case (TTY_B1800)
                baud = B1800
            case (TTY_B2400)
                baud = B2400
            case (TTY_B4800)
                baud = B4800
            case (TTY_B9600)
                baud = B9600
            case (TTY_B19200)
                baud = B19200
            case (TTY_B38400)
                baud = B38400
            case (TTY_B57600)
                baud = B57600
            case (TTY_B115200)
                baud = B115200
            case (TTY_B230400)
                baud = B230400
            case (TTY_B460800)
                baud = B460800
            case (TTY_B921600)
                baud = B921600
            case default
                return
        end select

        rc = E_IO

        ! Get current attributes.
        if (c_tcgetattr(tty%fd, term) /= 0) return

        ! Set baud rate (I/O).
        if (c_cfsetispeed(term, baud) /= 0) return
        if (c_cfsetospeed(term, baud) /= 0) return

        ! Set byte size.
        term%c_cflag = iand(term%c_cflag, not(CSIZE))
        term%c_cflag = ior(term%c_cflag, byte_size)

        ! Set stop bits.
        term%c_cflag = iand(term%c_cflag, not(CSTOPB))
        term%c_cflag = ior(term%c_cflag, stop_bits)

        ! Set parity.
        term%c_cflag = iand(term%c_cflag, not(PARENB + PARODD))
        term%c_cflag = ior(term%c_cflag, parity)

        ! Ignore modem controls, enable reading.
        term%c_cflag = ior(term%c_cflag, ior(CLOCAL, CREAD))

        ! No special handling of received bytes.
        term%c_iflag = ior(term%c_iflag, not(IGNBRK + BRKINT + PARMRK + ISTRIP + INLCR + IGNCR + ICRNL))

        ! No echo.
        term%c_lflag = iand(term%c_lflag, not(ECHO + ECHOE + ECHONL))

        ! No canonical processing.
        term%c_lflag = iand(term%c_lflag, not(ICANON))

        ! No signal chars.
        term%c_lflag = iand(term%c_lflag, not(ISIG))

        ! No special interpretation of output bytes.
        term%c_oflag = iand(term%c_oflag, not(OPOST + ONLCR))

        ! Blocking read with timeout in 1/10 seconds.
        term%c_cc(VMIN)  = 0
        term%c_cc(VTIME) = tty%timeout * 10

        ! Turn XON/XOFF control off.
        term%c_iflag = iand(term%c_iflag, not(IXON + IXOFF + IXANY))

        ! Set attributes.
        if (c_tcsetattr(tty%fd, TCSANOW, term) /= 0) return

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

    integer function dm_tty_write(tty, bytes) result(rc)
        !! Writes given string to TTY.
        use :: unix, only: c_write
        type(tty_type),   intent(inout) :: tty   !! TTY type.
        character(len=*), intent(in)    :: bytes !! Bytes to send.

        character, target :: a
        integer           :: i
        integer(kind=i8)  :: n

        rc = E_IO

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
        integer                       :: rc

        rc = c_close(tty%fd)
    end subroutine dm_tty_close
end module dm_tty
