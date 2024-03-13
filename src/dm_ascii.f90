! Author:  Philipp Engel
! Licence: ISC
module dm_ascii
    !! ASCII encoding/decoding procedures and predefined ASCII characters.
    implicit none (type, external)
    private

    character, parameter, public :: ASCII_NUL = achar(0)  !! Null.
    character, parameter, public :: ASCII_SOH = achar(1)  !! Start of heading.
    character, parameter, public :: ASCII_STX = achar(2)  !! Start of text.
    character, parameter, public :: ASCII_ETX = achar(3)  !! End of text.
    character, parameter, public :: ASCII_EOT = achar(4)  !! End of transmission.
    character, parameter, public :: ASCII_ENQ = achar(5)  !! Enquiry.
    character, parameter, public :: ASCII_ACK = achar(6)  !! Acknowledge.
    character, parameter, public :: ASCII_BEL = achar(7)  !! Bell.
    character, parameter, public :: ASCII_BS  = achar(8)  !! Backspace.
    character, parameter, public :: ASCII_TAB = achar(9)  !! Horizontal tab.
    character, parameter, public :: ASCII_LF  = achar(10) !! NL line feed, new line.
    character, parameter, public :: ASCII_VT  = achar(11) !! Vertical tab.
    character, parameter, public :: ASCII_FF  = achar(12) !! NP form feed, new page.
    character, parameter, public :: ASCII_CR  = achar(13) !! Carriage return.
    character, parameter, public :: ASCII_SO  = achar(14) !! Shift out.
    character, parameter, public :: ASCII_SI  = achar(15) !! Shift in.
    character, parameter, public :: ASCII_DLE = achar(16) !! Data link escape.
    character, parameter, public :: ASCII_DC1 = achar(17) !! Device control 1.
    character, parameter, public :: ASCII_DC2 = achar(18) !! Device control 2.
    character, parameter, public :: ASCII_DC3 = achar(19) !! Device control 3.
    character, parameter, public :: ASCII_DC4 = achar(20) !! Device control 4.
    character, parameter, public :: ASCII_NAK = achar(21) !! Negative acknowledge.
    character, parameter, public :: ASCII_SYN = achar(22) !! Synchronous idle.
    character, parameter, public :: ASCII_ETB = achar(23) !! End of trans. block.
    character, parameter, public :: ASCII_CAN = achar(24) !! Cancel.
    character, parameter, public :: ASCII_EM  = achar(25) !! End of medium.
    character, parameter, public :: ASCII_SUB = achar(26) !! Substitute.
    character, parameter, public :: ASCII_ESC = achar(27) !! Escape.
    character, parameter, public :: ASCII_FS  = achar(28) !! File separator.
    character, parameter, public :: ASCII_GS  = achar(29) !! Group separator.
    character, parameter, public :: ASCII_RS  = achar(30) !! Record separator.
    character, parameter, public :: ASCII_US  = achar(31) !! Unit separator.

    character(len=*), parameter, public :: CR_LF = ASCII_CR // ASCII_LF !! Carriage return + line feed (`\r\n`).

    public :: dm_ascii_escape
    public :: dm_ascii_is_alpha
    public :: dm_ascii_is_alpha_num
    public :: dm_ascii_is_blank
    public :: dm_ascii_is_control
    public :: dm_ascii_is_digit
    public :: dm_ascii_is_hex_digit
    public :: dm_ascii_is_lower
    public :: dm_ascii_is_printable
    public :: dm_ascii_is_octal_digit
    public :: dm_ascii_is_upper
    public :: dm_ascii_is_white_space
    public :: dm_ascii_unescape
contains
    pure function dm_ascii_escape(str) result(res)
        !! Escapes given character string by replacing ASCII control characters
        !! by an escape string. For instance, character `ASCII_LF` (new line)
        !! is turned into literal `\n`. Non-printable characters without common
        !! literal are replaced with `\x` plus ASCII character code in
        !! hexadecimal, from `\x00` to `\x1F`.
        character(len=*), intent(in)  :: str !! Input string.
        character(len=:), allocatable :: res !! Output string.

        character(len=4) :: a
        integer          :: i, stat

        res = ''

        do i = 1, len_trim(str)
            select case (str(i:i))
                case (ASCII_NUL:ASCII_ACK, ASCII_SO:ASCII_SUB, ASCII_FS:ASCII_US) ! 0:6, 14:26, 28:31
                    write (a, '("\x", z2.2)', iostat=stat) iachar(str(i:i))
                    if (stat == 0) res = res // a
                case (ASCII_BEL) ! 7
                    res = res // '\a'
                case (ASCII_BS)  ! 8
                    res = res // '\b'
                case (ASCII_TAB) ! 9
                    res = res // '\t'
                case (ASCII_LF)  ! 10
                    res = res // '\n'
                case (ASCII_VT)  ! 11
                    res = res // '\v'
                case (ASCII_FF)  ! 12
                    res = res // '\f'
                case (ASCII_CR)  ! 13
                    res = res // '\r'
                case (ASCII_ESC) ! 27
                    res = res // '\e'
                case (achar(92)) ! \
                    res = res // '\\'
                case default
                    res = res // str(i:i)
            end select
        end do
    end function dm_ascii_escape

    pure elemental logical function dm_ascii_is_alpha(a) result(is_alpha)
        !! Returns whether character is alpha letter.
        character, intent(in) :: a !! Character to check.

        is_alpha = (a >= 'A' .and. a <= 'Z') .or. (a >= 'a' .and. a <= 'z')
    end function dm_ascii_is_alpha

    pure elemental logical function dm_ascii_is_alpha_num(a) result(is_alpha_num)
        !! Returns whether character is alpha-numeric.
        character, intent(in) :: a !! Character to check.

        is_alpha_num = (a >= '0' .and. a <= '9') .or. &
                       (a >= 'A' .and. a <= 'Z') .or. &
                       (a >= 'a' .and. a <= 'z')
    end function dm_ascii_is_alpha_num

    pure elemental logical function dm_ascii_is_blank(a) result(is_blank)
        !! Returns whether character is space or tabular.
        character, intent(in) :: a !! Character to check.
        integer               :: i

        i = iachar(a)
        is_blank = (a == ' ') .or. (i == int(z'09'))
    end function dm_ascii_is_blank

    pure elemental logical function dm_ascii_is_control(a) result(is_control)
        !! Returns whether character is control character.
        character, intent(in) :: a !! Character to check.
        integer               :: i

        i = iachar(a)
        is_control = (i < int(z'20')) .or. (i == int(z'7F'))
    end function dm_ascii_is_control

    pure elemental logical function dm_ascii_is_digit(a) result(is_digit)
        !! Returns whether character is digit.
        character, intent(in) :: a !! Character to check.

        is_digit = (a >= '0') .and. (a <= '9')
    end function dm_ascii_is_digit

    pure elemental logical function dm_ascii_is_hex_digit(a) result(is_hex)
        !! Returns whether character is hex digit.
        character, intent(in) :: a !! Character to check.

        is_hex = (a >= '0' .and. a <= '9') .or. &
                 (a >= 'A' .and. a <= 'F') .or. &
                 (a >= 'a' .and. a <= 'f')
    end function dm_ascii_is_hex_digit

    pure elemental logical function dm_ascii_is_lower(a) result(is_lower)
        !! Returns whether character is lower-case.
        character, intent(in) :: a !! Character to check.
        integer               :: i

        i = iachar(a)
        is_lower = (i >= iachar('a')) .and. (i <= iachar('z'))
    end function dm_ascii_is_lower

    pure elemental logical function dm_ascii_is_octal_digit(a) result(is_octal)
        !! Returns whether character is an octal digit.
        character, intent(in) :: a !! Character to check.

        is_octal = (a >= '0') .and. (a <= '7')
    end function dm_ascii_is_octal_digit

    pure elemental logical function dm_ascii_is_printable(a) result(is_printable)
        !! Returns whether character is printable.
        character, intent(in) :: a !! Character to check.
        integer               :: i

        i = iachar(a)
        is_printable = (i >= iachar(' ')) .and. (i <= int(z'7E'))
    end function dm_ascii_is_printable

    pure elemental logical function dm_ascii_is_upper(a) result(is_upper)
        !! Returns whether character is upper-case.
        character, intent(in) :: a !! Character to check.

        is_upper = (a >= 'A') .and. (a <= 'Z')
    end function dm_ascii_is_upper

    pure elemental logical function dm_ascii_is_white_space(a) result(is_white)
        !! Returns whether character is white space (either `SPACE`, `TAB`,
        !! `LF`, `VT`, `FF`, or `CR`).
        character, intent(in) :: a !! Character to check.
        integer               :: i

        i = iachar(a)
        is_white = (a == ' ') .or. (i >= int(z'09') .and. i <= int(z'0D'))
    end function dm_ascii_is_white_space

    pure function dm_ascii_unescape(str) result(res)
        !! Returns unescaped string of given string with escaped ASCII
        !! characters.
        character(len=*), intent(in)  :: str !! Input string.
        character(len=:), allocatable :: res !! Output string.

        integer   :: i, k, n, stat
        logical   :: esc, npc

        res = ''
        esc = .false. ! Escaped flag.
        npc = .false. ! Non-printable character flag.

        i = 1
        n = len_trim(str)

        do while (i <= n)
            if (esc) then
                ! Escaped character literals.
                esc = .false.

                select case (str(i:i))
                    case ('a')
                        res = res // ASCII_BEL
                    case ('b')
                        res = res // ASCII_BS
                    case ('t')
                        res = res // ASCII_TAB
                    case ('n')
                        res = res // ASCII_LF
                    case ('v')
                        res = res // ASCII_VT
                    case ('f')
                        res = res // ASCII_FF
                    case ('r')
                        res = res // ASCII_CR
                    case ('e')
                        res = res // ASCII_ESC
                    case ('x')
                        npc = .true.
                    case (achar(92)) ! \
                        res = res // str(i:i)
                end select
            else if (npc) then
                ! Escaped non-printable characters, from `\x00` to `\xFF`.
                npc = .false.

                if (i + 1 <= n) then
                    read (str(i:i + 1), '(z2)', iostat=stat) k

                    if (stat == 0) then
                        res = res // achar(k)
                    else
                        res = res // '\x' // str(i:i + 1)
                    end if

                    i = i + 1
                else
                    res = res // '\x' // str(i:i)
                end if
            else
                if (str(i:i) == '\') then
                    ! Found escape character.
                    esc = .true.
                else
                    ! All other characters.
                    res = res // str(i:i)
                end if
            end if

            i = i + 1
        end do
    end function dm_ascii_unescape
end module dm_ascii
