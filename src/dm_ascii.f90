! Author:  Philipp Engel
! Licence: ISC
module dm_ascii
    !! ASCII encoding/decoding procedures and predefined ASCII characters.
    implicit none (type, external)
    private

    character, parameter, public :: ASCII_NUL = achar(0)  ! null
    character, parameter, public :: ASCII_SOH = achar(1)  ! start of heading
    character, parameter, public :: ASCII_STX = achar(2)  ! start of text
    character, parameter, public :: ASCII_ETX = achar(3)  ! end of text
    character, parameter, public :: ASCII_EOT = achar(4)  ! end of transmission
    character, parameter, public :: ASCII_ENQ = achar(5)  ! enquiry
    character, parameter, public :: ASCII_ACK = achar(6)  ! acknowledge
    character, parameter, public :: ASCII_BEL = achar(7)  ! bell
    character, parameter, public :: ASCII_BS  = achar(8)  ! backspace
    character, parameter, public :: ASCII_TAB = achar(9)  ! horizontal tab
    character, parameter, public :: ASCII_LF  = achar(10) ! NL line feed, new line
    character, parameter, public :: ASCII_VT  = achar(11) ! vertical tab
    character, parameter, public :: ASCII_FF  = achar(12) ! NP form feed, new page
    character, parameter, public :: ASCII_CR  = achar(13) ! carriage return
    character, parameter, public :: ASCII_SO  = achar(14) ! shift out
    character, parameter, public :: ASCII_SI  = achar(15) ! shift in
    character, parameter, public :: ASCII_DLE = achar(16) ! data link escape
    character, parameter, public :: ASCII_DC1 = achar(17) ! device control 1
    character, parameter, public :: ASCII_DC2 = achar(18) ! device control 2
    character, parameter, public :: ASCII_DC3 = achar(19) ! device control 3
    character, parameter, public :: ASCII_DC4 = achar(20) ! device control 4
    character, parameter, public :: ASCII_NAK = achar(21) ! negative acknowledge
    character, parameter, public :: ASCII_SYN = achar(22) ! synchronous idle
    character, parameter, public :: ASCII_ETB = achar(23) ! end of trans. block
    character, parameter, public :: ASCII_CAN = achar(24) ! cancel
    character, parameter, public :: ASCII_EM  = achar(25) ! end of medium
    character, parameter, public :: ASCII_SUB = achar(26) ! substitute
    character, parameter, public :: ASCII_ESC = achar(27) ! escape
    character, parameter, public :: ASCII_FS  = achar(28) ! file separator
    character, parameter, public :: ASCII_GS  = achar(29) ! group separator
    character, parameter, public :: ASCII_RS  = achar(30) ! record separator
    character, parameter, public :: ASCII_US  = achar(31) ! unit separator

    character(len=*), parameter, public :: CR_LF = ASCII_CR // ASCII_LF !! `\r\n`

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
        !! is turned into literal `\n`.
        character(len=*), intent(in)  :: str !! Input string.
        character(len=:), allocatable :: res !! Output string.
        integer                       :: i

        res = ''

        do i = 1, len_trim(str)
            select case (str(i:i))
                case (ASCII_BEL)
                    res = res // '\a'
                case (ASCII_BS)
                    res = res // '\b'
                case (ASCII_TAB)
                    res = res // '\t'
                case (ASCII_LF)
                    res = res // '\n'
                case (ASCII_VT)
                    res = res // '\v'
                case (ASCII_FF)
                    res = res // '\f'
                case (ASCII_CR)
                    res = res // '\r'
                case (ASCII_ESC)
                    res = res // '\e'
                case (achar(92))
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
        is_control = (i < int(z'20')) .or. (i == int(z'7f'))
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
        is_printable = (i >= iachar(' ')) .and. (i <= int(z'7e'))
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
        is_white = (a == ' ') .or. (i >= int(z'09') .and. i <= int(z'0d'))
    end function dm_ascii_is_white_space

    pure function dm_ascii_unescape(str) result(res)
        !! Returns unescaped string of given string with escaped ASCII
        !! characters.
        character(len=*), intent(in)  :: str !! Input string.
        character(len=:), allocatable :: res !! Output string.

        integer :: i
        logical :: esc

        res = ''
        esc = .false.

        do i = 1, len_trim(str)
            if (esc) then
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
                    case (achar(92))
                        res = res // str(i:i)
                end select

                esc = .false.
            else
                if (str(i:i) == '\') then
                    esc = .true.
                else
                    res = res // str(i:i)
                end if
            end if
        end do
    end function dm_ascii_unescape
end module dm_ascii
