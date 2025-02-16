! Author:  Philipp Engel
! Licence: ISC
module dm_string
    !! String utility routines.
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    character(len=*), parameter :: FMT_INTEGER = '(i0)'
    character(len=*), parameter :: FMT_REAL    = '(f0.12)'

    type, public :: string_type
        !! Derived type of allocatable character to be stored in an array.
        character(len=:), allocatable :: data !! String data.
    end type string_type

    interface dm_lower
        !! Alias for procedure.
        module procedure :: dm_string_lower
    end interface dm_lower

    interface dm_upper
        !! Alias for procedure.
        module procedure :: dm_string_upper
    end interface dm_upper

    interface dm_to_lower
        !! Alias for procedure.
        module procedure :: dm_string_to_lower
    end interface dm_to_lower

    interface dm_to_upper
        !! Alias for procedure.
        module procedure :: dm_string_to_upper
    end interface dm_to_upper

    interface dm_string_hex_to_int
        !! Converts hexadecimal number to integer.
        module procedure :: string_hex_to_int32
        module procedure :: string_hex_to_int64
    end interface dm_string_hex_to_int

    interface dm_string_from
        !! Generic number to string converter.
        module procedure :: string_from_int32
        module procedure :: string_from_int64
        module procedure :: string_from_real32
        module procedure :: string_from_real64
    end interface dm_string_from

    interface dm_string_to
        !! Generic string to number converter.
        module procedure :: string_to_int16
        module procedure :: string_to_int32
        module procedure :: string_to_int64
        module procedure :: string_to_real32
        module procedure :: string_to_real64
    end interface dm_string_to

    ! Public procedures.
    public :: dm_lower
    public :: dm_upper

    public :: dm_to_lower
    public :: dm_to_upper

    public :: dm_string_from
    public :: dm_string_to

    public :: dm_string_count_char
    public :: dm_string_count_lines
    public :: dm_string_count_substring
    public :: dm_string_hex_to_int
    public :: dm_string_is_empty
    public :: dm_string_is_present
    public :: dm_string_is_printable
    public :: dm_string_replace
    public :: dm_string_split
    public :: dm_string_starts_with

    public :: dm_string_lower
    public :: dm_string_upper

    public :: dm_string_to_lower
    public :: dm_string_to_upper

    ! Public string type procedures.
    public :: dm_string_type_allocate
    public :: dm_string_type_destroy

    ! Private procedures.
    private :: string_from_int32
    private :: string_from_int64
    private :: string_from_real32
    private :: string_from_real64
    private :: string_hex_to_int32
    private :: string_hex_to_int64
    private :: string_to_int16
    private :: string_to_int32
    private :: string_to_int64
    private :: string_to_real32
    private :: string_to_real64
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental integer function dm_string_count_char(string, a, n) result(count)
        !! Counts occurences of character `a` in `string`.
        character(len=*), intent(in)           :: string !! Input.
        character,        intent(in)           :: a      !! Character to count.
        integer,          intent(in), optional :: n      !! String length.

        integer :: i, n_

        count = 0

        if (present(n)) then
            n_ = n
        else
            n_ = len(string)
        end if

        do i = 1, n_
            if (string(i:i) == a) count = count + 1
        end do
    end function dm_string_count_char

    integer function dm_string_count_lines(string) result(count)
        !! Returns the number of line breaks in string.
        use :: dm_ascii, only: ASCII_LF

        character(len=*), intent(inout) :: string !! Input string.

        count = dm_string_count_char(string, ASCII_LF, len_trim(string)) + 1
    end function dm_string_count_lines

    pure elemental integer function dm_string_count_substring(string1, string2) result(count)
        !! Returns the number of occurences of `string2` in `string1`.
        character(len=*), intent(in) :: string1 !! Haystack string.
        character(len=*), intent(in) :: string2 !! Needle string.

        integer :: p
        integer :: pos_n

        count = 0
        if (len(string2) == 0) return

        p = 1

        do
            pos_n = index(string1(p:), string2)
            if (pos_n == 0) return
            count = count + 1
            p = p + pos_n + len(string2)
        end do
    end function dm_string_count_substring

    logical function dm_string_is_empty(string) result(is)
        !! Returns `.true.` if given allocatable string is not passed, not
        !! allocated, or contains only white spaces.
        character(len=:), allocatable, intent(inout), optional :: string !! Input string.

        is = .true.
        if (.not. present(string))   return
        if (.not. allocated(string)) return
        if (len_trim(string) == 0)   return
        is = .false.
    end function dm_string_is_empty

    pure logical function dm_string_is_present(string) result(is)
        !! Returns `.true.` if given string is present and not empty.
        character(len=*), intent(in), optional :: string !! Input string.

        is = .false.
        if (.not. present(string)) return
        if (len_trim(string) == 0) return
        is = .true.
    end function dm_string_is_present

    pure logical function dm_string_is_printable(string) result(is)
        !! Returns `.true.` if all characters is given string are printable
        !! ASCII characters.
        use :: dm_ascii, only: dm_ascii_is_printable

        character(len=*), intent(in) :: string !! String to validate.

        integer :: i

        is = .false.

        do i = 1, len_trim(string)
            if (.not. dm_ascii_is_printable(string(i:i))) return
        end do

        is = .true.
    end function dm_string_is_printable

    pure elemental function dm_string_to_lower(string) result(lower)
        !! Returns given string in lower case.
        character(len=*), intent(in) :: string !! String to convert.
        character(len=len(string))   :: lower  !! Result.

        character :: a
        integer   :: i

        do i = 1, len(string)
            a = string(i:i)
            if (a >= 'A' .and. a <= 'Z') a = achar(iachar(a) + 32)
            lower(i:i) = a
        end do
    end function dm_string_to_lower

    pure elemental function dm_string_to_upper(string) result(upper)
        !! Returns given string in upper case.
        character(len=*), intent(in) :: string !! String to convert.
        character(len=len(string))   :: upper  !! Result.

        character :: a
        integer   :: i

        do i = 1, len(string)
            a = string(i:i)
            if (a >= 'a' .and. a <= 'z') a = achar(iachar(a) - 32)
            upper(i:i) = a
        end do
    end function dm_string_to_upper

    ! **************************************************************************
    ! STRING TYPE SUBROUTINES.
    ! **************************************************************************
    pure elemental subroutine dm_string_type_allocate(string, n)
        !! Allocates string type to empty character of length 0 or `n`, if not
        !! allocated already.
        type(string_type), intent(inout)        :: string !! String type.
        integer,           intent(in), optional :: n      !! Length of string data.

        integer :: n_

        n_ = 0
        if (present(n)) n_ = max(0, n)

        if (.not. allocated(string%data)) allocate (character(len=n_) :: string%data)
    end subroutine dm_string_type_allocate

    pure elemental subroutine dm_string_type_destroy(string)
        !! Deallocates allocatable character inside of string type.
        type(string_type), intent(inout) :: string !! String type.

        if (allocated(string%data)) deallocate (string%data)
    end subroutine dm_string_type_destroy

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    pure elemental subroutine dm_string_lower(string)
        !! Converts given string to lower case.
        character(len=*), intent(inout) :: string !! Input/output string.

        character :: a
        integer   :: i

        do i = 1, len(string)
            a = string(i:i)
            if (a >= 'A' .and. a <= 'Z') string(i:i) = achar(iachar(a) + 32)
        end do
    end subroutine dm_string_lower

    subroutine dm_string_replace(string, a, b)
        !! Replaces character `a` in `string` with `b`.
        character(len=*), intent(inout) :: string !! String to parse.
        character,        intent(in)    :: a      !! Character to replace.
        character,        intent(in)    :: b      !! Substitute character.

        integer :: i

        do i = 1, len(string)
            if (string(i:i) == a) string(i:i) = b
        end do
    end subroutine dm_string_replace

    pure subroutine dm_string_split(string, array, del, n)
        !! Splits a string by a given delimiter into an array of strings.
        character(len=*), intent(in)            :: string   !! String to split.
        character(len=*), intent(inout)         :: array(:) !! Splitted components.
        character(len=*), intent(in)            :: del      !! Delimiter.
        integer,          intent(out), optional :: n        !! Number of array elements.

        integer :: i, pos1, pos2

        pos1 = 1
        i    = 0

        do
            pos2 = index(string(pos1:), del)

            if (pos2 == 0) then
                i = i + 1
                if (i > size(array)) exit
                array(i) = string(pos1:)
                exit
            end if

            i = i + 1
            array(i) = string(pos1:pos1 + pos2 - 2)
            pos1 = pos1 + pos2
        end do

        if (present(n)) n = i
    end subroutine dm_string_split

    pure elemental logical function dm_string_starts_with(string1, string2) result(starts)
        !! Returns `.true.` if `string1` starts with `string2`.
        character(len=*), intent(in) :: string1 !! First string.
        character(len=*), intent(in) :: string2 !! Second string.

        integer :: n1, n2

        n1 = len_trim(string1)
        n2 = len_trim(string2)

        starts = .false.
        if (n1 == 0 .or. n2 == 0 .or. n1 < n2) return

        starts = (string1(:n2) == string2)
    end function dm_string_starts_with

    pure elemental subroutine dm_string_upper(string)
        !! Converts given string to upper case.
        character(len=*), intent(inout) :: string !! Input/output string.

        character :: a
        integer   :: i

        do i = 1, len(string)
            a = string(i:i)
            if (a >= 'a' .and. a <= 'z') string(i:i) = achar(iachar(a) - 32)
        end do
    end subroutine dm_string_upper

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    pure elemental subroutine string_hex_to_int32(string, value, error)
        !! Returns hexadecimal value as 4-byte integer. The input string must
        !! start with `0x` or `0X`. The routine returns the following error
        !! codes in `error`:
        !!
        !! * `E_FORMAT` if string is not in expected format.
        !! * `E_INVALID` if string does not start with `0x`.
        !!
        character(len=*), intent(in)            :: string !! Hex. string of value.
        integer(kind=i4), intent(out)           :: value  !! Value.
        integer,          intent(out), optional :: error  !! Error.

        character(len=2) :: prefix
        integer          :: stat

        if (present(error)) error = E_FORMAT
        read (string, '(a2, z8)', iostat=stat) prefix, value
        if (stat /= 0) return

        if (present(error)) error = E_INVALID
        if (prefix /= '0x' .and. prefix /= '0X') return

        if (present(error)) error = E_NONE
    end subroutine string_hex_to_int32

    pure elemental subroutine string_hex_to_int64(string, value, error)
        !! Returns hexadecimal value as 8-byte integer. The input string must
        !! start with `0x` or `0X`. The routine returns the following error
        !! codes in `error`:
        !!
        !! * `E_FORMAT` if string is not in expected format.
        !! * `E_INVALID` if string does not start with `0x`.
        !!
        character(len=*), intent(in)            :: string !! Hex. string of value.
        integer(kind=i8), intent(out)           :: value  !! Value.
        integer,          intent(out), optional :: error  !! Error.

        character(len=2) :: prefix
        integer          :: stat

        if (present(error)) error = E_FORMAT
        read (string, '(a2, z16)', iostat=stat) prefix, value
        if (stat /= 0) return

        if (present(error)) error = E_INVALID
        if (prefix /= '0x' .and. prefix /= '0X') return

        if (present(error)) error = E_NONE
    end subroutine string_hex_to_int64

    pure subroutine string_from_int32(value, string, error)
        !! Returns string representation of given 4-byte integer.
        integer(kind=i4),              intent(in)            :: value  !! Input.
        character(len=:), allocatable, intent(out)           :: string !! Output.
        integer,                       intent(out), optional :: error  !! Error code.

        integer :: n, stat

        if (value == 0) then
            n = 1
        else
            n = floor(log10(real(abs(value))) + 1)
            if (value < 0) n = n + 1
        end if

        if (present(error)) error = E_FORMAT
        allocate (character(len=n) :: string)
        write (string, FMT_INTEGER, iostat=stat) value
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_from_int32

    pure subroutine string_from_int64(value, string, error)
        !! Returns string representation of given 8-byte integer.
        integer(kind=i8),              intent(in)            :: value  !! Input.
        character(len=:), allocatable, intent(out)           :: string !! Output.
        integer,                       intent(out), optional :: error  !! Error code.

        integer :: n, stat

        if (value == 0) then
            n = 1
        else
            n = floor(log10(real(abs(value))) + 1)
            if (value < 0) n = n + 1
        end if

        if (present(error)) error = E_FORMAT
        allocate (character(len=n) :: string)
        write (string, FMT_INTEGER, iostat=stat) value
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_from_int64

    pure subroutine string_from_real32(value, string, error)
        !! Returns string representation of given 4-byte real.
        real(kind=r4),                 intent(in)            :: value  !! Input.
        character(len=:), allocatable, intent(out)           :: string !! Output.
        integer,                       intent(out), optional :: error  !! Error code.

        character(len=20) :: buffer
        integer           :: stat

        if (present(error)) error = E_FORMAT
        write (buffer, FMT_REAL, iostat=stat) value
        if (stat /= 0) then
            string = ''
            return
        end if
        if (present(error)) error = E_NONE
        string = trim(buffer)
    end subroutine string_from_real32

    pure subroutine string_from_real64(value, string, error)
        !! Returns string representation of given 8-byte real.
        real(kind=r8),                 intent(in)            :: value  !! Input.
        character(len=:), allocatable, intent(out)           :: string !! Output.
        integer,                       intent(out), optional :: error  !! Error code.

        character(len=20) :: buffer
        integer           :: stat

        if (present(error)) error = E_FORMAT
        write (buffer, FMT_REAL, iostat=stat) value
        if (stat /= 0) then
            string = ''
            return
        end if
        if (present(error)) error = E_NONE
        string = trim(buffer)
    end subroutine string_from_real64

    pure elemental subroutine string_to_int16(string, value, error)
        !! Converts string to 2-byte integer.
        character(len=*), intent(in)            :: string !! Input.
        integer(kind=i2), intent(out)           :: value  !! Output.
        integer,          intent(out), optional :: error  !! Error code.

        integer :: stat

        value = 0_i2
        if (present(error)) error = E_TYPE
        read (string, *, iostat=stat) value
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_int16

    pure elemental subroutine string_to_int32(string, value, error)
        !! Converts string to 4-byte integer.
        character(len=*), intent(in)            :: string !! Input.
        integer(kind=i4), intent(out)           :: value  !! Output.
        integer,          intent(out), optional :: error  !! Error code.

        integer :: stat

        value = 0_i4
        if (present(error)) error = E_TYPE
        read (string, *, iostat=stat) value
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_int32

    pure elemental subroutine string_to_int64(string, value, error)
        !! Converts string to 8-byte integer.
        character(len=*), intent(in)            :: string !! Input.
        integer(kind=i8), intent(out)           :: value  !! Output.
        integer,          intent(out), optional :: error  !! Error code.

        integer :: stat

        value = 0_i8
        if (present(error)) error = E_TYPE
        read (string, *, iostat=stat) value
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_int64

    pure elemental subroutine string_to_real32(string, value, error)
        !! Converts string to 4-byte real.
        character(len=*), intent(in)            :: string !! Input.
        real(kind=r4),    intent(out)           :: value  !! Output.
        integer,          intent(out), optional :: error  !! Error code.

        integer :: stat

        value = 0.0_r4
        if (present(error)) error = E_TYPE
        read (string, *, iostat=stat) value
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_real32

    pure elemental subroutine string_to_real64(string, value, error)
        !! Converts string to 8-byte real.
        character(len=*), intent(in)            :: string !! Input.
        real(kind=r8),    intent(out)           :: value  !! Output.
        integer,          intent(out), optional :: error  !! Error code.

        integer :: stat

        value = 0.0_r8
        if (present(error)) error = E_TYPE
        read (string, *, iostat=stat) value
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_real64
end module dm_string
