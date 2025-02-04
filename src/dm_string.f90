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
        character(len=:), allocatable :: data
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
    private :: string_to_int16
    private :: string_to_int32
    private :: string_to_int64
    private :: string_to_real32
    private :: string_to_real64
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental integer function dm_string_count_char(string, a, quote) result(n)
        !! Counts occurences of character `a` in `string`, with optional quoting
        !! (`a` in between quote characters is not counted).
        character(len=*), intent(in)           :: string !! Input.
        character,        intent(in)           :: a      !! Character to count.
        character,        intent(in), optional :: quote  !! Quote character.

        character :: c
        integer   :: i
        logical   :: f, q

        n = 0
        f = .false.
        q = present(quote)
        if (q) c = quote

        do i = 1, len_trim(string)
            if (q .and. string(i:i) == c) f = .not. f
            if (.not. f .and. string(i:i) == a) n = n + 1
        end do
    end function dm_string_count_char

    integer function dm_string_count_lines(string) result(n)
        !! Returns the number of line breaks in string.
        use :: dm_ascii, only: ASCII_LF

        character(len=*), intent(inout) :: string !! Input string.

        n = dm_string_count_char(string, ASCII_LF) + 1
    end function dm_string_count_lines

    pure elemental integer function dm_string_count_substring(string1, string2) result(n)
        !! Returns the number of occurences of `string2` in `string1`.
        character(len=*), intent(in) :: string1 !! Haystack string.
        character(len=*), intent(in) :: string2 !! Needle string.

        integer :: p
        integer :: pos_n

        n = 0
        p = 1

        if (len(string2) == 0) return

        do
            pos_n = index(string1(p:), string2)
            if (pos_n == 0) return
            n = n + 1
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
        character(len=*), intent(in)            :: string
        character(len=*), intent(inout)         :: array(:)
        character(len=*), intent(in)            :: del
        integer,          intent(out), optional :: n

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
    pure subroutine string_from_int32(i, string, error)
        !! Returns string representation of given 4-byte integer.
        integer(kind=i4),              intent(in)            :: i      !! Input.
        character(len=:), allocatable, intent(out)           :: string !! Output.
        integer,                       intent(out), optional :: error  !! Error code.

        integer :: n, stat

        if (i == 0) then
            n = 1
        else
            n = floor(log10(real(abs(i))) + 1)
            if (i < 0) n = n + 1
        end if

        if (present(error)) error = E_FORMAT
        allocate (character(len=n) :: string)
        write (string, FMT_INTEGER, iostat=stat) i
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_from_int32

    pure subroutine string_from_int64(i, string, error)
        !! Returns string representation of given 8-byte integer.
        integer(kind=i8),              intent(in)            :: i      !! Input.
        character(len=:), allocatable, intent(out)           :: string !! Output.
        integer,                       intent(out), optional :: error  !! Error code.

        integer :: n, stat

        if (i == 0) then
            n = 1
        else
            n = floor(log10(real(abs(i))) + 1)
            if (i < 0) n = n + 1
        end if

        if (present(error)) error = E_FORMAT
        allocate (character(len=n) :: string)
        write (string, FMT_INTEGER, iostat=stat) i
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_from_int64

    pure subroutine string_from_real32(f, string, error)
        !! Returns string representation of given 4-byte real.
        real(kind=r4),                 intent(in)            :: f      !! Input.
        character(len=:), allocatable, intent(out)           :: string !! Output.
        integer,                       intent(out), optional :: error  !! Error code.

        integer           :: stat
        character(len=20) :: buf

        if (present(error)) error = E_FORMAT
        write (buf, FMT_REAL, iostat=stat) f
        if (stat /= 0) then
            string = ''
            return
        end if
        if (present(error)) error = E_NONE
        string = trim(buf)
    end subroutine string_from_real32

    pure subroutine string_from_real64(f, string, error)
        !! Returns string representation of given 8-byte real.
        real(kind=r8),                 intent(in)            :: f      !! Input.
        character(len=:), allocatable, intent(out)           :: string !! Output.
        integer,                       intent(out), optional :: error  !! Error code.

        integer           :: stat
        character(len=20) :: buf

        if (present(error)) error = E_FORMAT
        write (buf, FMT_REAL, iostat=stat) f
        if (stat /= 0) then
            string = ''
            return
        end if
        if (present(error)) error = E_NONE
        string = trim(buf)
    end subroutine string_from_real64

    pure elemental subroutine string_to_int16(string, i, error)
        !! Converts string to 2-byte integer.
        character(len=*), intent(in)            :: string !! Input.
        integer(kind=i2), intent(out)           :: i      !! Output.
        integer,          intent(out), optional :: error  !! Error code.

        integer :: stat

        i = 0_i2
        if (present(error)) error = E_TYPE
        read (string, *, iostat=stat) i
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_int16

    pure elemental subroutine string_to_int32(string, i, error)
        !! Converts string to 4-byte integer.
        character(len=*), intent(in)            :: string !! Input.
        integer(kind=i4), intent(out)           :: i      !! Output.
        integer,          intent(out), optional :: error  !! Error code.

        integer :: stat

        i = 0_i4
        if (present(error)) error = E_TYPE
        read (string, *, iostat=stat) i
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_int32

    pure elemental subroutine string_to_int64(string, i, error)
        !! Converts string to 8-byte integer.
        character(len=*), intent(in)            :: string   !! Input.
        integer(kind=i8), intent(out)           :: i     !! Output.
        integer,          intent(out), optional :: error !! Error code.

        integer :: stat

        i = 0_i8
        if (present(error)) error = E_TYPE
        read (string, *, iostat=stat) i
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_int64

    pure elemental subroutine string_to_real32(string, f, error)
        !! Converts string to 4-byte real.
        character(len=*), intent(in)            :: string   !! Input.
        real(kind=r4),    intent(out)           :: f     !! Output.
        integer,          intent(out), optional :: error !! Error code.

        integer :: stat

        f = 0.0_r4
        if (present(error)) error = E_TYPE
        read (string, *, iostat=stat) f
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_real32

    pure elemental subroutine string_to_real64(string, f, error)
        !! Converts string to 8-byte real.
        character(len=*), intent(in)            :: string !! Input.
        real(kind=r8),    intent(out)           :: f      !! Output.
        integer,          intent(out), optional :: error  !! Error code.

        integer :: stat

        f = 0.0_r8
        if (present(error)) error = E_TYPE
        read (string, *, iostat=stat) f
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_real64
end module dm_string
