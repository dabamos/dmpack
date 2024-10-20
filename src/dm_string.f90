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
    public :: dm_string_lower
    public :: dm_string_replace
    public :: dm_string_split
    public :: dm_string_starts_with
    public :: dm_string_to_lower
    public :: dm_string_to_upper
    public :: dm_string_upper

    ! Public string type procedures.
    public :: dm_string_type_allocate
    public :: dm_string_type_destroy

    ! Private procedures.
    private :: string_from_int32
    private :: string_from_int64
    private :: string_from_real32
    private :: string_from_real64
    private :: string_to_int32
    private :: string_to_int64
    private :: string_to_real32
    private :: string_to_real64
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental integer function dm_string_count_char(str, a, quote) result(n)
        !! Counts occurences of character `a` in `str`, with optional quoting
        !! (`a` in between quote characters is not counted).
        character(len=*), intent(in)           :: str   !! Input.
        character,        intent(in)           :: a     !! Character to count.
        character,        intent(in), optional :: quote !! Quote character.

        character :: c
        integer   :: i
        logical   :: f, q

        n = 0
        f = .false.
        q = present(quote)
        if (q) c = quote

        do i = 1, len_trim(str)
            if (q .and. str(i:i) == c) f = .not. f
            if (.not. f .and. str(i:i) == a) n = n + 1
        end do
    end function dm_string_count_char

    integer function dm_string_count_lines(str) result(n)
        !! Returns the number of line breaks in string.
        use :: dm_ascii, only: ASCII_LF

        character(len=*), intent(inout) :: str !! Input string.

        n = dm_string_count_char(str, ASCII_LF) + 1
    end function dm_string_count_lines

    pure elemental integer function dm_string_count_substring(s1, s2) result(n)
        !! Returns the number of occurences of string `s2` in string `s1`.
        character(len=*), intent(in) :: s1 !! Input string.
        character(len=*), intent(in) :: s2 !! Sub-string.

        integer :: p
        integer :: pos_n

        n = 0
        p = 1

        if (len(s2) == 0) return

        do
            pos_n = index(s1(p:), s2)
            if (pos_n == 0) return
            n = n + 1
            p = p + pos_n + len(s2)
        end do
    end function dm_string_count_substring

    logical function dm_string_is_empty(str) result(is)
        !! Returns `.true.` if given allocatable string is not passed, not
        !! allocated, or contains only white spaces.
        character(len=:), allocatable, intent(inout), optional :: str !! Input string.

        is = .true.
        if (.not. present(str))   return
        if (.not. allocated(str)) return
        if (len_trim(str) == 0)   return
        is = .false.
    end function dm_string_is_empty

    pure logical function dm_string_is_present(str) result(is)
        !! Returns `.true.` if given string is present and not empty.
        character(len=*), intent(in), optional :: str !! Input string.

        is = .false.
        if (.not. present(str)) return
        if (len_trim(str) == 0) return
        is = .true.
    end function dm_string_is_present

    pure logical function dm_string_is_printable(str) result(is)
        !! Returns `.true.` if all characters is given string are printable
        !! ASCII characters.
        use :: dm_ascii, only: dm_ascii_is_printable

        character(len=*), intent(in) :: str !! String to validate.

        integer :: i

        is = .false.

        do i = 1, len_trim(str)
            if (.not. dm_ascii_is_printable(str(i:i))) return
        end do

        is = .true.
    end function dm_string_is_printable

    pure elemental function dm_string_to_lower(str) result(lower)
        !! Returns given string in lower case.
        character(len=*), intent(in) :: str   !! String to convert.
        character(len=len(str))      :: lower !! Result.

        character :: a
        integer   :: i

        do i = 1, len(str)
            a = str(i:i)
            if (a >= 'A' .and. a <= 'Z') a = achar(iachar(a) + 32)
            lower(i:i) = a
        end do
    end function dm_string_to_lower

    pure elemental function dm_string_to_upper(str) result(upper)
        !! Returns given string in upper case.
        character(len=*), intent(in) :: str   !! String to convert.
        character(len=len(str))      :: upper !! Result.

        character :: a
        integer   :: i

        do i = 1, len(str)
            a = str(i:i)
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
    pure elemental subroutine dm_string_lower(str)
        !! Converts given string to lower case.
        character(len=*), intent(inout) :: str !! Input/output string.

        character :: a
        integer   :: i

        do i = 1, len(str)
            a = str(i:i)
            if (a >= 'A' .and. a <= 'Z') str(i:i) = achar(iachar(a) + 32)
        end do
    end subroutine dm_string_lower

    subroutine dm_string_replace(str, a, b)
        !! Replaces character `a` in `str` with `b`.
        character(len=*), intent(inout) :: str !! String to parse.
        character,        intent(in)    :: a   !! Character to replace.
        character,        intent(in)    :: b   !! Substitute character.

        integer :: i

        do i = 1, len(str)
            if (str(i:i) == a) str(i:i) = b
        end do
    end subroutine dm_string_replace

    pure subroutine dm_string_split(str, array, del, n)
        !! Splits a string by a given delimiter into an array of strings.
        character(len=*), intent(in)            :: str
        character(len=*), intent(inout)         :: array(:)
        character(len=*), intent(in)            :: del
        integer,          intent(out), optional :: n

        integer :: i, pos1, pos2

        pos1 = 1
        i    = 0

        do
            pos2 = index(str(pos1:), del)

            if (pos2 == 0) then
                i = i + 1
                if (i > size(array)) exit
                array(i) = str(pos1:)
                exit
            end if

            i = i + 1
            array(i) = str(pos1:pos1 + pos2 - 2)
            pos1 = pos1 + pos2
        end do

        if (present(n)) n = i
    end subroutine dm_string_split

    pure elemental logical function dm_string_starts_with(a, b) result(starts)
        !! Returns `.true.` if string `a` starts with string `b`.
        character(len=*), intent(in) :: a !! First string.
        character(len=*), intent(in) :: b !! Second string.

        integer :: na, nb

        na = len_trim(a)
        nb = len_trim(b)

        starts = .false.
        if (na == 0 .or. nb == 0 .or. na < nb) return

        starts = (a(:nb) == b)
    end function dm_string_starts_with

    pure elemental subroutine dm_string_upper(str)
        !! Converts given string to upper case.
        character(len=*), intent(inout) :: str !! Input/output string.

        character :: a
        integer   :: i

        do i = 1, len(str)
            a = str(i:i)
            if (a >= 'a' .and. a <= 'z') str(i:i) = achar(iachar(a) - 32)
        end do
    end subroutine dm_string_upper

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    pure subroutine string_from_int32(i, str, error)
        !! Returns string representation of given 4-byte integer.
        integer(kind=i4),              intent(in)            :: i     !! Input.
        character(len=:), allocatable, intent(out)           :: str   !! Output.
        integer,                       intent(out), optional :: error !! Error code.

        integer :: n, stat

        if (i == 0) then
            n = 1
        else
            n = floor(log10(real(abs(i))) + 1)
            if (i < 0) n = n + 1
        end if

        if (present(error)) error = E_FORMAT
        allocate (character(len=n) :: str)
        write (str, FMT_INTEGER, iostat=stat) i
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_from_int32

    pure subroutine string_from_int64(i, str, error)
        !! Returns string representation of given 8-byte integer.
        integer(kind=i8),              intent(in)            :: i     !! Input.
        character(len=:), allocatable, intent(out)           :: str   !! Output.
        integer,                       intent(out), optional :: error !! Error code.

        integer :: n, stat

        if (i == 0) then
            n = 1
        else
            n = floor(log10(real(abs(i))) + 1)
            if (i < 0) n = n + 1
        end if

        if (present(error)) error = E_FORMAT
        allocate (character(len=n) :: str)
        write (str, FMT_INTEGER, iostat=stat) i
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_from_int64

    pure subroutine string_from_real32(f, str, error)
        !! Returns string representation of given 4-byte real.
        real(kind=r4),                 intent(in)            :: f     !! Input.
        character(len=:), allocatable, intent(out)           :: str   !! Output.
        integer,                       intent(out), optional :: error !! Error code.

        integer           :: stat
        character(len=20) :: buf

        if (present(error)) error = E_FORMAT
        write (buf, FMT_REAL, iostat=stat) f
        if (stat /= 0) then
            str = ''
            return
        end if
        if (present(error)) error = E_NONE
        str = trim(buf)
    end subroutine string_from_real32

    pure subroutine string_from_real64(f, str, error)
        !! Returns string representation of given 8-byte real.
        real(kind=r8),                 intent(in)            :: f     !! Input.
        character(len=:), allocatable, intent(out)           :: str   !! Output.
        integer,                       intent(out), optional :: error !! Error code.

        integer           :: stat
        character(len=20) :: buf

        if (present(error)) error = E_FORMAT
        write (buf, FMT_REAL, iostat=stat) f
        if (stat /= 0) then
            str = ''
            return
        end if
        if (present(error)) error = E_NONE
        str = trim(buf)
    end subroutine string_from_real64

    pure elemental subroutine string_to_int32(str, i, error)
        !! Converts string to 4-byte integer.
        character(len=*), intent(in)            :: str   !! Input.
        integer(kind=i4), intent(out)           :: i     !! Output.
        integer,          intent(out), optional :: error !! Error code.

        integer :: stat

        i = 0_i4
        if (present(error)) error = E_TYPE
        read (str, *, iostat=stat) i
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_int32

    pure elemental subroutine string_to_int64(str, i, error)
        !! Converts string to 8-byte integer.
        character(len=*), intent(in)            :: str   !! Input.
        integer(kind=i8), intent(out)           :: i     !! Output.
        integer,          intent(out), optional :: error !! Error code.

        integer :: stat

        i = 0_i8
        if (present(error)) error = E_TYPE
        read (str, *, iostat=stat) i
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_int64

    pure elemental subroutine string_to_real32(str, f, error)
        !! Converts string to 4-byte real.
        character(len=*), intent(in)            :: str   !! Input.
        real(kind=r4),    intent(out)           :: f     !! Output.
        integer,          intent(out), optional :: error !! Error code.

        integer :: stat

        f = 0.0_r4
        if (present(error)) error = E_TYPE
        read (str, *, iostat=stat) f
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_real32

    pure elemental subroutine string_to_real64(str, f, error)
        !! Converts string to 8-byte real.
        character(len=*), intent(in)            :: str   !! Input.
        real(kind=r8),    intent(out)           :: f     !! Output.
        integer,          intent(out), optional :: error !! Error code.

        integer :: stat

        f = 0.0_r8
        if (present(error)) error = E_TYPE
        read (str, *, iostat=stat) f
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine string_to_real64
end module dm_string
