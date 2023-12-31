! Author:  Philipp Engel
! Licence: ISC
module dm_string
    !! String utility routines.
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    interface dm_string_to
        !! Generic string to number converter.
        module procedure :: string_to_i4
        module procedure :: string_to_i8
        module procedure :: string_to_r4
        module procedure :: string_to_r8
    end interface

    interface dm_string_from
        !! Generic number to string converter.
        module procedure :: string_from_i4
        module procedure :: string_from_i8
        module procedure :: string_from_r4
        module procedure :: string_from_r8
    end interface

    ! Public procedures.
    public :: dm_lower
    public :: dm_upper
    public :: dm_to_lower
    public :: dm_to_upper

    public :: dm_string_count_char
    public :: dm_string_count_lines
    public :: dm_string_count_substring
    public :: dm_string_split

    public :: dm_string_to
    public :: dm_string_from

    ! Private procedures.
    private :: string_from_i4
    private :: string_from_i8
    private :: string_from_r4
    private :: string_from_r8
    private :: string_to_i4
    private :: string_to_i8
    private :: string_to_r4
    private :: string_to_r8
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
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

    pure elemental function dm_lower(str) result(lower)
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
    end function dm_lower

    pure elemental function dm_upper(str) result(upper)
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
    end function dm_upper

    subroutine dm_string_split(str, array, del, n)
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

    subroutine dm_to_lower(str)
        !! Converts given string to lower case.
        character(len=*), intent(inout) :: str !! Input/output string.

        character :: a
        integer   :: i

        do i = 1, len(str)
            a = str(i:i)
            if (a >= 'A' .and. a <= 'Z') str(i:i) = achar(iachar(a) + 32)
        end do
    end subroutine dm_to_lower

    subroutine dm_to_upper(str)
        !! Converts given string to upper case.
        character(len=*), intent(inout) :: str !! Input/output string.

        character :: a
        integer   :: i

        do i = 1, len(str)
            a = str(i:i)
            if (a >= 'a' .and. a <= 'z') str(i:i) = achar(iachar(a) - 32)
        end do
    end subroutine dm_to_upper

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    pure subroutine string_from_i4(i, str, error)
        !! Returns string representation of given 4-byte integer.
        integer(kind=i4),              intent(in)            :: i     !! Input.
        character(len=:), allocatable, intent(out)           :: str   !! Output.
        integer,                       intent(out), optional :: error !! Error code.

        integer :: n, rc

        if (present(error)) error = E_FORMAT

        if (i == 0) then
            n = 1
        else
            n = floor(log10(real(abs(i))) + 1)
            if (i < 0) n = n + 1
        end if

        allocate (character(len=n) :: str)
        write (str, '(i0)', iostat=rc) i

        if (present(error)) error = E_NONE
    end subroutine string_from_i4

    pure subroutine string_from_i8(i, str, error)
        !! Returns string representation of given 8-byte integer.
        integer(kind=i8),              intent(in)            :: i     !! Input.
        character(len=:), allocatable, intent(out)           :: str   !! Output.
        integer,                       intent(out), optional :: error !! Error code.

        integer :: n, rc

        if (present(error)) error = E_FORMAT

        if (i == 0) then
            n = 1
        else
            n = floor(log10(real(abs(i))) + 1)
            if (i < 0) n = n + 1
        end if

        allocate (character(len=n) :: str)
        write (str, '(i0)', iostat=rc) i

        if (present(error)) error = E_NONE
    end subroutine string_from_i8

    pure subroutine string_from_r4(f, str, error)
        !! Returns string representation of given 4-byte real.
        real(kind=r4),                 intent(in)            :: f     !! Input.
        character(len=:), allocatable, intent(out)           :: str   !! Output.
        integer,                       intent(out), optional :: error !! Error code.

        integer           :: rc
        character(len=20) :: buf

        str = ''
        if (present(error)) error = E_FORMAT
        write (buf, '(f0.12)', iostat=rc) f
        if (rc /= 0) return
        str = trim(buf)
        if (present(error)) error = E_NONE
    end subroutine string_from_r4

    pure subroutine string_from_r8(f, str, error)
        !! Returns string representation of given 8-byte real.
        real(kind=r8),                 intent(in)            :: f     !! Input.
        character(len=:), allocatable, intent(out)           :: str   !! Output.
        integer,                       intent(out), optional :: error !! Error code.

        integer           :: rc
        character(len=20) :: buf

        str = ''
        if (present(error)) error = E_FORMAT
        write (buf, '(f0.12)', iostat=rc) f
        if (rc /= 0) return
        str = trim(buf)
        if (present(error)) error = E_NONE
    end subroutine string_from_r8

    pure elemental subroutine string_to_i4(str, i, error)
        !! Converts string to 4-byte integer.
        character(len=*), intent(in)            :: str   !! Input.
        integer(kind=i4), intent(out)           :: i     !! Output.
        integer,          intent(out), optional :: error !! error code.
        integer                                 :: rc

        i = 0
        if (present(error)) error = E_TYPE
        read (str, *, iostat=rc) i
        if (present(error)) error = E_NONE
    end subroutine string_to_i4

    pure elemental subroutine string_to_i8(str, i, error)
        !! Converts string to 8-byte integer.
        character(len=*), intent(in)            :: str   !! Input.
        integer(kind=i8), intent(out)           :: i     !! Output.
        integer,          intent(out), optional :: error !! error code.
        integer                                 :: rc

        i = 0
        if (present(error)) error = E_TYPE
        read (str, *, iostat=rc) i
        if (present(error)) error = E_NONE
    end subroutine string_to_i8

    pure elemental subroutine string_to_r4(str, f, error)
        !! Converts string to 4-byte real.
        character(len=*), intent(in)            :: str   !! Input.
        real(kind=r4),    intent(out)           :: f     !! Output.
        integer,          intent(out), optional :: error !! error code.
        integer                                 :: rc

        f = 0.0
        if (present(error)) error = E_TYPE
        read (str, *, iostat=rc) f
        if (present(error)) error = E_NONE
    end subroutine string_to_r4

    pure elemental subroutine string_to_r8(str, f, error)
        !! Converts string to 8-byte real.
        character(len=*), intent(in)            :: str   !! Input.
        real(kind=r8),    intent(out)           :: f     !! Output.
        integer,          intent(out), optional :: error !! error code.
        integer                                 :: rc

        f = 0.0
        if (present(error)) error = E_TYPE
        read (str, *, iostat=rc) f
        if (present(error)) error = E_NONE
    end subroutine string_to_r8
end module dm_string
