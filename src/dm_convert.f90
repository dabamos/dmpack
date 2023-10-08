! Author:  Philipp Engel
! Licence: ISC
module dm_convert
    !! Type conversion routines (type to string, string to type).
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    interface dm_convert_to
        !! Generic string to number converter.
        module procedure :: convert_to_i4
        module procedure :: convert_to_i8
        module procedure :: convert_to_r4
        module procedure :: convert_to_r8
    end interface

    interface dm_convert_from
        !! Generic number to string converter.
        module procedure :: convert_from_i4
        module procedure :: convert_from_i8
        module procedure :: convert_from_r4
        module procedure :: convert_from_r8
    end interface

    public :: dm_convert_to
    public :: dm_convert_from

    private :: convert_to_i4
    private :: convert_to_i8
    private :: convert_to_r4
    private :: convert_to_r8
    private :: convert_from_i4
    private :: convert_from_i8
    private :: convert_from_r4
    private :: convert_from_r8
contains
    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    pure subroutine convert_from_i4(i, str, error)
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
    end subroutine convert_from_i4

    pure subroutine convert_from_i8(i, str, error)
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
    end subroutine convert_from_i8

    pure subroutine convert_from_r4(f, str, error)
        !! Returns string representation of given 4-byte real.
        real(kind=r4),                 intent(in)            :: f     !! Input.
        character(len=:), allocatable, intent(out)           :: str   !! Output.
        integer,                       intent(out), optional :: error !! Error code.

        integer           :: rc
        character(len=20) :: buf

        str = ''
        if (present(error)) error = E_FORMAT
        write (buf, '(f0.8)', iostat=rc) f
        if (rc /= 0) return
        str = trim(buf)
        if (present(error)) error = E_NONE
    end subroutine convert_from_r4

    pure subroutine convert_from_r8(f, str, error)
        !! Returns string representation of given 8-byte real.
        real(kind=r8),                 intent(in)            :: f     !! Input.
        character(len=:), allocatable, intent(out)           :: str   !! Output.
        integer,                       intent(out), optional :: error !! Error code.

        integer           :: rc
        character(len=20) :: buf

        str = ''
        if (present(error)) error = E_FORMAT
        write (buf, '(f0.8)', iostat=rc) f
        if (rc /= 0) return
        str = trim(buf)
        if (present(error)) error = E_NONE
    end subroutine convert_from_r8

    pure subroutine convert_to_i4(str, i, error)
        !! Converts string to 4-byte integer.
        character(len=*), intent(in)            :: str   !! Input.
        integer(kind=i4), intent(out)           :: i     !! Output.
        integer,          intent(out), optional :: error !! error code.
        integer                                 :: rc

        i = 0
        if (present(error)) error = E_TYPE
        read (str, *, iostat=rc) i
        if (present(error)) error = E_NONE
    end subroutine convert_to_i4

    pure subroutine convert_to_i8(str, i, error)
        !! Converts string to 8-byte integer.
        character(len=*), intent(in)            :: str   !! Input.
        integer(kind=i8), intent(out)           :: i     !! Output.
        integer,          intent(out), optional :: error !! error code.
        integer                                 :: rc

        i = 0
        if (present(error)) error = E_TYPE
        read (str, *, iostat=rc) i
        if (present(error)) error = E_NONE
    end subroutine convert_to_i8

    pure subroutine convert_to_r4(str, f, error)
        !! Converts string to 4-byte real.
        character(len=*), intent(in)            :: str   !! Input.
        real(kind=r4),    intent(out)           :: f     !! Output.
        integer,          intent(out), optional :: error !! error code.
        integer                                 :: rc

        f = 0.0
        if (present(error)) error = E_TYPE
        read (str, *, iostat=rc) f
        if (present(error)) error = E_NONE
    end subroutine convert_to_r4

    pure subroutine convert_to_r8(str, f, error)
        !! Converts string to 8-byte real.
        character(len=*), intent(in)            :: str   !! Input.
        real(kind=r8),    intent(out)           :: f     !! Output.
        integer,          intent(out), optional :: error !! error code.
        integer                                 :: rc

        f = 0.0
        if (present(error)) error = E_TYPE
        read (str, *, iostat=rc) f
        if (present(error)) error = E_NONE
    end subroutine convert_to_r8
end module dm_convert
