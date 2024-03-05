! Author:  Philipp Engel
! Licence: ISC
module dm_util
    !! Type conversion functions and other utility procedures.
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    interface dm_array_has
        !! Returns whether array contains an integer value.
        module procedure :: array_has_i4
        module procedure :: array_has_i8
    end interface

    interface dm_equals
        !! Returns whether two real numbers are approximately the same.
        module procedure :: equals_r4
        module procedure :: equals_r8
    end interface

    interface dm_itoa
        !! Generic integer to string converter.
        module procedure :: i4_to_a
        module procedure :: i8_to_a
    end interface

    interface dm_ftoa
        !! Generic real to string converter.
        module procedure :: f4_to_a
        module procedure :: f8_to_a
    end interface

    interface dm_from_real64
        !! Converts 8-byte real to type (for response values).
        module procedure :: dm_real64_to_int32
        module procedure :: dm_real64_to_int64
        module procedure :: dm_real64_to_logical
        module procedure :: dm_real64_to_real32
    end interface

    interface dm_to_real64
        !! Converts type to 8-byte real (for response values).
        module procedure :: dm_int32_to_real64
        module procedure :: dm_int64_to_real64
        module procedure :: dm_logical_to_real64
        module procedure :: dm_real32_to_real64
    end interface

    interface dm_to_signed
        !! Converts unsigned integer to signed integer.
        module procedure :: dm_uint16_to_int32
        module procedure :: dm_uint32_to_int64
    end interface

    ! Public procedures.
    public :: dm_atof
    public :: dm_atoi
    public :: dm_btoi
    public :: dm_ftoa
    public :: dm_itoa

    public :: dm_array_has
    public :: dm_equals
    public :: dm_sleep
    public :: dm_usleep

    public :: dm_deg_to_gon
    public :: dm_deg_to_rad
    public :: dm_gon_to_deg
    public :: dm_gon_to_rad
    public :: dm_rad_to_deg
    public :: dm_rad_to_gon

    public :: dm_from_real64
    public :: dm_int32_to_real64
    public :: dm_int64_to_real64
    public :: dm_logical_to_real64
    public :: dm_real32_to_real64

    public :: dm_to_real64
    public :: dm_real64_to_int32
    public :: dm_real64_to_int64
    public :: dm_real64_to_logical
    public :: dm_real64_to_real32

    public :: dm_to_signed
    public :: dm_uint16_to_int32
    public :: dm_uint32_to_int64

    ! Private procedures.
    private :: array_has_i4
    private :: array_has_i8

    private :: equals_r4
    private :: equals_r8

    private :: f4_to_a
    private :: f8_to_a
    private :: i4_to_a
    private :: i8_to_a
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ****************************************************************** 
    pure elemental function dm_atof(str) result(f)
        !! Converts string to 8-byte real.
        character(len=*),intent(in) :: str !! Number string.
        real(kind=r8)               :: f   !! Value.

        integer :: stat

        f = 0.0_r8
        read (str, *, iostat=stat) f
    end function dm_atof

    pure elemental function dm_atoi(str) result(i)
        !! Converts string to 4-byte integer.
        character(len=*),intent(in) :: str !! Number string.
        integer                     :: i   !! Value.

        integer :: stat

        i = 0
        read (str, *, iostat=stat) i
    end function dm_atoi

    pure elemental function dm_btoi(l) result(i)
        !! Converts logical (boolean) to 4-byte integer.
        logical, intent(in) :: l !! Logical value.
        integer             :: i !! `0` or `1`.

        i = 0
        if (.not. l) return
        i = 1
    end function dm_btoi

    pure elemental function dm_deg_to_gon(a) result(b)
        !! Converts angle from degrees to gon.
        real(kind=r8), intent(in) :: a !! Angle [deg].
        real(kind=r8)             :: b !! Angle [gon].

        b = a * (10.0_r8 / 9.0_r8)
    end function dm_deg_to_gon

    pure elemental function dm_deg_to_rad(a) result(b)
        !! Converts angle from degrees to radiants.
        use :: dm_const, only: PI
        real(kind=r8), intent(in) :: a !! Angle [deg].
        real(kind=r8)             :: b !! Angle [rad].

        b = a * (PI / 180.0_r8)
    end function dm_deg_to_rad

    pure elemental function dm_gon_to_deg(a) result(b)
        !! Converts angle from gon to degrees.
        real(kind=r8), intent(in) :: a !! Angle [gon].
        real(kind=r8)             :: b !! Angle [deg].

        b = a * (9.0_r8 / 10.0_r8)
    end function dm_gon_to_deg

    pure elemental function dm_gon_to_rad(a) result(b)
        !! Converts angle from gon to radiants.
        use :: dm_const, only: PI
        real(kind=r8), intent(in) :: a !! Angle [gon].
        real(kind=r8)             :: b !! Angle [rad].

        b = a * (PI / 200.0_r8)
    end function dm_gon_to_rad

    pure elemental function dm_rad_to_deg(a) result(b)
        !! Converts angle from radiants to degrees.
        use :: dm_const, only: PI
        real(kind=r8), intent(in) :: a !! Angle [rad].
        real(kind=r8)             :: b !! Angle [deg]

        b = a * (180.0_r8 / PI)
    end function dm_rad_to_deg

    pure elemental function dm_rad_to_gon(a) result(b)
        !! Converts angle from radiants to gon.
        use :: dm_const, only: PI
        real(kind=r8), intent(in) :: a !! Angle [rad].
        real(kind=r8)             :: b !! Angle [gon]

        b = a * (200.0_r8 / PI)
    end function dm_rad_to_gon

    pure elemental function dm_int32_to_real64(i) result(r)
        !! Converts 4-byte integer to 8-byte real.
        integer(kind=i4), intent(in) :: i !! 4-byte integer value.
        real(kind=r8)                :: r !! Value as 8-byte real.

        r = real(i, kind=r8)
    end function dm_int32_to_real64

    pure elemental function dm_int64_to_real64(i) result(r)
        !! Converts 8-byte integer to 8-byte real.
        integer(kind=i8), intent(in) :: i !! 8-byte integer value.
        real(kind=r8)                :: r !! Value as 8-byte real.

        r = real(i, kind=r8)
    end function dm_int64_to_real64

    pure elemental function dm_logical_to_real64(l) result(r)
        !! Converts logical to 8-byte real (`0.0` for `.false.` and `1.0` for `.true.`).
        logical, intent(in) :: l !! Logical value.
        real(kind=r8)       :: r !! Value as 8-byte real.

        if (l) then
            r = 1.0_r8
        else
            r = 0.0_r8
        end if
    end function dm_logical_to_real64

    pure elemental function dm_uint16_to_int32(u) result(s)
        !! Converts unsigned 2-byte integer to signed 4-byte integer.
        integer(kind=i2), intent(in) :: u !! Unsigned integer.
        integer(kind=i4)             :: s !! Signed integer.

        if (u > 0) then
            s = int(u, kind=i4)
        else
            s = 65536_i4 + int(u, kind=i4)
        end if
    end function dm_uint16_to_int32

    pure elemental function dm_uint32_to_int64(u) result(s)
        !! Converts unsigned 4-byte integer to signed 8-byte integer.
        integer(kind=i4), intent(in) :: u !! Unsigned integer.
        integer(kind=i8)             :: s !! Signed integer.

        if (u > 0) then
            s = int(u, kind=i8)
        else
            s = 4294967296_i8 + int(u, kind=i8)
        end if
    end function dm_uint32_to_int64

    pure elemental function dm_real32_to_real64(f) result(r)
        !! Converts 4-byte real to 8-byte real.
        real(kind=r4), intent(in) :: f !! 4-byte real value.
        real(kind=r8)             :: r !! Value as 8-byte real.

        r = real(f, kind=r8)
    end function dm_real32_to_real64

    pure elemental subroutine dm_real64_to_int32(f, i)
        !! Converts 8-byte real to 4-byte integer.
        real(kind=r8),    intent(in)  :: f !! 8-byte real value.
        integer(kind=i4), intent(out) :: i !! 4-byte integer value.

        i = int(f, kind=i4)
    end subroutine dm_real64_to_int32

    pure elemental subroutine dm_real64_to_int64(f, i)
        !! Converts 8-byte real to 8-byte integer.
        real(kind=r8),    intent(in)  :: f !! 8-byte real value.
        integer(kind=i8), intent(out) :: i !! 8-byte integer value.

        i = int(f, kind=i8)
    end subroutine dm_real64_to_int64

    pure elemental subroutine dm_real64_to_logical(f, l)
        !! Converts 8-byte real to logical. If `f` is `0.0`, the result is
        !! `.false.`, else `.true.`.
        real(kind=r8), intent(in)  :: f !! 8-byte real value.
        logical,       intent(out) :: l !! Logical value.

        l = (.not. dm_equals(f, 0.0_r8))
    end subroutine dm_real64_to_logical

    pure elemental subroutine dm_real64_to_real32(f, r)
        !! Converts 8-byte real to 4-byte real
        real(kind=r8), intent(in)  :: f !! 8-byte real value.
        real(kind=r4), intent(out) :: r !! 4-byte real value.

        r = real(f, kind=r4)
    end subroutine dm_real64_to_real32

    subroutine dm_sleep(sec)
        !! Pauses program execution for given time in seconds.
        use :: unix, only: c_useconds_t, c_usleep
        integer, intent(in) :: sec !! Delay in seconds [s].
        integer             :: rc

        rc = c_usleep(int(sec * 10**6, kind=c_useconds_t))
    end subroutine dm_sleep

    subroutine dm_usleep(usec)
        !! Pauses program execution for given time in useconds.
        use :: unix, only: c_useconds_t, c_usleep
        integer, intent(in) :: usec !! Delay in useconds [us].
        integer             :: rc

        rc = c_usleep(int(usec, kind=c_useconds_t))
    end subroutine dm_usleep

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ****************************************************************** 
    logical function array_has_i4(array, value) result(has)
        !! Returns `.true.` if the integer array contains the given value.
        integer(kind=i4), intent(inout) :: array(:) !! Input array.
        integer(kind=i4), intent(in)    :: value    !! Value to search.

        has = .false.
        if (findloc(array, value, dim=1) == 0) return
        has = .true.
    end function array_has_i4

    logical function array_has_i8(array, value) result(has)
        !! Returns `.true.` if the integer array contains the given value.
        integer(kind=i8), intent(inout) :: array(:) !! Input array.
        integer(kind=i8), intent(in)    :: value    !! Value to search.

        has = .false.
        if (findloc(array, value, dim=1) == 0) return
        has = .true.
    end function array_has_i8

    pure elemental logical function equals_r4(a, b) result(equals)
        !! Returns `.true.` if the 4-byte real numbers `a` and `b` are
        !! approximately the same, else `.false.`.
        real(kind=r4), intent(in) :: a, b

        equals = abs(a - b) <= epsilon(a)
    end function equals_r4

    pure elemental logical function equals_r8(a, b) result(equals)
        !! Returns `.true.` if the 8-byte real numbers `a` and `b` are
        !! approximately the same, else `.false.`.
        real(kind=r8), intent(in) :: a, b

        equals = abs(a - b) <= epsilon(a)
    end function equals_r8

    pure function f4_to_a(f) result(str)
        !! Converts 4-byte real to allocatable string of length > 1.
        real(kind=r4), intent(in)     :: f   !! Value.
        character(len=:), allocatable :: str !! String of value.

        character(len=20) :: buf
        integer           :: stat

        str = ''
        write (buf, '(1pg0.12)', iostat=stat) f
        if (stat /= 0) return
        str = trim(buf)
    end function f4_to_a

    pure function f8_to_a(f) result(str)
        !! Converts 8-byte real to allocatable string of length > 1.
        real(kind=r8), intent(in)     :: f   !! Value.
        character(len=:), allocatable :: str !! String of value.

        character(len=20) :: buf
        integer           :: stat

        str = ''
        write (buf, '(1pg0.12)', iostat=stat) f
        if (stat /= 0) return
        str = trim(buf)
    end function f8_to_a

    pure function i4_to_a(i) result(str)
        !! Converts 4-byte integer to allocatable string of length > 0.
        integer, intent(in)           :: i   !! Value.
        character(len=:), allocatable :: str !! String of value.

        integer :: n, stat

        if (i == 0) then
            n = 1
        else
            n = floor(log10(real(abs(i))) + 1)
            if (i < 0) n = n + 1
        end if

        allocate (character(len=n) :: str)
        write (str, '(i0)', iostat=stat) i
    end function i4_to_a

    pure function i8_to_a(i) result(str)
        !! Converts 8-byte integer to allocatable string of length > 0.
        integer(kind=i8), intent(in)  :: i   !! Value.
        character(len=:), allocatable :: str !! String of value.

        integer :: n, stat

        if (i == 0) then
            n = 1
        else
            n = floor(log10(real(abs(i))) + 1)
            if (i < 0) n = n + 1
        end if

        allocate (character(len=n) :: str)
        write (str, '(i0)', iostat=stat) i
    end function i8_to_a
end module dm_util
