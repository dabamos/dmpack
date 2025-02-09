! Author:  Philipp Engel
! Licence: ISC
module dm_util
    !! Type conversion functions and other utility procedures.
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    character(len=*), parameter :: FMT_INTEGER = '(i0)'
    character(len=*), parameter :: FMT_REAL    = '(1pg0.12)'

    interface dm_array_has
        !! Returns whether array contains an integer value.
        module procedure :: array_has_int32
        module procedure :: array_has_int64
    end interface dm_array_has

    interface dm_equals
        !! Returns whether two real numbers are approximately the same.
        module procedure :: equals_real32
        module procedure :: equals_real64
    end interface dm_equals

    interface dm_hex_to_int
        !! Converts hexadecimal string to integer.
        module procedure :: hex_to_int32
        module procedure :: hex_to_int64
    end interface

    interface dm_inc
        !! Returns increased integer value.
        module procedure :: inc_int32
        module procedure :: inc_int64
    end interface dm_inc

    interface dm_itoa
        !! Generic integer to string converter.
        module procedure :: int32_to_string
        module procedure :: int64_to_string
    end interface dm_itoa

    interface dm_ftoa
        !! Generic real to string converter.
        module procedure :: real32_to_string
        module procedure :: real64_to_string
    end interface dm_ftoa

    interface dm_from_real64
        !! Converts 8-byte real to type (for response values).
        module procedure :: dm_real64_to_int32
        module procedure :: dm_real64_to_int64
        module procedure :: dm_real64_to_logical
        module procedure :: dm_real64_to_real32
    end interface dm_from_real64

    interface dm_to_real64
        !! Converts type to 8-byte real (for response values).
        module procedure :: dm_int32_to_real64
        module procedure :: dm_int64_to_real64
        module procedure :: dm_logical_to_real64
        module procedure :: dm_real32_to_real64
    end interface dm_to_real64

    interface dm_present
        !! Returns present argument or default value.
        module procedure :: present_character
        module procedure :: present_logical
        module procedure :: present_int32
        module procedure :: present_int64
        module procedure :: present_ptr
        module procedure :: present_real32
        module procedure :: present_real64
    end interface dm_present

    interface dm_sec_to_msec
        !! Generic seconds to milliseconds function.
        module procedure :: sec_to_msec_int32
        module procedure :: sec_to_msec_int64
    end interface dm_sec_to_msec

    interface dm_msec_to_sec
        !! Generic seconds to milliseconds function.
        module procedure :: msec_to_sec_int32
        module procedure :: msec_to_sec_int64
    end interface dm_msec_to_sec

    ! Public procedures.
    public :: dm_atof
    public :: dm_atoi
    public :: dm_btoa
    public :: dm_btoi
    public :: dm_ftoa
    public :: dm_itoa

    public :: dm_array_has
    public :: dm_equals
    public :: dm_hex_to_int
    public :: dm_inc
    public :: dm_msleep
    public :: dm_present
    public :: dm_sleep
    public :: dm_usleep

    public :: dm_deg_to_gon
    public :: dm_deg_to_rad
    public :: dm_gon_to_deg
    public :: dm_gon_to_rad
    public :: dm_rad_to_deg
    public :: dm_rad_to_gon

    public :: dm_from_real64
    public :: dm_to_real64

    public :: dm_int32_to_real64
    public :: dm_int64_to_real64
    public :: dm_logical_to_real64
    public :: dm_real64_to_int32
    public :: dm_real64_to_int64
    public :: dm_real64_to_logical
    public :: dm_real64_to_real32

    public :: dm_msec_to_sec
    public :: dm_sec_to_msec

    ! Private procedures.
    private :: array_has_int32
    private :: array_has_int64

    private :: equals_real32
    private :: equals_real64

    private :: inc_int32
    private :: inc_int64

    private :: int32_to_string
    private :: int64_to_string

    private :: real32_to_string
    private :: real64_to_string

    private :: hex_to_int32
    private :: hex_to_int64

    private :: present_character
    private :: present_logical
    private :: present_int32
    private :: present_int64
    private :: present_ptr
    private :: present_real32
    private :: present_real64

    private :: msec_to_sec_int32
    private :: msec_to_sec_int64
    private :: sec_to_msec_int32
    private :: sec_to_msec_int64
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    pure elemental function dm_atof(a) result(f)
        !! Converts string to 8-byte real.
        character(len=*),intent(in) :: a !! Number string.
        real(kind=r8)               :: f !! Value.

        integer :: stat

        f = 0.0_r8
        read (a, *, iostat=stat) f
    end function dm_atof

    pure elemental function dm_atoi(a) result(i)
        !! Converts string to 4-byte integer.
        character(len=*),intent(in) :: a !! Number string.
        integer(kind=i4)            :: i !! Value.

        integer :: stat

        i = 0
        read (a, *, iostat=stat) i
    end function dm_atoi

    pure function dm_btoa(b, true, false) result(a)
        !! Returns either argument `true` or `false` as allocatable character,
        !! depending on the value of logical value `b`. If `true` and/or
        !! `false` is missing, `T` or `F` is returned respectively. The result
        !! is trimmed.
        logical,          intent(in)           :: b     !! Logical value.
        character(len=*), intent(in), optional :: true  !! Return value on `.true.`.
        character(len=*), intent(in), optional :: false !! Return value on `.false.`.
        character(len=:), allocatable          :: a     !! Returned string.

        if (b) then
            if (present(true)) then
                a = trim(true)
            else
                a = 'T'
            end if
        else
            if (present(false)) then
                a = trim(false)
            else
                a = 'F'
            end if
        end if
    end function dm_btoa

    pure elemental function dm_btoi(b) result(i)
        !! Converts logical (boolean) to 4-byte integer.
        logical, intent(in) :: b !! Logical value.
        integer             :: i !! `0` or `1`.

        i = 0
        if (b) i = 1
    end function dm_btoi

    ! **************************************************************************
    ! PUBLIC ANGLE FUNCTIONS.
    ! **************************************************************************
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

    ! **************************************************************************
    ! PUBLIC INTRINSIC TYPE TO REAL FUNCTIONS.
    ! **************************************************************************
    pure elemental function dm_int32_to_real64(i32) result(r64)
        !! Converts 4-byte integer to 8-byte real.
        integer(kind=i4), intent(in) :: i32 !! 4-byte integer value.
        real(kind=r8)                :: r64 !! Value as 8-byte real.

        r64 = real(i32, kind=r8)
    end function dm_int32_to_real64

    pure elemental function dm_int64_to_real64(i64) result(r64)
        !! Converts 8-byte integer to 8-byte real.
        integer(kind=i8), intent(in) :: i64 !! 8-byte integer value.
        real(kind=r8)                :: r64 !! Value as 8-byte real.

        r64 = real(i64, kind=r8)
    end function dm_int64_to_real64

    pure elemental function dm_logical_to_real64(l) result(r64)
        !! Converts logical to 8-byte real (`0.0` for `.false.` and `1.0` for
        !! `.true.`).
        logical, intent(in) :: l   !! Logical value.
        real(kind=r8)       :: r64 !! Value as 8-byte real.

        if (l) then
            r64 = 1.0_r8
        else
            r64 = 0.0_r8
        end if
    end function dm_logical_to_real64

    pure elemental function dm_real32_to_real64(r32) result(r64)
        !! Converts 8-byte integer to 8-byte real.
        real(kind=r4), intent(in) :: r32 !! 4-byte real value.
        real(kind=r8)             :: r64 !! Value as 8-byte real.

        r64 = real(r32, kind=r8)
    end function dm_real32_to_real64

    ! **************************************************************************
    ! PUBLIC REAL TO INTRINSIC TYPE FUNCTIONS.
    ! **************************************************************************
    pure elemental subroutine dm_real64_to_int32(from, to)
        !! Converts 8-byte real to 4-byte integer.
        real(kind=r8),    intent(in)  :: from !! 8-byte real value.
        integer(kind=i4), intent(out) :: to   !! 4-byte integer value.

        to = int(from, kind=i4)
    end subroutine dm_real64_to_int32

    pure elemental subroutine dm_real64_to_int64(from, to)
        !! Converts 8-byte real to 8-byte integer.
        real(kind=r8),    intent(in)  :: from !! 8-byte real value.
        integer(kind=i8), intent(out) :: to   !! 8-byte integer value.

        to = int(from, kind=i8)
    end subroutine dm_real64_to_int64

    pure elemental subroutine dm_real64_to_logical(from, to)
        !! Converts 8-byte real to logical. If `f` equals `0.0`, the result is
        !! `.false.`, else `.true.`.
        real(kind=r8), intent(in)  :: from !! 8-byte real value.
        logical,       intent(out) :: to   !! Logical value.

        to = (.not. dm_equals(from, 0.0_r8))
    end subroutine dm_real64_to_logical

    pure elemental subroutine dm_real64_to_real32(from, to)
        !! Converts 8-byte real to 4-byte real
        real(kind=r8), intent(in)  :: from !! 8-byte real value.
        real(kind=r4), intent(out) :: to   !! 4-byte real value.

        to = real(from, kind=r4)
    end subroutine dm_real64_to_real32

    ! **************************************************************************
    ! PUBLIC SLEEP ROUTINES.
    ! **************************************************************************
    subroutine dm_msleep(msec)
        !! Pauses program execution for given time in mseconds.
        use :: unix, only: c_useconds_t, c_usleep

        integer, intent(in) :: msec !! Delay [msec].

        integer :: rc

        rc = c_usleep(int(msec * 1000, kind=c_useconds_t))
    end subroutine dm_msleep

    subroutine dm_sleep(sec)
        !! Pauses program execution for given time in seconds.
        use :: unix, only: c_useconds_t, c_usleep

        integer, intent(in) :: sec !! Delay [sec].

        integer :: rc

        rc = c_usleep(int(sec * 10**6, kind=c_useconds_t))
    end subroutine dm_sleep

    subroutine dm_usleep(usec)
        !! Pauses program execution for given time in useconds.
        use :: unix, only: c_useconds_t, c_usleep

        integer, intent(in) :: usec !! Delay [usec].

        integer :: rc

        rc = c_usleep(int(usec, kind=c_useconds_t))
    end subroutine dm_usleep

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    logical function array_has_int32(array, value) result(has)
        !! Returns `.true.` if the integer array contains the given value.
        integer(kind=i4), intent(inout) :: array(:) !! Input array.
        integer(kind=i4), intent(in)    :: value    !! Value to search.

        has = (findloc(array, value, dim=1) > 0)
    end function array_has_int32

    logical function array_has_int64(array, value) result(has)
        !! Returns `.true.` if the integer array contains the given value.
        integer(kind=i8), intent(inout) :: array(:) !! Input array.
        integer(kind=i8), intent(in)    :: value    !! Value to search.

        has = (findloc(array, value, dim=1) > 0)
    end function array_has_int64

    pure elemental logical function equals_real32(a, b) result(equals)
        !! Returns `.true.` if the 4-byte real numbers `a` and `b` are
        !! approximately the same, else `.false.`.
        real(kind=r4), intent(in) :: a, b

        equals = (abs(a - b) <= epsilon(a))
    end function equals_real32

    pure elemental logical function equals_real64(a, b) result(equals)
        !! Returns `.true.` if the 8-byte real numbers `a` and `b` are
        !! approximately the same, else `.false.`.
        real(kind=r8), intent(in) :: a, b

        equals = (abs(a - b) <= epsilon(a))
    end function equals_real64

    pure elemental integer(kind=i4) function inc_int32(a, b) result(c)
        !! Increases argument `a` by 1 or `b`.
        integer(kind=i4), intent(in)           :: a !! Value to increase.
        integer(kind=i4), intent(in), optional :: b !! Summand to use.

        if (present(b)) then
            c = a + b
        else
            c = a + 1
        end if
    end function inc_int32

    pure elemental integer(kind=i8) function inc_int64(a, b) result(c)
        !! Increases argument `a` by 1 or `b`.
        integer(kind=i8), intent(in)           :: a !! Value to increase.
        integer(kind=i8), intent(in), optional :: b !! Summand to use.

        if (present(b)) then
            c = a + b
        else
            c = a + 1
        end if
    end function inc_int64

    ! **************************************************************************
    ! PRIVATE PRESENT FUNCTIONS.
    ! **************************************************************************
    pure elemental function present_character(arg, default) result(value)
        !! Returns 1-byte character argument `arg` if present or `default`
        !! otherwise.
        character, intent(in), optional :: arg     !! Argument.
        character, intent(in)           :: default !! Default value.
        character                       :: value   !! Argument or default.

        if (present(arg)) then
            value = arg
        else
            value = default
        end if
    end function present_character

    pure elemental function present_logical(arg, default) result(value)
        !! Returns logical argument `arg` if present or `default` otherwise.
        logical, intent(in), optional :: arg     !! Argument.
        logical, intent(in)           :: default !! Default value.
        logical                       :: value   !! Argument or default.

        if (present(arg)) then
            value = arg
        else
            value = default
        end if
    end function present_logical

    pure elemental function present_int32(arg, default) result(value)
        !! Returns 4-byte integer argument `arg` if present or `default`
        !! otherwise.
        integer(kind=i4), intent(in), optional :: arg     !! Argument.
        integer(kind=i4), intent(in)           :: default !! Default value.
        integer(kind=i4)                       :: value   !! Argument or default.

        if (present(arg)) then
            value = arg
        else
            value = default
        end if
    end function present_int32

    pure elemental function present_int64(arg, default) result(value)
        !! Returns 8-byte integer argument `arg` if present or `default`
        !! otherwise.
        integer(kind=i8), intent(in), optional :: arg     !! Argument.
        integer(kind=i8), intent(in)           :: default !! Default value.
        integer(kind=i8)                       :: value   !! Argument or default.

        if (present(arg)) then
            value = arg
        else
            value = default
        end if
    end function present_int64

    pure elemental function present_ptr(arg, default) result(value)
        !! Returns 4-byte real argument `arg` if present or `default`
        !! otherwise.
        use, intrinsic :: iso_c_binding, only: c_ptr

        type(c_ptr), intent(in), optional :: arg     !! Argument.
        type(c_ptr), intent(in)           :: default !! Default value.
        type(c_ptr)                       :: value   !! Argument or default.

        if (present(arg)) then
            value = arg
        else
            value = default
        end if
    end function present_ptr

    pure elemental function present_real32(arg, default) result(value)
        !! Returns 4-byte real argument `arg` if present or `default`
        !! otherwise.
        real(kind=r4), intent(in), optional :: arg     !! Argument.
        real(kind=r4), intent(in)           :: default !! Default value.
        real(kind=r4)                       :: value   !! Argument or default.

        if (present(arg)) then
            value = arg
        else
            value = default
        end if
    end function present_real32

    pure elemental function present_real64(arg, default) result(value)
        !! Returns 8-byte real argument `arg` if present or `default`
        !! otherwise.
        real(kind=r8), intent(in), optional :: arg     !! Argument.
        real(kind=r8), intent(in)           :: default !! Default value.
        real(kind=r8)                       :: value   !! Argument or default.

        if (present(arg)) then
            value = arg
        else
            value = default
        end if
    end function present_real64

    ! **************************************************************************
    ! PRIVATE NUMBER TO STRING FUNCTIONS.
    ! **************************************************************************
    pure function int32_to_string(value) result(string)
        !! Converts 4-byte integer to allocatable string of length > 0.
        integer(kind=i4), intent(in)  :: value  !! Value.
        character(len=:), allocatable :: string !! String of value.

        integer :: n, stat

        if (value == 0) then
            n = 1
        else
            n = floor(log10(real(abs(value))) + 1)
            if (value < 0) n = n + 1
        end if

        allocate (character(len=n) :: string)
        write (string, FMT_INTEGER, iostat=stat) value
    end function int32_to_string

    pure function int64_to_string(value) result(string)
        !! Converts 8-byte integer to allocatable string of length > 0.
        integer(kind=i8), intent(in)  :: value  !! Value.
        character(len=:), allocatable :: string !! String of value.

        integer :: n, stat

        if (value == 0) then
            n = 1
        else
            n = floor(log10(real(abs(value))) + 1)
            if (value < 0) n = n + 1
        end if

        allocate (character(len=n) :: string)
        write (string, FMT_INTEGER, iostat=stat) value
    end function int64_to_string

    pure function real32_to_string(value) result(string)
        !! Converts 4-byte real to allocatable string of length > 1.
        real(kind=r4), intent(in)     :: value  !! Value.
        character(len=:), allocatable :: string !! String of value.

        character(len=20) :: buffer
        integer           :: stat

        write (buffer, FMT_REAL, iostat=stat) value

        if (stat == 0) then
            string = trim(buffer)
            return
        end if

        string = ''
    end function real32_to_string

    pure function real64_to_string(value) result(string)
        !! Converts 8-byte real to allocatable string of length > 1.
        real(kind=r8), intent(in)     :: value  !! Value.
        character(len=:), allocatable :: string !! String of value.

        character(len=20) :: buffer
        integer           :: stat

        write (buffer, FMT_REAL, iostat=stat) value

        if (stat == 0) then
            string = trim(buffer)
            return
        end if

        string = ''
    end function real64_to_string

    ! **************************************************************************
    ! PRIVATE HEX STRING TO INTEGER ROUTINES.
    ! **************************************************************************
    pure elemental subroutine hex_to_int32(string, value, error)
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
    end subroutine hex_to_int32

    pure elemental subroutine hex_to_int64(string, value, error)
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
    end subroutine hex_to_int64

    ! **************************************************************************
    ! PRIVATE TIME UNIT FUNCTIONS.
    ! **************************************************************************
    pure elemental function msec_to_sec_int32(msec) result(sec)
        !! Converts milliseconds to seconds (4 bytes).
        integer(kind=i4), intent(in) :: msec !! Milliseconds.
        integer(kind=i4)             :: sec  !! Seconds.

        sec = msec / 1000_i4
   end function msec_to_sec_int32

    pure elemental function msec_to_sec_int64(msec) result(sec)
        !! Converts milliseconds to seconds (8 bytes).
        integer(kind=i8), intent(in) :: msec !! Milliseconds.
        integer(kind=i8)             :: sec  !! Seconds.

        sec = msec / 1000_i8
   end function msec_to_sec_int64

    pure elemental function sec_to_msec_int32(sec) result(msec)
        !! Converts seconds to milliseconds (4 bytes).
        integer(kind=i4), intent(in) :: sec  !! Seconds
        integer(kind=i4)             :: msec !! Milliseconds.

        msec = sec * 1000_i4
   end function sec_to_msec_int32

    pure elemental function sec_to_msec_int64(sec) result(msec)
        !! Converts seconds to milliseconds (8 bytes).
        integer(kind=i8), intent(in) :: sec  !! Seconds
        integer(kind=i8)             :: msec !! Milliseconds.

        msec = sec * 1000_i8
   end function sec_to_msec_int64
end module dm_util
