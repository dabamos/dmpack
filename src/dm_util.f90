! Author:  Philipp Engel
! Licence: ISC
module dm_util
    !! Type conversion functions and other utility procedures.
    use :: dm_const, only: PI
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
        !! Converts integer to string.
        module procedure :: i4_to_a
        module procedure :: i8_to_a
    end interface

    interface dm_ftoa
        !! Converts real to string.
        module procedure :: f4_to_a
        module procedure :: f8_to_a
    end interface

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
        character(len=*),intent(in) :: str
        real(kind=r8)               :: f

        integer :: stat

        f = 0.0_r8
        read (str, *, iostat=stat) f
    end function dm_atof

    pure elemental function dm_atoi(str) result(i)
        !! Converts string to 4-byte integer.
        character(len=*),intent(in) :: str
        integer                     :: i

        integer :: stat

        i = 0
        read (str, *, iostat=stat) i
    end function dm_atoi

    pure elemental function dm_btoi(l) result(i)
        !! Converts logical (boolean) to 4-byte integer.
        logical, intent(in) :: l
        integer             :: i

        i = 0
        if (.not. l) return
        i = 1
    end function dm_btoi

    pure elemental function dm_deg_to_gon(a) result(b)
        !! Converts angle in degrees to gon.
        real(kind=r8), intent(in) :: a
        real(kind=r8)             :: b

        b = a * (10.0_r8 / 9.0_r8)
    end function dm_deg_to_gon

    pure elemental function dm_deg_to_rad(a) result(b)
        !! Converts angle in degrees to radiants.
        real(kind=r8), intent(in) :: a
        real(kind=r8)             :: b

        b = a * (PI / 180.0_r8)
    end function dm_deg_to_rad

    pure elemental function dm_gon_to_deg(a) result(b)
        !! Converts angle in gon to degrees.
        real(kind=r8), intent(in) :: a
        real(kind=r8)             :: b

        b = a * (9.0_r8 / 10.0_r8)
    end function dm_gon_to_deg

    pure elemental function dm_gon_to_rad(a) result(b)
        !! Converts angle in gon to radiants.
        real(kind=r8), intent(in) :: a
        real(kind=r8)             :: b

        b = a * (PI / 200.0_r8)
    end function dm_gon_to_rad

    pure elemental function dm_rad_to_deg(a) result(b)
        !! Converts angle in radiants to degrees.
        real(kind=r8), intent(in) :: a
        real(kind=r8)             :: b

        b = a * (180.0_r8 / PI)
    end function dm_rad_to_deg

    pure elemental function dm_rad_to_gon(a) result(b)
        !! Converts angle in radiants to gon.
        real(kind=r8), intent(in) :: a
        real(kind=r8)             :: b

        b = a * (200.0_r8 / PI)
    end function dm_rad_to_gon

    subroutine dm_sleep(sec)
        !! Pauses program execution for given time in seconds.
        use :: unix, only: c_usleep
        integer, intent(in) :: sec !! Delay in sec.
        integer             :: rc

        rc = c_usleep(sec * 10**6)
    end subroutine dm_sleep

    subroutine dm_usleep(usec)
        !! Pauses program execution for given time in useconds.
        use :: unix, only: c_usleep
        integer, intent(in) :: usec !! Delay in usec.
        integer             :: rc

        rc = c_usleep(usec)
    end subroutine dm_usleep

    ! ******************************************************************
    ! PUBLIC PROCEDURES.
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
        !! Converts 4-byte real to string.
        real(kind=r4), intent(in)     :: f
        character(len=:), allocatable :: str

        character(len=20) :: buf
        integer           :: stat

        str = ''
        write (buf, '(1pg0.8)', iostat=stat) f
        if (stat /= 0) return
        str = trim(buf)
    end function f4_to_a

    pure function f8_to_a(f) result(str)
        !! Converts 8-byte real to string.
        real(kind=r8), intent(in)     :: f
        character(len=:), allocatable :: str

        character(len=20) :: buf
        integer           :: stat

        str = ''
        write (buf, '(1pg0.8)', iostat=stat) f
        if (stat /= 0) return
        str = trim(buf)
    end function f8_to_a

    pure function i4_to_a(i) result(str)
        !! Converts 4-byte integer to string.
        integer, intent(in)           :: i
        character(len=:), allocatable :: str

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
        !! Converts 8-byte integer to string.
        integer(kind=i8), intent(in)  :: i
        character(len=:), allocatable :: str

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
