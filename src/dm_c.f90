! Author:  Philipp Engel
! Licence: ISC
module dm_c
    !! Utility procedures for C interoperability.
    use :: unix, only: c_int, c_unsigned_char
    use :: dm_kind
    implicit none (type, external)
    private

    public :: c_int
    public :: c_unsigned_char

    interface dm_to_signed
        !! Converts unsigned integer to signed integer.
        module procedure :: dm_uint16_to_int32
        module procedure :: dm_uint32_to_int64
    end interface dm_to_signed

    interface dm_to_unsigned
        !! Converts signed integer to unsigned integer.
        module procedure :: dm_int32_to_uint16
        module procedure :: dm_int64_to_uint32
    end interface dm_to_unsigned

    public :: dm_c_f_logical
    public :: dm_c_f_string_characters
    public :: dm_c_f_string_pointer
    public :: dm_f_c_logical
    public :: dm_f_c_string

    public :: dm_to_signed
    public :: dm_to_unsigned

    public :: dm_int32_to_uint16
    public :: dm_int64_to_uint32
    public :: dm_uint16_to_int32
    public :: dm_uint32_to_int64
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental logical function dm_c_f_logical(c) result(f)
        !! Converts C logical value to Fortran representation.
        integer(c_int), intent(in) :: c

        f = (c /= 0)
    end function dm_c_f_logical

    pure elemental integer(c_int) function dm_f_c_logical(f) result(c)
        !! Converts Fortran logical value to C representation.
        logical, intent(in) :: f

        if (f) then
            c = 1
        else
            c = 0
        end if
    end function dm_f_c_logical

    pure function dm_f_c_string(f) result(c)
        !! Returns trimmed `string` with appended null-termination.
        use, intrinsic :: iso_c_binding, only: c_null_char

        character(*), intent(in)   :: f !! Fortran string.
        character(len_trim(f) + 1) :: c !! Null-terminated string.

        c = trim(f) // c_null_char
    end function dm_f_c_string

    pure elemental function dm_int32_to_uint16(s) result(u)
        !! Converts signed 4-byte integer to unsigned 2-byte integer.
        integer(i4), intent(in) :: s !! Signed integer.
        integer(u2)             :: u !! Unsigned integer.

        integer(i4) :: i

        i = modulo(s, 65536_i4)

        if (i < 32768_i4) then
            u = int(i, u2)
        else
            u = int(i - 65536_i4, u2)
        end if
    end function dm_int32_to_uint16

    pure elemental function dm_int64_to_uint32(s) result(u)
        !! Converts signed 8-byte integer to unsigned 4-byte integer.
        integer(i8), intent(in) :: s !! Signed integer.
        integer(u4)             :: u !! Unsigned integer.

        integer(i8) :: i

        i = modulo(s, 4294967296_i8)

        if (i < 2147483648_i8) then
            u = int(i, u4)
        else
            u = int(i - 4294967296_i8, u4)
        end if
    end function dm_int64_to_uint32

    pure elemental function dm_uint16_to_int32(u) result(s)
        !! Converts unsigned 2-byte integer to signed 4-byte integer.
        integer(u2), intent(in) :: u !! Unsigned integer.
        integer(i4)             :: s !! Signed integer.

        if (u >= 0) then
            s = int(u, i4)
        else
            s = 65536_i4 + int(u, i4)
        end if
    end function dm_uint16_to_int32

    pure elemental function dm_uint32_to_int64(u) result(s)
        !! Converts unsigned 4-byte integer to signed 8-byte integer.
        integer(u4), intent(in) :: u !! Unsigned integer.
        integer(i8)             :: s !! Signed integer.

        if (u >= 0) then
            s = int(u, i8)
        else
            s = 4294967296_i8 + int(u, i8)
        end if
    end function dm_uint32_to_int64

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_c_f_string_characters(c, f)
        !! Copies a C string, passed as a C char array, to a Fortran string.
        use, intrinsic :: iso_c_binding, only: c_char, c_null_char

        character(c_char), intent(inout) :: c(:) !! C char array.
        character(size(c)), intent(out)  :: f    !! Fortran string.

        integer :: i

        f = ' '

        do i = 1, size(c)
            if (c(i) == c_null_char) exit
            f(i:i) = c(i)
        end do
    end subroutine dm_c_f_string_characters

    subroutine dm_c_f_string_pointer(c, f)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
        use, intrinsic :: iso_c_binding
        use :: unix, only: c_strlen

        type(c_ptr),               intent(in)  :: c !! C string pointer.
        character(:), allocatable, intent(out) :: f !! Fortran string.

        character(c_char), pointer :: ptrs(:)
        integer(c_size_t)          :: i, n, stat

        copy_block: block
            if (.not. c_associated(c)) exit copy_block
            n = c_strlen(c)
            if (n < 0) exit copy_block
            call c_f_pointer(c, ptrs, [ n ])
            allocate (character(n) :: f, stat=stat)
            if (stat /= 0) exit copy_block

            do i = 1, n
                f(i:i) = ptrs(i)
            end do

            return
        end block copy_block

        if (.not. allocated(f)) f = ''
    end subroutine dm_c_f_string_pointer
end module dm_c
