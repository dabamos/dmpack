! Author:  Philipp Engel
! Licence: ISC
module dm_c
    !! Utility procedures for C interoperability.
    use, intrinsic :: iso_c_binding
    use :: dm_kind
    implicit none (type, external)
    private

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
    public :: dm_f_c_logical

    public :: dm_to_signed
    public :: dm_to_unsigned

    public :: dm_int32_to_uint16
    public :: dm_int64_to_uint32
    public :: dm_uint16_to_int32
    public :: dm_uint32_to_int64
contains
    pure elemental logical function dm_c_f_logical(c) result(f)
        !! Converts C logical value to Fortran representation.
        integer(kind=c_int), intent(in) :: c

        f = (c /= 0)
    end function dm_c_f_logical

    pure elemental integer(kind=c_int) function dm_f_c_logical(f) result(c)
        !! Converts Fortran logical value to C representation.
        logical, intent(in) :: f

        if (f) then
            c = 1
        else
            c = 0
        end if
    end function dm_f_c_logical

    pure elemental function dm_int32_to_uint16(s) result(u)
        !! Converts signed 4-byte integer to unsigned 2-byte integer.
        integer(kind=i4), intent(in) :: s !! Signed integer.
        integer(kind=u2)             :: u !! Unsigned integer.

        integer(kind=i4) :: i

        i = modulo(s, 65536_i4)

        if (i < 32768_i4) then
            u = int(i, kind=u2)
        else
            u = int(i - 65536_i4, kind=u2)
        end if
    end function dm_int32_to_uint16

    pure elemental function dm_int64_to_uint32(s) result(u)
        !! Converts signed 8-byte integer to unsigned 4-byte integer.
        integer(kind=i8), intent(in) :: s !! Signed integer.
        integer(kind=u4)             :: u !! Unsigned integer.

        integer(kind=i8) :: i

        i = modulo(s, 4294967296_i8)

        if (i < 2147483648_i8) then
            u = int(i, kind=u4)
        else
            u = int(i - 4294967296_i8, kind=u4)
        end if
    end function dm_int64_to_uint32

    pure elemental function dm_uint16_to_int32(u) result(s)
        !! Converts unsigned 2-byte integer to signed 4-byte integer.
        integer(kind=u2), intent(in) :: u !! Unsigned integer.
        integer(kind=i4)             :: s !! Signed integer.

        if (u > 0) then
            s = int(u, kind=i4)
        else
            s = 65536_i4 + int(u, kind=i4)
        end if
    end function dm_uint16_to_int32

    pure elemental function dm_uint32_to_int64(u) result(s)
        !! Converts unsigned 4-byte integer to signed 8-byte integer.
        integer(kind=u4), intent(in) :: u !! Unsigned integer.
        integer(kind=i8)             :: s !! Signed integer.

        if (u > 0) then
            s = int(u, kind=i8)
        else
            s = 4294967296_i8 + int(u, kind=i8)
        end if
    end function dm_uint32_to_int64
end module dm_c
