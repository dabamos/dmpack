! curl_util.f90
!
! Utility procedures for C interoperability.
!
! Author:  Philipp Engel
! Licence: ISC
module curl_util
    use, intrinsic :: iso_c_binding, only: c_associated, c_f_pointer, c_loc, &
                                           c_char, c_double, c_int, c_intptr_t, c_long, c_size_t, &
                                           c_funptr, c_ptr, &
                                           c_null_char, c_null_ptr
#if HAS_UNSIGNED

    use, intrinsic :: iso_c_binding, only: c_unsigned

#endif
    use, intrinsic :: iso_fortran_env, only: i8 => int64
    implicit none
    private

#if HAS_UNSIGNED

    public :: c_unsigned

#else

    integer, parameter, public :: c_unsigned = c_int

#endif

    public :: c_associated
    public :: c_f_pointer
    public :: c_f_str_ptr
    public :: c_loc

    public :: c_char
    public :: c_double
    public :: c_int
    public :: c_intptr_t
    public :: c_long
    public :: c_size_t

    public :: c_funptr
    public :: c_ptr

    public :: c_null_char
    public :: c_null_ptr
contains
    subroutine c_f_str_ptr(c_str, f_str, size)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
        type(c_ptr),                   intent(in)           :: c_str
        character(len=:), allocatable, intent(out)          :: f_str
        integer(kind=i8),              intent(in), optional :: size

        character(kind=c_char), pointer :: ptrs(:)
        integer(kind=i8)                :: i, sz

        interface
            ! size_t strlen(const char *str)
            function c_strlen(str) bind(c, name='strlen')
                import :: c_ptr, c_size_t
                implicit none
                type(c_ptr), intent(in), value :: str
                integer(kind=c_size_t)         :: c_strlen
            end function c_strlen
        end interface

        copy_if: if (c_associated(c_str)) then
            if (present(size)) then
                sz = size
            else
                sz = c_strlen(c_str)
            end if

            if (sz < 0) exit copy_if
            call c_f_pointer(c_str, ptrs, [ sz ])
            allocate (character(len=sz) :: f_str)

            do i = 1, sz
                f_str(i:i) = ptrs(i)
            end do

            return
        end if copy_if

        if (.not. allocated(f_str)) f_str = ''
    end subroutine c_f_str_ptr
end module curl_util
