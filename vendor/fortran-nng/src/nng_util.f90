! nng_util.f90
!
! Author:  Philipp Engel
! Licence: ISC
module nng_util
    !! Utility routines.
    implicit none (type, external)
    private

    public :: c_f_str_ptr
    public :: f_c_str
contains
    subroutine c_f_str_ptr(c_str, f_str)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
        use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_size_t, c_associated, c_f_pointer

        type(c_ptr),               intent(in)  :: c_str
        character(:), allocatable, intent(out) :: f_str

        character(c_char), pointer :: ptrs(:)
        integer(c_size_t)          :: i, sz

        interface
            function c_strlen(str) bind(c, name='strlen')
                import :: c_ptr, c_size_t
                implicit none
                type(c_ptr), intent(in), value :: str
                integer(c_size_t)              :: c_strlen
            end function c_strlen
        end interface

        copy_block: block
            if (.not. c_associated(c_str)) exit copy_block
            sz = c_strlen(c_str)
            if (sz < 0) exit copy_block
            call c_f_pointer(c_str, ptrs, [ sz ])
            allocate (character(sz) :: f_str)

            do i = 1, sz
                f_str(i:i) = ptrs(i)
            end do

            return
        end block copy_block

        if (.not. allocated(f_str)) f_str = ''
    end subroutine c_f_str_ptr

    pure function f_c_str(f_str) result(c_str)
        !! Converts Fortran string to C string. In Fortran 2023, use
        !! `f_c_string()` instead.
        use, intrinsic :: iso_c_binding, only: c_null_char

        character(*), intent(in)  :: f_str
        character(:), allocatable :: c_str

        c_str = trim(f_str) // c_null_char
    end function f_c_str
end module nng_util
