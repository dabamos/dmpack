! zmq_util.F90
!
! Author:  Philipp Engel
! Licence: ISC
module zmq_util
    !! C types and utility routines.
    use, intrinsic :: iso_c_binding, only: &
        c_bool, c_char, c_int, c_int8_t, c_int32_t, c_int64_t, c_long, c_short, c_signed_char, c_size_t, c_funptr, c_ptr, &
        c_null_char, c_null_funptr, c_null_ptr, c_associated, c_f_pointer, c_funloc, c_loc, c_sizeof
#if HAS_UNSIGNED
    use, intrinsic :: iso_c_binding, only: c_uint8_t, c_uint32_t, c_uint64_t, c_unsigned, c_unsigned_long
#endif
    implicit none (type, external)
    private

    public :: c_bool
    public :: c_char
    public :: c_funptr
    public :: c_int
    public :: c_int8_t
    public :: c_int32_t
    public :: c_int64_t
    public :: c_long
    public :: c_null_char
    public :: c_null_funptr
    public :: c_null_ptr
    public :: c_ptr
    public :: c_short
    public :: c_signed_char
    public :: c_size_t

    public :: c_associated
    public :: c_f_pointer
    public :: c_funloc
    public :: c_loc
    public :: c_sizeof

#if HAS_UNSIGNED
    public :: c_uint8_t
    public :: c_uint32_t
    public :: c_uint64_t
    public :: c_unsigned
    public :: c_unsigned_long
#else
    integer, parameter, public :: c_uint8_t       = c_int8_t
    integer, parameter, public :: c_uint32_t      = c_int32_t
    integer, parameter, public :: c_uint64_t      = c_int64_t
    integer, parameter, public :: c_unsigned      = c_int
    integer, parameter, public :: c_unsigned_long = c_long
#endif

#if defined _WIN32
#if defined _WIN64
    integer, parameter, public :: zmq_fd_t = c_int64_t
#else
    integer, parameter, public :: zmq_fd_t = c_unsigned
#endif
#else
    integer, parameter, public :: zmq_fd_t = c_int
#endif

    integer, parameter, public :: c_byte = c_signed_char

    interface
        ! void free(void *ptr)
        subroutine c_free(ptr) bind(c, name='free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr
        end subroutine c_free
    end interface

    public :: c_f_str_ptr
    public :: c_free
    public :: f_c_str
contains
    subroutine c_f_str_ptr(c, f)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
        type(c_ptr),               intent(in)  :: c !! C string pointer.
        character(:), allocatable, intent(out) :: f !! Fortran string.

        integer(c_size_t) :: n

        interface
            function c_strlen(str) bind(c, name='strlen')
                import :: c_ptr, c_size_t
                implicit none
                type(c_ptr), intent(in), value :: str
                integer(c_size_t)              :: c_strlen
            end function c_strlen
        end interface

        copy_block: block
            if (.not. c_associated(c)) exit copy_block
            n = int(c_strlen(c), c_size_t)
            if (n <= 0) exit copy_block

            block
                character(n), pointer :: ptr
                call c_f_pointer(c, ptr)
                f = ptr
            end block

            return
        end block copy_block

        if (.not. allocated(f)) f = ''
    end subroutine c_f_str_ptr

    pure function f_c_str(f) result(c)
        !! Converts Fortran string to C string. In Fortran 2023, use
        !! `f_c_string()` instead.
        character(*), intent(in)   :: f !! Fortran string.
        character(len_trim(f) + 1) :: c !! Null-terminated C string.

        c = trim(f) // c_null_char
    end function f_c_str
end module zmq_util
