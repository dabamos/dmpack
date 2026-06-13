! xmpp_util.f90
!
! Author:  Philipp Engel
! Licence: ISC
module xmpp_util
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    interface
        ! void *memcpy(void *dst, const void *src, size_t len)
        subroutine c_memcpy(dst, src, len) bind(c, name='memcpy')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: dst
            type(c_ptr),            intent(in), value :: src
            integer(kind=c_size_t), intent(in), value :: len
        end subroutine c_memcpy

        ! size_t strlen(const char *str)
        function c_strlen(str) bind(c, name='strlen')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: str
            integer(kind=c_size_t)         :: c_strlen
        end function c_strlen
    end interface

    public :: c_f_str_ptr
    public :: c_memcpy
    public :: c_strlen
contains
    subroutine c_f_str_ptr(c, f, len)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
        type(c_ptr),               intent(in)           :: c   !! C string pointer.
        character(:), allocatable, intent(out)          :: f   !! Fortran string.
        integer(c_size_t),         intent(in), optional :: len !! Optional string length.

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

            if (present(len)) then
                n = len
            else
                n = c_strlen(c)
            end if

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
end module xmpp_util
