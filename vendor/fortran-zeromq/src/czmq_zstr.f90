! czmq_zstr.f90
!
! Author:  Philipp Engel
! Licence: ISC
module czmq_zstr
    !! Auto-generated Fortran 2018 interface bindings to libczmq 4.
    use :: zmq_util
    implicit none (type, external)
    private

    public :: zstr_free
    public :: zstr_recv
    public :: zstr_recv_
    public :: zstr_recv_nowait
    public :: zstr_recv_nowait_
    public :: zstr_send
    public :: zstr_send_
    public :: zstr_sendm
    public :: zstr_sendm_

    interface
        ! void zstr_free(char **string_p)
        subroutine zstr_free(string_p) bind(c, name='zstr_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(inout) :: string_p
        end subroutine zstr_free

        ! char *zstr_recv(void *source)
        function zstr_recv_(source) bind(c, name='zstr_recv')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: source
            type(c_ptr)                    :: zstr_recv_
        end function zstr_recv_

        ! char *zstr_recv_nowait(void *source)
        function zstr_recv_nowait_(source) bind(c, name='zstr_recv_nowait')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: source
            type(c_ptr)                    :: zstr_recv_nowait_
        end function zstr_recv_nowait_

        ! int zstr_send(void *dest, const char *string)
        function zstr_send_(dest, string) bind(c, name='zstr_send')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: dest
            character(c_char), intent(in)        :: string
            integer(c_int)                       :: zstr_send_
        end function zstr_send_

        ! int zstr_sendm(void *dest, const char *string)
        function zstr_sendm_(dest, string) bind(c, name='zstr_sendm')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: dest
            character(c_char), intent(in)        :: string
            integer(c_int)                       :: zstr_sendm_
        end function zstr_sendm_
    end interface
contains
    ! char *zstr_recv(void *source)
    function zstr_recv(source) result(str)
        type(c_ptr), intent(in)   :: source
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zstr_recv_(source)
        call c_f_str_ptr(ptr, str)
        call zstr_free(ptr)
    end function zstr_recv

    ! char *zstr_recv_nowait(void *source)
    function zstr_recv_nowait(source) result(str)
        type(c_ptr), intent(in)   :: source
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zstr_recv_nowait_(source)
        call c_f_str_ptr(ptr, str)
        call zstr_free(ptr)
    end function zstr_recv_nowait

    ! int zstr_send(void *dest, const char *string)
    integer function zstr_send(dest, string) result(rc)
        type(c_ptr),  intent(in) :: dest
        character(*), intent(in) :: string

        rc = zstr_send_(dest, f_c_str(string))
    end function zstr_send

    ! int zstr_sendm(void *dest, const char *string)
    integer function zstr_sendm(dest, string) result(rc)
        type(c_ptr),       intent(in) :: dest
        character(c_char), intent(in) :: string

        rc = zstr_sendm_(dest, f_c_str(string))
    end function zstr_sendm
end module czmq_zstr
