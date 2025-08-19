! modbus_tcp.f90
!
! Author:  Philipp Engel
! Licence: ISC
module modbus_tcp
    !! Auto-generated Fortran 2018 interace bindings to `modbus-tcp.h`.
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    integer(kind=c_int), parameter, public :: MODBUS_TCP_DEFAULT_PORT = 502
    integer(kind=c_int), parameter, public :: MODBUS_TCP_SLAVE        = int(z'FF')

    ! Modbus_Application_Protocol_V1_1b.pdf Chapter 4 Section 1 Page 5
    ! TCP MODBUS ADU = 253 bytes + MBAP (7 bytes) = 260 bytes
    integer(kind=c_int), parameter, public :: MODBUS_TCP_MAX_ADU_LENGTH = 260

    public :: modbus_new_tcp
    public :: modbus_new_tcp_
    public :: modbus_new_tcp_pi
    public :: modbus_new_tcp_pi_
    public :: modbus_tcp_accept
    public :: modbus_tcp_listen
    public :: modbus_tcp_pi_accept
    public :: modbus_tcp_pi_listen

    interface
        ! modbus_t *modbus_new_tcp(const char *ip_address, int port)
        function modbus_new_tcp_(ip_address, port) bind(c, name='modbus_new_tcp')
            import :: c_char, c_int, c_ptr
            implicit none
            character(kind=c_char), intent(in)        :: ip_address
            integer(kind=c_int),    intent(in), value :: port
            type(c_ptr)                               :: modbus_new_tcp_
        end function modbus_new_tcp_

        ! modbus_t *modbus_new_tcp_pi(const char *node, const char *service)
        function modbus_new_tcp_pi_(node, service) bind(c, name='modbus_new_tcp_pi')
            import :: c_char, c_ptr
            implicit none
            character(kind=c_char), intent(in) :: node
            character(kind=c_char), intent(in) :: service
            type(c_ptr)                        :: modbus_new_tcp_pi_
        end function modbus_new_tcp_pi_

        ! int modbus_tcp_accept(modbus_t *ctx, int *s)
        function modbus_tcp_accept(ctx, s) bind(c, name='modbus_tcp_accept')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(out)       :: s
            integer(kind=c_int)                    :: modbus_tcp_accept
        end function modbus_tcp_accept

        ! int modbus_tcp_listen(modbus_t *ctx, int nb_connection)
        function modbus_tcp_listen(ctx, nb_connection) bind(c, name='modbus_tcp_listen')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(in), value :: nb_connection
            integer(kind=c_int)                    :: modbus_tcp_listen
        end function modbus_tcp_listen

        ! int modbus_tcp_pi_accept(modbus_t *ctx, int *s)
        function modbus_tcp_pi_accept(ctx, s) bind(c, name='modbus_tcp_pi_accept')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(out)       :: s
            integer(kind=c_int)                    :: modbus_tcp_pi_accept
        end function modbus_tcp_pi_accept

        ! int modbus_tcp_pi_listen(modbus_t *ctx, int nb_connection)
        function modbus_tcp_pi_listen(ctx, nb_connection) bind(c, name='modbus_tcp_pi_listen')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(in), value :: nb_connection
            integer(kind=c_int)                    :: modbus_tcp_pi_listen
        end function modbus_tcp_pi_listen
    end interface
contains
    ! modbus_t *modbus_new_tcp(const char *ip_address, int port)
    function modbus_new_tcp(ip_address, port) result(ctx)
        character(len=*), intent(in) :: ip_address
        integer,          intent(in) :: port
        type(c_ptr)                  :: ctx

        ctx = modbus_new_tcp_(trim(ip_address) // c_null_char, port)
    end function modbus_new_tcp

    ! modbus_t *modbus_new_tcp_pi(const char *node, const char *service)
    function modbus_new_tcp_pi(node, service) result(ctx)
        character(len=*), intent(in) :: node
        character(len=*), intent(in) :: service
        type(c_ptr)                  :: ctx

        ctx = modbus_new_tcp_pi_(trim(node) // c_null_char, trim(service) // c_null_char)
    end function modbus_new_tcp_pi
end module modbus_tcp
