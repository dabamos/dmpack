! nng_bus0.f90
!
! Author:  Philipp Engel
! Licence: ISC
module nng_bus0
    !! Bindings to `protocol/bus0/bus.h`.
    use :: nng, only: c_int, c_null_char, nng_socket
    implicit none (type, external)
    private

    integer(c_int), parameter, public :: NNG_BUS0_SELF = int(z'70')
    integer(c_int), parameter, public :: NNG_BUS0_PEER = int(z'70')

    character(*), parameter, public :: NNG_BUS0_SELF_NAME = 'bus' // c_null_char
    character(*), parameter, public :: NNG_BUS0_PEER_NAME = 'bus' // c_null_char

    public :: nng_bus0_open
    public :: nng_bus0_open_raw

    interface
        ! int nng_bus0_open(nng_socket *s)
        function nng_bus0_open(s) bind(c, name='nng_bus0_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_bus0_open
        end function nng_bus0_open

        ! int nng_bus0_open_raw(nng_socket *s)
        function nng_bus0_open_raw(s) bind(c, name='nng_bus0_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_bus0_open_raw
        end function nng_bus0_open_raw
    end interface
end module nng_bus0
