! nng_pair1.f90
!
! Author:  Philipp Engel
! Licence: ISC
module nng_pair1
    !! Bindings to `protocol/pair1/pair.h`.
    use :: nng, only: c_int, c_null_char, nng_socket
    implicit none (type, external)
    private

    character(*), parameter, public :: NNG_OPT_PAIR1_POLY = 'pair1:polyamorous' // c_null_char

    integer(c_int), parameter, public :: NNG_PAIR1_SELF = int(z'11')
    integer(c_int), parameter, public :: NNG_PAIR1_PEER = int(z'11')

    character(*), parameter, public :: NNG_PAIR1_SELF_NAME = 'pair1' // c_null_char
    character(*), parameter, public :: NNG_PAIR1_PEER_NAME = 'pair1' // c_null_char

    public :: nng_pair1_open
    public :: nng_pair1_open_poly
    public :: nng_pair1_open_raw

    interface
        ! int nng_pair1_open(nng_socket *s)
        function nng_pair1_open(s) bind(c, name='nng_pair1_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_pair1_open
        end function nng_pair1_open

        ! int nng_pair1_open_poly(nng_socket *s)
        function nng_pair1_open_poly(s) bind(c, name='nng_pair1_open_poly')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_pair1_open_poly
        end function nng_pair1_open_poly

        ! int nng_pair1_open_raw(nng_socket *s)
        function nng_pair1_open_raw(s) bind(c, name='nng_pair1_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_pair1_open_raw
        end function nng_pair1_open_raw
    end interface
end module nng_pair1
