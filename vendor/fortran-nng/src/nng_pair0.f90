! nng_pair0.f90
!
! Author:  Philipp Engel
! Licence: ISC
module nng_pair0
    !! Bindings to `protocol/pair0/pair.h`.
    use :: nng, only: c_int, nng_socket
    implicit none (type, external)
    private

    public :: nng_pair0_open
    public :: nng_pair0_open_raw

    interface
        ! int nng_pair0_open(nng_socket *s)
        function nng_pair0_open(s) bind(c, name='nng_pair0_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_pair0_open
        end function nng_pair0_open

        ! int nng_pair0_open_raw(nng_socket *s)
        function nng_pair0_open_raw(s) bind(c, name='nng_pair0_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_pair0_open_raw
        end function nng_pair0_open_raw
    end interface
end module nng_pair0
