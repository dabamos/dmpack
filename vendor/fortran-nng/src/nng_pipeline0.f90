! nng_pipeline0.f90
!
! Author:  Philipp Engel
! Licence: ISC
module nng_pipeline0
    !! Bindings to `protocol/pipeline0/pull.h` and `protocol/pipeline0/push`.
    use :: nng, only: c_int, nng_socket
    implicit none (type, external)
    private

    ! pull.h
    public :: nng_pull0_open
    public :: nng_pull0_open_raw

    interface
        ! int nng_pull0_open(nng_socket *s)
        function nng_pull0_open(s) bind(c, name='nng_pull0_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_pull0_open
        end function nng_pull0_open

        ! int nng_pull0_open_raw(nng_socket *s)
        function nng_pull0_open_raw(s) bind(c, name='nng_pull0_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_pull0_open_raw
        end function nng_pull0_open_raw
    end interface

    ! push.h
    public :: nng_push0_open
    public :: nng_push0_open_raw

    interface
        ! int nng_push0_open(nng_socket *s)
        function nng_push0_open(s) bind(c, name='nng_push0_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_push0_open
        end function nng_push0_open

        ! int nng_push0_open_raw(nng_socket *s)
        function nng_push0_open_raw(s) bind(c, name='nng_push0_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_push0_open_raw
        end function nng_push0_open_raw
    end interface
end module nng_pipeline0
