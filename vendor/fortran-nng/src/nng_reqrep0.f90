! nng_reqrep0.f90
!
! Author:  Philipp Engel
! Licence: ISC
module nng_reqrep0
    !! Bindings to `protocol/reqrep0/rep.h` and `protocol/reqrep0/req.h`.
    use :: nng, only: c_int, c_null_char, nng_socket
    implicit none (type, external)
    private

    ! rep.h
    integer(c_int), parameter, public :: NNG_REP0_SELF = int(z'31')
    integer(c_int), parameter, public :: NNG_REP0_PEER = int(z'30')

    character(*), parameter, public :: NNG_REP0_SELF_NAME = 'rep' // c_null_char
    character(*), parameter, public :: NNG_REP0_PEER_NAME = 'req' // c_null_char

    public :: nng_rep0_open
    public :: nng_rep0_open_raw

    interface
        ! int nng_rep0_open(nng_socket *s)
        function nng_rep0_open(s) bind(c, name='nng_rep0_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_rep0_open
        end function nng_rep0_open

        ! int nng_rep0_open_raw(nng_socket *s)
        function nng_rep0_open_raw(s) bind(c, name='nng_rep0_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_rep0_open_raw
        end function nng_rep0_open_raw
    end interface

    ! req.h
    integer(c_int), parameter, public :: NNG_REQ0_SELF = int(z'30')
    integer(c_int), parameter, public :: NNG_REQ0_PEER = int(z'31')

    character(*), parameter, public :: NNG_REQ0_SELF_NAME = 'req' // c_null_char
    character(*), parameter, public :: NNG_REQ0_PEER_NAME = 'rep' // c_null_char

    character(*), parameter, public :: NNG_OPT_REQ_RESENDTIME = 'req:resend-time' // c_null_char
    character(*), parameter, public :: NNG_OPT_REQ_RESENDTICK = 'req:resend-tick' // c_null_char

    public :: nng_req0_open
    public :: nng_req0_open_raw

    interface
        ! int nng_req0_open(nng_socket *s)
        function nng_req0_open(s) bind(c, name='nng_req0_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_req0_open
        end function nng_req0_open

        ! int nng_req0_open_raw(nng_socket *s)
        function nng_req0_open_raw(s) bind(c, name='nng_req0_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_req0_open_raw
        end function nng_req0_open_raw
    end interface
end module nng_reqrep0
