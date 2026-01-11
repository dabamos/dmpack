! nng_survey0.f90
!
! Author:  Philipp Engel
! Licence: ISC
module nng_survey0
    !! Bindings to `protocol/survey0/respond.h` and `protocol/survey0/survey.h`.
    use :: nng, only: c_int, c_null_char, nng_socket
    implicit none (type, external)
    private

    ! respond.h
    integer(c_int), parameter, public :: NNG_RESPONDENT0_SELF = int(z'63')
    integer(c_int), parameter, public :: NNG_RESPONDENT0_PEER = int(z'62')

    character(*), parameter, public :: NNG_RESPONDENT0_SELF_NAME = 'respondent' // c_null_char
    character(*), parameter, public :: NNG_RESPONDENT0_PEER_NAME = 'surveyor' // c_null_char

    public :: nng_respondent0_open
    public :: nng_respondent0_open_raw

    interface
        ! int nng_respondent0_open(nng_socket *s)
        function nng_respondent0_open(s) bind(c, name='nng_respondent0_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_respondent0_open
        end function nng_respondent0_open

        ! int nng_respondent0_open_raw(nng_socket *s)
        function nng_respondent0_open_raw(s) bind(c, name='nng_respondent0_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_respondent0_open_raw
        end function nng_respondent0_open_raw
    end interface

    ! survey.h
    integer(c_int), parameter, public :: NNG_SURVEYOR0_SELF = int(z'62')
    integer(c_int), parameter, public :: NNG_SURVEYOR0_PEER = int(z'63')

    character(*), parameter, public :: NNG_SURVEYOR0_SELF_NAME = 'surveyor' // c_null_char
    character(*), parameter, public :: NNG_SURVEYOR0_PEER_NAME = 'respondent' // c_null_char

    character(*), parameter, public :: NNG_OPT_SURVEYOR_SURVEYTIME = 'surveyor:survey-time' // c_null_char

    public :: nng_surveyor0_open
    public :: nng_surveyor0_open_raw

    interface
        ! int nng_surveyor0_open(nng_socket *s)
        function nng_surveyor0_open(s) bind(c, name='nng_surveyor0_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_surveyor0_open
        end function nng_surveyor0_open

        ! int nng_surveyor0_open_raw(nng_socket *s)
        function nng_surveyor0_open_raw(s) bind(c, name='nng_surveyor0_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_surveyor0_open_raw
        end function nng_surveyor0_open_raw
    end interface
end module nng_survey0
