! nng_pubsub0.f90
!
! Author:  Philipp Engel
! Licence: ISC
module nng_pubsub0
    !! Bindings to `protocol/pubsub0/pub.h` and `protocol/pubsub0/sub.h`.
    use :: nng, only: c_int, c_null_char, c_ptr, c_size_t, nng_ctx, nng_socket
    implicit none (type, external)
    private

    ! pub.h
    public :: nng_pub0_open
    public :: nng_pub0_open_raw

    interface
        ! int nng_pub0_open(nng_socket *s)
        function nng_pub0_open(s) bind(c, name='nng_pub0_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_pub0_open
        end function nng_pub0_open

        ! int nng_pub0_open_raw(nng_socket *s)
        function nng_pub0_open_raw(s) bind(c, name='nng_pub0_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_pub0_open_raw
        end function nng_pub0_open_raw
    end interface

    ! sub.h
    character(*), parameter, public :: NNG_OPT_SUB_SUBSCRIBE   = 'sub:subscribe' // c_null_char
    character(*), parameter, public :: NNG_OPT_SUB_UNSUBSCRIBE = 'sub:unsubscribe' // c_null_char
    character(*), parameter, public :: NNG_OPT_SUB_PREFNEW     = 'sub:prefnew' // c_null_char

    public :: nng_sub0_ctx_subscribe
    public :: nng_sub0_ctx_unsubscribe
    public :: nng_sub0_open
    public :: nng_sub0_open_raw
    public :: nng_sub0_socket_subscribe
    public :: nng_sub0_socket_unsubscribe

    interface
        ! int nng_sub0_ctx_subscribe(nng_ctx id, const void *buf, size_t sz)
        function nng_sub0_ctx_subscribe(id, buf, sz) bind(c, name='nng_sub0_ctx_subscribe')
            import :: c_int, c_ptr, c_size_t, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: id
            type(c_ptr),       intent(in), value :: buf
            integer(c_size_t), intent(in), value :: sz
            integer(c_int)                       :: nng_sub0_ctx_subscribe
        end function nng_sub0_ctx_subscribe

        ! int nng_sub0_ctx_unsubscribe(nng_ctx id, const void *buf, size_t sz)
        function nng_sub0_ctx_unsubscribe(id, buf, sz) bind(c, name='nng_sub0_ctx_unsubscribe')
            import :: c_int, c_ptr, c_size_t, nng_ctx
            implicit none
            type(nng_ctx),     intent(in), value :: id
            type(c_ptr),       intent(in), value :: buf
            integer(c_size_t), intent(in), value :: sz
            integer(c_int)                       :: nng_sub0_ctx_unsubscribe
        end function nng_sub0_ctx_unsubscribe

        ! int nng_sub0_open(nng_socket *s)
        function nng_sub0_open(s) bind(c, name='nng_sub0_open')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_sub0_open
        end function nng_sub0_open

        ! int nng_sub0_open_raw(nng_socket *s)
        function nng_sub0_open_raw(s) bind(c, name='nng_sub0_open_raw')
            import :: c_int, nng_socket
            implicit none
            type(nng_socket), intent(out) :: s
            integer(c_int)                :: nng_sub0_open_raw
        end function nng_sub0_open_raw

        ! int nng_sub0_socket_subscribe(nng_socket id, const void *buf, size_t sz)
        function nng_sub0_socket_subscribe(id, buf, sz) bind(c, name='nng_sub0_socket_subscribe')
            import :: c_int, c_ptr, c_size_t, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: id
            type(c_ptr),       intent(in), value :: buf
            integer(c_size_t), intent(in), value :: sz
            integer(c_int)                       :: nng_sub0_socket_subscribe
        end function nng_sub0_socket_subscribe

        ! int nng_sub0_socket_unsubscribe(nng_socket id, const void *buf, size_t sz)
        function nng_sub0_socket_unsubscribe(id, buf, sz) bind(c, name='nng_sub0_socket_unsubscribe')
            import :: c_int, c_ptr, c_size_t, nng_socket
            implicit none
            type(nng_socket),  intent(in), value :: id
            type(c_ptr),       intent(in), value :: buf
            integer(c_size_t), intent(in), value :: sz
            integer(c_int)                       :: nng_sub0_socket_unsubscribe
        end function nng_sub0_socket_unsubscribe
    end interface
end module nng_pubsub0
