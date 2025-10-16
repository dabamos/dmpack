! basic.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! Basic libstrophe program that connects to an XMPP server, based on the
    !! official example `basic.c`.
    use :: xmpp
    implicit none (type, external)

    character(len=*), parameter :: JID      = 'user@example.com'
    character(len=*), parameter :: PASSWORD = 'secret'
    character(len=*), parameter :: HOST     = 'example.com'
    integer,          parameter :: PORT     = 5222

    logical, parameter :: TLS_REQUIRED = .true.
    logical, parameter :: TLS_TRUSTED  = .true.

    integer              :: stat
    integer(kind=c_long) :: flags

    type(c_ptr) :: ctx
    type(c_ptr) :: conn
    type(c_ptr) :: log

    ! Set connection flags.
    flags = 0
    if (TLS_REQUIRED) flags = ior(flags, XMPP_CONN_FLAG_MANDATORY_TLS)
    if (TLS_TRUSTED)  flags = ior(flags, XMPP_CONN_FLAG_TRUST_TLS)

    ! Initialise libstrophe.
    call xmpp_initialize()

    ! Set log level, create new connection, and set flags.
    log  = xmpp_get_default_logger(XMPP_LEVEL_DEBUG)
    ctx  = xmpp_ctx_new(c_null_ptr, log)
    conn = xmpp_conn_new(ctx)
    stat = xmpp_conn_set_flags(conn, flags)

    ! Set credentials.
    call xmpp_conn_set_jid(conn, JID)
    call xmpp_conn_set_pass(conn, PASSWORD)

    ! Connect to XMPP server.
    stat = xmpp_connect_client(conn, HOST, PORT, conn_handler, ctx)
    if (stat == XMPP_EOK) call xmpp_run(ctx)

    ! Clean-up.
    stat = xmpp_conn_release(conn)
    call xmpp_ctx_free(ctx)
    call xmpp_shutdown()
contains
    subroutine conn_handler(conn, event, error, stream_error, user_data) bind(c)
        !! C-interoperable connection handler of abstract interface `xmpp_conn_handler()`.
        type(c_ptr),               intent(in), value :: conn         !! xmpp_conn_t *
        integer(kind=c_int),       intent(in), value :: event        !! xmpp_conn_event_t
        integer(kind=c_int),       intent(in), value :: error        !! int
        type(xmpp_stream_error_t), intent(in)        :: stream_error !! xmpp_stream_error_t *
        type(c_ptr),               intent(in), value :: user_data    !! void *

        type(c_ptr) :: ctx

        ctx = user_data

        if (event == XMPP_CONN_CONNECT) then
            print '(">>>> DEBUG Connected.")'
            call xmpp_disconnect(conn)
        else
            print '(">>>> DEBUG Disconnected.")'
            call xmpp_stop(ctx)
        end if
    end subroutine conn_handler
end program main
