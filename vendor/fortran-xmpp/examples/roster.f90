! roster.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! Example program that prints the contact list (roster) of the user, based
    !! on the official example `roster.c`.
    use, intrinsic :: iso_c_binding
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

    ! Set connection flags.
    flags = 0
    if (TLS_REQUIRED) flags = ior(flags, XMPP_CONN_FLAG_MANDATORY_TLS)
    if (TLS_TRUSTED)  flags = ior(flags, XMPP_CONN_FLAG_TRUST_TLS)

    ! Initialise libstrophe.
    call xmpp_initialize()

    ! Create new connection and set flags.
    ctx  = xmpp_ctx_new(c_null_ptr, c_null_ptr)
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

        integer     :: stat
        type(c_ptr) :: ctx, iq, query

        ctx = user_data

        if (event == XMPP_CONN_CONNECT) then
            print '(">>>> DEBUG Connected.")'

            ! Create iq stanza for request.
            iq    = xmpp_iq_new(ctx, 'get', 'roster1')
            query = xmpp_stanza_new(ctx)

            stat = xmpp_stanza_set_name(query, 'query')
            stat = xmpp_stanza_set_ns(query, XMPP_NS_ROSTER)
            stat = xmpp_stanza_add_child(iq, query)
            stat = xmpp_stanza_release(query)

            ! Set up reply handler.
            call xmpp_id_handler_add(conn, reply_handler, 'roster1', ctx)

            ! Send out the stanza.
            call xmpp_send(conn, iq)

            ! Release the stanza.
            stat = xmpp_stanza_release(iq)
        else
            print '(">>>> DEBUG Disconnected.")'
            call xmpp_stop(ctx)
        end if
    end subroutine conn_handler

    function reply_handler(conn, stanza, user_data) bind(c)
        !! C-interoperable reply handler of abstract interface `xmpp_handler()`.
        type(c_ptr), intent(in), value :: conn          !! xmpp_conn_t *
        type(c_ptr), intent(in), value :: stanza        !! xmpp_stanza_t *
        type(c_ptr), intent(in), value :: user_data     !! void *
        integer(kind=c_int)            :: reply_handler !! int

        character(len=:), allocatable :: jid, name, sub, type
        type(c_ptr)                   :: item, query

        type = xmpp_stanza_get_type(stanza)

        if (type == 'error') then
            print '(">>>> ERROR Query failed.")'
        else
            print '("ROSTER LIST")'

            query = xmpp_stanza_get_child_by_name(stanza, 'query')
            item  = xmpp_stanza_get_children(query)

            do while (c_associated(item))
                name = xmpp_stanza_get_attribute(item, 'name')
                jid  = xmpp_stanza_get_attribute(item, 'jid')
                sub  = xmpp_stanza_get_attribute(item, 'subscription')

                if (len(name) > 0) then
                    print '(a, " (", a, ") sub=", a)', name, jid, sub
                else
                    print '(a, " sub=", a)', jid, sub
                end if

                item = xmpp_stanza_get_next(item)
            end do

            print '("END OF LIST")'
        end if

        call xmpp_disconnect(conn)

        reply_handler = 0
    end function reply_handler
end program main
