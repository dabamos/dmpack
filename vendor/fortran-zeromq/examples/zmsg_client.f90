program main
    !! Example of a REQ client, using zsock and zmsg.
    use :: czmq
    implicit none (type, external)

    character(*), parameter :: ADDR = 'tcp://localhost:5555'

    integer     :: rc
    type(c_ptr) :: rep, req, sock

    sock = zsock_new_req(ADDR)

    if (.not. c_associated(sock)) then
        print '("failed to create REQ socket")'
        stop
    end if

    req = zmsg_new()
    rc  = zmsg_addstr(req, 'Hello')

    print '("sending request ...")'
    rc = zmsg_send(req, sock)

    print '("waiting for reply ...")'
    rep = zmsg_recv(sock)

    if (c_associated(rep)) then
        print '("received reply: ", a)', zmsg_popstr(rep)
        call zmsg_destroy(rep)
    end if

    call zsock_destroy(sock)
end program main
