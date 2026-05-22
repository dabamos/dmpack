program main
    !! Example of a REP server, using zsock and zmsg.
    use :: czmq
    implicit none (type, external)

    character(*), parameter :: ADDR = 'tcp://localhost:5555'

    integer     :: rc
    type(c_ptr) :: rep, req, sock

    sock = zsock_new_rep(ADDR)

    if (.not. c_associated(sock)) then
        print '("failed to create REP socket")'
        stop
    end if

    print '("server waiting on port 5555 ...")'

    do
        req = zmsg_recv(sock)
        if (.not. c_associated(req)) exit

        print '("received: ", a)', zmsg_popstr(req)
        call zmsg_destroy(req)

        rep = zmsg_new()
        rc  = zmsg_addstr(rep, 'World')
        rc  = zmsg_send(rep, sock)
    end do

    call zsock_destroy(sock)
end program main
