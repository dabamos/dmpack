program main
    !! Example of a REQ client, using zsock.
    use :: czmq
    implicit none (type, external)

    character(*), parameter :: ADDR = 'tcp://localhost:5555'

    character(:), allocatable :: msg
    integer                   :: rc
    type(c_ptr)               :: req

    req = zsock_new_req(ADDR)

    if (.not. c_associated(req)) then
        print '("failed to create REQ socket")'
        stop
    end if

    print '("sending request ...")'

    rc  = zstr_send(req, 'Hello')
    msg = zstr_recv(req)

    if (len(msg) > 0) then
        print '("received reply: ", a)', msg
    end if

    call zsock_destroy(req)
end program main
