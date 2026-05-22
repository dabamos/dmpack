program main
    !! Example of a REP server, using CZMQ.
    use :: czmq
    implicit none (type, external)

    character(*), parameter :: ADDR = 'tcp://localhost:5555'

    character(:), allocatable :: msg
    integer                   :: rc
    type(c_ptr)               :: rep

    rep = zsock_new_rep(ADDR)

    if (.not. c_associated(rep)) then
        print '("failed to create REP socket")'
        stop
    end if

    print '("server waiting on port 5555 ...")'

    do
        msg = zstr_recv(rep)
        if (len(msg) == 0) exit
        print '("received: ", a)', msg

        ! Do some work.
        call zclock_sleep(1000)

        rc = zstr_send(rep, 'World')
    end do

    call zsock_destroy(rep)
end program main
