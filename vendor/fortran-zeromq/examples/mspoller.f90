program main
    !! Reading from multiple sockets. Based on the following example in the
    !! ZeroMQ Guide:
    !!
    !!     https://zguide.zeromq.org/docs/chapter2/#Handling-Multiple-Sockets
    !!
    use :: zmq
    implicit none (type, external)

    character(*), parameter :: RCV_ADDR = 'tcp://localhost:5557'
    character(*), parameter :: SUB_ADDR = 'tcp://localhost:5556'
    character(*), parameter :: TOPIC    = 'fortran'

    character(256), target :: msg
    integer                :: nb, rc
    integer(c_short)       :: event
    type(c_ptr)            :: ctx, rcv, sub
    type(zmq_pollitem_t)   :: items(2)

    ctx = zmq_ctx_new()
    rcv = zmq_socket(ctx, ZMQ_PULL)
    rc  = zmq_connect(rcv, RCV_ADDR)
    print '("[1] connected to ", a)', RCV_ADDR

    sub = zmq_socket(ctx, ZMQ_SUB)
    rc  = zmq_connect(sub, SUB_ADDR)
    print '("[2] connected to ", a)', SUB_ADDR

    rc = zmq_setsockopt(sub, ZMQ_SUBSCRIBE, TOPIC, len(TOPIC, c_size_t))
    print '("[2] subscribed topic <", a, ">")', TOPIC

    items = [ &
        zmq_pollitem_t(rcv, 0, ZMQ_POLLIN, 0), &
        zmq_pollitem_t(sub, 0, ZMQ_POLLIN, 0)  &
    ]

    do
        print '("polling events ...")'
        rc  = zmq_poll(items, size(items), -1_c_long)
        msg = ' '

        event = iand(items(1)%revents, int(ZMQ_POLLIN, c_short))

        if (event == 1) then
            nb = zmq_recv(rcv, c_loc(msg), len(msg, c_size_t), ZMQ_DONTWAIT)
            if (nb < 0) exit
            print '("[1] received ", a)', trim(msg)
        end if

        event = iand(items(2)%revents, int(ZMQ_POLLIN, c_short))

        if (event == 1) then
            nb = zmq_recv(sub, c_loc(msg), len(msg, c_size_t), ZMQ_DONTWAIT)
            if (nb < 0) exit
            print '("[2] received ", a)', trim(msg)
        end if
    end do

    rc = zmq_close(rcv)
    rc = zmq_close(sub)
    rc = zmq_ctx_destroy(ctx)
end program main
