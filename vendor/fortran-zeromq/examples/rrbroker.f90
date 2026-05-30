program main
    !! Simple request-reply broker based on the ZeroMQ Guide example
    !! `rrbroker.c`:
    !!
    !!      https://zguide.zeromq.org/docs/chapter2/
    !!
    !! Run `rrworker`, `rrbroker`, and `rrclient` for demonstration.
    use :: zmq
    implicit none (type, external)

    character(*), parameter :: BACK_ADDR  = 'tcp://*:5560'
    character(*), parameter :: FRONT_ADDR = 'tcp://*:5559'
    character(*), parameter :: TOPIC      = 'fortran'

    integer              :: flags, rc
    integer(c_short)     :: event
    logical              :: more
    type(c_ptr)          :: ctx
    type(c_ptr)          :: back, front
    type(zmq_msg_t)      :: msg
    type(zmq_pollitem_t) :: items(2)

    ctx   = zmq_ctx_new()
    front = zmq_socket(ctx, ZMQ_ROUTER)
    back  = zmq_socket(ctx, ZMQ_DEALER)

    rc = zmq_bind(front, FRONT_ADDR)
    print '("[1] bound to ", a)', FRONT_ADDR

    rc = zmq_bind(back, BACK_ADDR)
    print '("[2] bound to ", a)', BACK_ADDR

    items = [ &
        zmq_pollitem_t(front, 0, ZMQ_POLLIN, 0), &
        zmq_pollitem_t(back,  0, ZMQ_POLLIN, 0)  &
    ]

    do
        print '("polling events ...")'
        rc  = zmq_poll(items, size(items), -1_c_long)

        event = iand(items(1)%revents, int(ZMQ_POLLIN, c_short))

        if (event == 1) then
            do
                rc   = zmq_msg_init(msg)
                rc   = zmq_msg_recv(msg, front, 0)
                more = zmq_msg_more(msg)
                print '("[1] received message")'

                flags = 0
                if (more) flags = ZMQ_SNDMORE

                rc = zmq_msg_send(msg, back, flags)
                rc = zmq_msg_close(msg)
                print '("[1] sent message")'

                if (.not. more) exit
            end do
        end if

        event = iand(items(2)%revents, int(ZMQ_POLLIN, c_short))

        if (event == 1) then
            do
                rc   = zmq_msg_init(msg)
                rc   = zmq_msg_recv(msg, back, 0)
                more = zmq_msg_more(msg)
                print '("[2] received message")'

                flags = 0
                if (more) flags = ZMQ_SNDMORE

                rc = zmq_msg_send(msg, front, flags)
                rc = zmq_msg_close(msg)
                print '("[2] sent message")'

                if (.not. more) exit
            end do
        end if
    end do

    rc = zmq_close(front)
    rc = zmq_close(back)
    rc = zmq_ctx_destroy(ctx)
end program main
