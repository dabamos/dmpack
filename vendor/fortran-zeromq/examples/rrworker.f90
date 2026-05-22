program main
    !! "Hello World" worker based on ZeroMQ Guide example `rrworker.c`:
    !!
    !!      https://zguide.zeromq.org/docs/chapter2/
    !!
    !! Connects REP socket to `tcp://localhost:5559`. Expects "Hello" from
    !! client, replies with "World".
    use :: zmq
    implicit none (type, external)

    character(*), parameter :: ADDR = 'tcp://localhost:5560'

    character(32), target :: buf
    integer               :: nb, rc
    type(c_ptr)           :: ctx, sock

    ctx  = zmq_ctx_new()
    sock = zmq_socket(ctx, ZMQ_REP)

    rc = zmq_connect(sock, ADDR)
    print '("connected to ", a)', ADDR

    do
        buf = ' '
        nb  = zmq_recv(sock, c_loc(buf), len(buf, c_size_t), 0)
        print '("received request <", a, ">")', trim(buf)

        ! Do some work.
        call zmq_sleep(1)

        buf = 'World'
        nb  = zmq_send(sock, c_loc(buf), len_trim(buf, c_size_t), 0)
        print '("sent reply <", a, ">")', trim(buf)
    end do

    rc = zmq_close(sock)
    rc = zmq_ctx_destroy(ctx)
end program main
