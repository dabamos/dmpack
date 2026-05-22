program main
    !! "Hello World" client based on ZeroMQ Guide example `rrclient.c`:
    !!
    !!      https://zguide.zeromq.org/docs/chapter2/
    !!
    !! Connects REQ socket to `tcp://localhost:5559`. Sends "Hello" to server,
    !! expects "World" back.
    use :: zmq
    implicit none (type, external)

    character(*), parameter :: ADDR = 'tcp://localhost:5559'

    character(32), target :: buf
    integer               :: i, nb, rc
    type(c_ptr)           :: ctx, sock

    ctx  = zmq_ctx_new()
    sock = zmq_socket(ctx, ZMQ_REQ)

    zmq_block: block
        rc = zmq_connect(sock, ADDR)
        if (rc /= 0) exit zmq_block
        print '("connected to ", a)', ADDR

        do i = 1, 10
            buf = 'Hello'
            nb  = zmq_send(sock, c_loc(buf), len_trim(buf, c_size_t), 0)
            print '("(", i2, ") sent request <", a, ">")', i, trim(buf)

            buf = ' '
            nb  = zmq_recv(sock, c_loc(buf), len(buf, c_size_t), 0)
            print '("(", i2, ") received reply <", a, ">")', i, trim(buf)
        end do
    end block zmq_block

    rc = zmq_close(sock)
    rc = zmq_ctx_destroy(ctx)
end program main
