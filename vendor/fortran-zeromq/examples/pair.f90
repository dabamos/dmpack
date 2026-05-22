program main
    !! Compile this program with option `-fopenmp`.
    use, intrinsic :: omp_lib
    use :: zmq
    implicit none (type, external)

    character(*), parameter :: ADDR = 'inproc://fortran'

    integer     :: major, minor, patch, rc
    type(c_ptr) :: ctx

    call omp_set_dynamic(.false.)
    call omp_set_num_threads(2)

    call zmq_version(major, minor, patch)
    print '("ZeroMQ ", 2(i0, "."), i0)', major, minor, patch

    ctx = zmq_ctx_new()

    !$omp parallel shared(ctx)
    !$omp sections
    !$omp section
        call node1(ctx)
    !$omp section
        call node2(ctx)
    !$omp end sections
    !$omp end parallel

    rc = zmq_ctx_destroy(ctx)
contains
    subroutine node1(ctx)
        type(c_ptr), intent(in) :: ctx

        character(4)    :: data
        integer         :: nbytes, rc
        type(c_ptr)     :: socket
        type(zmq_msg_t) :: msg

        data   = 'PING'
        socket = zmq_socket(ctx, ZMQ_PAIR)
        print '("[1] created socket")'

        zmq_block: block
            rc = zmq_connect(socket, ADDR)
            if (rc /= 0) exit zmq_block
            print '("[1] connected to ", a)', ADDR

            call zmq_sleep(1) ! wait for peer

            rc = zmq_msg_init_data(msg, data, len(data, c_size_t))
            if (rc /= 0) exit zmq_block

            nbytes = zmq_msg_send(msg, socket, 0)
            print '("[1] sent message: ", a, " (", i0, ")")', trim(data), nbytes
        end block zmq_block

        if (rc /= 0) print '("[1] error ", i0)', zmq_errno()
        rc = zmq_close(socket)
    end subroutine node1

    subroutine node2(ctx)
        type(c_ptr), intent(in) :: ctx

        character(4)          :: data
        character(:), pointer :: data_ptr
        integer               :: nbytes, rc
        type(c_ptr)           :: ptr, socket
        type(zmq_msg_t)       :: msg

        socket = zmq_socket(ctx, ZMQ_PAIR)
        print '("[2] created socket")'

        zmq_block: block
            rc = zmq_bind(socket, ADDR)
            if (rc /= 0) exit zmq_block
            print '("[2] bound to ", a)', ADDR

            rc = zmq_msg_init(msg)
            if (rc /= 0) exit zmq_block

            nbytes = zmq_msg_recv(msg, socket, 0)
            if (nbytes /= 4) exit zmq_block

            ptr = zmq_msg_data(msg)
            if (.not. c_associated(ptr)) exit zmq_block

            call c_f_pointer(ptr, data_ptr)
            data = data_ptr(1:nbytes)
            print '("[2] received message: ", a, " (", i0, ")")', data, nbytes
        end block zmq_block

        if (rc /= 0) print '("[2] error ", i0)', zmq_errno()
        rc = zmq_msg_close(msg)
        rc = zmq_close(socket)
    end subroutine node2
end program main
