! dmtestzmq.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestzmq
    !! Test program for ZeroMQ access.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestzmq'
    integer,          parameter :: NTESTS    = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        stat = TEST_FAILED
        print '(" Version: ", a)', dm_zmq_version()
        print '(" Version: ", a)', dm_zmq_version(.true.)
        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(*), parameter :: ADDRESS     = 'inproc://dmpack'
        integer(i8),  parameter :: BUFFER_SIZE = 1024_i8

        integer                   :: rc
        type(buffer_type)         :: buffer1, buffer2
        type(message_header_type) :: header1, header2
        type(observ_type)         :: observ1, observ2
        type(zmq_context_type)    :: context
        type(zmq_message_type)    :: message1, message2
        type(zmq_socket_type)     :: pair1, pair2

        stat = TEST_FAILED

        test_block: block
            integer :: nbytes

            print *, 'Creating ZeroMQ context ...'
            rc = dm_zmq_context_create(context)
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening socket pair1 ...'
            rc = dm_zmq_socket_open_pair(pair1, context)
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening socket pair2 ...'
            rc = dm_zmq_socket_open_pair(pair2, context)
            if (dm_is_error(rc)) exit test_block

            print *, 'Binding socket pair1 to ' // ADDRESS // ' ...'
            rc = dm_zmq_socket_bind(pair1, ADDRESS)
            if (dm_is_error(rc)) exit test_block

            print *, 'Connecting socket pair2 to ' // ADDRESS // ' ...'
            rc = dm_zmq_socket_connect(pair2, ADDRESS)
            if (dm_is_error(rc)) exit test_block

            print *, 'Packing observation ...'
            header1 = dm_message_header_observ(from='dmdummy1', to='dmdummy2')
            call dm_test_dummy(observ1)

            call dm_buffer_init(buffer1, BUFFER_SIZE, error=rc)
            if (dm_is_error(rc)) exit test_block

            call dm_msgpack_pack_message(buffer1, header1, observ1, error=rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating message ...'
            rc = dm_zmq_message_create(message1, buffer1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Sending message ...'
            rc = dm_zmq_message_send(message1, pair1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Receiving message ...'
            rc = dm_zmq_message_receive(message2, pair2, nbytes=nbytes)
            print '(" #Bytes received: ", i0)', nbytes
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading message ...'
            rc = dm_zmq_message_data(message2, buffer2)
            if (dm_is_error(rc)) exit test_block

            print *, 'Unpacking observation ...'
            call dm_msgpack_unpack_message(buffer2, header2, observ2)

            print *, 'Validating header ...'
            print '(72("."))'
            call dm_message_header_out(header1)
            print '(72("."))'
            if (.not. (header1 == header2)) exit test_block

            print *, 'Validating observation ...'
            print '(72("."))'
            call dm_observ_out(observ1)
            print '(72("."))'
            if (.not. (observ1 == observ2)) exit test_block

            stat = TEST_PASSED
        end block test_block

        call dm_error_out(rc)

        print *, 'Destroying buffers ...'
        call dm_buffer_destroy(buffer1)
        call dm_buffer_destroy(buffer2)

        print *, 'Destroying message1 ...'
        rc = dm_zmq_message_destroy(message1)
        call dm_error_out(rc)

        print *, 'Destroying message2 ...'
        rc = dm_zmq_message_destroy(message2)
        call dm_error_out(rc)

        print *, 'Closing socket pair2 ...'
        rc = dm_zmq_socket_close(pair2)
        call dm_error_out(rc)

        print *, 'Closing socket pair1 ...'
        rc = dm_zmq_socket_close(pair1)
        call dm_error_out(rc)

        print *, 'Destroying ZeroMQ context ...'
        rc = dm_zmq_context_destroy(context)
        call dm_error_out(rc)
    end function test02
end program dmtestzmq
