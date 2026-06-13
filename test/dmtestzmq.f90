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
        character(*), parameter :: ADDR = 'inproc://dmpack'

        integer                :: rc
        type(zmq_context_type) :: context
        type(zmq_socket_type)  :: pair1, pair2

        stat = TEST_FAILED

        test_block: block
            print *, 'Creating ZeroMQ context ...'
            rc = dm_zmq_context_create(context)
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening socket pair1 ...'
            rc = dm_zmq_socket_open_pair(pair1, context)
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening socket pair2 ...'
            rc = dm_zmq_socket_open_pair(pair2, context)
            if (dm_is_error(rc)) exit test_block

            print *, 'Binding socket pair1 to ' // ADDR // ' ...'
            rc = dm_zmq_socket_bind(pair1, ADDR)
            if (dm_is_error(rc)) exit test_block

            print *, 'Connecting socket pair2 to ' // ADDR // ' ...'
            rc = dm_zmq_socket_connect(pair2, ADDR)
            if (dm_is_error(rc)) exit test_block

            stat = TEST_PASSED
        end block test_block

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
