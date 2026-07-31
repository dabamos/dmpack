! dmtestipc.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestipc
    !! Test program for IPC access.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestipc'
    integer,          parameter :: NTESTS    = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        stat = TEST_FAILED
        print '(" Version: ", a)', dm_ipc_version()
        print '(" Version: ", a)', dm_ipc_version(.true.)
        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer,      parameter :: N = 7
        character(*), parameter :: INVALIDS(N) = [ &
            character(32) ::          &
            ' ',                      &
            'abc',                    &
            'foo://bar',              &
            'tcp://192.168.0.1:ABCD', &
            'tcp://192.168.0.1:',     &
            'ipc://',                 &
            'ipc://tmp/feeds/0'       &
        ]
        character(*), parameter :: VALIDS(N) = [ &
            character(32) ::          &
            'inproc://*:5556',        &
            'inproc://dummy:5556',    &
            'tcp://192.168.0.1:5555', &
            'tcp://localhost:5555',   &
            'tcp://*:5555',           &
            'tcp://*:123',            &
            'ipc:///tmp/feeds/0'      &
        ]

        integer :: i

        stat = TEST_FAILED

        print *, 'Validating invalid socket addresses ...'

        do i = 1, N
            print '(1x, """", a, """")', trim(INVALIDS(i))
            if (dm_ipc_is_valid_address(trim(INVALIDS(i)))) return
        end do

        print *, 'Validating valid socket addresses ...'

        do i = 1, N
            print '(1x, """", a, """")', trim(VALIDS(i))
            if (.not. dm_ipc_is_valid_address(trim(VALIDS(i)))) return
        end do

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        character(*), parameter :: ADDRESS     = 'inproc://dmpack'
        ! character(*), parameter :: ADDRESS     = 'tcp://localhost:5555'
        integer(i8),  parameter :: BUFFER_SIZE = 1024_i8

        integer                :: rc
        type(buffer_type)      :: buffer1, buffer2
        type(ipc_context_type) :: context
        type(ipc_message_type) :: message1, message2
        type(ipc_socket_type)  :: pair1, pair2

        stat = TEST_FAILED

        test_block: block
            integer               :: nbytes
            type(ipc_header_type) :: header1, header2
            type(observ_type)     :: observ1, observ2
            type(timer_type)      :: timer1, timer2

            call dm_timer_start(timer1)
            print *, 'Creating IPC context ...'
            rc = dm_ipc_context_create(context)
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening socket pair1 ...'
            rc = dm_ipc_socket_open_pair(pair1, context)
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening socket pair2 ...'
            rc = dm_ipc_socket_open_pair(pair2, context)
            if (dm_is_error(rc)) exit test_block

            print *, 'Binding socket pair1 to ' // ADDRESS // ' ...'
            rc = dm_ipc_socket_bind(pair1, ADDRESS)
            if (dm_is_error(rc)) exit test_block

            print *, 'Connecting socket pair2 to ' // ADDRESS // ' ...'
            rc = dm_ipc_socket_connect(pair2, ADDRESS)
            if (dm_is_error(rc)) exit test_block

            print *, 'Packing observation ...'
            header1 = dm_ipc_header_observ(from='dmdummy1', to='dmdummy2')
            call dm_test_dummy(observ1)

            call dm_buffer_init(buffer1, BUFFER_SIZE, error=rc)
            if (dm_is_error(rc)) exit test_block

            call dm_msgpack_pack_message(buffer1, header1, observ1, error=rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating message ...'
            rc = dm_ipc_message_create(message1, buffer1)
            if (dm_is_error(rc)) exit test_block

            call dm_timer_start(timer2)
            print *, 'Sending message ...'
            rc = dm_ipc_message_send(message1, pair1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating message ...'
            rc = dm_ipc_message_create(message2)
            if (dm_is_error(rc)) exit test_block

            print *, 'Receiving message ...'
            rc = dm_ipc_message_receive(message2, pair2, nbytes=nbytes)
            print '(" #Bytes received: ", i0)', nbytes
            if (dm_is_error(rc)) exit test_block
            call dm_timer_stop(timer2)

            print *, 'Reading message ...'
            rc = dm_ipc_message_data(message2, buffer2)
            if (dm_is_error(rc)) exit test_block

            print *, 'Unpacking observation ...'
            call dm_msgpack_unpack_message(buffer2, header2, observ2)
            call dm_timer_stop(timer1)

            print *, 'Validating header ...'
            print '(72("."))'
            call dm_ipc_header_out(header1)
            print '(72("."))'
            if (.not. (header1 == header2)) exit test_block

            print *, 'Validating observation ...'
            print '(72("."))'
            call dm_observ_out(observ1)
            print '(72("."))'
            if (.not. (observ1 == observ2)) exit test_block

            print '(" Transmission time: ", f12.10, " sec")', dm_timer_result(timer2)
            print '(" Total time.......: ", f12.10, " sec")', dm_timer_result(timer1)
            print '(72("."))'

            stat = TEST_PASSED
        end block test_block

        call dm_error_out(rc)

        print *, 'Destroying buffers ...'
        call dm_buffer_destroy(buffer1)
        call dm_buffer_destroy(buffer2)

        print *, 'Destroying message1 ...'
        call dm_ipc_message_destroy(message1, error=rc)
        call dm_error_out(rc)

        print *, 'Destroying message2 ...'
        call dm_ipc_message_destroy(message2, error=rc)
        call dm_error_out(rc)

        print *, 'Closing socket pair2 ...'
        call dm_ipc_socket_close(pair2, error=rc)
        call dm_error_out(rc)

        print *, 'Closing socket pair1 ...'
        call dm_ipc_socket_close(pair1, error=rc)
        call dm_error_out(rc)

        print *, 'Destroying IPC context ...'
        call dm_ipc_context_destroy(context, error=rc)
        call dm_error_out(rc)
    end function test03
end program dmtestipc
