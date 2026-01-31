! dmtestipcasync.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestipcasync
    !! Test program for IPC through NNG sockets.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: TEST_NAME = 'dmtestipcasync'
    integer,      parameter :: NTESTS    = 1

    character(*), parameter :: URL       = 'tcp://127.0.0.1:5555'
    integer,      parameter :: MAX_TASKS = 4

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    subroutine client(socket)
        type(ipc_socket_type), intent(inout) :: socket

        integer                       :: i, rc
        type(ipc_disco_request_type)  :: request
        type(ipc_disco_response_type) :: response
        type(ipc_message_type)        :: message

        rc = dm_ipc_open_request(socket)
        call dm_error_out(rc, 'dm_ipc_open_request()')
        if (dm_is_error(rc)) return
        print '(" [CLIENT] opened request socket")'

        rc = dm_ipc_dial(socket, URL)
        call dm_error_out(rc, 'dm_ipc_dial()')
        if (dm_is_error(rc)) return
        print '(" [CLIENT] dialed ", a)', URL

        request%protocol = IPC_PROTOCOL_REQUEST
        request%service  = IPC_SERVICE_STORAGE_OBSERV

        rc = dm_ipc_message_create(message, request, from='client', to='server')
        call dm_error_out(rc, 'dm_ipc_message_create()')
        if (dm_is_error(rc)) return
        print '(" [CLIENT] created request ", i0)', i

        rc = dm_ipc_message_send(message, socket)
        call dm_error_out(rc, 'dm_ipc_message_send()')
        if (dm_is_error(rc)) return
        print '(" [CLIENT] sent disco request ", i0)', i

        rc = dm_ipc_message_receive(message, socket)
        call dm_error_out(rc, 'dm_ipc_message_receive()')
        if (dm_is_error(rc)) return

        call dm_ipc_message_header_out(message%header)

        if (message%header%type == IPC_MESSAGE_TYPE_DISCO_RESPONSE) then
            print '(" [CLIENT] received disco response from ", a)', trim(message%header%from)
            rc = dm_ipc_message_body(message, response)
            call dm_error_out(rc, 'dm_ipc_message_body()')
            if (response%url /= URL) call dm_error_out(E_INVALID, 'invalid response')
        else
            call dm_error_out(E_TYPE, 'dm_ipc_message_receive()')
        end if

        call dm_ipc_message_destroy(message)
    end subroutine client

    subroutine server(socket)
        type(ipc_socket_type), intent(inout) :: socket

        integer                           :: i, rc
        type(ipc_async_task_type), target :: tasks(MAX_TASKS)

        rc = dm_ipc_open_reply(socket)
        call dm_error_out(rc, 'dm_ipc_open_reply()')
        if (dm_is_error(rc)) return
        print '(" [SERVER] opened reply socket")'

        do i = 1, MAX_TASKS
            rc = dm_ipc_async_init(tasks(i), server_callback, i)
            call dm_error_out(rc, 'dm_ipc_async_init()')
            if (dm_is_error(rc)) return

            rc = dm_ipc_context_open(tasks(i)%context, socket)
            call dm_error_out(rc, 'dm_ipc_context_open()')
            if (dm_is_error(rc)) return

            print '(" [SERVER] initialised async context ", i0)', i
        end do

        rc = dm_ipc_listen(socket, URL)
        call dm_error_out(rc, 'dm_ipc_listen()')
        if (dm_is_error(rc)) return
        print '(" [SERVER] listening to ", a, " ...")', URL

        do i = 1, MAX_TASKS
            call server_callback(c_loc(tasks(i)))
        end do

        print '(" [SERVER] initialised task workers")'

        call dm_msleep(2000)
        call dm_ipc_async_destroy(tasks)

        print '(" [SERVER] destroyed task workers")'
    end subroutine server

    recursive subroutine server_callback(client_data) bind(c)
        type(c_ptr), intent(in), value :: client_data

        character(IPC_MESSAGE_HEADER_ID_LEN) :: id
        integer                              :: rc
        type(ipc_async_task_type), pointer   :: task
        type(ipc_message_type)               :: message
        type(ipc_disco_request_type)         :: request
        type(ipc_disco_response_type)        :: response

        if (.not. c_associated(client_data)) return
        call c_f_pointer(client_data, task)

        select case (task%state)
            case (IPC_ASYNC_TASK_STATE_INIT)
                call dm_ipc_async_receive(task)
                call dm_ipc_async_set_state(task, IPC_ASYNC_TASK_STATE_RECV)
                print '(" [SERVER TASK ", i0, "] initialised")', task%id

            case (IPC_ASYNC_TASK_STATE_RECV)
                rc = dm_ipc_async_result(task)
                if (dm_is_error(rc)) return ! server has been shutdown

                print '(" [SERVER TASK ", i0, "] received message")', task%id
                call dm_ipc_async_get_message(task, message)

                rc = dm_ipc_message_header(message)
                call dm_error_out(rc, 'dm_ipc_message_header()')
                if (dm_is_error(rc)) return

                call dm_ipc_message_header_out(message%header)

                if (message%header%type == IPC_MESSAGE_TYPE_DISCO_REQUEST) then
                    print '(" [SERVER TASK ", i0, "] received disco request from ", a)', task%id, trim(message%header%from)
                    rc = dm_ipc_message_body(message, request)
                    call dm_error_out(rc, 'dm_ipc_message_body()')
                else
                    rc = E_NOT_SUPPORTED
                    print '(" [SERVER TASK ", i0, "] received unsupported message")', task%id
                end if

                call dm_ipc_message_destroy(message, header=.false.)

                if (dm_is_error(rc)) then
                    call dm_ipc_async_set_state(task, IPC_ASYNC_TASK_STATE_INIT)
                    return
                end if

                task%message = message
                call dm_ipc_async_msleep(task%async, 10)
                call dm_ipc_async_set_state(task, IPC_ASYNC_TASK_STATE_WAIT)

            case (IPC_ASYNC_TASK_STATE_WAIT)
                id = task%message%header%from
                print '(" [SERVER TASK ", i0, "] preparing disco response for ", a, " ...")', task%id, trim(id)
                response = ipc_disco_response_type(IPC_PROTOCOL_REQUEST, IPC_TRANSPORT_TCP, IPC_SERVICE_STORAGE_OBSERV, IPC_STATUS_OK, URL)
                rc = dm_ipc_message_create(task%message, response, from='server', to=id)
                call dm_error_out(rc, 'dm_ipc_message_create()')

                call dm_ipc_async_set_message(task, task%message)
                call dm_ipc_async_send(task)

                call dm_ipc_async_set_state(task, IPC_ASYNC_TASK_STATE_SEND)
                print '(" [SERVER TASK ", i0, "] sent message")', task%id

            case (IPC_ASYNC_TASK_STATE_SEND)
                rc = dm_ipc_async_result(task)

                if (dm_is_error(rc)) then
                    call dm_error_out(rc, 'dm_ipc_async_result()')
                    call dm_ipc_message_destroy(task%message)
                    return
                end if

                call dm_ipc_async_set_state(task, IPC_ASYNC_TASK_STATE_INIT)
                print '(" [SERVER TASK ", i0, "] finished")', task%id

            case default
                call dm_error_out(E_INVALID, 'server_async_callback()')
        end select
    end subroutine server_callback

    logical function test01() result(stat)
        integer               :: pid
        type(ipc_socket_type) :: socket1, socket2

        stat = TEST_FAILED

        print *, 'Forking ...'
        call dm_posix_fork(pid)

        if (pid < 0) then
            call dm_error_out(E_SYSTEM, 'dm_system_fork()')
            return
        else if (pid == 0) then
            print *, 'Creating client ...'
            call dm_msleep(500)
            call client(socket1)
            call dm_ipc_close(socket1)
            call dm_stop(STOP_SUCCESS)
        else
            print *, 'Creating server ...'
            call server(socket2)
            call dm_ipc_close(socket2)
        end if

        stat = TEST_PASSED
    end function test01
end program dmtestipcasync
