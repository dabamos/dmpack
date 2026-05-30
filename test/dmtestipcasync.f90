! dmtestipcasync.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestipcasync
    !! Test program for asynchronous IPC through NNG sockets.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: TEST_NAME = 'dmtestipcasync'
    integer,      parameter :: NTESTS    = 1

    character(*), parameter :: TEST_URL  = 'tcp://127.0.0.1:5100'
    integer,      parameter :: MAX_TASKS = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    subroutine client(url, error)
        character(*), intent(in)  :: url
        integer,      intent(out) :: error

        integer :: rc

        ipc_block: block
            type(ipc_disco_response_type) :: response
            type(timer_type)              :: timer

            call dm_timer_start(timer)
            rc = dm_ipc_disco_send(url, response, IPC_SERVICE_RPC_OBSERV, IPC_TRANSPORT_TCP, from='client', to='server', timeout=100)
            call dm_timer_stop(timer)
            if (dm_is_error(rc)) exit ipc_block

            print '(" [CLIENT] received disco response in ", f8.6, " sec")', dm_timer_result(timer)
            call dm_ipc_disco_out(response)
        end block ipc_block

        error = rc
    end subroutine client

    subroutine server(url, runtime, error)
        character(*), intent(in)  :: url
        integer,      intent(in)  :: runtime
        integer,      intent(out) :: error

        integer                           :: i, rc
        type(ipc_async_task_type), target :: tasks(MAX_TASKS)
        type(ipc_socket_type)             :: socket

        ipc_block: block
            rc = dm_ipc_open_reply(socket)
            call dm_error_out(rc, 'dm_ipc_open_reply()')
            if (dm_is_error(rc)) exit ipc_block
            print '(" [SERVER] opened reply socket")'

            do i = 1, MAX_TASKS
                call dm_ipc_async_set_id(tasks(i), i)

                rc = dm_ipc_async_init(tasks(i), server_callback)
                call dm_error_out(rc, 'dm_ipc_async_init()', extra=.true.)
                if (dm_is_error(rc)) exit ipc_block

                rc = dm_ipc_context_open(tasks(i)%context, socket)
                call dm_error_out(rc, 'dm_ipc_context_open()', extra=.true.)
                if (dm_is_error(rc)) exit ipc_block

                print '(" [SERVER] initialized async context ", i0)', i
            end do

            rc = dm_ipc_listen(socket, url)
            call dm_error_out(rc, 'dm_ipc_listen()', extra=.true.)
            if (dm_is_error(rc)) exit ipc_block
            print '(" [SERVER] listening to ", a, " ...")', url

            do i = 1, MAX_TASKS
                call server_callback(c_loc(tasks(i)))
            end do

            print '(" [SERVER] initialized task workers")'
        end block ipc_block

        print '(" [SERVER] running for ", i0, " msec ...")', runtime
        call dm_posix_msleep(runtime) ! let the task workers run for a while

        call dm_ipc_async_destroy(tasks)
        print '(" [SERVER] destroyed task workers")'

        call dm_ipc_close(tasks%context)
        print '(" [SERVER] closed contexts")'

        call dm_ipc_close(socket)
        print '(" [SERVER] closed socket")'

        error = rc
    end subroutine server

    recursive subroutine server_callback(client_data) bind(c)
        character(*), parameter :: SERVICE_URL = 'tcp://127.0.0.1:5103'

        type(c_ptr), intent(in), value :: client_data

        integer                            :: rc
        type(ipc_async_task_type), pointer :: task
        type(ipc_disco_request_type)       :: request
        type(ipc_disco_response_type)      :: response
        type(ipc_message_header_type)      :: header

        if (.not. c_associated(client_data)) return
        call c_f_pointer(client_data, task)

        select case (task%state)
            case (IPC_ASYNC_TASK_STATE_INIT)
                print '(" [SERVER TASK ", i0, "] initialized")', task%id
                call dm_ipc_async_receive(task)
                task%state = IPC_ASYNC_TASK_STATE_RECV

            case (IPC_ASYNC_TASK_STATE_RECV)
                rc = dm_ipc_async_result(task)
                call dm_ipc_async_sleep(task%async, 0)
                call dm_ipc_async_get_message(task)

                if (dm_is_error(rc)) then
                    print '(" [SERVER TASK ", i0, "] failed to receive message: ", a)', task%id, dm_error_message(rc)
                    call dm_ipc_message_destroy(task%message)
                    task%state = IPC_ASYNC_TASK_STATE_INIT
                    return
                end if

                print '(" [SERVER TASK ", i0, "] received message")', task%id
                ! call dm_ipc_message_header_out(task%message%header)
                task%state = IPC_ASYNC_TASK_STATE_WORK

            case (IPC_ASYNC_TASK_STATE_WORK)
                rc = dm_ipc_disco_from_message(request, task%message)

                if (dm_is_error(rc)) then
                    print '(" [SERVER TASK ", i0, "] received invalid message: ", a)', task%id, dm_error_message(rc)
                    task%state = IPC_ASYNC_TASK_STATE_INIT
                    return
                end if

                header = task%message%header
                print '(" [SERVER TASK ", i0, "] received disco request from ", a)', task%id, trim(header%from)
                response = ipc_disco_response_type(request%service, request%transport, IPC_PROTOCOL_REQUEST, IPC_STATUS_OK, SERVICE_URL)
                rc = dm_ipc_disco_reply(task%message, header, response, error=rc)

                if (dm_is_error(rc)) then
                    print '(" [SERVER TASK ", i0, "] failed to prepare disco response: ", a)', task%id, dm_error_message(rc)
                    call dm_ipc_message_destroy(task%message)
                    task%state = IPC_ASYNC_TASK_STATE_INIT
                end if

                call dm_ipc_async_set_message(task)
                call dm_ipc_async_send(task)

                print '(" [SERVER TASK ", i0, "] sent message")', task%id
                task%state = IPC_ASYNC_TASK_STATE_SEND

            case (IPC_ASYNC_TASK_STATE_SEND)
                rc = dm_ipc_async_result(task)
                call dm_ipc_async_sleep(task%async, 0)

                if (dm_is_error(rc)) then
                    print '(" [SERVER TASK ", i0, "] failed to sent message: ", a)', task%id, dm_error_message(rc)
                    call dm_ipc_message_destroy(task%message)
                end if

                print '(" [SERVER TASK ", i0, "] finished")', task%id
                task%state = IPC_ASYNC_TASK_STATE_INIT

            case default
                print '(" [SERVER TASK ", i0, "] invalid task state: ", i0)', task%state
        end select
    end subroutine server_callback

    logical function test01() result(stat)
        integer :: i, pid, rc

        stat = TEST_FAILED

        print *, 'Forking ...'
        call dm_posix_fork(pid)

        if (pid < 0) then
            call dm_error_out(E_SYSTEM, 'dm_system_fork()')
            return
        else if (pid == 0) then
            print '(" [CLIENT] started client")'
            call dm_posix_msleep(100)
            i = 0

            do
                i = i + 1
                print '(" [CLIENT] sending disco request ", i0, " ...")', i
                call client(TEST_URL, rc)
                if (dm_is_ok(rc) .or. i > 9) exit
                print '(" [CLIENT] disco request ", i0, " failed: ", a)', i, dm_error_message(rc)
            end do

            print '(" [CLIENT] stopped client")'
            call dm_stop(STOP_SUCCESS)
        else
            print '(" [SERVER] started server")'
            call server(TEST_URL, 1500, rc)
            print '(" [SERVER] stopped server")'

            if (dm_is_error(rc)) then
                print '(" [SERVER] server error: ", a)', dm_error_message(rc)
                return
            end if
        end if

        stat = TEST_PASSED
    end function test01
end program dmtestipcasync
