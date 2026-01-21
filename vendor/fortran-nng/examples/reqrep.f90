! reqrep.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! This is just a simple REQ/REP demonstration application. It is derived
    !! from the legacy nanomsg demonstration program of the same name, written
    !! by Tim Dysinger, but updated for nng. It has also been updated it to pass
    !! simpler binary data rather than strings over the network.
    !!
    !! The program implements a simple RPC style service, which just returns the
    !! elapsed milliseconds since some arbitrary time in the past.
    !!
    !! Based on the NNG demo program `reqrep.c`.
    use :: nng
    use :: nng_reqrep0
    use :: nng_util, only: f_c_str
    implicit none (type, external)

    character(*), parameter :: DEFAULT_URL = 'tcp://localhost:3327'
    integer,      parameter :: CMD_CLOCK   = 1

    character(8) :: arg

    call get_command_argument(1, arg)

    select case (arg)
        case ('client'); call client(DEFAULT_URL)
        case ('server'); call server(DEFAULT_URL)
        case default;    print '("Usage: reqrep client|server")'
    end select

    call nng_fini()
contains
    subroutine fatal(name, rc)
        character(*), intent(in) :: name
        integer,      intent(in) :: rc

        print '(a, ": ", a)', name, nng_strerror(rc)
        call nng_fini()
        stop
    end subroutine fatal

    subroutine client(url)
        character(*), intent(in) :: url

        integer           :: rc
        integer(nng_time) :: now
        type(nng_socket)  :: socket
        type(nng_dialer)  :: dialer
        type(c_ptr)       :: msg

        rc = nng_req0_open(socket)
        if (rc /= 0) call fatal('nng_req0_open', rc)

        rc = nng_dialer_create(dialer, socket, f_c_str(url))
        if (rc /= 0) call fatal('nng_dialer_create', rc)

        rc = nng_socket_set_ms(socket, NNG_OPT_REQ_RESENDTIME, 2000)
        rc = nng_dialer_start(dialer, NNG_FLAG_NONBLOCK)

        do
            rc = nng_msg_alloc(msg, 0_c_size_t)
            if (rc /= 0) call fatal('nng_msg_alloc', rc)

            rc = nng_msg_append_u64(msg, int(CMD_CLOCK, c_uint64_t))
            if (rc /= 0) call fatal('nng_msg_append_u64', rc)

            print '("[CLIENT] sending clock request")'

            rc = nng_sendmsg(socket, msg, 0)
            if (rc /= 0) call fatal('nng_sendmsg', rc)

            rc = nng_recvmsg(socket, msg, 0)
            if (rc /= 0) call fatal('nng_recvmsg', rc)

            rc = nng_msg_trim_u64(msg, now)
            if (rc /= 0) call fatal('nng_msg_trim_u64', rc)

            call nng_msg_free(msg)

            print '("[CLIENT] received clock: ", i0)', now
            call nng_msleep(1000)
        end do

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal('nng_socket_close', rc)
    end subroutine client

    subroutine server(url)
        character(*), intent(in) :: url

        integer             :: rc
        integer(c_uint64_t) :: now, val
        type(nng_socket)    :: socket
        type(nng_listener)  :: listener
        type(c_ptr)         :: msg

        rc = nng_rep0_open(socket)
        if (rc /= 0) call fatal('nng_rep0_open', rc)

        rc = nng_listener_create(listener, socket, f_c_str(url))
        if (rc /= 0) call fatal('nng_listener_create', rc)

        rc = nng_socket_set_ms(socket, NNG_OPT_REQ_RESENDTIME, 2000)
        rc = nng_listener_start(listener, 0)

        do
            rc = nng_recvmsg(socket, msg, 0)
            if (rc /= 0) call fatal('nng_recvmsg', rc)

            rc = nng_msg_trim_u64(msg, val)

            if (rc == 0 .and. val == CMD_CLOCK) then
                print '("[SERVER] received clock request")'

                now = nng_clock() ! Elapsed msecs since some arbitrary time in the past.

                rc = nng_msg_append_u64(msg, now)
                if (rc /= 0) call fatal('nng_msg_append_u64', rc)

                print '("[SERVER] sending clock: ", i0)', now
                rc = nng_sendmsg(socket, msg, 0)
                if (rc /= 0) call fatal('nng_sendmsg', rc)
            else
                call nng_msg_free(msg)
            end if
        end do

        rc = nng_listener_close(listener)
        if (rc /= 0) call fatal('nng_listener_close', rc)

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal('nng_socket_close', rc)
    end subroutine server
end program main
