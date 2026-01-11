! async.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! This program serves as an example for how to write an async RPC service,
    !! using the request/reply pattern and contexts (_nng_ctx(5)_). The server
    !! allocates a number of contexts up front, which determines the amount of
    !! parallelism possible. The callbacks are handled asynchronously, so this
    !! could be done by threads, or something similar. For our uses we make use
    !! of an event driven architecture that we already have available.
    !!
    !! Our demonstration application layer protocol is simple. The client sends
    !! a number of milliseconds to wait before responding. The server just
    !! gives back an empty reply after waiting that long.
    !!
    !! To run this program, start the server as `async server <url>` Then
    !! connect to it with the client as `async client <url> <msec>`.
    !!
    !! Example usage:
    !!
    !! ```
    !! ./async server tcp://127.0.0.1:5555 &
    !! ./async client tcp://127.0.0.1:5555 323
    !! ```
    use :: nng
    use :: nng_reqrep0
    use :: nng_util, only: f_c_str
    implicit none (type, external)

    ! MAX_PARELLEL is the maximum number of outstanding requests we can handle.
    ! This is *NOT* the number of threads in use, but instead represents
    ! outstanding work items. Select a small number to reduce memory size.
    ! (Each one of these can be thought of as a request-reply loop.) Note that
    ! you will probably run into limitations on the number of open file
    ! descriptors if you set this too high. (If not for that limit, this could
    ! be set in the thousands, each context consumes a couple of KB.)
    integer, parameter :: MAX_PARALLEL = 128

    integer, parameter :: STATE_INIT = 0
    integer, parameter :: STATE_RECV = 1
    integer, parameter :: STATE_WAIT = 2
    integer, parameter :: STATE_SEND = 3

    ! The server keeps a list of work items, sorted by expiration time, so that
    ! we can use this to set the timeout to the correct value for use in poll.
    type :: work_type
        integer       :: state = STATE_INIT
        type(c_ptr)   :: aio   = c_null_ptr
        type(c_ptr)   :: msg   = c_null_ptr
        type(nng_ctx) :: ctx
    end type work_type

    character(80) :: type, url, arg
    integer       :: msec, stat

    call get_command_argument(1, type)
    call get_command_argument(2, url)
    call get_command_argument(3, arg)

    read (arg, *, iostat=stat) msec

    select case (type)
        case ('client'); call client(url, msec)
        case ('server'); call server(url)
        case default;    print '("Usage: async client|server <URL> <ARG> ...")'
    end select
contains
    subroutine fatal(rc, str)
        integer,      intent(in) :: rc
        character(*), intent(in) :: str

        print '(a, ": ", a)', str, nng_strerror(rc)
        stop
    end subroutine fatal

    subroutine client(url, msec)
        character(*), intent(in) :: url
        integer,      intent(in) :: msec

        integer           :: rc
        integer(nng_time) :: t1, t2
        type(c_ptr)       :: msg
        type(nng_dialer)  :: dialer
        type(nng_socket)  :: socket

        rc = nng_req0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_req0_open')

        rc = nng_dial(socket, f_c_str(url), dialer, 0)
        if (rc /= 0) call fatal(rc, 'nng_dial')

        t1 = nng_clock()

        rc = nng_msg_alloc(msg, 0_c_size_t)
        if (rc /= 0) call fatal(rc, 'nng_msg_alloc')

        rc = nng_msg_append_u32(msg, int(msec, c_uint32_t))
        if (rc /= 0) call fatal(rc, 'nng_msg_append_u32')

        rc = nng_sendmsg(socket, msg, 0)
        if (rc /= 0) call fatal(rc, 'nng_sendmsg')

        rc = nng_recvmsg(socket, msg, 0)
        if (rc /= 0) call fatal(rc, 'nng_recvmsg')

        t2 = nng_clock()
        call nng_msg_free(msg)

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')

        print '("Request took ", i0, " msecs.")', t2 - t1
    end subroutine client

    subroutine init_work(work, socket)
        type(work_type), target, intent(inout) :: work
        type(nng_socket),        intent(inout) :: socket

        integer :: rc

        rc = nng_aio_alloc(work%aio, c_funloc(server_callback), c_loc(work))
        if (rc /= 0) call fatal(rc, 'nng_aio_alloc')

        rc = nng_ctx_open(work%ctx, socket)
        if (rc /= 0) call fatal(rc, 'nng_ctx_open')
    end subroutine init_work

    subroutine server(url)
        character(*), intent(in) :: url

        integer                 :: i, rc
        type(nng_listener)      :: listener
        type(nng_socket)        :: socket
        type(work_type), target :: works(MAX_PARALLEL)

        rc = nng_rep0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_rep0_open')

        do i = 1, MAX_PARALLEL
            ! Initialise workers.
            call init_work(works(i), socket)
        end do

        rc = nng_listen(socket, f_c_str(url), listener, 0)
        if (rc /= 0) call fatal(rc, 'nng_listen')

        do i = 1, size(works)
            ! Start workers in state STATE_INIT.
            call server_callback(c_loc(works(i)))
        end do

        do
            ! Run forever.
            call nng_msleep(3600000)
        end do

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')
    end subroutine server

    subroutine server_callback(client_data) bind(c)
        type(c_ptr), intent(in), value :: client_data

        integer                  :: rc
        integer(c_uint32_t)      :: when
        type(c_ptr)              :: msg
        type(work_type), pointer :: work

        if (.not. c_associated(client_data)) return
        call c_f_pointer(client_data, work)

        ! State machine.
        select case (work%state)
            case (STATE_INIT)
                work%state = STATE_RECV
                call nng_ctx_recv(work%ctx, work%aio)

            case (STATE_RECV)
                rc = nng_aio_result(work%aio)
                if (rc /= 0) call fatal(rc, 'nng_ctx_recv')

                msg = nng_aio_get_msg(work%aio)
                rc  = nng_msg_trim_u32(msg, when)

                if (rc /= 0) then
                    ! Ignore bad messages.
                    call nng_msg_free(msg)
                    call nng_ctx_recv(work%ctx, work%aio)
                    return
                end if

                work%msg   = msg
                work%state = STATE_WAIT
                call nng_sleep_aio(when, work%aio)

            case (STATE_WAIT)
                ! We could add more data to the message here.
                call nng_aio_set_msg(work%aio, work%msg)
                work%msg   = c_null_ptr
                work%state = STATE_SEND
                call nng_ctx_send(work%ctx, work%aio)

            case (STATE_SEND)
                rc = nng_aio_result(work%aio)

                if (rc /= 0) then
                    call nng_msg_free(work%msg)
                    call fatal(rc, 'nng_aio_result')
                end if

                work%state = STATE_RECV
                call nng_ctx_recv(work%ctx, work%aio)

            case default
                call fatal(NNG_ESTATE, 'server_callback')
        end select
    end subroutine server_callback
end program main
