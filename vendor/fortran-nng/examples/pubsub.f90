! pubsub.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! Implementation of the pub/sub pattern. The program is based on the
    !! example in C listed in:
    !!
    !!     https://nanomsg.org/gettingstarted/nng/pubsub.html
    !!
    !! Example usage:
    !!
    !! ```
    !! ./pubsub server ipc:///tmp/pubsub.ipc & server=$! && sleep 1
    !! ./pubsub client ipc:///tmp/pubsub.ipc client0 & client0=$!
    !! ./pubsub client ipc:///tmp/pubsub.ipc client1 & client1=$!
    !! ./pubsub client ipc:///tmp/pubsub.ipc client2 & client2=$!
    !! sleep 15
    !! kill $server $client0 $client1 $client2
    !! ```
    use :: nng
    use :: nng_pubsub0
    use :: nng_util, only: f_c_str
    implicit none (type, external)

    character(80) :: name, type, url

    call get_command_argument(1, type)
    call get_command_argument(2, url)
    call get_command_argument(3, name)

    select case (type)
        case ('client'); call client(url, name)
        case ('server'); call server(url)
        case default;    print '("Usage: pubsub client|server <URL> <ARG> ...")'
    end select
contains
    function iso8601()
        character(*), parameter :: ISO_FMT = &
            '(i4, 2("-", i2.2), "T", 2(i0.2, ":"), i0.2, ".", i0.3, a, ":", a)'

        character(29) :: iso8601
        character(5)  :: zone
        integer       :: dt(8)

        call date_and_time(values=dt, zone=zone)
        write (iso8601, ISO_FMT) dt(1:3), dt(5:8), zone(1:3), zone(4:5)
    end function iso8601

    subroutine fatal(rc, str)
        integer,      intent(in) :: rc
        character(*), intent(in) :: str

        print '(a, ": ", a)', str, nng_strerror(rc)
        stop
    end subroutine fatal

    subroutine client(url, name)
        character(*), intent(in) :: url
        character(*), intent(in) :: name

        character(29), target :: buffer
        integer               :: rc
        integer(c_size_t)     :: sz
        type(nng_dialer)      :: dialer
        type(nng_socket)      :: socket

        rc = nng_sub0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_sub0_open')

        ! Subscribe to everything (empty means all topics). Do not set the topic
        ! with nng_socket_set_string(), instead we just set the value to an
        ! arbitrary string with target attribute and pass size 0.
        rc = nng_socket_set(socket, NNG_OPT_SUB_SUBSCRIBE, c_loc(buffer), 0_c_size_t)
        if (rc /= 0) call fatal(rc, 'nng_socket_set')

        print '("[CLIENT ", a, "] CONNECTING TO ", a)', trim(name), trim(url)

        rc = nng_dial(socket, f_c_str(url), dialer, 0)
        if (rc /= 0) call fatal(rc, 'nng_dial')

        do
            buffer = ' '
            sz = len(buffer, c_size_t)

            rc = nng_recv(socket, c_loc(buffer), sz, 0)
            if (rc /= 0) call fatal(rc, 'nng_recv')

            if (sz /= len(buffer)) then
                print '("[CLIENT] RECEIVED UNEXPECTED MESSAGE")'
                cycle
            end if

            print '("[CLIENT ", a, "] RECEIVED ", a)', trim(name), buffer
        end do

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')
    end subroutine client

    subroutine server(url)
        character(*), intent(in) :: url

        character(29), target :: date
        integer               :: rc
        type(nng_listener)    :: listener
        type(nng_socket)      :: socket

        rc = nng_pub0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_pub0_open')

        rc = nng_listen(socket, f_c_str(url), listener, 0)
        if (rc /= 0) call fatal(rc, 'nng_listen')

        do
            date = iso8601()
            print '("[SERVER] PUBLISHING DATE ", a)', date

            rc = nng_send(socket, c_loc(date), len(date, kind=c_size_t), 0)
            if (rc /= 0) call fatal(rc, 'nng_send')

            call nng_msleep(1000)
        end do

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')
    end subroutine server
end program main
