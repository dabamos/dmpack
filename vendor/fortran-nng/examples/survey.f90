! survey.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! Implementation of the surveyor pattern that is used to send a timed
    !! survey out, responses are individually returned until the survey has
    !! expired. This pattern is useful for service discovery and voting
    !! algorithms. The program is based on the example in C listed in:
    !!
    !!     https://nanomsg.org/gettingstarted/nng/survey.html
    !!
    !! Example usage:
    !!
    !! ```
    !! ./survey server ipc:///tmp/survey.ipc & server=$!
    !! ./survey client ipc:///tmp/survey.ipc client0 & client0=$!
    !! ./survey client ipc:///tmp/survey.ipc client1 & client1=$!
    !! ./survey client ipc:///tmp/survey.ipc client2 & client2=$!
    !! sleep 10
    !! kill $server $client0 $client1 $client2
    !! ```
    use :: nng
    use :: nng_survey0
    use :: nng_util, only: f_c_str
    implicit none (type, external)

    character(80) :: name, url, type

    call get_command_argument(1, type)
    call get_command_argument(2, url)
    call get_command_argument(3, name)

    select case (type)
        case ('client'); call client(url, name)
        case ('server'); call server(url)
        case default;    print '("Usage: survey client|server <URL> <ARG> ...")'
    end select
contains
    subroutine fatal(rc, str)
        integer,      intent(in) :: rc
        character(*), intent(in) :: str

        print '(a, ": ", a)', str, nng_strerror(rc)
        stop
    end subroutine fatal

    function iso8601()
        character(*), parameter :: ISO_FMT = &
            '(i4, 2("-", i2.2), "T", 2(i0.2, ":"), i0.2, ".", i0.3, a, ":", a)'

        character(29) :: iso8601
        character(5)  :: zone
        integer       :: dt(8)

        call date_and_time(values=dt, zone=zone)
        write (iso8601, ISO_FMT) dt(1:3), dt(5:8), zone(1:3), zone(4:5)
    end function iso8601

    subroutine client(url, name)
        character(*), intent(in) :: url
        character(*), intent(in) :: name

        character(80), target :: buffer
        integer               :: rc
        integer(c_size_t)     :: sz
        type(nng_dialer)      :: dialer
        type(nng_socket)      :: socket

        rc = nng_respondent0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_respondent0_open')

        rc = nng_dial(socket, f_c_str(url), dialer, NNG_FLAG_NONBLOCK)
        if (rc /= 0) call fatal(rc, 'nng_dial')

        do
            buffer = ' '
            sz = len(buffer, c_size_t)
            rc = nng_recv(socket, c_loc(buffer), sz, 0)
            if (rc /= 0) cycle
            print '("CLIENT (", a, "): RECEIVED ", a, " SURVEY REQUEST")', trim(name), trim(buffer)

            buffer = iso8601()
            sz = len_trim(buffer, c_size_t)
            print '("CLIENT (", a, "): SENDING DATE SURVEY RESPONSE")', trim(name)
            rc = nng_send(socket, c_loc(buffer), sz, 0)
            if (rc /= 0) call fatal(rc, 'nng_send')
        end do

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')
    end subroutine client

    subroutine server(url)
        character(*), intent(in) :: url

        character(80), target :: buffer
        integer               :: rc
        integer(c_size_t)     :: sz
        type(nng_listener)    :: listener
        type(nng_socket)      :: socket

        rc = nng_surveyor0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_surveyor0_open')

        rc = nng_listen(socket, f_c_str(url), listener, 0)
        if (rc /= 0) call fatal(rc, 'nng_listen')

        do
            buffer = 'DATE'
            sz = len_trim(buffer, c_size_t)

            print '("SERVER: SENDING ", a, " SURVEY REQUEST")', trim(buffer)
            rc = nng_send(socket, c_loc(buffer), sz, 0)
            if (rc /= 0) call fatal(rc, 'nng_send')

            do
                buffer = ' '
                sz = len(buffer, c_size_t)
                rc = nng_recv(socket, c_loc(buffer), sz, 0)

                if (rc == NNG_ETIMEDOUT) exit
                if (rc /= 0) call fatal(rc, 'nng_recv')

                print '("SERVER: RECEIVED ", a, " SURVEY RESPONSE")', trim(buffer)
            end do

            print '("SERVER: SURVEY COMPLETE")'
        end do

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')
    end subroutine server
end program main
