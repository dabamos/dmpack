! pair.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! Implementation of the pair pattern. The pair pattern is used when there a
    !! one-to-one peer relationship. Only one peer may be connected to another
    !! peer at a time, but both may speak freely. The program is based on the
    !! example in C listed in:
    !!
    !!     https://nanomsg.org/gettingstarted/nng/pair.html
    !!
    !! Example usage:
    !!
    !! ```
    !! ./pair node0 ipc:///tmp/pair.ipc & node0=$!
    !! ./pair node1 ipc:///tmp/pair.ipc & node1=$!
    !! sleep 3
    !! kill $node0 $node1
    !! ```
    use :: nng
    use :: nng_pair0
    use :: nng_util, only: f_c_str
    implicit none (type, external)

    character(80) :: name, url

    call get_command_argument(1, name)
    call get_command_argument(2, url)

    select case (name)
        case ('node0'); call node0(url)
        case ('node1'); call node1(url)
        case default;   print '("Usage: pair node0|node1 <URL> <ARG> ...")'
    end select
contains
    subroutine fatal(rc, str)
        integer,      intent(in) :: rc
        character(*), intent(in) :: str

        print '(a, ": ", a)', str, nng_strerror(rc)
        stop
    end subroutine fatal

    subroutine node0(url)
        character(*), intent(in) :: url

        integer            :: rc
        type(nng_listener) :: listener
        type(nng_socket)   :: socket

        rc = nng_pair0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_pair0_open')

        rc = nng_listen(socket, f_c_str(url), listener, 0)
        if (rc /= 0) call fatal(rc, 'nng_pull0_open')

        call send_recv(socket, 'node0')

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')
    end subroutine node0

    subroutine node1(url)
        character(*), intent(in) :: url

        integer          :: rc
        type(nng_dialer) :: dialer
        type(nng_socket) :: socket

        rc = nng_pair0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_pair0_open')

        ! Wait for the peer.
        call nng_msleep(1000)

        rc = nng_dial(socket, f_c_str(url), dialer, 0)
        if (rc /= 0) call fatal(rc, 'nng_dial')

        call send_recv(socket, 'node1')

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')
    end subroutine node1

    subroutine recv_name(socket, name)
        type(nng_socket), intent(inout) :: socket
        character(*),     intent(in)    :: name

        character(len(name)), target :: buffer
        integer                      :: rc
        integer(c_size_t)            :: sz

        buffer = ' '
        sz = len(buffer, c_size_t)
        rc = nng_recv(socket, c_loc(buffer), sz, 0)
        if (rc /= 0) return

        print '(a, " RECEIVED ", a)', trim(name), trim(buffer)
    end subroutine recv_name

    subroutine send_name(socket, name)
        type(nng_socket),     intent(inout) :: socket
        character(*), target, intent(in)    :: name

        integer :: rc

        print '(a, ": SENDING ", a)', trim(name), trim(name)
        rc = nng_send(socket, c_loc(name), len_trim(name, c_size_t), 0)
        if (rc /= 0) call fatal(rc, 'nng_send')
    end subroutine send_name

    subroutine send_recv(socket, name)
        type(nng_socket), intent(inout) :: socket
        character(*),     intent(in)    :: name

        integer :: rc

        rc = nng_socket_set_ms(socket, NNG_OPT_RECVTIMEO, 100)
        if (rc /= 0) call fatal(rc, 'nng_setopt_ms')

        do
            call recv_name(socket, name)
            call nng_msleep(1000)
            call send_name(socket, name)
        end do
    end subroutine send_recv
end program main
