! pipeline.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! Implementation of the pipeline pattern. The program is based on the
    !! example in C listed in:
    !!
    !!     https://nanomsg.org/gettingstarted/nng/pipeline.html
    !!
    !! Example usage:
    !!
    !! ```
    !! ./pipeline node0 ipc:///tmp/pipeline.ipc & node0=$! && sleep 1
    !! ./pipeline node1 ipc:///tmp/pipeline.ipc "Hello, World!"
    !! ./pipeline node1 ipc:///tmp/pipeline.ipc "Goodbye."
    !! kill $node0
    !! ```
    use :: nng
    use :: nng_pipeline0
    use :: nng_util
    implicit none (type, external)

    character(80) :: message, name, url

    call get_command_argument(1, name)
    call get_command_argument(2, url)
    call get_command_argument(3, message)

    select case (name)
        case ('node0'); call node0(url)
        case ('node1'); call node1(url, message)
        case default;   print '("Usage: pipeline node0|node1 <URL> <ARG> ...")'
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

        character(512), target :: buffer
        integer                :: rc
        integer(c_size_t)      :: sz
        type(nng_listener)     :: listener
        type(nng_socket)       :: socket

        rc = nng_pull0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_pull0_open')

        rc = nng_listen(socket, f_c_str(url), listener, 0)
        if (rc /= 0) call fatal(rc, 'nng_listen')

        do
            buffer = ' '
            sz = len(buffer, c_size_t)
            rc = nng_recv(socket, c_loc(buffer), sz, 0)
            if (rc /= 0) call fatal(rc, 'nng_recv')

            print '("[NODE0] RECEIVED ", a)', trim(buffer)
        end do

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')
    end subroutine node0

    subroutine node1(url, message)
        character(*),         intent(in) :: url
        character(*), target, intent(in) :: message

        integer          :: rc
        type(nng_dialer) :: dialer
        type(nng_socket) :: socket

        rc = nng_push0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_push0_open')

        rc = nng_dial(socket, f_c_str(url), dialer, 0)
        if (rc /= 0) call fatal(rc, 'nng_dial')

        print '("[NODE1] SENDING ", a)', trim(message)
        rc = nng_send(socket, c_loc(message), len_trim(message, c_size_t), 0)
        if (rc /= 0) call fatal(rc, 'nng_send')

        ! Wait for messages to flush before shutting down.
        call nng_msleep(1000)
        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')
    end subroutine node1
end program main
