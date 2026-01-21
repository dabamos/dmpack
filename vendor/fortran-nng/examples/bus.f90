! bus.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! The bus protocol is useful for routing applications, or for building
    !! fully interconnected mesh networks. In this pattern, messages are sent to
    !! every directly connected peer. The program is based on the example in C
    !! listed in:
    !!
    !!     https://nanomsg.org/gettingstarted/nng/bus.html
    !!
    !! Example usage:
    !!
    !! ```
    !! ./bus node0 ipc:///tmp/node0.ipc ipc:///tmp/node1.ipc ipc:///tmp/node2.ipc & node0=$!
    !! ./bus node1 ipc:///tmp/node1.ipc ipc:///tmp/node2.ipc ipc:///tmp/node3.ipc & node1=$!
    !! ./bus node2 ipc:///tmp/node2.ipc ipc:///tmp/node3.ipc & node2=$!
    !! ./bus node3 ipc:///tmp/node3.ipc ipc:///tmp/node0.ipc & node3=$!
    !! sleep 5
    !! kill $node0 $node1 $node2 $node3
    !! ```
    use :: nng
    use :: nng_bus0
    use :: nng_util
    implicit none (type, external)

    character(80)              :: name
    character(80), allocatable :: urls(:)
    integer                    :: i, n

    n = command_argument_count()

    if (n < 3) then
        print '("Usage: bus <NODE_NAME> <URL> <URL> ...")'
        stop
    end if

    allocate (urls(n))
    call get_command_argument(1, name)

    do i = 2, n
        call get_command_argument(i, urls(i - 1))
    end do

    call node(name, urls)
contains
    subroutine fatal(rc, str)
        integer,      intent(in) :: rc
        character(*), intent(in) :: str

        print '(a, ": ", a)', str, nng_strerror(rc)
        stop
    end subroutine fatal

    subroutine node(name, urls)
        character(*), intent(in) :: name
        character(*), intent(in) :: urls(:)

        character(512), target :: buffer
        integer                :: i, n, rc
        integer(c_size_t)      :: sz
        type(nng_dialer)       :: dialer
        type(nng_listener)     :: listener
        type(nng_socket)       :: socket

        n = size(urls)

        rc = nng_bus0_open(socket)
        if (rc /= 0) call fatal(rc, 'nng_bus0_open')

        rc = nng_listen(socket, f_c_str(urls(1)), listener, 0)
        if (rc /= 0) call fatal(rc, 'nng_listen')

        ! Wait for peers to bind.
        call nng_msleep(1000)

        do i = 2, n
            rc = nng_dial(socket, f_c_str(urls(2)), dialer, 0)
            if (rc /= 0) call fatal(rc, 'nng_dial')
        end do

        ! Wait for connects to establish.
        call nng_msleep(1000)

        print '(a, ": SENDING ", a, " ONTO BUS")', trim(name), trim(name)
        buffer = name
        sz = len_trim(buffer, c_size_t)
        rc = nng_send(socket, c_loc(buffer), sz, 0)
        if (rc /= 0) call fatal(rc, 'nng_send')

        ! Receive.
        do
            buffer = ' '
            sz = len(buffer, c_size_t)
            rc = nng_recv(socket, c_loc(buffer), sz, 0)
            if (rc == NNG_ETIMEDOUT) call fatal(rc, 'nng_recv')
            print '(a, ": RECEIVED ", a, " FROM BUS")', trim(name), trim(buffer)
        end do

        rc = nng_socket_close(socket)
        if (rc /= 0) call fatal(rc, 'nng_socket_close')
    end subroutine node
end program main
