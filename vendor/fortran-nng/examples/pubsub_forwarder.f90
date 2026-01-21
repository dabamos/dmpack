! pubsub_forwarder.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! Forwarder example based on the NNG demo program `pubsub_forwarder.c`.
    !! This example shows how to use raw sockets to set up a forwarder or proxy
    !! for pub/sub.
    !!
    !! An example setup for running this example would involve the following:
    !!
    !! Run this example binary (in the background or a terminal, etc.). In a
    !! new terminal, run:
    !!
    !! ```
    !! nngcat --sub --dial "tcp://localhost:3328" --quoted
    !! ```
    !!
    !! In a second terminal, run:
    !!
    !! ```
    !! nngcat --sub --dial "tcp://localhost:3328" --quoted
    !! ```
    !!
    !! In a third terminal, run:
    !!
    !! ```
    !! for n in $(seq 0 99);
    !!     do nngcat --pub --dial "tcp://localhost:3327" --data "$n";
    !! done
    !! ```
    use :: nng
    use :: nng_pubsub0
    use :: nng_util, only: f_c_str
    implicit none (type, external)

    character(*), parameter :: PROXY_FRONT_URL = 'tcp://localhost:3327'
    character(*), parameter :: PROXY_BACK_URL  = 'tcp://localhost:3328'

    integer            :: rc
    type(nng_socket)   :: sock_back, sock_front
    type(nng_listener) :: ls_back, ls_front

    print '("nng version: ", a)', nng_version()

    ! First we need some nng sockets. Not to be confused with network sockets.
    rc = nng_sub0_open_raw(sock_front)
    if (rc /= 0) call error(rc, 'Failed to open front end socket')

    rc = nng_pub0_open_raw(sock_back)
    if (rc /= 0) call error(rc, 'Failed to open back end socket')

    ! Now we need to set up a listener for each socket so that they have addresses.
    rc = nng_listener_create(ls_front, sock_front, f_c_str(PROXY_FRONT_URL))
    if (rc /= 0) call error(rc, 'Failed to create front listener')

    rc = nng_listener_create(ls_back, sock_back, f_c_str(PROXY_BACK_URL))
    if (rc /= 0) call error(rc, 'Failed to create back listener')

    rc = nng_listener_start(ls_front, 0)
    if (rc /= 0) call error(rc, 'Failed to start front listener')

    rc = nng_listener_start(ls_back, 0)
    if (rc /= 0) call error(rc, 'Failed to start back listener')

    ! Finally let nng do the forwarding/proxying.
    rc = nng_device(sock_front, sock_back)
    if (rc /= 0) call error(rc, 'nng_device failed')

    print '("done")'
    call nng_fini()
contains
    subroutine error(rc, str)
        integer,      intent(in) :: rc
        character(*), intent(in) :: str

        print '(a, ": ", a)', str, nng_strerror(rc)
        call nng_fini()
        stop
    end subroutine error
end program main
