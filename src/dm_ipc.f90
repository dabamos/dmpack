! Author:  Philipp Engel
! Licence: ISC
module dm_ipc
    !! Abstraction layer over NNG sockets.
    use :: nng
    use :: dm_c
    use :: dm_error
    implicit none (type, external)
    private

    type, public :: ipc_context_type
        type(nng_socket)   :: socket           !! NNG socket.
        type(nng_dialer)   :: dialer           !! NNG dialer.
        type(nng_listener) :: listener         !! NNG listener.
        type(c_ptr)        :: aio = c_null_ptr !! NNG aio.
        type(c_ptr)        :: ctx = c_null_ptr !! NNG context.
    end type ipc_context_type

    public :: dm_ipc_dial
    public :: dm_ipc_close
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ipc_dial(context, url) result(rc)
        type(ipc_context_type), intent(inout) :: context !! IPC context.
        character(*),           intent(in)    :: url     !! URL.

        integer :: stat

        rc = E_IO
        stat = nng_dial(context%socket, dm_f_c_string(url), context%dialer, 0)
        if (stat == 0) rc = E_NONE
    end function dm_ipc_dial

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_ipc_close(context)
        type(ipc_context_type), intent(inout) :: context !! IPC context.

        integer :: rc

        rc = nng_socket_close(context%socket)
    end subroutine dm_ipc_close
end module dm_ipc
