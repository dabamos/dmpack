! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_mutex
    !! Abstraction layer over NNG mutex handling.
    use :: nng
    use :: dm_c
    use :: dm_error
    implicit none (type, external)
    private

    type, public :: ipc_mutex_type
        !! Opaque IPC mutex type.
        private
        type(c_ptr) :: ctx = c_null_ptr !! NNG mutex context.
    end type ipc_mutex_type

    public :: dm_ipc_mutex_create
    public :: dm_ipc_mutex_destroy
    public :: dm_ipc_mutex_lock
    public :: dm_ipc_mutex_unlock
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ipc_mutex_create(mutex) result(rc)
        !! Creates NNG mutex. The mutex objects created by this function are
        !! suitable only for simple lock and unlock operations, and are not
        !! recursive. Every effort has been made to use light-weight underlying
        !! primitives when available.
        !!
        !! Mutex (mutual exclusion) objects can be thought of as binary
        !! semaphores, where only a single thread of execution is permitted to
        !! acquire the semaphore.
        use :: dm_ipc, only: dm_ipc_error

        type(ipc_mutex_type), intent(out) :: mutex !! IPC mutex.

        integer :: stat

        stat= nng_mtx_alloc(mutex%ctx)
        rc = dm_ipc_error(stat)
    end function dm_ipc_mutex_create

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_ipc_mutex_destroy(mutex)
        !! Destroys NNG mutex.
        type(ipc_mutex_type), intent(inout) :: mutex !! IPC mutex.

        call nng_mtx_free(mutex%ctx)
    end subroutine dm_ipc_mutex_destroy

    subroutine dm_ipc_mutex_lock(mutex)
        !! Locks NNG mutex.
        type(ipc_mutex_type), intent(inout) :: mutex !! IPC mutex.

        call nng_mtx_lock(mutex%ctx)
    end subroutine dm_ipc_mutex_lock

    subroutine dm_ipc_mutex_unlock(mutex)
        !! Unlocks NNG mutex.
        type(ipc_mutex_type), intent(inout) :: mutex !! IPC mutex.

        call nng_mtx_unlock(mutex%ctx)
    end subroutine dm_ipc_mutex_unlock
end module dm_ipc_mutex
