! Author:  Philipp Engel
! Licence: ISC
module dm_mutex
    !! POSIX mutex abstraction layer.
    use, intrinsic :: iso_c_binding, only: c_null_ptr
    use :: unix
    use :: dm_error
    implicit none (type, external)
    private

    type, public :: mutex_type
        !! Opaque mutex type.
        private
        type(c_pthread_mutex_t) :: ptr !! C pointer to mutex.
    end type mutex_type

    public :: dm_mutex_create
    public :: dm_mutex_destroy
    public :: dm_mutex_lock
    public :: dm_mutex_unlock
contains
    integer function dm_mutex_create(mutex) result(rc)
        !! Creates a new mutex mutex.
        type(mutex_type), intent(inout) :: mutex !! Mutex type.

        rc = E_SYSTEM
        if (c_pthread_mutex_init(mutex%ptr, c_null_ptr) /= 0) return
        rc = E_NONE
    end function dm_mutex_create

    integer function dm_mutex_destroy(mutex) result(rc)
        !! Destroys mutex.
        type(mutex_type), intent(inout) :: mutex !! Mutex type.

        rc = E_SYSTEM
        if (c_pthread_mutex_destroy(mutex%ptr) /= 0) return
        rc = E_NONE
    end function dm_mutex_destroy

    integer function dm_mutex_lock(mutex) result(rc)
        !! Locks mutex.
        type(mutex_type), intent(inout) :: mutex !! Mutex type.

        rc = E_SYSTEM
        if (c_pthread_mutex_lock(mutex%ptr) /= 0) return
        rc = E_NONE
    end function dm_mutex_lock

    integer function dm_mutex_unlock(mutex) result(rc)
        !! Unlocks mutex.
        type(mutex_type), intent(inout) :: mutex !! Mutex type.

        rc = E_SYSTEM
        if (c_pthread_mutex_unlock(mutex%ptr) /= 0) return
        rc = E_NONE
    end function dm_mutex_unlock
end module dm_mutex
