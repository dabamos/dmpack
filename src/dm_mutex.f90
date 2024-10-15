! Author:  Philipp Engel
! Licence: ISC
module dm_mutex
    !! POSIX mutex abstraction layer. Has to be linked with `-lpthread`.
    use :: unix
    use :: dm_error
    implicit none (type, external)
    private

    type, public :: mutex_type
        !! Opaque mutex type.
        private
        type(c_pthread_mutex_t) :: ctx !! Mutex context type.
    end type mutex_type

    public :: dm_mutex_create
    public :: dm_mutex_destroy
    public :: dm_mutex_is_locked
    public :: dm_mutex_lock
    public :: dm_mutex_try_lock
    public :: dm_mutex_unlock
contains
    integer function dm_mutex_create(mutex) result(rc)
        !! Creates a new mutex. Returns `E_SYSTEM` on error.
        type(mutex_type), intent(inout) :: mutex !! Mutex type.

        rc = E_SYSTEM
        if (c_pthread_mutex_init(mutex%ctx, c_null_ptr) /= 0) return
        rc = E_NONE
    end function dm_mutex_create

    integer function dm_mutex_destroy(mutex) result(rc)
        !! Destroys mutex. Returns `E_SYSTEM` on error.
        type(mutex_type), intent(inout) :: mutex !! Mutex type.

        rc = E_SYSTEM
        if (c_pthread_mutex_destroy(mutex%ctx) /= 0) return
        rc = E_NONE
    end function dm_mutex_destroy

    logical function dm_mutex_is_locked(mutex) result(is)
        !! Returns `.true.` if mutex is locked.
        type(mutex_type), intent(inout) :: mutex !! Mutex type.

        integer :: stat

        is = .false.

        if (c_pthread_mutex_trylock(mutex%ctx) == 0) then
            stat = c_pthread_mutex_unlock(mutex%ctx)
            return
        end if

        is = .true.
    end function dm_mutex_is_locked

    integer function dm_mutex_lock(mutex) result(rc)
        !! Locks mutex. Returns `E_SYSTEM` on error.
        type(mutex_type), intent(inout) :: mutex !! Mutex type.

        rc = E_SYSTEM
        if (c_pthread_mutex_lock(mutex%ctx) /= 0) return
        rc = E_NONE
    end function dm_mutex_lock

    integer function dm_mutex_try_lock(mutex) result(rc)
        !! Tries to lock mutex. Returns `E_SYSTEM` on error.
        type(mutex_type), intent(inout) :: mutex !! Mutex type.

        rc = E_SYSTEM
        if (c_pthread_mutex_trylock(mutex%ctx) /= 0) return
        rc = E_NONE
    end function dm_mutex_try_lock

    integer function dm_mutex_unlock(mutex) result(rc)
        !! Unlocks mutex. Returns `E_SYSTEM` on error.
        type(mutex_type), intent(inout) :: mutex !! Mutex type.

        rc = E_SYSTEM
        if (c_pthread_mutex_unlock(mutex%ctx) /= 0) return
        rc = E_NONE
    end function dm_mutex_unlock
end module dm_mutex
