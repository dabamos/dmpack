! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_trigger
    !! Module of mutex-shielded trigger.
    use :: dm_error
    use :: dm_ipc_mutex
    implicit none (type, external)
    private

    type, public :: ipc_trigger_type
        !! Opaque IPC trigger type.
        private
        logical              :: value = .false.
        type(ipc_mutex_type) :: mutex = ipc_mutex_type()
    end type ipc_trigger_type

    public :: dm_ipc_trigger_create
    public :: dm_ipc_trigger_destroy
    public :: dm_ipc_trigger_get
    public :: dm_ipc_trigger_set
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ipc_trigger_create(trigger, value) result(rc)
        type(ipc_trigger_type), intent(out)          :: trigger !! IPC trigger.
        logical,                intent(in), optional :: value   !! Trigger value.

        rc = dm_ipc_mutex_create(trigger%mutex)
        if (dm_is_error(rc)) return
        if (.not. present(value)) return
        call dm_ipc_trigger_set(trigger, value)
    end function dm_ipc_trigger_create

    logical function dm_ipc_trigger_get(trigger) result(value)
        !! Locks IPC mutex and gets value of IPC trigger before the mutex is
        !! unlocked.
        type(ipc_trigger_type), intent(inout) :: trigger !! IPC trigger.

        call dm_ipc_mutex_lock(trigger%mutex)
        value = trigger%value
        call dm_ipc_mutex_unlock(trigger%mutex)
    end function dm_ipc_trigger_get

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    impure elemental subroutine dm_ipc_trigger_destroy(trigger)
        !! Destroys IPC trigger.
        type(ipc_trigger_type), intent(inout) :: trigger !! IPC trigger.

        call dm_ipc_mutex_destroy(trigger%mutex)
    end subroutine dm_ipc_trigger_destroy

    subroutine dm_ipc_trigger_set(trigger, value)
        !! Locks IPC mutex and sets value of IPC trigger before the mutex is
        !! unlocked.
        type(ipc_trigger_type), intent(inout) :: trigger !! IPC trigger.
        logical,                intent(in)    :: value   !! Trigger value.

        call dm_ipc_mutex_lock(trigger%mutex)
        trigger%value = value
        call dm_ipc_mutex_unlock(trigger%mutex)
    end subroutine dm_ipc_trigger_set
end module dm_ipc_trigger
