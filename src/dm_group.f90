! Author:  Philipp Engel
! Licence: ISC
module dm_group
    !! Observation group module.
    use :: dm_error
    use :: dm_observ
    use :: dm_uuid
    implicit none (type, external)
    private

    integer, parameter, public :: GROUP_ID_LEN = OBSERV_ID_LEN

    type, public :: group_type
        !! The observation group is a container structure for storing
        !! observations which are related and form a group.
        character(GROUP_ID_LEN)        :: id    = UUID_DEFAULT !! Group id (UUIDv4).
        integer                        :: index = 0            !! Cursor.
        type(observ_type), allocatable :: observs(:)           !! Observations.
    end type group_type

    public :: dm_group_add
    public :: dm_group_count
    public :: dm_group_create
    public :: dm_group_destroy
    public :: dm_group_id
    public :: dm_group_is_valid
    public :: dm_group_next
    public :: dm_group_size
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_group_add(group, observ) result(rc)
        !! Adds observation to group and sets attribute `group_id` to the id of
        !! the group. If the observation array of the group is already
        !! allocated, the function grows the array by 1 to fit the observation
        !! in.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if group is not initialised.
        !! * `E_LIMIT` if group limit has been reached.
        !!
        type(group_type),  intent(inout) :: group  !! Observation group.
        type(observ_type), intent(in)    :: observ !! Observation to add.

        integer                        :: i, n, stat
        type(observ_type), allocatable :: buffer(:)

        n = dm_group_size(group)

        if (n == 0) then
            rc = dm_group_create(group, 1)
            if (dm_is_error(rc)) return
        else if (group%index > 0 .and. n == group%index) then
            rc = E_ALLOC
            allocate (buffer(n + 1), stat=stat)
            if (stat /= 0) return
            buffer(:n) = group%observs
            deallocate (group%observs)
            call move_alloc(buffer, group%observs)
        end if

        rc = E_INVALID
        if (.not. dm_group_is_valid(group)) return

        rc = E_LIMIT
        if (group%index >= size(group%observs)) return

        rc = E_NONE
        i = group%index + 1
        group%index = i
        group%observs(i) = observ
        group%observs(i)%group_id = group%id
    end function dm_group_add

    integer function dm_group_count(group) result(n)
        !! Returns number of observations in the group.
        type(group_type), intent(inout) :: group !! Observation group.

        n = 0
        if (.not. allocated(group%observs)) return
        n = group%index
    end function dm_group_count

    integer function dm_group_create(group, size) result(rc)
        !! Creates new observation group of given size and generates random
        !! group id.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if size is less than 0.
        !!
        type(group_type), intent(out) :: group !! Observation group.
        integer,          intent(in)  :: size  !! Max. group size.

        integer :: stat

        rc = E_INVALID
        if (size < 0) return

        rc = E_ALLOC
        allocate (group%observs(size), stat=stat)
        if (stat /= 0) return

        rc = E_NONE
        group%id = dm_uuid4()
    end function dm_group_create

    pure elemental subroutine dm_group_destroy(group)
        !! Resets observation group.
        type(group_type), intent(inout) :: group !! Observation group.

        group%id    = UUID_DEFAULT
        group%index = 0
        if (allocated(group%observs)) deallocate (group%observs)
    end subroutine dm_group_destroy

    function dm_group_id(group) result(id)
        !! Returns the id of the given observation group.
        type(group_type), intent(inout) :: group !! Observation group.
        character(GROUP_ID_LEN)         :: id

        id = group%id
    end function dm_group_id

    logical function dm_group_is_valid(group) result(valid)
        !! Returns `.true.` if group id is valid and observation array is
        !! allocated.
        type(group_type), intent(inout) :: group !! Observation group.

        valid = (dm_uuid4_is_valid(group%id) .and. dm_group_size(group) > 0)
    end function dm_group_is_valid

    integer function dm_group_next(group, index, observ) result(rc)
        !! Returns observation at given index in group and increases the index
        !! by 1. If the passed index is 0, it will be set to 1 first.
        !!
        !! If the all observations have been passed, the index is reset to 0 and
        !! the function returns `E_BOUNDS`.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_BOUNDS` if the index is out of bounds.
        !!
        type(group_type),  intent(inout) :: group  !! Observation group.
        integer,           intent(inout) :: index  !! Index of next observation.
        type(observ_type), intent(out)   :: observ !! Observation at given index.

        integer :: i

        rc = E_BOUNDS

        i = index
        if (i == 0) i = 1

        if (i < 1 .or. i > dm_group_size(group)) then
            index = 0
            return
        end if

        observ = group%observs(i)
        index  = i + 1

        rc = E_NONE
    end function dm_group_next

    integer function dm_group_size(group) result(n)
        !! Returns size of observation group.
        type(group_type), intent(inout) :: group !! Observation group.

        n = 0
        if (.not. allocated(group%observs)) return
        n = size(group%observs)
    end function dm_group_size
end module dm_group
