! Author:  Philipp Engel
! Licence: ISC
module dm_hash_table
    !! Very basic hash table implementation for a modest number of elements that
    !! stores only pointers to values.
    use :: dm_error
    use :: dm_hash
    use :: dm_kind
    implicit none (type, external)
    private

    type, public :: hash_value_type
        !! Container that keeps generic pointer to hash table value.
        class(*), pointer :: ptr => null()
    end type hash_value_type

    type, public :: hash_table_type
        !! Opaque hash table type of key-value pairs.
        private
        integer                            :: cursor = 0
        integer(kind=i8),      allocatable :: hashes(:)
        type(hash_value_type), allocatable :: values(:)
    end type hash_table_type

    interface dm_hash_table_get
        !! Generic interface to hash table get functions.
        module procedure :: hash_table_get_index
        module procedure :: hash_table_get_key
    end interface

    public :: dm_hash_table_allocated
    public :: dm_hash_table_create
    public :: dm_hash_table_destroy
    public :: dm_hash_table_get
    public :: dm_hash_table_set
    public :: dm_hash_table_size

    private :: hash_table_get_index
    private :: hash_table_get_key
    private :: hash_table_hash
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    logical function dm_hash_table_allocated(hash_table) result(is)
        !! Returns `.true.` if hash table arrays have been allocated.
        type(hash_table_type), intent(inout) :: hash_table  !! Hash table type.

        is = (allocated(hash_table%hashes) .and. allocated(hash_table%values))
    end function dm_hash_table_allocated

    integer function dm_hash_table_create(hash_table, max_entries) result(rc)
        !! Create a new hash table with maximum number of entries.
        type(hash_table_type), intent(inout) :: hash_table  !! Hash table type.
        integer,               intent(in)    :: max_entries !! Maximum number of entries.
        integer                              :: stat

        rc = E_INVALID
        if (max_entries < 1) return

        rc = E_ALLOC
        allocate (hash_table%hashes(max_entries), stat=stat)
        if (stat /= 0) return

        allocate (hash_table%values(max_entries), stat=stat)
        if (stat /= 0) return

        hash_table%hashes(:) = 0_i8
        rc = E_NONE
    end function dm_hash_table_create

    integer function dm_hash_table_set(hash_table, key, value) result(rc)
        !! Adds element to hash table, or replaces existing value. This function
        !! does not resolve hash collisions.
        type(hash_table_type), intent(inout) :: hash_table !! Hash table type.
        character(len=*),      intent(in)    :: key        !! Hash table key.
        class(*), target,      intent(inout) :: value      !! Associated value.

        integer          :: loc
        integer(kind=i8) :: hash

        rc = E_LIMIT

        hash = hash_table_hash(key)
        loc  = findloc(hash_table%hashes, hash, dim=1)

        if (loc == 0) loc = hash_table%cursor + 1
        if (loc > size(hash_table%hashes)) return
        if (loc > 0) hash_table%cursor = loc

        hash_table%hashes(hash_table%cursor) = hash
        hash_table%values(hash_table%cursor)%ptr => value

        rc = E_NONE
    end function dm_hash_table_set

    subroutine dm_hash_table_destroy(hash_table)
        !! Finalises hash table. If the hash table items contain allocatable
        !! data types, you have to deallocate them manually beforehand.
        type(hash_table_type), intent(inout) :: hash_table !! Hash table type.
        integer                              :: i

        if (allocated(hash_table%values)) then
            do i = 1, size(hash_table%values)
                nullify (hash_table%values(i)%ptr)
            end do

            deallocate (hash_table%values)
        end if

        if (allocated(hash_table%hashes)) deallocate (hash_table%hashes)
    end subroutine dm_hash_table_destroy

    subroutine dm_hash_table_size(hash_table, n, max_size)
        !! Returns cursor position in `n` and maximum size of hash table in
        !! `max_size`.
        type(hash_table_type), intent(inout)         :: hash_table !! Hash table type.
        integer,               intent(out), optional :: n          !! Current number of values.
        integer,               intent(out), optional :: max_size   !! Max. number of values.

        if (present(n)) n = hash_table%cursor

        if (present(max_size)) then
            max_size = 0
            if (allocated(hash_table%values)) max_size = size(hash_table%values)
        end if
    end subroutine dm_hash_table_size

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function hash_table_get_index(hash_table, loc, value) result(rc)
        !! Returns pointer to element in hash table by index `i`. On error,
        !! `value` will point to null.
        type(hash_table_type), intent(inout) :: hash_table !! Hash table type.
        integer,               intent(in)    :: loc        !! Hash value index.
        class(*), pointer,     intent(out)   :: value      !! Associated value.

        rc = E_BOUNDS
        value => null()

        if (loc < 1 .or. loc > size(hash_table%values)) return

        rc = E_INVALID
        if (.not. associated(hash_table%values(loc)%ptr)) return

        value => hash_table%values(loc)%ptr
        rc = E_NONE
    end function hash_table_get_index

    integer function hash_table_get_key(hash_table, key, value) result(rc)
        !! Returns pointer to element in hash table by `key`. On error, `value`
        !! will point to null. The intrinsic `findloc()` should be sufficient for
        !! a small number of elements. For larger hash tables, buckets have to be
        !! added.
        type(hash_table_type), intent(inout) :: hash_table !! Hash table type.
        character(len=*),      intent(in)    :: key        !! Hash table key.
        class(*), pointer,     intent(out)   :: value      !! Associated value.

        integer          :: loc
        integer(kind=i8) :: hash

        rc = E_EMPTY
        value => null()

        hash = hash_table_hash(key)
        loc  = findloc(hash_table%hashes, hash, dim=1)
        if (loc == 0) return

        rc = E_INVALID
        if (.not. associated(hash_table%values(loc)%ptr)) return

        value => hash_table%values(loc)%ptr
        rc = E_NONE
    end function hash_table_get_key

    pure elemental integer(kind=i8) function hash_table_hash(key) result(hash)
        !! Returns positive 8-byte integer hash (FNV1a) of given key.
        character(len=*), intent(in) :: key !! Key to be hashed.

        hash = dm_hash_fnv1a(key)
    end function hash_table_hash
end module dm_hash_table
