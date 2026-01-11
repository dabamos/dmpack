! Author:  Philipp Engel
! Licence: ISC
module dm_sync
    !! Module for keeping track of data record synchronisation status.
    use :: dm_kind
    use :: dm_observ
    use :: dm_string
    use :: dm_time
    use :: dm_uuid
    implicit none (type, external)
    private

    integer, parameter, public :: SYNC_ID_LEN = UUID_LEN !! Synchronisation id length (must equal UUID length).

    ! Synchronisation types.
    integer, parameter, public :: SYNC_TYPE_NONE   = 0 !! No type (invalid).
    integer, parameter, public :: SYNC_TYPE_NODE   = 1 !! Node.
    integer, parameter, public :: SYNC_TYPE_SENSOR = 2 !! Sensor.
    integer, parameter, public :: SYNC_TYPE_TARGET = 3 !! Target.
    integer, parameter, public :: SYNC_TYPE_OBSERV = 4 !! Observation.
    integer, parameter, public :: SYNC_TYPE_LOG    = 5 !! Log.
    integer, parameter, public :: SYNC_TYPE_IMAGE  = 6 !! Image.
    integer, parameter, public :: SYNC_TYPE_LAST   = 6 !! Never use this.

    integer, parameter, public :: SYNC_TYPE_NAME_LEN = 6 !! Max. type name length.

    character(*), parameter, public :: SYNC_TYPE_NAMES(SYNC_TYPE_NONE:SYNC_TYPE_LAST) = [ &
        character(SYNC_TYPE_NAME_LEN) :: 'none', 'node', 'sensor', 'target', 'observ', 'log', 'image' &
    ] !! Array of sync type names.

    type, public :: sync_type
        !! Log, observation, node, sensor and target synchronisation type.
        integer                :: type      = SYNC_TYPE_NONE !! Sync data type.
        character(SYNC_ID_LEN) :: id        = ' '            !! Sync data id.
        character(TIME_LEN)    :: timestamp = TIME_DEFAULT   !! Timestamp of last synchronisation attempt.
        integer                :: code      = 0              !! HTTP response code of DMPACK server.
        integer                :: attempts  = 0              !! Number of (unsuccessful) attempts to transfer.
    end type sync_type

    integer, parameter, public :: SYNC_TYPE_SIZE = storage_size(sync_type()) / 8 !! Size of `sync_type` in bytes.

    interface operator (==)
        !! Returns `.true.` if sync types are equal.
        module procedure :: dm_sync_equals
    end interface

    public :: operator (==)

    public :: dm_sync_equals
    public :: dm_sync_is_valid
    public :: dm_sync_name
    public :: dm_sync_out
    public :: dm_sync_set
    public :: dm_sync_type_from_name
    public :: dm_sync_type_is_valid
contains
    pure elemental logical function dm_sync_equals(sync1, sync2) result(equals)
        !! Returns `.true.` if given sync types are equal.
        type(sync_type), intent(in) :: sync1 !! First sync.
        type(sync_type), intent(in) :: sync2 !! Second sync.

        equals = (sync1%type      == sync2%type      .and. &
                  sync1%id        == sync2%id        .and. &
                  sync1%timestamp == sync2%timestamp .and. &
                  sync1%code      == sync2%code      .and. &
                  sync1%attempts  == sync2%attempts)
    end function dm_sync_equals

    pure elemental logical function dm_sync_is_valid(sync) result(valid)
        !! Returns `.true.` if given sync data is valid.
        type(sync_type), intent(in) :: sync !! Sync.

        valid = .false.
        if (.not. dm_sync_type_is_valid(sync%type)) return
        if (len_trim(sync%id) == 0) return
        valid = .true.
    end function dm_sync_is_valid

    pure elemental integer function dm_sync_type_from_name(name) result(type)
        !! Returns synchonisation type from given name.
        character(*), intent(in) :: name !! Sync type name.

        character(SYNC_TYPE_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (SYNC_TYPE_NAMES(SYNC_TYPE_NODE));   type = SYNC_TYPE_NODE
            case (SYNC_TYPE_NAMES(SYNC_TYPE_SENSOR)); type = SYNC_TYPE_SENSOR
            case (SYNC_TYPE_NAMES(SYNC_TYPE_TARGET)); type = SYNC_TYPE_TARGET
            case (SYNC_TYPE_NAMES(SYNC_TYPE_OBSERV)); type = SYNC_TYPE_OBSERV
            case (SYNC_TYPE_NAMES(SYNC_TYPE_LOG));    type = SYNC_TYPE_LOG
            case default;                             type = SYNC_TYPE_NONE
        end select
    end function dm_sync_type_from_name

    pure elemental logical function dm_sync_type_is_valid(type) result(valid)
        !! Returns `.true.` if given sync type enumerator is valid. The
        !! type `SYNC_TYPE_NONE` is invalid.
        integer, intent(in) :: type !! Sync type enum.

        valid = (type > SYNC_TYPE_NONE .and. type <= SYNC_TYPE_LAST)
    end function dm_sync_type_is_valid

    pure function dm_sync_name(type) result(name)
        !! Returns name of synchronisation type.
        integer, intent(in)       :: type !! Sync type enum.
        character(:), allocatable :: name !! Name of sync type.

        integer :: type_

        type_ = SYNC_TYPE_NONE
        if (type >= SYNC_TYPE_NONE .and. type <= SYNC_TYPE_LAST) type_ = type
        name = trim(SYNC_TYPE_NAMES(type_))
    end function dm_sync_name

    subroutine dm_sync_out(sync, unit)
        !! Prints sync type to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(sync_type), intent(inout)        :: sync !! Sync.
        integer,         intent(in), optional :: unit !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("sync.type: ", a)')       sync%type
        write (unit_, '("sync.id: ", a)')         trim(sync%id)
        write (unit_, '("sync.timestamp: ", a)')  sync%timestamp
        write (unit_, '("sync.code: ", i0)')      sync%code
        write (unit_, '("sync.attempts: ", i0)')  sync%attempts
    end subroutine dm_sync_out

    pure elemental subroutine dm_sync_set(sync, type, id, timestamp, code, attempts)
        !! Sets attributes of given sync type.
        type(sync_type),     intent(inout)        :: sync      !! Sync.
        integer,             intent(in), optional :: type      !! Sync data type.
        character(*),        intent(in), optional :: id        !! Sync data id.
        character(TIME_LEN), intent(in), optional :: timestamp !! Timestamp of last synchronisation attempt.
        integer,             intent(in), optional :: code      !! HTTP response code of DMPACK server.
        integer,             intent(in), optional :: attempts  !! Number of (unsuccessful) attempts to transfer.

        if (present(type))      sync%type      = type
        if (present(id))        sync%id        = id
        if (present(timestamp)) sync%timestamp = timestamp
        if (present(code))      sync%code      = code
        if (present(attempts))  sync%attempts  = attempts
    end subroutine dm_sync_set
end module dm_sync
