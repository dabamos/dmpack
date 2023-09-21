! Author:  Philipp Engel
! Licence: ISC
module dm_sync
    !! Module for keeping track of observation and log data synchronisation
    !! status.
    use :: dm_kind
    use :: dm_observ
    use :: dm_string
    use :: dm_time
    use :: dm_uuid
    implicit none (type, external)
    private

    integer, parameter, public :: SYNC_ID_LEN = UUID_LEN !! Synchronisation id length (must equal UUID4 length).

    integer, parameter, public :: SYNC_TYPE_NONE   = 0 !! No type (invalid).
    integer, parameter, public :: SYNC_TYPE_NODE   = 1 !! Node.
    integer, parameter, public :: SYNC_TYPE_SENSOR = 2 !! Sensor.
    integer, parameter, public :: SYNC_TYPE_TARGET = 3 !! Target.
    integer, parameter, public :: SYNC_TYPE_OBSERV = 4 !! Observation.
    integer, parameter, public :: SYNC_TYPE_LOG    = 5 !! Log.
    integer, parameter, public :: SYNC_NTYPES      = 6 !! Number of types.

    integer, parameter, public :: SYNC_TYPE_NAME_LEN = 6

    character(len=*), parameter, public :: SYNC_TYPE_NAMES(0:SYNC_NTYPES - 1) = [ &
        character(len=SYNC_TYPE_NAME_LEN) :: 'none', 'node', 'sensor', 'target', &
        'observ', 'log' ] !! Array of sync type names.

    type, public :: sync_type
        !! Log, observation, node, sensor and target synchronisation type.
        integer                    :: type      = SYNC_TYPE_NONE !! Sync data type.
        character(len=SYNC_ID_LEN) :: id        = ' '            !! Sync data id.
        character(len=TIME_LEN)    :: timestamp = TIME_DEFAULT   !! Timestamp of last synchronisation attempt.
        integer                    :: code      = 0              !! HTTP response code of DMPACK server.
        integer                    :: nattempts = 0              !! Number of (unsuccessful) attempts to transfer.
    end type sync_type

    interface operator (==)
        !! Returns whether sync types are equal.
        module procedure :: dm_sync_equals
    end interface

    public :: operator (==)

    public :: dm_sync_equals
    public :: dm_sync_name
    public :: dm_sync_out
    public :: dm_sync_type_from_name
    public :: dm_sync_valid
contains
    pure elemental logical function dm_sync_equals(sync1, sync2) result(equals)
        !! Returns `.true.` if given sync types are equal.
        type(sync_type), intent(in) :: sync1 !! First sync data.
        type(sync_type), intent(in) :: sync2 !! Second sync data.

        equals = .false.
        if (sync1%type      /= sync2%type)      return
        if (sync1%id        /= sync2%id)        return
        if (sync1%timestamp /= sync2%timestamp) return
        if (sync1%code      /= sync2%code)      return
        if (sync1%nattempts /= sync2%nattempts) return
        equals = .true.
    end function dm_sync_equals

    pure elemental integer function dm_sync_type_from_name(name) result(type)
        !! Returns synchonisation type from given name.
        character(len=*), intent(in) :: name !! Sync type name.

        character(len=SYNC_TYPE_NAME_LEN) :: n

        ! Normalise name.
        n = dm_lower(name)

        select case (n)
            case (SYNC_TYPE_NAMES(SYNC_TYPE_NODE))
                type = SYNC_TYPE_NODE
            case (SYNC_TYPE_NAMES(SYNC_TYPE_SENSOR))
                type = SYNC_TYPE_SENSOR
            case (SYNC_TYPE_NAMES(SYNC_TYPE_TARGET))
                type = SYNC_TYPE_TARGET
            case (SYNC_TYPE_NAMES(SYNC_TYPE_OBSERV))
                type = SYNC_TYPE_OBSERV
            case (SYNC_TYPE_NAMES(SYNC_TYPE_LOG))
                type = SYNC_TYPE_LOG
            case default
                type = SYNC_TYPE_NONE
        end select
    end function dm_sync_type_from_name

    pure elemental logical function dm_sync_valid(sync) result(valid)
        !! Returns `.true.` if given sync data is valid.
        type(sync_type), intent(in) :: sync !! Sync type.

        valid = .false.
        if (sync%type <= SYNC_TYPE_NONE .or. sync%type >= SYNC_NTYPES) return
        if (len_trim(sync%id) == 0) return
        valid = .true.
    end function dm_sync_valid

    pure function dm_sync_name(type) result(name)
        !! Returns name of synchronisation type.
        integer, intent(in)           :: type !! Sync type enum.
        character(len=:), allocatable :: name !! Name of sync type.
        integer                       :: type_

        type_ = SYNC_TYPE_NONE
        if (type >= 0 .and. type < SYNC_NTYPES) type_ = type
        name = trim(SYNC_TYPE_NAMES(type_))
    end function dm_sync_name

    subroutine dm_sync_out(sync, unit)
        !! Prints sync type to standard output or given file unit.
        type(sync_type), intent(inout)        :: sync !! Sync type.
        integer,         intent(in), optional :: unit !! File unit.

        integer :: unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        write (unit_, '("sync.type: ", a)')       dm_sync_name(sync%type)
        write (unit_, '("sync.id: ", a)')         trim(sync%id)
        write (unit_, '("sync.timestamp: ", a)')  sync%timestamp
        write (unit_, '("sync.code: ", i0)')      sync%code
        write (unit_, '("sync.nattempts: ", i0)') sync%code
    end subroutine dm_sync_out
end module dm_sync
