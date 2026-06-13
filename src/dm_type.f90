! Author:  Philipp Engel
! Licence: ISC
module dm_type
    !! DMPACK derived type enumerators and utilities.
    implicit none (type, external)
    public

    ! **************************************************************************
    ! PUBLIC PARAMETERS
    ! **************************************************************************
    ! Derived type enumeration.
    integer, parameter, public :: TYPE_NONE     =  0 !! Invalid type.
    integer, parameter, public :: TYPE_NODE     =  1 !! Node.
    integer, parameter, public :: TYPE_SENSOR   =  2 !! Sensor.
    integer, parameter, public :: TYPE_TARGET   =  3 !! Target.
    integer, parameter, public :: TYPE_OBSERV   =  4 !! Observation.
    integer, parameter, public :: TYPE_RESPONSE =  5 !! Response of observation.
    integer, parameter, public :: TYPE_DF       =  6 !! Data frame.
    integer, parameter, public :: TYPE_DP       =  7 !! Data point.
    integer, parameter, public :: TYPE_LOG      =  8 !! Log.
    integer, parameter, public :: TYPE_BEAT     =  9 !! Heartbeat.
    integer, parameter, public :: TYPE_TRANSFER = 10 !! File transfer.
    integer, parameter, public :: TYPE_IMAGE    = 11 !! Image file.
    integer, parameter, public :: TYPE_HEADER   = 12 !! Message header.
    integer, parameter, public :: TYPE_LAST     = 12 !! Never use this.

    integer, parameter, public :: TYPE_NAME_LEN = 8 !! Max. type name length.

    ! Derived type names.
    character(*), parameter, public :: TYPE_NAMES(TYPE_NONE:TYPE_LAST) = [ &
        character(TYPE_NAME_LEN) :: &
        'none',     & ! TYPE_NONE
        'node',     & ! TYPE_NODE
        'sensor',   & ! TYPE_SENSOR
        'target',   & ! TYPE_TARGET
        'observ',   & ! TYPE_OBSERV
        'response', & ! TYPE_RESPONSE
        'df',       & ! TYPE_DF
        'dp',       & ! TYPE_DP
        'log',      & ! TYPE_LOG
        'beat',     & ! TYPE_BEAT
        'transfer', & ! TYPE_TRANSFER
        'image',    & ! TYPE_IMAGE
        'header'    & ! TYPE_HEADER
    ] !! Type names array.

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_type_from_name
    public :: dm_type_is_valid
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    pure elemental integer function dm_type_from_name(name) result(type)
        !! Returns type from given name.
        use :: dm_string, only: dm_to_lower

        character(*), intent(in) :: name !! Derived type name.
        character(TYPE_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (TYPE_NAMES(TYPE_NODE));     type = TYPE_NODE
            case (TYPE_NAMES(TYPE_SENSOR));   type = TYPE_SENSOR
            case (TYPE_NAMES(TYPE_TARGET));   type = TYPE_TARGET
            case (TYPE_NAMES(TYPE_OBSERV));   type = TYPE_OBSERV
            case (TYPE_NAMES(TYPE_RESPONSE)); type = TYPE_RESPONSE
            case (TYPE_NAMES(TYPE_DF));       type = TYPE_DF
            case (TYPE_NAMES(TYPE_DP));       type = TYPE_DP
            case (TYPE_NAMES(TYPE_LOG));      type = TYPE_LOG
            case (TYPE_NAMES(TYPE_BEAT));     type = TYPE_BEAT
            case (TYPE_NAMES(TYPE_TRANSFER)); type = TYPE_TRANSFER
            case (TYPE_NAMES(TYPE_IMAGE));    type = TYPE_IMAGE
            case (TYPE_NAMES(TYPE_HEADER));   type = TYPE_HEADER
            case default;                     type = TYPE_NONE
        end select
    end function dm_type_from_name

    pure elemental logical function dm_type_is_valid(type) result(valid)
        !! Returns `.true.` if given type enumerator is valid. `TYPE_NONE` is
        !! an invalid type.
        integer, intent(in) :: type !! Type enumerator.

        valid = (type > TYPE_NONE .and. type <= TYPE_LAST)
    end function dm_type_is_valid
end module dm_type
