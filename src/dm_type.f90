! Author:  Philipp Engel
! Licence: ISC
module dm_type
    !! DMPACK derived type enumerators and utilities.
    implicit none (type, external)
    public

    ! Derived type enumeration.
    integer, parameter, public :: TYPE_NONE       = 0 !! Invalid type.
    integer, parameter, public :: TYPE_NODE       = 1 !! Node.
    integer, parameter, public :: TYPE_SENSOR     = 2 !! Sensor.
    integer, parameter, public :: TYPE_TARGET     = 3 !! Target.
    integer, parameter, public :: TYPE_OBSERV     = 4 !! Observation.
    integer, parameter, public :: TYPE_REQUEST    = 5 !! Request of observation.
    integer, parameter, public :: TYPE_RESPONSE   = 6 !! Response of request.
    integer, parameter, public :: TYPE_LOG        = 7 !! Log.
    integer, parameter, public :: TYPE_BEAT       = 8 !! Heartbeat.
    integer, parameter, public :: TYPE_DP         = 9 !! X/Y data point.
    integer, parameter, public :: TYPE_LAST       = 9 !! Never use this.

    integer, parameter, public :: TYPE_NAME_LEN = 8 !! Max. type name length.

    ! Derived type names.
    character(len=*), parameter, public :: TYPE_NAMES(TYPE_NONE:TYPE_LAST) = [ &
        character(len=TYPE_NAME_LEN) :: 'none', 'node', 'sensor', 'target', 'observ', &
        'request', 'response', 'log', 'beat', 'dp' ] !! Type names array.

    public :: dm_type_from_name
    public :: dm_type_valid
contains
    integer function dm_type_from_name(name) result(type)
        !! Returns type from given name. If `name` is longer than
        !! `TYPE_NAME_LEN`, only the characters from `1` to `TYPE_NAME_LEN` are
        !! compared.
        use :: dm_string, only: dm_lower
        character(len=*), intent(in) :: name !! Derived type name.
        character(len=TYPE_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_lower(name)

        select case (name_)
            case (TYPE_NAMES(TYPE_NODE))
                type = TYPE_NODE
            case (TYPE_NAMES(TYPE_SENSOR))
                type = TYPE_SENSOR
            case (TYPE_NAMES(TYPE_TARGET))
                type = TYPE_TARGET
            case (TYPE_NAMES(TYPE_OBSERV))
                type = TYPE_OBSERV
            case (TYPE_NAMES(TYPE_REQUEST))
                type = TYPE_REQUEST
            case (TYPE_NAMES(TYPE_RESPONSE))
                type = TYPE_RESPONSE
            case (TYPE_NAMES(TYPE_LOG))
                type = TYPE_LOG
            case (TYPE_NAMES(TYPE_BEAT))
                type = TYPE_BEAT
            case (TYPE_NAMES(TYPE_DP))
                type = TYPE_DP
            case default
                type = TYPE_NONE
        end select
    end function dm_type_from_name

    pure elemental logical function dm_type_valid(type) result(valid)
        !! Returns `.true.` if given type enumerator is valid. `TYPE_NONE` is
        !! an invalid type.
        integer, intent(in) :: type !! Type enumerator.

        valid = .false.
        if (type <= TYPE_NONE .or. type > TYPE_LAST) return
        valid = .true.
    end function dm_type_valid
end module dm_type
