! Author:  Philipp Engel
! Licence: ISC
module dm_type
    !! Type imports and definitions.
    use, intrinsic :: iso_fortran_env, only: i4     => int32, &
                                             i8     => int64, &
                                             r4     => real32, &
                                             r8     => real64, &
                                             stderr => error_unit, &
                                             stdin  => input_unit, &
                                             stdout => output_unit
    use :: dm_string
    implicit none (type, external)
    public

    ! Derived type enumeration.
    integer, parameter, public :: TYPE_NONE       =  0 !! Invalid type.
    integer, parameter, public :: TYPE_NODE       =  1 !! Node.
    integer, parameter, public :: TYPE_SENSOR     =  2 !! Sensor.
    integer, parameter, public :: TYPE_TARGET     =  3 !! Target.
    integer, parameter, public :: TYPE_OBSERV     =  4 !! Observation.
    integer, parameter, public :: TYPE_REQUEST    =  5 !! Request of observation.
    integer, parameter, public :: TYPE_RESPONSE   =  6 !! Response of request.
    integer, parameter, public :: TYPE_LOG        =  7 !! Log.
    integer, parameter, public :: TYPE_BEAT       =  8 !! Heartbeat.
    integer, parameter, public :: TYPE_DATA_POINT =  9 !! X/Y data point.
    integer, parameter, public :: TYPE_NTYPES     = 10 !! Number of types.

    integer, parameter, public :: TYPE_NAME_LEN = 8 !! Max. type name length.

    ! Derived type names.
    character(len=*), parameter, public :: TYPE_NAMES(0:TYPE_NTYPES - 1) = [ &
        character(len=TYPE_NAME_LEN) :: 'none', 'node', 'sensor', 'target', 'observ', &
        'request', 'response', 'log', 'beat', 'dp' ] !! Type names array.

    public :: dm_type_from_name
    public :: dm_type_valid
contains
    integer function dm_type_from_name(name) result(type)
        !! Returns type from given name.
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
            case (TYPE_NAMES(TYPE_DATA_POINT))
                type = TYPE_DATA_POINT
            case default
                type = TYPE_NONE
        end select
    end function dm_type_from_name

    pure elemental logical function dm_type_valid(type) result(valid)
        !! Returns `.true.` if given type enumerator is valid. `TYPE_NONE` is
        !! an invalid type.
        integer, intent(in) :: type !! Type enumerator.

        valid = .false.
        if (type <= TYPE_NONE .or. type >= TYPE_NTYPES) return
        valid = .true.
    end function dm_type_valid
end module dm_type
