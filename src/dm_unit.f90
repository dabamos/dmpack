! Author:  Philipp Engel
! Licence: ISC
module dm_unit
    !! Unit definitions and conversion functions.
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: UNIT_NONE = 0
    integer, parameter, public :: UNIT_RAD  = 1
    integer, parameter, public :: UNIT_GON  = 2
    integer, parameter, public :: UNIT_DEG  = 3

    type, public :: unit_type
        !! Generic unit type.
        real(kind=r8) :: value = real(0.0, kind=r8)
        integer       :: type  = UNIT_NONE
        integer       :: error = E_NONE
    end type unit_type

    type, extends(unit_type), public :: angle_type
        !! Angle type.
    end type angle_type

    public :: dm_unit_to_deg
    public :: dm_unit_to_gon
    public :: dm_unit_to_rad
    public :: dm_unit_type
    public :: dm_unit_value
contains
    pure elemental function dm_unit_to_deg(a) result(b)
        !! Converts angle to degrees.
        type(angle_type), intent(in) :: a
        type(angle_type)             :: b

        b%type = UNIT_DEG

        select case (a%type)
            case (UNIT_RAD)
                b%value = dm_rad_to_deg(a%value)
            case (UNIT_GON)
                b%value = dm_gon_to_deg(a%value)
            case (UNIT_DEG)
                b%value = a%value
            case default
                b%error = E_TYPE
        end select
    end function dm_unit_to_deg

    pure elemental function dm_unit_to_gon(a) result(b)
        !! Converts angle to gon.
        type(angle_type), intent(in) :: a
        type(angle_type)             :: b

        b%type = UNIT_GON

        select case (a%type)
            case (UNIT_RAD)
                b%value = dm_rad_to_gon(a%value)
            case (UNIT_GON)
                b%value = a%value
            case (UNIT_DEG)
                b%value = dm_deg_to_gon(a%value)
            case default
                b%error = E_TYPE
        end select
    end function dm_unit_to_gon

    pure elemental function dm_unit_to_rad(a) result(b)
        !! Converts angle to radiants.
        type(angle_type), intent(in) :: a
        type(angle_type)             :: b

        b%type = UNIT_RAD

        select case (a%type)
            case (UNIT_RAD)
                b%value = a%value
            case (UNIT_GON)
                b%value = dm_gon_to_rad(a%value)
            case (UNIT_DEG)
                b%value = dm_deg_to_rad(a%value)
            case default
                b%error = E_TYPE
        end select
    end function dm_unit_to_rad

    pure elemental function dm_unit_type(a) result(type)
        !! Returns unit type.
        type(unit_type), intent(in) :: a
        integer(kind=i8)            :: type

        type = a%type
    end function dm_unit_type

    pure elemental function dm_unit_value(a) result(value)
        !! Returns unit value.
        type(unit_type), intent(in) :: a
        real(kind=r8)               :: value

        value = a%value
    end function dm_unit_value
end module dm_unit
