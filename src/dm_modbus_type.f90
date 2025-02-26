! Author:  Philipp Engel
! Licence: ISC
module dm_modbus_type
    !! Modbus types and type functions.
    use :: dm_error
    use :: dm_kind
    use :: dm_string, only: dm_to_lower
    implicit none (type, external)
    private

    ! Access enumerators.
    integer, parameter, public :: MODBUS_ACCESS_NONE  = 0 !! No access (invalid).
    integer, parameter, public :: MODBUS_ACCESS_READ  = 1 !! Read access.
    integer, parameter, public :: MODBUS_ACCESS_WRITE = 2 !! Write access.

    ! Modbus mode.
    integer, parameter, public :: MODBUS_MODE_NONE = 0 !! Invalid mode.
    integer, parameter, public :: MODBUS_MODE_RTU  = 1 !! Modbus RTU.
    integer, parameter, public :: MODBUS_MODE_TCP  = 2 !! Modbus TCP.

    ! Byte order of 4-byte real values.
    integer, parameter, public :: MODBUS_ORDER_NONE = 0 !! None (integer or invalid).
    integer, parameter, public :: MODBUS_ORDER_ABCD = 1 !! ABCD byte order.
    integer, parameter, public :: MODBUS_ORDER_BADC = 2 !! BADC byte order.
    integer, parameter, public :: MODBUS_ORDER_CDAB = 3 !! CDBA byte order.
    integer, parameter, public :: MODBUS_ORDER_DCBA = 4 !! DCBA byte order.

    ! Modbus number types.
    integer, parameter, public :: MODBUS_TYPE_NONE    = 0                 !! None (invalid).
    integer, parameter, public :: MODBUS_TYPE_INT16   = 1                 !! 2-byte signed integer.
    integer, parameter, public :: MODBUS_TYPE_INT32   = 2                 !! 2-byte signed integer.
    integer, parameter, public :: MODBUS_TYPE_UINT16  = 3                 !! 2-byte unsigned integer.
    integer, parameter, public :: MODBUS_TYPE_UINT32  = 4                 !! 4-byte unsigned integer.
    integer, parameter, public :: MODBUS_TYPE_FLOAT   = 5                 !! 4-byte float.
    integer, parameter, public :: MODBUS_TYPE_LAST    = 5                 !! Never use this.
    integer, parameter, public :: MODBUS_TYPE_DEFAULT = MODBUS_TYPE_INT16 !! Default number type.

    ! String lengths.
    integer, parameter, public :: MODBUS_ACCESS_NAME_LEN = 5 !! Max. access name length.
    integer, parameter, public :: MODBUS_MODE_NAME_LEN   = 4 !! Max. mode name length.
    integer, parameter, public :: MODBUS_ORDER_NAME_LEN  = 4 !! Max. byte order name length.
    integer, parameter, public :: MODBUS_TYPE_NAME_LEN   = 6 !! Max. number type name length.

    character(len=*), parameter, public :: MODBUS_MODE_NAMES(MODBUS_MODE_NONE:MODBUS_MODE_TCP) = [ &
        character(len=MODBUS_MODE_NAME_LEN) :: 'none', 'rtu', 'tcp' &
    ] !! Modbus mode names.

    character(len=*), parameter, public :: MODBUS_TYPE_NAMES(MODBUS_TYPE_NONE:MODBUS_TYPE_LAST) = [ &
        character(len=MODBUS_TYPE_NAME_LEN) :: 'none', 'int16', 'int32', 'uint16', 'uint32', 'float' &
    ] !! Modbus number type names.

    public :: dm_modbus_access_from_name
    public :: dm_modbus_access_is_valid
    public :: dm_modbus_mode_is_valid
    public :: dm_modbus_mode_from_name
    public :: dm_modbus_order_is_valid
    public :: dm_modbus_order_from_name
    public :: dm_modbus_type_from_name
    public :: dm_modbus_type_is_valid
contains
    pure integer function dm_modbus_access_from_name(name) result(access)
        !! Returns access enumerator from string or `MODBUS_ACCESS_NONE` on
        !! error.
        character(len=*), intent(in) :: name !! Input string.

        character(len=MODBUS_ACCESS_NAME_LEN) :: name_

        name_ = dm_to_lower(name)

        select case (name_)
            case ('read');  access = MODBUS_ACCESS_READ
            case ('write'); access = MODBUS_ACCESS_WRITE
            case default;   access = MODBUS_ACCESS_NONE
        end select
    end function dm_modbus_access_from_name

    pure elemental logical function dm_modbus_access_is_valid(access) result(is)
        !! Returns `.true.` if access is a valid enumerator.
        !! `MODBUS_ACCESS_NONE` is invalid.
        integer, intent(in) :: access !! Modbus access enumerator.

        is = (access == MODBUS_ACCESS_READ .or. access == MODBUS_ACCESS_WRITE)
    end function dm_modbus_access_is_valid

    pure integer function dm_modbus_mode_from_name(name) result(mode)
        !! Returns mode enumerator from string or `MODBUS_MODE_NONE` on
        !! error.
        character(len=*), intent(in) :: name !! Input string.

        character(len=MODBUS_MODE_NAME_LEN) :: name_

        name_ = dm_to_lower(name)

        select case (name_)
            case (MODBUS_MODE_NAMES(MODBUS_MODE_RTU)); mode = MODBUS_MODE_RTU
            case (MODBUS_MODE_NAMES(MODBUS_MODE_TCP)); mode = MODBUS_MODE_TCP
            case default;                              mode = MODBUS_MODE_NONE
        end select
    end function dm_modbus_mode_from_name

    pure elemental logical function dm_modbus_mode_is_valid(mode) result(is)
        !! Returns `.true.` if mode is a valid enumerator. `MODBUS_MODE_NONE`
        !! is invalid.
        integer, intent(in) :: mode !! Modbus mode enumerator.

        is = (mode == MODBUS_MODE_RTU .or. mode == MODBUS_MODE_TCP)
    end function dm_modbus_mode_is_valid

    pure integer function dm_modbus_order_from_name(name) result(order)
        !! Returns byte order named parameter associated with given string.
        !! For example, the result will be `MODBUS_ORDER_ACBD` if `name` is
        !! `ABCD` (case-insensitive). Returns `MODBUS_ORDER_NONE` if the string
        !! is invalid.
        character(len=*), intent(in) :: name !! Input string.

        character(len=MODBUS_ORDER_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case ('abcd'); order = MODBUS_ORDER_ABCD
            case ('badc'); order = MODBUS_ORDER_BADC
            case ('cdab'); order = MODBUS_ORDER_CDAB
            case ('dcba'); order = MODBUS_ORDER_DCBA
            case default;  order = MODBUS_ORDER_NONE
        end select
    end function dm_modbus_order_from_name

    pure elemental logical function dm_modbus_order_is_valid(order) result(is)
        !! Returns `.true.` if argument is a valid float byte order enumerator.
        !! `MODBUS_ORDER_NONE` is not a valid byte order.
        integer, intent(in) :: order !! Modbus byte order enumerator.

        is = (order == MODBUS_ORDER_ABCD .or. &
              order == MODBUS_ORDER_BADC .or. &
              order == MODBUS_ORDER_CDAB .or. &
              order == MODBUS_ORDER_DCBA)
    end function dm_modbus_order_is_valid

    pure elemental logical function dm_modbus_type_is_valid(type) result(is)
        !! Returns `.true.` if the given Modbus number type is valid.
        !! `MODBUS_TYPE_NONE` is invalid.
        integer, intent(in) :: type !! Modbus number type.

        is = (type >= MODBUS_TYPE_INT16 .and. type <= MODBUS_TYPE_LAST)
    end function dm_modbus_type_is_valid

    pure elemental integer function dm_modbus_type_from_name(name) result(type)
        !! Returns Modbus number type from given name. If `name` is invalid,
        !! `MODBUS_TYPE_NONE` is returned.
        character(len=*), intent(in) :: name !! Modbus type name.

        character(len=MODBUS_TYPE_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (MODBUS_TYPE_NAMES(MODBUS_TYPE_INT16));  type = MODBUS_TYPE_INT16
            case (MODBUS_TYPE_NAMES(MODBUS_TYPE_INT32));  type = MODBUS_TYPE_INT32
            case (MODBUS_TYPE_NAMES(MODBUS_TYPE_UINT16)); type = MODBUS_TYPE_UINT16
            case (MODBUS_TYPE_NAMES(MODBUS_TYPE_UINT32)); type = MODBUS_TYPE_UINT32
            case (MODBUS_TYPE_NAMES(MODBUS_TYPE_FLOAT));  type = MODBUS_TYPE_FLOAT
            case default;                                 type = MODBUS_TYPE_NONE
        end select
    end function dm_modbus_type_from_name
end module dm_modbus_type
