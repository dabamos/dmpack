! Author:  Philipp Engel
! Licence: ISC
module dm_modbus_type
    !! Modbus types and utility procedures.
    use :: dm_error
    use :: dm_kind
    use :: dm_response
    use :: dm_string
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

    type, public :: modbus_register_type
        !! Modbus register value type.
        integer  :: access  = MODBUS_ACCESS_NONE !! Read or write access.
        integer  :: slave   = 0                  !! Slave id.
        integer  :: address = 0                  !! Register address.
        integer  :: type    = MODBUS_TYPE_INT16  !! Number type.
        integer  :: order   = MODBUS_ORDER_NONE  !! Byte order of float.
        integer  :: value   = 0                  !! Register value to write.
        integer  :: scale   = 1                  !! Scale denominator.
    end type modbus_register_type

    public :: dm_modbus_access_from_name
    public :: dm_modbus_access_is_valid
    public :: dm_modbus_mode_is_valid
    public :: dm_modbus_mode_from_name
    public :: dm_modbus_order_is_valid
    public :: dm_modbus_order_from_name
    public :: dm_modbus_register_has_scale
    public :: dm_modbus_register_is_valid
    public :: dm_modbus_register_out
    public :: dm_modbus_register_parse
    public :: dm_modbus_register_scale
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
            case default; mode = MODBUS_MODE_NONE
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

    pure elemental logical function dm_modbus_register_has_scale(register) result(has)
        !! Returns `.true.` if scale of register is not 0 or 1.
        type(modbus_register_type), intent(in) :: register !! Modbus register type.

        has = (register%scale /= 0 .and. register%scale /= 1)
    end function dm_modbus_register_has_scale

    pure elemental logical function dm_modbus_register_is_valid(register) result(is)
        !! Returns `.true.` if Modbus register type is valid.
        type(modbus_register_type), intent(in) :: register !! Modbus register type.

        is = .false.

        if (register%slave < 1)   return
        if (register%address < 1) return

        if (.not. dm_modbus_access_is_valid(register%access)) return
        if (.not. dm_modbus_type_is_valid(register%type))     return
        if (.not. dm_modbus_order_is_valid(register%order) .and. &
            register%order /= MODBUS_ORDER_NONE) return

        is = .true.
    end function dm_modbus_register_is_valid

    subroutine dm_modbus_register_out(register, unit)
        !! Outputs Modbus register type.
        use :: dm_util, only: dm_present

        type(modbus_register_type), intent(inout)        :: register !! Modbus register type.
        integer,                    intent(in), optional :: unit     !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, stdout)

        write (unit_, '("modbus_register.access: ", i0)')  register%access
        write (unit_, '("modbus_register.slave: ", i0)')   register%slave
        write (unit_, '("modbus_register.address: ", i0)') register%address
        write (unit_, '("modbus_register.type: ", i0)')    register%type
        write (unit_, '("modbus_register.order: ", i0)')   register%order
        write (unit_, '("modbus_register.scale: ", i0)')   register%scale
        write (unit_, '("modbus_register.value: ", i0)')   register%value
    end subroutine dm_modbus_register_out

    pure elemental subroutine dm_modbus_register_parse(string, register, error)
        !! Parses string for the following Modbus parameters and returns the
        !! values in `register`:
        !!
        !! | Parameter  | Type    | Description                                                    |
        !! |------------|---------|----------------------------------------------------------------|
        !! | `access`   | string  | Read from or write to register (`read`, `write`).              |
        !! | `slave`    | integer | Slave id.                                                      |
        !! | `address`  | integer | Register address.                                              |
        !! | `type`     | string  | Register type (`int16`, `int32`, `uint16`, `uint32`, `float`). |
        !! | `order`    | string  | Byte order ('none`, `abcd`, `badc`, `cdab`, `dcba`).           |
        !! | `scale`    | integer | Scale denominator (only for reading).                          |
        !! | `value`    | integer | Integer value (only for writing).                              |
        !!
        !! Key and value are separated by character `=`, parameter fields by
        !! character `,`. The parsing of parameter keys and values is
        !! case-insensitive, i.e., `INT16` equals `int16 and `ABCD` equals
        !! `abcd`.
        !!
        !! A string of parameters to read a register value as 2-byte signed
        !! integer and scale it by 1/10:
        !!
        !! ```
        !! access=read,slave=10,address=40050,scale=10
        !! ```
        !!
        !! A string of parameters to read a float value in ABCD byte order:
        !!
        !! ```
        !! access=read,slave=10,address=40060,type=float,order=abcd
        !! ```
        !!
        !! A string of parameters to write value `10` to register `30050`:
        !!
        !! ```
        !! access=write,slave=10,address=30050,value=10,type=int16
        !! ```
        !!
        !! The routine returns the following error codes in argument `error`:
        !!
        !! * `E_EMPTY` if the string is empty.
        !! * `E_FORMAT` if the string format is invalid.
        !! * `E_TYPE` if a parameter value type is invalid.
        !!
        character(len=*),           intent(in)            :: string   !! Input string.
        type(modbus_register_type), intent(out)           :: register !! Modbus register type.
        integer,                    intent(out), optional :: error    !! Error code.

        character(len=32) :: fields(6), pairs(2)
        character(len=32) :: key, value
        integer           :: i, nfields, npairs, rc

        parse_block: block
            rc = E_EMPTY
            if (len_trim(string) == 0) exit parse_block

            rc = E_FORMAT
            call dm_string_split(string, fields, del=',', n=nfields)
            if (nfields == 0) exit parse_block

            do i = 1, nfields
                rc = E_FORMAT
                call dm_string_split(fields(i), pairs, del='=', n=npairs)
                if (npairs /= 2) exit parse_block

                call dm_lower(pairs)

                key   = adjustl(pairs(1))
                value = adjustl(pairs(2))

                rc = E_TYPE
                select case (key)
                    case ('access')
                        register%access = dm_modbus_access_from_name(value)
                        if (.not. dm_modbus_access_is_valid(register%access)) exit parse_block

                    case ('slave')
                        call dm_string_to(value, register%slave, error=rc)
                        if (dm_is_error(rc)) exit parse_block

                    case ('address')
                        call dm_string_to(value, register%address, error=rc)
                        if (dm_is_error(rc)) exit parse_block

                    case ('type')
                        register%type = dm_modbus_type_from_name(value)

                    case ('order')
                        register%order = dm_modbus_order_from_name(value)

                    case ('scale')
                        call dm_string_to(value, register%scale, error=rc)
                        if (dm_is_error(rc)) exit parse_block

                    case ('value')
                        call dm_string_to(value, register%value, error=rc)
                        if (dm_is_error(rc)) exit parse_block

                    case default
                        cycle
                end select
            end do

            rc = E_NONE
        end block parse_block

        if (present(error)) error = rc
    end subroutine dm_modbus_register_parse

    pure elemental subroutine dm_modbus_register_scale(register, value)
        !! Scales given value by scale denominator in register. For example, if
        !! attribute `scale` is set to 10, the routine will device `value` by
        !! 10. If `scale` is 0 or 1, the value will not be modified.
        type(modbus_register_type), intent(inout) :: register !! Modbus register type.
        real(kind=r8),              intent(inout) :: value    !! Value to scale.

        if (.not. dm_modbus_register_has_scale(register)) return
        value = value / register%scale
    end subroutine dm_modbus_register_scale

    pure elemental logical function dm_modbus_type_is_valid(type) result(is)
        !! Returns `.true.` if the given Modbus number type is valid.
        !! `MODBUS_TYPE_NONE` is invalid.
        integer, intent(in) :: type !! Modbus number type.

        is = (type >= MODBUS_TYPE_INT16 .and. type <= MODBUS_TYPE_LAST)
    end function dm_modbus_type_is_valid

    pure elemental integer function dm_modbus_type_from_name(name) result(type)
        !! Returns Modbus number type from given name. If `name` is invalid,
        !! `MODBUS_TYPE_NONE` is returned.
        use :: dm_string, only: dm_to_lower

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
