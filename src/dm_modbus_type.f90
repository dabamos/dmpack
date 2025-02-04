! Author:  Philipp Engel
! Licence: ISC
module dm_modbus_type
    !! Modbus types and utility procedures.
    use :: dm_error
    use :: dm_kind
    use :: dm_string
    implicit none (type, external)
    private

    ! Action enumerators.
    integer, parameter, public :: MODBUS_ACTION_NONE  = 0 !! No action (invalid).
    integer, parameter, public :: MODBUS_ACTION_READ  = 1 !! Read action.
    integer, parameter, public :: MODBUS_ACTION_WRITE = 2 !! Write action.

    integer, parameter, public :: MODBUS_ACTION_NAME_LEN = 5 !! Action string length.
    integer, parameter, public :: MODBUS_ORDER_NAME_LEN  = 4 !! Byte order string length.

    ! Byte orders of 4-byte real values.
    integer, parameter, public :: MODBUS_REAL_NONE = 0 !! None (integer or invalid).
    integer, parameter, public :: MODBUS_REAL_ABCD = 1 !! ABCD byte order.
    integer, parameter, public :: MODBUS_REAL_BADC = 2 !! BADC byte order.
    integer, parameter, public :: MODBUS_REAL_CDAB = 3 !! CDBA byte order.
    integer, parameter, public :: MODBUS_REAL_DCBA = 4 !! DCBA byte order.

    type, public :: modbus_register_type
        integer          :: action     = MODBUS_ACTION_NONE !! Read or write action.
        integer          :: slave      = 0                  !! Slave device number.
        integer          :: address    = 0                  !! Register address.
        integer(kind=u2) :: value      = 0                  !! Register value to write.
        integer          :: byte_order = MODBUS_REAL_NONE   !! Float byte order.
    end type modbus_register_type

    public :: dm_modbus_action_from_name
    public :: dm_modbus_byte_order_from_name
    public :: dm_modbus_is_valid_action
    public :: dm_modbus_is_valid_byte_order
    public :: dm_modbus_parse
    public :: dm_modbus_register_out
contains
    pure integer function dm_modbus_action_from_name(name) result(action)
        !! Returns action enumerator from string. Returns `MODBUS_ACTION_NONE`
        !! on error.
        character(len=*), intent(in) :: name !! Input string.

        character(len=MODBUS_ACTION_NAME_LEN) :: name_

        name_ = dm_to_lower(name)

        select case (name_)
            case ('read');  action = MODBUS_ACTION_READ
            case ('write'); action = MODBUS_ACTION_WRITE
            case default;   action = MODBUS_ACTION_NONE
        end select
    end function dm_modbus_action_from_name

    pure integer function dm_modbus_byte_order_from_name(name) result(byte_order)
        !! Returns byte order named parameter associated with given string.
        !! For example, the result will be `MODBUS_REAL_ACBD` if `name` is
        !! `ABCD` (case-insensitive). Returns `MODBUS_REAL_NONE` if the string
        !! is invalid.
        character(len=*), intent(in) :: name !! Input string.

        character(len=MODBUS_ORDER_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case ('abcd'); byte_order = MODBUS_REAL_ABCD
            case ('badc'); byte_order = MODBUS_REAL_BADC
            case ('cdab'); byte_order = MODBUS_REAL_CDAB
            case ('dcba'); byte_order = MODBUS_REAL_DCBA
            case default;  byte_order = MODBUS_REAL_NONE
        end select
    end function dm_modbus_byte_order_from_name

    pure elemental logical function dm_modbus_is_valid_action(action) result(is)
        !! Returns `.true.` if action is a valid enumerator.
        !! `MODBUS_ACTION_NONE` is invalid.
        integer, intent(in) :: action !! Modbus action enumerator.

        is = (action == MODBUS_ACTION_READ .or. action == MODBUS_ACTION_WRITE)
    end function dm_modbus_is_valid_action

    pure elemental logical function dm_modbus_is_valid_byte_order(byte_order) result(is)
        !! Returns `.true.` if byte order is a valid enumerator.
        !! `MODBUS_REAL_NONE` is invalid.
        integer, intent(in) :: byte_order !! Modbus byte order enumerator.

        is = (byte_order == MODBUS_REAL_ABCD .or. &
              byte_order == MODBUS_REAL_BADC .or. &
              byte_order == MODBUS_REAL_CDAB .or. &
              byte_order == MODBUS_REAL_DCBA)
    end function dm_modbus_is_valid_byte_order

    pure elemental subroutine dm_modbus_parse(string, register, error)
        !! Parses string for the following Modbus parameters and returns the
        !! values in `register`:
        !!
        !! | Parameter  | Type    | Description                                                         |
        !! |------------|---------|---------------------------------------------------------------------|
        !! | `action`   | string  | Read from or write to register (`read`, `write`).                   |
        !! | `slave`    | integer | Slave number.                                                       |
        !! | `address`  | integer | Register address.                                                   |
        !! | `value`    | integer | Register integer value (only for writing).                          |
        !! | `order`    | string  | Byte order of float value ('none`, `abcd`, `badc`, `cdab`, `dcba`). |
        !!
        !! Key and value are separated by character `=`, parameter fields by
        !! character `,`. The parsing of parameter keys and values is
        !! case-insensitive, i.e., `ABCD` equals `abcd`.
        !!
        !! A string of parameters to read a register value:
        !!
        !! ```
        !! action=read,slave=10,address=50
        !! ```
        !!
        !! A string of parameters to read a float value in ABCD byte order:
        !!
        !! ```
        !! action=read,slave=10,address=60,order=abcd
        !! ```
        !!
        !! A string of parameters to write value `10` to register `50`:
        !!
        !! ```
        !! action=write,slave=10,address=50,value=10
        !! ```
        !!
        !! The routine returns the following error codes in argument `error`:
        !!
        !! * `E_EMPTY` if the string is empty.
        !! * `E_FORMAT` if the string format is invalid.
        !! * `E_TYPE` if a parameter value type is invalid.
        !!
        character(len=*),           intent(in)            :: string   !! Input string.
        type(modbus_register_type), intent(out)           :: register !! Modbus I/O type.
        integer,                    intent(out), optional :: error    !! Error code.

        character(len=32) :: fields(6), pair(2)
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
                call dm_string_split(fields(i), pair, del='=', n=npairs)
                if (npairs /= 2) exit parse_block

                call dm_lower(pair)

                key   = adjustl(pair(1))
                value = adjustl(pair(2))

                rc = E_TYPE
                select case (key)
                    case ('action')
                        register%action = dm_modbus_action_from_name(value)
                        if (.not. dm_modbus_is_valid_action(register%action)) exit parse_block

                    case ('slave')
                        call dm_string_to(value, register%slave, error=rc)
                        if (dm_is_error(rc)) exit parse_block

                    case ('address')
                        call dm_string_to(value, register%address, error=rc)
                        if (dm_is_error(rc)) exit parse_block

                    case ('value')
                        call dm_string_to(value, register%value, error=rc)
                        if (dm_is_error(rc)) exit parse_block

                    case ('order')
                        register%byte_order = dm_modbus_byte_order_from_name(value)

                    case default
                        cycle
                end select
            end do

            rc = E_NONE
        end block parse_block

        if (present(error)) error = rc
    end subroutine dm_modbus_parse

    subroutine dm_modbus_register_out(register, unit)
        type(modbus_register_type), intent(inout)        :: register !! Modbus register type.
        integer,                    intent(in), optional :: unit     !! File unit.

        integer :: i, unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        write (unit_, '("modbus_register.action: ", i0)')     register%action
        write (unit_, '("modbus_register.slave: ", i0)')      register%slave
        write (unit_, '("modbus_register.address: ", i0)')    register%address
        write (unit_, '("modbus_register.value: ", i0)')      register%value
        write (unit_, '("modbus_register.byte_order: ", i0)') register%byte_order
    end subroutine dm_modbus_register_out
end module dm_modbus_type
