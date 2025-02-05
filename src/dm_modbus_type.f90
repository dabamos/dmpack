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

    ! Byte orders of 4-byte real values.
    integer, parameter, public :: MODBUS_FLOAT_NONE = 0 !! None (integer or invalid).
    integer, parameter, public :: MODBUS_FLOAT_ABCD = 1 !! ABCD byte order.
    integer, parameter, public :: MODBUS_FLOAT_BADC = 2 !! BADC byte order.
    integer, parameter, public :: MODBUS_FLOAT_CDAB = 3 !! CDBA byte order.
    integer, parameter, public :: MODBUS_FLOAT_DCBA = 4 !! DCBA byte order.

    ! Access enumerators.
    integer, parameter, public :: MODBUS_ACCESS_NONE  = 0 !! No access (invalid).
    integer, parameter, public :: MODBUS_ACCESS_READ  = 1 !! Read access.
    integer, parameter, public :: MODBUS_ACCESS_WRITE = 2 !! Write access.

    integer, parameter, public :: MODBUS_ACCESS_NAME_LEN = 5 !! Access name length.
    integer, parameter, public :: MODBUS_FLOAT_NAME_LEN  = 4 !! Byte order name length.

    integer, parameter, public :: MODBUS_REGISTER_NAME_LEN = RESPONSE_NAME_LEN
    integer, parameter, public :: MODBUS_REGISTER_UNIT_LEN = RESPONSE_UNIT_LEN

    type, public :: modbus_register_type
        integer                                 :: access  = MODBUS_ACCESS_NONE !! Read or write access.
        integer                                 :: slave   = 0                  !! Slave id.
        integer                                 :: address = 0                  !! Register address.
        integer(kind=u2)                        :: value   = 0                  !! Register value to write.
        integer                                 :: float   = MODBUS_FLOAT_NONE  !! Number type.
        character(len=MODBUS_REGISTER_NAME_LEN) :: name    = ' '                !! Register name.
        character(len=MODBUS_REGISTER_UNIT_LEN) :: unit    = ' '                !! Register value unit.
    end type modbus_register_type

    public :: dm_modbus_access_from_name
    public :: dm_modbus_access_is_valid
    public :: dm_modbus_float_from_name
    public :: dm_modbus_float_is_valid
    public :: dm_modbus_parse
    public :: dm_modbus_register_out
contains
    pure integer function dm_modbus_access_from_name(name) result(access)
        !! Returns access enumerator from string. Returns `MODBUS_ACCESS_NONE`
        !! on error.
        character(len=*), intent(in) :: name !! Input string.

        character(len=MODBUS_ACCESS_NAME_LEN) :: name_

        name_ = dm_to_lower(name)

        select case (name_)
            case ('read');  access = MODBUS_ACCESS_READ
            case ('write'); access = MODBUS_ACCESS_WRITE
            case default;   access = MODBUS_ACCESS_NONE
        end select
    end function dm_modbus_access_from_name

    pure elemental logical function dm_modbus_access_is_valid(access) result(valid)
        !! Returns `.true.` if access is a valid enumerator.
        !! `MODBUS_ACCESS_NONE` is invalid.
        integer, intent(in) :: access !! Modbus access enumerator.

        valid = (access == MODBUS_ACCESS_READ .or. access == MODBUS_ACCESS_WRITE)
    end function dm_modbus_access_is_valid

    pure integer function dm_modbus_float_from_name(name) result(float)
        !! Returns byte order named parameter associated with given string.
        !! For example, the result will be `MODBUS_FLOAT_ACBD` if `name` is
        !! `ABCD` (case-insensitive). Returns `MODBUS_FLOAT_NONE` if the string
        !! is invalid.
        character(len=*), intent(in) :: name !! Input string.

        character(len=MODBUS_FLOAT_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case ('abcd'); float = MODBUS_FLOAT_ABCD
            case ('badc'); float = MODBUS_FLOAT_BADC
            case ('cdab'); float = MODBUS_FLOAT_CDAB
            case ('dcba'); float = MODBUS_FLOAT_DCBA
            case default;  float = MODBUS_FLOAT_NONE
        end select
    end function dm_modbus_float_from_name

    pure elemental logical function dm_modbus_float_is_valid(float) result(valid)
        !! Returns `.true.` if byte order is a valid enumerator.
        !! `MODBUS_FLOAT_NONE` is invalid.
        integer, intent(in) :: float !! Modbus float enumerator.

        valid = (float == MODBUS_FLOAT_ABCD .or. &
                 float == MODBUS_FLOAT_BADC .or. &
                 float == MODBUS_FLOAT_CDAB .or. &
                 float == MODBUS_FLOAT_DCBA)
    end function dm_modbus_float_is_valid

    pure elemental subroutine dm_modbus_parse(string, register, error)
        !! Parses string for the following Modbus parameters and returns the
        !! values in `register`:
        !!
        !! | Parameter  | Type    | Description                                           |
        !! |------------|---------|-------------------------------------------------------|
        !! | `access`   | string  | Read from or write to register (`read`, `write`).     |
        !! | `slave`    | integer | Slave number.                                         |
        !! | `address`  | integer | Register address.                                     |
        !! | `value`    | integer | Register integer value (only for writing).            |
        !! | `float`    | string  | Number type ('none`, `abcd`, `badc`, `cdab`, `dcba`). |
        !!
        !! Key and value are separated by character `=`, parameter fields by
        !! character `,`. The parsing of parameter keys and values is
        !! case-insensitive, i.e., `ABCD` equals `abcd`.
        !!
        !! A string of parameters to read a register value:
        !!
        !! ```
        !! access=read,slave=10,address=50
        !! ```
        !!
        !! A string of parameters to read a float value in ABCD byte order:
        !!
        !! ```
        !! access=read,slave=10,address=60,float=abcd
        !! ```
        !!
        !! A string of parameters to write value `10` to register `50`:
        !!
        !! ```
        !! access=write,slave=10,address=50,value=10
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
                    case ('access')
                        register%access = dm_modbus_access_from_name(value)
                        if (.not. dm_modbus_access_is_valid(register%access)) exit parse_block

                    case ('slave')
                        call dm_string_to(value, register%slave, error=rc)
                        if (dm_is_error(rc)) exit parse_block

                    case ('address')
                        call dm_string_to(value, register%address, error=rc)
                        if (dm_is_error(rc)) exit parse_block

                    case ('value')
                        call dm_string_to(value, register%value, error=rc)
                        if (dm_is_error(rc)) exit parse_block

                    case ('float')
                        register%float= dm_modbus_float_from_name(value)

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

        write (unit_, '("modbus_register.access: ", i0)')  register%access
        write (unit_, '("modbus_register.slave: ", i0)')   register%slave
        write (unit_, '("modbus_register.address: ", i0)') register%address
        write (unit_, '("modbus_register.value: ", i0)')   register%value
        write (unit_, '("modbus_register.float: ", i0)')   register%float
        write (unit_, '("modbus_register.name: ", a)')     trim(register%name)
        write (unit_, '("modbus_register.unit: ", a)')     trim(register%unit)
    end subroutine dm_modbus_register_out
end module dm_modbus_type
