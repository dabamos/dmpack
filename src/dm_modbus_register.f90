! Author:  Philipp Engel
! Licence: ISC
module dm_modbus_register
    !! Modbus register abstraction module.
    use :: dm_error
    use :: dm_kind
    use :: dm_modbus_type
    use :: dm_string
    implicit none (type, external)
    private

    type, public :: modbus_register_type
        !! Modbus register value type.
        integer :: access  = MODBUS_ACCESS_NONE !! Read or write access.
        integer :: slave   = 0                  !! Slave id.
        integer :: address = 0                  !! Register address.
        integer :: type    = MODBUS_TYPE_INT16  !! Number type.
        integer :: order   = MODBUS_ORDER_NONE  !! Byte order of float.
        integer :: scale   = 1                  !! Scale denominator.
        integer :: value   = 0                  !! Register value to write.
    end type modbus_register_type

    public :: dm_modbus_register_has_scale
    public :: dm_modbus_register_is_valid
    public :: dm_modbus_register_out
    public :: dm_modbus_register_parse
    public :: dm_modbus_register_scale
contains
    pure elemental logical function dm_modbus_register_has_scale(register) result(has)
        !! Returns `.true.` if register attribute `scale` is greater 1.
        type(modbus_register_type), intent(in) :: register !! Modbus register type.

        has = (register%scale > 1)
    end function dm_modbus_register_has_scale

    pure elemental logical function dm_modbus_register_is_valid(register) result(valid)
        !! Returns `.true.` if Modbus register type is valid.
        type(modbus_register_type), intent(in) :: register !! Modbus register type.

        valid = .false.

        if (register%slave < 1)   return
        if (register%address < 1) return

        if (.not. dm_modbus_access_is_valid(register%access)) return
        if (.not. dm_modbus_type_is_valid(register%type))     return
        if (.not. dm_modbus_order_is_valid(register%order) .and. &
            register%order /= MODBUS_ORDER_NONE) return

        valid = .true.
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
        !! case-insensitive, i.e., `INT16` equals `int16` and `ABCD` equals
        !! `abcd`.
        !!
        !! A string of parameters to read a register value as 2-byte signed
        !! integer and scale it by 1/10:
        !!
        !! ```text
        !! access=read,slave=10,address=40050,scale=10
        !! ```
        !!
        !! A string of parameters to read a float value in ABCD byte order:
        !!
        !! ```text
        !! access=read,slave=10,address=40060,type=float,order=abcd
        !! ```
        !!
        !! A string of parameters to write value `10` to register `30050`:
        !!
        !! ```text
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
end module dm_modbus_register
