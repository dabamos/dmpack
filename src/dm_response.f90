! Author:  Philipp Engel
! Licence: ISC
module dm_response
    !! The observation response data derived type declaration.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! Response types.
    integer, parameter, public :: RESPONSE_TYPE_REAL64  = 0 !! 8-byte signed real.
    integer, parameter, public :: RESPONSE_TYPE_REAL32  = 1 !! 4-byte signed real.
    integer, parameter, public :: RESPONSE_TYPE_INT64   = 2 !! 8-byte signed integer.
    integer, parameter, public :: RESPONSE_TYPE_INT32   = 3 !! 4-byte signed integer.
    integer, parameter, public :: RESPONSE_TYPE_LOGICAL = 4 !! Boolean.
    integer, parameter, public :: RESPONSE_TYPE_BYTE    = 5 !! Byte.
    integer, parameter, public :: RESPONSE_TYPE_STRING  = 6 !! Byte string.
    integer, parameter, public :: RESPONSE_TYPE_LAST    = 6 !! Never use this.

    integer, parameter, public :: RESPONSE_TYPE_DEFAULT  = RESPONSE_TYPE_REAL64 !! Default response type.
    integer, parameter, public :: RESPONSE_TYPE_NAME_LEN = 7                    !! Max. response type name length.

    character(*), parameter, public :: RESPONSE_TYPE_NAMES(RESPONSE_TYPE_REAL64:RESPONSE_TYPE_LAST) = [ &
        character(RESPONSE_TYPE_NAME_LEN) :: &
        'real64', 'real32', 'int64', 'int32', 'logical', 'byte', 'string' &
    ] !! Response value type names.

    integer, parameter, public :: RESPONSE_NAME_LEN = 32 !! Max. response name length.
    integer, parameter, public :: RESPONSE_UNIT_LEN = 8  !! Max. response unit length.

    type, public :: response_type
        !! Response of a sensor.
        character(RESPONSE_NAME_LEN) :: name  = ' '                   !! Response name (`-0-9A-Z_a-z`).
        character(RESPONSE_UNIT_LEN) :: unit  = ' '                   !! Response unit.
        integer                      :: type  = RESPONSE_TYPE_DEFAULT !! Response value type.
        integer                      :: error = E_NONE                !! Response error.
        real(r8)                     :: value = 0.0_r8                !! Response value.
    end type response_type

    integer, parameter, public :: RESPONSE_TYPE_SIZE = storage_size(response_type()) / 8 !! Size of `response_type` in bytes.

    interface operator (==)
        !! Returns `.true.` if requests are equal.
        module procedure :: dm_response_equals
    end interface

    public :: operator (==)

    public :: dm_response_equals
    public :: dm_response_get
    public :: dm_response_is_valid
    public :: dm_response_out
    public :: dm_response_set
    public :: dm_response_type_is_valid
    public :: dm_response_type_to_name
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental logical function dm_response_equals(response1, response2) result(equals)
        !! Returns `.true.` if given responses are equal.
        type(response_type), intent(in) :: response1 !! The first response.
        type(response_type), intent(in) :: response2 !! The second response.

        equals = .false.
        if (response1%name  /= response2%name)  return
        if (response1%unit  /= response2%unit)  return
        if (response1%type  /= response2%type)  return
        if (response1%error /= response2%error) return

        if (.not. dm_equals(response1%value, response2%value)) return
        equals = .true.
    end function dm_response_equals

    pure elemental logical function dm_response_is_valid(response) result(valid)
        !! Returns `.true.` if given response is valid. A response is valid if
        !! attribute _name_ is a valid id, attribute _type_ is a valid response
        !! value type, and attribute _error_ is a valid error code.
        use :: dm_string, only: dm_string_is_printable

        type(response_type), intent(in) :: response !! Response type.

        valid = .false.
        if (.not. dm_id_is_valid(response%name))            return
        if (.not. dm_string_is_printable(response%unit))    return
        if (.not. dm_response_type_is_valid(response%type)) return
        if (.not. dm_error_is_valid(response%error))        return
        valid = .true.
    end function dm_response_is_valid

    subroutine dm_response_out(response, unit)
        !! Prints response to standard output or given file unit.
        type(response_type), intent(inout)        :: response
        integer,             intent(in), optional :: unit

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("response.name: ", a)')        trim(response%name)
        write (unit_, '("response.unit: ", a)')        trim(response%unit)
        write (unit_, '("response.type: ", i0)')       response%type
        write (unit_, '("response.error: ", i0)')      response%error
        write (unit_, '("response.value: ", 1pg0.12)') response%value
    end subroutine dm_response_out

    pure elemental logical function dm_response_type_is_valid(type) result(valid)
        !! Returns `.true.` if the given response value type is valid.
        integer, intent(in) :: type !! Response value type.

        valid = (type >= RESPONSE_TYPE_REAL64 .and. type <= RESPONSE_TYPE_LAST)
    end function dm_response_type_is_valid

    pure function dm_response_type_to_name(type) result(name)
        !! Returns allocatable string of response value type name, or `invalid`
        !! if the type is invalid.
        integer, intent(in)       :: type !! Response value type.
        character(:), allocatable :: name !! Response value type name.

        if (.not. dm_response_type_is_valid(type)) then
            name = 'invalid'
            return
        end if

        name = trim(RESPONSE_TYPE_NAMES(type))
    end function dm_response_type_to_name

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    pure elemental subroutine dm_response_get(response, name, unit, type, error, value)
        !! Gets attributes of response type.
        type(response_type),          intent(inout)         :: response !! Response type.
        character(RESPONSE_NAME_LEN), intent(out), optional :: name     !! Name.
        character(RESPONSE_UNIT_LEN), intent(out), optional :: unit     !! Unit.
        integer,                      intent(out), optional :: type     !! Value type.
        integer,                      intent(out), optional :: error    !! Error code.
        real(r8),                     intent(out), optional :: value    !! Value.

        if (present(name))  name  = response%name
        if (present(unit))  unit  = response%unit
        if (present(type))  type  = response%type
        if (present(error)) error = response%error
        if (present(value)) value = response%value
    end subroutine dm_response_get

    pure elemental subroutine dm_response_set(response, name, unit, type, error, value)
        !! Sets attributes of response type.
        type(response_type), intent(inout)        :: response !! Response type.
        character(*),        intent(in), optional :: name     !! Name.
        character(*),        intent(in), optional :: unit     !! Unit.
        integer,             intent(in), optional :: type     !! Value type.
        integer,             intent(in), optional :: error    !! Error code.
        real(r8),            intent(in), optional :: value    !! Value.

        if (present(name))  response%name  = name
        if (present(unit))  response%unit  = unit
        if (present(type))  response%type  = type
        if (present(error)) response%error = error
        if (present(value)) response%value = value
    end subroutine dm_response_set
end module dm_response
