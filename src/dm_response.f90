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

    integer, parameter, public :: RESPONSE_NAME_LEN = 8 !! Max. response name length.
    integer, parameter, public :: RESPONSE_UNIT_LEN = 8 !! Max. response unit length.

    integer, parameter, public :: RESPONSE_TYPE_REAL64  = 0 !! 8-byte signed real.
    integer, parameter, public :: RESPONSE_TYPE_REAL32  = 1 !! 4-byte signed real.
    integer, parameter, public :: RESPONSE_TYPE_INT64   = 2 !! 8-byte signed integer.
    integer, parameter, public :: RESPONSE_TYPE_INT32   = 3 !! 4-byte signed integer.
    integer, parameter, public :: RESPONSE_TYPE_LOGICAL = 4 !! Boolean.
    integer, parameter, public :: RESPONSE_TYPE_BYTE    = 5 !! Byte.
    integer, parameter, public :: RESPONSE_TYPE_STRING  = 6 !! Byte string.
    integer, parameter, public :: RESPONSE_TYPE_LAST    = 6 !! Never use this.

    character(len=*), parameter, public :: RESPONSE_TYPE_NAMES(RESPONSE_TYPE_REAL64:RESPONSE_TYPE_LAST) = [ &
        character(len=7) :: 'real64', 'real32', 'int64', 'int32', 'logical', 'byte', 'string' ] !! Response value type names.

    type, public :: response_type
        !! Response of a sensor.
        character(len=RESPONSE_NAME_LEN) :: name  = ' '                  !! Response name.
        character(len=RESPONSE_UNIT_LEN) :: unit  = ' '                  !! Response unit.
        integer                          :: type  = RESPONSE_TYPE_REAL64 !! Response value type.
        integer                          :: error = E_NONE               !! Response error.
        real(kind=r8)                    :: value = 0.0_r8               !! Response value.
    end type response_type

    integer, parameter, public :: RESPONSE_SIZE = storage_size(response_type()) / 8 !! Size of `response_type` in bytes.

    interface operator (==)
        !! Returns whether requests are equal.
        module procedure :: dm_response_equals
    end interface

    public :: operator (==)

    public :: dm_response_equals
    public :: dm_response_out
    public :: dm_response_type_name
    public :: dm_response_type_valid
    public :: dm_response_valid
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
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

    pure function dm_response_type_name(type) result(str)
        !! Returns allocatable string of response value type name, or `invalid`
        !! if the type is invalid.
        integer, intent(in)           :: type !! Response value type.
        character(len=:), allocatable :: str  !! Response value type name.

        if (.not. dm_response_type_valid(type)) then
            str = 'invalid'
            return
        end if

        str = trim(RESPONSE_TYPE_NAMES(type))
    end function dm_response_type_name

    pure elemental logical function dm_response_type_valid(type) result(valid)
        !! Returns `.true.` if the given response value type is valid.
        integer, intent(in) :: type !! Response value type.

        valid = .false.
        if (type < RESPONSE_TYPE_REAL64 .or. type > RESPONSE_TYPE_LAST) return
        valid = .true.
    end function dm_response_type_valid

    pure elemental logical function dm_response_valid(response) result(valid)
        !! Returns `.true.` if given response is valid.
        type(response_type), intent(in) :: response !! Response type.

        valid = .false.
        if (.not. dm_id_valid(response%name)) return
        if (.not. dm_response_type_valid(response%type)) return
        if (.not. dm_error_valid(response%error)) return
        valid = .true.
    end function dm_response_valid

    subroutine dm_response_out(response, unit)
        !! Prints response to standard output or given file unit.
        type(response_type), intent(inout)        :: response
        integer,             intent(in), optional :: unit

        integer :: unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        write (unit_, '("response.name: ", a)')        trim(response%name)
        write (unit_, '("response.unit: ", a)')        trim(response%unit)
        write (unit_, '("response.type: ", i0)')       response%type
        write (unit_, '("response.error: ", i0)')      response%error
        write (unit_, '("response.value: ", 1pg0.12)') response%value
    end subroutine dm_response_out
end module dm_response
