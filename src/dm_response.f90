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

    integer, parameter, public :: RESPONSE_NAME_LEN = 8
    integer, parameter, public :: RESPONSE_UNIT_LEN = 8

    type, public :: response_type
        !! Response of a sensor.
        character(len=RESPONSE_NAME_LEN) :: name  = ' '    !! Response name.
        character(len=RESPONSE_UNIT_LEN) :: unit  = ' '    !! Response unit.
        integer                          :: error = E_NONE !! Response error.
        real(kind=r8)                    :: value = 0.0_r8 !! Response value.
    end type response_type

    integer, parameter, public :: RESPONSE_SIZE = storage_size(response_type()) / 8 !! Size of `response_type` in bytes.

    interface operator (==)
        !! Returns whether requests are equal.
        module procedure :: dm_response_equals
    end interface

    public :: operator (==)

    public :: dm_response_equals
    public :: dm_response_out
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

        if (.not. dm_equals(response1%value, response2%value)) return

        if (response1%name  /= response2%name)  return
        if (response1%unit  /= response2%unit)  return
        if (response1%error /= response2%error) return

        equals = .true.
    end function dm_response_equals

    pure elemental logical function dm_response_valid(response) result(valid)
        !! Returns `.true.` if given response is valid.
        type(response_type), intent(in) :: response !! Response type.

        valid = .false.
        if (.not. dm_id_valid(response%name)) return
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

        write (unit_, '("response.name: ", a)')     trim(response%name)
        write (unit_, '("response.value: ", f0.5)') response%value
        write (unit_, '("response.unit: ", a)')     trim(response%unit)
        write (unit_, '("response.error: ", i0)')   response%error
    end subroutine dm_response_out
end module dm_response
