! Author:  Philipp Engel
! Licence: ISC
module dm_request
    !! The observation request data derived type declaration.
    use :: dm_ascii
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_response
    use :: dm_time
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: REQUEST_REQUEST_LEN    = 256  !! Request string length.
    integer, parameter, public :: REQUEST_RESPONSE_LEN   = 256  !! Response string length.
    integer, parameter, public :: REQUEST_DELIMITER_LEN  = 8    !! Delimiter string length.
    integer, parameter, public :: REQUEST_PATTERN_LEN    = 256  !! Regular expression string length.
    integer, parameter, public :: REQUEST_MAX_NRESPONSES = 16   !! Response array size.

    integer, parameter, public :: REQUEST_MODE_NONE      = 0    !! Default mode.

    integer, parameter, public :: REQUEST_STATE_NONE     = 0    !! Default state.
    integer, parameter, public :: REQUEST_STATE_DISABLED = 1    !! Disabled state.

    type, public :: request_type
        !! Request to send to a sensor.
        character(len=TIME_LEN)              :: timestamp  = ' '                  !! ISO 8601 timestamp.
        character(len=REQUEST_REQUEST_LEN)   :: request    = ' '                  !! Request command.
        character(len=REQUEST_RESPONSE_LEN)  :: response   = ' '                  !! Raw response.
        character(len=REQUEST_DELIMITER_LEN) :: delimiter  = ' '                  !! Response delimiter.
        character(len=REQUEST_PATTERN_LEN)   :: pattern    = ' '                  !! Reg Exp pattern.
        integer                              :: delay      = 0                    !! Delay in msec.
        integer                              :: error      = E_NONE               !! Error code.
        integer                              :: mode       = REQUEST_MODE_NONE    !! Request mode.
        integer                              :: retries    = 0                    !! Number of retries.
        integer                              :: state      = REQUEST_STATE_NONE   !! Request state.
        integer                              :: timeout    = 0                    !! Timeout in msec.
        integer                              :: nresponses = 0                    !! Number of responses.
        type(response_type)                  :: responses(REQUEST_MAX_NRESPONSES) !! Responses array.
    end type request_type

    integer, parameter, public :: REQUEST_SIZE = storage_size(request_type()) / 8 !! Size of `request_type` in bytes.

    interface operator (==)
        !! Returns whether requests are equal.
        module procedure :: dm_request_equals
    end interface

    interface dm_request_get
        !! Generic function to get value, unit, and error of a response.
        module procedure :: request_get_i4
        module procedure :: request_get_i8
        module procedure :: request_get_r4
        module procedure :: request_get_r8
        module procedure :: request_get_type
    end interface

    public :: operator (==)

    public :: dm_request_add
    public :: dm_request_equals
    public :: dm_request_get
    public :: dm_request_set_response_error
    public :: dm_request_index
    public :: dm_request_out
    public :: dm_request_valid

    private :: request_get_i4
    private :: request_get_i8
    private :: request_get_r4
    private :: request_get_r8
    private :: request_get_type
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_request_add(request, response) result(rc)
        !! Appends response to the given request.
        type(request_type),  intent(inout) :: request  !! Request type.
        type(response_type), intent(inout) :: response !! Response data.

        rc = E_BOUNDS
        if (request%nresponses < 0 .or. request%nresponses >= REQUEST_MAX_NRESPONSES) return

        rc = E_INVALID
        if (.not. dm_response_valid(response)) return

        request%nresponses = request%nresponses + 1
        request%responses(request%nresponses) = response

        rc = E_NONE
    end function dm_request_add

    pure elemental logical function dm_request_equals(request1, request2) result(equals)
        !! Returns `.true.` if given requests are equal.
        type(request_type), intent(in) :: request1 !! The first request.
        type(request_type), intent(in) :: request2 !! The second request.

        equals = .false.

        if (request1%timestamp  /= request2%timestamp)  return
        if (request1%request    /= request2%request)    return
        if (request1%response   /= request2%response)   return
        if (request1%delimiter  /= request2%delimiter)  return
        if (request1%pattern    /= request2%pattern)    return
        if (request1%delay      /= request2%delay)      return
        if (request1%error      /= request2%error)      return
        if (request1%mode       /= request2%mode)       return
        if (request1%retries    /= request2%retries)    return
        if (request1%state      /= request2%state)      return
        if (request1%timeout    /= request2%timeout)    return
        if (request1%nresponses /= request2%nresponses) return

        if (request1%nresponses > 0) then
            if (.not. all(dm_response_equals(request1%responses(1:request1%nresponses), &
                                             request2%responses(1:request2%nresponses)))) return
        end if

        equals = .true.
    end function dm_request_equals

    integer function dm_request_index(request, name, index) result(rc)
        !! Searches request for responses of passed name and returns the index
        !! of the first found. If no response of this name is found,
        !! `E_NOT_FOUND` is returned and index is set to 0.
        type(request_type), intent(inout) :: request !! Request type.
        character(len=*),   intent(in)    :: name    !! Response name.
        integer,            intent(out)   :: index   !! Position of response in responses array.

        integer :: i

        rc = E_NONE
        index = 0

        do i = 1, request%nresponses
            if (request%responses(i)%name == name) then
                index = i
                return
            end if
        end do

        rc = E_NOT_FOUND
    end function dm_request_index

    integer function dm_request_set_response_error(request, error, name) result(rc)
        !! Sets error code to all responses of the given request. If argument
        !! `name` is given, the error is set only for the first response of the
        !! same name.
        type(request_type), intent(inout)        :: request !! Request type.
        integer,            intent(in)           :: error   !! Error code.
        character(len=*),   intent(in), optional :: name    !! Response name.

        integer :: i

        if (present(name)) then
            rc = dm_request_index(request, name, i)
            if (dm_is_error(rc)) return
            request%responses(i)%error = error
            return
        end if

        do i = 1, request%nresponses
            request%responses(i)%error = error
        end do

        rc = E_NONE
    end function dm_request_set_response_error

    pure elemental logical function dm_request_valid(request, timestamp) result(valid)
        !! Returns `.true.` if given observation request is valid.
        type(request_type), intent(in)           :: request   !! Request type.
        logical,            intent(in), optional :: timestamp !! Validate timestamp.

        integer :: i
        logical :: timestamp_

        valid = .false.

        timestamp_ = .true.
        if (present(timestamp)) timestamp_ = timestamp

        if (timestamp_) then
            if (.not. dm_time_valid(request%timestamp)) return
        end if

        do i = 1, len_trim(request%request)
            if (.not. dm_ascii_is_printable(request%request(i:i))) return
        end do

        if (request%delay < 0)   return
        if (request%error < 0)   return
        if (request%retries < 0) return
        if (request%state < 0)   return
        if (request%timeout < 0) return

        if (request%nresponses < 0 .or. &
            request%nresponses > REQUEST_MAX_NRESPONSES) return

        if (request%nresponses > 0) then
            if (.not. all(dm_response_valid(request%responses(1:request%nresponses)))) return
        end if

        valid = .true.
    end function dm_request_valid

    subroutine dm_request_out(request, unit)
        !! Prints request to standard output or given file unit.
        type(request_type), intent(inout)        :: request !! Request type.
        integer,            intent(in), optional :: unit    !! File unit.

        integer :: i, unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        write (unit_, '("request.timestamp: ", a)')   trim(request%timestamp)
        write (unit_, '("request.request: ", a)')     trim(request%request)
        write (unit_, '("request.response: ", a)')    trim(request%response)
        write (unit_, '("request.delimiter: ", a)')   trim(request%delimiter)
        write (unit_, '("request.pattern: ", a)')     trim(request%pattern)
        write (unit_, '("request.delay: ", i0)')      request%delay
        write (unit_, '("request.error: ", i0)')      request%error
        write (unit_, '("request.mode: ", i0)')       request%mode
        write (unit_, '("request.retries: ", i0)')    request%retries
        write (unit_, '("request.state: ", i0)')      request%state
        write (unit_, '("request.timeout: ", i0)')    request%timeout
        write (unit_, '("request.nresponses: ", i0)') request%nresponses

        do i = 1, request%nresponses
            write (unit_, '("request.responses(", i0, ").name: ", a)') &
                i, trim(request%responses(i)%name)
            write (unit_, '("request.responses(", i0, ").value: ", f0.5)') &
                i, request%responses(i)%value
            write (unit_, '("request.responses(", i0, ").unit: ", a)') &
                i, trim(request%responses(i)%unit)
            write (unit_, '("request.responses(", i0, ").error: ", i0)') &
                i, request%responses(i)%error
        end do
    end subroutine dm_request_out

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function request_get_i4(request, name, value, unit, error) result(rc)
        !! Returns 4-byte integer value, unit, and error of response with name
        !! `name`. If no response of this name exists in the responses array of
        !! the passed request, the function returns `E_NOT_FOUND`.
        type(request_type),               intent(inout)         :: request !! Request type.
        character(len=*),                 intent(in)            :: name    !! Response name.
        integer(kind=i4),                 intent(out)           :: value   !! Response value.
        character(len=RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                          intent(out), optional :: error   !! Response error.

        integer :: i

        value = 0_i4
        if (present(unit))  unit  = ' '
        if (present(error)) error = E_NOT_FOUND

        rc = dm_request_index(request, name, i)
        if (dm_is_error(rc)) return

        value = int(request%responses(i)%value, kind=i4)
        if (present(unit))  unit  = request%responses(i)%unit
        if (present(error)) error = request%responses(i)%error
    end function request_get_i4

    integer function request_get_i8(request, name, value, unit, error) result(rc)
        !! Returns 8-byte integer value, unit, and error of response with name
        !! `name`. If no response of this name exists in the responses array of
        !! the passed request, the function returns `E_NOT_FOUND`.
        type(request_type),               intent(inout)         :: request !! Request type.
        character(len=*),                 intent(in)            :: name    !! Response name.
        integer(kind=i8),                 intent(out)           :: value   !! Response value.
        character(len=RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                          intent(out), optional :: error   !! Response error.

        integer :: i

        value = 0_i8
        if (present(unit))  unit  = ' '
        if (present(error)) error = E_NOT_FOUND

        rc = dm_request_index(request, name, i)
        if (dm_is_error(rc)) return

        value = int(request%responses(i)%value, kind=i8)
        if (present(unit))  unit  = request%responses(i)%unit
        if (present(error)) error = request%responses(i)%error
    end function request_get_i8

    integer function request_get_r4(request, name, value, unit, error) result(rc)
        !! Returns 4-byte real value, unit, and error of response with name
        !! `name`. If no response of this name exists in the responses array of
        !! the passed request, the function returns `E_NOT_FOUND`.
        type(request_type),               intent(inout)         :: request !! Request type.
        character(len=*),                 intent(in)            :: name    !! Response name.
        real(kind=r4),                    intent(out)           :: value   !! Response value.
        character(len=RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                          intent(out), optional :: error   !! Response error.

        integer :: i

        value = 0.0_r4
        if (present(unit))  unit  = ' '
        if (present(error)) error = E_NOT_FOUND

        rc = dm_request_index(request, name, i)
        if (dm_is_error(rc)) return

        value = real(request%responses(i)%value, kind=r4)
        if (present(unit))  unit  = request%responses(i)%unit
        if (present(error)) error = request%responses(i)%error
    end function request_get_r4

    integer function request_get_r8(request, name, value, unit, error) result(rc)
        !! Returns 8-byte real value, unit, and error of response with name
        !! `name`. If no response of this name exists in the responses array of
        !! the passed request, the function returns `E_NOT_FOUND`.
        type(request_type),               intent(inout)         :: request !! Request type.
        character(len=*),                 intent(in)            :: name    !! Response name.
        real(kind=r8),                    intent(out), optional :: value   !! Response value.
        character(len=RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                          intent(out), optional :: error   !! Response error.

        integer :: i

        if (present(value)) value = 0.0_r8
        if (present(unit))  unit  = ' '
        if (present(error)) error = E_NOT_FOUND

        rc = dm_request_index(request, name, i)
        if (dm_is_error(rc)) return

        if (present(value)) value = request%responses(i)%value
        if (present(unit))  unit  = request%responses(i)%unit
        if (present(error)) error = request%responses(i)%error
    end function request_get_r8

    integer function request_get_type(request, name, response) result(rc)
        !! Returns response of name `name`. If no response of this name exists
        !! in the responses array of the passed request, the function returns
        !! `E_NOT_FOUND`.
        type(request_type),  intent(inout) :: request  !! Request type.
        character(len=*),    intent(in)    :: name     !! Response name.
        type(response_type), intent(out)   :: response !! Response type.

        integer :: i

        response%error = E_NOT_FOUND
        rc = dm_request_index(request, name, i)
        if (dm_is_error(rc)) return
        response = request%responses(i)
    end function request_get_type
end module dm_request
