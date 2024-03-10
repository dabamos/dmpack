! Author:  Philipp Engel
! Licence: ISC
module dm_request
    !! The observation request data derived type declaration.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_response
    use :: dm_string
    use :: dm_time
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: REQUEST_NAME_LEN       = 32  !! Request name length.
    integer, parameter, public :: REQUEST_REQUEST_LEN    = 256 !! Raw request string length.
    integer, parameter, public :: REQUEST_RESPONSE_LEN   = 256 !! Raw response string length.
    integer, parameter, public :: REQUEST_DELIMITER_LEN  = 8   !! Delimiter string length.
    integer, parameter, public :: REQUEST_PATTERN_LEN    = 256 !! Regular expression string length.
    integer, parameter, public :: REQUEST_MAX_NRESPONSES = 16  !! Response array size.

    ! Request modes.
    integer, parameter, public :: REQUEST_MODE_NONE        = 0   !! Default mode.
    integer, parameter, public :: REQUEST_MODE_GEOCOM_FILE = 512 !! GeoCOM file download mode.

    ! Request states.
    integer, parameter, public :: REQUEST_STATE_NONE     = 0 !! Default state.
    integer, parameter, public :: REQUEST_STATE_DISABLED = 1 !! Disabled state.

    type, public :: request_type
        !! Request to send to a sensor.
        character(len=REQUEST_NAME_LEN)      :: name       = ' '                  !! Request name (`-0-9A-Z_a-z`).
        character(len=TIME_LEN)              :: timestamp  = ' '                  !! ISO 8601 time stamp.
        character(len=REQUEST_REQUEST_LEN)   :: request    = ' '                  !! Raw request command (printable).
        character(len=REQUEST_RESPONSE_LEN)  :: response   = ' '                  !! Raw response (printable).
        character(len=REQUEST_DELIMITER_LEN) :: delimiter  = ' '                  !! Response delimiter (printable).
        character(len=REQUEST_PATTERN_LEN)   :: pattern    = ' '                  !! Regular expression pattern.
        integer                              :: delay      = 0                    !! Delay in [msec] (optional).
        integer                              :: error      = E_NONE               !! Error code.
        integer                              :: mode       = REQUEST_MODE_NONE    !! Request mode (optional).
        integer                              :: retries    = 0                    !! Number of executed retries.
        integer                              :: state      = REQUEST_STATE_NONE   !! Request state (optional).
        integer                              :: timeout    = 0                    !! Timeout in [msec] (optional).
        integer                              :: nresponses = 0                    !! Number of responses.
        type(response_type)                  :: responses(REQUEST_MAX_NRESPONSES) !! Responses array.
    end type request_type

    integer, parameter, public :: REQUEST_SIZE = storage_size(request_type()) / 8 !! Size of `request_type` in bytes.

    interface operator (==)
        !! Returns whether requests are equal.
        module procedure :: dm_request_equals
    end interface

    interface dm_request_get
        !! Generic function to get value, unit, type, and error of a response.
        module procedure :: request_get_byte
        module procedure :: request_get_int32
        module procedure :: request_get_int64
        module procedure :: request_get_logical
        module procedure :: request_get_real32
        module procedure :: request_get_real64
        module procedure :: request_get_type
    end interface

    interface dm_request_set
        !! Generic function to set value, unit, type, and error of a response.
        module procedure :: request_set_int32
        module procedure :: request_set_int64
        module procedure :: request_set_logical
        module procedure :: request_set_real32
        module procedure :: request_set_real64
    end interface

    public :: operator (==)

    ! Public procedures.
    public :: dm_request_add
    public :: dm_request_equals
    public :: dm_request_get
    public :: dm_request_set
    public :: dm_request_set_response_error
    public :: dm_request_index
    public :: dm_request_out
    public :: dm_request_valid

    ! Private procedures.
    private :: request_set_int32
    private :: request_set_int64
    private :: request_set_logical
    private :: request_set_real32
    private :: request_set_real64

    private :: request_get_int32
    private :: request_get_int64
    private :: request_get_logical
    private :: request_get_real32
    private :: request_get_real64
    private :: request_get_type
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_request_add(request, response) result(rc)
        !! Validates and appends response to the given request.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if the responses array is full.
        !! * `E_INVALID` if the response is invalid.
        !!
        !! The request attribute `nresponses` must be between 0 and one less
        !! than `REQUEST_MAX_NRESPONSES` for the response to be added.
        type(request_type),  intent(inout) :: request  !! Request type.
        type(response_type), intent(inout) :: response !! Response to add.

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

        integer :: n

        equals = .false.

        if (request1%name       /= request2%name)       return
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

        n = max(0, min(REQUEST_MAX_NRESPONSES, request1%nresponses))

        if (n > 0) then
            if (.not. all(dm_response_equals(request1%responses(1:n), request2%responses(1:n)))) return
        end if

        equals = .true.
    end function dm_request_equals

    pure elemental integer function dm_request_index(request, name) result(index)
        !! Searches request for responses of passed name and returns the index
        !! of the first found. If no response of this name is found,
        !! the index is set to 0.
        type(request_type), intent(in) :: request !! Request type.
        character(len=*),   intent(in) :: name    !! Response name.

        integer :: i, n

        index = 0

        n = max(0, min(REQUEST_MAX_NRESPONSES, request%nresponses))

        do i = 1, n
            if (request%responses(i)%name == name) then
                index = i
                return
            end if
        end do
    end function dm_request_index

    integer function dm_request_set_response_error(request, error, name) result(rc)
        !! Sets error code of all responses of the given request. If argument
        !! `name` is given, the error is set only for the first response of the
        !! same name. The function returns `E_NOT_FOUND` is argument `name` is
        !! given and not found within the responses.
        type(request_type), intent(inout)        :: request !! Request type.
        integer,            intent(in)           :: error   !! Error code.
        character(len=*),   intent(in), optional :: name    !! Response name.

        integer :: i, n

        ! Set error code for single response.
        if (present(name)) then
            rc = E_NOT_FOUND
            i = dm_request_index(request, name)
            if (i == 0) return
            request%responses(i)%error = error
            return
        end if

        ! Set error code for all responses.
        n = max(0, min(REQUEST_MAX_NRESPONSES, request%nresponses))

        do i = 1, n
            request%responses(i)%error = error
        end do

        rc = E_NONE
    end function dm_request_set_response_error

    pure elemental logical function dm_request_valid(request, timestamp) result(valid)
        !! Returns `.true.` if given observation request is valid. A request is
        !! valid if it conforms to the following requirements:
        !!
        !! * A request name is set and a valid id.
        !! * A time stamp is set and in ISO 8601 format, unless argument
        !!   `timestamp` is passed and `.false.`.
        !! * All ASCII characters in attribute _request_ are printable.
        !! * The attributes _delay_, _retries_, _state_ and _timeout_ are not
        !!   negative.
        !! * The attribute _error_ is a valid error code.
        !! * The attribute _nresponses_ is within the bounds of array
        !!   _responses_.
        !! * All responses are valid.
        !!
        type(request_type), intent(in)           :: request   !! Request type.
        logical,            intent(in), optional :: timestamp !! Validate or ignore timestamp.

        logical :: timestamp_

        valid = .false.

        timestamp_ = .true.
        if (present(timestamp)) timestamp_ = timestamp

        if (.not. dm_id_valid(request%name)) return

        if (timestamp_) then
            if (.not. dm_time_valid(request%timestamp, strict=.true.)) return
        end if

        if (.not. dm_string_is_printable(request%request)) return

        if (request%delay < 0) return
        if (.not. dm_error_valid(request%error)) return
        if (request%retries < 0) return
        if (request%state < 0) return
        if (request%timeout < 0) return

        if (request%nresponses < 0 .or. request%nresponses > REQUEST_MAX_NRESPONSES) return

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

        write (unit_, '("request.name: ", a)')        trim(request%name)
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
            write (unit_, '("request.responses(", i0, ").name: ", a)')        i, trim(request%responses(i)%name)
            write (unit_, '("request.responses(", i0, ").unit: ", a)')        i, trim(request%responses(i)%unit)
            write (unit_, '("request.responses(", i0, ").type: ", a)')        i, request%responses(i)%type
            write (unit_, '("request.responses(", i0, ").error: ", i0)')      i, request%responses(i)%error
            write (unit_, '("request.responses(", i0, ").value: ", 1pg0.12)') i, request%responses(i)%value
        end do
    end subroutine dm_request_out

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    pure elemental subroutine request_get_byte(request, name, value, unit, type, error, status, default)
        !! Returns byte response as single character value, unit, type, and error
        !! of response of name `name`.
        !!
        !! The routine returns the following error codes in `status`:
        !!
        !! * `E_EMPTY` if the request has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type byte.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_BYTE

        type(request_type),               intent(inout)         :: request !! Request type.
        character(len=*),                 intent(in)            :: name    !! Response name.
        character,                        intent(inout)         :: value   !! Response value.
        character(len=RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                          intent(out), optional :: type    !! Response value type.
        integer,                          intent(out), optional :: error   !! Response error.
        integer,                          intent(out), optional :: status  !! Error code.
        character,                        intent(in),  optional :: default !! Default value.

        integer :: i, rc

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (request%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_request_index(request, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (request%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            value = achar(floor(request%responses(i)%value, kind=i4))

            if (present(unit))  unit  = request%responses(i)%unit
            if (present(type))  type  = request%responses(i)%type
            if (present(error)) error = request%responses(i)%error
        end block response_block

        if (present(status)) status = rc
        if (rc == E_NONE) return

        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end subroutine request_get_byte

    pure elemental subroutine request_get_int32(request, name, value, unit, type, error, status, default)
        !! Returns 4-byte integer response value, unit, type, and error of
        !! response of name `name`.
        !!
        !! The routine returns the following error codes in `status`:
        !!
        !! * `E_EMPTY` if the request has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type int32.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_INT32

        type(request_type),               intent(inout)         :: request !! Request type.
        character(len=*),                 intent(in)            :: name    !! Response name.
        integer(kind=i4),                 intent(inout)         :: value   !! Response value.
        character(len=RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                          intent(out), optional :: type    !! Response value type.
        integer,                          intent(out), optional :: error   !! Response error.
        integer,                          intent(out), optional :: status  !! Error code.
        integer(kind=i4),                 intent(in),  optional :: default !! Default value.

        integer :: i, rc

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (request%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_request_index(request, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (request%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            value = floor(request%responses(i)%value, kind=i4)

            if (present(unit))  unit  = request%responses(i)%unit
            if (present(type))  type  = request%responses(i)%type
            if (present(error)) error = request%responses(i)%error
        end block response_block

        if (present(status)) status = rc
        if (rc == E_NONE) return

        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end subroutine request_get_int32

    pure elemental subroutine request_get_int64(request, name, value, unit, type, error, status, default)
        !! Returns 8-byte integer response value, unit, type, and error of
        !! response of name `name`.
        !!
        !! The routine returns the following error codes in `status`:
        !!
        !! * `E_EMPTY` if the request has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type int64.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_INT64

        type(request_type),               intent(inout)         :: request !! Request type.
        character(len=*),                 intent(in)            :: name    !! Response name.
        integer(kind=i8),                 intent(inout)         :: value   !! Response value.
        character(len=RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                          intent(out), optional :: type    !! Response value type.
        integer,                          intent(out), optional :: error   !! Response error.
        integer,                          intent(out), optional :: status  !! Error code.
        integer(kind=i8),                 intent(in),  optional :: default !! Default value.

        integer :: i, rc

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (request%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_request_index(request, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (request%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            value = floor(request%responses(i)%value, kind=i8)

            if (present(unit))  unit  = request%responses(i)%unit
            if (present(type))  type  = request%responses(i)%type
            if (present(error)) error = request%responses(i)%error
        end block response_block

        if (present(status)) status = rc
        if (rc == E_NONE) return

        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end subroutine request_get_int64

    pure elemental subroutine request_get_logical(request, name, value, unit, type, error, status, default)
        !! Returns logical response value, unit, type, and error of response of
        !! name `name`.
        !!
        !! The routine returns the following error codes in `status`:
        !!
        !! * `E_EMPTY` if the request has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type logical.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_LOGICAL

        type(request_type),               intent(inout)         :: request !! Request type.
        character(len=*),                 intent(in)            :: name    !! Response name.
        logical,                          intent(inout)         :: value   !! Response value.
        character(len=RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                          intent(out), optional :: type    !! Response value type.
        integer,                          intent(out), optional :: error   !! Response error.
        integer,                          intent(out), optional :: status  !! Error code.
        logical,                          intent(in),  optional :: default !! Default value.

        integer :: i, rc

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (request%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_request_index(request, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (request%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            value = (floor(request%responses(i)%value) >= 1)

            if (present(unit))  unit  = request%responses(i)%unit
            if (present(type))  type  = request%responses(i)%type
            if (present(error)) error = request%responses(i)%error
        end block response_block

        if (present(status)) status = rc
        if (rc == E_NONE) return

        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end subroutine request_get_logical

    pure elemental subroutine request_get_real32(request, name, value, unit, type, error, status, default)
        !! Returns 4-byte real response value, unit, type, and error of
        !! response of name `name`.
        !!
        !! The routine returns the following error codes in `status`:
        !!
        !! * `E_EMPTY` if the request has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type real32.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_REAL32

        type(request_type),               intent(inout)         :: request !! Request type.
        character(len=*),                 intent(in)            :: name    !! Response name.
        real(kind=r4),                    intent(inout)         :: value   !! Response value.
        character(len=RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                          intent(out), optional :: type    !! Response value type.
        integer,                          intent(out), optional :: error   !! Response error.
        integer,                          intent(out), optional :: status  !! Error code.
        real(kind=r4),                    intent(in),  optional :: default !! Default value.

        integer :: i, rc

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (request%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_request_index(request, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (request%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            value = real(request%responses(i)%value, kind=r4)

            if (present(unit))  unit  = request%responses(i)%unit
            if (present(type))  type  = request%responses(i)%type
            if (present(error)) error = request%responses(i)%error
        end block response_block

        if (present(status)) status = rc
        if (rc == E_NONE) return

        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end subroutine request_get_real32

    pure elemental subroutine request_get_real64(request, name, value, unit, type, error, status, default)
        !! Returns 8-byte real response value, unit, type, and error of
        !! response of name `name`.
        !!
        !! The routine returns the following error codes in `status`:
        !!
        !! * `E_EMPTY` if the request has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type real64.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_REAL64

        type(request_type),               intent(inout)         :: request !! Request type.
        character(len=*),                 intent(in)            :: name    !! Response name.
        real(kind=r8),                    intent(inout)         :: value   !! Response value.
        character(len=RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                          intent(out), optional :: type    !! Response value type.
        integer,                          intent(out), optional :: error   !! Response error.
        integer,                          intent(out), optional :: status  !! Error code.
        real(kind=r8),                    intent(in),  optional :: default !! Default value.

        integer :: i, rc

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (request%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_request_index(request, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (request%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            value = request%responses(i)%value

            if (present(unit))  unit  = request%responses(i)%unit
            if (present(type))  type  = request%responses(i)%type
            if (present(error)) error = request%responses(i)%error
        end block response_block

        if (present(status)) status = rc
        if (rc == E_NONE) return

        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end subroutine request_get_real64

    pure elemental subroutine request_get_type(request, name, response, status, default)
        !! Returns response of name `name`.
        !!
        !! The routine returns the following error codes in `status`:
        !!
        !! * `E_EMPTY` if the request has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !!
        !! On error, an empty response will be returned, unless `default` is
        !! passed.
        type(request_type),  intent(inout)         :: request  !! Request type.
        character(len=*),    intent(in)            :: name     !! Response name.
        type(response_type), intent(out)           :: response !! Response type.
        integer,             intent(out), optional :: status   !! Error code.
        type(response_type), intent(in),  optional :: default  !! Default response.

        integer :: i, rc

        response_block: block
            rc = E_EMPTY
            if (request%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_request_index(request, name)
            if (i == 0) exit response_block

            rc = E_NONE
            response = request%responses(i)
        end block response_block

        if (present(status)) status = rc

        if (present(default)) then
            if (rc /= E_NONE) response = default
        endif
    end subroutine request_get_type

    pure elemental subroutine request_set_int32(request, index, name, value, unit, error)
        !! Updates response name, value, and optional unit and error, of
        !! response at position `index` to given 4-byte integer value. This
        !! routine does not update the number of responses
        !! `request%nresponses`. No update is performed if `index` is out of
        !! bounds. An existing response at `index` will be overwritten.
        type(request_type), intent(inout)        :: request !! Request type.
        integer,            intent(in)           :: index   !! Response index.
        character(len=*),   intent(in)           :: name    !! Response name.
        integer(kind=i4),   intent(in)           :: value   !! Response value.
        character(len=*),   intent(in), optional :: unit    !! Response unit.
        integer,            intent(in), optional :: error   !! Response error.

        if (index < 1 .or. index > REQUEST_MAX_NRESPONSES) return

        request%responses(index)%name  = name
        request%responses(index)%value = dm_to_real64(value)
        request%responses(index)%type  = RESPONSE_TYPE_INT32

        if (present(unit)) request%responses(index)%unit = unit

        if (present(error)) then
            request%responses(index)%error = error
        else
            request%responses(index)%error = E_NONE
        end if
    end subroutine request_set_int32

    pure elemental subroutine request_set_int64(request, index, name, value, unit, error)
        !! Updates response name, value, and optional unit and error, of
        !! response at position `index` to given 8-byte integer value. This
        !! routine does not update the number of responses
        !! `request%nresponses`. No update is performed if `index` is out of
        !! bounds. An existing response at `index` will be overwritten.
        type(request_type), intent(inout)        :: request !! Request type.
        integer,            intent(in)           :: index   !! Response index.
        character(len=*),   intent(in)           :: name    !! Response name.
        integer(kind=i8),   intent(in)           :: value   !! Response value.
        character(len=*),   intent(in), optional :: unit    !! Response unit.
        integer,            intent(in), optional :: error   !! Response error.

        if (index < 1 .or. index > REQUEST_MAX_NRESPONSES) return

        request%responses(index)%name  = name
        request%responses(index)%value = dm_to_real64(value)
        request%responses(index)%type  = RESPONSE_TYPE_INT64

        if (present(unit)) request%responses(index)%unit = unit

        if (present(error)) then
            request%responses(index)%error = error
        else
            request%responses(index)%error = E_NONE
        end if
    end subroutine request_set_int64

    pure elemental subroutine request_set_logical(request, index, name, value, unit, error)
        !! Updates response name, value, and optional unit and error, of
        !! response at position `index` to given logical value. This routine
        !! does not update the number of responses `request%nresponses`. No
        !! update is performed if `index` is out of bounds. An existing
        !! response at `index` will be overwritten.
        type(request_type), intent(inout)        :: request !! Request type.
        integer,            intent(in)           :: index   !! Response index.
        character(len=*),   intent(in)           :: name    !! Response name.
        logical,            intent(in)           :: value   !! Response value.
        character(len=*),   intent(in), optional :: unit    !! Response unit.
        integer,            intent(in), optional :: error   !! Response error.

        if (index < 1 .or. index > REQUEST_MAX_NRESPONSES) return

        request%responses(index)%name  = name
        request%responses(index)%value = dm_to_real64(value)
        request%responses(index)%type  = RESPONSE_TYPE_LOGICAL

        if (present(unit)) request%responses(index)%unit = unit

        if (present(error)) then
            request%responses(index)%error = error
        else
            request%responses(index)%error = E_NONE
        end if
    end subroutine request_set_logical

    pure elemental subroutine request_set_real32(request, index, name, value, unit, error)
        !! Updates response name, value, and optional unit and error, of
        !! response at position `index` to given 4-byte real value. This
        !! routine does not update the number of responses
        !! `request%nresponses`. No update is performed if `index` is out of
        !! bounds. An existing response at `index` will be overwritten.
        type(request_type), intent(inout)        :: request !! Request type.
        integer,            intent(in)           :: index   !! Response index.
        character(len=*),   intent(in)           :: name    !! Response name.
        real(kind=r4),      intent(in)           :: value   !! Response value.
        character(len=*),   intent(in), optional :: unit    !! Response unit.
        integer,            intent(in), optional :: error   !! Response error.

        if (index < 1 .or. index > REQUEST_MAX_NRESPONSES) return

        request%responses(index)%name  = name
        request%responses(index)%value = dm_to_real64(value)
        request%responses(index)%type  = RESPONSE_TYPE_REAL32

        if (present(unit)) request%responses(index)%unit = unit

        if (present(error)) then
            request%responses(index)%error = error
        else
            request%responses(index)%error = E_NONE
        end if
    end subroutine request_set_real32

    pure elemental subroutine request_set_real64(request, index, name, value, unit, error)
        !! Updates response name, value, and optional unit and error, of
        !! response at position `index` to given 8-byte real value. This
        !! routine does not update the number of responses
        !! `request%nresponses`. No update is performed if `index` is out of
        !! bounds. An existing response at `index` will be overwritten.
        type(request_type), intent(inout)        :: request !! Request type.
        integer,            intent(in)           :: index   !! Response index.
        character(len=*),   intent(in)           :: name    !! Response name.
        real(kind=r8),      intent(in)           :: value   !! Response value.
        character(len=*),   intent(in), optional :: unit    !! Response unit.
        integer,            intent(in), optional :: error   !! Response error.

        if (index < 1 .or. index > REQUEST_MAX_NRESPONSES) return

        request%responses(index)%name  = name
        request%responses(index)%value = value
        request%responses(index)%type  = RESPONSE_TYPE_REAL64

        if (present(unit)) request%responses(index)%unit = unit

        if (present(error)) then
            request%responses(index)%error = error
        else
            request%responses(index)%error = E_NONE
        end if
    end subroutine request_set_real64
end module dm_request
