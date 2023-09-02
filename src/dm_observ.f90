! Author:  Philipp Engel
! Licence: ISC
module dm_observ
    !! The observation data derived type declaration, and all associated
    !! procedures.
    use :: dm_ascii
    use :: dm_error
    use :: dm_id
    use :: dm_node
    use :: dm_sensor
    use :: dm_target
    use :: dm_time
    use :: dm_type
    use :: dm_util
    use :: dm_uuid
    implicit none (type, external)
    private

    ! ******************************************************************
    ! OBSERVATION RESPONSE.
    ! ******************************************************************
    integer, parameter, public :: RESPONSE_NAME_LEN = 8
    integer, parameter, public :: RESPONSE_UNIT_LEN = 8

    type, public :: response_type
        !! Response of a sensor.
        character(len=RESPONSE_NAME_LEN) :: name  = ' '    !! Response name.
        character(len=RESPONSE_UNIT_LEN) :: unit  = ' '    !! Response unit.
        integer                          :: error = E_NONE !! Response error.
        real(kind=r8)                    :: value = 0.0_r8 !! Response value.
    end type response_type

    ! ******************************************************************
    ! OBSERVATION REQUEST.
    ! ******************************************************************
    integer, parameter, public :: REQUEST_REQUEST_LEN    = 256
    integer, parameter, public :: REQUEST_RESPONSE_LEN   = 256
    integer, parameter, public :: REQUEST_DELIMITER_LEN  = 8
    integer, parameter, public :: REQUEST_PATTERN_LEN    = 256
    integer, parameter, public :: REQUEST_MAX_NRESPONSES = 16

    integer, parameter, public :: REQUEST_STATE_NONE     = 0
    integer, parameter, public :: REQUEST_STATE_DISABLED = 1

    type, public :: request_type
        !! Request to send to a sensor.
        character(len=TIME_LEN)              :: timestamp  = ' '                  !! ISO 8601 timestamp.
        character(len=REQUEST_REQUEST_LEN)   :: request    = ' '                  !! Request command.
        character(len=REQUEST_RESPONSE_LEN)  :: response   = ' '                  !! Raw response.
        character(len=REQUEST_DELIMITER_LEN) :: delimiter  = ' '                  !! Response delimiter.
        character(len=REQUEST_PATTERN_LEN)   :: pattern    = ' '                  !! Reg Exp pattern.
        integer                              :: delay      = 0                    !! Delay in msec.
        integer                              :: error      = E_NONE               !! Error code.
        integer                              :: retries    = 0                    !! Number of retries.
        integer                              :: state      = REQUEST_STATE_NONE   !! State error.
        integer                              :: timeout    = 0                    !! Timeout in msec.
        integer                              :: nresponses = 0                    !! Number of responses.
        type(response_type)                  :: responses(REQUEST_MAX_NRESPONSES) !! Responses array.
    end type request_type

    ! ******************************************************************
    ! OBSERVATION.
    ! ******************************************************************
    integer, parameter, public :: OBSERV_ID_LEN         = UUID_LEN
    integer, parameter, public :: OBSERV_NAME_LEN       = ID_LEN
    integer, parameter, public :: OBSERV_PATH_LEN       = 32
    integer, parameter, public :: OBSERV_RECEIVER_LEN   = ID_LEN
    integer, parameter, public :: OBSERV_MAX_NRECEIVERS = 16
    integer, parameter, public :: OBSERV_MAX_NREQUESTS  = 8

    type, public :: observ_type
        !! Observation with receivers, requests, and responses. Modifying this
        !! type requires changes in `dm_db`, `dm_html`, `dm_json`, and other
        !! modules (you probably don't want that!).
        character(len=OBSERV_ID_LEN)       :: id         = UUID_DEFAULT !! Observation id.
        character(len=NODE_ID_LEN)         :: node_id    = ' '          !! Node id.
        character(len=SENSOR_ID_LEN)       :: sensor_id  = ' '          !! Sensor id.
        character(len=TARGET_ID_LEN)       :: target_id  = ' '          !! Target id.
        character(len=OBSERV_NAME_LEN)     :: name       = ' '          !! Observation name.
        character(len=TIME_LEN)            :: timestamp  = ' '          !! ISO 8601 timestamp.
        character(len=OBSERV_PATH_LEN)     :: path       = ' '          !! TTY/PTY path.
        integer                            :: priority   = 0            !! Message queue priority (>= 0).
        integer                            :: error      = E_NONE       !! Error code.
        integer                            :: next       = 0            !! Next receiver index.
        integer                            :: nreceivers = 0            !! Number of receivers.
        integer                            :: nrequests  = 0            !! Number of requests.
        character(len=OBSERV_RECEIVER_LEN) :: receivers(OBSERV_MAX_NRECEIVERS) = ' ' !! Array of receivers.
        type(request_type)                 :: requests(OBSERV_MAX_NREQUESTS) !! Array of requests.
    end type observ_type

    integer, parameter, public :: OBSERV_SIZE = storage_size(observ_type(), kind=i8) / 8 !! Size of `observ_type` in bytes.

    ! ******************************************************************
    ! OBSERVATION VIEW.
    ! ******************************************************************
    type, public :: observ_view_type
        !! View of an observation with only one response of a single request.
        character(len=OBSERV_ID_LEN)     :: observ_id         = UUID_DEFAULT !! Observation id.
        character(len=NODE_ID_LEN)       :: node_id           = ' '          !! Node id.
        character(len=SENSOR_ID_LEN)     :: sensor_id         = ' '          !! Sensor id.
        character(len=TARGET_ID_LEN)     :: target_id         = ' '          !! Target id.
        character(len=OBSERV_NAME_LEN)   :: observ_name       = ' '          !! Observation name.
        integer                          :: observ_error      = E_NONE       !! Observation error code.
        character(len=TIME_LEN)          :: request_timestamp = ' '          !! Request timestamp (ISO 8601).
        integer                          :: request_error     = E_NONE       !! Request error code.
        character(len=RESPONSE_NAME_LEN) :: response_name     = ' '          !! Response name.
        character(len=RESPONSE_UNIT_LEN) :: response_unit     = ' '          !! Response unit (optional).
        integer                          :: response_error    = E_NONE       !! Response error code.
        real(kind=r8)                    :: response_value    = 0.0_r8       !! Response value.
    end type observ_view_type

    integer(kind=i8), parameter, public :: OBSERV_VIEW_SIZE = &
        storage_size(observ_view_type(), kind=i8) / 8 !! Size of `observ_view_type` in bytes.

    interface operator (==)
        !! Returns whether observations or observation views are equal.
        module procedure :: dm_observ_equals
        module procedure :: dm_observ_view_equals
    end interface

    interface dm_observ_index
        !! Searches the observation for a response of given name and returns the index.
        module procedure :: observ_index_observ
        module procedure :: observ_index_request
    end interface

    interface dm_observ_valid
        !! Returns whether observations or observation views are valid.
        module procedure :: observ_valid_observ
        module procedure :: observ_valid_request
    end interface

    ! interface write (formatted)
    !     module procedure :: observ_write_formatted
    ! end interface

    public :: operator (==)
    ! public :: write (formatted)

    public :: dm_observ_add_receiver
    public :: dm_observ_add_request
    public :: dm_observ_add_response
    public :: dm_observ_equals
    public :: dm_observ_index
    public :: dm_observ_out
    public :: dm_observ_valid
    public :: dm_observ_view_equals

    private :: observ_index_observ
    private :: observ_index_request
    private :: observ_valid_observ
    private :: observ_valid_request

    ! private :: observ_write_formatted
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_observ_add_receiver(observ, receiver) result(rc)
        !! Adds receiver to observation.
        type(observ_type), intent(inout) :: observ   !! Observation type.
        character(len=*),  intent(in)    :: receiver !! Receiver data.
        integer                          :: n

        rc = E_BOUNDS
        if (observ%nreceivers < 0 .or. observ%nreceivers >= OBSERV_MAX_NRECEIVERS) return

        rc = E_INVALID
        n = len_trim(receiver)
        if (n == 0 .or. n > OBSERV_RECEIVER_LEN) return

        observ%nreceivers = observ%nreceivers + 1
        observ%receivers(observ%nreceivers) = receiver

        rc = E_NONE
    end function dm_observ_add_receiver

    integer function dm_observ_add_request(observ, request) result(rc)
        !! Appends a request to an observation.
        type(observ_type),  intent(inout) :: observ  !! Observation type.
        type(request_type), intent(inout) :: request !! Request type.

        rc = E_BOUNDS
        if (observ%nrequests < 0 .or. observ%nrequests >= OBSERV_MAX_NREQUESTS) return

        observ%nrequests = observ%nrequests + 1
        observ%requests(observ%nrequests) = request

        rc = E_NONE
    end function dm_observ_add_request

    integer function dm_observ_add_response(request, response) result(rc)
        !! Appends a response to a request.
        type(request_type),  intent(inout) :: request  !! Request type.
        type(response_type), intent(inout) :: response !! Response data.

        rc = E_BOUNDS
        if (request%nresponses < 0 .or. request%nresponses >= REQUEST_MAX_NRESPONSES) return

        rc = E_INVALID
        if (len_trim(response%name) == 0) return

        request%nresponses = request%nresponses + 1
        request%responses(request%nresponses) = response

        rc = E_NONE
    end function dm_observ_add_response

    pure elemental logical function dm_observ_equals(observ1, observ2) result(equals)
        !! Returns `.true.` if given observations are equal.
        type(observ_type), intent(in) :: observ1 !! The first observation.
        type(observ_type), intent(in) :: observ2 !! The second observation.
        integer                       :: i, j

        equals = .false.

        if (observ1%id         /= observ2%id)         return
        if (observ1%node_id    /= observ2%node_id)    return
        if (observ1%sensor_id  /= observ2%sensor_id)  return
        if (observ1%target_id  /= observ2%target_id)  return
        if (observ1%name       /= observ2%name)       return
        if (observ1%timestamp  /= observ2%timestamp)  return
        if (observ1%path       /= observ2%path)       return
        if (observ1%priority   /= observ2%priority)   return
        if (observ1%error      /= observ2%error)      return
        if (observ1%next       /= observ2%next)       return
        if (observ1%nreceivers /= observ2%nreceivers) return
        if (observ1%nrequests  /= observ2%nrequests)  return

        do i = 1, observ1%nreceivers
            if (observ1%receivers(i) /= observ2%receivers(i)) return
        end do

        do i = 1, observ1%nrequests
            if (observ1%requests(i)%timestamp  /= observ2%requests(i)%timestamp)  return
            if (observ1%requests(i)%request    /= observ2%requests(i)%request)    return
            if (observ1%requests(i)%response   /= observ2%requests(i)%response)   return
            if (observ1%requests(i)%delimiter  /= observ2%requests(i)%delimiter)  return
            if (observ1%requests(i)%pattern    /= observ2%requests(i)%pattern)    return
            if (observ1%requests(i)%delay      /= observ2%requests(i)%delay)      return
            if (observ1%requests(i)%retries    /= observ2%requests(i)%retries)    return
            if (observ1%requests(i)%timeout    /= observ2%requests(i)%timeout)    return
            if (observ1%requests(i)%error      /= observ2%requests(i)%error)      return
            if (observ1%requests(i)%nresponses /= observ2%requests(i)%nresponses) return
            if (observ1%requests(i)%state      /= observ2%requests(i)%state)      return

            do j = 1, observ1%requests(i)%nresponses
                if (.not. dm_equals(observ1%requests(i)%responses(j)%value, &
                                    observ2%requests(i)%responses(j)%value)) return

                if (observ1%requests(i)%responses(j)%name  /= observ2%requests(i)%responses(j)%name)  return
                if (observ1%requests(i)%responses(j)%unit  /= observ2%requests(i)%responses(j)%unit)  return
                if (observ1%requests(i)%responses(j)%error /= observ2%requests(i)%responses(j)%error) return
            end do
        end do

        equals = .true.
    end function dm_observ_equals

    pure elemental logical function dm_observ_view_equals(view1, view2) result(equals)
        !! Returns `.true.` if given observation views are equal.
        type(observ_view_type), intent(in) :: view1 !! The first observation view.
        type(observ_view_type), intent(in) :: view2 !! The second observation view.

        equals = .false.

        if (view1%observ_id         /= view2%observ_id)         return
        if (view1%node_id           /= view2%node_id)           return
        if (view1%sensor_id         /= view2%sensor_id)         return
        if (view1%target_id         /= view2%target_id)         return
        if (view1%observ_name       /= view2%observ_name)       return
        if (view1%observ_error      /= view2%observ_error)      return
        if (view1%request_timestamp /= view2%request_timestamp) return
        if (view1%request_error     /= view2%request_error)     return
        if (view1%response_name     /= view2%response_name)     return
        if (view1%response_unit     /= view2%response_unit)     return
        if (view1%response_error    /= view2%response_error)    return

        if (.not. dm_equals(view1%response_value, view2%response_value)) return

        equals = .true.
    end function dm_observ_view_equals

    subroutine dm_observ_out(observ, unit)
        !! Prints observation to standard output or given file unit.
        type(observ_type), intent(inout)        :: observ
        integer,           intent(in), optional :: unit

        integer :: i, j, unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        write (unit_, '("observ%id=", a)')          trim(observ%id)
        write (unit_, '("observ%node_id=", a)')     trim(observ%node_id)
        write (unit_, '("observ%sensor_id=", a)')   trim(observ%sensor_id)
        write (unit_, '("observ%target_id=", a)')   trim(observ%target_id)
        write (unit_, '("observ%name=", a)')        trim(observ%name)
        write (unit_, '("observ%timestamp=", a)')   observ%timestamp
        write (unit_, '("observ%path=", a)')        trim(observ%path)
        write (unit_, '("observ%priority=", i0)')   observ%priority
        write (unit_, '("observ%error=", i0)')      observ%error
        write (unit_, '("observ%next=", i0)')       observ%next
        write (unit_, '("observ%nreceivers=", i0)') observ%nreceivers
        write (unit_, '("observ%nrequests=", i0)')  observ%nrequests

        do i = 1, observ%nreceivers
            write (unit_, '("observ%receivers(", i0, ")=", a)') i, trim(observ%receivers(i))
        end do

        do i = 1, observ%nrequests
            write (unit_, '("observ%requests(", i0, ")%timestamp=", a)')   i, trim(observ%requests(i)%timestamp)
            write (unit_, '("observ%requests(", i0, ")%request=", a)')     i, trim(observ%requests(i)%request)
            write (unit_, '("observ%requests(", i0, ")%response=", a)')    i, trim(observ%requests(i)%response)
            write (unit_, '("observ%requests(", i0, ")%delimiter=", a)')   i, trim(observ%requests(i)%delimiter)
            write (unit_, '("observ%requests(", i0, ")%pattern=", a)')     i, trim(observ%requests(i)%pattern)
            write (unit_, '("observ%requests(", i0, ")%delay=", i0)')      i, observ%requests(i)%delay
            write (unit_, '("observ%requests(", i0, ")%error=", i0)')      i, observ%requests(i)%error
            write (unit_, '("observ%requests(", i0, ")%retries=", i0)')    i, observ%requests(i)%retries
            write (unit_, '("observ%requests(", i0, ")%state=", i0)')      i, observ%requests(i)%state
            write (unit_, '("observ%requests(", i0, ")%timeout=", i0)')    i, observ%requests(i)%timeout
            write (unit_, '("observ%requests(", i0, ")%nresponses=", i0)') i, observ%requests(i)%nresponses

            do j = 1, observ%requests(i)%nresponses
                write (unit_, '("observ%requests(", i0, ")%responses(", i0, ")%name=", a)') &
                    i, j, trim(observ%requests(i)%responses(j)%name)
                write (unit_, '("observ%requests(", i0, ")%responses(", i0, ")%value=", f0.5)') &
                    i, j, observ%requests(i)%responses(j)%value
                write (unit_, '("observ%requests(", i0, ")%responses(", i0, ")%unit=", a)') &
                    i, j, trim(observ%requests(i)%responses(j)%unit)
                write (unit_, '("observ%requests(", i0, ")%responses(", i0, ")%error=", i0)') &
                    i, j, observ%requests(i)%responses(j)%error
            end do
        end do
    end subroutine dm_observ_out

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function observ_index_observ(observ, name, request_index, response_index) result(rc)
        !! Searches requests array of the observation for responses of passed name
        !! and returns the index of the first found. If no request of this name
        !! is found, `E_NOT_FOUND` is returned and request and index are set to 0.
        type(observ_type), intent(inout) :: observ         !! Observation type.
        character(len=*),  intent(in)    :: name           !! Response name.
        integer,           intent(out)   :: request_index  !! Position of request in requests array.
        integer,           intent(out)   :: response_index !! Position of response in responses array.

        integer :: i, j

        rc = E_NONE

        request_index  = 0
        response_index = 0

        do i = 1, observ%nrequests
            do j = 1, observ%requests(i)%nresponses
                if (observ%requests(i)%responses(i)%name == name) then
                    request_index  = i
                    response_index = j
                    return
                end if
            end do
        end do

        rc = E_NOT_FOUND
    end function observ_index_observ

    integer function observ_index_request(request, name, index) result(rc)
        !! Searches responses array of the request for the first response of
        !! passed name and returns the index if found. If no response of this
        !! name is found, `E_NOT_FOUND` is returned and index is set to 0.
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
    end function observ_index_request

    pure elemental logical function observ_valid_observ(observ, id, timestamp) result(valid)
        !! Returns `.true.` if given observation has at least a valid UUID as
        !! id, and node id, sensor id, target id, and name set. Validating the
        !! node, observation, sensor, and target id is optional (only if `id`
        !! is `.true.`).
        type(observ_type), intent(in)           :: observ    !! Observation type.
        logical,           intent(in), optional :: id        !! Validate ids.
        logical,           intent(in), optional :: timestamp !! Validate timestamps.

        logical :: id_, ts_
        integer :: i

        valid = .false.

        id_ = .true.
        ts_ = .true.
        if (present(id)) id_ = id
        if (present(timestamp)) ts_ = timestamp

        if (id_) then
            if (observ%id == UUID_DEFAULT) return
            if (.not. dm_uuid4_valid(observ%id)) return
            if (.not. dm_id_valid(observ%node_id)) return
            if (.not. dm_id_valid(observ%sensor_id)) return
            if (.not. dm_id_valid(observ%target_id)) return
        end if

        if (.not. dm_id_valid(observ%name)) return

        if (ts_) then
            if (.not. dm_time_valid(observ%timestamp)) return
        end if

        if (observ%priority < 0) return
        if (observ%error < 0) return
        if (observ%next < 0 .or. observ%next > OBSERV_MAX_NRECEIVERS) return
        if (observ%nreceivers < 0 .or. observ%nreceivers > OBSERV_MAX_NRECEIVERS) return
        if (observ%nrequests < 0 .or. observ%nrequests > OBSERV_MAX_NREQUESTS) return

        do i = 1, observ%nreceivers
            if (.not. dm_id_valid(observ%receivers(i))) return
        end do

        do i = 1, observ%nrequests
            if (.not. dm_observ_valid(observ%requests(i), timestamp=ts_)) return
        end do

        valid = .true.
    end function observ_valid_observ

    pure elemental logical function observ_valid_request(request, timestamp) result(valid)
        !! Returns `.true.` if given observation request is valid.
        type(request_type), intent(in)           :: request   !! Request type.
        logical,            intent(in), optional :: timestamp !! Validate timestamp.

        integer :: i
        logical :: ts_

        valid = .false.

        ts_ = .true.
        if (present(timestamp)) ts_ = timestamp

        if (ts_) then
            if (.not. dm_time_valid(request%timestamp)) return
        end if

        do i = 1, len_trim(request%request)
            if (.not. dm_ascii_is_printable(request%request(i:i))) return
        end do

        if (request%delay < 0) return
        if (request%error < 0) return
        if (request%retries < 0) return
        if (request%state < 0) return
        if (request%timeout < 0) return
        if (request%nresponses < 0 .or. request%nresponses > REQUEST_MAX_NRESPONSES) return

        do i = 1, request%nresponses
            if (.not. dm_id_valid(request%responses(i)%name)) return
            if (request%responses(i)%error < 0) return
        end do

        valid = .true.
    end function observ_valid_request

!   subroutine observ_write_formatted(observ, unit, iotype, vlist, iostat, iomsg)
!       !! User-defined derived type I/O.
!       class(observ_type), intent(in)    :: observ
!       integer,            intent(in)    :: unit
!       character(len=*),   intent(in)    :: iotype
!       integer,            intent(in)    :: vlist(:)
!       integer,            intent(out)   :: iostat
!       character(len=*),   intent(inout) :: iomsg
!       integer                           :: i, j
!
!       write (unit, '("observ%id=", a, /)')          trim(observ%id)
!       write (unit, '("observ%node_id=", a, /)')     trim(observ%node_id)
!       write (unit, '("observ%sensor_id=", a, /)')   trim(observ%sensor_id)
!       write (unit, '("observ%target_id=", a, /)')   trim(observ%target_id)
!       write (unit, '("observ%name=", a, /)')        trim(observ%name)
!       write (unit, '("observ%timestamp=", a, /)')   observ%timestamp
!       write (unit, '("observ%path=", a, /)')        trim(observ%path)
!       write (unit, '("observ%priority=", i0, /)')   observ%priority
!       write (unit, '("observ%error=", i0, /)')      observ%error
!       write (unit, '("observ%next=", i0, /)')       observ%next
!       write (unit, '("observ%nreceivers=", i0, /)') observ%nreceivers
!       write (unit, '("observ%nrequests=", i0, /)')  observ%nrequests
!
!       do i = 1, observ%nreceivers
!           write (unit, '("observ%receivers(", i0, ")=", a, /)') i, trim(observ%receivers(i))
!       end do
!
!       do i = 1, observ%nrequests
!           write (unit, '("observ%requests(", i0, ")%timestamp=", a, /)')   i, trim(observ%requests(i)%timestamp)
!           write (unit, '("observ%requests(", i0, ")%request=", a, /)')     i, trim(observ%requests(i)%request)
!           write (unit, '("observ%requests(", i0, ")%response=", a, /)')    i, trim(observ%requests(i)%response)
!           write (unit, '("observ%requests(", i0, ")%delimiter=", a, /)')   i, trim(observ%requests(i)%delimiter)
!           write (unit, '("observ%requests(", i0, ")%pattern=", a, /)')     i, trim(observ%requests(i)%pattern)
!           write (unit, '("observ%requests(", i0, ")%delay=", i0, /)')      i, observ%requests(i)%delay
!           write (unit, '("observ%requests(", i0, ")%error=", i0, /)')      i, observ%requests(i)%error
!           write (unit, '("observ%requests(", i0, ")%retries=", i0, /)')    i, observ%requests(i)%retries
!           write (unit, '("observ%requests(", i0, ")%state=", i0, /)')      i, observ%requests(i)%state
!           write (unit, '("observ%requests(", i0, ")%timeout=", i0, /)')    i, observ%requests(i)%timeout
!           write (unit, '("observ%requests(", i0, ")%nresponses=", i0, /)') i, observ%requests(i)%nresponses
!
!           do j = 1, observ%requests(i)%nresponses
!               write (unit, '("observ%requests(", i0, ")%responses(", i0, ")%name=", a, /)') &
!                   i, j, trim(observ%requests(i)%responses(j)%name)
!               write (unit, '("observ%requests(", i0, ")%responses(", i0, ")%value=", f0.5, /)') &
!                   i, j, observ%requests(i)%responses(j)%value
!               write (unit, '("observ%requests(", i0, ")%responses(", i0, ")%unit=", a, /)') &
!                   i, j, trim(observ%requests(i)%responses(j)%unit)
!               write (unit, '("observ%requests(", i0, ")%responses(", i0, ")%error=", i0, /)') &
!                   i, j, observ%requests(i)%responses(j)%error
!           end do
!       end do
!
!       iostat = 0
!   end subroutine observ_write_formatted
end module dm_observ
