! Author:  Philipp Engel
! Licence: ISC
module dm_observ
    !! The observation data derived type declaration, and all associated
    !! procedures.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_node
    use :: dm_response
    use :: dm_sensor
    use :: dm_target
    use :: dm_time
    use :: dm_util
    use :: dm_uuid
    implicit none (type, external)
    private

    integer, parameter, public :: OBSERV_DELIMITER_LEN  = 8        !! Max. observation delimiter length.
    integer, parameter, public :: OBSERV_DEVICE_LEN     = 32       !! Max. observation device length.
    integer, parameter, public :: OBSERV_ID_LEN         = UUID_LEN !! Max. observation id length.
    integer, parameter, public :: OBSERV_NAME_LEN       = ID_LEN   !! Max. observation name length.
    integer, parameter, public :: OBSERV_PATTERN_LEN    = 512      !! Max. observation regular expression length.
    integer, parameter, public :: OBSERV_REQUEST_LEN    = 512      !! Max. observation raw request length.
    integer, parameter, public :: OBSERV_RESPONSE_LEN   = 512      !! Max. observation raw response length.
    integer, parameter, public :: OBSERV_SOURCE_LEN     = ID_LEN   !! Max. observation source length.
    integer, parameter, public :: OBSERV_MAX_NRESPONSES = 64       !! Max. number of responses.

    ! Observation modes.
    integer, parameter, public :: OBSERV_MODE_NONE        = 0      !! Default mode.
    integer, parameter, public :: OBSERV_MODE_GEOCOM_FILE = 512    !! GeoCOM file download mode.

    ! Observation states.
    integer, parameter, public :: OBSERV_STATE_NONE     = 0        !! Default state.
    integer, parameter, public :: OBSERV_STATE_DISABLED = 1        !! Disabled state.

    type, public :: observ_type
        !! Observation with responses. Modifying this type requires changes in
        !! `dm_csv`, `dm_db`, `dm_hdf5`, `dm_html`, `dm_json`, and several
        !! other modules (you probably don’t want that!).
        sequence
        character(OBSERV_ID_LEN)        :: id                               = UUID_NONE         !! Observation id (UUIDv4).
        character(OBSERV_ID_LEN)        :: group_id                         = ' '               !! Observation group id (UUIDv4).
        character(NODE_ID_LEN)          :: node_id                          = ' '               !! Node id (`-0-9A-Z_a-z`).
        character(SENSOR_ID_LEN)        :: sensor_id                        = ' '               !! Sensor id (`-0-9A-Z_a-z`).
        character(TARGET_ID_LEN)        :: target_id                        = ' '               !! Target id (`-0-9A-Z_a-z`).
        character(TIME_LEN)             :: timestamp                        = ' '               !! ISO 8601 timestamp.
        character(OBSERV_NAME_LEN)      :: name                             = ' '               !! Observation name (`-0-9A-Z_a-z`).
        character(OBSERV_SOURCE_LEN)    :: source                           = ' '               !! Observation source (`-0-9A-Z_a-z`).
        character(OBSERV_DEVICE_LEN)    :: device                           = ' '               !! Physical device (TTY/PTY).
        character(OBSERV_REQUEST_LEN)   :: request                          = ' '               !! Raw request command (printable).
        character(OBSERV_RESPONSE_LEN)  :: response                         = ' '               !! Raw response (printable).
        character(OBSERV_DELIMITER_LEN) :: delimiter                        = ' '               !! Response delimiter (printable).
        character(OBSERV_PATTERN_LEN)   :: pattern                          = ' '               !! Regular expression pattern.
        integer                         :: delay                            = 0                 !! Delay in [msec] (optional).
        integer                         :: error                            = E_NONE            !! Error code (optional).
        integer                         :: mode                             = OBSERV_MODE_NONE  !! Request mode (optional).
        integer                         :: retries                          = 0                 !! Number of executed retries.
        integer                         :: state                            = OBSERV_STATE_NONE !! Request state (optional).
        integer                         :: timeout                          = 0                 !! Timeout in [msec] (optional).
        integer                         :: nresponses                       = 0                 !! Number of responses.
        type(response_type)             :: responses(OBSERV_MAX_NRESPONSES) = response_type()   !! Responses array.
    end type observ_type

    integer, parameter, public :: OBSERV_TYPE_SIZE = storage_size(observ_type()) / 8 !! Size of `observ_type` in bytes.

    ! **************************************************************************
    ! OBSERVATION VIEW.
    ! **************************************************************************
    type, public :: observ_view_type
        !! View of an observation with a single response only.
        character(OBSERV_ID_LEN)     :: id             = UUID_NONE            !! Observation id (UUIDv4).
        character(OBSERV_ID_LEN)     :: group_id       = ' '                  !! Group id (UUIDv4).
        character(NODE_ID_LEN)       :: node_id        = ' '                  !! Node id (`-0-9A-Z_a-z`).
        character(SENSOR_ID_LEN)     :: sensor_id      = ' '                  !! Sensor id (`-0-9A-Z_a-z`).
        character(TARGET_ID_LEN)     :: target_id      = ' '                  !! Target id (`-0-9A-Z_a-z`).
        character(TIME_LEN)          :: timestamp      = ' '                  !! ISO 8601 timestamp.
        character(OBSERV_NAME_LEN)   :: name           = ' '                  !! Observation name.
        integer                      :: error          = E_NONE               !! Observation error code.
        character(RESPONSE_NAME_LEN) :: response_name  = ' '                  !! Response name (`-0-9A-Z_a-z`).
        character(RESPONSE_UNIT_LEN) :: response_unit  = ' '                  !! Response unit (optional).
        integer                      :: response_type  = RESPONSE_TYPE_REAL64 !! Response value type.
        integer                      :: response_error = E_NONE               !! Response error code.
        real(r8)                     :: response_value = 0.0_r8               !! Response value.
    end type observ_view_type

    integer, parameter, public :: OBSERV_VIEW_SIZE = storage_size(observ_view_type()) / 8 !! Size of `observ_view_type` in bytes.

    interface dm_observ_add_response
        !! Generic function to add response.
        module procedure :: observ_add_response_int32
        module procedure :: observ_add_response_int64
        module procedure :: observ_add_response_real32
        module procedure :: observ_add_response_real64
        module procedure :: observ_add_response_type
    end interface dm_observ_add_response

    interface dm_observ_get_response
        !! Generic function to get value, unit, type, and error of a response.
        module procedure :: observ_get_response_byte
        module procedure :: observ_get_response_int32
        module procedure :: observ_get_response_int64
        module procedure :: observ_get_response_logical
        module procedure :: observ_get_response_real32
        module procedure :: observ_get_response_real64
        module procedure :: observ_get_response_type
    end interface dm_observ_get_response

    interface operator (==)
        !! Returns `.true.` if observations or observation views are equal.
        module procedure :: dm_observ_equals
        module procedure :: dm_observ_view_equals
    end interface

    public :: operator (==)

    public :: dm_observ_add_response
    public :: dm_observ_equals
    public :: dm_observ_find
    public :: dm_observ_get_response
    public :: dm_observ_has_pattern
    public :: dm_observ_is_disabled
    public :: dm_observ_is_valid
    public :: dm_observ_out
    public :: dm_observ_set
    public :: dm_observ_set_response_error
    public :: dm_observ_view_equals

    private :: observ_add_response_int32
    private :: observ_add_response_int64
    private :: observ_add_response_real32
    private :: observ_add_response_real64
    private :: observ_add_response_type
    private :: observ_get_response_byte
    private :: observ_get_response_int32
    private :: observ_get_response_int64
    private :: observ_get_response_real64
    private :: observ_get_response_real32
    private :: observ_get_response_type
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    pure elemental logical function dm_observ_equals(observ1, observ2) result(equals)
        !! Returns `.true.` if given observations are equal.
        type(observ_type), intent(in) :: observ1 !! The first observation.
        type(observ_type), intent(in) :: observ2 !! The second observation.

        integer :: n

        equals = (observ1%id         == observ2%id         .and. &
                  observ1%group_id   == observ2%group_id   .and. &
                  observ1%node_id    == observ2%node_id    .and. &
                  observ1%sensor_id  == observ2%sensor_id  .and. &
                  observ1%target_id  == observ2%target_id  .and. &
                  observ1%timestamp  == observ2%timestamp  .and. &
                  observ1%name       == observ2%name       .and. &
                  observ1%source     == observ2%source     .and. &
                  observ1%device     == observ2%device     .and. &
                  observ1%request    == observ2%request    .and. &
                  observ1%response   == observ2%response   .and. &
                  observ1%delimiter  == observ2%delimiter  .and. &
                  observ1%pattern    == observ2%pattern    .and. &
                  observ1%delay      == observ2%delay      .and. &
                  observ1%error      == observ2%error      .and. &
                  observ1%mode       == observ2%mode       .and. &
                  observ1%retries    == observ2%retries    .and. &
                  observ1%state      == observ2%state      .and. &
                  observ1%timeout    == observ2%timeout    .and. &
                  observ1%nresponses == observ2%nresponses)
        if (.not. equals) return

        n = max(0, min(OBSERV_MAX_NRESPONSES, observ1%nresponses))
        if (n > 0) equals = all(dm_response_equals(observ1%responses(1:n), observ2%responses(1:n)))
    end function dm_observ_equals

    pure elemental integer function dm_observ_find(observ, name) result(index)
        !! Searches observation for responses of passed name and returns the
        !! index of the first found. If no response of this name is found, the
        !! index is set to 0.
        type(observ_type), intent(in) :: observ !! Observation.
        character(*),      intent(in) :: name   !! Response name.

        integer :: i, n

        index = 0

        n = max(0, min(OBSERV_MAX_NRESPONSES, observ%nresponses))

        do i = 1, n
            if (observ%responses(i)%name == name) then
                index = i
                return
            end if
        end do
    end function dm_observ_find

    pure elemental logical function dm_observ_has_pattern(observ) result(has)
        !! Returns `.true.` if attribute `pattern` of observation is not empty.
        type(observ_type), intent(in) :: observ !! Observation.

        has = (len_trim(observ%pattern) > 0)
    end function dm_observ_has_pattern

    pure elemental logical function dm_observ_is_disabled(observ) result(disabled)
        type(observ_type), intent(in) :: observ !! Observation.

        disabled = (observ%state == OBSERV_STATE_DISABLED)
    end function dm_observ_is_disabled

    pure elemental logical function dm_observ_is_valid(observ, id, timestamp) result(valid)
        !! Returns `.true.` if given observation is valid. An observation is
        !! valid if it conforms to the following requirements:
        !!
        !! * A valid observation id, node id, sensor id, and target id are set,
        !!   and the observation id does not equal the default UUID, unless
        !!   argument `id` is passed and `.false.`.
        !! * The observation name is a valid id (limited character set, no
        !!   white spaces).
        !! * The attribute _timestamp_ is set and in ISO 8601 format, unless
        !!   argument `timestamp` is passed and `.false.`.
        !! * The attribute _source_ is a valid id.
        !! * The attribute _device_ contains only printable characters.
        !! * The attribute _error_ is a valid error code.
        !! * The attribute _nresponses_ is within the bounds of the array
        !!   _responses_, or 0.
        !! * All responses are valid.
        !!
        use :: dm_string, only: dm_string_is_printable

        type(observ_type), intent(in)           :: observ    !! Observation.
        logical,           intent(in), optional :: id        !! Enable id validation (enabled by default).
        logical,           intent(in), optional :: timestamp !! Enable timestamp validation (enabled by default).

        valid = .false.

        if (dm_present(id, .true.)) then
            if (observ%id == UUID_NONE) return
            if (.not. dm_uuid_is_valid(observ%id)) return

            if (.not. dm_id_is_valid(observ%node_id, NODE_ID_LEN))     return
            if (.not. dm_id_is_valid(observ%sensor_id, SENSOR_ID_LEN)) return
            if (.not. dm_id_is_valid(observ%target_id, TARGET_ID_LEN)) return

            if (len_trim(observ%group_id) > 0 .and. .not. dm_uuid_is_valid(observ%group_id)) return
        end if

        if (dm_present(timestamp, .true.)) then
            if (.not. dm_time_is_valid(observ%timestamp, strict=.true.)) return
        end if

        if (.not. dm_id_is_valid(observ%name, OBSERV_NAME_LEN)) return

        if (len_trim(observ%source) > 0 .and. .not. dm_id_is_valid(observ%source, OBSERV_SOURCE_LEN)) return
        if (len_trim(observ%device) > 0 .and. .not. dm_string_is_printable(observ%device))            return

        if (.not. dm_error_is_valid(observ%error)) return
        if (observ%delay < 0 .or. observ%retries < 0 .or. observ%state < 0 .or. observ%timeout < 0) return
        if (observ%nresponses < 0 .or. observ%nresponses > OBSERV_MAX_NRESPONSES) return

        if (.not. dm_string_is_printable(observ%request)   .or. &
            .not. dm_string_is_printable(observ%response)  .or. &
            .not. dm_string_is_printable(observ%delimiter) .or. &
            .not. dm_string_is_printable(observ%pattern)) return

        if (observ%nresponses > 0) then
            if (.not. all(dm_response_is_valid(observ%responses(1:observ%nresponses)))) return
        end if

        valid = .true.
    end function dm_observ_is_valid

    pure elemental subroutine dm_observ_set_response_error(observ, error, name)
        !! Sets error code of all responses of the given observation. If
        !! argument `name` is given, the error is set only for the first
        !! response of the same name.
        type(observ_type), intent(inout)        :: observ !! Observation.
        integer,           intent(in)           :: error  !! Error code.
        character(*),      intent(in), optional :: name   !! Response name.

        integer :: i, n

        ! Set error code for single response.
        if (present(name)) then
            i = dm_observ_find(observ, name)
            if (i == 0) return
            observ%responses(i)%error = error
            return
        end if

        ! Set error code for all responses.
        n = max(0, min(OBSERV_MAX_NRESPONSES, observ%nresponses))

        do i = 1, n
            observ%responses(i)%error = error
        end do
    end subroutine dm_observ_set_response_error

    pure elemental logical function dm_observ_view_equals(view1, view2) result(equals)
        !! Returns `.true.` if given observation views are equal.
        type(observ_view_type), intent(in) :: view1 !! The first observation view.
        type(observ_view_type), intent(in) :: view2 !! The second observation view.

        equals = (view1%id             == view2%id             .and. &
                  view1%group_id       == view2%group_id       .and. &
                  view1%node_id        == view2%node_id        .and. &
                  view1%sensor_id      == view2%sensor_id      .and. &
                  view1%target_id      == view2%target_id      .and. &
                  view1%timestamp      == view2%timestamp      .and. &
                  view1%name           == view2%name           .and. &
                  view1%error          == view2%error          .and. &
                  view1%response_name  == view2%response_name  .and. &
                  view1%response_unit  == view2%response_unit  .and. &
                  view1%response_type  == view2%response_type  .and. &
                  view1%response_error == view2%response_error .and. &
                  dm_equals(view1%response_value, view2%response_value))
    end function dm_observ_view_equals

    subroutine dm_observ_out(observ, unit)
        !! Prints observation to standard output or given file unit.
        character(*), parameter :: FMT_REAL = '(1pg0.12)'

        type(observ_type), intent(in)           :: observ !! Observation.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: i, unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("observ.id: ", a)')          trim(observ%id)
        write (unit_, '("observ.group_id: ", a)')    trim(observ%group_id)
        write (unit_, '("observ.node_id: ", a)')     trim(observ%node_id)
        write (unit_, '("observ.sensor_id: ", a)')   trim(observ%sensor_id)
        write (unit_, '("observ.target_id: ", a)')   trim(observ%target_id)
        write (unit_, '("observ.name: ", a)')        trim(observ%name)
        write (unit_, '("observ.timestamp: ", a)')   observ%timestamp
        write (unit_, '("observ.source: ", a)')      trim(observ%source)
        write (unit_, '("observ.device: ", a)')      trim(observ%device)
        write (unit_, '("observ.request: ", a)')     trim(observ%request)
        write (unit_, '("observ.response: ", a)')    trim(observ%response)
        write (unit_, '("observ.delimiter: ", a)')   trim(observ%delimiter)
        write (unit_, '("observ.pattern: ", a)')     trim(observ%pattern)
        write (unit_, '("observ.delay: ", i0)')      observ%delay
        write (unit_, '("observ.error: ", i0)')      observ%error
        write (unit_, '("observ.mode: ", i0)')       observ%mode
        write (unit_, '("observ.retries: ", i0)')    observ%retries
        write (unit_, '("observ.state: ", i0)')      observ%state
        write (unit_, '("observ.timeout: ", i0)')    observ%timeout
        write (unit_, '("observ.nresponses: ", i0)') observ%nresponses

        do i = 1, observ%nresponses
            write (unit_, '("observ.responses(", i0, ").name: ", a)')   i, trim(observ%responses(i)%name)
            write (unit_, '("observ.responses(", i0, ").unit: ", a)')   i, trim(observ%responses(i)%unit)
            write (unit_, '("observ.responses(", i0, ").type: ", i0)')  i, observ%responses(i)%type
            write (unit_, '("observ.responses(", i0, ").error: ", i0)') i, observ%responses(i)%error
            write (unit_, '("observ.responses(", i0, ").value: ", ' // FMT_REAL // ')') i, observ%responses(i)%value
        end do
    end subroutine dm_observ_out

    pure elemental subroutine dm_observ_set(observ, id, group_id, node_id, sensor_id, target_id, timestamp, name, source, &
                                            device, request, response, delimiter, pattern, delay, error, mode, retries, &
                                            state, timeout, nresponses)
        !! Sets attributes of observation, except responses.
        type(observ_type), intent(inout)        :: observ     !! Observation.
        character(*),      intent(in), optional :: id         !! Observation id.
        character(*),      intent(in), optional :: group_id   !! Group id.
        character(*),      intent(in), optional :: node_id    !! Node id.
        character(*),      intent(in), optional :: sensor_id  !! Sensor id.
        character(*),      intent(in), optional :: target_id  !! Target id.
        character(*),      intent(in), optional :: timestamp  !! ISO 8601 timestamp.
        character(*),      intent(in), optional :: name       !! Observation name.
        character(*),      intent(in), optional :: source     !! Observation source.
        character(*),      intent(in), optional :: device     !! Physical device.
        character(*),      intent(in), optional :: request    !! Request string.
        character(*),      intent(in), optional :: response   !! Request string.
        character(*),      intent(in), optional :: delimiter  !! Delimiter.
        character(*),      intent(in), optional :: pattern    !! Regular expression pattern.
        integer,           intent(in), optional :: delay      !! Post-observation delay [msec].
        integer,           intent(in), optional :: error      !! Error code.
        integer,           intent(in), optional :: mode       !! Mode enumerator.
        integer,           intent(in), optional :: retries    !! Number of retries.
        integer,           intent(in), optional :: state      !! State enumerator.
        integer,           intent(in), optional :: timeout    !! Timeout [msec].
        integer,           intent(in), optional :: nresponses !! Number of responses.

        if (present(id))         observ%id         = id
        if (present(group_id))   observ%group_id   = group_id
        if (present(node_id))    observ%node_id    = node_id
        if (present(sensor_id))  observ%sensor_id  = sensor_id
        if (present(target_id))  observ%target_id  = target_id
        if (present(timestamp))  observ%timestamp  = timestamp
        if (present(name))       observ%name       = name
        if (present(source))     observ%source     = source
        if (present(device))     observ%device     = device
        if (present(request))    observ%request    = request
        if (present(response))   observ%response   = response
        if (present(delimiter))  observ%delimiter  = delimiter
        if (present(pattern))    observ%pattern    = pattern
        if (present(delay))      observ%delay      = delay
        if (present(error))      observ%error      = error
        if (present(mode))       observ%mode       = mode
        if (present(retries))    observ%retries    = retries
        if (present(state))      observ%state      = state
        if (present(timeout))    observ%timeout    = timeout
        if (present(nresponses)) observ%nresponses = nresponses
    end subroutine dm_observ_set

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    integer function observ_add_response_int32(observ, name, unit, value, error) result(rc)
        type(observ_type), intent(inout)        :: observ !! Observation.
        character(*),      intent(in)           :: name   !! Response name.
        character(*),      intent(in)           :: unit   !! Response unit.
        integer(i4),       intent(in)           :: value  !! Response value.
        integer,           intent(in), optional :: error  !! Response error.

        type(response_type) :: response

        response = response_type(name=name, unit=unit, type=RESPONSE_TYPE_INT32, value=dm_to_real64(value))
        if (present(error)) response%error = error
        rc = observ_add_response_type(observ, response)
    end function observ_add_response_int32

    integer function observ_add_response_int64(observ, name, unit, value, error) result(rc)
        type(observ_type), intent(inout)        :: observ !! Observation.
        character(*),      intent(in)           :: name   !! Response name.
        character(*),      intent(in)           :: unit   !! Response unit.
        integer(i8),       intent(in)           :: value  !! Response value.
        integer,           intent(in), optional :: error  !! Response error.

        type(response_type) :: response

        response = response_type(name=name, unit=unit, type=RESPONSE_TYPE_INT64, value=dm_to_real64(value))
        if (present(error)) response%error = error
        rc = observ_add_response_type(observ, response)
    end function observ_add_response_int64

    integer function observ_add_response_real32(observ, name, unit, value, error) result(rc)
        type(observ_type), intent(inout)        :: observ !! Observation.
        character(*),      intent(in)           :: name   !! Response name.
        character(*),      intent(in)           :: unit   !! Response unit.
        real(r4),          intent(in)           :: value  !! Response value.
        integer,           intent(in), optional :: error  !! Response error.

        type(response_type) :: response

        response = response_type(name=name, unit=unit, type=RESPONSE_TYPE_REAL32, value=dm_to_real64(value))
        if (present(error)) response%error = error
        rc = observ_add_response_type(observ, response)
    end function observ_add_response_real32

    integer function observ_add_response_real64(observ, name, unit, value, error) result(rc)
        type(observ_type), intent(inout)        :: observ !! Observation.
        character(*),      intent(in)           :: name   !! Response name.
        character(*),      intent(in)           :: unit   !! Response unit.
        real(r8),          intent(in)           :: value  !! Response value.
        integer,           intent(in), optional :: error  !! Response error.

        type(response_type) :: response

        response = response_type(name=name, unit=unit, type=RESPONSE_TYPE_REAL64, value=value)
        if (present(error)) response%error = error
        rc = observ_add_response_type(observ, response)
    end function observ_add_response_real64

    integer function observ_add_response_type(observ, response) result(rc)
        !! Validates and appends response to the given observation.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if the responses array is full.
        !! * `E_INVALID` if the response is invalid.
        !!
        !! The observ attribute `nresponses` must be between 0 and one less
        !! than `OBSERV_MAX_NRESPONSES` for the response to be added.
        type(observ_type),   intent(inout) :: observ   !! Observation.
        type(response_type), intent(in)    :: response !! Response to add.

        rc = E_BOUNDS
        if (observ%nresponses < 0 .or. observ%nresponses >= OBSERV_MAX_NRESPONSES) return

        rc = E_INVALID
        if (.not. dm_response_is_valid(response)) return

        rc = E_NONE
        observ%nresponses = observ%nresponses + 1
        observ%responses(observ%nresponses) = response
    end function observ_add_response_type

    integer function observ_get_response_byte(observ, name, value, unit, type, error, default) result(rc)
        !! Returns byte response as single character value, unit, type, and error
        !! of response of name `name`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the observation has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type byte.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_BYTE

        type(observ_type),            intent(in)            :: observ  !! Observation.
        character(*),                 intent(in)            :: name    !! Response name.
        character,                    intent(inout)         :: value   !! Response value.
        character(RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                      intent(out), optional :: type    !! Response value type.
        integer,                      intent(out), optional :: error   !! Response error.
        character,                    intent(in),  optional :: default !! Default value.

        integer :: i

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (observ%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_observ_find(observ, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (observ%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            call dm_response_get(observ%responses(i), unit=unit, type=type, error=error)
            value = char(floor(observ%responses(i)%value, i4))
        end block response_block

        if (rc == E_NONE) return
        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end function observ_get_response_byte

    integer function observ_get_response_int32(observ, name, value, unit, type, error, default) result(rc)
        !! Returns 4-byte integer response value, unit, type, and error of
        !! response of name `name`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the observation has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type int32.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_INT32

        type(observ_type),            intent(in)            :: observ  !! Observation.
        character(*),                 intent(in)            :: name    !! Response name.
        integer(i4),                  intent(inout)         :: value   !! Response value.
        character(RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                      intent(out), optional :: type    !! Response value type.
        integer,                      intent(out), optional :: error   !! Response error.
        integer(i4),                  intent(in),  optional :: default !! Default value.

        integer :: i

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (observ%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_observ_find(observ, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (observ%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            call dm_response_get(observ%responses(i), unit=unit, type=type, error=error)
            value = floor(observ%responses(i)%value, i4)
        end block response_block

        if (rc == E_NONE) return
        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end function observ_get_response_int32

    integer function observ_get_response_int64(observ, name, value, unit, type, error, default) result(rc)
        !! Returns 8-byte integer response value, unit, type, and error of
        !! response of name `name`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the observation has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type int64.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_INT64

        type(observ_type),            intent(in)            :: observ  !! Observation.
        character(*),                 intent(in)            :: name    !! Response name.
        integer(i8),                  intent(inout)         :: value   !! Response value.
        character(RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                      intent(out), optional :: type    !! Response value type.
        integer,                      intent(out), optional :: error   !! Response error.
        integer(i8),                  intent(in),  optional :: default !! Default value.

        integer :: i

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (observ%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_observ_find(observ, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (observ%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            call dm_response_get(observ%responses(i), unit=unit, type=type, error=error)
            value = floor(observ%responses(i)%value, i8)
        end block response_block

        if (rc == E_NONE) return
        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end function observ_get_response_int64

    integer function observ_get_response_logical(observ, name, value, unit, type, error, default) result(rc)
        !! Returns logical response value, unit, type, and error of response of
        !! name `name`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the observation has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type logical.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_LOGICAL

        type(observ_type),            intent(in)            :: observ  !! Observation.
        character(*),                 intent(in)            :: name    !! Response name.
        logical,                      intent(inout)         :: value   !! Response value.
        character(RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                      intent(out), optional :: type    !! Response value type.
        integer,                      intent(out), optional :: error   !! Response error.
        logical,                      intent(in),  optional :: default !! Default value.

        integer :: i

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (observ%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_observ_find(observ, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (observ%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            call dm_response_get(observ%responses(i), unit=unit, type=type, error=error)
            value = (floor(observ%responses(i)%value) >= 1)
        end block response_block

        if (rc == E_NONE) return
        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end function observ_get_response_logical

    integer function observ_get_response_real32(observ, name, value, unit, type, error, default) result(rc)
        !! Returns 4-byte real response value, unit, type, and error of
        !! response of name `name`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the observation has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type real32.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_REAL32

        type(observ_type),            intent(in)            :: observ  !! Observation.
        character(*),                 intent(in)            :: name    !! Response name.
        real(r4),                     intent(inout)         :: value   !! Response value.
        character(RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                      intent(out), optional :: type    !! Response value type.
        integer,                      intent(out), optional :: error   !! Response error.
        real(r4),                     intent(in),  optional :: default !! Default value.

        integer :: i

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (observ%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_observ_find(observ, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (observ%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            call dm_response_get(observ%responses(i), unit=unit, type=type, error=error)
            value = real(observ%responses(i)%value, r4)
        end block response_block

        if (rc == E_NONE) return
        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end function observ_get_response_real32

    integer function observ_get_response_real64(observ, name, value, unit, type, error, default) result(rc)
        !! Returns 8-byte real response value, unit, type, and error of
        !! response of name `name`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the observation has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !! * `E_TYPE` if the response value is not of type real64.
        !!
        !! On error, `value` will not be modified, unless `default` is passed.
        integer, parameter :: VALUE_TYPE = RESPONSE_TYPE_REAL64

        type(observ_type),            intent(in)            :: observ  !! Observation.
        character(*),                 intent(in)            :: name    !! Response name.
        real(r8),                     intent(inout)         :: value   !! Response value.
        character(RESPONSE_UNIT_LEN), intent(out), optional :: unit    !! Response unit.
        integer,                      intent(out), optional :: type    !! Response value type.
        integer,                      intent(out), optional :: error   !! Response error.
        real(r8),                     intent(in),  optional :: default !! Default value.

        integer :: i

        if (present(default)) value = default

        response_block: block
            rc = E_EMPTY
            if (observ%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_observ_find(observ, name)
            if (i == 0) exit response_block

            rc = E_TYPE
            if (observ%responses(i)%type /= VALUE_TYPE) exit response_block

            rc = E_NONE
            call dm_response_get(observ%responses(i), unit=unit, type=type, error=error)
            value = observ%responses(i)%value
        end block response_block

        if (rc == E_NONE) return
        if (present(unit))  unit  = ' '
        if (present(type))  type  = VALUE_TYPE
        if (present(error)) error = E_NONE
    end function observ_get_response_real64

    integer function observ_get_response_type(observ, name, response, default) result(rc)
        !! Returns response of name `name` in `response`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the observation has no responses.
        !! * `E_NOT_FOUND` if a response of the given name does not exist.
        !!
        !! On error, an empty response will be returned, unless `default` is
        !! passed.
        type(observ_type),   intent(in)           :: observ   !! Observation.
        character(*),        intent(in)           :: name     !! Response name.
        type(response_type), intent(out)          :: response !! Response type.
        type(response_type), intent(in), optional :: default  !! Default response.

        integer :: i

        response_block: block
            rc = E_EMPTY
            if (observ%nresponses == 0) exit response_block

            rc = E_NOT_FOUND
            i = dm_observ_find(observ, name)
            if (i == 0) exit response_block

            rc = E_NONE
            response = observ%responses(i)
        end block response_block

        if (present(default) .and. dm_is_error(rc)) response = default
    end function observ_get_response_type
end module dm_observ
