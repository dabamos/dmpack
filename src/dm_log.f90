! Author:  Philipp Engel
! Licence: ISC
module dm_log
    !! Log type and log level declaration.
    !!
    !! The following log levels are supported:
    !!
    !! | Level | String     | Description                               |
    !! |-------|------------|-------------------------------------------|
    !! | 0     | `none`     | invalid log level (unused)                |
    !! | 1     | `debug`    | debug level                               |
    !! | 2     | `info`     | hint level                                |
    !! | 3     | `warning`  | warning level                             |
    !! | 4     | `error`    | non-critical error level                  |
    !! | 5     | `critical` | critical error level (not used by DMPACK) |
    !! | 6     | `user`     | user-defined level (not used by DMPACK)   |
    !!
    !! Log level _critical_ is reserved for monitoring events and not used by
    !! DMPACK internally. Level _user_ is reserved for user-defined events and
    !! also not used.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_node,   only: NODE_ID_LEN
    use :: dm_observ, only: OBSERV_ID_LEN
    use :: dm_sensor, only: SENSOR_ID_LEN
    use :: dm_target, only: TARGET_ID_LEN
    use :: dm_time
    use :: dm_uuid
    implicit none (type, external)
    private

    ! Log level.
    integer, parameter, public :: LL_NONE     = 0 !! Invalid log level, not used by DMPACK.
    integer, parameter, public :: LL_DEBUG    = 1 !! For debugging purposes.
    integer, parameter, public :: LL_INFO     = 2 !! For information regarding normal system behaviour.
    integer, parameter, public :: LL_WARNING  = 3 !! For events requiring the attention of the system operator.
    integer, parameter, public :: LL_ERROR    = 4 !! Unexpected behaviour, may indicate failure.
    integer, parameter, public :: LL_CRITICAL = 5 !! Reserved for monitoring events, not used by DMPACK internally.
    integer, parameter, public :: LL_USER     = 6 !! User-defined level, not used by DMPACK.
    integer, parameter, public :: LL_LAST     = 6 !! Never use this.

    ! Log parameters.
    integer, parameter, public :: LOG_NLEVEL      = LL_LAST + 1 !! Number of log level.
    integer, parameter, public :: LOG_ID_LEN      = UUID_LEN    !! Max. log id length.
    integer, parameter, public :: LOG_SOURCE_LEN  = ID_LEN      !! Max. log source length.
    integer, parameter, public :: LOG_MESSAGE_LEN = 512         !! Max. log message length.

    integer, parameter, public :: LOG_LEVEL_NAME_LEN = 8

    character(len=*), parameter, public :: LOG_LEVEL_NAMES(0:LL_LAST) = [ &
        character(len=LOG_LEVEL_NAME_LEN) :: 'NONE', 'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL', 'USER' &
    ] !! Log level strings.

    character(len=*), parameter, public :: LOG_LEVEL_NAMES_LOWER(0:LL_LAST) = [ &
        character(len=LOG_LEVEL_NAME_LEN) :: 'none', 'debug', 'info', 'warning', 'error', 'critical', 'user' &
    ] !! Log level strings in lower-case.

    type, public :: log_type
        !! Log message type.
        character(len=LOG_ID_LEN)      :: id        = UUID_DEFAULT !! Database log id (mandatory).
        integer                        :: level     = LL_WARNING   !! Log level (mandatory).
        integer                        :: error     = E_NONE       !! Error code (optional).
        character(len=TIME_LEN)        :: timestamp = TIME_DEFAULT !! Timestamp, shall be in ISO 8601 plus milliseconds and time zone (mandatory).
        character(len=NODE_ID_LEN)     :: node_id   = ' '          !! Sensor node ID (optional).
        character(len=SENSOR_ID_LEN)   :: sensor_id = ' '          !! Sensor ID (optional).
        character(len=TARGET_ID_LEN)   :: target_id = ' '          !! Target ID (optional).
        character(len=OBSERV_ID_LEN)   :: observ_id = ' '          !! Observation ID (optional).
        character(len=LOG_SOURCE_LEN)  :: source    = ' '          !! Log message source (optional).
        character(len=LOG_MESSAGE_LEN) :: message   = ' '          !! Log message (mandatory).
    end type log_type

    integer, parameter, public :: LOG_SIZE = storage_size(log_type()) / 8 !! Size of `log_type` in bytes.

    interface operator (==)
        !! Returns whether logs are equal.
        module procedure :: dm_log_equals
    end interface

    public :: operator (==)

    public :: dm_log_equals
    public :: dm_log_is_valid
    public :: dm_log_level_from_name
    public :: dm_log_level_from_string
    public :: dm_log_level_is_valid
    public :: dm_log_out
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    pure elemental logical function dm_log_equals(log1, log2) result(equals)
        !! Returns `.true.` if given logs are equal.
        type(log_type), intent(in) :: log1 !! The first log.
        type(log_type), intent(in) :: log2 !! The second log.

        equals = .false.

        if (log1%id        /= log2%id)        return
        if (log1%level     /= log2%level)     return
        if (log1%error     /= log2%error)     return
        if (log1%timestamp /= log2%timestamp) return
        if (log1%node_id   /= log2%node_id)   return
        if (log1%sensor_id /= log2%sensor_id) return
        if (log1%target_id /= log2%target_id) return
        if (log1%observ_id /= log2%observ_id) return
        if (log1%source    /= log2%source)    return
        if (log1%message   /= log2%message)   return

        equals = .true.
    end function dm_log_equals

    pure elemental logical function dm_log_is_valid(log) result(valid)
        !! Returns `.true.` if given log is valid. A log is valid if it conforms
        !! to the following rules:
        !!
        !! * The log level and the error code are valid.
        !! * The log id is a valid UUID and not the default UUID.
        !! * The time stamp is in ISO 8601 format.
        !! * All ASCII characters of the log message are printable.
        !!
        use :: dm_string, only: dm_string_is_printable

        type(log_type), intent(in) :: log !! Log to validate.

        valid = .false.

        if (.not. dm_log_level_is_valid(log%level))    return
        if (.not. dm_error_is_valid(log%error))        return
        if (log%id == UUID_DEFAULT)                    return
        if (.not. dm_uuid4_is_valid(log%id))           return
        if (.not. dm_time_is_valid(log%timestamp))     return
        if (.not. dm_string_is_printable(log%message)) return

        valid = .true.
    end function dm_log_is_valid

    pure elemental integer function dm_log_level_from_name(name) result(level)
        !! Returns log level from string argument `name`. The string is
        !! converted to lower-case before. If `name` neither matches `none`,
        !! `debug`, `warning`, `error`, `critical`, nor `user` this function
        !! returns `LL_NONE`.
        use :: dm_string, only: dm_to_lower

        character(len=*), intent(in) :: name !! Log level name.

        character(len=LOG_LEVEL_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (LOG_LEVEL_NAMES_LOWER(LL_DEBUG));    level = LL_DEBUG
            case (LOG_LEVEL_NAMES_LOWER(LL_INFO));     level = LL_INFO
            case (LOG_LEVEL_NAMES_LOWER(LL_WARNING));  level = LL_WARNING
            case (LOG_LEVEL_NAMES_LOWER(LL_ERROR));    level = LL_ERROR
            case (LOG_LEVEL_NAMES_LOWER(LL_CRITICAL)); level = LL_CRITICAL
            case (LOG_LEVEL_NAMES_LOWER(LL_USER));     level = LL_USER
            case default;                              level = LL_NONE
        end select
    end function dm_log_level_from_name

    pure elemental integer function dm_log_level_from_string(string) result(level)
        !! Return log level from string, either level name or numeric level.
        use :: dm_string, only: dm_string_to

        character(len=*), intent(in) :: string !! Log level name or numeric level.

        integer :: rc

        ! Convert string to integer.
        call dm_string_to(string, level, error=rc)
        if (dm_is_ok(rc)) return

        ! On error, try to read level from level name. An invalid log level name
        ! is turned into `LL_NONE`.
        level = dm_log_level_from_name(string)
    end function dm_log_level_from_string

    pure elemental logical function dm_log_level_is_valid(level) result(valid)
        !! Returns `.true.` if given log level is valid. The following level
        !! are valid:
        !!
        !! * `LL_DEBUG`
        !! * `LL_WARNING`
        !! * `LL_ERROR`
        !! * `LL_CRITICAL`
        !! * `LL_USER`
        !!
        !! The level `LL_NONE` is invalid.
        integer, intent(in) :: level !! Log level.

        valid = (level > LL_NONE .and. level <= LL_LAST)
    end function dm_log_level_is_valid

    subroutine dm_log_out(log, unit)
        !! Prints log to standard output or given file unit.
        type(log_type), intent(inout)        :: log  !! Log type.
        integer,        intent(in), optional :: unit !! File unit.

        integer :: unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        write (unit_, '("log.id: ", a)')        trim(log%id)
        write (unit_, '("log.level: ", i0)')    log%level
        write (unit_, '("log.error: ", i0)')    log%error
        write (unit_, '("log.timestamp: ", a)') trim(log%timestamp)
        write (unit_, '("log.node_id: ", a)')   trim(log%node_id)
        write (unit_, '("log.sensor_id: ", a)') trim(log%sensor_id)
        write (unit_, '("log.target_id: ", a)') trim(log%target_id)
        write (unit_, '("log.observ_id: ", a)') trim(log%observ_id)
        write (unit_, '("log.source: ", a)')    trim(log%source)
        write (unit_, '("log.message: ", a)')   trim(log%message)
    end subroutine dm_log_out
end module dm_log
