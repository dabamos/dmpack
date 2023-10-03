! Author:  Philipp Engel
! Licence: ISC
module dm_log
    !! Log type and log level declaration.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_node
    use :: dm_observ
    use :: dm_sensor
    use :: dm_target
    use :: dm_time
    use :: dm_uuid
    implicit none (type, external)
    private

    ! Log level.
    integer, parameter, public :: LOG_NONE     = 0 !! Invalid log level, not used by DMPACK.
    integer, parameter, public :: LOG_DEBUG    = 1 !! For debugging purposes.
    integer, parameter, public :: LOG_INFO     = 2 !! For information regarding normal system behaviour.
    integer, parameter, public :: LOG_WARNING  = 3 !! For events requiring the attention of the system operator.
    integer, parameter, public :: LOG_ERROR    = 4 !! Unexpected behaviour, may indicate failure.
    integer, parameter, public :: LOG_CRITICAL = 5 !! Reserved for monitoring events, not used by DMPACK internally.
    integer, parameter, public :: LOG_NLEVEL   = 6 !! Number of log level.

    ! Log parameters.
    integer, parameter, public :: LOG_ID_LEN      = UUID_LEN !! Max. log id length.
    integer, parameter, public :: LOG_SOURCE_LEN  = ID_LEN   !! Max. log source length.
    integer, parameter, public :: LOG_MESSAGE_LEN = 512      !! Max. log message length.

    character(len=*), parameter, public :: LOG_LEVEL_NAMES(0:LOG_NLEVEL - 1) = [ &
        character(len=8) :: 'NONE', 'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL' ] !! Log level strings.

    character(len=*), parameter, public :: LOG_LEVEL_NAMES_LOWER(0:LOG_NLEVEL - 1) = [ &
        character(len=8) :: 'none', 'debug', 'info', 'warning', 'error', 'critical' ] !! Log level strings in lower-case.

    type, public :: log_type
        !! Log message type.
        character(len=LOG_ID_LEN)      :: id        = UUID_DEFAULT !! Database log id (mandatory).
        integer                        :: level     = LOG_WARNING  !! Log level (mandatory).
        integer                        :: error     = E_NONE       !! Error code (optional).
        character(len=TIME_LEN)        :: timestamp = TIME_DEFAULT !! Timestamp, shall be in ISO 8601 plus milliseconds and time zone (mandatory).
        character(len=NODE_ID_LEN)     :: node_id   = ' '          !! Sensor node ID (optional).
        character(len=SENSOR_ID_LEN)   :: sensor_id = ' '          !! Sensor ID (optional).
        character(len=TARGET_ID_LEN)   :: target_id = ' '          !! Target ID (optional).
        character(len=OBSERV_ID_LEN)   :: observ_id = ' '          !! Observation ID (optional).
        character(len=LOG_SOURCE_LEN)  :: source    = ' '          !! Log message source (optional).
        character(len=LOG_MESSAGE_LEN) :: message   = ' '          !! Log message (mandatory).
    end type log_type

    integer, parameter, public :: LOG_SIZE = storage_size(log_type(), kind=i8) / 8 !! Size of `log_type` in bytes.

    interface dm_log_valid
        !! Generic log validation function.
        module procedure :: dm_log_valid_level
        module procedure :: dm_log_valid_log
    end interface

    interface operator (==)
        !! Returns whether logs are equal.
        module procedure :: dm_log_equals
    end interface

    public :: operator (==)

    public :: dm_log_equals
    public :: dm_log_out
    public :: dm_log_valid

    private :: dm_log_valid_level
    private :: dm_log_valid_log
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
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

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    pure elemental logical function dm_log_valid_level(level) result(valid)
        !! Returns `.true.` if given log level is valid, i.e., either
        !! `LOG_DEBUG`, `LOG_WARNING`, `LOG_ERROR`, or `LOG_CRITICAL`.
        !!
        !! The level `LOG_NONE` is invalid.
        integer, intent(in) :: level !! Log level.

        valid = .false.
        if (level < LOG_DEBUG .or. level > LOG_CRITICAL) return
        valid = .true.
    end function dm_log_valid_level

    pure elemental logical function dm_log_valid_log(log) result(valid)
        !! Returns `.true.` if given log is valid.
        type(log_type), intent(in) :: log !! Log to validate.

        valid = .false.

        if (.not. dm_log_valid(log%level)) return
        if (.not. dm_error_valid(log%error)) return
        if (log%id == UUID_DEFAULT) return
        if (.not. dm_uuid4_valid(log%id)) return
        if (.not. dm_time_valid(log%timestamp)) return

        valid = .true.
    end function dm_log_valid_log
end module dm_log
