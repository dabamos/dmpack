! Author:  Philipp Engel
! Licence: ISC
module dm_log
    !! Log type and log level declaration.
    use :: dm_error
    use :: dm_id
    use :: dm_node
    use :: dm_observ
    use :: dm_sensor
    use :: dm_target
    use :: dm_time
    use :: dm_type
    use :: dm_uuid
    implicit none (type, external)
    private

    ! Log level.
    integer, parameter, public :: LOG_NONE     = 0
    integer, parameter, public :: LOG_DEBUG    = 1
    integer, parameter, public :: LOG_INFO     = 2
    integer, parameter, public :: LOG_WARNING  = 3
    integer, parameter, public :: LOG_ERROR    = 4
    integer, parameter, public :: LOG_CRITICAL = 5
    integer, parameter, public :: LOG_NLEVEL   = 6

    ! Log parameters.
    integer, parameter, public :: LOG_ID_LEN      = ID_LEN
    integer, parameter, public :: LOG_SOURCE_LEN  = ID_LEN
    integer, parameter, public :: LOG_MESSAGE_LEN = 512

    character(len=*), parameter, public :: LOG_LEVEL_NAMES(0:LOG_NLEVEL - 1) = [ &
        character(len=8) :: 'NONE', 'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL' ] !! Log level strings.

    type, public :: log_type
        !! Log message type.
        character(len=LOG_ID_LEN)      :: id        = UUID_DEFAULT !! Database log id (mandatory).
        integer                        :: level     = LOG_WARNING   !! Log level (mandatory).
        integer                        :: error     = E_NONE        !! Error code (optional).
        character(len=TIME_LEN)        :: timestamp = TIME_DEFAULT  !! Timestamp, should be in ISO 8601 plus milliseconds and time zone (mandatory).
        character(len=NODE_ID_LEN)     :: node_id   = ' '           !! Sensor node ID (optional).
        character(len=SENSOR_ID_LEN)   :: sensor_id = ' '           !! Sensor ID (optional).
        character(len=TARGET_ID_LEN)   :: target_id = ' '           !! Target ID (optional).
        character(len=OBSERV_ID_LEN)   :: observ_id = ' '           !! Observation ID (optional).
        character(len=LOG_SOURCE_LEN)  :: source    = ' '           !! Log message source (optional).
        character(len=LOG_MESSAGE_LEN) :: message   = ' '           !! Log message (mandatory).
    end type log_type

    integer(kind=i8), parameter, public :: LOG_SIZE = storage_size(log_type(), kind=i8) / 8 !! Log type size in bytes.

    interface operator (==)
        module procedure :: dm_log_equals
    end interface

    public :: operator (==)

    public :: dm_log_equals
    public :: dm_log_valid
contains
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

    pure elemental logical function dm_log_valid(log) result(valid)
        !! Returns `.true.` if given log is valid.
        type(log_type), intent(in) :: log !! Log to validate.

        valid = .false.

        if (log%level < LOG_NONE .or. log%level > LOG_CRITICAL) return
        if (log%id == UUID_DEFAULT) return
        if (.not. dm_uuid4_valid(log%id)) return
        if (len_trim(log%timestamp) == 0) return

        valid = .true.
    end function dm_log_valid
end module dm_log
