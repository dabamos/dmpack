! Author:  Philipp Engel
! Licence: ISC
module dm_error
    !! Error codes, error messages, and utility routines.
    use :: dm_ascii
    use :: dm_kind
    implicit none (type, external)
    private

    ! Generic errors.
    integer, parameter, public :: E_NONE           =   0 !! No error.
    integer, parameter, public :: E_ERROR          =   1 !! Generic error.
    integer, parameter, public :: E_DUMMY          =   2 !! Dummy error or not implemented.
    integer, parameter, public :: E_INVALID        =   3 !! Invalid input/argument.
    integer, parameter, public :: E_INCOMPLETE     =   4 !! Input/argument missing.
    integer, parameter, public :: E_TYPE           =   5 !! Type error.
    integer, parameter, public :: E_IO             =   6 !! I/O operation failed.
    integer, parameter, public :: E_READ           =   7 !! Read operation failed.
    integer, parameter, public :: E_WRITE          =   8 !! Write operation failed.
    integer, parameter, public :: E_EOF            =   9 !! I/O end of file.
    integer, parameter, public :: E_EOR            =  10 !! I/O end of record.
    integer, parameter, public :: E_ALLOC          =  11 !! Memory allocation failed.
    integer, parameter, public :: E_BOUNDS         =  12 !! Out of bounds error.
    integer, parameter, public :: E_EXIST          =  13 !! Resource exists.
    integer, parameter, public :: E_SYSTEM         =  14 !! System call failed.
    integer, parameter, public :: E_TIMEOUT        =  15 !! Timeout occured.
    integer, parameter, public :: E_EMPTY          =  16 !! No data.
    integer, parameter, public :: E_LIMIT          =  17 !! Memory limit reached.
    integer, parameter, public :: E_FORMAT         =  18 !! Format error.
    integer, parameter, public :: E_NOT_FOUND      =  19 !! Resource not found.
    integer, parameter, public :: E_READ_ONLY      =  20 !! Read-only access.
    integer, parameter, public :: E_CONFIG         =  21 !! Invalid configuration error.
    integer, parameter, public :: E_HDF5           =  22 !! HDF5 error.

    ! Database errors.
    integer, parameter, public :: E_DB             =  30 !! Generic database error.
    integer, parameter, public :: E_DB_ID          =  31 !! Invalid database application id.
    integer, parameter, public :: E_DB_MEM         =  32 !! No memory or disk full.
    integer, parameter, public :: E_DB_BUSY        =  33 !! Database is busy.
    integer, parameter, public :: E_DB_LOCKED      =  34 !! Database is locked.
    integer, parameter, public :: E_DB_EXEC        =  35 !! Execution failed.
    integer, parameter, public :: E_DB_CONSTRAINT  =  36 !! Contraint error.
    integer, parameter, public :: E_DB_TRANSACTION =  37 !! Transaction failed.
    integer, parameter, public :: E_DB_ROLLBACK    =  38 !! Transaction rollback error.
    integer, parameter, public :: E_DB_PREPARE     =  39 !! Prepare failed.
    integer, parameter, public :: E_DB_FINALIZE    =  40 !! Statement error.
    integer, parameter, public :: E_DB_BIND        =  41 !! Bind failed.
    integer, parameter, public :: E_DB_TYPE        =  42 !! Type mismatch.
    integer, parameter, public :: E_DB_STEP        =  43 !! Step failed.
    integer, parameter, public :: E_DB_NO_ROWS     =  44 !! No rows returned.
    integer, parameter, public :: E_DB_BACKUP      =  45 !! Backup error.

    ! Command-line argument errors.
    integer, parameter, public :: E_ARG            =  50 !! Generic argument error.
    integer, parameter, public :: E_ARG_NOT_FOUND  =  51 !! Option not passed.
    integer, parameter, public :: E_ARG_INVALID    =  52 !! Invalid option or argument missing.
    integer, parameter, public :: E_ARG_NO_VALUE   =  53 !! Argument given but no value.
    integer, parameter, public :: E_ARG_TYPE       =  54 !! Type mismatch.
    integer, parameter, public :: E_ARG_LENGTH     =  55 !! Wrong value length.

    ! Message queue errors.
    integer, parameter, public :: E_MQUEUE         =  60 !! Generic message queue error.
    integer, parameter, public :: E_MQUEUE_EMPTY   =  61 !! Empty message.

    ! Matching errors.
    integer, parameter, public :: E_REGEX          =  70 !! Generic regular expression error.
    integer, parameter, public :: E_REGEX_COMPILE  =  71 !! Failed to compile regular expression.
    integer, parameter, public :: E_REGEX_EXCEEDED =  72 !! Number of matches exceeds array size.
    integer, parameter, public :: E_REGEX_NO_MATCH =  73 !! No match.
    integer, parameter, public :: E_REGEX_NO_GROUP =  74 !! No group.

    ! Sensor errors.
    integer, parameter, public :: E_SENSOR         =  80 !! Generic sensor error.

    ! RPC errors.
    integer, parameter, public :: E_RPC            =  90 !! Generic RPC error.
    integer, parameter, public :: E_RPC_CONNECT    =  91 !! RPC connection error.
    integer, parameter, public :: E_RPC_SSL        =  92 !! RPC SSL/TLS error.
    integer, parameter, public :: E_RPC_API        =  93 !! RPC API call failed.
    integer, parameter, public :: E_RPC_AUTH       =  94 !! Unauthorised.
    integer, parameter, public :: E_RPC_CONFLICT   =  95 !! Resource exists.
    integer, parameter, public :: E_RPC_SERVER     =  96 !! Internal server error.

    integer, parameter, public :: E_MAIL           = 100 !! Generic SMTP error.
    integer, parameter, public :: E_MQTT           = 110 !! Generic MQTT error.

    integer, parameter, public :: E_LUA            = 120 !! Generic Lua error.
    integer, parameter, public :: E_LUA_YIELD      = 121 !! Lua thread (coroutine) yields.
    integer, parameter, public :: E_LUA_RUNTIME    = 122 !! Lua runtime error.
    integer, parameter, public :: E_LUA_SYNTAX     = 123 !! Lua syntax error.
    integer, parameter, public :: E_LUA_MEM        = 124 !! Lua memory allocation error.
    integer, parameter, public :: E_LUA_ERROR      = 125 !! Lua message handling error.
    integer, parameter, public :: E_LUA_FILE       = 126 !! Lua file I/O error.

    integer, parameter, public :: E_LAST           = 126 !! DO NOT USE.

    interface dm_perror
        !! Alias for `dm_error_out()`.
        module procedure :: dm_error_out
    end interface

    public :: dm_error_message
    public :: dm_error_out
    public :: dm_error_valid
    public :: dm_is_error
    public :: dm_is_ok
    public :: dm_perror
    public :: dm_stop
contains
    pure function dm_error_message(error) result(str)
        !! Returns error message of given error code `error`.
        integer, intent(in)           :: error !! Error code.
        character(len=:), allocatable :: str   !! Error string.

        select case (error)
            ! General errors.
            case (E_NONE)
                str = 'none'
            case (E_ERROR)
                str = 'error'
            case (E_DUMMY)
                str = 'dummy error'
            case (E_INVALID)
                str = 'invalid'
            case (E_INCOMPLETE)
                str = 'missing or incomplete'
            case (E_TYPE)
                str = 'type error'
            case (E_IO)
                str = 'I/O error'
            case (E_READ)
                str = 'read error'
            case (E_WRITE)
                str = 'write error'
            case (E_EOF)
                str = 'end of file'
            case (E_EOR)
                str = 'end of record'
            case (E_ALLOC)
                str = 'memory allocation error'
            case (E_BOUNDS)
                str = 'out of bounds'
            case (E_EXIST)
                str = 'resource exists'
            case (E_SYSTEM)
                str = 'system call failed'
            case (E_TIMEOUT)
                str = 'timeout'
            case (E_EMPTY)
                str = 'no data'
            case (E_LIMIT)
                str = 'limit reached'
            case (E_FORMAT)
                str = 'format error'
            case (E_NOT_FOUND)
                str = 'resource not found'
            case (E_READ_ONLY)
                str = 'read only'
            case (E_CONFIG)
                str = 'configuration error'
            case (E_HDF5)
                str = 'HDF5 error'

            ! Database.
            case (E_DB)
                str = 'database error'
            case (E_DB_ID)
                str = 'database application id invalid'
            case (E_DB_MEM)
                str = 'database memory error'
            case (E_DB_BUSY)
                str = 'database busy'
            case (E_DB_LOCKED)
                str = 'database locked'
            case (E_DB_EXEC)
                str = 'database execution failed'
            case (E_DB_CONSTRAINT)
                str = 'database contraint error'
            case (E_DB_TRANSACTION)
                str = 'database transaction failed'
            case (E_DB_ROLLBACK)
                str = 'database rollback failed'
            case (E_DB_PREPARE)
                str = 'database statement preparation failed'
            case (E_DB_FINALIZE)
                str = 'database statement finalization failed'
            case (E_DB_BIND)
                str = 'database bind failed'
            case (E_DB_TYPE)
                str = 'database type mismatch'
            case (E_DB_STEP)
                str = 'database execution step failed'
            case (E_DB_NO_ROWS)
                str = 'database returned no rows'
            case (E_DB_BACKUP)
                str = 'database backup error'

            ! Options.
            case (E_ARG)
                str = 'argument error'
            case (E_ARG_NOT_FOUND)
                str = 'argument not found'
            case (E_ARG_INVALID)
                str = 'argument invalid or missing'
            case (E_ARG_NO_VALUE)
                str = 'argument value missing'
            case (E_ARG_TYPE)
                str = 'argument type invalid'
            case (E_ARG_LENGTH)
                str = 'argument length invalid'

            ! POSIX message queue.
            case (E_MQUEUE)
                str = 'message queue operation failed'
            case (E_MQUEUE_EMPTY)
                str = 'message empty'

            ! Regular expressions.
            case (E_REGEX)
                str = 'regular expression failed'
            case (E_REGEX_COMPILE)
                str = 'regular expression failed to compile'
            case (E_REGEX_EXCEEDED)
                str = 'regular expression pattern exceeds maximum matches'
            case (E_REGEX_NO_MATCH)
                str = 'regular expression pattern does not match'
            case (E_REGEX_NO_GROUP)
                str = 'regular expression group not found'

            ! Sensor.
            case (E_SENSOR)
                str = 'sensor error'

            ! RPC.
            case (E_RPC)
                str = 'RPC error'
            case (E_RPC_CONNECT)
                str = 'RPC connection error'
            case (E_RPC_SSL)
                str = 'RPC SSL error'
            case (E_RPC_API)
                str = 'RPC API error'
            case (E_RPC_AUTH)
                str = 'RPC unauthorized'
            case (E_RPC_CONFLICT)
                str = 'RPC conflict'
            case (E_RPC_SERVER)
                str = 'RPC server error'

            ! Mail.
            case (E_MAIL)
                str = 'SMTP error'

            ! MQTT.
            case (E_MQTT)
                str = 'MQTT error'

            ! Lua.
            case (E_LUA)
                str = 'Lua error'
            case (E_LUA_YIELD)
                str = 'Lua thread yields'
            case (E_LUA_RUNTIME)
                str = 'Lua runtime error'
            case (E_LUA_SYNTAX)
                str = 'Lua syntax error'
            case (E_LUA_MEM)
                str = 'Lua memory error'
            case (E_LUA_ERROR)
                str = 'Lua message handling error'
            case (E_LUA_FILE)
                str = 'Lua file I/O error'

            case default
                str = 'unknown'
        end select
    end function dm_error_message

    pure elemental logical function dm_error_valid(error) result(valid)
        !! Returns whether given code is (likely) valid.
        integer, intent(in) :: error !! Error code.

        valid = (error >= E_NONE .and. error <= E_LAST)
    end function dm_error_valid

    pure elemental logical function dm_is_error(error) result(is_error)
        !! Returns `.true.` if given code is an error.
        integer, intent(in) :: error !! Error code.

        is_error = (error /= E_NONE)
    end function dm_is_error

    pure elemental logical function dm_is_ok(error) result(is_ok)
        !! Returns `.true.` if given code is not an error.
        integer, intent(in) :: error !! Error code.

        is_ok = (error == E_NONE)
    end function dm_is_ok

    subroutine dm_error_out(error, message, verbose, extra, quit)
        !! Prints error description to `stderr`. If `verbose` is true, the
        !! routine outputs even if no error occured (`E_NONE`).
        !!
        !! If `extra` is `.true.`, the routine outputs the default error
        !! message for the given error instead of the code. If `quit` is
        !! `.true.`, the routine terminates with exit code `0` on error
        !! `E_NONE`, else with `1`.
        character(len=*), parameter :: FMT_ERROR = '("Error: ", a, " (E", i0.3, ")")'
        character(len=*), parameter :: FMT_EXTRA = '("Error: ", a, " (", a, ")")'

        integer,          intent(in)           :: error   !! Error code.
        character(len=*), intent(in), optional :: message !! Optional error message.
        logical,          intent(in), optional :: verbose !! If true, print message on `E_NONE`.
        logical,          intent(in), optional :: extra   !! If true, print default message additionally.
        logical,          intent(in), optional :: quit    !! If true, stop program.

        integer :: stat
        logical :: extra_, verbose_, quit_

        extra_   = .false.
        verbose_ = .false.
        quit_    = .false.

        if (present(extra))   extra_   = extra
        if (present(verbose)) verbose_ = verbose
        if (present(quit))    quit_    = quit

        if (.not. verbose_ .and. error == E_NONE) return

        stat = 0
        if (error /= E_NONE) stat = 1

        if (present(message)) then
            if (extra_) then
                write (stderr, FMT_EXTRA) dm_ascii_escape(message), dm_error_message(error)
            else
                write (stderr, FMT_ERROR) dm_ascii_escape(message), error
            end if

            if (.not. quit_) return
            call dm_stop(stat)
        end if

        write (stderr, FMT_ERROR) dm_error_message(error), error
        if (quit_) call dm_stop(stat)
    end subroutine dm_error_out

    subroutine dm_stop(stat)
        !! Stops program execution and returns optional status.
        use :: unix, only: c_exit
        integer, intent(in), optional :: stat !! Exit status.

        if (present(stat)) then
            call c_exit(stat)
        else
            call c_exit(0)
        end if
    end subroutine dm_stop
end module dm_error
