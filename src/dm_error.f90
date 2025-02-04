! Author:  Philipp Engel
! Licence: ISC
module dm_error
    !! Error codes, error messages, and utility routines.
    use :: dm_ascii
    use :: dm_kind
    use :: dm_string
    implicit none (type, external)
    private

    ! ********************************************************************************
    !                                ATTENTION
    ! Any additional error code must be exported in `dm_lua_api_register()`
    ! of module `dm_lua_api`.
    ! ********************************************************************************

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
    integer, parameter, public :: E_NOT_FOUND      =  14 !! Resource not found.
    integer, parameter, public :: E_SYSTEM         =  15 !! System call failed.
    integer, parameter, public :: E_MEMORY         =  16 !! No memory.
    integer, parameter, public :: E_FULL           =  17 !! Disk full.
    integer, parameter, public :: E_EMPTY          =  18 !! No data.
    integer, parameter, public :: E_NULL           =  19 !! Pointer not associated.
    integer, parameter, public :: E_LIMIT          =  20 !! Memory limit reached.
    integer, parameter, public :: E_TIMEOUT        =  21 !! Timeout occured.
    integer, parameter, public :: E_FORMAT         =  22 !! Format error.
    integer, parameter, public :: E_PERM           =  23 !! No permission.
    integer, parameter, public :: E_READ_ONLY      =  24 !! Read-only access.
    integer, parameter, public :: E_CORRUPT        =  25 !! Data corrupted.
    integer, parameter, public :: E_CONFIG         =  26 !! Invalid configuration error.
    integer, parameter, public :: E_GEOCOM         =  28 !! GeoCOM error.
    ! Database errors.
    integer, parameter, public :: E_DB             =  30 !! Generic database error.
    integer, parameter, public :: E_DB_ID          =  31 !! Invalid database application id.
    integer, parameter, public :: E_DB_BUSY        =  32 !! Database is busy.
    integer, parameter, public :: E_DB_LOCKED      =  33 !! Database is locked.
    integer, parameter, public :: E_DB_EXEC        =  34 !! Execution failed.
    integer, parameter, public :: E_DB_CONSTRAINT  =  35 !! Contraint error.
    integer, parameter, public :: E_DB_TRANSACTION =  36 !! Transaction failed.
    integer, parameter, public :: E_DB_ROLLBACK    =  37 !! Transaction rollback error.
    integer, parameter, public :: E_DB_PREPARE     =  38 !! Prepare failed.
    integer, parameter, public :: E_DB_ROW         =  39 !! Statement row (not an error).
    integer, parameter, public :: E_DB_DONE        =  40 !! Statement done (not an error).
    integer, parameter, public :: E_DB_FINALIZE    =  41 !! Statement error.
    integer, parameter, public :: E_DB_BIND        =  42 !! Bind failed.
    integer, parameter, public :: E_DB_TYPE        =  43 !! Type mismatch.
    integer, parameter, public :: E_DB_STEP        =  44 !! Step failed.
    integer, parameter, public :: E_DB_NO_ROWS     =  45 !! No rows returned.
    integer, parameter, public :: E_DB_BACKUP      =  46 !! Backup error.
    integer, parameter, public :: E_DB_ATTACH      =  47 !! Attach failed.
    integer, parameter, public :: E_DB_DETACH      =  48 !! Detach error.
    integer, parameter, public :: E_DB_VERSION     =  49 !! Incompatible version.
    ! Command-line argument errors.
    integer, parameter, public :: E_ARG            =  50 !! Generic argument error.
    integer, parameter, public :: E_ARG_NOT_FOUND  =  51 !! Option not passed.
    integer, parameter, public :: E_ARG_INVALID    =  52 !! Invalid option or argument missing.
    integer, parameter, public :: E_ARG_NO_VALUE   =  53 !! Argument given but no value.
    integer, parameter, public :: E_ARG_TYPE       =  54 !! Type mismatch.
    integer, parameter, public :: E_ARG_LENGTH     =  55 !! Wrong value length.
    integer, parameter, public :: E_ARG_UNKNOWN    =  56 !! Unknown argument passed.
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
    ! Mail errors.
    integer, parameter, public :: E_MAIL           = 100 !! Generic mail error.
    integer, parameter, public :: E_MAIL_CONNECT   = 101 !! Mail connection error.
    integer, parameter, public :: E_MAIL_SSL       = 102 !! Mail SSL/TLS error.
    integer, parameter, public :: E_MAIL_AUTH      = 103 !! Unauthorised.
    ! MQTT errors.
    integer, parameter, public :: E_MQTT           = 110 !! Generic MQTT error.
    ! Lua errors.
    integer, parameter, public :: E_LUA            = 120 !! Generic Lua error.
    integer, parameter, public :: E_LUA_YIELD      = 121 !! Lua thread (coroutine) yields (not an error).
    integer, parameter, public :: E_LUA_RUNTIME    = 122 !! Lua runtime error.
    integer, parameter, public :: E_LUA_SYNTAX     = 123 !! Lua syntax error.
    integer, parameter, public :: E_LUA_MEM        = 124 !! Lua memory allocation error.
    integer, parameter, public :: E_LUA_ERROR      = 125 !! Lua message handling error.
    integer, parameter, public :: E_LUA_FILE       = 126 !! Lua file I/O error.
    ! Additional errors.
    integer, parameter, public :: E_LIB            = 130 !! Generic library error.
    integer, parameter, public :: E_MODBUS         = 131 !! Modbus error.
    integer, parameter, public :: E_HDF5           = 132 !! HDF5 error.
    integer, parameter, public :: E_ZLIB           = 133 !! zlib error.
    integer, parameter, public :: E_ZSTD           = 134 !! Zstandard error.
    integer, parameter, public :: E_XMPP           = 135 !! libstrophe error.
    integer, parameter, public :: E_LAST           = 135 !! Never use this.

    ! Exit status codes for `dm_stop(stat)`.
    integer, parameter, public :: STOP_SUCCESS = 0 !! Exit status 0.
    integer, parameter, public :: STOP_FAILURE = 1 !! Exit status 1.

    interface dm_perror
        !! Alias for `dm_error_out()`, do not use.
        module procedure :: dm_error_out
    end interface dm_perror

    public :: dm_error_is_valid
    public :: dm_error_message
    public :: dm_error_out
    public :: dm_is_error
    public :: dm_is_ok
    public :: dm_perror
    public :: dm_stop
contains
    pure elemental logical function dm_error_is_valid(error) result(valid)
        !! Returns whether given error code is (likely) valid.
        integer, intent(in) :: error !! Error code.

        valid = (error >= E_NONE .and. error <= E_LAST)
    end function dm_error_is_valid

    pure function dm_error_message(error) result(message)
        !! Returns error message of given error code `error`.
        integer, intent(in)           :: error   !! Error code.
        character(len=:), allocatable :: message !! Error message.

        select case (error)
            ! General errors.
            case (E_NONE);           message = 'none'
            case (E_ERROR);          message = 'error'
            case (E_DUMMY);          message = 'dummy error'
            case (E_INVALID);        message = 'invalid'
            case (E_INCOMPLETE);     message = 'incomplete'
            case (E_TYPE);           message = 'type error'
            case (E_IO);             message = 'I/O error'
            case (E_READ);           message = 'read error'
            case (E_WRITE);          message = 'write error'
            case (E_EOF);            message = 'end of file'
            case (E_EOR);            message = 'end of record'
            case (E_ALLOC);          message = 'memory allocation error'
            case (E_BOUNDS);         message = 'out of bounds'
            case (E_EXIST);          message = 'resource exists'
            case (E_NOT_FOUND);      message = 'resource not found'
            case (E_SYSTEM);         message = 'system call failed'
            case (E_MEMORY);         message = 'no memory'
            case (E_FULL);           message = 'disk full'
            case (E_EMPTY);          message = 'no data'
            case (E_NULL);           message = 'null pointer'
            case (E_LIMIT);          message = 'limit reached'
            case (E_TIMEOUT);        message = 'timeout'
            case (E_FORMAT);         message = 'format error'
            case (E_PERM);           message = 'no permission'
            case (E_READ_ONLY);      message = 'read only'
            case (E_CORRUPT);        message = 'data corrupted'
            case (E_CONFIG);         message = 'configuration error'
            case (E_GEOCOM);         message = 'GeoCOM error'
            ! Database.
            case (E_DB);             message = 'database error'
            case (E_DB_ID);          message = 'database application id invalid'
            case (E_DB_BUSY);        message = 'database busy'
            case (E_DB_LOCKED);      message = 'database locked'
            case (E_DB_EXEC);        message = 'database execution failed'
            case (E_DB_CONSTRAINT);  message = 'database contraint error'
            case (E_DB_TRANSACTION); message = 'database transaction failed'
            case (E_DB_ROLLBACK);    message = 'database rollback failed'
            case (E_DB_PREPARE);     message = 'database statement preparation failed'
            case (E_DB_ROW);         message = 'database statement row (not an error)'
            case (E_DB_DONE);        message = 'database statement done (not an error)'
            case (E_DB_FINALIZE);    message = 'database statement finalization failed'
            case (E_DB_BIND);        message = 'database bind failed'
            case (E_DB_TYPE);        message = 'database type mismatch'
            case (E_DB_STEP);        message = 'database execution step failed'
            case (E_DB_NO_ROWS);     message = 'database returned no rows'
            case (E_DB_BACKUP);      message = 'database backup error'
            case (E_DB_ATTACH);      message = 'database attach failed'
            case (E_DB_DETACH);      message = 'database detach failed'
            case (E_DB_VERSION);     message = 'database version incompatible'
            ! Options.
            case (E_ARG);            message = 'argument error'
            case (E_ARG_NOT_FOUND);  message = 'argument not found'
            case (E_ARG_INVALID);    message = 'argument invalid or missing'
            case (E_ARG_NO_VALUE);   message = 'argument value missing'
            case (E_ARG_TYPE);       message = 'argument type invalid'
            case (E_ARG_LENGTH);     message = 'argument length invalid'
            case (E_ARG_UNKNOWN);    message = 'argument is unknown'
            ! POSIX message queue.
            case (E_MQUEUE);         message = 'message queue operation failed'
            case (E_MQUEUE_EMPTY);   message = 'message empty'
            ! Regular expressions.
            case (E_REGEX);          message = 'regular expression failed'
            case (E_REGEX_COMPILE);  message = 'regular expression failed to compile'
            case (E_REGEX_EXCEEDED); message = 'regular expression pattern exceeds maximum matches'
            case (E_REGEX_NO_MATCH); message = 'regular expression pattern does not match'
            case (E_REGEX_NO_GROUP); message = 'regular expression group not found'
            ! Sensor.
            case (E_SENSOR);         message = 'sensor error'
            ! RPC.
            case (E_RPC);            message = 'RPC error'
            case (E_RPC_CONNECT);    message = 'RPC connection error'
            case (E_RPC_SSL);        message = 'RPC SSL error'
            case (E_RPC_API);        message = 'RPC API error'
            case (E_RPC_AUTH);       message = 'RPC authorization error'
            case (E_RPC_CONFLICT);   message = 'RPC conflict'
            case (E_RPC_SERVER);     message = 'RPC server error'
            ! Mail.
            case (E_MAIL);           message = 'mail error'
            case (E_MAIL_CONNECT);   message = 'mail connection error'
            case (E_MAIL_SSL);       message = 'mail SSL error'
            case (E_MAIL_AUTH);      message = 'mail authorization error'
            ! MQTT.
            case (E_MQTT);           message = 'MQTT error'
            ! Lua.
            case (E_LUA);            message = 'Lua error'
            case (E_LUA_YIELD);      message = 'Lua thread yields'
            case (E_LUA_RUNTIME);    message = 'Lua runtime error'
            case (E_LUA_SYNTAX);     message = 'Lua syntax error'
            case (E_LUA_MEM);        message = 'Lua memory error'
            case (E_LUA_ERROR);      message = 'Lua message handling error'
            case (E_LUA_FILE);       message = 'Lua file I/O error'
            ! Libraries.
            case (E_LIB);            message = 'library error'
            case (E_MODBUS);         message = 'Modbus error'
            case (E_HDF5);           message = 'HDF5 error'
            case (E_ZLIB);           message = 'zlib error'
            case (E_ZSTD);           message = 'zstd error'
            case (E_XMPP);           message = 'XMPP error'
            ! Unknown.
            case default;            message = 'unknown error'
        end select
    end function dm_error_message

    pure elemental logical function dm_is_error(error) result(is_error)
        !! Returns `.true.` if given code is an error (not `E_NONE`).
        integer, intent(in) :: error !! Error code.

        is_error = (error /= E_NONE .and. error /= E_DB_ROW .and. error /= E_DB_DONE)
    end function dm_is_error

    pure elemental logical function dm_is_ok(error) result(is_ok)
        !! Returns `.true.` if given code is not an error (`E_NONE`).
        integer, intent(in) :: error !! Error code.

        is_ok = (error == E_NONE .or. error == E_DB_ROW .or. error == E_DB_DONE)
    end function dm_is_ok

    subroutine dm_error_out(error, message, verbose, extra, fatal)
        !! Prints error description to `stderr`. If `verbose` is `.true.`, the
        !! routine outputs even if no error occured (`E_NONE`).
        !!
        !! If `extra` is `.true.`, the routine outputs the default error
        !! message for the given error code additionally to the message.
        !!
        !! If `fatal` is `.true.`, the routine terminates with exit code `1` on
        !! error.
        character(len=*), parameter :: FMT_ERROR = '("Error ", i0.3, ": ", a)'
        character(len=*), parameter :: FMT_EXTRA = '("Error ", i0.3, ": ", a, " (", a, ")")'

        integer,          intent(in)           :: error   !! DMPACK error code.
        character(len=*), intent(in), optional :: message !! Optional error message.
        logical,          intent(in), optional :: verbose !! If `.true.`, print message on `E_NONE` too.
        logical,          intent(in), optional :: extra   !! If `.true.`, print additional error code message.
        logical,          intent(in), optional :: fatal   !! If `.true.`, stop program on error.

        logical :: extra_, fatal_, verbose_

        verbose_ = .false.
        extra_   = .false.
        fatal_   = .false.

        if (present(verbose)) verbose_ = verbose
        if (present(extra))   extra_   = extra
        if (present(fatal))   fatal_   = fatal

        if (error == E_NONE .and. .not. verbose_) return

        if (present(message)) then
            if (extra_) then
                write (stderr, FMT_EXTRA) error, dm_ascii_escape(message), dm_error_message(error)
            else
                write (stderr, FMT_ERROR) error, dm_ascii_escape(message)
            end if
        else
            write (stderr, FMT_ERROR) error, dm_error_message(error)
        end if

        if (error == E_NONE) return
        if (fatal_) call dm_stop(STOP_FAILURE)
    end subroutine dm_error_out

    subroutine dm_stop(stat)
        !! Stops program execution with optional exit status `stat`. The exit
        !! status may be `STOP_SUCCESS` or `STOP_FAILURE`.
        use :: unix, only: c_exit

        integer, intent(in), optional :: stat !! Exit status.

        if (present(stat)) then
            call c_exit(stat)
        else
            call c_exit(STOP_SUCCESS)
        end if
    end subroutine dm_stop
end module dm_error
