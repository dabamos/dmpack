! Author:  Philipp Engel
! Licence: ISC
module dm_db
    !! SQLite core API access.
    use, intrinsic :: iso_c_binding
    use :: sqlite3
    use :: dm_db_query
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! SQLite 3 transactions.
    integer, parameter, public :: DB_TRANS_DEFERRED  = 0 !! Deferred transaction (default).
    integer, parameter, public :: DB_TRANS_IMMEDIATE = 1 !! Start a new write immediately (may fail with `E_DB_BUSY`).
    integer, parameter, public :: DB_TRANS_EXCLUSIVE = 2 !! No reading while transactions are underway.

    ! Private parameters.
    character(len=*), parameter :: DB_ATTACHED_NAME = 'attached' !! Default attached database name.

    type, public :: db_type
        !! SQLite database connectivity type.
        type(c_ptr) :: ctx       = c_null_ptr !! C pointer to SQLite 3 database.
        logical     :: read_only = .false.    !! Read-only flag.
    end type db_type

    type, public :: db_stmt_type
        !! SQLite database statement type.
        type(c_ptr) :: ctx = c_null_ptr !! C pointer to SQLite 3 statement.
    end type db_stmt_type

    abstract interface
        function dm_db_busy_callback(client_data, n) bind(c)
            !! C-interoperable callback function that is invoked on error
            !! `SQL_BUSY`. May return 0 to signal that no more invocations are
            !! desired.
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: client_data         !! Client data.
            integer(kind=c_int), intent(in), value :: n                   !! Number of times the busy callback has been invoked previously.
            integer(kind=c_int)                    :: dm_db_busy_callback !! Returns value.
        end function dm_db_busy_callback

        subroutine dm_db_backup_callback(remaining, page_count)
            !! Callback routine that is invoked if passed to `dm_db_backup()`.
            implicit none
            integer, intent(in) :: remaining  !! Remaining pages.
            integer, intent(in) :: page_count !! Total number of pages.
        end subroutine dm_db_backup_callback

        subroutine dm_db_log_callback(client_data, err_code, err_msg_ptr) bind(c)
            !! C-interoperable callback routine that is invoked for each created SQLite log.
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: client_data !! Client data.
            integer(kind=c_int), intent(in), value :: err_code    !! SQLite error code.
            type(c_ptr),         intent(in), value :: err_msg_ptr !! SQLite error message.
        end subroutine dm_db_log_callback

        subroutine dm_db_update_callback(client_data, type, db_name, table_name, row_id) bind(c)
            !! C-interoperable callback routine that is invoked on database updates.
            import :: c_int, c_int64_t, c_ptr
            implicit none
            type(c_ptr),             intent(in), value :: client_data !! Client data.
            integer(kind=c_int),     intent(in), value :: type        !! Database operation.
            type(c_ptr),             intent(in), value :: db_name     !! Database name.
            type(c_ptr),             intent(in), value :: table_name  !! Table name.
            integer(kind=c_int64_t), intent(in), value :: row_id      !! Row id.
        end subroutine dm_db_update_callback
    end interface

    interface dm_db_bind
        !! Generic bind function.
        module procedure :: db_bind_double
        module procedure :: db_bind_int
        module procedure :: db_bind_int64
        module procedure :: db_bind_query
        module procedure :: db_bind_text
    end interface dm_db_bind

    interface dm_db_changes
        !! Generic routine to return number of rows changed.
        module procedure :: db_changes_int32
        module procedure :: db_changes_int64
    end interface dm_db_changes

    interface dm_db_column
        !! Generic column function.
        module procedure :: db_column_allocatable
        module procedure :: db_column_double
        module procedure :: db_column_int
        module procedure :: db_column_int64
        module procedure :: db_column_text
    end interface dm_db_column

    ! Public abstract interfaces.
    public :: dm_db_backup_callback
    public :: dm_db_busy_callback
    public :: dm_db_log_callback
    public :: dm_db_update_callback

    ! Public procedures.
    public :: dm_db_attach
    public :: dm_db_begin
    public :: dm_db_bind
    public :: dm_db_changes
    public :: dm_db_column
    public :: dm_db_column_is_float
    public :: dm_db_column_is_integer
    public :: dm_db_column_is_text
    public :: dm_db_column_size
    public :: dm_db_commit
    public :: dm_db_detach
    public :: dm_db_error
    public :: dm_db_error_message
    public :: dm_db_exec
    public :: dm_db_finalize
    public :: dm_db_init
    public :: dm_db_is_connected
    public :: dm_db_is_read_only
    public :: dm_db_is_threadsafe
    public :: dm_db_log
    public :: dm_db_prepare
    public :: dm_db_release
    public :: dm_db_reset
    public :: dm_db_rollback
    public :: dm_db_save_point
    public :: dm_db_set_busy_callback
    public :: dm_db_set_busy_timeout
    public :: dm_db_set_log_callback
    public :: dm_db_set_update_callback
    public :: dm_db_shutdown
    public :: dm_db_sleep
    public :: dm_db_stmt_is_prepared
    public :: dm_db_step
    public :: dm_db_version

    ! Private procedures.
    private :: db_bind_double
    private :: db_bind_int
    private :: db_bind_int64
    private :: db_bind_query
    private :: db_bind_text
    private :: db_changes_int32
    private :: db_changes_int64
    private :: db_column_double
    private :: db_column_int
    private :: db_column_int64
    private :: db_column_text
    private :: db_column_allocatable
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_db_attach(db, path, name) result(rc)
        !! Attaches the database at `path` to the current connection. If no name
        !! is passed for the attached database, the name will be set to
        !! `attached`. The function trims the given path and name strings.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_ATTACH` if database attach query failed.
        !! * `E_NOT_FOUND` if database at `path` does not exist.
        !!
        use :: dm_file, only: dm_file_exists

        character(len=*), parameter :: QUERY = 'ATTACH DATABASE '

        type(db_type),    intent(inout)        :: db   !! Database type.
        character(len=*), intent(in)           :: path !! Path of database to attach.
        character(len=*), intent(in), optional :: name !! Name of attached database.

        integer :: stat

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) return

        rc = E_DB_ATTACH
        stat = dm_db_exec(db, QUERY // dm_btoa(present(name), name, DB_ATTACHED_NAME))
        if (dm_is_error(stat)) return

        rc = E_NONE
    end function dm_db_attach

    integer function dm_db_begin(db, mode) result(rc)
        !! Starts a transactions in IMMEDIATE mode. Mode shall be either
        !! `DB_TRANS_DEFERRED`, `DB_TRANS_IMMEDIATE`, or `DB_TRANS_EXLCUSIVE`.
        !! Default is `DB_TRANS_IMMEDIATE`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_TRANSACTION` if the transaction failed.
        !! * `E_INVALID` if the transaction mode is invalid.
        !!
        type(db_type), intent(inout)        :: db   !! Database type.
        integer,       intent(in), optional :: mode !! Transaction mode.

        integer :: mode_

        mode_ = dm_present(mode, DB_TRANS_IMMEDIATE)

        rc = E_INVALID
        select case (mode_)
            case (DB_TRANS_DEFERRED);  rc = dm_db_exec(db, 'BEGIN')
            case (DB_TRANS_IMMEDIATE); rc = dm_db_exec(db, 'BEGIN IMMEDIATE')
            case (DB_TRANS_EXCLUSIVE); rc = dm_db_exec(db, 'BEGIN EXCLUSIVE')
            case default;              return
        end select

        if (dm_is_error(rc)) rc = E_DB_TRANSACTION
    end function dm_db_begin

    logical function dm_db_column_is_float(db_stmt, index) result(is)
        type(db_stmt_type), intent(inout) :: db_stmt
        integer,            intent(in)    :: index

        is = (sqlite3_column_type(db_stmt%ctx, index) == SQLITE_FLOAT)
    end function dm_db_column_is_float

    logical function dm_db_column_is_integer(db_stmt, index) result(is)
        type(db_stmt_type), intent(inout) :: db_stmt
        integer,            intent(in)    :: index

        is = (sqlite3_column_type(db_stmt%ctx, index) == SQLITE_INTEGER)
    end function dm_db_column_is_integer

    logical function dm_db_column_is_text(db_stmt, index) result(is)
        type(db_stmt_type), intent(inout) :: db_stmt
        integer,            intent(in)    :: index

        is = (sqlite3_column_type(db_stmt%ctx, index) == SQLITE_TEXT)
    end function dm_db_column_is_text

    integer function dm_db_commit(db) result(rc)
        !! Commits a transaction. Returns `E_DB_EXEC` on error.
        type(db_type), intent(inout) :: db !! Database type.

        rc = dm_db_exec(db, 'COMMIT')
    end function dm_db_commit

    integer function dm_db_detach(db, name) result(rc)
        !! Detaches database from the current connection. If no name is passed
        !! for the attached database, the name is assumed to be `attached`. The
        !! function trims the given name string. Returns `E_DB_DETACH` on error.
        character(len=*), parameter :: QUERY = 'DETACH DATABASE '

        type(db_type),    intent(inout)        :: db   !! Database type.
        character(len=*), intent(in), optional :: name !! Name of attached database.

        integer :: stat

        rc = E_DB_DETACH
        stat = dm_db_exec(db, QUERY // dm_btoa(present(name), name, DB_ATTACHED_NAME))
        if (dm_is_error(stat)) return

        rc = E_NONE
    end function dm_db_detach

    integer function dm_db_error(db, sqlite_error) result(rc)
        !! Returns last database error as DMPACK error code, optionally the
        !! SQLite error code in argument `sqlite_error`. This function matches
        !! an SQLite error code to the corresponding DMPACK error code. For
        !! example, if the last SQLite error is `SQLITE_OK`, the function
        !! returns `E_NONE`.
        type(db_type), intent(inout)         :: db           !! Database type.
        integer,       intent(out), optional :: sqlite_error !! SQLite error code.

        integer :: error

        error = sqlite3_errcode(db%ctx)
        if (present(sqlite_error)) sqlite_error = error

        select case (error)
            case (SQLITE_OK);         rc = E_NONE
            case (SQLITE_DONE);       rc = E_DB_DONE
            case (SQLITE_ROW);        rc = E_DB_ROW
            case (SQLITE_BUSY);       rc = E_DB_BUSY
            case (SQLITE_LOCKED);     rc = E_DB_LOCKED
            case (SQLITE_NOMEM);      rc = E_MEMORY
            case (SQLITE_READONLY);   rc = E_READ_ONLY
            case (SQLITE_IOERR);      rc = E_IO
            case (SQLITE_CORRUPT);    rc = E_CORRUPT
            case (SQLITE_FULL);       rc = E_FULL
            case (SQLITE_EMPTY);      rc = E_EMPTY
            case (SQLITE_TOOBIG);     rc = E_LIMIT
            case (SQLITE_CONSTRAINT); rc = E_DB_CONSTRAINT
            case (SQLITE_MISMATCH);   rc = E_DB_TYPE
            case (SQLITE_FORMAT);     rc = E_FORMAT
            case (SQLITE_RANGE);      rc = E_DB_BIND
            case default;             rc = E_DB
        end select
    end function dm_db_error

    function dm_db_error_message(db) result(message)
        !! Returns last SQLite error message.
        type(db_type), intent(inout)  :: db      !! Database type.
        character(len=:), allocatable :: message !! Error message.

        message = sqlite3_errmsg(db%ctx)
    end function dm_db_error_message

    integer function dm_db_exec(db, query, error_message) result(rc)
        !! Executes given query, and returns optional error message if `rc` is
        !! not `E_NONE`. Otherwise, `err_msg` is not allocated. Returns
        !! `E_DB_EXEC` on error
        type(db_type),                 intent(inout)         :: db            !! Database type.
        character(len=*),              intent(in)            :: query         !! SQL query.
        character(len=:), allocatable, intent(out), optional :: error_message !! Optional error message.

        integer :: stat

        rc = E_DB_EXEC
        stat = sqlite3_exec(db%ctx, query, c_null_funptr, c_null_ptr, error_message)
        if (stat == SQLITE_OK) rc = E_NONE
    end function dm_db_exec

    integer function dm_db_finalize(db_stmt) result(rc)
        !! Finalises given database statement. Returns `E_DB_FINALIZE` on
        !! error.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.

        rc = E_NONE
        if (.not. c_associated(db_stmt%ctx)) return
        if (sqlite3_finalize(db_stmt%ctx) /= SQLITE_OK) rc = E_DB_FINALIZE
    end function dm_db_finalize

    integer function dm_db_init() result(rc)
        !! Initialises SQLite backend. Returns `E_DB` on error.
        rc = E_DB
        if (sqlite3_initialize() == SQLITE_OK) rc = E_NONE
    end function dm_db_init

    logical function dm_db_is_connected(db) result(is)
        !! Returns `.true.` if database type has associated pointer.
        type(db_type), intent(inout) :: db !! Database type.

        is = c_associated(db%ctx)
    end function dm_db_is_connected

    logical function dm_db_is_read_only(db) result(is)
        !! Returns `.true.` if database is in read-only mode. This function
        !! checks only the opaque database type for the read-only flag. It is
        !! still possible to enable ready-only access by calling
        !! `dm_db_set_query_only()`. The function `dm_db_get_query_only()`
        !! returns the status of the `query_only` pragma.
        type(db_type), intent(inout) :: db !! Database type.

        is = db%read_only
    end function dm_db_is_read_only

    logical function dm_db_is_threadsafe() result(is)
        !! Returns true if SQLite 3 was compiled threadsafe.
        is = (sqlite3_threadsafe() == SQLITE_OK)
    end function dm_db_is_threadsafe

    integer function dm_db_prepare(db, db_stmt, sql) result(rc)
        !! Prepares database statement. Returns `E_DB_PREPARE` on error.
        type(db_type),      intent(inout) :: db      !! Database type.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        character(len=*),   intent(in)    :: sql     !! SQL query.

        rc = E_DB_PREPARE
        if (sqlite3_prepare_v2(db%ctx, sql, db_stmt%ctx) == SQLITE_OK) rc = E_NONE
    end function dm_db_prepare

    integer function dm_db_release(db, name) result(rc)
        !! Jumps back to a save point. Returns `E_DB_EXEC` on error.
        type(db_type),    intent(inout) :: db   !! Database type.
        character(len=*), intent(in)    :: name !! Save point name.

        rc = dm_db_exec(db, 'RELEASE "' // trim(name) // '"')
    end function dm_db_release

    integer function dm_db_reset(db_stmt) result(rc)
        !! Resets database statement. The function returns `E_DB` on error.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.

        rc = E_DB
        if (sqlite3_reset(db_stmt%ctx) == SQLITE_OK) rc = E_NONE
    end function dm_db_reset

    integer function dm_db_rollback(db, name) result(rc)
        !! Rolls a transaction back, optionally to save point `name`. The
        !! function returns `E_DB_ROLLBACK` is the rollback failed.
        type(db_type),    intent(inout)        :: db   !! Database type.
        character(len=*), intent(in), optional :: name !! Save point name.

        if (present(name)) then
            rc = dm_db_exec(db, 'ROLLBACK TO "' // trim(name) // '"')
        else
            rc = dm_db_exec(db, 'ROLLBACK')
        end if

        if (dm_is_error(rc)) rc = E_DB_ROLLBACK
    end function dm_db_rollback

    integer function dm_db_save_point(db, name) result(rc)
        !! Creates a save point `name`. Returns `E_DB_EXEC` on error.
        type(db_type),    intent(inout) :: db   !! Database type.
        character(len=*), intent(in)    :: name !! Save point name.

        rc = dm_db_exec(db, 'SAVEPOINT "' // trim(name) // '"')
    end function dm_db_save_point

    integer function dm_db_set_busy_callback(db, callback, client_data) result(rc)
        !! Sets SQLite busy callback that is invoked whenever the database is
        !! busy.
        !!
        !! The dummy argument `client_data` is passed to the callback function.
        !! The callback may return 0 to signal that no more invocations are
        !! desired.
        !!
        !! The function returns `E_DB` on error.
        type(db_type), intent(inout)   :: db          !! Database type.
        procedure(dm_db_busy_callback) :: callback    !! Callback function.
        type(c_ptr),   intent(in)      :: client_data !! C pointer to client data.

        rc = E_DB
        if (sqlite3_busy_handler(db%ctx, c_funloc(callback), client_data) == SQLITE_OK) rc = E_NONE
    end function dm_db_set_busy_callback

    integer function dm_db_set_busy_timeout(db, msec) result(rc)
        !! Sets SQLite busy timeout in msec. Returns `E_DB` on error.
        type(db_type), intent(inout) :: db   !! Database type.
        integer,       intent(in)    :: msec !! Timeout in mseconds.

        rc = E_DB
        if (sqlite3_busy_timeout(db%ctx, msec) == SQLITE_OK) rc = E_NONE
    end function dm_db_set_busy_timeout

    integer function dm_db_set_log_callback(callback, client_data) result(rc)
        !! Sets SQLite error log callback. The dummy argument `client_data` is
        !! passed to the callback routine. The function returns `E_DB` on error.
        procedure(dm_db_log_callback)     :: callback    !! Callback routine.
        type(c_ptr), intent(in), optional :: client_data !! C pointer to client data.

        rc = E_DB

        if (present(client_data)) then
            if (sqlite3_config(SQLITE_CONFIG_LOG, c_funloc(callback), client_data) == SQLITE_OK) rc = E_NONE
        else
            if (sqlite3_config(SQLITE_CONFIG_LOG, c_funloc(callback), c_null_ptr) == SQLITE_OK) rc = E_NONE
        end if
    end function dm_db_set_log_callback

    integer function dm_db_set_update_callback(db, callback, client_data) result(rc)
        !! Sets SQLite error log callback. The dummy argument `client_data` is
        !! passed to the callback routine. The function returns `E_DB` on error.
        type(db_type), intent(inout)        :: db          !! Database type.
        procedure(dm_db_update_callback)    :: callback    !! Callback routine.
        type(c_ptr),   intent(in), optional :: client_data !! C pointer to client data.

        type(c_ptr) :: udp

        rc = E_DB

        if (present(client_data)) then
            udp = sqlite3_update_hook(db%ctx, c_funloc(callback), client_data)
        else
            udp = sqlite3_update_hook(db%ctx, c_funloc(callback), c_null_ptr)
        end if

        if (c_associated(udp)) rc = E_NONE
    end function dm_db_set_update_callback

    integer function dm_db_shutdown() result(rc)
        !! Finalises SQLite handle. Returns `E_DB` on error.
        rc = E_DB
        if (sqlite3_shutdown() == SQLITE_OK) rc = E_NONE
    end function dm_db_shutdown

    logical function dm_db_stmt_is_prepared(db_stmt) result(prepared)
        !! Returns `.true.` if given statement has been prepared.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.

        prepared = c_associated(db_stmt%ctx)
    end function dm_db_stmt_is_prepared

    integer function dm_db_step(db_stmt) result(rc)
        !! Steps rows. Returns `E_DB_STEP` on error.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.

        integer :: stat

        stat = sqlite3_step(db_stmt%ctx)

        select case (stat)
            case (SQLITE_ROW);  rc = E_DB_ROW
            case (SQLITE_DONE); rc = E_DB_DONE
            case (SQLITE_OK);   rc = E_NONE
            case default;       rc = E_DB_STEP
        end select
    end function dm_db_step

    function dm_db_version(name) result(version)
        !! Returns SQLite 3 library version as allocatable string.
        logical, intent(in), optional :: name    !! Add prefix `libsqlite/'.
        character(len=:), allocatable :: version !! Version string.

        if (dm_present(name, .false.)) then
            version = 'libsqlite3/' // sqlite3_libversion()
        else
            version = sqlite3_libversion()
        end if
    end function dm_db_version

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_db_column_size(db_stmt, index, value)
        !! Returns byte size of column value of given index.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Column index.
        integer,            intent(out)   :: value   !! Value.

        value = sqlite3_column_bytes(db_stmt%ctx, index)
    end subroutine dm_db_column_size

    subroutine dm_db_log(err_code, err_msg)
        !! Sends log message to SQLite error log handler. The callback has to
        !! be set through `dm_db_set_log_callback()` initially.
        use :: dm_c, only: dm_f_c_string

        integer,          intent(in) :: err_code !! Error code.
        character(len=*), intent(in) :: err_msg  !! Error message.

        call sqlite3_log(err_code, dm_f_c_string(err_msg))
    end subroutine dm_db_log

    subroutine dm_db_sleep(msec)
        !! Delays execution for given duration in msec.
        integer, intent(in) :: msec !! Time in [msec].

        integer :: n

        n = sqlite3_sleep(msec)
    end subroutine dm_db_sleep

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function db_bind_double(db_stmt, index, value) result(rc)
        !! Binds 64-bit real value to statement. Returns `E_DB_BIND` on error.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Value index.
        real(kind=r8),      intent(in)    :: value   !! Value.

        integer :: stat

        rc = E_DB_BIND
        stat = sqlite3_bind_double(db_stmt%ctx, index, value)
        if (stat == SQLITE_OK) rc = E_NONE
    end function db_bind_double

    integer function db_bind_int(db_stmt, index, value) result(rc)
        !! Binds 32-bit integer value to statement. Returns `E_DB_BIND` on
        !! error.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Value index.
        integer(kind=i4),   intent(in)    :: value   !! Value.

        integer :: stat

        rc = E_DB_BIND
        stat = sqlite3_bind_int(db_stmt%ctx, index, value)
        if (stat == SQLITE_OK) rc = E_NONE
    end function db_bind_int

    integer function db_bind_int64(db_stmt, index, value) result(rc)
        !! Binds 64-bit integer value to statement. Returns `E_DB_BIND` on
        !! error.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Value index.
        integer(kind=i8),   intent(in)    :: value   !! Value.

        integer :: stat

        rc = E_DB_BIND
        stat = sqlite3_bind_int64(db_stmt%ctx, index, value)
        if (stat == SQLITE_OK) rc = E_NONE
    end function db_bind_int64

    integer function db_bind_query(db_stmt, db_query) result(rc)
        !! Binds query parameters to SQLite statement. Returns `E_DB_BIND` on
        !! binding error.
        type(db_stmt_type),  intent(inout) :: db_stmt  !! Database statement type.
        type(db_query_type), intent(inout) :: db_query !! Database query type.

        integer :: i, j, stat

        rc = E_DB_BIND
        j  = 1

        ! UPDATE values.
        do i = 1, db_query%nupdates
            select case (db_query%updates(i)%type)
                case (DB_QUERY_TYPE_DOUBLE); stat = sqlite3_bind_double(db_stmt%ctx, j, db_query%updates(i)%value_double)
                case (DB_QUERY_TYPE_INT);    stat = sqlite3_bind_int   (db_stmt%ctx, j, db_query%updates(i)%value_int)
                case (DB_QUERY_TYPE_INT64);  stat = sqlite3_bind_int64 (db_stmt%ctx, j, db_query%updates(i)%value_int64)
                case (DB_QUERY_TYPE_TEXT);   stat = sqlite3_bind_text  (db_stmt%ctx, j, db_query%updates(i)%value_text)
                case default;                stat = SQLITE_ERROR
            end select

            if (stat /= SQLITE_OK) return
            j = j + 1
        end do

        ! WHERE values.
        do i = 1, db_query%nparams
            select case (db_query%params(i)%type)
                case (DB_QUERY_TYPE_DOUBLE); stat = sqlite3_bind_double(db_stmt%ctx, j, db_query%params(i)%value_double)
                case (DB_QUERY_TYPE_INT);    stat = sqlite3_bind_int   (db_stmt%ctx, j, db_query%params(i)%value_int)
                case (DB_QUERY_TYPE_INT64);  stat = sqlite3_bind_int64 (db_stmt%ctx, j, db_query%params(i)%value_int64)
                case (DB_QUERY_TYPE_TEXT);   stat = sqlite3_bind_text  (db_stmt%ctx, j, db_query%params(i)%value_text)
                case default;                stat = SQLITE_ERROR
            end select

            if (stat /= SQLITE_OK) return
            j = j + 1
        end do

        ! LIMIT value.
        if (db_query%limit > 0) then
            stat = sqlite3_bind_int64(db_stmt%ctx, j, db_query%limit)
            if (stat /= SQLITE_OK) return
        end if

        rc = E_NONE
    end function db_bind_query

    integer function db_bind_text(db_stmt, index, value) result(rc)
        !! Binds string value to statement. The value will be trimmed before
        !! binding. Returns `E_DB_BIND` on error.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Value index.
        character(len=*),   intent(in)    :: value   !! Value.

        integer :: stat

        rc = E_DB_BIND
        stat = sqlite3_bind_text(db_stmt%ctx, index, trim(value))
        if (stat == SQLITE_OK) rc = E_NONE
    end function db_bind_text

    ! **************************************************************************
    ! PRIVATE SUBROUTINES.
    ! **************************************************************************
    subroutine db_column_allocatable(db_stmt, index, value)
        !! Returns string value from column of given index.
        type(db_stmt_type),            intent(inout) :: db_stmt !! Database statement type.
        integer,                       intent(in)    :: index   !! Column index.
        character(len=:), allocatable, intent(out)   :: value   !! Value.

        value = sqlite3_column_text(db_stmt%ctx, index)
    end subroutine db_column_allocatable

    subroutine db_changes_int32(db, n)
        !! The function returns the number of rows modified, inserted or deleted
        !! by the most recently completed INSERT, UPDATE or DELETE statement on
        !! the database connection. Auxiliary changes caused by triggers,
        !! foreign key actions or REPLACE constraint resolution are not counted.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i4), intent(out)   :: n  !! Number of changes.

        n = sqlite3_changes(db%ctx)
    end subroutine db_changes_int32

    subroutine db_changes_int64(db, n)
        !! The function returns the number of rows modified, inserted or deleted
        !! by the most recently completed INSERT, UPDATE or DELETE statement on
        !! the database connection. Auxiliary changes caused by triggers,
        !! foreign key actions or REPLACE constraint resolution are not counted.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of changes.

        n = sqlite3_changes64(db%ctx)
    end subroutine db_changes_int64

    subroutine db_column_double(db_stmt, index, value)
        !! Returns double value from column of given index.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Column index.
        real(kind=r8),      intent(out)   :: value   !! Value.

        value = sqlite3_column_double(db_stmt%ctx, index)
    end subroutine db_column_double

    subroutine db_column_int(db_stmt, index, value)
        !! Returns 32-bit integer value from column of given index.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Column index.
        integer(kind=i4),   intent(out)   :: value   !! Value.

        value = sqlite3_column_int(db_stmt%ctx, index)
    end subroutine db_column_int

    subroutine db_column_int64(db_stmt, index, value)
        !! Returns 64-bit integer value from column of given index.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Column index.
        integer(kind=i8),   intent(out)   :: value   !! Value.

        value = sqlite3_column_int64(db_stmt%ctx, index)
    end subroutine db_column_int64

    subroutine db_column_text(db_stmt, index, value, n)
        !! Returns string value from column of given index.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Column index.
        character(len=*),   intent(inout) :: value   !! Value.
        integer,            intent(out)   :: n       !! Actual string length.

        value = sqlite3_column_text (db_stmt%ctx, index)
        n     = sqlite3_column_bytes(db_stmt%ctx, index)
    end subroutine db_column_text
end module dm_db
