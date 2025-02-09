! Author:  Philipp Engel
! Licence: ISC
module dm_db
    !! Database abstraction layer over SQLite 3. The SQL statements are stored in
    !! module `dm_sql`.
    !!
    !! Load the last 10 observations into allocatable array `observs`:
    !!
    !! ```fortran
    !! integer       :: rc
    !! type(db_type) :: db
    !! type(observ_type), allocatable :: observs(:)
    !!
    !! rc = dm_db_open(db, '/var/dmpack/observ.sqlite')
    !! rc = dm_db_select_observs(db, observs, desc=.true., limit=10)
    !! rc = dm_db_close(db)
    !! ```
    !!
    !! Using the iterator interface instead:
    !!
    !! ```fortran
    !! integer            :: rc      ! Return code.
    !! type(db_type)      :: db      ! Database handle.
    !! type(db_stmt_type) :: db_stmt ! Database statement.
    !! type(observ_type)  :: observ  ! Returned observation.
    !!
    !! rc = dm_db_open(db, '/var/dmpack/observ.sqlite')
    !!
    !! do while (dm_is_ok(rc))
    !!     rc = dm_db_select_observs(db, db_stmt, observ, desc=.true., limit=10)
    !!     if (rc == E_DB_DONE) exit
    !!     if (dm_is_ok(rc)) print '(a)', trim(observ%name)
    !! end do
    !!
    !! rc = dm_db_finalize(db_stmt)
    !! rc = dm_db_close(db)
    !! ```
    !!
    !! The database functions return `E_NONE` if the respective operation was
    !! successful.
    use, intrinsic :: iso_c_binding
    use :: sqlite3
    use :: dm_db_query
    use :: dm_db_stmt
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_sql
    use :: dm_string
    use :: dm_time
    use :: dm_uuid
    use :: dm_util
    implicit none (type, external)
    private

    ! SQLite 3 journal modes.
    integer, parameter, public :: DB_JOURNAL_OFF      = 0             !! No rollback journals.
    integer, parameter, public :: DB_JOURNAL_DELETE   = 1             !! Delete journal (default).
    integer, parameter, public :: DB_JOURNAL_TRUNCATE = 2             !! Delete journal by truncating (may be faster than `DB_JOURNAL_DELETE`).
    integer, parameter, public :: DB_JOURNAL_PERSIST  = 3             !! Overwrite journal instead of deleting (may be faster than deleting or truncating).
    integer, parameter, public :: DB_JOURNAL_MEMORY   = 4             !! Store journal in memory (fast, volatile).
    integer, parameter, public :: DB_JOURNAL_WAL      = 5             !! Use Write-Ahead Log (WAL) journal.

    ! SQLite 3 auto vacuum modes.
    integer, parameter, public :: DB_AUTO_VACUUM_NONE        = 0      !! No auto-vacuum (default).
    integer, parameter, public :: DB_AUTO_VACUUM_FULL        = 1      !! Enable auto-vacuum.
    integer, parameter, public :: DB_AUTO_VACUUM_INCREMENTAL = 2      !! Vacuum requires additional PRAGMA.

    ! SQLite 3 transactions.
    integer, parameter, public :: DB_TRANS_DEFERRED  = 0              !! Deferred transaction (default).
    integer, parameter, public :: DB_TRANS_IMMEDIATE = 1              !! Start a new write immediately (may fail with `E_DB_BUSY`).
    integer, parameter, public :: DB_TRANS_EXCLUSIVE = 2              !! No reading while transactions are underway.

    ! Additional parameters.
    integer, parameter, public :: DB_APPLICATION_ID  = int(z'444D31') !! Application id of DMPACK databases (`DM1` in ASCII).
    integer, parameter, public :: DB_SCHEMA_VERSION  = 2              !! Database schema version, increased on updates.
    integer, parameter, public :: DB_TIMEOUT_DEFAULT = 1000           !! Default SQLite 3 busy timeout in mseconds.

    ! Private parameters.
    character(len=*), parameter :: DB_ATTACHED_NAME = 'attached'
    integer,          parameter :: DB_MAX_QUERY_LEN = 4096

    type, public :: db_type
        !! Opaque SQLite database connectivity type.
        private
        type(c_ptr) :: ctx       = c_null_ptr !! C pointer to SQLite 3 database.
        logical     :: read_only = .false.    !! Read-only flag.
    end type db_type

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

    interface db_bind
        !! Private generic bind function.
        module procedure :: db_bind_double
        module procedure :: db_bind_int
        module procedure :: db_bind_int64
        module procedure :: db_bind_query
        module procedure :: db_bind_text
    end interface db_bind

    interface db_column
        !! Private generic column function.
        module procedure :: db_column_double
        module procedure :: db_column_int
        module procedure :: db_column_int64
        module procedure :: db_column_text
        module procedure :: db_column_text_alloc
    end interface db_column

    interface db_next_row
        !! Private generic table row access function.
        module procedure :: db_next_row_allocatable
        module procedure :: db_next_row_character
        module procedure :: db_next_row_beat
        module procedure :: db_next_row_data_point
        module procedure :: db_next_row_log
        module procedure :: db_next_row_node
        module procedure :: db_next_row_observ
        module procedure :: db_next_row_observ_view
        module procedure :: db_next_row_sensor
        module procedure :: db_next_row_string
        module procedure :: db_next_row_sync
        module procedure :: db_next_row_target
    end interface db_next_row

    interface dm_db_begin
        !! Starts a transaction. Public alias for `db_begin()`.
        !!
        !! Optional argument `mode` may be one of:
        !!
        !! * `DB_TRANS_DEFERRED`
        !! * `DB_TRANS_IMMEDIATE`
        !! * `DB_TRANS_EXCLUSIVE`
        !!
        !! The default mode is `DB_TRANS_DEFERRED`.
        module procedure :: db_begin
    end interface dm_db_begin

    interface dm_db_finalize
        !! Finalises given database statement. Public alias for
        !! `db_finalize()`. Returns `E_DB_FINALIZE` on error.
        module procedure :: db_finalize
    end interface dm_db_finalize

    interface dm_db_insert
        !! Generic database insert function.
        module procedure :: dm_db_insert_beat
        module procedure :: dm_db_insert_beats
        module procedure :: dm_db_insert_log
        module procedure :: dm_db_insert_node
        module procedure :: dm_db_insert_observ
        module procedure :: dm_db_insert_observs
        module procedure :: dm_db_insert_sensor
        module procedure :: dm_db_insert_target
    end interface dm_db_insert

    interface dm_db_select
        !! Generic database select function.
        module procedure :: dm_db_select_beat
        module procedure :: dm_db_select_log
        module procedure :: dm_db_select_node
        module procedure :: dm_db_select_observ
        module procedure :: dm_db_select_sensor
        module procedure :: dm_db_select_target
    end interface dm_db_select

    interface dm_db_select_beats
        !! Generic beats select function.
        module procedure :: db_select_beats_array
        module procedure :: db_select_beats_iter
    end interface dm_db_select_beats

    interface dm_db_select_data_points
        !! Generic data points select function.
        module procedure :: db_select_data_points_array
        module procedure :: db_select_data_points_iter
    end interface dm_db_select_data_points

    interface dm_db_select_json_beats
        !! Generic JSON logs select function.
        module procedure :: db_select_json_beats_array
        module procedure :: db_select_json_beats_iter
    end interface dm_db_select_json_beats

    interface dm_db_select_json_logs
        !! Generic JSON logs select function.
        module procedure :: db_select_json_logs_array
        module procedure :: db_select_json_logs_iter
    end interface dm_db_select_json_logs

    interface dm_db_select_json_nodes
        !! Generic JSON nodes select function.
        module procedure :: db_select_json_nodes_array
        module procedure :: db_select_json_nodes_iter
    end interface dm_db_select_json_nodes

    interface dm_db_select_logs
        !! Generic logs select function.
        module procedure :: db_select_logs_array
        module procedure :: db_select_logs_iter
    end interface dm_db_select_logs

    interface dm_db_select_nodes
        !! Generic nodes select function.
        module procedure :: db_select_nodes_array
        module procedure :: db_select_nodes_iter
    end interface dm_db_select_nodes

    interface dm_db_select_observs
        !! Generic observations select function.
        module procedure :: db_select_observs_array
        module procedure :: db_select_observs_iter
    end interface dm_db_select_observs

    interface dm_db_select_sensors
        !! Generic sensors select function.
        module procedure :: db_select_sensors_array
        module procedure :: db_select_sensors_iter
    end interface dm_db_select_sensors

    interface dm_db_select_targets
        !! Generic targets select function.
        module procedure :: db_select_targets_array
        module procedure :: db_select_targets_iter
    end interface dm_db_select_targets

    interface dm_db_update
        !! Generic database update function.
        module procedure :: dm_db_update_node
        module procedure :: dm_db_update_sensor
        module procedure :: dm_db_update_target
    end interface dm_db_update

    ! Public abstract interfaces.
    public :: dm_db_backup_callback
    public :: dm_db_busy_callback
    public :: dm_db_log_callback
    public :: dm_db_update_callback

    ! Public procedures.
    public :: dm_db_attach
    public :: dm_db_backup
    public :: dm_db_begin
    public :: dm_db_close
    public :: dm_db_commit
    public :: dm_db_count_beats
    public :: dm_db_count_logs
    public :: dm_db_count_nodes
    public :: dm_db_count_observs
    public :: dm_db_count_receivers
    public :: dm_db_count_requests
    public :: dm_db_count_responses
    public :: dm_db_count_sensors
    public :: dm_db_count_sync_logs
    public :: dm_db_count_sync_nodes
    public :: dm_db_count_sync_observs
    public :: dm_db_count_sync_sensors
    public :: dm_db_count_sync_targets
    public :: dm_db_count_targets
    public :: dm_db_create_beats
    public :: dm_db_create_logs
    public :: dm_db_create_observs
    public :: dm_db_delete_beat
    public :: dm_db_delete_log
    public :: dm_db_delete_node
    public :: dm_db_delete_observ
    public :: dm_db_delete_sensor
    public :: dm_db_delete_target
    public :: dm_db_detach
    public :: dm_db_error
    public :: dm_db_error_message
    public :: dm_db_finalize
    public :: dm_db_get_application_id
    public :: dm_db_get_data_version
    public :: dm_db_get_foreign_keys
    public :: dm_db_get_journal_mode
    public :: dm_db_get_query_only
    public :: dm_db_get_schema_version
    public :: dm_db_has_log
    public :: dm_db_has_node
    public :: dm_db_has_observ
    public :: dm_db_has_sensor
    public :: dm_db_has_table
    public :: dm_db_has_target
    public :: dm_db_init
    public :: dm_db_insert
    public :: dm_db_insert_beat
    public :: dm_db_insert_beats
    public :: dm_db_insert_log
    public :: dm_db_insert_node
    public :: dm_db_insert_observ
    public :: dm_db_insert_observs
    public :: dm_db_insert_sensor
    public :: dm_db_insert_sync
    public :: dm_db_insert_sync_log
    public :: dm_db_insert_sync_node
    public :: dm_db_insert_sync_observ
    public :: dm_db_insert_sync_sensor
    public :: dm_db_insert_sync_target
    public :: dm_db_insert_target
    public :: dm_db_is_connected
    public :: dm_db_is_read_only
    public :: dm_db_is_threadsafe
    public :: dm_db_log
    public :: dm_db_open
    public :: dm_db_optimize
    public :: dm_db_rollback
    public :: dm_db_select
    public :: dm_db_select_beat
    public :: dm_db_select_beats
    public :: dm_db_select_data_points
    public :: dm_db_select_json_beat
    public :: dm_db_select_json_beats
    public :: dm_db_select_json_log
    public :: dm_db_select_json_logs
    public :: dm_db_select_json_node
    public :: dm_db_select_json_nodes
    public :: dm_db_select_log
    public :: dm_db_select_logs
    public :: dm_db_select_node
    public :: dm_db_select_nodes
    public :: dm_db_select_observ
    public :: dm_db_select_observ_ids
    public :: dm_db_select_observ_views
    public :: dm_db_select_observs
    public :: dm_db_select_observs_by_id
    public :: dm_db_select_observs_by_time
    public :: dm_db_select_sensor
    public :: dm_db_select_sensors
    public :: dm_db_select_sync_log
    public :: dm_db_select_sync_logs
    public :: dm_db_select_sync_node
    public :: dm_db_select_sync_nodes
    public :: dm_db_select_sync_observ
    public :: dm_db_select_sync_observs
    public :: dm_db_select_sync_sensor
    public :: dm_db_select_sync_sensors
    public :: dm_db_select_sync_target
    public :: dm_db_select_sync_targets
    public :: dm_db_select_tables
    public :: dm_db_select_target
    public :: dm_db_select_targets
    public :: dm_db_set_application_id
    public :: dm_db_set_auto_vacuum
    public :: dm_db_set_busy_callback
    public :: dm_db_set_busy_timeout
    public :: dm_db_set_foreign_keys
    public :: dm_db_set_journal_mode
    public :: dm_db_set_log_callback
    public :: dm_db_set_query_only
    public :: dm_db_set_schema_version
    public :: dm_db_set_update_callback
    public :: dm_db_shutdown
    public :: dm_db_size
    public :: dm_db_sleep
    public :: dm_db_update
    public :: dm_db_update_node
    public :: dm_db_update_sensor
    public :: dm_db_update_target
    public :: dm_db_vacuum
    public :: dm_db_validate
    public :: dm_db_version

    ! Private procedures.
    private :: db_begin
    private :: db_bind
    private :: db_bind_double
    private :: db_bind_int
    private :: db_bind_int64
    private :: db_bind_query
    private :: db_bind_text
    private :: db_column
    private :: db_column_bytes
    private :: db_column_double
    private :: db_column_int
    private :: db_column_int64
    private :: db_column_is_float
    private :: db_column_is_integer
    private :: db_column_is_text
    private :: db_column_text
    private :: db_column_text_alloc
    private :: db_commit
    private :: db_count
    private :: db_delete_receivers ! obsolete
    private :: db_delete_requests  ! obsolete
    private :: db_delete_responses ! obsolete
    private :: db_exec
    private :: db_has
    private :: db_insert_receivers
    private :: db_insert_requests
    private :: db_insert_responses
    private :: db_insert_sync
    private :: db_next_row
    private :: db_next_row_allocatable
    private :: db_next_row_character
    private :: db_next_row_data_point
    private :: db_next_row_log
    private :: db_next_row_node
    private :: db_next_row_observ
    private :: db_next_row_observ_view
    private :: db_next_row_sensor
    private :: db_next_row_string
    private :: db_next_row_target
    private :: db_prepare
    private :: db_release
    private :: db_reset
    private :: db_rollback
    private :: db_save_point
    private :: db_select_beats_array
    private :: db_select_beats_iter
    private :: db_select_data_points_array
    private :: db_select_data_points_iter
    private :: db_select_json_beats_array
    private :: db_select_json_beats_iter
    private :: db_select_json_logs_array
    private :: db_select_json_logs_iter
    private :: db_select_json_nodes_array
    private :: db_select_json_nodes_iter
    private :: db_select_logs_array
    private :: db_select_logs_iter
    private :: db_select_nodes_array
    private :: db_select_nodes_iter
    private :: db_select_observs_array
    private :: db_select_observs_data
    private :: db_select_observs_iter
    private :: db_select_receivers
    private :: db_select_requests
    private :: db_select_responses
    private :: db_select_sensors_array
    private :: db_select_sensors_iter
    private :: db_select_sync
    private :: db_select_syncs
    private :: db_select_targets_array
    private :: db_select_targets_iter
    private :: db_step
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
        stat = db_exec(db, QUERY // dm_btoa(present(name), name, DB_ATTACHED_NAME))
        if (dm_is_error(stat)) return

        rc = E_NONE
    end function dm_db_attach

    integer function dm_db_backup(db, path, wal, callback, nsteps, sleep_time) result(rc)
        !! Creates online backup of given database. The functions assumes 500
        !! steps and a sleep time of 250 msec by default, if the arguments are
        !! not passed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB` if closing the database failed.
        !! * `E_DB_BACKUP` if SQLite backup failed.
        !! * `E_EXIST` if backup database exists.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_file

        integer, parameter :: NSTEPS_DEFAULT     = 500 !! Number of steps.
        integer, parameter :: SLEEP_TIME_DEFAULT = 250 !! Busy sleep time in [msec].

        type(db_type),    intent(inout)            :: db         !! Database type.
        character(len=*), intent(in)               :: path       !! File path of backup database to be created.
        logical,          intent(in),     optional :: wal        !! Enable WAL mode for backup.
        procedure(dm_db_backup_callback), optional :: callback   !! Progress callback routine.
        integer,          intent(in),     optional :: nsteps     !! Number of steps per iteration (default: 500).
        integer,          intent(in),     optional :: sleep_time !! Sleep time per iteration in msec (default: 250 msec).

        integer       :: stat
        integer       :: nsteps_, sleep_time_
        logical       :: wal_
        type(db_type) :: backup
        type(c_ptr)   :: ptr

        rc = E_READ_ONLY
        if (db%read_only) return

        wal_        = dm_present(wal, .false.)
        nsteps_     = dm_present(nsteps, NSTEPS_DEFAULT)
        sleep_time_ = dm_present(sleep_time, SLEEP_TIME_DEFAULT)

        rc = E_EXIST
        if (dm_file_exists(path)) return

        rc = dm_db_open(backup, path, create=.true., wal=wal_)
        if (dm_is_error(rc)) return

        sql_block: block
            rc = E_DB_BACKUP
            ptr = sqlite3_backup_init(backup%ctx, 'main', db%ctx, 'main')
            if (.not. c_associated(ptr)) exit sql_block

            do
                stat = sqlite3_backup_step(ptr, nsteps_)

                if (present(callback)) then
                    call callback(remaining  = sqlite3_backup_remaining(ptr), &
                                  page_count = sqlite3_backup_pagecount(ptr))
                end if

                if (stat == SQLITE_OK .or. stat == SQLITE_BUSY .or. stat == SQLITE_LOCKED) then
                    call dm_db_sleep(sleep_time_)
                    cycle
                end if

                exit
            end do

            if (sqlite3_backup_finish(ptr) /= SQLITE_OK) exit sql_block
            rc = E_NONE
        end block sql_block

        if (dm_is_error(dm_db_close(backup))) rc = E_DB
    end function dm_db_backup

    integer function dm_db_close(db, optimize) result(rc)
        !! Closes connection to SQLite database. Optimises the database if
        !! `optimize` is `.true.`. Returns `E_DB` on error.
        type(db_type), intent(inout)        :: db       !! Database type.
        logical,       intent(in), optional :: optimize !! Optimise on close.

        if (dm_present(optimize, .false.)) then
            rc = dm_db_optimize(db)
            if (dm_is_error(rc)) return
        end if

        rc = E_DB
        if (sqlite3_close(db%ctx) /= SQLITE_OK) return

        db%ctx = c_null_ptr
        rc = E_NONE
    end function dm_db_close

    integer function dm_db_commit(db) result(rc)
        !! Ends transaction and commits changes to database. On error, does a
        !! rollback automatically. Returns `E_DB_ROLLBACK` if the rollback
        !! failed.
        type(db_type), intent(inout) :: db !! Database type.

        rc = db_commit(db)
        if (dm_is_ok(rc)) return
        if (dm_is_error(db_rollback(db))) rc = E_DB_ROLLBACK
    end function dm_db_commit

    integer function dm_db_count_beats(db, n) result(rc)
        !! Returns number of rows in table `beats`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_BEATS, n)
    end function dm_db_count_beats

    integer function dm_db_count_logs(db, n) result(rc)
        !! Returns number of rows in table `logs`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_LOGS, n)
    end function dm_db_count_logs

    integer function dm_db_count_nodes(db, n) result(rc)
        !! Returns number of rows in table `nodes`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_NODES, n)
    end function dm_db_count_nodes

    integer function dm_db_count_observs(db, n) result(rc)
        !! Returns number of rows in table `observs`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_OBSERVS, n)
    end function dm_db_count_observs

    integer function dm_db_count_receivers(db, n) result(rc)
        !! Returns number of rows in table `receivers`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_RECEIVERS, n)
    end function dm_db_count_receivers

    integer function dm_db_count_requests(db, n) result(rc)
        !! Returns number of rows in table `requests`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_REQUESTS, n)
    end function dm_db_count_requests

    integer function dm_db_count_responses(db, n) result(rc)
        !! Returns number of rows in table `responses`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_RESPONSES, n)
    end function dm_db_count_responses

    integer function dm_db_count_sensors(db, n) result(rc)
        !! Returns number of rows in table `sensors`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SENSORS, n)
    end function dm_db_count_sensors

    integer function dm_db_count_targets(db, n) result(rc)
        !! Returns number of rows in table `sensors`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_TARGETS, n)
    end function dm_db_count_targets

    integer function dm_db_count_sync_logs(db, n) result(rc)
        !! Returns number of rows in table `sync_logs`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SYNC_LOGS, n)
    end function dm_db_count_sync_logs

    integer function dm_db_count_sync_nodes(db, n) result(rc)
        !! Returns number of rows in table `sync_nodes`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SYNC_NODES, n)
    end function dm_db_count_sync_nodes

    integer function dm_db_count_sync_observs(db, n) result(rc)
        !! Returns number of rows in table `sync_observs`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SYNC_OBSERVS, n)
    end function dm_db_count_sync_observs

    integer function dm_db_count_sync_sensors(db, n) result(rc)
        !! Returns number of rows in table `sync_sensors`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SYNC_SENSORS, n)
    end function dm_db_count_sync_sensors

    integer function dm_db_count_sync_targets(db, n) result(rc)
        !! Returns number of rows in table `sync_sensors`.
        type(db_type),    intent(inout) :: db !! Database type.
        integer(kind=i8), intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SYNC_TARGETS, n)
    end function dm_db_count_sync_targets

    integer function dm_db_create_beats(db) result(rc)
        !! Creates logs table in given database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table or index creation failed.
        !! * `E_INVALID` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout) :: db !! Database type.

        integer :: i

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (.not. dm_db_is_connected(db)) return

        rc = db_exec(db, SQL_CREATE_BEATS)
        if (dm_is_error(rc)) return

        do i = 1, size(SQL_CREATE_BEATS_INDICES)
            rc = db_exec(db, trim(SQL_CREATE_BEATS_INDICES(i)))
            if (dm_is_error(rc)) return
        end do

        rc = E_NONE
    end function dm_db_create_beats

    integer function dm_db_create_logs(db, sync) result(rc)
        !! Creates logs table in given database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table or index creation failed.
        !! * `E_INVALID` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout)        :: db   !! Database type.
        logical,       intent(in), optional :: sync !! Create synchronisation tables.

        integer :: i

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (.not. dm_db_is_connected(db)) return

        ! Create logs table.
        rc = db_exec(db, SQL_CREATE_LOGS)
        if (dm_is_error(rc)) return

        ! Create sync logs table.
        if (dm_present(sync, .false.)) then
            rc = db_exec(db, SQL_CREATE_SYNC_LOGS)
            if (dm_is_error(rc)) return
        end if

        ! Create indices.
        do i = 1, size(SQL_CREATE_LOGS_INDICES)
            rc = db_exec(db, trim(SQL_CREATE_LOGS_INDICES(i)))
            if (dm_is_error(rc)) return
        end do

        rc = E_NONE
    end function dm_db_create_logs

    integer function dm_db_create_observs(db, sync) result(rc)
        !! Initialises a connected SQLite 3 database by creating all necessary
        !! tables if they do not exist already. The function also creates
        !! additional indices and triggers on the tables.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table, index, or trigger creation failed.
        !! * `E_INVALID` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout)        :: db   !! Database type.
        logical,       intent(in), optional :: sync !! Create synchronisation tables.

        integer :: i

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (.not. dm_db_is_connected(db)) return

        ! Create tables.
        rc = db_exec(db, SQL_CREATE_NODES);     if (dm_is_error(rc)) return
        rc = db_exec(db, SQL_CREATE_SENSORS);   if (dm_is_error(rc)) return
        rc = db_exec(db, SQL_CREATE_TARGETS);   if (dm_is_error(rc)) return
        rc = db_exec(db, SQL_CREATE_OBSERVS);   if (dm_is_error(rc)) return
        rc = db_exec(db, SQL_CREATE_RECEIVERS); if (dm_is_error(rc)) return
        rc = db_exec(db, SQL_CREATE_REQUESTS);  if (dm_is_error(rc)) return
        rc = db_exec(db, SQL_CREATE_RESPONSES); if (dm_is_error(rc)) return

        ! Create sync tables.
        if (dm_present(sync, .false.)) then
            rc = db_exec(db, SQL_CREATE_SYNC_NODES);   if (dm_is_error(rc)) return
            rc = db_exec(db, SQL_CREATE_SYNC_OBSERVS); if (dm_is_error(rc)) return
            rc = db_exec(db, SQL_CREATE_SYNC_SENSORS); if (dm_is_error(rc)) return
            rc = db_exec(db, SQL_CREATE_SYNC_TARGETS); if (dm_is_error(rc)) return
        end if

        ! Add additional indices.
        do i = 1, size(SQL_CREATE_OBSERVS_INDICES)
            rc = db_exec(db, trim(SQL_CREATE_OBSERVS_INDICES(i)))
            if (dm_is_error(rc)) return
        end do

        ! Add triggers.
        rc = db_exec(db, SQL_DELETE_OBSERV_TRIGGER)
        if (dm_is_error(rc)) return

        rc = E_NONE
    end function dm_db_create_observs

    integer function dm_db_delete_beat(db, node_id) result(rc)
        !! Deletes heartbeat from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if node id is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type),    intent(inout) :: db      !! Database type.
        character(len=*), intent(in)    :: node_id !! Node id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_DELETE_BEAT)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, node_id)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_delete_beat

    integer function dm_db_delete_log(db, log_id) result(rc)
        !! Deletes log from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if log id is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type),    intent(inout) :: db     !! Database type.
        character(len=*), intent(in)    :: log_id !! Log id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(log_id) == 0) return

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_DELETE_LOG)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, log_id)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_delete_log

    integer function dm_db_delete_node(db, node_id) result(rc)
        !! Deletes node from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if node id is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type),    intent(inout) :: db      !! Database type.
        character(len=*), intent(in)    :: node_id !! Node id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_DELETE_NODE)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, node_id)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_delete_node

    integer function dm_db_delete_observ(db, observ_id) result(rc)
        !! Deletes observation from database. The function expects the SQLite
        !! trigger `delete_observ_trigger` as defined in module `dm_sql` to be
        !! present in the database, in order to delete receivers, requests, and
        !! responses automatically.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_EXEC` if query execution failed (commit).
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_ROLLBACK` if transaction rollback failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_DB_TRANSACTION` if transaction failed.
        !! * `E_INVALID` if observation id is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: observ_id !! Observation id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(observ_id) == 0) return

        ! Start transaction.
        rc = db_begin(db)
        if (dm_is_error(rc)) return

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_DELETE_OBSERV)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)

        ! Commit transaction.
        if (dm_is_ok(rc)) then
            rc = db_commit(db)
            return
        end if

        ! Rollback transaction on error.
        if (dm_is_error(db_rollback(db))) rc = E_DB_ROLLBACK
    end function dm_db_delete_observ

    integer function dm_db_delete_sensor(db, sensor_id) result(rc)
        !! Deletes sensor of given id from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if sensor id is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: sensor_id !! Sensor id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(sensor_id) == 0) return

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_DELETE_SENSOR)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, sensor_id)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_delete_sensor

    integer function dm_db_delete_target(db, target_id) result(rc)
        !! Deletes target from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if target id is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: target_id !! Target id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(target_id) == 0) return

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_DELETE_TARGET)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, target_id)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_delete_target

    integer function dm_db_detach(db, name) result(rc)
        !! Detaches database from the current connection. If no name is passed
        !! for the attached database, the name is assumed to be `attached`. The
        !! function trims the given name string. Returns `E_DB_DETACH` on error.
        character(len=*), parameter :: QUERY = 'DETACH DATABASE '

        type(db_type),    intent(inout)        :: db   !! Database type.
        character(len=*), intent(in), optional :: name !! Name of attached database.

        integer :: stat

        rc = E_DB_DETACH

        stat = db_exec(db, QUERY // dm_btoa(present(name), name, DB_ATTACHED_NAME))
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

    integer function dm_db_get_application_id(db, id) result(rc)
        !! Returns application id of database in `id`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !!
        type(db_type), intent(inout) :: db !! Database type.
        integer,       intent(out)   :: id !! Database application id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        id = 0

        sql_block: block
            rc = db_prepare(db, db_stmt, 'PRAGMA application_id')
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_TYPE
            if (.not. db_column_is_integer(db_stmt, 0)) exit sql_block

            rc = E_NONE
            call db_column(db_stmt, 0, id)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_get_application_id

    integer function dm_db_get_data_version(db, version) result(rc)
        !! Returns data version in `version`.
        !!
        !! The integer values returned by two invocations of `PRAGMA data_version`
        !! from the same connection will be different if changes were committed to
        !! the database by any other connection in the interim.
        !!
        !! The `PRAGMA data_version` value is unchanged for commits made on the same
        !! database connection. The behaviour of `PRAGMA data_version` is the same
        !! for all database connections, including database connections in separate
        !! processes and shared cache database connections.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !!
        type(db_type), intent(inout) :: db      !! Database type.
        integer,       intent(out)   :: version !! Data version.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        version = 0

        sql_block: block
            rc = db_prepare(db, db_stmt, 'PRAGMA data_version')
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_TYPE
            if (.not. db_column_is_integer(db_stmt, 0)) exit sql_block

            rc = E_NONE
            call db_column(db_stmt, 0, version)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_get_data_version

    integer function dm_db_get_foreign_keys(db, enabled) result(rc)
        !! Returns status of foreign keys contraint in `enabled`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !!
        type(db_type), intent(inout) :: db      !! Database type.
        logical,       intent(out)   :: enabled !! Foreign keys constraint is enabled.

        integer            :: i, stat
        type(db_stmt_type) :: db_stmt

        enabled = .false.

        sql_block: block
            rc = db_prepare(db, db_stmt, 'PRAGMA foreign_keys')
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_TYPE
            if (.not. db_column_is_integer(db_stmt, 0)) exit sql_block

            rc = E_NONE
            call db_column(db_stmt, 0, i)
            enabled = (i == 1)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_get_foreign_keys

    integer function dm_db_get_journal_mode(db, mode, name) result(rc)
        !! Returns journal mode of database in `mode`. The name of the mode is
        !! optionally passed in `name`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !!
        type(db_type),                 intent(inout)         :: db   !! Database type.
        integer,                       intent(out)           :: mode !! Journal mode.
        character(len=:), allocatable, intent(out), optional :: name !! Journal mode name.

        character(len=:), allocatable :: string
        integer                       :: stat
        type(db_stmt_type)            :: db_stmt

        mode = DB_JOURNAL_OFF

        sql_block: block
            rc = db_prepare(db, db_stmt, 'PRAGMA journal_mode')
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_TYPE
            if (.not. db_column_is_text(db_stmt, 0)) exit sql_block

            rc = E_NONE
            call db_column(db_stmt, 0, string)
        end block sql_block

        stat = db_finalize(db_stmt)

        select case (string)
            case ('delete');   mode = DB_JOURNAL_DELETE
            case ('truncate'); mode = DB_JOURNAL_TRUNCATE
            case ('persist');  mode = DB_JOURNAL_PERSIST
            case ('memory');   mode = DB_JOURNAL_MEMORY
            case ('wal');      mode = DB_JOURNAL_WAL
            case default;      mode = DB_JOURNAL_OFF
        end select

        if (present(name)) name = string
    end function dm_db_get_journal_mode

    integer function dm_db_get_query_only(db, enabled) result(rc)
        !! Returns status of query-only pragma in `enabled`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !!
        type(db_type), intent(inout) :: db      !! Database type.
        logical,       intent(out)   :: enabled !! Query-only mode is enabled.

        integer            :: i, stat
        type(db_stmt_type) :: db_stmt

        enabled = .false.

        sql_block: block
            rc = db_prepare(db, db_stmt, 'PRAGMA query_only')
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_TYPE
            if (.not. db_column_is_integer(db_stmt, 0)) exit sql_block

            rc = E_NONE
            call db_column(db_stmt, 0, i)
            enabled = (i == 1)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_get_query_only

    integer function dm_db_get_schema_version(db, version) result(rc)
        !! Returns user version of database in `version`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !!
        type(db_type), intent(inout) :: db      !! Database type.
        integer,       intent(out)   :: version !! Database user version.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        version = 0

        sql_block: block
            rc = db_prepare(db, db_stmt, 'PRAGMA user_version')
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_TYPE
            if (.not. db_column_is_integer(db_stmt, 0)) exit sql_block

            rc = E_NONE
            call db_column(db_stmt, 0, version)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_get_schema_version

    logical function dm_db_has_log(db, log_id) result(has)
        !! Returns `.true.` if log id exists.
        type(db_type),    intent(inout) :: db     !! Database type.
        character(len=*), intent(in)    :: log_id !! Log id (UUID).

        has = db_has(db, SQL_TABLE_LOGS, log_id)
    end function dm_db_has_log

    logical function dm_db_has_node(db, node_id) result(has)
        !! Returns `.true.` if node id exists.
        type(db_type),    intent(inout) :: db      !! Database type.
        character(len=*), intent(in)    :: node_id !! Node id.

        has = db_has(db, SQL_TABLE_NODES, node_id)
    end function dm_db_has_node

    logical function dm_db_has_observ(db, observ_id) result(has)
        !! Returns `.true.` if observation id exists.
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: observ_id !! Observation id (UUID).

        has = db_has(db, SQL_TABLE_OBSERVS, observ_id)
    end function dm_db_has_observ

    logical function dm_db_has_sensor(db, sensor_id) result(exists)
        !! Returns `.true.` if sensor id exists.
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: sensor_id !! Sensor id.

        exists = db_has(db, SQL_TABLE_SENSORS, sensor_id)
    end function dm_db_has_sensor

    logical function dm_db_has_table(db, table) result(has)
        !! Returns `.true.` if given table exists in database.
        type(db_type), intent(inout) :: db    !! Database type.
        integer,       intent(in)    :: table !! Table enumerator.

        integer            :: rc
        type(db_stmt_type) :: db_stmt

        has = .false.
        if (table < SQL_TABLE_NODES .or. table > SQL_TABLE_LAST) return

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_SELECT_TABLE);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 1, SQL_TABLE_NAMES(table)); if (dm_is_error(rc)) exit sql_block
            rc = db_step(db_stmt);                            if (dm_is_error(rc)) exit sql_block

            has = .true.
        end block sql_block

        rc = db_finalize(db_stmt)
    end function dm_db_has_table

    logical function dm_db_has_target(db, target_id) result(has)
        !! Returns `.true.` if target id exists.
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: target_id !! Target id.

        has = db_has(db, SQL_TABLE_TARGETS, target_id)
    end function dm_db_has_target

    integer function dm_db_init() result(rc)
        !! Initialises SQLite backend. Returns `E_DB` on error.
        rc = E_DB
        if (sqlite3_initialize() /= SQLITE_OK) return
        rc = E_NONE
    end function dm_db_init

    integer function dm_db_insert_beat(db, beat, db_stmt, validate) result(rc)
        !! Adds the given heartbeat to database. The beat data is validated by
        !! default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB` if statement reset failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if argument `beat` is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_beat

        type(db_type),      intent(inout)           :: db       !! Database type.
        type(beat_type),    intent(inout)           :: beat     !! Beat to insert.
        type(db_stmt_type), intent(inout), optional :: db_stmt  !! Database statement type.
        logical,            intent(in),    optional :: validate !! Validate beat.

        integer            :: stat
        type(db_stmt_type) :: db_stmt_

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate beat.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_beat_is_valid(beat)) return
        end if

        ! Set given statement.
        if (present(db_stmt)) db_stmt_ = db_stmt

        sql_block: block
            if (.not. dm_db_stmt_is_prepared(db_stmt_)) then
                rc = db_prepare(db, db_stmt_, SQL_INSERT_BEAT)
                if (dm_is_error(rc)) exit sql_block
            end if

            rc = db_bind(db_stmt_, 1, beat%node_id);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 2, beat%address);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 3, beat%client);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 4, beat%time_sent);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 5, beat%time_recv);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 6, beat%error);      if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 7, beat%interval);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 8, beat%uptime);     if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt_)
            if (dm_is_error(rc)) exit sql_block

            rc = db_reset(db_stmt_)
        end block sql_block

        if (present(db_stmt)) then
            db_stmt = db_stmt_
        else
            stat = db_finalize(db_stmt_)
        end if
    end function dm_db_insert_beat

    integer function dm_db_insert_beats(db, beats, transaction, validate) result(rc)
        !! Adds array of beats to database. A transaction is used unless
        !! `transaction` is `.false.`. The beat data is validated by default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB` if statement reset failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_EXEC` if execution of transaction statement failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_ROLLBACK` if transaction rollback failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_DB_TRANSACTION` if transaction failed.
        !! * `E_EMPTY` if array `beats` is empty.
        !! * `E_INVALID` if an element in `beats` is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_beat

        type(db_type),   intent(inout)        :: db          !! Database type.
        type(beat_type), intent(inout)        :: beats(:)    !! Beat type array.
        logical,         intent(in), optional :: transaction !! Use SQL transaction.
        logical,         intent(in), optional :: validate    !! Validate beats.

        integer            :: i, stat
        logical            :: transaction_
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_EMPTY
        if (size(beats) == 0) return

        transaction_ = dm_present(transaction, .true.)

        ! Start transaction.
        if (transaction_) then
            rc = db_begin(db)
            if (dm_is_error(rc)) return
        end if

        ! Re-use statement.
        do i = 1, size(beats)
            rc = dm_db_insert_beat(db, beats(i), db_stmt, validate=validate)
            if (dm_is_error(rc)) exit
        end do

        stat = db_finalize(db_stmt)

        if (transaction_) then
            ! Commit transaction.
            if (dm_is_ok(rc)) rc = db_commit(db)
            if (dm_is_ok(rc)) return
            ! Rollback transaction.
            if (dm_is_error(db_rollback(db))) rc = E_DB_ROLLBACK
        end if
    end function dm_db_insert_beats

    integer function dm_db_insert_log(db, log, validate) result(rc)
        !! Adds the given log to database. The log data is validated by
        !! default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if argument `log` is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_log

        type(db_type),  intent(inout)        :: db       !! Database type.
        type(log_type), intent(inout)        :: log      !! Log message to insert.
        logical,        intent(in), optional :: validate !! Validate log.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate log.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_log_is_valid(log)) return
        end if

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_INSERT_LOG)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt,  1, log%id);        if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  2, log%level);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  3, log%error);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  4, log%timestamp); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  5, log%node_id);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  6, log%sensor_id); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  7, log%target_id); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  8, log%observ_id); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  9, log%source);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 10, log%message);   if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_insert_log

    integer function dm_db_insert_node(db, node, validate) result(rc)
        !! Adds the given node to database. The node data is validated by
        !! default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if argument `node` is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_node

        type(db_type),   intent(inout)        :: db       !! Database type.
        type(node_type), intent(inout)        :: node     !! Node to insert.
        logical,         intent(in), optional :: validate !! Validate node.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate node.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_node_is_valid(node)) return
        end if

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_INSERT_NODE)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, node%id);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 2, node%name); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 3, node%meta); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 4, node%x);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 5, node%y);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 6, node%z);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 7, node%lon);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 8, node%lat);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 9, node%alt);  if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_insert_node

    integer function dm_db_insert_observ(db, observ, db_stmt, validate) result(rc)
        !! Adds single observation to database, including receivers, requests,
        !! and responses. If the insert query fails, the transaction will be
        !! rolled back, i.e., no part of the observation is written to the
        !! database on error. The observation data is validated by default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB` if statement reset failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_EXEC` if execution of transaction statement failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_ROLLBACK` if transaction rollback failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_DB_TRANSACTION` if transaction failed.
        !! * `E_INVALID` if argument `observ` is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_observ

        character(len=*), parameter :: SAVE_POINT = 'observ'

        type(db_type),      intent(inout)           :: db       !! Database type.
        type(observ_type),  intent(inout)           :: observ   !! Observation type.
        type(db_stmt_type), intent(inout), optional :: db_stmt  !! Database statement type.
        logical,            intent(in),    optional :: validate !! Validate observation.

        integer            :: i, n, stat
        type(db_stmt_type) :: db_stmt_

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate observation.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_observ_is_valid(observ)) return
        end if

        ! Set given statement.
        if (present(db_stmt)) db_stmt_ = db_stmt

        ! Begin transaction.
        rc = E_DB_TRANSACTION
        if (dm_is_error(db_save_point(db, SAVE_POINT))) return

        sql_block: block
            if (.not. dm_db_stmt_is_prepared(db_stmt_)) then
                rc = db_prepare(db, db_stmt_, SQL_INSERT_OBSERV)
                if (dm_is_error(rc)) exit sql_block
            end if

            ! Add observation data.
            rc = db_bind(db_stmt_,  1, observ%id);         if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_,  2, observ%node_id);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_,  3, observ%sensor_id);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_,  4, observ%target_id);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_,  5, observ%name);       if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_,  6, observ%timestamp);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_,  7, observ%source);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_,  8, observ%device);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_,  9, observ%priority);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 10, observ%error);      if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 11, observ%next);       if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 12, observ%nreceivers); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 13, observ%nrequests);  if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt_);  if (dm_is_error(rc)) exit sql_block
            rc = db_reset(db_stmt_); if (dm_is_error(rc)) exit sql_block

            ! Add receivers.
            if (observ%nreceivers > 0) then
                rc = db_insert_receivers(db, observ%id, observ%receivers(1:observ%nreceivers))
                if (dm_is_error(rc)) exit sql_block
            end if

            ! Add requests.
            if (observ%nrequests > 0) then
                rc = db_insert_requests(db, observ%id, observ%requests(1:observ%nrequests))
                if (dm_is_error(rc)) exit sql_block

                ! Add responses.
                do i = 1, observ%nrequests
                    n = observ%requests(i)%nresponses
                    if (n == 0) cycle

                    rc = db_insert_responses(db, observ%id, i, observ%requests(i)%responses(1:n))
                    if (dm_is_error(rc)) exit sql_block
                end do
            end if

            rc = E_NONE
        end block sql_block

        if (present(db_stmt)) then
            db_stmt = db_stmt_
        else
            stat = db_finalize(db_stmt_)
        end if

        ! Commit or rollback transaction.
        if (dm_is_error(rc)) then
            rc = dm_db_error(db)
            if (db_rollback(db, SAVE_POINT) /= SQLITE_OK) rc = E_DB_ROLLBACK
        end if

        if (dm_is_error(db_release(db, SAVE_POINT))) rc = E_DB_TRANSACTION
    end function dm_db_insert_observ

    integer function dm_db_insert_observs(db, observs, transaction, validate) result(rc)
        !! Adds array of observations to database. A transaction is used unless
        !! `transaction` is `.false.`. The observation data is validated by
        !! default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB` if statement reset failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_EXEC` if execution of transaction statement failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_ROLLBACK` if transaction rollback failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_DB_TRANSACTION` if transaction failed.
        !! * `E_EMPTY` if array `observs` is empty.
        !! * `E_INVALID` if an element in `observs` is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_observ

        type(db_type),     intent(inout)        :: db          !! Database type.
        type(observ_type), intent(inout)        :: observs(:)  !! Observation type array.
        logical,           intent(in), optional :: transaction !! Use SQL transaction.
        logical,           intent(in), optional :: validate    !! Validate observations.

        integer            :: i, stat
        logical            :: transaction_
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_EMPTY
        if (size(observs) == 0) return

        transaction_ = dm_present(transaction, .true.)

        ! Start transaction.
        if (transaction_) then
            rc = db_begin(db)
            if (dm_is_error(rc)) return
        end if

        ! Re-use statement.
        do i = 1, size(observs)
            rc = dm_db_insert_observ(db, observs(i), db_stmt, validate=validate)
            if (dm_is_error(rc)) exit
        end do

        stat = db_finalize(db_stmt)

        if (transaction_) then
            ! Commit transaction.
            if (dm_is_ok(rc)) rc = db_commit(db)
            if (dm_is_ok(rc)) return
            ! Rollback transaction.
            if (dm_is_error(db_rollback(db))) rc = E_DB_ROLLBACK
        end if
    end function dm_db_insert_observs

    integer function dm_db_insert_sensor(db, sensor, validate) result(rc)
        !! Adds given sensor to database. The sensor data is validated by
        !! default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if argument `sensor` is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_sensor

        type(db_type),     intent(inout)        :: db       !! Database type.
        type(sensor_type), intent(inout)        :: sensor   !! Sensor to insert.
        logical,           intent(in), optional :: validate !! Validate sensor.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate sensor.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_sensor_is_valid(sensor)) return
        end if

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_INSERT_SENSOR)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt,  1, sensor%id);      if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  2, sensor%node_id); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  3, sensor%type);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  4, sensor%name);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  5, sensor%sn);      if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  6, sensor%meta);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  7, sensor%x);       if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  8, sensor%y);       if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  9, sensor%z);       if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 10, sensor%lon);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 11, sensor%lat);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 12, sensor%alt);     if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_insert_sensor

    integer function dm_db_insert_sync(db, sync) result(rc)
        !! Wrapper function that inserts or replaces given sync data into
        !! database. Sync data must have a valid type.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if sync type is invalid.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(inout) :: sync !! Sync data to insert.

        select case (sync%type)
            case (SYNC_TYPE_LOG);    rc = dm_db_insert_sync_log   (db, sync)
            case (SYNC_TYPE_NODE);   rc = dm_db_insert_sync_node  (db, sync)
            case (SYNC_TYPE_OBSERV); rc = dm_db_insert_sync_observ(db, sync)
            case (SYNC_TYPE_SENSOR); rc = dm_db_insert_sync_sensor(db, sync)
            case (SYNC_TYPE_TARGET); rc = dm_db_insert_sync_target(db, sync)
            case default;            rc = E_INVALID
        end select
    end function dm_db_insert_sync

    integer function dm_db_insert_sync_log(db, sync) result(rc)
        !! Inserts or replaces given log sync data into database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if sync type is invalid.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(inout) :: sync !! Sync data to insert.

        rc = E_INVALID
        if (sync%type /= SYNC_TYPE_LOG) return

        rc = db_insert_sync(db, sync, SQL_INSERT_SYNC_LOG)
    end function dm_db_insert_sync_log

    integer function dm_db_insert_sync_node(db, sync) result(rc)
        !! Inserts or replaces given node sync data into database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if sync type is invalid.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(inout) :: sync !! Sync data to insert.

        rc = E_INVALID
        if (sync%type /= SYNC_TYPE_NODE) return

        rc = db_insert_sync(db, sync, SQL_INSERT_SYNC_NODE)
    end function dm_db_insert_sync_node

    integer function dm_db_insert_sync_observ(db, sync) result(rc)
        !! Inserts or replaces given observation sync data into database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if sync type is invalid.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(inout) :: sync !! Sync data to insert.

        rc = E_INVALID
        if (sync%type /= SYNC_TYPE_OBSERV) return

        rc = db_insert_sync(db, sync, SQL_INSERT_SYNC_OBSERV)
    end function dm_db_insert_sync_observ

    integer function dm_db_insert_sync_sensor(db, sync) result(rc)
        !! Inserts or replaces given sensor sync data into database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if sync type is invalid.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(inout) :: sync !! Sync data to insert.

        rc = E_INVALID
        if (sync%type /= SYNC_TYPE_SENSOR) return

        rc = db_insert_sync(db, sync, SQL_INSERT_SYNC_SENSOR)
    end function dm_db_insert_sync_sensor

    integer function dm_db_insert_sync_target(db, sync) result(rc)
        !! Inserts or replaces given target sync data into database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if sync type is invalid.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(inout) :: sync !! Sync data to insert.

        rc = E_INVALID
        if (sync%type /= SYNC_TYPE_TARGET) return

        rc = db_insert_sync(db, sync, SQL_INSERT_SYNC_TARGET)
    end function dm_db_insert_sync_target

    integer function dm_db_insert_target(db, target, validate) result(rc)
        !! Adds given target to database. The target data is validated by
        !! default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if argument `target` is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_target

        type(db_type),     intent(inout)        :: db       !! Database type.
        type(target_type), intent(inout)        :: target   !! Target to insert.
        logical,           intent(in), optional :: validate !! Validate target.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate target.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_target_is_valid(target)) return
        end if

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_INSERT_TARGET)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt,  1, target%id);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  2, target%name);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  3, target%meta);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  4, target%state); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  5, target%x);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  6, target%y);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  7, target%z);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  8, target%lon);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  9, target%lat);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 10, target%alt);   if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_insert_target

    logical function dm_db_is_connected(db) result(connected)
        !! Returns `.true.` if database type has associated pointer.
        type(db_type), intent(inout) :: db !! Database type.

        connected = c_associated(db%ctx)
    end function dm_db_is_connected

    logical function dm_db_is_read_only(db) result(read_only)
        !! Returns `.true.` if database is in read-only mode. This function
        !! checks only the opaque database type for the read-only flag. It is
        !! still possible to enable ready-only access by calling
        !! `dm_db_set_query_only()`. The function `dm_db_get_query_only()`
        !! returns the status of the `query_only` pragma.
        type(db_type), intent(inout) :: db !! Database type.

        read_only = db%read_only
    end function dm_db_is_read_only

    logical function dm_db_is_threadsafe() result(safe)
        !! Returns true if SQLite 3 was compiled threadsafe.
        safe = (sqlite3_threadsafe() == SQLITE_OK)
    end function dm_db_is_threadsafe

    integer function dm_db_open(db, path, create, foreign_keys, read_only, threaded, &
                                timeout, validate, wal) result(rc)
        !! Opens connection to the SQLite database at `path`, or creates a new
        !! database with given file path if `create` is passed and `.true.`.
        !! The foreign key constraint is enabled unless `foreign_keys` is
        !! `.false.`.
        !!
        !! The databases is opened in read-only mode if `read_only` is `.true.`.
        !! Threaded access to the database is disabled by default. The busy
        !! timeout is 0 mseconds unless argument `timeout` is passed.
        !!
        !! If argument `validate` is passed and `.true.`, the application id of
        !! the database is validated. The function returns `E_DB_ID` if the id
        !! is missing or invalid.
        !!
        !! Write-Ahead Logging is enabled if `wal` is `.true.`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB` if opening the database failed.
        !! * `E_DB_ID` if the database has a wrong application id.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_DB_VERSION` if the user version is incompatible.
        !! * `E_INVALID` if database is already opened.
        !! * `E_NOT_FOUND` if database has not been found.
        !!
        use :: dm_file

        type(db_type),    intent(inout)        :: db           !! Database type.
        character(len=*), intent(in)           :: path         !! File path of database.
        logical,          intent(in), optional :: create       !! Create flag (off by default).
        logical,          intent(in), optional :: foreign_keys !! Foreign keys contraint flag (on by default).
        logical,          intent(in), optional :: read_only    !! Read-only mode (off by default).
        logical,          intent(in), optional :: threaded     !! Threaded access flag (off by default).
        integer,          intent(in), optional :: timeout      !! Busy timeout in mseconds (0 by default).
        logical,          intent(in), optional :: validate     !! Validate application id (off by default).
        logical,          intent(in), optional :: wal          !! WAL journal mode flag (off by default).

        integer :: flag, timeout_
        logical :: create_, exists, foreign_keys_, threaded_, validate_, wal_

        create_       = dm_present(create,       .false.) ! Create database.
        foreign_keys_ = dm_present(foreign_keys, .true.)  ! Foreign keys contraint.
        db%read_only  = dm_present(read_only,    .false.) ! Read-only mode.
        threaded_     = dm_present(threaded,     .false.) ! Threaded access (not recommended).
        timeout_      = dm_present(timeout,      0)       ! Busy timeout.
        validate_     = dm_present(validate,     .true.)  ! App ID validation.
        wal_          = dm_present(wal,          .false.) ! WAL mode.

        ! Validate options.
        exists = dm_file_exists(path)

        rc = E_INVALID
        if (dm_db_is_connected(db)) return

        rc = E_NOT_FOUND
        if (.not. create_ .and. .not. exists) return

        ! Set database flags.
        rc = E_DB
        flag = SQLITE_OPEN_PRIVATECACHE

        if (db%read_only) then
            flag = ior(flag, SQLITE_OPEN_READONLY)
        else
            flag = ior(flag, SQLITE_OPEN_READWRITE)
        end if

        if (create_ .and. .not. exists) then
            flag = ior(flag, SQLITE_OPEN_CREATE)
        end if

        if (threaded_) then
            flag = ior(flag, SQLITE_OPEN_FULLMUTEX)
        end if

        ! Open database.
        if (sqlite3_initialize() /= SQLITE_OK) return
        if (sqlite3_open_v2(trim(path), db%ctx, flag) /= SQLITE_OK) return

        ! Enable foreign keys constraint.
        if (foreign_keys_) then
            rc = dm_db_set_foreign_keys(db, .true.)
            if (dm_is_error(rc)) return
        end if

        ! Prepare database on create.
        if (create_) then
            if (.not. exists) then
                ! Set application id.
                rc = dm_db_set_application_id(db, DB_APPLICATION_ID)
                if (dm_is_error(rc)) return

                ! Set database version.
                rc = dm_db_set_schema_version(db, DB_SCHEMA_VERSION)
                if (dm_is_error(rc)) return
            end if

            ! Enable WAL mode.
            if (wal_) then
                rc = dm_db_set_journal_mode(db, DB_JOURNAL_WAL)
                if (dm_is_error(rc)) return
            end if
        end if

        ! Set busy timeout.
        if (timeout_ > 0) then
            rc = dm_db_set_busy_timeout(db, timeout_)
            if (dm_is_error(rc)) return
        end if

        ! Validate the application id and the user version of the database.
        if (validate_) then
            rc = dm_db_validate(db)
            if (dm_is_error(rc)) return
        end if

        rc = E_NONE
    end function dm_db_open

    integer function dm_db_optimize(db) result(rc)
        !! Attempts to optimise the database. All schemas are optimised.
        !!
        !! To achieve the best long-term query performance without the need to
        !! do a detailed engineering analysis of the application schema and SQL,
        !! it is recommended that applications run `PRAGMA optimize` (with no
        !! arguments) just before closing each database connection.
        !! Long-running applications might also benefit from setting a timer to
        !! run `PRAGMA optimize` every few hours.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        type(db_type), intent(inout) :: db !! Database type.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = db_prepare(db, db_stmt, 'PRAGMA optimize')
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_optimize

    integer function dm_db_rollback(db) result(rc)
        !! Rolls a transaction back. Returns `E_DB_ROLLBACK` on error.
        type(db_type), intent(inout) :: db !! Database type.

        rc = db_rollback(db)
    end function dm_db_rollback

    integer function dm_db_select_beat(db, beat, node_id) result(rc)
        !! Returns heartbeat associated with given node id in `beat`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_beat

        type(db_type),    intent(inout) :: db      !! Database type.
        type(beat_type),  intent(out)   :: beat    !! Returned beat type.
        character(len=*), intent(in)    :: node_id !! Node id.

        integer             :: stat
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        call dm_db_query_add_text(db_query, 'node_id = ?', node_id)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_BEATS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_NO_ROWS
            if (dm_is_error(db_step(db_stmt))) exit sql_block

            rc = db_next_row(db_stmt, beat)
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
    end function dm_db_select_beat

    integer function dm_db_select_json_beat(db, json, node_id) result(rc)
        !! Returns heartbeat associated with given node id as allocatable
        !! character `json` in JSON format.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        type(db_type),                 intent(inout) :: db      !! Database type.
        character(len=:), allocatable, intent(out)   :: json    !! Returned JSON.
        character(len=*),              intent(in)    :: node_id !! Node id.

        integer             :: stat
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        call dm_db_query_add_text(db_query, 'node_id = ?', node_id)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_JSON_BEATS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_NO_ROWS
            if (dm_is_error(db_step(db_stmt))) exit sql_block

            rc = db_next_row(db_stmt, json)
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(json)) json = ''
    end function dm_db_select_json_beat

    integer function dm_db_select_json_log(db, json, log_id) result(rc)
        !! Returns log associated with given id as allocatable character in
        !! JSON format in `json`. If no log has been found, the string will
        !! be empty and the function returns `E_DB_NO_ROWS`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        type(db_type),                 intent(inout) :: db     !! Database type.
        character(len=:), allocatable, intent(out)   :: json   !! Returned JSON.
        character(len=*),              intent(in)    :: log_id !! Log id.

        integer             :: stat
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(log_id) == 0) return

        call dm_db_query_add_text(db_query, 'id = ?', log_id)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_JSON_LOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_NO_ROWS
            if (dm_is_error(db_step(db_stmt))) exit sql_block

            rc = db_next_row(db_stmt, json)
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(json)) json = ''
    end function dm_db_select_json_log

    integer function dm_db_select_json_node(db, json, node_id) result(rc)
        !! Returns node associated with given node id as allocatable character
        !! `json` in JSON format.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        type(db_type),                 intent(inout) :: db      !! Database type.
        character(len=:), allocatable, intent(out)   :: json    !! Returned JSON.
        character(len=*),              intent(in)    :: node_id !! Node id.

        integer             :: stat
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        call dm_db_query_add_text(db_query, 'id = ?', node_id)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_JSON_NODES))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_NO_ROWS
            if (dm_is_error(db_step(db_stmt))) exit sql_block

            rc = db_next_row(db_stmt, json)
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(json)) json = ''
    end function dm_db_select_json_node

    integer function dm_db_select_log(db, log, log_id) result(rc)
        !! Returns log associated with given id in `log`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_log

        type(db_type),    intent(inout) :: db     !! Database type.
        type(log_type),   intent(out)   :: log    !! Returned log data.
        character(len=*), intent(in)    :: log_id !! Log id.

        integer             :: stat
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(log_id) == 0) return

        call dm_db_query_add_text(db_query, 'id = ?', log_id)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_LOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_NO_ROWS
            if (dm_is_error(db_step(db_stmt))) exit sql_block

            rc = db_next_row(db_stmt, log)
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
    end function dm_db_select_log

    integer function dm_db_select_node(db, node, node_id) result(rc)
        !! Returns node data associated with given id in `node`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_node

        type(db_type),    intent(inout) :: db      !! Database type.
        type(node_type),  intent(out)   :: node    !! Returned node data.
        character(len=*), intent(in)    :: node_id !! Node id.

        integer             :: stat
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        call dm_db_query_add_text(db_query, 'nodes.id = ?', node_id)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NODES))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_NO_ROWS
            if (dm_is_error(db_step(db_stmt))) exit sql_block

            rc = db_next_row(db_stmt, node)
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
    end function dm_db_select_node

    integer function dm_db_select_observ(db, observ, observ_id) result(rc)
        !! Returns observation referenced by the given id from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_observ

        type(db_type),     intent(inout) :: db        !! Database type.
        type(observ_type), intent(out)   :: observ    !! Selected observation.
        character(len=*),  intent(in)    :: observ_id !! Observation id (UUID).

        integer             :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(observ_id) == 0) return

        call dm_db_query_add_text(db_query, 'observs.id = ?', observ_id)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_NO_ROWS
            if (dm_is_error(db_step(db_stmt))) exit sql_block

            rc = db_next_row(db_stmt, observ)
        end block sql_block

        call dm_db_query_destroy(db_query)
        rc = db_finalize(db_stmt)
        if (dm_is_error(rc)) return

        ! Get receivers.
        if (observ%nreceivers > 0) then
            rc = db_select_receivers(db, observ%receivers, observ%id)
            if (dm_is_error(rc)) return
        end if

        ! Get requests.
        if (observ%nrequests > 0) then
            rc = db_select_requests(db, observ%requests, observ%id)
            if (dm_is_error(rc)) return
        end if

        ! Get responses.
        do i = 1, observ%nrequests
            n = observ%requests(i)%nresponses
            if (n == 0) cycle

            rc = db_select_responses(db, observ%requests(i)%responses, observ%id, i)
            if (dm_is_error(rc)) exit
        end do
    end function dm_db_select_observ

    integer function dm_db_select_observ_ids(db, ids, node_id, sensor_id, target_id, from, to, &
                                             desc, limit, nids) result(rc)
        !! Returns observation ids in `ids`, with optional node id, sensor id,
        !! target id, from, to. By default, ids are returned ordered by
        !! ascending observation timestamp, unless `desc` is passed and
        !! `.true.`. The maximum number of ids may be passed in `limit`.
        !!
        !! The total number of ids is returned in optional argument `nids`,
        !! which may be greater than `limit`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if the database returned an invalid id.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),                      intent(inout)         :: db        !! Database type.
        character(len=ID_LEN), allocatable, intent(out)           :: ids(:)    !! Returned observation ids.
        character(len=*),                   intent(in),  optional :: node_id   !! Node id.
        character(len=*),                   intent(in),  optional :: sensor_id !! Sensor id.
        character(len=*),                   intent(in),  optional :: target_id !! Target id.
        character(len=*),                   intent(in),  optional :: from      !! Beginning of time span.
        character(len=*),                   intent(in),  optional :: to        !! End of time span.
        logical,                            intent(in),  optional :: desc      !! Descending order.
        integer(kind=i8),                   intent(in),  optional :: limit     !! Max. number of observations.
        integer(kind=i8),                   intent(out), optional :: nids      !! Total number of observation ids (may be greater than limit).

        integer             :: nbytes, stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nids)) nids = 0_i8

        call dm_db_query_add_text(db_query, 'nodes.id = ?',           node_id)
        call dm_db_query_add_text(db_query, 'sensors.id = ?',         sensor_id)
        call dm_db_query_add_text(db_query, 'targets.id = ?',         target_id)
        call dm_db_query_add_text(db_query, 'observs.timestamp >= ?', from)
        call dm_db_query_add_text(db_query, 'observs.timestamp < ?',  to)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NOBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call db_column(db_stmt, 0, n)

            rc = db_finalize(db_stmt)
            if (dm_is_error(rc)) return

            if (present(nids))  nids = n
            if (present(limit)) n    = min(n, limit)

            rc = E_ALLOC
            allocate (ids(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'observs.timestamp', desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERV_IDS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, ids(i), nbytes, (i == 1))
                if (dm_is_error(rc)) exit sql_block

                rc = E_INVALID
                if (nbytes /= OBSERV_ID_LEN) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(ids)) allocate (ids(0))
    end function dm_db_select_observ_ids

    integer function dm_db_select_observ_views(db, views, node_id, sensor_id, target_id, response_name, &
                                               from, to, limit, nviews) result(rc)
        !! Returns observation views of the given time range from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),                       intent(inout)         :: db            !! Database type.
        type(observ_view_type), allocatable, intent(out)           :: views(:)      !! Returned observation views.
        character(len=*),                    intent(in)            :: node_id       !! Node id.
        character(len=*),                    intent(in)            :: sensor_id     !! Sensor id.
        character(len=*),                    intent(in)            :: target_id     !! Target id.
        character(len=*),                    intent(in)            :: response_name !! Response name.
        character(len=*),                    intent(in)            :: from          !! Beginning of time span.
        character(len=*),                    intent(in)            :: to            !! End of time span.
        integer(kind=i8),                    intent(in),  optional :: limit         !! Max. number of views.
        integer(kind=i8),                    intent(out), optional :: nviews        !! Total number of views (may be greater than limit).

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nviews)) nviews = 0_i8

        call dm_db_query_add_text(db_query, 'nodes.id = ?',            node_id)
        call dm_db_query_add_text(db_query, 'sensors.id = ?',          sensor_id)
        call dm_db_query_add_text(db_query, 'targets.id = ?',          target_id)
        call dm_db_query_add_text(db_query, 'responses.name = ?',      response_name)
        call dm_db_query_add_text(db_query, 'requests.timestamp >= ?', from)
        call dm_db_query_add_text(db_query, 'requests.timestamp < ?',  to)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NOBSERV_VIEWS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call db_column(db_stmt, 0, n)

            rc = db_finalize(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            if (present(nviews)) nviews = n
            if (present(limit))  n      = min(n, limit)

            rc = E_ALLOC
            allocate (views(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'requests.timestamp', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERV_VIEWS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, views(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(views)) allocate (views(0))
    end function dm_db_select_observ_views

    integer function dm_db_select_observs_by_id(db, observs, after, before, limit, stub, nobservs) result(rc)
        !! Returns observations of a given id range in `observs`. The argument
        !! `after` is the id of the observation after which the range starts,
        !! `before` the id of the observation that limits the range.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if observations of given ids are not related.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(observ_type), allocatable, intent(out)           :: observs(:) !! Returned observation data.
        character(len=*),               intent(in)            :: after      !! Id of observation with timestamp before first of range.
        character(len=*),               intent(in),  optional :: before     !! Id of observation with timestamp after last of range.
        integer(kind=i8),               intent(in),  optional :: limit      !! Max. number of observations.
        logical,                        intent(in),  optional :: stub       !! Without receivers, requests, responses.
        integer(kind=i8),               intent(out), optional :: nobservs   !! Total number of observations (may be greater than limit).

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt
        type(observ_type)   :: observ1, observ2

        if (present(nobservs)) nobservs = 0_i8

        rc = dm_db_select_observ(db, observ1, after)
        if (dm_is_error(rc)) return

        if (present(before)) then
            rc = dm_db_select_observ(db, observ2, before)
            if (dm_is_error(rc)) return

            rc = E_INVALID
            if (observ1%node_id   /= observ2%node_id)   return
            if (observ1%sensor_id /= observ2%sensor_id) return
            if (observ1%target_id /= observ2%target_id) return
        end if

        call dm_db_query_add_text(db_query, 'nodes.id = ?',    observ1%node_id)
        call dm_db_query_add_text(db_query, 'sensors.id = ?',  observ1%sensor_id)
        call dm_db_query_add_text(db_query, 'targets.id = ?',  observ1%target_id)
        call dm_db_query_add_text(db_query, 'observs.id <> ?', after)
        call dm_db_query_add_text(db_query, 'observs.timestamp >= (SELECT timestamp FROM observs WHERE id = ?)', after)
        call dm_db_query_add_text(db_query, 'observs.timestamp < (SELECT timestamp FROM observs WHERE id = ?)',  before)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NOBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call db_column(db_stmt, 0, n)

            rc = db_finalize(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            if (present(nobservs)) nobservs = n
            if (present(limit))    n        = min(n, limit)

            rc = E_ALLOC
            allocate (observs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'observs.timestamp', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, observs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)

        if (.not. allocated(observs)) allocate (observs(0))
        if (dm_is_error(rc)) return
        if (dm_present(stub, .false.)) return
        if (size(observs) == 0) return

        rc = db_select_observs_data(db, observs)
    end function dm_db_select_observs_by_id

    integer function dm_db_select_observs_by_time(db, observs, node_id, sensor_id, target_id, from, to, &
                                                  limit, stub, nobservs) result(rc)
        !! Returns observations of a given time span in `observs`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(observ_type), allocatable, intent(out)           :: observs(:) !! Returned observation data.
        character(len=*),               intent(in)            :: node_id    !! Node id.
        character(len=*),               intent(in)            :: sensor_id  !! Sensor id.
        character(len=*),               intent(in)            :: target_id  !! Target id.
        character(len=*),               intent(in)            :: from       !! Beginning of time span.
        character(len=*),               intent(in)            :: to         !! End of time span.
        integer(kind=i8),               intent(in),  optional :: limit      !! Max. number of observations.
        logical,                        intent(in),  optional :: stub       !! No receivers, requests, responses.
        integer(kind=i8),               intent(out), optional :: nobservs   !! Total number of observations (may be greater than limit).

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nobservs)) nobservs = 0_i8

        call dm_db_query_add_text(db_query, 'nodes.id = ?',           node_id)
        call dm_db_query_add_text(db_query, 'sensors.id = ?',         sensor_id)
        call dm_db_query_add_text(db_query, 'targets.id = ?',         target_id)
        call dm_db_query_add_text(db_query, 'observs.timestamp >= ?', from)
        call dm_db_query_add_text(db_query, 'observs.timestamp < ?',  to)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NOBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call db_column(db_stmt, 0, n)

            if (present(nobservs)) nobservs = n
            if (present(limit))    n        = min(n, limit)

            rc = db_finalize(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_ALLOC
            allocate (observs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'observs.timestamp', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, observs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)

        if (.not. allocated(observs)) allocate (observs(0))
        if (dm_is_error(rc)) return
        if (dm_present(stub, .false.)) return
        if (size(observs) == 0) return

        rc = db_select_observs_data(db, observs)
    end function dm_db_select_observs_by_time

    integer function dm_db_select_sensor(db, sensor, sensor_id) result(rc)
        !! Returns sensor data associated with given sensor id from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_sensor

        type(db_type),     intent(inout) :: db        !! Database type.
        type(sensor_type), intent(out)   :: sensor    !! Returned sensor data.
        character(len=*),  intent(in)    :: sensor_id !! Sensor id.

        integer             :: stat
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(sensor_id) == 0) return

        call dm_db_query_add_text(db_query, 'sensors.id = ?', sensor_id)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_SENSORS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_NO_ROWS
            if (dm_is_error(db_step(db_stmt))) exit sql_block

            rc = db_next_row(db_stmt, sensor)
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
    end function dm_db_select_sensor

    integer function dm_db_select_sync_log(db, sync) result(rc)
        !! Returns log synchronisation data (oldest not transmitted log).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(out)   :: sync !! Returned sync data.

        rc = db_select_sync(db, SYNC_TYPE_LOG, SQL_SELECT_SYNC_LOGS // ' LIMIT 1', sync)
    end function dm_db_select_sync_log

    integer function dm_db_select_sync_logs(db, syncs, nsyncs, limit) result(rc)
        !! Returns log synchronisation data.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !!
        use :: dm_sync

        type(db_type),                intent(inout)         :: db       !! Database type.
        type(sync_type), allocatable, intent(out)           :: syncs(:) !! Returned sync data.
        integer(kind=i8),             intent(out), optional :: nsyncs   !! Array size.
        integer(kind=i8),             intent(in),  optional :: limit    !! Max. number of sync data to fetch.

        integer(kind=i8) :: n

        rc = db_select_syncs(db, SYNC_TYPE_LOG, SQL_SELECT_NSYNC_LOGS, SQL_SELECT_SYNC_LOGS, syncs, n, limit)
        if (present(nsyncs)) nsyncs = n
    end function dm_db_select_sync_logs

    integer function dm_db_select_sync_node(db, sync) result(rc)
        !! Returns node synchronisation data (oldest not transmitted node).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(out)   :: sync !! Returned sync data.

        rc = db_select_sync(db, SYNC_TYPE_NODE, SQL_SELECT_SYNC_NODES // ' LIMIT 1', sync)
    end function dm_db_select_sync_node

    integer function dm_db_select_sync_nodes(db, syncs, nsyncs, limit) result(rc)
        !! Returns node synchronisation data.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !!
        use :: dm_sync

        type(db_type),                intent(inout)         :: db       !! Database type.
        type(sync_type), allocatable, intent(out)           :: syncs(:) !! Returned sync data.
        integer(kind=i8),             intent(out), optional :: nsyncs   !! Array size.
        integer(kind=i8),             intent(in),  optional :: limit    !! Max. number of sync data to fetch.

        integer(kind=i8) :: n

        rc = db_select_syncs(db, SYNC_TYPE_NODE, SQL_SELECT_NSYNC_NODES, SQL_SELECT_SYNC_NODES, syncs, n, limit)
        if (present(nsyncs)) nsyncs = n
    end function dm_db_select_sync_nodes

    integer function dm_db_select_sync_observ(db, sync) result(rc)
        !! Returns observation synchronisation data (oldest not transmitted
        !! observation).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(out)   :: sync !! Returned sync data.

        rc = db_select_sync(db, SYNC_TYPE_OBSERV, SQL_SELECT_SYNC_OBSERVS // ' LIMIT 1', sync)
    end function dm_db_select_sync_observ

    integer function dm_db_select_sync_observs(db, syncs, nsyncs, limit) result(rc)
        !! Returns observation synchronisation data.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !!
        use :: dm_sync

        type(db_type),                intent(inout)         :: db       !! Database type.
        type(sync_type), allocatable, intent(out)           :: syncs(:) !! Returned sync data.
        integer(kind=i8),             intent(out), optional :: nsyncs   !! Array size.
        integer(kind=i8),             intent(in),  optional :: limit    !! Max. number of sync data to fetch.

        integer(kind=i8) :: n

        rc = db_select_syncs(db, SYNC_TYPE_OBSERV, SQL_SELECT_NSYNC_OBSERVS, SQL_SELECT_SYNC_OBSERVS, syncs, n, limit)
        if (present(nsyncs)) nsyncs = n
    end function dm_db_select_sync_observs

    integer function dm_db_select_sync_sensor(db, sync) result(rc)
        !! Returns sensor synchronisation data (oldest not transmitted sensor).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(out)   :: sync !! Returned sync data.

        rc = db_select_sync(db, SYNC_TYPE_SENSOR, SQL_SELECT_SYNC_SENSORS // ' LIMIT 1', sync)
    end function dm_db_select_sync_sensor

    integer function dm_db_select_sync_sensors(db, syncs, nsyncs, limit) result(rc)
        !! Returns sensor synchronisation data.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !!
        use :: dm_sync

        type(db_type),                intent(inout)         :: db       !! Database type.
        type(sync_type), allocatable, intent(out)           :: syncs(:) !! Returned sync data.
        integer(kind=i8),             intent(out), optional :: nsyncs   !! Array size.
        integer(kind=i8),             intent(in),  optional :: limit    !! Max. number of sync data to fetch.

        integer(kind=i8) :: n

        rc = db_select_syncs(db, SYNC_TYPE_SENSOR, SQL_SELECT_NSYNC_SENSORS, SQL_SELECT_SYNC_SENSORS, syncs, n, limit)
        if (present(nsyncs)) nsyncs = n
    end function dm_db_select_sync_sensors

     integer function dm_db_select_sync_target(db, sync) result(rc)
        !! Returns target synchronisation data (oldest not transmitted target).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !!
        use :: dm_sync

        type(db_type),   intent(inout) :: db   !! Database type.
        type(sync_type), intent(out)   :: sync !! Returned sync data.

        rc = db_select_sync(db, SYNC_TYPE_TARGET, SQL_SELECT_SYNC_TARGETS // ' LIMIT 1', sync)
    end function dm_db_select_sync_target

    integer function dm_db_select_sync_targets(db, syncs, nsyncs, limit) result(rc)
        !! Returns sensor synchronisation data.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !!
        use :: dm_sync

        type(db_type),                intent(inout)         :: db       !! Database type.
        type(sync_type), allocatable, intent(out)           :: syncs(:) !! Returned sync data.
        integer(kind=i8),             intent(out), optional :: nsyncs   !! Array size.
        integer(kind=i8),             intent(in),  optional :: limit    !! Max. number of sync data to fetch.

        integer(kind=i8) :: n

        rc = db_select_syncs(db, SYNC_TYPE_TARGET, SQL_SELECT_NSYNC_TARGETS, SQL_SELECT_SYNC_TARGETS, syncs, n, limit)
        if (present(nsyncs)) nsyncs = n
    end function dm_db_select_sync_targets

    integer function dm_db_select_tables(db, tables) result(rc)
        !! Returns an array containing the names of all tables in the given
        !! database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_BOUNDS` if more tables are returned than expected.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        type(db_type),                                  intent(inout) :: db        !! Database type.
        character(len=SQL_TABLE_NAME_LEN), allocatable, intent(out)   :: tables(:) !! Array of tables.

        character(len=:), allocatable :: table
        integer                       :: i, n, stat
        type(db_stmt_type)            :: db_stmt

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_SELECT_TABLES)
            if (dm_is_error(rc)) exit sql_block

            i = 1

            do
                if (dm_is_error(db_step(db_stmt))) exit

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (.not. db_column_is_integer(db_stmt, 0)) exit
                    if (.not. db_column_is_text   (db_stmt, 1)) exit
                end if

                call db_column(db_stmt, 0, n)
                call db_column(db_stmt, 1, table)

                if (.not. allocated(tables)) then
                    rc = E_ALLOC
                    allocate (tables(n), stat=stat)
                    if (stat /= 0) exit
                end if

                rc = E_BOUNDS
                if (i > size(tables)) exit
                tables(i) = table

                rc = E_NONE
                i = i + 1
                if (i > n) exit
            end do
        end block sql_block

        stat = db_finalize(db_stmt)
        if (.not. allocated(tables)) allocate (tables(0))
    end function dm_db_select_tables

    integer function dm_db_select_target(db, target, target_id) result(rc)
        !! Returns target data associated with given target id from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_target

        type(db_type),     intent(inout) :: db        !! Database type.
        type(target_type), intent(out)   :: target    !! Returned target data.
        character(len=*),  intent(in)    :: target_id !! Target id.

        integer             :: stat
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(target_id) == 0) return

        call dm_db_query_add_text(db_query, 'targets.id = ?', target_id)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_TARGETS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_NO_ROWS
            if (dm_is_error(db_step(db_stmt))) exit sql_block

            rc = db_next_row(db_stmt, target)
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
    end function dm_db_select_target

    integer function dm_db_set_application_id(db, id) result(rc)
        !! Set the 32-bit signed big-endian “Application ID” integer located at
        !! offset 68 into the database header.
        !!
        !! Applications that use SQLite as their application file-format should
        !! set the Application ID integer to a unique integer so that utilities
        !! such as _file(1)_ can determine the specific file type rather than
        !! just reporting “SQLite3 Database”. A list of assigned application
        !! IDs can be seen by consulting the `magic.txt` file in the SQLite
        !! source repository.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        character(len=*), parameter :: QUERY = 'PRAGMA application_id = '

        type(db_type), intent(inout) :: db !! Database type.
        integer,       intent(in)    :: id !! Application id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        sql_block: block
            rc = db_prepare(db, db_stmt, QUERY // dm_itoa(id))
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_set_application_id

    integer function dm_db_set_auto_vacuum(db, mode) result(rc)
        !! Auto-vacuuming is only possible if the database stores some
        !! additional information that allows each database page to be traced
        !! backwards to its referrer. Therefore, auto-vacuuming must be turned
        !! on before any tables are created. It is not possible to enable or
        !! disable auto-vacuum after a table has been created.
        !!
        !! In mode `DB_AUTO_VACUUM_INCREMENTAL` the additional information
        !! needed to do auto-vacuuming is stored in the database file but
        !! auto-vacuuming does not occur automatically at each commit as it does
        !! with `DB_AUTO_VACUUM_FULL`. In incremental mode, the separate
        !! incremental_vacuum pragma must be invoked to cause the auto-vacuum to
        !! occur.
        !!
        !! The database connection can be changed between full and incremental
        !! auto-vacuum mode at any time. However, changing from
        !! `DB_AUTO_VACUUM_NONE` to `DB_AUTO_VACUUM_FULL` or
        !! `DB_AUTO_VACUUM_INCREMENTAL` can only occur when the database is new
        !! (no tables have yet been created) or by running the VACUUM command.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if the auto-vacuum mode is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        character(len=*), parameter :: QUERY = 'PRAGMA auto_vacuum = '

        type(db_type), intent(inout) :: db   !! Database type.
        integer,       intent(in)    :: mode !! Database auto vacuum mode.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (mode < DB_AUTO_VACUUM_NONE .or. mode > DB_AUTO_VACUUM_INCREMENTAL) return

        sql_block: block
            select case (mode)
                case (DB_AUTO_VACUUM_NONE);        rc = db_prepare(db, db_stmt, QUERY // '0')
                case (DB_AUTO_VACUUM_FULL);        rc = db_prepare(db, db_stmt, QUERY // '1')
                case (DB_AUTO_VACUUM_INCREMENTAL); rc = db_prepare(db, db_stmt, QUERY // '2')
            end select
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_set_auto_vacuum

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
        if (sqlite3_busy_handler(db%ctx, c_funloc(callback), client_data) /= SQLITE_OK) return
        rc = E_NONE
    end function dm_db_set_busy_callback

    integer function dm_db_set_busy_timeout(db, msec) result(rc)
        !! Sets SQLite busy timeout in msec. Returns `E_DB` on error.
        type(db_type), intent(inout) :: db   !! Database type.
        integer,       intent(in)    :: msec !! Timeout in mseconds.

        rc = E_DB
        if (sqlite3_busy_timeout(db%ctx, msec) /= SQLITE_OK) return
        rc = E_NONE
    end function dm_db_set_busy_timeout

    integer function dm_db_set_foreign_keys(db, enabled) result(rc)
        !! Sets foreign keys constraint.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        character(len=*), parameter :: QUERY = 'PRAGMA foreign_keys = '

        type(db_type), intent(inout) :: db      !! Database type.
        logical,       intent(in)    :: enabled !! Enable foreign keys constraint.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = db_prepare(db, db_stmt, QUERY // dm_btoa(enabled, 'ON', 'OFF'))
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_set_foreign_keys

    integer function dm_db_set_journal_mode(db, mode) result(rc)
        !! Sets journal mode.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if journal mode is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        character(len=*), parameter :: QUERY = 'PRAGMA journal_mode = '

        type(db_type), intent(inout) :: db   !! Database type.
        integer,       intent(in)    :: mode !! Journal mode.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (mode < DB_JOURNAL_OFF .or. mode > DB_JOURNAL_WAL) return

        sql_block: block
            select case (mode)
                case (DB_JOURNAL_OFF);      rc = db_prepare(db, db_stmt, QUERY // 'OFF')
                case (DB_JOURNAL_DELETE);   rc = db_prepare(db, db_stmt, QUERY // 'DELETE')
                case (DB_JOURNAL_TRUNCATE); rc = db_prepare(db, db_stmt, QUERY // 'TRUNCATE')
                case (DB_JOURNAL_PERSIST);  rc = db_prepare(db, db_stmt, QUERY // 'PERSIST')
                case (DB_JOURNAL_MEMORY);   rc = db_prepare(db, db_stmt, QUERY // 'MEMORY')
                case (DB_JOURNAL_WAL);      rc = db_prepare(db, db_stmt, QUERY // 'WAL')
            end select
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_set_journal_mode

    integer function dm_db_set_log_callback(callback, client_data) result(rc)
        !! Sets SQLite error log callback. The dummy argument `client_data` is
        !! passed to the callback routine. The function returns `E_DB` on error.
        procedure(dm_db_log_callback)     :: callback    !! Callback routine.
        type(c_ptr), intent(in), optional :: client_data !! C pointer to client data.

        rc = E_DB
        if (present(client_data)) then
            if (sqlite3_config(SQLITE_CONFIG_LOG, c_funloc(callback), client_data) /= SQLITE_OK) return
        else
            if (sqlite3_config(SQLITE_CONFIG_LOG, c_funloc(callback), c_null_ptr) /= SQLITE_OK) return
        end if
        rc = E_NONE
    end function dm_db_set_log_callback

    integer function dm_db_set_query_only(db, enabled) result(rc)
        !! Sets query-only pragma.
        !!
        !! The SQLite `query_only` pragma prevents data changes on database
        !! files when enabled. When this pragma is enabled, any attempt to
        !! `CREATE`, `DELETE`, `DROP`, `INSERT`, or `UPDATE` will result in an
        !! `E_READ_ONLY` error.
        !!
        !! However, the database is not truly read-only. You can still run a
        !! checkpoint or a COMMIT and the return value of the
        !! `sqlite3_db_readonly()` routine is not affected.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        character(len=*), parameter :: QUERY = 'PRAGMA query_only = '

        type(db_type), intent(inout) :: db      !! Database type.
        logical,       intent(in)    :: enabled !! Enable query-only mode.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = db_prepare(db, db_stmt, QUERY // dm_btoa(enabled, 'ON', 'OFF'))
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_set_query_only

    integer function dm_db_set_schema_version(db, version) result(rc)
        !! Sets database schema version.
        !!
        !! The `user_version` pragma will get or set the value of the
        !! user-version integer at offset 60 in the database header. The
        !! user-version is an integer that is available to applications to use
        !! however they want. SQLite makes no use of the user-version itself.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        character(len=*), parameter :: QUERY = 'PRAGMA user_version = '

        type(db_type), intent(inout) :: db      !! Database type.
        integer,       intent(in)    :: version !! Database user version.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        sql_block: block
            rc = db_prepare(db, db_stmt, QUERY // dm_itoa(version))
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_set_schema_version

    integer function dm_db_set_update_callback(db, callback, client_data) result(rc)
        !! Sets SQLite error log callback. The dummy argument `client_data` is
        !! passed to the callback routine. The function returns `E_DB` on error.
        type(db_type), intent(inout)         :: db          !! Database type.
        procedure(dm_db_update_callback)     :: callback    !! Callback routine.
        type(c_ptr),   intent(in), optional  :: client_data !! C pointer to client data.

        type(c_ptr) :: udp

        rc = E_DB
        if (present(client_data)) then
            udp = sqlite3_update_hook(db%ctx, c_funloc(callback), client_data)
        else
            udp = sqlite3_update_hook(db%ctx, c_funloc(callback), c_null_ptr)
        end if
        rc = E_NONE
    end function dm_db_set_update_callback

    integer function dm_db_shutdown() result(rc)
        !! Finalises SQLite handle. Returns `E_DB` on error.
        rc = E_DB
        if (sqlite3_shutdown() /= SQLITE_OK) return
        rc = E_NONE
    end function dm_db_shutdown

    integer function dm_db_size(db, nbytes) result(rc)
        !! Returns SQLite database size in bytes.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_DB_TYPE` if returned column is unexpected.
        !!
        character(len=*), parameter :: QUERY = &
            'SELECT page_count * page_size FROM pragma_page_count(), pragma_page_size()'

        type(db_type),    intent(inout) :: db     !! Database type.
        integer(kind=i8), intent(out)   :: nbytes !! Database size in bytes.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        nbytes = 0_i8

        sql_block: block
            rc = db_prepare(db, db_stmt, QUERY)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_TYPE
            if (.not. db_column_is_integer(db_stmt, 0)) exit sql_block

            rc = E_NONE
            call db_column(db_stmt, 0, nbytes)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_size

    integer function dm_db_update_node(db, node, validate) result(rc)
        !! Updates the given node in database. The node data is validated by
        !! default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if node is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_node

        type(db_type),   intent(inout)        :: db       !! Database type.
        type(node_type), intent(inout)        :: node     !! Node to update.
        logical,         intent(in), optional :: validate !! Validate node.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate node.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_node_is_valid(node)) return
        end if

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_UPDATE_NODE)
            if (dm_is_error(rc)) exit sql_block

            ! Node id must be last argument!
            rc = db_bind(db_stmt, 1, node%name); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 2, node%meta); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 3, node%x);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 4, node%y);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 5, node%z);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 6, node%lon);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 7, node%lat);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 8, node%alt);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 9, node%id);   if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_update_node

    integer function dm_db_update_sensor(db, sensor, validate) result(rc)
        !! Updates given sensor in database. The sensor data is validated by
        !! default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if sensor is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_sensor

        type(db_type),     intent(inout)        :: db       !! Database type.
        type(sensor_type), intent(inout)        :: sensor   !! Sensor to update.
        logical,           intent(in), optional :: validate !! Validate sensor.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate sensor.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_sensor_is_valid(sensor)) return
        end if

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_UPDATE_SENSOR)
            if (dm_is_error(rc)) exit sql_block

            ! Sensor id must be last argument!
            rc = db_bind(db_stmt,  1, sensor%node_id); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  2, sensor%type);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  3, sensor%name);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  4, sensor%sn);      if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  5, sensor%meta);    if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  6, sensor%x);       if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  7, sensor%y);       if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  8, sensor%z);       if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  9, sensor%lon);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 10, sensor%lat);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 11, sensor%alt);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 12, sensor%id);      if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_update_sensor

    integer function dm_db_update_target(db, target, validate) result(rc)
        !! Updates the given target in database. The target data is validated
        !! by default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if target is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_target

        type(db_type),     intent(inout)        :: db       !! Database type.
        type(target_type), intent(inout)        :: target   !! Target to update.
        logical,           intent(in), optional :: validate !! Validate target.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate target.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_target_is_valid(target)) return
        end if

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_UPDATE_TARGET)
            if (dm_is_error(rc)) exit sql_block

            ! Target id must be last argument!
            rc = db_bind(db_stmt,  1, target%name);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  2, target%meta);  if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  3, target%state); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  4, target%x);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  5, target%y);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  6, target%z);     if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  7, target%lon);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  8, target%lat);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt,  9, target%alt);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 10, target%id);    if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_update_target

    integer function dm_db_vacuum(db, into) result(rc)
        !! Vacuums database schema `main`, or, if `into` is passed, vacuums
        !! it into new database at given path.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_EXIST` if `into` is passed and the file exists.
        !!
        use :: dm_file

        character(len=*), parameter :: QUERY = "VACUUM 'main' "

        type(db_type),    intent(inout)        :: db   !! Database type.
        character(len=*), intent(in), optional :: into !! File path to vacuum database.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        if (present(into)) then
            rc = E_EXIST
            if (dm_file_exists(into)) return
        end if

        sql_block: block
            if (present(into)) then
                rc = db_prepare(db, db_stmt, QUERY // 'INTO ?')
                if (dm_is_error(rc)) exit sql_block

                rc = db_bind(db_stmt, 1, into)
                if (dm_is_error(rc)) exit sql_block
            else
                rc = db_prepare(db, db_stmt, QUERY)
                if (dm_is_error(rc)) exit sql_block
            end if

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function dm_db_vacuum

    integer function dm_db_validate(db) result(rc)
        !! Validates an opened DMPACK database. The application id must match
        !! the constant `DB_APPLICATION_ID`, and the user version must be equal
        !! to `DB_SCHEMA_VERSION`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_ID` if application id is missing or invalid.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !! * `E_DB_VERSION` if the schema version is incompatible.
        !!
        type(db_type), intent(inout) :: db !! Database type.

        integer :: id, schema_version

        rc = dm_db_get_application_id(db, id)
        if (dm_is_error(rc)) return

        rc = E_DB_ID
        if (id /= DB_APPLICATION_ID) return

        rc = dm_db_get_schema_version(db, schema_version)
        if (dm_is_error(rc)) return

        rc = E_DB_VERSION
        if (schema_version /= DB_SCHEMA_VERSION) return

        rc = E_NONE
    end function dm_db_validate

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
    subroutine dm_db_log(err_code, err_msg)
        !! Sends log message to SQLite error log handler. The callback has to
        !! be set through `dm_db_set_log_callback()` initially.
        integer,          intent(in) :: err_code !! Error code.
        character(len=*), intent(in) :: err_msg  !! Error message.

        call sqlite3_log(err_code, err_msg // c_null_char)
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
    integer function db_begin(db, mode) result(rc)
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
            case (DB_TRANS_DEFERRED);  rc = db_exec(db, 'BEGIN')
            case (DB_TRANS_IMMEDIATE); rc = db_exec(db, 'BEGIN IMMEDIATE')
            case (DB_TRANS_EXCLUSIVE); rc = db_exec(db, 'BEGIN EXCLUSIVE')
            case default;              return
        end select

        if (dm_is_error(rc)) rc = E_DB_TRANSACTION
    end function db_begin

    integer function db_bind_double(db_stmt, index, value) result(rc)
        !! Binds 64-bit real value to statement. Returns `E_DB_BIND` on error.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Value index.
        real(kind=r8),      intent(in)    :: value   !! Value.

        integer :: stat

        rc = E_DB_BIND
        stat = sqlite3_bind_double(db_stmt%ctx, index, value)
        if (stat /= SQLITE_OK) return
        rc = E_NONE
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
        if (stat /= SQLITE_OK) return
        rc = E_NONE
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
        if (stat /= SQLITE_OK) return
        rc = E_NONE
    end function db_bind_int64

    integer function db_bind_query(db_stmt, db_query) result(rc)
        !! Binds query parameters to SQLite statement. Returns `E_DB_BIND` on
        !! binding error.
        type(db_stmt_type),  intent(inout) :: db_stmt  !! Database statement type.
        type(db_query_type), intent(inout) :: db_query !! Database query type.

        integer :: i, stat

        stat = SQLITE_OK

        do i = 1, db_query%nparams
            select case (db_query%params(i)%type)
                case (DB_QUERY_TYPE_DOUBLE); stat = sqlite3_bind_double(db_stmt%ctx, i, db_query%params(i)%value_double)
                case (DB_QUERY_TYPE_INT);    stat = sqlite3_bind_int   (db_stmt%ctx, i, db_query%params(i)%value_int)
                case (DB_QUERY_TYPE_INT64);  stat = sqlite3_bind_int64 (db_stmt%ctx, i, db_query%params(i)%value_int64)
                case (DB_QUERY_TYPE_TEXT);   stat = sqlite3_bind_text  (db_stmt%ctx, i, db_query%params(i)%value_text)
                case default;                stat = SQLITE_ERROR
            end select
        end do

        rc = E_DB_BIND
        if (stat /= SQLITE_OK) return

        if (db_query%limit > 0) then
            stat = sqlite3_bind_int64(db_stmt%ctx, i, db_query%limit)
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
        if (stat /= SQLITE_OK) return
        rc = E_NONE
    end function db_bind_text

    integer function db_commit(db) result(rc)
        !! Commits a transaction. Returns `E_DB_EXEC` on error.
        type(db_type), intent(inout) :: db !! Database type.

        rc = db_exec(db, 'COMMIT')
    end function db_commit

    logical function db_column_is_float(db_stmt, index) result(is)
        type(db_stmt_type), intent(inout) :: db_stmt
        integer,            intent(in)    :: index

        is = (sqlite3_column_type(db_stmt%ctx, index) == SQLITE_FLOAT)
    end function db_column_is_float

    logical function db_column_is_integer(db_stmt, index) result(is)
        type(db_stmt_type), intent(inout) :: db_stmt
        integer,            intent(in)    :: index

        is = (sqlite3_column_type(db_stmt%ctx, index) == SQLITE_INTEGER)
    end function db_column_is_integer

    logical function db_column_is_text(db_stmt, index) result(is)
        type(db_stmt_type), intent(inout) :: db_stmt
        integer,            intent(in)    :: index

        is = (sqlite3_column_type(db_stmt%ctx, index) == SQLITE_TEXT)
    end function db_column_is_text

    integer function db_count(db, table, n) result(rc)
        !! Returns number of rows in table `table` in `n`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if the table enumerator is invalid.
        !!
        character(len=*), parameter :: QUERY = 'SELECT COUNT(row_id) FROM '

        type(db_type),    intent(inout) :: db    !! Database type.
        integer,          intent(in)    :: table !! Table type from `dm_sql`.
        integer(kind=i8), intent(out)   :: n     !! Number of rows in table.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        n = 0_i8
        rc = E_INVALID
        if (table < SQL_TABLE_NODES .or. table > SQL_TABLE_LAST) return

        sql_block: block
            rc = db_prepare(db, db_stmt, QUERY // SQL_TABLE_NAMES(table))
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_TYPE
            if (.not. db_column_is_integer(db_stmt, 0)) exit sql_block

            rc = E_NONE
            call db_column(db_stmt, 0, n)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function db_count

    integer function db_delete_receivers(db, observ_id) result(rc)
        !! Deletes all receivers of given observation. This function is not
        !! strictly necessary, as receivers are deleted by a SQL trigger.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: observ_id !! Observation id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_DELETE_RECEIVERS)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function db_delete_receivers

    integer function db_delete_requests(db, observ_id) result(rc)
        !! Deletes all requests of given observation. This function is not
        !! strictly necessary, as receivers are deleted by a SQL trigger.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: observ_id !! Observation id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_DELETE_REQUESTS)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function db_delete_requests

    integer function db_delete_responses(db, observ_id) result(rc)
        !! Deletes all responses of given observation. This function is not
        !! strictly necessary, as receivers are deleted by a SQL trigger.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: observ_id !! Observation id.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_DELETE_OBSERV_RESPONSES)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function db_delete_responses

    integer function db_exec(db, query, err_msg) result(rc)
        !! Executes given query, and returns optional error message if `rc` is
        !! not `E_NONE`. Otherwise, `err_msg` is not allocated. Returns
        !! `E_DB_EXEC` on error
        type(db_type),                 intent(inout)         :: db      !! Database type.
        character(len=*),              intent(in)            :: query   !! SQL query.
        character(len=:), allocatable, intent(out), optional :: err_msg !! Optional error message.

        integer :: stat

        rc = E_DB_EXEC
        stat = sqlite3_exec(db%ctx, query, c_null_funptr, c_null_ptr, err_msg)
        if (stat /= SQLITE_OK) return
        rc = E_NONE
    end function db_exec

    integer function db_finalize(db_stmt) result(rc)
        !! Finalises given database statement. Returns `E_DB_FINALIZE` on
        !! error.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.

        rc = E_NONE
        if (.not. c_associated(db_stmt%ctx)) return
        if (sqlite3_finalize(db_stmt%ctx) /= SQLITE_OK) rc = E_DB_FINALIZE
    end function db_finalize

    logical function db_has(db, table, id) result(has)
        !! Returns `.true.` if id exists in table. Argument `table` must be one
        !! of the following:
        !!
        !! * `SQL_TABLE_LOGS`
        !! * `SQL_TABLE_NODES`
        !! * `SQL_TABLE_OBSERVS`
        !! * `SQL_TABLE_SENSORS`
        !! * `SQL_TABLE_TARGETS`
        !!
        !! This function returns no error code.
        type(db_type),    intent(inout) :: db    !! Database type.
        integer,          intent(in)    :: table !! Enumerator of table to search.
        character(len=*), intent(in)    :: id    !! Record id.

        integer            :: i, rc
        type(db_stmt_type) :: db_stmt

        has = .false.

        sql_block: block
            select case (table)
                case (SQL_TABLE_LOGS);    rc = db_prepare(db, db_stmt, SQL_HAS_LOG)
                case (SQL_TABLE_NODES);   rc = db_prepare(db, db_stmt, SQL_HAS_NODE)
                case (SQL_TABLE_OBSERVS); rc = db_prepare(db, db_stmt, SQL_HAS_OBSERV)
                case (SQL_TABLE_SENSORS); rc = db_prepare(db, db_stmt, SQL_HAS_SENSOR)
                case (SQL_TABLE_TARGETS); rc = db_prepare(db, db_stmt, SQL_HAS_TARGET)
                case default;             return
            end select
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, id)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            if (.not. db_column_is_integer(db_stmt, 0)) exit sql_block

            call db_column(db_stmt, 0, i)
            has = (i == 1)
        end block sql_block

        rc = db_finalize(db_stmt)
    end function db_has

    integer function db_insert_receivers(db, observ_id, receivers) result(rc)
        !! Adds receivers of an observation to database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if receivers array size exceeds maximum.
        !! * `E_DB` if statement reset failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if one of the receivers is invalid.
        !!
        use :: dm_observ

        type(db_type),    intent(inout) :: db           !! Database type.
        character(len=*), intent(in)    :: observ_id    !! Observation id.
        character(len=*), intent(inout) :: receivers(:) !! Array of receivers to insert.

        integer            :: i, n, stat
        type(db_stmt_type) :: db_stmt

        n = size(receivers)

        rc = E_BOUNDS
        if (n > OBSERV_MAX_NRECEIVERS) return

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_INSERT_RECEIVER)
            if (dm_is_error(rc)) exit sql_block

            row_loop: do i = 1, n
                rc = E_INVALID
                if (.not. dm_id_is_valid(receivers(i))) exit row_loop

                rc = db_bind(db_stmt, 1, observ_id);    if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 2, i);            if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 3, receivers(i)); if (dm_is_error(rc)) exit row_loop

                rc = db_step(db_stmt);  if (dm_is_error(rc)) exit row_loop
                rc = db_reset(db_stmt); if (dm_is_error(rc)) exit row_loop
            end do row_loop
        end block sql_block

        stat = db_finalize(db_stmt)
    end function db_insert_receivers

    integer function db_insert_requests(db, observ_id, requests) result(rc)
        !! Adds requests of an observation to database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if requests array size exceeds maximum.
        !! * `E_DB` if statement reset failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        use :: dm_observ
        use :: dm_request

        type(db_type),      intent(inout) :: db          !! Database type.
        character(len=*),   intent(in)    :: observ_id   !! Observation id.
        type(request_type), intent(inout) :: requests(:) !! Array of requests to insert.

        integer            :: i, nreq, stat
        type(db_stmt_type) :: db_stmt

        nreq = size(requests)

        rc = E_BOUNDS
        if (nreq > OBSERV_MAX_NREQUESTS) return

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_INSERT_REQUEST)
            if (dm_is_error(rc)) exit sql_block

            row_loop: do i = 1, nreq
                rc = db_bind(db_stmt,  1, observ_id);              if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt,  2, i);                      if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt,  3, requests(i)%name);       if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt,  4, requests(i)%timestamp);  if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt,  5, requests(i)%request);    if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt,  6, requests(i)%response);   if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt,  7, requests(i)%delimiter);  if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt,  8, requests(i)%pattern);    if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt,  9, requests(i)%delay);      if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 10, requests(i)%error);      if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 11, requests(i)%mode);       if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 12, requests(i)%retries);    if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 13, requests(i)%state);      if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 14, requests(i)%timeout);    if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 15, requests(i)%nresponses); if (dm_is_error(rc)) exit row_loop

                rc = db_step(db_stmt);  if (dm_is_error(rc)) exit row_loop
                rc = db_reset(db_stmt); if (dm_is_error(rc)) exit row_loop
            end do row_loop
        end block sql_block

        stat = db_finalize(db_stmt)
    end function db_insert_requests

    integer function db_insert_responses(db, observ_id, request_idx, responses) result(rc)
        !! Adds responses, all referencing the same observation and request,
        !! to the database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if requests array size exceeds maximum.
        !! * `E_DB` if statement reset failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        use :: dm_observ
        use :: dm_request
        use :: dm_response

        type(db_type),       intent(inout) :: db           !! Database type.
        character(len=*),    intent(in)    :: observ_id    !! Observation id.
        integer,             intent(in)    :: request_idx  !! Request index.
        type(response_type), intent(inout) :: responses(:) !! Array of responses to insert.

        integer            :: i, nres, stat
        type(db_stmt_type) :: db_stmt

        nres = size(responses)

        rc = E_BOUNDS
        if (request_idx < 1 .or. request_idx > OBSERV_MAX_NREQUESTS) return
        if (nres > REQUEST_MAX_NRESPONSES) return

        sql_block: block
            rc = db_prepare(db, db_stmt, SQL_INSERT_RESPONSE)
            if (dm_is_error(rc)) exit sql_block

            row_loop: do i = 1, nres
                rc = db_bind(db_stmt, 1, observ_id);          if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 2, request_idx);        if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 3, i);                  if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 4, responses(i)%name);  if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 5, responses(i)%unit);  if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 6, responses(i)%type);  if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 7, responses(i)%error); if (dm_is_error(rc)) exit row_loop
                rc = db_bind(db_stmt, 8, responses(i)%value); if (dm_is_error(rc)) exit row_loop

                rc = db_step(db_stmt);  if (dm_is_error(rc)) exit row_loop
                rc = db_reset(db_stmt); if (dm_is_error(rc)) exit row_loop
            end do row_loop
        end block sql_block

        stat = db_finalize(db_stmt)
    end function db_insert_responses

    integer function db_insert_sync(db, sync, query) result(rc)
        !! Inserts or replaces given sync data into database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        use :: dm_sync

        type(db_type),    intent(inout) :: db    !! Database type.
        type(sync_type),  intent(inout) :: sync  !! Sync data to insert.
        character(len=*), intent(in)    :: query !! SQL query to perform.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = db_prepare(db, db_stmt, query)
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, 1, sync%id);        if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 2, sync%timestamp); if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 3, sync%code);      if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt, 4, sync%attempts);  if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
        end block sql_block

        stat = db_finalize(db_stmt)
    end function db_insert_sync

    integer function db_next_row_allocatable(db_stmt, string, validate) result(rc)
        !! Reads string from table row and returns it as allocatable character
        !! string. Column types are validated by default. Returns `E_DB_TYPE`
        !! if the validation failed.
        type(db_stmt_type),            intent(inout)        :: db_stmt  !! Database statement type.
        character(len=:), allocatable, intent(out)          :: string   !! Allocatable character string.
        logical,                       intent(in), optional :: validate !! Validate column types.

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text(db_stmt, 0)) then
                string = ''
                return
            end if
        end if

        call db_column(db_stmt, 0, string)

        rc = E_NONE
    end function db_next_row_allocatable

    integer function db_next_row_character(db_stmt, string, nbytes, validate) result(rc)
        !! Reads string from table row. The passed argument `str` must be
        !! allocated! Column types are validated by default. Returns
        !! `E_DB_TYPE` if the validation failed.
        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        character(len=*),   intent(inout)        :: string   !! Character string.
        integer,            intent(out)          :: nbytes   !! Size of string in bytes.
        logical,            intent(in), optional :: validate !! Validate column types.

        nbytes = 0

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text(db_stmt, 0)) then
                string = ''
                return
            end if
        end if

        call db_column(db_stmt, 0, string, nbytes)

        rc = E_NONE
    end function db_next_row_character

    integer function db_next_row_beat(db_stmt, beat, validate) result(rc)
        !! Reads beat data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_beat

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(beat_type),    intent(inout)        :: beat     !! Beat type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text   (db_stmt, 0)) return
            if (.not. db_column_is_text   (db_stmt, 1)) return
            if (.not. db_column_is_text   (db_stmt, 2)) return
            if (.not. db_column_is_text   (db_stmt, 3)) return
            if (.not. db_column_is_text   (db_stmt, 4)) return
            if (.not. db_column_is_integer(db_stmt, 5)) return
            if (.not. db_column_is_integer(db_stmt, 6)) return
            if (.not. db_column_is_integer(db_stmt, 7)) return
        end if

        call db_column(db_stmt, 0, beat%node_id,   n)
        call db_column(db_stmt, 1, beat%address,   n)
        call db_column(db_stmt, 2, beat%client,    n)
        call db_column(db_stmt, 3, beat%time_sent, n)
        call db_column(db_stmt, 4, beat%time_recv, n)
        call db_column(db_stmt, 5, beat%error)
        call db_column(db_stmt, 6, beat%interval)
        call db_column(db_stmt, 7, beat%uptime)

        rc = E_NONE
    end function db_next_row_beat

    integer function db_next_row_data_point(db_stmt, dp, validate) result(rc)
        !! Reads observation data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_dp

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(dp_type),      intent(inout)        :: dp       !! Data point type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text (db_stmt, 0)) return
            if (.not. db_column_is_float(db_stmt, 1)) return
        end if

        call db_column(db_stmt, 0, dp%x, n)
        call db_column(db_stmt, 1, dp%y)

        rc = E_NONE
    end function db_next_row_data_point

    integer function db_next_row_log(db_stmt, log, validate) result(rc)
        !! Reads log data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_log

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(log_type),     intent(inout)        :: log      !! Log type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text   (db_stmt, 0)) return
            if (.not. db_column_is_integer(db_stmt, 1)) return
            if (.not. db_column_is_integer(db_stmt, 2)) return
            if (.not. db_column_is_text   (db_stmt, 3)) return
            if (.not. db_column_is_text   (db_stmt, 4)) return
            if (.not. db_column_is_text   (db_stmt, 5)) return
            if (.not. db_column_is_text   (db_stmt, 6)) return
            if (.not. db_column_is_text   (db_stmt, 7)) return
            if (.not. db_column_is_text   (db_stmt, 8)) return
            if (.not. db_column_is_text   (db_stmt, 9)) return
        end if

        call db_column(db_stmt, 0, log%id,        n)
        call db_column(db_stmt, 1, log%level)
        call db_column(db_stmt, 2, log%error)
        call db_column(db_stmt, 3, log%timestamp, n)
        call db_column(db_stmt, 4, log%node_id,   n)
        call db_column(db_stmt, 5, log%sensor_id, n)
        call db_column(db_stmt, 6, log%target_id, n)
        call db_column(db_stmt, 7, log%observ_id, n)
        call db_column(db_stmt, 8, log%source,    n)
        call db_column(db_stmt, 9, log%message,   n)

        rc = E_NONE
    end function db_next_row_log

    integer function db_next_row_node(db_stmt, node, validate) result(rc)
        !! Reads node data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_node

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(node_type),    intent(inout)        :: node     !! Node type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text (db_stmt, 0)) return
            if (.not. db_column_is_text (db_stmt, 1)) return
            if (.not. db_column_is_text (db_stmt, 2)) return
            if (.not. db_column_is_float(db_stmt, 3)) return
            if (.not. db_column_is_float(db_stmt, 4)) return
            if (.not. db_column_is_float(db_stmt, 5)) return
            if (.not. db_column_is_float(db_stmt, 6)) return
            if (.not. db_column_is_float(db_stmt, 7)) return
            if (.not. db_column_is_float(db_stmt, 8)) return
        end if

        call db_column(db_stmt, 0, node%id,   n)
        call db_column(db_stmt, 1, node%name, n)
        call db_column(db_stmt, 2, node%meta, n)
        call db_column(db_stmt, 3, node%x)
        call db_column(db_stmt, 4, node%y)
        call db_column(db_stmt, 5, node%z)
        call db_column(db_stmt, 6, node%lon)
        call db_column(db_stmt, 7, node%lat)
        call db_column(db_stmt, 8, node%alt)

        rc = E_NONE
    end function db_next_row_node

    integer function db_next_row_observ(db_stmt, observ, validate) result(rc)
        !! Reads observation data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_observ

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(observ_type),  intent(inout)        :: observ   !! Observation type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text   (db_stmt,  0)) return
            if (.not. db_column_is_text   (db_stmt,  1)) return
            if (.not. db_column_is_text   (db_stmt,  2)) return
            if (.not. db_column_is_text   (db_stmt,  3)) return
            if (.not. db_column_is_text   (db_stmt,  4)) return
            if (.not. db_column_is_text   (db_stmt,  5)) return
            if (.not. db_column_is_text   (db_stmt,  6)) return
            if (.not. db_column_is_text   (db_stmt,  7)) return
            if (.not. db_column_is_integer(db_stmt,  8)) return
            if (.not. db_column_is_integer(db_stmt,  9)) return
            if (.not. db_column_is_integer(db_stmt, 10)) return
            if (.not. db_column_is_integer(db_stmt, 11)) return
            if (.not. db_column_is_integer(db_stmt, 12)) return
        end if

        call db_column(db_stmt,  0, observ%id,        n)
        call db_column(db_stmt,  1, observ%node_id,   n)
        call db_column(db_stmt,  2, observ%sensor_id, n)
        call db_column(db_stmt,  3, observ%target_id, n)
        call db_column(db_stmt,  4, observ%name,      n)
        call db_column(db_stmt,  5, observ%timestamp, n)
        call db_column(db_stmt,  6, observ%source,    n)
        call db_column(db_stmt,  7, observ%device,    n)
        call db_column(db_stmt,  8, observ%priority)
        call db_column(db_stmt,  9, observ%error)
        call db_column(db_stmt, 10, observ%next)
        call db_column(db_stmt, 11, observ%nreceivers)
        call db_column(db_stmt, 12, observ%nrequests)

        rc = E_NONE
    end function db_next_row_observ

    integer function db_next_row_observ_view(db_stmt, view, validate) result(rc)
        !! Reads observation data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_observ

        type(db_stmt_type),     intent(inout)        :: db_stmt  !! Database statement type.
        type(observ_view_type), intent(inout)        :: view     !! Observation view type.
        logical,                intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text   (db_stmt,  0)) return
            if (.not. db_column_is_text   (db_stmt,  1)) return
            if (.not. db_column_is_text   (db_stmt,  2)) return
            if (.not. db_column_is_text   (db_stmt,  3)) return
            if (.not. db_column_is_text   (db_stmt,  4)) return
            if (.not. db_column_is_integer(db_stmt,  5)) return
            if (.not. db_column_is_text   (db_stmt,  6)) return
            if (.not. db_column_is_text   (db_stmt,  7)) return
            if (.not. db_column_is_integer(db_stmt,  8)) return
            if (.not. db_column_is_text   (db_stmt,  9)) return
            if (.not. db_column_is_text   (db_stmt, 10)) return
            if (.not. db_column_is_integer(db_stmt, 11)) return
            if (.not. db_column_is_integer(db_stmt, 12)) return
            if (.not. db_column_is_float  (db_stmt, 13)) return
        end if

        call db_column(db_stmt,  0, view%observ_id,         n)
        call db_column(db_stmt,  1, view%node_id,           n)
        call db_column(db_stmt,  2, view%sensor_id,         n)
        call db_column(db_stmt,  3, view%target_id,         n)
        call db_column(db_stmt,  4, view%observ_name,       n)
        call db_column(db_stmt,  5, view%observ_error)
        call db_column(db_stmt,  6, view%request_name,      n)
        call db_column(db_stmt,  7, view%request_timestamp, n)
        call db_column(db_stmt,  8, view%request_error)
        call db_column(db_stmt,  9, view%response_name,     n)
        call db_column(db_stmt, 10, view%response_unit,     n)
        call db_column(db_stmt, 11, view%response_type)
        call db_column(db_stmt, 12, view%response_error)
        call db_column(db_stmt, 13, view%response_value)

        rc = E_NONE
    end function db_next_row_observ_view

    integer function db_next_row_sensor(db_stmt, sensor, validate) result(rc)
        !! Reads sensor data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_sensor

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(sensor_type),  intent(inout)        :: sensor   !! Sensor type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text   (db_stmt,  0)) return
            if (.not. db_column_is_text   (db_stmt,  1)) return
            if (.not. db_column_is_integer(db_stmt,  2)) return
            if (.not. db_column_is_text   (db_stmt,  3)) return
            if (.not. db_column_is_text   (db_stmt,  4)) return
            if (.not. db_column_is_text   (db_stmt,  5)) return
            if (.not. db_column_is_float  (db_stmt,  6)) return
            if (.not. db_column_is_float  (db_stmt,  7)) return
            if (.not. db_column_is_float  (db_stmt,  8)) return
            if (.not. db_column_is_float  (db_stmt,  9)) return
            if (.not. db_column_is_float  (db_stmt, 10)) return
            if (.not. db_column_is_float  (db_stmt, 11)) return
        end if

        call db_column(db_stmt,  0, sensor%id,      n)
        call db_column(db_stmt,  1, sensor%node_id, n)
        call db_column(db_stmt,  2, sensor%type)
        call db_column(db_stmt,  3, sensor%name,    n)
        call db_column(db_stmt,  4, sensor%sn,      n)
        call db_column(db_stmt,  5, sensor%meta,    n)
        call db_column(db_stmt,  6, sensor%x)
        call db_column(db_stmt,  7, sensor%y)
        call db_column(db_stmt,  8, sensor%z)
        call db_column(db_stmt,  9, sensor%lon)
        call db_column(db_stmt, 10, sensor%lat)
        call db_column(db_stmt, 11, sensor%alt)

        rc = E_NONE
    end function db_next_row_sensor

    integer function db_next_row_string(db_stmt, string, validate) result(rc)
        !! Reads string from table row and returns it as derived type
        !! `string_type`. Column types are validated by default. Returns
        !! `E_DB_TYPE` if the validation failed.
        use :: dm_string, only: string_type

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(string_type),  intent(out)          :: string   !! String type.
        logical,            intent(in), optional :: validate !! Validate column types.

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text(db_stmt, 0)) then
                string%data = ''
                return
            end if
        end if

        call db_column(db_stmt, 0, string%data)

        rc = E_NONE
    end function db_next_row_string

    integer function db_next_row_sync(db_stmt, sync) result(rc)
        !! Reads sync data from table row. Returns `E_DB_TYPE` on error.
        use :: dm_sync

        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        type(sync_type),    intent(inout) :: sync    !! Sync type.

        integer :: n

        rc = E_DB_TYPE
        if (.not. db_column_is_text(db_stmt, 0)) return

        call db_column(db_stmt, 0, sync%id, n)

        if (db_column_is_text(db_stmt, 1)) then
            call db_column(db_stmt, 1, sync%timestamp, n)
        else
            sync%timestamp = TIME_DEFAULT
        end if

        if (db_column_is_integer(db_stmt, 2)) then
            call db_column(db_stmt, 2, sync%code)
        else
            sync%code = 0
        end if

        if (db_column_is_integer(db_stmt, 3)) then
            call db_column(db_stmt, 3, sync%attempts)
        else
            sync%attempts = 0
        end if

        rc = E_NONE
    end function db_next_row_sync

    integer function db_next_row_target(db_stmt, target, validate) result(rc)
        !! Reads target data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_target

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(target_type),  intent(inout)        :: target   !! Target type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. db_column_is_text   (db_stmt, 0)) return
            if (.not. db_column_is_text   (db_stmt, 1)) return
            if (.not. db_column_is_text   (db_stmt, 2)) return
            if (.not. db_column_is_integer(db_stmt, 3)) return
            if (.not. db_column_is_float  (db_stmt, 4)) return
            if (.not. db_column_is_float  (db_stmt, 5)) return
            if (.not. db_column_is_float  (db_stmt, 6)) return
            if (.not. db_column_is_float  (db_stmt, 7)) return
            if (.not. db_column_is_float  (db_stmt, 8)) return
            if (.not. db_column_is_float  (db_stmt, 9)) return
        end if

        call db_column(db_stmt, 0, target%id,   n)
        call db_column(db_stmt, 1, target%name, n)
        call db_column(db_stmt, 2, target%meta, n)
        call db_column(db_stmt, 3, target%state)
        call db_column(db_stmt, 4, target%x)
        call db_column(db_stmt, 5, target%y)
        call db_column(db_stmt, 6, target%z)
        call db_column(db_stmt, 7, target%lon)
        call db_column(db_stmt, 8, target%lat)
        call db_column(db_stmt, 9, target%alt)

        rc = E_NONE
    end function db_next_row_target

    integer function db_prepare(db, db_stmt, sql) result(rc)
        !! Prepares database statement. Returns `E_DB_PREPARE` on error.
        type(db_type),      intent(inout) :: db      !! Database type.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        character(len=*),   intent(in)    :: sql     !! SQL query.

        rc = E_DB_PREPARE
        if (sqlite3_prepare_v2(db%ctx, sql, db_stmt%ctx) /= SQLITE_OK) return
        rc = E_NONE
    end function db_prepare

    integer function db_release(db, name) result(rc)
        !! Jumps back to a save point. Returns `E_DB_EXEC` on error.
        type(db_type),    intent(inout) :: db   !! Database type.
        character(len=*), intent(in)    :: name !! Save point name.

        rc = db_exec(db, 'RELEASE "' // trim(name) // '"')
    end function db_release

    integer function db_reset(db_stmt) result(rc)
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.

        rc = E_DB
        if (sqlite3_reset(db_stmt%ctx) /= SQLITE_OK) return
        rc = E_NONE
    end function db_reset

    integer function db_rollback(db, name) result(rc)
        !! Rolls a transaction back, optionally to save point `name`. The
        !! function returns `E_DB_ROLLBACK` is the rollback failed.
        type(db_type),    intent(inout)        :: db   !! Database type.
        character(len=*), intent(in), optional :: name !! Save point name.

        if (present(name)) then
            rc = db_exec(db, 'ROLLBACK TO "' // trim(name) // '"')
        else
            rc = db_exec(db, 'ROLLBACK')
        end if

        if (dm_is_error(rc)) rc = E_DB_ROLLBACK
    end function db_rollback

    integer function db_save_point(db, name) result(rc)
        !! Creates a save point `name`. Returns `E_DB_EXEC` on error.
        type(db_type),    intent(inout) :: db   !! Database type.
        character(len=*), intent(in)    :: name !! Save point name.

        rc = db_exec(db, 'SAVEPOINT "' // trim(name) // '"')
    end function db_save_point

    integer function db_select_beats_array(db, beats, limit, nbeats) result(rc)
        !! Returns heatbeats from database in array `beats`. An optional limit
        !! may be passed in `limit`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_beat

        type(db_type),                intent(inout)         :: db       !! Database type.
        type(beat_type), allocatable, intent(out)           :: beats(:) !! Returned beat types.
        integer(kind=i8),             intent(in),  optional :: limit    !! Max. number of beats.
        integer(kind=i8),             intent(out), optional :: nbeats   !! Total number of beats in database.

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nbeats)) nbeats = 0_i8

        sql_block: block
            rc = db_count(db, SQL_TABLE_BEATS, n)
            if (dm_is_error(rc)) exit sql_block

            if (present(nbeats)) nbeats = n
            if (present(limit))  n      = min(n, limit)

            rc = E_ALLOC
            allocate (beats(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'node_id', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_BEATS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, beats(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(beats)) allocate (beats(0))
    end function db_select_beats_array

    integer function db_select_beats_iter(db, db_stmt, beat, limit) result(rc)
        !! Iterator function that returns heatbeats from database in `beat`. An
        !! optional limit may be passed in `limit`. The statement `db_stmt`
        !! must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no more rows are available.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_beat

        type(db_type),      intent(inout)        :: db      !! Database type.
        type(db_stmt_type), intent(inout)        :: db_stmt !! Database statement type.
        type(beat_type),    intent(out)          :: beat    !! Returned beat type.
        integer(kind=i8),   intent(in), optional :: limit   !! Max. number of beats.

        type(db_query_type) :: db_query

        if (.not. dm_db_stmt_is_prepared(db_stmt)) then
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_BEATS))
            if (dm_is_error(rc)) return

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = E_DB_NO_ROWS
        if (dm_is_error(db_step(db_stmt))) return

        rc = db_next_row(db_stmt, beat)
    end function db_select_beats_iter

    integer function db_select_data_points_array(db, dps, node_id, sensor_id, target_id, response_name, &
                                                 from, to, error, limit, npoints) result(rc)
        !! Returns data points from observations database in `dps`. This
        !! function selects only responses of error `E_NONE`, unless argument
        !! `error` is passed, then only of the given error code.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_dp

        type(db_type),              intent(inout)         :: db            !! Database type.
        type(dp_type), allocatable, intent(out)           :: dps(:)        !! Returned data points.
        character(len=*),           intent(in)            :: node_id       !! Node id.
        character(len=*),           intent(in)            :: sensor_id     !! Sensor id.
        character(len=*),           intent(in)            :: target_id     !! Target id.
        character(len=*),           intent(in)            :: response_name !! Response name.
        character(len=*),           intent(in)            :: from          !! Beginning of time span.
        character(len=*),           intent(in)            :: to            !! End of time span.
        integer,                    intent(in),  optional :: error         !! Response error code.
        integer(kind=i8),           intent(in),  optional :: limit         !! Max. number of data points.
        integer(kind=i8),           intent(out), optional :: npoints       !! Number of data points.

        integer             :: error_, stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        error_ = dm_present(error, E_NONE)
        if (present(npoints)) npoints = 0_i8

        call dm_db_query_add_text(db_query, 'nodes.id = ?',            node_id)
        call dm_db_query_add_text(db_query, 'sensors.id = ?',          sensor_id)
        call dm_db_query_add_text(db_query, 'targets.id = ?',          target_id)
        call dm_db_query_add_text(db_query, 'responses.name = ?',      response_name)
        call dm_db_query_add_int (db_query, 'responses.error = ?',     error_)
        call dm_db_query_add_text(db_query, 'requests.timestamp >= ?', from)
        call dm_db_query_add_text(db_query, 'requests.timestamp < ?',  to)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NDATA_POINTS))
            if (dm_is_error(rc)) return

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call db_column(db_stmt, 0, n)

            rc = db_finalize(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            if (present(npoints)) npoints = n
            if (present(limit))   n       = min(n, limit)

            rc = E_ALLOC
            allocate (dps(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'requests.timestamp', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_DATA_POINTS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, dps(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(dps)) allocate (dps(0))
    end function db_select_data_points_array

    integer function db_select_data_points_iter(db, db_stmt, dp, node_id, sensor_id, target_id, response_name, &
                                                from, to, error, limit) result(rc)
        !! Iterator function that returns data points from observations
        !! database in `dp`. This function selects only responses of error
        !! `E_NONE`, unless argument `error` is passed, then only of the given
        !! error code. The statement `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no more rows are available.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_dp

        type(db_type),      intent(inout)        :: db            !! Database type.
        type(db_stmt_type), intent(inout)        :: db_stmt       !! Database statement type.
        type(dp_type),      intent(out)          :: dp            !! Returned data point.
        character(len=*),   intent(in)           :: node_id       !! Node id.
        character(len=*),   intent(in)           :: sensor_id     !! Sensor id.
        character(len=*),   intent(in)           :: target_id     !! Target id.
        character(len=*),   intent(in)           :: response_name !! Response name.
        character(len=*),   intent(in)           :: from          !! Beginning of time span.
        character(len=*),   intent(in)           :: to            !! End of time span.
        integer,            intent(in), optional :: error         !! Response error code.
        integer(kind=i8),   intent(in), optional :: limit         !! Max. number of data points.

        integer             :: error_
        type(db_query_type) :: db_query

        error_ = dm_present(error, E_NONE)

        if (.not. dm_db_stmt_is_prepared(db_stmt)) then
            call dm_db_query_add_text(db_query, 'nodes.id = ?',            node_id)
            call dm_db_query_add_text(db_query, 'sensors.id = ?',          sensor_id)
            call dm_db_query_add_text(db_query, 'targets.id = ?',          target_id)
            call dm_db_query_add_text(db_query, 'responses.name = ?',      response_name)
            call dm_db_query_add_int (db_query, 'responses.error = ?',     error_)
            call dm_db_query_add_text(db_query, 'requests.timestamp >= ?', from)
            call dm_db_query_add_text(db_query, 'requests.timestamp < ?',  to)

            call dm_db_query_set_order(db_query, 'requests.timestamp', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_DATA_POINTS))
            if (dm_is_error(rc)) return

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = E_DB_NO_ROWS
        if (dm_is_error(db_step(db_stmt))) return

        rc = db_next_row(db_stmt, dp)
    end function db_select_data_points_iter

    integer function db_select_json_beats_array(db, strings, limit, nbeats) result(rc)
        !! Returns beats in JSON format in allocatable string type array
        !! `strings`.
        !!
        !! If no beats have been found, the array will be empty, and the
        !! function returns `E_DB_NO_ROWS`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_string, only: string_type

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(string_type), allocatable, intent(out)           :: strings(:) !! Returned JSON array.
        integer(kind=i8),               intent(in),  optional :: limit      !! Max. number of beats.
        integer(kind=i8),               intent(out), optional :: nbeats     !! Number of beats.

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nbeats)) nbeats = 0_i8

        sql_block: block
            rc = db_count(db, SQL_TABLE_BEATS, n)
            if (dm_is_error(rc)) exit sql_block

            if (present(nbeats)) nbeats = n
            if (present(limit))  n      = min(n, limit)

            rc = E_ALLOC
            allocate (strings(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_JSON_BEATS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(strings)) allocate (strings(0))
    end function db_select_json_beats_array

    integer function db_select_json_beats_iter(db, db_stmt, json, limit) result(rc)
        !! Iterator function that returns beats in JSON format in allocatable
        !! string `json`. The statement `db_stmt` must be finalised once
        !! finished.
        !!
        !! If no beats have been found, the string will be empty, and the
        !! function returns `E_DB_NO_ROWS`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        type(db_type),                 intent(inout)        :: db      !! Database type.
        type(db_stmt_type),            intent(inout)        :: db_stmt !! Database statement type.
        character(len=:), allocatable, intent(out)          :: json    !! Returned JSON.
        integer(kind=i8),              intent(in), optional :: limit   !! Max. number of beats.

        type(db_query_type) :: db_query

        if (.not. dm_db_stmt_is_prepared(db_stmt)) then
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_JSON_BEATS))
            if (dm_is_error(rc)) return

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = E_DB_NO_ROWS
        if (dm_is_error(db_step(db_stmt))) return

        rc = db_next_row(db_stmt, json)
    end function db_select_json_beats_iter

    integer function db_select_json_logs_array(db, strings, node_id, sensor_id, target_id, source, from, to, &
                                               min_level, max_level, error, desc, limit, nlogs) result(rc)
        !! Returns logs in JSON format in allocatable string type array
        !! `strings`.
        !!
        !! If no logs have been found, the array will be empty, and the
        !! function returns `E_DB_NO_ROWS`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_string, only: string_type

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(string_type), allocatable, intent(out)           :: strings(:) !! Returned JSON array.
        character(len=*),               intent(in),  optional :: node_id    !! Node id.
        character(len=*),               intent(in),  optional :: sensor_id  !! Sensor id.
        character(len=*),               intent(in),  optional :: target_id  !! Target id.
        character(len=*),               intent(in),  optional :: source     !! Source name.
        character(len=*),               intent(in),  optional :: from       !! Begin of time range.
        character(len=*),               intent(in),  optional :: to         !! End of time range.
        integer,                        intent(in),  optional :: min_level  !! Minimum log level.
        integer,                        intent(in),  optional :: max_level  !! Maximum log level.
        integer,                        intent(in),  optional :: error      !! Error code.
        logical,                        intent(in),  optional :: desc       !! Descending order.
        integer(kind=i8),               intent(in),  optional :: limit      !! Max. numbers of logs.
        integer(kind=i8),               intent(out), optional :: nlogs      !! Number of logs.

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nlogs)) nlogs = 0_i8

        call dm_db_query_add_int (db_query, 'level >= ?',     min_level)
        call dm_db_query_add_int (db_query, 'level <= ?',     max_level)
        call dm_db_query_add_int (db_query, 'error = ?',      error)
        call dm_db_query_add_text(db_query, 'timestamp >= ?', from)
        call dm_db_query_add_text(db_query, 'timestamp < ?',  to)
        call dm_db_query_add_text(db_query, 'node_id = ?',    node_id)
        call dm_db_query_add_text(db_query, 'sensor_id = ?',  sensor_id)
        call dm_db_query_add_text(db_query, 'target_id = ?',  target_id)
        call dm_db_query_add_text(db_query, 'source = ?',     source)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NLOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call db_column(db_stmt, 0, n)

            rc = db_finalize(db_stmt)
            if (dm_is_error(rc)) return

            if (present(nlogs)) nlogs = n
            if (present(limit)) n     = min(n, limit)

            rc = E_ALLOC
            allocate (strings(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'timestamp', desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_JSON_LOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(strings)) allocate (strings(0))
    end function db_select_json_logs_array

    integer function db_select_json_logs_iter(db, db_stmt, json, node_id, sensor_id, target_id, source, &
                                              from, to, min_level, max_level, error, desc, limit) result(rc)
        !! Iterator function that returns logs in JSON format in allocatable
        !! character `json`. The statement `db_stmt` must be finalised once
        !! finished.
        !!
        !! If no logs have been found, the string will be empty, and the
        !! function returns `E_DB_NO_ROWS`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        type(db_type),                 intent(inout)        :: db        !! Database type.
        type(db_stmt_type),            intent(inout)        :: db_stmt   !! Database statement type.
        character(len=:), allocatable, intent(out)          :: json      !! Returned JSON.
        character(len=*),              intent(in), optional :: node_id   !! Node id.
        character(len=*),              intent(in), optional :: sensor_id !! Sensor id.
        character(len=*),              intent(in), optional :: target_id !! Target id.
        character(len=*),              intent(in), optional :: source    !! Source name.
        character(len=*),              intent(in), optional :: from      !! Begin of time range.
        character(len=*),              intent(in), optional :: to        !! End of time range.
        integer,                       intent(in), optional :: min_level !! Minimum log level.
        integer,                       intent(in), optional :: max_level !! Maximum log level.
        integer,                       intent(in), optional :: error     !! Error code.
        logical,                       intent(in), optional :: desc      !! Descending order.
        integer(kind=i8),              intent(in), optional :: limit     !! Max. numbers of logs.

        type(db_query_type) :: db_query

        if (.not. dm_db_stmt_is_prepared(db_stmt)) then
            call dm_db_query_add_int (db_query, 'level >= ?',     min_level)
            call dm_db_query_add_int (db_query, 'level <= ?',     max_level)
            call dm_db_query_add_int (db_query, 'error = ?',      error)
            call dm_db_query_add_text(db_query, 'timestamp >= ?', from)
            call dm_db_query_add_text(db_query, 'timestamp < ?',  to)
            call dm_db_query_add_text(db_query, 'node_id = ?',    node_id)
            call dm_db_query_add_text(db_query, 'sensor_id = ?',  sensor_id)
            call dm_db_query_add_text(db_query, 'target_id = ?',  target_id)
            call dm_db_query_add_text(db_query, 'source = ?',     source)

            call dm_db_query_set_order(db_query, 'timestamp', desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_JSON_LOGS))
            if (dm_is_error(rc)) return

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = E_DB_NO_ROWS
        if (dm_is_error(db_step(db_stmt))) return

        rc = db_next_row(db_stmt, json)
    end function db_select_json_logs_iter

    integer function db_select_json_nodes_array(db, strings, limit, nnodes) result(rc)
        !! Returns nodes in JSON format in allocatable string type array
        !! `strings`.
        !!
        !! If no nodes have been found, the array will be empty, and the
        !! function returns `E_DB_NO_ROWS`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_string, only: string_type

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(string_type), allocatable, intent(out)           :: strings(:) !! Returned JSON array.
        integer(kind=i8),               intent(in),  optional :: limit      !! Max. number of nodes.
        integer(kind=i8),               intent(out), optional :: nnodes     !! Number of nodes.

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nnodes)) nnodes = 0_i8

        sql_block: block
            rc = db_count(db, SQL_TABLE_NODES, n)
            if (dm_is_error(rc)) exit sql_block

            if (present(nnodes)) nnodes = n
            if (present(limit))  n      = min(n, limit)

            rc = E_ALLOC
            allocate (strings(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'nodes.row_id', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_JSON_NODES))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(strings)) allocate (strings(0))
    end function db_select_json_nodes_array

    integer function db_select_json_nodes_iter(db, db_stmt, json, limit) result(rc)
        !! Iterator function that returns nodes in JSON format in allocatable
        !! string `json`. The statement `db_stmt` must be finalised once
        !! finished.
        !!
        !! If no nodes have been found, the string will be empty, and the
        !! function returns `E_DB_NO_ROWS`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        type(db_type),                 intent(inout)        :: db      !! Database type.
        type(db_stmt_type),            intent(inout)        :: db_stmt !! Database statement type.
        character(len=:), allocatable, intent(out)          :: json    !! Returned JSON.
        integer(kind=i8),              intent(in), optional :: limit   !! Max. number of nodes.

        type(db_query_type) :: db_query

        if (.not. dm_db_stmt_is_prepared(db_stmt)) then
            call dm_db_query_set_order(db_query, 'nodes.row_id', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_JSON_NODES))
            if (dm_is_error(rc)) return

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = E_DB_NO_ROWS
        if (dm_is_error(db_step(db_stmt))) return

        rc = db_next_row(db_stmt, json)
    end function db_select_json_nodes_iter

    integer function db_select_logs_array(db, logs, node_id, sensor_id, target_id, observ_id, source, from, to, &
                                          min_level, max_level, error, desc, limit, nlogs) result(rc)
        !! Returns logs in allocatable array `logs`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_log

        type(db_type),               intent(inout)         :: db        !! Database type.
        type(log_type), allocatable, intent(out)           :: logs(:)   !! Returned log data array.
        character(len=*),            intent(in),  optional :: node_id   !! Node id.
        character(len=*),            intent(in),  optional :: sensor_id !! Sensor id.
        character(len=*),            intent(in),  optional :: target_id !! Target id.
        character(len=*),            intent(in),  optional :: observ_id !! Observation id.
        character(len=*),            intent(in),  optional :: source    !! Source name.
        character(len=*),            intent(in),  optional :: from      !! Begin of time range.
        character(len=*),            intent(in),  optional :: to        !! End of time range.
        integer,                     intent(in),  optional :: min_level !! Minimum log level.
        integer,                     intent(in),  optional :: max_level !! Maximum log level.
        integer,                     intent(in),  optional :: error     !! Error code.
        logical,                     intent(in),  optional :: desc      !! Descending order.
        integer(kind=i8),            intent(in),  optional :: limit     !! Max. numbers of logs.
        integer(kind=i8),            intent(out), optional :: nlogs     !! Total number of logs.

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nlogs)) nlogs = 0_i8

        call dm_db_query_add_int (db_query, 'level >= ?',     min_level)
        call dm_db_query_add_int (db_query, 'level <= ?',     max_level)
        call dm_db_query_add_int (db_query, 'error = ?',      error)
        call dm_db_query_add_text(db_query, 'timestamp >= ?', from)
        call dm_db_query_add_text(db_query, 'timestamp < ?',  to)
        call dm_db_query_add_text(db_query, 'node_id = ?',    node_id)
        call dm_db_query_add_text(db_query, 'sensor_id = ?',  sensor_id)
        call dm_db_query_add_text(db_query, 'target_id = ?',  target_id)
        call dm_db_query_add_text(db_query, 'observ_id = ?',  observ_id)
        call dm_db_query_add_text(db_query, 'source = ?',     source)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NLOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call db_column(db_stmt, 0, n)

            rc = db_finalize(db_stmt)
            if (dm_is_error(rc)) return

            if (present(nlogs)) nlogs = n
            if (present(limit)) n     = min(n, limit)

            rc = E_ALLOC
            allocate (logs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'timestamp', desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_LOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, logs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(logs)) allocate (logs(0))
    end function db_select_logs_array

    integer function db_select_logs_iter(db, db_stmt, log, node_id, sensor_id, target_id, observ_id, source, from, to, &
                                         min_level, max_level, error, desc, limit) result(rc)
        !! Iterator function that returns logs in `logs`. The statement
        !! `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_log

        type(db_type),      intent(inout)        :: db        !! Database type.
        type(db_stmt_type), intent(inout)        :: db_stmt   !! Database statement type.
        type(log_type),     intent(out)          :: log       !! Returned log type.
        character(len=*),   intent(in), optional :: node_id   !! Node id.
        character(len=*),   intent(in), optional :: sensor_id !! Sensor id.
        character(len=*),   intent(in), optional :: target_id !! Target id.
        character(len=*),   intent(in), optional :: observ_id !! Observation id.
        character(len=*),   intent(in), optional :: source    !! Source name.
        character(len=*),   intent(in), optional :: from      !! Begin of time range.
        character(len=*),   intent(in), optional :: to        !! End of time range.
        integer,            intent(in), optional :: min_level !! Minimum log level.
        integer,            intent(in), optional :: max_level !! Maximum log level.
        integer,            intent(in), optional :: error     !! Error code.
        logical,            intent(in), optional :: desc      !! Descending order.
        integer(kind=i8),   intent(in), optional :: limit     !! Max. numbers of logs.

        type(db_query_type) :: db_query

        if (.not. dm_db_stmt_is_prepared(db_stmt)) then
            call dm_db_query_add_int (db_query, 'level >= ?',     min_level)
            call dm_db_query_add_int (db_query, 'level <= ?',     max_level)
            call dm_db_query_add_int (db_query, 'error = ?',      error)
            call dm_db_query_add_text(db_query, 'timestamp >= ?', from)
            call dm_db_query_add_text(db_query, 'timestamp < ?',  to)
            call dm_db_query_add_text(db_query, 'node_id = ?',    node_id)
            call dm_db_query_add_text(db_query, 'sensor_id = ?',  sensor_id)
            call dm_db_query_add_text(db_query, 'target_id = ?',  target_id)
            call dm_db_query_add_text(db_query, 'observ_id = ?',  observ_id)
            call dm_db_query_add_text(db_query, 'source = ?',     source)

            call dm_db_query_set_order(db_query, 'timestamp', desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_LOGS))
            if (dm_is_error(rc)) return

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = E_DB_NO_ROWS
        if (dm_is_error(db_step(db_stmt))) return

        rc = db_next_row(db_stmt, log)
    end function db_select_logs_iter

    integer function db_select_nodes_array(db, nodes, nnodes) result(rc)
        !! Returns all sensor nodes in allocatable array `nodes`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_node

        type(db_type),                intent(inout)         :: db       !! Database type.
        type(node_type), allocatable, intent(out)           :: nodes(:) !! Returned node data array.
        integer(kind=i8),             intent(out), optional :: nnodes   !! Number of nodes.

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nnodes)) nnodes = 0_i8

        sql_block: block
            rc = db_count(db, SQL_TABLE_NODES, n)
            if (dm_is_error(rc)) exit sql_block

            rc = E_ALLOC
            allocate (nodes(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'nodes.id', desc=.false.)

            rc = db_prepare(db, db_stmt, SQL_SELECT_NODES)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, nodes(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(nnodes)) nnodes = n
            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(nodes)) allocate (nodes(0))
    end function db_select_nodes_array

    integer function db_select_nodes_iter(db, db_stmt, node) result(rc)
        !! Iterator function that returns all sensor nodes in `node`. The
        !! statement `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_node

        type(db_type),      intent(inout) :: db      !! Database type.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        type(node_type),    intent(out)   :: node    !! Returned node data.

        type(db_query_type) :: db_query

        if (.not. dm_db_stmt_is_prepared(db_stmt)) then
            call dm_db_query_set_order(db_query, 'nodes.id', desc=.false.)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NODES))
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = E_DB_NO_ROWS
        if (dm_is_error(db_step(db_stmt))) return

        rc = db_next_row(db_stmt, node)
    end function db_select_nodes_iter

    integer function db_select_observs_array(db, observs, node_id, sensor_id, target_id, from, to, &
                                             desc, limit, stub, nobservs) result(rc)
        !! Returns observations in `observs`, with optional node id, sensor id,
        !! target id, from, to. By default, observations are returned in
        !! ascending order, unless `desc` is passed and `.true.`. The maximum
        !! number of observations may be passed in `limit`.
        !!
        !! The `stub` is `.true.`, neither receivers nor requests are read from
        !! database.
        !!
        !! The total number of observations is returned in optional argument
        !! `nobservs`.
        !!
        !! Calling this function is usually slower than
        !! `db_select_observs_by_id()` or `db_select_observs_by_time()`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(observ_type), allocatable, intent(out)           :: observs(:) !! Returned observation data.
        character(len=*),               intent(in),  optional :: node_id    !! Node id.
        character(len=*),               intent(in),  optional :: sensor_id  !! Sensor id.
        character(len=*),               intent(in),  optional :: target_id  !! Target id.
        character(len=*),               intent(in),  optional :: from       !! Beginning of time span.
        character(len=*),               intent(in),  optional :: to         !! End of time span.
        logical,                        intent(in),  optional :: desc       !! Descending order.
        integer(kind=i8),               intent(in),  optional :: limit      !! Max. number of observations.
        logical,                        intent(in),  optional :: stub       !! Without receivers, requests, responses.
        integer(kind=i8),               intent(out), optional :: nobservs   !! Total number of observations (may be greater than limit).

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nobservs)) nobservs  = 0_i8

        call dm_db_query_add_text(db_query, 'nodes.id = ?',           node_id)
        call dm_db_query_add_text(db_query, 'sensors.id = ?',         sensor_id)
        call dm_db_query_add_text(db_query, 'targets.id = ?',         target_id)
        call dm_db_query_add_text(db_query, 'observs.timestamp >= ?', from)
        call dm_db_query_add_text(db_query, 'observs.timestamp < ?',  to)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NOBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call db_column(db_stmt, 0, n)

            rc = db_finalize(db_stmt)
            if (dm_is_error(rc)) return

            if (present(nobservs)) nobservs = n
            if (present(limit))    n        = min(n, limit)

            rc = E_ALLOC
            allocate (observs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'observs.timestamp', desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, observs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)

        if (.not. allocated(observs)) allocate (observs(0))
        if (dm_is_error(rc)) return
        if (dm_present(stub, .false.)) return
        if (size(observs) == 0) return

        rc = db_select_observs_data(db, observs)
    end function db_select_observs_array

    integer function db_select_observs_iter(db, db_stmt, observ, node_id, sensor_id, target_id, from, to, &
                                            desc, limit, stub) result(rc)
        !! Iterator function that returns observations in `observ`, with
        !! optional node id, sensor id, target id, from, to. By default,
        !! observations are returned in ascending order, unless `desc` is
        !! passed and `.true.`. The maximum number of observations may be
        !! passed in `limit`. The statement `db_stmt` must be finalised once
        !! finished.
        !!
        !! The `stub` is `.true.`, neither receivers nor requests are read from
        !! database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),      intent(inout)        :: db        !! Database type.
        type(db_stmt_type), intent(inout)        :: db_stmt   !! Database statement type.
        type(observ_type),  intent(out)          :: observ    !! Returned observation type.
        character(len=*),   intent(in), optional :: node_id   !! Node id.
        character(len=*),   intent(in), optional :: sensor_id !! Sensor id.
        character(len=*),   intent(in), optional :: target_id !! Target id.
        character(len=*),   intent(in), optional :: from      !! Beginning of time span.
        character(len=*),   intent(in), optional :: to        !! End of time span.
        logical,            intent(in), optional :: desc      !! Descending order.
        integer(kind=i8),   intent(in), optional :: limit     !! Max. number of observations.
        logical,            intent(in), optional :: stub      !! Without receivers, requests, responses.

        integer             :: i, n
        type(db_query_type) :: db_query

        if (.not. dm_db_stmt_is_prepared(db_stmt)) then
            call dm_db_query_add_text(db_query, 'nodes.id = ?',           node_id)
            call dm_db_query_add_text(db_query, 'sensors.id = ?',         sensor_id)
            call dm_db_query_add_text(db_query, 'targets.id = ?',         target_id)
            call dm_db_query_add_text(db_query, 'observs.timestamp >= ?', from)
            call dm_db_query_add_text(db_query, 'observs.timestamp < ?',  to)

            call dm_db_query_set_order(db_query, 'observs.timestamp', desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERVS))
            if (dm_is_error(rc)) return

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = E_DB_NO_ROWS
        if (dm_is_error(db_step(db_stmt))) return

        rc = db_next_row(db_stmt, observ)
        if (dm_is_error(rc)) return
        if (dm_present(stub, .false.)) return

        ! Get receivers.
        if (observ%nreceivers > 0) then
            rc = db_select_receivers(db, observ%receivers, observ%id)
            if (dm_is_error(rc)) return
        end if

        ! Get requests.
        if (observ%nrequests > 0) then
            rc = db_select_requests(db, observ%requests, observ%id)
            if (dm_is_error(rc)) return
        end if

        ! Get responses.
        do i = 1, observ%nrequests
            n = observ%requests(i)%nresponses
            if (n == 0) cycle

            rc = db_select_responses(db, observ%requests(i)%responses, observ%id, i)
            if (dm_is_error(rc)) exit
        end do
    end function db_select_observs_iter

    integer function db_select_observs_data(db, observs) result(rc)
        !! Fill receivers, requests, and responses into `observs` from
        !! database. Caches the SQLite prepared statements for re-use.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),     intent(inout) :: db         !! Database type.
        type(observ_type), intent(inout) :: observs(:) !! Returned observation data.

        integer            :: j, nres, stat
        integer(kind=i8)   :: i, nobs
        type(db_stmt_type) :: db_stmt

        nobs = size(observs, kind=i8)

        rc = E_DB_NO_ROWS
        if (nobs == 0) return

        ! Get receivers (re-use statement).
        do i = 1, nobs
            if (observs(i)%nreceivers == 0) cycle
            rc = db_select_receivers(db, observs(i)%receivers, observs(i)%id, db_stmt=db_stmt)
            if (dm_is_error(rc)) exit
        end do

        stat = db_finalize(db_stmt)
        if (dm_is_error(rc)) return

        ! Get requests (re-use statement).
        do i = 1, nobs
            if (observs(i)%nrequests == 0) cycle
            rc = db_select_requests(db, observs(i)%requests, observs(i)%id, db_stmt=db_stmt)
            if (dm_is_error(rc)) exit
        end do

        stat = db_finalize(db_stmt)
        if (dm_is_error(rc)) return

        ! Get responses (re-use statement).
        obs_loop: do i = 1, nobs
            req_loop: do j = 1, observs(i)%nrequests
                nres = observs(i)%requests(j)%nresponses
                if (nres == 0) cycle req_loop

                rc = db_select_responses(db          = db, &
                                         responses   = observs(i)%requests(j)%responses, &
                                         observ_id   = observs(i)%id, &
                                         request_idx = j, &
                                         db_stmt     = db_stmt)
                if (dm_is_error(rc)) exit obs_loop
            end do req_loop
        end do obs_loop

        stat = db_finalize(db_stmt)
    end function db_select_observs_data

    integer function db_select_receivers(db, receivers, observ_id, nreceivers, db_stmt) result(rc)
        !! Returns receivers of an observation in array `receivers`. On error,
        !! the error code is returned. If `statement` is passed, the statement
        !! will not be finalised in order to be re-used again. Finalisation has
        !! to be done by the caller. If `statement` is passed and set to
        !! `c_null_ptr`, it will be prepared by the function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if too many rows are returned.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),                      intent(inout)           :: db         !! Database type.
        character(len=OBSERV_RECEIVER_LEN), intent(out)             :: receivers(OBSERV_MAX_NRECEIVERS) !! Returned receivers array.
        character(len=*),                   intent(in)              :: observ_id  !! Observation id.
        integer,                            intent(out),   optional :: nreceivers !! Number of receivers.
        type(db_stmt_type),                 intent(inout), optional :: db_stmt    !! Database statement type.

        integer            :: i, n, stat
        type(db_stmt_type) :: db_stmt_

        if (present(db_stmt))    db_stmt_   = db_stmt
        if (present(nreceivers)) nreceivers = 0

        sql_block: block
            if (.not. dm_db_stmt_is_prepared(db_stmt_)) then
                rc = db_prepare(db, db_stmt_, SQL_SELECT_RECEIVERS)
                if (dm_is_error(rc)) exit sql_block
            end if

            rc = db_bind(db_stmt_, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            i = 0

            do while (db_step(db_stmt_) == E_DB_ROW)
                rc = E_BOUNDS
                if (i >= OBSERV_MAX_NRECEIVERS) exit

                i = i + 1

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (.not. db_column_is_text(db_stmt_, 0)) exit sql_block
                end if

                call db_column(db_stmt_, 0, receivers(i), n)
            end do

            if (present(nreceivers)) nreceivers = i
            rc = db_reset(db_stmt_)
        end block sql_block

        if (.not. present(db_stmt)) then
            stat = db_finalize(db_stmt_)
            return
        end if

        db_stmt = db_stmt_
    end function db_select_receivers

    integer function db_select_requests(db, requests, observ_id, nrequests, db_stmt) result(rc)
        !! Returns the request data of a given observation in array `requests`.
        !! If `statement` is passed, the statement will not be finalised in
        !! order to be re-used again. Finalisation has to be done by the
        !! caller. If `statement` is passed and set to `c_null_ptr`, it will
        !! be prepared by the function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if too many rows are returned.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ
        use :: dm_request

        type(db_type),      intent(inout)           :: db        !! Database type.
        type(request_type), intent(out)             :: requests(OBSERV_MAX_NREQUESTS) !! Requests data.
        character(len=*),   intent(in)              :: observ_id !! Observation id.
        integer,            intent(out),   optional :: nrequests !! Number of requests.
        type(db_stmt_type), intent(inout), optional :: db_stmt   !! Database statement type.

        integer            :: i, n, stat
        type(db_stmt_type) :: db_stmt_

        if (present(db_stmt))   db_stmt_  = db_stmt
        if (present(nrequests)) nrequests = 0

        sql_block: block
            if (.not. dm_db_stmt_is_prepared(db_stmt_)) then
                rc = db_prepare(db, db_stmt_, SQL_SELECT_REQUESTS)
                if (dm_is_error(rc)) exit sql_block
            end if

            rc = db_bind(db_stmt_, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            i = 0

            do while (db_step(db_stmt_) == E_DB_ROW)
                rc = E_BOUNDS
                if (i >= OBSERV_MAX_NREQUESTS) exit

                i = i + 1

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (.not. db_column_is_text   (db_stmt_,  0)) exit sql_block
                    if (.not. db_column_is_text   (db_stmt_,  1)) exit sql_block
                    if (.not. db_column_is_text   (db_stmt_,  2)) exit sql_block
                    if (.not. db_column_is_text   (db_stmt_,  3)) exit sql_block
                    if (.not. db_column_is_text   (db_stmt_,  4)) exit sql_block
                    if (.not. db_column_is_text   (db_stmt_,  5)) exit sql_block
                    if (.not. db_column_is_integer(db_stmt_,  6)) exit sql_block
                    if (.not. db_column_is_integer(db_stmt_,  7)) exit sql_block
                    if (.not. db_column_is_integer(db_stmt_,  8)) exit sql_block
                    if (.not. db_column_is_integer(db_stmt_,  9)) exit sql_block
                    if (.not. db_column_is_integer(db_stmt_, 10)) exit sql_block
                    if (.not. db_column_is_integer(db_stmt_, 11)) exit sql_block
                    if (.not. db_column_is_integer(db_stmt_, 12)) exit sql_block
                end if

                call db_column(db_stmt_,  0, requests(i)%name,      n)
                call db_column(db_stmt_,  1, requests(i)%timestamp, n)
                call db_column(db_stmt_,  2, requests(i)%request,   n)
                call db_column(db_stmt_,  3, requests(i)%response,  n)
                call db_column(db_stmt_,  4, requests(i)%delimiter, n)
                call db_column(db_stmt_,  5, requests(i)%pattern,   n)
                call db_column(db_stmt_,  6, requests(i)%delay)
                call db_column(db_stmt_,  7, requests(i)%error)
                call db_column(db_stmt_,  8, requests(i)%mode)
                call db_column(db_stmt_,  9, requests(i)%retries)
                call db_column(db_stmt_, 10, requests(i)%state)
                call db_column(db_stmt_, 11, requests(i)%timeout)
                call db_column(db_stmt_, 12, requests(i)%nresponses)
            end do

            if (present(nrequests)) nrequests = i

            rc = db_reset(db_stmt_)
        end block sql_block

        if (.not. present(db_stmt)) then
            stat = db_finalize(db_stmt_)
            return
        end if

        db_stmt = db_stmt_
    end function db_select_requests

    integer function db_select_responses(db, responses, observ_id, request_idx, nresponses, db_stmt) result(rc)
        !! Returns all responses from a given observation and request index in
        !! array `responses`. If `statement` is passed, the statement will not
        !! be finalised in order to be re-used again. Finalisation has to be
        !! done by the caller. If `statement` is passed and set to
        !! `c_null_ptr`, it will be prepared by the function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if too many rows are returned.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_request
        use :: dm_response

        type(db_type),       intent(inout)           :: db          !! Database type.
        type(response_type), intent(out)             :: responses(REQUEST_MAX_NRESPONSES) !! Returned responses array.
        character(len=*),    intent(in)              :: observ_id   !! Observation id.
        integer,             intent(in)              :: request_idx !! Request index.
        integer,             intent(out),   optional :: nresponses  !! Number of responses.
        type(db_stmt_type),  intent(inout), optional :: db_stmt     !! Database statement type.

        integer            :: i, n
        type(db_stmt_type) :: db_stmt_

        if (present(db_stmt))    db_stmt_   = db_stmt
        if (present(nresponses)) nresponses = 0

        sql_block: block
            if (.not. dm_db_stmt_is_prepared(db_stmt_)) then
                rc = db_prepare(db, db_stmt_, SQL_SELECT_RESPONSES)
                if (dm_is_error(rc)) exit sql_block
            end if

            rc = db_bind(db_stmt_, 1, observ_id);   if (dm_is_error(rc)) exit sql_block
            rc = db_bind(db_stmt_, 2, request_idx); if (dm_is_error(rc)) exit sql_block

            i = 0

            do while (db_step(db_stmt_) == E_DB_ROW)
                rc = E_BOUNDS
                if (i >= REQUEST_MAX_NRESPONSES) exit

                i = i + 1

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (.not. db_column_is_text   (db_stmt_, 0)) exit sql_block
                    if (.not. db_column_is_text   (db_stmt_, 1)) exit sql_block
                    if (.not. db_column_is_integer(db_stmt_, 2)) exit sql_block
                    if (.not. db_column_is_integer(db_stmt_, 3)) exit sql_block
                    if (.not. db_column_is_float  (db_stmt_, 4)) exit sql_block
                end if

                call db_column(db_stmt_, 0, responses(i)%name, n)
                call db_column(db_stmt_, 1, responses(i)%unit, n)
                call db_column(db_stmt_, 2, responses(i)%type)
                call db_column(db_stmt_, 3, responses(i)%error)
                call db_column(db_stmt_, 4, responses(i)%value)
            end do

            if (present(nresponses)) nresponses = i
            rc = db_reset(db_stmt_)
        end block sql_block

        if (.not. present(db_stmt)) then
            rc = db_finalize(db_stmt_)
            return
        end if

        db_stmt = db_stmt_
    end function db_select_responses

    integer function db_select_sensors_array(db, sensors, node_id, nsensors) result(rc)
        !! Returns all sensors in allocatable array `sensors`. If argument
        !! `node_id` is passed, returns only sensors of this node.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if node id is empty.
        !!
        use :: dm_sensor

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(sensor_type), allocatable, intent(out)           :: sensors(:) !! Returned sensor data array.
        character(len=*),               intent(in),  optional :: node_id    !! Node id.
        integer(kind=i8),               intent(out), optional :: nsensors   !! Number of returned sensors.

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nsensors)) nsensors = 0_i8

        if (present(node_id)) then
            rc = E_INVALID
            if (len_trim(node_id) == 0) return
        end if

        call dm_db_query_add_text(db_query, 'nodes.id = ?', node_id)

        sql_block: block
            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NSENSORS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call db_column(db_stmt, 0, n)

            rc = db_finalize(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            if (present(nsensors)) nsensors = n

            rc = E_ALLOC
            allocate (sensors(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, 'sensors.id', desc=.false.)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_SENSORS))
            if (dm_is_error(rc)) exit sql_block

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, sensors(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(sensors)) allocate (sensors(0))
    end function db_select_sensors_array

    integer function db_select_sensors_iter(db, db_stmt, sensor, node_id) result(rc)
        !! Iterator function that returns all sensors in `sensor`. The
        !! statement `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if node id is empty.
        !!
        use :: dm_sensor

        type(db_type),      intent(inout)        :: db      !! Database type.
        type(db_stmt_type), intent(inout)        :: db_stmt !! Database statement type.
        type(sensor_type),  intent(out)          :: sensor  !! Returned sensor data.
        character(len=*),   intent(in), optional :: node_id !! Node id.

        type(db_query_type) :: db_query

        if (.not. dm_db_stmt_is_prepared(db_stmt)) then
            if (present(node_id)) then
                rc = E_INVALID
                if (len_trim(node_id) == 0) return
            end if

            call dm_db_query_add_text(db_query, 'nodes.id = ?', node_id)
            call dm_db_query_set_order(db_query, 'sensors.id', desc=.false.)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_SENSORS))
            if (dm_is_error(rc)) return

            rc = db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = E_DB_NO_ROWS
        if (dm_is_error(db_step(db_stmt))) return

        rc = db_next_row(db_stmt, sensor)
    end function db_select_sensors_iter

    integer function db_select_sync(db, type, query, sync) result(rc)
        !! Utility function that returns synchronisation data of given query.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_INVALID` if sync data type is invalid.
        !!
        use :: dm_sync

        type(db_type),    intent(inout) :: db    !! Database type.
        integer,          intent(in)    :: type  !! Sync data type.
        character(len=*), intent(in)    :: query !! Select query.
        type(sync_type),  intent(out)   :: sync  !! Returned sync data.

        integer            :: stat
        type(db_stmt_type) :: db_stmt

        rc = E_INVALID
        if (.not. dm_sync_type_is_valid(type)) return

        sql_block: block
            rc = db_prepare(db, db_stmt, trim(query))
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_NO_ROWS
            if (dm_is_error(db_step(db_stmt))) exit sql_block

            rc = db_next_row(db_stmt, sync)
        end block sql_block

        stat = db_finalize(db_stmt)
        if (dm_is_ok(rc)) sync%type = type
    end function db_select_sync

    integer function db_select_syncs(db, type, count_query, query, syncs, nsyncs, limit) result(rc)
        !! Returns synchronisation data. The number of sync records `nsyncs` may
        !! be greater than `limit`. However, the size of array `syncs` is always
        !! less-equal `limit`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_sync

        type(db_type),                intent(inout)        :: db          !! Database type.
        integer,                      intent(in)           :: type        !! Sync data type.
        character(len=*),             intent(in)           :: count_query !! Select count query.
        character(len=*),             intent(in)           :: query       !! Select query.
        type(sync_type), allocatable, intent(out)          :: syncs(:)    !! Returned sync data.
        integer(kind=i8),             intent(out)          :: nsyncs      !! Total number of sync records.
        integer(kind=i8),             intent(in), optional :: limit       !! Max. number of rows to fetch.

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        nsyncs = 0_i8

        rc = E_INVALID
        if (.not. dm_sync_type_is_valid(type)) return

        sql_block: block
            rc = db_prepare(db, db_stmt, trim(count_query))
            if (dm_is_error(rc)) exit sql_block

            rc = db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call db_column(db_stmt, 0, nsyncs)

            rc = db_finalize(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            n = nsyncs
            if (present(limit)) n = min(limit, nsyncs)

            rc = E_ALLOC
            allocate (syncs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (nsyncs == 0) exit sql_block

            call dm_db_query_set_limit(db_query, limit)

            rc = db_prepare(db, db_stmt, dm_db_query_build(db_query, trim(query)))
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, syncs(i))
                syncs(i)%type = type
                if (dm_is_error(rc)) exit
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        stat = db_finalize(db_stmt)
        if (.not. allocated(syncs)) allocate (syncs(0))
    end function db_select_syncs

    integer function db_select_targets_array(db, targets, ntargets) result(rc)
        !! Returns number of targets and array of target data in allocatable
        !! array `targets`, if query was successful.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_target

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(target_type), allocatable, intent(out)           :: targets(:) !! Target data array.
        integer(kind=i8),               intent(out), optional :: ntargets   !! Number of selected targets.

        integer            :: stat
        integer(kind=i8)   :: i, n
        type(db_stmt_type) :: db_stmt

        if (present(ntargets)) ntargets = 0_i8

        sql_block: block
            rc = db_count(db, SQL_TABLE_TARGETS, n)
            if (dm_is_error(rc)) exit sql_block

            rc = E_ALLOC
            allocate (targets(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            rc = db_prepare(db, db_stmt, SQL_SELECT_TARGETS)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = db_next_row(db_stmt, targets(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(ntargets)) ntargets = n
            rc = E_NONE
        end block sql_block

        stat = db_finalize(db_stmt)
        if (.not. allocated(targets)) allocate (targets(0))
    end function db_select_targets_array

    integer function db_select_targets_iter(db, db_stmt, target) result(rc)
        !! Iterator function that returns all targets in `target`. The
        !! statement `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no more rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_target

        type(db_type),      intent(inout) :: db      !! Database type.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        type(target_type),  intent(out)   :: target  !! Target data.

        if (.not. dm_db_stmt_is_prepared(db_stmt)) then
            rc = db_prepare(db, db_stmt, SQL_SELECT_TARGETS)
            if (dm_is_error(rc)) return
        end if

        rc = E_DB_NO_ROWS
        if (dm_is_error(db_step(db_stmt))) return

        rc = db_next_row(db_stmt, target)
    end function db_select_targets_iter

    integer function db_step(db_stmt) result(rc)
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
    end function db_step

    ! **************************************************************************
    ! PRIVATE SUBROUTINES.
    ! **************************************************************************
    subroutine db_column_bytes(db_stmt, index, value)
        !! Returns byte size of column value of given index.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        integer,            intent(in)    :: index   !! Column index.
        integer,            intent(out)   :: value   !! Value.

        value = sqlite3_column_bytes(db_stmt%ctx, index)
    end subroutine db_column_bytes

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

    subroutine db_column_text_alloc(db_stmt, index, value)
        !! Returns string value from column of given index.
        type(db_stmt_type),            intent(inout) :: db_stmt !! Database statement type.
        integer,                       intent(in)    :: index   !! Column index.
        character(len=:), allocatable, intent(out)   :: value   !! Value.

        value = sqlite3_column_text(db_stmt%ctx, index)
    end subroutine db_column_text_alloc
end module dm_db
