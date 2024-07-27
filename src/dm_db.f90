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
    integer, parameter, public :: DB_JOURNAL_OFF      = 0 !! No rollback journals.
    integer, parameter, public :: DB_JOURNAL_DELETE   = 1 !! Delete journal (default).
    integer, parameter, public :: DB_JOURNAL_TRUNCATE = 2 !! Delete journal by truncating (may be faster than `DB_JOURNAL_DELETE`).
    integer, parameter, public :: DB_JOURNAL_PERSIST  = 3 !! Overwrite journal instead of deleting (may be faster than deleting or truncating).
    integer, parameter, public :: DB_JOURNAL_MEMORY   = 4 !! Store journal in memory (fast, volatile).
    integer, parameter, public :: DB_JOURNAL_WAL      = 5 !! Use Write-Ahead Log (WAL) journal.

    ! SQLite 3 auto vacuum modes.
    integer, parameter, public :: DB_AUTO_VACUUM_NONE        = 0 !! No auto-vacuum (default).
    integer, parameter, public :: DB_AUTO_VACUUM_FULL        = 1 !! Enable auto-vacuum.
    integer, parameter, public :: DB_AUTO_VACUUM_INCREMENTAL = 2 !! Vacuum requires additional PRAGMA.

    ! SQLite 3 transactions.
    integer, parameter, public :: DB_TRANS_DEFERRED  = 0 !! Deferred transaction (default).
    integer, parameter, public :: DB_TRANS_IMMEDIATE = 1 !! Start a new write immediately (may fail with `E_DB_BUSY`).
    integer, parameter, public :: DB_TRANS_EXCLUSIVE = 2 !! No reading while transactions are underway.

    ! Additional parameters.
    integer, parameter, public :: DB_APPLICATION_ID  = int(z'444D31') !! Application id of DMPACK databases (`DM1` in ASCII).
    integer, parameter, public :: DB_USER_VERSION    = 1              !! Database schema version, increased on updates.
    integer, parameter, public :: DB_TIMEOUT_DEFAULT = 1000           !! Default SQLite 3 busy timeout in mseconds.

    ! Private parameters.
    character(len=*), parameter :: DB_ATTACHED_NAME = 'attached'
    integer,          parameter :: DB_MAX_QUERY_LEN = 4096

    type, public :: db_type
        !! Opaque SQLite database connectivity type.
        private
        type(c_ptr) :: ptr       = c_null_ptr !! C pointer to SQLite 3 database.
        logical     :: read_only = .false.    !! Read-only flag.
    end type db_type

    type, public :: db_stmt_type
        !! Opaque SQLite database statement type.
        private
        type(c_ptr) :: ptr = c_null_ptr !! C pointer to SQLite 3 statement.
    end type db_stmt_type

    abstract interface
        function dm_db_busy_handler(client_data, n) bind(c)
            !! C-interoperable callback function that is invoked on error
            !! `SQL_BUSY`. May return 0 to signal that no more invocations are
            !! desired.
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: client_data        !! Client data.
            integer(kind=c_int), intent(in), value :: n                  !! Number of times the busy handler has been invoked previously.
            integer(kind=c_int)                    :: dm_db_busy_handler !! Returns value.
        end function dm_db_busy_handler

        subroutine dm_db_backup_handler(remaining, page_count)
            !! Callback routine that is invoked if passed to `dm_db_backup()`.
            implicit none
            integer, intent(in) :: remaining  !! Remaining pages.
            integer, intent(in) :: page_count !! Total number of pages.
        end subroutine dm_db_backup_handler

        subroutine dm_db_log_handler(client_data, err_code, err_msg_ptr) bind(c)
            !! C-interoperable callback routine that is invoked for each created SQLite log.
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: client_data !! Client data.
            integer(kind=c_int), intent(in), value :: err_code    !! SQLite error code.
            type(c_ptr),         intent(in), value :: err_msg_ptr !! SQLite error message.
        end subroutine dm_db_log_handler

        subroutine dm_db_update_handler(client_data, type, db_name, table_name, row_id) bind(c)
            !! C-interoperable callback routine that is invoked on database updates.
            import :: c_int, c_int64_t, c_ptr
            implicit none
            type(c_ptr),             intent(in), value :: client_data !! Client data.
            integer(kind=c_int),     intent(in), value :: type        !! Database operation.
            type(c_ptr),             intent(in), value :: db_name     !! Database name.
            type(c_ptr),             intent(in), value :: table_name  !! Table name.
            integer(kind=c_int64_t), intent(in), value :: row_id      !! Row id.
        end subroutine dm_db_update_handler
    end interface

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
        module procedure :: db_select_sensors_by_node_array
        module procedure :: db_select_sensors_by_node_iter
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

    ! Abstract interfaces.
    public :: dm_db_backup_handler
    public :: dm_db_busy_handler
    public :: dm_db_log_handler
    public :: dm_db_update_handler

    ! Public procedures.
    public :: dm_db_attach
    public :: dm_db_backup
    public :: dm_db_begin
    public :: dm_db_close
    public :: dm_db_commit
    public :: dm_db_connected
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
    public :: dm_db_get_user_version
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
    public :: dm_db_log
    public :: dm_db_log_exists
    public :: dm_db_node_exists
    public :: dm_db_observ_exists
    public :: dm_db_open
    public :: dm_db_optimize
    public :: dm_db_prepared
    public :: dm_db_read_only
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
    public :: dm_db_select_logs_by_observ
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
    public :: dm_db_sensor_exists
    public :: dm_db_set_application_id
    public :: dm_db_set_auto_vacuum
    public :: dm_db_set_busy_handler
    public :: dm_db_set_busy_timeout
    public :: dm_db_set_foreign_keys
    public :: dm_db_set_journal_mode
    public :: dm_db_set_log_handler
    public :: dm_db_set_query_only
    public :: dm_db_set_update_handler
    public :: dm_db_set_user_version
    public :: dm_db_shutdown
    public :: dm_db_sleep
    public :: dm_db_table_exists
    public :: dm_db_target_exists
    public :: dm_db_threadsafe
    public :: dm_db_update
    public :: dm_db_update_node
    public :: dm_db_update_sensor
    public :: dm_db_update_target
    public :: dm_db_vacuum
    public :: dm_db_valid
    public :: dm_db_version

    ! Private procedures.
    private :: db_begin
    private :: db_commit
    private :: db_count
    private :: db_delete_receivers ! obsolete
    private :: db_delete_requests  ! obsolete
    private :: db_delete_responses ! obsolete
    private :: db_exec
    private :: db_exists
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
    private :: db_query_where
    private :: db_release
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
    private :: db_select_nrows
    private :: db_select_observs_array
    private :: db_select_observs_iter
    private :: db_select_observs_data
    private :: db_select_receivers
    private :: db_select_requests
    private :: db_select_responses
    private :: db_select_sensors_array
    private :: db_select_sensors_iter
    private :: db_select_sensors_by_node_array
    private :: db_select_sensors_by_node_iter
    private :: db_select_sync
    private :: db_select_syncs
    private :: db_select_targets_array
    private :: db_select_targets_iter
contains
    ! ******************************************************************
    ! PUBLIC FUNCTIONS.
    ! ******************************************************************
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
        use :: dm_file

        character(len=*), parameter :: QUERY_FMT = '("ATTACH DATABASE ''", a, "'' AS ", a)'

        type(db_type),    intent(inout)        :: db   !! Database type.
        character(len=*), intent(in)           :: path !! Path of database to attach.
        character(len=*), intent(in), optional :: name !! Name of attached database.

        character(len=DB_MAX_QUERY_LEN) :: query

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) return

        rc = E_DB_ATTACH
        if (present(name)) then
            write (query, QUERY_FMT) trim(path), name
        else
            write (query, QUERY_FMT) trim(path), DB_ATTACHED_NAME
        end if

        if (dm_is_error(db_exec(db, trim(query)))) return
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

        type(db_type),    intent(inout)           :: db         !! Database type.
        character(len=*), intent(in)              :: path       !! File path of backup database to be created.
        logical,          intent(in),    optional :: wal        !! Enable WAL mode for backup.
        procedure(dm_db_backup_handler), optional :: callback   !! Progress callback routine.
        integer,          intent(in),    optional :: nsteps     !! Number of steps per iteration (default: 500).
        integer,          intent(in),    optional :: sleep_time !! Sleep time per iteration in msec (default: 250 msec).

        integer       :: stat
        integer       :: nsteps_, sleep_time_
        logical       :: wal_
        type(db_type) :: backup
        type(c_ptr)   :: ptr

        rc = E_READ_ONLY
        if (db%read_only) return

        wal_ = .false.
        if (present(wal)) wal_ = wal

        nsteps_ = NSTEPS_DEFAULT
        if (present(nsteps)) nsteps_ = nsteps

        sleep_time_ = SLEEP_TIME_DEFAULT
        if (present(sleep_time)) sleep_time_ = sleep_time

        rc = E_EXIST
        if (dm_file_exists(path)) return

        rc = dm_db_open(backup, path, create=.true., wal=wal_)
        if (dm_is_error(rc)) return

        sql_block: block
            rc = E_DB_BACKUP
            ptr = sqlite3_backup_init(backup%ptr, 'main', db%ptr, 'main')
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

        logical :: optimize_

        optimize_ = .false.
        if (present(optimize)) optimize_ = optimize
        if (optimize_) rc = dm_db_optimize(db)

        rc = E_DB
        if (sqlite3_close(db%ptr) /= SQLITE_OK) return

        db%ptr = c_null_ptr
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

    logical function dm_db_connected(db) result(connected)
        !! Returns `.true.` if database type has associated pointer.
        type(db_type), intent(inout) :: db !! Database type.

        connected = c_associated(db%ptr)
    end function dm_db_connected

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
        if (.not. dm_db_connected(db)) return

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
        logical :: sync_

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (.not. dm_db_connected(db)) return

        sync_ = .false.
        if (present(sync)) sync_ = sync

        ! Create logs table.
        rc = db_exec(db, SQL_CREATE_LOGS)
        if (dm_is_error(rc)) return

        ! Create sync logs table.
        if (sync_) then
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
        logical :: sync_

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (.not. dm_db_connected(db)) return

        sync_ = .false.
        if (present(sync)) sync_ = sync

        ! Create tables.
        rc = db_exec(db, SQL_CREATE_NODES)
        if (dm_is_error(rc)) return

        rc = db_exec(db, SQL_CREATE_SENSORS)
        if (dm_is_error(rc)) return

        rc = db_exec(db, SQL_CREATE_TARGETS)
        if (dm_is_error(rc)) return

        rc = db_exec(db, SQL_CREATE_OBSERVS)
        if (dm_is_error(rc)) return

        rc = db_exec(db, SQL_CREATE_RECEIVERS)
        if (dm_is_error(rc)) return

        rc = db_exec(db, SQL_CREATE_REQUESTS)
        if (dm_is_error(rc)) return

        rc = db_exec(db, SQL_CREATE_RESPONSES)
        if (dm_is_error(rc)) return

        ! Create sync tables.
        if (sync_) then
            rc = db_exec(db, SQL_CREATE_SYNC_NODES)
            if (dm_is_error(rc)) return

            rc = db_exec(db, SQL_CREATE_SYNC_OBSERVS)
            if (dm_is_error(rc)) return

            rc = db_exec(db, SQL_CREATE_SYNC_SENSORS)
            if (dm_is_error(rc)) return

            rc = db_exec(db, SQL_CREATE_SYNC_TARGETS)
            if (dm_is_error(rc)) return
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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_DELETE_BEAT, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(log_id) == 0) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_DELETE_LOG, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(log_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_DELETE_NODE, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(observ_id) == 0) return

        ! Start transaction.
        rc = db_begin(db)
        if (dm_is_error(rc)) return

        sql_block: block
            ! Delete observation.
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_DELETE_OBSERV, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)

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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(sensor_id) == 0) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_DELETE_SENSOR, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(sensor_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (len_trim(target_id) == 0) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_DELETE_TARGET, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(target_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_delete_target

    integer function dm_db_detach(db, name) result(rc)
        !! Detaches database from the current connection. If no name is passed
        !! for the attached database, the name is assumed to be `attached`. The
        !! function trims the given name string. Returns `E_DB_DETACH` on error.
        character(len=*), parameter :: QUERY_FMT = '("DETACH DATABASE ", a)'

        type(db_type),    intent(inout)        :: db   !! Database type.
        character(len=*), intent(in), optional :: name !! Name of attached database.

        character(len=DB_MAX_QUERY_LEN) :: query

        rc = E_DB_DETACH
        if (present(name)) then
            write (query, QUERY_FMT) name
        else
            write (query, QUERY_FMT) DB_ATTACHED_NAME
        end if

        if (dm_is_error(db_exec(db, trim(query)))) return
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

        error = sqlite3_errcode(db%ptr)
        if (present(sqlite_error)) sqlite_error = error

        select case (error)
            case (SQLITE_OK, SQLITE_DONE, SQLITE_ROW)
                rc = E_NONE
            case (SQLITE_BUSY)
                rc = E_DB_BUSY
            case (SQLITE_LOCKED)
                rc = E_DB_LOCKED
            case (SQLITE_NOMEM)
                rc = E_MEMORY
            case (SQLITE_READONLY)
                rc = E_READ_ONLY
            case (SQLITE_IOERR)
                rc = E_IO
            case (SQLITE_CORRUPT)
                rc = E_CORRUPT
            case (SQLITE_FULL)
                rc = E_FULL
            case (SQLITE_EMPTY)
                rc = E_EMPTY
            case (SQLITE_TOOBIG)
                rc = E_LIMIT
            case (SQLITE_CONSTRAINT)
                rc = E_DB_CONSTRAINT
            case (SQLITE_MISMATCH)
                rc = E_DB_TYPE
            case (SQLITE_FORMAT)
                rc = E_FORMAT
            case (SQLITE_RANGE)
                rc = E_DB_BIND
            case default
                rc = E_DB
        end select
    end function dm_db_error

    function dm_db_error_message(db) result(message)
        !! Returns last SQLite error message.
        type(db_type), intent(inout)  :: db      !! Database type.
        character(len=:), allocatable :: message !! Error message.

        message = sqlite3_errmsg(db%ptr)
    end function dm_db_error_message

    integer function dm_db_finalize(db_stmt) result(rc)
        !! Finalises given database statement. Returns `E_DB_FINALIZE` on
        !! error.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.

        rc = E_NONE
        if (.not. c_associated(db_stmt%ptr)) return
        if (sqlite3_finalize(db_stmt%ptr) /= SQLITE_OK) rc = E_DB_FINALIZE
    end function dm_db_finalize

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

        integer     :: stat
        type(c_ptr) :: stmt

        id = 0

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, 'PRAGMA application_id', stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_INTEGER) exit sql_block

            id = sqlite3_column_int(stmt, 0)
            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        version = 0

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, 'PRAGMA data_version', stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_INTEGER) exit sql_block

            version = sqlite3_column_int(stmt, 0)
            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: i, stat
        type(c_ptr) :: stmt

        enabled = .false.

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, 'PRAGMA foreign_keys', stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_INTEGER) exit sql_block

            i = sqlite3_column_int(stmt, 0)
            enabled = (i == 1)

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        character(len=:), allocatable :: str
        integer                       :: stat
        type(c_ptr)                   :: stmt

        mode = DB_JOURNAL_OFF

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, 'PRAGMA journal_mode', stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT) exit sql_block
            str = sqlite3_column_text(stmt, 0)

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)

        select case (str)
            case ('delete')
                mode = DB_JOURNAL_DELETE
            case ('truncate')
                mode = DB_JOURNAL_TRUNCATE
            case ('persist')
                mode = DB_JOURNAL_PERSIST
            case ('memory')
                mode = DB_JOURNAL_MEMORY
            case ('wal')
                mode = DB_JOURNAL_WAL
            case default
                mode = DB_JOURNAL_OFF
        end select

        if (present(name)) name = str
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

        integer     :: i, stat
        type(c_ptr) :: stmt

        enabled = .false.

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, 'PRAGMA query_only', stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_INTEGER) exit sql_block

            i = sqlite3_column_int(stmt, 0)
            enabled = (i == 1)

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_get_query_only

    integer function dm_db_get_user_version(db, version) result(rc)
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

        integer     :: stat
        type(c_ptr) :: stmt

        version = 0

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, 'PRAGMA user_version', stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_INTEGER) exit sql_block

            version = sqlite3_column_int(stmt, 0)
            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_get_user_version

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

        integer     :: stat
        logical     :: validate_
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate beat.
        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_INVALID
            if (.not. dm_beat_valid(beat)) return
        end if

        ! Set given statement.
        stmt = c_null_ptr
        if (present(db_stmt)) stmt = db_stmt%ptr

        sql_block: block
            if (.not. c_associated(stmt)) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_INSERT_BEAT, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(beat%node_id))   /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 2, trim(beat%address))   /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 3, trim(beat%client))    /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 4, trim(beat%time_sent)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 5, trim(beat%time_recv)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 6, beat%error)           /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 7, beat%interval)        /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 8, beat%uptime)          /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_DB
            if (sqlite3_reset(stmt) /= SQLITE_OK) exit sql_block

            rc = E_NONE
        end block sql_block

        if (present(db_stmt)) then
            db_stmt%ptr = stmt
        else
            stat = sqlite3_finalize(stmt)
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

        transaction_ = .true.
        if (present(transaction)) transaction_ = transaction

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

        stat = dm_db_finalize(db_stmt)

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

        integer     :: stat
        logical     :: validate_
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate log.
        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_INVALID
            if (.not. dm_log_valid(log)) return
        end if

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_INSERT_LOG, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt,  1, trim(log%id))        /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt,  2, log%level)           /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt,  3, log%error)           /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  4, trim(log%timestamp)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  5, trim(log%node_id))   /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  6, trim(log%sensor_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  7, trim(log%target_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  8, trim(log%observ_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  9, trim(log%source))    /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 10, trim(log%message))   /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        logical     :: validate_
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate node.
        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_INVALID
            if (.not. dm_node_valid(node)) return
        end if

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_INSERT_NODE, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text  (stmt, 1, trim(node%id))   /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 2, trim(node%name)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 3, trim(node%meta)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 4, node%x)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 5, node%y)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 6, node%z)          /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: i, n
        logical     :: validate_
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate observation.
        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_INVALID
            if (.not. dm_observ_valid(observ)) return
        end if

        ! Set given statement.
        stmt = c_null_ptr
        if (present(db_stmt)) stmt = db_stmt%ptr

        ! Begin transaction.
        rc = E_DB_TRANSACTION
        if (dm_is_error(db_save_point(db, SAVE_POINT))) return

        sql_block: block
            if (.not. c_associated(stmt)) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_INSERT_OBSERV, stmt) /= SQLITE_OK) exit sql_block
            end if

            ! Add observation data.
            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt,  1, trim(observ%id))        /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  2, trim(observ%node_id))   /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  3, trim(observ%sensor_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  4, trim(observ%target_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  5, trim(observ%name))      /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  6, trim(observ%timestamp)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  7, trim(observ%source))    /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt,  8, trim(observ%device))    /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt,  9, observ%priority)        /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 10, observ%error)           /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 11, observ%next)            /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 12, observ%nreceivers)      /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 13, observ%nrequests)       /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_DB
            if (sqlite3_reset(stmt) /= SQLITE_OK) exit sql_block

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
            db_stmt%ptr = stmt
        else
            if (sqlite3_finalize(stmt) /= SQLITE_OK) rc = E_DB_FINALIZE
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

        transaction_ = .true.
        if (present(transaction)) transaction_ = transaction

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

        stat = dm_db_finalize(db_stmt)

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

        integer     :: stat
        logical     :: validate_
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate sensor.
        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_INVALID
            if (.not. dm_sensor_valid(sensor)) return
        end if

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_INSERT_SENSOR, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text  (stmt, 1, trim(sensor%id))      /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 2, trim(sensor%node_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int   (stmt, 3, sensor%type)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 4, trim(sensor%name))    /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 5, trim(sensor%sn))      /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 6, trim(sensor%meta))    /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 7, sensor%x)             /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 8, sensor%y)             /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 9, sensor%z)             /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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
            case (SYNC_TYPE_LOG)
                rc = dm_db_insert_sync_log(db, sync)
            case (SYNC_TYPE_NODE)
                rc = dm_db_insert_sync_node(db, sync)
            case (SYNC_TYPE_OBSERV)
                rc = dm_db_insert_sync_observ(db, sync)
            case (SYNC_TYPE_SENSOR)
                rc = dm_db_insert_sync_sensor(db, sync)
            case (SYNC_TYPE_TARGET)
                rc = dm_db_insert_sync_target(db, sync)
            case default
                rc = E_INVALID
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

        integer     :: stat
        logical     :: validate_
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate target.
        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_INVALID
            if (.not. dm_target_valid(target)) return
        end if

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_INSERT_TARGET, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text  (stmt, 1, trim(target%id))   /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 2, trim(target%name)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 3, trim(target%meta)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int   (stmt, 4, target%state)      /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 5, target%x)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 6, target%y)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 7, target%z)          /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_insert_target

    logical function dm_db_log_exists(db, log_id) result(exists)
        !! Returns `.true.` if log id exists.
        type(db_type),     intent(inout) :: db     !! Database type.
        character(len=*),  intent(in)    :: log_id !! Log id (UUID).

        exists = db_exists(db, SQL_TABLE_LOGS, log_id)
    end function dm_db_log_exists

    logical function dm_db_node_exists(db, node_id) result(exists)
        !! Returns `.true.` if node id exists.
        type(db_type),     intent(inout) :: db      !! Database type.
        character(len=*),  intent(in)    :: node_id !! Node id.

        exists = db_exists(db, SQL_TABLE_NODES, node_id)
    end function dm_db_node_exists

    logical function dm_db_observ_exists(db, observ_id) result(exists)
        !! Returns `.true.` if observation id exists.
        type(db_type),     intent(inout) :: db        !! Database type.
        character(len=*),  intent(in)    :: observ_id !! Observation id (UUID).

        exists = db_exists(db, SQL_TABLE_OBSERVS, observ_id)
    end function dm_db_observ_exists

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

        ! Create database.
        create_ = .false.
        if (present(create)) create_ = create

        ! Foreign keys contraint.
        foreign_keys_ = .true.
        if (present(foreign_keys)) foreign_keys_ = foreign_keys

        ! Read-only mode.
        if (present(read_only)) db%read_only = read_only

        ! Threaded access (not recommended).
        threaded_ = .false.
        if (present(threaded)) threaded_ = threaded

        ! Busy timeout.
        timeout_ = 0
        if (present(timeout)) timeout_ = timeout

        ! App ID validation.
        validate_ = .false.
        if (present(validate)) validate_ = validate

        ! WAL mode.
        wal_ = .false.
        if (present(wal)) wal_ = wal

        ! Validate options.
        exists = dm_file_exists(path)

        rc = E_INVALID
        if (dm_db_connected(db)) return

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
        if (sqlite3_open_v2(trim(path), db%ptr, flag) /= SQLITE_OK) return

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
                rc = dm_db_set_user_version(db, DB_USER_VERSION)
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
            rc = dm_db_valid(db)
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

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, 'PRAGMA optimize', stmt) /= SQLITE_OK) exit sql_block
            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block
            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_optimize

    logical function dm_db_prepared(db_stmt) result(prepared)
        !! Returns `.true.` if given statement has been prepared.
        type(db_stmt_type), intent(inout) :: db_stmt

        prepared = c_associated(db_stmt%ptr)
    end function dm_db_prepared

    logical function dm_db_read_only(db) result(read_only)
        !! Returns `.true.` if database is in read-only mode. This function
        !! checks only the opaque database type for the read-only flag. It is
        !! still possible to enable ready-only access by calling
        !! `dm_db_set_query_only()`. The function `dm_db_get_query_only()`
        !! returns the status of the `query_only` pragma.
        type(db_type), intent(inout) :: db !! Database type.

        read_only = db%read_only
    end function dm_db_read_only

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
        !!
        use :: dm_beat

        type(db_type),    intent(inout) :: db      !! Database type.
        type(beat_type),  intent(out)   :: beat    !! Returned beat type.
        character(len=*), intent(in)    :: node_id !! Node id.

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_BEAT, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = db_next_row(stmt, beat)
        end block sql_block

        stat = sqlite3_finalize(stmt)
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
        !!
        character(len=*), parameter :: QUERY = ' WHERE node_id = ?'

        type(db_type),                 intent(inout) :: db      !! Database type.
        character(len=:), allocatable, intent(out)   :: json    !! Returned JSON.
        character(len=*),              intent(in)    :: node_id !! Node id.

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_BEATS // QUERY, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = db_next_row(stmt, json)
        end block sql_block

        stat = sqlite3_finalize(stmt)
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
        !!
        character(len=*), parameter :: QUERY = ' WHERE id = ?'

        type(db_type),                 intent(inout) :: db     !! Database type.
        character(len=:), allocatable, intent(out)   :: json   !! Returned JSON.
        character(len=*),              intent(in)    :: log_id !! Log id.

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_LOGS // QUERY, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(log_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = db_next_row(stmt, json)
        end block sql_block

        stat = sqlite3_finalize(stmt)
        if (.not. allocated(json)) json = ''
    end function dm_db_select_json_log

    integer function dm_db_select_json_node(db, json, node_id) result(rc)
        !! Returns nodes associated with given node id as allocatable character
        !! `json` in JSON format.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        character(len=*), parameter :: QUERY = ' WHERE node_id = ?'

        type(db_type),                 intent(inout) :: db      !! Database type.
        character(len=:), allocatable, intent(out)   :: json    !! Returned JSON.
        character(len=*),              intent(in)    :: node_id !! Node id.

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_NODES // QUERY, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = db_next_row(stmt, json)
        end block sql_block

        stat = sqlite3_finalize(stmt)
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
        !!
        use :: dm_log

        type(db_type),    intent(inout) :: db     !! Database type.
        type(log_type),   intent(out)   :: log    !! Returned log data.
        character(len=*), intent(in)    :: log_id !! Log id.

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_LOG, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(log_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = db_next_row(stmt, log)
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_select_log

    integer function dm_db_select_logs_by_observ(db, logs, observ_id, nlogs) result(rc)
        !! Returns logs by observation id in allocatable array `logs`.
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
        character(len=*),            intent(in)            :: observ_id !! Observation id.
        integer(kind=i8),            intent(out), optional :: nlogs     !! Number of logs.

        integer          :: stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        if (present(nlogs)) nlogs = 0_i8

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NLOGS_BY_OBSERV, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            n = sqlite3_column_int64(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            rc = E_ALLOC
            allocate (logs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_LOGS_BY_OBSERV, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ_id)) /= SQLITE_OK) exit sql_block

            do i = 1, n
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, logs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(nlogs)) nlogs = n
        end block sql_block

        stat = sqlite3_finalize(stmt)
        if (.not. allocated(logs)) allocate (logs(0))
    end function dm_db_select_logs_by_observ

    integer function dm_db_select_node(db, node, node_id) result(rc)
        !! Returns node data associated with given id in `node`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_node

        type(db_type),    intent(inout) :: db      !! Database type.
        type(node_type),  intent(out)   :: node    !! Returned node data.
        character(len=*), intent(in)    :: node_id !! Node id.

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NODE, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = db_next_row(stmt, node)
        end block sql_block

        stat = sqlite3_finalize(stmt)
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
        !!
        use :: dm_observ

        type(db_type),     intent(inout) :: db        !! Database type.
        type(observ_type), intent(out)   :: observ    !! Selected observation.
        character(len=*),  intent(in)    :: observ_id !! Observation id (UUID).

        integer     :: i, n
        type(c_ptr) :: stmt

        rc = E_INVALID
        if (len_trim(observ_id) == 0) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_OBSERV, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = db_next_row(stmt, observ)
        end block sql_block

        if (sqlite3_finalize(stmt) /= SQLITE_OK) rc = E_DB_FINALIZE
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

        character(len=:), allocatable :: query
        integer                       :: k, nbytes, stat
        integer(kind=i8)              :: i, n
        type(c_ptr)                   :: stmt

        logical :: has_param, has_node_id, has_sensor_id, has_target_id
        logical :: has_from, has_to, has_limit
        logical :: desc_order, more

        if (present(nids)) nids = 0_i8

        has_param     = .false.; has_node_id = .false.; has_sensor_id = .false.
        has_target_id = .false.; has_from    = .false.; has_to        = .false.
        has_limit     = .false.; desc_order  = .false.

        if (dm_string_is_present(node_id)) then
            has_param = .true.
            has_node_id = .true.
        end if

        if (dm_string_is_present(sensor_id)) then
            has_param = .true.
            has_sensor_id = .true.
        end if

        if (dm_string_is_present(target_id)) then
            has_param = .true.
            has_target_id = .true.
        end if

        if (dm_string_is_present(from)) then
            has_param = .true.
            has_from = .true.
        end if

        if (dm_string_is_present(to)) then
            has_param = .true.
            has_to = .true.
        end if

        if (present(limit)) has_limit  = .true.
        if (present(desc))  desc_order = desc

        ! Build SQL query.
        allocate (character(len=0) :: query)

        if (has_param) then
            more = .false.
            if (has_node_id)   call db_query_where(query, 'nodes.id = ?',           more)
            if (has_sensor_id) call db_query_where(query, 'sensors.id = ?',         more)
            if (has_target_id) call db_query_where(query, 'targets.id = ?',         more)
            if (has_from)      call db_query_where(query, 'observs.timestamp >= ?', more)
            if (has_to)        call db_query_where(query, 'observs.timestamp < ?',  more)
        end if

        sql_block: block
            if (has_param) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NOBSERVS // query, stmt) /= SQLITE_OK) exit sql_block

                rc = db_bind_observs(k)
                if (dm_is_error(rc)) exit sql_block
            else
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NOBSERVS, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            n = sqlite3_column_int64(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            if (present(nids)) nids = n
            if (has_limit) n = min(n, limit)

            rc = E_ALLOC
            allocate (ids(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            query = query // ' ORDER BY observs.timestamp'
            if (desc_order) query = query // ' DESC'
            if (has_limit)  query = query // ' LIMIT ?'

            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_OBSERV_IDS // query, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            k  = 1

            if (has_param) then
                if (dm_is_error(db_bind_observs(k))) exit sql_block
            end if

            if (has_limit) then
                if (sqlite3_bind_int64(stmt, k, limit) /= SQLITE_OK) exit sql_block
            end if

            do i = 1, n
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, ids(i), nbytes, (i == 1))
                if (dm_is_error(rc)) exit sql_block
                rc = E_INVALID
                if (nbytes /= OBSERV_ID_LEN) exit sql_block
            end do
        end block sql_block

        stat = sqlite3_finalize(stmt)
        if (.not. allocated(ids)) allocate (ids(0))
    contains
        integer function db_bind_observs(i) result(rc)
            integer, intent(out) :: i

            rc = E_DB_BIND
            i = 1

            if (has_node_id) then
                if (sqlite3_bind_text(stmt, i, trim(node_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_sensor_id) then
                if (sqlite3_bind_text(stmt, i, trim(sensor_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_target_id) then
                if (sqlite3_bind_text(stmt, i, trim(target_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_from) then
                if (sqlite3_bind_text(stmt, i, trim(from)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_to) then
                if (sqlite3_bind_text(stmt, i, trim(to)) /= SQLITE_OK) return
                i = i + 1
            end if

            rc = E_NONE
        end function db_bind_observs
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

        integer          :: stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        if (present(nviews)) nviews = 0_i8

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NOBSERV_VIEWS, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id))       /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 2, trim(sensor_id))     /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 3, trim(target_id))     /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 4, trim(response_name)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 5, trim(from))          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 6, trim(to))            /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            n = sqlite3_column_int64(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            rc = E_ALLOC
            if (present(limit)) n = min(n, limit)
            allocate (views(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            rc = E_DB_PREPARE
            if (present(limit)) then
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_OBSERV_VIEWS // ' LIMIT ?', stmt) /= SQLITE_OK) exit sql_block
            else
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_OBSERV_VIEWS, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id))       /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 2, trim(sensor_id))     /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 3, trim(target_id))     /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 4, trim(response_name)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 5, trim(from))          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 6, trim(to))            /= SQLITE_OK) exit sql_block

            if (present(limit)) then
                if (sqlite3_bind_int64(stmt, 7, limit) /= SQLITE_OK) exit sql_block
            end if

            do i = 1, n
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, views(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(nviews)) nviews = n
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer           :: stat
        integer(kind=i8)  :: i, nobs
        logical           :: stub_
        type(c_ptr)       :: stmt
        type(observ_type) :: observ1, observ2

        if (present(nobservs)) nobservs = 0_i8

        stub_ = .false.
        if (present(stub)) stub_ = stub

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

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NOBSERVS_BY_ID, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ1%node_id))   /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 2, trim(observ1%sensor_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 3, trim(observ1%target_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 4, trim(after))             /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 5, trim(after))             /= SQLITE_OK) exit sql_block

            if (present(before)) then
                if (sqlite3_bind_text(stmt, 6, trim(before)) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            nobs = sqlite3_column_int(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            rc = E_ALLOC
            if (present(limit)) nobs = min(nobs, limit)
            allocate (observs(nobs), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (nobs == 0) exit sql_block

            rc = E_DB_PREPARE
            if (present(limit)) then
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_OBSERVS_BY_ID // ' LIMIT ?', stmt) /= SQLITE_OK) exit sql_block
            else
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_OBSERVS_BY_ID, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ1%node_id))   /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 2, trim(observ1%sensor_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 3, trim(observ1%target_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 4, trim(after))             /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 5, trim(after))             /= SQLITE_OK) exit sql_block

            if (present(before)) then
                if (sqlite3_bind_text(stmt, 6, trim(before)) /= SQLITE_OK) exit sql_block
            end if

            if (present(limit)) then
                if (sqlite3_bind_int64(stmt, 7, limit) /= SQLITE_OK) exit sql_block
            end if

            do i = 1, nobs
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, observs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(nobservs)) nobservs = nobs
        end block sql_block

        stat = sqlite3_finalize(stmt)

        if (.not. allocated(observs)) allocate (observs(0))
        if (dm_is_error(rc)) return
        if (stub_) return
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

        integer          :: stat
        integer(kind=i8) :: i, nobs
        logical          :: stub_
        type(c_ptr)      :: stmt

        if (present(nobservs)) nobservs = 0_i8

        stub_ = .false.
        if (present(stub)) stub_ = stub

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NOBSERVS_BY_TIME, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id))   /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 2, trim(sensor_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 3, trim(target_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 4, trim(from))      /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 5, trim(to))        /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            nobs = sqlite3_column_int(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            rc = E_ALLOC
            if (present(limit)) nobs = min(nobs, limit)
            allocate (observs(nobs), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (nobs == 0) exit sql_block

            rc = E_DB_PREPARE
            if (present(limit)) then
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_OBSERVS_BY_TIME // ' LIMIT ?', stmt) /= SQLITE_OK) exit sql_block
            else
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_OBSERVS_BY_TIME, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id))   /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 2, trim(sensor_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 3, trim(target_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 4, trim(from))      /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 5, trim(to))        /= SQLITE_OK) exit sql_block

            if (present(limit)) then
                if (sqlite3_bind_int64(stmt, 6, limit) /= SQLITE_OK) exit sql_block
            end if

            do i = 1, nobs
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, observs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(nobservs)) nobservs = nobs
        end block sql_block

        stat = sqlite3_finalize(stmt)

        if (.not. allocated(observs)) allocate (observs(0))
        if (dm_is_error(rc)) return
        if (stub_) return
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
        !!
        use :: dm_sensor

        type(db_type),     intent(inout) :: db        !! Database type.
        type(sensor_type), intent(out)   :: sensor    !! Returned sensor data.
        character(len=*),  intent(in)    :: sensor_id !! Sensor id.

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_INVALID
        if (len_trim(sensor_id) == 0) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_SENSOR, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(sensor_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = db_next_row(stmt, sensor)
        end block sql_block

        stat = sqlite3_finalize(stmt)
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
        integer(kind=i8)                                    :: n

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
        integer(kind=i8)                                    :: n

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
        integer(kind=i8)                                    :: n

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
        integer(kind=i8)                                    :: n

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
        integer(kind=i8)                                    :: n

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

        character(len=SQL_TABLE_NAME_LEN) :: table
        integer                           :: i, n, stat
        type(c_ptr)                       :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_TABLES, stmt) /= SQLITE_OK) exit sql_block

            i = 1

            do
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (sqlite3_column_type(stmt, 0) /= SQLITE_INTEGER) exit
                    if (sqlite3_column_type(stmt, 1) /= SQLITE_TEXT) exit
                end if

                n     = sqlite3_column_int (stmt, 0)
                table = sqlite3_column_text(stmt, 1)

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

        stat = sqlite3_finalize(stmt)
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
        !!
        use :: dm_target

        type(db_type),     intent(inout) :: db        !! Database type.
        type(target_type), intent(out)   :: target    !! Returned target data.
        character(len=*),  intent(in)    :: target_id !! Target id.

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_INVALID
        if (len_trim(target_id) == 0) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_TARGET, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(target_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = db_next_row(stmt, target)
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_select_target

    logical function dm_db_sensor_exists(db, sensor_id) result(exists)
        !! Returns `.true.` if sensor id exists.
        type(db_type),     intent(inout) :: db        !! Database type.
        character(len=*),  intent(in)    :: sensor_id !! Sensor id.

        exists = db_exists(db, SQL_TABLE_SENSORS, sensor_id)
    end function dm_db_sensor_exists

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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, QUERY // dm_itoa(id), stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (mode < DB_AUTO_VACUUM_NONE .or. mode > DB_AUTO_VACUUM_INCREMENTAL) return

        sql_block: block
            rc = E_DB_PREPARE
            select case (mode)
                case (DB_AUTO_VACUUM_NONE)
                    stat = sqlite3_prepare_v2(db%ptr, QUERY // '0', stmt)
                case (DB_AUTO_VACUUM_FULL)
                    stat = sqlite3_prepare_v2(db%ptr, QUERY // '1', stmt)
                case (DB_AUTO_VACUUM_INCREMENTAL)
                    stat = sqlite3_prepare_v2(db%ptr, QUERY // '2', stmt)
            end select
            if (stat /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_set_auto_vacuum

    integer function dm_db_set_busy_handler(db, callback, client_data) result(rc)
        !! Sets SQLite busy callback that is invoked whenever the database is
        !! busy.
        !!
        !! The dummy argument `client_data` is passed to the callback function.
        !! The callback may return 0 to signal that no more invocations are
        !! desired.
        !!
        !! The function returns `E_DB` on error.
        type(db_type), intent(inout)  :: db          !! Database type.
        procedure(dm_db_busy_handler) :: callback    !! Callback function.
        type(c_ptr),   intent(in)     :: client_data !! C pointer to client data.

        rc = E_DB
        if (sqlite3_busy_handler(db%ptr, c_funloc(callback), client_data) /= SQLITE_OK) return
        rc = E_NONE
    end function dm_db_set_busy_handler

    integer function dm_db_set_busy_timeout(db, msec) result(rc)
        !! Sets SQLite busy timeout in msec. Returns `E_DB` on error.
        type(db_type), intent(inout) :: db   !! Database type.
        integer,       intent(in)    :: msec !! Timeout in mseconds.

        rc = E_DB
        if (sqlite3_busy_timeout(db%ptr, msec) /= SQLITE_OK) return
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

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (enabled) then
                stat = sqlite3_prepare_v2(db%ptr, QUERY // 'ON', stmt)
            else
                stat = sqlite3_prepare_v2(db%ptr, QUERY // 'OFF', stmt)
            end if
            if (stat /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        rc = E_INVALID
        if (mode < DB_JOURNAL_OFF .or. mode > DB_JOURNAL_WAL) return

        sql_block: block
            rc = E_DB_PREPARE

            select case (mode)
                case (DB_JOURNAL_OFF)
                    stat = sqlite3_prepare_v2(db%ptr, QUERY // 'OFF', stmt)
                case (DB_JOURNAL_DELETE)
                    stat = sqlite3_prepare_v2(db%ptr, QUERY // 'DELETE', stmt)
                case (DB_JOURNAL_TRUNCATE)
                    stat = sqlite3_prepare_v2(db%ptr, QUERY // 'TRUNCATE', stmt)
                case (DB_JOURNAL_PERSIST)
                    stat = sqlite3_prepare_v2(db%ptr, QUERY // 'PERSIST', stmt)
                case (DB_JOURNAL_MEMORY)
                    stat = sqlite3_prepare_v2(db%ptr, QUERY // 'MEMORY', stmt)
                case (DB_JOURNAL_WAL)
                    stat = sqlite3_prepare_v2(db%ptr, QUERY // 'WAL', stmt)
            end select

            if (stat /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_set_journal_mode

    integer function dm_db_set_log_handler(callback, client_data) result(rc)
        !! Sets SQLite error log callback. The dummy argument `client_data` is
        !! passed to the callback routine. The function returns `E_DB` on error.
        procedure(dm_db_log_handler)      :: callback    !! Callback routine.
        type(c_ptr), intent(in), optional :: client_data !! C pointer to client data.

        rc = E_DB
        if (present(client_data)) then
            if (sqlite3_config(SQLITE_CONFIG_LOG, c_funloc(callback), client_data) /= SQLITE_OK) return
        else
            if (sqlite3_config(SQLITE_CONFIG_LOG, c_funloc(callback), c_null_ptr) /= SQLITE_OK) return
        end if
        rc = E_NONE
    end function dm_db_set_log_handler

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

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (enabled) then
                stat = sqlite3_prepare_v2(db%ptr, QUERY // 'ON', stmt)
            else
                stat = sqlite3_prepare_v2(db%ptr, QUERY // 'OFF', stmt)
            end if
            if (stat /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_set_query_only

    integer function dm_db_set_update_handler(db, callback, client_data) result(rc)
        !! Sets SQLite error log callback. The dummy argument `client_data` is
        !! passed to the callback routine. The function returns `E_DB` on error.
        type(db_type), intent(inout)        :: db          !! Database type.
        procedure(dm_db_update_handler)     :: callback    !! Callback routine.
        type(c_ptr),   intent(in), optional :: client_data !! C pointer to client data.

        type(c_ptr) :: udp

        rc = E_DB
        if (present(client_data)) then
            udp = sqlite3_update_hook(db%ptr, c_funloc(callback), client_data)
        else
            udp = sqlite3_update_hook(db%ptr, c_funloc(callback), c_null_ptr)
        end if
        rc = E_NONE
    end function dm_db_set_update_handler

    integer function dm_db_set_user_version(db, version) result(rc)
        !! Sets database user version.
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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, QUERY // dm_itoa(version), stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_set_user_version

    integer function dm_db_shutdown() result(rc)
        !! Finalises SQLite handle. Returns `E_DB` on error.
        rc = E_DB
        if (sqlite3_shutdown() /= SQLITE_OK) return
        rc = E_NONE
    end function dm_db_shutdown

    logical function dm_db_threadsafe() result(safe)
        !! Returns true if SQLite 3 was compiled threadsafe.
        safe = (sqlite3_threadsafe() == SQLITE_OK)
    end function dm_db_threadsafe

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

        integer     :: stat
        logical     :: validate_
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate node.
        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_INVALID
            if (.not. dm_node_valid(node)) return
        end if

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_UPDATE_NODE, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            ! Node id must be last argument!
            if (sqlite3_bind_text  (stmt, 1, trim(node%name)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 2, trim(node%meta)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 3, node%x)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 4, node%y)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 5, node%z)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 6, trim(node%id))   /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        logical     :: validate_
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate sensor.
        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_INVALID
            if (.not. dm_sensor_valid(sensor)) return
        end if

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_UPDATE_SENSOR, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            ! Sensor id must be last argument!
            if (sqlite3_bind_text  (stmt, 1, trim(sensor%node_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int   (stmt, 2, sensor%type)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 3, trim(sensor%name))    /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 4, trim(sensor%sn))      /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 5, trim(sensor%meta))    /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 6, sensor%x)             /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 7, sensor%y)             /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 8, sensor%z)             /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 9, trim(sensor%id))      /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        logical     :: validate_
        type(c_ptr) :: stmt

        rc = E_READ_ONLY
        if (db%read_only) return

        ! Validate target.
        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_INVALID
            if (.not. dm_target_valid(target)) return
        end if

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_UPDATE_TARGET, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            ! Target id must be last argument!
            if (sqlite3_bind_text  (stmt, 1, trim(target%name)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 2, trim(target%meta)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int   (stmt, 3, target%state)      /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 4, target%x)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 5, target%y)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_double(stmt, 6, target%z)          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text  (stmt, 7, trim(target%id))   /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_update_target

    integer function dm_db_table_exists(db, table, exists) result(rc)
        !! Returns whether given table exists in database. The result code is
        !! `E_NONE` if the table has been found, else `E_DB_NO_ROWS`. The
        !! logical result is returned in `exists`. Pass the enumerator
        !! `SQL_TABLE_*` from `dm_sql`, for instance:
        !!
        !! ```fortran
        !! integer :: rc
        !! logical :: exists
        !!
        !! rc = dm_db_table_exists(db, SQL_TABLE_LOGS, exists)
        !! ```
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !! * `E_INVALID` if argument `table` is invalid.
        !!
        type(db_type), intent(inout)         :: db     !! Database type.
        integer,       intent(in)            :: table  !! Table enumerator.
        logical,       intent(out), optional :: exists !! Table exists.

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_INVALID
        if (present(exists)) exists = .false.
        if (table < SQL_TABLE_NODES .or. table > SQL_TABLE_LAST) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_TABLE, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(SQL_TABLE_NAMES(table))) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
        if (present(exists) .and. rc == E_NONE) exists = .true.
    end function dm_db_table_exists

    logical function dm_db_target_exists(db, target_id) result(exists)
        !! Returns `.true.` if target id exists.
        type(db_type),     intent(inout) :: db        !! Database type.
        character(len=*),  intent(in)    :: target_id !! Target id.

        exists = db_exists(db, SQL_TABLE_TARGETS, target_id)
    end function dm_db_target_exists

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

        character(len=*), parameter :: QUERY = "VACUUM 'main'"

        type(db_type),    intent(inout)        :: db   !! Database type.
        character(len=*), intent(in), optional :: into !! File path to vacuum database.

        integer     :: stat
        type(c_ptr) :: stmt

        if (present(into)) then
            rc = E_EXIST
            if (dm_file_exists(into)) return
        end if

        sql_block: block
            if (present(into)) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, QUERY // ' INTO ?', stmt) /= SQLITE_OK) exit sql_block

                rc = E_DB_BIND
                if (sqlite3_bind_text(stmt, 1, trim(into)) /= SQLITE_OK) exit sql_block
            else
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, QUERY, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function dm_db_vacuum

    integer function dm_db_valid(db) result(rc)
        !! Validates an opened DMPACK database. The application id must match
        !! the constant `DB_APPLICATION_ID`, and the user version must be equal
        !! to `DB_USER_VERSION`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_ID` if application id is missing or invalid.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !! * `E_DB_VERSION` if the user version is incompatible.
        !!
        type(db_type), intent(inout) :: db !! Database type.

        integer :: id, user_version

        rc = dm_db_get_application_id(db, id)
        if (dm_is_error(rc)) return

        rc = E_DB_ID
        if (id /= DB_APPLICATION_ID) return

        rc = dm_db_get_user_version(db, user_version)
        if (dm_is_error(rc)) return

        rc = E_DB_VERSION
        if (user_version /= DB_USER_VERSION) return

        rc = E_NONE
    end function dm_db_valid

    function dm_db_version(name) result(version)
        !! Returns SQLite 3 library version as allocatable string.
        logical, intent(in), optional :: name !! Add prefix `libsqlite/'.
        character(len=:), allocatable :: version

        logical :: name_

        name_ = .false.
        if (present(name)) name_ = name

        if (name_) then
            version = 'libsqlite3/' // sqlite3_libversion()
        else
            version = sqlite3_libversion()
        end if
    end function dm_db_version

    ! ******************************************************************
    ! PUBLIC SUBROUTINES.
    ! ******************************************************************
    subroutine dm_db_log(err_code, err_msg)
        !! Sends log message to SQLite error log handler. The callback has to
        !! be set through `dm_db_set_log_handler()` initially.
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

    ! ******************************************************************
    ! PRIVATE FUNCTIONS.
    ! ******************************************************************
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

        mode_ = DB_TRANS_IMMEDIATE
        if (present(mode)) mode_ = mode

        select case (mode_)
            case (DB_TRANS_DEFERRED)
                rc = db_exec(db, 'BEGIN')
            case (DB_TRANS_IMMEDIATE)
                rc = db_exec(db, 'BEGIN IMMEDIATE')
            case (DB_TRANS_EXCLUSIVE)
                rc = db_exec(db, 'BEGIN EXCLUSIVE')
            case default
                rc = E_INVALID
                return
        end select

        if (dm_is_error(rc)) rc = E_DB_TRANSACTION
    end function db_begin

    integer function db_commit(db) result(rc)
        !! Commits a transaction. Returns `E_DB_EXEC` on error.
        type(db_type), intent(inout) :: db !! Database type.

        rc = db_exec(db, 'COMMIT')
    end function db_commit

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
        character(len=*), parameter :: QUERY = 'SELECT COUNT(*) FROM '

        type(db_type),    intent(inout) :: db    !! Database type.
        integer,          intent(in)    :: table !! Table type from `dm_sql`.
        integer(kind=i8), intent(out)   :: n     !! Number of rows in table.

        integer     :: stat
        type(c_ptr) :: stmt

        n = 0_i8
        rc = E_INVALID
        if (table < SQL_TABLE_NODES .or. table > SQL_TABLE_LAST) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, QUERY // SQL_TABLE_NAMES(table), stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_INTEGER) exit sql_block
            n = sqlite3_column_int64(stmt, 0)

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_DELETE_RECEIVERS, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_DELETE_REQUESTS, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_DELETE_OBSERV_RESPONSES, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
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
        stat = sqlite3_exec(db%ptr, query, c_null_funptr, c_null_ptr, err_msg)

        if (stat /= SQLITE_OK) return
        rc = E_NONE
    end function db_exec

    logical function db_exists(db, table, id) result(exists)
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
        type(db_type),     intent(inout) :: db    !! Database type.
        integer,           intent(in)    :: table !! Enumerator of table to search.
        character(len=*),  intent(in)    :: id    !! Record id.

        integer     :: rc
        type(c_ptr) :: stmt

        exists = .false.

        sql_block: block
            select case (table)
                case (SQL_TABLE_LOGS)
                    rc = sqlite3_prepare_v2(db%ptr, SQL_EXISTS_LOG, stmt)
                case (SQL_TABLE_NODES)
                    rc = sqlite3_prepare_v2(db%ptr, SQL_EXISTS_NODE, stmt)
                case (SQL_TABLE_OBSERVS)
                    rc = sqlite3_prepare_v2(db%ptr, SQL_EXISTS_OBSERV, stmt)
                case (SQL_TABLE_SENSORS)
                    rc = sqlite3_prepare_v2(db%ptr, SQL_EXISTS_SENSOR, stmt)
                case (SQL_TABLE_TARGETS)
                    rc = sqlite3_prepare_v2(db%ptr, SQL_EXISTS_TARGET, stmt)
                case default
                    return
            end select

            if (rc /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 1, trim(id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            if (sqlite3_column_type(stmt, 0) /= SQLITE_INTEGER) exit sql_block
            if (sqlite3_column_int(stmt, 0) == 1) exists = .true.
        end block sql_block

        rc = sqlite3_finalize(stmt)
    end function db_exists

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

        integer     :: i, n, stat
        type(c_ptr) :: stmt

        rc = E_BOUNDS
        n = size(receivers)
        if (n > OBSERV_MAX_NRECEIVERS) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_INSERT_RECEIVER, stmt) /= SQLITE_OK) exit sql_block

            row_loop: do i = 1, n
                rc = E_INVALID
                if (.not. dm_id_valid(receivers(i))) exit row_loop

                rc = E_DB_BIND
                if (sqlite3_bind_text(stmt, 1, trim(observ_id))    /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int (stmt, 2, i)                  /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_text(stmt, 3, trim(receivers(i))) /= SQLITE_OK) exit row_loop

                rc = E_DB_STEP
                if (sqlite3_step(stmt) /= SQLITE_DONE) exit row_loop

                rc = E_DB
                if (sqlite3_reset(stmt) /= SQLITE_OK) exit row_loop

                rc = E_NONE
            end do row_loop
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: i, nreq, stat
        type(c_ptr) :: stmt

        rc = E_BOUNDS
        nreq = size(requests)
        if (nreq > OBSERV_MAX_NREQUESTS) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_INSERT_REQUEST, stmt) /= SQLITE_OK) exit sql_block

            row_loop: do i = 1, nreq
                rc = E_DB_BIND
                if (sqlite3_bind_text(stmt,  1, trim(observ_id))             /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int (stmt,  2, i)                           /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_text(stmt,  3, trim(requests(i)%name))      /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_text(stmt,  4, trim(requests(i)%timestamp)) /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_text(stmt,  5, trim(requests(i)%request))   /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_text(stmt,  6, trim(requests(i)%response))  /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_text(stmt,  7, trim(requests(i)%delimiter)) /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_text(stmt,  8, trim(requests(i)%pattern))   /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int (stmt,  9, requests(i)%delay)           /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int (stmt, 10, requests(i)%error)           /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int (stmt, 11, requests(i)%mode)            /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int (stmt, 12, requests(i)%retries)         /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int (stmt, 13, requests(i)%state)           /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int (stmt, 14, requests(i)%timeout)         /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int (stmt, 15, requests(i)%nresponses)      /= SQLITE_OK) exit row_loop

                rc = E_DB_STEP
                if (sqlite3_step(stmt) /= SQLITE_DONE) exit row_loop

                rc = E_DB
                if (sqlite3_reset(stmt) /= SQLITE_OK) exit row_loop

                rc = E_NONE
            end do row_loop
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: i, nres, stat
        type(c_ptr) :: stmt

        rc = E_BOUNDS
        if (request_idx < 1 .or. request_idx > OBSERV_MAX_NREQUESTS) return

        nres = size(responses)
        if (nres > REQUEST_MAX_NRESPONSES) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_INSERT_RESPONSE, stmt) /= SQLITE_OK) exit sql_block

            row_loop: do i = 1, nres
                rc = E_DB_BIND
                if (sqlite3_bind_text  (stmt, 1, trim(observ_id))         /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int   (stmt, 2, request_idx)             /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int   (stmt, 3, i)                       /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_text  (stmt, 4, trim(responses(i)%name)) /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_text  (stmt, 5, trim(responses(i)%unit)) /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int   (stmt, 6, responses(i)%type)       /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_int   (stmt, 7, responses(i)%error)      /= SQLITE_OK) exit row_loop
                if (sqlite3_bind_double(stmt, 8, responses(i)%value)      /= SQLITE_OK) exit row_loop

                rc = E_DB_STEP
                if (sqlite3_step(stmt) /= SQLITE_DONE) exit row_loop

                rc = E_DB
                if (sqlite3_reset(stmt) /= SQLITE_OK) exit row_loop

                rc = E_NONE
            end do row_loop
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer     :: stat
        type(c_ptr) :: stmt

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, query, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(sync%id))        /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 2, trim(sync%timestamp)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 3, sync%code)            /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 4, sync%attempts)        /= SQLITE_OK) exit sql_block

            rc = E_DB_STEP
            if (sqlite3_step(stmt) /= SQLITE_DONE) exit sql_block

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function db_insert_sync

    integer function db_next_row_allocatable(stmt, str, validate) result(rc)
        !! Reads string from table row and returns it as allocatable character
        !! string. Column types are validated by default. Returns `E_DB_TYPE`
        !! if the validation failed.
        type(c_ptr),                   intent(inout)        :: stmt     !! SQLite statement.
        character(len=:), allocatable, intent(out)          :: str      !! Allocatable character string.
        logical,                       intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT) then
                str = ''
                return
            end if
        end if

        str = sqlite3_column_text(stmt, 0)
        rc = E_NONE
    end function db_next_row_allocatable

    integer function db_next_row_character(stmt, str, nbytes, validate) result(rc)
        !! Reads string from table row. The passed argument `str` must be
        !! allocated! Column types are validated by default. Returns
        !! `E_DB_TYPE` if the validation failed.
        type(c_ptr),      intent(inout)        :: stmt     !! SQLite statement.
        character(len=*), intent(inout)        :: str      !! Character string.
        integer,          intent(out)          :: nbytes   !! Size of string in bytes.
        logical,          intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        nbytes = 0

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT) then
                str = ''
                return
            end if
        end if

        nbytes = sqlite3_column_bytes(stmt, 0)
        str    = sqlite3_column_text(stmt, 0)

        rc = E_NONE
    end function db_next_row_character

    integer function db_next_row_beat(stmt, beat, validate) result(rc)
        !! Reads beat data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_beat

        type(c_ptr),     intent(inout)        :: stmt     !! SQLite statement.
        type(beat_type), intent(inout)        :: beat     !! Beat type.
        logical,         intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 1) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 2) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 3) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 4) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 5) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 6) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 7) /= SQLITE_INTEGER) return
        end if

        beat%node_id   = sqlite3_column_text(stmt, 0)
        beat%address   = sqlite3_column_text(stmt, 1)
        beat%client    = sqlite3_column_text(stmt, 2)
        beat%time_sent = sqlite3_column_text(stmt, 3)
        beat%time_recv = sqlite3_column_text(stmt, 4)
        beat%error     = sqlite3_column_int (stmt, 5)
        beat%interval  = sqlite3_column_int (stmt, 6)
        beat%uptime    = sqlite3_column_int (stmt, 7)

        rc = E_NONE
    end function db_next_row_beat

    integer function db_next_row_data_point(stmt, dp, validate) result(rc)
        !! Reads observation data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_dp

        type(c_ptr),   intent(inout)        :: stmt     !! SQLite statement.
        type(dp_type), intent(inout)        :: dp       !! Data point type.
        logical,       intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT)  return
            if (sqlite3_column_type(stmt, 1) /= SQLITE_FLOAT) return
        end if

        dp%x = sqlite3_column_text  (stmt, 0)
        dp%y = sqlite3_column_double(stmt, 1)

        rc = E_NONE
    end function db_next_row_data_point

    integer function db_next_row_log(stmt, log, validate) result(rc)
        !! Reads log data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_log

        type(c_ptr),    intent(inout)        :: stmt     !! SQLite statement.
        type(log_type), intent(inout)        :: log      !! Log type.
        logical,        intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 1) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 2) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 3) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 4) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 5) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 6) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 7) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 8) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 9) /= SQLITE_TEXT)    return
        end if

        log%id        = sqlite3_column_text(stmt, 0)
        log%level     = sqlite3_column_int (stmt, 1)
        log%error     = sqlite3_column_int (stmt, 2)
        log%timestamp = sqlite3_column_text(stmt, 3)
        log%node_id   = sqlite3_column_text(stmt, 4)
        log%sensor_id = sqlite3_column_text(stmt, 5)
        log%target_id = sqlite3_column_text(stmt, 6)
        log%observ_id = sqlite3_column_text(stmt, 7)
        log%source    = sqlite3_column_text(stmt, 8)
        log%message   = sqlite3_column_text(stmt, 9)

        rc = E_NONE
    end function db_next_row_log

    integer function db_next_row_node(stmt, node, validate) result(rc)
        !! Reads node data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_node

        type(c_ptr),     intent(inout)        :: stmt     !! SQLite statement.
        type(node_type), intent(inout)        :: node     !! Node type.
        logical,         intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT)  return
            if (sqlite3_column_type(stmt, 1) /= SQLITE_TEXT)  return
            if (sqlite3_column_type(stmt, 2) /= SQLITE_TEXT)  return
            if (sqlite3_column_type(stmt, 3) /= SQLITE_FLOAT) return
            if (sqlite3_column_type(stmt, 4) /= SQLITE_FLOAT) return
            if (sqlite3_column_type(stmt, 5) /= SQLITE_FLOAT) return
        end if

        node%id   = sqlite3_column_text  (stmt, 0)
        node%name = sqlite3_column_text  (stmt, 1)
        node%meta = sqlite3_column_text  (stmt, 2)
        node%x    = sqlite3_column_double(stmt, 3)
        node%y    = sqlite3_column_double(stmt, 4)
        node%z    = sqlite3_column_double(stmt, 5)

        rc = E_NONE
    end function db_next_row_node

    integer function db_next_row_observ(stmt, observ, validate) result(rc)
        !! Reads observation data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_observ

        type(c_ptr),       intent(inout)        :: stmt     !! SQLite statement.
        type(observ_type), intent(inout)        :: observ   !! Observation type.
        logical,           intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt,  0) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  1) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  2) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  3) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  4) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  5) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  6) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  7) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  8) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt,  9) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 10) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 11) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 12) /= SQLITE_INTEGER) return
        end if

        observ%id         = sqlite3_column_text(stmt,  0)
        observ%node_id    = sqlite3_column_text(stmt,  1)
        observ%sensor_id  = sqlite3_column_text(stmt,  2)
        observ%target_id  = sqlite3_column_text(stmt,  3)
        observ%name       = sqlite3_column_text(stmt,  4)
        observ%timestamp  = sqlite3_column_text(stmt,  5)
        observ%source     = sqlite3_column_text(stmt,  6)
        observ%device     = sqlite3_column_text(stmt,  7)
        observ%priority   = sqlite3_column_int (stmt,  8)
        observ%error      = sqlite3_column_int (stmt,  9)
        observ%next       = sqlite3_column_int (stmt, 10)
        observ%nreceivers = sqlite3_column_int (stmt, 11)
        observ%nrequests  = sqlite3_column_int (stmt, 12)

        rc = E_NONE
    end function db_next_row_observ

    integer function db_next_row_observ_view(stmt, view, validate) result(rc)
        !! Reads observation data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_observ

        type(c_ptr),            intent(inout)        :: stmt     !! SQLite statement.
        type(observ_view_type), intent(inout)        :: view     !! Observation view type.
        logical,                intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt,  0) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  1) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  2) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  3) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  4) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  5) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt,  6) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  7) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt,  8) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt,  9) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 10) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 11) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 12) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 13) /= SQLITE_FLOAT)   return
        end if

        view%observ_id         = sqlite3_column_text  (stmt,  0)
        view%node_id           = sqlite3_column_text  (stmt,  1)
        view%sensor_id         = sqlite3_column_text  (stmt,  2)
        view%target_id         = sqlite3_column_text  (stmt,  3)
        view%observ_name       = sqlite3_column_text  (stmt,  4)
        view%observ_error      = sqlite3_column_int   (stmt,  5)
        view%request_name      = sqlite3_column_text  (stmt,  6)
        view%request_timestamp = sqlite3_column_text  (stmt,  7)
        view%request_error     = sqlite3_column_int   (stmt,  8)
        view%response_name     = sqlite3_column_text  (stmt,  9)
        view%response_unit     = sqlite3_column_text  (stmt, 10)
        view%response_type     = sqlite3_column_int   (stmt, 11)
        view%response_error    = sqlite3_column_int   (stmt, 12)
        view%response_value    = sqlite3_column_double(stmt, 13)

        rc = E_NONE
    end function db_next_row_observ_view

    integer function db_next_row_sensor(stmt, sensor, validate) result(rc)
        !! Reads sensor data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_sensor

        type(c_ptr),       intent(inout)        :: stmt     !! SQLite statement.
        type(sensor_type), intent(inout)        :: sensor   !! Sensor type.
        logical,           intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 1) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 2) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 3) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 4) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 5) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 6) /= SQLITE_FLOAT)   return
            if (sqlite3_column_type(stmt, 7) /= SQLITE_FLOAT)   return
            if (sqlite3_column_type(stmt, 8) /= SQLITE_FLOAT)   return
        end if

        sensor%id      = sqlite3_column_text  (stmt, 0)
        sensor%node_id = sqlite3_column_text  (stmt, 1)
        sensor%type    = sqlite3_column_int   (stmt, 2)
        sensor%name    = sqlite3_column_text  (stmt, 3)
        sensor%sn      = sqlite3_column_text  (stmt, 4)
        sensor%meta    = sqlite3_column_text  (stmt, 5)
        sensor%x       = sqlite3_column_double(stmt, 6)
        sensor%y       = sqlite3_column_double(stmt, 7)
        sensor%z       = sqlite3_column_double(stmt, 8)

        rc = E_NONE
    end function db_next_row_sensor

    integer function db_next_row_string(stmt, string, validate) result(rc)
        !! Reads string from table row and returns it as derived type
        !! `string_type`. Column types are validated by default. Returns
        !! `E_DB_TYPE` if the validation failed.
        use :: dm_string, only: string_type

        type(c_ptr),       intent(inout)        :: stmt     !! SQLite statement.
        type(string_type), intent(out)          :: string   !! String type.
        logical,           intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT) then
                string%data = ''
                return
            end if
        end if

        string%data = sqlite3_column_text(stmt, 0)
        rc = E_NONE
    end function db_next_row_string

    integer function db_next_row_sync(stmt, sync) result(rc)
        !! Reads sync data from table row. Returns `E_DB_TYPE` on error.
        use :: dm_sync

        type(c_ptr),     intent(inout) :: stmt !! SQLite statement.
        type(sync_type), intent(inout) :: sync !! Sync type.

        rc = E_DB_TYPE
        if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT) return

        sync%id = sqlite3_column_text(stmt, 0)

        if (sqlite3_column_type(stmt, 1) == SQLITE_TEXT) then
            sync%timestamp = sqlite3_column_text(stmt, 1)
        else
            sync%timestamp = TIME_DEFAULT
        end if

        if (sqlite3_column_type(stmt, 2) == SQLITE_INTEGER) then
            sync%code = sqlite3_column_int(stmt, 2)
        else
            sync%code = 0
        end if

        if (sqlite3_column_type(stmt, 3) == SQLITE_INTEGER) then
            sync%attempts = sqlite3_column_int(stmt, 3)
        else
            sync%attempts = 0
        end if

        rc = E_NONE
    end function db_next_row_sync

    integer function db_next_row_target(stmt, target, validate) result(rc)
        !! Reads target data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_target

        type(c_ptr),       intent(inout)        :: stmt     !! SQLite statement.
        type(target_type), intent(inout)        :: target   !! Target type.
        logical,           intent(in), optional :: validate !! Validate column types.

        logical :: validate_

        validate_ = .true.
        if (present(validate)) validate_ = validate

        if (validate_) then
            rc = E_DB_TYPE
            if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 1) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 2) /= SQLITE_TEXT)    return
            if (sqlite3_column_type(stmt, 3) /= SQLITE_INTEGER) return
            if (sqlite3_column_type(stmt, 4) /= SQLITE_FLOAT)   return
            if (sqlite3_column_type(stmt, 5) /= SQLITE_FLOAT)   return
            if (sqlite3_column_type(stmt, 6) /= SQLITE_FLOAT)   return
        end if

        target%id    = sqlite3_column_text  (stmt, 0)
        target%name  = sqlite3_column_text  (stmt, 1)
        target%meta  = sqlite3_column_text  (stmt, 2)
        target%state = sqlite3_column_int   (stmt, 3)
        target%x     = sqlite3_column_double(stmt, 4)
        target%y     = sqlite3_column_double(stmt, 5)
        target%z     = sqlite3_column_double(stmt, 6)

        rc = E_NONE
    end function db_next_row_target

    integer function db_release(db, name) result(rc)
        !! Jumps back to a save point. Returns `E_DB_EXEC` on error.
        type(db_type),    intent(inout) :: db   !! Database type.
        character(len=*), intent(in)    :: name !! Save point name.

        rc = db_exec(db, 'RELEASE "' // trim(name) // '"')
    end function db_release

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

        integer          :: stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        if (present(nbeats)) nbeats = 0_i8

        alloc_block: block
            rc = db_select_nrows(db, SQL_TABLE_BEATS, n)
            if (dm_is_error(rc)) exit alloc_block

            if (present(limit)) n = min(n, limit)

            rc = E_ALLOC
            allocate (beats(n), stat=stat)
            if (stat /= 0) exit alloc_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit alloc_block

            sql_block: block
                if (present(limit)) then
                    rc = E_DB_PREPARE
                    if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_BEATS // ' LIMIT ?', stmt) /= SQLITE_OK) exit sql_block

                    rc = E_DB_BIND
                    if (sqlite3_bind_int64(stmt, 1, limit) /= SQLITE_OK) exit sql_block
                else
                    rc = E_DB_PREPARE
                    if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_BEATS, stmt) /= SQLITE_OK) exit sql_block
                end if

                do i = 1, n
                    rc = E_DB_NO_ROWS
                    if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                    rc = db_next_row(stmt, beats(i), (i == 1))
                    if (dm_is_error(rc)) exit sql_block
                end do

                if (present(nbeats)) nbeats = n
            end block sql_block

            stat = sqlite3_finalize(stmt)
        end block alloc_block

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

        if (.not. dm_db_prepared(db_stmt)) then
            if (present(limit)) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_BEATS // ' LIMIT ?', db_stmt%ptr) /= SQLITE_OK) return

                rc = E_DB_BIND
                if (sqlite3_bind_int64(db_stmt%ptr, 1, limit) /= SQLITE_OK) return
            else
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_BEATS, db_stmt%ptr) /= SQLITE_OK) return
            end if
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, beat)
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

        integer          :: error_, stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        error_ = E_NONE
        if (present(error)) error_ = error

        if (present(npoints)) npoints = 0_i8

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NDATA_POINTS, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id))       /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 2, trim(sensor_id))     /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 3, trim(target_id))     /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 4, trim(response_name)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 5, error_)              /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 6, trim(from))          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 7, trim(to))            /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            n = sqlite3_column_int64(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            rc = E_ALLOC
            if (present(limit)) n = min(n, limit)
            allocate (dps(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            rc = E_DB_PREPARE
            if (present(limit)) then
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_DATA_POINTS // ' LIMIT ?', stmt) /= SQLITE_OK) exit sql_block
            else
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_DATA_POINTS, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id))       /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 2, trim(sensor_id))     /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 3, trim(target_id))     /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 4, trim(response_name)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 5, error_)              /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 6, trim(from))          /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_text(stmt, 7, trim(to))            /= SQLITE_OK) exit sql_block

            if (present(limit)) then
                if (sqlite3_bind_int64(stmt, 8, limit) /= SQLITE_OK) exit sql_block
            end if

            do i = 1, n
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, dps(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(npoints)) npoints = n
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer :: error_

        error_ = E_NONE
        if (present(error)) error_ = error

        if (.not. dm_db_prepared(db_stmt)) then
            rc = E_DB_PREPARE
            if (present(limit)) then
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_DATA_POINTS // ' LIMIT ?', db_stmt%ptr) /= SQLITE_OK) return
            else
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_DATA_POINTS, db_stmt%ptr) /= SQLITE_OK) return
            end if

            rc = E_DB_BIND
            if (sqlite3_bind_text(db_stmt%ptr, 1, trim(node_id))       /= SQLITE_OK) return
            if (sqlite3_bind_text(db_stmt%ptr, 2, trim(sensor_id))     /= SQLITE_OK) return
            if (sqlite3_bind_text(db_stmt%ptr, 3, trim(target_id))     /= SQLITE_OK) return
            if (sqlite3_bind_text(db_stmt%ptr, 4, trim(response_name)) /= SQLITE_OK) return
            if (sqlite3_bind_int (db_stmt%ptr, 5, error_)              /= SQLITE_OK) return
            if (sqlite3_bind_text(db_stmt%ptr, 6, trim(from))          /= SQLITE_OK) return
            if (sqlite3_bind_text(db_stmt%ptr, 7, trim(to))            /= SQLITE_OK) return

            if (present(limit)) then
                if (sqlite3_bind_int64(db_stmt%ptr, 8, limit) /= SQLITE_OK) return
            end if
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, dp)
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

        integer          :: stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        if (present(nbeats)) nbeats = 0_i8

        alloc_block: block
            rc = db_select_nrows(db, SQL_TABLE_BEATS, n)
            if (dm_is_error(rc)) exit alloc_block

            if (present(limit)) n = min(n, limit)

            rc = E_ALLOC
            allocate (strings(n), stat=stat)
            if (stat /= 0) exit alloc_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit alloc_block

            sql_block: block
                if (present(limit)) then
                    rc = E_DB_PREPARE
                    if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_BEATS // ' LIMIT ?', stmt) /= SQLITE_OK) exit sql_block

                    rc = E_DB_BIND
                    if (sqlite3_bind_int64(stmt, 1, n) /= SQLITE_OK) exit sql_block
                else
                    rc = E_DB_PREPARE
                    if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_BEATS, stmt) /= SQLITE_OK) exit sql_block
                end if

                do i = 1, n
                    rc = E_DB_NO_ROWS
                    if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                    rc = db_next_row(stmt, strings(i), (i == 1))
                    if (dm_is_error(rc)) exit sql_block
                end do

                if (present(nbeats)) nbeats = n
                rc = E_NONE
            end block sql_block

            stat = sqlite3_finalize(stmt)
        end block alloc_block

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

        if (.not. dm_db_prepared(db_stmt)) then
            if (present(limit)) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_BEATS // ' LIMIT ?', db_stmt%ptr) /= SQLITE_OK) return

                rc = E_DB_BIND
                if (sqlite3_bind_int64(db_stmt%ptr, 1, limit) /= SQLITE_OK) return
            else
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_BEATS, db_stmt%ptr) /= SQLITE_OK) return
            end if
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, json)
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

        character(len=:), allocatable :: query
        integer                       :: k, stat
        integer(kind=i8)              :: i, n
        type(c_ptr)                   :: stmt

        logical :: has_param, has_node_id, has_sensor_id, has_target_id, has_source
        logical :: has_from, has_to, has_min_level, has_max_level, has_error, has_limit
        logical :: desc_order, more

        has_param     = .false.; has_node_id   = .false.; has_sensor_id = .false.
        has_target_id = .false.; has_source    = .false.; has_from      = .false.
        has_to        = .false.; has_min_level = .false.; has_max_level = .false.
        has_error     = .false.; has_limit     = .false.; desc_order    = .false.

        if (dm_string_is_present(node_id)) then
            has_param = .true.
            has_node_id = .true.
        end if

        if (dm_string_is_present(sensor_id)) then
            has_param = .true.
            has_sensor_id = .true.
        end if

        if (dm_string_is_present(target_id)) then
            has_param = .true.
            has_target_id = .true.
        end if

        if (dm_string_is_present(source)) then
            has_param = .true.
            has_source = .true.
        end if

        if (dm_string_is_present(from)) then
            has_param = .true.
            has_from = .true.
        end if

        if (dm_string_is_present(to)) then
            has_param = .true.
            has_to = .true.
        end if

        if (present(min_level)) then
            has_param = .true.
            has_min_level = .true.
        end if

        if (present(max_level)) then
            has_param = .true.
            has_max_level = .true.
        end if

        if (present(error)) then
            has_param = .true.
            has_error = .true.
        end if

        if (present(desc)) desc_order = desc
        if (present(limit)) has_limit = .true.
        if (present(nlogs)) nlogs = 0_i8

        ! Build SQL query.
        allocate (character(len=0) :: query)

        if (has_param) then
            more = .false.
            if (has_min_level) call db_query_where(query, 'level >= ?',     more)
            if (has_max_level) call db_query_where(query, 'level <= ?',     more)
            if (has_error)     call db_query_where(query, 'error = ?',      more)
            if (has_from)      call db_query_where(query, 'timestamp >= ?', more)
            if (has_to)        call db_query_where(query, 'timestamp < ?',  more)
            if (has_node_id)   call db_query_where(query, 'node_id = ?',    more)
            if (has_sensor_id) call db_query_where(query, 'sensor_id = ?',  more)
            if (has_target_id) call db_query_where(query, 'target_id = ?',  more)
            if (has_source)    call db_query_where(query, 'source = ?',     more)
        end if

        sql_block: block
            if (has_param) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NLOGS // query, stmt) /= SQLITE_OK) exit sql_block

                rc = db_bind_logs(k)
                if (dm_is_error(rc)) exit sql_block
            else
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NLOGS, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            n = sqlite3_column_int64(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            if (has_limit) n = min(n, limit)

            rc = E_ALLOC
            allocate (strings(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            if (desc_order) then
                query = query // ' ORDER BY timestamp DESC'
            else
                query = query // ' ORDER BY timestamp ASC'
            end if

            if (has_limit) query = query // ' LIMIT ?'

            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_LOGS // query, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            k  = 1

            if (has_param) then
                ! Bind query parameters.
                if (dm_is_error(db_bind_logs(k))) exit sql_block
            end if

            if (has_limit) then
                ! Bind limit.
                if (sqlite3_bind_int64(stmt, k, n) /= SQLITE_OK) exit sql_block
            end if

            do i = 1, n
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(nlogs)) nlogs = n
            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
        if (.not. allocated(strings)) allocate (strings(0))
    contains
        integer function db_bind_logs(i) result(rc)
            integer, intent(out) :: i

            rc = E_DB_BIND
            i = 1

            if (has_min_level) then
                if (sqlite3_bind_int(stmt, i, min_level) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_max_level) then
                if (sqlite3_bind_int(stmt, i, max_level) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_error) then
                if (sqlite3_bind_int(stmt, i, error) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_from) then
                if (sqlite3_bind_text(stmt, i, trim(from)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_to) then
                if (sqlite3_bind_text(stmt, i, trim(to)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_node_id) then
                if (sqlite3_bind_text(stmt, i, trim(node_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_sensor_id) then
                if (sqlite3_bind_text(stmt, i, trim(sensor_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_target_id) then
                if (sqlite3_bind_text(stmt, i, trim(target_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_source) then
                if (sqlite3_bind_text(stmt, i, trim(source)) /= SQLITE_OK) return
                i = i + 1
            end if

            rc = E_NONE
        end function db_bind_logs
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

        character(len=:), allocatable :: query
        integer                       :: k

        logical :: has_param, has_node_id, has_sensor_id, has_target_id, has_source
        logical :: has_from, has_to, has_min_level, has_max_level, has_error, has_limit
        logical :: desc_order, more

        if (.not. dm_db_prepared(db_stmt)) then
            has_param     = .false.; has_node_id   = .false.; has_sensor_id = .false.
            has_target_id = .false.; has_source    = .false.; has_from      = .false.
            has_to        = .false.; has_min_level = .false.; has_max_level = .false.
            has_error     = .false.; has_limit     = .false.; desc_order    = .false.

            if (dm_string_is_present(node_id)) then
                has_param = .true.
                has_node_id = .true.
            end if

            if (dm_string_is_present(sensor_id)) then
                has_param = .true.
                has_sensor_id = .true.
            end if

            if (dm_string_is_present(target_id)) then
                has_param = .true.
                has_target_id = .true.
            end if

            if (dm_string_is_present(source)) then
                has_param = .true.
                has_source = .true.
            end if

            if (dm_string_is_present(from)) then
                has_param = .true.
                has_from = .true.
            end if

            if (dm_string_is_present(to)) then
                has_param = .true.
                has_to = .true.
            end if

            if (present(min_level)) then
                has_param = .true.
                has_min_level = .true.
            end if

            if (present(max_level)) then
                has_param = .true.
                has_max_level = .true.
            end if

            if (present(error)) then
                has_param = .true.
                has_error = .true.
            end if

            if (present(desc)) desc_order = desc
            if (present(limit)) has_limit = .true.

            ! Build SQL query.
            allocate (character(len=0) :: query)

            if (has_param) then
                more = .false.
                if (has_min_level) call db_query_where(query, 'level >= ?',     more)
                if (has_max_level) call db_query_where(query, 'level <= ?',     more)
                if (has_error)     call db_query_where(query, 'error = ?',      more)
                if (has_from)      call db_query_where(query, 'timestamp >= ?', more)
                if (has_to)        call db_query_where(query, 'timestamp < ?',  more)
                if (has_node_id)   call db_query_where(query, 'node_id = ?',    more)
                if (has_sensor_id) call db_query_where(query, 'sensor_id = ?',  more)
                if (has_target_id) call db_query_where(query, 'target_id = ?',  more)
                if (has_source)    call db_query_where(query, 'source = ?',     more)
            end if

            if (desc_order) then
                query = query // ' ORDER BY timestamp DESC'
            else
                query = query // ' ORDER BY timestamp ASC'
            end if

            if (has_limit) query = query // ' LIMIT ?'

            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_LOGS // query, db_stmt%ptr) /= SQLITE_OK) return

            rc = E_DB_BIND
            k  = 1

            if (has_param) then
                ! Bind query parameters.
                if (dm_is_error(db_bind_logs(k))) return
            end if

            if (has_limit) then
                ! Bind limit.
                if (sqlite3_bind_int64(db_stmt%ptr, k, limit) /= SQLITE_OK) return
            end if
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, json)
    contains
        integer function db_bind_logs(i) result(rc)
            integer, intent(out) :: i

            rc = E_DB_BIND
            i = 1

            if (has_min_level) then
                if (sqlite3_bind_int(db_stmt%ptr, i, min_level) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_max_level) then
                if (sqlite3_bind_int(db_stmt%ptr, i, max_level) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_error) then
                if (sqlite3_bind_int(db_stmt%ptr, i, error) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_from) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(from)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_to) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(to)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_node_id) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(node_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_sensor_id) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(sensor_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_target_id) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(target_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_source) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(source)) /= SQLITE_OK) return
                i = i + 1
            end if

            rc = E_NONE
        end function db_bind_logs
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

        character(len=*), parameter :: QUERY = ' ORDER BY nodes.node_id ASC'

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(string_type), allocatable, intent(out)           :: strings(:) !! Returned JSON array.
        integer(kind=i8),               intent(in),  optional :: limit      !! Max. number of nodes.
        integer(kind=i8),               intent(out), optional :: nnodes     !! Number of nodes.

        integer          :: stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        if (present(nnodes)) nnodes = 0_i8

        alloc_block: block
            rc = db_select_nrows(db, SQL_TABLE_NODES, n)
            if (dm_is_error(rc)) exit alloc_block

            if (present(limit)) n = min(n, limit)

            rc = E_ALLOC
            allocate (strings(n), stat=stat)
            if (stat /= 0) exit alloc_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit alloc_block

            sql_block: block
                if (present(limit)) then
                    rc = E_DB_PREPARE
                    if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_NODES // QUERY // ' LIMIT ?', stmt) /= SQLITE_OK) exit sql_block

                    rc = E_DB_BIND
                    if (sqlite3_bind_int64(stmt, 1, n) /= SQLITE_OK) exit sql_block
                else
                    rc = E_DB_PREPARE
                    if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_NODES // QUERY, stmt) /= SQLITE_OK) exit sql_block
                end if

                do i = 1, n
                    rc = E_DB_NO_ROWS
                    if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                    rc = db_next_row(stmt, strings(i), (i == 1))
                    if (dm_is_error(rc)) exit sql_block
                end do

                if (present(nnodes)) nnodes = n
                rc = E_NONE
            end block sql_block

            stat = sqlite3_finalize(stmt)
        end block alloc_block

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
        character(len=*), parameter :: QUERY = ' ORDER BY nodes.node_id ASC'

        type(db_type),                 intent(inout)        :: db      !! Database type.
        type(db_stmt_type),            intent(inout)        :: db_stmt !! Database statement type.
        character(len=:), allocatable, intent(out)          :: json    !! Returned JSON.
        integer(kind=i8),              intent(in), optional :: limit   !! Max. number of nodes.

        if (.not. dm_db_prepared(db_stmt)) then
            if (present(limit)) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_NODES // QUERY // ' LIMIT ?', db_stmt%ptr) /= SQLITE_OK) return

                rc = E_DB_BIND
                if (sqlite3_bind_int64(db_stmt%ptr, 1, limit) /= SQLITE_OK) return
            else
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_JSON_NODES // QUERY, db_stmt%ptr) /= SQLITE_OK) return
            end if
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, json)
    end function db_select_json_nodes_iter

    integer function db_select_logs_array(db, logs, node_id, sensor_id, target_id, source, from, to, &
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
        character(len=*),            intent(in),  optional :: source    !! Source name.
        character(len=*),            intent(in),  optional :: from      !! Begin of time range.
        character(len=*),            intent(in),  optional :: to        !! End of time range.
        integer,                     intent(in),  optional :: min_level !! Minimum log level.
        integer,                     intent(in),  optional :: max_level !! Maximum log level.
        integer,                     intent(in),  optional :: error     !! Error code.
        logical,                     intent(in),  optional :: desc      !! Descending order.
        integer(kind=i8),            intent(in),  optional :: limit     !! Max. numbers of logs.
        integer(kind=i8),            intent(out), optional :: nlogs     !! Total number of logs.

        character(len=:), allocatable :: query
        integer                       :: k, stat
        integer(kind=i8)              :: i, n
        type(c_ptr)                   :: stmt

        logical :: has_param, has_node_id, has_sensor_id, has_target_id, has_source
        logical :: has_from, has_to, has_min_level, has_max_level, has_error, has_limit
        logical :: desc_order, more

        has_param     = .false.; has_node_id   = .false.; has_sensor_id = .false.
        has_target_id = .false.; has_source    = .false.; has_from      = .false.
        has_to        = .false.; has_min_level = .false.; has_max_level = .false.
        has_error     = .false.; has_limit     = .false.; desc_order    = .false.

        if (dm_string_is_present(node_id)) then
            has_param = .true.
            has_node_id = .true.
        end if

        if (dm_string_is_present(sensor_id)) then
            has_param = .true.
            has_sensor_id = .true.
        end if

        if (dm_string_is_present(target_id)) then
            has_param = .true.
            has_target_id = .true.
        end if

        if (dm_string_is_present(source)) then
            has_param = .true.
            has_source = .true.
        end if

        if (dm_string_is_present(from)) then
            has_param = .true.
            has_from = .true.
        end if

        if (dm_string_is_present(to)) then
            has_param = .true.
            has_to = .true.
        end if

        if (present(min_level)) then
            has_param = .true.
            has_min_level = .true.
        end if

        if (present(max_level)) then
            has_param = .true.
            has_max_level = .true.
        end if

        if (present(error)) then
            has_param = .true.
            has_error = .true.
        end if

        if (present(desc)) desc_order = desc
        if (present(limit)) has_limit = .true.
        if (present(nlogs)) nlogs = 0_i8

        ! Build SQL query.
        allocate (character(len=0) :: query)

        if (has_param) then
            more = .false.
            if (has_min_level) call db_query_where(query, 'level >= ?',     more)
            if (has_max_level) call db_query_where(query, 'level <= ?',     more)
            if (has_error)     call db_query_where(query, 'error = ?',      more)
            if (has_from)      call db_query_where(query, 'timestamp >= ?', more)
            if (has_to)        call db_query_where(query, 'timestamp < ?',  more)
            if (has_node_id)   call db_query_where(query, 'node_id = ?',    more)
            if (has_sensor_id) call db_query_where(query, 'sensor_id = ?',  more)
            if (has_target_id) call db_query_where(query, 'target_id = ?',  more)
            if (has_source)    call db_query_where(query, 'source = ?',     more)
        end if

        sql_block: block
            if (has_param) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NLOGS // query, stmt) /= SQLITE_OK) exit sql_block

                rc = db_bind_logs(k)
                if (dm_is_error(rc)) exit sql_block
            else
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NLOGS, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            n = sqlite3_column_int64(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            rc = E_ALLOC
            if (has_limit) n = min(n, limit)
            allocate (logs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            if (desc_order) then
                query = query // ' ORDER BY timestamp DESC'
            else
                query = query // ' ORDER BY timestamp ASC'
            end if

            if (has_limit) query = query // ' LIMIT ?'

            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_LOGS // query, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            k  = 1

            if (has_param) then
                if (dm_is_error(db_bind_logs(k))) exit sql_block
            end if

            if (has_limit) then
                if (sqlite3_bind_int64(stmt, k, limit) /= SQLITE_OK) exit sql_block
            end if

            do i = 1, n
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, logs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(nlogs)) nlogs = n
        end block sql_block

        stat = sqlite3_finalize(stmt)
        if (.not. allocated(logs)) allocate (logs(0))
    contains
        integer function db_bind_logs(i) result(rc)
            integer, intent(out) :: i

            rc = E_DB_BIND
            i = 1

            if (has_min_level) then
                if (sqlite3_bind_int(stmt, i, min_level) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_max_level) then
                if (sqlite3_bind_int(stmt, i, max_level) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_error) then
                if (sqlite3_bind_int(stmt, i, error) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_from) then
                if (sqlite3_bind_text(stmt, i, trim(from)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_to) then
                if (sqlite3_bind_text(stmt, i, trim(to)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_node_id) then
                if (sqlite3_bind_text(stmt, i, trim(node_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_sensor_id) then
                if (sqlite3_bind_text(stmt, i, trim(sensor_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_target_id) then
                if (sqlite3_bind_text(stmt, i, trim(target_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_source) then
                if (sqlite3_bind_text(stmt, i, trim(source)) /= SQLITE_OK) return
                i = i + 1
            end if

            rc = E_NONE
        end function db_bind_logs
    end function db_select_logs_array

    integer function db_select_logs_iter(db, db_stmt, log, node_id, sensor_id, target_id, source, from, to, &
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

        type(db_type),               intent(inout)        :: db        !! Database type.
        type(db_stmt_type),          intent(inout)        :: db_stmt   !! Database statement type.
        type(log_type),              intent(out)          :: log       !! Returned log type.
        character(len=*),            intent(in), optional :: node_id   !! Node id.
        character(len=*),            intent(in), optional :: sensor_id !! Sensor id.
        character(len=*),            intent(in), optional :: target_id !! Target id.
        character(len=*),            intent(in), optional :: source    !! Source name.
        character(len=*),            intent(in), optional :: from      !! Begin of time range.
        character(len=*),            intent(in), optional :: to        !! End of time range.
        integer,                     intent(in), optional :: min_level !! Minimum log level.
        integer,                     intent(in), optional :: max_level !! Maximum log level.
        integer,                     intent(in), optional :: error     !! Error code.
        logical,                     intent(in), optional :: desc      !! Descending order.
        integer(kind=i8),            intent(in), optional :: limit     !! Max. numbers of logs.

        character(len=:), allocatable :: query
        integer                       :: k

        logical :: has_param, has_node_id, has_sensor_id, has_target_id, has_source
        logical :: has_from, has_to, has_min_level, has_max_level, has_error, has_limit
        logical :: desc_order, more

        if (.not. dm_db_prepared(db_stmt)) then
            has_param     = .false.; has_node_id   = .false.; has_sensor_id = .false.
            has_target_id = .false.; has_source    = .false.; has_from      = .false.
            has_to        = .false.; has_min_level = .false.; has_max_level = .false.
            has_error     = .false.; has_limit     = .false.; desc_order    = .false.

            if (dm_string_is_present(node_id)) then
                has_param = .true.
                has_node_id = .true.
            end if

            if (dm_string_is_present(sensor_id)) then
                has_param = .true.
                has_sensor_id = .true.
            end if

            if (dm_string_is_present(target_id)) then
                has_param = .true.
                has_target_id = .true.
            end if

            if (dm_string_is_present(source)) then
                has_param = .true.
                has_source = .true.
            end if

            if (dm_string_is_present(from)) then
                has_param = .true.
                has_from = .true.
            end if

            if (dm_string_is_present(to)) then
                has_param = .true.
                has_to = .true.
            end if

            if (present(min_level)) then
                has_param = .true.
                has_min_level = .true.
            end if

            if (present(max_level)) then
                has_param = .true.
                has_max_level = .true.
            end if

            if (present(error)) then
                has_param = .true.
                has_error = .true.
            end if

            if (present(desc)) desc_order = desc
            if (present(limit)) has_limit = .true.

            ! Build SQL query.
            allocate (character(len=0) :: query)

            if (has_param) then
                more = .false.
                if (has_min_level) call db_query_where(query, 'level >= ?',     more)
                if (has_max_level) call db_query_where(query, 'level <= ?',     more)
                if (has_error)     call db_query_where(query, 'error = ?',      more)
                if (has_from)      call db_query_where(query, 'timestamp >= ?', more)
                if (has_to)        call db_query_where(query, 'timestamp < ?',  more)
                if (has_node_id)   call db_query_where(query, 'node_id = ?',    more)
                if (has_sensor_id) call db_query_where(query, 'sensor_id = ?',  more)
                if (has_target_id) call db_query_where(query, 'target_id = ?',  more)
                if (has_source)    call db_query_where(query, 'source = ?',     more)
            end if

            if (desc_order) then
                query = query // ' ORDER BY timestamp DESC'
            else
                query = query // ' ORDER BY timestamp ASC'
            end if

            if (has_limit) query = query // ' LIMIT ?'

            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_LOGS // query, db_stmt%ptr) /= SQLITE_OK) return

            rc = E_DB_BIND
            k  = 1

            if (has_param) then
                if (dm_is_error(db_bind_logs(k))) return
            end if

            if (has_limit) then
                if (sqlite3_bind_int64(db_stmt%ptr, k, limit) /= SQLITE_OK) return
            end if
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, log)
    contains
        integer function db_bind_logs(i) result(rc)
            integer, intent(out) :: i

            rc = E_DB_BIND
            i = 1

            if (has_min_level) then
                if (sqlite3_bind_int(db_stmt%ptr, i, min_level) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_max_level) then
                if (sqlite3_bind_int(db_stmt%ptr, i, max_level) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_error) then
                if (sqlite3_bind_int(db_stmt%ptr, i, error) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_from) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(from)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_to) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(to)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_node_id) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(node_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_sensor_id) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(sensor_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_target_id) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(target_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_source) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(source)) /= SQLITE_OK) return
                i = i + 1
            end if

            rc = E_NONE
        end function db_bind_logs
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

        integer          :: stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        if (present(nnodes)) nnodes = 0_i8

        alloc_block: block
            rc = db_select_nrows(db, SQL_TABLE_NODES, n)
            if (dm_is_error(rc)) exit alloc_block

            rc = E_ALLOC
            allocate (nodes(n), stat=stat)
            if (stat /= 0) exit alloc_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit alloc_block

            sql_block: block
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NODES, stmt) /= SQLITE_OK) exit sql_block

                do i = 1, n
                    rc = E_DB_NO_ROWS
                    if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                    rc = db_next_row(stmt, nodes(i), (i == 1))
                    if (dm_is_error(rc)) exit sql_block
                end do

                if (present(nnodes)) nnodes = n
            end block sql_block

            stat = sqlite3_finalize(stmt)
        end block alloc_block

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

        if (.not. dm_db_prepared(db_stmt)) then
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NODES, db_stmt%ptr) /= SQLITE_OK) return
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, node)
    end function db_select_nodes_iter

    integer function db_select_nrows(db, table, n) result(rc)
        !! Returns number of rows in table `table` in `n`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_INVALID` if table is invalid.
        !!
        character(len=*), parameter :: QUERY = 'SELECT COUNT(*) FROM '

        type(db_type),    intent(inout) :: db    !! Database type.
        integer,          intent(in)    :: table !! Enumerator of table to count (`SQL_TABLE_*`).
        integer(kind=i8), intent(out)   :: n     !! Number of rows.

        integer     :: stat
        type(c_ptr) :: stmt

        n = 0_i8
        rc = E_INVALID
        if (table < 1 .or. table > SQL_TABLE_LAST) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, QUERY // SQL_TABLE_NAMES(table), stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            n = sqlite3_column_int64(stmt, 0)

            rc = E_NONE
        end block sql_block

        stat = sqlite3_finalize(stmt)
    end function db_select_nrows

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

        character(len=:), allocatable :: query
        integer                       :: k, stat
        integer(kind=i8)              :: i, n
        type(c_ptr)                   :: stmt

        logical :: has_param, has_node_id, has_sensor_id, has_target_id
        logical :: has_from, has_to, has_limit
        logical :: desc_order, more, stub_view

        if (present(nobservs)) nobservs = 0_i8

        has_param     = .false.; has_node_id = .false.; has_sensor_id = .false.
        has_target_id = .false.; has_from    = .false.; has_to        = .false.
        has_limit     = .false.; desc_order  = .false.; stub_view     = .false.

        if (dm_string_is_present(node_id)) then
            has_param = .true.
            has_node_id = .true.
        end if

        if (dm_string_is_present(sensor_id)) then
            has_param = .true.
            has_sensor_id = .true.
        end if

        if (dm_string_is_present(target_id)) then
            has_param = .true.
            has_target_id = .true.
        end if

        if (dm_string_is_present(from)) then
            has_param = .true.
            has_from = .true.
        end if

        if (dm_string_is_present(to)) then
            has_param = .true.
            has_to = .true.
        end if

        if (present(limit)) has_limit  = .true.
        if (present(desc))  desc_order = desc
        if (present(stub))  stub_view  = stub

        ! Build SQL query.
        allocate (character(len=0) :: query)

        if (has_param) then
            more = .false.
            if (has_node_id)   call db_query_where(query, 'nodes.id = ?',           more)
            if (has_sensor_id) call db_query_where(query, 'sensors.id = ?',         more)
            if (has_target_id) call db_query_where(query, 'targets.id = ?',         more)
            if (has_from)      call db_query_where(query, 'observs.timestamp >= ?', more)
            if (has_to)        call db_query_where(query, 'observs.timestamp < ?',  more)
        end if

        sql_block: block
            if (has_param) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NOBSERVS // query, stmt) /= SQLITE_OK) exit sql_block

                rc = db_bind_observs(k)
                if (dm_is_error(rc)) exit sql_block
            else
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NOBSERVS, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            n = sqlite3_column_int64(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            if (has_limit) n = min(n, limit)

            rc = E_ALLOC
            allocate (observs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            query = query // ' ORDER BY observs.timestamp'
            if (desc_order) query = query // ' DESC'
            if (has_limit)  query = query // ' LIMIT ?'

            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_OBSERVS // query, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            k  = 1

            if (has_param) then
                if (dm_is_error(db_bind_observs(k))) exit sql_block
            end if

            if (has_limit) then
                if (sqlite3_bind_int64(stmt, k, limit) /= SQLITE_OK) exit sql_block
            end if

            do i = 1, n
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, observs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(nobservs)) nobservs = n
        end block sql_block

        stat = sqlite3_finalize(stmt)

        if (.not. allocated(observs)) allocate (observs(0))
        if (dm_is_error(rc)) return
        if (stub_view) return
        if (size(observs) == 0) return

        rc = db_select_observs_data(db, observs)
    contains
        integer function db_bind_observs(i) result(rc)
            integer, intent(out) :: i

            rc = E_DB_BIND
            i = 1

            if (has_node_id) then
                if (sqlite3_bind_text(stmt, i, trim(node_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_sensor_id) then
                if (sqlite3_bind_text(stmt, i, trim(sensor_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_target_id) then
                if (sqlite3_bind_text(stmt, i, trim(target_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_from) then
                if (sqlite3_bind_text(stmt, i, trim(from)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_to) then
                if (sqlite3_bind_text(stmt, i, trim(to)) /= SQLITE_OK) return
                i = i + 1
            end if

            rc = E_NONE
        end function db_bind_observs
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

        character(len=:), allocatable :: query
        integer                       :: i, k, n

        logical :: has_param, has_node_id, has_sensor_id, has_target_id
        logical :: has_from, has_to, has_limit
        logical :: desc_order, more, stub_view

        if (.not. dm_db_prepared(db_stmt)) then
            has_param     = .false.; has_node_id = .false.; has_sensor_id = .false.
            has_target_id = .false.; has_from    = .false.; has_to        = .false.
            has_limit     = .false.; desc_order  = .false.; stub_view     = .false.

            if (dm_string_is_present(node_id)) then
                has_param = .true.
                has_node_id = .true.
            end if

            if (dm_string_is_present(sensor_id)) then
                has_param = .true.
                has_sensor_id = .true.
            end if

            if (dm_string_is_present(target_id)) then
                has_param = .true.
                has_target_id = .true.
            end if

            if (dm_string_is_present(from)) then
                has_param = .true.
                has_from = .true.
            end if

            if (dm_string_is_present(to)) then
                has_param = .true.
                has_to = .true.
            end if

            if (present(limit)) has_limit  = .true.
            if (present(desc))  desc_order = desc
            if (present(stub))  stub_view  = stub

            ! Build SQL query.
            allocate (character(len=0) :: query)

            if (has_param) then
                more = .false.
                if (has_node_id)   call db_query_where(query, 'nodes.id = ?',           more)
                if (has_sensor_id) call db_query_where(query, 'sensors.id = ?',         more)
                if (has_target_id) call db_query_where(query, 'targets.id = ?',         more)
                if (has_from)      call db_query_where(query, 'observs.timestamp >= ?', more)
                if (has_to)        call db_query_where(query, 'observs.timestamp < ?',  more)
            end if

            query = query // ' ORDER BY observs.timestamp'
            if (desc_order) query = query // ' DESC'
            if (has_limit)  query = query // ' LIMIT ?'

            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_OBSERVS // query, db_stmt%ptr) /= SQLITE_OK) return

            rc = E_DB_BIND
            k  = 1

            if (has_param) then
                if (dm_is_error(db_bind_observs(k))) return
            end if

            if (has_limit) then
                if (sqlite3_bind_int64(db_stmt%ptr, k, limit) /= SQLITE_OK) return
            end if
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, observ)
        if (dm_is_error(rc)) return
        if (stub_view) return

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
    contains
        integer function db_bind_observs(i) result(rc)
            integer, intent(out) :: i

            rc = E_DB_BIND
            i = 1

            if (has_node_id) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(node_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_sensor_id) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(sensor_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_target_id) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(target_id)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_from) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(from)) /= SQLITE_OK) return
                i = i + 1
            end if

            if (has_to) then
                if (sqlite3_bind_text(db_stmt%ptr, i, trim(to)) /= SQLITE_OK) return
                i = i + 1
            end if

            rc = E_NONE
        end function db_bind_observs
    end function db_select_observs_iter

    integer function db_select_observs_data(db, observs) result(rc)
        !! Fill receivers, requests, and responses into `observs` from
        !! database. Caches the SQLite prepared statements for re-use.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),     intent(inout) :: db         !! Database type.
        type(observ_type), intent(inout) :: observs(:) !! Returned observation data.

        integer          :: j, nres
        integer(kind=i8) :: i, nobs
        type(c_ptr)      :: stmt

        nobs = size(observs, kind=i8)

        rc = E_DB_NO_ROWS
        if (nobs == 0) return

        ! Get receivers (re-use statement).
        stmt = c_null_ptr

        do i = 1, nobs
            if (observs(i)%nreceivers == 0) cycle
            rc = db_select_receivers(db, observs(i)%receivers, observs(i)%id, statement=stmt)
            if (dm_is_error(rc)) exit
        end do

        if (sqlite3_finalize(stmt) /= SQLITE_OK) rc = E_DB_FINALIZE
        if (dm_is_error(rc)) return

        ! Get requests (re-use statement).
        do i = 1, nobs
            if (observs(i)%nrequests == 0) cycle
            rc = db_select_requests(db, observs(i)%requests, observs(i)%id, statement=stmt)
            if (dm_is_error(rc)) exit
        end do

        if (sqlite3_finalize(stmt) /= SQLITE_OK) rc = E_DB_FINALIZE
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
                                         statement   = stmt)
                if (dm_is_error(rc)) exit obs_loop
            end do req_loop
        end do obs_loop

        if (sqlite3_finalize(stmt) /= SQLITE_OK) rc = E_DB_FINALIZE
    end function db_select_observs_data

    integer function db_select_receivers(db, receivers, observ_id, nreceivers, statement) result(rc)
        !! Returns receivers of an observation in array `receivers`. On error,
        !! the error code is returned. If `statement` is passed, the statement
        !! will not be finalised in order to be re-used again. Finalisation has
        !! to be done by the caller. If `statement` is passed and set to
        !! `c_null_ptr`, it will be prepared by the function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),                      intent(inout)           :: db         !! Database type.
        character(len=OBSERV_RECEIVER_LEN), intent(out)             :: receivers(OBSERV_MAX_NRECEIVERS) !! Returned receivers array.
        character(len=*),                   intent(in)              :: observ_id  !! Observation id.
        integer,                            intent(out),   optional :: nreceivers !! Number of receivers.
        type(c_ptr),                        intent(inout), optional :: statement  !! SQLite statement.

        integer     :: i, nrec
        type(c_ptr) :: stmt

        stmt = c_null_ptr
        if (present(statement)) stmt = statement
        if (present(nreceivers)) nreceivers = 0

        sql_block: block
            if (.not. c_associated(stmt)) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_RECEIVERS, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ_id)) /= SQLITE_OK) exit sql_block

            nrec = 0

            row_loop: do i = 1, OBSERV_MAX_NRECEIVERS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit row_loop

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT) exit sql_block
                end if

                receivers(i) = sqlite3_column_text(stmt, 0)
                nrec = nrec + 1
            end do row_loop

            if (present(nreceivers)) nreceivers = nrec

            rc = E_DB
            if (sqlite3_reset(stmt) /= SQLITE_OK) exit sql_block

            rc = E_NONE
        end block sql_block

        if (.not. present(statement)) then
            if (sqlite3_finalize(stmt) /= SQLITE_OK) rc = E_DB_FINALIZE
            return
        end if

        statement = stmt
    end function db_select_receivers

    integer function db_select_requests(db, requests, observ_id, nrequests, statement) result(rc)
        !! Returns the request data of a given observation in array `requests`.
        !! If `statement` is passed, the statement will not be finalised in
        !! order to be re-used again. Finalisation has to be done by the
        !! caller. If `statement` is passed and set to `c_null_ptr`, it will
        !! be prepared by the function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
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
        type(c_ptr),        intent(inout), optional :: statement !! SQLite statement.

        integer     :: i, nreq
        type(c_ptr) :: stmt

        stmt = c_null_ptr
        if (present(statement)) stmt = statement
        if (present(nrequests)) nrequests = 0

        sql_block: block
            if (.not. c_associated(stmt)) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_REQUESTS, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ_id)) /= SQLITE_OK) exit sql_block

            nreq = 0

            row_loop: do i = 1, OBSERV_MAX_NREQUESTS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit row_loop

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (sqlite3_column_type(stmt,  0) /= SQLITE_TEXT)    exit sql_block
                    if (sqlite3_column_type(stmt,  1) /= SQLITE_TEXT)    exit sql_block
                    if (sqlite3_column_type(stmt,  2) /= SQLITE_TEXT)    exit sql_block
                    if (sqlite3_column_type(stmt,  3) /= SQLITE_TEXT)    exit sql_block
                    if (sqlite3_column_type(stmt,  4) /= SQLITE_TEXT)    exit sql_block
                    if (sqlite3_column_type(stmt,  5) /= SQLITE_TEXT)    exit sql_block
                    if (sqlite3_column_type(stmt,  6) /= SQLITE_INTEGER) exit sql_block
                    if (sqlite3_column_type(stmt,  7) /= SQLITE_INTEGER) exit sql_block
                    if (sqlite3_column_type(stmt,  8) /= SQLITE_INTEGER) exit sql_block
                    if (sqlite3_column_type(stmt,  9) /= SQLITE_INTEGER) exit sql_block
                    if (sqlite3_column_type(stmt, 10) /= SQLITE_INTEGER) exit sql_block
                    if (sqlite3_column_type(stmt, 11) /= SQLITE_INTEGER) exit sql_block
                    if (sqlite3_column_type(stmt, 12) /= SQLITE_INTEGER) exit sql_block
                end if

                requests(i)%name       = sqlite3_column_text(stmt,  0)
                requests(i)%timestamp  = sqlite3_column_text(stmt,  1)
                requests(i)%request    = sqlite3_column_text(stmt,  2)
                requests(i)%response   = sqlite3_column_text(stmt,  3)
                requests(i)%delimiter  = sqlite3_column_text(stmt,  4)
                requests(i)%pattern    = sqlite3_column_text(stmt,  5)
                requests(i)%delay      = sqlite3_column_int (stmt,  6)
                requests(i)%error      = sqlite3_column_int (stmt,  7)
                requests(i)%mode       = sqlite3_column_int (stmt,  8)
                requests(i)%retries    = sqlite3_column_int (stmt,  9)
                requests(i)%state      = sqlite3_column_int (stmt, 10)
                requests(i)%timeout    = sqlite3_column_int (stmt, 11)
                requests(i)%nresponses = sqlite3_column_int (stmt, 12)

                nreq = nreq + 1
            end do row_loop

            if (present(nrequests)) nrequests = nreq

            rc = E_DB
            if (sqlite3_reset(stmt) /= SQLITE_OK) exit sql_block

            rc = E_NONE
        end block sql_block

        if (.not. present(statement)) then
            if (sqlite3_finalize(stmt) /= SQLITE_OK) rc = E_DB_FINALIZE
            return
        end if

        statement = stmt
    end function db_select_requests

    integer function db_select_responses(db, responses, observ_id, request_idx, nresponses, statement) result(rc)
        !! Returns all responses from a given observation and request index in
        !! array `responses`. If `statement` is passed, the statement will not
        !! be finalised in order to be re-used again. Finalisation has to be
        !! done by the caller. If `statement` is passed and set to
        !! `c_null_ptr`, it will be prepared by the function.
        !!
        !! The function returns the following error codes:
        !!
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
        type(c_ptr),         intent(inout), optional :: statement   !! SQLite statement.

        integer     :: i, nres
        type(c_ptr) :: stmt

        stmt = c_null_ptr
        if (present(statement)) stmt = statement
        if (present(nresponses)) nresponses = 0

        sql_block: block
            if (.not. c_associated(stmt)) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_RESPONSES, stmt) /= SQLITE_OK) exit sql_block
            end if

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(observ_id)) /= SQLITE_OK) exit sql_block
            if (sqlite3_bind_int (stmt, 2, request_idx)     /= SQLITE_OK) exit sql_block

            nres = 0

            row_loop: do i = 1, REQUEST_MAX_NRESPONSES
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit row_loop

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (sqlite3_column_type(stmt, 0) /= SQLITE_TEXT)    exit sql_block
                    if (sqlite3_column_type(stmt, 1) /= SQLITE_TEXT)    exit sql_block
                    if (sqlite3_column_type(stmt, 2) /= SQLITE_INTEGER) exit sql_block
                    if (sqlite3_column_type(stmt, 3) /= SQLITE_INTEGER) exit sql_block
                    if (sqlite3_column_type(stmt, 4) /= SQLITE_FLOAT)   exit sql_block
                end if

                responses(i)%name  = sqlite3_column_text  (stmt, 0)
                responses(i)%unit  = sqlite3_column_text  (stmt, 1)
                responses(i)%type  = sqlite3_column_int   (stmt, 2)
                responses(i)%error = sqlite3_column_int   (stmt, 3)
                responses(i)%value = sqlite3_column_double(stmt, 4)

                nres = nres + 1
            end do row_loop

            if (present(nresponses)) nresponses = nres

            rc = E_DB
            if (sqlite3_reset(stmt) /= SQLITE_OK) exit sql_block

            rc = E_NONE
        end block sql_block

        if (.not. present(statement)) then
            if (sqlite3_finalize(stmt) /= SQLITE_OK) rc = E_DB_FINALIZE
            return
        end if

        statement = stmt
    end function db_select_responses

    integer function db_select_sensors_array(db, sensors, nsensors) result(rc)
        !! Returns all sensors in allocatable array `sensors`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_sensor

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(sensor_type), allocatable, intent(out)           :: sensors(:) !! Returned sensor data array.
        integer(kind=i8),               intent(out), optional :: nsensors   !! Number of returned sensors.

        integer          :: stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        if (present(nsensors)) nsensors = 0_i8

        alloc_block: block
            rc = db_select_nrows(db, SQL_TABLE_SENSORS, n)
            if (dm_is_error(rc)) exit alloc_block

            rc = E_ALLOC
            allocate (sensors(n), stat=stat)
            if (stat /= 0) exit alloc_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit alloc_block

            sql_block: block
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_SENSORS, stmt) /= SQLITE_OK) exit sql_block

                row_loop: do i = 1, n
                    rc = E_DB_NO_ROWS
                    if (sqlite3_step(stmt) /= SQLITE_ROW) exit row_loop
                    rc = db_next_row(stmt, sensors(i), (i == 1))
                    if (dm_is_error(rc)) exit sql_block
                end do row_loop

                if (present(nsensors)) nsensors = n
            end block sql_block

            stat = sqlite3_finalize(stmt)
        end block alloc_block

        if (.not. allocated(sensors)) allocate (sensors(0))
    end function db_select_sensors_array

    integer function db_select_sensors_iter(db, db_stmt, sensor) result(rc)
        !! Iterator function that returns all sensors in `sensor`. The
        !! statement `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_sensor

        type(db_type),      intent(inout) :: db      !! Database type.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        type(sensor_type),  intent(out)   :: sensor  !! Returned sensor data.

        if (.not. dm_db_prepared(db_stmt)) then
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_SENSORS, db_stmt%ptr) /= SQLITE_OK) return
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, sensor)
    end function db_select_sensors_iter

    integer function db_select_sensors_by_node_array(db, node_id, sensors, nsensors) result(rc)
        !! Returns all sensors of node `node_id` in allocatable array `sensors`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if node id is empty.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_sensor

        type(db_type),                  intent(inout)         :: db         !! Database type.
        character(len=*),               intent(in)            :: node_id    !! Node id.
        type(sensor_type), allocatable, intent(out)           :: sensors(:) !! Returned sensor data array.
        integer(kind=i8),               intent(out), optional :: nsensors   !! Number of returned sensors.

        integer          :: stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        if (present(nsensors)) nsensors = 0_i8

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_NSENSORS_BY_NODE, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id)) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
            n = sqlite3_column_int64(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            rc = E_ALLOC
            allocate (sensors(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_SENSORS_BY_NODE, stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_BIND
            if (sqlite3_bind_text(stmt, 1, trim(node_id)) /= SQLITE_OK) exit sql_block

            do i = 1, n
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, sensors(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(nsensors)) nsensors = n
        end block sql_block

        stat = sqlite3_finalize(stmt)
        if (.not. allocated(sensors)) allocate (sensors(0))
    end function db_select_sensors_by_node_array

    integer function db_select_sensors_by_node_iter(db, db_stmt, node_id, sensor) result(rc)
        !! Iterator function that returns all sensors of node `node_id` in
        !! `sensor`. The statement `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if node id is empty.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_sensor

        type(db_type),      intent(inout) :: db      !! Database type.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        character(len=*),   intent(in)    :: node_id !! Node id.
        type(sensor_type),  intent(out)   :: sensor  !! Returned sensor data.

        if (.not. dm_db_prepared(db_stmt)) then
            rc = E_INVALID
            if (len_trim(node_id) == 0) return

            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_SENSORS_BY_NODE, db_stmt%ptr) /= SQLITE_OK) return

            rc = E_DB_BIND
            if (sqlite3_bind_text(db_stmt%ptr, 1, trim(node_id)) /= SQLITE_OK) return
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, sensor)
    end function db_select_sensors_by_node_iter

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

        integer     :: stat
        type(c_ptr) :: stmt

        rc = E_INVALID
        if (.not. dm_sync_type_valid(type)) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, trim(query), stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            rc = db_next_row(stmt, sync)
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer          :: stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        nsyncs = 0_i8

        rc = E_INVALID
        if (.not. dm_sync_type_valid(type)) return

        sql_block: block
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, trim(count_query), stmt) /= SQLITE_OK) exit sql_block

            rc = E_DB_NO_ROWS
            if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block

            nsyncs = sqlite3_column_int64(stmt, 0)

            rc = E_DB_FINALIZE
            if (sqlite3_finalize(stmt) /= SQLITE_OK) exit sql_block

            n = nsyncs
            if (present(limit)) n = min(limit, nsyncs)

            rc = E_ALLOC
            allocate (syncs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (nsyncs == 0) exit sql_block

            if (present(limit)) then
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, trim(query) // ' LIMIT ?', stmt) /= SQLITE_OK) exit sql_block

                rc = E_DB_BIND
                if (sqlite3_bind_int64(stmt, 1, limit) /= SQLITE_OK) exit sql_block
            else
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, trim(query), stmt) /= SQLITE_OK) exit sql_block
            end if

            do i = 1, n
                rc = E_DB_NO_ROWS
                if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                rc = db_next_row(stmt, syncs(i))
                syncs(i)%type = type
                if (dm_is_error(rc)) exit
            end do
        end block sql_block

        stat = sqlite3_finalize(stmt)
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

        integer          :: stat
        integer(kind=i8) :: i, n
        type(c_ptr)      :: stmt

        if (present(ntargets)) ntargets = 0_i8

        alloc_block: block
            rc = db_select_nrows(db, SQL_TABLE_TARGETS, n)
            if (dm_is_error(rc)) exit alloc_block

            rc = E_ALLOC
            allocate (targets(n), stat=stat)
            if (stat /= 0) exit alloc_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit alloc_block

            sql_block: block
                rc = E_DB_PREPARE
                if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_TARGETS, stmt) /= SQLITE_OK) exit sql_block

                do i = 1, n
                    rc = E_DB_NO_ROWS
                    if (sqlite3_step(stmt) /= SQLITE_ROW) exit sql_block
                    rc = db_next_row(stmt, targets(i), (i == 1))
                    if (dm_is_error(rc)) exit sql_block
                end do

                if (present(ntargets)) ntargets = n
            end block sql_block

            stat = sqlite3_finalize(stmt)
        end block alloc_block

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

        if (.not. dm_db_prepared(db_stmt)) then
            rc = E_DB_PREPARE
            if (sqlite3_prepare_v2(db%ptr, SQL_SELECT_TARGETS, db_stmt%ptr) /= SQLITE_OK) return
        end if

        rc = E_DB_NO_ROWS
        if (sqlite3_step(db_stmt%ptr) /= SQLITE_ROW) return

        rc = db_next_row(db_stmt%ptr, target)
    end function db_select_targets_iter

    ! ******************************************************************
    ! PRIVATE SUBROUTINES.
    ! ******************************************************************
    pure subroutine db_query_where(query, part, more)
        !! Sub-query builder for `WHERE` clauses that appends `part` to query
        !! string `query`. If `query` is not allocated or of length 0, or if
        !! `more` is `.false.`, `query` will start with ` WHERE` on exit.
        !!
        !! This routine may be called multiple times on succession to add more
        !! parameters to the query.
        !!
        !! On first call, argument `more` must be `.false.`, as it will be set
        !! to `.true.` on exit to indicate an SQL `AND` operation on the next
        !! call.
        character(len=:), allocatable, intent(inout) :: query !! Query input/output string.
        character(len=*),              intent(in)    :: part  !! Part to add to query string.
        logical,                       intent(inout) :: more  !! Append `AND` to query first if `.true.`.

        integer :: n
        logical :: alloc

        n = 0
        alloc = allocated(query)
        if (alloc) n = len(query)

        if (.not. alloc .or. n == 0 .or. .not. more) then
            ! On first call just add the part.
            query = ' WHERE ' // trim(part)
        else
            ! On successive calls append `AND` first.
            query = query // ' AND ' // trim(part)
        end if

        more = .true.
    end subroutine db_query_where
end module dm_db
