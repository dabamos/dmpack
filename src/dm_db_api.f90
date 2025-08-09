! Author:  Philipp Engel
! Licence: ISC
module dm_db_api
    !! Database API module for CRUD operations, based on the core module
    !! `dm_db`. The SQL statements are stored in module `dm_sql`.
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
    !! call dm_db_close(db)
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
    !! call dm_error_out(rc, fatal=.true.)
    !!
    !! do
    !!     rc = dm_db_select_observs(db, db_stmt, observ, desc=.true., limit=10)
    !!     if (rc /= E_DB_ROW) exit
    !!     print '(a)', trim(observ%name)
    !! end do
    !!
    !! call dm_db_finalize(db_stmt)
    !! call dm_db_close(db)
    !! ```
    !!
    !! The database functions return `E_NONE` if the respective operation was
    !! successful.
    use, intrinsic :: iso_c_binding
    use :: sqlite3
    use :: dm_db
    use :: dm_db_count
    use :: dm_db_pragma
    use :: dm_db_query
    use :: dm_db_row
    use :: dm_db_table
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_sql
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
    integer, parameter, public :: DB_JOURNAL_WAL2     = 6             !! Use Write-Ahead Log 2 (WAL2) journal.
    integer, parameter, public :: DB_JOURNAL_LAST     = 6             !! Never use this.

    ! SQLite 3 auto vacuum modes.
    integer, parameter, public :: DB_AUTO_VACUUM_NONE        = 0      !! No auto-vacuum (default).
    integer, parameter, public :: DB_AUTO_VACUUM_FULL        = 1      !! Enable auto-vacuum.
    integer, parameter, public :: DB_AUTO_VACUUM_INCREMENTAL = 2      !! Vacuum requires additional PRAGMA.

    ! Additional parameters.
    integer, parameter, public :: DB_APPLICATION_ID  = int(z'444D31') !! Application id of DMPACK databases (`DM1` in ASCII).
    integer, parameter, public :: DB_SCHEMA_VERSION  = 3              !! Database schema version, increased on updates.
    integer, parameter, public :: DB_TIMEOUT_DEFAULT = 1000           !! Default SQLite 3 busy timeout [msec].

    interface dm_db_insert
        !! Generic database insert function.
        module procedure :: dm_db_insert_beat
        module procedure :: dm_db_insert_beats
        module procedure :: dm_db_insert_image
        module procedure :: dm_db_insert_log
        module procedure :: dm_db_insert_node
        module procedure :: dm_db_insert_observ
        module procedure :: dm_db_insert_observs
        module procedure :: dm_db_insert_sensor
        module procedure :: dm_db_insert_target
        module procedure :: dm_db_insert_transfer
    end interface dm_db_insert

    interface dm_db_select
        !! Generic database select function.
        module procedure :: dm_db_select_beat
        module procedure :: dm_db_select_image
        module procedure :: dm_db_select_log
        module procedure :: dm_db_select_node
        module procedure :: dm_db_select_observ
        module procedure :: dm_db_select_sensor
        module procedure :: dm_db_select_target
        module procedure :: dm_db_select_transfer
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

    ! Public procedures.
    public :: dm_db_backup
    public :: dm_db_close
    public :: dm_db_delete_beat
    public :: dm_db_delete_image
    public :: dm_db_delete_log
    public :: dm_db_delete_node
    public :: dm_db_delete_observ
    public :: dm_db_delete_sensor
    public :: dm_db_delete_target
    public :: dm_db_delete_transfer
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
    public :: dm_db_has_target
    public :: dm_db_has_transfer
    public :: dm_db_init
    public :: dm_db_insert
    public :: dm_db_insert_beat
    public :: dm_db_insert_beats
    public :: dm_db_insert_image
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
    public :: dm_db_insert_transfer
    public :: dm_db_open
    public :: dm_db_optimize
    public :: dm_db_select
    public :: dm_db_select_beat
    public :: dm_db_select_beats
    public :: dm_db_select_data_points
    public :: dm_db_select_image
    public :: dm_db_select_log
    public :: dm_db_select_logs
    public :: dm_db_select_node
    public :: dm_db_select_nodes
    public :: dm_db_select_observ
    public :: dm_db_select_observ_ids
    public :: dm_db_select_observ_views
    public :: dm_db_select_observs
    public :: dm_db_select_observs_by_id
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
    public :: dm_db_select_target
    public :: dm_db_select_targets
    public :: dm_db_select_transfer
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
    public :: dm_db_size
    public :: dm_db_update
    public :: dm_db_update_node
    public :: dm_db_update_sensor
    public :: dm_db_update_target
    public :: dm_db_update_transfer
    public :: dm_db_vacuum
    public :: dm_db_validate

    ! Private procedures.
    private :: db_delete_receivers ! obsolete
    private :: db_delete_requests  ! obsolete
    private :: db_delete_responses ! obsolete
    private :: db_has
    private :: db_insert_receivers
    private :: db_insert_requests
    private :: db_insert_responses
    private :: db_insert_sync
    private :: db_select_beats_array
    private :: db_select_beats_iter
    private :: db_select_data_points_array
    private :: db_select_data_points_iter
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
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
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

        integer       :: nsteps_, sleep_time_, stat
        logical       :: wal_
        type(db_type) :: backup
        type(c_ptr)   :: ptr

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        wal_        = dm_present(wal,        .false.)
        nsteps_     = dm_present(nsteps,     NSTEPS_DEFAULT)
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

        call dm_db_close(backup, error=stat)
        if (dm_is_error(stat)) rc = stat
    end function dm_db_backup

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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_BEAT)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, node_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function dm_db_delete_beat

    integer function dm_db_delete_image(db, image_id) result(rc)
        !! Deletes image from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if image id is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type),    intent(inout) :: db       !! Database type.
        character(len=*), intent(in)    :: image_id !! Image id.

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_INVALID
        if (len_trim(image_id) == 0) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_IMAGE)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, image_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function dm_db_delete_image

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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_INVALID
        if (len_trim(log_id) == 0) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_LOG)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, log_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_NODE)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, node_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_INVALID
        if (len_trim(observ_id) == 0) return

        ! Start transaction.
        rc = dm_db_begin(db)
        if (dm_is_error(rc)) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_OBSERV)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)

        ! Commit transaction.
        if (dm_is_ok(rc)) then
            rc = dm_db_commit(db)
            return
        end if

        ! Rollback transaction on error.
        if (dm_is_error(dm_db_rollback(db))) rc = E_DB_ROLLBACK
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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_INVALID
        if (len_trim(sensor_id) == 0) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_SENSOR)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, sensor_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_INVALID
        if (len_trim(target_id) == 0) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_TARGET)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, target_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function dm_db_delete_target

    integer function dm_db_delete_transfer(db, transfer_id) result(rc)
        !! Deletes transfer from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if transfer id is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type),    intent(inout) :: db          !! Database type.
        character(len=*), intent(in)    :: transfer_id !! Transfer id.

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_INVALID
        if (len_trim(transfer_id) /= UUID_LEN) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_TRANSFER)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, transfer_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function dm_db_delete_transfer

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

        rc = dm_db_pragma_get(db, 'application_id', id)
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

        rc = dm_db_pragma_get(db, 'data_version', version)
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

        integer :: foreign_keys

        enabled = .false.
        rc = dm_db_pragma_get(db, 'foreign_keys', foreign_keys)
        if (dm_is_error(rc)) return
        enabled = (foreign_keys == 1)
    end function dm_db_get_foreign_keys

    integer function dm_db_get_journal_mode(db, mode, name) result(rc)
        !! Returns journal mode of database in `mode`. The name of the mode is
        !! optionally passed in `name`.
        !!
        !! Argument `mode` is set to either:
        !!
        !! * `DB_JOURNAL_OFF`
        !! * `DB_JOURNAL_DELETE`
        !! * `DB_JOURNAL_TRUNCATE`
        !! * `DB_JOURNAL_PERSIST`
        !! * `DB_JOURNAL_MEMORY`
        !! * `DB_JOURNAL_WAL`
        !! * `DB_JOURNAL_WAL2`
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !!
        type(db_type),                 intent(inout)         :: db   !! Database type.
        integer,                       intent(out)           :: mode !! Journal mode enumerator.
        character(len=:), allocatable, intent(out), optional :: name !! Journal mode name.

        character(len=:), allocatable :: journal

        mode = DB_JOURNAL_OFF
        rc = dm_db_pragma_get(db, 'journal_mode', journal)
        if (dm_is_error(rc)) return

        select case (journal)
            case ('delete');   mode = DB_JOURNAL_DELETE
            case ('truncate'); mode = DB_JOURNAL_TRUNCATE
            case ('persist');  mode = DB_JOURNAL_PERSIST
            case ('memory');   mode = DB_JOURNAL_MEMORY
            case ('wal');      mode = DB_JOURNAL_WAL
            case ('wal2');     mode = DB_JOURNAL_WAL2
            case default;      mode = DB_JOURNAL_OFF
        end select

        if (present(name)) name = journal
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

        integer :: query_only

        enabled = .false.
        rc = dm_db_pragma_get(db, 'query_only', query_only)
        if (dm_is_error(rc)) return
        enabled = (query_only == 1)
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

        rc = dm_db_pragma_get(db, 'user_version', version)
    end function dm_db_get_schema_version

    logical function dm_db_has_log(db, log_id) result(has)
        !! Returns `.true.` if log of passed id exists.
        type(db_type),    intent(inout) :: db     !! Database type.
        character(len=*), intent(in)    :: log_id !! Log id (UUID).

        has = db_has(db, SQL_TABLE_LOGS, log_id)
    end function dm_db_has_log

    logical function dm_db_has_node(db, node_id) result(has)
        !! Returns `.true.` if node of passed id exists.
        type(db_type),    intent(inout) :: db      !! Database type.
        character(len=*), intent(in)    :: node_id !! Node id.

        has = db_has(db, SQL_TABLE_NODES, node_id)
    end function dm_db_has_node

    logical function dm_db_has_observ(db, observ_id) result(has)
        !! Returns `.true.` if observation of passed id exists.
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: observ_id !! Observation id (UUID).

        has = db_has(db, SQL_TABLE_OBSERVS, observ_id)
    end function dm_db_has_observ

    logical function dm_db_has_sensor(db, sensor_id) result(exists)
        !! Returns `.true.` if sensor of passed id exists.
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: sensor_id !! Sensor id.

        exists = db_has(db, SQL_TABLE_SENSORS, sensor_id)
    end function dm_db_has_sensor

    logical function dm_db_has_target(db, target_id) result(has)
        !! Returns `.true.` if target of passed id exists.
        type(db_type),    intent(inout) :: db        !! Database type.
        character(len=*), intent(in)    :: target_id !! Target id.

        has = db_has(db, SQL_TABLE_TARGETS, target_id)
    end function dm_db_has_target

    logical function dm_db_has_transfer(db, transfer_id) result(has)
        !! Returns `.true.` if transfer of passed id exists.
        type(db_type),    intent(inout) :: db          !! Database type.
        character(len=*), intent(in)    :: transfer_id !! Transfer id.

        has = db_has(db, SQL_TABLE_TRANSFERS, transfer_id)
    end function dm_db_has_transfer

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

        type(db_stmt_type) :: db_stmt_

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate beat.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_beat_is_valid(beat)) return
        end if

        ! Set given statement.
        if (present(db_stmt)) db_stmt_ = db_stmt

        sql_block: block
            if (.not. dm_db_is_prepared(db_stmt_)) then
                rc = dm_db_prepare(db, db_stmt_, SQL_INSERT_BEAT)
                if (dm_is_error(rc)) exit sql_block
            end if

            rc = dm_db_bind(db_stmt_, 1, beat%node_id);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 2, beat%address);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 3, beat%client);    if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 4, beat%time_sent); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 5, beat%time_recv); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 6, beat%error);     if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 7, beat%interval);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 8, beat%uptime);    if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt_)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_reset(db_stmt_)
        end block sql_block

        if (present(db_stmt)) then
            db_stmt = db_stmt_
        else
            call dm_db_finalize(db_stmt_)
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

        integer            :: i
        logical            :: transaction_
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_EMPTY
        if (size(beats) == 0) return

        transaction_ = dm_present(transaction, .true.)

        ! Start transaction.
        if (transaction_) then
            rc = dm_db_begin(db)
            if (dm_is_error(rc)) return
        end if

        ! Re-use statement.
        do i = 1, size(beats)
            rc = dm_db_insert_beat(db, beats(i), db_stmt, validate=validate)
            if (dm_is_error(rc)) exit
        end do

        call dm_db_finalize(db_stmt)

        if (transaction_) then
            ! Commit transaction.
            if (dm_is_ok(rc)) rc = dm_db_commit(db)
            if (dm_is_ok(rc)) return
            ! Rollback transaction.
            if (dm_is_error(dm_db_rollback(db))) rc = E_DB_ROLLBACK
        end if
    end function dm_db_insert_beats

    integer function dm_db_insert_image(db, image, validate) result(rc)
        !! Adds given image to database. The image data is validated by
        !! default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if argument `image` is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_image

        type(db_type),    intent(inout)        :: db       !! Database type.
        type(image_type), intent(inout)        :: image    !! Image to insert.
        logical,          intent(in), optional :: validate !! Validate image.

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate image.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_image_is_valid(image)) return
        end if

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_INSERT_IMAGE)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, image%id);        if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 2, image%node_id);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 3, image%sensor_id); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 4, image%target_id); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 5, image%timestamp); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 6, image%mime);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 7, image%width);     if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 8, image%height);    if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 9, image%size);      if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function dm_db_insert_image

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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate log.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_log_is_valid(log)) return
        end if

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_INSERT_LOG)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt,  1, log%id);        if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  2, log%level);     if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  3, log%error);     if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  4, log%timestamp); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  5, log%node_id);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  6, log%sensor_id); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  7, log%target_id); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  8, log%observ_id); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  9, log%source);    if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 10, log%message);   if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate node.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_node_is_valid(node)) return
        end if

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_INSERT_NODE)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, node%id);        if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 2, node%name);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 3, node%meta);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 4, node%x);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 5, node%y);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 6, node%z);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 7, node%longitude); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 8, node%latitude);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 9, node%elevation); if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        integer            :: i, n
        type(db_stmt_type) :: db_stmt_

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate observation.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_observ_is_valid(observ)) return
        end if

        ! Set given statement.
        if (present(db_stmt)) db_stmt_ = db_stmt

        ! Begin transaction.
        rc = E_DB_TRANSACTION
        if (dm_is_error(dm_db_save_point(db, SAVE_POINT))) return

        sql_block: block
            if (.not. dm_db_is_prepared(db_stmt_)) then
                rc = dm_db_prepare(db, db_stmt_, SQL_INSERT_OBSERV)
                if (dm_is_error(rc)) exit sql_block
            end if

            ! Add observation data.
            rc = dm_db_bind(db_stmt_,  1, observ%id);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_,  2, observ%node_id);    if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_,  3, observ%sensor_id);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_,  4, observ%target_id);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_,  5, observ%name);       if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_,  6, observ%timestamp);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_,  7, observ%source);     if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_,  8, observ%device);     if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_,  9, observ%priority);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 10, observ%error);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 11, observ%next);       if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 12, observ%nreceivers); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 13, observ%nrequests);  if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt_);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_reset(db_stmt_);    if (dm_is_error(rc)) exit sql_block

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
            call dm_db_finalize(db_stmt_)
        end if

        ! Commit or rollback transaction.
        if (dm_is_error(rc)) then
            rc = dm_db_error(db)
            if (dm_db_rollback(db, SAVE_POINT) /= SQLITE_OK) rc = E_DB_ROLLBACK
        end if

        if (dm_is_error(dm_db_release(db, SAVE_POINT))) rc = E_DB_TRANSACTION
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

        integer            :: i
        logical            :: transaction_
        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_EMPTY
        if (size(observs) == 0) return

        transaction_ = dm_present(transaction, .true.)

        ! Start transaction.
        if (transaction_) then
            rc = dm_db_begin(db)
            if (dm_is_error(rc)) return
        end if

        ! Re-use statement.
        do i = 1, size(observs)
            rc = dm_db_insert_observ(db, observs(i), db_stmt, validate=validate)
            if (dm_is_error(rc)) exit
        end do

        call dm_db_finalize(db_stmt)

        if (transaction_) then
            ! Commit transaction.
            if (dm_is_ok(rc)) rc = dm_db_commit(db)
            if (dm_is_ok(rc)) return
            ! Rollback transaction.
            if (dm_is_error(dm_db_rollback(db))) rc = E_DB_ROLLBACK
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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate sensor.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_sensor_is_valid(sensor)) return
        end if

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_INSERT_SENSOR)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt,  1, sensor%id);        if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  2, sensor%node_id);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  3, sensor%type);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  4, sensor%name);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  5, sensor%sn);        if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  6, sensor%meta);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  7, sensor%x);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  8, sensor%y);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  9, sensor%z);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 10, sensor%longitude); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 11, sensor%latitude);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 12, sensor%elevation); if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate target.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_target_is_valid(target)) return
        end if

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_INSERT_TARGET)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt,  1, target%id);        if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  2, target%name);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  3, target%meta);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  4, target%state);     if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  5, target%x);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  6, target%y);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  7, target%z);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  8, target%longitude); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  9, target%latitude);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 10, target%elevation); if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function dm_db_insert_target

    integer function dm_db_insert_transfer(db, transfer, validate) result(rc)
        !! Adds given transfer to database. The transfer data is validated by
        !! default.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if argument `transfer` is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_transfer

        type(db_type),       intent(inout)        :: db       !! Database type.
        type(transfer_type), intent(inout)        :: transfer !! Transfer to insert.
        logical,             intent(in), optional :: validate !! Validate transfer.

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate transfer.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_transfer_is_valid(transfer)) return
        end if

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_INSERT_TRANSFER)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, transfer%id);        if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 2, transfer%node_id);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 3, transfer%type_id);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 4, transfer%timestamp); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 5, transfer%address);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 6, transfer%type);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 7, transfer%state);     if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 8, transfer%error);     if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 9, transfer%size);      if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function dm_db_insert_transfer

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
        !! * `E_DB` if initialising SQLite failed.
        !! * `E_DB_ID` if the database has a wrong application id.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_DB_VERSION` if the user version is incompatible.
        !! * `E_EXIST` if database is already opened.
        !! * `E_IO` if opening the database failed.
        !! * `E_NOT_FOUND` if database has not been found.
        !! * `E_PERM` if no read or write permission.
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
        logical :: create_, exists, foreign_keys_, read_only_, threaded_, validate_, wal_

        create_       = dm_present(create,       .false.) ! Create database.
        foreign_keys_ = dm_present(foreign_keys, .true.)  ! Foreign keys contraint.
        read_only_    = dm_present(read_only,    .false.) ! Read-only mode.
        threaded_     = dm_present(threaded,     .false.) ! Threaded access (not recommended).
        timeout_      = dm_present(timeout,      0)       ! Busy timeout.
        validate_     = dm_present(validate,     .true.)  ! App ID validation.
        wal_          = dm_present(wal,          .false.) ! WAL mode.

        ! Validate options.
        rc = E_EXIST
        if (dm_db_is_connected(db)) return

        exists = dm_file_exists(path)

        if (.not. create_) then
            rc = E_NOT_FOUND
            if (.not. exists) return

            rc = E_PERM
            if (.not. dm_file_is_readable(path)) return
            if (.not. read_only_ .and. .not. dm_file_is_writeable(path)) return
        end if

        ! Set database flags.
        flag = SQLITE_OPEN_PRIVATECACHE

        if (read_only_) then
            flag = ior(flag, SQLITE_OPEN_READONLY)
        else
            flag = ior(flag, SQLITE_OPEN_READWRITE)
        end if

        if (create_ .and. .not. exists) flag = ior(flag, SQLITE_OPEN_CREATE)
        if (threaded_)                  flag = ior(flag, SQLITE_OPEN_FULLMUTEX)

        db%read_only = read_only_

        ! Initialise SQLite.
        rc = E_DB
        if (sqlite3_initialize() /= SQLITE_OK) return

        ! Open database.
        rc = E_IO
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

        rc = dm_db_pragma_set(db, 'optimize')
    end function dm_db_optimize

    integer function dm_db_select_beat(db, beat, node_id) result(rc)
        !! Returns heartbeat associated with given node id in `beat`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_beat

        type(db_type),    intent(inout) :: db      !! Database type.
        type(beat_type),  intent(out)   :: beat    !! Returned beat type.
        character(len=*), intent(in)    :: node_id !! Node id.

        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        call dm_db_query_where(db_query, 'node_id = ?', node_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_BEATS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, beat)
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
    end function dm_db_select_beat

    integer function dm_db_select_image(db, image, image_id) result(rc)
        !! Returns image data associated with given image id from images table.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if image id or type id is not passed or invalid.
        !!
        use :: dm_image

        type(db_type),    intent(inout) :: db       !! Database type.
        type(image_type), intent(out)   :: image    !! Returned image data.
        character(len=*), intent(in)    :: image_id !! Image id.

        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        call dm_db_query_where(db_query, 'images.id = ?', image_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_IMAGES))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, image)
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
    end function dm_db_select_image

    integer function dm_db_select_log(db, log, log_id) result(rc)
        !! Returns log associated with given id in `log`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_log

        type(db_type),    intent(inout) :: db     !! Database type.
        type(log_type),   intent(out)   :: log    !! Returned log data.
        character(len=*), intent(in)    :: log_id !! Log id.

        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(log_id) == 0) return

        call dm_db_query_where(db_query, 'id = ?', log_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_LOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, log)
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
    end function dm_db_select_log

    integer function dm_db_select_node(db, node, node_id) result(rc)
        !! Returns node data associated with given id in `node`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_node

        type(db_type),    intent(inout) :: db      !! Database type.
        type(node_type),  intent(out)   :: node    !! Returned node data.
        character(len=*), intent(in)    :: node_id !! Node id.

        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        call dm_db_query_where(db_query, 'nodes.id = ?', node_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NODES))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, node)
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
    end function dm_db_select_node

    integer function dm_db_select_observ(db, observ, observ_id) result(rc)
        !! Returns observation referenced by the given id from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
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

        call dm_db_query_where(db_query, 'observs.id = ?', observ_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, observ)
        end block sql_block

        call dm_db_query_destroy(db_query)

        call dm_db_finalize(db_stmt, error=rc)
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
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),                             intent(inout)         :: db        !! Database type.
        character(len=OBSERV_ID_LEN), allocatable, intent(out)           :: ids(:)    !! Returned observation ids.
        character(len=*),                          intent(in),  optional :: node_id   !! Node id.
        character(len=*),                          intent(in),  optional :: sensor_id !! Sensor id.
        character(len=*),                          intent(in),  optional :: target_id !! Target id.
        character(len=*),                          intent(in),  optional :: from      !! Beginning of time span.
        character(len=*),                          intent(in),  optional :: to        !! End of time span.
        logical,                                   intent(in),  optional :: desc      !! Descending order.
        integer(kind=i8),                          intent(in),  optional :: limit     !! Max. number of observations.
        integer(kind=i8),                          intent(out), optional :: nids      !! Total number of observation ids (may be greater than limit).

        integer             :: nbyte, stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        if (present(nids)) nids = 0_i8

        if (present(node_id))   call dm_db_query_where(db_query, 'nodes.id = ?',           node_id)
        if (present(sensor_id)) call dm_db_query_where(db_query, 'sensors.id = ?',         sensor_id)
        if (present(target_id)) call dm_db_query_where(db_query, 'targets.id = ?',         target_id)
        if (present(from))      call dm_db_query_where(db_query, 'observs.timestamp >= ?', from)
        if (present(to))        call dm_db_query_where(db_query, 'observs.timestamp < ?',  to)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NOBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(db_stmt, 0, n)

            call dm_db_finalize(db_stmt, error=rc)
            if (dm_is_error(rc)) return

            if (present(nids))  nids = n
            if (present(limit)) n    = min(n, limit)

            rc = E_ALLOC
            allocate (ids(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, by='observs.timestamp', desc=desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERV_IDS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, ids(i), nbyte, (i == 1))
                if (dm_is_error(rc)) exit sql_block

                rc = E_INVALID
                if (nbyte /= OBSERV_ID_LEN) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
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
        !! * `E_DB_DONE` if statement finished.
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

        call dm_db_query_where(db_query, 'nodes.id = ?',            node_id)
        call dm_db_query_where(db_query, 'sensors.id = ?',          sensor_id)
        call dm_db_query_where(db_query, 'targets.id = ?',          target_id)
        call dm_db_query_where(db_query, 'responses.name = ?',      response_name)
        call dm_db_query_where(db_query, 'requests.timestamp >= ?', from)
        call dm_db_query_where(db_query, 'requests.timestamp < ?',  to)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NOBSERV_VIEWS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(db_stmt, 0, n)

            call dm_db_finalize(db_stmt, error=rc)
            if (dm_is_error(rc)) exit sql_block

            if (present(nviews)) nviews = n
            if (present(limit))  n      = min(n, limit)

            rc = E_ALLOC
            allocate (views(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, by='requests.timestamp', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERV_VIEWS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, views(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(views)) allocate (views(0))
    end function dm_db_select_observ_views

    integer function dm_db_select_observs_by_id(db, observs, after_id, before_id, limit, stub, nobservs) result(rc)
        !! Returns observations of a given id range in `observs`. The argument
        !! `after_id` is the id of the observation after which the range
        !! starts, `before_id` the id of the observation that limits the range.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if observations of given ids are not related.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_NO_ROWS` if no rows are returned.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),                  intent(inout)         :: db         !! Database type.
        type(observ_type), allocatable, intent(out)           :: observs(:) !! Returned observation data.
        character(len=*),               intent(in)            :: after_id   !! Id of observation with timestamp before first of range.
        character(len=*),               intent(in),  optional :: before_id  !! Id of observation with timestamp after last of range.
        integer(kind=i8),               intent(in),  optional :: limit      !! Max. number of observations.
        logical,                        intent(in),  optional :: stub       !! Without receivers, requests, responses.
        integer(kind=i8),               intent(out), optional :: nobservs   !! Total number of observations (may be greater than limit).

        integer             :: stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt
        type(observ_type)   :: observ1, observ2

        if (present(nobservs)) nobservs = 0_i8

        rc = dm_db_select_observ(db, observ1, after_id)
        if (dm_is_error(rc)) return

        if (present(before_id)) then
            rc = dm_db_select_observ(db, observ2, before_id)
            if (dm_is_error(rc)) return

            rc = E_INVALID
            if (observ1%node_id   /= observ2%node_id)   return
            if (observ1%sensor_id /= observ2%sensor_id) return
            if (observ1%target_id /= observ2%target_id) return
        end if

        call dm_db_query_where(db_query, 'nodes.id = ?',    observ1%node_id)
        call dm_db_query_where(db_query, 'sensors.id = ?',  observ1%sensor_id)
        call dm_db_query_where(db_query, 'targets.id = ?',  observ1%target_id)
        call dm_db_query_where(db_query, 'observs.id <> ?', after_id)
        call dm_db_query_where(db_query, 'observs.timestamp >= (SELECT timestamp FROM observs WHERE id = ?)', after_id)

        if (present(before_id)) call dm_db_query_where(db_query, 'observs.timestamp < (SELECT timestamp FROM observs WHERE id = ?)', before_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NOBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(db_stmt, 0, n)

            call dm_db_finalize(db_stmt, error=rc)
            if (dm_is_error(rc)) exit sql_block

            if (present(nobservs)) nobservs = n
            if (present(limit))    n        = min(n, limit)

            rc = E_ALLOC
            allocate (observs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, by='observs.timestamp', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, observs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)

        if (.not. allocated(observs)) allocate (observs(0))

        if (dm_is_error(rc))           return
        if (dm_present(stub, .false.)) return
        if (size(observs) == 0)        return

        rc = db_select_observs_data(db, observs)
    end function dm_db_select_observs_by_id

    integer function dm_db_select_sensor(db, sensor, sensor_id) result(rc)
        !! Returns sensor data associated with given sensor id from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_sensor

        type(db_type),     intent(inout) :: db        !! Database type.
        type(sensor_type), intent(out)   :: sensor    !! Returned sensor data.
        character(len=*),  intent(in)    :: sensor_id !! Sensor id.

        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(sensor_id) == 0) return

        call dm_db_query_where(db_query, 'sensors.id = ?', sensor_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_SENSORS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, sensor)
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
    end function dm_db_select_sensor

    integer function dm_db_select_sync_log(db, sync) result(rc)
        !! Returns log synchronisation data (oldest not transmitted log).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_DONE` if statement finished.
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
        !! * `E_DB_DONE` if statement finished.
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
        !! * `E_DB_DONE` if statement finished.
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
        !! * `E_DB_DONE` if statement finished.
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
        !! * `E_DB_DONE` if statement finished.
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

    integer function dm_db_select_target(db, target, target_id) result(rc)
        !! Returns target data associated with given target id from database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        use :: dm_target

        type(db_type),     intent(inout) :: db        !! Database type.
        type(target_type), intent(out)   :: target    !! Returned target data.
        character(len=*),  intent(in)    :: target_id !! Target id.

        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(target_id) == 0) return

        call dm_db_query_where(db_query, 'targets.id = ?', target_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_TARGETS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, target)
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
    end function dm_db_select_target

    integer function dm_db_select_transfer(db, transfer, transfer_id, type_id) result(rc)
        !! Returns transfer data associated with given transfer id and/or
        !! type_id from transfer database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if transfer id or type id is not passed or invalid.
        !!
        use :: dm_transfer

        type(db_type),       intent(inout)        :: db          !! Database type.
        type(transfer_type), intent(out)          :: transfer    !! Returned transfer data.
        character(len=*),    intent(in), optional :: transfer_id !! Transfer id.
        character(len=*),    intent(in), optional :: type_id     !! Transfer type id.

        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (.not. present(transfer_id) .and. .not. present(type_id)) return

        if (present(transfer_id)) call dm_db_query_where(db_query, 'transfers.id = ?',      transfer_id)
        if (present(type_id))     call dm_db_query_where(db_query, 'transfers.type_id = ?', type_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_TRANSFERS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, transfer)
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
    end function dm_db_select_transfer

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
        type(db_type), intent(inout) :: db !! Database type.
        integer,       intent(in)    :: id !! Application id.

        rc = dm_db_pragma_set(db, 'application_id', id)
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
        type(db_type), intent(inout) :: db   !! Database type.
        integer,       intent(in)    :: mode !! Database auto vacuum mode.

        integer :: vacuum

        rc = E_INVALID

        select case (mode)
            case (DB_AUTO_VACUUM_NONE);        vacuum = 0
            case (DB_AUTO_VACUUM_FULL);        vacuum = 1
            case (DB_AUTO_VACUUM_INCREMENTAL); vacuum = 2
            case default;                      return
        end select

        rc = dm_db_pragma_set(db, 'auto_vacuum', vacuum)
    end function dm_db_set_auto_vacuum

    integer function dm_db_set_foreign_keys(db, enabled) result(rc)
        !! Sets foreign keys constraint.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        type(db_type), intent(inout) :: db      !! Database type.
        logical,       intent(in)    :: enabled !! Enable foreign keys constraint.

        rc = dm_db_pragma_set(db, 'foreign_keys', dm_btoa(enabled, 'ON', 'OFF'))
    end function dm_db_set_foreign_keys

    integer function dm_db_set_journal_mode(db, mode) result(rc)
        !! Sets journal mode. Argument `mode` has to be one of:
        !!
        !! * `DB_JOURNAL_OFF`
        !! * `DB_JOURNAL_DELETE`
        !! * `DB_JOURNAL_TRUNCATE`
        !! * `DB_JOURNAL_PERSIST`
        !! * `DB_JOURNAL_MEMORY`
        !! * `DB_JOURNAL_WAL`
        !! * `DB_JOURNAL_WAL2`
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if journal mode is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout) :: db   !! Database type.
        integer,       intent(in)    :: mode !! Journal mode.

        character(len=8) :: journal

        rc = E_INVALID

        select case (mode)
            case (DB_JOURNAL_OFF);      journal = 'off'
            case (DB_JOURNAL_DELETE);   journal = 'delete'
            case (DB_JOURNAL_TRUNCATE); journal = 'truncate'
            case (DB_JOURNAL_PERSIST);  journal = 'persist'
            case (DB_JOURNAL_MEMORY);   journal = 'memory'
            case (DB_JOURNAL_WAL);      journal = 'wal'
            case (DB_JOURNAL_WAL2);     journal = 'wal2'
            case default;               return
        end select

        rc = dm_db_pragma_set(db, 'journal_mode', journal)
    end function dm_db_set_journal_mode

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
        type(db_type), intent(inout) :: db      !! Database type.
        logical,       intent(in)    :: enabled !! Enable query-only mode.

        rc = dm_db_pragma_set(db, 'query_only', dm_btoa(enabled, 'ON', 'OFF'))
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
        type(db_type), intent(inout) :: db      !! Database type.
        integer,       intent(in)    :: version !! Database user version.

        rc = dm_db_pragma_set(db, 'user_version', version)
    end function dm_db_set_schema_version

    integer function dm_db_size(db, nbyte) result(rc)
        !! Returns SQLite database size in bytes.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_DB_TYPE` if returned column is unexpected.
        !!
        type(db_type),    intent(inout) :: db    !! Database type.
        integer(kind=i8), intent(out)   :: nbyte !! Database size [byte].

        type(db_stmt_type) :: db_stmt

        nbyte = 0_i8

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, 'SELECT page_count * page_size FROM pragma_page_count(), pragma_page_size()')
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_TYPE
            if (.not. dm_db_column_is_integer(db_stmt, 0)) exit sql_block

            rc = E_NONE
            call dm_db_column(db_stmt, 0, nbyte)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate node.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_node_is_valid(node)) return
        end if

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_UPDATE_NODE)
            if (dm_is_error(rc)) exit sql_block

            ! Node id must be last argument!
            rc = dm_db_bind(db_stmt, 1, node%name);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 2, node%meta);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 3, node%x);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 4, node%y);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 5, node%z);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 6, node%longitude); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 7, node%latitude);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 8, node%elevation); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 9, node%id);        if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate sensor.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_sensor_is_valid(sensor)) return
        end if

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_UPDATE_SENSOR)
            if (dm_is_error(rc)) exit sql_block

            ! Sensor id must be last argument!
            rc = dm_db_bind(db_stmt,  1, sensor%node_id);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  2, sensor%type);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  3, sensor%name);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  4, sensor%sn);        if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  5, sensor%meta);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  6, sensor%x);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  7, sensor%y);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  8, sensor%z);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  9, sensor%longitude); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 10, sensor%latitude);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 11, sensor%elevation); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 12, sensor%id);        if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate target.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_target_is_valid(target)) return
        end if

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_UPDATE_TARGET)
            if (dm_is_error(rc)) exit sql_block

            ! Target id must be last argument!
            rc = dm_db_bind(db_stmt,  1, target%name);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  2, target%meta);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  3, target%state);     if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  4, target%x);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  5, target%y);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  6, target%z);         if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  7, target%longitude); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  8, target%latitude);  if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt,  9, target%elevation); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 10, target%id);        if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function dm_db_update_target

    integer function dm_db_update_transfer(db, transfer_id, timestamp, state, error, validate) result(rc)
        !! Updates attributes of transfer with passed transfer id. The
        !! arguments are validated by default. This function is not
        !! thread-safe. If executed by multiple threads, `E_NOT_FOUND` may be
        !! returned even on a successful transfer update.
        !!
        !! If argument `timestamp` is passed, the update is performed only if
        !! the old time stamp is less-equal the new time stamp.
        !!
        !! If argument `state` is passed, an update is performed only when:
        !!
        !! * the new value is `TRANSFER_STATE_ACTIVE` and the old value is
        !!   `TRANSFER_STATE_CREATED`;
        !! * the new value is `TRANSFER_STATE_DONE` and the old value is
        !!   `TRANSFER_STATE_ACTIVE`;
        !! * the new value is `TRANSFER_STATE_FAILED` and the old value is
        !!   `TRANSFER_STATE_ACTIVE`.
        !!
        !! In any other case, the function returns `E_INVALID` as no transfer
        !! was found with the passed id and required state. This behaviour is
        !! required to catch race conditions. If argument `error` is passed
        !! additionally, the error code is not updated.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_INVALID` if transfer id or state is invalid.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_transfer

        type(db_type),    intent(inout)        :: db          !! Database type.
        character(len=*), intent(in)           :: transfer_id !! Transfer id (UUIDv4).
        character(len=*), intent(in), optional :: timestamp   !! Transfer time stamp (ISO 8601).
        integer,          intent(in), optional :: state       !! Transfer state (`TRANSFER_STATE_*`).
        integer,          intent(in), optional :: error       !! Error code.
        logical,          intent(in), optional :: validate    !! Validate arguments.

        integer             :: old_state
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        rc = E_NONE
        if (.not. present(state) .and. .not. present(error)) return

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        ! Validate attributes.
        if (dm_present(validate, .true.)) then
            rc = E_INVALID
            if (.not. dm_uuid4_is_valid(transfer_id)) return

            if (present(timestamp)) then
                if (.not. dm_time_is_valid(timestamp)) return
            end if

            if (present(state)) then
                if (.not. dm_transfer_state_is_valid(state)) return
            end if

            if (present(error)) then
                if (.not. dm_error_is_valid(error)) return
            end if
        end if

        if (present(timestamp)) then
            call dm_db_query_update(db_query, 'timestamp',      timestamp) ! SET parameter.
            call dm_db_query_where (db_query, 'timestamp <= ?', timestamp) ! WHERE parameter.
        end if

        if (present(state)) then
            select case (state)
                case (TRANSFER_STATE_ACTIVE); old_state = TRANSFER_STATE_CREATED
                case (TRANSFER_STATE_FAILED); old_state = TRANSFER_STATE_ACTIVE
                case (TRANSFER_STATE_DONE);   old_state = TRANSFER_STATE_ACTIVE
                case default;                 old_state = TRANSFER_STATE_NONE
            end select

            call dm_db_query_update(db_query, 'state',     state)     ! SET parameter.
            call dm_db_query_where (db_query, 'state = ?', old_state) ! WHERE parameter.
        end if

        if (present(error)) call dm_db_query_update(db_query, 'error', error) ! SET parameter.
        call dm_db_query_where(db_query, 'id = ?', transfer_id)               ! WHERE parameter.

        sql_block: block
            integer :: n

            ! UPDATE transfers SET timestamp = ?, state = ?, error = ? WHERE timestamp <= ? AND state = ? AND id = ?;
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, 'UPDATE transfers'))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_changes(db, n)
            if (n == 0) rc = E_INVALID
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
    end function dm_db_update_transfer

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

        type(db_stmt_type) :: db_stmt

        if (present(into)) then
            rc = E_EXIST
            if (dm_file_exists(into)) return
        end if

        sql_block: block
            if (present(into)) then
                rc = dm_db_prepare(db, db_stmt, QUERY // ' INTO ?')
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_bind(db_stmt, 1, into)
                if (dm_is_error(rc)) exit sql_block
            else
                rc = dm_db_prepare(db, db_stmt, QUERY)
                if (dm_is_error(rc)) exit sql_block
            end if

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_db_close(db, optimize, error)
        !! Closes connection to SQLite database. Optimises the database if
        !! argument `optimize` is `.true.`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_COMPILER` if a pointer could not be deassociated (compiler bug).
        !! * `E_DB` if closing the database failed.
        !! * `E_DB_BUSY` if database is still busy.
        !! * `E_DB_PREPARE` if database optimisation failed.
        !! * `E_DB_STEP` if database optimisation failed (no write access).
        !!
        type(db_type), intent(inout)         :: db       !! Database type.
        logical,       intent(in),  optional :: optimize !! Optimise on close.
        integer,       intent(out), optional :: error    !! Error code.

        integer :: rc, stat

        db_block: block
            if (dm_present(optimize, .false.)) then
                rc = dm_db_optimize(db)
                if (dm_is_error(rc)) exit db_block
            end if

            stat = sqlite3_close(db%ctx)

            rc = E_DB_BUSY
            if (stat == SQLITE_BUSY) exit db_block

            rc = E_DB
            if (stat /= SQLITE_OK) exit db_block

            rc = E_COMPILER
            db%ctx = c_null_ptr
            if (c_associated(db%ctx)) exit db_block

            rc = E_NONE
        end block db_block

        if (present(error)) error = rc
    end subroutine dm_db_close

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
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

        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_RECEIVERS)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_REQUESTS)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_DELETE_OBSERV_RESPONSES)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function db_delete_responses

    logical function db_has(db, table, id) result(has)
        !! Returns `.true.` if id exists in table. Argument `table` must be one
        !! of the following:
        !!
        !! * `SQL_TABLE_IMAGES`
        !! * `SQL_TABLE_LOGS`
        !! * `SQL_TABLE_NODES`
        !! * `SQL_TABLE_OBSERVS`
        !! * `SQL_TABLE_SENSORS`
        !! * `SQL_TABLE_TARGETS`
        !! * `SQL_TABLE_TRANSFERS`
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
                case (SQL_TABLE_IMAGES);    rc = dm_db_prepare(db, db_stmt, SQL_HAS_IMAGE)
                case (SQL_TABLE_LOGS);      rc = dm_db_prepare(db, db_stmt, SQL_HAS_LOG)
                case (SQL_TABLE_NODES);     rc = dm_db_prepare(db, db_stmt, SQL_HAS_NODE)
                case (SQL_TABLE_OBSERVS);   rc = dm_db_prepare(db, db_stmt, SQL_HAS_OBSERV)
                case (SQL_TABLE_SENSORS);   rc = dm_db_prepare(db, db_stmt, SQL_HAS_SENSOR)
                case (SQL_TABLE_TARGETS);   rc = dm_db_prepare(db, db_stmt, SQL_HAS_TARGET)
                case (SQL_TABLE_TRANSFERS); rc = dm_db_prepare(db, db_stmt, SQL_HAS_TRANSFER)
                case default;               return
            end select
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, id)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            if (.not. dm_db_column_is_integer(db_stmt, 0)) exit sql_block

            call dm_db_column(db_stmt, 0, i)
            has = (i == 1)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        integer            :: i, n
        type(db_stmt_type) :: db_stmt

        n = size(receivers)

        rc = E_BOUNDS
        if (n > OBSERV_MAX_NRECEIVERS) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_INSERT_RECEIVER)
            if (dm_is_error(rc)) exit sql_block

            row_loop: do i = 1, n
                rc = E_INVALID
                if (.not. dm_id_is_valid(receivers(i))) exit row_loop

                rc = dm_db_bind(db_stmt, 1, observ_id);    if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 2, i);            if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 3, receivers(i)); if (dm_is_error(rc)) exit row_loop

                rc = dm_db_step(db_stmt);  if (dm_is_error(rc)) exit row_loop
                rc = dm_db_reset(db_stmt); if (dm_is_error(rc)) exit row_loop
            end do row_loop
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        integer            :: i, nreq
        type(db_stmt_type) :: db_stmt

        nreq = size(requests)

        rc = E_BOUNDS
        if (nreq > OBSERV_MAX_NREQUESTS) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_INSERT_REQUEST)
            if (dm_is_error(rc)) exit sql_block

            row_loop: do i = 1, nreq
                rc = dm_db_bind(db_stmt,  1, observ_id);              if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt,  2, i);                      if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt,  3, requests(i)%name);       if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt,  4, requests(i)%timestamp);  if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt,  5, requests(i)%request);    if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt,  6, requests(i)%response);   if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt,  7, requests(i)%delimiter);  if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt,  8, requests(i)%pattern);    if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt,  9, requests(i)%delay);      if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 10, requests(i)%error);      if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 11, requests(i)%mode);       if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 12, requests(i)%retries);    if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 13, requests(i)%state);      if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 14, requests(i)%timeout);    if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 15, requests(i)%nresponses); if (dm_is_error(rc)) exit row_loop

                rc = dm_db_step(db_stmt);  if (dm_is_error(rc)) exit row_loop
                rc = dm_db_reset(db_stmt); if (dm_is_error(rc)) exit row_loop
            end do row_loop
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        integer            :: i, nres
        type(db_stmt_type) :: db_stmt

        nres = size(responses)

        rc = E_BOUNDS
        if (request_idx < 1 .or. request_idx > OBSERV_MAX_NREQUESTS) return
        if (nres > REQUEST_MAX_NRESPONSES) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_INSERT_RESPONSE)
            if (dm_is_error(rc)) exit sql_block

            row_loop: do i = 1, nres
                rc = dm_db_bind(db_stmt, 1, observ_id);          if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 2, request_idx);        if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 3, i);                  if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 4, responses(i)%name);  if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 5, responses(i)%unit);  if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 6, responses(i)%type);  if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 7, responses(i)%error); if (dm_is_error(rc)) exit row_loop
                rc = dm_db_bind(db_stmt, 8, responses(i)%value); if (dm_is_error(rc)) exit row_loop

                rc = dm_db_step(db_stmt);  if (dm_is_error(rc)) exit row_loop
                rc = dm_db_reset(db_stmt); if (dm_is_error(rc)) exit row_loop
            end do row_loop
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        type(db_stmt_type) :: db_stmt

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, 1, sync%id);        if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 2, sync%timestamp); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 3, sync%code);      if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 4, sync%attempts);  if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function db_insert_sync

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
            rc = dm_db_count_beats(db, n)
            if (dm_is_error(rc)) exit sql_block

            if (present(nbeats)) nbeats = n
            if (present(limit))  n      = min(n, limit)

            rc = E_ALLOC
            allocate (beats(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, by='node_id', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_BEATS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, beats(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(beats)) allocate (beats(0))
    end function db_select_beats_array

    integer function db_select_beats_iter(db, db_stmt, beat, limit, validate) result(rc)
        !! Iterator function that returns heatbeats from database in `beat`. An
        !! optional limit may be passed in `limit`. The statement `db_stmt`
        !! must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_beat

        type(db_type),      intent(inout)        :: db       !! Database type.
        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(beat_type),    intent(out)          :: beat     !! Returned beat type.
        integer(kind=i8),   intent(in), optional :: limit    !! Max. number of beats.
        logical,            intent(in), optional :: validate !! Validate column types.

        type(db_query_type) :: db_query

        if (.not. dm_db_is_prepared(db_stmt)) then
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_BEATS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = dm_db_step(db_stmt)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(db_stmt, beat, validate)
    end function db_select_beats_iter

    integer function db_select_data_points_array(db, dps, node_id, sensor_id, target_id, response_name, &
                                                 from, to, error, limit, ndps) result(rc)
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
        integer(kind=i8),           intent(out), optional :: ndps          !! Number of data points.

        integer             :: error_, stat
        integer(kind=i8)    :: i, n
        type(db_query_type) :: db_query
        type(db_stmt_type)  :: db_stmt

        error_ = dm_present(error, E_NONE)
        if (present(ndps)) ndps = 0_i8

        call dm_db_query_where(db_query, 'nodes.id = ?',            node_id)
        call dm_db_query_where(db_query, 'sensors.id = ?',          sensor_id)
        call dm_db_query_where(db_query, 'targets.id = ?',          target_id)
        call dm_db_query_where(db_query, 'responses.name = ?',      response_name)
        call dm_db_query_where(db_query, 'responses.error = ?',     error_)
        call dm_db_query_where(db_query, 'requests.timestamp >= ?', from)
        call dm_db_query_where(db_query, 'requests.timestamp < ?',  to)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NDATA_POINTS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(db_stmt, 0, n)

            call dm_db_finalize(db_stmt, error=rc)
            if (dm_is_error(rc)) exit sql_block

            if (present(ndps))  ndps = n
            if (present(limit)) n    = min(n, limit)

            rc = E_ALLOC
            allocate (dps(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, by='requests.timestamp', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_DATA_POINTS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, dps(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(dps)) allocate (dps(0))
    end function db_select_data_points_array

    integer function db_select_data_points_iter(db, db_stmt, dp, node_id, sensor_id, target_id, response_name, &
                                                from, to, error, limit, validate) result(rc)
        !! Iterator function that returns data points from observations
        !! database in `dp`. This function selects only responses of error
        !! `E_NONE`, unless argument `error` is passed, then only of the given
        !! error code. The statement `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
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
        logical,            intent(in), optional :: validate      !! Validate column types.

        integer             :: error_
        type(db_query_type) :: db_query

        error_ = dm_present(error, E_NONE)

        if (.not. dm_db_is_prepared(db_stmt)) then
            call dm_db_query_where(db_query, 'nodes.id = ?',            node_id)
            call dm_db_query_where(db_query, 'sensors.id = ?',          sensor_id)
            call dm_db_query_where(db_query, 'targets.id = ?',          target_id)
            call dm_db_query_where(db_query, 'responses.name = ?',      response_name)
            call dm_db_query_where(db_query, 'responses.error = ?',     error_)
            call dm_db_query_where(db_query, 'requests.timestamp >= ?', from)
            call dm_db_query_where(db_query, 'requests.timestamp < ?',  to)

            call dm_db_query_set_order(db_query, by='requests.timestamp', desc=.false.)
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_DATA_POINTS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = dm_db_step(db_stmt)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(db_stmt, dp, validate)
    end function db_select_data_points_iter

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

        if (present(min_level)) call dm_db_query_where(db_query, 'level >= ?',     min_level)
        if (present(max_level)) call dm_db_query_where(db_query, 'level <= ?',     max_level)
        if (present(error))     call dm_db_query_where(db_query, 'error = ?',      error)
        if (present(from))      call dm_db_query_where(db_query, 'timestamp >= ?', from)
        if (present(to))        call dm_db_query_where(db_query, 'timestamp < ?',  to)
        if (present(node_id))   call dm_db_query_where(db_query, 'node_id = ?',    node_id)
        if (present(sensor_id)) call dm_db_query_where(db_query, 'sensor_id = ?',  sensor_id)
        if (present(target_id)) call dm_db_query_where(db_query, 'target_id = ?',  target_id)
        if (present(observ_id)) call dm_db_query_where(db_query, 'observ_id = ?',  observ_id)
        if (present(source))    call dm_db_query_where(db_query, 'source = ?',     source)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NLOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(db_stmt, 0, n)

            call dm_db_finalize(db_stmt, error=rc)
            if (dm_is_error(rc)) return

            if (present(nlogs)) nlogs = n
            if (present(limit)) n     = min(n, limit)

            rc = E_ALLOC
            allocate (logs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, by='timestamp', desc=desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_LOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, logs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(logs)) allocate (logs(0))
    end function db_select_logs_array

    integer function db_select_logs_iter(db, db_stmt, log, node_id, sensor_id, target_id, observ_id, source, from, to, &
                                         min_level, max_level, error, desc, limit, validate) result(rc)
        !! Iterator function that returns logs in `logs`. The statement
        !! `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
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
        logical,            intent(in), optional :: validate  !! Validate column types.

        type(db_query_type) :: db_query

        if (.not. dm_db_is_prepared(db_stmt)) then
            if (present(min_level)) call dm_db_query_where(db_query, 'level >= ?',     min_level)
            if (present(max_level)) call dm_db_query_where(db_query, 'level <= ?',     max_level)
            if (present(error))     call dm_db_query_where(db_query, 'error = ?',      error)
            if (present(from))      call dm_db_query_where(db_query, 'timestamp >= ?', from)
            if (present(to))        call dm_db_query_where(db_query, 'timestamp < ?',  to)
            if (present(node_id))   call dm_db_query_where(db_query, 'node_id = ?',    node_id)
            if (present(sensor_id)) call dm_db_query_where(db_query, 'sensor_id = ?',  sensor_id)
            if (present(target_id)) call dm_db_query_where(db_query, 'target_id = ?',  target_id)
            if (present(observ_id)) call dm_db_query_where(db_query, 'observ_id = ?',  observ_id)
            if (present(source))    call dm_db_query_where(db_query, 'source = ?',     source)

            call dm_db_query_set_order(db_query, by='timestamp', desc=desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_LOGS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = dm_db_step(db_stmt)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(db_stmt, log, validate)
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
            rc = dm_db_count_nodes(db, n)
            if (dm_is_error(rc)) exit sql_block

            rc = E_ALLOC
            allocate (nodes(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, by='nodes.id', desc=.false.)

            rc = dm_db_prepare(db, db_stmt, SQL_SELECT_NODES)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, nodes(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(nnodes)) nnodes = n
            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(nodes)) allocate (nodes(0))
    end function db_select_nodes_array

    integer function db_select_nodes_iter(db, db_stmt, node, validate) result(rc)
        !! Iterator function that returns all sensor nodes in `node`. The
        !! statement `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_node

        type(db_type),      intent(inout)        :: db       !! Database type.
        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(node_type),    intent(out)          :: node     !! Returned node data.
        logical,            intent(in), optional :: validate !! Validate column types.

        type(db_query_type) :: db_query

        if (.not. dm_db_is_prepared(db_stmt)) then
            call dm_db_query_set_order(db_query, by='nodes.id', desc=.false.)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NODES))
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = dm_db_step(db_stmt)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(db_stmt, node, validate)
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

        if (present(node_id))   call dm_db_query_where(db_query, 'nodes.id = ?',           node_id)
        if (present(sensor_id)) call dm_db_query_where(db_query, 'sensors.id = ?',         sensor_id)
        if (present(target_id)) call dm_db_query_where(db_query, 'targets.id = ?',         target_id)
        if (present(from))      call dm_db_query_where(db_query, 'observs.timestamp >= ?', from)
        if (present(to))        call dm_db_query_where(db_query, 'observs.timestamp < ?',  to)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NOBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(db_stmt, 0, n)

            call dm_db_finalize(db_stmt, error=rc)
            if (dm_is_error(rc)) return

            if (present(nobservs)) nobservs = n
            if (present(limit))    n        = min(n, limit)

            rc = E_ALLOC
            allocate (observs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, by='observs.timestamp', desc=desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, observs(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)

        if (.not. allocated(observs)) allocate (observs(0))
        if (dm_is_error(rc)) return
        if (dm_present(stub, .false.)) return
        if (size(observs) == 0) return

        rc = db_select_observs_data(db, observs)
    end function db_select_observs_array

    integer function db_select_observs_iter(db, db_stmt, observ, node_id, sensor_id, target_id, from, to, &
                                            desc, limit, stub, validate) result(rc)
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
        !! * `E_DB_DONE` if statement finished.
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
        logical,            intent(in), optional :: validate  !! Validate column types.

        integer             :: i, n
        type(db_query_type) :: db_query

        if (.not. dm_db_is_prepared(db_stmt)) then
            if (present(node_id))   call dm_db_query_where(db_query, 'nodes.id = ?',           node_id)
            if (present(sensor_id)) call dm_db_query_where(db_query, 'sensors.id = ?',         sensor_id)
            if (present(target_id)) call dm_db_query_where(db_query, 'targets.id = ?',         target_id)
            if (present(from))      call dm_db_query_where(db_query, 'observs.timestamp >= ?', from)
            if (present(to))        call dm_db_query_where(db_query, 'observs.timestamp < ?',  to)

            call dm_db_query_set_order(db_query, by='observs.timestamp', desc=desc)
            call dm_db_query_set_limit(db_query, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_OBSERVS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = dm_db_step(db_stmt)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(db_stmt, observ, validate)
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

        integer            :: j, nres
        integer(kind=i8)   :: i, nobs
        type(db_stmt_type) :: db_stmt

        nobs = size(observs, kind=i8)

        rc = E_DB_NO_ROWS
        if (nobs == 0) return

        ! Get receivers (re-use statement).
        do i = 1, nobs
            associate (observ => observs(i))
                if (observ%nreceivers == 0) cycle
                rc = db_select_receivers(db, observ%receivers, observ%id, db_stmt=db_stmt)
                if (dm_is_error(rc)) exit
            end associate
        end do

        call dm_db_finalize(db_stmt)
        if (dm_is_error(rc)) return

        ! Get requests (re-use statement).
        do i = 1, nobs
            associate (observ => observs(i))
                if (observ%nrequests == 0) cycle
                rc = db_select_requests(db, observ%requests, observ%id, db_stmt=db_stmt)
                if (dm_is_error(rc)) exit
            end associate
        end do

        call dm_db_finalize(db_stmt)
        if (dm_is_error(rc)) return

        ! Get responses (re-use statement).
        obs_loop: do i = 1, nobs
            associate (observ => observs(i))
                req_loop: do j = 1, observ%nrequests
                    nres = observ%requests(j)%nresponses
                    if (nres == 0) cycle req_loop

                    rc = db_select_responses(db          = db, &
                                             responses   = observ%requests(j)%responses, &
                                             observ_id   = observ%id, &
                                             request_idx = j, &
                                             db_stmt     = db_stmt)
                    if (dm_is_error(rc)) exit obs_loop
                end do req_loop
            end associate
        end do obs_loop

        call dm_db_finalize(db_stmt)
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
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ

        type(db_type),                      intent(inout)           :: db                               !! Database type.
        character(len=OBSERV_RECEIVER_LEN), intent(out)             :: receivers(OBSERV_MAX_NRECEIVERS) !! Returned receivers array.
        character(len=*),                   intent(in)              :: observ_id                        !! Observation id.
        integer,                            intent(out),   optional :: nreceivers                       !! Number of receivers.
        type(db_stmt_type),                 intent(inout), optional :: db_stmt                          !! Database statement type.

        integer            :: i, n
        type(db_stmt_type) :: db_stmt_

        if (present(db_stmt))    db_stmt_   = db_stmt
        if (present(nreceivers)) nreceivers = 0

        sql_block: block
            if (.not. dm_db_is_prepared(db_stmt_)) then
                rc = dm_db_prepare(db, db_stmt_, SQL_SELECT_RECEIVERS)
                if (dm_is_error(rc)) exit sql_block
            end if

            rc = dm_db_bind(db_stmt_, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            i = 0

            do while (dm_db_step(db_stmt_) == E_DB_ROW)
                rc = E_BOUNDS
                if (i >= OBSERV_MAX_NRECEIVERS) exit

                i = i + 1

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (.not. dm_db_column_is_text(db_stmt_, 0)) exit sql_block
                end if

                call dm_db_column(db_stmt_, 0, receivers(i), n)
            end do

            if (present(nreceivers)) nreceivers = i
            rc = dm_db_reset(db_stmt_)
        end block sql_block

        if (.not. present(db_stmt)) then
            call dm_db_finalize(db_stmt_)
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
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_observ
        use :: dm_request

        type(db_type),      intent(inout)           :: db                             !! Database type.
        type(request_type), intent(out)             :: requests(OBSERV_MAX_NREQUESTS) !! Requests data.
        character(len=*),   intent(in)              :: observ_id                      !! Observation id.
        integer,            intent(out),   optional :: nrequests                      !! Number of requests.
        type(db_stmt_type), intent(inout), optional :: db_stmt                        !! Database statement type.

        integer            :: i, n
        type(db_stmt_type) :: db_stmt_

        if (present(db_stmt))   db_stmt_  = db_stmt
        if (present(nrequests)) nrequests = 0

        sql_block: block
            if (.not. dm_db_is_prepared(db_stmt_)) then
                rc = dm_db_prepare(db, db_stmt_, SQL_SELECT_REQUESTS)
                if (dm_is_error(rc)) exit sql_block
            end if

            rc = dm_db_bind(db_stmt_, 1, observ_id)
            if (dm_is_error(rc)) exit sql_block

            i = 0

            do while (dm_db_step(db_stmt_) == E_DB_ROW)
                rc = E_BOUNDS
                if (i >= OBSERV_MAX_NREQUESTS) exit

                i = i + 1

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (.not. dm_db_column_is_text   (db_stmt_,  0)) exit sql_block
                    if (.not. dm_db_column_is_text   (db_stmt_,  1)) exit sql_block
                    if (.not. dm_db_column_is_text   (db_stmt_,  2)) exit sql_block
                    if (.not. dm_db_column_is_text   (db_stmt_,  3)) exit sql_block
                    if (.not. dm_db_column_is_text   (db_stmt_,  4)) exit sql_block
                    if (.not. dm_db_column_is_text   (db_stmt_,  5)) exit sql_block
                    if (.not. dm_db_column_is_integer(db_stmt_,  6)) exit sql_block
                    if (.not. dm_db_column_is_integer(db_stmt_,  7)) exit sql_block
                    if (.not. dm_db_column_is_integer(db_stmt_,  8)) exit sql_block
                    if (.not. dm_db_column_is_integer(db_stmt_,  9)) exit sql_block
                    if (.not. dm_db_column_is_integer(db_stmt_, 10)) exit sql_block
                    if (.not. dm_db_column_is_integer(db_stmt_, 11)) exit sql_block
                    if (.not. dm_db_column_is_integer(db_stmt_, 12)) exit sql_block
                end if

                call dm_db_column(db_stmt_,  0, requests(i)%name,      n)
                call dm_db_column(db_stmt_,  1, requests(i)%timestamp, n)
                call dm_db_column(db_stmt_,  2, requests(i)%request,   n)
                call dm_db_column(db_stmt_,  3, requests(i)%response,  n)
                call dm_db_column(db_stmt_,  4, requests(i)%delimiter, n)
                call dm_db_column(db_stmt_,  5, requests(i)%pattern,   n)
                call dm_db_column(db_stmt_,  6, requests(i)%delay)
                call dm_db_column(db_stmt_,  7, requests(i)%error)
                call dm_db_column(db_stmt_,  8, requests(i)%mode)
                call dm_db_column(db_stmt_,  9, requests(i)%retries)
                call dm_db_column(db_stmt_, 10, requests(i)%state)
                call dm_db_column(db_stmt_, 11, requests(i)%timeout)
                call dm_db_column(db_stmt_, 12, requests(i)%nresponses)
            end do

            if (present(nrequests)) nrequests = i

            rc = dm_db_reset(db_stmt_)
        end block sql_block

        if (.not. present(db_stmt)) then
            call dm_db_finalize(db_stmt_)
            return
        end if

        db_stmt = db_stmt_
    end function db_select_requests

    integer function db_select_responses(db, responses, observ_id, request_idx, nresponses, db_stmt) result(rc)
        !! Returns all responses from a given observation and request index in
        !! array `responses`. If `statement` is passed, the statement will not
        !! be finalised in order to be re-used again. Finalisation has to be
        !! done by the caller. If `statement` is passed and set to `c_null_ptr`,
        !! it will be prepared by the function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if too many rows are returned.
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_FINALIZE` if statement finalisation failed.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_request
        use :: dm_response

        type(db_type),       intent(inout)           :: db                                !! Database type.
        type(response_type), intent(out)             :: responses(REQUEST_MAX_NRESPONSES) !! Returned responses array.
        character(len=*),    intent(in)              :: observ_id                         !! Observation id.
        integer,             intent(in)              :: request_idx                       !! Request index.
        integer,             intent(out),   optional :: nresponses                        !! Number of responses.
        type(db_stmt_type),  intent(inout), optional :: db_stmt                           !! Database statement type.

        integer            :: i, n
        type(db_stmt_type) :: db_stmt_

        if (present(db_stmt))    db_stmt_   = db_stmt
        if (present(nresponses)) nresponses = 0

        sql_block: block
            if (.not. dm_db_is_prepared(db_stmt_)) then
                rc = dm_db_prepare(db, db_stmt_, SQL_SELECT_RESPONSES)
                if (dm_is_error(rc)) exit sql_block
            end if

            rc = dm_db_bind(db_stmt_, 1, observ_id);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt_, 2, request_idx); if (dm_is_error(rc)) exit sql_block

            i = 0

            do while (dm_db_step(db_stmt_) == E_DB_ROW)
                rc = E_BOUNDS
                if (i >= REQUEST_MAX_NRESPONSES) exit

                i = i + 1

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (.not. dm_db_column_is_text   (db_stmt_, 0)) exit sql_block
                    if (.not. dm_db_column_is_text   (db_stmt_, 1)) exit sql_block
                    if (.not. dm_db_column_is_integer(db_stmt_, 2)) exit sql_block
                    if (.not. dm_db_column_is_integer(db_stmt_, 3)) exit sql_block
                    if (.not. dm_db_column_is_float  (db_stmt_, 4)) exit sql_block
                end if

                call dm_db_column(db_stmt_, 0, responses(i)%name, n)
                call dm_db_column(db_stmt_, 1, responses(i)%unit, n)
                call dm_db_column(db_stmt_, 2, responses(i)%type)
                call dm_db_column(db_stmt_, 3, responses(i)%error)
                call dm_db_column(db_stmt_, 4, responses(i)%value)
            end do

            if (present(nresponses)) nresponses = i
            rc = dm_db_reset(db_stmt_)
        end block sql_block

        if (.not. present(db_stmt)) then
            call dm_db_finalize(db_stmt_)
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

        if (present(node_id)) call dm_db_query_where(db_query, 'nodes.id = ?', node_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_NSENSORS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(db_stmt, 0, n)

            call dm_db_finalize(db_stmt, error=rc)
            if (dm_is_error(rc)) exit sql_block

            if (present(nsensors)) nsensors = n

            rc = E_ALLOC
            allocate (sensors(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(db_query, by='sensors.id', desc=.false.)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_SENSORS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, sensors(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(db_query)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(sensors)) allocate (sensors(0))
    end function db_select_sensors_array

    integer function db_select_sensors_iter(db, db_stmt, sensor, node_id, validate) result(rc)
        !! Iterator function that returns all sensors in `sensor`. The
        !! statement `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if node id is empty.
        !!
        use :: dm_sensor

        type(db_type),      intent(inout)        :: db       !! Database type.
        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(sensor_type),  intent(out)          :: sensor   !! Returned sensor data.
        character(len=*),   intent(in), optional :: node_id  !! Node id.
        logical,            intent(in), optional :: validate !! Validate column types.

        type(db_query_type) :: db_query

        if (.not. dm_db_is_prepared(db_stmt)) then
            if (present(node_id)) then
                rc = E_INVALID
                if (len_trim(node_id) == 0) return
            end if

            if (present(node_id)) call dm_db_query_where(db_query, 'nodes.id = ?', node_id)
            call dm_db_query_set_order(db_query, by='sensors.id', desc=.false.)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(db_query, SQL_SELECT_SENSORS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(db_stmt, db_query)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(db_query)
        end if

        rc = dm_db_step(db_stmt)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(db_stmt, sensor, validate)
    end function db_select_sensors_iter

    integer function db_select_sync(db, type, query, sync) result(rc)
        !! Utility function that returns synchronisation data of given query.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_INVALID` if sync data type is invalid.
        !!
        use :: dm_sync

        type(db_type),    intent(inout) :: db    !! Database type.
        integer,          intent(in)    :: type  !! Sync data type.
        character(len=*), intent(in)    :: query !! Select query.
        type(sync_type),  intent(out)   :: sync  !! Returned sync data.

        type(db_stmt_type) :: db_stmt

        rc = E_INVALID
        if (.not. dm_sync_type_is_valid(type)) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, trim(query))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, sync)
        end block sql_block

        call dm_db_finalize(db_stmt)
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

        integer            :: stat
        integer(kind=i8)   :: i, n
        type(db_stmt_type) :: db_stmt

        nsyncs = 0_i8

        rc = E_INVALID
        if (.not. dm_sync_type_is_valid(type)) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, count_query)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(db_stmt, 0, nsyncs)

            call dm_db_finalize(db_stmt, error=rc)
            if (dm_is_error(rc)) exit sql_block

            n = nsyncs
            if (present(limit)) n = min(limit, nsyncs)

            rc = E_ALLOC
            allocate (syncs(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (nsyncs == 0) exit sql_block

            if (present(limit)) then
                rc = dm_db_prepare(db, db_stmt, query // ' LIMIT ' // dm_itoa(limit))
            else
                rc = dm_db_prepare(db, db_stmt, query)
            end if
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, syncs(i))
                syncs(i)%type = type
                if (dm_is_error(rc)) exit
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_finalize(db_stmt)
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
            rc = dm_db_count_targets(db, n)
            if (dm_is_error(rc)) exit sql_block

            rc = E_ALLOC
            allocate (targets(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            rc = dm_db_prepare(db, db_stmt, SQL_SELECT_TARGETS)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, targets(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            if (present(ntargets)) ntargets = n
            rc = E_NONE
        end block sql_block

        call dm_db_finalize(db_stmt)
        if (.not. allocated(targets)) allocate (targets(0))
    end function db_select_targets_array

    integer function db_select_targets_iter(db, db_stmt, target, validate) result(rc)
        !! Iterator function that returns all targets in `target`. The
        !! statement `db_stmt` must be finalised once finished.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        use :: dm_target

        type(db_type),      intent(inout)        :: db       !! Database type.
        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(target_type),  intent(out)          :: target   !! Target data.
        logical,            intent(in), optional :: validate !! Validate column types.

        if (.not. dm_db_is_prepared(db_stmt)) then
            rc = dm_db_prepare(db, db_stmt, SQL_SELECT_TARGETS)
            if (dm_is_error(rc)) return
        end if

        rc = dm_db_step(db_stmt)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(db_stmt, target, validate)
    end function db_select_targets_iter
end module dm_db_api
