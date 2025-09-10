! Author:  Philipp Engel
! Licence: ISC
module dm_db_count
    !! Database row count functions.
    use :: dm_db
    use :: dm_error
    use :: dm_kind
    use :: dm_sql
    implicit none (type, external)
    private

    ! Public procedures.
    public :: dm_db_count_beats
    public :: dm_db_count_images
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
    public :: dm_db_count_transfers

    ! Private procedures.
    private :: db_count
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_db_count_beats(db, n) result(rc)
        !! Returns number of rows in table `beats`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_BEATS, n)
    end function dm_db_count_beats

    integer function dm_db_count_images(db, n) result(rc)
        !! Returns number of rows in table `images`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_IMAGES, n)
    end function dm_db_count_images

    integer function dm_db_count_logs(db, n) result(rc)
        !! Returns number of rows in table `logs`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_LOGS, n)
    end function dm_db_count_logs

    integer function dm_db_count_nodes(db, n) result(rc)
        !! Returns number of rows in table `nodes`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_NODES, n)
    end function dm_db_count_nodes

    integer function dm_db_count_observs(db, n) result(rc)
        !! Returns number of rows in table `observs`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_OBSERVS, n)
    end function dm_db_count_observs

    integer function dm_db_count_receivers(db, n) result(rc)
        !! Returns number of rows in table `receivers`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_RECEIVERS, n)
    end function dm_db_count_receivers

    integer function dm_db_count_requests(db, n) result(rc)
        !! Returns number of rows in table `requests`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_REQUESTS, n)
    end function dm_db_count_requests

    integer function dm_db_count_responses(db, n) result(rc)
        !! Returns number of rows in table `responses`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_RESPONSES, n)
    end function dm_db_count_responses

    integer function dm_db_count_sensors(db, n) result(rc)
        !! Returns number of rows in table `sensors`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SENSORS, n)
    end function dm_db_count_sensors

    integer function dm_db_count_sync_logs(db, n) result(rc)
        !! Returns number of rows in table `sync_logs`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SYNC_LOGS, n)
    end function dm_db_count_sync_logs

    integer function dm_db_count_sync_nodes(db, n) result(rc)
        !! Returns number of rows in table `sync_nodes`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SYNC_NODES, n)
    end function dm_db_count_sync_nodes

    integer function dm_db_count_sync_observs(db, n) result(rc)
        !! Returns number of rows in table `sync_observs`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SYNC_OBSERVS, n)
    end function dm_db_count_sync_observs

    integer function dm_db_count_sync_sensors(db, n) result(rc)
        !! Returns number of rows in table `sync_sensors`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SYNC_SENSORS, n)
    end function dm_db_count_sync_sensors

    integer function dm_db_count_sync_targets(db, n) result(rc)
        !! Returns number of rows in table `sync_targets`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_SYNC_TARGETS, n)
    end function dm_db_count_sync_targets

    integer function dm_db_count_targets(db, n) result(rc)
        !! Returns number of rows in table `targets`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_TARGETS, n)
    end function dm_db_count_targets

    integer function dm_db_count_transfers(db, n) result(rc)
        !! Returns number of rows in table `transfers`.
        type(db_type), intent(inout) :: db !! Database type.
        integer(i8),   intent(out)   :: n  !! Number of rows in table.

        rc = db_count(db, SQL_TABLE_TRANSFERS, n)
    end function dm_db_count_transfers

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
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
        type(db_type), intent(inout) :: db    !! Database type.
        integer,       intent(in)    :: table !! Table type from `dm_sql`.
        integer(i8),   intent(out)   :: n     !! Number of rows in table.

        type(db_stmt_type) :: db_stmt

        n = 0_i8
        rc = E_INVALID
        if (table < SQL_TABLE_NODES .or. table > SQL_TABLE_LAST) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, 'SELECT COUNT(row_id) FROM ' // SQL_TABLE_NAMES(table))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            rc = E_DB_TYPE
            if (.not. dm_db_column_is_integer(db_stmt, 0)) exit sql_block

            rc = E_NONE
            call dm_db_column(db_stmt, 0, n)
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function db_count
end module dm_db_count
