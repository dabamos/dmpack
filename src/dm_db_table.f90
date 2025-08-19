! Author:  Philipp Engel
! Licence: ISC
module dm_db_table
    !! Database table access module.
    use :: dm_db
    use :: dm_db_query
    use :: dm_error
    use :: dm_sql
    use :: dm_util
    implicit none (type, external)
    private

    ! Public procedures.
    public :: dm_db_table_create_beats
    public :: dm_db_table_create_images
    public :: dm_db_table_create_logs
    public :: dm_db_table_create_observs
    public :: dm_db_table_create_sync_images
    public :: dm_db_table_create_sync_logs
    public :: dm_db_table_create_sync_observs
    public :: dm_db_table_create_transfers
    public :: dm_db_table_has
    public :: dm_db_table_has_beats
    public :: dm_db_table_has_images
    public :: dm_db_table_has_logs
    public :: dm_db_table_has_observs
    public :: dm_db_table_has_sync_images
    public :: dm_db_table_has_sync_logs
    public :: dm_db_table_has_sync_observs
    public :: dm_db_table_has_transfers
    public :: dm_db_table_select
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_db_table_create_beats(db) result(rc)
        !! Creates beats table in given database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table or index creation failed.
        !! * `E_NULL` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout) :: db !! Database type.

        integer :: i

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_NULL
        if (.not. dm_db_is_connected(db)) return

        rc = dm_db_exec(db, SQL_CREATE_BEATS)
        if (dm_is_error(rc)) return

        do i = 1, size(SQL_CREATE_BEAT_INDICES)
            rc = dm_db_exec(db, trim(SQL_CREATE_BEAT_INDICES(i)))
            if (dm_is_error(rc)) return
        end do

        rc = E_NONE
    end function dm_db_table_create_beats

    integer function dm_db_table_create_images(db, sync, transfer) result(rc)
        !! Creates images table in given database. Adds syncs table if argument
        !! `sync` is `.true.`, and transfers table if argument `transfer` is
        !! `.true.`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table or index creation failed.
        !! * `E_NULL` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout)        :: db       !! Database type.
        logical,       intent(in), optional :: sync     !! Add table `sync_logs`.
        logical,       intent(in), optional :: transfer !! Add table `transfers`.

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_NULL
        if (.not. dm_db_is_connected(db)) return

        rc = dm_db_exec(db, SQL_CREATE_IMAGES)
        if (dm_is_error(rc)) return

        ! Create sync images table.
        if (dm_present(sync, .false.)) then
            rc = dm_db_table_create_sync_images(db)
            if (dm_is_error(rc)) return
        end if

        ! Create transfers table.
        if (dm_present(transfer, .false.)) then
            rc = dm_db_table_create_transfers(db)
            if (dm_is_error(rc)) return
        end if
    end function dm_db_table_create_images

    integer function dm_db_table_create_logs(db, sync) result(rc)
        !! Creates logs table in given database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table or index creation failed.
        !! * `E_NULL` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout)        :: db   !! Database type.
        logical,       intent(in), optional :: sync !! Create synchronisation tables.

        integer :: i

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_NULL
        if (.not. dm_db_is_connected(db)) return

        ! Create logs table.
        rc = dm_db_exec(db, SQL_CREATE_LOGS)
        if (dm_is_error(rc)) return

        ! Create sync logs table.
        if (dm_present(sync, .false.)) then
            rc = dm_db_table_create_sync_logs(db)
            if (dm_is_error(rc)) return
        end if

        ! Create indices.
        do i = 1, size(SQL_CREATE_LOG_INDICES)
            rc = dm_db_exec(db, trim(SQL_CREATE_LOG_INDICES(i)))
            if (dm_is_error(rc)) return
        end do

        rc = E_NONE
    end function dm_db_table_create_logs

    integer function dm_db_table_create_observs(db, sync) result(rc)
        !! Initialises a connected SQLite 3 database by creating all necessary
        !! tables if they do not exist already. The function also creates
        !! additional indices and triggers on the tables.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table, index, or trigger creation failed.
        !! * `E_NULL` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout)        :: db   !! Database type.
        logical,       intent(in), optional :: sync !! Create synchronisation tables.

        integer :: i

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_NULL
        if (.not. dm_db_is_connected(db)) return

        ! Create tables.
        rc = dm_db_exec(db, SQL_CREATE_NODES);     if (dm_is_error(rc)) return
        rc = dm_db_exec(db, SQL_CREATE_SENSORS);   if (dm_is_error(rc)) return
        rc = dm_db_exec(db, SQL_CREATE_TARGETS);   if (dm_is_error(rc)) return
        rc = dm_db_exec(db, SQL_CREATE_OBSERVS);   if (dm_is_error(rc)) return
        rc = dm_db_exec(db, SQL_CREATE_RECEIVERS); if (dm_is_error(rc)) return
        rc = dm_db_exec(db, SQL_CREATE_REQUESTS);  if (dm_is_error(rc)) return
        rc = dm_db_exec(db, SQL_CREATE_RESPONSES); if (dm_is_error(rc)) return

        ! Create sync tables.
        if (dm_present(sync, .false.)) then
            rc = dm_db_table_create_sync_observs(db)
            if (dm_is_error(rc)) return
        end if

        ! Add additional indices.
        do i = 1, size(SQL_CREATE_OBSERV_INDICES)
            rc = dm_db_exec(db, trim(SQL_CREATE_OBSERV_INDICES(i)))
            if (dm_is_error(rc)) return
        end do

        ! Add triggers.
        rc = dm_db_exec(db, SQL_DELETE_OBSERV_TRIGGER)
        if (dm_is_error(rc)) return

        rc = E_NONE
    end function dm_db_table_create_observs

    integer function dm_db_table_create_sync_images(db) result(rc)
        !! Creates image synchronisation table.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table or index creation failed.
        !! * `E_NULL` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout) :: db !! Database type.

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_NULL
        if (.not. dm_db_is_connected(db)) return

        rc = dm_db_exec(db, SQL_CREATE_SYNC_IMAGES)
    end function dm_db_table_create_sync_images

    integer function dm_db_table_create_sync_logs(db) result(rc)
        !! Creates log synchronisation table.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table or index creation failed.
        !! * `E_NULL` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout) :: db !! Database type.

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_NULL
        if (.not. dm_db_is_connected(db)) return

        rc = dm_db_exec(db, SQL_CREATE_SYNC_LOGS)
    end function dm_db_table_create_sync_logs

    integer function dm_db_table_create_sync_observs(db) result(rc)
        !! Creates observation synchronisation tables (nodes, sensors, targets,
        !! observations).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table, index, or trigger creation failed.
        !! * `E_NULL` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout) :: db !! Database type.

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_NULL
        if (.not. dm_db_is_connected(db)) return

        rc = dm_db_exec(db, SQL_CREATE_SYNC_NODES);   if (dm_is_error(rc)) return
        rc = dm_db_exec(db, SQL_CREATE_SYNC_OBSERVS); if (dm_is_error(rc)) return
        rc = dm_db_exec(db, SQL_CREATE_SYNC_SENSORS); if (dm_is_error(rc)) return
        rc = dm_db_exec(db, SQL_CREATE_SYNC_TARGETS); if (dm_is_error(rc)) return
    end function dm_db_table_create_sync_observs

    integer function dm_db_table_create_transfers(db) result(rc)
        !! Creates transfers table in given database.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_EXEC` if table or index creation failed.
        !! * `E_NULL` if the database is not connected.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout) :: db !! Database type.

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        rc = E_NULL
        if (.not. dm_db_is_connected(db)) return

        rc = dm_db_exec(db, SQL_CREATE_TRANSFERS)
        if (dm_is_error(rc)) return

        rc = E_NONE
    end function dm_db_table_create_transfers

    logical function dm_db_table_has(db, table) result(has)
        !! Returns `.true.` if given table exists in database.
        type(db_type), intent(inout) :: db    !! Database type.
        integer,       intent(in)    :: table !! Table enumerator.

        integer            :: rc
        type(db_stmt_type) :: db_stmt

        has = .false.
        if (table < SQL_TABLE_NODES .or. table > SQL_TABLE_LAST) return

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, SQL_SELECT_TABLE);   if (dm_is_error(rc)) exit sql_block
            rc = dm_db_bind(db_stmt, 1, SQL_TABLE_NAMES(table)); if (dm_is_error(rc)) exit sql_block
            rc = dm_db_step(db_stmt);                            if (rc /= E_DB_ROW)  exit sql_block
            has = .true.
        end block sql_block

        call dm_db_finalize(db_stmt)
    end function dm_db_table_has

    logical function dm_db_table_has_beats(db) result(has)
        !! Returns `.true.` if database contains beats table.
        type(db_type), intent(inout) :: db !! Database type.

        has = dm_db_table_has(db, SQL_TABLE_BEATS)
    end function dm_db_table_has_beats

    logical function dm_db_table_has_images(db) result(has)
        !! Returns `.true.` if database contains images table.
        type(db_type), intent(inout) :: db !! Database type.

        has = dm_db_table_has(db, SQL_TABLE_IMAGES)
    end function dm_db_table_has_images

    logical function dm_db_table_has_logs(db) result(has)
        !! Returns `.true.` if database contains logs table.
        type(db_type), intent(inout) :: db !! Database type.

        has = dm_db_table_has(db, SQL_TABLE_LOGS)
    end function dm_db_table_has_logs

    logical function dm_db_table_has_observs(db) result(has)
        !! Returns `.true.` if database contains observation tables (`nodes`,
        !! `sensors`, `targets`, `observs`, `receivers`, `requests`,
        !! `responses`).
        type(db_type), intent(inout) :: db !! Database type.

        has = .false.
        if (.not. dm_db_table_has(db, SQL_TABLE_NODES))     return
        if (.not. dm_db_table_has(db, SQL_TABLE_SENSORS))   return
        if (.not. dm_db_table_has(db, SQL_TABLE_TARGETS))   return
        if (.not. dm_db_table_has(db, SQL_TABLE_OBSERVS))   return
        if (.not. dm_db_table_has(db, SQL_TABLE_RECEIVERS)) return
        if (.not. dm_db_table_has(db, SQL_TABLE_REQUESTS))  return
        if (.not. dm_db_table_has(db, SQL_TABLE_RESPONSES)) return
        has = .true.
    end function dm_db_table_has_observs

    logical function dm_db_table_has_sync_images(db) result(has)
        !! Returns `.true.` if database contains image synchronisation tables.
        type(db_type), intent(inout) :: db !! Database type.

        has = dm_db_table_has(db, SQL_TABLE_SYNC_IMAGES)
    end function dm_db_table_has_sync_images

    logical function dm_db_table_has_sync_logs(db) result(has)
        !! Returns `.true.` if database contains log synchronisation tables.
        type(db_type), intent(inout) :: db !! Database type.

        has = dm_db_table_has(db, SQL_TABLE_SYNC_LOGS)
    end function dm_db_table_has_sync_logs

    logical function dm_db_table_has_sync_observs(db) result(has)
        !! Returns `.true.` if database contains observation synchronisation tables.
        type(db_type), intent(inout) :: db !! Database type.

        has = .false.
        if (.not. dm_db_table_has(db, SQL_TABLE_SYNC_NODES))   return
        if (.not. dm_db_table_has(db, SQL_TABLE_SYNC_SENSORS)) return
        if (.not. dm_db_table_has(db, SQL_TABLE_SYNC_TARGETS)) return
        if (.not. dm_db_table_has(db, SQL_TABLE_SYNC_OBSERVS)) return
        has = .true.
    end function dm_db_table_has_sync_observs

    logical function dm_db_table_has_transfers(db) result(has)
        !! Returns `.true.` if database contains transfers table.
        type(db_type), intent(inout) :: db !! Database type.

        has = dm_db_table_has(db, SQL_TABLE_TRANSFERS)
    end function dm_db_table_has_transfers

    integer function dm_db_table_select(db, tables) result(rc)
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
            rc = dm_db_prepare(db, db_stmt, SQL_SELECT_TABLES)
            if (dm_is_error(rc)) exit sql_block

            i = 1

            do
                rc = dm_db_step(db_stmt)

                if (rc == E_DB_DONE) rc = E_NONE
                if (rc /= E_DB_ROW) exit

                if (i == 1) then
                    rc = E_DB_TYPE
                    if (.not. dm_db_column_is_integer(db_stmt, 0)) exit
                    if (.not. dm_db_column_is_text   (db_stmt, 1)) exit
                end if

                call dm_db_column(db_stmt, 0, n)
                call dm_db_column(db_stmt, 1, table)

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

        call dm_db_finalize(db_stmt)
        if (.not. allocated(tables)) allocate (tables(0))
        if (size(tables) == 0) rc = E_DB_NO_ROWS
    end function dm_db_table_select
end module dm_db_table
