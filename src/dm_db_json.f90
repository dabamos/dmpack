! Author:  Philipp Engel
! Licence: ISC
module dm_db_json
    !! Database JSON functions.
    use :: dm_db
    use :: dm_db_count
    use :: dm_db_query
    use :: dm_db_row
    use :: dm_error
    use :: dm_kind
    use :: dm_sql
    implicit none (type, external)
    private

    interface dm_db_json_select_beats
        !! Generic JSON logs select function.
        module procedure :: db_json_select_beats_array
        module procedure :: db_json_select_beats_iter
    end interface dm_db_json_select_beats

    interface dm_db_json_select_logs
        !! Generic JSON logs select function.
        module procedure :: db_json_select_logs_array
        module procedure :: db_json_select_logs_iter
    end interface dm_db_json_select_logs

    interface dm_db_json_select_nodes
        !! Generic JSON nodes select function.
        module procedure :: db_json_select_nodes_array
        module procedure :: db_json_select_nodes_iter
    end interface dm_db_json_select_nodes

    ! Public procedures.
    public :: dm_db_json_select_beat
    public :: dm_db_json_select_beats
    public :: dm_db_json_select_log
    public :: dm_db_json_select_logs
    public :: dm_db_json_select_node
    public :: dm_db_json_select_nodes

    ! Private procedures.
    private :: db_json_select_beats_array
    private :: db_json_select_beats_iter
    private :: db_json_select_logs_array
    private :: db_json_select_logs_iter
    private :: db_json_select_nodes_array
    private :: db_json_select_nodes_iter
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_db_json_select_beat(db, json, node_id) result(rc)
        !! Returns heartbeat associated with given node id as allocatable
        !! character `json` in JSON format.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        type(db_type),                 intent(inout) :: db      !! Database type.
        character(len=:), allocatable, intent(out)   :: json    !! Returned JSON.
        character(len=*),              intent(in)    :: node_id !! Node id.

        type(db_query_type) :: dbq
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        call dm_db_query_where(dbq, 'node_id = ?', node_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(dbq, SQL_SELECT_JSON_BEATS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, dbq)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, json)
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(json)) json = ''
    end function dm_db_json_select_beat

    integer function dm_db_json_select_log(db, json, log_id) result(rc)
        !! Returns log associated with given id as allocatable character in
        !! JSON format in `json`. If no log has been found, the string will
        !! be empty and the function returns `E_DB_NO_ROWS`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        type(db_type),                 intent(inout) :: db     !! Database type.
        character(len=:), allocatable, intent(out)   :: json   !! Returned JSON.
        character(len=*),              intent(in)    :: log_id !! Log id.

        type(db_query_type) :: dbq
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(log_id) == 0) return

        call dm_db_query_where(dbq, 'id = ?', log_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(dbq, SQL_SELECT_JSON_LOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, dbq)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, json)
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(json)) json = ''
    end function dm_db_json_select_log

    integer function dm_db_json_select_node(db, json, node_id) result(rc)
        !! Returns node associated with given node id as allocatable character
        !! `json` in JSON format.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !! * `E_INVALID` if id is invalid.
        !!
        type(db_type),                 intent(inout) :: db      !! Database type.
        character(len=:), allocatable, intent(out)   :: json    !! Returned JSON.
        character(len=*),              intent(in)    :: node_id !! Node id.

        type(db_query_type) :: dbq
        type(db_stmt_type)  :: db_stmt

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        call dm_db_query_where(dbq, 'id = ?', node_id)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(dbq, SQL_SELECT_JSON_NODES))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, dbq)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(db_stmt, json)
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(json)) json = ''
    end function dm_db_json_select_node

    integer function db_json_select_beats_array(db, strings, limit, nbeats) result(rc)
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
        type(db_query_type) :: dbq
        type(db_stmt_type)  :: db_stmt

        if (present(nbeats)) nbeats = 0_i8

        sql_block: block
            rc = dm_db_count_beats(db, n)
            if (dm_is_error(rc)) exit sql_block

            if (present(nbeats)) nbeats = n
            if (present(limit))  n      = min(n, limit)

            rc = E_ALLOC
            allocate (strings(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_limit(dbq, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(dbq, SQL_SELECT_JSON_BEATS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, dbq)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(strings)) allocate (strings(0))
    end function db_json_select_beats_array

    integer function db_json_select_beats_iter(db, db_stmt, json, limit, validate) result(rc)
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
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        type(db_type),                 intent(inout)        :: db       !! Database type.
        type(db_stmt_type),            intent(inout)        :: db_stmt  !! Database statement type.
        character(len=:), allocatable, intent(out)          :: json     !! Returned JSON.
        integer(kind=i8),              intent(in), optional :: limit    !! Max. number of beats.
        logical,                       intent(in), optional :: validate !! Validate column types.

        type(db_query_type) :: dbq

        if (.not. dm_db_is_prepared(db_stmt)) then
            call dm_db_query_set_limit(dbq, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(dbq, SQL_SELECT_JSON_BEATS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(db_stmt, dbq)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(dbq)
        end if

        rc = dm_db_step(db_stmt)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(db_stmt, json, validate)
    end function db_json_select_beats_iter

    integer function db_json_select_logs_array(db, strings, node_id, sensor_id, target_id, source, from, to, &
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
        type(db_query_type) :: dbq
        type(db_stmt_type)  :: db_stmt

        if (present(nlogs)) nlogs = 0_i8

        if (present(min_level)) call dm_db_query_where(dbq, 'level >= ?',     min_level)
        if (present(max_level)) call dm_db_query_where(dbq, 'level <= ?',     max_level)
        if (present(error))     call dm_db_query_where(dbq, 'error = ?',      error)
        if (present(from))      call dm_db_query_where(dbq, 'timestamp >= ?', from)
        if (present(to))        call dm_db_query_where(dbq, 'timestamp < ?',  to)
        if (present(node_id))   call dm_db_query_where(dbq, 'node_id = ?',    node_id)
        if (present(sensor_id)) call dm_db_query_where(dbq, 'sensor_id = ?',  sensor_id)
        if (present(target_id)) call dm_db_query_where(dbq, 'target_id = ?',  target_id)
        if (present(source))    call dm_db_query_where(dbq, 'source = ?',     source)

        sql_block: block
            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(dbq, SQL_SELECT_NLOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, dbq)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(db_stmt)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(db_stmt, 0, n)

            call dm_db_finalize(db_stmt, error=rc)
            if (dm_is_error(rc)) return

            if (present(nlogs)) nlogs = n
            if (present(limit)) n     = min(n, limit)

            rc = E_ALLOC
            allocate (strings(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(dbq, by='timestamp', desc=desc)
            call dm_db_query_set_limit(dbq, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(dbq, SQL_SELECT_JSON_LOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, dbq)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(strings)) allocate (strings(0))
    end function db_json_select_logs_array

    integer function db_json_select_logs_iter(db, db_stmt, json, node_id, sensor_id, target_id, source, from, to, &
                                              min_level, max_level, error, desc, limit, validate) result(rc)
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
        !! * `E_DB_DONE` if statement finished.
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
        logical,                       intent(in), optional :: validate  !! Validate column types.

        type(db_query_type) :: dbq

        if (.not. dm_db_is_prepared(db_stmt)) then
            if (present(min_level)) call dm_db_query_where(dbq, 'level >= ?',     min_level)
            if (present(max_level)) call dm_db_query_where(dbq, 'level <= ?',     max_level)
            if (present(error))     call dm_db_query_where(dbq, 'error = ?',      error)
            if (present(from))      call dm_db_query_where(dbq, 'timestamp >= ?', from)
            if (present(to))        call dm_db_query_where(dbq, 'timestamp < ?',  to)
            if (present(node_id))   call dm_db_query_where(dbq, 'node_id = ?',    node_id)
            if (present(sensor_id)) call dm_db_query_where(dbq, 'sensor_id = ?',  sensor_id)
            if (present(target_id)) call dm_db_query_where(dbq, 'target_id = ?',  target_id)
            if (present(source))    call dm_db_query_where(dbq, 'source = ?',     source)

            call dm_db_query_set_order(dbq, by='timestamp', desc=desc)
            call dm_db_query_set_limit(dbq, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(dbq, SQL_SELECT_JSON_LOGS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(db_stmt, dbq)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(dbq)
        end if

        rc = dm_db_step(db_stmt)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(db_stmt, json, validate)
    end function db_json_select_logs_iter

    integer function db_json_select_nodes_array(db, strings, limit, nnodes) result(rc)
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
        type(db_query_type) :: dbq
        type(db_stmt_type)  :: db_stmt

        if (present(nnodes)) nnodes = 0_i8

        sql_block: block
            rc = dm_db_count_nodes(db, n)
            if (dm_is_error(rc)) exit sql_block

            if (present(nnodes)) nnodes = n
            if (present(limit))  n      = min(n, limit)

            rc = E_ALLOC
            allocate (strings(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(dbq, by='nodes.row_id', desc=.false.)
            call dm_db_query_set_limit(dbq, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(dbq, SQL_SELECT_JSON_NODES))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(db_stmt, dbq)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(db_stmt)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(db_stmt, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(db_stmt)
        if (.not. allocated(strings)) allocate (strings(0))
    end function db_json_select_nodes_array

    integer function db_json_select_nodes_iter(db, db_stmt, json, limit, validate) result(rc)
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
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        type(db_type),                 intent(inout)        :: db       !! Database type.
        type(db_stmt_type),            intent(inout)        :: db_stmt  !! Database statement type.
        character(len=:), allocatable, intent(out)          :: json     !! Returned JSON.
        integer(kind=i8),              intent(in), optional :: limit    !! Max. number of nodes.
        logical,                       intent(in), optional :: validate !! Validate column types.

        type(db_query_type) :: dbq

        if (.not. dm_db_is_prepared(db_stmt)) then
            call dm_db_query_set_order(dbq, by='nodes.row_id', desc=.false.)
            call dm_db_query_set_limit(dbq, limit)

            rc = dm_db_prepare(db, db_stmt, dm_db_query_build(dbq, SQL_SELECT_JSON_NODES))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(db_stmt, dbq)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(dbq)
        end if

        rc = dm_db_step(db_stmt)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(db_stmt, json, validate)
    end function db_json_select_nodes_iter
end module dm_db_json
