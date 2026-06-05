! Author:  Philipp Engel
! Licence: ISC
module dm_db_json
    !! Database JSON functions.
    use :: dm_db
    use :: dm_db_count
    use :: dm_db_query
    use :: dm_db_row
    use :: dm_db_sql
    use :: dm_error
    use :: dm_kind
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

    interface dm_db_json_select_observs
        !! Generic JSON observs select function.
        module procedure :: db_json_select_observs_array
        module procedure :: db_json_select_observs_iter
    end interface dm_db_json_select_observs

    ! Public procedures.
    public :: dm_db_json_select_beat
    public :: dm_db_json_select_beats
    public :: dm_db_json_select_log
    public :: dm_db_json_select_logs
    public :: dm_db_json_select_node
    public :: dm_db_json_select_nodes
    public :: dm_db_json_select_observ
    public :: dm_db_json_select_observs

    ! Private procedures.
    private :: db_json_select_beats_array
    private :: db_json_select_beats_iter
    private :: db_json_select_logs_array
    private :: db_json_select_logs_iter
    private :: db_json_select_nodes_array
    private :: db_json_select_nodes_iter
    private :: db_json_select_observs_array
    private :: db_json_select_observs_iter
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS
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
        type(db_type),             intent(inout) :: db      !! Database.
        character(:), allocatable, intent(out)   :: json    !! Returned JSON.
        character(*),              intent(in)    :: node_id !! Node id.

        type(db_query_type) :: dbq
        type(db_stmt_type)  :: dbs

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        call dm_db_query_where(dbq, 'node_id = ?', node_id)

        sql_block: block
            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_BEATS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(dbs)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(dbs, json)
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(dbs)
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
        type(db_type),             intent(inout) :: db     !! Database.
        character(:), allocatable, intent(out)   :: json   !! Returned JSON.
        character(*),              intent(in)    :: log_id !! Log id.

        type(db_query_type) :: dbq
        type(db_stmt_type)  :: dbs

        rc = E_INVALID
        if (len_trim(log_id) == 0) return

        call dm_db_query_where(dbq, 'id = ?', log_id)

        sql_block: block
            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_LOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(dbs)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(dbs, json)
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(dbs)
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
        type(db_type),             intent(inout) :: db      !! Database.
        character(:), allocatable, intent(out)   :: json    !! Returned JSON.
        character(*),              intent(in)    :: node_id !! Node id.

        type(db_query_type) :: dbq
        type(db_stmt_type)  :: dbs

        rc = E_INVALID
        if (len_trim(node_id) == 0) return

        call dm_db_query_where(dbq, 'id = ?', node_id)

        sql_block: block
            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_NODES))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(dbs)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(dbs, json)
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(dbs)
        if (.not. allocated(json)) json = ''
    end function dm_db_json_select_node

    integer function dm_db_json_select_observ(db, json, observ_id) result(rc)
        !! Returns observation associated with given observ id as allocatable
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
        type(db_type),             intent(inout) :: db        !! Database.
        character(:), allocatable, intent(out)   :: json      !! Returned JSON.
        character(*),              intent(in)    :: observ_id !! Observation id.

        type(db_query_type) :: dbq
        type(db_stmt_type)  :: dbs

        rc = E_INVALID
        if (len_trim(observ_id) == 0) return

        call dm_db_query_where(dbq, 'id = ?', observ_id)

        sql_block: block
            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_OBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(dbs)
            if (rc /= E_DB_ROW) exit sql_block

            rc = dm_db_row_next(dbs, json)
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(dbs)
        if (.not. allocated(json)) json = ''
    end function dm_db_json_select_observ

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
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

        type(db_type),                  intent(inout)         :: db         !! Database.
        type(string_type), allocatable, intent(out)           :: strings(:) !! Returned JSON array.
        integer(i8),                    intent(in),  optional :: limit      !! Max. number of beats.
        integer(i8),                    intent(out), optional :: nbeats     !! Number of beats.

        integer             :: stat
        integer(i8)         :: i, n
        type(db_query_type) :: dbq
        type(db_stmt_type)  :: dbs

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

            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_BEATS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(dbs)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(dbs, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(dbs)
        if (.not. allocated(strings)) allocate (strings(0))
    end function db_json_select_beats_array

    integer function db_json_select_beats_iter(db, dbs, json, limit, validate) result(rc)
        !! Iterator function that returns beats in JSON format in allocatable
        !! string `json`. The statement `dbs` must be finalised once
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
        type(db_type),             intent(inout)        :: db       !! Database.
        type(db_stmt_type),        intent(inout)        :: dbs      !! Database statement.
        character(:), allocatable, intent(out)          :: json     !! Returned JSON.
        integer(i8),               intent(in), optional :: limit    !! Max. number of beats.
        logical,                   intent(in), optional :: validate !! Validate column types.

        type(db_query_type) :: dbq

        if (.not. dm_db_is_prepared(dbs)) then
            call dm_db_query_set_limit(dbq, limit)

            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_BEATS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(dbq)
        end if

        rc = dm_db_step(dbs)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(dbs, json, validate)
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

        type(db_type),                  intent(inout)         :: db         !! Database.
        type(string_type), allocatable, intent(out)           :: strings(:) !! Returned JSON array.
        character(*),                   intent(in),  optional :: node_id    !! Node id.
        character(*),                   intent(in),  optional :: sensor_id  !! Sensor id.
        character(*),                   intent(in),  optional :: target_id  !! Target id.
        character(*),                   intent(in),  optional :: source     !! Source name.
        character(*),                   intent(in),  optional :: from       !! Begin of time range.
        character(*),                   intent(in),  optional :: to         !! End of time range.
        integer,                        intent(in),  optional :: min_level  !! Minimum log level.
        integer,                        intent(in),  optional :: max_level  !! Maximum log level.
        integer,                        intent(in),  optional :: error      !! Error code.
        logical,                        intent(in),  optional :: desc       !! Descending order.
        integer(i8),                    intent(in),  optional :: limit      !! Max. numbers of logs.
        integer(i8),                    intent(out), optional :: nlogs      !! Number of logs.

        integer             :: stat
        integer(i8)         :: i, n
        type(db_query_type) :: dbq
        type(db_stmt_type)  :: dbs

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
            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_NLOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(dbs)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(dbs, 0, n)

            call dm_db_finalize(dbs, error=rc)
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

            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_LOGS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(dbs)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(dbs, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(dbs)
        if (.not. allocated(strings)) allocate (strings(0))
    end function db_json_select_logs_array

    integer function db_json_select_logs_iter(db, dbs, json, node_id, sensor_id, target_id, source, from, to, &
                                              min_level, max_level, error, desc, limit, validate) result(rc)
        !! Iterator function that returns logs in JSON format in allocatable
        !! character `json`. The statement `dbs` must be finalised once
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
        type(db_type),             intent(inout)        :: db        !! Database.
        type(db_stmt_type),        intent(inout)        :: dbs       !! Database statement.
        character(:), allocatable, intent(out)          :: json      !! Returned JSON.
        character(*),              intent(in), optional :: node_id   !! Node id.
        character(*),              intent(in), optional :: sensor_id !! Sensor id.
        character(*),              intent(in), optional :: target_id !! Target id.
        character(*),              intent(in), optional :: source    !! Source name.
        character(*),              intent(in), optional :: from      !! Begin of time range.
        character(*),              intent(in), optional :: to        !! End of time range.
        integer,                   intent(in), optional :: min_level !! Minimum log level.
        integer,                   intent(in), optional :: max_level !! Maximum log level.
        integer,                   intent(in), optional :: error     !! Error code.
        logical,                   intent(in), optional :: desc      !! Descending order.
        integer(i8),               intent(in), optional :: limit     !! Max. numbers of logs.
        logical,                   intent(in), optional :: validate  !! Validate column types.

        type(db_query_type) :: dbq

        if (.not. dm_db_is_prepared(dbs)) then
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

            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_LOGS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(dbq)
        end if

        rc = dm_db_step(dbs)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(dbs, json, validate)
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

        type(db_type),                  intent(inout)         :: db         !! Database.
        type(string_type), allocatable, intent(out)           :: strings(:) !! Returned JSON array.
        integer(i8),                    intent(in),  optional :: limit      !! Max. number of nodes.
        integer(i8),                    intent(out), optional :: nnodes     !! Number of nodes.

        integer             :: stat
        integer(i8)         :: i, n
        type(db_query_type) :: dbq
        type(db_stmt_type)  :: dbs

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

            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_NODES))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(dbs)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(dbs, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(dbs)
        if (.not. allocated(strings)) allocate (strings(0))
    end function db_json_select_nodes_array

    integer function db_json_select_nodes_iter(db, dbs, json, limit, validate) result(rc)
        !! Iterator function that returns nodes in JSON format in allocatable
        !! string `json`. The statement `dbs` must be finalised once
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
        type(db_type),             intent(inout)        :: db       !! Database.
        type(db_stmt_type),        intent(inout)        :: dbs      !! Database statement.
        character(:), allocatable, intent(out)          :: json     !! Returned JSON.
        integer(i8),               intent(in), optional :: limit    !! Max. number of nodes.
        logical,                   intent(in), optional :: validate !! Validate column types.

        type(db_query_type) :: dbq

        if (.not. dm_db_is_prepared(dbs)) then
            call dm_db_query_set_order(dbq, by='nodes.row_id', desc=.false.)
            call dm_db_query_set_limit(dbq, limit)

            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_NODES))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(dbq)
        end if

        rc = dm_db_step(dbs)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(dbs, json, validate)
    end function db_json_select_nodes_iter

    integer function db_json_select_observs_array(db, strings, node_id, sensor_id, target_id, from, to, &
                                                  desc, limit, nobservs) result(rc)
        !! Returns observations in JSON format in allocatable string type array
        !! `strings`.
        !!
        !! If no observations have been found, the array will be empty, and the
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

        type(db_type),                  intent(inout)         :: db         !! Database.
        type(string_type), allocatable, intent(out)           :: strings(:) !! Returned JSON array.
        character(*),                   intent(in),  optional :: node_id    !! Node id.
        character(*),                   intent(in),  optional :: sensor_id  !! Sensor id.
        character(*),                   intent(in),  optional :: target_id  !! Target id.
        character(*),                   intent(in),  optional :: from       !! Beginning of time span.
        character(*),                   intent(in),  optional :: to         !! End of time span.
        logical,                        intent(in),  optional :: desc       !! Descending order.
        integer(i8),                    intent(in),  optional :: limit      !! Max. number of observations.
        integer(i8),                    intent(out), optional :: nobservs   !! Number of observations.

        integer             :: stat
        integer(i8)         :: i, n
        type(db_query_type) :: dbq
        type(db_stmt_type)  :: dbs

        if (present(nobservs)) nobservs  = 0_i8

        if (present(node_id))   call dm_db_query_where(dbq, 'nodes.id = ?',           node_id)
        if (present(sensor_id)) call dm_db_query_where(dbq, 'sensors.id = ?',         sensor_id)
        if (present(target_id)) call dm_db_query_where(dbq, 'targets.id = ?',         target_id)
        if (present(from))      call dm_db_query_where(dbq, 'observs.timestamp >= ?', from)
        if (present(to))        call dm_db_query_where(dbq, 'observs.timestamp < ?',  to)

        call dm_db_query_set_order(dbq, by='observs.timestamp', desc=desc)
        call dm_db_query_set_limit(dbq, limit)

        sql_block: block
            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_NOBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(dbs)
            if (dm_is_error(rc)) exit sql_block

            call dm_db_column(dbs, 0, n)

            call dm_db_finalize(dbs, error=rc)
            if (dm_is_error(rc)) return

            if (present(nobservs)) nobservs = n
            if (present(limit))    n        = min(n, limit)

            rc = E_ALLOC
            allocate (strings(n), stat=stat)
            if (stat /= 0) exit sql_block

            rc = E_DB_NO_ROWS
            if (n == 0) exit sql_block

            call dm_db_query_set_order(dbq, by='timestamp', desc=desc)
            call dm_db_query_set_limit(dbq, limit)

            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_OBSERVS))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) exit sql_block

            do i = 1, n
                rc = dm_db_step(dbs)
                if (dm_is_error(rc)) exit sql_block

                rc = dm_db_row_next(dbs, strings(i), (i == 1))
                if (dm_is_error(rc)) exit sql_block
            end do

            rc = E_NONE
        end block sql_block

        call dm_db_query_destroy(dbq)
        call dm_db_finalize(dbs)
        if (.not. allocated(strings)) allocate (strings(0))
    end function db_json_select_observs_array

    integer function db_json_select_observs_iter(db, dbs, json, node_id, sensor_id, target_id, from, to, &
                                                 desc, limit, validate) result(rc)
        !! Iterator function that returns observations in JSON format in
        !! allocatable character `json`. The statement `dbs` must be finalised
        !! once finished.
        !!
        !! If no observations have been found, the string will be empty, and the
        !! function returns `E_DB_NO_ROWS`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_BIND` if value binding failed.
        !! * `E_DB_DONE` if statement finished.
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_TYPE` if returned columns are unexpected.
        !!
        type(db_type),             intent(inout)        :: db        !! Database.
        type(db_stmt_type),        intent(inout)        :: dbs       !! Database statement.
        character(:), allocatable, intent(out)          :: json      !! Returned JSON.
        character(*),              intent(in), optional :: node_id   !! Node id.
        character(*),              intent(in), optional :: sensor_id !! Sensor id.
        character(*),              intent(in), optional :: target_id !! Target id.
        character(*),              intent(in), optional :: from      !! Beginning of time span.
        character(*),              intent(in), optional :: to        !! End of time span.
        logical,                   intent(in), optional :: desc      !! Descending order.
        integer(i8),               intent(in), optional :: limit     !! Max. number of observations.
        logical,                   intent(in), optional :: validate  !! Validate column types.

        type(db_query_type) :: dbq

        if (.not. dm_db_is_prepared(dbs)) then
            if (present(node_id))   call dm_db_query_where(dbq, 'nodes.id = ?',           node_id)
            if (present(sensor_id)) call dm_db_query_where(dbq, 'sensors.id = ?',         sensor_id)
            if (present(target_id)) call dm_db_query_where(dbq, 'targets.id = ?',         target_id)
            if (present(from))      call dm_db_query_where(dbq, 'observs.timestamp >= ?', from)
            if (present(to))        call dm_db_query_where(dbq, 'observs.timestamp < ?',  to)

            call dm_db_query_set_order(dbq, by='observs.timestamp', desc=desc)
            call dm_db_query_set_limit(dbq, limit)

            rc = dm_db_prepare(db, dbs, dm_db_query_build(dbq, DB_SQL_SELECT_JSON_OBSERVS))
            if (dm_is_error(rc)) return

            rc = dm_db_bind(dbs, dbq)
            if (dm_is_error(rc)) return

            call dm_db_query_destroy(dbq)
        end if

        rc = dm_db_step(dbs)
        if (rc /= E_DB_ROW) return

        rc = dm_db_row_next(dbs, json, validate)
    end function db_json_select_observs_iter
end module dm_db_json
