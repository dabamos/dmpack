! Author:  Philipp Engel
! Licence: ISC
module dm_db_query
    !! Basic SQL query builder.
    !!
    !! The following example uses the query builder to extend an SQL query
    !! with WHERE, ORDER BY, and LIMIT parameters. An observation database
    !! `observ.sqlite` must be provided, from which an observation id
    !! `observ_id` is read.
    !!
    !! ```fortran
    !! character(len=:), allocatable :: node_id, observ_id, sensor_id, target_id
    !! character(len=:), allocatable :: sql
    !!
    !! integer             :: rc
    !! type(db_type)       :: db
    !! type(db_query_type) :: dbq
    !! type(db_stmt_type)  :: dbs
    !!
    !! ! Query parameters.
    !! node_id   = 'dummy-node'
    !! sensor_id = 'dummy-sensor'
    !! target_id = 'dummy-target'
    !!
    !! ! Open an existing observation database first.
    !! rc = dm_db_open(db, 'observ.sqlite', read_only=.true.)
    !! call dm_error_out(rc, fatal=.true.)
    !!
    !! ! Set SQL base query string.
    !! call dm_db_query_set_sql(dbq,                                      &
    !!     'SELECT observs.id FROM observs '                           // &
    !!     'INNER JOIN nodes ON nodes.row_id = observs.node_id '       // &
    !!     'INNER JOIN sensors ON sensors.row_id = observs.sensor_id ' // &
    !!     'INNER JOIN targets ON targets.row_id = observs.target_id')
    !!
    !! ! Set WHERE clause of the query.
    !! call dm_db_query_where(dbq, 'nodes.id = ?',   node_id)
    !! call dm_db_query_where(dbq, 'sensors.id = ?', sensor_id)
    !! call dm_db_query_where(dbq, 'targets.id = ?', target_id)
    !!
    !! ! Set ORDER BY and LIMIT of the query.
    !! call dm_db_query_set_order(dbq, by='observs.timestamp', desc=.true.)
    !! call dm_db_query_set_limit(dbq, 1_i8)
    !!
    !! ! Create full query string from base query.
    !! sql = dm_db_query_build(dbq)
    !!
    !! sql_block: block
    !!     ! Prepare the database statement.
    !!     rc = dm_db_prepare(db, dbs, sql)
    !!     if (dm_is_error(rc)) exit sql_block
    !!
    !!     ! Bind the query parameters to the statement.
    !!     rc = dm_db_bind(dbs, dbq)
    !!     if (dm_is_error(rc)) exit sql_block
    !!
    !!     ! Run the statement.
    !!     rc = dm_db_step(dbs)
    !!     if (rc /= E_DB_ROW) exit sql_block
    !!
    !!     ! Get next row as allocatable character string.
    !!     rc = dm_db_row_next(dbs, observ_id)
    !!     if (dm_is_error(rc)) exit sql_block
    !! end block sql_block
    !!
    !! call dm_error_out(rc, verbose=.true.)
    !! call dm_db_query_destroy(dbq)
    !! call dm_db_finalize(dbs)
    !! call dm_db_close(db)
    !!
    !! if (allocated(observ_id)) print '("observ_id: ", a)', observ_id
    !! ```
    !!
    !! Make sure to not add more than `DB_QUERY_NPARAMS` parameters to a query
    !! or increase the constant first (32 is assumed to be sufficient for WHERE
    !! and SET parameters).
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    ! Query value types.
    integer, parameter, public :: DB_QUERY_TYPE_NONE   = 0 !! No type (invalid).
    integer, parameter, public :: DB_QUERY_TYPE_DOUBLE = 1 !! SQLite double precision.
    integer, parameter, public :: DB_QUERY_TYPE_INT    = 2 !! SQLite 32-bit integer.
    integer, parameter, public :: DB_QUERY_TYPE_INT64  = 3 !! SQLite 64-bit integer.
    integer, parameter, public :: DB_QUERY_TYPE_TEXT   = 4 !! SQLite text.

    integer, parameter :: DB_QUERY_NPARAMS = 32 !! Max. number of WHERE and SET parameters in a query.

    type, public :: db_query_param_type
        !! Single WHERE or SET parameter of database query.
        integer                       :: type         = DB_QUERY_TYPE_NONE !! Value type.
        real(kind=r8)                 :: value_double = 0.0_r8             !! Double value.
        integer                       :: value_int    = 0                  !! Integer value.
        integer(kind=i8)              :: value_int64  = 0.0_i8             !! 64-bit integer value.
        character(len=:), allocatable :: value_text                        !! Text value.
        character(len=:), allocatable :: sql                               !! WHERE clause or column name.
    end type db_query_param_type

    type, public :: db_query_type
        !! Database query with SET values (for SQL UPDATE), WHERE parameters,
        !! LIMIT, and ORDER BY. Do not modify this derived type directly!
        character(len=:), allocatable :: sql                       !! SQL base query.
        character(len=:), allocatable :: order_by                  !! ORDER BY clause.
        logical                       :: order_desc = .false.      !! ASC or DESC order.
        integer(kind=i8)              :: limit      = 0_i8         !! Row limit.
        integer                       :: nparams    = 0            !! Current WHERE parameter array size.
        integer                       :: nupdates   = 0            !! Current SET parameter array size.
        type(db_query_param_type)     :: params(DB_QUERY_NPARAMS)  !! WHERE parameter array.
        type(db_query_param_type)     :: updates(DB_QUERY_NPARAMS) !! SET parameter array.
    end type db_query_type

    interface dm_db_query_update
        !! Generic subroutine to add SET values to UPDATE query. The procedures
        !! do not validate that values have been added only once. The function
        !! is prone to SQL injections. Only pass parametrised strings!
        module procedure :: db_query_update_double
        module procedure :: db_query_update_int
        module procedure :: db_query_update_int64
        module procedure :: db_query_update_text
    end interface dm_db_query_update

    interface dm_db_query_where
        !! Generic subroutine to add WHERE values to query. The procedures
        !! do not validate that values have been added only once. The function
        !! is prone to SQL injections. Only pass parametrised strings!
        module procedure :: db_query_where_double
        module procedure :: db_query_where_int
        module procedure :: db_query_where_int64
        module procedure :: db_query_where_text
    end interface dm_db_query_where

    public :: dm_db_query_build
    public :: dm_db_query_destroy
    public :: dm_db_query_set_limit
    public :: dm_db_query_set_order
    public :: dm_db_query_set_sql
    public :: dm_db_query_update
    public :: dm_db_query_where

    private :: db_query_param_destroy
    private :: db_query_update
    private :: db_query_update_double
    private :: db_query_update_int
    private :: db_query_update_int64
    private :: db_query_update_text
    private :: db_query_where
    private :: db_query_where_double
    private :: db_query_where_int
    private :: db_query_where_int64
    private :: db_query_where_text
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    function dm_db_query_build(db_query, base) result(sql)
        !! Returns SQL string from query. If no base SQL query `base` is
        !! passed, uses query attribute `sql` instead. If attribute `sql` is
        !! not allocated, the SQL base query will be empty (and only the WHERE,
        !! ORDER BY, and LIMIT parameters are returned). A passed base query
        !! will overwrite the attribute of argument `db_query`.
        !!
        !! For UPDATE queries, the columns must be specified with
        !! `dm_db_query_update()` and likely `dm_db_query_where()` first.
        !!
        !! The function is prone to SQL injections. Only pass parametrised
        !! strings!
        use :: dm_util, only: dm_btoa

        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        character(len=*),    intent(in), optional :: base     !! Base query string.
        character(len=:), allocatable             :: sql      !! SQL string.

        integer :: i

        ! SQL query.
        if (present(base)) then
            sql = trim(base)
        else if (allocated(db_query%sql)) then
            sql = db_query%sql
        else
            sql = ''
        end if

        ! UPDATE parameters.
        do i = 1, db_query%nupdates
            if (i == 1) then
                sql = sql // ' SET ' // db_query%updates(i)%sql // ' = ?'
            else
                sql = sql // ', '    // db_query%updates(i)%sql // ' = ?'
            end if
        end do

        ! WHERE parameters.
        do i = 1, db_query%nparams
            if (i == 1) then
                sql = sql // ' WHERE ' // db_query%params(i)%sql
            else
                sql = sql // ' AND '   // db_query%params(i)%sql
            end if
        end do

        ! ORDER BY.
        if (allocated(db_query%order_by)) then
            sql = sql // ' ORDER BY ' // db_query%order_by // dm_btoa(db_query%order_desc, ' DESC', ' ASC')
        end if

        ! LIMIT.
        if (db_query%limit > 0) sql = sql // ' LIMIT ?'
    end function dm_db_query_build

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    pure elemental subroutine dm_db_query_destroy(db_query)
        !! Resets query.
        type(db_query_type), intent(inout) :: db_query !! Database query type.

        if (allocated(db_query%sql))      deallocate (db_query%sql)
        if (allocated(db_query%order_by)) deallocate (db_query%order_by)

        call db_query_param_destroy(db_query%params)
        call db_query_param_destroy(db_query%updates)

        db_query%order_desc = .false.
        db_query%limit      = 0_i8
        db_query%nparams    = 0
    end subroutine dm_db_query_destroy

    pure elemental subroutine dm_db_query_set_limit(db_query, limit)
        !! Sets `LIMIT` clause of query. If argument `limit` is not passed, not
        !! limit is set. Passing 0 disables the LIMIT parameter.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        integer(kind=i8),    intent(in), optional :: limit    !! Limit value.

        if (.not. present(limit)) return
        db_query%limit = max(0_i8, limit)
    end subroutine dm_db_query_set_limit

    pure elemental subroutine dm_db_query_set_order(db_query, by, desc)
        !! Sets `ORDER BY` clause of query. Argument `by` must be a valid field
        !! name. The function is prone to SQL injections. Only pass
        !! parametrised strings! If `desc` is not passed, ascending order is
        !! used.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        character(len=*),    intent(in)           :: by       !! Field name.
        logical,             intent(in), optional :: desc     !! Descending order.

        db_query%order_by = trim(by)
        if (present(desc)) db_query%order_desc = desc
    end subroutine dm_db_query_set_order

    pure elemental subroutine dm_db_query_set_sql(db_query, base)
        !! Sets SQL base query. The function is prone to SQL injections. Only
        !! pass parametrised strings!
        type(db_query_type), intent(inout) :: db_query !! Database query type.
        character(len=*),    intent(in)    :: base     !! SQL base query.

        db_query%sql = trim(base)
    end subroutine dm_db_query_set_sql

    ! **************************************************************************
    ! PRIVATE SUBROUTINES.
    ! **************************************************************************
    pure elemental subroutine db_query_param_destroy(param)
        type(db_query_param_type), intent(inout) :: param !! Database query param type.

        param%type         = DB_QUERY_TYPE_NONE
        param%value_double = 0.0_r8
        param%value_int    = 0
        param%value_int64  = 0.0_i8

        if (allocated(param%value_text)) deallocate (param%value_text)
        if (allocated(param%sql))        deallocate (param%sql)
    end subroutine db_query_param_destroy

    subroutine db_query_update(db_query, type, value_double, value_int, value_int64, value_text, sql, error)
        !! Adds SET parameter to query. Returns `E_LIMIT` in `error` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query     !! Database query type.
        integer,             intent(in)            :: type         !! Column type.
        real(kind=r8),       intent(in),  optional :: value_double !! New column value.
        integer(kind=i4),    intent(in),  optional :: value_int    !! New column value.
        integer(kind=i8),    intent(in),  optional :: value_int64  !! New column value.
        character(len=*),    intent(in),  optional :: value_text   !! New column value.
        character(len=*),    intent(in),  optional :: sql          !! SQL part.
        integer,             intent(out), optional :: error        !! Error code.

        if (present(error)) error = E_LIMIT
        if (db_query%nupdates >= size(db_query%updates)) return
        if (present(error)) error = E_NONE

        db_query%nupdates = db_query%nupdates + 1

        associate (update => db_query%updates(db_query%nupdates))
            update%type = type
            if (present(value_double)) update%value_double = value_double
            if (present(value_int))    update%value_int    = value_int
            if (present(value_int64))  update%value_int64  = value_int64
            if (present(value_text))   update%value_text   = trim(value_text)
            if (present(sql))          update%sql          = trim(sql)
        end associate
    end subroutine db_query_update

    subroutine db_query_update_double(db_query, column, value, error)
        !! Adds double precision SET parameter to query. Returns `E_LIMIT` in
        !! `error` if parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: column   !! Column name.
        real(kind=r8),       intent(in)            :: value    !! New column value.
        integer,             intent(out), optional :: error    !! Error code.

        call db_query_update(db_query, type=DB_QUERY_TYPE_DOUBLE, value_double=value, sql=column, error=error)
    end subroutine db_query_update_double

    subroutine db_query_update_int(db_query, column, value, error)
        !! Adds 32-bit integer SET parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: column   !! Column name.
        integer(kind=i4),    intent(in)            :: value    !! New column value.
        integer,             intent(out), optional :: error    !! Error code.

        call db_query_update(db_query, type=DB_QUERY_TYPE_INT, value_int=value, sql=column, error=error)
    end subroutine db_query_update_int

    subroutine db_query_update_int64(db_query, column, value, error)
        !! Adds 64-bit integer SET parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: column   !! Column name.
        integer(kind=i8),    intent(in)            :: value    !! New column value.
        integer,             intent(out), optional :: error    !! Error code.

        call db_query_update(db_query, type=DB_QUERY_TYPE_INT64, value_int64=value, sql=column, error=error)
    end subroutine db_query_update_int64

    subroutine db_query_update_text(db_query, column, value, error)
        !! Adds text parameter to SET query. Returns `E_LIMIT` if parameter
        !! limit has been reached.
        use :: dm_util, only: dm_present

        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: column   !! Column name.
        character(len=*),    intent(in)            :: value    !! New column value.
        integer,             intent(out), optional :: error    !! Error code.

        if (present(error)) error = E_NONE
        call db_query_update(db_query, type=DB_QUERY_TYPE_TEXT, value_text=value, sql=column, error=error)
    end subroutine db_query_update_text

    subroutine db_query_where(db_query, type, value_double, value_int, value_int64, value_text, sql, error)
        !! Adds WHERE parameter to query. Returns `E_LIMIT` in `error` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query     !! Database query type.
        integer,             intent(in)            :: type         !! Query parameter type.
        real(kind=r8),       intent(in),  optional :: value_double !! Query parameter value.
        integer(kind=i4),    intent(in),  optional :: value_int    !! Query parameter value.
        integer(kind=i8),    intent(in),  optional :: value_int64  !! Query parameter value.
        character(len=*),    intent(in),  optional :: value_text   !! Query parameter value.
        character(len=*),    intent(in),  optional :: sql          !! SQL part.
        integer,             intent(out), optional :: error        !! Error code.

        if (present(error)) error = E_LIMIT
        if (db_query%nparams >= size(db_query%params)) return
        if (present(error)) error = E_NONE

        db_query%nparams = db_query%nparams + 1

        associate (param => db_query%params(db_query%nparams))
            param%type = type
            if (present(value_double)) param%value_double = value_double
            if (present(value_int))    param%value_int    = value_int
            if (present(value_int64))  param%value_int64  = value_int64
            if (present(value_text))   param%value_text   = trim(value_text)
            if (present(sql))          param%sql          = trim(sql)
        end associate
    end subroutine db_query_where

    subroutine db_query_where_double(db_query, param, value, error)
        !! Adds double precision WHERE parameter to query. Returns `E_LIMIT` in
        !! `error` if parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        real(kind=r8),       intent(in)            :: value    !! Query parameter value.
        integer,             intent(out), optional :: error    !! Error code.

        call db_query_where(db_query, type=DB_QUERY_TYPE_DOUBLE, value_double=value, sql=param, error=error)
    end subroutine db_query_where_double

    subroutine db_query_where_int(db_query, param, value, error)
        !! Adds 32-bit integer WHERE parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        integer(kind=i4),    intent(in)            :: value    !! Query parameter value.
        integer,             intent(out), optional :: error    !! Error code.

        call db_query_where(db_query, type=DB_QUERY_TYPE_INT, value_int=value, sql=param, error=error)
    end subroutine db_query_where_int

    subroutine db_query_where_int64(db_query, param, value, error)
        !! Adds 64-bit integer WHERE parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        integer(kind=i8),    intent(in)            :: value    !! Query parameter value.
        integer,             intent(out), optional :: error    !! Error code.

        call db_query_where(db_query, type=DB_QUERY_TYPE_INT64, value_int64=value, sql=param, error=error)
    end subroutine db_query_where_int64

    subroutine db_query_where_text(db_query, param, value, empty, error)
        !! Adds text parameter to WHERE query. Returns `E_LIMIT` if parameter
        !! limit has been reached. Empty strings and strings containing only
        !! white-space characters are ignored, unless argument `empty` is set
        !! to `.true.`.
        use :: dm_util, only: dm_present

        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        character(len=*),    intent(in)            :: value    !! Query parameter value.
        logical,             intent(in),  optional :: empty    !! Add empty string.
        integer,             intent(out), optional :: error    !! Error code.

        if (present(error)) error = E_NONE
        if (.not. dm_present(empty, .false.) .and. len_trim(value) == 0) return
        call db_query_where(db_query, type=DB_QUERY_TYPE_TEXT, value_text=value, sql=param, error=error)
    end subroutine db_query_where_text
end module dm_db_query
