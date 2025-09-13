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
    !! character(:), allocatable :: node_id, observ_id, sensor_id, target_id
    !! character(:), allocatable :: sql
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
        integer                   :: type         = DB_QUERY_TYPE_NONE !! Value type.
        real(r8)                  :: value_double = 0.0_r8             !! Double value.
        integer                   :: value_int    = 0                  !! Integer value.
        integer(i8)               :: value_int64  = 0.0_i8             !! 64-bit integer value.
        character(:), allocatable :: value_text                        !! Text value.
        character(:), allocatable :: sql                               !! WHERE clause or column name.
    end type db_query_param_type

    type, public :: db_query_type
        !! Database query with SET values (for SQL UPDATE), WHERE parameters,
        !! LIMIT, and ORDER BY. Do not modify this derived type directly!
        character(:), allocatable :: sql                       !! SQL base query.
        character(:), allocatable :: order_by                  !! ORDER BY clause.
        logical                   :: order_desc = .false.      !! ASC or DESC order.
        integer(i8)               :: limit      = 0_i8         !! Row limit.
        integer                   :: nparams    = 0            !! Current WHERE parameter array size.
        integer                   :: nupdates   = 0            !! Current SET parameter array size.
        type(db_query_param_type) :: params(DB_QUERY_NPARAMS)  !! WHERE parameter array.
        type(db_query_param_type) :: updates(DB_QUERY_NPARAMS) !! SET parameter array.
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
    function dm_db_query_build(dbq, base) result(sql)
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

        type(db_query_type), intent(inout)        :: dbq  !! Database query type.
        character(*),        intent(in), optional :: base !! Base query string.
        character(:), allocatable                 :: sql  !! SQL string.

        integer :: i

        ! SQL query.
        if (present(base)) then
            sql = trim(base)
        else if (allocated(dbq%sql)) then
            sql = dbq%sql
        else
            sql = ''
        end if

        ! UPDATE parameters.
        do i = 1, dbq%nupdates
            if (i == 1) then
                sql = sql // ' SET ' // dbq%updates(i)%sql // ' = ?'
            else
                sql = sql // ', '    // dbq%updates(i)%sql // ' = ?'
            end if
        end do

        ! WHERE parameters.
        do i = 1, dbq%nparams
            if (i == 1) then
                sql = sql // ' WHERE ' // dbq%params(i)%sql
            else
                sql = sql // ' AND '   // dbq%params(i)%sql
            end if
        end do

        ! ORDER BY.
        if (allocated(dbq%order_by)) then
            sql = sql // ' ORDER BY ' // dbq%order_by // dm_btoa(dbq%order_desc, ' DESC', ' ASC')
        end if

        ! LIMIT.
        if (dbq%limit > 0) sql = sql // ' LIMIT ?'
    end function dm_db_query_build

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    pure elemental subroutine dm_db_query_destroy(dbq)
        !! Resets query.
        type(db_query_type), intent(inout) :: dbq !! Database query type.

        if (allocated(dbq%sql))      deallocate (dbq%sql)
        if (allocated(dbq%order_by)) deallocate (dbq%order_by)

        call db_query_param_destroy(dbq%params)
        call db_query_param_destroy(dbq%updates)

        dbq%order_desc = .false.
        dbq%limit      = 0_i8
        dbq%nparams    = 0
    end subroutine dm_db_query_destroy

    pure elemental subroutine dm_db_query_set_limit(dbq, limit)
        !! Sets `LIMIT` clause of query. If argument `limit` is not passed, not
        !! limit is set. Passing 0 disables the LIMIT parameter.
        type(db_query_type), intent(inout)        :: dbq   !! Database query type.
        integer(i8),         intent(in), optional :: limit !! Limit value.

        if (.not. present(limit)) return
        dbq%limit = max(0_i8, limit)
    end subroutine dm_db_query_set_limit

    pure elemental subroutine dm_db_query_set_order(dbq, by, desc)
        !! Sets `ORDER BY` clause of query. Argument `by` must be a valid field
        !! name. The function is prone to SQL injections. Only pass
        !! parametrised strings! If `desc` is not passed, ascending order is
        !! used.
        type(db_query_type), intent(inout)        :: dbq  !! Database query type.
        character(*),        intent(in)           :: by   !! Field name.
        logical,             intent(in), optional :: desc !! Descending order.

        dbq%order_by = trim(by)
        if (present(desc)) dbq%order_desc = desc
    end subroutine dm_db_query_set_order

    pure elemental subroutine dm_db_query_set_sql(dbq, base)
        !! Sets SQL base query. The function is prone to SQL injections. Only
        !! pass parametrised strings!
        type(db_query_type), intent(inout) :: dbq  !! Database query type.
        character(*),        intent(in)    :: base !! SQL base query.

        dbq%sql = trim(base)
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

    subroutine db_query_update(dbq, type, value_double, value_int, value_int64, value_text, sql, error)
        !! Adds SET parameter to query. Returns `E_LIMIT` in `error` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: dbq          !! Database query type.
        integer,             intent(in)            :: type         !! Column type.
        real(r8),            intent(in),  optional :: value_double !! New column value.
        integer(i4),         intent(in),  optional :: value_int    !! New column value.
        integer(i8),         intent(in),  optional :: value_int64  !! New column value.
        character(*),        intent(in),  optional :: value_text   !! New column value.
        character(*),        intent(in),  optional :: sql          !! SQL part.
        integer,             intent(out), optional :: error        !! Error code.

        if (present(error)) error = E_LIMIT
        if (dbq%nupdates >= size(dbq%updates)) return
        if (present(error)) error = E_NONE

        dbq%nupdates = dbq%nupdates + 1

        associate (update => dbq%updates(dbq%nupdates))
            update%type = type
            if (present(value_double)) update%value_double = value_double
            if (present(value_int))    update%value_int    = value_int
            if (present(value_int64))  update%value_int64  = value_int64
            if (present(value_text))   update%value_text   = trim(value_text)
            if (present(sql))          update%sql          = trim(sql)
        end associate
    end subroutine db_query_update

    subroutine db_query_update_double(dbq, column, value, error)
        !! Adds double precision SET parameter to query. Returns `E_LIMIT` in
        !! `error` if parameter limit has been reached.
        type(db_query_type), intent(inout)         :: dbq    !! Database query type.
        character(*),        intent(in)            :: column !! Column name.
        real(r8),            intent(in)            :: value  !! New column value.
        integer,             intent(out), optional :: error  !! Error code.

        call db_query_update(dbq, type=DB_QUERY_TYPE_DOUBLE, value_double=value, sql=column, error=error)
    end subroutine db_query_update_double

    subroutine db_query_update_int(dbq, column, value, error)
        !! Adds 32-bit integer SET parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: dbq    !! Database query type.
        character(*),        intent(in)            :: column !! Column name.
        integer(i4),         intent(in)            :: value  !! New column value.
        integer,             intent(out), optional :: error  !! Error code.

        call db_query_update(dbq, type=DB_QUERY_TYPE_INT, value_int=value, sql=column, error=error)
    end subroutine db_query_update_int

    subroutine db_query_update_int64(dbq, column, value, error)
        !! Adds 64-bit integer SET parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: dbq    !! Database query type.
        character(*),        intent(in)            :: column !! Column name.
        integer(i8),         intent(in)            :: value  !! New column value.
        integer,             intent(out), optional :: error  !! Error code.

        call db_query_update(dbq, type=DB_QUERY_TYPE_INT64, value_int64=value, sql=column, error=error)
    end subroutine db_query_update_int64

    subroutine db_query_update_text(dbq, column, value, error)
        !! Adds text parameter to SET query. Returns `E_LIMIT` if parameter
        !! limit has been reached.
        use :: dm_util, only: dm_present

        type(db_query_type), intent(inout)         :: dbq    !! Database query type.
        character(*),        intent(in)            :: column !! Column name.
        character(*),        intent(in)            :: value  !! New column value.
        integer,             intent(out), optional :: error  !! Error code.

        if (present(error)) error = E_NONE
        call db_query_update(dbq, type=DB_QUERY_TYPE_TEXT, value_text=value, sql=column, error=error)
    end subroutine db_query_update_text

    subroutine db_query_where(dbq, type, value_double, value_int, value_int64, value_text, sql, error)
        !! Adds WHERE parameter to query. Returns `E_LIMIT` in `error` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: dbq          !! Database query type.
        integer,             intent(in)            :: type         !! Query parameter type.
        real(r8),            intent(in),  optional :: value_double !! Query parameter value.
        integer(i4),         intent(in),  optional :: value_int    !! Query parameter value.
        integer(i8),         intent(in),  optional :: value_int64  !! Query parameter value.
        character(*),        intent(in),  optional :: value_text   !! Query parameter value.
        character(*),        intent(in),  optional :: sql          !! SQL part.
        integer,             intent(out), optional :: error        !! Error code.

        if (present(error)) error = E_LIMIT
        if (dbq%nparams >= size(dbq%params)) return
        if (present(error)) error = E_NONE

        dbq%nparams = dbq%nparams + 1

        associate (param => dbq%params(dbq%nparams))
            param%type = type
            if (present(value_double)) param%value_double = value_double
            if (present(value_int))    param%value_int    = value_int
            if (present(value_int64))  param%value_int64  = value_int64
            if (present(value_text))   param%value_text   = trim(value_text)
            if (present(sql))          param%sql          = trim(sql)
        end associate
    end subroutine db_query_where

    subroutine db_query_where_double(dbq, param, value, error)
        !! Adds double precision WHERE parameter to query. Returns `E_LIMIT` in
        !! `error` if parameter limit has been reached.
        type(db_query_type), intent(inout)         :: dbq   !! Database query type.
        character(*),        intent(in)            :: param !! Query parameter.
        real(r8),            intent(in)            :: value !! Query parameter value.
        integer,             intent(out), optional :: error !! Error code.

        call db_query_where(dbq, type=DB_QUERY_TYPE_DOUBLE, value_double=value, sql=param, error=error)
    end subroutine db_query_where_double

    subroutine db_query_where_int(dbq, param, value, error)
        !! Adds 32-bit integer WHERE parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: dbq   !! Database query type.
        character(*),        intent(in)            :: param !! Query parameter.
        integer(i4),         intent(in)            :: value !! Query parameter value.
        integer,             intent(out), optional :: error !! Error code.

        call db_query_where(dbq, type=DB_QUERY_TYPE_INT, value_int=value, sql=param, error=error)
    end subroutine db_query_where_int

    subroutine db_query_where_int64(dbq, param, value, error)
        !! Adds 64-bit integer WHERE parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: dbq   !! Database query type.
        character(*),        intent(in)            :: param !! Query parameter.
        integer(i8),         intent(in)            :: value !! Query parameter value.
        integer,             intent(out), optional :: error !! Error code.

        call db_query_where(dbq, type=DB_QUERY_TYPE_INT64, value_int64=value, sql=param, error=error)
    end subroutine db_query_where_int64

    subroutine db_query_where_text(dbq, param, value, empty, error)
        !! Adds text parameter to WHERE query. Returns `E_LIMIT` if parameter
        !! limit has been reached. Empty strings and strings containing only
        !! white-space characters are ignored, unless argument `empty` is set
        !! to `.true.`.
        use :: dm_util, only: dm_present

        type(db_query_type), intent(inout)         :: dbq   !! Database query type.
        character(*),        intent(in)            :: param !! Query parameter.
        character(*),        intent(in)            :: value !! Query parameter value.
        logical,             intent(in),  optional :: empty !! Add empty string.
        integer,             intent(out), optional :: error !! Error code.

        if (present(error)) error = E_NONE
        if (.not. dm_present(empty, .false.) .and. len_trim(value) == 0) return
        call db_query_where(dbq, type=DB_QUERY_TYPE_TEXT, value_text=value, sql=param, error=error)
    end subroutine db_query_where_text
end module dm_db_query
