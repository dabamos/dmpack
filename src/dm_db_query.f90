! Author:  Philipp Engel
! Licence: ISC
module dm_db_query
    !! SQL query builder.
    use :: dm_db_stmt
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: DB_QUERY_TYPE_NONE   = 0 !! No type (invalid).
    integer, parameter, public :: DB_QUERY_TYPE_DOUBLE = 1 !! SQLite double precision.
    integer, parameter, public :: DB_QUERY_TYPE_INT    = 2 !! SQLite 32-bit integer.
    integer, parameter, public :: DB_QUERY_TYPE_INT64  = 3 !! SQLite 64-bit integer.
    integer, parameter, public :: DB_QUERY_TYPE_TEXT   = 4 !! SQLite text.

    integer, parameter :: DB_QUERY_NPARAMS = 16 !! Max. number of parameters in query type.

    type, public :: db_query_param_type
        !! Opaque database query parameter.
        private
        integer                       :: type         = DB_QUERY_TYPE_NONE !! Value type.
        real(kind=r8)                 :: value_double = 0.0_r8             !! Double value.
        integer                       :: value_int    = 0                  !! Integer value.
        integer(kind=i8)              :: value_int64  = 0.0_i8             !! 64-bit integer value.
        character(len=:), allocatable :: value_text                        !! Text value.
        character(len=:), allocatable :: sql                               !! Query string.
    end type db_query_param_type

    type, public :: db_query_type
        !! Opaque database query.
        private
        character(len=:), allocatable :: order_by                 !! ORDER BY clause.
        logical                       :: order_desc = .false.     !! Descending order.
        integer(kind=i8)              :: limit      = 0_i8        !! Limit value.
        integer                       :: index      = 0           !! Parameter array index.
        type(db_query_param_type)     :: params(DB_QUERY_NPARAMS) !! Parameters.
    end type db_query_type

    public :: dm_db_query_add_double
    public :: dm_db_query_add_int
    public :: dm_db_query_add_int64
    public :: dm_db_query_add_text
    public :: dm_db_query_bind
    public :: dm_db_query_build
    public :: dm_db_query_destroy
    public :: dm_db_query_limit
    public :: dm_db_query_order
contains
    integer function dm_db_query_add_double(db_query, param, value) result(rc)
        !! Adds double precision parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        character(len=*),    intent(in)           :: param !! Query parameter.
        real(kind=r8),       intent(in), optional :: value !! Query parameter value.

        rc = E_LIMIT
        if (db_query%index >= size(db_query%params)) return

        rc = E_NONE
        if (.not. present(value)) return

        db_query%index = db_query%index + 1

        db_query%params(db_query%index)%type         = DB_QUERY_TYPE_DOUBLE
        db_query%params(db_query%index)%sql          = trim(param)
        db_query%params(db_query%index)%value_double = value
    end function dm_db_query_add_double

    integer function dm_db_query_add_int(db_query, param, value) result(rc)
        !! Adds 32-bit integer parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        character(len=*),    intent(in)           :: param !! Query parameter.
        integer(kind=i4),    intent(in), optional :: value !! Query parameter value.

        rc = E_LIMIT
        if (db_query%index >= size(db_query%params)) return

        rc = E_NONE
        if (.not. present(value)) return

        db_query%index = db_query%index + 1

        db_query%params(db_query%index)%type      = DB_QUERY_TYPE_INT
        db_query%params(db_query%index)%sql       = trim(param)
        db_query%params(db_query%index)%value_int = value
    end function dm_db_query_add_int

    integer function dm_db_query_add_int64(db_query, param, value) result(rc)
        !! Adds 64-bit integer parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        character(len=*),    intent(in)           :: param !! Query parameter.
        integer(kind=i8),    intent(in), optional :: value !! Query parameter value.

        rc = E_LIMIT
        if (db_query%index >= size(db_query%params)) return

        rc = E_NONE
        if (.not. present(value)) return

        db_query%index = db_query%index + 1

        db_query%params(db_query%index)%type        = DB_QUERY_TYPE_INT64
        db_query%params(db_query%index)%sql         = trim(param)
        db_query%params(db_query%index)%value_int64 = value
    end function dm_db_query_add_int64

    integer function dm_db_query_add_text(db_query, param, value) result(rc)
        !! Adds text parameter to query. Returns `E_LIMIT` if parameter limit
        !! has been reached.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        character(len=*),    intent(in)           :: param !! Query parameter.
        character(len=*),    intent(in), optional :: value !! Query parameter value.

        rc = E_LIMIT
        if (db_query%index >= size(db_query%params)) return

        rc = E_NONE
        if (.not. present(value)) return

        db_query%index = db_query%index + 1

        db_query%params(db_query%index)%type       = DB_QUERY_TYPE_TEXT
        db_query%params(db_query%index)%sql        = trim(param)
        db_query%params(db_query%index)%value_text = trim(value)
    end function dm_db_query_add_text

    integer function dm_db_query_bind(db_query, db_stmt) result(rc)
        !! Binds query parameters to SQLite statement. Returns `E_DB_BIND` on
        !! binding error.
        use :: sqlite3

        type(db_query_type), intent(inout) :: db_query !! Database query type.
        type(db_stmt_type),  intent(inout) :: db_stmt  !! Database statement type.

        integer :: i, stat

        stat = SQLITE_OK

        do i = 1, db_query%index
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
    end function dm_db_query_bind

    function dm_db_query_build(db_query, base) result(sql)
        !! Returns SQL string from query.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        character(len=*),    intent(in), optional :: base     !! Base query string.
        character(len=:), allocatable             :: sql      !! SQL string.

        integer :: i

        if (present(base)) then
            sql = trim(base)
        else
            sql = ''
        end if

        do i = 1, db_query%index
            if (i == 1) then
                sql = sql // ' WHERE ' // db_query%params(i)%sql
            else
                sql = sql // ' AND ' // db_query%params(i)%sql
            end if
        end do

        if (allocated(db_query%order_by)) then
            if (db_query%order_desc) then
                sql = sql // ' ORDER BY ' // db_query%order_by // ' DESC'
            else
                sql = sql // ' ORDER BY ' // db_query%order_by // ' ASC'
            end if
        end if

        if (db_query%limit > 0) sql = sql // ' LIMIT ?'
    end function dm_db_query_build

    pure elemental subroutine dm_db_query_destroy(db_query)
        !! Resets query.
        type(db_query_type), intent(inout) :: db_query !! Database query type.

        integer :: i

        if (allocated(db_query%order_by)) deallocate (db_query%order_by)

        do i = 1, db_query%index
            if (allocated(db_query%params(i)%value_text)) deallocate (db_query%params(i)%value_text)
            if (allocated(db_query%params(i)%sql))        deallocate (db_query%params(i)%sql)
        end do

        db_query = db_query_type()
    end subroutine dm_db_query_destroy

    pure elemental subroutine dm_db_query_limit(db_query, limit)
        !! Sets `LIMIT` clause of query.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        integer(kind=i8),    intent(in), optional :: limit    !! Limit value.

        if (.not. present(limit)) return
        db_query%limit = max(0_i8, limit)
    end subroutine dm_db_query_limit

    pure elemental subroutine dm_db_query_order(db_query, by, desc)
        !! Sets `ORDER BY` clause of query.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        character(len=*),    intent(in)           :: by    !! Field name.
        logical,             intent(in), optional :: desc  !! Descending order.

        db_query%order_by = trim(by)
        if (present(desc)) db_query%order_desc = desc
    end subroutine dm_db_query_order
end module dm_db_query
