! Author:  Philipp Engel
! Licence: ISC
module dm_db_query
    !! Basic SQL query builder.
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
        !! Database query parameter.
        integer                       :: type         = DB_QUERY_TYPE_NONE !! Value type.
        real(kind=r8)                 :: value_double = 0.0_r8             !! Double value.
        integer                       :: value_int    = 0                  !! Integer value.
        integer(kind=i8)              :: value_int64  = 0.0_i8             !! 64-bit integer value.
        character(len=:), allocatable :: value_text                        !! Text value.
        character(len=:), allocatable :: sql                               !! Query parameter string.
    end type db_query_param_type

    type, public :: db_query_type
        !! Database query.
        character(len=:), allocatable :: order_by                 !! ORDER BY clause.
        logical                       :: order_desc = .false.     !! ASC or DESC order.
        integer(kind=i8)              :: limit      = 0_i8        !! Row limit.
        integer                       :: nparams    = 0           !! Current parameter array size.
        type(db_query_param_type)     :: params(DB_QUERY_NPARAMS) !! Parameter array.
    end type db_query_type

    public :: dm_db_query_add_double
    public :: dm_db_query_add_int
    public :: dm_db_query_add_int64
    public :: dm_db_query_add_text
    public :: dm_db_query_build
    public :: dm_db_query_destroy
    public :: dm_db_query_set_limit
    public :: dm_db_query_set_order
contains
    subroutine dm_db_query_add_double(db_query, param, value, error)
        !! Adds double precision parameter to query. Returns `E_LIMIT` in
        !! `error` if parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        real(kind=r8),       intent(in),  optional :: value    !! Query parameter value.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nparams >= size(db_query%params)) return

        if (present(error)) error = E_NONE
        if (.not. present(value)) return

        n = db_query%nparams + 1

        db_query%nparams                = n
        db_query%params(n)%type         = DB_QUERY_TYPE_DOUBLE
        db_query%params(n)%sql          = trim(param)
        db_query%params(n)%value_double = value
    end subroutine dm_db_query_add_double

    subroutine dm_db_query_add_int(db_query, param, value, error)
        !! Adds 32-bit integer parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        integer(kind=i4),    intent(in),  optional :: value    !! Query parameter value.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nparams >= size(db_query%params)) return

        if (present(error)) error = E_NONE
        if (.not. present(value)) return

        n = db_query%nparams + 1

        db_query%nparams             = n
        db_query%params(n)%type      = DB_QUERY_TYPE_INT
        db_query%params(n)%sql       = trim(param)
        db_query%params(n)%value_int = value
    end subroutine dm_db_query_add_int

    subroutine dm_db_query_add_int64(db_query, param, value, error)
        !! Adds 64-bit integer parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        integer(kind=i8),    intent(in),  optional :: value    !! Query parameter value.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nparams >= size(db_query%params)) return

        if (present(error)) error = E_NONE
        if (.not. present(value)) return

        n = db_query%nparams + 1

        db_query%nparams               = n
        db_query%params(n)%type        = DB_QUERY_TYPE_INT64
        db_query%params(n)%sql         = trim(param)
        db_query%params(n)%value_int64 = value
    end subroutine dm_db_query_add_int64

    subroutine dm_db_query_add_text(db_query, param, value, empty, error)
        !! Adds text parameter to query. Returns `E_LIMIT` if parameter limit
        !! has been reached. Empty strings and strings containing only
        !! white-space characters are ignored, unless argument `empty` is set
        !! to `.true.`.
        use :: dm_util, only: dm_present

        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        character(len=*),    intent(in),  optional :: value    !! Query parameter value.
        logical,             intent(in),  optional :: empty    !! Add empty string.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nparams >= size(db_query%params)) return

        if (present(error)) error = E_NONE
        if (.not. present(value)) return
        if (.not. dm_present(empty, .false.) .and. len_trim(value) == 0) return

        n = db_query%nparams + 1

        db_query%nparams              = n
        db_query%params(n)%type       = DB_QUERY_TYPE_TEXT
        db_query%params(n)%sql        = trim(param)
        db_query%params(n)%value_text = trim(value)
    end subroutine dm_db_query_add_text

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

        do i = 1, db_query%nparams
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

        do i = 1, db_query%nparams
            if (allocated(db_query%params(i)%value_text)) deallocate (db_query%params(i)%value_text)
            if (allocated(db_query%params(i)%sql))        deallocate (db_query%params(i)%sql)
        end do

        db_query = db_query_type()
    end subroutine dm_db_query_destroy

    pure elemental subroutine dm_db_query_set_limit(db_query, limit)
        !! Sets `LIMIT` clause of query.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        integer(kind=i8),    intent(in), optional :: limit    !! Limit value.

        if (.not. present(limit)) return
        db_query%limit = max(0_i8, limit)
    end subroutine dm_db_query_set_limit

    pure elemental subroutine dm_db_query_set_order(db_query, by, desc)
        !! Sets `ORDER BY` clause of query.
        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        character(len=*),    intent(in)           :: by       !! Field name.
        logical,             intent(in), optional :: desc     !! Descending order.

        db_query%order_by = trim(by)
        if (present(desc)) db_query%order_desc = desc
    end subroutine dm_db_query_set_order
end module dm_db_query
