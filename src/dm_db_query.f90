! Author:  Philipp Engel
! Licence: ISC
module dm_db_query
    use, intrinsic :: iso_c_binding
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: DB_QUERY_TYPE_NONE   = 0
    integer, parameter, public :: DB_QUERY_TYPE_DOUBLE = 1
    integer, parameter, public :: DB_QUERY_TYPE_INT    = 2
    integer, parameter, public :: DB_QUERY_TYPE_INT64  = 3
    integer, parameter, public :: DB_QUERY_TYPE_TEXT   = 4

    integer, parameter :: DB_QUERY_NPARAMS = 16

    type, public :: db_query_param_type
        integer                       :: type         = DB_QUERY_TYPE_NONE
        real(kind=r8)                 :: value_double = 0.0_r8
        integer                       :: value_int    = 0
        integer(kind=i8)              :: value_int64  = 0.0_i8
        character(len=:), allocatable :: value_text
        character(len=:), allocatable :: sql
    end type db_query_param_type

    type, public :: db_query_type
        character(len=:), allocatable :: base
        character(len=:), allocatable :: order_by
        logical                       :: order_desc = .false.
        integer(kind=i8)              :: limit      = 0_i8
        integer                       :: index      = 0
        type(db_query_param_type)     :: params(DB_QUERY_NPARAMS)
    end type db_query_type

    public :: dm_db_query_add_double
    public :: dm_db_query_add_int
    public :: dm_db_query_add_int64
    public :: dm_db_query_add_text
    public :: dm_db_query_bind
    public :: dm_db_query_destroy
    public :: dm_db_query_generate
    public :: dm_db_query_limit
    public :: dm_db_query_order
contains
    integer function dm_db_query_add_double(query, param, value) result(rc)
        type(db_query_type), intent(inout)        :: query !! Database query type.
        character(len=*),    intent(in)           :: param !! Query parameter.
        real(kind=r8),       intent(in), optional :: value !! Query parameter value.

        rc = E_LIMIT
        if (query%index >= size(query%params)) return

        rc = E_NONE
        if (.not. present(value)) return

        query%index = query%index + 1

        query%params(query%index)%type         = DB_QUERY_TYPE_DOUBLE
        query%params(query%index)%sql          = trim(param)
        query%params(query%index)%value_double = value
    end function dm_db_query_add_double

    integer function dm_db_query_add_int(query, param, value) result(rc)
        type(db_query_type), intent(inout)        :: query !! Database query type.
        character(len=*),    intent(in)           :: param !! Query parameter.
        integer,             intent(in), optional :: value !! Query parameter value.

        rc = E_LIMIT
        if (query%index >= size(query%params)) return

        rc = E_NONE
        if (.not. present(value)) return

        query%index = query%index + 1

        query%params(query%index)%type      = DB_QUERY_TYPE_INT
        query%params(query%index)%sql       = trim(param)
        query%params(query%index)%value_int = value
    end function dm_db_query_add_int

    integer function dm_db_query_add_int64(query, param, value) result(rc)
        type(db_query_type), intent(inout)        :: query !! Database query type.
        character(len=*),    intent(in)           :: param !! Query parameter.
        integer(kind=i8),    intent(in), optional :: value !! Query parameter value.

        rc = E_LIMIT
        if (query%index >= size(query%params)) return

        rc = E_NONE
        if (.not. present(value)) return

        query%index = query%index + 1

        query%params(query%index)%type        = DB_QUERY_TYPE_INT64
        query%params(query%index)%sql         = trim(param)
        query%params(query%index)%value_int64 = value
    end function dm_db_query_add_int64

    integer function dm_db_query_add_text(query, param, value) result(rc)
        type(db_query_type), intent(inout)        :: query !! Database query type.
        character(len=*),    intent(in)           :: param !! Query parameter.
        character(len=*),    intent(in), optional :: value !! Query parameter value.

        rc = E_LIMIT
        if (query%index >= size(query%params)) return

        rc = E_NONE
        if (.not. present(value)) return

        query%index = query%index + 1

        query%params(query%index)%type       = DB_QUERY_TYPE_TEXT
        query%params(query%index)%sql        = trim(param)
        query%params(query%index)%value_text = trim(value)
    end function dm_db_query_add_text

    integer function dm_db_query_bind(query, stmt) result(rc)
        use :: sqlite3

        type(db_query_type), intent(inout) :: query
        type(c_ptr),         intent(inout) :: stmt

        integer :: i, stat

        do i = 1, query%index
            select case (query%params(i)%type)
                case (DB_QUERY_TYPE_DOUBLE); stat = sqlite3_bind_double(stmt, i, query%params(i)%value_double)
                case (DB_QUERY_TYPE_INT);    stat = sqlite3_bind_int   (stmt, i, query%params(i)%value_int)
                case (DB_QUERY_TYPE_INT64);  stat = sqlite3_bind_int64 (stmt, i, query%params(i)%value_int64)
                case (DB_QUERY_TYPE_TEXT);   stat = sqlite3_bind_text  (stmt, i, query%params(i)%value_text)
                case default;                stat = SQLITE_ERROR
            end select
        end do

        rc = E_DB_BIND
        if (stat /= SQLITE_OK) return

        if (query%limit > 0) then
            stat = sqlite3_bind_int64(stmt, i, query%limit)
            if (stat /= SQLITE_OK) return
        end if

        rc = E_NONE
    end function dm_db_query_bind

    function dm_db_query_generate(query, base) result(sql)
        type(db_query_type), intent(inout) :: query !! Query type.
        character(len=*),    intent(in)    :: base  !! Base query string.
        character(len=:), allocatable      :: sql   !! SQL string.

        integer :: i

        sql = trim(base)

        do i = 1, query%index
            if (i == 1) then
                sql = sql // ' WHERE ' // query%params(i)%sql
            else
                sql = sql // ' AND ' // query%params(i)%sql
            end if
        end do

        if (allocated(query%order_by)) then
            if (query%order_desc) then
                sql = sql // ' ORDER BY ' // query%order_by // ' DESC'
            else
                sql = sql // ' ORDER BY ' // query%order_by // ' ASC'
            end if
        end if

        if (query%limit > 0) sql = sql // ' LIMIT ?'
    end function dm_db_query_generate

    subroutine dm_db_query_destroy(query)
        type(db_query_type), intent(inout) :: query

        integer :: i

        if (allocated(query%base)) deallocate (query%base)

        do i = 1, query%index
            if (allocated(query%params(i)%value_text)) deallocate (query%params(i)%value_text)
            if (allocated(query%params(i)%sql))        deallocate (query%params(i)%sql)
        end do

        query = db_query_type()
    end subroutine dm_db_query_destroy

    subroutine dm_db_query_limit(query, limit)
        type(db_query_type), intent(inout)        :: query
        integer(kind=i8),    intent(in), optional :: limit

        if (.not. present(limit)) return
        query%limit = max(0_i8, limit)
    end subroutine dm_db_query_limit

    subroutine dm_db_query_order(query, by, desc)
        type(db_query_type), intent(inout)        :: query
        character(len=*),    intent(in)           :: by
        logical,             intent(in), optional :: desc

        query%order_by = trim(by)
        if (present(desc)) query%order_desc = desc
    end subroutine dm_db_query_order
end module dm_db_query
