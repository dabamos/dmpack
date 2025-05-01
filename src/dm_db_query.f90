! Author:  Philipp Engel
! Licence: ISC
module dm_db_query
    !! Basic SQL query builder.
    !!
    !! Make sure to not add more than `DB_QUERY_NPARAMS` parameters to a query
    !! or increase the parameter first.
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

    integer, parameter :: DB_QUERY_NPARAMS = 16 !! Max. number of WHERE and SET parameters in a query.

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
        !! do not validate that values have been added only once.
        module procedure :: db_query_update_double
        module procedure :: db_query_update_int
        module procedure :: db_query_update_int64
        module procedure :: db_query_update_text
    end interface dm_db_query_update

    interface dm_db_query_where
        !! Generic subroutine to add WHERE values to query. The procedures
        !! do not validate that values have been added only once.
        module procedure :: db_query_where_double
        module procedure :: db_query_where_int
        module procedure :: db_query_where_int64
        module procedure :: db_query_where_text
    end interface dm_db_query_where

    public :: dm_db_query_build
    public :: dm_db_query_destroy
    public :: dm_db_query_set_limit
    public :: dm_db_query_set_order
    public :: dm_db_query_update
    public :: dm_db_query_where

    private :: db_query_param_destroy
    private :: db_query_update_double
    private :: db_query_update_int
    private :: db_query_update_int64
    private :: db_query_update_text
    private :: db_query_where_double
    private :: db_query_where_int
    private :: db_query_where_int64
    private :: db_query_where_text
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    function dm_db_query_build(db_query, base) result(sql)
        !! Returns SQL string from query.
        use :: dm_util, only: dm_btoa

        type(db_query_type), intent(inout)        :: db_query !! Database query type.
        character(len=*),    intent(in), optional :: base     !! Base query string.
        character(len=:), allocatable             :: sql      !! SQL string.

        integer :: i

        ! SQL query.
        if (present(base)) then
            sql = trim(base)
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

        if (allocated(db_query%order_by)) deallocate (db_query%order_by)

        call db_query_param_destroy(db_query%params)
        call db_query_param_destroy(db_query%updates)

        db_query%order_desc = .false.
        db_query%limit      = 0_i8
        db_query%nparams    = 0
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

    pure subroutine db_query_update_double(db_query, column, value, error)
        !! Adds double precision SET parameter to query. Returns `E_LIMIT` in
        !! `error` if parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: column   !! Column name.
        real(kind=r8),       intent(in)            :: value    !! New column value.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nupdates >= size(db_query%updates)) return
        if (present(error)) error = E_NONE

        n = db_query%nupdates + 1

        db_query%nupdates                = n
        db_query%updates(n)%type         = DB_QUERY_TYPE_DOUBLE
        db_query%updates(n)%value_double = value
        db_query%updates(n)%sql          = trim(column)
    end subroutine db_query_update_double

    pure subroutine db_query_update_int(db_query, column, value, error)
        !! Adds 32-bit integer SET parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: column   !! Column name.
        integer(kind=i4),    intent(in)            :: value    !! New column value.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nupdates >= size(db_query%updates)) return
        if (present(error)) error = E_NONE

        n = db_query%nupdates + 1

        db_query%nupdates             = n
        db_query%updates(n)%type      = DB_QUERY_TYPE_INT
        db_query%updates(n)%value_int = value
        db_query%updates(n)%sql       = trim(column)
    end subroutine db_query_update_int

    pure subroutine db_query_update_int64(db_query, column, value, error)
        !! Adds 64-bit integer SET parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: column   !! Column name.
        integer(kind=i8),    intent(in)            :: value    !! New column value.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nupdates >= size(db_query%updates)) return
        if (present(error)) error = E_NONE

        n = db_query%nupdates + 1

        db_query%nupdates               = n
        db_query%updates(n)%type        = DB_QUERY_TYPE_INT64
        db_query%updates(n)%value_int64 = value
        db_query%updates(n)%sql         = trim(column)
    end subroutine db_query_update_int64

    pure subroutine db_query_update_text(db_query, column, value, error)
        !! Adds text parameter to SET query. Returns `E_LIMIT` if parameter
        !! limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: column   !! Column name.
        character(len=*),    intent(in)            :: value    !! New column value.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nupdates >= size(db_query%updates)) return
        if (present(error)) error = E_NONE

        n = db_query%nupdates + 1

        db_query%nupdates              = n
        db_query%updates(n)%type       = DB_QUERY_TYPE_TEXT
        db_query%updates(n)%value_text = trim(value)
        db_query%updates(n)%sql        = trim(column)
    end subroutine db_query_update_text

    subroutine db_query_where_double(db_query, param, value, error)
        !! Adds double precision WHERE parameter to query. Returns `E_LIMIT` in
        !! `error` if parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        real(kind=r8),       intent(in)            :: value    !! Query parameter value.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nparams >= size(db_query%params)) return
        if (present(error)) error = E_NONE

        n = db_query%nparams + 1

        db_query%nparams                = n
        db_query%params(n)%type         = DB_QUERY_TYPE_DOUBLE
        db_query%params(n)%value_double = value
        db_query%params(n)%sql          = trim(param)
    end subroutine db_query_where_double

    subroutine db_query_where_int(db_query, param, value, error)
        !! Adds 32-bit integer WHERE parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        integer(kind=i4),    intent(in)            :: value    !! Query parameter value.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nparams >= size(db_query%params)) return
        if (present(error)) error = E_NONE

        n = db_query%nparams + 1

        db_query%nparams             = n
        db_query%params(n)%type      = DB_QUERY_TYPE_INT
        db_query%params(n)%value_int = value
        db_query%params(n)%sql       = trim(param)
    end subroutine db_query_where_int

    subroutine db_query_where_int64(db_query, param, value, error)
        !! Adds 64-bit integer WHERE parameter to query. Returns `E_LIMIT` if
        !! parameter limit has been reached.
        type(db_query_type), intent(inout)         :: db_query !! Database query type.
        character(len=*),    intent(in)            :: param    !! Query parameter.
        integer(kind=i8),    intent(in)            :: value    !! Query parameter value.
        integer,             intent(out), optional :: error    !! Error code.

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nparams >= size(db_query%params)) return
        if (present(error)) error = E_NONE

        n = db_query%nparams + 1

        db_query%nparams               = n
        db_query%params(n)%type        = DB_QUERY_TYPE_INT64
        db_query%params(n)%value_int64 = value
        db_query%params(n)%sql         = trim(param)
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

        integer :: n

        if (present(error)) error = E_LIMIT
        if (db_query%nparams >= size(db_query%params)) return
        if (present(error)) error = E_NONE

        if (.not. dm_present(empty, .false.) .and. len_trim(value) == 0) return

        n = db_query%nparams + 1

        db_query%nparams              = n
        db_query%params(n)%type       = DB_QUERY_TYPE_TEXT
        db_query%params(n)%value_text = trim(value)
        db_query%params(n)%sql        = trim(param)
    end subroutine db_query_where_text
end module dm_db_query
