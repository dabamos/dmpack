! sqlite3_meta.f90
!
! Bindings to column meta data, for a SQLite 3 library compiled with option
! `-DSQLITE_ENABLE_COLUMN_METADATA=1`.
!
! Author:  Philipp Engel
! Licence: ISC
module sqlite3_meta
    use, intrinsic :: iso_c_binding
    use :: sqlite3_util
    implicit none (type, external)
    private

    integer, parameter :: c_unsigned_int = c_int

    public :: sqlite3_column_database_name
    public :: sqlite3_column_database_name_
    public :: sqlite3_column_origin_name
    public :: sqlite3_column_origin_name_
    public :: sqlite3_column_table_name
    public :: sqlite3_column_table_name_

    interface
        ! const char *sqlite3_column_database_name(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_database_name_(stmt, idx) bind(c, name='sqlite3_column_database_name')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr)                            :: sqlite3_column_database_name_
        end function sqlite3_column_database_name_

        ! const char *sqlite3_column_origin_name(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_origin_name_(stmt, idx) bind(c, name='sqlite3_column_origin_name')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr)                            :: sqlite3_column_origin_name_
        end function sqlite3_column_origin_name_

        ! const char *sqlite3_column_table_name(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_table_name_(stmt, idx) bind(c, name='sqlite3_column_table_name')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr)                            :: sqlite3_column_table_name_
        end function sqlite3_column_table_name_
    end interface
contains
    function sqlite3_column_database_name(stmt, idx)
        type(c_ptr), intent(inout)    :: stmt
        integer,     intent(in)       :: idx
        character(len=:), allocatable :: sqlite3_column_database_name
        type(c_ptr)                   :: ptr

        ptr = sqlite3_column_database_name_(stmt, idx)
        call c_f_str_ptr(ptr, sqlite3_column_database_name)
    end function sqlite3_column_database_name

    function sqlite3_column_origin_name(stmt, idx)
        type(c_ptr), intent(inout)    :: stmt
        integer,     intent(in)       :: idx
        character(len=:), allocatable :: sqlite3_column_origin_name
        type(c_ptr)                   :: ptr

        ptr = sqlite3_column_origin_name_(stmt, idx)
        call c_f_str_ptr(ptr, sqlite3_column_origin_name)
    end function sqlite3_column_origin_name

    function sqlite3_column_table_name(stmt, idx)
        type(c_ptr), intent(inout)    :: stmt
        integer,     intent(in)       :: idx
        character(len=:), allocatable :: sqlite3_column_table_name
        type(c_ptr)                   :: ptr

        ptr = sqlite3_column_table_name_(stmt, idx)
        call c_f_str_ptr(ptr, sqlite3_column_table_name)
    end function sqlite3_column_table_name
end module sqlite3_meta
