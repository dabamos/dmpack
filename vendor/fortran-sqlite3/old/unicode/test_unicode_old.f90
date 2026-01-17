! test_unicode.f90
program test_unicode
    use, intrinsic :: iso_c_binding
    use :: sqlite3
    use :: sqlite3_unicode
    implicit none (type, external)
    character(len=*), parameter :: DB_FILE  = 'test.db'
    character(len=*), parameter :: DB_TABLE = 'test_table'

    character(len=:, kind=ucs2), allocatable :: val ! Error message.
    character(len=:), allocatable :: errmsg ! Error message.
    integer                       :: rc     ! Return code.
    type(c_ptr)                   :: db     ! SQLite database.
    type(c_ptr)                   :: stmt   ! SQLite statement.

    ! Open SQLite database.
    rc = sqlite3_open(DB_FILE, db)
    if (rc /= SQLITE_OK) stop 'sqlite3_open(): failed'

    ! Create table.
    rc = sqlite3_exec(db, "CREATE TABLE IF NOT EXISTS " // DB_TABLE // " (" // &
                          "id     INTEGER PRIMARY KEY," // &
                          "string TEXT," // &
                          "value  INTEGER)", &
                      c_null_funptr, c_null_ptr, errmsg)

    rc = sqlite3_prepare_v2(db, "INSERT INTO " // DB_TABLE // " (string, value) VALUES (?, ?)", stmt)
    val = ucs2_'öööööö'
    print *, val, ' ', len(val)
    rc = sqlite3_bind_text16(stmt, 1, val)
    rc = sqlite3_bind_int   (stmt, 2, 4444)
    rc = sqlite3_step(stmt)
    print *, sqlite3_errmsg(db)
    rc = sqlite3_finalize(stmt)

    ! Read values.
    rc = sqlite3_prepare_v2(db, "SELECT * FROM " // DB_TABLE // " ORDER BY id DESC LIMIT 1", stmt)
    call print_values(stmt, 3)
    rc = sqlite3_finalize(stmt)

    rc = sqlite3_close(db)
contains
    subroutine print_values(stmt, ncols)
        type(c_ptr), intent(inout) :: stmt
        integer,     intent(in)    :: ncols
        integer                    :: col_type

        integer                                   :: i
        character(len=:, kind=ucs2), allocatable  :: buf

        do i = 0, ncols - 1
            col_type = sqlite3_column_type(stmt, i)

            select case (col_type)
                case (SQLITE_INTEGER)
                    write (*, '(i12)', advance='no') sqlite3_column_int(stmt, i)

                case (SQLITE_FLOAT)
                    write (*, '(f0.8)', advance='no') sqlite3_column_double(stmt, i)

                case (SQLITE_TEXT)
                    buf = sqlite3_column_text16(stmt, i)
                    write (*, '(a12)', advance='no') buf

                case (SQLITE_NULL)
                    write (*, '(" null")', advance='no')

                case default
                    write (*, '(" not implemented")', advance='no')
            end select
        end do

        print *
    end subroutine print_values
end program test_unicode
