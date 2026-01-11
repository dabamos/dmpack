! Author:  Philipp Engel
! Licence: ISC
module dm_db_pragma
    !! SQLite PRAGMA access functions.
    !!
    !! Get and set the SQLite foreign keys PRAGMA value:
    !!
    !! ```fortran
    !! character(:), allocatable :: value
    !! integer :: rc
    !!
    !! rc = dm_db_pragma_get(db, 'foreign_keys', value)
    !! rc = dm_db_pragma_set(db, 'foreign_keys', 'ON')
    !! ```
    use :: dm_db
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    interface dm_db_pragma_get
        !! Generic PRAGMA get function.
        module procedure :: db_pragma_get_int32
        module procedure :: db_pragma_get_int64
        module procedure :: db_pragma_get_string
    end interface dm_db_pragma_get

    interface dm_db_pragma_set
        !! Generic PRAGMA set function.
        module procedure :: db_pragma_set
        module procedure :: db_pragma_set_int32
        module procedure :: db_pragma_set_int64
        module procedure :: db_pragma_set_string
    end interface dm_db_pragma_set

    ! Public procedures.
    public :: dm_db_pragma_get
    public :: dm_db_pragma_set

    ! Private procedures.
    private :: db_pragma_get_int32
    private :: db_pragma_get_int64
    private :: db_pragma_get_string
    private :: db_pragma_set
    private :: db_pragma_set_int32
    private :: db_pragma_set_int64
    private :: db_pragma_set_string
contains
    integer function db_pragma_get_int32(db, name, value) result(rc)
        !! Returns PRAGMA value as 4-byte integer.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !!
        type(db_type), intent(inout) :: db    !! Database.
        character(*),  intent(in)    :: name  !! PRAGMA name.
        integer(i4),   intent(out)   :: value !! PRAGMA value.

        type(db_stmt_type) :: dbs

        value = 0_i4

        sql_block: block
            rc = dm_db_prepare(db, dbs, 'PRAGMA ' // trim(name))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(dbs)
            if (rc /= E_DB_ROW) exit sql_block

            rc = E_DB_TYPE
            if (.not. dm_db_column_is_integer(dbs, 0)) exit sql_block

            rc = E_NONE
            call dm_db_column(dbs, 0, value)
        end block sql_block

        call dm_db_finalize(dbs)
    end function db_pragma_get_int32

    integer function db_pragma_get_int64(db, name, value) result(rc)
        !! Returns PRAGMA value as 8-byte integer.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !!
        type(db_type), intent(inout) :: db    !! Database.
        character(*),  intent(in)    :: name  !! PRAGMA name.
        integer(i8),   intent(out)   :: value !! PRAGMA value.

        type(db_stmt_type) :: dbs

        value = 0_i8

        sql_block: block
            rc = dm_db_prepare(db, dbs, 'PRAGMA ' // trim(name))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(dbs)
            if (rc /= E_DB_ROW) exit sql_block

            rc = E_DB_TYPE
            if (.not. dm_db_column_is_integer(dbs, 0)) exit sql_block

            rc = E_NONE
            call dm_db_column(dbs, 0, value)
        end block sql_block

        call dm_db_finalize(dbs)
    end function db_pragma_get_int64

    integer function db_pragma_get_string(db, name, value) result(rc)
        !! Returns PRAGMA value as allocatable string. On error, the string is
        !! allocated but empty.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed.
        !! * `E_DB_TYPE` if query result is of unexpected type.
        !!
        type(db_type),             intent(inout) :: db    !! Database.
        character(*),              intent(in)    :: name  !! PRAGMA name.
        character(:), allocatable, intent(out)   :: value !! PRAGMA value.

        type(db_stmt_type) :: dbs

        sql_block: block
            rc = dm_db_prepare(db, dbs, 'PRAGMA ' // trim(name))
            if (dm_is_error(rc)) exit sql_block

            rc = dm_db_step(dbs)
            if (rc /= E_DB_ROW) exit sql_block

            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text(dbs, 0)) exit sql_block

            rc = E_NONE
            call dm_db_column(dbs, 0, value)
        end block sql_block

        call dm_db_finalize(dbs)
        if (.not. allocated(value)) value = ''
    end function db_pragma_get_string

    integer function db_pragma_set(db, name) result(rc)
        !! Executes PRAGMA of `name`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        type(db_type), intent(inout) :: db   !! Database.
        character(*),  intent(in)    :: name !! PRAGMA name.

        type(db_stmt_type) :: dbs

        rc = E_READ_ONLY
        if (dm_db_is_read_only(db)) return

        sql_block: block
            rc = dm_db_prepare(db, dbs, 'PRAGMA ' // trim(name))
            if (dm_is_error(rc)) exit sql_block
            rc = dm_db_step(dbs)
        end block sql_block

        call dm_db_finalize(dbs)
    end function db_pragma_set

    integer function db_pragma_set_int32(db, name, value) result(rc)
        !! Sets PRAGMA of `name` to 4-byte integer value.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_util, only: dm_itoa

        type(db_type), intent(inout) :: db    !! Database.
        character(*),  intent(in)    :: name  !! PRAGMA name.
        integer(i4),   intent(in)    :: value !! PRAGMA value.

        rc = db_pragma_set_string(db, name, dm_itoa(value))
    end function db_pragma_set_int32

    integer function db_pragma_set_int64(db, name, value) result(rc)
        !! Sets PRAGMA of `name` to 8-byte integer value.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !! * `E_READ_ONLY` if database is opened read-only.
        !!
        use :: dm_util, only: dm_itoa

        type(db_type), intent(inout) :: db    !! Database.
        character(*),  intent(in)    :: name  !! PRAGMA name.
        integer(i8),   intent(in)    :: value !! PRAGMA value.

        rc = db_pragma_set_string(db, name, dm_itoa(value))
    end function db_pragma_set_int64

    integer function db_pragma_set_string(db, name, value) result(rc)
        !! Sets PRAGMA of `name` to string value.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_DB_PREPARE` if statement preparation failed.
        !! * `E_DB_STEP` if step execution failed or no write permission.
        !!
        type(db_type), intent(inout) :: db    !! Database.
        character(*),  intent(in)    :: name  !! PRAGMA name.
        character(*),  intent(in)    :: value !! PRAGMA value.

        type(db_stmt_type) :: dbs

        sql_block: block
            rc = dm_db_prepare(db, dbs, 'PRAGMA ' // trim(name) // ' = ' // trim(value))
            if (dm_is_error(rc)) exit sql_block
            rc = dm_db_step(dbs)
        end block sql_block

        call dm_db_finalize(dbs)
    end function db_pragma_set_string
end module dm_db_pragma
