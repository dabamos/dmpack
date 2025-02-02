! Author:  Philipp Engel
! Licence: ISC
module dm_db_stmt
    !! Database statement declaration.
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    type, public :: db_stmt_type
        !! SQLite database statement type.
        type(c_ptr) :: ctx = c_null_ptr !! C pointer to SQLite 3 statement.
    end type db_stmt_type

    public :: dm_db_stmt_is_prepared
contains
    logical function dm_db_stmt_is_prepared(db_stmt) result(is)
        !! Returns `.true.` if given statement has been prepared.
        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.

        is = c_associated(db_stmt%ctx)
    end function dm_db_stmt_is_prepared
end module dm_db_stmt
