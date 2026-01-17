! sqlite3_unicode.f90
!
! Author:  Philipp Engel
! Licence: ISC
module sqlite3_unicode
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: i8 => int64, r8 => real64
    use :: sqlite3
    implicit none (type, external)
    private

    integer, parameter, public :: ucs2 = selected_char_kind('ISO_10646')

    interface
        ! int sqlite3_bind_text16(sqlite3_stmt *stmt, int idx, const void *val, int l, void(*)(void*))
        function sqlite3_bind_text16_(stmt, idx, val, l, destructor) bind(c, name='sqlite3_bind_text16')
            import :: c_funptr, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: stmt
            integer(kind=c_int),    intent(in), value :: idx
            type(c_ptr),            intent(in), value :: val
            integer(kind=c_int),    intent(in), value :: l
            integer(kind=c_size_t), intent(in), value :: destructor
            integer(kind=c_int)                       :: sqlite3_bind_text16_
        end function sqlite3_bind_text16_

        ! const void *sqlite3_column_text16(sqlite3_stmt *stmt, int idx)
        function sqlite3_column_text16_(stmt, idx) bind(c, name='sqlite3_column_text16')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: stmt
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr)                            :: sqlite3_column_text16_
        end function sqlite3_column_text16_
    end interface

    public :: sqlite3_bind_text16
    public :: sqlite3_bind_text16_
    public :: sqlite3_column_text16
    public :: sqlite3_column_text16_

    private :: c_f_ucs_ptr
contains
    function sqlite3_bind_text16(stmt, idx, val, destructor)
        !! Binds text to column. This wrapper passes destructor
        !! `SQLITE_TRANSIENT` by default!
        type(c_ptr),                 intent(inout)        :: stmt
        integer,                     intent(in)           :: idx
        character(len=*, kind=ucs2), target, intent(in)           :: val
        integer(kind=i8),            intent(in), optional :: destructor
        integer                                           :: sqlite3_bind_text16

        if (present(destructor)) then
            sqlite3_bind_text16 = sqlite3_bind_text16_(stmt, idx, c_loc(val), len(val), int(destructor, kind=c_size_t))
            return
        end if

        sqlite3_bind_text16 = sqlite3_bind_text16_(stmt, idx, c_loc(val), len(val), SQLITE_TRANSIENT)
    end function sqlite3_bind_text16

    function sqlite3_column_text16(stmt, idx)
        type(c_ptr), intent(inout)               :: stmt
        integer,     intent(in)                  :: idx
        character(len=:, kind=ucs2), allocatable :: sqlite3_column_text16
        type(c_ptr)                              :: ptr

        ptr = sqlite3_column_text16_(stmt, idx)
        if (.not. c_associated(ptr)) return
        call c_f_ucs_ptr(ptr, sqlite3_column_text16)
    end function sqlite3_column_text16

    subroutine c_f_ucs_ptr(c_str, f_str)
        type(c_ptr),                              intent(in)  :: c_str
        character(len=:, kind=ucs2), allocatable, intent(out) :: f_str

        integer(kind=c_int), pointer :: ptrs(:)
        integer(kind=i8)             :: i, sz

        if (.not. c_associated(c_str)) return
        call c_f_pointer(c_str, ptrs, [ huge(0) ])
        sz = findloc(ptrs, 0, 1, kind=i8)
        allocate (character(len=sz, kind=ucs2) :: f_str)

        do i = 1, sz
            f_str(i:i) = char(ptrs(i), kind=ucs2)
        end do
    end subroutine c_f_ucs_ptr
end module sqlite3_unicode
