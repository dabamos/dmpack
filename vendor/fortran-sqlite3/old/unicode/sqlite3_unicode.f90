! sqlite3_unicode.f90
!
! Author:  Philipp Engel
! Licence: ISC
module sqlite3_unicode
    use, intrinsic :: iso_c_binding
    use :: sqlite3
    implicit none (type, external)
    private

    interface
        ! int sqlite3_bind_text16(sqlite3_stmt *stmt, int idx, const void *val, int l, void(*)(void*))
        function sqlite3_bind_text16_(stmt, idx, val, l, destructor) bind(c, name='sqlite3_bind_text16')
            import :: c_funptr, c_int, c_int16_t, c_ptr, c_size_t
            implicit none
            type(c_ptr),             intent(in), value :: stmt
            integer(kind=c_int),     intent(in), value :: idx
            integer(kind=c_int16_t), intent(in)        :: val(*)
            integer(kind=c_int),     intent(in), value :: l
            integer(kind=c_size_t),  intent(in), value :: destructor
            integer(kind=c_int)                        :: sqlite3_bind_text16_
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
contains
    function sqlite3_bind_text16(stmt, idx, str, destructor)
        !! Binds text to column. This wrapper passes destructor
        !! `SQLITE_TRANSIENT` by default!
        type(c_ptr),            intent(inout)        :: stmt
        integer,                intent(in)           :: idx
        character(len=*),       intent(in)           :: str
        integer(kind=c_size_t), intent(in), optional :: destructor
        integer                                      :: sqlite3_bind_text16

        integer(kind=c_int16_t) :: utf16(len(str))
        integer :: i
        integer :: sz

        do i = 1, size(utf16)
            utf16(i) = iachar(str(i:i), kind=c_int16_t)
        end do

        sz = storage_size(utf16) / 8

        if (present(destructor)) then
            sqlite3_bind_text16 = sqlite3_bind_text16_(stmt, idx, utf16, sz, destructor)
            return
        end if

        sqlite3_bind_text16 = sqlite3_bind_text16_(stmt, idx, utf16, sz, SQLITE_TRANSIENT)
    end function sqlite3_bind_text16

    function sqlite3_column_text16(stmt, idx)
        type(c_ptr), intent(inout)               :: stmt
        integer,     intent(in)                  :: idx
        character(len=:), allocatable :: sqlite3_column_text16
        type(c_ptr)                              :: ptr

        integer(kind=c_int16_t), pointer :: buffer(:)
        integer :: i, n

        ptr = sqlite3_column_text16_(stmt, idx)
        if (.not. c_associated(ptr)) return
        call c_f_pointer(ptr, buffer, [ huge(0) ])

        n = 0
        do
            if (buffer(n) == 0) exit
            n = n + 1
        end do

        allocate (character(len=n) :: sqlite3_column_text16)

        do i = 1, n
            sqlite3_column_text16(i:i) = achar(buffer(i))
        end do
    end function sqlite3_column_text16
end module sqlite3_unicode
