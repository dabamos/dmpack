! Author:  Philipp Engel
! Licence: ISC
module dm_path
    !! File system path utility routines.
    implicit none (type, external)
    private

    public :: dm_path_join
    public :: dm_path_parsed
contains
    pure function dm_path_join(path1, path2) result(path)
        !! Joins paths and adds `/` between them.
        character(len=*), intent(in)  :: path1 !! First path.
        character(len=*), intent(in)  :: path2 !! Second path.
        character(len=:), allocatable :: path  !! Joined path.

        integer :: n1, n2

        n1 = len_trim(path1)
        n2 = len_trim(path2)

        if (n1 == 0 .and. n2 == 0) then
            path = ''
            return
        else if (n1 == 0) then
            path = path2(:n2)
            return
        else if (n2 == 0) then
            path = path1(:n1)
            return
        end if

        if (path1(n1:n1) /= '/' .and. path2(1:1) /= '/') then
            path = path1(:n1) // '/' // path2(:n2)
        else
            path = path1(:n1) // path2(:n2)
        end if
    end function dm_path_join

    function dm_path_parsed(path) result(parsed)
        !! Returns a parsed path or an empty string on error. The following
        !! format descriptors are allowed:
        !!
        !! | Format | Description  |
        !! |--------|--------------|
        !! | `%Y`   | year         |
        !! | `%M`   | month        |
        !! | `%D`   | day of month |
        !! | `%h`   | hour         |
        !! | `%m`   | minute       |
        !! | `%s`   | second       |
        !!
        !! The descriptors will be replaced with their current values.
        character(len=*), intent(in)  :: path   !! Input path.
        character(len=:), allocatable :: parsed !! Output path.

        character         :: a
        character(len=8)  :: date
        character(len=10) :: time
        logical           :: flag
        integer           :: i, n

        parsed = ''

        n = len_trim(path)
        if (n == 0) return

        call date_and_time(date, time)
        flag = .false.

        do i = 1, n
            a = path(i:i)

            if (.not. flag) then
                if (a == '%') then
                    flag = .true.
                else
                    parsed = parsed // a
                end if

                cycle
            end if

            select case (a)
                case ('D');   parsed = parsed // date(7:8)
                case ('M');   parsed = parsed // date(5:6)
                case ('Y');   parsed = parsed // date(3:4)
                case ('h');   parsed = parsed // time(1:2)
                case ('m');   parsed = parsed // time(3:4)
                case ('s');   parsed = parsed // time(5:6)
                case default; parsed = parsed // '%' // a
            end select

            flag = .false.
        end do
    end function dm_path_parsed
end module dm_path
