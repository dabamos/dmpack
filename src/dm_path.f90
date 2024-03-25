! Author:  Philipp Engel
! Licence: ISC
module dm_path
    !! File system path utility routines.
    implicit none (type, external)
    private

    public :: dm_path_parsed
contains
    function dm_path_parsed(path) result(parsed)
        !! Returns a parsed path, or an empty string. The following format
        !! descriptors are allowed:
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
        !! The descriptors wil be replaced with their current values.
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
                case ('D')
                    parsed = parsed // date(7:8)
                case ('M')
                    parsed = parsed // date(5:6)
                case ('Y')
                    parsed = parsed // date(3:4)
                case ('h')
                    parsed = parsed // time(1:2)
                case ('m')
                    parsed = parsed // time(3:4)
                case ('s')
                    parsed = parsed // time(5:6)
                case default
                    parsed = parsed // '%' // a
            end select

            flag = .false.
        end do
    end function dm_path_parsed
end module dm_path
