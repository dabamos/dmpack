! Author:  Philipp Engel
! Licence: ISC
module dm_path
    !! File system path utility routines.
    implicit none (type, external)
    private

    public :: dm_path_join
    public :: dm_path_name
contains
    pure function dm_path_join(path1, path2) result(path)
        !! Joins paths and adds `/` between them.
        character(*), intent(in)  :: path1 !! First path.
        character(*), intent(in)  :: path2 !! Second path.
        character(:), allocatable :: path  !! Joined path.

        character(:), allocatable :: p1, p2
        integer                   :: n1, n2

        p1 = trim(adjustl(path1))
        p2 = trim(adjustl(path2))

        n1 = len_trim(p1)
        n2 = len_trim(p2)

        ! Handle empty cases.
        if (n1 == 0 .and. n2 == 0) then
            path = ''
            return
        else if (n1 == 0) then
            path = p2(1:n2)
            return
        else if (n2 == 0) then
            path = p1(1:n1)
            return
        end if

        ! Special case: both are "/".
        if (p1 == '/' .and. p2 == '/') then
            path = '/'
            return
        end if

        ! Join with correct slash handling.
        if (p1(n1:n1) == '/' .and. p2(1:1) == '/') then
            if (n2 > 1) then
                path = p1(1:n1) // p2(2:n2)
            else
                path = p1(1:n1)
            end if
        else if (p1(n1:n1) /= '/' .and. p2(1:1) /= '/') then
            path = p1(1:n1) // '/' // p2(1:n2)
        else
            path = p1(1:n1) // p2(1:n2)
        end if
    end function dm_path_join

    pure function dm_path_name(path) result(name)
        character(*), intent(in)  :: path
        character(:), allocatable :: name

        integer :: i, j, n

        n = len_trim(path)
        i = index(path, '/', back=.true.)
        j = i + 1

        if (i == 0) then
            name = trim(path)
        else if (j <= n) then
            name = trim(path(j:))
        else
            name = ''
        end if
    end function dm_path_name
end module dm_path
