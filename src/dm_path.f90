! Author:  Philipp Engel
! Licence: ISC
module dm_path
    !! File system path utility routines.
    implicit none (type, external)
    private

    public :: dm_path_join
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
end module dm_path
