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

        integer :: n1, n2

        n1 = len_trim(path1)
        n2 = len_trim(path2)

        if (n1 == 0 .and. n2 == 0) then
            path = ''
            return
        else if (n1 == 0) then
            path = adjustl(path2(:n2))
            return
        else if (n2 == 0) then
            path = adjustl(path1(:n1))
            return
        end if

        if (path1(n1:n1) /= '/' .and. path2(1:1) /= '/') then
            path = adjustl(path1(:n1)) // '/' // adjustl(path2(:n2))
        else
            path = adjustl(path1(:n1)) // adjustl(path2(:n2))
        end if
    end function dm_path_join
end module dm_path
