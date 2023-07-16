! Author:  Philipp Engel
! Licence: ISC
module dm_id
    !! ID string verification.
    implicit none (type, external)
    private

    integer, parameter, public :: ID_LEN = 32 !! Max. id length.

    public :: dm_id_valid
contains
    pure elemental logical function dm_id_valid(id) result(valid)
        !! Returns `.true.` if given string is a valid id. A valid id must
        !! be between 1 and 32 characters long, and all characters have to
        !! be in set [-0-9A-Za-z].
        character(len=*), parameter :: ID_SET = &
            '-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
        character(len=*), intent(in) :: id !! String to validate.
        integer                      :: n

        valid = .false.
        n = len_trim(id)
        if (n == 0 .or. n > ID_LEN) return
        if (id(1:1) == ' ') return
        if (verify(id(1:n), ID_SET) > 0) return
        valid = .true.
    end function dm_id_valid
end module dm_id
