! Author:  Philipp Engel
! Licence: ISC
module dm_id
    !! ID string verification.
    implicit none (type, external)
    private

    integer, parameter, public :: ID_LEN = 32 !! Max. id length.

    public :: dm_id_is_valid
contains
    pure elemental logical function dm_id_is_valid(id, max_len) result(valid)
        !! Returns `.true.` if given string is a valid id. A valid id must
        !! be between 1 and 32 characters long, and all characters have to
        !! be in set `[-0-9A-Z_a-z]`. The optional argument `max_len`
        !! overwrites the default maximum id length.
        use :: dm_util, only: dm_present

        character(*), parameter :: ID_SET = &
            '-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz'

        character(*), intent(in)           :: id      !! String to validate.
        integer,      intent(in), optional :: max_len !! Max. id length.

        integer :: max_len_, n

        valid = .false.
        max_len_ = dm_present(max_len, ID_LEN)
        n = len_trim(id)
        if (n == 0 .or. n > max_len_) return
        if (id(1:1) == ' ') return
        if (verify(id(1:n), ID_SET) > 0) return
        valid = .true.
    end function dm_id_is_valid
end module dm_id
