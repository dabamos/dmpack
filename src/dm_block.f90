! Author:  Philipp Engel
! Licence: ISC
module dm_block
    !! Module to serialise derived types into ASCII block format.
    use :: dm_dp
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: BLOCK_LINE_LEN = 55

    interface dm_block_from
        !! Generic derived type to block converter.
        module procedure :: block_from_dp
    end interface dm_block_from

    interface dm_block_write
        !! Generic derived type to block writer.
        module procedure :: block_write_dp
        module procedure :: block_write_dps
    end interface dm_block_write

    public :: dm_block_from
    public :: dm_block_write

    private :: block_from_dp
    private :: block_write_dp
    private :: block_write_dps
contains
    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    pure elemental character(BLOCK_LINE_LEN) &
    function block_from_dp(dp) result(string)
        !! Returns ASCII block representation of data point.
        type(dp_type), intent(in) :: dp !! Data point type.

        write (string, '(a32, 1x, f25.8)') dp%x, dp%y
    end function block_from_dp

    integer function block_write_dp(dp, unit) result(rc)
        !! Writes data point in ASCII block format to file or standard output.
        type(dp_type), intent(in)           :: dp !! Data point type.
        integer,       intent(in), optional :: unit       !! File unit.

        integer :: unit_, stat

        rc = E_WRITE
        unit_ = dm_present(unit, STDOUT)
        write (unit_, '(a32, 1x, f25.8)', iostat=stat) dp%x, dp%y
        if (stat /= 0) return
        rc = E_NONE
    end function block_write_dp

    integer function block_write_dps(dps, unit) result(rc)
        !! Writes observations to file or standard output.
        type(dp_type), intent(in)           :: dps(:) !! Data points array.
        integer,       intent(in), optional :: unit           !! File unit.

        integer :: i, unit_, stat

        rc = E_WRITE
        unit_ = dm_present(unit, STDOUT)

        do i = 1, size(dps)
            write (unit_, '(a32, 1x, f25.8)', iostat=stat) dps(i)%x, dps(i)%y
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function block_write_dps
end module dm_block
