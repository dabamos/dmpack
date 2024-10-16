! Author:  Philipp Engel
! Licence: ISC
module dm_block
    !! Module to serialise derived types into ASCII block format.
    use :: dm_dp
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: BLOCK_LINE_LEN = 55

    interface dm_block_from
        !! Generic derived type to block converter.
        module procedure :: block_from_data_point
    end interface dm_block_from

    interface dm_block_write
        !! Generic derived type to block writer.
        module procedure :: block_write_data_point
        module procedure :: block_write_data_points
    end interface dm_block_write

    public :: dm_block_from
    public :: dm_block_write

    private :: block_from_data_point
    private :: block_write_data_point
    private :: block_write_data_points
contains
    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    pure elemental character(len=BLOCK_LINE_LEN) &
    function block_from_data_point(data_point) result(str)
        !! Returns ASCII block representation of data point.
        type(dp_type), intent(in) :: data_point !! Data point type.

        write (str, '(a32, 1x, f25.8)') data_point%x, data_point%y
    end function block_from_data_point

    integer function block_write_data_point(data_point, unit) result(rc)
        !! Writes data point in ASCII block format to file or standard output.
        type(dp_type), intent(in)           :: data_point !! Data point type.
        integer,       intent(in), optional :: unit       !! File unit.

        integer :: unit_, stat

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a32, 1x, f25.8)', iostat=stat) data_point%x, data_point%y
        if (stat /= 0) return
        rc = E_NONE
    end function block_write_data_point

    integer function block_write_data_points(data_points, unit) result(rc)
        !! Writes observations to file or standard output.
        type(dp_type), intent(in)           :: data_points(:) !! Data points array.
        integer,       intent(in), optional :: unit           !! File unit.

        integer :: i, unit_, stat

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit

        do i = 1, size(data_points)
            write (unit_, '(a32, 1x, f25.8)', iostat=stat) data_points(i)%x, data_points(i)%y
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function block_write_data_points
end module dm_block
