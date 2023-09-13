! Author:  Philipp Engel
! Licence: ISC
module dm_dp
    !! X/Y data point type declaration that stores a single set of a time
    !! series, as well as accompanying utility routines for reading from file
    !! and writing to file.
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_time
    implicit none (type, external)
    private

    type, public :: dp_type
        !! Data point type that contains a timestamp and an associated value,
        !! like a single response of an observation or a single data point of
        !! a time series.
        character(len=TIME_LEN) :: x = TIME_DEFAULT !! Timestamp in ISO 8601.
        real(kind=r8)           :: y = 0.0_r8       !! Response value.
    end type dp_type

    public :: dm_dp_from_file
    public :: dm_dp_to_string
contains
    integer function dm_dp_from_file(path, dps, n, error_line) result(rc)
        !! Reads X, Y data from a CSV file. Returns the lines in allocatable
        !! array `dps` and the number of elements in `n`. On error, the
        !! array might not be allocated. The argument `error_line` returns the
        !! line in the input file in which the error occured.
        character(len=*),           intent(in)            :: path       !! Path to input file.
        type(dp_type), allocatable, intent(out)           :: dps(:)     !! Array of data points.
        integer(kind=i8),           intent(out)           :: n          !! Size of array.
        integer(kind=i8),           intent(out), optional :: error_line !! Line number of error or 0.

        integer          :: fu, stat
        integer(kind=i8) :: i

        if (present(error_line)) error_line = 0

        n = dm_file_line_count(path, rc)
        if (rc /= E_NONE) return

        rc = E_EMPTY
        if (n == 0) return

        rc = E_ALLOC
        allocate (dps(n), stat=stat)
        if (stat /= 0) return

        rc = E_IO
        open (action='read', file=trim(path), iostat=stat, newunit=fu, status='old')
        if (stat /= 0) return

        do i = 1, n
            rc = E_INVALID
            read (fu, *, iostat=stat) dps%x, dps%y
            if (stat /= 0) then
                if (present(error_line)) error_line = i
                exit
            end if
            rc = E_NONE
        end do

        close (fu)
    end function dm_dp_from_file

    pure elemental character(len=55) function dm_dp_to_string(dp) result(str)
        !! Returns data point as 55 characters long string. The attributes `x`
        !! and `y` are separated by white space.
        type(dp_type), intent(in) :: dp !! Data point type.

        write (str, '(a29, 1x, f25.8)') dp%x, dp%y
    end function dm_dp_to_string
end module dm_dp
