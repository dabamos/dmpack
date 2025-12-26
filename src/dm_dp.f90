! Author:  Philipp Engel
! Licence: ISC
module dm_dp
    !! X/Y data point type declaration that stores a single set of a time
    !! series.
    use :: dm_error
    use :: dm_kind
    use :: dm_time
    implicit none (type, external)
    private

    character(*), parameter :: FMT_XY = '(a32, 1x, f25.8)'

    integer, parameter, public :: DP_STRING_LEN = 58

    type, public :: dp_type
        !! Data point type that contains a timestamp and an associated value,
        !! like a single response of an observation or a single data point of
        !! a time series.
        character(TIME_LEN) :: x = TIME_DEFAULT !! Timestamp in ISO 8601.
        real(r8)            :: y = 0.0_r8       !! Response value.
    end type dp_type

    integer, parameter, public :: DP_TYPE_SIZE = storage_size(dp_type()) / 8 !! Size of `dp_type` in bytes.

!   public :: dm_dp_from_file
    public :: dm_dp_out
    public :: dm_dp_scale
    public :: dm_dp_to_string
contains
!   integer function dm_dp_from_file(path, dps, n, error_line) result(rc)
!       !! Reads X, Y data from a CSV file. Returns the lines in allocatable
!       !! array `dps` and the number of elements in `n`. On error, the
!       !! array might not be allocated. The argument `error_line` returns the
!       !! line in the input file in which the error occured.
!       use :: dm_file
!       character(*),               intent(in)            :: path       !! Path to input file.
!       type(dp_type), allocatable, intent(out)           :: dps(:)     !! Array of data points.
!       integer(i8),                intent(out)           :: n          !! Size of array.
!       integer(i8),                intent(out), optional :: error_line !! Line number of error or 0.
!
!       integer          :: fu, stat
!       integer(i8) :: i
!
!       if (present(error_line)) error_line = 0
!
!       n = dm_file_line_count(path, rc)
!       if (rc /= E_NONE) return
!
!       rc = E_EMPTY
!       if (n == 0) return
!
!       rc = E_ALLOC
!       allocate (dps(n), stat=stat)
!       if (stat /= 0) return
!
!       rc = E_IO
!       open (action='read', file=trim(path), iostat=stat, newunit=fu, status='old')
!       if (stat /= 0) return
!
!       do i = 1, n
!           rc = E_INVALID
!           read (fu, *, iostat=stat) dps%x, dps%y
!           if (stat /= 0) then
!               if (present(error_line)) error_line = i
!               exit
!           end if
!           rc = E_NONE
!       end do
!
!       close (fu)
!   end function dm_dp_from_file

    subroutine dm_dp_out(dp, unit)
        !! Prints data point to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(dp_type), intent(inout)        :: dp
        integer,       intent(in), optional :: unit

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("dp.x: ", a)')       dp%x
        write (unit_, '("dp.y: ", 1pg0.12)') dp%y
    end subroutine dm_dp_out

    pure elemental character(DP_STRING_LEN) function dm_dp_to_string(dp) result(string)
        !! Returns data point as 58 characters long string. The attributes `x`
        !! and `y` are separated by white space.
        type(dp_type), intent(in) :: dp !! Data point type.

        write (string, FMT_XY) dp%x, dp%y
    end function dm_dp_to_string

    pure subroutine dm_dp_scale(dps, scale)
        !! Scales Y value of data points if scale is neither 0.0 nor 1.0.
        use :: dm_util, only: dm_equals

        type(dp_type), intent(inout) :: dps(:) !! Data point type array.
        real(r8),      intent(in)    :: scale  !! Scale factor.

        if (dm_equals(scale, 0.0_r8) .or. dm_equals(scale, 1.0_r8)) return
        dps%y = dps%y * scale
    end subroutine dm_dp_scale
end module dm_dp
