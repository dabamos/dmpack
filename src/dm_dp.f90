! Author:  Philipp Engel
! Licence: ISC
module dm_dp
    !! X/Y data point type declaration that stores a single set of a time
    !! series.
    use :: dm_error
    use :: dm_kind
    use :: dm_time
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: DP_STRING_LEN = 60

    type, public :: dp_type
        !! Data point type that contains a timestamp and an associated value,
        !! like a single response of an observation or a single data point of
        !! a time series.
        character(TIME_LEN) :: x = TIME_DEFAULT !! Timestamp in ISO 8601.
        real(r8)            :: y = 0.0_r8       !! Value.
    end type dp_type

    integer, parameter, public :: DP_TYPE_SIZE = storage_size(dp_type()) / 8 !! Size of `dp_type` in bytes.

    interface operator (==)
        !! Returns `.true.` if data points are equal.
        module procedure :: dm_dp_equals
    end interface

    public :: operator (==)

    public :: dm_dp_equals
    public :: dm_dp_out
    public :: dm_dp_scale
    public :: dm_dp_to_string
contains
    pure elemental logical function dm_dp_equals(dp1, dp2) result(equals)
        !! Returns `.true.` if given data points are equal.
        use :: dm_util, only: dm_equals

        type(dp_type), intent(in) :: dp1 !! The first data point.
        type(dp_type), intent(in) :: dp2 !! The second data point.

        equals = (dp1%x == dp2%x .and. dm_equals(dp1%y, dp2%y))
    end function dm_dp_equals

    subroutine dm_dp_out(dp, unit)
        !! Prints data point to standard output or given file unit.
        character(*), parameter :: FMT_REAL = '(1pg0.12)'

        type(dp_type), intent(inout)        :: dp   !! Data point.
        integer,       intent(in), optional :: unit !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("dp.x: ", a)')                  dp%x
        write (unit_, '("dp.y: ", ' // FMT_REAL // ')') dp%y
    end subroutine dm_dp_out

    pure elemental character(DP_STRING_LEN) function dm_dp_to_string(dp) result(string)
        !! Returns data point as 58 characters long string. The attributes `x`
        !! and `y` are separated by white space.
        character(*), parameter :: FMT_XY = '(a32, 1x, 1pg0.20)'

        type(dp_type), intent(in) :: dp !! Data point.

        integer :: stat

        string = ' '
        write (string, FMT_XY, iostat=stat) dp%x, dp%y
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
