! Author:  Philipp Engel
! Licence: ISC
module dm_statistics
    !! Statistics module.
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter :: MEAN_NO_RECURSE = 32

    interface dm_statistics_mean
        !! Calculates mean of real values.
        module procedure :: statistics_mean_real32
        module procedure :: statistics_mean_real64
    end interface dm_statistics_mean

    interface dm_statistics_std_dev
        !! Calculates standard deviation (σ) of real values.
        module procedure :: statistics_std_dev_real32
        module procedure :: statistics_std_dev_real64
    end interface dm_statistics_std_dev

    public :: dm_statistics_mean
    public :: dm_statistics_std_dev

    private :: statistics_mean_real32
    private :: statistics_mean_real64
    private :: statistics_std_dev_real32
    private :: statistics_std_dev_real64
contains
    recursive function statistics_mean_real32(x) result(mean)
        !! Calculates mean of 4-byte real values.
        real(r4), intent(inout) :: x(:) !! Values.
        real(r4)                :: mean !! Mean value.

        integer :: n

        n = size(x)

        if (n <= MEAN_NO_RECURSE) then
            mean = sum(x) / max(1, n)
        else if (modulo(n, 2) == 0) then
            mean = (statistics_mean_real32(x(:n / 2)) + statistics_mean_real32(x(n / 2 + 1:))) / 2
        else
            mean = ((n - 1) * statistics_mean_real32(x(:n - 1)) + x(n)) / n
        end if
    end function statistics_mean_real32

    recursive function statistics_mean_real64(x) result(mean)
        !! Calculates mean of 8-byte real values.
        real(r8), intent(inout) :: x(:) !! Values.
        real(r8)                :: mean !! Mean value.

        integer :: n

        n = size(x)

        if (n <= MEAN_NO_RECURSE) then
            mean = sum(x) / max(1, n)
        else if (modulo(n, 2) == 0) then
            mean = (statistics_mean_real64(x(:n / 2)) + statistics_mean_real64(x(n / 2 + 1:))) / 2
        else
            mean = ((n - 1) * statistics_mean_real64(x(:n - 1)) + x(n)) / n
        end if
    end function statistics_mean_real64

    recursive function statistics_std_dev_real32(x) result(std_dev)
        !! Calculates standard deviation (σ) of 4-byte real values.
        real(r4), intent(inout) :: x(:)    !! Values.
        real(r4)                :: std_dev !! Standard deviation.

        integer(i8) :: i, n
        real(r8)    :: q, s

        std_dev = 0.0_r8

        n = size(x, kind=i8)
        if (n == 0) return

        q = sum(x**2)
        s = sum(x)

        std_dev = real(sqrt(q / n - (s / n)**2))
    end function statistics_std_dev_real32

    recursive function statistics_std_dev_real64(x) result(std_dev)
        !! Calculates standard deviation (σ) of 8-byte real values.
        real(r8), intent(inout) :: x(:)    !! Values.
        real(r8)                :: std_dev !! Standard deviation.

        integer(i8) :: i, n
        real(r8)    :: q, s

        std_dev = 0.0_r8

        n = size(x, kind=i8)
        if (n == 0) return

        q = 0.0_r8
        s = 0.0_r8

        q = sum(x**2)
        s = sum(x)

        std_dev = sqrt(q / n - (s / n)**2)
    end function statistics_std_dev_real64
end module dm_statistics
