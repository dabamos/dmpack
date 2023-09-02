! Author:  Philipp Engel
! Licence: ISC
module dm_timer
    !! Basic timer to measure the seconds between start and stop by counting
    !! clock cycles.
    use :: dm_type
    implicit none (type, external)
    private

    type, public :: timer_type
        !! Timer type that holds CPU clock counts and rate.
        integer(kind=i8) :: t(2) = 0_i8 !! CPU clock counts t1, t2.
        integer(kind=i8) :: rate = 0_i8 !! CPU clock rate.
    end type timer_type

    public :: dm_timer_delta
    public :: dm_timer_start
    public :: dm_timer_stop
contains
    real(kind=r8) function dm_timer_delta(timer) result(dt)
        !! Returns timer value as 8-byte real.
        type(timer_type), intent(inout) :: timer !! Timer type.

        dt = 0_i8
        if (timer%rate == 0_i8) return
        dt = (timer%t(2) - timer%t(1)) / dble(timer%rate)
    end function dm_timer_delta

    real(kind=r8) function dm_timer_stop(timer) result(dt)
        !! Stops the timer and returns the time delta as 8-byte real.
        type(timer_type), intent(inout) :: timer !! Timer type.

        call system_clock(count=timer%t(2))
        dt = (timer%t(2) - timer%t(1)) / dble(timer%rate)
    end function dm_timer_stop

    subroutine dm_timer_start(timer)
        !! Starts the timer by setting clock rate and first clock count.
        type(timer_type), intent(inout) :: timer !! Timer type.

        call system_clock(count=timer%t(1), count_rate=timer%rate)
    end subroutine dm_timer_start
end module dm_timer
