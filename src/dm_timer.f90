! Author:  Philipp Engel
! Licence: ISC
module dm_timer
    !! Basic timer to measure the seconds between start and stop by counting
    !! clock cycles.
    use :: dm_kind
    implicit none (type, external)
    private

    type, public :: timer_type
        !! Sequential timer type that holds CPU clock counts and rate.
        sequence
        integer(kind=i8) :: t(2) = 0_i8 !! CPU clock counts t1, t2.
        integer(kind=i8) :: rate = 0_i8 !! CPU clock rate.
    end type timer_type

    public :: dm_timer_result
    public :: dm_timer_start
    public :: dm_timer_stop
contains
    real(kind=r8) function dm_timer_result(timer) result(duration)
        !! Returns elapsed time between timer start and stop in seconds as
        !! 8-byte real. The result is 0.0 if the timer has not been stopped yet.
        type(timer_type), intent(inout) :: timer !! Timer type.

        duration = 0.0_r8
        if (timer%rate == 0_i8) return
        duration = (timer%t(2) - timer%t(1)) / dble(timer%rate)
    end function dm_timer_result

    subroutine dm_timer_stop(timer, duration)
        !! Stops the timer and optionally returns result as 8-byte real in
        !! `duration`.
        type(timer_type), intent(inout)         :: timer    !! Timer type.
        real(kind=r8),    intent(out), optional :: duration !! Elapsed time between start and stop [sec].

        call system_clock(count=timer%t(2), count_rate=timer%rate)
        if (present(duration)) duration = dm_timer_result(timer)
    end subroutine dm_timer_stop

    subroutine dm_timer_start(timer)
        !! Starts the timer by setting clock rate and first clock count.
        type(timer_type), intent(inout) :: timer !! Timer type.

        call system_clock(count=timer%t(1))
    end subroutine dm_timer_start
end module dm_timer
