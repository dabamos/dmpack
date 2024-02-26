! Author:  Philipp Engel
! Licence: ISC
module dm_mqueue_util
    !! Utility procedures for message queue access.
    use :: dm_error
    use :: dm_mqueue
    use :: dm_type
    use :: dm_util
    implicit none (type, external)
    private

    interface dm_mqueue_forward
        !! Generic function to forward derived types via message queue.
        module procedure :: mqueue_forward_observ
    end interface

    ! Public procedures.
    public :: dm_mqueue_forward

    ! Private procedures.
    private :: mqueue_forward_observ
contains
    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function mqueue_forward_observ(observ, name, blocking, quiet, self) result(rc)
        !! Forwards given observation to next receiver. This function creates
        !! log messages about the progress and requires a configured logger,
        !! unless `quiet` is `.false.`. Therefore, it should not be called by
        !! a library procedure (for programs only).
        !!
        !! If `name` is passed and equals the next receiver, the receiver will
        !! be skipped, unless `self` is `.true.`. This behaviour prevents the
        !! observation to be forwarded back to the sender if the sender is the
        !! next receiver in the list.
        use :: dm_id
        use :: dm_log
        use :: dm_logger, dm_log => dm_logger_log
        use :: dm_observ
        use :: dm_system, only: dm_system_error_message
        type(observ_type), intent(inout)        :: observ   !! Observation to forward.
        character(len=*),  intent(in), optional :: name     !! App name.
        logical,           intent(in), optional :: blocking !! Message queue access is blocking.
        logical,           intent(in), optional :: quiet    !! No logging if `.true.`.
        logical,           intent(in), optional :: self     !! Allow forwarding to `name`.

        integer           :: next
        logical           :: blocking_, quiet_, self_
        type(mqueue_type) :: mqueue

        rc   = E_NONE
        next = observ%next

        ! Blocking message queue access.
        blocking_ = .true.
        if (present(blocking)) blocking_ = blocking

        ! Generate log messages.
        quiet_ = .false.
        if (present(quiet)) quiet_ = quiet

        ! Allow forwarding to sender.
        self_ = .false.
        if (present(self)) self_ = self

        do
            ! Increase the receiver index.
            next = max(0, next) + 1

            ! End of receiver list reached?
            if (next > observ%nreceivers) then
                if (.not. quiet_) call dm_log(LOG_DEBUG, 'no receivers left in observ ' // observ%name, observ=observ)
                return
            end if

            ! Invalid receiver name?
            if (.not. dm_id_valid(observ%receivers(next))) then
                rc = E_INVALID
                if (.not. quiet_) then
                    call dm_log(LOG_ERROR, 'invalid receiver ' // trim(observ%receivers(next)) // &
                                ' in observ ' // observ%name, observ=observ, error=rc)
                end if
                return
            end if

            ! Cycle to next + 1 if receiver name equals app name. We don't want
            ! to send the observation to the program instance that called this
            ! function.
            if (.not. present(name)) exit

            ! Forwarding to self is allowed, or valid receiver is found?
            if (self_ .or. observ%receivers(next) /= name) exit

            if (.not. quiet_) then
                call dm_log(LOG_DEBUG, 'skipped receiver ' // dm_itoa(next) // ' (' // &
                            trim(observ%receivers(next)) // ') of observ ' // observ%name)
            end if
        end do

        mqueue_block: block
            ! Open message queue of receiver for writing.
            rc = dm_mqueue_open(mqueue   = mqueue, &
                                type     = TYPE_OBSERV, &
                                name     = observ%receivers(next), &
                                access   = MQUEUE_WRONLY, &
                                blocking = blocking_)

            ! Exit on error.
            if (dm_is_error(rc)) then
                if (.not. quiet_) then
                    call dm_log(LOG_ERROR, 'failed to open mqueue /' // trim(observ%receivers(next)) // ': ' // &
                                dm_system_error_message(), observ=observ, error=rc)
                end if
                exit mqueue_block
            end if

            ! Send observation to message queue.
            observ%next = next
            rc = dm_mqueue_write(mqueue, observ)

            ! Exit on error.
            if (dm_is_error(rc)) then
                if (.not. quiet_) then
                    call dm_log(LOG_ERROR, 'failed to send observ ' // trim(observ%name) // &
                                ' to mqueue /' // observ%receivers(next), observ=observ, error=rc)
                end if
                exit mqueue_block
            end if

            if (.not. quiet_) then
                call dm_log(LOG_DEBUG, 'sent observ ' // trim(observ%name) // ' from ' // &
                            trim(observ%source) // ' to mqueue /' // observ%receivers(next), observ=observ)
            end if
        end block mqueue_block

        ! Close message queue.
        rc = dm_mqueue_close(mqueue)

        if (dm_is_error(rc) .and. .not. quiet_) then
            call dm_log(LOG_WARNING, 'failed to close mqueue /' // observ%receivers(next) // ': ' // &
                        dm_system_error_message(), observ=observ, error=rc)
        end if
    end function mqueue_forward_observ
end module dm_mqueue_util
