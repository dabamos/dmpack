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
    integer function mqueue_forward_observ(observ, name, blocking, self, verbose) result(rc)
        !! Forwards given observation to next receiver. This function creates
        !! log messages, unless `verbose` is passed and `.false.`.
        !!
        !! If `name` is passed and equals the next receiver, the receiver will
        !! be skipped, unless `self` is `.true.`. This behaviour prevents the
        !! observation to be forwarded back to the sender if the sender is the
        !! next receiver in the list.
        use :: dm_id
        use :: dm_log
        use :: dm_logger
        use :: dm_observ
        use :: dm_system, only: dm_system_error_message

        type(observ_type),            intent(inout)        :: observ   !! Observation to forward.
        character(len=*),             intent(in), optional :: name     !! App name.
        logical,                      intent(in), optional :: blocking !! Blocking message queue access.
        logical,                      intent(in), optional :: self     !! Allow forwarding to `name`.
        logical,                      intent(in), optional :: verbose  !! Create log messages (enabled by default).

        class(logger_class), pointer :: logger

        integer           :: next
        logical           :: blocking_, self_, verbose_
        type(mqueue_type) :: mqueue

        rc   = E_NONE
        next = observ%next

        ! Blocking message queue access.
        blocking_ = .true.
        if (present(blocking)) blocking_ = blocking

        ! Allow forwarding to sender.
        self_ = .false.
        if (present(self)) self_ = self

        ! Enable logging.
        verbose_ = .true.
        if (present(verbose)) verbose_ = verbose
        if (verbose_) logger => dm_logger_get()

        do
            ! Increase the receiver index.
            next = max(0, next) + 1

            ! End of receiver list reached?
            if (next > observ%nreceivers) then
                if (verbose_) call logger%debug('no receivers left in observ ' // observ%name, observ=observ)
                return
            end if

            ! Invalid receiver name?
            if (.not. dm_id_valid(observ%receivers(next))) then
                rc = E_INVALID
                if (verbose_) then
                    call logger%error('invalid receiver ' // trim(observ%receivers(next)) // &
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

            if (verbose_) then
                call logger%debug('skipped receiver ' // dm_itoa(next) // ' (' // &
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
                if (verbose_) then
                    call logger%error('failed to open mqueue /' // trim(observ%receivers(next)) // ': ' // &
                                      dm_system_error_message(), observ=observ, error=rc)
                end if
                exit mqueue_block
            end if

            ! Send observation to message queue.
            observ%next = next
            rc = dm_mqueue_write(mqueue, observ)

            ! Exit on error.
            if (dm_is_error(rc)) then
                if (verbose_) then
                    call logger%error('failed to send observ ' // trim(observ%name) // &
                                      ' to mqueue /' // observ%receivers(next), observ=observ, error=rc)
                end if
                exit mqueue_block
            end if

            if (verbose_) then
                call logger%debug('sent observ ' // trim(observ%name) // ' to mqueue /' // &
                                  observ%receivers(next), observ=observ)
            end if
        end block mqueue_block

        ! Close message queue.
        rc = dm_mqueue_close(mqueue)

        if (dm_is_error(rc) .and. verbose_) then
            call logger%warning('failed to close mqueue /' // observ%receivers(next) // ': ' // &
                                dm_system_error_message(), observ=observ, error=rc)
        end if
    end function mqueue_forward_observ
end module dm_mqueue_util
