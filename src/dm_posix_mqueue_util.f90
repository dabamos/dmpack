! Author:  Philipp Engel
! Licence: ISC
module dm_posix_mqueue_util
    !! Utility procedures for message queue access.
    use :: dm_error
    use :: dm_posix_mqueue
    use :: dm_type
    use :: dm_util
    implicit none (type, external)
    private

    interface dm_posix_mqueue_forward
        !! Generic function to forward derived types via message queue.
        module procedure :: posix_mqueue_forward_observ
    end interface dm_posix_mqueue_forward

    ! Public procedures.
    public :: dm_posix_mqueue_forward

    ! Private procedures.
    private :: posix_mqueue_forward_observ
contains
    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function posix_mqueue_forward_observ(observ, name, blocking, allow_self, use_logger) result(rc)
        !! Forwards given observation to next receiver. This function creates
        !! log messages, unless `user_logger` is passed and `.false.`.
        !!
        !! If `name` is passed and equals the next receiver, the receiver will
        !! be skipped, unless `allow_self` is `.true.`. This behaviour prevents
        !! the observation from being forwarded back to the sender if the sender
        !! is the next receiver in the list.
        use :: dm_id
        use :: dm_log
        use :: dm_logger
        use :: dm_observ
        use :: dm_posix, only: dm_posix_error_message

        type(observ_type), intent(inout)        :: observ     !! Observation to forward.
        character(*),      intent(in), optional :: name       !! App name.
        logical,           intent(in), optional :: blocking   !! Blocking message queue access.
        logical,           intent(in), optional :: allow_self !! Allow forwarding to `name`.
        logical,           intent(in), optional :: use_logger !! Create log messages (enabled by default).

!       class(logger_class), pointer :: logger
!
!       integer                 :: stat
!       logical                 :: allow_self_, blocking_, use_logger_
!       type(posix_mqueue_type) :: mqueue
!
!       rc = E_NONE
!
!       blocking_   = dm_present(blocking,   .true.)  ! Blocking message queue access.
!       allow_self_ = dm_present(allow_self, .false.) ! Allow forwarding to sender.
!       use_logger_ = dm_present(use_logger, .true.)  ! Enable logging.
!
!       if (use_logger_) logger => dm_logger_get_default()
!
!       ! End of receiver list reached?
!       if (.not. dm_observ_has_receiver(observ)) then
!           if (use_logger_) call logger%debug('no receiver in observation ' // observ%name, observ=observ)
!           return
!       end if
!
!       ! Invalid receiver name?
!       if (.not. dm_id_is_valid(observ%receiver)) then
!           rc = E_INVALID
!           if (use_logger_) call logger%error('invalid receiver ' // trim(observ%receiver) // ' in observation ' // observ%name, observ=observ, error=rc)
!           return
!       end if
!
!       ! Do not forward to self.
!       if (.not. allow_self_ .and. observ%receiver == name) return
!
!       mqueue_block: block
!           ! Open message queue of receiver for writing.
!           rc = dm_posix_mqueue_open(mqueue   = mqueue,              &
!                                     type     = TYPE_OBSERV,         &
!                                     name     = observ%receiver,     &
!                                     access   = POSIX_MQUEUE_WRONLY, &
!                                     blocking = blocking_)
!
!           ! Exit on error.
!           if (dm_is_error(rc)) then
!               if (use_logger_) call logger%error('failed to open mqueue /' // trim(observ%receiver) // ': ' // dm_posix_error_message(), observ=observ, error=rc)
!               exit mqueue_block
!           end if
!
!           ! Send observation to message queue.
!           rc = dm_posix_mqueue_write(mqueue, observ)
!
!           ! Exit on error.
!           if (dm_is_error(rc)) then
!               if (use_logger_) call logger%error('failed to send observation ' // trim(observ%name) // ' to mqueue /' // observ%receiver, observ=observ, error=rc)
!               exit mqueue_block
!           end if
!
!           if (use_logger_) call logger%debug('sent observation ' // trim(observ%name) // ' to mqueue /' // observ%receiver, observ=observ)
!       end block mqueue_block
!
!       ! Close message queue.
!       call dm_posix_mqueue_close(mqueue, stat)
!
!       if (dm_is_error(stat) .and. use_logger_) then
!           rc = stat
!           call logger%warning('failed to close mqueue /' // trim(observ%receiver) // ': ' // dm_posix_error_message(), observ=observ, error=rc)
!       end if
    end function posix_mqueue_forward_observ
end module dm_posix_mqueue_util
