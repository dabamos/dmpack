! Author:  Philipp Engel
! Licence: ISC
module dm_posix_mqueue_util
    !! Utility procedures for message queue access.
    use :: dm_error
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
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    integer function posix_mqueue_forward_observ(observ, name, blocking, allow_self, use_logger) result(rc)
        !! Deprecated. Will be removed.
        use :: dm_observ

        type(observ_type), intent(inout)        :: observ     !! Observation to forward.
        character(*),      intent(in), optional :: name       !! App name.
        logical,           intent(in), optional :: blocking   !! Blocking message queue access.
        logical,           intent(in), optional :: allow_self !! Allow forwarding to `name`.
        logical,           intent(in), optional :: use_logger !! Create log messages (enabled by default).

        rc = E_NONE
    end function posix_mqueue_forward_observ
end module dm_posix_mqueue_util
