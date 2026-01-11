! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_message
    !! Abstraction layer over NNG messages.
    use :: dm_c
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_uuid
    implicit none (type, external)
    private

    integer, parameter, public :: IPC_MESSAGE_MAGIC  = int(z'444D32') !! Magic bytes of IPC messages (`DM2` in ASCII).
    integer, parameter, public :: IPC_MESSAGE_ID_LEN = ID_LEN         !! Max. IPC message id len.

    integer, parameter, public :: IPC_MESSAGE_TYPE_NONE     = 0 !! No message body.
    integer, parameter, public :: IPC_MESSAGE_TYPE_REQUEST  = 1
    integer, parameter, public :: IPC_MESSAGE_TYPE_RESPONSE = 2
    integer, parameter, public :: IPC_MESSAGE_TYPE_OBSERV   = 3
    integer, parameter, public :: IPC_MESSAGE_TYPE_LOG      = 4
    integer, parameter, public :: IPC_MESSAGE_TYPE_IMAGE    = 5
    integer, parameter, public :: IPC_MESSAGE_TYPE_LAST     = 5

    type, public :: ipc_message_header_type
        !! IPC message header.
        integer(i4),   private        :: magic   = IPC_MESSAGE_MAGIC     !! Magic bytes.
        character(IPC_MESSAGE_ID_LEN) :: id      = ' '                   !! Message id (UUIDv4).
        character(IPC_MESSAGE_ID_LEN) :: from    = ' '                   !! Name of sender (`-0-9A-Z_a-z`).
        character(IPC_MESSAGE_ID_LEN) :: to      = ' '                   !! Name of receiver (`-0-9A-Z_a-z`).
        integer(i4)                   :: error   = E_NONE                !! Error code.
        integer(i4)                   :: size    = 0                     !! Actual body size [byte].
        integer(i4)                   :: type    = IPC_MESSAGE_TYPE_NONE !! Message body type (`IPC_MESSAGE_TYPE_*`).
        character(16), private        :: padding = achar(0)              !! Padding.
    end type ipc_message_header_type

    integer, parameter, public :: IPC_MESSAGE_HEADER_TYPE_SIZE = storage_size(ipc_message_header_type()) / 8 !! Size of `ipc_message_header_type` [byte].

    type, public :: ipc_message_type
        !! Opaque IPC message type.
        private
        type(c_ptr) :: nng_msg = c_null_ptr !! NNG message context.
    end type ipc_message_type

    interface dm_ipc_message_body
        module procedure :: ipc_message_body_log
        module procedure :: ipc_message_body_observ
    end interface dm_ipc_message_body

    public :: dm_ipc_message_body
    public :: dm_ipc_message_header
    public :: dm_ipc_message_header_is_valid
    public :: dm_ipc_message_header_out
    public :: dm_ipc_message_type_is_valid
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ipc_message_create(message, from, to, error, size, type) result(rc)
        type(ipc_message_type), intent(out)          :: message !! IPC message.
        character(*),           intent(in)           :: from    !! Sender id.
        character(*),           intent(in), optional :: to      !! Receiver id.
        integer,                intent(in), optional :: error   !! DMPACK error code.
        integer,                intent(in), optional :: size    !! IPC message size (incl. header) [byte].
        integer,                intent(in), optional :: type    !! IPC message type.

        type(ipc_message_header_type) :: header

        rc = dm_ipc_message_header_create(header, from, to, error, size, type)
        if (dm_is_error(rc)) return
    end function dm_ipc_message_create

    integer function dm_ipc_message_header_create(header, from, to, error, size, type) result(rc)
        type(ipc_message_header_type), intent(out)          :: header !! IPC message header.
        character(*),                  intent(in)           :: from   !! Sender id.
        character(*),                  intent(in), optional :: to     !! Receiver id.
        integer,                       intent(in), optional :: error  !! DMPACK error code.
        integer,                       intent(in), optional :: size   !! IPC message size (incl. header) [byte].
        integer,                       intent(in), optional :: type   !! IPC message type.

        header%id = dm_uuid4()

        if (present(to))    header%to    = to
        if (present(error)) header%error = error
        if (present(size))  header%size  = size
        if (present(type))  header%type  = type

        rc = E_INVALID
        if (dm_ipc_message_header_is_valid(header)) rc = E_NONE
    end function dm_ipc_message_header_create

    integer function dm_ipc_message_header(message, header) result(rc)
        type(ipc_message_type),        intent(inout) :: message !! IPC message.
        type(ipc_message_header_type), intent(out)   :: header  !! IPC message header.

        rc = E_NONE
    end function dm_ipc_message_header

    pure elemental logical function dm_ipc_message_header_is_valid(header) result(valid)
        type(ipc_message_header_type), intent(in) :: header !! IPC message header.

        valid = (header%magic == IPC_MESSAGE_MAGIC .and. &
                 dm_uuid4_is_valid(header%id)      .and. &
                 dm_id_is_valid(header%from)       .and. &
                 dm_id_is_valid(header%to)         .and. &
                 dm_error_is_valid(header%error)   .and. &
                 header%size >= 0                  .and. &
                 dm_ipc_message_type_is_valid(header%type))
    end function dm_ipc_message_header_is_valid

    pure elemental logical function dm_ipc_message_type_is_valid(type) result(valid)
        integer, intent(in) :: type !! IPC message type (`IPC_MESSAGE_TYPE_*`).

        valid = (type >= IPC_MESSAGE_TYPE_NONE .and. type <= IPC_MESSAGE_TYPE_LAST)
    end function dm_ipc_message_type_is_valid

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_ipc_message_header_out(header, unit)
        !! Prints IPC message header to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(ipc_message_header_type), intent(inout)        :: header !! IPC message header.
        integer,                       intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("ipc_message_header.magic: ", i0)') header%magic
        write (unit_, '("ipc_message_header.id: ", a)')     trim(header%id)
        write (unit_, '("ipc_message_header.from: ", a)')   trim(header%from)
        write (unit_, '("ipc_message_header.to: ", a)')     trim(header%to)
        write (unit_, '("ipc_message_header.error: ", i0)') header%error
        write (unit_, '("ipc_message_header.size: ", i0)')  header%size
        write (unit_, '("ipc_message_header.type: ", i0)')  header%type
    end subroutine dm_ipc_message_header_out

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function ipc_message_body_log(message, log) result(rc)
        use :: dm_log

        type(ipc_message_type), intent(inout) :: message !! IPC message.
        type(log_type),         intent(out)   :: log     !! Log.

        rc = E_NONE
    end function ipc_message_body_log

    integer function ipc_message_body_observ(message, observ) result(rc)
        use :: dm_observ

        type(ipc_message_type), intent(inout) :: message !! IPC message.
        type(observ_type),      intent(out)   :: observ  !! Observation.

        rc = E_NONE
    end function ipc_message_body_observ
end module dm_ipc_message
