! Author:  Philipp Engel
! Licence: ISC
module dm_message
    !! Message header type for message passing (ZeroMQ + MessagePack).
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_type
    use :: dm_util
    use :: dm_uuid
    implicit none (type, external)
    private

    type, public :: message_header_type
        !! Message header.
        sequence
        character(UUID_LEN) :: id    = UUID_NONE !! Message id (UUIDv4).
        character(ID_LEN)   :: from  = ' '       !! Name of sender (`-0-9A-Z_a-z`).
        character(ID_LEN)   :: to    = ' '       !! Name of receiver (`-0-9A-Z_a-z`).
        integer(i4)         :: type  = TYPE_NONE !! Payload type (`TYPE_*`).
        integer(i4)         :: size  = 0         !! Payload size [byte].
        integer(i4)         :: error = E_NONE    !! Error code (optional).
    end type message_header_type

    integer, parameter, public :: HEADER_TYPE_SIZE = storage_size(message_header_type()) / 8 !! Size of `message_header_type` [byte].

    public :: operator (==)

    interface operator (==)
        !! Returns `.true.` if headers are equal.
        module procedure :: dm_message_header_equals
    end interface

    public :: dm_message_header_equals
    public :: dm_message_header_init
    public :: dm_message_header_is_valid
    public :: dm_message_header_out
    public :: dm_message_header_reset
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    pure elemental logical function dm_message_header_equals(header1, header2) result(equals)
        !! Returns `.true.` if given headers are equal.
        type(message_header_type), intent(in) :: header1 !! The first header.
        type(message_header_type), intent(in) :: header2 !! The second header.

        equals = (header1%id    == header2%id   .and. &
                  header1%from  == header2%from .and. &
                  header1%to    == header2%to   .and. &
                  header1%type  == header2%type .and. &
                  header1%size  == header2%size .and. &
                  header1%error == header2%error)
    end function dm_message_header_equals

    pure elemental logical function dm_message_header_is_valid(header) result(valid)
        type(message_header_type), intent(in) :: header !! Message header.

        valid = (dm_uuid_is_valid(header%id)                                   .and. &
                 (len_trim(header%from) == 0 .or. dm_id_is_valid(header%from)) .and. &
                 (len_trim(header%to)   == 0 .or. dm_id_is_valid(header%to))   .and. &
                 (header%type >= TYPE_NONE .and. header%type <= TYPE_LAST)     .and. &
                 header%size >= 0                                              .and. &
                 dm_error_is_valid(header%error))
    end function dm_message_header_is_valid

    subroutine dm_message_header_init(header, id, from, to, type, size, error)
        type(message_header_type), intent(out)          :: header !! Message header.
        character(*),              intent(in), optional :: id     !! Message id.
        character(*),              intent(in), optional :: from   !! Sender id.
        character(*),              intent(in), optional :: to     !! Receiver id.
        integer,                   intent(in), optional :: type   !! Message type.
        integer,                   intent(in), optional :: size   !! Message payload size [byte].
        integer,                   intent(in), optional :: error  !! DMPACK error code.

        if (present(id)) then
            header%id = id
        else
            header%id = dm_uuid_new()
        end if

        if (present(from))  header%from  = from
        if (present(to))    header%to    = to
        if (present(type))  header%type  = type
        if (present(size))  header%size  = size
        if (present(error)) header%error = error
    end subroutine dm_message_header_init

    subroutine dm_message_header_out(header, unit)
        !! Prints message header to standard output or given file unit.
        type(message_header_type), intent(in)           :: header !! Message header.
        integer,                   intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("header.id: ", a)')     trim(header%id)
        write (unit_, '("header.from: ", a)')   trim(header%from)
        write (unit_, '("header.to: ", a)')     trim(header%to)
        write (unit_, '("header.type: ", i0)')  header%type
        write (unit_, '("header.size: ", i0)')  header%size
        write (unit_, '("header.error: ", i0)') header%error
    end subroutine dm_message_header_out

    pure elemental subroutine dm_message_header_reset(header)
        type(message_header_type), intent(inout) :: header !! Message header.

        header = message_header_type()
    end subroutine dm_message_header_reset
end module dm_message
