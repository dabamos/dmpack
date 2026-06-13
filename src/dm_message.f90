! Author:  Philipp Engel
! Licence: ISC
module dm_message
    !! Message header type for message passing.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_type
    use :: dm_util
    use :: dm_uuid
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PUBLIC DERIVED TYPES
    ! **************************************************************************
    type, public :: message_header_type
        !! Message header.
        sequence
        character(UUID_LEN) :: id    = UUID_NONE !! Message id (UUIDv4).
        character(ID_LEN)   :: from  = ' '       !! Name of sender (`-0-9A-Z_a-z`).
        character(ID_LEN)   :: to    = ' '       !! Name of receiver (`-0-9A-Z_a-z`).
        integer(i4)         :: type  = TYPE_NONE !! Payload type (`TYPE_*`).
        integer(i4)         :: error = E_NONE    !! Error code (optional).
    end type message_header_type

    integer, parameter, public :: HEADER_TYPE_SIZE = storage_size(message_header_type()) / 8 !! Size of `message_header_type` [byte].

    ! **************************************************************************
    ! PUBLIC OPERATORS
    ! **************************************************************************
    public :: operator (==)

    interface operator (==)
        !! Returns `.true.` if headers are equal.
        module procedure :: dm_message_header_equals
    end interface

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_message_header
    public :: dm_message_header_beat
    public :: dm_message_header_dp
    public :: dm_message_header_log
    public :: dm_message_header_node
    public :: dm_message_header_observ
    public :: dm_message_header_sensor
    public :: dm_message_header_target

    public :: dm_message_header_equals
    public :: dm_message_header_is_valid
    public :: dm_message_header_is_valid_name
    public :: dm_message_header_is_valid_type
    public :: dm_message_header_out
    public :: dm_message_header_reset
contains
    ! **************************************************************************
    ! PUBLIC HEADER PROCEDURES
    ! **************************************************************************
    subroutine dm_message_header(header, id, from, to, type, error)
        type(message_header_type), intent(out)          :: header !! Message header.
        character(*),              intent(in), optional :: id     !! Message id.
        character(*),              intent(in), optional :: from   !! Sender id.
        character(*),              intent(in), optional :: to     !! Receiver id.
        integer,                   intent(in), optional :: type   !! Message type (`TYPE_*`).
        integer,                   intent(in), optional :: error  !! DMPACK error code.

        if (present(id)) then
            header%id = id
        else
            header%id = dm_uuid_new()
        end if

        if (present(from))  header%from  = from
        if (present(to))    header%to    = to
        if (present(type))  header%type  = type
        if (present(error)) header%error = error
    end subroutine dm_message_header

    function dm_message_header_beat(from, to, error) result(header)
        !! Returns beat header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(message_header_type)          :: header !! Message header.

        call dm_message_header(header, from=from, to=to, type=TYPE_BEAT, error=error)
    end function dm_message_header_beat

    function dm_message_header_dp(from, to, error) result(header)
        !! Returns data point header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(message_header_type)          :: header !! Message header.

        call dm_message_header(header, from=from, to=to, type=TYPE_DP, error=error)
    end function dm_message_header_dp

    function dm_message_header_log(from, to, error) result(header)
        !! Returns log header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(message_header_type)          :: header !! Message header.

        call dm_message_header(header, from=from, to=to, type=TYPE_LOG, error=error)
    end function dm_message_header_log

    function dm_message_header_node(from, to, error) result(header)
        !! Returns node header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(message_header_type)          :: header !! Message header.

        call dm_message_header(header, from=from, to=to, type=TYPE_NODE, error=error)
    end function dm_message_header_node

    function dm_message_header_observ(from, to, error) result(header)
        !! Returns observation header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(message_header_type)          :: header !! Message header.

        call dm_message_header(header, from=from, to=to, type=TYPE_OBSERV, error=error)
    end function dm_message_header_observ

    function dm_message_header_sensor(from, to, error) result(header)
        !! Returns sensor header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(message_header_type)          :: header !! Message header.

        call dm_message_header(header, from=from, to=to, type=TYPE_SENSOR, error=error)
    end function dm_message_header_sensor

    function dm_message_header_target(from, to, error) result(header)
        !! Returns target header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(message_header_type)          :: header !! Message header.

        call dm_message_header(header, from=from, to=to, type=TYPE_TARGET, error=error)
    end function dm_message_header_target

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
                  header1%error == header2%error)
    end function dm_message_header_equals

    pure elemental logical function dm_message_header_is_valid(header) result(valid)
        type(message_header_type), intent(in) :: header !! Message header.

        valid = (dm_uuid_is_valid(header%id)                  .and. &
                 dm_message_header_is_valid_name(header%from) .and. &
                 dm_message_header_is_valid_name(header%to)   .and. &
                 dm_message_header_is_valid_type(header%type) .and. &
                 dm_error_is_valid(header%error))
    end function dm_message_header_is_valid

    pure elemental logical function dm_message_header_is_valid_name(name) result(valid)
        character(*), intent(in) :: name !! Sender or receiver name.

        valid = .true.
        if (len_trim(name) == 0 .or. name == '*') return
        valid = dm_id_is_valid(name)
    end function dm_message_header_is_valid_name

    pure elemental logical function dm_message_header_is_valid_type(type) result(valid)
        integer, intent(in) :: type !! Type enumerator (`TYPE_*`).

        valid = (type >= TYPE_NONE .and. type <= TYPE_LAST)
    end function dm_message_header_is_valid_type

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
        write (unit_, '("header.error: ", i0)') header%error
    end subroutine dm_message_header_out

    pure elemental subroutine dm_message_header_reset(header)
        type(message_header_type), intent(inout) :: header !! Message header.

        header = message_header_type()
    end subroutine dm_message_header_reset
end module dm_message
