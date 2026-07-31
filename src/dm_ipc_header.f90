! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_header
    !! Message header type for IPC.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_type
    use :: dm_uuid
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PUBLIC DERIVED TYPES
    ! **************************************************************************
    type, public :: ipc_header_type
        !! IPC header.
        character(UUID_LEN) :: id    = UUID_NONE !! Unique message id (UUIDv4).
        character(ID_LEN)   :: from  = ' '       !! Name of sender (`-0-9A-Z_a-z`).
        character(ID_LEN)   :: to    = ' '       !! Name of receiver (`-0-9A-Z_a-z`).
        integer(i4)         :: type  = TYPE_NONE !! Payload type (`TYPE_*`).
        integer(i4)         :: error = E_NONE    !! Error code (optional).
    end type ipc_header_type

    integer, parameter, public :: IPC_HEADER_TYPE_SIZE = storage_size(ipc_header_type()) / 8 !! Size of `ipc_header_type` [byte].

    ! **************************************************************************
    ! PUBLIC OPERATORS
    ! **************************************************************************
    public :: operator (==)

    interface operator (==)
        !! Returns `.true.` if headers are equal.
        module procedure :: dm_ipc_header_equals
    end interface

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_ipc_header_beat
    public :: dm_ipc_header_dp
    public :: dm_ipc_header_log
    public :: dm_ipc_header_node
    public :: dm_ipc_header_observ
    public :: dm_ipc_header_sensor
    public :: dm_ipc_header_target

    public :: dm_ipc_header_init

    public :: dm_ipc_header_equals
    public :: dm_ipc_header_is_valid
    public :: dm_ipc_header_is_valid_name
    public :: dm_ipc_header_is_valid_type
    public :: dm_ipc_header_out
    public :: dm_ipc_header_reset
contains
    ! **************************************************************************
    ! PUBLIC HEADER PROCEDURES
    ! **************************************************************************
    function dm_ipc_header_beat(from, to, error) result(header)
        !! Returns beat header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(ipc_header_type)              :: header !! IPC header.

        call dm_ipc_header_init(header, from=from, to=to, type=TYPE_BEAT, error=error)
    end function dm_ipc_header_beat

    function dm_ipc_header_dp(from, to, error) result(header)
        !! Returns data point header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(ipc_header_type)              :: header !! IPC header.

        call dm_ipc_header_init(header, from=from, to=to, type=TYPE_DP, error=error)
    end function dm_ipc_header_dp

    function dm_ipc_header_log(from, to, error) result(header)
        !! Returns log header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(ipc_header_type)              :: header !! IPC header.

        call dm_ipc_header_init(header, from=from, to=to, type=TYPE_LOG, error=error)
    end function dm_ipc_header_log

    function dm_ipc_header_node(from, to, error) result(header)
        !! Returns node header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(ipc_header_type)              :: header !! IPC header.

        call dm_ipc_header_init(header, from=from, to=to, type=TYPE_NODE, error=error)
    end function dm_ipc_header_node

    function dm_ipc_header_observ(from, to, error) result(header)
        !! Returns observation header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(ipc_header_type)              :: header !! IPC header.

        call dm_ipc_header_init(header, from=from, to=to, type=TYPE_OBSERV, error=error)
    end function dm_ipc_header_observ

    function dm_ipc_header_sensor(from, to, error) result(header)
        !! Returns sensor header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(ipc_header_type)              :: header !! IPC header.

        call dm_ipc_header_init(header, from=from, to=to, type=TYPE_SENSOR, error=error)
    end function dm_ipc_header_sensor

    function dm_ipc_header_target(from, to, error) result(header)
        !! Returns target header.
        character(*), intent(in), optional :: from   !! Sender id.
        character(*), intent(in), optional :: to     !! Receiver id.
        integer,      intent(in), optional :: error  !! DMPACK error code.
        type(ipc_header_type)              :: header !! IPC header.

        call dm_ipc_header_init(header, from=from, to=to, type=TYPE_TARGET, error=error)
    end function dm_ipc_header_target

    subroutine dm_ipc_header_init(header, id, from, to, type, error)
        !! Returns header of given type. If `id` is not passed, a unique UUIDv4
        !! will be generated. The subroutine does not validate the arguments.
        type(ipc_header_type), intent(out)          :: header !! IPC header.
        character(*),          intent(in), optional :: id     !! Message id.
        character(*),          intent(in), optional :: from   !! Sender id.
        character(*),          intent(in), optional :: to     !! Receiver id.
        integer,               intent(in), optional :: type   !! Message type (`TYPE_*`).
        integer,               intent(in), optional :: error  !! DMPACK error code.

        if (present(id)) then
            header%id = id
        else
            header%id = dm_uuid_new()
        end if

        if (present(from))  header%from  = from
        if (present(to))    header%to    = to
        if (present(type))  header%type  = type
        if (present(error)) header%error = error
    end subroutine dm_ipc_header_init

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    pure elemental logical function dm_ipc_header_equals(header1, header2) result(equals)
        !! Returns `.true.` if given headers are equal.
        type(ipc_header_type), intent(in) :: header1 !! The first header.
        type(ipc_header_type), intent(in) :: header2 !! The second header.

        equals = (header1%id    == header2%id   .and. &
                  header1%from  == header2%from .and. &
                  header1%to    == header2%to   .and. &
                  header1%type  == header2%type .and. &
                  header1%error == header2%error)
    end function dm_ipc_header_equals

    pure elemental logical function dm_ipc_header_is_valid(header) result(valid)
        type(ipc_header_type), intent(in) :: header !! IPC header.

        valid = (dm_uuid_is_valid(header%id)              .and. &
                 dm_ipc_header_is_valid_name(header%from) .and. &
                 dm_ipc_header_is_valid_name(header%to)   .and. &
                 dm_ipc_header_is_valid_type(header%type) .and. &
                 dm_error_is_valid(header%error))
    end function dm_ipc_header_is_valid

    pure elemental logical function dm_ipc_header_is_valid_name(name) result(valid)
        !! Returns `.true.` if name is a valid sender or receiver name. Empty
        !! strings, character `*`, and ids are valid.
        character(*), intent(in) :: name !! Sender or receiver name.

        valid = (len_trim(name) == 0 .or. name == '*' .or. dm_id_is_valid(name))
    end function dm_ipc_header_is_valid_name

    pure elemental logical function dm_ipc_header_is_valid_type(type) result(valid)
        !! Returns `.true.` if header type is valid. `TYPE_NONE` is a valid type.
        integer, intent(in) :: type !! Type enumerator (`TYPE_*`).

        valid = (type >= TYPE_NONE .and. type <= TYPE_LAST)
    end function dm_ipc_header_is_valid_type

    subroutine dm_ipc_header_out(header, unit)
        !! Prints message header to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(ipc_header_type), intent(in)           :: header !! IPC header.
        integer,               intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("ipc_header.id: ", a)')     trim(header%id)
        write (unit_, '("ipc_header.from: ", a)')   trim(header%from)
        write (unit_, '("ipc_header.to: ", a)')     trim(header%to)
        write (unit_, '("ipc_header.type: ", i0)')  header%type
        write (unit_, '("ipc_header.error: ", i0)') header%error
    end subroutine dm_ipc_header_out

    pure elemental subroutine dm_ipc_header_reset(header)
        !! Resets the header to default values.
        type(ipc_header_type), intent(inout) :: header !! IPC header.

        header = ipc_header_type()
    end subroutine dm_ipc_header_reset
end module dm_ipc_header
