! Author:  Philipp Engel
! Licence: ISC
module dm_transfer
    !! Transfer type module for data upload from client to server. The transfer
    !! derived type is stored on server-side only.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_mime
    use :: dm_net
    use :: dm_node
    use :: dm_time
    use :: dm_uuid
    implicit none (type, external)
    private

    integer, parameter, public :: TRANSFER_TYPE_NONE  = 0 !! No type (invalid).
    integer, parameter, public :: TRANSFER_TYPE_BLOB  = 1 !! Arbitrary binary object.
    integer, parameter, public :: TRANSFER_TYPE_IMAGE = 2 !! Image type (`image_type` from `dm_image`).
    integer, parameter, public :: TRANSFER_TYPE_LAST  = 2 !! Never use this.

    integer, parameter, public :: TRANSFER_STATE_NONE    = 0 !! Unprepared transfer (invalid).
    integer, parameter, public :: TRANSFER_STATE_CREATED = 1 !! Transfer is initialised.
    integer, parameter, public :: TRANSFER_STATE_ACTIVE  = 2 !! Transfer is running.
    integer, parameter, public :: TRANSFER_STATE_FAILED  = 3 !! Transfer failed.
    integer, parameter, public :: TRANSFER_STATE_DONE    = 4 !! Transfer finished.
    integer, parameter, public :: TRANSFER_STATE_LAST    = 4 !! Never use this.

    type, public :: transfer_type
        !! Transfer type for data upload from client to server. The attribute
        !! `type_id` is the id of the transfered object, for example, an image
        !! id. The derived type is invalid by default. The attributes `type`,
        !! `id`, `node_id`, `type_id`, `state`, and `size` have to be set
        !! initially.
        integer                     :: type      = TRANSFER_TYPE_NONE  !! Transfer type.
        character(len=NODE_ID_LEN)  :: node_id   = ' '                 !! Node id.
        character(len=UUID_LEN)     :: type_id   = ' '                 !! Transfer object id (UUIDv4).
        character(len=UUID_LEN)     :: id        = ' '                 !! Transfer id (UUIDv4).
        character(len=TIME_LEN)     :: timestamp = TIME_DEFAULT        !! Timestamp of current state (ISO 8601).
        character(len=NET_IPV6_LEN) :: address   = ' '                 !! Client IP address (IPv4, IPv6).
        integer                     :: error     = E_NONE              !! Error code.
        integer                     :: state     = TRANSFER_STATE_NONE !! Transfer state.
        integer(kind=i8)            :: size      = 0_i8                !! File size [byte]
    end type transfer_type

    public :: dm_transfer_create
    public :: dm_transfer_set
    public :: dm_transfer_is_valid
    public :: dm_transfer_state_is_valid
    public :: dm_transfer_type_is_valid
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_transfer_create(transfer, type, node_id, type_id, size) result(rc)
        !! Creates and prepares transfer for given object id. The function
        !! returns `E_INVALID` if one of the arguments is invalid. Argument
        !! `node_id` must be a valid id, `type_id` must be a valid UUIDv4, and
        !! size greater than 0.
        type(transfer_type),     intent(out) :: transfer !! Transfer type.
        integer,                 intent(in)  :: type     !! Object type (`TRANSFER_TYPE_*`).
        character(len=*),        intent(in)  :: node_id  !! Node id.
        character(len=UUID_LEN), intent(in)  :: type_id  !! Object id.
        integer(kind=i8),        intent(in)  :: size     !! File size [byte].

        rc = E_INVALID

        if (.not. dm_transfer_type_is_valid(type) .or. &
            .not. dm_id_is_valid(node_id)         .or. &
            .not. dm_uuid4_is_valid(type_id)      .or. &
            size <= 0) return

        transfer%type      = type
        transfer%id        = dm_uuid4()
        transfer%node_id   = node_id
        transfer%type_id   = type_id
        transfer%timestamp = dm_time_now()
        transfer%state     = TRANSFER_STATE_CREATED
        transfer%size      = size

        rc = E_NONE
    end function dm_transfer_create

    pure elemental logical function dm_transfer_is_valid(transfer) result(valid)
        !! Returns `.true.` if transfer type is valid.
        use :: dm_string, only: dm_string_is_printable

        type(transfer_type), intent(in) :: transfer !! Transfer type.

        valid = (dm_transfer_type_is_valid(transfer%type)            .and. &
                 dm_id_is_valid(transfer%node_id)                    .and. &
                 dm_uuid4_is_valid(transfer%type_id)                 .and. &
                 dm_uuid4_is_valid(transfer%id)                      .and. &
                 dm_time_is_valid(transfer%timestamp, strict=.true.) .and. &
                 dm_string_is_printable(transfer%address)            .and. &
                 dm_error_is_valid(transfer%error)                   .and. &
                 dm_transfer_state_is_valid(transfer%state)          .and. &
                 transfer%size > 0)
    end function dm_transfer_is_valid

    pure elemental logical function dm_transfer_state_is_valid(state) result(valid)
        !! Returns `.true.` if transfer state is valid. Type
        !! `TRANSFER_STATE_NONE` is invalid.
        integer, intent(in) :: state !! Transfer state (`TRANSFER_STATE_*`).

        valid = (state > TRANSFER_STATE_NONE .and. state <= TRANSFER_STATE_LAST)
    end function dm_transfer_state_is_valid

    pure elemental logical function dm_transfer_type_is_valid(type) result(valid)
        !! Returns `.true.` if object type is valid. Type `TRANSFER_TYPE_NONE`
        !! is invalid.
        integer, intent(in) :: type !! Object type (`TRANSFER_TYPE_*`).

        valid = (type > TRANSFER_TYPE_NONE .and. type <= TRANSFER_TYPE_LAST)
    end function dm_transfer_type_is_valid

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    pure elemental subroutine dm_transfer_set(transfer, type, node_id, type_id, id, timestamp, address, error, state, size)
        !! Set transfer attributes. This routine does not validate the arguments.
        type(transfer_type),     intent(inout)        :: transfer  !! Transfer type.
        integer,                 intent(in), optional :: type      !! Object type (`TRANSFER_TYPE_*`).
        character(len=*),        intent(in), optional :: node_id   !! Node id.
        character(len=UUID_LEN), intent(in), optional :: type_id   !! Object id.
        character(len=UUID_LEN), intent(in), optional :: id        !! Transfer id.
        character(len=TIME_LEN), intent(in), optional :: timestamp !! Timestamp of current transfer state.
        character(len=*),        intent(in), optional :: address   !! Client IP address.
        integer,                 intent(in), optional :: error     !! Error code.
        integer,                 intent(in), optional :: state     !! Transfer state.
        integer(kind=i8),        intent(in), optional :: size      !! Object size [byte].

        if (present(type))      transfer%type      = type
        if (present(node_id))   transfer%node_id   = node_id
        if (present(type_id))   transfer%type_id   = type_id
        if (present(id))        transfer%id        = id
        if (present(timestamp)) transfer%timestamp = timestamp
        if (present(address))   transfer%address   = address
        if (present(error))     transfer%error     = error
        if (present(state))     transfer%state     = state
        if (present(size))      transfer%size      = size
    end subroutine dm_transfer_set
end module dm_transfer
