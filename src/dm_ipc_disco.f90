! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_disco
    !! IPC service discovery module.
    use :: dm_error
    use :: dm_ipc_type
    use :: dm_kind
    implicit none (type, external)
    private

    ! Request.
    type, public :: ipc_disco_request_type
        !! IPC discovery request.
        sequence
        integer :: protocol  = IPC_PROTOCOL_NONE  !! IPC protocol.
        integer :: transport = IPC_TRANSPORT_NONE !! IPC transport.
        integer :: service   = IPC_SERVICE_NONE   !! IPC service.
    end type ipc_disco_request_type

    integer, parameter, public :: IPC_DISCO_REQUEST_TYPE_SIZE = storage_size(ipc_disco_request_type()) / 8 !! Size of `ipc_disco_request_type` [byte].

    ! Response.
    integer, parameter, public :: IPC_DISCO_URL_LEN = 256 !! Max. URL string length.

    type, public :: ipc_disco_response_type
        !! IPC discovery response.
        sequence
        integer                      :: protocol  = IPC_PROTOCOL_NONE  !! IPC protocol.
        integer                      :: transport = IPC_TRANSPORT_NONE !! IPC transport.
        integer                      :: service   = IPC_SERVICE_NONE   !! IPC service.
        integer                      :: status    = IPC_STATUS_NONE    !! IPC status.
        character(IPC_DISCO_URL_LEN) :: url       = ' '                !! IPC service URL.
    end type ipc_disco_response_type

    integer, parameter, public :: IPC_DISCO_RESPONSE_TYPE_SIZE = storage_size(ipc_disco_response_type()) / 8 !! Size of `ipc_disco_response_type` [byte].

    interface dm_ipc_disco_out
        module procedure :: dm_ipc_disco_request_out
        module procedure :: dm_ipc_disco_response_out
    end interface dm_ipc_disco_out

    public :: dm_ipc_disco_out
    public :: dm_ipc_disco_request_is_valid
    public :: dm_ipc_disco_response_is_valid
    public :: dm_ipc_disco_request_out
    public :: dm_ipc_disco_response_out
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental logical function dm_ipc_disco_request_is_valid(request) result(valid)
        !! Returns `.true.` if given IPC disco request is valid.
        type(ipc_disco_request_type), intent(in) :: request !! IPC disco request.

        valid = (dm_ipc_protocol_is_valid(request%protocol)   .and. &
                 dm_ipc_transport_is_valid(request%transport) .and. &
                 dm_ipc_service_is_valid(request%service))
    end function dm_ipc_disco_request_is_valid

    pure elemental logical function dm_ipc_disco_response_is_valid(response) result(valid)
        !! Returns `.true.` if given IPC disco response is valid.
        use :: dm_string, only: dm_string_is_printable

        type(ipc_disco_response_type), intent(in) :: response !! IPC disco response.

        valid = (dm_ipc_protocol_is_valid(response%protocol)   .and. &
                 dm_ipc_transport_is_valid(response%transport) .and. &
                 dm_ipc_service_is_valid(response%service)     .and. &
                 dm_ipc_status_is_valid(response%status)       .and. &
                 dm_string_is_printable(response%url))
    end function dm_ipc_disco_response_is_valid

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_ipc_disco_request_out(request, unit)
        !! Prints IPC disco request to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(ipc_disco_request_type), intent(inout)        :: request !! IPC disco request.
        integer,                      intent(in), optional :: unit    !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("ipc_disco_request.protocol: ", i0)')  request%protocol
        write (unit_, '("ipc_disco_request.transport: ", i0)') request%transport
        write (unit_, '("ipc_disco_request.service: ", i0)')   request%service
    end subroutine dm_ipc_disco_request_out

    subroutine dm_ipc_disco_response_out(response, unit)
        !! Prints IPC disco response to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(ipc_disco_response_type), intent(inout)        :: response !! IPC disco response.
        integer,                       intent(in), optional :: unit     !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("ipc_disco_response.protocol: ", i0)')  response%protocol
        write (unit_, '("ipc_disco_response.transport: ", i0)') response%transport
        write (unit_, '("ipc_disco_response.service: ", i0)')   response%service
        write (unit_, '("ipc_disco_response.status: ", i0)')    response%status
        write (unit_, '("ipc_disco_response.url: ", a)')        trim(response%url)
    end subroutine dm_ipc_disco_response_out
end module dm_ipc_disco
