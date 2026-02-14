! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_type
    !! IPC type enumerators.
    implicit none (type, external)
    private

    integer, parameter, public :: IPC_URL_LEN = 256                   !! Max. URL string length.

    ! IPC async task states.
    integer, parameter, public :: IPC_ASYNC_TASK_STATE_INIT = 0       !! Initial state.
    integer, parameter, public :: IPC_ASYNC_TASK_STATE_RECV = 1       !! Receive message.
    integer, parameter, public :: IPC_ASYNC_TASK_STATE_WAIT = 2       !! Wait.
    integer, parameter, public :: IPC_ASYNC_TASK_STATE_WORK = 3       !! Work.
    integer, parameter, public :: IPC_ASYNC_TASK_STATE_SEND = 4       !! Send message.
    integer, parameter, public :: IPC_ASYNC_TASK_STATE_LAST = 4       !! Never use this.

    ! IPC message types.
    integer, parameter, public :: IPC_MESSAGE_TYPE_NONE           = 0 !! No message body.
    integer, parameter, public :: IPC_MESSAGE_TYPE_DISCO_REQUEST  = 1 !! Discovery request.
    integer, parameter, public :: IPC_MESSAGE_TYPE_DISCO_RESPONSE = 2 !! Discovery response.
    integer, parameter, public :: IPC_MESSAGE_TYPE_IMAGE          = 3 !! Image payload.
    integer, parameter, public :: IPC_MESSAGE_TYPE_LOG            = 4 !! Log payload.
    integer, parameter, public :: IPC_MESSAGE_TYPE_OBSERV         = 5 !! Observation payload.
    integer, parameter, public :: IPC_MESSAGE_TYPE_LAST           = 5 !! Never use this.

    ! IPC ports (TCP) used by DMPACK.
    integer, parameter, public :: IPC_PORT_DISCO           = 5100
    integer, parameter, public :: IPC_PORT_RPC_BLOB        = 5110
    integer, parameter, public :: IPC_PORT_RPC_IMAGE       = 5111
    integer, parameter, public :: IPC_PORT_RPC_LOG         = 5112
    integer, parameter, public :: IPC_PORT_RPC_OBSERV      = 5113
    integer, parameter, public :: IPC_PORT_PIPELINE_BLOB   = 5120
    integer, parameter, public :: IPC_PORT_PIPELINE_IMAGE  = 5121
    integer, parameter, public :: IPC_PORT_PIPELINE_LOG    = 5122
    integer, parameter, public :: IPC_PORT_PIPELINE_OBSERV = 5123
    integer, parameter, public :: IPC_PORT_QUEUE_BLOB      = 5130
    integer, parameter, public :: IPC_PORT_QUEUE_IMAGE     = 5131
    integer, parameter, public :: IPC_PORT_QUEUE_LOG       = 5132
    integer, parameter, public :: IPC_PORT_QUEUE_OBSERV    = 5133

    ! IPC protocols supported by NNG.
    integer, parameter, public :: IPC_PROTOCOL_ANY        =  0        !! No or invalid protocol.
    integer, parameter, public :: IPC_PROTOCOL_NONE       =  0        !! No or invalid protocol.
    integer, parameter, public :: IPC_PROTOCOL_BUS        =  1        !! Bus protocol.
    integer, parameter, public :: IPC_PROTOCOL_PAIR       =  2        !! Pair protocol.
    integer, parameter, public :: IPC_PROTOCOL_PUB        =  3        !! Pub protocol.
    integer, parameter, public :: IPC_PROTOCOL_PULL       =  4        !! Pull protocol.
    integer, parameter, public :: IPC_PROTOCOL_PUSH       =  5        !! Push protocol.
    integer, parameter, public :: IPC_PROTOCOL_REPLY      =  6        !! Reply protocol.
    integer, parameter, public :: IPC_PROTOCOL_REQUEST    =  7        !! Request protocol.
    integer, parameter, public :: IPC_PROTOCOL_RESPONDENT =  8        !! Respondent protocol.
    integer, parameter, public :: IPC_PROTOCOL_SUB        =  9        !! Subscriber protocol.
    integer, parameter, public :: IPC_PROTOCOL_SURVEYOR   = 10        !! Surveyor protocol.
    integer, parameter, public :: IPC_PROTOCOL_LAST       = 10        !! Never use this.

    ! IPC transports supported by NNG.
    integer, parameter, public :: IPC_TRANSPORT_ANY    = 0            !! No or invalid transport.
    integer, parameter, public :: IPC_TRANSPORT_NONE   = 0            !! No or invalid transport.
    integer, parameter, public :: IPC_TRANSPORT_INPROC = 1            !! Intra-process transport.
    integer, parameter, public :: IPC_TRANSPORT_IPC    = 2            !! Inter-process transport (UNIX domain socket).
    integer, parameter, public :: IPC_TRANSPORT_SOCKET = 3            !! BSD socket transport (experimental).
    integer, parameter, public :: IPC_TRANSPORT_TCP    = 4            !! TCP transport.
    integer, parameter, public :: IPC_TRANSPORT_TLS    = 5            !! TLS over TCP transport.
    integer, parameter, public :: IPC_TRANSPORT_WS     = 6            !! WebSocket transport.
    integer, parameter, public :: IPC_TRANSPORT_LAST   = 6            !! Never use this.

    ! IPC services.
    integer, parameter, public :: IPC_SERVICE_NONE            =  0    !! No or invalid service.
    integer, parameter, public :: IPC_SERVICE_DISCO           =  1    !! Discovery service.
    integer, parameter, public :: IPC_SERVICE_RPC_BLOB        =  2    !! Sending of BLOBs (Request/Response).
    integer, parameter, public :: IPC_SERVICE_RPC_IMAGE       =  3    !! Sending of images (Request/Response).
    integer, parameter, public :: IPC_SERVICE_RPC_LOG         =  4    !! Sending of logs (Request/Response).
    integer, parameter, public :: IPC_SERVICE_RPC_OBSERV      =  5    !! Sending of observations (Request/Response).
    integer, parameter, public :: IPC_SERVICE_PIPELINE_BLOB   =  6    !! Sending of BLOBs.
    integer, parameter, public :: IPC_SERVICE_PIPELINE_IMAGE  =  7    !! Sending of images.
    integer, parameter, public :: IPC_SERVICE_PIPELINE_LOG    =  8    !! Sending of logs.
    integer, parameter, public :: IPC_SERVICE_PIPELINE_OBSERV =  9    !! Sending of observations.
    integer, parameter, public :: IPC_SERVICE_QUEUE_BLOB      = 10    !! Subscription of BLOBs.
    integer, parameter, public :: IPC_SERVICE_QUEUE_IMAGE     = 11    !! Subscription of images.
    integer, parameter, public :: IPC_SERVICE_QUEUE_LOG       = 12    !! Subscription of logs.
    integer, parameter, public :: IPC_SERVICE_QUEUE_OBSERV    = 13    !! Subscription of observations.
    integer, parameter, public :: IPC_SERVICE_LAST            = 13    !! Never use this.

    ! IPC service status.
    integer, parameter, public :: IPC_STATUS_NONE        =  0         !! No or invalid status.
    integer, parameter, public :: IPC_STATUS_OK          =  1         !! Service is available.
    integer, parameter, public :: IPC_STATUS_UNAVAILABLE =  2         !! Service is not available.
    integer, parameter, public :: IPC_STATUS_UNKNOWN     =  3         !! Service does not exist.
    integer, parameter, public :: IPC_STATUS_LAST        =  3         !! Never use this.

    public :: dm_ipc_async_task_state_is_valid
    public :: dm_ipc_message_type_is_valid
    public :: dm_ipc_protocol_is_valid
    public :: dm_ipc_service_is_valid
    public :: dm_ipc_status_is_valid
    public :: dm_ipc_transport_is_valid
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental logical function dm_ipc_async_task_state_is_valid(state) result(valid)
        !! Returns `.true.` if given state is valid.
        integer, intent(in) :: state !! IPC async task state enumerator.

        valid = (state >= IPC_ASYNC_TASK_STATE_INIT .and. state <= IPC_ASYNC_TASK_STATE_LAST)
    end function dm_ipc_async_task_state_is_valid

    pure elemental logical function dm_ipc_message_type_is_valid(type) result(valid)
        !! Returns `.true.` if given message is valid. `IPC_TRANSPORT_NONE`
        !! is a valid message.
        integer, intent(in) :: type !! IPC message type enumerator.

        valid = (type>= IPC_MESSAGE_TYPE_NONE .and. type <= IPC_MESSAGE_TYPE_LAST)
    end function dm_ipc_message_type_is_valid

    pure elemental logical function dm_ipc_protocol_is_valid(protocol) result(valid)
        !! Returns `.true.` if given protocol is valid. `IPC_PROTOCOL_NONE` is
        !! a valid protocol.
        integer, intent(in) :: protocol !! IPC protocol enumerator.

        valid = (protocol >= IPC_PROTOCOL_NONE .and. protocol <= IPC_PROTOCOL_LAST)
    end function dm_ipc_protocol_is_valid

    pure elemental logical function dm_ipc_service_is_valid(service) result(valid)
        !! Returns `.true.` if given service is valid. `IPC_SERVICE_NONE` is
        !! a valid service.
        integer, intent(in) :: service !! IPC service enumerator.

        valid = (service >= IPC_SERVICE_NONE .and. service <= IPC_SERVICE_LAST)
    end function dm_ipc_service_is_valid

    pure elemental logical function dm_ipc_status_is_valid(status) result(valid)
        !! Returns `.true.` if given status is valid. `IPC_STATUS_NONE` is a
        !! valid service status.
        integer, intent(in) :: status !! IPC status enumerator.

        valid = (status >= IPC_STATUS_NONE .and. status <= IPC_STATUS_LAST)
    end function dm_ipc_status_is_valid

    pure elemental logical function dm_ipc_transport_is_valid(transport) result(valid)
        !! Returns `.true.` if given transport is valid. `IPC_TRANSPORT_NONE`
        !! is a valid transport.
        integer, intent(in) :: transport !! IPC transport enumerator.

        valid = (transport >= IPC_TRANSPORT_NONE .and. transport <= IPC_TRANSPORT_LAST)
    end function dm_ipc_transport_is_valid
end module dm_ipc_type
