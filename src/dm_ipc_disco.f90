! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_disco
    !! IPC service discovery module.
    use :: dm_c
    use :: dm_error
    use :: dm_ipc
    use :: dm_ipc_message
    use :: dm_ipc_type
    use :: dm_kind
    implicit none (type, external)
    private

    type, public :: ipc_disco_request_type
        !! IPC discovery request.
        sequence
        integer :: service   = IPC_SERVICE_NONE   !! IPC service.
        integer :: transport = IPC_TRANSPORT_NONE !! IPC transport.
    end type ipc_disco_request_type

    integer, parameter, public :: IPC_DISCO_REQUEST_TYPE_SIZE = storage_size(ipc_disco_request_type()) / 8 !! Size of `ipc_disco_request_type` [byte].

    type, public :: ipc_disco_response_type
        !! IPC discovery response.
        sequence
        integer                :: service   = IPC_SERVICE_NONE   !! IPC service.
        integer                :: transport = IPC_TRANSPORT_NONE !! IPC transport.
        integer                :: protocol  = IPC_PROTOCOL_NONE  !! IPC protocol.
        integer                :: status    = IPC_STATUS_NONE    !! IPC status.
        character(IPC_URL_LEN) :: url       = ' '                !! IPC service URL.
    end type ipc_disco_response_type

    integer, parameter, public :: IPC_DISCO_RESPONSE_TYPE_SIZE = storage_size(ipc_disco_response_type()) / 8 !! Size of `ipc_disco_response_type` [byte].

    ! Public interfaces.
    interface dm_ipc_disco_from_message
        module procedure :: dm_ipc_disco_request_from_message
        module procedure :: dm_ipc_disco_response_from_message
    end interface dm_ipc_disco_from_message

    interface dm_ipc_disco_out
        module procedure :: dm_ipc_disco_request_out
        module procedure :: dm_ipc_disco_response_out
    end interface dm_ipc_disco_out

    interface dm_ipc_disco_send
        module procedure :: ipc_disco_request_send_socket
        module procedure :: ipc_disco_request_send_url
    end interface dm_ipc_disco_send

    interface dm_ipc_disco_to_message
        module procedure :: dm_ipc_disco_request_to_message
        module procedure :: dm_ipc_disco_response_to_message
    end interface dm_ipc_disco_to_message

    public :: dm_ipc_disco_from_message
    public :: dm_ipc_disco_out
    public :: dm_ipc_disco_send
    public :: dm_ipc_disco_to_message

    ! Public procedures.
    public :: dm_ipc_disco_reply
    public :: dm_ipc_disco_request_from_message
    public :: dm_ipc_disco_request_is_valid
    public :: dm_ipc_disco_request_out
    public :: dm_ipc_disco_request_to_message
    public :: dm_ipc_disco_response_from_message
    public :: dm_ipc_disco_response_is_valid
    public :: dm_ipc_disco_response_out
    public :: dm_ipc_disco_response_to_message

    ! Private procedures.
    private :: ipc_disco_request_body
    private :: ipc_disco_response_body

    private :: ipc_disco_request_send_socket
    private :: ipc_disco_request_send_url
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ipc_disco_reply(message, header, response, error) result(rc)
        !! Creates reply message to IPC disco request message. On `error` is
        !! passed and an error code, an empty response message with header only
        !! is created.
        use :: dm_util, only: dm_present

        type(ipc_message_type),                intent(out)          :: message  !! IPC disco response message.
        type(ipc_message_header_type),         intent(in)           :: header   !! IPC message header of request.
        type(ipc_disco_response_type), target, intent(in)           :: response !! IPC disco response.
        integer,                               intent(in), optional :: error    !! Error code.

        integer :: error_

        error_ = dm_present(error, E_NONE)

        if (dm_is_error(error_)) then
            rc = dm_ipc_message_create(message, from=header%to, to=header%from, id=header%id, error=error_)
        else
            rc = dm_ipc_message_create(message, from=header%to, to=header%from, id=header%id, size=IPC_DISCO_RESPONSE_TYPE_SIZE, type=IPC_MESSAGE_TYPE_DISCO_RESPONSE)
            if (dm_is_error(rc)) return
            rc = dm_ipc_message_append(message, c_loc(response), IPC_DISCO_RESPONSE_TYPE_SIZE)
        end if
    end function dm_ipc_disco_reply

    integer function dm_ipc_disco_request_from_message(request, message, from, to) result(rc)
        !! Reads IPC disco request from IPC message. If `from` is passed,
        !! only messages for this id will be accepted. If `to` is passed, only
        !! message from this id will be accepted. Otherwise, the function
        !! returns `E_IGNORED`. The NNG message is destroyed automatically.
        type(ipc_disco_request_type),  intent(out)           :: request !! IPC disco request.
        type(ipc_message_type),        intent(inout)         :: message !! IPC message.
        character(*),                  intent(in),  optional :: from    !! Message sender.
        character(*),                  intent(in),  optional :: to      !! Message receiver.

        rc = E_IGNORED
        if (present(from)) then
            if (len_trim(from) > 0 .and. message%header%from /= from) return
        end if

        if (present(to)) then
            if (len_trim(to) > 0 .and. message%header%to /= to) return
        end if

        rc = ipc_disco_request_body(message, request)
        call dm_ipc_message_destroy(message, header=.false.)
    end function dm_ipc_disco_request_from_message

    pure elemental logical function dm_ipc_disco_request_is_valid(request) result(valid)
        !! Returns `.true.` if given IPC disco request is valid.
        type(ipc_disco_request_type), intent(in) :: request !! IPC disco request.

        valid = (dm_ipc_service_is_valid(request%service) .and. dm_ipc_transport_is_valid(request%transport))
    end function dm_ipc_disco_request_is_valid

    integer function dm_ipc_disco_request_to_message(message, request, from, to, id, error) result(rc)
        !! Creates IPC message from IPC disco request.
        type(ipc_message_type),               intent(out)          :: message !! IPC message.
        type(ipc_disco_request_type), target, intent(in)           :: request !! IPC disco request.
        character(*),                         intent(in)           :: from    !! Sender id.
        character(*),                         intent(in), optional :: to      !! Receiver id.
        character(*),                         intent(in), optional :: id      !! Message id.
        integer,                              intent(in), optional :: error   !! DMPACK error code.

        rc = dm_ipc_message_create(message, from=from, to=to, id=id, error=error, size=IPC_DISCO_REQUEST_TYPE_SIZE, type=IPC_MESSAGE_TYPE_DISCO_REQUEST)
        if (dm_is_error(rc)) return

        rc = dm_ipc_message_append(message, c_loc(request), IPC_DISCO_REQUEST_TYPE_SIZE)
    end function dm_ipc_disco_request_to_message

    integer function dm_ipc_disco_response_from_message(response, message, from, to) result(rc)
        !! Reads IPC disco response from IPC message. If `from` is passed,
        !! only messages for this id will be accepted. If `to` is passed, only
        !! message from this id will be accepted. Otherwise, the function
        !! returns `E_IGNORED`. The NNG message is destroyed automatically.
        type(ipc_disco_response_type), intent(out)           :: response !! IPC disco response.
        type(ipc_message_type),        intent(inout)         :: message  !! IPC message.
        character(*),                  intent(in),  optional :: from     !! Message sender.
        character(*),                  intent(in),  optional :: to       !! Message receiver.

        rc = E_IGNORED
        if (present(from)) then
            if (len_trim(from) > 0 .and. message%header%from /= from) return
        end if

        if (present(to)) then
            if (len_trim(to) > 0 .and. message%header%to /= to) return
        end if

        rc = ipc_disco_response_body(message, response)
        call dm_ipc_message_destroy(message, header=.false.)
    end function dm_ipc_disco_response_from_message

    pure elemental logical function dm_ipc_disco_response_is_valid(response) result(valid)
        !! Returns `.true.` if given IPC disco response is valid.
        use :: dm_string, only: dm_string_is_printable

        type(ipc_disco_response_type), intent(in) :: response !! IPC disco response.

        valid = (dm_ipc_service_is_valid(response%service)     .and. &
                 dm_ipc_transport_is_valid(response%transport) .and. &
                 dm_ipc_protocol_is_valid(response%protocol)   .and. &
                 dm_ipc_status_is_valid(response%status)       .and. &
                 dm_string_is_printable(response%url))
    end function dm_ipc_disco_response_is_valid

    integer function dm_ipc_disco_response_to_message(message, response, from, to, id, error) result(rc)
        !! Creates IPC message from IPC disco response.
        type(ipc_message_type),                intent(out)          :: message  !! IPC message.
        type(ipc_disco_response_type), target, intent(in)           :: response !! IPC disco response.
        character(*),                          intent(in)           :: from     !! Sender id.
        character(*),                          intent(in), optional :: to       !! Receiver id.
        character(*),                          intent(in), optional :: id       !! Message id.
        integer,                               intent(in), optional :: error    !! DMPACK error code.

        rc = dm_ipc_message_create(message, from=from, to=to, id=id, error=error, size=IPC_DISCO_RESPONSE_TYPE_SIZE, type=IPC_MESSAGE_TYPE_DISCO_RESPONSE)
        if (dm_is_error(rc)) return

        rc = dm_ipc_message_append(message, c_loc(response), IPC_DISCO_RESPONSE_TYPE_SIZE)
    end function dm_ipc_disco_response_to_message

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

        write (unit_, '("ipc_disco_request.service: ", i0)')   request%service
        write (unit_, '("ipc_disco_request.transport: ", i0)') request%transport
    end subroutine dm_ipc_disco_request_out

    subroutine dm_ipc_disco_response_out(response, unit)
        !! Prints IPC disco response to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(ipc_disco_response_type), intent(inout)        :: response !! IPC disco response.
        integer,                       intent(in), optional :: unit     !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("ipc_disco_response.service: ", i0)')   response%service
        write (unit_, '("ipc_disco_response.transport: ", i0)') response%transport
        write (unit_, '("ipc_disco_response.protocol: ", i0)')  response%protocol
        write (unit_, '("ipc_disco_response.status: ", i0)')    response%status
        write (unit_, '("ipc_disco_response.url: ", a)')        trim(response%url)
    end subroutine dm_ipc_disco_response_out

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function ipc_disco_request_body(message, request) result(rc)
        !! Returns IPC disco request from NNG message body in `request`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if payload size does not match type size.
        !! * `E_EMPTY` if NNG message body is empty.
        !! * `E_NULL` if NNG message body is not associated.
        !! * `E_TYPE` if message type is not supported.
        !!
        type(ipc_message_type),       intent(inout) :: message !! IPC message.
        type(ipc_disco_request_type), intent(out)   :: request !! IPC disco request.

        integer                               :: nbyte
        type(c_ptr)                           :: ptr
        type(ipc_disco_request_type), pointer :: request_ptr

        rc = E_TYPE
        if (message%header%type /= IPC_MESSAGE_TYPE_DISCO_REQUEST) return

        rc = E_CORRUPT
        if (message%header%size /= IPC_DISCO_REQUEST_TYPE_SIZE) return

        rc = dm_ipc_message_pointer(message, ptr)
        if (dm_is_error(rc)) return

        nbyte = dm_ipc_message_length(message)

        rc = E_EMPTY
        if (nbyte == 0) return

        rc = E_CORRUPT
        if (nbyte < IPC_DISCO_REQUEST_TYPE_SIZE) return

        rc = E_NONE
        call c_f_pointer(ptr, request_ptr)
        request = request_ptr
    end function ipc_disco_request_body

    integer function ipc_disco_response_body(message, response) result(rc)
        !! Returns IPC disco response from NNG message body in `response`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if payload size does not match type size.
        !! * `E_EMPTY` if NNG message body is empty.
        !! * `E_NULL` if NNG message body is not associated.
        !! * `E_TYPE` if message type is not supported.
        !!
        type(ipc_message_type),        intent(inout) :: message  !! IPC message.
        type(ipc_disco_response_type), intent(out)   :: response !! IPC disco response.

        integer                                :: nbyte
        type(c_ptr)                            :: ptr
        type(ipc_disco_response_type), pointer :: response_ptr

        rc = E_TYPE
        if (message%header%type /= IPC_MESSAGE_TYPE_DISCO_RESPONSE) return

        rc = E_CORRUPT
        if (message%header%size /= IPC_DISCO_RESPONSE_TYPE_SIZE) return

        rc = dm_ipc_message_pointer(message, ptr)
        if (dm_is_error(rc)) return

        nbyte = dm_ipc_message_length(message)

        rc = E_EMPTY
        if (nbyte == 0) return

        rc = E_CORRUPT
        if (nbyte < IPC_DISCO_RESPONSE_TYPE_SIZE) return

        rc = E_NONE
        call c_f_pointer(ptr, response_ptr)
        response = response_ptr
    end function ipc_disco_response_body

    integer function ipc_disco_request_send_socket(socket, response, service, transport, from, to, timeout, header) result(rc)
        !! Sends IPC discovery request message to passed socket and returns the
        !! IPC discovery response in `response`. Argument `transport` may be
        !! `IPC_TRANSPORT_ANY`.
        type(ipc_socket_type),         intent(inout)         :: socket    !! IPC socket.
        type(ipc_disco_response_type), intent(out)           :: response  !! IPC disco response.
        integer,                       intent(in)            :: service   !! IPC service.
        integer,                       intent(in)            :: transport !! IPC transport.
        character(*),                  intent(in),  optional :: from      !! Sender.
        character(*),                  intent(in),  optional :: to        !! Receiver.
        integer,                       intent(in),  optional :: timeout   !! Timeout [sec].
        type(ipc_message_header_type), intent(out), optional :: header    !! IPC message header.

        type(ipc_disco_request_type) :: request
        type(ipc_message_type)       :: message

        request = ipc_disco_request_type(service=service, transport=transport)

        ipc_block: block
            rc = dm_ipc_disco_to_message(message, request, from=from, to=to)
            call dm_error_out(rc, 'dm_ipc_disco_message()')
            if (dm_is_error(rc)) exit ipc_block

            rc = dm_ipc_message_send(message, socket)
            call dm_error_out(rc, 'dm_ipc_message_send()')
            if (dm_is_error(rc)) exit ipc_block

            rc = dm_ipc_message_receive(message, socket, timeout=timeout)
            call dm_error_out(rc, 'dm_ipc_message_receive()')
            if (dm_is_error(rc)) exit ipc_block

            if (present(header)) header = message%header
            rc = dm_ipc_disco_from_message(response, message)
            call dm_error_out(rc, 'dm_ipc_disco_from_message()')
            if (dm_is_error(rc)) exit ipc_block

            return
        end block ipc_block

        call dm_ipc_message_destroy(message)
    end function ipc_disco_request_send_socket

    integer function ipc_disco_request_send_url(url, response, service, transport, from, to, timeout, header) result(rc)
        !! Sends IPC discovery request message to passed socket and returns the
        !! IPC discovery response in `response`. Argument `transport` may be
        !! `IPC_TRANSPORT_ANY`.
        character(*),                  intent(in)            :: url       !! IPC disco server URL.
        type(ipc_disco_response_type), intent(out)           :: response  !! IPC disco response.
        integer,                       intent(in)            :: service   !! IPC service.
        integer,                       intent(in)            :: transport !! IPC transport.
        character(*),                  intent(in),  optional :: from      !! Sender.
        character(*),                  intent(in),  optional :: to        !! Receiver.
        integer,                       intent(in),  optional :: timeout   !! Timeout [sec].
        type(ipc_message_header_type), intent(out), optional :: header    !! IPC message header.

        type(ipc_disco_request_type) :: request
        type(ipc_message_type)       :: message
        type(ipc_socket_type)        :: socket

        request = ipc_disco_request_type(service=service, transport=transport)

        ipc_block: block
            rc = dm_ipc_open_request(socket)
            call dm_error_out(rc, 'dm_ipc_open_request()')
            if (dm_is_error(rc)) exit ipc_block

            rc = dm_ipc_dial(socket, url)
            call dm_error_out(rc, 'dm_ipc_dial()')
            if (dm_is_error(rc)) exit ipc_block

            rc = dm_ipc_disco_to_message(message, request, from=from, to=to)
            call dm_error_out(rc, 'dm_ipc_disco_to_message()')
            if (dm_is_error(rc)) exit ipc_block

            rc = dm_ipc_message_send(message, socket)
            call dm_error_out(rc, 'dm_ipc_message_send()')
            if (dm_is_error(rc)) exit ipc_block

            rc = dm_ipc_message_receive(message, socket, timeout=timeout)
            call dm_error_out(rc, 'dm_ipc_message_receive()')
            if (dm_is_error(rc)) exit ipc_block

            if (present(header)) header = message%header
            rc = dm_ipc_disco_from_message(response, message)
            call dm_error_out(rc, 'dm_ipc_disco_from_message()')
        end block ipc_block

        call dm_ipc_message_destroy(message)
        call dm_ipc_close(socket)
    end function ipc_disco_request_send_url
end module dm_ipc_disco
