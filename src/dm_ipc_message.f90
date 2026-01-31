! Author:  Philipp Engel
! Licence: ISC
module dm_ipc_message
    !! Abstraction layer over NNG messages.
    !!
    !! Send an observation message:
    !!
    !! ``` fortran
    !! integer                :: rc      ! Return code.
    !! type(ipc_socket_type)  :: socket  ! IPC socket.
    !! type(ipc_message_type) :: message ! IPC message.
    !! type(observ_type)      :: observ  ! Observation.
    !!
    !! rc = dm_ipc_open_pair(socket)
    !! rc = dm_ipc_dial(socket, 'ipc:///tmp/socket.ipc')
    !! rc = dm_ipc_message_create(message, observ, from='dmdummy1', to='dmdummy2')
    !! rc = dm_ipc_message_send(message, socket)
    !! call dm_ipc_close(socket)
    !! ```
    !!
    !! The message is destroyed automatically. Receive an observation message:
    !!
    !! ``` fortran
    !! integer                       :: rc      ! Return code.
    !! type(ipc_socket_type)         :: socket  ! IPC socket.
    !! type(ipc_message_type)        :: message ! IPC message.
    !! type(observ_type)             :: observ  ! Received observation.
    !!
    !! rc = dm_ipc_open_pair(socket)
    !! rc = dm_ipc_listen(socket, 'ipc:///tmp/socket.ipc')
    !! rc = dm_ipc_message_receive(message, socket, timeout=500)
    !!
    !! if (message%header%type == IPC_MESSAGE_TYPE_OBSERV) then
    !!     rc = dm_ipc_message_body(message, observ)
    !! end if
    !!
    !! call dm_ipc_message_destroy(message)
    !! call dm_ipc_close(socket)
    !! ```
    !!
    !! Receive an observation message using the wrapper:
    !!
    !! ``` fortran
    !! integer                       :: rc     ! Return code.
    !! type(ipc_socket_type)         :: socket ! IPC socket.
    !! type(ipc_message_header_type) :: header ! Optional IPC message header.
    !! type(observ_type)             :: observ ! Received observation.
    !!
    !! rc = dm_ipc_open_pair(socket)
    !! rc = dm_ipc_listen(socket, 'ipc:///tmp/socket.ipc')
    !! rc = dm_ipc_message_receive(observ, socket, header=header, from='dmdummy1', to='dmdummy2', timeout=500)
    !! call dm_ipc_close(socket)
    !! ```
    use :: dm_c
    use :: dm_error
    use :: dm_id
    use :: dm_ipc
    use :: dm_ipc_type
    use :: dm_kind
    use :: dm_util
    use :: dm_uuid
    implicit none (type, external)
    private

    integer, parameter, public :: IPC_MESSAGE_HEADER_MAGIC  = int(z'444D32') !! Magic bytes of IPC messages (`DM2` in ASCII).
    integer, parameter, public :: IPC_MESSAGE_HEADER_ID_LEN = UUID_LEN       !! Max. IPC message id length.

    type, public :: ipc_message_header_type
        !! IPC message header (128 byte).
        sequence
        integer(i4),   private               :: magic   = IPC_MESSAGE_HEADER_MAGIC !! Magic bytes.
        character(IPC_MESSAGE_HEADER_ID_LEN) :: id      = ' '                      !! Message id (UUIDv4).
        character(IPC_MESSAGE_HEADER_ID_LEN) :: from    = ' '                      !! Name of sender (`-0-9A-Z_a-z`).
        character(IPC_MESSAGE_HEADER_ID_LEN) :: to      = ' '                      !! Name of receiver (`-0-9A-Z_a-z`).
        integer(i4)                          :: error   = E_NONE                   !! Error code.
        integer(i4)                          :: size    = 0                        !! Actual body size [byte].
        integer(i4)                          :: type    = IPC_MESSAGE_TYPE_NONE    !! Message body type (`IPC_MESSAGE_TYPE_*`).
        character(16), private               :: padding = achar(0)                 !! Padding.
    end type ipc_message_header_type

    integer, parameter, public :: IPC_MESSAGE_HEADER_TYPE_SIZE = storage_size(ipc_message_header_type()) / 8 !! Size of `ipc_message_header_type` [byte].

    type, public :: ipc_message_type
        !! IPC message type.
        type(c_ptr)                   :: context = c_null_ptr                !! NNG message pointer.
        type(ipc_message_header_type) :: header  = ipc_message_header_type() !! IPC message header.
    end type ipc_message_type

    interface dm_ipc_message_create
        module procedure :: ipc_message_create_disco_request
        module procedure :: ipc_message_create_disco_response
        module procedure :: ipc_message_create_observ
    end interface dm_ipc_message_create

    interface dm_ipc_message_receive
        module procedure :: ipc_message_receive
        module procedure :: ipc_message_receive_disco_request
        module procedure :: ipc_message_receive_disco_response
        module procedure :: ipc_message_receive_log
        module procedure :: ipc_message_receive_observ
    end interface dm_ipc_message_receive

    interface ipc_message_append
        module procedure :: ipc_message_append_body
        module procedure :: ipc_message_append_header
        module procedure :: ipc_message_append_disco_request
        module procedure :: ipc_message_append_disco_response
        module procedure :: ipc_message_append_observ
    end interface ipc_message_append

    interface dm_ipc_message_body
        module procedure :: ipc_message_body_disco_request
        module procedure :: ipc_message_body_disco_response
        module procedure :: ipc_message_body_log
        module procedure :: ipc_message_body_observ
        module procedure :: ipc_message_body_raw
    end interface dm_ipc_message_body

    public :: dm_ipc_message_body
    public :: dm_ipc_message_create
    public :: dm_ipc_message_destroy
    public :: dm_ipc_message_header
    public :: dm_ipc_message_header_init
    public :: dm_ipc_message_header_is_valid
    public :: dm_ipc_message_header_out
    public :: dm_ipc_message_receive
    public :: dm_ipc_message_send

    private :: ipc_message_allocate
    private :: ipc_message_append
    private :: ipc_message_append_body
    private :: ipc_message_append_disco_request
    private :: ipc_message_append_disco_response
    private :: ipc_message_append_header
    private :: ipc_message_append_observ
    private :: ipc_message_body_disco_request
    private :: ipc_message_body_disco_response
    private :: ipc_message_body_log
    private :: ipc_message_body_observ
    private :: ipc_message_body_raw
    private :: ipc_message_create
    private :: ipc_message_create_disco_request
    private :: ipc_message_create_disco_response
    private :: ipc_message_create_observ
    private :: ipc_message_length
    private :: ipc_message_pointer
    private :: ipc_message_receive_disco_request
    private :: ipc_message_receive_disco_response
    private :: ipc_message_receive_log
    private :: ipc_message_receive_observ
    private :: ipc_message_receive
    private :: ipc_message_trim
    private :: ipc_message_trim_header
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ipc_message_header_init(header, from, to, error, size, type) result(rc)
        type(ipc_message_header_type), intent(out)          :: header !! IPC message header.
        character(*),                  intent(in)           :: from   !! Sender id.
        character(*),                  intent(in), optional :: to     !! Receiver id.
        integer,                       intent(in), optional :: error  !! DMPACK error code.
        integer,                       intent(in), optional :: size   !! IPC message body size [byte].
        integer,                       intent(in), optional :: type   !! IPC message type.

        header%id   = dm_uuid4()
        header%from = from

        if (present(to))    header%to    = to
        if (present(error)) header%error = error
        if (present(size))  header%size  = size
        if (present(type))  header%type  = type

        rc = E_INVALID
        if (dm_ipc_message_header_is_valid(header)) rc = E_NONE
    end function dm_ipc_message_header_init

    pure elemental logical function dm_ipc_message_header_is_valid(header) result(valid)
        type(ipc_message_header_type), intent(in) :: header !! IPC message header.

        valid = (header%magic == IPC_MESSAGE_HEADER_MAGIC                  .and. &
                 dm_uuid4_is_valid(header%id)                              .and. &
                 dm_id_is_valid(header%from)                               .and. &
                 (len_trim(header%to) == 0 .or. dm_id_is_valid(header%to)) .and. &
                 dm_error_is_valid(header%error)                           .and. &
                 header%size >= 0                                          .and. &
                 dm_ipc_message_type_is_valid(header%type))
    end function dm_ipc_message_header_is_valid

    pure elemental logical function dm_ipc_message_is_allocated(message) result(allocated)
        !! Returns `.true.` if NNG message context pointer is associated.
        type(ipc_message_type), intent(in) :: message !! IPC message.

        allocated = c_associated(message%context)
    end function dm_ipc_message_is_allocated

    integer function dm_ipc_message_header(message) result(rc)
        !! Reads header from IPC message. The header is removed (trimmed) from
        !! the underlying NNG message.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if header size is unexpected.
        !! * `E_EMPTY` if NNG message body is empty.
        !! * `E_INVALID` if header is invalid.
        !! * `E_NULL` if NNG message body is not associated.
        !!
        type(ipc_message_type), intent(inout) :: message !! IPC message.

        integer                                :: nbyte
        type(c_ptr)                            :: ptr
        type(ipc_message_header_type), pointer :: header_ptr

        rc = ipc_message_pointer(message, ptr)
        if (dm_is_error(rc)) return

        nbyte = ipc_message_length(message)

        rc = E_EMPTY
        if (nbyte == 0) return

        rc = E_CORRUPT
        if (nbyte < IPC_MESSAGE_HEADER_TYPE_SIZE) return

        call c_f_pointer(ptr, header_ptr)
        message%header = header_ptr

        rc = ipc_message_trim_header(message)
        if (dm_is_error(rc)) return

        if (.not. dm_ipc_message_header_is_valid(message%header)) rc = E_INVALID
    end function dm_ipc_message_header

    integer function dm_ipc_message_send(message, socket, non_blocking, timeout) result(rc)
        !! Sends IPC message to socket.
        use :: nng, only: NNG_FLAG_NONBLOCK, nng_sendmsg

        type(ipc_message_type), intent(inout)        :: message      !! IPC message.
        type(ipc_socket_type),  intent(inout)        :: socket       !! IPC socket.
        logical,                intent(in), optional :: non_blocking !! Non-blocking access.
        integer,                intent(in), optional :: timeout      !! Timeout [msec].

        integer :: flags

        flags = 0
        if (dm_present(non_blocking, .false.)) flags = NNG_FLAG_NONBLOCK

        rc = E_NULL
        if (.not. dm_ipc_message_is_allocated(message)) return

        if (present(timeout)) then
            rc = dm_ipc_set_send_timeout(socket, timeout)
            if (dm_is_error(rc)) return
        end if

        socket%error_nng = nng_sendmsg(socket%socket, message%context, flags)
        rc = dm_ipc_error(socket%error_nng)

        call dm_ipc_message_destroy(message)
    end function dm_ipc_message_send

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_ipc_message_destroy(message, header)
        !! Frees memory of NNG message and resets header unless `header` is
        !! `.false.`.
        use :: nng, only: nng_msg_free

        type(ipc_message_type), intent(inout)        :: message !! IPC message.
        logical,                intent(in), optional :: header  !! Reset header.

        if (dm_present(header, .true.)) message%header = ipc_message_header_type()
        if (.not. c_associated(message%context)) return
        call nng_msg_free(message%context)
    end subroutine dm_ipc_message_destroy

    subroutine dm_ipc_message_header_out(header, unit)
        !! Prints IPC message header to standard output or given file unit.
        type(ipc_message_header_type), intent(in)           :: header !! IPC message header.
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
    integer function ipc_message_allocate(message, size) result(rc)
        !! Allocates NNG message memory.
        use :: nng, only: nng_msg_alloc

        type(ipc_message_type), intent(inout)        :: message !! IPC message.
        integer,                intent(in), optional :: size    !! Message size or 0.

        rc = dm_ipc_error(nng_msg_alloc(message%context, int(dm_present(size, 0), c_size_t)))
    end function ipc_message_allocate

    integer function ipc_message_append_body(message, body) result(rc)
        !! Appends IPC message body to NNG message.
        use :: nng, only: nng_msg_append

        type(ipc_message_type), intent(inout) :: message !! IPC message.
        character(*), target,   intent(inout) :: body    !! IPC message body.

        rc = dm_ipc_error(nng_msg_append(message%context, c_loc(body), len(body, c_size_t)))
    end function ipc_message_append_body

    integer function ipc_message_append_disco_response(message, response) result(rc)
        !! Appends IPC disco response to NNG message.
        use :: nng, only: nng_msg_append
        use :: dm_ipc_disco

        type(ipc_message_type),                intent(inout) :: message  !! IPC message.
        type(ipc_disco_response_type), target, intent(inout) :: response !! IPC disco response.

        rc = dm_ipc_error(nng_msg_append(message%context, c_loc(response), int(IPC_DISCO_RESPONSE_TYPE_SIZE, c_size_t)))
    end function ipc_message_append_disco_response

    integer function ipc_message_append_disco_request(message, request) result(rc)
        !! Appends IPC disco request to NNG message.
        use :: nng, only: nng_msg_append
        use :: dm_ipc_disco

        type(ipc_message_type),               intent(inout) :: message !! IPC message.
        type(ipc_disco_request_type), target, intent(inout) :: request !! IPC disco request.

        rc = dm_ipc_error(nng_msg_append(message%context, c_loc(request), int(IPC_DISCO_REQUEST_TYPE_SIZE, c_size_t)))
    end function ipc_message_append_disco_request

    integer function ipc_message_append_header(message, header) result(rc)
        !! Appends IPC message header to NNG message.
        use :: nng, only: nng_msg_append

        type(ipc_message_type),                intent(inout) :: message !! IPC message.
        type(ipc_message_header_type), target, intent(inout) :: header  !! IPC message header.

        rc = dm_ipc_error(nng_msg_append(message%context, c_loc(header), int(IPC_MESSAGE_HEADER_TYPE_SIZE, c_size_t)))
    end function ipc_message_append_header

    integer function ipc_message_append_observ(message, observ) result(rc)
        !! Appends observation to NNG message.
        use :: nng, only: nng_msg_append
        use :: dm_observ

        type(ipc_message_type),    intent(inout) :: message !! IPC message.
        type(observ_type), target, intent(inout) :: observ  !! Observation.

        rc = dm_ipc_error(nng_msg_append(message%context, c_loc(observ), int(OBSERV_TYPE_SIZE, c_size_t)))
    end function ipc_message_append_observ

    integer function ipc_message_body_disco_request(message, request) result(rc)
        !! Returns IPC disco request from NNG message body in `request`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if payload size does not match type size.
        !! * `E_EMPTY` if NNG message body is empty.
        !! * `E_NULL` if NNG message body is not associated.
        !! * `E_TYPE` if message type is not supported.
        !!
        use :: dm_ipc_disco

        type(ipc_message_type),       intent(inout) :: message !! IPC message.
        type(ipc_disco_request_type), intent(out)   :: request !! IPC disco request.

        integer                               :: nbyte
        type(c_ptr)                           :: ptr
        type(ipc_disco_request_type), pointer :: request_ptr

        rc = E_TYPE
        if (message%header%type /= IPC_MESSAGE_TYPE_DISCO_REQUEST) return

        rc = E_CORRUPT
        if (message%header%size /= IPC_DISCO_REQUEST_TYPE_SIZE) return

        rc = ipc_message_pointer(message, ptr)
        if (dm_is_error(rc)) return

        nbyte = ipc_message_length(message)

        rc = E_EMPTY
        if (nbyte == 0) return

        rc = E_CORRUPT
        if (nbyte < IPC_DISCO_REQUEST_TYPE_SIZE) return

        rc = E_NONE
        call c_f_pointer(ptr, request_ptr)
        request = request_ptr
    end function ipc_message_body_disco_request

    integer function ipc_message_body_disco_response(message, response) result(rc)
        !! Returns IPC disco response from NNG message body in `response`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if payload size does not match type size.
        !! * `E_EMPTY` if NNG message body is empty.
        !! * `E_NULL` if NNG message body is not associated.
        !! * `E_TYPE` if message type is not supported.
        !!
        use :: dm_ipc_disco

        type(ipc_message_type),        intent(inout) :: message  !! IPC message.
        type(ipc_disco_response_type), intent(out)   :: response !! IPC disco response.

        integer                                :: nbyte
        type(c_ptr)                            :: ptr
        type(ipc_disco_response_type), pointer :: response_ptr

        rc = E_TYPE
        if (message%header%type /= IPC_MESSAGE_TYPE_DISCO_RESPONSE) return

        rc = E_CORRUPT
        if (message%header%size /= IPC_DISCO_RESPONSE_TYPE_SIZE) return

        rc = ipc_message_pointer(message, ptr)
        if (dm_is_error(rc)) return

        nbyte = ipc_message_length(message)

        rc = E_EMPTY
        if (nbyte == 0) return

        rc = E_CORRUPT
        if (nbyte < IPC_DISCO_RESPONSE_TYPE_SIZE) return

        rc = E_NONE
        call c_f_pointer(ptr, response_ptr)
        response = response_ptr
    end function ipc_message_body_disco_response

    integer function ipc_message_body_log(message, log) result(rc)
        !! Returns log message from NNG message body in `log`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if payload size does not match type size.
        !! * `E_EMPTY` if NNG message body is empty.
        !! * `E_NULL` if NNG message body is not associated.
        !! * `E_TYPE` if message type is not supported.
        !!
        use :: dm_log

        type(ipc_message_type), intent(inout) :: message !! IPC message.
        type(log_type),         intent(out)   :: log     !! Log.

        integer                 :: nbyte
        type(c_ptr)             :: ptr
        type(log_type), pointer :: log_ptr

        rc = E_TYPE
        if (message%header%type /= IPC_MESSAGE_TYPE_LOG) return

        rc = E_CORRUPT
        if (message%header%size /= LOG_TYPE_SIZE) return

        rc = ipc_message_pointer(message, ptr)
        if (dm_is_error(rc)) return

        nbyte = ipc_message_length(message)

        rc = E_EMPTY
        if (nbyte == 0) return

        rc = E_CORRUPT
        if (nbyte < LOG_TYPE_SIZE) return

        rc = E_NONE
        call c_f_pointer(ptr, log_ptr)
        log = log_ptr
    end function ipc_message_body_log

    integer function ipc_message_body_observ(message, observ) result(rc)
        !! Returns observation from NNG message body in `observ`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if payload size does not match type size.
        !! * `E_EMPTY` if NNG message body is empty.
        !! * `E_NULL` if NNG message body is not associated.
        !! * `E_TYPE` if message type is not supported.
        !!
        use :: dm_observ

        type(ipc_message_type), intent(inout) :: message !! IPC message.
        type(observ_type),      intent(out)   :: observ  !! Observation.

        integer                    :: nbyte
        type(c_ptr)                :: ptr
        type(observ_type), pointer :: observ_ptr

        rc = E_TYPE
        if (message%header%type /= IPC_MESSAGE_TYPE_OBSERV) return

        rc = E_CORRUPT
        if (message%header%size /= OBSERV_TYPE_SIZE) return

        rc = ipc_message_pointer(message, ptr)
        if (dm_is_error(rc)) return

        nbyte = ipc_message_length(message)

        rc = E_EMPTY
        if (nbyte == 0) return

        rc = E_CORRUPT
        if (nbyte < OBSERV_TYPE_SIZE) return

        rc = E_NONE
        call c_f_pointer(ptr, observ_ptr)
        observ = observ_ptr
    end function ipc_message_body_observ

    integer function ipc_message_body_raw(message, bytes, nbyte) result(rc)
        !! Reads chunk of NNG message.
        type(ipc_message_type), intent(inout) :: message !! IPC message.
        character(*),           intent(inout) :: bytes   !! Bytes read from message.
        integer,                intent(out)   :: nbyte   !! Message size.

        integer     :: n
        type(c_ptr) :: ptr

        rc = ipc_message_pointer(message, ptr)
        if (dm_is_error(rc)) return

        nbyte = ipc_message_length(message)

        rc = E_EMPTY
        if (nbyte == 0) return

        rc = E_NONE
        n = min(nbyte, len(bytes))

        block
            character(n), pointer :: bytes_ptr
            call c_f_pointer(ptr, bytes_ptr)
            bytes = bytes_ptr
        end block
    end function ipc_message_body_raw

    integer function ipc_message_create(message, from, to, error, size, type) result(rc)
        !! Allocated NNG message and appends IPC message header.
        type(ipc_message_type), intent(out)          :: message !! IPC message.
        character(*),           intent(in)           :: from    !! Sender id.
        character(*),           intent(in), optional :: to      !! Receiver id.
        integer,                intent(in), optional :: error   !! DMPACK error code.
        integer,                intent(in), optional :: size    !! IPC message body size [byte].
        integer,                intent(in), optional :: type    !! IPC message type.

        ! Initialise message header.
        rc = dm_ipc_message_header_init(message%header, from, to, error, size, type)
        if (dm_is_error(rc)) return

        ! Allocate memory.
        rc = ipc_message_allocate(message)
        if (dm_is_error(rc)) return

        ! Append message header.
        rc = ipc_message_append(message, message%header)
    end function ipc_message_create

    integer function ipc_message_create_disco_request(message, request, from, to, error) result(rc)
        !! Creates IPC message from IPC disco request.
        use :: dm_ipc_disco

        type(ipc_message_type),       intent(out)          :: message !! IPC message.
        type(ipc_disco_request_type), intent(inout)        :: request !! IPC disco request.
        character(*),                 intent(in)           :: from    !! Sender id.
        character(*),                 intent(in), optional :: to      !! Receiver id.
        integer,                      intent(in), optional :: error   !! DMPACK error code.

        ! Create and append header.
        rc = ipc_message_create(message, from, to, error, IPC_DISCO_REQUEST_TYPE_SIZE, IPC_MESSAGE_TYPE_DISCO_REQUEST)
        if (dm_is_error(rc)) return

        ! Append message body.
        rc = ipc_message_append(message, request)
    end function ipc_message_create_disco_request

    integer function ipc_message_create_disco_response(message, response, from, to, error) result(rc)
        !! Creates IPC message from IPC disco response.
        use :: dm_ipc_disco

        type(ipc_message_type),        intent(out)          :: message  !! IPC message.
        type(ipc_disco_response_type), intent(inout)        :: response !! IPC disco response.
        character(*),                  intent(in)           :: from     !! Sender id.
        character(*),                  intent(in), optional :: to       !! Receiver id.
        integer,                       intent(in), optional :: error    !! DMPACK error code.

        ! Create and append header.
        rc = ipc_message_create(message, from, to, error, IPC_DISCO_RESPONSE_TYPE_SIZE, IPC_MESSAGE_TYPE_DISCO_RESPONSE)
        if (dm_is_error(rc)) return

        ! Append message body.
        rc = ipc_message_append(message, response)
    end function ipc_message_create_disco_response

    integer function ipc_message_create_observ(message, observ, from, to, error) result(rc)
        !! Creates IPC message from observation.
        use :: dm_observ

        type(ipc_message_type), intent(out)          :: message !! IPC message.
        type(observ_type),      intent(inout)        :: observ  !! Observation.
        character(*),           intent(in)           :: from    !! Sender id.
        character(*),           intent(in), optional :: to      !! Receiver id.
        integer,                intent(in), optional :: error   !! DMPACK error code.

        ! Create and append header.
        rc = ipc_message_create(message, from, to, error, OBSERV_TYPE_SIZE, IPC_MESSAGE_TYPE_OBSERV)
        if (dm_is_error(rc)) return

        ! Append message body.
        rc = ipc_message_append(message, observ)
    end function ipc_message_create_observ

    integer function ipc_message_length(message) result(length)
        !! Returns length of NNG message body (number of bytes).
        use :: nng, only: nng_msg_len

        type(ipc_message_type), intent(inout) :: message !! IPC message.

        length = int(nng_msg_len(message%context))
    end function ipc_message_length

    integer function ipc_message_pointer(message, ptr) result(rc)
        !! Returns C pointer to NNG message body in `ptr`. The function return
        !! `E_NULL` on error.
        use :: nng, only: nng_msg_body

        type(ipc_message_type), intent(inout) :: message !! IPC message.
        type(c_ptr),            intent(out)   :: ptr     !! Pointer to NNG message body.

        rc  = E_NULL
        ptr = nng_msg_body(message%context)
        if (c_associated(ptr)) rc = E_NONE
    end function ipc_message_pointer

    integer function ipc_message_receive(message, socket, non_blocking, timeout) result(rc)
        !! Reads NNG message and IPC message header.
        use :: nng, only: NNG_FLAG_NONBLOCK, nng_recvmsg

        type(ipc_message_type), intent(out)          :: message      !! IPC message.
        type(ipc_socket_type),  intent(inout)        :: socket       !! IPC socket.
        logical,                intent(in), optional :: non_blocking !! Non-blocking access.
        integer,                intent(in), optional :: timeout      !! Timeout [msec].

        integer :: flags

        flags = 0
        if (dm_present(non_blocking, .false.)) flags = NNG_FLAG_NONBLOCK

        if (present(timeout)) then
            rc = dm_ipc_set_receive_timeout(socket, timeout)
            if (dm_is_error(rc)) return
        end if

        ! Receive NNG message.
        socket%error_nng = nng_recvmsg(socket%socket, message%context, flags)
        rc = dm_ipc_error(socket%error_nng)
        if (dm_is_error(rc)) return

        ! Read IPC message header from NNG message body.
        rc = dm_ipc_message_header(message)
    end function ipc_message_receive

    integer function ipc_message_receive_disco_request(request, socket, header, from, to, non_blocking, timeout) result(rc)
        !! Receives IPC disco request from IPC socket. If `from` is passed,
        !! only messages for this id will be accepted. If `to` is passed, only
        !! message from this id will be accepted. Otherwise, the function
        !! returns `E_IGNORED`. The NNG message is destroyed automatically.
        use :: dm_ipc_disco

        type(ipc_disco_request_type),  intent(out)           :: request      !! IPC disco request.
        type(ipc_socket_type),         intent(inout)         :: socket       !! IPC socket.
        type(ipc_message_header_type), intent(out), optional :: header       !! IPC message header.
        character(*),                  intent(in),  optional :: from         !! Message sender.
        character(*),                  intent(in),  optional :: to           !! Message receiver.
        logical,                       intent(in),  optional :: non_blocking !! Return if no message is present.
        integer,                       intent(in),  optional :: timeout      !! Timeout [msec].

        type(ipc_message_type) :: message

        ipc_block: block
            rc = ipc_message_receive(message, socket, non_blocking, timeout)
            if (dm_is_error(rc)) exit ipc_block

            rc = E_IGNORED
            if (present(from)) then
                if (len_trim(from) > 0 .and. message%header%from /= from) return
            end if

            if (present(to)) then
                if (len_trim(to) > 0 .and. message%header%to /= to) return
            end if

            rc = ipc_message_body_disco_request(message, request)
        end block ipc_block

        if (present(header)) header = message%header
        call dm_ipc_message_destroy(message)
    end function ipc_message_receive_disco_request

    integer function ipc_message_receive_disco_response(response, socket, header, from, to, non_blocking, timeout) result(rc)
        !! Receives IPC disco response from IPC socket. If `from` is passed,
        !! only messages for this id will be accepted. If `to` is passed, only
        !! message from this id will be accepted. Otherwise, the function
        !! returns `E_IGNORED`. The NNG message is destroyed automatically.
        use :: dm_ipc_disco

        type(ipc_disco_response_type), intent(out)           :: response     !! IPC disco response.
        type(ipc_socket_type),         intent(inout)         :: socket       !! IPC socket.
        type(ipc_message_header_type), intent(out), optional :: header       !! IPC message header.
        character(*),                  intent(in),  optional :: from         !! Message sender.
        character(*),                  intent(in),  optional :: to           !! Message receiver.
        logical,                       intent(in),  optional :: non_blocking !! Return if no message is present.
        integer,                       intent(in),  optional :: timeout      !! Timeout [msec].

        type(ipc_message_type) :: message

        ipc_block: block
            rc = ipc_message_receive(message, socket, non_blocking, timeout)
            if (dm_is_error(rc)) exit ipc_block

            rc = E_IGNORED
            if (present(from)) then
                if (len_trim(from) > 0 .and. message%header%from /= from) return
            end if

            if (present(to)) then
                if (len_trim(to) > 0 .and. message%header%to /= to) return
            end if

            rc = ipc_message_body_disco_response(message, response)
        end block ipc_block

        if (present(header)) header = message%header
        call dm_ipc_message_destroy(message)
    end function ipc_message_receive_disco_response

    integer function ipc_message_receive_log(log, socket, header, from, to, non_blocking, timeout) result(rc)
        !! Receives log message from IPC socket. If `from` is passed, only
        !! messages for this id will be accepted. If `to` is passed, only
        !! message from this id will be accepted. Otherwise, the function
        !! returns `E_IGNORED`. The NNG message is destroyed automatically.
        use :: dm_log

        type(log_type),                intent(out)           :: log          !! Log.
        type(ipc_socket_type),         intent(inout)         :: socket       !! IPC socket.
        type(ipc_message_header_type), intent(out), optional :: header       !! IPC message header.
        character(*),                  intent(in),  optional :: from         !! Message sender.
        character(*),                  intent(in),  optional :: to           !! Message receiver.
        logical,                       intent(in),  optional :: non_blocking !! Return if no message is present.
        integer,                       intent(in),  optional :: timeout      !! Timeout [msec].

        type(ipc_message_type) :: message

        ipc_block: block
            rc = ipc_message_receive(message, socket, non_blocking, timeout)
            if (dm_is_error(rc)) exit ipc_block

            rc = E_IGNORED
            if (present(from)) then
                if (len_trim(from) > 0 .and. message%header%from /= from) return
            end if

            if (present(to)) then
                if (len_trim(to) > 0 .and. message%header%to /= to) return
            end if

            rc = ipc_message_body_log(message, log)
        end block ipc_block

        if (present(header)) header = message%header
        call dm_ipc_message_destroy(message)
    end function ipc_message_receive_log

    integer function ipc_message_receive_observ(observ, socket, header, from, to, non_blocking, timeout) result(rc)
        !! Receives observation from IPC socket. If `from` is passed, only
        !! messages for this id will be accepted. If `to` is passed, only
        !! message from this id will be accepted. Otherwise, the function
        !! returns `E_IGNORED`. The NNG message is destroyed automatically.
        use :: dm_observ

        type(observ_type),             intent(out)           :: observ       !! Observation.
        type(ipc_socket_type),         intent(inout)         :: socket       !! IPC socket.
        type(ipc_message_header_type), intent(out), optional :: header       !! IPC message header.
        character(*),                  intent(in),  optional :: from         !! Message sender.
        character(*),                  intent(in),  optional :: to           !! Message receiver.
        logical,                       intent(in),  optional :: non_blocking !! Return if no message is present.
        integer,                       intent(in),  optional :: timeout      !! Timeout [msec].

        type(ipc_message_type) :: message

        ipc_block: block
            rc = ipc_message_receive(message, socket, non_blocking, timeout)
            if (dm_is_error(rc)) exit ipc_block

            rc = E_IGNORED
            if (present(from)) then
                if (len_trim(from) > 0 .and. message%header%from /= from) return
            end if

            if (present(to)) then
                if (len_trim(to) > 0 .and. message%header%to /= to) return
            end if

            rc = ipc_message_body_observ(message, observ)
        end block ipc_block

        if (present(header)) header = message%header
        call dm_ipc_message_destroy(message)
    end function ipc_message_receive_observ

    integer function ipc_message_trim(message, nbyte) result(rc)
        !! Removes header from NNG message.
        use :: nng, only: nng_msg_trim

        type(ipc_message_type), intent(inout) :: message !! IPC message.
        integer,                intent(in)    :: nbyte   !! Number of bytes to trim.

        rc = dm_ipc_error(nng_msg_trim(message%context, int(nbyte, c_size_t)))
    end function ipc_message_trim

    integer function ipc_message_trim_header(message) result(rc)
        type(ipc_message_type), intent(inout) :: message !! IPC message.

        rc = ipc_message_trim(message, IPC_MESSAGE_HEADER_TYPE_SIZE)
    end function ipc_message_trim_header
end module dm_ipc_message
