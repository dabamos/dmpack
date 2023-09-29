! Author:  Philipp Engel
! Licence: ISC
module dm_rpc
    !! Abstraction layer for Remote Procedure Calls (RPCs) over HTTP,
    !! using libcurl.
    !!
    !! To send an observation to an HTTP-RPC API on `localhost`:
    !!
    !! ```fortran
    !! character(len=:), allocatable :: url
    !! integer                       :: rc
    !! type(observ_type)             :: observ
    !! type(rpc_request_type)        :: request
    !! type(rpc_response_type)       :: response
    !!
    !! rc = dm_rpc_init()
    !! url = dm_rpc_url('localhost', port=80, endpoint=RPC_ROUTE_OBSERV)
    !! rc = dm_rpc_send(request, response, observ, url)
    !! call dm_rpc_destroy()
    !! ```
    !!
    !! The URL returned by `dm_rpc_url()` will equal
    !! `http://localhost:80/api/v1/observ` in this case.
    !!
    !! The procedures `dm_rpc_init()` and `dm_rpc_destroy()` have to be called
    !! once per process, and only if neither the MQTT nor the mail backend was
    !! initialised already.
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_beat
    use :: dm_error
    use :: dm_http
    use :: dm_kind
    use :: dm_log
    use :: dm_mime
    use :: dm_nml
    use :: dm_node
    use :: dm_observ
    use :: dm_sensor
    use :: dm_target
    use :: dm_util
    use :: dm_version
    use :: dm_z
    implicit none (type, external)
    private

    character(len=*), parameter, public :: RPC_BASE       = '/api/v1'                      !! Base path of dmapi service.
    character(len=*), parameter, public :: RPC_USER_AGENT = 'DMPACK ' // DM_VERSION_STRING !! Default user agent of RPC client.

    character(len=*), parameter, public :: RPC_ROUTE_BEAT   = '/beat'   !! Resolves to `/api/v1/beat`.
    character(len=*), parameter, public :: RPC_ROUTE_LOG    = '/log'    !! Resolves to `/api/v1/log`.
    character(len=*), parameter, public :: RPC_ROUTE_OBSERV = '/observ' !! Resolves to `/api/v1/observ`.
    character(len=*), parameter, public :: RPC_ROUTE_NODE   = '/node'   !! Resolves to `/api/v1/node`.
    character(len=*), parameter, public :: RPC_ROUTE_SENSOR = '/sensor' !! Resolves to `/api/v1/sensor`.
    character(len=*), parameter, public :: RPC_ROUTE_TARGET = '/target' !! Resolves to `/api/v1/target`.

    ! HTTP Auth.
    integer, parameter, public :: RPC_AUTH_NONE  = 0 !! No authentication.
    integer, parameter, public :: RPC_AUTH_BASIC = 1 !! HTTP Basic Auth.

    ! HTTP Method.
    integer, parameter, public :: RPC_METHOD_GET  = 0 !! HTTP GET method.
    integer, parameter, public :: RPC_METHOD_POST = 1 !! HTTP POST method.

    ! TCP Keep-Alive.
    integer, parameter, public :: RPC_KEEP_ALIVE          = 1   !! Enable TCP keep-alive.
    integer, parameter, public :: RPC_KEEP_ALIVE_IDLE     = 120 !! TCP keep-alive idle time in seconds.
    integer, parameter, public :: RPC_KEEP_ALIVE_INTERVAL = 60  !! Interval time between TCP keep-alive probes in seconds.

    abstract interface
        function dm_rpc_callback(ptr, size, nmemb, data) bind(c)
            !! Abstract read/write callback for libcurl.
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: ptr             !! C pointer to a chunk of the response.
            integer(kind=c_size_t), intent(in), value :: size            !! Always 1.
            integer(kind=c_size_t), intent(in), value :: nmemb           !! Size of the response chunk.
            type(c_ptr),            intent(in), value :: data            !! C pointer to client data passed by caller.
            integer(kind=c_size_t)                    :: dm_rpc_callback !! Function return value.
        end function dm_rpc_callback
    end interface

    type, public :: rpc_response_type
        !! HTTP-RPC response type.
        integer                       :: code       = 0        !! HTTP response code.
        integer                       :: error      = E_NONE   !! DMPACK error code.
        integer                       :: error_curl = CURLE_OK !! cURL error code.
        real(kind=r8)                 :: total_time = 0.0_r8   !! Total transmission time.
        character(len=:), allocatable :: error_message         !! cURL error message.
        character(len=:), allocatable :: content_type          !! Response payload type (MIME).
        character(len=:), allocatable :: payload               !! Response payload.
    end type rpc_response_type

    type, public :: rpc_request_type
        !! HTTP-RPC request type.
        integer                                     :: auth            = RPC_AUTH_NONE  !! HTTP Auth.
        integer                                     :: method          = RPC_METHOD_GET !! HTTP method (GET, POST).
        integer                                     :: timeout         = 30             !! Timeout in seconds.
        integer                                     :: connect_timeout = 30             !! Connection timeout in seconds.
        logical                                     :: deflate         = .false.        !! Use deflate compression.
        logical                                     :: follow_location = .true.         !! Follow HTTP 3xx redirects.
        character(len=:), allocatable               :: payload                          !! Request payload.
        character(len=:), allocatable               :: content_type                     !! Request payload type (MIME).
        character(len=:), allocatable               :: accept                           !! HTTP Accept header.
        character(len=:), allocatable               :: username                         !! HTTP Basic Auth user name.
        character(len=:), allocatable               :: password                         !! HTTP Basic Auth password.
        character(len=:), allocatable               :: url                              !! Request URL.
        character(len=:), allocatable               :: user_agent                       !! User Agent.
        procedure(dm_rpc_callback), pointer, nopass :: callback        => dm_rpc_write_callback !! C-interoperable write callback function.
        type(c_ptr), private                        :: curl_ptr        = c_null_ptr     !! cURL handle.
        type(c_ptr), private                        :: list_ptr        = c_null_ptr     !! cURL list handle.
    end type rpc_request_type

    interface rpc_request
        !! Generic RPC request function.
        module procedure :: rpc_request_multi
        module procedure :: rpc_request_single
    end interface

    interface dm_rpc_request
        !! Generic RPC request function.
        module procedure :: dm_rpc_request_multi
        module procedure :: dm_rpc_request_single
    end interface

    interface dm_rpc_send
        !! Generic RPC send function.
        module procedure :: dm_rpc_send_type
        module procedure :: dm_rpc_send_types
    end interface

    public :: dm_rpc_callback
    public :: dm_rpc_destroy
    public :: dm_rpc_error
    public :: dm_rpc_error_multi
    public :: dm_rpc_init
    public :: dm_rpc_request
    public :: dm_rpc_request_multi
    public :: dm_rpc_request_single
    public :: dm_rpc_reset
    public :: dm_rpc_send
    public :: dm_rpc_send_type
    public :: dm_rpc_send_types
    public :: dm_rpc_url
    public :: dm_rpc_write_callback

    private :: rpc_request
    private :: rpc_request_multi
    private :: rpc_request_prepare
    private :: rpc_request_single
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_rpc_error(curl_error) result(rc)
        !! Converts cURL easy stack error code to DMPACK error code.
        integer, intent(in) :: curl_error !! cURL easy error code.

        select case (curl_error)
            case (CURLE_OK)
                rc = E_NONE

            case (CURLE_UNSUPPORTED_PROTOCOL)
            case (CURLE_FAILED_INIT)
            case (CURLE_URL_MALFORMAT)
            case (CURLE_NOT_BUILT_IN)
            case (CURLE_BAD_FUNCTION_ARGUMENT)
            case (CURLE_UNKNOWN_OPTION)
            case (CURLE_BAD_CONTENT_ENCODING)
                rc = E_INVALID

            case (CURLE_COULDNT_RESOLVE_PROXY)
            case (CURLE_COULDNT_RESOLVE_HOST)
            case (CURLE_COULDNT_CONNECT)
                rc = E_RPC_CONNECT

            case (CURLE_WEIRD_SERVER_REPLY)
                rc = E_RPC_API

            case (CURLE_REMOTE_ACCESS_DENIED)
            case (CURLE_AUTH_ERROR)
                rc = E_RPC_AUTH

            case (CURLE_WRITE_ERROR)
                rc = E_WRITE

            case (CURLE_READ_ERROR)
                rc = E_READ

            case (CURLE_OUT_OF_MEMORY)
                rc = E_ALLOC

            case (CURLE_OPERATION_TIMEDOUT)
                rc = E_TIMEOUT

            case (CURLE_GOT_NOTHING)
                rc = E_EMPTY

            case (CURLE_SSL_CONNECT_ERROR)
            case (CURLE_SSL_ENGINE_NOTFOUND)
            case (CURLE_SSL_ENGINE_SETFAILED)
            case (CURLE_SSL_CERTPROBLEM)
            case (CURLE_SSL_CIPHER)
            case (CURLE_PEER_FAILED_VERIFICATION)
            case (CURLE_SSL_ENGINE_INITFAILED)
            case (CURLE_SSL_CACERT_BADFILE)
            case (CURLE_SSL_SHUTDOWN_FAILED)
            case (CURLE_SSL_CRL_BADFILE)
            case (CURLE_SSL_ISSUER_ERROR)
            case (CURLE_SSL_PINNEDPUBKEYNOTMATCH)
            case (CURLE_SSL_INVALIDCERTSTATUS)
            case (CURLE_SSL_CLIENTCERT)
                rc = E_RPC_SSL

            case (CURLE_FILESIZE_EXCEEDED)
                rc = E_LIMIT

            case (CURLE_REMOTE_FILE_NOT_FOUND)
                rc = E_NOT_FOUND

            case default
                rc = E_RPC
        end select
    end function dm_rpc_error

    integer function dm_rpc_error_multi(multi_error) result(rc)
        !! Converts cURL multi stack error code to DMPACK error code.
        integer, intent(in) :: multi_error !! cURL multi error code.

        select case (multi_error)
            case (CURLM_OK)
                rc = E_NONE

            case (CURLM_BAD_HANDLE)
            case (CURLM_BAD_EASY_HANDLE)
            case (CURLM_BAD_FUNCTION_ARGUMENT)
            case (CURLM_UNKNOWN_OPTION)
                rc = E_INVALID

            case (CURLM_OUT_OF_MEMORY)
                rc = E_ALLOC

            case default
                rc = E_RPC
        end select
    end function dm_rpc_error_multi

    integer function dm_rpc_init() result(rc)
        !! Initialises libcurl backend.
        rc = E_RPC
        if (curl_global_init(CURL_GLOBAL_DEFAULT) /= CURLE_OK) return
        rc = E_NONE
    end function dm_rpc_init

    integer function dm_rpc_request_multi(requests, responses, url, method, accept, username, password) result(rc)
        !! Sends multiple HTTP requests by GET or POST method.
        type(rpc_request_type),               intent(inout)        :: requests(:)  !! RPC request type array.
        type(rpc_response_type), allocatable, intent(inout)        :: responses(:) !! RPC response type array.
        character(len=*),                     intent(in), optional :: url          !! URL of RPC API (may include port).
        integer,                              intent(in), optional :: method       !! `RPC_METHOD_GET` or `RPC_METHOD_POST`.
        character(len=*),                     intent(in), optional :: accept       !! HTTP accept header.
        character(len=*),                     intent(in), optional :: username     !! HTTP Basic Auth user name.
        character(len=*),                     intent(in), optional :: password     !! HTTP Basic Auth password.

        integer :: i

        do i = 1, size(requests)
            ! Set request parameters.
            if (present(accept)) requests(i)%accept = trim(accept)
            if (present(method)) requests(i)%method = method
            if (present(url))    requests(i)%url    = trim(url)

            ! HTTP Basic Auth.
            if (present(username) .and. present(password)) then
                requests(i)%auth     = RPC_AUTH_BASIC
                requests(i)%username = trim(username)
                requests(i)%password = trim(password)
            end if
        end do

        rc = rpc_request_multi(requests, responses)
    end function dm_rpc_request_multi

    integer function dm_rpc_request_single(request, response, url, method, payload, content_type, &
                                           accept, username, password, deflate) result(rc)
        !! Sends single HTTP request by GET or POST method, and with optional
        !! deflate compression.
        type(rpc_request_type),  intent(inout)           :: request      !! RPC request type.
        type(rpc_response_type), intent(out)             :: response     !! RPC response type.
        character(len=*),        intent(in),    optional :: url          !! URL of RPC API (may include port).
        integer,                 intent(in),    optional :: method       !! `RPC_METHOD_GET` or `RPC_METHOD_POST`.
        character(len=*),        intent(inout), optional :: payload      !! For POST only.
        character(len=*),        intent(in),    optional :: content_type !! For POST only.
        character(len=*),        intent(in),    optional :: accept       !! HTTP accept header.
        character(len=*),        intent(in),    optional :: username     !! HTTP Basic Auth user name.
        character(len=*),        intent(in),    optional :: password     !! HTTP Basic Auth password.
        logical,                 intent(in),    optional :: deflate      !! For POST only.

        ! Set request parameters.
        if (present(url))     request%url     = trim(url)
        if (present(method))  request%method  = method
        if (present(accept))  request%accept  = trim(accept)
        if (present(deflate)) request%deflate = deflate

        if (present(username) .and. present(password)) then
            request%auth     = RPC_AUTH_BASIC
            request%username = trim(username)
            request%password = trim(password)
        end if

        if (request%method == RPC_METHOD_POST) then
            if (present(content_type)) request%content_type = trim(content_type)
            if (present(payload))      request%payload      = payload
        end if

        rc = rpc_request_single(request, response)
    end function dm_rpc_request_single

    integer function dm_rpc_send_type(request, response, type, url, username, password, deflate) result(rc)
        !! Sends a single derived type in Namelist format to a given URL, with
        !! optional authentication and deflate compression. The URL has to be
        !! the API endpoint that accepts HTTP POST requests.
        !!
        !! The dummy argument `type` may be of derived type `beat_type`,
        !! `log_type`, `node_type`, `observ_type`, `sensor_type`, or
        !! `target_type`. The function returns `E_INVALID` on any other type.
        type(rpc_request_type),  intent(inout)        :: request  !! RPC request type.
        type(rpc_response_type), intent(out)          :: response !! RPC response type.
        class(*),                intent(inout)        :: type     !! Derived type.
        character(len=*),        intent(in), optional :: url      !! URL of RPC API (may include port).
        character(len=*),        intent(in), optional :: username !! HTTP Basic Auth user name.
        character(len=*),        intent(in), optional :: password !! HTTP Basic Auth password.
        logical,                 intent(in), optional :: deflate  !! Deflate compression.

        character(len=NML_BEAT_LEN)   :: payload_beat
        character(len=NML_LOG_LEN)    :: payload_log
        character(len=NML_NODE_LEN)   :: payload_node
        character(len=NML_OBSERV_LEN) :: payload_observ
        character(len=NML_SENSOR_LEN) :: payload_sensor
        character(len=NML_TARGET_LEN) :: payload_target

        request%accept       = MIME_TEXT
        request%content_type = MIME_NML
        request%method       = RPC_METHOD_POST

        if (present(url))     request%url     = url
        if (present(deflate)) request%deflate = deflate

        if (present(username) .and. present(password)) then
            request%auth     = RPC_AUTH_BASIC
            request%username = trim(username)
            request%password = trim(password)
        end if

        ! Convert derived type to Namelist representation.
        if (request%deflate) then
            select type (t => type)
                type is (beat_type)
                    rc = dm_nml_from(t, payload_beat)
                    if (dm_is_error(rc)) return
                    rc = dm_z_compress(payload_beat, request%payload)
                type is (log_type)
                    rc = dm_nml_from(t, payload_log)
                    if (dm_is_error(rc)) return
                    rc = dm_z_compress(payload_log, request%payload)
                type is (node_type)
                    rc = dm_nml_from(t, payload_node)
                    if (dm_is_error(rc)) return
                    rc = dm_z_compress(payload_node, request%payload)
                type is (observ_type)
                    rc = dm_nml_from(t, payload_observ)
                    if (dm_is_error(rc)) return
                    rc = dm_z_compress(payload_observ, request%payload)
                type is (sensor_type)
                    rc = dm_nml_from(t, payload_sensor)
                    if (dm_is_error(rc)) return
                    rc = dm_z_compress(payload_sensor, request%payload)
                type is (target_type)
                    rc = dm_nml_from(t, payload_target)
                    if (dm_is_error(rc)) return
                    rc = dm_z_compress(payload_target, request%payload)
                class default
                    rc = E_TYPE
            end select
        else
            select type (t => type)
                type is (beat_type)
                    rc = dm_nml_from(t, request%payload, len(payload_beat))
                type is (log_type)
                    rc = dm_nml_from(t, request%payload, len(payload_log))
                type is (node_type)
                    rc = dm_nml_from(t, request%payload, len(payload_node))
                type is (observ_type)
                    rc = dm_nml_from(t, request%payload, len(payload_observ))
                type is (sensor_type)
                    rc = dm_nml_from(t, request%payload, len(payload_sensor))
                type is (target_type)
                    rc = dm_nml_from(t, request%payload, len(payload_target))
                class default
                    rc = E_TYPE
            end select
        end if

        rc = rpc_request(request, response)
    end function dm_rpc_send_type

    integer function dm_rpc_send_types(requests, responses, types, url, username, password, deflate, sequential) result(rc)
        !! Sends multiple derived types concurrently in Namelist format to the
        !! given URL, with optional authentication and deflate compression.
        !! The URL has to be the API endpoint that accepts HTTP POST requests.
        !!
        !! The dummy argument `types` may be of derived type `beat_type`,
        !! `log_type`, `node_type`, `observ_type`, `sensor_type`, or
        !! `target_type`. The function returns `E_INVALID` on any other type.
        !!
        !! If `sequential` is `.true.`, the transfer will be sequentially
        !! instead of concurrently.
        type(rpc_request_type),               intent(inout)        :: requests(:)  !! RPC request type array.
        type(rpc_response_type), allocatable, intent(out)          :: responses(:) !! RPC response type array.
        class(*),                             intent(inout)        :: types(:)     !! Derived type array.
        character(len=*),                     intent(in), optional :: url          !! URL of RPC API (may include port).
        character(len=*),                     intent(in), optional :: username     !! HTTP Basic Auth user name.
        character(len=*),                     intent(in), optional :: password     !! HTTP Basic Auth password.
        logical,                              intent(in), optional :: deflate      !! Deflate compression.
        logical,                              intent(in), optional :: sequential   !! Sequential instead of concurrent transfer.

        character(len=NML_BEAT_LEN)   :: payload_beat
        character(len=NML_LOG_LEN)    :: payload_log
        character(len=NML_NODE_LEN)   :: payload_node
        character(len=NML_OBSERV_LEN) :: payload_observ
        character(len=NML_SENSOR_LEN) :: payload_sensor
        character(len=NML_TARGET_LEN) :: payload_target

        integer :: i, n, stat
        logical :: sequential_

        rc = E_BOUNDS
        if (size(requests) /= size(types)) return

        sequential_ = .false.
        if (present(sequential)) sequential_ = sequential

        n = size(requests)

        if (sequential_) then
            rc = E_ALLOC
            allocate (responses(n), stat=stat)
            if (stat /= 0) return
        end if

        ! Prepare all requests.
        do i = 1, n
            requests(i)%accept       = MIME_TEXT
            requests(i)%content_type = MIME_NML
            requests(i)%method       = RPC_METHOD_POST

            if (present(url))     requests(i)%url     = url
            if (present(deflate)) requests(i)%deflate = deflate

            if (present(username) .and. present(password)) then
                requests(i)%auth     = RPC_AUTH_BASIC
                requests(i)%username = trim(username)
                requests(i)%password = trim(password)
            end if

            ! Convert derived type to Namelist representation.
            if (requests(i)%deflate) then
                ! Set compressed payload.
                select type (t => types(i))
                    type is (beat_type)
                        rc = dm_nml_from(t, payload_beat)
                        if (dm_is_error(rc)) return
                        rc = dm_z_compress(payload_beat, requests(i)%payload)
                    type is (log_type)
                        rc = dm_nml_from(t, payload_log)
                        if (dm_is_error(rc)) return
                        rc = dm_z_compress(payload_log, requests(i)%payload)
                    type is (node_type)
                        rc = dm_nml_from(t, payload_node)
                        if (dm_is_error(rc)) return
                        rc = dm_z_compress(payload_node, requests(i)%payload)
                    type is (observ_type)
                        rc = dm_nml_from(t, payload_observ)
                        if (dm_is_error(rc)) return
                        rc = dm_z_compress(payload_observ, requests(i)%payload)
                    type is (sensor_type)
                        rc = dm_nml_from(t, payload_sensor)
                        if (dm_is_error(rc)) return
                        rc = dm_z_compress(payload_sensor, requests(i)%payload)
                    type is (target_type)
                        rc = dm_nml_from(t, payload_target)
                        if (dm_is_error(rc)) return
                        rc = dm_z_compress(payload_target, requests(i)%payload)
                    class default
                        rc = E_TYPE
                end select
            else
                ! Set uncompressed payload.
                select type (t => types(i))
                    type is (beat_type)
                        rc = dm_nml_from(t, requests(i)%payload, len(payload_beat))
                    type is (log_type)
                        rc = dm_nml_from(t, requests(i)%payload, len(payload_log))
                    type is (node_type)
                        rc = dm_nml_from(t, requests(i)%payload, len(payload_node))
                    type is (observ_type)
                        rc = dm_nml_from(t, requests(i)%payload, len(payload_observ))
                    type is (sensor_type)
                        rc = dm_nml_from(t, requests(i)%payload, len(payload_sensor))
                    type is (target_type)
                        rc = dm_nml_from(t, requests(i)%payload, len(payload_target))
                    class default
                        rc = E_TYPE
                end select
            end if

            if (dm_is_error(rc)) return
        end do

        ! Send all requests concurrently.
        if (.not. sequential_) then
            rc = rpc_request(requests, responses)
            return
        end if

        ! Send request sequentially.
        do i = 1, n
            rc = rpc_request(requests(i), responses(i))
        end do
    end function dm_rpc_send_types

    function dm_rpc_url(host, port, base, endpoint, tls) result(url)
        !! Returns allocatable string of URL to RPC-API service.
        character(len=*), intent(in)           :: host     !! IP or FQDN of remote host.
        integer,          intent(in), optional :: port     !! API port.
        character(len=*), intent(in), optional :: base     !! API base path.
        character(len=*), intent(in), optional :: endpoint !! API endpoint.
        logical,          intent(in), optional :: tls      !! TLS encryption (HTTPS).

        character(len=:), allocatable :: url
        logical                       :: tls_

        tls_ = .false.
        if (present(tls)) tls_ = tls

        if (tls_) then
            url = 'https://' // trim(host)
        else
            url = 'http://' // trim(host)
        end if

        if (present(port)) then
            if (port > 0) url = url // ':' // dm_itoa(port)
        end if

        if (present(base)) then
            url = url // trim(base)
        else
            url = url // RPC_BASE
        end if

        if (present(endpoint)) url = url // trim(endpoint)
    end function dm_rpc_url

    integer(kind=c_size_t) function dm_rpc_write_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable write callback function for libcurl. Writes the
        !! received response chunks to `rpc_response_type` pointer that has to
        !! be passed through C pointer `data`.
        type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of the response.
        integer(kind=c_size_t), intent(in), value :: sz    !! Always 1.
        integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the response chunk.
        type(c_ptr),            intent(in), value :: data  !! C pointer to argument passed by caller.

        character(len=:), allocatable    :: chunk
        type(rpc_response_type), pointer :: response

        n = int(0, kind=c_size_t)

        if (.not. c_associated(ptr)) return
        if (.not. c_associated(data)) return

        call c_f_pointer(data, response)
        if (.not. allocated(response%payload)) response%payload = ''
        call c_f_str_ptr(ptr, chunk, nmemb)
        response%payload = response%payload // chunk

        n = nmemb
    end function dm_rpc_write_callback

    subroutine dm_rpc_destroy()
        !! Cleans-up libcurl.
        call curl_global_cleanup()
    end subroutine dm_rpc_destroy

    impure elemental subroutine dm_rpc_reset(request)
        !! Auxiliary destructor routine to free allocated request memory.
        !! Cleans-up the cURL handles of the request.
        type(rpc_request_type), intent(inout) :: request !! Request type.

        if (c_associated(request%list_ptr)) call curl_slist_free_all(request%list_ptr)
        if (c_associated(request%curl_ptr)) call curl_easy_cleanup(request%curl_ptr)

        request = rpc_request_type()
    end subroutine dm_rpc_reset

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function rpc_request_multi(requests, responses) result(rc)
        !! Sends multiple HTTP requests by calling libcurl.
        integer, parameter :: POLL_TIMEOUT = 1000 !! Poll timeout in msec.

        type(rpc_request_type),               intent(inout) :: requests(:)  !! Request type array.
        type(rpc_response_type), allocatable, intent(out)   :: responses(:) !! Response type array.

        integer :: error, i, n, stat
        integer :: idx, nfds, nqueued, nrun

        type(c_ptr) :: msg_ptr
        type(c_ptr) :: multi_ptr

        type(curl_msg), pointer :: msg

        n = size(requests)

        ! Allocate response array.
        rc = E_ALLOC
        allocate (responses(n), stat=stat)
        if (stat /= 0) return

        rc = E_EMPTY
        if (n == 0) return

        ! Create and prepare libcurl stacks.
        msg_ptr   = c_null_ptr
        multi_ptr = c_null_ptr

        curl_block: block
            ! Create and prepare transfer handles.
            rc = E_IO

            do i = 1, n
                if (.not. c_associated(requests(i)%curl_ptr)) then
                    requests(i)%curl_ptr = curl_easy_init()
                    if (.not. c_associated(requests(i)%curl_ptr)) exit curl_block
                end if

                rc = rpc_request_prepare(requests(i), responses(i))
                if (dm_is_error(rc)) exit curl_block
            end do

            ! Create multi stack and add individual transfers.
            multi_ptr = curl_multi_init()
            if (.not. c_associated(multi_ptr)) exit curl_block

            do i = 1, n
                stat = curl_multi_add_handle(multi_ptr, requests(i)%curl_ptr)
                rc = dm_rpc_error_multi(stat)
                if (dm_is_error(rc)) exit curl_block
            end do

            ! Perform transfers.
            nrun = 1

            do while (nrun > 0)
                error = curl_multi_perform(multi_ptr, nrun)
                if (error /= CURLM_OK) exit curl_block

                ! Wait for activity, timeout, or "nothing".
                if (nrun > 0) then
                    nfds = 0
                    stat = curl_multi_poll(multi_ptr, c_null_ptr, 0, POLL_TIMEOUT, nfds)
                    if (stat /= CURLM_OK) exit curl_block
                end if
            end do

            rc = dm_rpc_error_multi(error)

            ! Get status of each transfer.
            do
                ! If result is NULL, no more messages are remaining.
                msg_ptr = curl_multi_info_read(multi_ptr, nqueued)
                if (.not. c_associated(msg_ptr)) exit

                call c_f_pointer(msg_ptr, msg)
                if (msg%msg /= CURLMSG_DONE) cycle

                idx = 0

                ! Find request handle index.
                do i = 1, n
                    if (c_associated(msg%easy_handle, requests(i)%curl_ptr)) then
                        idx = i
                        exit
                    end if
                end do

                if (idx == 0) cycle
                responses(idx)%error_curl = int(msg%result)
            end do

            ! Get response info and clean-up requests.
            do i = 1, n
                ! Get HTTP response code.
                stat = curl_easy_getinfo(requests(i)%curl_ptr, CURLINFO_RESPONSE_CODE, responses(i)%code)

                ! Get connection info.
                stat = curl_easy_getinfo(requests(i)%curl_ptr, CURLINFO_CONTENT_TYPE, responses(i)%content_type)

                ! Get transmission time.
                stat = curl_easy_getinfo(requests(i)%curl_ptr, CURLINFO_TOTAL_TIME, responses(i)%total_time)

                ! Set error code and message.
                if (responses(i)%error_curl /= CURLE_OK) then
                    responses(i)%error         = dm_rpc_error(responses(i)%error_curl)
                    responses(i)%error_message = curl_easy_strerror(responses(i)%error_curl)
                else
                    responses(i)%error         = E_NONE
                    responses(i)%error_message = ''
                end if

                ! Clean-up requests.
                if (.not. allocated(responses(i)%content_type)) responses(i)%content_type = ''
                if (.not. allocated(responses(i)%payload)) responses(i)%payload = ''

                stat = curl_multi_remove_handle(multi_ptr, requests(i)%curl_ptr)
                call curl_slist_free_all(requests(i)%list_ptr)
                call curl_easy_cleanup(requests(i)%curl_ptr)

                requests(i)%list_ptr = c_null_ptr
                requests(i)%curl_ptr = c_null_ptr
            end do
        end block curl_block

        stat = curl_multi_cleanup(multi_ptr)
    end function rpc_request_multi

    integer function rpc_request_prepare(request, response) result(rc)
        !! Prepares a request by setting the necessary libcurl options.
        type(rpc_request_type),  target, intent(inout) :: request  !! Request type.
        type(rpc_response_type), target, intent(inout) :: response !! Response type.

        integer :: stat

        rc = E_INVALID
        if (.not. c_associated(request%curl_ptr)) return

        if (c_associated(request%list_ptr)) then
            call curl_slist_free_all(request%list_ptr)
            request%list_ptr = c_null_ptr
        end if

        if (.not. allocated(request%url)) return
        if (len_trim(request%url) == 0) return

        rc = E_RPC

        ! Set URL.
        stat = curl_easy_setopt(request%curl_ptr, CURLOPT_URL, request%url)
        if (stat /= CURLE_OK) return

        ! Set HTTP accept header.
        if (allocated(request%accept)) then
            request%list_ptr = curl_slist_append(request%list_ptr, 'Accept: ' // request%accept)
        end if

        ! Set HTTP Basic Auth header.
        if (request%auth == RPC_AUTH_BASIC) then
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_HTTPAUTH, CURLAUTH_BASIC)
            if (stat /= CURLE_OK) return

            ! User name.
            if (allocated(request%username)) then
                stat = curl_easy_setopt(request%curl_ptr, CURLOPT_USERNAME, request%username)
                if (stat /= CURLE_OK) return
            end if

            ! Password.
            if (allocated(request%password)) then
                stat = curl_easy_setopt(request%curl_ptr, CURLOPT_PASSWORD, request%password)
                if (stat /= CURLE_OK) return
            end if
        end if

        ! Set response callback.
        if (associated(request%callback)) then
            ! Set write function.
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_WRITEFUNCTION, c_funloc(request%callback))
            if (stat /= CURLE_OK) return

            ! Set write function client data.
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_WRITEDATA, c_loc(response))
            if (stat /= CURLE_OK) return
        end if

        ! Set HTTP POST method.
        post_if: if (request%method == RPC_METHOD_POST) then
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_POST, 1)
            if (stat /= CURLE_OK) return

            if (.not. allocated(request%payload)) exit post_if

            ! Pass POST data directly.
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_POSTFIELDSIZE, len(request%payload, kind=i8))
            if (stat /= CURLE_OK) return

            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_POSTFIELDS, c_loc(request%payload))
            if (stat /= CURLE_OK) return

            ! Signal deflate encoding.
            if (request%deflate) then
                request%list_ptr = curl_slist_append(request%list_ptr, 'Content-Encoding: deflate')
            end if

            ! Set content type.
            if (allocated(request%content_type)) then
                request%list_ptr = curl_slist_append(request%list_ptr, 'Content-Type: ' // request%content_type)
            end if
        end if post_if

        ! Set follow location header.
        if (request%follow_location) then
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_FOLLOWLOCATION, 1)
            if (stat /= CURLE_OK) return
        end if

        ! Set HTTP Accept header.
        stat = curl_easy_setopt(request%curl_ptr, CURLOPT_ACCEPT_ENCODING, 'deflate')
        if (stat /= CURLE_OK) return

        ! No debug messages to stdout.
        stat = curl_easy_setopt(request%curl_ptr, CURLOPT_NOSIGNAL, 1)
        if (stat /= CURLE_OK) return

        ! Set read timeout.
        stat = curl_easy_setopt(request%curl_ptr, CURLOPT_TIMEOUT, request%timeout)
        if (stat /= CURLE_OK) return

        ! Set connection timeout.
        stat = curl_easy_setopt(request%curl_ptr, CURLOPT_CONNECTTIMEOUT, request%connect_timeout)
        if (stat /= CURLE_OK) return

        ! Enable TCP keep-alive.
        stat = curl_easy_setopt(request%curl_ptr, CURLOPT_TCP_KEEPALIVE, RPC_KEEP_ALIVE)
        if (stat /= CURLE_OK) return

        ! Set TCP keep-alive idle time in seconds.
        stat = curl_easy_setopt(request%curl_ptr, CURLOPT_TCP_KEEPIDLE, RPC_KEEP_ALIVE_IDLE)
        if (stat /= CURLE_OK) return

        ! Interval time between TCP keep-alive probes in seconds.
        stat = curl_easy_setopt(request%curl_ptr, CURLOPT_TCP_KEEPINTVL, RPC_KEEP_ALIVE_INTERVAL)
        if (stat /= CURLE_OK) return

        ! Set HTTP headers.
        if (c_associated(request%list_ptr)) then
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_HTTPHEADER, request%list_ptr)
            if (stat /= CURLE_OK) return
        end if

        ! Set User Agent.
        ua_block: block
            if (allocated(request%user_agent)) then
                if (len_trim(request%user_agent) > 0) then
                    stat = curl_easy_setopt(request%curl_ptr, CURLOPT_USERAGENT, request%user_agent)
                    exit ua_block
                end if
            end if

            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_USERAGENT, RPC_USER_AGENT)
        end block ua_block

        if (stat /= CURLE_OK) return

        rc = E_NONE
    end function rpc_request_prepare

    integer function rpc_request_single(request, response) result(rc)
        !! Sends single HTTP request by calling libcurl.
        type(rpc_request_type),  intent(inout) :: request  !! Request type.
        type(rpc_response_type), intent(out)   :: response !! Response type.

        integer :: error, stat

        rc = E_IO

        if (.not. c_associated(request%curl_ptr)) then
            request%curl_ptr = curl_easy_init()
            if (.not. c_associated(request%curl_ptr)) return
        end if

        curl_block: block
            stat = CURLE_OK
            rc = rpc_request_prepare(request, response)
            if (dm_is_error(rc)) exit curl_block

            rc = E_RPC
            error = curl_easy_perform(request%curl_ptr)
            if (stat /= CURLE_OK) exit curl_block

            rc = E_NONE
        end block curl_block

        if (rc /= E_INVALID) then
            ! Get HTTP response code.
            stat = curl_easy_getinfo(request%curl_ptr, CURLINFO_RESPONSE_CODE, response%code)

            ! Get connection info.
            stat = curl_easy_getinfo(request%curl_ptr, CURLINFO_CONTENT_TYPE, response%content_type)

            ! Get transmission time.
            stat = curl_easy_getinfo(request%curl_ptr, CURLINFO_TOTAL_TIME, response%total_time)
        end if

        ! Set error code and message.
        if (error /= CURLE_OK) then
            response%error         = dm_rpc_error(error)
            response%error_curl    = error
            response%error_message = curl_easy_strerror(error)
        else
            response%error         = rc
            response%error_message = ''
        end if

        if (.not. allocated(response%content_type)) response%content_type = ''
        if (.not. allocated(response%payload))      response%payload      = ''

        call curl_slist_free_all(request%list_ptr)
        call curl_easy_cleanup(request%curl_ptr)

        request%list_ptr = c_null_ptr
        request%curl_ptr = c_null_ptr
    end function rpc_request_single
end module dm_rpc
