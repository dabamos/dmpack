! Author:  Philipp Engel
! Licence: ISC
module dm_rpc
    !! Abstraction layer for Remote Procedure Calls (RPCs) over HTTP,
    !! using cURL.
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_beat
    use :: dm_error
    use :: dm_http
    use :: dm_log
    use :: dm_mime
    use :: dm_nml
    use :: dm_node
    use :: dm_observ
    use :: dm_sensor
    use :: dm_target
    use :: dm_type
    use :: dm_util
    use :: dm_version
    use :: dm_z
    implicit none (type, external)
    private

    character(len=*), parameter, public :: RPC_BASE_DEFAULT = '/api/v1'                      !! Base path of dmapi service.
    character(len=*), parameter, public :: RPC_USER_AGENT   = 'DMPACK ' // DM_VERSION_STRING !! User agent of RPC client.

    integer, parameter, public :: RPC_AUTH_NONE  = 0 !! No authentication.
    integer, parameter, public :: RPC_AUTH_BASIC = 1 !! HTTP Basic Auth.

    integer, parameter, public :: RPC_METHOD_GET  = 0 !! HTTP GET method.
    integer, parameter, public :: RPC_METHOD_POST = 1 !! HTTP POST method.

    integer, parameter, public :: RPC_KEEP_ALIVE          = 1   !! Enable TCP keep-alive.
    integer, parameter, public :: RPC_KEEP_ALIVE_IDLE     = 120 !! TCP keep-alive idle time in seconds.
    integer, parameter, public :: RPC_KEEP_ALIVE_INTERVAL = 60  !! Interval time between TCP keep-alive probes in seconds.

    abstract interface
        function rpc_callback(ptr, size, nmemb, data) bind(c) result(n)
            !! Abstract read/write callback for libcurl.
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of the response.
            integer(kind=c_size_t), intent(in), value :: size  !! Always 1.
            integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the response chunk.
            type(c_ptr),            intent(in), value :: data  !! C pointer to client data passed by caller.
            integer(kind=c_size_t)                    :: n     !! Function return value.
        end function rpc_callback
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
        integer                       :: auth            = RPC_AUTH_NONE  !! HTTP Auth.
        integer                       :: method          = RPC_METHOD_GET !! HTTP method (GET, POST).
        integer                       :: timeout         = 30             !! Timeout in seconds.
        integer                       :: connect_timeout = 30             !! Connection timeout in seconds.
        logical                       :: deflate         = .false.        !! Use deflate compression.
        logical                       :: follow_location = .true.         !! Follow HTTP 3xx redirects.
        character(len=:), pointer     :: payload         => null()        !! Pointer to request payload.
        character(len=:), allocatable :: content_type                     !! Request payload type (MIME).
        character(len=:), allocatable :: accept                           !! HTTP Accept header.
        character(len=:), allocatable :: username                         !! HTTP Basic Auth user name.
        character(len=:), allocatable :: password                         !! HTTP Basic Auth password.
        character(len=:), allocatable :: url                              !! Request URL.
        procedure(rpc_callback), &
            pointer, nopass           :: callback        => null()        !! C-interoperable write callback function.
    end type rpc_request_type

    interface dm_rpc_send
        !! Generic RPC send function.
        module procedure :: rpc_send_beat
        module procedure :: rpc_send_log
        module procedure :: rpc_send_node
        module procedure :: rpc_send_observ
        module procedure :: rpc_send_sensor
        module procedure :: rpc_send_target
    end interface

    public :: dm_rpc_destroy
    public :: dm_rpc_error
    public :: dm_rpc_init
    public :: dm_rpc_request
    public :: dm_rpc_reset
    public :: dm_rpc_send
    public :: dm_rpc_url

    private :: rpc_callback
    private :: rpc_request
    private :: rpc_write_callback

    private :: rpc_send_beat
    private :: rpc_send_log
    private :: rpc_send_node
    private :: rpc_send_observ
    private :: rpc_send_sensor
    private :: rpc_send_target
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_rpc_error(curl_error) result(rc)
        !! Converts cURL error code into DMPACK error code.
        integer, intent(in) :: curl_error !! cURL error code.

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

    integer function dm_rpc_init() result(rc)
        !! Initialises libcurl backend.
        rc = E_RPC
        if (curl_global_init(CURL_GLOBAL_DEFAULT) /= CURLE_OK) return
        rc = E_NONE
    end function dm_rpc_init

    integer function dm_rpc_request(request, response, accept, username, password, payload, &
                                    content_type, deflate, method, url) result(rc)
        !! Sends HTTP request with GET or POST method, and optional deflate compression.
        type(rpc_request_type),   intent(inout)           :: request      !! RPC request type.
        type(rpc_response_type),  intent(out)             :: response     !! RPC response type.
        character(len=*),         intent(in),    optional :: accept       !! HTTP accept header.
        character(len=*),         intent(in),    optional :: username     !! HTTP Basic Auth user name.
        character(len=*),         intent(in),    optional :: password     !! HTTP Basic Auth password.
        character(len=*), target, intent(inout), optional :: payload      !! For POST only.
        character(len=*),         intent(in),    optional :: content_type !! For POST only.
        logical,                  intent(in),    optional :: deflate      !! Deflate compression.
        integer,                  intent(in),    optional :: method       !! `RPC_METHOD_GET` or `RPC_METHOD_POST`.
        character(len=*),         intent(in),    optional :: url          !! URL of RPC API (may include port).

        ! Set request parameters.
        if (present(accept)) request%accept = trim(accept)

        auth_if: if (present(username)) then
            if (len_trim(username) == 0) exit auth_if
            request%auth     = RPC_AUTH_BASIC
            request%username = trim(username)
            if (present(password)) request%password = trim(password)
        end if auth_if

        if (present(deflate)) request%deflate = deflate
        if (present(method))  request%method  = method
        if (present(url))     request%url     = trim(url)

        if (request%method == RPC_METHOD_POST) then
            if (present(content_type)) request%content_type = trim(content_type)
            if (present(payload))      request%payload      => payload
        end if

        request%callback => rpc_write_callback
        rc = rpc_request(request, response)
    end function dm_rpc_request

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
            url = url // RPC_BASE_DEFAULT
        end if

        if (present(endpoint)) url = url // trim(endpoint)
    end function dm_rpc_url

    subroutine dm_rpc_destroy()
        !! Cleans-up libcurl handle.

        call curl_global_cleanup()
    end subroutine dm_rpc_destroy

    pure elemental subroutine dm_rpc_reset(request)
        !! Auxiliary destructor routine to free allocated request memory.
        type(rpc_request_type), intent(inout) :: request

        request = rpc_request_type()
    end subroutine dm_rpc_reset

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function rpc_request(request, response) result(rc)
        !! Sends HTTP request by calling libcurl.
        type(rpc_request_type),          intent(in)  :: request  !! Request type.
        type(rpc_response_type), target, intent(out) :: response !! Response type.

        integer     :: er ! cURL error.
        type(c_ptr) :: curl_ptr, list_ptr

        rc = E_IO

        curl_ptr = curl_easy_init()
        if (.not. c_associated(curl_ptr)) return

        list_ptr = c_null_ptr

        curl_block: block
            ! Prepare request.
            rc = E_INVALID
            er = CURLE_OK

            ! Set URL.
            if (.not. allocated(request%url)) exit curl_block
            if (len_trim(request%url) == 0) exit curl_block
            er = curl_easy_setopt(curl_ptr, CURLOPT_URL, request%url)
            if (er /= CURLE_OK) exit curl_block

            ! Set HTTP accept header.
            if (allocated(request%accept)) then
                list_ptr = curl_slist_append(list_ptr, 'Accept: ' // request%accept)
            end if

            ! Set HTTP Basic Auth header.
            if (request%auth == RPC_AUTH_BASIC) then
                er = curl_easy_setopt(curl_ptr, CURLOPT_HTTPAUTH, CURLAUTH_BASIC)
                if (er /= CURLE_OK) exit curl_block

                ! User name.
                if (allocated(request%username)) then
                    er = curl_easy_setopt(curl_ptr, CURLOPT_USERNAME, request%username)
                    if (er /= CURLE_OK) exit curl_block
                end if

                ! Password.
                if (allocated(request%password)) then
                    er = curl_easy_setopt(curl_ptr, CURLOPT_PASSWORD, request%password)
                    if (er /= CURLE_OK) exit curl_block
                end if
            end if

            ! Set response callback.
            if (associated(request%callback)) then
                ! Set write function.
                er = curl_easy_setopt(curl_ptr, CURLOPT_WRITEFUNCTION, c_funloc(request%callback))
                if (er /= CURLE_OK) exit curl_block

                ! Set write function client data.
                er = curl_easy_setopt(curl_ptr, CURLOPT_WRITEDATA, c_loc(response))
                if (er /= CURLE_OK) exit curl_block
            end if

            ! Set HTTP POST method.
            post_if: if (request%method == RPC_METHOD_POST) then
                er = curl_easy_setopt(curl_ptr, CURLOPT_POST, 1)
                if (er /= CURLE_OK) exit curl_block
                if (.not. associated(request%payload)) exit post_if

                ! Pass POST data directly.
                er = curl_easy_setopt(curl_ptr, CURLOPT_POSTFIELDSIZE, len(request%payload, kind=i8))
                if (er /= CURLE_OK) exit curl_block

                er = curl_easy_setopt(curl_ptr, CURLOPT_POSTFIELDS, c_loc(request%payload))
                if (er /= CURLE_OK) exit curl_block

                ! Signal deflate encoding.
                if (request%deflate) then
                    list_ptr = curl_slist_append(list_ptr, 'Content-Encoding: deflate')
                end if

                ! Set content type.
                if (allocated(request%content_type)) then
                    list_ptr = curl_slist_append(list_ptr, 'Content-Type: ' // request%content_type)
                end if
            end if post_if

            ! Set follow location header.
            if (request%follow_location) then
                er = curl_easy_setopt(curl_ptr, CURLOPT_FOLLOWLOCATION, 1)
                if (er /= CURLE_OK) exit curl_block
            end if

            ! Set HTTP Accept header.
            er = curl_easy_setopt(curl_ptr, CURLOPT_ACCEPT_ENCODING, 'deflate')
            if (er /= CURLE_OK) exit curl_block

            ! No debug messages to stdout.
            er = curl_easy_setopt(curl_ptr, CURLOPT_NOSIGNAL, 1)
            if (er /= CURLE_OK) exit curl_block

            ! Set read timeout.
            er = curl_easy_setopt(curl_ptr, CURLOPT_TIMEOUT, request%timeout)
            if (er /= CURLE_OK) exit curl_block

            ! Set connection timeout.
            er = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT, request%connect_timeout)
            if (er /= CURLE_OK) exit curl_block

            ! Enable TCP keep-alive.
            er = curl_easy_setopt(curl_ptr, CURLOPT_TCP_KEEPALIVE, RPC_KEEP_ALIVE)
            if (er /= CURLE_OK) exit curl_block

            ! Set TCP keep-alive idle time in seconds.
            er = curl_easy_setopt(curl_ptr, CURLOPT_TCP_KEEPIDLE, RPC_KEEP_ALIVE_IDLE)
            if (er /= CURLE_OK) exit curl_block

            ! Interval time between TCP keep-alive probes in seconds.
            er = curl_easy_setopt(curl_ptr, CURLOPT_TCP_KEEPINTVL, RPC_KEEP_ALIVE_INTERVAL)
            if (er /= CURLE_OK) exit curl_block

            ! Set HTTP headers.
            if (c_associated(list_ptr)) then
                er = curl_easy_setopt(curl_ptr, CURLOPT_HTTPHEADER, list_ptr)
                if (er /= CURLE_OK) exit curl_block
            end if

            ! Set User Agent.
            er = curl_easy_setopt(curl_ptr, CURLOPT_USERAGENT, RPC_USER_AGENT)
            if (er /= CURLE_OK) exit curl_block

            ! Send request.
            er = curl_easy_perform(curl_ptr)
            if (er /= CURLE_OK) exit curl_block

            ! Get connection info.
            er = curl_easy_getinfo(curl_ptr, CURLINFO_CONTENT_TYPE, response%content_type)
            if (er /= CURLE_OK) exit curl_block

            ! Get HTTP response code.
            er = curl_easy_getinfo(curl_ptr, CURLINFO_RESPONSE_CODE, response%code)
            if (er /= CURLE_OK) exit curl_block

            ! Get transmission time.
            er = curl_easy_getinfo(curl_ptr, CURLINFO_TOTAL_TIME, response%total_time)
            if (er /= CURLE_OK) exit curl_block

            rc = E_NONE
        end block curl_block

        ! Set error code and message.
        if (er /= CURLE_OK) then
            response%error         = dm_rpc_error(er)
            response%error_curl    = er
            response%error_message = curl_easy_strerror(er)
        else
            response%error         = rc
            response%error_message = ''
        end if

        if (.not. allocated(response%content_type)) response%content_type = ''
        if (.not. allocated(response%payload)) response%payload = ''

        call curl_slist_free_all(list_ptr)
        call curl_easy_cleanup(curl_ptr)
    end function rpc_request

    integer function rpc_send_beat(request, response, beat, url, username, password, deflate) result(rc)
        !! Sends a heartbeat to a given URL in Namelist format, with optional
        !! authentication and deflate compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests. For `dmapi`, this could be,
        !! for example: `http://localhost/api/v1/beat`.
        type(rpc_request_type),        intent(inout)         :: request       !! RPC request type.
        type(rpc_response_type),       intent(out)           :: response      !! RPC response type.
        type(beat_type),               intent(inout)         :: beat          !! Beat type.
        character(len=*),              intent(in),  optional :: url           !! URL of RPC API (may include port).
        character(len=*),              intent(in),  optional :: username      !! HTTP Basic Auth user name.
        character(len=*),              intent(in),  optional :: password      !! HTTP Basic Auth password.
        logical,                       intent(in),  optional :: deflate       !! Deflate compression.

        character(len=NML_BEAT_LEN)   :: payload
        character(len=:), allocatable :: compressed

        if (present(url)) request%url = url
        if (present(deflate)) request%deflate = deflate

        auth_if: if (present(username)) then
            if (len_trim(username) == 0) exit auth_if
            request%auth = RPC_AUTH_BASIC
            request%username = trim(username)
            if (present(password)) request%password = trim(password)
        end if auth_if

        request%accept       = MIME_TEXT
        request%content_type = MIME_NML
        request%method       = RPC_METHOD_POST

        ! Convert derived type to Namelist representation.
        rc = dm_nml_from(beat, payload)
        if (dm_is_error(rc)) return

        if (request%deflate) then
            rc = dm_z_compress(payload, compressed)
            if (dm_is_error(rc)) return
            rc = dm_rpc_request(request, response, payload=compressed)
        else
            rc = dm_rpc_request(request, response, payload=payload)
        end if
    end function rpc_send_beat

    integer function rpc_send_log(request, response, log, url, username, password, deflate) result(rc)
        !! Sends a sensor node to a given URL in Namelist format, with optional
        !! authentication and deflate compression.  The URL has to be the API
        !! endpoint that accepts HTTP POST requests. For `dmapi`, this could be,
        !! for example: `http://localhost/api/v1/log`.
        type(rpc_request_type),  intent(inout)        :: request  !! RPC request type.
        type(rpc_response_type), intent(out)          :: response !! RPC response type.
        type(log_type),          intent(inout)        :: log      !! Logtype.
        character(len=*),        intent(in), optional :: url      !! URL of RPC API (may include port).
        character(len=*),        intent(in), optional :: username !! HTTP Basic Auth user name.
        character(len=*),        intent(in), optional :: password !! HTTP Basic Auth password.
        logical,                 intent(in), optional :: deflate  !! Deflate compression.

        character(len=NML_LOG_LEN)    :: payload
        character(len=:), allocatable :: compressed

        if (present(url)) request%url = url
        if (present(deflate)) request%deflate = deflate

        auth_if: if (present(username)) then
            if (len_trim(username) == 0) exit auth_if
            request%auth = RPC_AUTH_BASIC
            request%username = trim(username)
            if (present(password)) request%password = trim(password)
        end if auth_if

        request%accept       = MIME_TEXT
        request%content_type = MIME_NML
        request%method       = RPC_METHOD_POST

        ! Convert derived type to Namelist representation.
        rc = dm_nml_from(log, payload)
        if (dm_is_error(rc)) return

        if (request%deflate) then
            rc = dm_z_compress(payload, compressed)
            if (dm_is_error(rc)) return
            rc = dm_rpc_request(request, response, payload=compressed)
        else
            rc = dm_rpc_request(request, response, payload=payload)
        end if
    end function rpc_send_log

    integer function rpc_send_node(request, response, node, url, username, password, deflate) result(rc)
        !! Sends a sensor node to a given URL in Namelist format, with optional
        !! authentication and deflate compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests. For `dmapi`, this could be,
        !! for example: `http://localhost/api/v1/node`.
        type(rpc_request_type),  intent(inout)        :: request  !! RPC request type.
        type(rpc_response_type), intent(out)          :: response !! RPC response type.
        type(node_type),         intent(inout)        :: node     !! Node type.
        character(len=*),        intent(in), optional :: url      !! URL of RPC API (may include port).
        character(len=*),        intent(in), optional :: username !! HTTP Basic Auth user name.
        character(len=*),        intent(in), optional :: password !! HTTP Basic Auth password.
        logical,                 intent(in), optional :: deflate  !! Deflate compression.

        character(len=NML_NODE_LEN)   :: payload
        character(len=:), allocatable :: compressed

        if (present(url)) request%url = url
        if (present(deflate)) request%deflate = deflate

        auth_if: if (present(username)) then
            if (len_trim(username) == 0) exit auth_if
            request%auth = RPC_AUTH_BASIC
            request%username = trim(username)
            if (present(password)) request%password = trim(password)
        end if auth_if

        request%accept       = MIME_TEXT
        request%content_type = MIME_NML
        request%method       = RPC_METHOD_POST

        ! Convert derived type to Namelist representation.
        rc = dm_nml_from(node, payload)
        if (dm_is_error(rc)) return

        if (request%deflate) then
            ! Compress Namelist before sending HTTP request.
            rc = dm_z_compress(payload, compressed)
            if (dm_is_error(rc)) return
            rc = dm_rpc_request(request, response, payload=compressed)
        else
            rc = dm_rpc_request(request, response, payload=payload)
        end if
    end function rpc_send_node

    integer function rpc_send_observ(request, response, observ, url, username, password, deflate) result(rc)
        !! Sends an observation to a given URL in Namelist format, with optional
        !! authentication and deflate compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests. For `dmapi`, this could be,
        !! for example: `http://localhost/api/v1/observ`.
        type(rpc_request_type),  intent(inout)        :: request  !! RPC request type.
        type(rpc_response_type), intent(out)          :: response !! RPC response type.
        type(observ_type),       intent(inout)        :: observ   !! Observation type.
        character(len=*),        intent(in), optional :: url      !! URL of RPC API (may include port).
        character(len=*),        intent(in), optional :: username !! HTTP Basic Auth user name.
        character(len=*),        intent(in), optional :: password !! HTTP Basic Auth password.
        logical,                 intent(in), optional :: deflate  !! Deflate compression.

        character(len=NML_OBSERV_LEN) :: payload
        character(len=:), allocatable :: compressed

        if (present(url)) request%url = url
        if (present(deflate)) request%deflate = deflate

        auth_if: if (present(username)) then
            if (len_trim(username) == 0) exit auth_if
            request%auth = RPC_AUTH_BASIC
            request%username = trim(username)
            if (present(password)) request%password = trim(password)
        end if auth_if

        request%accept       = MIME_TEXT
        request%content_type = MIME_NML
        request%method       = RPC_METHOD_POST

        ! Convert derived type to Namelist representation.
        rc = dm_nml_from(observ, payload)
        if (dm_is_error(rc)) return

        if (request%deflate) then
            rc = dm_z_compress(payload, compressed)
            if (dm_is_error(rc)) return
            rc = dm_rpc_request(request, response, payload=compressed)
        else
            rc = dm_rpc_request(request, response, payload=payload)
        end if
    end function rpc_send_observ

    integer function rpc_send_sensor(request, response, sensor, url, username, password, deflate) result(rc)
        !! Sends a sensor to a given URL in Namelist format, with optional
        !! authentication and deflate compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests. For `dmapi`, this could be,
        !! for example: `http://localhost/api/v1/sensor`.
        type(rpc_request_type),  intent(inout)        :: request  !! RPC request type.
        type(rpc_response_type), intent(out)          :: response !! RPC response type.
        type(sensor_type),       intent(inout)        :: sensor   !! Sensor type.
        character(len=*),        intent(in), optional :: url      !! URL of RPC API (may include port).
        character(len=*),        intent(in), optional :: username !! HTTP Basic Auth user name.
        character(len=*),        intent(in), optional :: password !! HTTP Basic Auth password.
        logical,                 intent(in), optional :: deflate  !! Deflate compression.

        character(len=NML_SENSOR_LEN) :: payload
        character(len=:), allocatable :: compressed

        if (present(url)) request%url= url
        if (present(deflate)) request%deflate = deflate

        auth_if: if (present(username)) then
            if (len_trim(username) == 0) exit auth_if
            request%auth     = RPC_AUTH_BASIC
            request%username = trim(username)
            if (present(password)) request%password = trim(password)
        end if auth_if

        request%accept       = MIME_TEXT
        request%content_type = MIME_NML
        request%method       = RPC_METHOD_POST

        ! Convert derived type to Namelist representation.
        rc = dm_nml_from(sensor, payload)
        if (dm_is_error(rc)) return

        if (request%deflate) then
            rc = dm_z_compress(payload, compressed)
            if (dm_is_error(rc)) return
            rc = dm_rpc_request(request, response, payload=compressed)
        else
            rc = dm_rpc_request(request, response, payload=payload)
        end if
    end function rpc_send_sensor

    integer function rpc_send_target(request, response, target, url, username, password, deflate) result(rc)
        !! Sends a sensor target to a given URL in Namelist format, with optional
        !! authentication and deflate compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests. For `dmapi`, this could be,
        !! for example: `http://localhost/api/v1/target`.
        type(rpc_request_type),  intent(inout)        :: request  !! RPC request type.
        type(rpc_response_type), intent(out)          :: response !! RPC response type.
        type(target_type),       intent(inout)        :: target   !! Target type.
        character(len=*),        intent(in), optional :: url      !! URL of RPC API (may include port).
        character(len=*),        intent(in), optional :: username !! HTTP Basic Auth user name.
        character(len=*),        intent(in), optional :: password !! HTTP Basic Auth password.
        logical,                 intent(in), optional :: deflate  !! Deflate compression.

        character(len=NML_TARGET_LEN) :: payload
        character(len=:), allocatable :: compressed

        if (present(url)) request%url= url
        if (present(deflate)) request%deflate = deflate

        auth_if: if (present(username)) then
            if (len_trim(username) == 0) exit auth_if
            request%auth     = RPC_AUTH_BASIC
            request%username = trim(username)
            if (present(password)) request%password = trim(password)
        end if auth_if

        request%accept       = MIME_TEXT
        request%content_type = MIME_NML
        request%method       = RPC_METHOD_POST

        ! Convert derived type to Namelist representation.
        rc = dm_nml_from(target, payload)
        if (dm_is_error(rc)) return

        if (request%deflate) then
            rc = dm_z_compress(payload, compressed)
            if (dm_is_error(rc)) return
            rc = dm_rpc_request(request, response, payload=compressed)
        else
            rc = dm_rpc_request(request, response, payload=payload)
        end if
    end function rpc_send_target

    integer(kind=c_size_t) function rpc_write_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable write callback function for libcurl.
        use :: curl
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
    end function rpc_write_callback
end module dm_rpc
