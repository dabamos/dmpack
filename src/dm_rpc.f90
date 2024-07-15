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
    !! call dm_error_out(rc, fatal=.true.)
    !!
    !! url = dm_rpc_url('localhost', port=80, endpoint=RPC_ROUTE_OBSERV)
    !! rc  = dm_rpc_send(request, response, observ, url)
    !!
    !! call dm_error_out(rc)
    !! call dm_rpc_destroy()
    !! ```
    !!
    !! The URL returned by `dm_rpc_url()` will equal
    !! `http://localhost:80/api/v1/observ` in this case.
    !!
    !! The procedures `dm_rpc_init()` and `dm_rpc_destroy()` have to be called
    !! once per process, and only if neither the MQTT nor the mail backend is
    !! initialised already.
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_error
    use :: dm_http
    use :: dm_kind
    use :: dm_mime
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
    logical, parameter, public :: RPC_KEEP_ALIVE          = .true. !! Enable TCP keep-alive.
    integer, parameter, public :: RPC_KEEP_ALIVE_IDLE     = 120    !! TCP keep-alive idle time in seconds.
    integer, parameter, public :: RPC_KEEP_ALIVE_INTERVAL = 60     !! Interval time between TCP keep-alive probes in seconds.

    abstract interface
        function dm_rpc_callback(ptr, size, nmemb, data) bind(c)
            !! C-interoperable read/write callback for libcurl.
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
        integer                                     :: compression     = Z_TYPE_NONE    !! Use deflate or zstd compression (`Z_TYPE_*`).
        integer                                     :: connect_timeout = 30             !! Connection timeout in seconds.
        integer                                     :: timeout         = 30             !! Timeout in seconds.
        logical                                     :: follow_location = .true.         !! Follow HTTP 3xx redirects.
        character(len=:), allocatable               :: payload                          !! Request payload.
        character(len=:), allocatable               :: content_type                     !! Request payload type (MIME).
        character(len=:), allocatable               :: accept                           !! HTTP Accept header.
        character(len=:), allocatable               :: username                         !! HTTP Basic Auth user name.
        character(len=:), allocatable               :: password                         !! HTTP Basic Auth password.
        character(len=:), allocatable               :: url                              !! Request URL.
        character(len=:), allocatable               :: user_agent                       !! User Agent.
        procedure(dm_rpc_callback), pointer, nopass :: callback        => null()        !! C-interoperable write callback function.
        type(c_ptr), private                        :: curl_ptr        = c_null_ptr     !! cURL handle.
        type(c_ptr), private                        :: list_ptr        = c_null_ptr     !! cURL list handle.
    end type rpc_request_type

    interface rpc_request
        !! Generic RPC request function.
        module procedure :: rpc_request_multi
        module procedure :: rpc_request_single
    end interface rpc_request

    interface dm_rpc_request
        !! Generic RPC request function.
        module procedure :: dm_rpc_request_multi
        module procedure :: dm_rpc_request_single
    end interface dm_rpc_request

    interface dm_rpc_send
        !! Generic RPC send function.
        module procedure :: dm_rpc_send_type
        module procedure :: dm_rpc_send_types
    end interface dm_rpc_send

    public :: dm_rpc_callback
    public :: dm_rpc_destroy
    public :: dm_rpc_error
    public :: dm_rpc_error_message
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
    public :: dm_rpc_version

    public :: dm_rpc_write_callback

    private :: rpc_request
    private :: rpc_request_multi
    private :: rpc_request_prepare
    private :: rpc_request_single
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    function dm_rpc_version() result(version)
        !! Returns version number of libcurl an linked libreries as allocatable
        !! string.
        character(len=:), allocatable :: version

        version = curl_version()
    end function dm_rpc_version

    integer function dm_rpc_error(error_curl) result(rc)
        !! Converts cURL easy stack error code to DMPACK error code.
        integer, intent(in) :: error_curl !! cURL easy error code.

        select case (error_curl)
            case (CURLE_OK)
                rc = E_NONE

            case (CURLE_UNSUPPORTED_PROTOCOL,  &
                  CURLE_FAILED_INIT,           &
                  CURLE_URL_MALFORMAT,         &
                  CURLE_NOT_BUILT_IN,          &
                  CURLE_BAD_FUNCTION_ARGUMENT, &
                  CURLE_UNKNOWN_OPTION,        &
                  CURLE_BAD_CONTENT_ENCODING)
                rc = E_INVALID

            case (CURLE_COULDNT_RESOLVE_PROXY, &
                  CURLE_COULDNT_RESOLVE_HOST,  &
                  CURLE_COULDNT_CONNECT)
                rc = E_RPC_CONNECT

            case (CURLE_WEIRD_SERVER_REPLY)
                rc = E_RPC_API

            case (CURLE_REMOTE_ACCESS_DENIED, &
                  CURLE_AUTH_ERROR)
                rc = E_RPC_AUTH

            case (CURLE_WRITE_ERROR)
                rc = E_WRITE

            case (CURLE_READ_ERROR)
                rc = E_READ

            case (CURLE_OUT_OF_MEMORY)
                rc = E_MEMORY

            case (CURLE_OPERATION_TIMEDOUT)
                rc = E_TIMEOUT

            case (CURLE_GOT_NOTHING)
                rc = E_EMPTY

            case (CURLE_SSL_CONNECT_ERROR,        &
                  CURLE_SSL_ENGINE_NOTFOUND,      &
                  CURLE_SSL_ENGINE_SETFAILED,     &
                  CURLE_SSL_CERTPROBLEM,          &
                  CURLE_SSL_CIPHER,               &
                  CURLE_PEER_FAILED_VERIFICATION, &
                  CURLE_SSL_ENGINE_INITFAILED,    &
                  CURLE_SSL_CACERT_BADFILE,       &
                  CURLE_SSL_SHUTDOWN_FAILED,      &
                  CURLE_SSL_CRL_BADFILE,          &
                  CURLE_SSL_ISSUER_ERROR,         &
                  CURLE_SSL_PINNEDPUBKEYNOTMATCH, &
                  CURLE_SSL_INVALIDCERTSTATUS,    &
                  CURLE_SSL_CLIENTCERT)
                rc = E_RPC_SSL

            case (CURLE_FILESIZE_EXCEEDED)
                rc = E_LIMIT

            case (CURLE_REMOTE_FILE_NOT_FOUND)
                rc = E_NOT_FOUND

            case default
                rc = E_RPC
        end select
    end function dm_rpc_error

    function dm_rpc_error_message(error_curl) result(message)
        !! Return message associated with given cURL error code as allocatable
        !! character string.
        integer, intent(in)           :: error_curl !! cURL error code.
        character(len=:), allocatable :: message    !! Error message.

        message = curl_easy_strerror(error_curl)
    end function dm_rpc_error_message

    integer function dm_rpc_error_multi(multi_error) result(rc)
        !! Converts cURL multi stack error code to DMPACK error code.
        integer, intent(in) :: multi_error !! cURL multi error code.

        select case (multi_error)
            case (CURLM_OK)
                rc = E_NONE

            case (CURLM_BAD_HANDLE,            &
                  CURLM_BAD_EASY_HANDLE,       &
                  CURLM_BAD_FUNCTION_ARGUMENT, &
                  CURLM_UNKNOWN_OPTION)
                rc = E_INVALID

            case (CURLM_OUT_OF_MEMORY)
                rc = E_MEMORY

            case default
                rc = E_RPC
        end select
    end function dm_rpc_error_multi

    integer function dm_rpc_init() result(rc)
        !! Initialises RPC backend. The function returns `E_RPC` on error.
        rc = E_RPC
        if (curl_global_init(CURL_GLOBAL_DEFAULT) /= CURLE_OK) return
        rc = E_NONE
    end function dm_rpc_init

    integer function dm_rpc_request_multi(requests, responses, url, method, accept, username, password, &
                                          user_agent, compression) result(rc)
        !! Sends multiple HTTP requests by GET or POST method, with optional
        !! deflate or zstd compression.
        type(rpc_request_type),               intent(inout)        :: requests(:)  !! RPC request type array.
        type(rpc_response_type), allocatable, intent(out)          :: responses(:) !! RPC response type array.
        character(len=*),                     intent(in), optional :: url          !! URL of RPC API (may include port).
        integer,                              intent(in), optional :: method       !! `RPC_METHOD_GET` or `RPC_METHOD_POST`.
        character(len=*),                     intent(in), optional :: accept       !! HTTP Accept header.
        character(len=*),                     intent(in), optional :: username     !! HTTP Basic Auth user name.
        character(len=*),                     intent(in), optional :: password     !! HTTP Basic Auth password.
        character(len=*),                     intent(in), optional :: user_agent   !! HTTP User Agent.
        integer,                              intent(in), optional :: compression  !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).

        integer :: i

        do i = 1, size(requests)
            ! Set request parameters.
            if (.not. associated(requests(i)%callback)) requests(i)%callback => dm_rpc_write_callback

            if (present(accept))      requests(i)%accept      = trim(accept)
            if (present(method))      requests(i)%method      = method
            if (present(url))         requests(i)%url         = trim(url)
            if (present(user_agent))  requests(i)%user_agent  = trim(user_agent)
            if (present(compression)) requests(i)%compression = compression

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
                                           accept, username, password, user_agent, compression) result(rc)
        !! Sends single HTTP request by GET or POST method, and with optional
        !! deflate or zstd compression.
        type(rpc_request_type),  intent(inout)           :: request      !! RPC request type.
        type(rpc_response_type), intent(out)             :: response     !! RPC response type.
        character(len=*),        intent(in),    optional :: url          !! URL of RPC API (may include port).
        integer,                 intent(in),    optional :: method       !! `RPC_METHOD_GET` or `RPC_METHOD_POST`.
        character(len=*),        intent(inout), optional :: payload      !! Payload data (for POST only).
        character(len=*),        intent(in),    optional :: content_type !! Payload content type (for POST only).
        character(len=*),        intent(in),    optional :: accept       !! HTTP Accept header.
        character(len=*),        intent(in),    optional :: username     !! HTTP Basic Auth user name.
        character(len=*),        intent(in),    optional :: password     !! HTTP Basic Auth password.
        character(len=*),        intent(in),    optional :: user_agent   !! HTTP User Agent.
        integer,                 intent(in),    optional :: compression  !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).

        ! Set request parameters.
        if (.not. associated(request%callback)) request%callback => dm_rpc_write_callback

        if (present(url))         request%url         = trim(url)
        if (present(method))      request%method      = method
        if (present(accept))      request%accept      = trim(accept)
        if (present(user_agent))  request%user_agent  = trim(user_agent)
        if (present(compression)) request%compression = compression

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

    integer function dm_rpc_send_type(request, response, type, url, username, password, &
                                      user_agent, compression) result(rc)
        !! Sends a single derived type in Namelist format to a given URL, with
        !! optional authentication and compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests.
        !!
        !! The dummy argument `type` may be of derived type `beat_type`,
        !! `log_type`, `node_type`, `observ_type`, `sensor_type`, or
        !! `target_type`. The function returns `E_TYPE` on any other type.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_TYPE` if `type` is unsupported.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        type(rpc_request_type),  intent(inout)        :: request     !! RPC request type.
        type(rpc_response_type), intent(out)          :: response    !! RPC response type.
        class(*),                intent(inout)        :: type        !! Derived type.
        character(len=*),        intent(in), optional :: url         !! URL of RPC API (may include port).
        character(len=*),        intent(in), optional :: username    !! HTTP Basic Auth user name.
        character(len=*),        intent(in), optional :: password    !! HTTP Basic Auth password.
        character(len=*),        intent(in), optional :: user_agent  !! HTTP User Agent.
        integer,                 intent(in), optional :: compression !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).

        request%accept       = MIME_TEXT
        request%content_type = MIME_NML
        request%method       = RPC_METHOD_POST

        if (present(url))         request%url         = trim(url)
        if (present(user_agent))  request%user_agent  = trim(user_agent)
        if (present(compression)) request%compression = compression

        if (present(username) .and. present(password)) then
            request%auth     = RPC_AUTH_BASIC
            request%username = trim(username)
            request%password = trim(password)
        end if

        rc = E_INVALID
        if (.not. dm_z_valid(request%compression)) return

        rc = dm_z_compress_type(type, request%compression, request%payload)
        if (dm_is_error(rc)) return

        rc = rpc_request(request, response)
    end function dm_rpc_send_type

    integer function dm_rpc_send_types(requests, responses, types, url, username, password, &
                                       user_agent, compression, sequential) result(rc)
        !! Sends multiple derived types concurrently in Namelist format to the
        !! given URL, with optional authentication and compression. The URL
        !! has to be the API endpoint that accepts HTTP POST requests.
        !!
        !! The dummy argument `types` may be of derived type `beat_type`,
        !! `log_type`, `node_type`, `observ_type`, `sensor_type`, or
        !! `target_type`. The function returns `E_TYPE` on any other type.
        !!
        !! If `sequential` is `.true.`, the transfer will be sequentially
        !! instead of concurrently. The number of requests must match the
        !! number of types, or `E_CORRUPT` is returned.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_CORRUPT` if sizes of requests and types array mismatch.
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_TYPE` if type of `types` is unsupported.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_zstd, only: dm_zstd_destroy, zstd_context_type

        type(rpc_request_type),               intent(inout)        :: requests(:)  !! RPC request type array.
        type(rpc_response_type), allocatable, intent(out)          :: responses(:) !! RPC response type array.
        class(*),                             intent(inout)        :: types(:)     !! Derived type array.
        character(len=*),                     intent(in), optional :: url          !! URL of RPC API (may include port).
        character(len=*),                     intent(in), optional :: username     !! HTTP Basic Auth user name.
        character(len=*),                     intent(in), optional :: password     !! HTTP Basic Auth password.
        character(len=*),                     intent(in), optional :: user_agent   !! HTTP User Agent.
        integer,                              intent(in), optional :: compression  !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                              intent(in), optional :: sequential   !! Sequential instead of concurrent transfer.

        integer                 :: i, n, stat, z
        logical                 :: sequential_
        type(zstd_context_type) :: context

        rc = E_CORRUPT
        if (size(requests) /= size(types)) return

        z = Z_TYPE_NONE
        if (present(compression)) z = compression

        rc = E_INVALID
        if (.not. dm_z_valid(z)) return

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
            if (.not. associated(requests(i)%callback)) requests(i)%callback => dm_rpc_write_callback

            requests(i)%accept       = MIME_TEXT
            requests(i)%content_type = MIME_NML
            requests(i)%method       = RPC_METHOD_POST
            requests(i)%compression  = z

            if (present(url))        requests(i)%url        = trim(url)
            if (present(user_agent)) requests(i)%user_agent = trim(user_agent)

            if (present(username) .and. present(password)) then
                requests(i)%auth     = RPC_AUTH_BASIC
                requests(i)%username = trim(username)
                requests(i)%password = trim(password)
            end if

            ! Serialise and compress payload.
            if (z == Z_TYPE_ZSTD) then
                ! Use Zstandard compression context.
                rc = dm_z_compress_type(types(i), z, requests(i)%payload, context=context)
            else
                rc = dm_z_compress_type(types(i), z, requests(i)%payload)
            end if

            if (dm_is_error(rc)) exit
        end do

        ! Clean-up Zstandard context.
        if (z == Z_TYPE_ZSTD) stat = dm_zstd_destroy(context)

        if (dm_is_error(rc)) then
            allocate (responses(n), stat=stat)
            return
        end if

        ! Send requests concurrently.
        if (.not. sequential_) then
            rc = rpc_request(requests, responses)
            return
        end if

        ! Send requests sequentially.
        do i = 1, n
            rc = rpc_request(requests(i), responses(i))
        end do
    end function dm_rpc_send_types

    function dm_rpc_url(host, port, base, endpoint, tls) result(url)
        !! Returns allocatable string of URL to HTTP-RPC API endpoint. Uses the
        !! URL API of libcurl to create the URL. The base path and the endpoint
        !! must both start with a `/`.
        !!
        !! The function returns an empty string on error.
        character(len=*), intent(in)           :: host     !! IP or FQDN of remote host.
        integer,          intent(in), optional :: port     !! API port (up to 5 digits).
        character(len=*), intent(in), optional :: base     !! API base path (for example, `/api/v1`).
        character(len=*), intent(in), optional :: endpoint !! API endpoint (for example, `/observ`).
        logical,          intent(in), optional :: tls      !! TLS encryption (HTTPS).
        character(len=:), allocatable          :: url      !! HTTP-RPC API endpoint URL.

        character(len=:), allocatable :: path

        integer     :: stat
        integer     :: port_
        logical     :: tls_
        type(c_ptr) :: ptr

        tls_ = .false.
        if (present(tls)) tls_ = tls

        port_ = 0
        if (present(port)) port_ = port

        url_block: block
            ptr = curl_url()
            if (.not. c_associated(ptr)) exit url_block

            ! URL scheme.
            if (tls_) then
                stat = curl_url_set(ptr, CURLUPART_SCHEME, 'https')
                if (stat /= CURLUE_OK) exit url_block
            else
                stat = curl_url_set(ptr, CURLUPART_SCHEME, 'http')
                if (stat /= CURLUE_OK) exit url_block
            end if

            ! URL host.
            stat = curl_url_set(ptr, CURLUPART_HOST, trim(host))
            if (stat /= CURLUE_OK) exit url_block

            ! URL port.
            if (port_ > 0) then
                stat = curl_url_set(ptr, CURLUPART_PORT, dm_itoa(port_))
                if (stat /= CURLUE_OK) exit url_block
            end if

            ! Base path.
            if (present(base)) then
                if (len_trim(base) == 0) exit url_block
                if (base(1:1) /= '/') exit url_block
                path = trim(base)
            else
                path = RPC_BASE
            end if

            ! Endpoint path.
            if (present(endpoint)) then
                if (len_trim(endpoint) == 0) exit url_block
                if (endpoint(1:1) /= '/') exit url_block
                path = path // trim(endpoint)
            end if

            ! URL path.
            stat = curl_url_set(ptr, CURLUPART_PATH, path)
            if (stat /= CURLUE_OK) exit url_block

            ! Get full URL.
            stat = curl_url_get(ptr, CURLUPART_URL, url)
        end block url_block

        call curl_url_cleanup(ptr)
        if (.not. allocated(url)) url = ''
    end function dm_rpc_url

    subroutine dm_rpc_destroy()
        !! Cleans-up RPC backend.
        call curl_global_cleanup()
    end subroutine dm_rpc_destroy

    impure elemental subroutine dm_rpc_reset(request)
        !! Auxiliary destructor routine to free allocated request memory.
        !! Cleans-up the cURL handles of the request.
        type(rpc_request_type), intent(inout) :: request !! Request type.

        if (c_associated(request%list_ptr)) then
            call curl_slist_free_all(request%list_ptr)
            request%list_ptr = c_null_ptr
        end if

        if (c_associated(request%curl_ptr)) then
            call curl_easy_cleanup(request%curl_ptr)
            request%curl_ptr = c_null_ptr
        end if

        request = rpc_request_type()
    end subroutine dm_rpc_reset

    ! ******************************************************************
    ! PUBLIC CALLBACK FUNCTIONS.
    ! ******************************************************************
    integer(kind=c_size_t) function dm_rpc_write_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable write callback function for libcurl. Writes the
        !! received response chunks to `rpc_response_type` pointer that has to
        !! be passed through C pointer `data`. Do not call this function
        !! directly.
        type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of the response.
        integer(kind=c_size_t), intent(in), value :: sz    !! Always 1.
        integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the response chunk.
        type(c_ptr),            intent(in), value :: data  !! C pointer to argument passed by caller.

        character(len=:), allocatable    :: chunk
        type(rpc_response_type), pointer :: response

        n = 0_c_size_t

        if (.not. c_associated(ptr))  return
        if (.not. c_associated(data)) return

        call c_f_pointer(data, response)
        if (.not. allocated(response%payload)) allocate (character(len=0) :: response%payload)
        call c_f_str_ptr(ptr, chunk, nmemb)
        response%payload = response%payload // chunk

        n = nmemb
    end function dm_rpc_write_callback

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function rpc_request_multi(requests, responses) result(rc)
        !! Sends multiple HTTP requests by calling libcurl.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if RPC response array allocation failed.
        !! * `E_EMPTY` if no RPC requests are given.
        !! * `E_RPC` if RPC backend initialisation failed.
        !!
        !! Other DMPACK errors may occur, depending on the result of the
        !! transmission. Specific transfer error codes are returned in the
        !! responses.
        integer, parameter :: POLL_TIMEOUT = 1000 !! Poll timeout [msec].

        type(rpc_request_type),               intent(inout) :: requests(:)  !! Request type array.
        type(rpc_response_type), allocatable, intent(out)   :: responses(:) !! Response type array.

        integer                 :: error, i, n, stat
        integer                 :: idx, nfds, nqueued, nrun
        type(c_ptr)             :: msg_ptr
        type(c_ptr)             :: multi_ptr
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
            rc = E_RPC

            ! Create and prepare transfer handles.
            do i = 1, n
                ! Initialise easy handle.
                if (.not. c_associated(requests(i)%curl_ptr)) then
                    requests(i)%curl_ptr = curl_easy_init()
                    if (.not. c_associated(requests(i)%curl_ptr)) exit curl_block
                end if

                ! Prepare request.
                rc = rpc_request_prepare(requests(i), responses(i))
                if (dm_is_error(rc)) exit curl_block
            end do

            ! Create multi-stack and add individual transfers.
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
                if (error /= CURLM_OK) exit

                ! Wait for activity, timeout, or "nothing".
                if (nrun > 0) then
                    nfds = 0
                    stat = curl_multi_poll(multi_ptr, c_null_ptr, 0, POLL_TIMEOUT, nfds)
                    if (stat /= CURLM_OK) exit
                end if
            end do

            ! Get DMPACK error code from cURL error.
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
                ! Get content type of response.
                stat = curl_easy_getinfo(requests(i)%curl_ptr, CURLINFO_CONTENT_TYPE, responses(i)%content_type)
                ! Get transmission time.
                stat = curl_easy_getinfo(requests(i)%curl_ptr, CURLINFO_TOTAL_TIME, responses(i)%total_time)

                ! Set error code and message.
                if (responses(i)%error_curl /= CURLE_OK) then
                    responses(i)%error         = dm_rpc_error(responses(i)%error_curl)
                    responses(i)%error_message = dm_rpc_error_message(responses(i)%error_curl)
                else
                    responses(i)%error         = E_NONE
                    responses(i)%error_message = ''
                end if

                if (.not. allocated(responses(i)%content_type)) responses(i)%content_type = ''
                if (.not. allocated(responses(i)%payload))      responses(i)%payload      = ''

                ! Clean-up requests.
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
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if libcurl is not initialised.
        !! * `E_RPC` if request preparation failed.
        !!
        use :: dm_c,      only: dm_f_c_logical
        use :: dm_string, only: dm_string_is_empty

        type(rpc_request_type),  target, intent(inout) :: request  !! Request type.
        type(rpc_response_type), target, intent(inout) :: response !! Response type.

        integer :: stat

        rc = E_INVALID
        if (.not. c_associated(request%curl_ptr)) return

        ! Reset HTTP header list.
        if (c_associated(request%list_ptr)) then
            call curl_slist_free_all(request%list_ptr)
            request%list_ptr = c_null_ptr
        end if

        ! Validate URL.
        if (dm_string_is_empty(request%url)) return

        rc = E_RPC

        ! Set URL.
        stat = curl_easy_setopt(request%curl_ptr, CURLOPT_URL, request%url)
        if (stat /= CURLE_OK) return

        ! Set HTTP accept header.
        if (.not. dm_string_is_empty(request%accept)) then
            request%list_ptr = curl_slist_append(request%list_ptr, 'Accept: ' // request%accept)
        end if

        ! Set HTTP Basic Auth header.
        if (request%auth == RPC_AUTH_BASIC) then
            ! Enable HTTP Basic Auth.
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_HTTPAUTH, CURLAUTH_BASIC)
            if (stat /= CURLE_OK) return

            ! Set user name.
            if (.not. dm_string_is_empty(request%username)) then
                stat = curl_easy_setopt(request%curl_ptr, CURLOPT_USERNAME, request%username)
                if (stat /= CURLE_OK) return
            end if

            ! Set password.
            if (.not. dm_string_is_empty(request%password)) then
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
            ! Enable POST.
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_POST, 1)
            if (stat /= CURLE_OK) return

            ! Exit if POST payload is missing.
            if (.not. allocated(request%payload)) exit post_if

            ! Pass POST data directly.
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_POSTFIELDSIZE, len(request%payload, kind=i8))
            if (stat /= CURLE_OK) return
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_POSTFIELDS, c_loc(request%payload))
            if (stat /= CURLE_OK) return

            ! Signal content encoding (deflate, zstd).
            if (request%compression > Z_TYPE_NONE) then
                request%list_ptr = curl_slist_append(request%list_ptr, 'Content-Encoding: ' // &
                                                     dm_z_type_to_encoding(request%compression))
            end if

            ! Set content type.
            if (.not. dm_string_is_empty(request%content_type)) then
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
        stat = curl_easy_setopt(request%curl_ptr, CURLOPT_TCP_KEEPALIVE, dm_f_c_logical(RPC_KEEP_ALIVE))
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

        if (dm_string_is_empty(request%user_agent)) then
            ! Set default User Agent.
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_USERAGENT, RPC_USER_AGENT)
        else
            ! Set custom User Agent.
            stat = curl_easy_setopt(request%curl_ptr, CURLOPT_USERAGENT, trim(request%user_agent))
        end if

        if (stat /= CURLE_OK) return
        rc = E_NONE
    end function rpc_request_prepare

    integer function rpc_request_single(request, response) result(rc)
        !! Sends single HTTP request by calling libcurl. The function returns
        !! `E_RPC` on error. A more specific error code may be available in the
        !! response attribute `error`.
        type(rpc_request_type),  intent(inout) :: request  !! Request type.
        type(rpc_response_type), intent(out)   :: response !! Response type.

        integer :: error, stat

        rc = E_RPC

        ! Initialise libcurl.
        if (.not. c_associated(request%curl_ptr)) then
            request%curl_ptr = curl_easy_init()
            if (.not. c_associated(request%curl_ptr)) return
        end if

        ! Prepare and send HTTP request.
        error = CURLE_OK

        curl_block: block
            rc = rpc_request_prepare(request, response)
            if (dm_is_error(rc)) exit curl_block
            error = curl_easy_perform(request%curl_ptr)
            rc = dm_rpc_error(error)
        end block curl_block

        ! Get response info.
        if (dm_is_ok(rc)) then
            ! Get HTTP response code.
            stat = curl_easy_getinfo(request%curl_ptr, CURLINFO_RESPONSE_CODE, response%code)
            ! Get connection info.
            stat = curl_easy_getinfo(request%curl_ptr, CURLINFO_CONTENT_TYPE, response%content_type)
            ! Get transmission time.
            stat = curl_easy_getinfo(request%curl_ptr, CURLINFO_TOTAL_TIME, response%total_time)
        end if

        ! Set error code and message.
        if (dm_is_error(rc)) then
            response%error         = dm_rpc_error(error)
            response%error_curl    = error
            response%error_message = dm_rpc_error_message(error)
        else
            response%error         = rc
            response%error_message = ''
        end if

        if (.not. allocated(response%content_type)) response%content_type = ''
        if (.not. allocated(response%payload))      response%payload      = ''

        ! Clean-up.
        call curl_slist_free_all(request%list_ptr)
        call curl_easy_cleanup(request%curl_ptr)

        request%list_ptr = c_null_ptr
        request%curl_ptr = c_null_ptr
    end function rpc_request_single
end module dm_rpc
