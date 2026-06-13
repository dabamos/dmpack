! Author:  Philipp Engel
! Licence: ISC
module dm_rpc
    !! Abstraction layer for Remote Procedure Calls (RPCs) over HTTP, using
    !! libcurl.
    !!
    !! Send the observation `observ` to an HTTP-RPC API on `localhost`:
    !!
    !! ``` fortran
    !! character(:), allocatable :: url
    !! integer                   :: rc
    !! type(observ_type)         :: observ
    !! type(rpc_request_type)    :: request
    !! type(rpc_response_type)   :: response
    !!
    !! rc = dm_rpc_init()
    !! call dm_error_out(rc, fatal=.true.)
    !!
    !! url = dm_rpc_url('localhost', port=80, endpoint=RPC_ROUTE_OBSERV)
    !! rc  = dm_rpc_post(request, response, observ, url)
    !! call dm_error_out(rc)
    !!
    !! call dm_rpc_destroy(request)
    !! call dm_rpc_destroy(response)
    !! call dm_rpc_shutdown()
    !! ```
    !!
    !! The URL returned by `dm_rpc_url()` will equal
    !! `http://localhost:80/api/v2/observ` in this case. Add HTTP response
    !! header names to array `response%headers` to read them automatically:
    !!
    !! ``` fortran
    !! rc = dm_rpc_header_create(response, max_size=1)
    !! rc = dm_rpc_header_add(response, name='etag')
    !! rc = dm_rpc_post(request, response, observ, url)
    !! ```
    !!
    !! The HTTP response header `etag` is stored in `response%headers`
    !! afterwards:
    !!
    !! ``` fortran
    !! character(:), allocatable :: value
    !!
    !! rc = dm_rpc_header_get(response, 'etag', value)
    !! ```
    use, intrinsic :: iso_c_binding
    use :: curl, curl_cleanup   => curl_easy_cleanup, &
                 curl_get       => curl_easy_getinfo, &
                 curl_init      => curl_easy_init,    &
                 curl_perform   => curl_easy_perform, &
                 curl_set       => curl_easy_setopt,  &
                 curl_str_error => curl_easy_strerror
    use :: dm_error
    use :: dm_file, only: FILE_UNIT_NONE
    use :: dm_http
    use :: dm_kind
    use :: dm_mime
    use :: dm_util
    use :: dm_version
    use :: dm_z
    implicit none (type, external)
    private

    character(*), parameter, public :: RPC_BASE        = '/api/v2'                      !! Base path of dmapi service.
    character(*), parameter, public :: RPC_USER_AGENT  = 'DMPACK ' // DM_VERSION_STRING !! Default user agent of RPC client.

    character(*), parameter, public :: RPC_HEADER_TRANSFER_ID = 'dmpack-transfer-id'    !! HTTP transfer id header name.

    character(*), parameter, public :: RPC_ROUTE_BEAT   = '/beat'   !! Resolves to `/api/v2/beat`.
    character(*), parameter, public :: RPC_ROUTE_IMAGE  = '/image'  !! Resolves to `/api/v2/image`.
    character(*), parameter, public :: RPC_ROUTE_LOG    = '/log'    !! Resolves to `/api/v2/log`.
    character(*), parameter, public :: RPC_ROUTE_OBSERV = '/observ' !! Resolves to `/api/v2/observ`.
    character(*), parameter, public :: RPC_ROUTE_NODE   = '/node'   !! Resolves to `/api/v2/node`.
    character(*), parameter, public :: RPC_ROUTE_SENSOR = '/sensor' !! Resolves to `/api/v2/sensor`.
    character(*), parameter, public :: RPC_ROUTE_TARGET = '/target' !! Resolves to `/api/v2/target`.

    ! HTTP Auth.
    integer, parameter, public :: RPC_AUTH_NONE  = 0 !! No authentication.
    integer, parameter, public :: RPC_AUTH_BASIC = 1 !! HTTP Basic Auth.

    ! HTTP Method.
    integer, parameter, public :: RPC_METHOD_GET  = 0 !! HTTP GET method.
    integer, parameter, public :: RPC_METHOD_POST = 1 !! HTTP POST method.
    integer, parameter, public :: RPC_METHOD_PUT  = 2 !! HTTP PUT method.

    ! TCP Keep-Alive.
    logical, parameter, public :: RPC_KEEP_ALIVE          = .true. !! Enable TCP keep-alive.
    integer, parameter, public :: RPC_KEEP_ALIVE_IDLE     = 120    !! TCP keep-alive idle time in seconds.
    integer, parameter, public :: RPC_KEEP_ALIVE_INTERVAL = 60     !! Interval time between TCP keep-alive probes in seconds.

    abstract interface
        function dm_rpc_callback(ptr, size, nmemb, data) bind(c)
            !! C-interoperable read/write callback for libcurl.
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: ptr             !! C pointer to a chunk of the response.
            integer(c_size_t), intent(in), value :: size            !! Always 1.
            integer(c_size_t), intent(in), value :: nmemb           !! Size of the response chunk.
            type(c_ptr),       intent(in), value :: data            !! C pointer to client data passed by caller.
            integer(c_size_t)                    :: dm_rpc_callback !! Function return value.
        end function dm_rpc_callback
    end interface

    type, public :: rpc_header_type
        !! HTTP request and response header type.
        character(:), allocatable :: name  !! Header name.
        character(:), allocatable :: value !! Header value.
    end type rpc_header_type

    type, public :: rpc_response_type
        !! HTTP-RPC response type.
        integer                            :: code          = HTTP_NONE      !! HTTP response code.
        integer                            :: error         = E_NONE         !! Error code of DMPACK.
        integer                            :: error_curl    = CURLE_OK       !! Error code of libcurl easy.
        integer                            :: unit          = FILE_UNIT_NONE !! Optional file unit.
        integer(i8)                        :: last_modified = -1_i8          !! File time, -1 if unavailable [Epoch].
        real(r8)                           :: total_time    = 0.0_r8         !! Total transmission time.
        character(:),          allocatable :: error_message                  !! libcurl error message.
        character(:),          allocatable :: content_type                   !! Response payload type [MIME].
        character(:),          allocatable :: payload                        !! Response payload.
        type(rpc_header_type), allocatable :: headers(:)                     !! HTTP response header.
    end type rpc_response_type

    type, public :: rpc_request_type
        !! HTTP-RPC request type.
        integer                                     :: auth            = RPC_AUTH_NONE  !! HTTP Auth.
        integer                                     :: method          = RPC_METHOD_GET !! HTTP method (GET, POST, PUT).
        integer                                     :: compression     = Z_TYPE_NONE    !! Use deflate or zstd compression (`Z_TYPE_*`).
        integer                                     :: connect_timeout = 30             !! Connection timeout in seconds.
        integer                                     :: timeout         = 30             !! Timeout in seconds.
        integer(i8)                                 :: modified_since  = 0_i8           !! If-modified-since timestamp (Epoch).
        logical                                     :: follow_location = .true.         !! Follow HTTP 3xx redirects.
        character(:),               allocatable     :: payload                          !! Request payload (POST).
        character(:),               allocatable     :: payload_path                     !! Request payload file path (PUT).
        character(:),               allocatable     :: content_type                     !! Request payload type (MIME).
        character(:),               allocatable     :: accept                           !! HTTP Accept header.
        character(:),               allocatable     :: username                         !! HTTP Basic Auth user name.
        character(:),               allocatable     :: password                         !! HTTP Basic Auth password.
        character(:),               allocatable     :: url                              !! Request URL.
        character(:),               allocatable     :: user_agent                       !! User Agent.
        type(rpc_header_type),      allocatable     :: headers(:)                       !! HTTP request header.
        procedure(dm_rpc_callback), pointer, nopass :: callback        => null()        !! C-interoperable write callback function.
        type(c_ptr),                private         :: curl            = c_null_ptr     !! libcurl context.
        type(c_ptr),                private         :: file            = c_null_ptr     !! FILE * of payload (PUT).
        type(c_ptr),                private         :: list            = c_null_ptr     !! libcurl list context.
    end type rpc_request_type

    interface dm_rpc_header_add
        !! Generic RPC header add function.
        module procedure :: rpc_header_add
        module procedure :: rpc_header_add_request
        module procedure :: rpc_header_add_response
    end interface dm_rpc_header_add

    interface dm_rpc_header_create
        !! Generic RPC header create function.
        module procedure :: rpc_header_create_request
        module procedure :: rpc_header_create_response
    end interface dm_rpc_header_create

    interface dm_rpc_header_get
        !! Generic RPC header get function.
        module procedure :: rpc_header_get
        module procedure :: rpc_header_get_request
        module procedure :: rpc_header_get_response
    end interface dm_rpc_header_get

    interface dm_rpc_destroy
        !! Generic RPC destroy routine.
        module procedure :: rpc_header_destroy
        module procedure :: rpc_request_destroy
        module procedure :: rpc_response_destroy
    end interface dm_rpc_destroy

    interface dm_rpc_post
        !! Generic RPC post function for derived types `beat_type`, `image_type`, `log_type`,
        !! `node_type`, `observ_type`, `sensor_type`, `target_type`.
        module procedure :: dm_rpc_post_beat
        module procedure :: dm_rpc_post_beats
        module procedure :: dm_rpc_post_image
        module procedure :: dm_rpc_post_images
        module procedure :: dm_rpc_post_log
        module procedure :: dm_rpc_post_logs
        module procedure :: dm_rpc_post_node
        module procedure :: dm_rpc_post_nodes
        module procedure :: dm_rpc_post_observ
        module procedure :: dm_rpc_post_observs
        module procedure :: dm_rpc_post_request
        module procedure :: dm_rpc_post_sensor
        module procedure :: dm_rpc_post_sensors
        module procedure :: dm_rpc_post_target
        module procedure :: dm_rpc_post_targets
    end interface dm_rpc_post

    interface dm_rpc_request
        !! Generic RPC request function.
        module procedure :: dm_rpc_request_multi
        module procedure :: dm_rpc_request_single
    end interface dm_rpc_request

    interface dm_rpc_reset
        !! Generic RPC reset routine.
        module procedure :: rpc_request_reset
        module procedure :: rpc_response_reset
    end interface dm_rpc_reset

    interface rpc_request
        !! Generic RPC request function.
        module procedure :: rpc_request_multi
        module procedure :: rpc_request_single
    end interface rpc_request

    ! Public callbacks.
    public :: dm_rpc_callback
    public :: dm_rpc_read_callback
    public :: dm_rpc_write_callback

    ! Public procedures.
    public :: dm_rpc_destroy
    public :: dm_rpc_error
    public :: dm_rpc_error_message
    public :: dm_rpc_error_multi
    public :: dm_rpc_get
    public :: dm_rpc_header_add
    public :: dm_rpc_header_create
    public :: dm_rpc_header_get
    public :: dm_rpc_init
    public :: dm_rpc_post
    public :: dm_rpc_post_beat
    public :: dm_rpc_post_image
    public :: dm_rpc_post_log
    public :: dm_rpc_post_node
    public :: dm_rpc_post_observ
    public :: dm_rpc_post_request
    public :: dm_rpc_post_sensor
    public :: dm_rpc_post_target
    public :: dm_rpc_put
    public :: dm_rpc_request
    public :: dm_rpc_request_has_callback
    public :: dm_rpc_request_multi
    public :: dm_rpc_request_set
    public :: dm_rpc_request_single
    public :: dm_rpc_reset
    public :: dm_rpc_shutdown
    public :: dm_rpc_url
    public :: dm_rpc_version

    ! Private procedures.
    private :: rpc_header_add
    private :: rpc_header_add_request
    private :: rpc_header_add_response
    private :: rpc_header_create_request
    private :: rpc_header_create_response
    private :: rpc_header_destroy
    private :: rpc_header_get
    private :: rpc_header_get_request
    private :: rpc_header_get_response
    private :: rpc_request
    private :: rpc_request_destroy
    private :: rpc_request_multi
    private :: rpc_request_prepare
    private :: rpc_request_reset
    private :: rpc_request_set_response
    private :: rpc_request_single
    private :: rpc_response_destroy
    private :: rpc_response_header
    private :: rpc_response_reset
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS
    ! **************************************************************************
    function dm_rpc_version() result(version)
        !! Returns version number of libcurl an linked libreries as allocatable
        !! string.
        character(:), allocatable :: version

        version = curl_version()
    end function dm_rpc_version

    integer function dm_rpc_error(error_curl) result(rc)
        !! Converts libcurl easy stack error code to DMPACK error code.
        integer, intent(in) :: error_curl !! libcurl easy error code.

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
        !! Return message associated with given libcurl error code as
        !! allocatable character string.
        integer, intent(in)       :: error_curl !! libcurl error code.
        character(:), allocatable :: message    !! Error message.

        message = curl_str_error(error_curl)
    end function dm_rpc_error_message

    integer function dm_rpc_error_multi(multi_error) result(rc)
        !! Converts libcurl multi stack error code to DMPACK error code.
        integer, intent(in) :: multi_error !! libcurl multi error code.

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

    integer function dm_rpc_get(request, response, url, accept, username, password, user_agent, &
                                modified_since, callback) result(rc)
        !! Sends generic HTTP GET request to URL.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_RPC` if the HTTP request failed.
        !!
        type(rpc_request_type),  intent(inout)        :: request        !! RPC request.
        type(rpc_response_type), intent(inout)        :: response       !! RPC response.
        character(*),            intent(in), optional :: url            !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: accept         !! HTTP Accept header.
        character(*),            intent(in), optional :: username       !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password       !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent     !! HTTP User Agent.
        integer(i8),             intent(in), optional :: modified_since !! Only fetch if modified since given time [Epoch].
        procedure(dm_rpc_callback),          optional :: callback       !! Callback function to pass to libcurl.

        call dm_rpc_request_set(request        = request,        &
                                modified_since = modified_since, &
                                accept         = accept,         &
                                url            = url,            &
                                user_agent     = user_agent,     &
                                callback       = callback)

        if (.not. dm_rpc_request_has_callback(request)) then
            call dm_rpc_request_set(request, callback=dm_rpc_write_callback)
        end if

        if (present(username) .and. present(password)) then
            call dm_rpc_request_set(request, auth=RPC_AUTH_BASIC, username=username, password=password)
        end if

        rc = rpc_request_single(request, response)
    end function dm_rpc_get

    integer function dm_rpc_init() result(rc)
        !! Initialises RPC backend. The function returns `E_RPC` on error.
        rc = E_RPC
        if (curl_global_init(CURL_GLOBAL_DEFAULT) == CURLE_OK) rc = E_NONE
    end function dm_rpc_init

    integer function dm_rpc_post_beat(request, response, beat, url, username, password, &
                                      user_agent, compression, prepare) result(rc)
        !! Sends a single derived type in Namelist format to a given URL, with
        !! optional authentication and compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_beat

        type(rpc_request_type),  intent(inout)        :: request     !! RPC request.
        type(rpc_response_type), intent(inout)        :: response    !! RPC response.
        type(beat_type),         intent(inout)        :: beat        !! Beat.
        character(*),            intent(in), optional :: url         !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username    !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password    !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent  !! HTTP User Agent.
        integer,                 intent(in), optional :: compression !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: prepare     !! Only prepare request.

        rc = dm_z_compress(beat, request%compression, request%payload)
        if (dm_is_error(rc)) return

        rc = dm_rpc_post(request, response, url, MIME_NML, username, password, user_agent, compression, prepare)
    end function dm_rpc_post_beat

    integer function dm_rpc_post_beats(requests, responses, beats, url, username, password, &
                                       user_agent, compression, sequential) result(rc)
        !! Sends multiple derived beats concurrently in Namelist format to the
        !! given URL, with optional authentication and compression. The URL
        !! has to be the API endpoint that accepts HTTP POST requests.
        !!
        !! If `sequential` is `.true.`, the transfer will be sequentially
        !! instead of concurrently.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_beat
        use :: dm_zstd, only: dm_zstd_destroy, zstd_context_type

        type(rpc_request_type),  intent(inout)        :: requests(:)               !! RPC request type array.
        type(rpc_response_type), intent(inout)        :: responses(size(requests)) !! RPC response type array.
        type(beat_type),         intent(inout)        :: beats(size(requests))     !! Beats.
        character(*),            intent(in), optional :: url                       !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username                  !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password                  !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent                !! HTTP User Agent.
        integer,                 intent(in), optional :: compression               !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: sequential                !! Sequential instead of concurrent transfer.

        integer :: i

        ! Prepare all requests.
        do i = 1, size(requests)
            rc = dm_rpc_post_beat(requests(i), responses(i), beats(i), url, username, password, user_agent, compression, prepare=.true.)
            if (dm_is_error(rc)) return
        end do

        ! Send requests concurrently by default.
        if (.not. dm_present(sequential, .false.)) then
            rc = rpc_request(requests, responses)
            return
        end if

        ! Send requests sequentially.
        do i = 1, size(requests)
            rc = rpc_request(requests(i), responses(i))
        end do
    end function dm_rpc_post_beats

    integer function dm_rpc_post_image(request, response, image, url, username, password, &
                                       user_agent, compression, prepare) result(rc)
        !! Sends a single derived type in Namelist format to a given URL, with
        !! optional authentication and compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_image

        type(rpc_request_type),  intent(inout)        :: request     !! RPC request.
        type(rpc_response_type), intent(inout)        :: response    !! RPC response.
        type(image_type),        intent(inout)        :: image       !! Image.
        character(*),            intent(in), optional :: url         !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username    !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password    !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent  !! HTTP User Agent.
        integer,                 intent(in), optional :: compression !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: prepare     !! Only prepare request.

        rc = dm_z_compress(image, request%compression, request%payload)
        if (dm_is_error(rc)) return

        rc = dm_rpc_post(request, response, url, MIME_NML, username, password, user_agent, compression, prepare)
    end function dm_rpc_post_image

    integer function dm_rpc_post_images(requests, responses, images, url, username, password, &
                                        user_agent, compression, sequential) result(rc)
        !! Sends multiple derived images concurrently in Namelist format to the
        !! given URL, with optional authentication and compression. The URL
        !! has to be the API endpoint that accepts HTTP POST requests.
        !!
        !! If `sequential` is `.true.`, the transfer will be sequentially
        !! instead of concurrently.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_image
        use :: dm_zstd, only: dm_zstd_destroy, zstd_context_type

        type(rpc_request_type),  intent(inout)        :: requests(:)               !! RPC request type array.
        type(rpc_response_type), intent(inout)        :: responses(size(requests)) !! RPC response type array.
        type(image_type),        intent(inout)        :: images(size(requests))    !! Images.
        character(*),            intent(in), optional :: url                       !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username                  !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password                  !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent                !! HTTP User Agent.
        integer,                 intent(in), optional :: compression               !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: sequential                !! Sequential instead of concurrent transfer.

        integer :: i

        ! Prepare all requests.
        do i = 1, size(requests)
            rc = dm_rpc_post_image(requests(i), responses(i), images(i), url, username, password, user_agent, compression, prepare=.true.)
            if (dm_is_error(rc)) return
        end do

        ! Send requests concurrently by default.
        if (.not. dm_present(sequential, .false.)) then
            rc = rpc_request(requests, responses)
            return
        end if

        ! Send requests sequentially.
        do i = 1, size(requests)
            rc = rpc_request(requests(i), responses(i))
        end do
    end function dm_rpc_post_images

    integer function dm_rpc_post_log(request, response, log, url, username, password, &
                                     user_agent, compression, prepare) result(rc)
        !! Sends a single derived type in Namelist format to a given URL, with
        !! optional authentication and compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_log

        type(rpc_request_type),  intent(inout)        :: request     !! RPC request.
        type(rpc_response_type), intent(inout)        :: response    !! RPC response.
        type(log_type),          intent(inout)        :: log         !! Log.
        character(*),            intent(in), optional :: url         !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username    !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password    !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent  !! HTTP User Agent.
        integer,                 intent(in), optional :: compression !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: prepare     !! Only prepare request.

        rc = dm_z_compress(log, request%compression, request%payload)
        if (dm_is_error(rc)) return

        rc = dm_rpc_post(request, response, url, MIME_NML, username, password, user_agent, compression, prepare)
    end function dm_rpc_post_log

    integer function dm_rpc_post_logs(requests, responses, logs, url, username, password, &
                                      user_agent, compression, sequential) result(rc)
        !! Sends multiple derived logs concurrently in Namelist format to the
        !! given URL, with optional authentication and compression. The URL
        !! has to be the API endpoint that accepts HTTP POST requests.
        !!
        !! If `sequential` is `.true.`, the transfer will be sequentially
        !! instead of concurrently.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_log
        use :: dm_zstd, only: dm_zstd_destroy, zstd_context_type

        type(rpc_request_type),  intent(inout)        :: requests(:)               !! RPC request type array.
        type(rpc_response_type), intent(inout)        :: responses(size(requests)) !! RPC response type array.
        type(log_type),          intent(inout)        :: logs(size(requests))      !! Logs.
        character(*),            intent(in), optional :: url                       !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username                  !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password                  !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent                !! HTTP User Agent.
        integer,                 intent(in), optional :: compression               !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: sequential                !! Sequential instead of concurrent transfer.

        integer :: i

        ! Prepare all requests.
        do i = 1, size(requests)
            rc = dm_rpc_post_log(requests(i), responses(i), logs(i), url, username, password, user_agent, compression, prepare=.true.)
            if (dm_is_error(rc)) return
        end do

        ! Send requests concurrently by default.
        if (.not. dm_present(sequential, .false.)) then
            rc = rpc_request(requests, responses)
            return
        end if

        ! Send requests sequentially.
        do i = 1, size(requests)
            rc = rpc_request(requests(i), responses(i))
        end do
    end function dm_rpc_post_logs

    integer function dm_rpc_post_node(request, response, node, url, username, password, &
                                      user_agent, compression, prepare) result(rc)
        !! Sends a single derived type in Namelist format to a given URL, with
        !! optional authentication and compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_node

        type(rpc_request_type),  intent(inout)        :: request     !! RPC request.
        type(rpc_response_type), intent(inout)        :: response    !! RPC response.
        type(node_type),         intent(inout)        :: node        !! Node.
        character(*),            intent(in), optional :: url         !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username    !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password    !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent  !! HTTP User Agent.
        integer,                 intent(in), optional :: compression !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: prepare     !! Only prepare request.

        rc = dm_z_compress(node, request%compression, request%payload)
        if (dm_is_error(rc)) return

        rc = dm_rpc_post(request, response, url, MIME_NML, username, password, user_agent, compression, prepare)
    end function dm_rpc_post_node

    integer function dm_rpc_post_nodes(requests, responses, nodes, url, username, password, &
                                       user_agent, compression, sequential) result(rc)
        !! Sends multiple derived nodes concurrently in Namelist format to the
        !! given URL, with optional authentication and compression. The URL
        !! has to be the API endpoint that accepts HTTP POST requests.
        !!
        !! If `sequential` is `.true.`, the transfer will be sequentially
        !! instead of concurrently.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_node
        use :: dm_zstd, only: dm_zstd_destroy, zstd_context_type

        type(rpc_request_type),  intent(inout)        :: requests(:)               !! RPC request type array.
        type(rpc_response_type), intent(inout)        :: responses(size(requests)) !! RPC response type array.
        type(node_type),         intent(inout)        :: nodes(size(requests))     !! Nodes.
        character(*),            intent(in), optional :: url                       !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username                  !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password                  !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent                !! HTTP User Agent.
        integer,                 intent(in), optional :: compression               !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: sequential                !! Sequential instead of concurrent transfer.

        integer :: i

        ! Prepare all requests.
        do i = 1, size(requests)
            rc = dm_rpc_post_node(requests(i), responses(i), nodes(i), url, username, password, user_agent, compression, prepare=.true.)
            if (dm_is_error(rc)) return
        end do

        ! Send requests concurrently by default.
        if (.not. dm_present(sequential, .false.)) then
            rc = rpc_request(requests, responses)
            return
        end if

        ! Send requests sequentially.
        do i = 1, size(requests)
            rc = rpc_request(requests(i), responses(i))
        end do
    end function dm_rpc_post_nodes

    integer function dm_rpc_post_observ(request, response, observ, url, username, password, &
                                        user_agent, compression, prepare) result(rc)
        !! Sends a single derived type in Namelist format to a given URL, with
        !! optional authentication and compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_observ

        type(rpc_request_type),  intent(inout)        :: request     !! RPC request.
        type(rpc_response_type), intent(inout)        :: response    !! RPC response.
        type(observ_type),       intent(inout)        :: observ      !! Observation.
        character(*),            intent(in), optional :: url         !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username    !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password    !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent  !! HTTP User Agent.
        integer,                 intent(in), optional :: compression !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: prepare     !! Only prepare request.

        rc = dm_z_compress(observ, request%compression, request%payload)
        if (dm_is_error(rc)) return

        rc = dm_rpc_post(request, response, url, MIME_NML, username, password, user_agent, compression, prepare)
    end function dm_rpc_post_observ

    integer function dm_rpc_post_observs(requests, responses, observs, url, username, password, &
                                         user_agent, compression, sequential) result(rc)
        !! Sends multiple derived observs concurrently in Namelist format to the
        !! given URL, with optional authentication and compression. The URL
        !! has to be the API endpoint that accepts HTTP POST requests.
        !!
        !! If `sequential` is `.true.`, the transfer will be sequentially
        !! instead of concurrently.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_observ
        use :: dm_zstd, only: dm_zstd_destroy, zstd_context_type

        type(rpc_request_type),  intent(inout)        :: requests(:)               !! RPC request type array.
        type(rpc_response_type), intent(inout)        :: responses(size(requests)) !! RPC response type array.
        type(observ_type),       intent(inout)        :: observs(size(requests))   !! Observations.
        character(*),            intent(in), optional :: url                       !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username                  !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password                  !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent                !! HTTP User Agent.
        integer,                 intent(in), optional :: compression               !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: sequential                !! Sequential instead of concurrent transfer.

        integer :: i

        ! Prepare all requests.
        do i = 1, size(requests)
            rc = dm_rpc_post_observ(requests(i), responses(i), observs(i), url, username, password, user_agent, compression, prepare=.true.)
            if (dm_is_error(rc)) return
        end do

        ! Send requests concurrently by default.
        if (.not. dm_present(sequential, .false.)) then
            rc = rpc_request(requests, responses)
            return
        end if

        ! Send requests sequentially.
        do i = 1, size(requests)
            rc = rpc_request(requests(i), responses(i))
        end do
    end function dm_rpc_post_observs

    integer function dm_rpc_post_request(request, response, url, content_type, username, password, &
                                         user_agent, compression, prepare) result(rc)
        !! Sends HTTP POST request. If `prepare` is passed and `.true.`, the
        !! request is only prepared and not sent.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_RPC` if request failed.
        !!
        type(rpc_request_type),  intent(inout)        :: request      !! RPC request.
        type(rpc_response_type), intent(inout)        :: response     !! RPC response.
        character(*),            intent(in), optional :: url          !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: content_type !! MIME type of payload file.
        character(*),            intent(in), optional :: username     !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password     !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent   !! HTTP User Agent.
        integer,                 intent(in), optional :: compression  !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: prepare      !! Only prepare request.

        call dm_rpc_request_set(request      = request,         &
                                method       = RPC_METHOD_POST, &
                                compression  = compression,     &
                                content_type = content_type,    &
                                accept       = MIME_TEXT,       &
                                url          = url,             &
                                user_agent   = user_agent)

        if (.not. dm_rpc_request_has_callback(request)) then
            call dm_rpc_request_set(request, callback=dm_rpc_write_callback)
        end if

        if (present(username) .and. present(password)) then
            call dm_rpc_request_set(request, auth=RPC_AUTH_BASIC, username=username, password=password)
        end if

        if (.not. dm_present(prepare, .false.)) rc = rpc_request(request, response)
    end function dm_rpc_post_request

    integer function dm_rpc_post_sensor(request, response, sensor, url, username, password, &
                                        user_agent, compression, prepare) result(rc)
        !! Sends a single derived type in Namelist format to a given URL, with
        !! optional authentication and compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_sensor

        type(rpc_request_type),  intent(inout)        :: request     !! RPC request.
        type(rpc_response_type), intent(inout)        :: response    !! RPC response.
        type(sensor_type),       intent(inout)        :: sensor      !! Sensor.
        character(*),            intent(in), optional :: url         !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username    !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password    !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent  !! HTTP User Agent.
        integer,                 intent(in), optional :: compression !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: prepare     !! Only prepare request.

        rc = dm_z_compress(sensor, request%compression, request%payload)
        if (dm_is_error(rc)) return

        rc = dm_rpc_post(request, response, url, MIME_NML, username, password, user_agent, compression, prepare)
    end function dm_rpc_post_sensor

    integer function dm_rpc_post_sensors(requests, responses, sensors, url, username, password, &
                                         user_agent, compression, sequential) result(rc)
        !! Sends multiple derived sensors concurrently in Namelist format to the
        !! given URL, with optional authentication and compression. The URL
        !! has to be the API endpoint that accepts HTTP POST requests.
        !!
        !! If `sequential` is `.true.`, the transfer will be sequentially
        !! instead of concurrently.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_sensor
        use :: dm_zstd, only: dm_zstd_destroy, zstd_context_type

        type(rpc_request_type),  intent(inout)        :: requests(:)               !! RPC request type array.
        type(rpc_response_type), intent(inout)        :: responses(size(requests)) !! RPC response type array.
        type(sensor_type),       intent(inout)        :: sensors(size(requests))   !! Sensors.
        character(*),            intent(in), optional :: url                       !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username                  !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password                  !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent                !! HTTP User Agent.
        integer,                 intent(in), optional :: compression               !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: sequential                !! Sequential instead of concurrent transfer.

        integer :: i

        ! Prepare all requests.
        do i = 1, size(requests)
            rc = dm_rpc_post_sensor(requests(i), responses(i), sensors(i), url, username, password, user_agent, compression, prepare=.true.)
            if (dm_is_error(rc)) return
        end do

        ! Send requests concurrently by default.
        if (.not. dm_present(sequential, .false.)) then
            rc = rpc_request(requests, responses)
            return
        end if

        ! Send requests sequentially.
        do i = 1, size(requests)
            rc = rpc_request(requests(i), responses(i))
        end do
    end function dm_rpc_post_sensors

    integer function dm_rpc_post_target(request, response, target, url, username, password, &
                                        user_agent, compression, prepare) result(rc)
        !! Sends a single derived type in Namelist format to a given URL, with
        !! optional authentication and compression. The URL has to be the API
        !! endpoint that accepts HTTP POST requests.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_target

        type(rpc_request_type),  intent(inout)        :: request     !! RPC request.
        type(rpc_response_type), intent(inout)        :: response    !! RPC response.
        type(target_type),       intent(inout)        :: target      !! Target.
        character(*),            intent(in), optional :: url         !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username    !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password    !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent  !! HTTP User Agent.
        integer,                 intent(in), optional :: compression !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: prepare     !! Only prepare request.

        rc = dm_z_compress(target, request%compression, request%payload)
        if (dm_is_error(rc)) return

        rc = dm_rpc_post(request, response, url, MIME_NML, username, password, user_agent, compression, prepare)
    end function dm_rpc_post_target

    integer function dm_rpc_post_targets(requests, responses, targets, url, username, password, &
                                         user_agent, compression, sequential) result(rc)
        !! Sends multiple derived targets concurrently in Namelist format to the
        !! given URL, with optional authentication and compression. The URL
        !! has to be the API endpoint that accepts HTTP POST requests.
        !!
        !! If `sequential` is `.true.`, the transfer will be sequentially
        !! instead of concurrently.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_RPC` if request failed.
        !! * `E_ZLIB` if zlib libray call failed.
        !! * `E_ZSTD` if zstd libray call failed.
        !!
        use :: dm_target
        use :: dm_zstd, only: dm_zstd_destroy, zstd_context_type

        type(rpc_request_type),  intent(inout)        :: requests(:)               !! RPC request type array.
        type(rpc_response_type), intent(inout)        :: responses(size(requests)) !! RPC response type array.
        type(target_type),       intent(inout)        :: targets(size(requests))   !! Targets.
        character(*),            intent(in), optional :: url                       !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: username                  !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password                  !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent                !! HTTP User Agent.
        integer,                 intent(in), optional :: compression               !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).
        logical,                 intent(in), optional :: sequential                !! Sequential instead of concurrent transfer.

        integer :: i

        ! Prepare all requests.
        do i = 1, size(requests)
            rc = dm_rpc_post_target(requests(i), responses(i), targets(i), url, username, password, user_agent, compression, prepare=.true.)
            if (dm_is_error(rc)) return
        end do

        ! Send requests concurrently by default.
        if (.not. dm_present(sequential, .false.)) then
            rc = rpc_request(requests, responses)
            return
        end if

        ! Send requests sequentially.
        do i = 1, size(requests)
            rc = rpc_request(requests(i), responses(i))
        end do
    end function dm_rpc_post_targets

    integer function dm_rpc_put(request, response, url, payload_path, content_type, username, password, user_agent) result(rc)
        !! Sends a file via HTTP PUT.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_RPC` if request failed.
        !!
        type(rpc_request_type),  intent(inout)        :: request      !! RPC request.
        type(rpc_response_type), intent(inout)        :: response     !! RPC response.
        character(*),            intent(in), optional :: url          !! URL of RPC API (may include port).
        character(*),            intent(in), optional :: payload_path !! Path to payload file.
        character(*),            intent(in), optional :: content_type !! MIME type of payload file.
        character(*),            intent(in), optional :: username     !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password     !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent   !! HTTP User Agent.

        call dm_rpc_request_set(request      = request,        &
                                method       = RPC_METHOD_PUT, &
                                payload_path = payload_path,   &
                                content_type = content_type,   &
                                accept       = MIME_TEXT,      &
                                url          = url,            &
                                user_agent   = user_agent)

        if (.not. dm_rpc_request_has_callback(request)) then
            call dm_rpc_request_set(request, callback=dm_rpc_write_callback)
        end if

        if (present(username) .and. present(password)) then
            call dm_rpc_request_set(request, auth=RPC_AUTH_BASIC, username=username, password=password)
        end if

        rc = rpc_request(request, response)
    end function dm_rpc_put

    logical function dm_rpc_request_has_callback(request) result(has)
        !! Returns `.true.` if request has associated callback procedure.
        type(rpc_request_type), intent(inout) :: request !! RPC request.

        has = associated(request%callback)
    end function dm_rpc_request_has_callback

    integer function dm_rpc_request_multi(requests, responses, url, method, accept, username, password, &
                                          user_agent, compression) result(rc)
        !! Sends multiple HTTP requests by GET, POST, or PUT method, with
        !! optional deflate or zstd compression.
        type(rpc_request_type),  intent(inout)        :: requests(:)               !! RPC request type array.
        type(rpc_response_type), intent(inout)        :: responses(size(requests)) !! RPC response type array.
        character(*),            intent(in), optional :: url                       !! URL of RPC API (may include port).
        integer,                 intent(in), optional :: method                    !! `RPC_METHOD_GET` or `RPC_METHOD_POST`.
        character(*),            intent(in), optional :: accept                    !! HTTP Accept header.
        character(*),            intent(in), optional :: username                  !! HTTP Basic Auth user name.
        character(*),            intent(in), optional :: password                  !! HTTP Basic Auth password.
        character(*),            intent(in), optional :: user_agent                !! HTTP User Agent.
        integer,                 intent(in), optional :: compression               !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).

        integer :: i

        do i = 1, size(requests)
            call dm_rpc_request_set(request     = requests(i), &
                                    method      = method,      &
                                    compression = compression, &
                                    accept      = accept,      &
                                    url         = url,         &
                                    user_agent  = user_agent)

            if (.not. dm_rpc_request_has_callback(requests(i))) then
                call dm_rpc_request_set(requests(i), callback=dm_rpc_write_callback)
            end if

            if (present(username) .and. present(password)) then
                call dm_rpc_request_set(requests(i), auth=RPC_AUTH_BASIC, username=username, password=password)
            end if
        end do

        rc = rpc_request_multi(requests, responses)
    end function dm_rpc_request_multi

    integer function dm_rpc_request_single(request, response, url, method, payload, content_type, &
                                           accept, username, password, user_agent, compression) result(rc)
        !! Sends single HTTP request by GET, POST, or PUT method, and with
        !! optional deflate or zstd compression.
        type(rpc_request_type),  intent(inout)           :: request      !! RPC request.
        type(rpc_response_type), intent(inout)           :: response     !! RPC response.
        character(*),            intent(in),    optional :: url          !! URL of RPC API (may include port).
        integer,                 intent(in),    optional :: method       !! `RPC_METHOD_GET` or `RPC_METHOD_POST`.
        character(*),            intent(inout), optional :: payload      !! Payload data (for POST only).
        character(*),            intent(in),    optional :: content_type !! Payload content type (for POST only).
        character(*),            intent(in),    optional :: accept       !! HTTP Accept header.
        character(*),            intent(in),    optional :: username     !! HTTP Basic Auth user name.
        character(*),            intent(in),    optional :: password     !! HTTP Basic Auth password.
        character(*),            intent(in),    optional :: user_agent   !! HTTP User Agent.
        integer,                 intent(in),    optional :: compression  !! Deflate or Zstandard compression of payload for POST requests (`Z_TYPE_*`).

        call dm_rpc_request_set(request     = request,     &
                                method      = method,      &
                                compression = compression, &
                                accept      = accept,      &
                                url         = url,         &
                                user_agent  = user_agent)

        if (.not. dm_rpc_request_has_callback(request)) then
            call dm_rpc_request_set(request, callback=dm_rpc_write_callback)
        end if

        if (present(username) .and. present(password)) then
            call dm_rpc_request_set(request, auth=RPC_AUTH_BASIC, username=username, password=password)
        end if

        if (request%method == RPC_METHOD_POST) then
            call dm_rpc_request_set(request, content_type=content_type, payload=payload)
        end if

        rc = rpc_request_single(request, response)
    end function dm_rpc_request_single

    function dm_rpc_url(host, port, base, endpoint, tls) result(url)
        !! Returns allocatable string of URL to HTTP-RPC API endpoint. Uses the
        !! URL API of libcurl to create the URL. The base path and the endpoint
        !! must both start with a `/`.
        !!
        !! The function returns an empty string on error.
        character(*), intent(in)           :: host     !! IP or FQDN of remote host.
        integer,      intent(in), optional :: port     !! API port (up to 5 digits).
        character(*), intent(in), optional :: base     !! API base path (for example, `/api/v2`).
        character(*), intent(in), optional :: endpoint !! API endpoint (for example, `/observ`).
        logical,      intent(in), optional :: tls      !! TLS encryption (HTTPS).
        character(:), allocatable          :: url      !! HTTP-RPC API endpoint URL.

        character(:), allocatable :: path

        integer     :: crc, port_
        logical     :: tls_
        type(c_ptr) :: ptr

        tls_  = dm_present(tls, .false.) ! HTTP by default.
        port_ = dm_present(port, 0)      ! Auto-select port by default.

        url_block: block
            ptr = curl_url()
            if (.not. c_associated(ptr)) exit url_block

            ! URL scheme.
            if (tls_) then
                crc = curl_url_set(ptr, CURLUPART_SCHEME, 'https')
            else
                crc = curl_url_set(ptr, CURLUPART_SCHEME, 'http')
            end if

            if (crc /= CURLUE_OK) exit url_block

            ! URL host.
            crc = curl_url_set(ptr, CURLUPART_HOST, trim(host))
            if (crc /= CURLUE_OK) exit url_block

            ! URL port.
            if (port_ > 0) then
                crc = curl_url_set(ptr, CURLUPART_PORT, dm_itoa(port_))
                if (crc /= CURLUE_OK) exit url_block
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
            crc = curl_url_set(ptr, CURLUPART_PATH, path)
            if (crc /= CURLUE_OK) exit url_block

            ! Get full URL.
            crc = curl_url_get(ptr, CURLUPART_URL, url)
        end block url_block

        call curl_url_cleanup(ptr)
        if (.not. allocated(url)) url = ''
    end function dm_rpc_url

    ! **************************************************************************
    ! PUBLIC SUBROUTINES
    ! **************************************************************************
    subroutine dm_rpc_request_set(request, auth, method, compression, connect_timeout, timeout, modified_since, follow_location, &
                                  payload, payload_path, content_type, accept, username, password, url, user_agent, callback)
        !! Sets RPC request settings.
        type(rpc_request_type), intent(inout)        :: request
        integer,                intent(in), optional :: auth            !! HTTP Auth type (`RPC_AUTH_*`).
        integer,                intent(in), optional :: method          !! HTTP method (GET, POST).
        integer,                intent(in), optional :: compression     !! Use deflate or zstd compression (`Z_TYPE_*`).
        integer,                intent(in), optional :: connect_timeout !! Connection timeout in seconds.
        integer,                intent(in), optional :: timeout         !! Timeout in seconds.
        integer(i8),            intent(in), optional :: modified_since  !! If-modified-since timestamp (Epoch).
        logical,                intent(in), optional :: follow_location !! Follow HTTP 3xx redirects.
        character(*),           intent(in), optional :: payload         !! Request payload (POST).
        character(*),           intent(in), optional :: payload_path    !! Request payload file (PUT).
        character(*),           intent(in), optional :: content_type    !! Request payload type (MIME).
        character(*),           intent(in), optional :: accept          !! HTTP Accept header.
        character(*),           intent(in), optional :: username        !! HTTP Basic Auth user name.
        character(*),           intent(in), optional :: password        !! HTTP Basic Auth password.
        character(*),           intent(in), optional :: url             !! Request URL.
        character(*),           intent(in), optional :: user_agent      !! User Agent.
        procedure(dm_rpc_callback),         optional :: callback        !! C-interoperable write callback function.

        if (present(auth))            request%auth            = auth
        if (present(method))          request%method          = method
        if (present(compression))     request%compression     = compression
        if (present(connect_timeout)) request%connect_timeout = max(0, connect_timeout)
        if (present(timeout))         request%timeout         = max(0, timeout)
        if (present(modified_since))  request%modified_since  = modified_since
        if (present(follow_location)) request%follow_location = follow_location
        if (present(payload))         request%payload         = payload
        if (present(payload_path))    request%payload_path    = trim(payload_path)
        if (present(content_type))    request%content_type    = trim(content_type)
        if (present(accept))          request%accept          = trim(accept)
        if (present(username))        request%username        = trim(username)
        if (present(password))        request%password        = trim(password)
        if (present(url))             request%url             = trim(url)
        if (present(user_agent))      request%user_agent      = trim(user_agent)
        if (present(callback))        request%callback        => callback
    end subroutine dm_rpc_request_set

    subroutine dm_rpc_shutdown()
        !! Cleans up RPC backend.
        call curl_global_cleanup()
    end subroutine dm_rpc_shutdown

    ! **************************************************************************
    ! PUBLIC CALLBACK FUNCTIONS
    ! **************************************************************************
    function dm_rpc_read_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable read callback function for libcurl. Reads chunks
        !! using _fread(3)_. Do not call this function directly.
        use :: unix, only: c_fread

        type(c_ptr),       intent(in), value :: ptr   !! C pointer to a chunk of the response.
        integer(c_size_t), intent(in), value :: sz    !! Always 1.
        integer(c_size_t), intent(in), value :: nmemb !! Size of the response chunk.
        type(c_ptr),       intent(in), value :: data  !! C pointer to argument passed by caller.
        integer(c_size_t)                    :: n     !! Function return value.

        n = c_fread(ptr, sz, nmemb, data)
    end function dm_rpc_read_callback

    function dm_rpc_write_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable write callback function for libcurl. Writes the
        !! received response chunks to `rpc_response_type` pointer that has to
        !! be passed through C pointer `data`. Do not call this function
        !! directly.
        type(c_ptr),       intent(in), value :: ptr   !! C pointer to a chunk of the response.
        integer(c_size_t), intent(in), value :: sz    !! Always 1.
        integer(c_size_t), intent(in), value :: nmemb !! Size of the response chunk.
        type(c_ptr),       intent(in), value :: data  !! C pointer to argument passed by caller.
        integer(c_size_t)                    :: n     !! Function return value.

        character(:), allocatable        :: chunk
        type(rpc_response_type), pointer :: response

        n = 0_c_size_t

        if (.not. c_associated(ptr))  return
        if (.not. c_associated(data)) return

        call c_f_pointer(data, response)
        if (.not. allocated(response%payload)) allocate (character(0) :: response%payload)
        call c_f_str_ptr(ptr, chunk, nmemb)
        response%payload = response%payload // chunk

        n = nmemb
    end function dm_rpc_write_callback

    ! **************************************************************************
    ! PRIVATE FUNCTIONS
    ! **************************************************************************
    integer function rpc_header_add(headers, name, value) result(rc)
        !! Adds header to request.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if headers array is full.
        !! * `E_INVALID` if name is empty.
        !!
        type(rpc_header_type), intent(inout)        :: headers(:) !! Header type array.
        character(*),          intent(in)           :: name       !! Header name.
        character(*),          intent(in), optional :: value      !! Header value.

        integer :: i

        rc = E_INVALID
        if (len_trim(name) == 0) return

        rc = E_BOUNDS
        do i = 1, size(headers)
            if (allocated(headers(i)%name)) then
                if (len(headers(i)%name) /= 0) cycle
            end if

            headers(i)%name = trim(name)

            if (present(value)) then
                headers(i)%value = trim(value)
            else
                headers(i)%value = ''
            end if

            rc = E_NONE
            exit
        end do
    end function rpc_header_add

    integer function rpc_header_add_request(request, name, value) result(rc)
        !! Adds header to request.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if headers array is full.
        !! * `E_INVALID` if name is empty.
        !! * `E_NULL` if headers array is not allocated.
        !!
        type(rpc_request_type), intent(inout)        :: request !! RPC request.
        character(*),           intent(in)           :: name    !! Header name.
        character(*),           intent(in), optional :: value   !! Header value.

        rc = E_NULL
        if (.not. allocated(request%headers)) return

        rc = rpc_header_add(request%headers, name, value)
    end function rpc_header_add_request

    integer function rpc_header_add_response(response, name, value) result(rc)
        !! Adds header to response.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if headers array is full.
        !! * `E_INVALID` if name is empty.
        !! * `E_NULL` if headers array is not allocated.
        !!
        type(rpc_response_type), intent(inout)        :: response !! RPC response.
        character(*),            intent(in)           :: name     !! Header name.
        character(*),            intent(in), optional :: value    !! Header value.

        rc = E_NULL
        if (.not. allocated(response%headers)) return

        rc = rpc_header_add(response%headers, name, value)
    end function rpc_header_add_response

    integer function rpc_header_create_request(request, max_size) result(rc)
        !! Creates request header array of given maximum size.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if argument `max_size` is less than 0.
        !!
        type(rpc_request_type), intent(inout) :: request  !! RPC request.
        integer,                intent(in)    :: max_size !! Max. number of headers.

        integer :: stat

        rc = E_INVALID
        if (max_size < 1) return

        rc = E_ALLOC
        if (allocated(request%headers)) deallocate (request%headers)
        allocate (request%headers(max_size), stat=stat)
        if (stat /= 0) return

        rc = E_NONE
    end function rpc_header_create_request

    integer function rpc_header_create_response(response, max_size) result(rc)
        !! Creates response header array of given maximum size.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if argument `max_size` is less than 0.
        !!
        type(rpc_response_type), intent(inout) :: response !! RPC response.
        integer,                 intent(in)    :: max_size !! Max. number of headers.

        integer :: stat

        rc = E_INVALID
        if (max_size < 1) return

        rc = E_ALLOC
        if (allocated(response%headers)) deallocate (response%headers)
        allocate (response%headers(max_size), stat=stat)
        if (stat /= 0) return

        rc = E_NONE
    end function rpc_header_create_response

    integer function rpc_header_get(headers, name, value) result(rc)
        !! Gets header from request.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if headers array is empty.
        !! * `E_NOT_FOUND` if header has not been found.
        !!
        type(rpc_header_type),     intent(inout) :: headers(:) !! Header type array.
        character(*),              intent(in)    :: name       !! Header name.
        character(:), allocatable, intent(out)   :: value      !! Header value.

        integer :: i

        rc = E_EMPTY
        if (size(headers) == 0) return

        rc = E_NOT_FOUND
        do i = 1, size(headers)
            if (.not. allocated(headers(i)%name)) cycle
            if (headers(i)%name /= name)          cycle

            rc = E_NONE
            if (allocated(headers(i)%value)) value = headers(i)%value
            exit
        end do

        if (.not. allocated(value)) value = ''
    end function rpc_header_get

    integer function rpc_header_get_request(request, name, value) result(rc)
        !! Gets header from request.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if headers array is empty.
        !! * `E_NOT_FOUND` if header has not been found.
        !! * `E_NULL` if headers array is not allocated.
        !!
        type(rpc_request_type),    intent(inout) :: request !! RPC request.
        character(*),              intent(in)    :: name    !! Header name.
        character(:), allocatable, intent(out)   :: value   !! Header value.

        rc = E_NULL
        if (.not. allocated(request%headers)) then
            value = ''
            return
        end if

        rc = rpc_header_get(request%headers, name, value)
    end function rpc_header_get_request

    integer function rpc_header_get_response(response, name, value) result(rc)
        !! Gets header from response.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if headers array is empty.
        !! * `E_NOT_FOUND` if header has not been found.
        !! * `E_NULL` if headers array is not allocated.
        !!
        type(rpc_response_type),   intent(inout) :: response !! RPC response.
        character(*),              intent(in)    :: name     !! Header name.
        character(:), allocatable, intent(out)   :: value    !! Header value.

        rc = E_NULL
        if (.not. allocated(response%headers)) then
            value = ''
            return
        end if

        rc = rpc_header_get(response%headers, name, value)
    end function rpc_header_get_response

    integer function rpc_request_multi(requests, responses) result(rc)
        !! Sends multiple HTTP requests by calling libcurl.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_COMPILER` if C pointer could not be nullified (compiler bug).
        !! * `E_EMPTY` if no RPC requests are given.
        !! * `E_RPC` if RPC backend initialisation failed.
        !!
        !! Other DMPACK errors may occur, depending on the result of the
        !! transmission. Specific transfer error codes are returned in the
        !! responses.
        use :: unix, only: c_fclose

        integer, parameter :: POLL_TIMEOUT = 1000 !! Poll timeout [msec].

        type(rpc_request_type),  intent(inout) :: requests(:)               !! Request type array.
        type(rpc_response_type), intent(inout) :: responses(size(requests)) !! Response type array.

        integer                 :: crc, error, i, n
        integer                 :: idx, nfds, nqueued, nrun
        type(c_ptr)             :: msg_ptr
        type(c_ptr)             :: multi_ptr
        type(curl_msg), pointer :: msg

        n = size(requests)

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
                if (.not. c_associated(requests(i)%curl)) then
                    requests(i)%curl = curl_init()
                    if (.not. c_associated(requests(i)%curl)) exit curl_block
                end if

                ! Prepare request.
                rc = rpc_request_prepare(requests(i), responses(i))
                if (dm_is_error(rc)) exit curl_block
            end do

            ! Create multi-stack and add individual transfers.
            multi_ptr = curl_multi_init()

            rc = E_RPC
            if (.not. c_associated(multi_ptr)) exit curl_block

            do i = 1, n
                crc = curl_multi_add_handle(multi_ptr, requests(i)%curl)
                rc  = dm_rpc_error_multi(crc)
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
                    crc  = curl_multi_poll(multi_ptr, c_null_ptr, 0, POLL_TIMEOUT, nfds)
                    if (crc /= CURLM_OK) exit
                end if
            end do

            ! Get DMPACK error code from curl error.
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
                    if (.not. c_associated(msg%easy_handle, requests(i)%curl)) cycle
                    idx = i
                    exit
                end do

                if (idx == 0) cycle
                responses(idx)%error_curl = int(msg%result)
            end do

            ! Get response info and clean-up requests.
            do i = 1, n
                associate (request => requests(i), response => responses(i))
                    call rpc_request_set_response(request, response)

                    if (c_associated(request%file)) then
                        if (c_fclose(request%file) == 0) request%file = c_null_ptr
                    end if

                    crc = curl_multi_remove_handle(multi_ptr, request%curl)
                    call curl_slist_free_all(request%list)
                    call curl_cleanup(request%curl)
                end associate
            end do
        end block curl_block

        crc = curl_multi_cleanup(multi_ptr)
        if (dm_is_error(rc)) return
        if (c_associated(multi_ptr)) rc = E_COMPILER
    end function rpc_request_multi

    integer function rpc_request_prepare(request, response) result(rc)
        !! Prepares a request by setting the necessary libcurl options.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ACCESS` if payload file is not readable (PUT).
        !! * `E_COMPILER` if list pointer could not be nullified (compiler bug).
        !! * `E_INVALID` if libcurl is not initialised.
        !! * `E_IO` if payload file could not be opened (PUT).
        !! * `E_NOT_FOUND` if payload file does not exist (PUT).
        !! * `E_RPC` if request preparation failed.
        !!
        use :: dm_c,      only: dm_f_c_logical, dm_f_c_string
        use :: dm_file,   only: dm_file_exists, dm_file_is_readable, dm_file_size
        use :: dm_string, only: dm_string_is_empty
        use :: unix,      only: c_fclose, c_fopen

        type(rpc_request_type),  target, intent(inout) :: request  !! RPC request.
        type(rpc_response_type), target, intent(inout) :: response !! RPC response.

        integer :: crc, i, stat

        associate (ctx => request%curl, headers => request%headers, list => request%list)
            rc = E_NULL
            if (.not. c_associated(ctx)) return

            ! Reset HTTP header list.
            if (c_associated(list)) then
                rc = E_COMPILER
                call curl_slist_free_all(list)
                if (c_associated(list)) return
            end if

            ! Validate URL.
            rc = E_INVALID
            if (dm_string_is_empty(request%url)) return

            ! Set URL.
            rc  = E_RPC
            crc = curl_set(ctx, CURLOPT_URL, request%url); if (crc /= CURLE_OK) return

            ! Set HTTP accept header.
            if (.not. dm_string_is_empty(request%accept)) then
                list = curl_slist_append(list, 'Accept: ' // request%accept)
            end if

            ! Set HTTP Basic Auth header.
            if (request%auth == RPC_AUTH_BASIC) then
                crc = curl_set(ctx, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);   if (crc /= CURLE_OK) return ! Enable HTTP Basic Auth.
                crc = curl_set(ctx, CURLOPT_USERNAME, request%username); if (crc /= CURLE_OK) return ! Set user name.
                crc = curl_set(ctx, CURLOPT_PASSWORD, request%password); if (crc /= CURLE_OK) return ! Set password.
            end if

            ! Set response callback.
            if (associated(request%callback)) then
                crc = curl_set(ctx, CURLOPT_WRITEFUNCTION, c_funloc(request%callback)); if (crc /= CURLE_OK) return ! Set write function.
                crc = curl_set(ctx, CURLOPT_WRITEDATA,     c_loc(response));            if (crc /= CURLE_OK) return ! Set write function client data.
            end if

            method_select: select case (request%method)
                case (RPC_METHOD_POST)
                    ! Enable POST.
                    crc = curl_set(ctx, CURLOPT_POST, 1); if (crc /= CURLE_OK) return

                    ! Exit if POST payload is missing.
                    if (.not. allocated(request%payload)) exit method_select

                    ! Pass POST data directly.
                    crc = curl_set(ctx, CURLOPT_POSTFIELDSIZE, len(request%payload, kind=i8)); if (crc /= CURLE_OK) return
                    crc = curl_set(ctx, CURLOPT_POSTFIELDS,    c_loc(request%payload));        if (crc /= CURLE_OK) return

                    ! Signal content encoding (deflate, zstd).
                    if (request%compression > Z_TYPE_NONE) then
                        list = curl_slist_append(list, 'Content-Encoding: ' // dm_z_type_to_encoding(request%compression))
                    end if

                    ! Set content type.
                    if (.not. dm_string_is_empty(request%content_type)) then
                        list = curl_slist_append(list, 'Content-Type: ' // request%content_type)
                    end if

                case (RPC_METHOD_PUT)
                    ! Enable PUT.
                    crc = curl_set(ctx, CURLOPT_UPLOAD, 1); if (crc /= CURLE_OK) return

                    ! Add payload file.
                    rc = E_NOT_FOUND
                    if (.not. dm_file_exists(request%payload_path)) exit method_select

                    rc = E_ACCESS
                    if (.not. dm_file_is_readable(request%payload_path)) exit method_select

                    rc = E_IO
                    if (c_associated(request%file)) stat = c_fclose(request%file)
                    request%file = c_fopen(dm_f_c_string(request%payload_path), dm_f_c_string('r'))
                    if (.not. c_associated(request%file)) exit method_select

                    ! Set PUT read callback.
                    rc  = E_NONE
                    crc = curl_set(ctx, CURLOPT_READFUNCTION, c_funloc(dm_rpc_read_callback));     if (crc /= CURLE_OK) return
                    crc = curl_set(ctx, CURLOPT_READDATA,     request%file);                       if (crc /= CURLE_OK) return
                    crc = curl_set(ctx, CURLOPT_INFILESIZE,   dm_file_size(request%payload_path)); if (crc /= CURLE_OK) return

                    ! Signal content encoding (deflate, zstd).
                    if (request%compression > Z_TYPE_NONE) then
                        list = curl_slist_append(list, 'Content-Encoding: ' // dm_z_type_to_encoding(request%compression))
                    end if

                    ! Set content type.
                    if (.not. dm_string_is_empty(request%content_type)) then
                        list = curl_slist_append(list, 'Content-Type: ' // request%content_type)
                    end if

                    ! Add HTTP request headers.
                    if (allocated(request%headers)) then
                        do i = 1, size(headers)
                            associate (header => headers(i))
                                if (.not. allocated(header%name) .or. .not. allocated(header%value)) cycle
                                if (len_trim(header%name) == 0) cycle

                                if (len_trim(header%value) == 0) then
                                    list = curl_slist_append(list, trim(header%name) // ';')
                                else
                                    list = curl_slist_append(list, trim(header%name) // ': ' // trim(header%value))
                                end if
                            end associate
                        end do
                    end if

                case default
                    ! Only fetch if file has been modified since timestamp. May not be supported by the server.
                    if (request%modified_since > 0) then
                        crc = curl_set(ctx, CURLOPT_TIMECONDITION, CURL_TIMECOND_IFMODSINCE); if (crc /= CURLE_OK) return
                        crc = curl_set(ctx, CURLOPT_TIMEVALUE,     request%modified_since);   if (crc /= CURLE_OK) return
                    end if
            end select method_select

            ! Set follow location header.
            if (request%follow_location) then
                crc = curl_set(ctx, CURLOPT_FOLLOWLOCATION, 1); if (crc /= CURLE_OK) return
            end if

            crc = curl_set(ctx, CURLOPT_ACCEPT_ENCODING, 'deflate');                      if (crc /= CURLE_OK) return ! Set HTTP Accept header.
            crc = curl_set(ctx, CURLOPT_CONNECTTIMEOUT,  request%connect_timeout);        if (crc /= CURLE_OK) return ! Set connection timeout.
            crc = curl_set(ctx, CURLOPT_FILETIME,        1);                              if (crc /= CURLE_OK) return ! Get last modified time.
            crc = curl_set(ctx, CURLOPT_NOSIGNAL,        1);                              if (crc /= CURLE_OK) return ! No debug messages to stdout.
            crc = curl_set(ctx, CURLOPT_TCP_KEEPALIVE,   dm_f_c_logical(RPC_KEEP_ALIVE)); if (crc /= CURLE_OK) return ! Enable TCP keep-alive.
            crc = curl_set(ctx, CURLOPT_TCP_KEEPIDLE,    RPC_KEEP_ALIVE_IDLE);            if (crc /= CURLE_OK) return ! Set TCP keep-alive idle time in seconds.
            crc = curl_set(ctx, CURLOPT_TCP_KEEPINTVL,   RPC_KEEP_ALIVE_INTERVAL);        if (crc /= CURLE_OK) return ! Interval time between TCP keep-alive probes in seconds.
            crc = curl_set(ctx, CURLOPT_TIMEOUT,         request%timeout);                if (crc /= CURLE_OK) return ! Set read timeout.
            crc = curl_set(ctx, CURLOPT_VERBOSE,         0);                              if (crc /= CURLE_OK) return ! No verbose output.

            ! Set HTTP headers.
            if (c_associated(list)) then
                crc = curl_set(ctx, CURLOPT_HTTPHEADER, list); if (crc /= CURLE_OK) return
            end if

            ! User Agent.
            if (dm_string_is_empty(request%user_agent)) request%user_agent = RPC_USER_AGENT
            crc = curl_set(ctx, CURLOPT_USERAGENT, trim(request%user_agent)); if (crc /= CURLE_OK) return

            rc = E_NONE
        end associate
    end function rpc_request_prepare

    integer function rpc_request_single(request, response) result(rc)
        !! Sends single HTTP request by calling libcurl. The function returns
        !! the following error codes:
        !!
        !! * `E_COMPILER` if C pointers could not be nullified.
        !! * `E_IO` if payload file could not be closed.
        !! * `E_RPC` if the HTTP request failed.
        !!
        !! A more specific error code may be available in response attribute
        !! `error`.
        use :: unix, only: c_fclose

        type(rpc_request_type),  intent(inout) :: request  !! RPC request.
        type(rpc_response_type), intent(inout) :: response !! RPC response.

        integer :: crc

        rc = E_RPC

        ! Initialise libcurl.
        if (.not. c_associated(request%curl)) then
            request%curl = curl_init()
            if (.not. c_associated(request%curl)) return
        end if

        crc = CURLE_OK

        curl_block: block
            ! Prepare request.
            rc = rpc_request_prepare(request, response)
            if (dm_is_error(rc)) exit curl_block

            ! Perform request.
            crc = curl_perform(request%curl)
            rc  = dm_rpc_error(crc)
        end block curl_block

        call rpc_request_set_response(request, response, crc)

        ! Clean-up.
        if (c_associated(request%file)) then
            rc = E_IO

            if (c_fclose(request%file) == 0) then
                rc = E_NONE
                request%file = c_null_ptr
            end if
        end if

        call curl_slist_free_all(request%list)
        call curl_cleanup(request%curl)

        if (dm_is_error(rc)) return
        if (c_associated(request%curl) .or. c_associated(request%list)) rc = E_COMPILER
    end function rpc_request_single

    ! **************************************************************************
    ! PRIVATE SUBROUTINES
    ! **************************************************************************
    pure elemental subroutine rpc_header_destroy(header)
        !! Frees memory allocated by header type.
        type(rpc_header_type), intent(inout) :: header !! RPC header.

        if (allocated(header%name))  deallocate (header%name)
        if (allocated(header%value)) deallocate (header%value)
    end subroutine rpc_header_destroy

    impure elemental subroutine rpc_request_destroy(request)
        !! Frees memory allocated by request type.
        type(rpc_request_type), intent(inout) :: request !! RPC request.

        if (allocated(request%payload))      deallocate (request%payload)
        if (allocated(request%payload_path)) deallocate (request%payload_path)
        if (allocated(request%content_type)) deallocate (request%content_type)
        if (allocated(request%accept))       deallocate (request%accept)
        if (allocated(request%username))     deallocate (request%username)
        if (allocated(request%password))     deallocate (request%password)
        if (allocated(request%url))          deallocate (request%url)
        if (allocated(request%user_agent))   deallocate (request%user_agent)

        if (allocated(request%headers)) then
            call dm_rpc_destroy(request%headers)
            deallocate (request%headers)
        end if

        request%callback => null()
        call dm_rpc_reset(request)
    end subroutine rpc_request_destroy

    impure elemental subroutine rpc_request_reset(request)
        !! Auxiliary routine to reset request for future reuse. Cleans-up the
        !! libcurl handles of the request.
        use :: unix, only: c_fclose

        type(rpc_request_type), intent(inout) :: request !! RPC request.

        if (c_associated(request%file)) then
            if (c_fclose(request%file) == 0) request%file = c_null_ptr
        end if

        if (allocated(request%headers)) call dm_rpc_destroy(request%headers)

        call curl_slist_free_all(request%list)
        call curl_cleanup(request%curl)
    end subroutine rpc_request_reset

    subroutine rpc_request_set_response(request, response, error_curl)
        !! Sets HTTP response info to given RPC response.
        type(rpc_request_type),  intent(inout)        :: request    !! RPC request.
        type(rpc_response_type), intent(inout)        :: response   !! RPC response.
        integer,                 intent(in), optional :: error_curl !! libcurl error code.

        integer :: crc, error_curl_, i

        error_curl_ = dm_present(error_curl, response%error_curl)

        associate (ctx => request%curl, headers => response%headers)
            ! Response meta data and errors.
            if (error_curl_ == CURLE_OK) then
                crc = curl_get(ctx, CURLINFO_CONTENT_TYPE,  response%content_type)  ! Get content type.
                crc = curl_get(ctx, CURLINFO_FILETIME,      response%last_modified) ! Get file time.
                crc = curl_get(ctx, CURLINFO_RESPONSE_CODE, response%code)          ! Get HTTP response code.
                crc = curl_get(ctx, CURLINFO_TOTAL_TIME,    response%total_time)    ! Get transmission time.

                response%error         = E_NONE
                response%error_curl    = CURLE_OK
                response%error_message = ''
            else
                response%error         = dm_rpc_error(error_curl_)
                response%error_curl    = error_curl_
                response%error_message = dm_rpc_error_message(error_curl_)
            end if

            ! HTTP response headers. Only add predefined headers.
            if (allocated(response%headers)) then
                do i = 1, size(headers)
                    if (.not. allocated(headers(i)%name)) cycle
                    crc = rpc_response_header(request, headers(i)%name, headers(i)%value)
                end do
            end if

            if (.not. allocated(response%content_type)) response%content_type = ''
            if (.not. allocated(response%payload))      response%payload      = ''
        end associate
    end subroutine rpc_request_set_response

    pure elemental subroutine rpc_response_destroy(response)
        !! Frees memory allocated by response type.
        type(rpc_response_type), intent(inout) :: response !! RPC response.

        if (allocated(response%error_message)) deallocate (response%error_message)
        if (allocated(response%content_type))  deallocate (response%content_type)
        if (allocated(response%payload))       deallocate (response%payload)

        if (allocated(response%headers)) then
            call dm_rpc_destroy(response%headers)
            deallocate (response%headers)
        end if
    end subroutine rpc_response_destroy

    pure elemental subroutine rpc_response_reset(response, reset_unit)
        !! Auxiliary routine to reset response for future reuse.  Response
        !! headers are kept and only header values are deallocated.  This
        !! routine does not reset the file unit by default.
        type(rpc_response_type), intent(inout)        :: response   !! RPC response.
        logical,                 intent(in), optional :: reset_unit !! Reset file unit.

        logical :: reset_unit_

        reset_unit_ = dm_present(reset_unit, .false.)
        if (reset_unit_) response%unit = FILE_UNIT_NONE

        response%code          = HTTP_NONE
        response%error         = E_NONE
        response%error_curl    = CURLE_OK
        response%last_modified = -1_i8
        response%total_time    = 0.0_r8

        if (allocated(response%error_message)) deallocate (response%error_message)
        if (allocated(response%content_type))  deallocate (response%content_type)
        if (allocated(response%payload))       deallocate (response%payload)

        if (allocated(response%headers)) call dm_rpc_destroy(response%headers)
    end subroutine rpc_response_reset

    integer function rpc_response_header(request, name, value, n) result(rc)
        !! Returns response header value of name `name` in argument `value`. On
        !! error, `value` is allocated but empty.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if libcurl context is not associated.
        !! * `E_RPC` if reading of header failed.
        !!
        use :: dm_c

        type(rpc_request_type),    intent(inout)         :: request !! RPC request.
        character(*),              intent(in)            :: name    !! Header name.
        character(:), allocatable, intent(out)           :: value   !! Header value.
        integer(i8),               intent(out), optional :: n       !! Number of headers of this name.

        if (present(n)) n = 0_i8

        rpc_block: block
            integer                    :: crc
            type(curl_header), pointer :: header

            rc = E_NULL
            if (.not. c_associated(request%curl)) exit rpc_block

            rc  = E_RPC
            crc = curl_easy_header(request%curl, trim(name), 0_i8, CURLH_HEADER, -1, header)
            if (crc /= CURLHE_OK) exit rpc_block

            rc = E_NONE
            call dm_c_f_string_pointer(header%value, value)
            if (present(n)) n = int(header%amount, i8)
        end block rpc_block

        if (.not. allocated(value)) value = ''
    end function rpc_response_header
end module dm_rpc
