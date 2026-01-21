! nng_http.f90
!
! Author:  Philipp Engel
! Licence: ISC
module nng_http
    !! Bindings to `supplemental/http/http.h`.
    use :: nng, only: c_bool, c_char, c_funptr, c_int, c_ptr, c_size_t, c_uint16_t
    use :: nng_util
    implicit none (type, external)
    private

    ! enum nng_http_status
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_CONTINUE                 = 100
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_SWITCHING                = 101
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_PROCESSING               = 102
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_OK                       = 200
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_CREATED                  = 201
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_ACCEPTED                 = 202
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_NOT_AUTHORITATIVE        = 203
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_NO_CONTENT               = 204
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_RESET_CONTENT            = 205
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_PARTIAL_CONTENT          = 206
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_MULTI_STATUS             = 207
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_ALREADY_REPORTED         = 208
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_IM_USED                  = 226
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_MULTIPLE_CHOICES         = 300
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_STATUS_MOVED_PERMANENTLY = 301
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_FOUND                    = 302
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_SEE_OTHER                = 303
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_NOT_MODIFIED             = 304
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_USE_PROXY                = 305
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_TEMPORARY_REDIRECT       = 307
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_PERMANENT_REDIRECT       = 308
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_BAD_REQUEST              = 400
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_UNAUTHORIZED             = 401
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_PAYMENT_REQUIRED         = 402
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_FORBIDDEN                = 403
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_NOT_FOUND                = 404
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_METHOD_NOT_ALLOWED       = 405
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_NOT_ACCEPTABLE           = 406
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_PROXY_AUTH_REQUIRED      = 407
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_REQUEST_TIMEOUT          = 408
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_CONFLICT                 = 409
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_GONE                     = 410
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_LENGTH_REQUIRED          = 411
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_PRECONDITION_FAILED      = 412
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_PAYLOAD_TOO_LARGE        = 413
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_ENTITY_TOO_LONG          = 414
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_UNSUPPORTED_MEDIA_TYPE   = 415
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_RANGE_NOT_SATISFIABLE    = 416
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_EXPECTATION_FAILED       = 417
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_TEAPOT                   = 418
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_UNPROCESSABLE_ENTITY     = 422
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_LOCKED                   = 423
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_FAILED_DEPENDENCY        = 424
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_UPGRADE_REQUIRED         = 426
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_PRECONDITION_REQUIRED    = 428
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_TOO_MANY_REQUESTS        = 429
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_HEADERS_TOO_LARGE        = 431
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_UNAVAIL_LEGAL_REASONS    = 451
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_INTERNAL_SERVER_ERROR    = 500
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_NOT_IMPLEMENTED          = 501
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_BAD_GATEWAY              = 502
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_SERVICE_UNAVAILABLE      = 503
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_GATEWAY_TIMEOUT          = 504
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_HTTP_VERSION_NOT_SUPP    = 505
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_VARIANT_ALSO_NEGOTIATES  = 506
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_INSUFFICIENT_STORAGE     = 507
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_LOOP_DETECTED            = 508
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_NOT_EXTENDED             = 510
    integer(c_int), parameter, public :: NNG_HTTP_STATUS_NETWORK_AUTH_REQUIRED    = 511

    public :: nng_http_client_alloc
    public :: nng_http_client_connect
    public :: nng_http_client_free
    public :: nng_http_client_get_tls
    public :: nng_http_client_set_tls
    public :: nng_http_client_transact
    public :: nng_http_conn_close
    public :: nng_http_conn_read
    public :: nng_http_conn_read_all
    public :: nng_http_conn_read_req
    public :: nng_http_conn_read_res
    public :: nng_http_conn_transact
    public :: nng_http_conn_write
    public :: nng_http_conn_write_all
    public :: nng_http_conn_write_req
    public :: nng_http_conn_write_res
    public :: nng_http_handler_alloc
    public :: nng_http_handler_alloc_directory
    public :: nng_http_handler_alloc_file
    public :: nng_http_handler_alloc_redirect
    public :: nng_http_handler_alloc_static
    public :: nng_http_handler_collect_body
    public :: nng_http_handler_free
    public :: nng_http_handler_get_data
    public :: nng_http_handler_set_data
    public :: nng_http_handler_set_host
    public :: nng_http_handler_set_method
    public :: nng_http_handler_set_tree
    public :: nng_http_handler_set_tree_exclusive
    public :: nng_http_hijack
    public :: nng_http_req_add_header
    public :: nng_http_req_alloc
    public :: nng_http_req_copy_data
    public :: nng_http_req_del_header
    public :: nng_http_req_free
    public :: nng_http_req_get_data
    public :: nng_http_req_get_header
    public :: nng_http_req_get_header_
    public :: nng_http_req_get_method
    public :: nng_http_req_get_method_
    public :: nng_http_req_get_uri
    public :: nng_http_req_get_uri_
    public :: nng_http_req_get_version
    public :: nng_http_req_get_version_
    public :: nng_http_req_reset
    public :: nng_http_req_set_data
    public :: nng_http_req_set_header
    public :: nng_http_req_set_method
    public :: nng_http_req_set_uri
    public :: nng_http_req_set_version
    public :: nng_http_res_add_header
    public :: nng_http_res_alloc
    public :: nng_http_res_alloc_error
    public :: nng_http_res_copy_data
    public :: nng_http_res_del_header
    public :: nng_http_res_free
    public :: nng_http_res_get_data
    public :: nng_http_res_get_header
    public :: nng_http_res_get_header_
    public :: nng_http_res_get_reason
    public :: nng_http_res_get_reason_
    public :: nng_http_res_get_status
    public :: nng_http_res_get_version
    public :: nng_http_res_get_version_
    public :: nng_http_res_reset
    public :: nng_http_res_set_data
    public :: nng_http_res_set_header
    public :: nng_http_res_set_reason
    public :: nng_http_res_set_status
    public :: nng_http_res_set_version
    public :: nng_http_server_add_handler
    public :: nng_http_server_del_handler
    public :: nng_http_server_get_addr
    public :: nng_http_server_get_tls
    public :: nng_http_server_hold
    public :: nng_http_server_release
    public :: nng_http_server_res_error
    public :: nng_http_server_set_error_file
    public :: nng_http_server_set_error_page
    public :: nng_http_server_set_tls
    public :: nng_http_server_start
    public :: nng_http_server_stop

    interface
        ! int nng_http_client_alloc(nng_http_client **clientp, const nng_url *url)
        function nng_http_client_alloc(clientp, url) bind(c, name='nng_http_client_alloc')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out)       :: clientp
            type(c_ptr), intent(in), value :: url
            integer(c_int)                 :: nng_http_client_alloc
        end function nng_http_client_alloc

        ! void nng_http_client_connect(nng_http_client *client, nng_aio *aio)
        subroutine nng_http_client_connect(client, aio) bind(c, name='nng_http_client_connect')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: client
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_client_connect

        ! void nng_http_client_free(nng_http_client *client)
        subroutine nng_http_client_free(client) bind(c, name='nng_http_client_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: client
        end subroutine nng_http_client_free

        ! int nng_http_client_get_tls(nng_http_client *client, nng_tls_config **cfgp)
        function nng_http_client_get_tls(client, cfgp) bind(c, name='nng_http_client_get_tls')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: client
            type(c_ptr), intent(out)       :: cfgp
            integer(c_int)                 :: nng_http_client_get_tls
        end function nng_http_client_get_tls

        ! int nng_http_client_set_tls(nng_http_client *client, nng_tls_config *cfg)
        function nng_http_client_set_tls(client, cfg) bind(c, name='nng_http_client_set_tls')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: client
            type(c_ptr), intent(in), value :: cfg
            integer(c_int)                 :: nng_http_client_set_tls
        end function nng_http_client_set_tls

        ! void nng_http_client_transact(nng_http_client *client, nng_http_req *req, nng_http_res *res, nng_aio *aio)
        subroutine nng_http_client_transact(client, req, res, aio) bind(c, name='nng_http_client_transact')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: client
            type(c_ptr), intent(in), value :: req
            type(c_ptr), intent(in), value :: res
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_client_transact

        ! void nng_http_conn_close(nng_http_conn *conn)
        subroutine nng_http_conn_close(conn) bind(c, name='nng_http_conn_close')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
        end subroutine nng_http_conn_close

        ! void nng_http_conn_read(nng_http_conn *conn, nng_aio *aio)
        subroutine nng_http_conn_read(conn, aio) bind(c, name='nng_http_conn_read')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_conn_read

        ! void nng_http_conn_read_all(nng_http_conn *conn, nng_aio *aio)
        subroutine nng_http_conn_read_all(conn, aio) bind(c, name='nng_http_conn_read_all')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_conn_read_all

        ! void nng_http_conn_read_req(nng_http_conn *conn, nng_http_req *req, nng_aio *aio)
        subroutine nng_http_conn_read_req(conn, req, aio) bind(c, name='nng_http_conn_read_req')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: req
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_conn_read_req

        ! void nng_http_conn_read_res(nng_http_conn *conn, nng_http_res *res, nng_aio *aio)
        subroutine nng_http_conn_read_res(conn, res, aio) bind(c, name='nng_http_conn_read_res')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: res
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_conn_read_res

        ! void nng_http_conn_transact(nng_http_conn *conn, nng_http_req *req, nng_http_res *res, nng_aio *aio)
        subroutine nng_http_conn_transact(conn, req, res, aio) bind(c, name='nng_http_conn_transact')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: req
            type(c_ptr), intent(in), value :: res
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_conn_transact

        ! void nng_http_conn_write(nng_http_conn *conn, nng_aio *aio)
        subroutine nng_http_conn_write(conn, aio) bind(c, name='nng_http_conn_write')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_conn_write

        ! void nng_http_conn_write_all(nng_http_conn *conn, nng_aio *aio)
        subroutine nng_http_conn_write_all(conn, aio) bind(c, name='nng_http_conn_write_all')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_conn_write_all

        ! void nng_http_conn_write_req(nng_http_conn *conn, nng_http_req *req, nng_aio *aio)
        subroutine nng_http_conn_write_req(conn, req, aio) bind(c, name='nng_http_conn_write_req')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: req
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_conn_write_req

        ! void nng_http_conn_write_res(nng_http_conn *conn, nng_http_res *res, nng_aio *aio)
        subroutine nng_http_conn_write_res(conn, res, aio) bind(c, name='nng_http_conn_write_res')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            type(c_ptr), intent(in), value :: res
            type(c_ptr), intent(in), value :: aio
        end subroutine nng_http_conn_write_res

        ! int nng_http_handler_alloc(nng_http_handler **hp, const char *path, void (*fn)(nng_aio *aio))
        function nng_http_handler_alloc(hp, path, fn) bind(c, name='nng_http_handler_alloc')
            import :: c_char, c_funptr, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(out)       :: hp
            character(c_char), intent(in)        :: path
            type(c_funptr),    intent(in), value :: fn
            integer(c_int)                       :: nng_http_handler_alloc
        end function nng_http_handler_alloc

        ! int nng_http_handler_alloc_directory(nng_http_handler **hp, const char *path, const char *dirname)
        function nng_http_handler_alloc_directory(hp, path, dirname) bind(c, name='nng_http_handler_alloc_directory')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(out) :: hp
            character(c_char), intent(in)  :: path
            character(c_char), intent(in)  :: dirname
            integer(c_int)                 :: nng_http_handler_alloc_directory
        end function nng_http_handler_alloc_directory

        ! int nng_http_handler_alloc_file(nng_http_handler **hp, const char *path, const char *filename)
        function nng_http_handler_alloc_file(hp, path, filename) bind(c, name='nng_http_handler_alloc_file')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(out) :: hp
            character(c_char), intent(in)  :: path
            character(c_char), intent(in)  :: filename
            integer(c_int)                 :: nng_http_handler_alloc_file
        end function nng_http_handler_alloc_file

        ! int nng_http_handler_alloc_redirect(nng_http_handler **hp, const char *path, uint16_t status, const char *location)
        function nng_http_handler_alloc_redirect(hp, path, status, location) bind(c, name='nng_http_handler_alloc_redirect')
            import :: c_char, c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(out)       :: hp
            character(c_char),   intent(in)        :: path
            integer(c_uint16_t), intent(in), value :: status
            character(c_char),   intent(in)        :: location
            integer(c_int)                         :: nng_http_handler_alloc_redirect
        end function nng_http_handler_alloc_redirect

        ! int nng_http_handler_alloc_static(nng_http_handler **hp, const char *path, const void *data, size_t size, const char *content_type)
        function nng_http_handler_alloc_static(hp, path, data, size, content_type) bind(c, name='nng_http_handler_alloc_static')
            import :: c_char, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(out)       :: hp
            character(c_char), intent(in)        :: path
            type(c_ptr),       intent(in), value :: data
            integer(c_size_t), intent(in), value :: size
            character(c_char), intent(in)        :: content_type
            integer(c_int)                       :: nng_http_handler_alloc_static
        end function nng_http_handler_alloc_static

        ! int nng_http_handler_collect_body(nng_http_handler *h, bool want, size_t maxsz)
        function nng_http_handler_collect_body(h, want, maxsz) bind(c, name='nng_http_handler_collect_body')
            import :: c_bool, c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: h
            logical(c_bool),   intent(in), value :: want
            integer(c_size_t), intent(in), value :: maxsz
            integer(c_int)                       :: nng_http_handler_collect_body
        end function nng_http_handler_collect_body

        ! void nng_http_handler_free(nng_http_handler *h)
        subroutine nng_http_handler_free(h) bind(c, name='nng_http_handler_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: h
        end subroutine nng_http_handler_free

        ! void *nng_http_handler_get_data(nng_http_handler *h)
        function nng_http_handler_get_data(h) bind(c, name='nng_http_handler_get_data')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: h
            type(c_ptr)                    :: nng_http_handler_get_data
        end function nng_http_handler_get_data

        ! int nng_http_handler_set_data(nng_http_handler *h, void *data, void (*dtor)(void *))
        function nng_http_handler_set_data(h, data, dtor) bind(c, name='nng_http_handler_set_data')
            import :: c_funptr, c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: h
            type(c_ptr),    intent(in), value :: data
            type(c_funptr), intent(in), value :: dtor
            integer(c_int)                    :: nng_http_handler_set_data
        end function nng_http_handler_set_data

        ! int nng_http_handler_set_host(nng_http_handler *h, const char *host)
        function nng_http_handler_set_host(h, host) bind(c, name='nng_http_handler_set_host')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: h
            character(c_char), intent(in)        :: host
            integer(c_int)                       :: nng_http_handler_set_host
        end function nng_http_handler_set_host

        ! int nng_http_handler_set_method(nng_http_handler *h, const char *method)
        function nng_http_handler_set_method(h, method) bind(c, name='nng_http_handler_set_method')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: h
            character(c_char), intent(in)        :: method
            integer(c_int)                       :: nng_http_handler_set_method
        end function nng_http_handler_set_method

        ! int nng_http_handler_set_tree(nng_http_handler *h)
        function nng_http_handler_set_tree(h) bind(c, name='nng_http_handler_set_tree')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: h
            integer(c_int)                 :: nng_http_handler_set_tree
        end function nng_http_handler_set_tree

        ! int nng_http_handler_set_tree_exclusive(nng_http_handler *h)
        function nng_http_handler_set_tree_exclusive(h) bind(c, name='nng_http_handler_set_tree_exclusive')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: h
            integer(c_int)                 :: nng_http_handler_set_tree_exclusive
        end function nng_http_handler_set_tree_exclusive

        ! int nng_http_hijack(nng_http_conn *conn)
        function nng_http_hijack(conn) bind(c, name='nng_http_hijack')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: conn
            integer(c_int)                 :: nng_http_hijack
        end function nng_http_hijack

        ! int nng_http_req_add_header(nng_http_req *req, const char *key, const char *val)
        function nng_http_req_add_header(req, key, val) bind(c, name='nng_http_req_add_header')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: req
            character(c_char), intent(in)        :: key
            character(c_char), intent(in)        :: val
            integer(c_int)                       :: nng_http_req_add_header
        end function nng_http_req_add_header

        ! int nng_http_req_alloc(nng_http_req **reqp, const nng_url *url)
        function nng_http_req_alloc(reqp, url) bind(c, name='nng_http_req_alloc')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out)       :: reqp
            type(c_ptr), intent(in), value :: url
            integer(c_int)                 :: nng_http_req_alloc
        end function nng_http_req_alloc

        ! int nng_http_req_copy_data(nng_http_req *req, const void *body, size_t size)
        function nng_http_req_copy_data(req, body, size) bind(c, name='nng_http_req_copy_data')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: req
            type(c_ptr),       intent(in), value :: body
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_http_req_copy_data
        end function nng_http_req_copy_data

        ! int nng_http_req_del_header(nng_http_req *req, const char *key)
        function nng_http_req_del_header(req, key) bind(c, name='nng_http_req_del_header')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: req
            character(c_char), intent(in)        :: key
            integer(c_int)                       :: nng_http_req_del_header
        end function nng_http_req_del_header

        ! void nng_http_req_free(nng_http_req *req)
        subroutine nng_http_req_free(req) bind(c, name='nng_http_req_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: req
        end subroutine nng_http_req_free

        ! void nng_http_req_get_data(nng_http_req *req, void **bodyp, size_t *sizep)
        subroutine nng_http_req_get_data(req, bodyp, sizep) bind(c, name='nng_http_req_get_data')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: req
            type(c_ptr),       intent(out)       :: bodyp
            integer(c_size_t), intent(out)       :: sizep
        end subroutine nng_http_req_get_data

        ! const char *nng_http_req_get_header(nng_http_req *req, const char *key)
        function nng_http_req_get_header_(req, key) bind(c, name='nng_http_req_get_header')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: req
            character(c_char), intent(in)        :: key
            type(c_ptr)                          :: nng_http_req_get_header_
        end function nng_http_req_get_header_

        ! const char *nng_http_req_get_method(nng_http_req *req)
        function nng_http_req_get_method_(req) bind(c, name='nng_http_req_get_method')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: req
            type(c_ptr)                    :: nng_http_req_get_method_
        end function nng_http_req_get_method_

        ! const char *nng_http_req_get_uri(nng_http_req *req)
        function nng_http_req_get_uri_(req) bind(c, name='nng_http_req_get_uri')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: req
            type(c_ptr)                    :: nng_http_req_get_uri_
        end function nng_http_req_get_uri_

        ! const char *nng_http_req_get_version(nng_http_req *req)
        function nng_http_req_get_version_(req) bind(c, name='nng_http_req_get_version')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: req
            type(c_ptr)                    :: nng_http_req_get_version_
        end function nng_http_req_get_version_

        ! void nng_http_req_reset(nng_http_req *req)
        subroutine nng_http_req_reset(req) bind(c, name='nng_http_req_reset')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: req
        end subroutine nng_http_req_reset

        ! int nng_http_req_set_data(nng_http_req *req, const void *body, size_t size)
        function nng_http_req_set_data(req, body, size) bind(c, name='nng_http_req_set_data')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: req
            type(c_ptr),       intent(in), value :: body
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_http_req_set_data
        end function nng_http_req_set_data

        ! int nng_http_req_set_header(nng_http_req *req, const char *key, const char *val)
        function nng_http_req_set_header(req, key, val) bind(c, name='nng_http_req_set_header')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: req
            character(c_char), intent(in)        :: key
            character(c_char), intent(in)        :: val
            integer(c_int)                       :: nng_http_req_set_header
        end function nng_http_req_set_header

        ! int nng_http_req_set_method(nng_http_req *req, const char *method)
        function nng_http_req_set_method(req, method) bind(c, name='nng_http_req_set_method')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: req
            character(c_char), intent(in)        :: method
            integer(c_int)                       :: nng_http_req_set_method
        end function nng_http_req_set_method

        ! int nng_http_req_set_uri(nng_http_req *req, const char *uri)
        function nng_http_req_set_uri(req, uri) bind(c, name='nng_http_req_set_uri')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: req
            character(c_char), intent(in)        :: uri
            integer(c_int)                       :: nng_http_req_set_uri
        end function nng_http_req_set_uri

        ! int nng_http_req_set_version(nng_http_req *req, const char *version)
        function nng_http_req_set_version(req, version) bind(c, name='nng_http_req_set_version')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: req
            character(c_char), intent(in)        :: version
            integer(c_int)                       :: nng_http_req_set_version
        end function nng_http_req_set_version

        ! int nng_http_res_add_header(nng_http_res *res, const char *key, const char *val)
        function nng_http_res_add_header(res, key, val) bind(c, name='nng_http_res_add_header')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: res
            character(c_char), intent(in)        :: key
            character(c_char), intent(in)        :: val
            integer(c_int)                       :: nng_http_res_add_header
        end function nng_http_res_add_header

        ! int nng_http_res_alloc(nng_http_res **resp)
        function nng_http_res_alloc(resp) bind(c, name='nng_http_res_alloc')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out) :: resp
            integer(c_int)           :: nng_http_res_alloc
        end function nng_http_res_alloc

        ! int nng_http_res_alloc_error(nng_http_res **resp, uint16_t status)
        function nng_http_res_alloc_error(resp, status) bind(c, name='nng_http_res_alloc_error')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(out)       :: resp
            integer(c_uint16_t), intent(in), value :: status
            integer(c_int)                         :: nng_http_res_alloc_error
        end function nng_http_res_alloc_error

        ! int nng_http_res_copy_data(nng_http_res *res, const void *body, size_t size)
        function nng_http_res_copy_data(res, body, size) bind(c, name='nng_http_res_copy_data')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: res
            type(c_ptr),       intent(in), value :: body
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_http_res_copy_data
        end function nng_http_res_copy_data

        ! int nng_http_res_del_header(nng_http_res *res, const char *key)
        function nng_http_res_del_header(res, key) bind(c, name='nng_http_res_del_header')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: res
            character(c_char), intent(in)        :: key
            integer(c_int)                       :: nng_http_res_del_header
        end function nng_http_res_del_header

        ! void nng_http_res_free(nng_http_res *res)
        subroutine nng_http_res_free(res) bind(c, name='nng_http_res_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: res
        end subroutine nng_http_res_free

        ! void nng_http_res_get_data(nng_http_res *res, void **bodyp, size_t *sizep)
        subroutine nng_http_res_get_data(res, bodyp, sizep) bind(c, name='nng_http_res_get_data')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: res
            type(c_ptr),       intent(out)       :: bodyp
            integer(c_size_t), intent(out)       :: sizep
        end subroutine nng_http_res_get_data

        ! const char *nng_http_res_get_header(nng_http_res *res, const char *key)
        function nng_http_res_get_header_(res, key) bind(c, name='nng_http_res_get_header')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: res
            character(c_char), intent(in)        :: key
            type(c_ptr)                          :: nng_http_res_get_header_
        end function nng_http_res_get_header_

        ! const char *nng_http_res_get_reason(nng_http_res *res)
        function nng_http_res_get_reason_(res) bind(c, name='nng_http_res_get_reason')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: res
            type(c_ptr)                    :: nng_http_res_get_reason_
        end function nng_http_res_get_reason_

        ! uint16_t nng_http_res_get_status(nng_http_res *res)
        function nng_http_res_get_status(res) bind(c, name='nng_http_res_get_status')
            import :: c_ptr, c_uint16_t
            implicit none
            type(c_ptr), intent(in), value :: res
            integer(c_uint16_t)            :: nng_http_res_get_status
        end function nng_http_res_get_status

        ! const char *nng_http_res_get_version(nng_http_res *res)
        function nng_http_res_get_version_(res) bind(c, name='nng_http_res_get_version')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: res
            type(c_ptr)                    :: nng_http_res_get_version_
        end function nng_http_res_get_version_

        ! void nng_http_res_reset(nng_http_res *res)
        subroutine nng_http_res_reset(res) bind(c, name='nng_http_res_reset')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: res
        end subroutine nng_http_res_reset

        ! int nng_http_res_set_data(nng_http_res *res, const void *body, size_t size)
        function nng_http_res_set_data(res, body, size) bind(c, name='nng_http_res_set_data')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: res
            type(c_ptr),       intent(in), value :: body
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: nng_http_res_set_data
        end function nng_http_res_set_data

        ! int nng_http_res_set_header(nng_http_res *res, const char *key, const char *val)
        function nng_http_res_set_header(res, key, val) bind(c, name='nng_http_res_set_header')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: res
            character(c_char), intent(in)        :: key
            character(c_char), intent(in)        :: val
            integer(c_int)                       :: nng_http_res_set_header
        end function nng_http_res_set_header

        ! int nng_http_res_set_reason(nng_http_res *res, const char *reason)
        function nng_http_res_set_reason(res, reason) bind(c, name='nng_http_res_set_reason')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: res
            character(c_char), intent(in)        :: reason
            integer(c_int)                       :: nng_http_res_set_reason
        end function nng_http_res_set_reason

        ! int nng_http_res_set_status(nng_http_res *res, uint16_t status)
        function nng_http_res_set_status(res, status) bind(c, name='nng_http_res_set_status')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: res
            integer(c_uint16_t), intent(in), value :: status
            integer(c_int)                         :: nng_http_res_set_status
        end function nng_http_res_set_status

        ! int nng_http_res_set_version(nng_http_res *res, const char *version)
        function nng_http_res_set_version(res, version) bind(c, name='nng_http_res_set_version')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: res
            character(c_char), intent(in)        :: version
            integer(c_int)                       :: nng_http_res_set_version
        end function nng_http_res_set_version

        ! int nng_http_server_add_handler(nng_http_server *server, nng_http_handler *h)
        function nng_http_server_add_handler(server, h) bind(c, name='nng_http_server_add_handler')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: server
            type(c_ptr), intent(in), value :: h
            integer(c_int)                 :: nng_http_server_add_handler
        end function nng_http_server_add_handler

        ! int nng_http_server_del_handler(nng_http_server *server, nng_http_handler *h)
        function nng_http_server_del_handler(server, h) bind(c, name='nng_http_server_del_handler')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: server
            type(c_ptr), intent(in), value :: h
            integer(c_int)                 :: nng_http_server_del_handler
        end function nng_http_server_del_handler

        ! int nng_http_server_get_addr(nng_http_server *server, nng_sockaddr *sa)
        function nng_http_server_get_addr(server, sa) bind(c, name='nng_http_server_get_addr')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: server
            type(c_ptr), intent(in), value :: sa
            integer(c_int)                 :: nng_http_server_get_addr
        end function nng_http_server_get_addr

        ! int nng_http_server_get_tls(nng_http_server *server, nng_tls_config **cfgp)
        function nng_http_server_get_tls(server, cfgp) bind(c, name='nng_http_server_get_tls')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: server
            type(c_ptr), intent(out)       :: cfgp
            integer(c_int)                 :: nng_http_server_get_tls
        end function nng_http_server_get_tls

        ! int nng_http_server_hold(nng_http_server **serverp, const nng_url *url)
        function nng_http_server_hold(serverp, url) bind(c, name='nng_http_server_hold')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(out)       :: serverp
            type(c_ptr), intent(in), value :: url
            integer(c_int)                 :: nng_http_server_hold
        end function nng_http_server_hold

        ! void nng_http_server_release(nng_http_server *server)
        subroutine nng_http_server_release(server) bind(c, name='nng_http_server_release')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: server
        end subroutine nng_http_server_release

        ! int nng_http_server_res_error(nng_http_server *server, nng_http_res *res)
        function nng_http_server_res_error(server, res) bind(c, name='nng_http_server_res_error')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: server
            type(c_ptr), intent(in), value :: res
            integer(c_int)                 :: nng_http_server_res_error
        end function nng_http_server_res_error

        ! int nng_http_server_set_error_file(nng_http_server *server, uint16_t code, const char *path)
        function nng_http_server_set_error_file(server, code, path) bind(c, name='nng_http_server_set_error_file')
            import :: c_char, c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: server
            integer(c_uint16_t), intent(in), value :: code
            character(c_char),   intent(in)        :: path
            integer(c_int)                         :: nng_http_server_set_error_file
        end function nng_http_server_set_error_file

        ! int nng_http_server_set_error_page(nng_http_server *server, uint16_t code, const char *html)
        function nng_http_server_set_error_page(server, code, html) bind(c, name='nng_http_server_set_error_page')
            import :: c_char, c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),         intent(in), value :: server
            integer(c_uint16_t), intent(in), value :: code
            character(c_char),   intent(in)        :: html
            integer(c_int)                         :: nng_http_server_set_error_page
        end function nng_http_server_set_error_page

        ! int nng_http_server_set_tls(nng_http_server *server, nng_tls_config *cfg)
        function nng_http_server_set_tls(server, cfg) bind(c, name='nng_http_server_set_tls')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: server
            type(c_ptr), intent(in), value :: cfg
            integer(c_int)                 :: nng_http_server_set_tls
        end function nng_http_server_set_tls

        ! int nng_http_server_start(nng_http_server *server)
        function nng_http_server_start(server) bind(c, name='nng_http_server_start')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: server
            integer(c_int)                 :: nng_http_server_start
        end function nng_http_server_start

        ! void nng_http_server_stop(nng_http_server *server)
        subroutine nng_http_server_stop(server) bind(c, name='nng_http_server_stop')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: server
        end subroutine nng_http_server_stop
    end interface
contains
    ! const char *nng_http_req_get_header(nng_http_req *req, const char *key)
    function nng_http_req_get_header(req, key) result(str)
        type(c_ptr),  intent(in)  :: req
        character(*), intent(in)  :: key
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_http_req_get_header_(req, key)
        call c_f_str_ptr(ptr, str)
    end function nng_http_req_get_header

    ! const char *nng_http_req_get_method(nng_http_req *req)
    function nng_http_req_get_method(req) result(str)
        type(c_ptr),  intent(in)  :: req
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_http_req_get_method_(req)
        call c_f_str_ptr(ptr, str)
    end function nng_http_req_get_method

    ! const char *nng_http_req_get_uri(nng_http_req *req)
    function nng_http_req_get_uri(req) result(str)
        type(c_ptr),  intent(in)  :: req
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_http_req_get_uri_(req)
        call c_f_str_ptr(ptr, str)
    end function nng_http_req_get_uri

    ! const char *nng_http_req_get_version(nng_http_req *req)
    function nng_http_req_get_version(req) result(str)
        type(c_ptr),  intent(in)  :: req
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_http_req_get_version_(req)
        call c_f_str_ptr(ptr, str)
    end function nng_http_req_get_version

    ! const char *nng_http_res_get_header(nng_http_res *res, const char *key)
    function nng_http_res_get_header(res, key) result(str)
        type(c_ptr),  intent(in)  :: res
        character(*), intent(in)  :: key
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_http_res_get_header_(res, key)
        call c_f_str_ptr(ptr, str)
    end function nng_http_res_get_header

    ! const char *nng_http_res_get_reason(nng_http_res *res)
    function nng_http_res_get_reason(res) result(str)
        type(c_ptr), intent(in)   :: res
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_http_res_get_reason_(res)
        call c_f_str_ptr(ptr, str)
    end function nng_http_res_get_reason

    ! const char *nng_http_res_get_version(nng_http_res *res)
    function nng_http_res_get_version(res) result(str)
        type(c_ptr), intent(in)   :: res
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = nng_http_res_get_version_(res)
        call c_f_str_ptr(ptr, str)
    end function nng_http_res_get_version
end module nng_http
