! Author:  Philipp Engel
! Licence: ISC
module dm_http
    !! HTTP status codes.
    implicit none (type, external)
    private

    ! HTTP 1.1 status codes (incomplete).
    integer, parameter, public :: HTTP_OK                            = 200
    integer, parameter, public :: HTTP_CREATED                       = 201
    integer, parameter, public :: HTTP_ACCEPTED                      = 202
    integer, parameter, public :: HTTP_NO_CONTENT                    = 204
    integer, parameter, public :: HTTP_RESET_CONTENT                 = 205

    integer, parameter, public :: HTTP_BAD_REQUEST                   = 400
    integer, parameter, public :: HTTP_UNAUTHORIZED                  = 401
    integer, parameter, public :: HTTP_PAYMENT_REQUIRED              = 402
    integer, parameter, public :: HTTP_FORBIDDEN                     = 403
    integer, parameter, public :: HTTP_NOT_FOUND                     = 404
    integer, parameter, public :: HTTP_METHOD_NOT_ALLOWED            = 405
    integer, parameter, public :: HTTP_NOT_ACCEPTABLE                = 406
    integer, parameter, public :: HTTP_PROXY_AUTHENTICATION_REQUIRED = 407
    integer, parameter, public :: HTTP_REQUEST_TIMEOUT               = 408
    integer, parameter, public :: HTTP_CONFLICT                      = 409
    integer, parameter, public :: HTTP_GONE                          = 410
    integer, parameter, public :: HTTP_LENGTH_REQUIRED               = 411
    integer, parameter, public :: HTTP_UNSUPPORTED_MEDIA_TYPE        = 415

    integer, parameter, public :: HTTP_INTERNAL_SERVER_ERROR         = 500
    integer, parameter, public :: HTTP_NOT_IMPLEMENTED               = 501
    integer, parameter, public :: HTTP_BAD_GATEWAY                   = 502
    integer, parameter, public :: HTTP_SERVICE_UNAVAILABLE           = 503

    public :: dm_http_status_string
contains
    pure function dm_http_status_string(status) result(string)
        !! Returns allocatable string of HTTP status. Returns an empty string
        !! if the passed status is unknown.
        integer, intent(in)           :: status !! HTTP code.
        character(len=:), allocatable :: string !! Status string.

        select case (status)
            ! 20X
            case (HTTP_OK);                            string = 'OK'
            case (HTTP_CREATED);                       string = 'Created'
            case (HTTP_ACCEPTED);                      string = 'Accepted'
            case (HTTP_NO_CONTENT);                    string = 'No Content'
            case (HTTP_RESET_CONTENT);                 string = 'Reset Content'
            ! 4XX
            case (HTTP_BAD_REQUEST);                   string = 'Bad Request'
            case (HTTP_UNAUTHORIZED);                  string = 'Unauthorized'
            case (HTTP_PAYMENT_REQUIRED);              string = 'Payment Required'
            case (HTTP_FORBIDDEN);                     string = 'Forbidden'
            case (HTTP_NOT_FOUND);                     string = 'Not Found'
            case (HTTP_METHOD_NOT_ALLOWED);            string = 'Method Not Allowed'
            case (HTTP_NOT_ACCEPTABLE);                string = 'Not Acceptable'
            case (HTTP_PROXY_AUTHENTICATION_REQUIRED); string = 'Proxy Authentication Required'
            case (HTTP_REQUEST_TIMEOUT);               string = 'Request Timeout'
            case (HTTP_CONFLICT);                      string = 'Conflict'
            case (HTTP_GONE);                          string = 'Gone'
            case (HTTP_LENGTH_REQUIRED);               string = 'Length Required'
            case (HTTP_UNSUPPORTED_MEDIA_TYPE);        string = 'Unsupported Media Type'
            ! 50X
            case (HTTP_INTERNAL_SERVER_ERROR);         string = 'Internal Server Error'
            case (HTTP_NOT_IMPLEMENTED);               string = 'Not Implemented'
            case (HTTP_BAD_GATEWAY);                   string = 'Bad Gateway'
            case (HTTP_SERVICE_UNAVAILABLE);           string = 'Service Unavailable'
            ! NIY
            case default;                              string = ''
        end select
    end function dm_http_status_string
end module dm_http
