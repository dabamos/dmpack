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
    pure function dm_http_status_string(http_status) result(str)
        !! Returns allocatable string of HTTP status. Returns an empty string
        !! if the passed status is unknown.
        integer, intent(in)           :: http_status !! HTTP code.
        character(len=:), allocatable :: str         !! Status string.

        select case (http_status)
            ! 20X
            case (HTTP_OK);                            str = 'OK'
            case (HTTP_CREATED);                       str = 'Created'
            case (HTTP_ACCEPTED);                      str = 'Accepted'
            case (HTTP_NO_CONTENT);                    str = 'No Content'
            case (HTTP_RESET_CONTENT);                 str = 'Reset Content'
            ! 4XX
            case (HTTP_BAD_REQUEST);                   str = 'Bad Request'
            case (HTTP_UNAUTHORIZED);                  str = 'Unauthorized'
            case (HTTP_PAYMENT_REQUIRED);              str = 'Payment Required'
            case (HTTP_FORBIDDEN);                     str = 'Forbidden'
            case (HTTP_NOT_FOUND);                     str = 'Not Found'
            case (HTTP_METHOD_NOT_ALLOWED);            str = 'Method Not Allowed'
            case (HTTP_NOT_ACCEPTABLE);                str = 'Not Acceptable'
            case (HTTP_PROXY_AUTHENTICATION_REQUIRED); str = 'Proxy Authentication Required'
            case (HTTP_REQUEST_TIMEOUT);               str = 'Request Timeout'
            case (HTTP_CONFLICT);                      str = 'Conflict'
            case (HTTP_GONE);                          str = 'Gone'
            case (HTTP_LENGTH_REQUIRED);               str = 'Length Required'
            case (HTTP_UNSUPPORTED_MEDIA_TYPE);        str = 'Unsupported Media Type'
            ! 50X
            case (HTTP_INTERNAL_SERVER_ERROR);         str = 'Internal Server Error'
            case (HTTP_NOT_IMPLEMENTED);               str = 'Not Implemented'
            case (HTTP_BAD_GATEWAY);                   str = 'Bad Gateway'
            case (HTTP_SERVICE_UNAVAILABLE);           str = 'Service Unavailable'
            ! NIY
            case default;                              str = ''
        end select
    end function dm_http_status_string
end module dm_http
