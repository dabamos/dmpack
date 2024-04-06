! Author:  Philipp Engel
! Licence: ISC
module dm_mime
    !! MIME types.
    implicit none (type, external)
    private

    character(len=*), parameter, public :: MIME_ATOM      = 'application/atom+xml'
    character(len=*), parameter, public :: MIME_CSS       = 'text/css'
    character(len=*), parameter, public :: MIME_CSV       = 'text/comma-separated-values'
    character(len=*), parameter, public :: MIME_FORM      = 'application/x-www-form-urlencoded'
    character(len=*), parameter, public :: MIME_FORM_DATA = 'multipart/form-data'
    character(len=*), parameter, public :: MIME_GIF       = 'image/gif'
    character(len=*), parameter, public :: MIME_HTML      = 'text/html'
    character(len=*), parameter, public :: MIME_JS        = 'application/javascript'
    character(len=*), parameter, public :: MIME_JSON      = 'application/json'
    character(len=*), parameter, public :: MIME_JSONL     = 'application/jsonl'
    character(len=*), parameter, public :: MIME_NML       = 'application/namelist'
    character(len=*), parameter, public :: MIME_PNG       = 'image/png'
    character(len=*), parameter, public :: MIME_STREAM    = 'application/octet-stream'
    character(len=*), parameter, public :: MIME_SVG       = 'image/svg+xml'
    character(len=*), parameter, public :: MIME_TEXT      = 'text/plain'
    character(len=*), parameter, public :: MIME_XHTML     = 'application/xhtml+xml'
    character(len=*), parameter, public :: MIME_XML       = 'text/xml'
    character(len=*), parameter, public :: MIME_ZIP       = 'application/zip'
    character(len=*), parameter, public :: MIME_ZSTD      = 'application/zstd'
end module dm_mime
