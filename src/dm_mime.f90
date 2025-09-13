! Author:  Philipp Engel
! Licence: ISC
module dm_mime
    !! MIME types.
    implicit none (type, external)
    private

    integer, parameter, public :: MIME_LEN = 48 !! Max. MIME type length.

    character(*), parameter, public :: MIME_ATOM      = 'application/atom+xml'
    character(*), parameter, public :: MIME_CSS       = 'text/css'
    character(*), parameter, public :: MIME_CSV       = 'text/comma-separated-values'
    character(*), parameter, public :: MIME_FORM      = 'application/x-www-form-urlencoded'
    character(*), parameter, public :: MIME_FORM_DATA = 'multipart/form-data'
    character(*), parameter, public :: MIME_GEOJSON   = 'application/geo+json'
    character(*), parameter, public :: MIME_GIF       = 'image/gif'
    character(*), parameter, public :: MIME_HTML      = 'text/html'
    character(*), parameter, public :: MIME_JPEG      = 'image/jpeg'
    character(*), parameter, public :: MIME_JS        = 'application/javascript'
    character(*), parameter, public :: MIME_JSON      = 'application/json'
    character(*), parameter, public :: MIME_JSONL     = 'application/jsonl'
    character(*), parameter, public :: MIME_NML       = 'application/namelist'
    character(*), parameter, public :: MIME_PNG       = 'image/png'
    character(*), parameter, public :: MIME_STREAM    = 'application/octet-stream'
    character(*), parameter, public :: MIME_SVG       = 'image/svg+xml'
    character(*), parameter, public :: MIME_TEXT      = 'text/plain'
    character(*), parameter, public :: MIME_XHTML     = 'application/xhtml+xml'
    character(*), parameter, public :: MIME_XML       = 'text/xml'
    character(*), parameter, public :: MIME_ZSTD      = 'application/zstd'
end module dm_mime
