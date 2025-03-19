! Author:  Philipp Engel
! Licence: ISC
module dm_dwd_api
    !! HTTP abstraction layer for Deutsche Wetterdienst (DWD) API. This module
    !! must be linked against libcurl (`-lcurl`).
    !!
    !! To fetch weather data, create the URL to the DWD weather report and make
    !! an HTTP GET request. The data will be cached in a scratch file. The
    !! file may be opened as `formatted` or `unformatted` (byte stream):
    !!
    !! ```fortran
    !! character(len=:), allocatable :: url
    !! integer                       :: rc
    !! type(rpc_request_type)        :: request
    !! type(rpc_response_type)       :: response
    !!
    !! type(dwd_weather_report_type), allocatable :: reports(:)
    !!
    !! rc = dm_rpc_init()
    !!
    !! open (action='readwrite', form='formatted', newunit=response%unit, status='scratch')
    !!
    !! url = dm_dwd_api_weather_report_url(station_id='10281', tls=.false.)
    !! rc  = dm_rpc_get(request, response, url, callback=dm_dwd_api_callback)
    !!
    !! rewind (response%unit)
    !!
    !! rc = dm_dwd_weather_report_read(reports, response%unit)
    !!
    !! close (response%unit)
    !!
    !! call dm_rpc_shutdown()
    !! ```
    !!
    !! See the DWD Open Data Server for the terms and conditions of the service:
    !!
    !! * https://opendata.dwd.de/README.txt
    !!
    !! Legal notice:
    !!
    !! * https://www.dwd.de/EN/service/legal_notice/legal_notice_node.html
    !!
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_dwd
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    public :: dm_dwd_api_callback
    public :: dm_dwd_api_weather_report_url
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    function dm_dwd_api_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable write callback function for libcurl. Writes response
        !! chunks to file unit in passed client data of type `response_type`.
        use :: dm_rpc, only: RPC_RESPONSE_UNIT_DEFAULT, rpc_response_type

        type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of the response.
        integer(kind=c_size_t), intent(in), value :: sz    !! Always 1.
        integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the response chunk.
        type(c_ptr),            intent(in), value :: data  !! C pointer to argument passed by caller.
        integer(kind=c_size_t)                    :: n     !! Number of bytes consumed.

        character(len=8)                 :: formatted
        character(len=:), allocatable    :: chunk
        integer                          :: stat
        logical                          :: file_exists
        type(rpc_response_type), pointer :: response

        n = 0_c_size_t

        if (.not. c_associated(ptr))  return
        if (.not. c_associated(data)) return

        call c_f_pointer(data, response)
        if (response%unit == RPC_RESPONSE_UNIT_DEFAULT) return
        call c_f_str_ptr(ptr, chunk, nmemb)

        inquire (exist=file_exists, formatted=formatted, unit=response%unit)
        if (.not. file_exists) return

        if (formatted == 'YES') then
            write (response%unit, '(a)', advance='no', iostat=stat) chunk
        else
            write (response%unit, iostat=stat) chunk
        end if

        n = nmemb
    end function dm_dwd_api_callback

    function dm_dwd_api_weather_report_url(station_id, tls) result(url)
        !! Returns allocatable string of URL to DWD weather report endpoint of
        !! given station id `station_id`. Uses the URL API of libcurl to create
        !! the URL. The function returns an empty string on error. TLS is
        !! disabled by default.
        use :: dm_util, only: dm_present

        character(len=*), parameter :: DWD_HOST              = 'opendata.dwd.de'
        character(len=*), parameter :: WEATHER_REPORT_PATH   = '/weather/weather_reports/poi/'
        character(len=*), parameter :: WEATHER_REPORT_SUFFIX = '-BEOB.csv'

        character(len=*), intent(in)           :: station_id !! MOSMIX station id.
        logical,          intent(in), optional :: tls        !! Use HTTPS.
        character(len=:), allocatable          :: url        !! DWD weather report URL.

        character(len=DWD_MOSMIX_STATION_ID_LEN) :: id
        character(len=:), allocatable            :: path

        integer     :: n, stat
        logical     :: tls_
        type(c_ptr) :: ptr

        tls_ = dm_present(tls, .false.)

        url_block: block
            ptr = curl_url()
            if (.not. c_associated(ptr)) exit url_block

            ! URL scheme.
            if (tls_) then
                stat = curl_url_set(ptr, CURLUPART_SCHEME, 'https')
            else
                stat = curl_url_set(ptr, CURLUPART_SCHEME, 'http')
            end if
            if (stat /= CURLUE_OK) exit url_block

            ! URL host.
            stat = curl_url_set(ptr, CURLUPART_HOST, DWD_HOST)
            if (stat /= CURLUE_OK) exit url_block

            ! URL path. If the station id is shorter than 5 characters, pad it
            ! with underscores.
            n = len_trim(station_id)
            if (n > DWD_MOSMIX_STATION_ID_LEN) exit url_block

            id      = repeat('_', len(id))
            id(1:n) = station_id(1:n)
            path    = WEATHER_REPORT_PATH // id // WEATHER_REPORT_SUFFIX

            stat = curl_url_set(ptr, CURLUPART_PATH, path)
            if (stat /= CURLUE_OK) exit url_block

            ! Get full URL.
            stat = curl_url_get(ptr, CURLUPART_URL, url)
        end block url_block

        call curl_url_cleanup(ptr)
        if (.not. allocated(url)) url = ''
    end function dm_dwd_api_weather_report_url
end module dm_dwd_api
