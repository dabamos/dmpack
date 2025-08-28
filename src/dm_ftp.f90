! Author:  Philipp Engel
! Licence: ISC
module dm_ftp
    !! Module for file transfer via FTP(S).
    !!
    !! Upload a local file to an FTP server:
    !!
    !! ```fortran
    !! character(len=*), parameter :: HOST        = '192.168.0.100'
    !! character(len=*), parameter :: LOCAL_FILE  = '/tmp/observ.csv'
    !! character(len=*), parameter :: REMOTE_FILE = 'test/observ.csv'
    !!
    !! integer               :: rc
    !! type(ftp_server_type) :: server
    !!
    !! rc = dm_ftp_init()
    !! call dm_ftp_server_set(server, host=HOST)
    !! rc = dm_ftp_upload(server, LOCAL_FILE, REMOTE_FILE, create_missing=.true.)
    !! call dm_ftp_shutdown()
    !! ```
    !!
    !! If remote directory `test/` does not exist, it will be created. The
    !! remote path is relative to the default directory of the FTP server.
    !! Absolute paths have to start with `//`.
    !!
    !! Download remote file `test/observ.csv` instead:
    !!
    !! ```fortran
    !! call dm_ftp_server_set(server, host=HOST)
    !! rc = dm_ftp_download(server, REMOTE_FILE, LOCAL_FILE, replace=.true.)
    !! ```
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    use :: dm_version
    implicit none (type, external)
    private

    character(len=*), parameter, public :: FTP_USER_AGENT = 'DMPACK ' // DM_VERSION_STRING !! User agent of FTP client.

    integer, parameter, public :: FTP_HOST_LEN     = 256  !! Max. host length.
    integer, parameter, public :: FTP_USERNAME_LEN = 32   !! Max. user name length.
    integer, parameter, public :: FTP_PASSWORD_LEN = 32   !! Max. password length.
    integer, parameter, public :: FTP_URL_LEN      = 2048 !! Max. URL length.

    integer, parameter :: FTP_BUFFER_SIZE        = 1024 * 1024 * 8 !! Buffer size [byte].
    integer, parameter :: FTP_MAX_REDIRECTS      = 10              !! Max. number of redirects.
    integer, parameter :: FTP_TRANSFER_UNIT_NONE = -99999          !! Default file unit.

    type :: ftp_transfer_type
        !! Opaque FTP transfer type.
        private
        character(len=FTP_URL_LEN) :: url    = ' '                    !! URL of remote file.
        type(c_ptr)                :: curl   = c_null_ptr             !! libcurl context.
        type(c_ptr)                :: list   = c_null_ptr             !! Header context.
        type(c_ptr)                :: stream = c_null_ptr             !! `FILE *`.
        integer                    :: unit   = FTP_TRANSFER_UNIT_NONE !! File unit.
        integer(kind=i8)           :: size   = 0                      !! Upload file size [byte].
    end type ftp_transfer_type

    type, public :: ftp_server_type
        !! FTP server type.
        character(len=FTP_HOST_LEN)     :: host            = ' '     !! IP address or FQDN of FTP server.
        integer                         :: port            = 0       !! Control port (0 for default port 21).
        character(len=FTP_USERNAME_LEN) :: username        = ' '     !! User name (empty for none).
        character(len=FTP_PASSWORD_LEN) :: password        = ' '     !! Password (empty for none).
        integer                         :: accept_timeout  = 5       !! Accept timeout [sec].
        integer                         :: connect_timeout = 30      !! Connection timeout [sec].
        integer                         :: timeout         = 30      !! Response timeout [sec].
        logical                         :: active          = .false. !! Active mode.
        logical                         :: tls             = .false. !! Use Transport-Layer Security (FTPS).
        logical                         :: verify_tls      = .false. !! Verify SSL certificate.
    end type ftp_server_type

    ! Public procedures.
    public :: dm_ftp_error
    public :: dm_ftp_error_message
    public :: dm_ftp_init
    public :: dm_ftp_server_out
    public :: dm_ftp_server_set
    public :: dm_ftp_shutdown
    public :: dm_ftp_url

    ! Public FTP procedures.
    public :: dm_ftp_delete
    public :: dm_ftp_download
    public :: dm_ftp_list
    public :: dm_ftp_upload

    ! Public callbacks.
    public :: dm_ftp_discard_callback
    public :: dm_ftp_read_stream_callback
    public :: dm_ftp_write_stream_callback
    public :: dm_ftp_write_unit_callback

    ! Private procedures.
    private :: ftp_prepare
    private :: ftp_prepare_delete
    private :: ftp_prepare_download
    private :: ftp_prepare_list
    private :: ftp_prepare_upload
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ftp_error(error_curl) result(rc)
        !! Converts cURL easy stack error code to DMPACK error code.
        integer, intent(in) :: error_curl !! cURL easy error code.

        select case (error_curl)
            case (CURLE_OK)
                rc = E_NONE

            case (CURLE_FTP_WEIRD_PASS_REPLY,  &
                  CURLE_FTP_WEIRD_PASV_REPLY,  &
                  CURLE_FTP_WEIRD_227_FORMAT,  &
                  CURLE_FTP_CANT_GET_HOST,     &
                  CURLE_FTP_COULDNT_SET_TYPE,  &
                  CURLE_FTP_COULDNT_RETR_FILE, &
                  CURLE_FTP_PORT_FAILED,       &
                  CURLE_FTP_COULDNT_USE_REST,  &
                  CURLE_FTP_PRET_FAILED,       &
                  CURLE_FTP_BAD_FILE_LIST)
                rc = E_FTP

            case (CURLE_UNSUPPORTED_PROTOCOL,  &
                  CURLE_FAILED_INIT,           &
                  CURLE_URL_MALFORMAT,         &
                  CURLE_NOT_BUILT_IN,          &
                  CURLE_BAD_FUNCTION_ARGUMENT, &
                  CURLE_UNKNOWN_OPTION,        &
                  CURLE_BAD_CONTENT_ENCODING)
                rc = E_INVALID

            case (CURLE_FTP_ACCEPT_FAILED,     &
                  CURLE_COULDNT_RESOLVE_PROXY, &
                  CURLE_COULDNT_RESOLVE_HOST,  &
                  CURLE_COULDNT_CONNECT)
                rc = E_FTP_CONNECT

            case (CURLE_REMOTE_ACCESS_DENIED, &
                  CURLE_AUTH_ERROR)
                rc = E_FTP_AUTH

            case (CURLE_WRITE_ERROR)
                rc = E_WRITE

            case (CURLE_READ_ERROR)
                rc = E_READ

            case (CURLE_OUT_OF_MEMORY)
                rc = E_MEMORY

            case (CURLE_FTP_ACCEPT_TIMEOUT, &
                  CURLE_OPERATION_TIMEDOUT)
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
                rc = E_FTP_SSL

            case (CURLE_FILESIZE_EXCEEDED)
                rc = E_LIMIT

            case default
                rc = E_FTP
        end select
    end function dm_ftp_error

    function dm_ftp_error_message(error_curl) result(message)
        !! Return message associated with given cURL error code as allocatable
        !! character string.
        integer, intent(in)           :: error_curl !! cURL error code.
        character(len=:), allocatable :: message    !! Error message.

        message = curl_easy_strerror(error_curl)
    end function dm_ftp_error_message

    integer function dm_ftp_init() result(rc)
        !! Initialises FTP backend. The function returns `E_FTP` on error.
        rc = E_FTP
        if (curl_global_init(CURL_GLOBAL_DEFAULT) == CURLE_OK) rc = E_NONE
    end function dm_ftp_init

    function dm_ftp_url(host, port, path, tls) result(url)
        !! Returns allocatable string of FTP server URL in the form
        !! `ftp[s]://host[:port]/path`. An absolute path has to start with
        !! `//`. Uses the URL API of libcurl to create the URL. By default,
        !! Transport-Layer Security (FTPS) is disabled.
        use :: dm_string, only: dm_string_is_present

        character(len=*), intent(in)           :: host !! FTP host.
        integer,          intent(in), optional :: port !! FTP port (up to 5 digits).
        character(len=*), intent(in), optional :: path !! FTP file path.
        logical,          intent(in), optional :: tls  !! Enable Transport-Layer Security.
        character(len=:), allocatable          :: url  !! URL of FTP server.

        integer     :: port_
        integer     :: stat
        logical     :: tls_
        type(c_ptr) :: ptr

        port_ = dm_present(port, 0)
        tls_  = dm_present(tls, .false.)

        url_block: block
            logical :: has_path, has_port

            has_port = (port_ > 0)
            has_path = dm_string_is_present(path)

            ptr = curl_url()
            if (.not. c_associated(ptr)) exit url_block

            ! URL scheme.
            if (tls_) then
                stat = curl_url_set(ptr, CURLUPART_SCHEME, 'ftps'); if (stat /= CURLUE_OK) exit url_block
            else
                stat = curl_url_set(ptr, CURLUPART_SCHEME, 'ftp');  if (stat /= CURLUE_OK) exit url_block
            end if

                          stat = curl_url_set(ptr, CURLUPART_HOST, trim(host));     if (stat /= CURLUE_OK) exit url_block ! URL host.
            if (has_port) stat = curl_url_set(ptr, CURLUPART_PORT, dm_itoa(port_)); if (stat /= CURLUE_OK) exit url_block ! URL port.
            if (has_path) stat = curl_url_set(ptr, CURLUPART_PATH, trim(path));     if (stat /= CURLUE_OK) exit url_block ! URL path.

            ! Get full URL.
            stat = curl_url_get(ptr, CURLUPART_URL, url)
        end block url_block

        call curl_url_cleanup(ptr)
        if (.not. allocated(url)) url = ''
    end function dm_ftp_url

    ! **************************************************************************
    ! PUBLIC FTP FUNCTIONS.
    ! **************************************************************************
    integer function dm_ftp_delete(server, remote_file, debug, error_message, error_curl) result(rc)
        !! Deletes remote file on FTP server.
        !!
        !! Delete file `dummy.txt` on a local FTP server:
        !!
        !! ```fortran
        !! integer               :: rc
        !! type(ftp_server_type) :: server
        !!
        !! rc = dm_ftp_init()
        !! call dm_ftp_server_set(server, host='localhost', username='user', password='secret', tls=.false.)
        !! rc = dm_ftp_delete(server, 'dummy.txt')
        !! call dm_ftp_shutdown()
        !! ```
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_COMPILER` if C pointers could not be nullified (compiler bug).
        !! * `E_FTP` if initialisation or connection failed.
        !! * `E_FTP_AUTH` if FTP authentication failed.
        !! * `E_FTP_CONNECT` if connection to server could not be established.
        !! * `E_FTP_SSL` if SSL/TLS error occured.
        !! * `E_INVALID` if arguments or FTP server type attributes are invalid.
        !!
        type(ftp_server_type),         intent(inout)         :: server        !! FTP server type.
        character(len=*),              intent(in)            :: remote_file   !! Path of remote file to delete.
        logical,                       intent(in),  optional :: debug         !! Output debug messages.
        character(len=:), allocatable, intent(out), optional :: error_message !! Error message.
        integer,                       intent(out), optional :: error_curl    !! cURL error code.

        integer                         :: stat
        type(ftp_transfer_type), target :: transfer

        stat = CURLE_OK

        ftp_block: block
            rc = E_INVALID
            if (len_trim(server%host) == 0) exit ftp_block
            if (len_trim(remote_file) == 0) exit ftp_block

            transfer%url = dm_ftp_url(server%host, port=server%port, path=remote_file, tls=server%tls)
            if (len_trim(transfer%url) == 0) exit ftp_block

            rc = E_FTP
            transfer%curl = curl_easy_init()
            if (.not. c_associated(transfer%curl)) exit ftp_block

            rc = ftp_prepare_delete(server, transfer, remote_file, FTP_BUFFER_SIZE, FTP_MAX_REDIRECTS, debug)
            if (dm_is_error(rc)) exit ftp_block

            stat = curl_easy_perform(transfer%curl)
            rc   = dm_ftp_error(stat)
        end block ftp_block

        if (present(error_curl)) error_curl = stat
        if (present(error_message)) then
            if (dm_is_error(rc)) error_message = dm_ftp_error_message(stat)
            if (.not. allocated(error_message)) error_message = ''
        end if

        call curl_slist_free_all(transfer%list)
        call curl_easy_cleanup(transfer%curl)

        if (dm_is_error(rc)) return
        if (c_associated(transfer%list) .or. c_associated(transfer%curl)) rc = E_COMPILER
    end function dm_ftp_delete

    integer function dm_ftp_download(server, remote_file, local_file, replace, debug, error_message, error_curl) result(rc)
        !! Downloads remote file from FTP server. If `local_file` exists, the
        !! download will be aborted unless `replace` is passed and `.true.`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_COMPILER` if C pointers could not be nullified (compiler bug).
        !! * `E_EXIST` if local file exists.
        !! * `E_FTP` if initialisation or connection failed.
        !! * `E_FTP_AUTH` if FTP authentication failed.
        !! * `E_FTP_CONNECT` if connection to server could not be established.
        !! * `E_FTP_SSL` if SSL/TLS error occured.
        !! * `E_INVALID` if arguments or FTP server type attributes are invalid.
        !! * `E_IO` if local file could not be opened for writing.
        !!
        use :: unix,    only: c_fclose, c_fopen
        use :: dm_c,    only: dm_f_c_string
        use :: dm_file, only: dm_file_delete, dm_file_exists

        type(ftp_server_type),         intent(inout)         :: server        !! FTP server type.
        character(len=*),              intent(in)            :: remote_file   !! Path of remote file.
        character(len=*),              intent(in)            :: local_file    !! Path of file to upload.
        logical,                       intent(in),  optional :: replace       !! Replace existing file.
        logical,                       intent(in),  optional :: debug         !! Output debug messages.
        character(len=:), allocatable, intent(out), optional :: error_message !! Error message.
        integer,                       intent(out), optional :: error_curl    !! cURL error code.

        integer :: stat
        logical :: replace_

        type(ftp_transfer_type), target :: transfer

        replace_ = dm_present(replace, .false.)
        stat = CURLE_OK

        ftp_block: block
            rc = E_INVALID
            if (len_trim(server%host) == 0) exit ftp_block
            if (len_trim(remote_file) == 0) exit ftp_block
            if (len_trim(local_file)  == 0) exit ftp_block

            transfer%url = dm_ftp_url(server%host, port=server%port, path=remote_file, tls=server%tls)
            if (len_trim(transfer%url) == 0) exit ftp_block

            rc = E_EXIST
            if (dm_file_exists(local_file)) then
                if (.not. replace_) exit ftp_block
                call dm_file_delete(local_file)
            end if

            rc = E_IO
            transfer%stream = c_fopen(dm_f_c_string(local_file), dm_f_c_string('wb'))
            if (.not. c_associated(transfer%stream)) exit ftp_block

            rc = E_FTP
            transfer%curl = curl_easy_init()
            if (.not. c_associated(transfer%curl)) exit ftp_block

            rc = ftp_prepare_download(server, transfer, FTP_BUFFER_SIZE, FTP_MAX_REDIRECTS, debug)
            if (dm_is_error(rc)) exit ftp_block

            stat = curl_easy_perform(transfer%curl)
            rc   = dm_ftp_error(stat)
        end block ftp_block

        if (present(error_curl)) error_curl = stat
        if (present(error_message)) then
            if (dm_is_error(rc)) error_message = dm_ftp_error_message(stat)
            if (.not. allocated(error_message)) error_message = ''
        end if

        if (c_associated(transfer%curl)) then
            call curl_easy_cleanup(transfer%curl)
            if (c_associated(transfer%curl)) rc = E_COMPILER
        end if

        if (c_associated(transfer%stream)) then
            stat = c_fclose(transfer%stream)
            if (stat == 0) transfer%stream = c_null_ptr
            if (c_associated(transfer%stream)) rc = E_COMPILER
        end if
    end function dm_ftp_download

    integer function dm_ftp_list(server, unit, directory, names_only, debug, error_message, error_curl) result(rc)
        !! Writes list of FTP directory contents to passed Fortran file unit
        !! `unit`.
        !!
        !! The following example writes the contents of the FTP root directory
        !! to a scratch file and outputs the file afterwards:
        !!
        !! ```fortran
        !! character(len=512)    :: line
        !! integer               :: rc, stat, unit
        !! type(ftp_server_type) :: server
        !!
        !! open (action='readwrite', form='formatted', newunit=unit, status='scratch')
        !!
        !! rc = dm_ftp_init()
        !! call dm_ftp_server_set(server, host='localhost', username='user', password='secret', tls=.false.)
        !! rc = dm_ftp_list(server, unit, '/', names_only=.true.)
        !! call dm_ftp_shutdown()
        !!
        !! rewind (unit)
        !!
        !! do
        !!     read (unit, '(a)', iostat=stat) line
        !!     if (stat /= 0) exit
        !!     print '(a)', trim(line)
        !! end do
        !!
        !! close (unit)
        !! ```
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_COMPILER` if C pointers could not be nullified (compiler bug).
        !! * `E_FTP` if initialisation or connection failed.
        !! * `E_FTP_AUTH` if FTP authentication failed.
        !! * `E_FTP_CONNECT` if connection to server could not be established.
        !! * `E_FTP_SSL` if SSL/TLS error occured.
        !! * `E_INVALID` if arguments or FTP server type attributes are invalid.
        !! * `E_IO` if unit is not opened.
        !!
        type(ftp_server_type),         intent(inout)         :: server        !! FTP server type.
        integer,                       intent(in)            :: unit          !! File unit to write to.
        character(len=*),              intent(in)            :: directory     !! Path of remote FTP directory.
        logical,                       intent(in),  optional :: names_only    !! List only names (NLST command).
        logical,                       intent(in),  optional :: debug         !! Output debug messages.
        character(len=:), allocatable, intent(out), optional :: error_message !! Error message.
        integer,                       intent(out), optional :: error_curl    !! cURL error code.

        integer                         :: stat
        type(ftp_transfer_type), target :: transfer

        stat = CURLE_OK

        ftp_block: block
            logical :: file_exists

            rc = E_INVALID
            if (len_trim(server%host) == 0) exit ftp_block
            transfer%url = dm_ftp_url(server%host, port=server%port, path=directory, tls=server%tls)
            if (len_trim(transfer%url) == 0) exit ftp_block

            rc = E_IO
            if (unit == FTP_TRANSFER_UNIT_NONE) exit ftp_block
            inquire (exist=file_exists, unit=unit)
            if (.not. file_exists) exit ftp_block
            transfer%unit = unit

            rc = E_FTP
            transfer%curl = curl_easy_init()
            if (.not. c_associated(transfer%curl)) exit ftp_block

            rc = ftp_prepare_list(server, transfer, names_only, FTP_BUFFER_SIZE, FTP_MAX_REDIRECTS, debug)
            if (dm_is_error(rc)) exit ftp_block

            stat = curl_easy_perform(transfer%curl)
            rc   = dm_ftp_error(stat)
        end block ftp_block

        if (present(error_curl)) error_curl = stat
        if (present(error_message)) then
            if (dm_is_error(rc)) error_message = dm_ftp_error_message(stat)
            if (.not. allocated(error_message)) error_message = ''
        end if

        call curl_slist_free_all(transfer%list)
        call curl_easy_cleanup(transfer%curl)

        if (dm_is_error(rc)) return
        if (c_associated(transfer%list) .or. c_associated(transfer%curl) .or. c_associated(transfer%stream)) rc = E_COMPILER
    end function dm_ftp_list

    integer function dm_ftp_upload(server, local_file, remote_file, rename_file_to, create_missing, debug, &
                                   error_message, error_curl) result(rc)
        !! Uploads local file to FTP server.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_COMPILER` if C pointers could not be nullified (compiler bug).
        !! * `E_FTP` if initialisation or connection failed.
        !! * `E_FTP_AUTH` if FTP authentication failed.
        !! * `E_FTP_CONNECT` if connection to server could not be established.
        !! * `E_FTP_SSL` if SSL/TLS error occured.
        !! * `E_INVALID` if arguments or FTP server type attributes are invalid.
        !! * `E_IO` if local file could not be opened for reading.
        !! * `E_NOT_FOUND` if local file does not exist.
        !! * `E_PERM` if local file is not readable.
        !!
        use :: unix,    only: c_fclose, c_fopen
        use :: dm_c,    only: dm_f_c_string
        use :: dm_file, only: dm_file_exists, dm_file_is_readable, dm_file_size

        type(ftp_server_type),         intent(inout)         :: server         !! FTP server type.
        character(len=*),              intent(in)            :: local_file     !! Path of file to upload.
        character(len=*),              intent(in)            :: remote_file    !! Path of remote file.
        character(len=*),              intent(in),  optional :: rename_file_to !! File name to rename to remote file to.
        logical,                       intent(in),  optional :: create_missing !! Create missing directories.
        logical,                       intent(in),  optional :: debug          !! Output debug messages.
        character(len=:), allocatable, intent(out), optional :: error_message  !! Error message.
        integer,                       intent(out), optional :: error_curl     !! cURL error code.

        integer                         :: stat
        type(ftp_transfer_type), target :: transfer

        stat = CURLE_OK

        ftp_block: block
            rc = E_INVALID
            if (len_trim(server%host) == 0) exit ftp_block
            if (len_trim(local_file)  == 0) exit ftp_block
            if (len_trim(remote_file) == 0) exit ftp_block

            transfer%url = dm_ftp_url(server%host, port=server%port, path=remote_file, tls=server%tls)
            if (len_trim(transfer%url) == 0) exit ftp_block

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(local_file)) exit ftp_block

            rc = E_PERM
            if (.not. dm_file_is_readable(local_file)) exit ftp_block

            rc = E_IO
            transfer%stream = c_fopen(dm_f_c_string(local_file), dm_f_c_string('rb'))
            transfer%size   = dm_file_size(local_file)
            if (.not. c_associated(transfer%stream)) exit ftp_block

            rc = E_FTP
            transfer%curl = curl_easy_init()
            if (.not. c_associated(transfer%curl)) exit ftp_block

            rc = ftp_prepare_upload(server, transfer, remote_file, rename_file_to, create_missing, &
                                    FTP_BUFFER_SIZE, FTP_MAX_REDIRECTS, debug)
            if (dm_is_error(rc)) exit ftp_block

            stat = curl_easy_perform(transfer%curl)
            rc   = dm_ftp_error(stat)
        end block ftp_block

        if (present(error_curl)) error_curl = stat
        if (present(error_message)) then
            if (dm_is_error(rc)) error_message = dm_ftp_error_message(stat)
            if (.not. allocated(error_message)) error_message = ''
        end if

        call curl_slist_free_all(transfer%list)
        call curl_easy_cleanup(transfer%curl)

        if (c_associated(transfer%stream)) then
            stat = c_fclose(transfer%stream)
            if (stat == 0) transfer%stream = c_null_ptr
        end if

        if (dm_is_error(rc)) return
        if (c_associated(transfer%list) .or. c_associated(transfer%curl) .or. c_associated(transfer%stream)) rc = E_COMPILER
    end function dm_ftp_upload

    ! **************************************************************************
    ! PUBLIC CALLBACK FUNCTIONS.
    ! **************************************************************************
    function dm_ftp_discard_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable discard download function for libcurl. Do not call
        !! this function directly.
        type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of memory.
        integer(kind=c_size_t), intent(in), value :: sz    !! Always 1.
        integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the memory chunk.
        type(c_ptr),            intent(in), value :: data  !! C pointer to argument passed by caller.
        integer(kind=c_size_t)                    :: n     !! Function return value.

        n = sz * nmemb
    end function dm_ftp_discard_callback

    function dm_ftp_read_stream_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable upload callback function for libcurl. Do not call
        !! this function directly.
        use :: unix, only: c_fread

        type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of memory.
        integer(kind=c_size_t), intent(in), value :: sz    !! Always 1.
        integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the memory chunk.
        type(c_ptr),            intent(in), value :: data  !! C pointer to argument passed by caller.
        integer(kind=c_size_t)                    :: n     !! Function return value.

        type(ftp_transfer_type), pointer :: transfer

        n = int(0, kind=c_size_t)

        if (.not. c_associated(ptr))  return
        if (.not. c_associated(data)) return

        call c_f_pointer(data, transfer)
        if (.not. c_associated(transfer%stream)) return

        n = c_fread(ptr, sz, nmemb, transfer%stream)
    end function dm_ftp_read_stream_callback

    function dm_ftp_write_stream_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable download callback function for libcurl. Do not call
        !! this function directly.
        use :: unix, only: c_fwrite

        type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of memory.
        integer(kind=c_size_t), intent(in), value :: sz    !! Always 1.
        integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the memory chunk.
        type(c_ptr),            intent(in), value :: data  !! C pointer to argument passed by caller.
        integer(kind=c_size_t)                    :: n     !! Function return value.

        type(ftp_transfer_type), pointer :: transfer

        n = int(0, kind=c_size_t)

        if (.not. c_associated(ptr))  return
        if (.not. c_associated(data)) return

        call c_f_pointer(data, transfer)
        if (.not. c_associated(transfer%stream)) return

        n = c_fwrite(ptr, sz, nmemb, transfer%stream)
    end function dm_ftp_write_stream_callback

    function dm_ftp_write_unit_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable callback function to write received bytes to Fortran
        !! file unit passed in client data of type `ftp_transfer_type`. Do not
        !! call this function directly.
        type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of memory.
        integer(kind=c_size_t), intent(in), value :: sz    !! Always 1.
        integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the memory chunk.
        type(c_ptr),            intent(in), value :: data  !! C pointer to argument passed by caller.
        integer(kind=c_size_t)                    :: n     !! Function return value.

        character(len=8)                 :: formatted
        character(len=:), allocatable    :: chunk
        integer                          :: stat
        logical                          :: file_exists
        type(ftp_transfer_type), pointer :: transfer

        n = int(0, kind=c_size_t)

        if (.not. c_associated(ptr))  return
        if (.not. c_associated(data)) return

        call c_f_pointer(data, transfer)
        call c_f_str_ptr(ptr, chunk, nmemb)

        inquire (exist=file_exists, formatted=formatted, unit=transfer%unit)
        if (.not. file_exists) return

        if (formatted == 'YES') then
            write (transfer%unit, '(a)', advance='no', iostat=stat) chunk
        else
            write (transfer%unit, iostat=stat) chunk
        end if

        n = nmemb
    end function dm_ftp_write_unit_callback

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_ftp_shutdown()
        !! Cleans up FTP backend.
        call curl_global_cleanup()
    end subroutine dm_ftp_shutdown

    subroutine dm_ftp_server_out(server, unit)
        !! Prints FTP connection type to standard output or given file unit.
        type(ftp_server_type), intent(inout)        :: server !! FTP server type.
        integer,               intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, stdout)

        write (unit_, '("ftp.host: ", a)')             trim(server%host)
        write (unit_, '("ftp.port: ", i0)')            server%port
        write (unit_, '("ftp.username: ", a)')         trim(server%username)
        write (unit_, '("ftp.password: ", a)')         trim(server%password)
        write (unit_, '("ftp.accept_timeout: ", i0)')  server%accept_timeout
        write (unit_, '("ftp.connect_timeout: ", i0)') server%connect_timeout
        write (unit_, '("ftp.timeout: ", i0)')         server%timeout
        write (unit_, '("ftp.active: ", l1)')          server%active
        write (unit_, '("ftp.tls: ", l1)')             server%tls
        write (unit_, '("ftp.verify_tls: ", l1)')      server%verify_tls
    end subroutine dm_ftp_server_out

    subroutine dm_ftp_server_set(server, host, port, username, password, accept_timeout, connect_timeout, &
                                 timeout, active, tls, verify_tls)
        !! Sets attributes of given server type.
        type(ftp_server_type), intent(inout)        :: server          !! FTP server type.
        character(len=*),      intent(in), optional :: host            !! Host.
        integer,               intent(in), optional :: port            !! Port (or 0 for default).
        character(len=*),      intent(in), optional :: username        !! User name.
        character(len=*),      intent(in), optional :: password        !! Password.
        integer,               intent(in), optional :: accept_timeout  !! Accept timeout [sec].
        integer,               intent(in), optional :: connect_timeout !! Connection timeout [sec].
        integer,               intent(in), optional :: timeout         !! Response timeout [sec].
        logical,               intent(in), optional :: active          !! Enable active mode.
        logical,               intent(in), optional :: tls             !! Enable Transport-Layer Security.
        logical,               intent(in), optional :: verify_tls      !! Verify SSL cert.

        if (present(host))            server%host            = host
        if (present(port))            server%port            = port
        if (present(username))        server%username        = username
        if (present(password))        server%password        = password
        if (present(accept_timeout))  server%accept_timeout  = accept_timeout
        if (present(connect_timeout)) server%connect_timeout = connect_timeout
        if (present(timeout))         server%timeout         = timeout
        if (present(active))          server%active          = active
        if (present(tls))             server%tls             = tls
        if (present(verify_tls))      server%verify_tls      = verify_tls
    end subroutine dm_ftp_server_set

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function ftp_prepare(server, transfer, buffer_size, max_redirects, debug) result(rc)
        !! Prepares libcurl. Sets URL, timeouts, buffer size, max. redirects,
        !! credentials, TLS verification, FTP mode, and debug flags, depending
        !! on server and transfer attributes.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FTP` if libcurl options could not be set.
        !! * `E_NULL` if the libcurl context of the transfer is not associated.
        !!
        type(ftp_server_type),           intent(inout)        :: server        !! FTP server type.
        type(ftp_transfer_type), target, intent(inout)        :: transfer      !! FTP transfer type.
        integer,                         intent(in), optional :: buffer_size   !! Buffer size [byte].
        integer,                         intent(in), optional :: max_redirects !! Max. number of redirects.
        logical,                         intent(in), optional :: debug         !! Debug mode.

        integer :: stat
        logical :: debug_

        debug_ = dm_present(debug, .false.)

        rc = E_NULL
        if (.not. c_associated(transfer%curl)) return

        curl_block: block
            character(len=:), allocatable :: user_password

            ! URL of remote file.
            if (len_trim(transfer%url) > 0) then
                stat = curl_easy_setopt(transfer%curl, CURLOPT_URL, trim(transfer%url)); if (stat /= CURLE_OK) exit curl_block
            end if

            ! Request settings.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_USERAGENT,                  FTP_USER_AGENT);                         if (stat /= CURLE_OK) exit curl_block ! User agent.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_ACCEPTTIMEOUT_MS,           dm_sec_to_msec(server%accept_timeout));  if (stat /= CURLE_OK) exit curl_block ! Accept timeout.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_CONNECTTIMEOUT_MS,          dm_sec_to_msec(server%connect_timeout)); if (stat /= CURLE_OK) exit curl_block ! Connection timeout.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_SERVER_RESPONSE_TIMEOUT_MS, dm_sec_to_msec(server%timeout));         if (stat /= CURLE_OK) exit curl_block ! Response timeout.

            ! Buffer size.
            if (present(buffer_size)) then
                stat = curl_easy_setopt(transfer%curl, CURLOPT_BUFFERSIZE, buffer_size); if (stat /= CURLE_OK) exit curl_block
            end if

            ! Max. number of redirects.
            if (present(max_redirects)) then
                stat = curl_easy_setopt(transfer%curl, CURLOPT_MAXREDIRS, max_redirects); if (stat /= CURLE_OK) exit curl_block
            end if

            ! User name and password.
            if (len_trim(server%username) > 0) then
                if (len_trim(server%password) > 0) then
                    user_password = trim(server%username) // ':' // trim(server%password)
                else
                    user_password = trim(server%username)
                end if

                stat = curl_easy_setopt(transfer%curl, CURLOPT_USERPWD, user_password); if (stat /= CURLE_OK) exit curl_block
            end if

            ! Active mode.
            if (server%active) then
                stat = curl_easy_setopt(transfer%curl, CURLOPT_FTPPORT, '-'); if (stat /= CURLE_OK) exit curl_block
            end if

            ! Verify TLS.
            if (server%tls .and. .not. server%verify_tls) then
                stat = curl_easy_setopt(transfer%curl, CURLOPT_SSL_VERIFYPEER, 0); if (stat /= CURLE_OK) exit curl_block ! Skip peer verification.
                stat = curl_easy_setopt(transfer%curl, CURLOPT_SSL_VERIFYHOST, 0); if (stat /= CURLE_OK) exit curl_block ! Skip host verification.
            end if

            ! Debug mode.
            if (debug_) then
                stat = curl_easy_setopt(transfer%curl, CURLOPT_VERBOSE,  1); if (stat /= CURLE_OK) exit curl_block ! Enable debug messages.
            else
                stat = curl_easy_setopt(transfer%curl, CURLOPT_NOSIGNAL, 1); if (stat /= CURLE_OK) exit curl_block ! Disable debug messages.
            end if
        end block curl_block

        rc = dm_ftp_error(stat)
    end function ftp_prepare

    integer function ftp_prepare_delete(server, transfer, remote_file, buffer_size, max_redirects, debug) result(rc)
        !! Prepares libcurl to delete file `remote_file` on FTP server.
        !!
        !! You have to call `curl_slist_free_all(transfer%list)` afterwards.
        !! Additionally, set `transfer%list` to `c_null_ptr` if you want to
        !! reuse the transfer type.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FTP` if libcurl options could not be set.
        !! * `E_NULL` if the libcurl context of the transfer is not associated.
        !!
        type(ftp_server_type),           intent(inout)        :: server        !! FTP server type.
        type(ftp_transfer_type), target, intent(inout)        :: transfer      !! FTP transfer type.
        character(len=*),                intent(in)           :: remote_file   !! Path of file to delete.
        integer,                         intent(in), optional :: buffer_size   !! Buffer size [byte].
        integer,                         intent(in), optional :: max_redirects !! Max. number of redirects.
        logical,                         intent(in), optional :: debug         !! Debug mode.

        integer :: stat

        rc = ftp_prepare(server, transfer, buffer_size, max_redirects, debug)
        if (dm_is_error(rc)) return

        stat = CURLE_OK

        curl_block: block
            ! FTP commands.
            transfer%list = curl_slist_append(transfer%list, 'DELE ' // trim(remote_file))

            ! Request settings.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_POSTQUOTE,     transfer%list);                     if (stat /= CURLE_OK) exit curl_block ! FTP commands.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_WRITEFUNCTION, c_funloc(dm_ftp_discard_callback)); if (stat /= CURLE_OK) exit curl_block ! Write function.
        end block curl_block

        rc = dm_ftp_error(stat)
    end function ftp_prepare_delete

    integer function ftp_prepare_download(server, transfer, buffer_size, max_redirects, debug) result(rc)
        !! Prepares libcurl for FTP download. The transfer must have an URL.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FTP` if libcurl options could not be set.
        !! * `E_NULL` if the libcurl context of the transfer is not associated.
        !!
        use :: dm_string, only: dm_string_is_present

        type(ftp_server_type),           intent(inout)        :: server        !! FTP server type.
        type(ftp_transfer_type), target, intent(inout)        :: transfer      !! FTP transfer type.
        integer,                         intent(in), optional :: buffer_size   !! Buffer size [byte].
        integer,                         intent(in), optional :: max_redirects !! Max. number of redirects.
        logical,                         intent(in), optional :: debug         !! Debug mode.

        integer :: stat

        rc = ftp_prepare(server, transfer, buffer_size, max_redirects, debug)
        if (dm_is_error(rc)) return

        curl_block: block
            ! Request settings.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_WRITEFUNCTION, c_funloc(dm_ftp_write_stream_callback)); if (stat /= CURLE_OK) exit curl_block ! Read function.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_WRITEDATA,     c_loc(transfer));                        if (stat /= CURLE_OK) exit curl_block ! Read function client data.
        end block curl_block

        rc = dm_ftp_error(stat)
    end function ftp_prepare_download

    integer function ftp_prepare_list(server, transfer, names_only, buffer_size, max_redirects, debug) result(rc)
        !! Prepares libcurl for FTP directory listing. The transfer must have
        !! an URL and a file unit other than `FTP_TRANSFER_UNIT_NONE`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FTP` if libcurl options could not be set.
        !! * `E_INVALID` if the unit in `transfer` is invalid.
        !! * `E_NULL` if the libcurl context of the transfer is not associated.
        !!
        type(ftp_server_type),           intent(inout)        :: server        !! FTP server type.
        type(ftp_transfer_type), target, intent(inout)        :: transfer      !! FTP transfer type.
        logical,                         intent(in), optional :: names_only    !! Read file names only (NLST command).
        integer,                         intent(in), optional :: buffer_size   !! Buffer size [byte].
        integer,                         intent(in), optional :: max_redirects !! Max. number of redirects.
        logical,                         intent(in), optional :: debug         !! Debug mode.

        integer :: stat
        logical :: names_only_

        names_only_ = dm_present(names_only, .false.)

        rc = E_INVALID
        if (transfer%unit == FTP_TRANSFER_UNIT_NONE) return

        rc = ftp_prepare(server, transfer, buffer_size, max_redirects, debug)
        if (dm_is_error(rc)) return

        curl_block: block
            ! Request settings.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_WRITEFUNCTION, c_funloc(dm_ftp_write_unit_callback)); if (stat /= CURLE_OK) exit curl_block ! Read function.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_WRITEDATA,     c_loc(transfer));                      if (stat /= CURLE_OK) exit curl_block ! Read function client data.

            ! NLST command.
            if (names_only_) then
                stat = curl_easy_setopt(transfer%curl, CURLOPT_DIRLISTONLY, 1)
                if (stat /= CURLE_OK) exit curl_block
            end if
        end block curl_block

        rc = dm_ftp_error(stat)
    end function ftp_prepare_list

    integer function ftp_prepare_upload(server, transfer, remote_file, rename_file_to, create_missing, &
                                        buffer_size, max_redirects, debug) result(rc)
        !! Prepares libcurl for FTP upload. The transfer must have an URL.
        !!
        !! You have to call `curl_slist_free_all(transfer%list)` afterwards.
        !! Additionally, set `transfer%list` to `c_null_ptr` if you want to
        !! reuse the transfer type.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FTP` if libcurl options could not be set.
        !! * `E_NULL` if the libcurl context of the transfer is not associated.
        !!
        use :: dm_string, only: dm_string_is_present

        type(ftp_server_type),           intent(inout)        :: server         !! FTP server type.
        type(ftp_transfer_type), target, intent(inout)        :: transfer       !! FTP transfer type.
        character(len=*),                intent(in)           :: remote_file    !! Path of remote file.
        character(len=*),                intent(in), optional :: rename_file_to !! File name to rename the remote file to.
        logical,                         intent(in), optional :: create_missing !! Create missing directories.
        integer,                         intent(in), optional :: buffer_size    !! Buffer size [byte].
        integer,                         intent(in), optional :: max_redirects  !! Max. number of redirects.
        logical,                         intent(in), optional :: debug          !! Debug mode.

        integer :: stat

        rc = ftp_prepare(server, transfer, buffer_size, max_redirects, debug)
        if (dm_is_error(rc)) return

        stat = CURLE_OK

        curl_block: block
            ! FTP commands.
            if (dm_string_is_present(rename_file_to)) then
                transfer%list = curl_slist_append(transfer%list, 'RNFR ' // trim(remote_file))
                transfer%list = curl_slist_append(transfer%list, 'RNTO ' // trim(rename_file_to))
            end if

            ! Request settings.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_INFILESIZE,   transfer%size);                         if (stat /= CURLE_OK) exit curl_block ! File size.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_POSTQUOTE,    transfer%list);                         if (stat /= CURLE_OK) exit curl_block ! FTP commands.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_UPLOAD,       1);                                     if (stat /= CURLE_OK) exit curl_block ! Upload file.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_READFUNCTION, c_funloc(dm_ftp_read_stream_callback)); if (stat /= CURLE_OK) exit curl_block ! Read function.
            stat = curl_easy_setopt(transfer%curl, CURLOPT_READDATA,     c_loc(transfer));                       if (stat /= CURLE_OK) exit curl_block ! Read function client data.

            ! Create missing directories.
            if (dm_present(create_missing, .false.)) then
                stat = curl_easy_setopt(transfer%curl, CURLOPT_FTP_CREATE_MISSING_DIRS, 1)
                if (stat /= CURLE_OK) exit curl_block
            end if
        end block curl_block

        rc = dm_ftp_error(stat)
    end function ftp_prepare_upload
end module dm_ftp
