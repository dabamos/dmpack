! Author:  Philipp Engel
! Licence: ISC
module dm_ftp
    !! Module for file upload/download via FTP(S).
    !!
    !! Upload a file to FTP server `192.168.0.100`:
    !!
    !! ```fortran
    !! character(len=*), parameter :: HOST        = '192.168.0.100'
    !! character(len=*), parameter :: LOCAL_FILE  = '/tmp/observ.csv'
    !! character(len=*), parameter :: REMOTE_FILE = '/var/ftp/observ.tmp'
    !!
    !! integer        :: rc
    !! type(ftp_type) :: ftp
    !!
    !! rc = dm_ftp_init()
    !! call dm_ftp_set(ftp, host=HOST)
    !! rc = dm_ftp_upload(ftp, LOCAL_FILE, REMOTE_FILE)
    !! call dm_ftp_shutdown()
    !! ```
    !!
    !! The procedure `dm_ftp_init()` has to be called once per process, and
    !! only if neither the RPC nor the MQTT backend is initialised already.
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    use :: dm_version
    implicit none (type, external)
    private

    character(len=*), parameter, public :: FTP_USER_AGENT   = 'DMPACK ' // DM_VERSION_STRING !! Default user agent of RPC client.
    integer,          parameter, public :: FTP_HOST_LEN     = 256                            !! Max. host length.
    integer,          parameter, public :: FTP_USERNAME_LEN = 32                             !! Max. user name length.
    integer,          parameter, public :: FTP_PASSWORD_LEN = 32                             !! Max. password length.
    integer,          parameter, public :: FTP_URL_LEN      = 512                            !! Max. URL length.

    type :: ftp_upload_type
        !! FTP upload type.
        integer(kind=i8) :: size   = 0          !! Total file size.
        type(c_ptr)      :: stream = c_null_ptr !! FILE *.
    end type ftp_upload_type

    type, public :: ftp_type
        !! FTP connection type.
        character(len=FTP_HOST_LEN)     :: host            = ' '     !! IP address or FQDN of FTP server.
        integer                         :: port            = 0       !! Control port (0 for default port 21).
        integer                         :: accept_timeout  = 5       !! Accept timeout [sec].
        integer                         :: connect_timeout = 30      !! Connection timeout [sec].
        integer                         :: timeout         = 30      !! Response timeout [sec].
        logical                         :: active          = .false. !! Active mode.
        logical                         :: create_missing  = .false. !! Create missing directories.
        logical                         :: tls             = .false. !! Use Transport-Layer Security (FTPS).
        logical                         :: verify_tls      = .false. !! Verify SSL certificate.
        character(len=FTP_USERNAME_LEN) :: username        = ' '     !! User name (empty for none).
        character(len=FTP_PASSWORD_LEN) :: password        = ' '     !! Password (empty for none).
        character(len=FTP_URL_LEN)      :: url             = ' '     !! Generated URL.
    end type ftp_type

    public :: dm_ftp_error
    public :: dm_ftp_error_message
    public :: dm_ftp_init
    public :: dm_ftp_out
    public :: dm_ftp_set
    public :: dm_ftp_shutdown
    public :: dm_ftp_upload
    public :: dm_ftp_url

    public :: dm_ftp_discard_callback
    public :: dm_ftp_read_callback
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

            case (CURLE_FTP_ACCEPT_FAILED,     &
                  CURLE_FTP_WEIRD_PASS_REPLY,  &
                  CURLE_FTP_ACCEPT_TIMEOUT,    &
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

            case (CURLE_COULDNT_RESOLVE_PROXY, &
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
        if (curl_global_init(CURL_GLOBAL_DEFAULT) /= CURLE_OK) return
        rc = E_NONE
    end function dm_ftp_init

    integer function dm_ftp_upload(ftp, local_file, remote_file, rename_file_to, &
                                   error_message, error_curl, debug) result(rc)
        !! Uploads local file to FTP server.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if FTP type is not initialised properly.
        !! * `E_INVALID` if FTP type attributes are invalid.
        !! * `E_FTP` if libcurl initialisation failed.
        !! * `E_FTP_AUTH` if FTP authentication failed.
        !! * `E_FTP_CONNECT` if connection to server could not be established.
        !! * `E_FTP_SSL` if SSL/TLS error occured.
        !!
        use :: unix,      only: c_fclose, c_fopen
        use :: dm_c,      only: dm_f_c_string
        use :: dm_file,   only: dm_file_exists, dm_file_is_readable, dm_file_size
        use :: dm_string, only: dm_string_is_present

        integer, parameter :: BUFFER_SIZE   = 1024 * 1024 * 8 !! Buffer size in bytes.
        integer, parameter :: MAX_REDIRECTS = 10              !! Max. number of redirects.

        type(ftp_type),                intent(inout)         :: ftp            !! FTP type.
        character(len=*),              intent(in)            :: local_file     !! Path of file to upload.
        character(len=*),              intent(in)            :: remote_file    !! Path of remote file.
        character(len=*),              intent(in),  optional :: rename_file_to !! File name to rename to remote file to.
        character(len=:), allocatable, intent(out), optional :: error_message  !! Error message.
        integer,                       intent(out), optional :: error_curl     !! cURL error code.
        logical,                       intent(in),  optional :: debug          !! Output debug messages.

        integer :: stat
        logical :: debug_

        stat   = CURLE_OK
        debug_ = dm_present(debug, .false.)

        ftp_block: block
            type(c_ptr)                   :: curl_ctx, list_ctx
            type(ftp_upload_type), target :: upload

            rc = E_INVALID
            if (len_trim(ftp%host) == 0)    exit ftp_block
            if (len_trim(local_file) == 0)  exit ftp_block
            if (len_trim(remote_file) == 0) exit ftp_block

            ftp%url = dm_ftp_url(ftp%host, port=ftp%port, path=remote_file, tls=ftp%tls)
            if (len_trim(ftp%url) == 0) exit ftp_block

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(local_file)) exit ftp_block

            rc = E_PERM
            if (.not. dm_file_is_readable(local_file)) exit ftp_block

            rc = E_IO
            upload%size   = dm_file_size(local_file)
            upload%stream = c_fopen(dm_f_c_string(local_file), dm_f_c_string('rb'))
            if (.not. c_associated(upload%stream)) exit ftp_block

            rc = E_FTP
            curl_block: block
                character(len=:), allocatable :: user_password

                list_ctx = c_null_ptr
                curl_ctx = curl_easy_init()

                if (.not. c_associated(curl_ctx)) exit curl_block

                ! FTP commands.
                list_ctx = curl_slist_append(list_ctx, 'RNFR ' // trim(remote_file))
                if (dm_string_is_present(rename_file_to)) list_ctx = curl_slist_append(list_ctx, 'RNTO ' // trim(rename_file_to))

                ! Request settings.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_BUFFERSIZE,   BUFFER_SIZE);                    if (stat /= CURLE_OK) exit curl_block ! Buffer size.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_INFILESIZE,   upload%size);                    if (stat /= CURLE_OK) exit curl_block ! File size.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_MAXREDIRS,    MAX_REDIRECTS);                  if (stat /= CURLE_OK) exit curl_block ! Number of redirects.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_POSTQUOTE,    list_ctx);                       if (stat /= CURLE_OK) exit curl_block ! FTP commands.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_UPLOAD,       1);                              if (stat /= CURLE_OK) exit curl_block ! Upload file.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_URL,          trim(ftp%url));                  if (stat /= CURLE_OK) exit curl_block ! FTP URL.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_USERAGENT,    FTP_USER_AGENT);                 if (stat /= CURLE_OK) exit curl_block ! User agent.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_READFUNCTION, c_funloc(dm_ftp_read_callback)); if (stat /= CURLE_OK) exit curl_block ! Read function.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_READDATA,     c_loc(upload));                  if (stat /= CURLE_OK) exit curl_block ! Read function client data.

                ! Timeouts.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_ACCEPTTIMEOUT_MS,           dm_sec_to_msec(ftp%accept_timeout));  if (stat /= CURLE_OK) exit curl_block ! Accept timeout.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_CONNECTTIMEOUT_MS,          dm_sec_to_msec(ftp%connect_timeout)); if (stat /= CURLE_OK) exit curl_block ! Connection timeout.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_SERVER_RESPONSE_TIMEOUT_MS, dm_sec_to_msec(ftp%timeout));         if (stat /= CURLE_OK) exit curl_block ! Response timeout.

                if (len_trim(ftp%username) > 0) then
                    if (len_trim(ftp%password) > 0) then
                        user_password = trim(ftp%username) // ':' // trim(ftp%password)
                    else
                        user_password = trim(ftp%username)
                    end if

                    stat = curl_easy_setopt(curl_ctx, CURLOPT_USERPWD, user_password); if (stat /= CURLE_OK) exit curl_block
                end if

                if (ftp%active) then
                    stat = curl_easy_setopt(curl_ctx, CURLOPT_FTPPORT, '-'); if (stat /= CURLE_OK) exit curl_block ! Active mode.
                end if

                if (ftp%create_missing) then
                    stat = curl_easy_setopt(curl_ctx, CURLOPT_FTP_CREATE_MISSING_DIRS, 1); if (stat /= CURLE_OK) exit curl_block ! Create missing directories.
                end if

                if (ftp%tls .and. .not. ftp%verify_tls) then
                    stat = curl_easy_setopt(curl_ctx, CURLOPT_SSL_VERIFYPEER, 0); if (stat /= CURLE_OK) exit curl_block ! Skip peer verification.
                    stat = curl_easy_setopt(curl_ctx, CURLOPT_SSL_VERIFYHOST, 0); if (stat /= CURLE_OK) exit curl_block ! Skip host verification.
                end if

                if (debug_) then
                    stat = curl_easy_setopt(curl_ctx, CURLOPT_VERBOSE,  1); if (stat /= CURLE_OK) exit curl_block ! Enable debug messages.
                else
                    stat = curl_easy_setopt(curl_ctx, CURLOPT_NOSIGNAL, 1); if (stat /= CURLE_OK) exit curl_block ! Disable debug messages.
                end if

                stat = curl_easy_perform(curl_ctx)
            end block curl_block

            rc = dm_ftp_error(stat)

            call curl_slist_free_all(list_ctx)
            call curl_easy_cleanup(curl_ctx)

            if (c_fclose(upload%stream) == 0) upload%stream = c_null_ptr
        end block ftp_block

        if (present(error_curl)) error_curl = stat
        if (.not. present(error_message)) return

        if (dm_is_error(rc)) then
            error_message = dm_ftp_error_message(stat)
        else
            error_message = ''
        end if
    end function dm_ftp_upload

    function dm_ftp_url(host, port, path, tls) result(url)
        !! Returns allocatable string of FTP server URL in the form
        !! `ftp[s]://host[:port]/path`. Uses the URL API of libcurl to create
        !! the URL. By default, Transport-Layer Security (FTPS) is disabled.
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
                stat = curl_url_set(ptr, CURLUPART_SCHEME, 'ftps')
            else
                stat = curl_url_set(ptr, CURLUPART_SCHEME, 'ftp')
            end if
            if (stat /= CURLUE_OK) exit url_block

            stat = curl_url_set(ptr, CURLUPART_HOST, trim(host)); if (stat /= CURLUE_OK) exit url_block ! URL host.

            if (has_port) stat = curl_url_set(ptr, CURLUPART_PORT, dm_itoa(port_)); if (stat /= CURLUE_OK) exit url_block ! URL port.
            if (has_path) stat = curl_url_set(ptr, CURLUPART_PATH, trim(path));     if (stat /= CURLUE_OK) exit url_block ! URL path.

            ! Get full URL.
            stat = curl_url_get(ptr, CURLUPART_URL, url)
        end block url_block

        call curl_url_cleanup(ptr)
        if (.not. allocated(url)) url = ''
    end function dm_ftp_url

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

    function dm_ftp_read_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! C-interoperable read callback function for libcurl. Do not call
        !! this function directly.
        use :: unix, only: c_fread

        type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of memory.
        integer(kind=c_size_t), intent(in), value :: sz    !! Always 1.
        integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the memory chunk.
        type(c_ptr),            intent(in), value :: data  !! C pointer to argument passed by caller.
        integer(kind=c_size_t)                    :: n     !! Function return value.

        type(ftp_upload_type), pointer :: upload

        n = int(0, kind=c_size_t)
        if (.not. c_associated(data)) return
        call c_f_pointer(data, upload)
        if (.not. c_associated(upload%stream)) return
        n = c_fread(ptr, sz, nmemb, upload%stream)
    end function dm_ftp_read_callback

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_ftp_shutdown()
        !! Cleans up FTP backend.
        call curl_global_cleanup()
    end subroutine dm_ftp_shutdown

    subroutine dm_ftp_out(ftp, unit)
        !! Prints FTP connection type to standard output or given file unit.
        type(ftp_type), intent(inout)        :: ftp  !! FTP type.
        integer,        intent(in), optional :: unit !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, stdout)

        write (unit_, '("ftp.host: ", a)')             trim(ftp%host)
        write (unit_, '("ftp.port: ", i0)')            ftp%port
        write (unit_, '("ftp.accept_timeout: ", i0)')  ftp%accept_timeout
        write (unit_, '("ftp.connect_timeout: ", i0)') ftp%connect_timeout
        write (unit_, '("ftp.timeout: ", i0)')         ftp%timeout
        write (unit_, '("ftp.create_missing: ", l1)')  ftp%create_missing
        write (unit_, '("ftp.tls: ", l1)')             ftp%tls
        write (unit_, '("ftp.verify_tls: ", l1)')      ftp%verify_tls
        write (unit_, '("ftp.username: ", a)')         trim(ftp%username)
        write (unit_, '("ftp.password: ", a)')         trim(ftp%password)
        write (unit_, '("ftp.url: ", a)')              trim(ftp%url)
    end subroutine dm_ftp_out

    subroutine dm_ftp_set(ftp, host, port, accept_timeout, connect_timeout, timeout, active, create_missing, &
                          tls, verify_tls, username, password, url)
        type(ftp_type),   intent(inout)        :: ftp             !! FTP type.
        character(len=*), intent(in), optional :: host            !! FTP host.
        integer,          intent(in), optional :: port            !! FTP port (or 0 for default).
        integer,          intent(in), optional :: accept_timeout  !! cURL accept timeout [sec].
        integer,          intent(in), optional :: connect_timeout !! cURL connection timeout [sec].
        integer,          intent(in), optional :: timeout         !! cURL timeout [sec].
        logical,          intent(in), optional :: active          !! Active mode.
        logical,          intent(in), optional :: create_missing  !! Create missing directories.
        logical,          intent(in), optional :: tls             !! Transport-Layer Security.
        logical,          intent(in), optional :: verify_tls      !! Verify SSL cert.
        character(len=*), intent(in), optional :: username        !! FTP user name.
        character(len=*), intent(in), optional :: password        !! FTP password.
        character(len=*), intent(in), optional :: url             !! Remote file URL.

        if (present(host))            ftp%host            = host
        if (present(port))            ftp%port            = port
        if (present(accept_timeout))  ftp%accept_timeout  = accept_timeout
        if (present(connect_timeout)) ftp%connect_timeout = connect_timeout
        if (present(timeout))         ftp%timeout         = timeout
        if (present(active))          ftp%active          = active
        if (present(create_missing))  ftp%create_missing  = create_missing
        if (present(tls))             ftp%tls             = tls
        if (present(verify_tls))      ftp%verify_tls      = verify_tls
        if (present(username))        ftp%username        = username
        if (present(password))        ftp%password        = password
        if (present(url))             ftp%url             = url
    end subroutine dm_ftp_set
end module dm_ftp
