! Author:  Philipp Engel
! Licence: ISC
module dm_mail
    !! Module for sending plain-text e-mails via SMTP, using libcurl.
    !!
    !! To send an e-mail, create an SMTP server configuration and a mail type
    !! first:
    !!
    !! ```fortran
    !! integer                :: rc
    !! type(mail_type)        :: mail
    !! type(mail_server_type) :: server
    !! type(person_type)      :: from, to(1)
    !!
    !! ! Initialise SMTP backend and set SMTP server details.
    !! rc = dm_mail_init()
    !! rc = dm_mail_create(server, 'example.com', 'username', 'password')
    !!
    !! ! Create sender, receiver, and e-mail.
    !! from  = person_type(mail='alice@example.com')
    !! to(1) = person_type(mail='bob@example.com')
    !!
    !! rc = dm_mail_create(mail, from=from, to=to, subject='Subject', message='Message')
    !!
    !! ! Send e-mail and finalise SMTP backend.
    !! rc = dm_mail_send(mail, server)
    !! call dm_mail_shutdown()
    !! ```
    !!
    !! The procedure `dm_mail_init()` has to be called once per process, and
    !! only if neither the RPC nor the MQTT backend is initialised already.
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_error
    use :: dm_kind
    use :: dm_person
    use :: dm_time
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: MAIL_TLS_NONE     = 0 !! No transport-layer security.
    integer, parameter, public :: MAIL_TLS_EXPLICIT = 1 !! Explicit SSL.
    integer, parameter, public :: MAIL_TLS_IMPLICIT = 2 !! Implicit TLS (StartTLS).
    integer, parameter, public :: MAIL_TLS_LAST     = 2 !! Never use thise.

    type :: payload_type
        !! Private payload type.
        character(len=:), allocatable :: data          !! Bytes to send.
        integer(kind=i8)              :: length = 0_i8 !! Length of bytes string.
        integer(kind=i8)              :: nbytes = 0_i8 !! Number of bytes sent.
    end type payload_type

    type, public :: mail_server_type
        !! Opaque SMTP server type that stores connection settings.
        private
        character(len=:), allocatable :: url                             !! SMTP server URL.
        character(len=:), allocatable :: username                        !! SMTP user name.
        character(len=:), allocatable :: password                        !! SMTP password.
        integer                       :: connect_timeout = 30            !! Connection timeout [sec].
        integer                       :: timeout         = 30            !! Timeout [sec].
        integer                       :: tls             = MAIL_TLS_NONE !! Transport-layer security.
        logical                       :: verify_tls      = .false.       !! Verify SSL cert and host name.
        logical                       :: allocated       = .false.       !! Allocation status.
    end type mail_server_type

    type, public :: mail_type
        !! Opaque e-mail type that stores sender, recipients, subject, message,
        !! and allocation status.
        private
        type(person_type)              :: from                !! E-mail From.
        type(person_type), allocatable :: to(:)               !! E-mail To.
        type(person_type), allocatable :: cc(:)               !! E-mail CC.
        type(person_type), allocatable :: bcc(:)              !! E-mail BCC.
        character(len=:),  allocatable :: subject             !! E-mail subject.
        character(len=:),  allocatable :: message             !! E-mail message.
        logical                        :: allocated = .false. !! Allocation status.
    end type mail_type

    interface dm_mail_address
        !! Generic function that returns formatted addresses.
        module procedure :: mail_address_person
        module procedure :: mail_address_persons
    end interface dm_mail_address

    interface dm_mail_create
        !! Generic function to create mail or server data type.
        module procedure :: dm_mail_create_mail
        module procedure :: dm_mail_create_server
    end interface dm_mail_create

    interface dm_mail_out
        !! Generic routine to print mail and server type.
        module procedure :: mail_out_mail
        module procedure :: mail_out_server
    end interface dm_mail_out

    public :: dm_mail_address
    public :: dm_mail_create
    public :: dm_mail_create_mail
    public :: dm_mail_create_server
    public :: dm_mail_error
    public :: dm_mail_error_message
    public :: dm_mail_init
    public :: dm_mail_out
    public :: dm_mail_send
    public :: dm_mail_shutdown
    public :: dm_mail_url
    public :: dm_mail_write

    public :: dm_mail_read_callback

    private :: mail_address_person
    private :: mail_address_persons
    private :: mail_out_mail
    private :: mail_out_server
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_mail_create_mail(mail, from, to, subject, message, cc, bcc) result(rc)
        !! Creates new mail type and inserts passed values. The function return
        !! `E_INVALID` if given arguments are invalid or incomplete.
        type(mail_type),   intent(out)          :: mail    !! Mail type.
        type(person_type), intent(in)           :: from    !! Mail sender.
        type(person_type), intent(in)           :: to(:)   !! Mail recipients.
        character(len=*),  intent(in)           :: subject !! Mail subject.
        character(len=*),  intent(in)           :: message !! Mail message.
        type(person_type), intent(in), optional :: cc(:)   !! Mail CC recipients.
        type(person_type), intent(in), optional :: bcc(:)  !! Mail BCC recipients.

        rc = E_INVALID

        if (size(to) == 0) return
        if (.not. dm_person_has_mail(from)) return
        if (.not. all(dm_person_has_mail(to))) return

        if (present(cc)) then
            if (.not. all(dm_person_has_mail(cc))) return
            mail%cc = cc
        end if

        if (present(bcc)) then
            if (.not. all(dm_person_has_mail(bcc))) return
            mail%bcc = bcc
        end if

        if (.not. allocated(mail%cc))  allocate (mail%cc(0))
        if (.not. allocated(mail%bcc)) allocate (mail%bcc(0))

        mail%from      = from
        mail%to        = to
        mail%subject   = trim(subject)
        mail%message   = trim(message)
        mail%allocated = .true.

        rc = E_NONE
    end function dm_mail_create_mail

    integer function dm_mail_create_server(server, host, username, password, port, tls, &
                                           timeout, connect_timeout, verify_tls) result(rc)
        !! Returns SMTP server type. Argument `tls` may be one of the following:
        !!
        !! * `MAIL_TLS_NONE`     – No transport-layer security.
        !! * `MAIL_TLS_EXPLICIT` – Explicit SSL.
        !! * `MAIL_TLS_IMPLICIT` – Implicit TLS (StartTLS).
        !!
        !! Parameter `MAIL_TLS_NONE` is used by default. The function returns
        !! `E_INVALID` on error.
        type(mail_server_type), intent(out)          :: server          !! Mail server type.
        character(len=*),       intent(in)           :: host            !! SMTP server host.
        character(len=*),       intent(in)           :: username        !! SMTP user name.
        character(len=*),       intent(in)           :: password        !! SMTP password.
        integer,                intent(in), optional :: port            !! SMTP server port (or 0).
        integer,                intent(in), optional :: tls             !! SMTP transport-layer security.
        integer,                intent(in), optional :: timeout         !! cURL timeout in seconds.
        integer,                intent(in), optional :: connect_timeout !! cURL connection timeout in seconds.
        logical,                intent(in), optional :: verify_tls      !! Verify SSL cert.

        integer :: port_
        logical :: tls_

        port_ = dm_present(port, 0)
        tls_  = .false.

        rc = E_INVALID
        if (len_trim(host) == 0 .or. port_ < 0) return

        if (present(tls)) server%tls = tls
        if (server%tls < MAIL_TLS_NONE .or. server%tls > MAIL_TLS_LAST) return
        tls_ = (server%tls /= MAIL_TLS_NONE)

        if (present(timeout))         server%timeout         = timeout
        if (present(connect_timeout)) server%connect_timeout = connect_timeout
        if (present(verify_tls))      server%verify_tls      = verify_tls

        if (server%connect_timeout < 0) return

        server%url       = dm_mail_url(host, port=port_, tls=tls_)
        server%username  = trim(username)
        server%password  = trim(password)
        server%allocated = .true.

        rc = E_NONE
    end function dm_mail_create_server

    integer function dm_mail_error(error_curl) result(rc)
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
                rc = E_MAIL_CONNECT

            case (CURLE_REMOTE_ACCESS_DENIED, &
                  CURLE_AUTH_ERROR)
                rc = E_MAIL_AUTH

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
                rc = E_MAIL_SSL

            case (CURLE_FILESIZE_EXCEEDED)
                rc = E_LIMIT

            case default
                rc = E_MAIL
        end select
    end function dm_mail_error

    function dm_mail_error_message(error_curl) result(message)
        !! Return message associated with given cURL error code as allocatable
        !! character string.
        integer, intent(in)           :: error_curl !! cURL error code.
        character(len=:), allocatable :: message    !! Error message.

        message = curl_easy_strerror(error_curl)
    end function dm_mail_error_message

    integer function dm_mail_init() result(rc)
        !! Initialises SMTP backend. The function returns `E_MAIL` on error.
        rc = E_MAIL
        if (curl_global_init(CURL_GLOBAL_DEFAULT) /= CURLE_OK) return
        rc = E_NONE
    end function dm_mail_init

    integer function dm_mail_send(mail, server, error_message, error_curl, debug) result(rc)
        !! Sends SMTP request by calling libcurl.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if mail or server type is not initialised properly.
        !! * `E_INVALID` if mail or server data is invalid.
        !! * `E_MAIL` if libcurl initialisation failed.
        !! * `E_MAIL_AUTH` if SMTP authentication failed.
        !! * `E_MAIL_CONNECT` if connection to server could not be established.
        !! * `E_MAIL_SSL` if SSL/TLS error occured.
        !!
        type(mail_type),               intent(inout)         :: mail          !! Mail type.
        type(mail_server_type),        intent(inout)         :: server        !! Mail server type.
        character(len=:), allocatable, intent(out), optional :: error_message !! Error message.
        integer,                       intent(out), optional :: error_curl    !! cURL error code.
        logical,                       intent(in),  optional :: debug         !! Output debug messages.

        integer                    :: i, stat
        logical                    :: debug_
        type(c_ptr)                :: curl_ctx, list_ctx
        type(payload_type), target :: payload

        stat   = CURLE_OK
        debug_ = dm_present(debug, .false.)

        mail_block: block
            ! Mail and server must be initialised.
            rc = E_CORRUPT
            if (.not. mail%allocated .or. .not. server%allocated) exit mail_block

            ! Prepare payload.
            payload%data   = dm_mail_write(mail)
            payload%length = len(payload%data)

            ! Send mail to server.
            list_ctx = c_null_ptr
            curl_ctx = curl_easy_init()

            rc = E_MAIL
            if (.not. c_associated(curl_ctx)) exit mail_block

            ! Prepare request.
            curl_block: block
                ! SMTP server URL.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_URL, server%url)
                if (stat /= CURLE_OK) exit curl_block

                ! SMTP user name.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_USERNAME, server%username)
                if (stat /= CURLE_OK) exit curl_block

                ! SMTP password.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_PASSWORD, server%password)
                if (stat /= CURLE_OK) exit curl_block

                ! Transport-Layer Security.
                if (server%tls /= MAIL_TLS_NONE) then
                    ! StartTLS.
                    if (server%tls == MAIL_TLS_IMPLICIT) then
                        stat = curl_easy_setopt(curl_ctx, CURLOPT_USE_SSL, CURLUSESSL_ALL)
                        if (stat /= CURLE_OK) exit curl_block
                    end if

                    if (.not. server%verify_tls) then
                        ! Skip peer verification.
                        stat = curl_easy_setopt(curl_ctx, CURLOPT_SSL_VERIFYPEER, 0)
                        if (stat /= CURLE_OK) exit curl_block

                        ! Skip host verification.
                        stat = curl_easy_setopt(curl_ctx, CURLOPT_SSL_VERIFYHOST, 0)
                        if (stat /= CURLE_OK) exit curl_block
                    end if
                end if

                ! Set MAIL FROM.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_MAIL_FROM, dm_mail_address(mail%from))
                if (stat /= CURLE_OK) exit curl_block

                ! Set recipients.
                do i = 1, size(mail%to)
                    list_ctx = curl_slist_append(list_ctx, dm_mail_address(mail%to(i)))
                end do

                do i = 1, size(mail%cc)
                    list_ctx = curl_slist_append(list_ctx, dm_mail_address(mail%cc(i)))
                end do

                do i = 1, size(mail%bcc)
                    list_ctx = curl_slist_append(list_ctx, dm_mail_address(mail%bcc(i)))
                end do

                stat = curl_easy_setopt(curl_ctx, CURLOPT_MAIL_RCPT, list_ctx)
                if (stat /= CURLE_OK) exit curl_block

                ! Set timeout.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_TIMEOUT, server%timeout)
                if (stat /= CURLE_OK) exit curl_block

                ! Set connection timeout.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_CONNECTTIMEOUT, server%connect_timeout)
                if (stat /= CURLE_OK) exit curl_block

                ! Set callback function.
                stat = curl_easy_setopt(curl_ctx, CURLOPT_READFUNCTION, c_funloc(dm_mail_read_callback))
                if (stat /= CURLE_OK) exit curl_block

                stat = curl_easy_setopt(curl_ctx, CURLOPT_READDATA, c_loc(payload))
                if (stat /= CURLE_OK) exit curl_block

                stat = curl_easy_setopt(curl_ctx, CURLOPT_UPLOAD, 1)
                if (stat /= CURLE_OK) exit curl_block

                ! Enable or disable debug messages.
                if (debug_) then
                    stat = curl_easy_setopt(curl_ctx, CURLOPT_VERBOSE, 1)
                    if (stat /= CURLE_OK) exit curl_block
                else
                    stat = curl_easy_setopt(curl_ctx, CURLOPT_NOSIGNAL, 1)
                    if (stat /= CURLE_OK) exit curl_block
                end if

                ! Send request.
                stat = curl_easy_perform(curl_ctx)
            end block curl_block

            rc = dm_mail_error(stat)

            call curl_slist_free_all(list_ctx)
            call curl_easy_cleanup(curl_ctx)
        end block mail_block

        if (present(error_curl)) error_curl = stat
        if (.not. present(error_message)) return

        if (dm_is_error(rc)) then
            error_message = dm_mail_error_message(stat)
        else
            error_message = ''
        end if
    end function dm_mail_send

    function dm_mail_url(host, port, tls) result(url)
        !! Returns allocatable string of SMTP server URL in the form
        !! `smtp[s]://host[:port]/`. Uses the URL API of libcurl to create the
        !! URL. By default, Transport Layer Security is disabled.
        character(len=*), intent(in)           :: host !! SMTP server host name.
        integer,          intent(in), optional :: port !! SMTP server port (up to 5 digits).
        logical,          intent(in), optional :: tls  !! Transport-layer security (`MAIL_TLS_*`).
        character(len=:), allocatable          :: url  !! URL of SMTP server.

        integer     :: port_
        integer     :: stat
        logical     :: tls_
        type(c_ptr) :: ptr

        port_ = dm_present(port, 0)
        tls_  = dm_present(tls, .false.)

        url_block: block
            ptr = curl_url()
            if (.not. c_associated(ptr)) exit url_block

            ! URL scheme.
            if (tls_) then
                stat = curl_url_set(ptr, CURLUPART_SCHEME, 'smtps')
                if (stat /= CURLUE_OK) exit url_block
            else
                stat = curl_url_set(ptr, CURLUPART_SCHEME, 'smtp')
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

            ! Get full URL.
            stat = curl_url_get(ptr, CURLUPART_URL, url)
        end block url_block

        call curl_url_cleanup(ptr)
        if (.not. allocated(url)) url = ''
    end function dm_mail_url

    function dm_mail_write(mail) result(payload)
        !! Returns allocatable e-mail string. This function does not verify
        !! the allocation state of the given mail type. It has been made
        !! public to simplify testing.
        use :: dm_ascii, only: CR_LF

        type(mail_type), intent(inout) :: mail    !! Mail type.
        character(len=:), allocatable  :: payload !! E-mail data.

        payload = 'Date: ' // dm_time_rfc2822()          // CR_LF // &
                  'To: '   // dm_mail_address(mail%to)   // CR_LF // &
                  'From: ' // dm_mail_address(mail%from) // CR_LF

        if (size(mail%cc) > 0) then
            payload = payload // 'Cc: ' // dm_mail_address(mail%cc) // CR_LF
        end if

        payload = payload // 'Subject: ' // mail%subject // CR_LF // &
                  CR_LF // mail%message // CR_LF
    end function dm_mail_write

    subroutine dm_mail_shutdown()
        !! Cleans up SMTP backend.
        call curl_global_cleanup()
    end subroutine dm_mail_shutdown

    ! **************************************************************************
    ! PUBLIC CALLBACK FUNCTIONS.
    ! **************************************************************************
    function dm_mail_read_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! Callback function to upload payload passed via `data` to the
        !! memory chunk in `ptr`. Do not call this function directly.
        type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of memory.
        integer(kind=c_size_t), intent(in), value :: sz    !! Always 1.
        integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the memory chunk.
        type(c_ptr),            intent(in), value :: data  !! C pointer to argument passed by caller.
        integer(kind=c_size_t)                    :: n     !! Function return value.

        character(len=:),   pointer :: chunk
        integer(kind=i8)            :: length, room
        type(payload_type), pointer :: payload

        n = int(0, kind=c_size_t)
        room = sz * nmemb

        if (sz == 0 .or. nmemb == 0 .or. room < 1) return
        if (.not. c_associated(ptr) .or. .not. c_associated(data)) return

        chunk   => null()
        payload => null()

        call c_f_pointer(ptr, chunk)
        call c_f_pointer(data, payload)

        if (.not. associated(chunk))   return
        if (.not. associated(payload)) return

        if (.not. allocated(payload%data) .or. payload%length <= 0) return
        if (payload%nbytes == payload%length) return

        length = payload%length - payload%nbytes
        if (room < length) length = room

        chunk = payload%data(payload%nbytes + 1:payload%nbytes + length)
        payload%nbytes = payload%nbytes + length

        n = int(length, kind=c_size_t)
    end function dm_mail_read_callback

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    pure function mail_address_person(person) result(string)
        !! Returns e-mail address as allocatable string in the form `<address>`
        !! or `"name" <address>`, depending on whether the person has a name.
        type(person_type), intent(in) :: person !! Person type.
        character(len=:), allocatable :: string !! Address string.

        if (dm_person_has_name(person)) then
            string = '"' // trim(person%name) // '" <' // trim(person%mail) // '>'
        else
            string = '<' // trim(person%mail) // '>'
        end if
    end function mail_address_person

    pure function mail_address_persons(persons) result(string)
        !! Returns list of e-mail addresses in allocatable string.
        type(person_type), intent(in) :: persons(:) !! Array of person types.
        character(len=:), allocatable :: string     !! List of addresses.

        integer :: i, n

        n = size(persons)

        if (n == 0) then
            string = ''
            return
        end if

        string = dm_mail_address(persons(1))
        if (n == 1) return

        do i = 2, n
            string = string // ', ' // dm_mail_address(persons(i))
        end do
    end function mail_address_persons

    subroutine mail_out_mail(mail, unit)
        !! Prints mail type to standard output or given file unit.
        type(mail_type), intent(inout)        :: mail !! Mail type.
        integer,         intent(in), optional :: unit !! File unit.

        integer :: i, unit_

        unit_ = dm_present(unit, stdout)

        write (unit_, '("mail.from: ", a)') dm_mail_address(mail%from)

        if (allocated(mail%to)) then
            do i = 1, size(mail%to)
                write (unit_, '("mail.to(", i0, "): ", a)') i, dm_mail_address(mail%to(i))
            end do
        end if

        if (allocated(mail%cc)) then
            do i = 1, size(mail%cc)
                write (unit_, '("mail.cc(", i0, "): ", a)') i, dm_mail_address(mail%cc(i))
            end do
        end if

        if (allocated(mail%bcc)) then
            do i = 1, size(mail%bcc)
                write (unit_, '("mail.bcc(", i0, "): ", a)') i, dm_mail_address(mail%bcc(i))
            end do
        end if

        if (allocated(mail%subject)) write (unit_, '("mail.subject: ", a)') trim(mail%subject)
        if (allocated(mail%message)) write (unit_, '("mail.message: ", a)') trim(mail%message)
    end subroutine mail_out_mail

    subroutine mail_out_server(server, unit)
        !! Prints mail server type to standard output or given file unit.
        type(mail_server_type), intent(inout)        :: server !! Mail server type.
        integer,                intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, stdout)

        if (allocated(server%url))      write (unit_, '("mail_server.url: ", a)')      trim(server%url)
        if (allocated(server%username)) write (unit_, '("mail_server.username: ", a)') trim(server%username)
        if (allocated(server%password)) write (unit_, '("mail_server.password: ", a)') trim(server%password)

        write (unit_, '("mail_server.connect_timeout: ", i0)') server%connect_timeout
        write (unit_, '("mail_server.timeout: ", i0)')         server%timeout
        write (unit_, '("mail_server.tls: ", i0)')             server%tls
        write (unit_, '("mail_server.verify_tls: ", l1)')      server%verify_tls
    end subroutine mail_out_server
end module dm_mail
