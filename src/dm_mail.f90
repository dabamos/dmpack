! Author:  Philipp Engel
! Licence: ISC
module dm_mail
    !! Module for sending plain-text e-mails via SMTP, using cURL.
    !!
    !! To send an e-mail, create an SMTP server configuration and a mail type
    !! first:
    !!
    !! ```fortran
    !! integer           :: rc
    !! type(mail_type)   :: mail
    !! type(server_type) :: server
    !!
    !! rc = dm_mail_create(server, 'example.com', 'username', 'password')
    !! rc = dm_mail_create(mail, from=person_type(mail='alice@example.com'), &
    !!                     to=[person_type(mail='bob@example.com')], &
    !!                     subject='Subject', message='Message')
    !! rc = dm_mail_send(mail, server)
    !! ```
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_error
    use :: dm_person
    use :: dm_time
    use :: dm_type
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: MAIL_PLAIN = 0 !! No transport-layer security.
    integer, parameter, public :: MAIL_SSL   = 1 !! Explicit SSL.
    integer, parameter, public :: MAIL_TLS   = 2 !! Implicit TLS (StartTLS).

    abstract interface
        function mail_callback(ptr, sz, nmemb, data) bind(c) result(n)
            !! Private abstract interface of cURL read callback.
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: ptr   !! C pointer to a chunk of memory.
            integer(kind=c_size_t), intent(in), value :: sz    !! Always 1.
            integer(kind=c_size_t), intent(in), value :: nmemb !! Size of the memory chunk.
            type(c_ptr),            intent(in), value :: data  !! C pointer to client data passed by caller.
            integer(kind=c_size_t)                    :: n     !! Function return value.
        end function mail_callback
    end interface

    type :: payload_type
        !! Payload type.
        character(len=:), allocatable :: data
        integer(kind=i8)              :: length = 0_i8
        integer(kind=i8)              :: nbytes = 0_i8
    end type payload_type

    type, public :: mail_server_type
        !! Opaque SMTP server type.
        private
        character(len=:), allocatable :: url                          !! SMTP server URL.
        character(len=:), allocatable :: username                     !! SMTP user name.
        character(len=:), allocatable :: password                     !! SMTP password.
        integer                       :: connect_timeout = 30         !! Connection timeout in seconds.
        integer                       :: timeout         = 30         !! Timeout in seconds.
        integer                       :: tls             = MAIL_PLAIN !! Transport-layer security.
        logical                       :: verify_ssl      = .false.    !! Verify SSL cert and host name.
        logical                       :: allocated       = .false.    !! Allocation status.
    end type mail_server_type

    type, public :: mail_type
        !! Opaque e-mail type.
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
    end interface

    interface dm_mail_create
        !! Generic function to create mail or server data type.
        module procedure :: dm_mail_create_mail
        module procedure :: dm_mail_create_server
    end interface

    public :: dm_mail_address
    public :: dm_mail_create
    public :: dm_mail_create_mail
    public :: dm_mail_create_server
    public :: dm_mail_send
    public :: dm_mail_url
    public :: dm_mail_write

    private :: mail_address_person
    private :: mail_address_persons
    private :: mail_read_callback
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_mail_create_mail(mail, from, to, subject, message, cc, bcc) result(rc)
        !! Creates new mail type and inserts passed values.
        type(mail_type),   intent(out)          :: mail    !! Mail type.
        type(person_type), intent(in)           :: from    !! Mail sender.
        type(person_type), intent(in)           :: to(:)   !! Mail recipients.
        character(len=*),  intent(in)           :: subject !! Mail subject.
        character(len=*),  intent(in)           :: message !! Mail message.
        type(person_type), intent(in), optional :: cc(:)   !! Mail recipients (CC).
        type(person_type), intent(in), optional :: bcc(:)  !! Mail recipients (BCC).

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

        mail%from      = from
        mail%to        = to
        mail%subject   = trim(subject)
        mail%message   = trim(message)
        mail%allocated = .true.

        if (.not. allocated(mail%cc))  allocate (mail%cc(0))
        if (.not. allocated(mail%bcc)) allocate (mail%bcc(0))

        rc = E_NONE
    end function dm_mail_create_mail

    integer function dm_mail_create_server(server, host, username, password, port, tls, &
                                           timeout, connect_timeout, verify_ssl) result(rc)
        !! Returns SMTP server type.
        type(mail_server_type), intent(out)          :: server          !! SMTP server type.
        character(len=*),       intent(in)           :: host            !! SMTP server host.
        character(len=*),       intent(in)           :: username        !! SMTP user name.
        character(len=*),       intent(in)           :: password        !! SMTP password.
        integer,                intent(in), optional :: port            !! SMTP server port (or 0).
        integer,                intent(in), optional :: tls             !! SMTP transport-layer security.
        integer,                intent(in), optional :: timeout         !! cURL timeout in seconds.
        integer,                intent(in), optional :: connect_timeout !! cURL connection timeout in seconds.
        logical,                intent(in), optional :: verify_ssl      !! Verify SSL cert.

        integer :: port_

        rc = E_INVALID

        port_ = 0
        if (present(port)) port_ = port
        if (len_trim(host) == 0 .or. port_ < 0) return

        if (present(tls)) server%tls = tls
        if (server%tls < MAIL_PLAIN .or. server%tls > MAIL_TLS) return

        if (present(verify_ssl))      server%verify_ssl      = verify_ssl
        if (present(timeout))         server%timeout         = timeout
        if (present(connect_timeout)) server%connect_timeout = connect_timeout

        server%url       = dm_mail_url(host, port=port_, tls=server%tls)
        server%username  = trim(username)
        server%password  = trim(password)
        server%allocated = .true.

        rc = E_NONE
    end function dm_mail_create_server

    integer function dm_mail_send(mail, server, error_message, error_curl, debug) result(rc)
        !! Sends SMTP request by calling libcurl.
        type(mail_type),               intent(inout)         :: mail
        type(mail_server_type),        intent(inout)         :: server
        character(len=:), allocatable, intent(out), optional :: error_message
        integer,                       intent(out), optional :: error_curl
        logical,                       intent(in),  optional :: debug

        integer                    :: er, i
        logical                    :: debug_
        type(c_ptr)                :: curl_ptr, list_ptr
        type(payload_type), target :: payload

        if (present(error_message)) error_message = ''
        if (present(error_curl)) error_curl = CURLE_OK

        debug_ = .false.
        if (present(debug)) debug_ = debug

        rc = E_INVALID
        if (.not. mail%allocated) return
        if (.not. server%allocated) return

        ! Prepare payload.
        payload%data   = dm_mail_write(mail)
        payload%length = len(payload%data)

        ! Send mail to server.
        list_ptr = c_null_ptr
        curl_ptr = curl_easy_init()

        rc = E_IO
        if (.not. c_associated(curl_ptr)) return

        curl_block: block
            rc = E_INVALID

            ! SMTP server URL.
            er = curl_easy_setopt(curl_ptr, CURLOPT_URL, server%url)
            if (er /= CURLE_OK) exit curl_block

            ! SMTP user name.
            er = curl_easy_setopt(curl_ptr, CURLOPT_USERNAME, server%username)
            if (er /= CURLE_OK) exit curl_block

            ! SMTP password.
            er = curl_easy_setopt(curl_ptr, CURLOPT_PASSWORD, server%password)
            if (er /= CURLE_OK) exit curl_block

            ! Transport-Layer Security.
            if (server%tls /= MAIL_PLAIN) then
                ! StartTLS.
                if (server%tls == MAIL_TLS) then
                    er = curl_easy_setopt(curl_ptr, CURLOPT_USE_SSL, CURLUSESSL_ALL)
                    if (er /= CURLE_OK) exit curl_block
                end if

                if (.not. server%verify_ssl) then
                    ! Skip peer verification.
                    er = curl_easy_setopt(curl_ptr, CURLOPT_SSL_VERIFYPEER, 0)
                    if (er /= CURLE_OK) exit curl_block

                    ! Skip host verification.
                    er = curl_easy_setopt(curl_ptr, CURLOPT_SSL_VERIFYHOST, 0)
                    if (er /= CURLE_OK) exit curl_block
                end if
            end if

            ! Set MAIL FROM.
            er = curl_easy_setopt(curl_ptr, CURLOPT_MAIL_FROM, dm_mail_address(mail%from))
            if (er /= CURLE_OK) exit curl_block

            ! Set recipients.
            do i = 1, size(mail%to)
                list_ptr = curl_slist_append(list_ptr, dm_mail_address(mail%to(i)))
            end do

            do i = 1, size(mail%cc)
                list_ptr = curl_slist_append(list_ptr, dm_mail_address(mail%cc(i)))
            end do

            do i = 1, size(mail%bcc)
                list_ptr = curl_slist_append(list_ptr, dm_mail_address(mail%bcc(i)))
            end do

            er = curl_easy_setopt(curl_ptr, CURLOPT_MAIL_RCPT, list_ptr)
            if (er /= CURLE_OK) exit curl_block

            ! Set timeout.
            er = curl_easy_setopt(curl_ptr, CURLOPT_TIMEOUT, server%timeout)
            if (er /= CURLE_OK) exit curl_block

            ! Set connection timeout.
            er = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT, server%connect_timeout)
            if (er /= CURLE_OK) exit curl_block

            ! Set callback function.
            er = curl_easy_setopt(curl_ptr, CURLOPT_READFUNCTION, c_funloc(mail_read_callback))
            if (er /= CURLE_OK) exit curl_block

            er = curl_easy_setopt(curl_ptr, CURLOPT_READDATA, c_loc(payload))
            if (er /= CURLE_OK) exit curl_block

            er = curl_easy_setopt(curl_ptr, CURLOPT_UPLOAD, 1)
            if (er /= CURLE_OK) exit curl_block

            ! Enable or disable debug messages.
            if (debug_) then
                er = curl_easy_setopt(curl_ptr, CURLOPT_VERBOSE, 1)
                if (er /= CURLE_OK) exit curl_block
            else
                er = curl_easy_setopt(curl_ptr, CURLOPT_NOSIGNAL, 1)
                if (er /= CURLE_OK) exit curl_block
            end if

            ! Send request.
            rc = E_IO
            er = curl_easy_perform(curl_ptr)
            if (er /= CURLE_OK) exit curl_block
            rc = E_NONE
        end block curl_block

        if (present(error_message) .and. er /= CURLE_OK) then
            error_message = curl_easy_strerror(er)
        end if

        if (present(error_curl)) error_curl = er

        call curl_slist_free_all(list_ptr)
        call curl_easy_cleanup(curl_ptr)
    end function dm_mail_send

    pure function dm_mail_url(host, port, tls) result(url)
        !! Returns allocatable string of SMTP server URL in the form
        !! `smtp[s]://host[:port]`.
        character(len=*), intent(in)           :: host !! SMTP server host name.
        integer,          intent(in), optional :: port !! SMTP server port.
        integer,          intent(in), optional :: tls  !! Transport-layer security.
        character(len=:), allocatable          :: url

        integer :: port_, tls_

        port_ = 0
        if (present(port)) port_ = port

        tls_ = MAIL_PLAIN
        if (present(tls)) tls_ = tls

        if (tls_ == MAIL_SSL) then
            url = 'smtps://'
        else
            url = 'smtp://'
        end if

        if (port > 0) then
            url = url // trim(host) // ':' // dm_itoa(port)
        else
            url = url // trim(host)
        end if
    end function dm_mail_url

    function dm_mail_write(mail) result(payload)
        !! Returns allocatable e-mail string. This function does not verify
        !! the allocation state of the given mail type. It has been made
        !! public to simplify testing.
        use :: dm_ascii, only: CR_LF
        type(mail_type), intent(inout) :: mail !! Mail type.
        character(len=:), allocatable  :: payload

        payload = 'Date: ' // dm_time_rfc2822()          // CR_LF // &
                  'To: '   // dm_mail_address(mail%to)   // CR_LF // &
                  'From: ' // dm_mail_address(mail%from) // CR_LF

        if (size(mail%cc) > 0) then
            payload = payload // 'Cc: ' // dm_mail_address(mail%cc) // CR_LF
        end if

        payload = payload // 'Subject: ' // mail%subject // CR_LF // &
                  CR_LF // mail%message // CR_LF
    end function dm_mail_write

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    pure function mail_address_person(person) result(str)
        !! Returns e-mail address as allocatable string in the form `<address>`
        !! or `"name" <address>`, depending on whether the person has a name.
        type(person_type), intent(in) :: person !! Person type.
        character(len=:), allocatable :: str    !! Address string.

        if (dm_person_has_name(person)) then
            str = '"' // trim(person%name) // '" <' // trim(person%mail) // '>'
        else
            str = '<' // trim(person%mail) // '>'
        end if
    end function mail_address_person

    pure function mail_address_persons(persons) result(str)
        !! Returns list of e-mail addresses in allocatable string.
        type(person_type), intent(in) :: persons(:) !! Array of person types.
        character(len=:), allocatable :: str        !! List of addresses.
        integer                       :: i, n

        n = size(persons)

        if (n == 0) then
            str = ''
            return
        end if

        str = dm_mail_address(persons(1))
        if (n == 1) return

        do i = 2, n
            str = str // ', ' // dm_mail_address(persons(i))
        end do
    end function mail_address_persons

    function mail_read_callback(ptr, sz, nmemb, data) bind(c) result(n)
        !! Callback function to upload payload passed via `data` to the
        !! memory chunk in `ptr`.
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

        chunk => null()
        payload => null()

        call c_f_pointer(ptr, chunk)
        call c_f_pointer(data, payload)

        if (.not. associated(chunk)) return
        if (.not. associated(payload)) return

        if (.not. allocated(payload%data) .or. payload%length <= 0) return
        if (payload%nbytes == payload%length) return

        length = payload%length - payload%nbytes
        if (room < length) length = room

        chunk = payload%data(payload%nbytes + 1:payload%nbytes + length)
        payload%nbytes = payload%nbytes + length

        n = int(length, kind=c_size_t)
    end function mail_read_callback
end module dm_mail
