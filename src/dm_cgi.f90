! Author:  Philipp Engel
! Licence: ISC
module dm_cgi
    !! Common Gateway Interface (CGI) utility procedures.
    use :: dm_ascii, only: CR_LF
    use :: dm_error
    use :: dm_hash
    use :: dm_http
    use :: dm_kind
    use :: dm_mime
    use :: dm_string
    use :: dm_time
    use :: dm_util
    implicit none (type, external)
    private

    ! HTTP header names.
    character(len=*), parameter, public :: CGI_ENV_TRANSFER_ID = 'HTTP_DMPACK_TRANSFER_ID'

    integer, parameter :: CGI_ENV_LEN     = 128 !! Maximum length of CGI environment variable name.
    integer, parameter :: CGI_MAX_NPARAMS = 32  !! Maximum number of CGI query parameters.
    integer, parameter :: CGI_PARAM_LEN   = 512 !! Maximum length of CGI query parameter key, value.

    type, public :: cgi_env_type
        !! CGI environment variables type. Changes to this type have to be
        !! regarded in subroutine `dm_html_cgi_env()`.
        character(len=16)  :: auth_type             = ' '  !! AUTH_TYPE
        integer(kind=i8)   :: content_length        = 0_i8 !! CONTENT_LENGTH
        character(len=128) :: content_type          = ' '  !! CONTENT_TYPE
        character(len=512) :: document_root         = ' '  !! DOCUMENT_ROOT
        character(len=8)   :: gateway_interface     = ' '  !! GATEWAY_INTERFACE
        character(len=512) :: http_accept           = ' '  !! HTTP_ACCEPT
        character(len=16)  :: http_content_encoding = ' '  !! HTTP_CONTENT_ENCODING
        character(len=512) :: http_cookie           = ' '  !! HTTP_COOKIE
        character(len=128) :: http_from             = ' '  !! HTTP_FROM
        character(len=512) :: http_referer          = ' '  !! HTTP_REFERER
        character(len=512) :: http_user_agent       = ' '  !! HTTP_USER_AGENT
        character(len=512) :: path_info             = ' '  !! PATH_INFO
        character(len=512) :: path_translated       = ' '  !! PATH_TRANSLATED
        character(len=512) :: query_string          = ' '  !! QUERY_STRING
        character(len=32)  :: remote_addr           = ' '  !! REMOTE_ADDR
        character(len=512) :: remote_host           = ' '  !! REMOTE_HOST
        character(len=128) :: remote_ident          = ' '  !! REMOTE_IDENT
        character(len=128) :: remote_user           = ' '  !! REMOTE_USER
        character(len=8)   :: request_method        = ' '  !! REQUEST_METHOD
        character(len=512) :: request_uri           = ' '  !! REQUEST_URI
        character(len=512) :: script_name           = ' '  !! SCRIPT_NAME
        character(len=128) :: server_name           = ' '  !! SERVER_NAME
        integer            :: server_port           = 0    !! SERVER_PORT
        character(len=32)  :: server_protocol       = ' '  !! SERVER_PROTOCOL
        character(len=32)  :: server_software       = ' '  !! SERVER_SOFTWARE
    end type cgi_env_type

    type, public :: cgi_query_type
        !! Opaque CGI query type. Stores GET and POST parameters as key-value pairs.
        private
        character(len=CGI_PARAM_LEN) :: keys(CGI_MAX_NPARAMS)   = ' '  !! Array of keys.
        character(len=CGI_PARAM_LEN) :: values(CGI_MAX_NPARAMS) = ' '  !! Array of values.
        integer(kind=i8)             :: hashes(CGI_MAX_NPARAMS) = 0_i8 !! Array of hashes.
        integer                      :: size                    = 0    !! Number of elements.
    end type cgi_query_type

    interface dm_cgi_get
        !! Generic interface to CGI get functions.
        module procedure :: cgi_get_int32
        module procedure :: cgi_get_int64
        module procedure :: cgi_get_logical
        module procedure :: cgi_get_real32
        module procedure :: cgi_get_real64
        module procedure :: cgi_get_string
    end interface dm_cgi_get

    ! Public procedures.
    public :: dm_cgi_content
    public :: dm_cgi_decode
    public :: dm_cgi_env
    public :: dm_cgi_form
    public :: dm_cgi_get
    public :: dm_cgi_has
    public :: dm_cgi_has_value
    public :: dm_cgi_header
    public :: dm_cgi_is_authenticated
    public :: dm_cgi_key
    public :: dm_cgi_parse
    public :: dm_cgi_query
    public :: dm_cgi_size
    public :: dm_cgi_value
    public :: dm_cgi_write

    ! Private procedures.
    private :: cgi_get_int32
    private :: cgi_get_int64
    private :: cgi_get_logical
    private :: cgi_get_real32
    private :: cgi_get_real64
    private :: cgi_get_string
    private :: cgi_query_loc
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_cgi_content(env, content) result(rc)
        !! Reads HTTP request body (POST method). We have to rely on _read(2)_
        !! as Fortran cannot read unformatted content from standard input. On
        !! error, the string `content` is allocated but empty.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_EMPTY` if no content is available.
        !! * `E_EOF` if end of file is reached.
        !! * `E_READ` if system call failed.
        !!
        use :: unix

        type(cgi_env_type),                    intent(inout) :: env     !! CGI environment type.
        character(len=:), allocatable, target, intent(out)   :: content !! Returned request body.

        integer                :: stat
        integer(kind=c_size_t) :: nn, sz

        rc = E_ALLOC
        allocate (character(len=env%content_length) :: content, stat=stat)
        if (stat /= 0) return

        rc = E_EMPTY
        if (env%content_length == 0) return

        nn = int(env%content_length, kind=c_size_t)
        sz = c_read(STDIN_FILENO, c_loc(content), nn)

        rc = E_EOF
        if (sz == 0) return

        rc = E_READ
        if (sz /= nn) return

        rc = E_NONE
    end function dm_cgi_content

    integer function dm_cgi_decode(input, output) result(rc)
        !! Unwinds percent-encoding in given input string.
        character(len=*),          intent(in)  :: input  !! Encoded input string.
        character(len=len(input)), intent(out) :: output !! Decoded output string.

        integer :: i, ii, j, jj, k
        integer :: stat

        ii = len_trim(input)
        jj = len(output)
        output = ' '

        rc = E_BOUNDS
        if (jj < ii) return

        i = 1
        j = 1

        do
            if (i > ii) exit
            if (j > jj) return

            select case (input(i:i))
                case ('%')
                    if (i + 2 > ii) exit
                    read (input(i + 1:i + 2), '(z2)', iostat=stat) k

                    if (stat == 0) then
                        ! Bytes are in hex.
                        output(j:j) = achar(k)
                        i = i + 2
                    else
                        ! Bytes are not in hex.
                        output(j:j) = input(i:i)
                    end if

                case ('+')
                    output(j:j) = ' '

                case default
                    output(j:j) = input(i:i)
            end select

            i = i + 1
            j = j + 1
        end do

        rc = E_NONE
    end function dm_cgi_decode

    logical function dm_cgi_has(query, key) result(has)
        !! Returns `.true.` if key exists in `query`.
        type(cgi_query_type), intent(inout) :: query !! CGI query type.
        character(len=*),     intent(in)    :: key   !! Parameter key.

        integer          :: loc
        integer(kind=i8) :: hash

        hash = dm_hash_fnv1a(trim(key))
        loc  = findloc(query%hashes, hash, dim=1)
        has  = (loc > 0)
    end function dm_cgi_has

    logical function dm_cgi_has_value(query, key) result(has)
        !! Returns `.true.` if key exists in `query` and has value.
        type(cgi_query_type), intent(inout) :: query !! CGI query type.
        character(len=*),     intent(in)    :: key   !! Parameter key.

        integer          :: loc
        integer(kind=i8) :: hash

        has  = .false.
        hash = dm_hash_fnv1a(trim(key))
        loc  = findloc(query%hashes, hash, dim=1)

        if (loc == 0) return
        if (len_trim(query%values(loc)) == 0) return
        has = .true.
    end function dm_cgi_has_value

    logical function dm_cgi_is_authenticated(env) result(auth)
        !! Returns `.true.` if CGI environment variable `AUTH` is set to
        !! `Basic`.
        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        auth = (env%auth_type == 'Basic')
    end function dm_cgi_is_authenticated

    function dm_cgi_key(query, loc) result(key)
        !! Returns key at index `loc` in keys array of `query`.
        type(cgi_query_type), intent(inout) :: query !! CGI query type.
        integer,              intent(in)    :: loc   !! Array index.
        character(len=:), allocatable       :: key   !! Key or empty.

        if ((query%size == 0) .or. (loc < 1) .or. (loc > query%size)) then
            allocate (character(len=0) :: key)
            return
        end if

        key = trim(query%keys(loc))
    end function dm_cgi_key

    integer function dm_cgi_size(query) result(sz)
        !! Returns the current number of elements in the given (opaque) CGI
        !! parameter type.
        type(cgi_query_type), intent(inout) :: query !! CGI query type.

        sz = query%size
    end function dm_cgi_size

    function dm_cgi_value(query, loc) result(value)
        !! Returns value at index `loc` in values array of `query`.
        type(cgi_query_type), intent(inout) :: query !! CGI query type.
        integer,              intent(in)    :: loc   !! Array index.
        character(len=:), allocatable       :: value !! Value or empty.

        if ((query%size == 0) .or. (loc < 1) .or. (loc > query%size)) then
            allocate (character(len=0) :: value)
            return
        end if

        value = trim(query%values(loc))
    end function dm_cgi_value

    subroutine dm_cgi_env(env)
        !! Reads CGI environment variables and writes values into `env`.
        type(cgi_env_type), intent(out) :: env !! CGI environment type.

        character(len=32) :: content_length, server_port
        integer           :: stat

        call get_environment_variable('AUTH_TYPE',             env%auth_type)
        call get_environment_variable('CONTENT_TYPE',          env%content_type)
        call get_environment_variable('DOCUMENT_ROOT',         env%document_root)
        call get_environment_variable('GATEWAY_INTERFACE',     env%gateway_interface)
        call get_environment_variable('HTTP_ACCEPT',           env%http_accept)
        call get_environment_variable('HTTP_CONTENT_ENCODING', env%http_content_encoding)
        call get_environment_variable('HTTP_COOKIE',           env%http_cookie)
        call get_environment_variable('HTTP_FROM',             env%http_from)
        call get_environment_variable('HTTP_REFERER',          env%http_referer)
        call get_environment_variable('HTTP_USER_AGENT',       env%http_user_agent)
        call get_environment_variable('PATH_INFO',             env%path_info)
        call get_environment_variable('PATH_TRANSLATED',       env%path_translated)
        call get_environment_variable('QUERY_STRING',          env%query_string)
        call get_environment_variable('REMOTE_ADDR',           env%remote_addr)
        call get_environment_variable('REMOTE_HOST',           env%remote_host)
        call get_environment_variable('REMOTE_IDENT',          env%remote_ident)
        call get_environment_variable('REMOTE_USER',           env%remote_user)
        call get_environment_variable('REQUEST_METHOD',        env%request_method)
        call get_environment_variable('REQUEST_URI',           env%request_uri)
        call get_environment_variable('SCRIPT_NAME',           env%script_name)
        call get_environment_variable('SERVER_NAME',           env%server_name)
        call get_environment_variable('SERVER_PROTOCOL',       env%server_protocol)
        call get_environment_variable('SERVER_SOFTWARE',       env%server_software)

        call get_environment_variable('CONTENT_LENGTH', content_length)
        read (content_length, *, iostat=stat) env%content_length

        call get_environment_variable('SERVER_PORT', server_port)
        read (server_port, *, iostat=stat) env%server_port
    end subroutine dm_cgi_env

    subroutine dm_cgi_form(env, query)
        !! Returns HTTP form data from standard input
        !! (`application/x-www-form-urlencoded`).
        type(cgi_env_type),   intent(inout) :: env   !! CGI environment type.
        type(cgi_query_type), intent(out)   :: query !! CGI query type.

        character(len=:), allocatable :: content
        integer                       :: rc

        if (env%content_type /= MIME_FORM) return
        if (env%content_length == 0) return

        rc = dm_cgi_content(env, content)
        if (dm_is_error(rc)) return

        call dm_cgi_parse(content, query)
    end subroutine dm_cgi_form

    subroutine dm_cgi_header(content_type, http_status, location)
        !! Writes HTTP header. A sane HTTP server converts the status code passed
        !! in the header to a real HTTP status code, as we cannot return it in any
        !! other way with CGI. Default HTTP status is 200.
        character(len=*), intent(in)           :: content_type !! MIME type.
        integer,          intent(in), optional :: http_status  !! HTTP status code.
        character(len=*), intent(in), optional :: location     !! Optional redirect.

        integer :: status ! HTTP code.

        status = HTTP_OK
        if (present(http_status)) status = http_status

        write (stdout, '("Content-Type: ", 2a)',   advance='no') content_type, CR_LF
        write (stdout, '("Status: ", i3, 1x, 2a)', advance='no') status, dm_http_status_string(status), CR_LF

        if (present(location)) then
            write (stdout, '("Location: ", 2a)', advance='no') location, CR_LF
        end if

        write (stdout, '(a)', advance='no') CR_LF
    end subroutine dm_cgi_header

    subroutine dm_cgi_parse(input, query)
        !! Decodes and parses given character string containing new-line
        !! separated key-values pairs, and returns CGI query parameters in `query`.
        character(len=*),     intent(in)  :: input !! Input string.
        type(cgi_query_type), intent(out) :: query !! CGI query type.

        character(len=CGI_PARAM_LEN) :: pair(2)
        character(len=CGI_PARAM_LEN) :: pairs(CGI_MAX_NPARAMS)
        character(len=len(input))    :: content
        integer                      :: i, j, n, rc

        if (len_trim(input) == 0) return

        rc = dm_cgi_decode(input, content)
        if (dm_is_error(rc)) return

        n = dm_string_count_char(content, '&') + 1
        n = min(n, CGI_MAX_NPARAMS)
        call dm_string_split(content, pairs, '&')

        do i = 1, n
            j = dm_string_count_char(pairs(i), '=')

            if (j > 0) then
                call dm_string_split(pairs(i), pair, '=')
                query%keys(i)   = pair(1)
                query%values(i) = pair(2)
            else
                query%keys(i)   = pairs(i)
                query%values(i) = ' '
            end if

            query%hashes(i) = dm_hash_fnv1a(trim(query%keys(i)))
            query%size      = query%size + 1
        end do
    end subroutine dm_cgi_parse

    subroutine dm_cgi_query(env, query)
        !! Returns CGI GET parameters from environment variable `QUERY_STRING`
        !! as key–value pairs in `query`.
        type(cgi_env_type),   intent(inout) :: env   !! CGI environment type.
        type(cgi_query_type), intent(out)   :: query !! CGI query type.

        call dm_cgi_parse(env%query_string, query)
    end subroutine dm_cgi_query

    subroutine dm_cgi_write(content)
        !! Prints content to standard output, returning it to the web server.
        character(len=*), intent(in) :: content !! Response content.

        write (stdout, '(a)') content
    end subroutine dm_cgi_write

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function cgi_get_int32(query, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `query` as 32-bit integer.
        !! The return code is set to `E_EMPTY` if the key does not exist and
        !! `required` has not been passed or is `.true.`
        type(cgi_query_type), intent(inout)        :: query    !! CGI query type.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        integer(kind=i4),     intent(out)          :: value    !! Parameter value.
        integer(kind=i4),     intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: loc

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        value = 0
        if (present(default)) value = default

        loc = cgi_query_loc(query, key)
        if (loc == 0) return
        if (len_trim(query%values(loc)) == 0) return
        call dm_string_to(query%values(loc), value, rc)
    end function cgi_get_int32

    integer function cgi_get_int64(query, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `query` as 64-bit
        !! integer. The return code is set to `E_EMPTY` if the key does not
        !! exist and `required` has not been passed or is `.true.`
        type(cgi_query_type), intent(inout)        :: query    !! CGI query parameters.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        integer(kind=i8),     intent(out)          :: value    !! Parameter value.
        integer(kind=i8),     intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: loc

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        value = 0
        if (present(default)) value = default

        loc = cgi_query_loc(query, key)
        if (loc == 0) return
        if (len_trim(query%values(loc)) == 0) return
        call dm_string_to(query%values(loc), value, rc)
    end function cgi_get_int64

    integer function cgi_get_logical(query, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `query` as logical. The
        !! return code is set to `E_EMPTY` if the key does not exist and
        !! `required` has not been passed or is `.true.`
        type(cgi_query_type), intent(inout)        :: query    !! CGI query type.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        logical,              intent(out)          :: value    !! Parameter value.
        logical,              intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: i, loc, stat

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        value = .false.
        if (present(default)) value = default

        loc = cgi_query_loc(query, key)
        if (loc == 0) return

        rc = E_TYPE
        call dm_string_to(query%values(loc), i, stat)
        if (dm_is_error(stat)) return
        value = (i /= 0)
        rc = E_NONE
    end function cgi_get_logical

    integer function cgi_get_real32(query, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `query` as 32-bit real.
        !! The return code is set to `E_EMPTY` if the key does not exist and
        !! `required` has not been passed or is `.true.`
        type(cgi_query_type), intent(inout)        :: query    !! CGI query type.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        real(kind=r4),        intent(out)          :: value    !! Parameter value.
        real(kind=r4),        intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: loc

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        value = 0.0
        if (present(default)) value = default

        loc = cgi_query_loc(query, key)
        if (loc == 0) return
        if (len_trim(query%values(loc)) == 0) return
        call dm_string_to(query%values(loc), value, rc)
    end function cgi_get_real32

    integer function cgi_get_real64(query, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `query` as 64-bit real.
        !! The return code is set to `E_EMPTY` if the key does not exist and
        !! `required` has not been passed or is `.true.`.
        type(cgi_query_type), intent(inout)        :: query    !! CGI query type.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        real(kind=r8),        intent(out)          :: value    !! Parameter value.
        real(kind=r8),        intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: loc

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        value = 0.0
        if (present(default)) value = default

        loc = cgi_query_loc(query, key)
        if (loc == 0) return
        if (len_trim(query%values(loc)) == 0) return
        call dm_string_to(query%values(loc), value, rc)
    end function cgi_get_real64

    integer function cgi_get_string(query, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `query`. The return code
        !! is set to `E_EMPTY` if the key does not exist and `required` has not
        !! been passed or is `.true.`
        type(cgi_query_type), intent(inout)        :: query    !! CGI query type.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        character(len=*),     intent(inout)        :: value    !! Parameter value.
        character(len=*),     intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: loc

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        get_block: block
            value = ''
            loc = cgi_query_loc(query, key)
            if (loc == 0) exit get_block
            if (len_trim(query%values(loc)) == 0) exit get_block
            value = trim(query%values(loc))
            rc = E_NONE
            return
        end block get_block

        if (present(default)) value = default
    end function cgi_get_string

    integer function cgi_query_loc(query, key) result(loc)
        !! Returns location of key in parameter keys array, or 0 if not found.
        type(cgi_query_type), intent(inout) :: query !! CGI query type.
        character(len=*),     intent(in)    :: key   !! Parameter key.

        integer(kind=i8) :: hash

        hash = dm_hash_fnv1a(trim(key))
        loc  = findloc(query%hashes, hash, dim=1)
    end function cgi_query_loc
end module dm_cgi
