! Author:  Philipp Engel
! Licence: ISC
module dm_cgi
    !! Common Gateway Interface (CGI) utility procedures.
    use :: dm_ascii
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

    integer, parameter, public :: CGI_MAX_PARAMS = 32  !! Maximum number of CGI parameters.
    integer, parameter, public :: CGI_PARAM_LEN  = 512 !! Maximum length of CGI parameter key, value.

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

    type, public :: cgi_param_type
        !! Opaque CGI parameter type. Stores GET and POST parameters as
        !! key-value pairs.
        private
        character(len=CGI_PARAM_LEN) :: keys(CGI_MAX_PARAMS)   = ' '  !! Array of keys.
        character(len=CGI_PARAM_LEN) :: values(CGI_MAX_PARAMS) = ' '  !! Array of values.
        integer(kind=i8)             :: hashes(CGI_MAX_PARAMS) = 0_i8 !! Array of hashes.
        integer                      :: cursor                 = 0    !! Number of elements.
    end type cgi_param_type

    interface dm_cgi_get
        !! Generic interface to CGI get functions.
        module procedure :: cgi_get_int32
        module procedure :: cgi_get_int64
        module procedure :: cgi_get_logical
        module procedure :: cgi_get_real32
        module procedure :: cgi_get_real64
        module procedure :: cgi_get_string
    end interface

    ! Public procedures.
    public :: dm_cgi_auth
    public :: dm_cgi_content
    public :: dm_cgi_decode
    public :: dm_cgi_env
    public :: dm_cgi_form
    public :: dm_cgi_get
    public :: dm_cgi_has
    public :: dm_cgi_has_value
    public :: dm_cgi_header
    public :: dm_cgi_key
    public :: dm_cgi_out
    public :: dm_cgi_parse
    public :: dm_cgi_query
    public :: dm_cgi_size
    public :: dm_cgi_value

    ! Private procedures.
    private :: cgi_get_int32
    private :: cgi_get_int64
    private :: cgi_get_logical
    private :: cgi_get_real32
    private :: cgi_get_real64
    private :: cgi_get_string
    private :: cgi_param_loc
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    logical function dm_cgi_auth(env) result(auth)
        !! Returns `.true.` is CGI environment variable `AUTH` is set.
        type(cgi_env_type), intent(inout) :: env !! CGI environment type.

        auth = .false.
        if (len_trim(env%auth_type) > 0) auth = .true.
    end function dm_cgi_auth

    integer function dm_cgi_content(env, content) result(rc)
        !! Reads HTTP request body (POST method). We have to rely on _read(2)_
        !! as Fortran cannot read unformatted content from standard input.
        use :: unix

        type(cgi_env_type),            intent(inout) :: env     !! CGI environment type.
        character(len=:), allocatable, intent(out)   :: content !! Returned request body.

        character, target      :: buf
        integer                :: stat
        integer(kind=c_size_t) :: sz
        integer(kind=i8)       :: i

        rc = E_NONE
        if (env%content_length <= 0) return

        rc = E_ALLOC
        allocate (character(len=env%content_length) :: content, stat=stat)
        if (stat /= 0) return

        do i = 1, env%content_length
            rc = E_IO
            sz = c_read(STDIN_FILENO, c_loc(buf), 1_c_size_t)
            if (sz < 1) exit
            content(i:i) = buf
            rc = E_NONE
        end do
    end function dm_cgi_content

    integer function dm_cgi_decode(input, output) result(rc)
        !! Unwinds percent-encoding in given input string.
        character(len=*),          intent(in)  :: input  !! Encoded input string.
        character(len=len(input)), intent(out) :: output !! Decoded output string.

        integer :: i, j, k, n, m
        integer :: stat

        rc = E_BOUNDS
        n = len_trim(input)
        m = len(output)
        output = ' '

        if (m < n) return

        i = 1
        j = 1

        do
            if (i > n) exit
            if (j > m) return

            select case (input(i:i))
                case ('%')
                    if (i + 2 > n) exit
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

    function dm_cgi_has(param, key) result(has)
        !! Returns whether key exists in `param`.
        type(cgi_param_type), intent(inout) :: param !! CGI parameter type.
        character(len=*),     intent(in)    :: key   !! Parameter key.

        logical          :: has
        integer          :: i
        integer(kind=i8) :: hash

        has = .false.
        hash = dm_hash_fnv1a(trim(key))
        i = findloc(param%hashes, hash, dim=1)
        if (i == 0) return
        has = .true.
    end function dm_cgi_has

    function dm_cgi_has_value(param, key) result(has)
        !! Returns whether key exists in `param` and has value.
        type(cgi_param_type), intent(inout) :: param !! CGI parameter type.
        character(len=*),     intent(in)    :: key   !! Parameter key.

        logical          :: has
        integer          :: i
        integer(kind=i8) :: hash

        has = .false.
        hash = dm_hash_fnv1a(trim(key))
        i = findloc(param%hashes, hash, dim=1)
        if (i == 0) return
        if (len_trim(param%values(i)) == 0) return
        has = .true.
    end function dm_cgi_has_value

    function dm_cgi_key(param, i) result(str)
        !! Returns key at index `i` in keys array of `param`.
        type(cgi_param_type), intent(inout) :: param !! CGI parameter type.
        integer,              intent(in)    :: i     !! Array index.
        character(len=:), allocatable       :: str   !! Key or empty.

        if ((param%cursor == 0) .or. (i < 1) .or. (i > param%cursor)) then
            str = ''
            return
        end if

        str = trim(param%keys(i))
    end function dm_cgi_key

    function dm_cgi_size(param) result(sz)
        !! Returns the current number of elements in the given (opaque) CGI
        !! parameter type.
        type(cgi_param_type), intent(inout) :: param !! CGI parameter type.
        integer                             :: sz    !! Number of parameters.

        sz = param%cursor
    end function dm_cgi_size

    function dm_cgi_value(param, i) result(str)
        !! Returns value at index `i` in values array of `param`.
        type(cgi_param_type), intent(inout) :: param !! CGI parameter type.
        integer,              intent(in)    :: i     !! Array index.
        character(len=:), allocatable       :: str   !! Value or empty.

        if ((param%cursor == 0) .or. (i < 1) .or. (i > param%cursor)) then
            str = ''
            return
        end if

        str = trim(param%values(i))
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

    subroutine dm_cgi_form(env, param)
        !! Returns HTTP form data from standard input
        !! (`application/x-www-form-urlencoded`).
        type(cgi_env_type),   intent(inout) :: env   !! CGI environment type.
        type(cgi_param_type), intent(out)   :: param !! CGI parameter type.

        character(len=:), allocatable :: content

        if (env%content_type /= MIME_FORM) return
        if (dm_cgi_content(env, content) /= E_NONE) return
        call dm_cgi_parse(content, param)
        if (allocated(content)) deallocate (content)
    end subroutine dm_cgi_form

    subroutine dm_cgi_header(content_type, http_status, location)
        !! Writes HTTP header. A sane HTTP server converts the status code passed
        !! in the header to a real HTTP status code, as we cannot return it in any
        !! other way with CGI. Default HTTP status is 200.
        character(len=*), intent(in)           :: content_type !! MIME type.
        integer,          intent(in), optional :: http_status  !! HTTP status code.
        character(len=*), intent(in), optional :: location     !! Optional redirect.

        integer :: code ! HTTP code.

        code = HTTP_OK
        if (present(http_status)) code = http_status

        write (stdout, '("Content-Type: ", 2a)',   advance='no') content_type, CR_LF
        write (stdout, '("Status: ", i3, 1x, 2a)', advance='no') code, dm_http_status_string(code), CR_LF

        if (present(location)) then
            write (stdout, '("Location: ", 2a)', advance='no') location, CR_LF
        end if

        write (stdout, '(a)', advance='no') CR_LF
    end subroutine dm_cgi_header

    subroutine dm_cgi_out(content)
        !! Prints content to standard output, returning it to the web server.
        character(len=*), intent(in) :: content !! Response content.

        write (stdout, '(a)') content
    end subroutine dm_cgi_out

    subroutine dm_cgi_parse(str, param)
        !! Decodes and parses given character string containing new-line
        !! separated key-values pairs, and returns CGI parameters in `param`.
        character(len=*),     intent(in)  :: str   !! Input string.
        type(cgi_param_type), intent(out) :: param !! CGI parameter type.

        character(len=CGI_PARAM_LEN) :: pair(2)
        character(len=CGI_PARAM_LEN) :: pairs(CGI_MAX_PARAMS)
        character(len=len(str))      :: content
        integer                      :: i, j, n

        if (len_trim(str) == 0) return
        if (dm_cgi_decode(str, content) /= E_NONE) return

        n = dm_string_count_char(content, '&') + 1
        n = min(n, CGI_MAX_PARAMS)
        call dm_string_split(content, pairs, '&')

        do i = 1, n
            j = dm_string_count_char(pairs(i), '=')

            if (j > 0) then
                call dm_string_split(pairs(i), pair, '=')
                param%keys(i)   = pair(1)
                param%values(i) = pair(2)
            else
                param%keys(i)   = pairs(i)
                param%values(i) = ' '
            end if

            param%hashes(i) = dm_hash_fnv1a(trim(param%keys(i)))
            param%cursor = param%cursor + 1
        end do
    end subroutine dm_cgi_parse

    subroutine dm_cgi_query(env, param)
        !! Returns CGI GET parameters from environment variable `QUERY_STRING`
        !! as key–value pairs in `param`.
        type(cgi_env_type),   intent(inout) :: env   !! CGI environment type.
        type(cgi_param_type), intent(out)   :: param !! CGI parameter type.

        call dm_cgi_parse(env%query_string, param)
    end subroutine dm_cgi_query

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function cgi_get_int32(param, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `param` as 32-bit integer.
        !! The return code is set to `E_EMPTY` if the key does not exist and
        !! `required` has not been not passed or is `.true.`
        type(cgi_param_type), intent(inout)        :: param    !! CGI parameter type.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        integer(kind=i4),     intent(out)          :: value    !! Parameter value.
        integer(kind=i4),     intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: i

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        value = 0
        if (present(default)) value = default

        i = cgi_param_loc(param, key)
        if (i == 0) return
        if (len_trim(param%values(i)) == 0) return
        call dm_string_to(param%values(i), value, rc)
    end function cgi_get_int32

    integer function cgi_get_int64(param, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `param` as 64-bit
        !! integer. The return code is set to `E_EMPTY` if the key does not
        !! exist and `required` has not been not passed or is `.true.`
        type(cgi_param_type), intent(inout)        :: param    !! CGI parameters.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        integer(kind=i8),     intent(out)          :: value    !! Parameter value.
        integer(kind=i8),     intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: i

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        value = 0
        if (present(default)) value = default

        i = cgi_param_loc(param, key)
        if (i == 0) return
        if (len_trim(param%values(i)) == 0) return
        call dm_string_to(param%values(i), value, rc)
    end function cgi_get_int64

    integer function cgi_get_logical(param, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `param` as logical. The
        !! return code is set to `E_EMPTY` if the key does not exist and
        !! `required` has not been not passed or is `.true.`
        type(cgi_param_type), intent(inout)        :: param    !! CGI parameter type.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        logical,              intent(out)          :: value    !! Parameter value.
        logical,              intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: i, j, stat

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        value = .false.
        if (present(default)) value = default

        i = cgi_param_loc(param, key)
        if (i == 0) return

        rc = E_TYPE
        call dm_string_to(param%values(i), j, stat)
        if (stat /= E_NONE) return
        value = .not. (j == 0)
        rc = E_NONE
    end function cgi_get_logical

    integer function cgi_get_real32(param, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `param` as 32-bit real.
        !! The return code is set to `E_EMPTY` if the key does not exist and
        !! `required` has not been not passed or is `.true.`
        type(cgi_param_type), intent(inout)        :: param    !! CGI parameter type.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        real(kind=r4),        intent(out)          :: value    !! Parameter value.
        real(kind=r4),        intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: i

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        value = 0.0
        if (present(default)) value = default

        i = cgi_param_loc(param, key)
        if (i == 0) return
        if (len_trim(param%values(i)) == 0) return
        call dm_string_to(param%values(i), value, rc)
    end function cgi_get_real32

    integer function cgi_get_real64(param, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `param` as 64-bit real.
        !! The return code is set to `E_EMPTY` if the key does not exist and
        !! `required` has not been not passed or is `.true.`.
        type(cgi_param_type), intent(inout)        :: param    !! CGI parameter type.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        real(kind=r8),        intent(out)          :: value    !! Parameter value.
        real(kind=r8),        intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: i

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        value = 0.0
        if (present(default)) value = default

        i = cgi_param_loc(param, key)
        if (i == 0) return
        if (len_trim(param%values(i)) == 0) return
        call dm_string_to(param%values(i), value, rc)
    end function cgi_get_real64

    integer function cgi_get_string(param, key, value, default, required) result(rc)
        !! Returns (last) value associated with key in `param`. The return code
        !! is set to `E_EMPTY` if the key does not exist and `required` has not
        !! been not passed or is `.true.`
        type(cgi_param_type), intent(inout)        :: param    !! CGI parameter type.
        character(len=*),     intent(in)           :: key      !! Parameter key.
        character(len=*),     intent(inout)        :: value    !! Parameter value.
        character(len=*),     intent(in), optional :: default  !! Default value.
        logical,              intent(in), optional :: required !! Required flag.

        integer :: i

        rc = E_EMPTY
        if (present(required)) then
            if (.not. required) rc = E_NONE
        end if

        get_block: block
            value = ''
            i = cgi_param_loc(param, key)
            if (i == 0) exit get_block
            if (len_trim(param%values(i)) == 0) exit get_block
            value = trim(param%values(i))
            rc = E_NONE
            return
        end block get_block

        if (present(default)) value = default
    end function cgi_get_string

    integer function cgi_param_loc(param, key) result(i)
        !! Returns location of key in parameter keys array, or 0 if not found.
        type(cgi_param_type), intent(inout) :: param !! CGI parameter type.
        character(len=*),     intent(in)    :: key   !! Parameter key.

        integer(kind=i8) :: hash

        hash = dm_hash_fnv1a(trim(key))
        i = findloc(param%hashes, hash, dim=1, back=.true.)
    end function cgi_param_loc
end module dm_cgi
