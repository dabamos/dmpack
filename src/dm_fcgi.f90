! Author:  Philipp Engel
! Licence: ISC
module dm_fcgi
    !! FastCGI interface bindings and procedures.
    use, intrinsic :: iso_c_binding
    use :: dm_cgi
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    interface
        ! int FCGI_Accept(void)
        function fcgi_accept() bind(c, name='FCGI_Accept')
            import :: c_int
            implicit none
            integer(kind=c_int) :: fcgi_accept
        end function fcgi_accept

        ! int FCGI_getchar(void)
        function fcgi_getchar() bind(c, name='FCGI_getchar')
            import :: c_int
            implicit none
            integer(kind=c_int) :: fcgi_getchar
        end function fcgi_getchar

        ! int FCGI_putchar(int c)
        function fcgi_putchar(c) bind(c, name='FCGI_putchar')
            import :: c_int
            implicit none
            integer(kind=c_int), intent(in), value :: c
            integer(kind=c_int)                    :: fcgi_putchar
        end function fcgi_putchar

        ! int FCGI_puts(const char *str)
        function fcgi_puts(str) bind(c, name='FCGI_puts')
            import :: c_char, c_int
            implicit none
            character(kind=c_char), intent(in) :: str
            integer(kind=c_int)                :: fcgi_puts
        end function fcgi_puts
    end interface

    public :: dm_fcgi_accept
    public :: dm_fcgi_header
    public :: dm_fcgi_read
    public :: dm_fcgi_read_to_file
    public :: dm_fcgi_write
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    logical function dm_fcgi_accept() result(accept)
        !! Accepts new FastCGI connection (blocking). The function returns
        !! `.false.` on error.
        !!
        !! The function accepts a new request from the HTTP server and creates a
        !! CGI-compatible execution environment for the request.
        !!
        !! If the application was invoked as a CGI program, the first call to
        !! `dm_fcgi_accept()` is essentially a no-op and the second call returns
        !! `.false.`. This causes a correctly coded FastCGI Responder
        !! application to run a single request and exit, giving CGI behaviour.
        !!
        !! If the application was invoked as a FastCGI server, the first call to
        !! the function indicates that the application has completed its
        !! initialisation and is ready to accept its first request. Subsequent
        !! calls indicate that the application has completed processing its
        !! current request and is ready to accept a new request.
        !!
        !! In completing the current request, the called FastCGI function may
        !! detect errors, e.g. a broken pipe to a client who has disconnected
        !! early. The API function ignores such errors.
        accept = (fcgi_accept() == 0)
    end function dm_fcgi_accept

    integer function dm_fcgi_read(env, content) result(rc)
        !! Reads HTTP request body (POST method). The the content length is 0,
        !! the argument `content` will be allocated but empty on output.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_BOUNDS` if content length is negative.
        !! * `E_EMPTY` if content length is zero.
        !!
        type(cgi_env_type),            intent(inout) :: env     !! CGI environment.
        character(len=:), allocatable, intent(out)   :: content !! Returned request body.

        integer          :: stat
        integer(kind=i8) :: i, n

        n = env%content_length

        io_block: block
            rc = E_EMPTY
            if (n == 0) exit io_block

            rc = E_BOUNDS
            if (n < 0) exit io_block

            rc = E_ALLOC
            allocate (character(len=n) :: content, stat=stat)
            if (stat /= 0) exit io_block

            rc = E_NONE
            do i = 1, n
                content(i:i) = char(fcgi_getchar())
            end do

            return
        end block io_block

        if (.not. allocated(content)) allocate (character(len=0) :: content)
    end function dm_fcgi_read

    integer function dm_fcgi_read_to_file(env, path) result(rc)
        !! Reads HTTP request body and writes content to file. No file will be
        !! created if the content length is 0. An existing file will be
        !! replaced.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if content length is negative.
        !! * `E_EMPTY` if content length is zero.
        !! * `E_IO` if file could not be opened.
        !! * `E_WRITE` if writing to file failed.
        !!
        type(cgi_env_type), intent(inout) :: env  !! CGI environment.
        character(len=*),   intent(in)    :: path !! Path of output file.

        integer          :: stat, unit
        integer(kind=i8) :: i, n

        n = env%content_length

        rc = E_EMPTY
        if (n == 0) return

        rc = E_BOUNDS
        if (n < 0) return

        io_block: block
            rc = E_IO
            open (access  = 'stream',      &
                  action  = 'write',       &
                  file    = trim(path),    &
                  form    = 'unformatted', &
                  iostat  = stat,          &
                  newunit = unit,          &
                  status  = 'replace')
            if (stat /= 0) exit io_block

            rc = E_WRITE
            do i = 1, n
                write (unit, iostat=stat) char(fcgi_getchar())
                if (stat /= 0) exit io_block
            end do

            rc = E_NONE
        end block io_block

        close (unit)
    end function dm_fcgi_read_to_file

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_fcgi_header(content_type, http_status, http_headers)
        !! Writes HTTP header. A sane HTTP server converts the status code in
        !! the header to a real HTTP status code, as we cannot return it in any
        !! other way with FastCGI. The default HTTP status code is 200.
        !!
        !! Argument `http_headers` have to contain additional HTTP headers as
        !! key–value pairs.
        use :: dm_ascii, only: CR_LF
        use :: dm_http,  only: HTTP_OK, dm_http_status_string
        use :: dm_util,  only: dm_itoa

        character(len=*), intent(in)              :: content_type    !! MIME type.
        integer,          intent(in),    optional :: http_status     !! HTTP status code.
        character(len=*), intent(inout), optional :: http_headers(:) !! Additional HTTP headers.

        character(len=:), allocatable :: header
        integer                       :: code, i, n, stat

        code = HTTP_OK
        if (present(http_status)) code = http_status

        n = 0
        if (present(http_headers)) n = size(http_headers)

        header = 'Content-Type: ' // trim(content_type) // CR_LF // &
                 'Status: '       // dm_itoa(code) // ' ' // dm_http_status_string(code) // CR_LF

        do i = 1, n, 2
            if (len_trim(http_headers(i)) == 0) cycle
            header = header // trim(http_headers(i)) // ': ' // trim(http_headers(i + 1)) // CR_LF
        end do

        stat = fcgi_puts(header // c_null_char)
    end subroutine dm_fcgi_header

    subroutine dm_fcgi_write(string)
        !! Writes given string as response.
        character(len=*), intent(in) :: string !! Response content.

        integer :: i, n

        do i = 1, len(string)
            n = fcgi_putchar(ichar(string(i:i)))
        end do
    end subroutine dm_fcgi_write
end module dm_fcgi
