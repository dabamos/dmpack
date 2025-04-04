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

        ! int FCGI_puts(const char *str)
        function fcgi_puts(str) bind(c, name='FCGI_puts')
            import :: c_char, c_int
            implicit none
            character(kind=c_char), intent(in) :: str
            integer(kind=c_int)                :: fcgi_puts
        end function fcgi_puts
    end interface

    public :: dm_fcgi_accept
    public :: dm_fcgi_content
    public :: dm_fcgi_header
    public :: dm_fcgi_out
contains
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

    integer function dm_fcgi_content(env, content) result(rc)
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

        fcgi_block: block
            rc = E_EMPTY
            if (n == 0) exit fcgi_block

            rc = E_BOUNDS
            if (n < 0) exit fcgi_block

            rc = E_ALLOC
            allocate (character(len=n) :: content, stat=stat)
            if (stat /= 0) exit fcgi_block

            rc = E_NONE
            do i = 1, n
                content(i:i) = achar(fcgi_getchar())
            end do

            return
        end block fcgi_block

        if (.not. allocated(content)) allocate (character(len=0) :: content)
    end function dm_fcgi_content

    subroutine dm_fcgi_header(content_type, http_status)
        !! Writes HTTP header. A sane HTTP server converts the status code in
        !! the header to a real HTTP status code, as we cannot return it in any
        !! other way with FastCGI. The default HTTP status code is 200.
        use :: dm_ascii, only: CR_LF
        use :: dm_http,  only: HTTP_OK, dm_http_status_string
        use :: dm_util,  only: dm_itoa

        character(len=*), intent(in)           :: content_type !! MIME type.
        integer,          intent(in), optional :: http_status  !! HTTP status code.

        integer :: code, stat

        code = HTTP_OK
        if (present(http_status)) code = http_status

        stat = fcgi_puts('Content-Type: ' // trim(content_type) // CR_LF // &
                         'Status: ' // dm_itoa(code) // ' ' // dm_http_status_string(code) // CR_LF // &
                         c_null_char)
    end subroutine dm_fcgi_header

    subroutine dm_fcgi_out(content)
        !! Writes given content as response. The argument will be
        !! null-terminated.
        character(len=*), intent(in) :: content !! Response content.

        integer :: stat

        stat = fcgi_puts(content // c_null_char)
    end subroutine dm_fcgi_out
end module dm_fcgi
