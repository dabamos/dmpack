! Author:  Philipp Engel
! Licence: ISC
module dm_fcgi
    !! FastCGI interface bindings and procedures.
    use, intrinsic :: iso_c_binding
    use :: dm_ascii
    use :: dm_cgi
    use :: dm_error
    use :: dm_kind
    use :: dm_http
    use :: dm_util
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
    integer function dm_fcgi_accept() result(rc)
        !! Accepts new FastCGI connection (blocking). The function returns
        !! `E_FCGI` on error.
        rc = E_FCGI
        if (fcgi_accept() >= 0) rc = E_NONE
    end function dm_fcgi_accept

    integer function dm_fcgi_content(env, content) result(rc)
        !! Reads HTTP request body (POST method). The the content length is 0,
        !! the argument `content` will be allocated but empty on output.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_BOUNDS` if content length is negative.
        !!
        type(cgi_env_type),            intent(inout) :: env     !! CGI environment.
        character(len=:), allocatable, intent(out)   :: content !! Returned request body.

        integer          :: stat
        integer(kind=i8) :: i

        rc = E_BOUNDS
        if (env%content_length < 0) return

        rc = E_ALLOC
        allocate (character(len=env%content_length) :: content, stat=stat)
        if (stat /= 0) return

        rc = E_NONE
        if (env%content_length < 0) return

        do i = 1, env%content_length
            content(i:i) = achar(fcgi_getchar())
        end do
    end function dm_fcgi_content

    subroutine dm_fcgi_header(content_type, http_status)
        !! Writes HTTP header. A sane HTTP server converts the status code in
        !! the header to a real HTTP status code, as we cannot return it in any
        !! other way with FastCGI. Default HTTP status code is 200.
        character(len=*), intent(in)           :: content_type !! MIME type.
        integer,          intent(in), optional :: http_status  !! HTTP status code.
        integer                                :: code, stat

        code = 200
        if (present(http_status)) code = http_status

        stat = fcgi_puts('Content-Type: ' // trim(content_type) // CR_LF // &
                         'Status: ' // dm_itoa(code) // ' ' // dm_http_status_string(code) // CR_LF // &
                         c_null_char)
    end subroutine dm_fcgi_header

    subroutine dm_fcgi_out(content)
        !! Writes given content as response.
        character(len=*), intent(in) :: content !! Response content.
        integer                      :: stat

        stat = fcgi_puts(content // c_null_char)
    end subroutine dm_fcgi_out
end module dm_fcgi
