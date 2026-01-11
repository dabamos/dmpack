! http_client.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! This is a very simple HTTP client based on the NNG demo program
    !! `http_client.c`. It only performs HTTP GET operations, and does not
    !! follow HTTP redirects. Think of it as a trivialized version of cURL. It
    !! is super simple, taking the URL on the command line, and emitting the
    !! results to stdout. For clarity, we are eliding TLS support.
    !!
    !! It may not work on all systems, but it should work anywhere that both the
    !! standard C library and NNG itself are available.
    !!
    !! Unfortunately many famous sites use redirects, so you won't see that
    !! emitted.
    !!
    !! Example usage:
    !!
    !! ```
    !! ./http_client http://httpbin.org/ip
    !! ```
    use :: nng
    use :: nng_http
    use :: nng_util
    implicit none (type, external)

    character(80) :: arg
    integer       :: rc
    type(c_ptr)   :: aio, client, conn, url
    type(c_ptr)   :: req, res

    aio    = c_null_ptr
    client = c_null_ptr
    conn   = c_null_ptr
    url    = c_null_ptr
    req    = c_null_ptr
    res    = c_null_ptr

    if (command_argument_count() < 1) then
        print '("Usage: http_client <URL>")'
        stop
    end if

    call get_command_argument(1, arg)

    http_block: block
        ! Allocate data structures.
        rc = nng_url_parse(url, f_c_str(arg));              if (rc /= 0) exit http_block
        rc = nng_http_client_alloc(client, url);            if (rc /= 0) exit http_block
        rc = nng_http_req_alloc(req, url);                  if (rc /= 0) exit http_block
        rc = nng_http_res_alloc(res);                       if (rc /= 0) exit http_block
        rc = nng_aio_alloc(aio, c_null_funptr, c_null_ptr); if (rc /= 0) exit http_block

        ! Start connection process and wait for it to finish.
        call nng_http_client_connect(client, aio)
        call nng_aio_wait(aio)
        rc = nng_aio_result(aio)
        if (rc /= 0) exit http_block

        ! Get the connection, at the 0th output.
        conn = nng_aio_get_output(aio, 0)

        ! Request is already set up with URL, and for GET via HTTP/1.1.
        ! The Host: header is already set up, too.

        ! Send the request, and wait for that to finish.
        call nng_http_conn_write_req(conn, req, aio)
        call nng_aio_wait(aio)
        rc = nng_aio_result(aio)
        if (rc /= 0) exit http_block

        block
            character(:), allocatable         :: header
            character(:), allocatable, target :: data

            integer           :: stat
            integer(c_size_t) :: n
            type(nng_iov)     :: iov(1)

            ! Read the response.
            call nng_http_conn_read_res(conn, res, aio)
            call nng_aio_wait(aio)
            rc = nng_aio_result(aio)
            if (rc /= 0) exit http_block

            if (nng_http_res_get_status(res) /= NNG_HTTP_STATUS_OK) then
                print '("HTTP server responded: ", i0, 1x, a)', nng_http_res_get_status(res), &
                                                                nng_http_res_get_reason(res)
            end if

            ! Get payload size from response header.
            header = nng_http_res_get_header(res, f_c_str('Content-Length'))
            read (header, *, iostat=stat) n

            if (len(header) == 0 .or. stat /= 0) then
                print '("Response is missing Content-Length header")'
                exit http_block
            end if

            if (n == 0) exit http_block

            ! Allocate a buffer to receive the body data.
            allocate (character(n) :: data)

            ! Set up a single iov to point to the buffer.
            iov(1) = nng_iov(c_loc(data), n)

            ! Following never fails with fewer than 5 elements.
            rc = nng_aio_set_iov(aio, size(iov), iov)

            ! Now attempt to receive the data and wait for it to complete.
            call nng_http_conn_read_all(conn, aio)
            call nng_aio_wait(aio)
            rc = nng_aio_result(aio)
            if (rc /= 0) exit http_block

            ! Output payload.
            write (*, '(a)', advance='no') data
        end block
    end block http_block

    if (rc /= 0) print '("Error: ", a)', nng_strerror(rc)

    call nng_aio_free(aio)
    call nng_http_res_free(res)
    call nng_http_req_free(req)
    call nng_http_client_free(client)
    call nng_url_free(url)
    call nng_fini()
end program main
