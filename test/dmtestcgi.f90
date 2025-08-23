! dmtestcgi.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestcgi
    !! Tests CGI and CGI router procedures.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestcgi'
    integer,          parameter :: NTESTS    = 4

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        !! Decodes encoded string.
        character(len=*), parameter :: URL = 'https://www.example.com/api/v1/test/?p=1&amp;x="test"'

        character(len=128) :: input, output
        integer            :: rc

        stat = TEST_FAILED

        input  = 'https%3A%2F%2Fwww.example.com%2Fapi%2Fv1%2Ftest%2F%3Fp%3D1%26amp%3Bx%3D%22test%22'
        output = ' '

        rc = dm_cgi_decode(input, output)
        print *, input
        print *, output

        if (dm_is_error(rc)) then
            call dm_error_out(rc)
            return
        end if

        if (output /= URL) then
            print *, URL
            print *, trim(output)
        end if

        print *, 'strings match'

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! Reads HTTP POST data.
        character(len=:), allocatable :: content
        integer                       :: rc
        type(cgi_env_type)            :: env

        stat = TEST_FAILED

        call dm_cgi_env(env)
        rc = dm_cgi_content(env, content)
        call dm_error_out(rc)
        print *, 'Content length: ', env%content_length
        call dm_cgi_header(MIME_HTML, HTTP_OK)

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        !! Reads `QUERY_STRING` parameters as key-value pairs.
        !!
        !! Set the environment variable first:
        !! ```
        !! $ export QUERY_STRING="dummy=fortran"
        !! $ ./dmtestcgi
        !! ```
        character(len=32)    :: dummy
        integer              :: i, rc
        type(cgi_env_type)   :: env
        type(cgi_query_type) :: query

        stat = TEST_FAILED

        call dm_cgi_env(env)
        call dm_cgi_query(env, query)

        do i = 1, dm_cgi_size(query)
            print *, dm_cgi_key(query, i), ': ', dm_cgi_value(query, i)
        end do

        rc = dm_cgi_get(query, 'dummy', dummy)
        print *, 'dummy: ', trim(dummy)

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        !! Tests CGI router.
        integer                       :: rc
        type(cgi_env_type)            :: env
        type(cgi_router_type)         :: router
        type(cgi_route_type), target  :: routes(2)
        type(cgi_route_type), pointer :: ptr

        stat = TEST_FAILED

        print *, 'Creating router ...'
        rc = dm_cgi_router_create(router, 32)
        if (dm_is_error(rc)) return

        print *, 'Creating routes ...'
        routes(1)%path = '/'
        routes(1)%callback => default_resource

        routes(2)%path = '/index'
        routes(2)%callback => index_resource

        print *, 'Adding routes to router ...'
        rc = dm_cgi_router_add(router, routes(1))
        if (dm_is_error(rc)) return

        rc = dm_cgi_router_add(router, routes(2))
        if (dm_is_error(rc)) return

        print *, 'Retrieving routes ...'
        ptr => null()
        rc = dm_cgi_router_get(router, '/', ptr)
        if (dm_is_error(rc)) return
        if (.not. associated(ptr)) return
        if (.not. allocated(ptr%path)) return
        if (ptr%path /= '/') return
        call ptr%callback(env)

        ptr => null()
        rc = dm_cgi_router_get(router, '/index', ptr)
        if (dm_is_error(rc)) return
        if (.not. associated(ptr)) return
        if (.not. allocated(ptr%path)) return
        if (ptr%path /= '/index') return
        call ptr%callback(env)

        print *, 'Testing invalid route ...'
        ptr => null()
        rc = dm_cgi_router_get(router, '/invalid', ptr)
        if (rc == E_NONE) return
        if (associated(ptr)) return

        print *, 'Filtering request URI ...'
        env%path_info = '/index'
        call dm_cgi_router_dispatch(router, env, rc)
        if (rc /= HTTP_OK) return

        print *, 'Filtering invalid request URI ...'
        env%path_info = '/invalid'
        call dm_cgi_router_dispatch(router, env, rc)
        if (rc == HTTP_OK) return

        stat = TEST_PASSED
    end function test04

    subroutine default_resource(env)
        type(cgi_env_type), intent(inout) :: env

        print *, '-- default_resource'
    end subroutine default_resource

    subroutine index_resource(env)
        type(cgi_env_type), intent(inout) :: env

        print *, '-- index resource'
    end subroutine index_resource 
end program dmtestcgi
