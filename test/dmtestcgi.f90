! dmtestcgi.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestcgi
    !! Tests CGI procedures.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestcgi.test01', test01)
    tests(2) = test_type('dmtestcgi.test02', test02)
    tests(3) = test_type('dmtestcgi.test03', test03)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
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
            call dm_perror(rc)
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
        call dm_perror(rc)
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
        type(cgi_param_type) :: param

        stat = TEST_FAILED

        call dm_cgi_env(env)
        call dm_cgi_query(env, param)

        do i = 1, dm_cgi_size(param)
            print *, dm_cgi_key(param, i), ': ', dm_cgi_value(param, i)
        end do

        rc = dm_cgi_get(param, 'dummy', dummy)
        print *, 'dummy: ', trim(dummy)

        stat = TEST_PASSED
    end function test03
end program dmtestcgi
