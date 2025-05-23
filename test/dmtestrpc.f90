! dmtestrpc.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestrpc
    !! Test program for the HTTP-based RPC-API of DMPACK.
    !!
    !! To run this program, set the following environment variables
    !! beforehand:
    !!
    !!      DM_API_HOST     - IP or FQDN of the RPC-API server.
    !!      DM_API_USERNAME - User name.
    !!      DM_API_PASSWORD - Password.
    !!
    !! All tests will be skipped if these are not set.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestrpc'
    integer,          parameter :: NTESTS    = 5

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    no_color = dm_env_has('NO_COLOR')

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04), &
        test_type('test05', test05)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function get_env(host, username, password) result(has)
        character(len=:), allocatable, intent(out) :: host
        character(len=:), allocatable, intent(out) :: username
        character(len=:), allocatable, intent(out) :: password

        integer :: rcs(3)

        has = .false.

        rcs(1) = dm_env_get('DM_API_HOST',     host)
        rcs(2) = dm_env_get('DM_API_USERNAME', username)
        rcs(3) = dm_env_get('DM_API_PASSWORD', password)

        if (any(dm_is_error(rcs))) then
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("> Set environment vars DM_RPC_HOST, DM_RPC_USERNAME, DM_API_PASSWORD")'
            print '("> of the DMPACK RPC API. This test will be skipped.")'
            call dm_ansi_reset(no_color)
            return
        end if

        has = .true.
    end function get_env

    logical function test01() result(stat)
        character(len=*), parameter :: URL1 = 'http://example.com:8080/api/v1'
        character(len=*), parameter :: URL2 = 'https://example.com/api/v1/observ'

        character(len=:), allocatable :: url

        stat = TEST_FAILED

        print '(" Libraries: ", a)', dm_rpc_version()
        print *, 'Validating URLs ...'
        url = dm_rpc_url('example.com', 8080, '/api/v1')
        print *, url
        if (url /= URL1) return

        url = dm_rpc_url('example.com', base='/api/v1', endpoint='/observ', tls=.true.)
        print *, url
        if (url /= URL2) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! This test sends multiple observations via HTTP POST request to
        !! an RPC-API.
        integer, parameter :: NOBSERVS = 100

        integer                        :: i, rc
        real(kind=r8)                  :: dt
        type(rpc_request_type)         :: request
        type(rpc_response_type)        :: response
        type(timer_type)               :: timer
        type(observ_type), allocatable :: observs(:)
        character(len=:),  allocatable :: host, username, password, url

        stat = TEST_PASSED
        if (.not. get_env(host, username, password)) return

        stat = TEST_FAILED
        rc = dm_rpc_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        url = dm_rpc_url(host)
        print *, 'Sending raw HTTP GET request to "' // url // '" ...'
        rc = dm_rpc_request(request  = request, &
                            response = response, &
                            username = username, &
                            password = password, &
                            method   = RPC_METHOD_GET, &
                            url      = url)

        if (response%code == HTTP_OK) then
            print *, 'Response content:'
            print '(a)', response%payload
        else
            print '("HTTP code...: ", i0)', response%code
            print '("cURL message: ", a)',  response%error_message
        end if

        call dm_rpc_reset(request)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return
        if (response%code /= HTTP_OK) return

        ! POST request.
        allocate (observs(NOBSERVS))
        call dm_test_dummy(observs)

        print '(1x, a, i0, a)', 'Sending ', NOBSERVS, ' observations via HTTP POST ...'
        call dm_timer_start(timer)

        do i = 1, NOBSERVS
            if (modulo(i, 10) == 0) print *, '>>> ', i

            rc = dm_rpc_post(request     = request, &
                             response    = response, &
                             type        = observs(i), &
                             username    = username, &
                             password    = password, &
                             compression = Z_TYPE_ZSTD, &
                             url         = dm_rpc_url(host, endpoint='/observ'))

            call dm_rpc_reset(request)

            if (response%code /= HTTP_CREATED) then
                print *, 'HTTP: ', response%code
                print '(a)', response%payload
                exit
            end if

            if (dm_is_error(rc)) exit
        end do

        call dm_timer_stop(timer, dt)
        call dm_error_out(rc)
        print '(1x, i0, a, f8.5, a)', (i - 1), ' observations sent in ', dt, ' sec'

        call dm_rpc_shutdown()
        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        !! This test sends multiple observations concurrently via HTTP POST
        !! request to an RPC-API.
        integer, parameter :: NOBSERVS = 10

        character(len=:), allocatable :: host, username, password
        integer                       :: i, rc
        logical                       :: error
        real(kind=r8)                 :: dt
        type(timer_type)              :: timer

        type(observ_type),       allocatable :: observs(:)
        type(rpc_request_type),  allocatable :: requests(:)
        type(rpc_response_type), allocatable :: responses(:)

        stat = TEST_PASSED
        if (.not. get_env(host, username, password)) return

        stat = TEST_FAILED
        rc = dm_rpc_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        allocate (observs(NOBSERVS), requests(NOBSERVS), responses(NOBSERVS))
        call dm_test_dummy(observs)

        print '(1x, a, i0, a)', 'Sending ', NOBSERVS, ' observations concurrently via HTTP POST ...'
        call dm_timer_start(timer)
        rc = dm_rpc_post(requests, responses, observs, dm_rpc_url(host, endpoint='/observ'), &
                         username, password, compression=Z_TYPE_ZSTD)
        call dm_timer_stop(timer, dt)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        error = .false.

        do i = 1, size(responses)
            if (responses(i)%code /= HTTP_CREATED) then
                error = .true.
                print '(" Request: ", i0, " HTTP: ", i0, " Error: ", i0, " cURL: ", i0)', &
                    i, responses(i)%code, responses(i)%error, responses(i)%error_curl
            end if
        end do

        print '(1x, i0, a, f8.5, a)', NOBSERVS, ' observations sent in ', dt, ' sec'
        call dm_rpc_shutdown()

        if (error) return
        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        !! This test sends multiple observations sequentially via HTTP POST
        !! request to an RPC-API.
        integer, parameter :: NOBSERVS = 10

        character(len=:), allocatable :: host, username, password
        integer                       :: i, rc
        logical                       :: error
        real(kind=r8)                 :: dt
        type(timer_type)              :: timer

        type(observ_type),       allocatable :: observs(:)
        type(rpc_request_type),  allocatable :: requests(:)
        type(rpc_response_type), allocatable :: responses(:)

        stat = TEST_PASSED
        if (.not. get_env(host, username, password)) return

        stat = TEST_FAILED
        rc = dm_rpc_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        allocate (observs(NOBSERVS), requests(NOBSERVS), responses(NOBSERVS))
        call dm_test_dummy(observs)

        print '(1x, a, i0, a)', 'Sending ', NOBSERVS, ' observations sequentially via HTTP POST ...'
        call dm_timer_start(timer)
        rc = dm_rpc_post(requests, responses, observs, dm_rpc_url(host, endpoint='/observ'), &
                         username, password, compression=Z_TYPE_ZSTD, sequential=.true.)
        call dm_timer_stop(timer, dt)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        error = .false.

        do i = 1, size(responses)
            if (responses(i)%code /= HTTP_CREATED) then
                error = .true.
                print '(" Request: ", i0, " HTTP: ", i0, " Error: ", i0, " cURL: ", i0)', &
                    i, responses(i)%code, responses(i)%error, responses(i)%error_curl
            end if
        end do

        print '(1x, i0, a, f8.5, a)', NOBSERVS, ' observations sent in ', dt, ' sec'
        call dm_rpc_shutdown()

        if (error) return
        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        !! This test sends a beat via HTTP POST request to an RPC-API.
        character(len=:), allocatable :: host, username, password
        integer                       :: rc
        integer(kind=i8)              :: uptime
        real(kind=r8)                 :: dt
        type(beat_type)               :: beat
        type(rpc_request_type)        :: request
        type(rpc_response_type)       :: response
        type(timer_type)              :: timer

        stat = TEST_PASSED
        if (.not. get_env(host, username, password)) return

        stat = TEST_FAILED
        rc = dm_rpc_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        beat = beat_type(node_id='dummy-node', time_sent=dm_time_now(), interval=600)
        call dm_system_uptime(uptime, rc)
        if (rc == E_NONE) beat%uptime = int(uptime)

        print *, 'Sending beat via HTTP POST ...'
        call dm_timer_start(timer)
        rc = dm_rpc_post(request     = request, &
                         response    = response, &
                         type        = beat, &
                         username    = username, &
                         password    = password, &
                         compression = Z_TYPE_ZLIB, &
                         url         = dm_rpc_url(host, endpoint='/beat'))
        call dm_timer_stop(timer, dt)
        call dm_error_out(rc)

        if (response%code /= HTTP_CREATED) print '(" HTTP: ", i0)', response%code

        print '(a)', response%payload
        call dm_rpc_reset(request)
        call dm_rpc_shutdown()

        print '(1x, a, f8.5, a)', 'Beat sent in ', dt, ' sec'
        stat = TEST_PASSED
    end function test05
end program dmtestrpc
