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
    use :: dmpack
    implicit none (type, external)

    integer, parameter :: NOBSERVS = 100
    integer, parameter :: NTESTS   = 3

    character(len=:), allocatable :: host, username, password
    type(test_type)               :: tests(NTESTS)
    logical                       :: stats(NTESTS)
    logical                       :: no_color

    call dm_init()
    no_color = dm_env_has('NO_COLOR')

    if (dm_is_error(dm_env_get('DM_API_HOST',     host))     .or. &
        dm_is_error(dm_env_get('DM_API_USERNAME', username)) .or. &
        dm_is_error(dm_env_get('DM_API_PASSWORD', password))) then

        print '(72("-"))'
        call dm_ansi_color(COLOR_RED, no_color)
        print '("dmtestrpc:", /)'
        print '("    Set environment variables DM_API_HOST, DM_API_USERNAME, and")'
        print '("    DM_API_PASSWORD of the DMPACK RPC API. Otherwise, the tests")'
        print '("    of this program will be skipped.")'
        call dm_ansi_reset(no_color)
        print '(72("-"))'

        call dm_stop(0)
    end if

    if (len(host) == 0) then
        call dm_error_out(E_INVALID, 'invalid host')
        call dm_stop(0)
    end if

    tests(1) = test_type('dmtestrpc%dm_test01', dm_test01)
    tests(2) = test_type('dmtestrpc%dm_test02', dm_test02)
    tests(3) = test_type('dmtestrpc%dm_test03', dm_test03)

    call dm_test_run(tests, stats, no_color)
contains
    logical function dm_test01() result(stat)
        character(len=*), parameter :: URL1 = 'http://example.com:8080/api/v1'
        character(len=*), parameter :: URL2 = 'https://example.com/api/v1'

        character(len=:), allocatable :: res

        stat = TEST_FAILED

        print *, 'Validating URLs ...'

        res = dm_rpc_url('example.com', 8080, '/api/v1')
        print *, res
        if (res /= URL1) return

        res = dm_rpc_url('example.com', base='/api/v1', tls=.true.)
        print *, res
        if (res /= URL2) return

        stat = TEST_PASSED
    end function dm_test01

    logical function dm_test02() result(stat)
        !! This test sends multiple observations via HTTP GET request to
        !! an RPC-API.
        integer                 :: i, rc
        real(kind=r8)           :: dt
        type(rpc_request_type)  :: request
        type(rpc_response_type) :: response
        type(timer_type)        :: timer

        type(observ_type), allocatable :: observs(:)

        stat = TEST_FAILED

        rc = dm_rpc_init()
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Sending raw HTTP GET request to ' // dm_rpc_url(host) // ' ...'
        rc = dm_rpc_request(request  = request, &
                            response = response, &
                            username = username, &
                            password = password, &
                            method   = RPC_METHOD_GET, &
                            url      = dm_rpc_url(host))

        if (response%code == HTTP_OK) then
            print *, 'Response content:'
            print '(a)', response%payload
        else
            print *, 'HTTP code: ', response%code
            print *, 'cURL message: ', response%error_message
        end if

        call dm_rpc_reset(request)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        if (response%code /= HTTP_OK) return

        ! POST request.
        allocate (observs(NOBSERVS))
        call dm_dummy_observ(observs)

        print *, 'Sending ', NOBSERVS, ' observations via HTTP POST ...'
        call dm_timer_start(timer)

        do i = 1, NOBSERVS
            if (modulo(i, 10) == 0) print *, '>>> ', i

            rc = dm_rpc_send(request  = request, &
                             response = response, &
                             observ   = observs(i), &
                             username = username, &
                             password = password, &
                             deflate  = .true., &
                             url      = dm_rpc_url(host, endpoint='/observ'))

            call dm_rpc_reset(request)

            if (response%code /= HTTP_CREATED) then
                print *, 'HTTP: ', response%code
                print '(a)', response%payload
                exit
            end if

            if (dm_is_error(rc)) exit
        end do

        dt = dm_timer_stop(timer)
        call dm_perror(rc)
        print *, i - 1, ' observations sent in ', real(dt), ' sec'

        call dm_rpc_destroy()
        stat = TEST_PASSED
    end function dm_test02

    logical function dm_test03() result(stat)
        !! This test sends a beat via HTTP POST request to an RPC-API.
        integer          :: rc
        integer(kind=i8) :: uptime
        real(kind=r8)    :: dt

        type(beat_type)         :: beat
        type(rpc_request_type)  :: request
        type(rpc_response_type) :: response
        type(timer_type)        :: timer

        stat = TEST_FAILED

        rc = dm_rpc_init()
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        beat = beat_type(node_id='dummy-node', time_sent=dm_time_now(), interval=600)
        call dm_system_uptime(uptime, rc)
        if (rc == E_NONE) beat%uptime = int(uptime)

        print *, 'Sending beat via HTTP POST ...'
        call dm_timer_start(timer)
        rc = dm_rpc_send(request  = request, &
                         response = response, &
                         beat   = beat, &
                         username = username, &
                         password = password, &
                         deflate  = .true., &
                         url      = dm_rpc_url(host, endpoint='/beat'))
        dt = dm_timer_stop(timer)
        call dm_perror(rc)

        if (response%code /= HTTP_CREATED) print *, 'HTTP: ', response%code

        print '(a)', response%payload
        call dm_rpc_reset(request)
        call dm_rpc_destroy()

        print *, 'beat sent in ', real(dt), ' sec'
        stat = TEST_PASSED
    end function dm_test03
end program dmtestrpc
