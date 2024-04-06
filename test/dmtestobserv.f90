! dmtestobserv.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestobserv
    !! Test programs that validates observation, request, and response data
    !! handling,
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestobserv'
    integer,          parameter :: NTESTS    = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        integer             :: rc
        type(observ_type)   :: observ1, observ2
        type(request_type)  :: request
        type(response_type) :: response

        stat = TEST_FAILED

        print *, 'Creating new observation ...'

        observ1%id        = '9273ab62f9a349b6a4da6dd274ee83e7'
        observ1%node_id   = 'dummy-node'
        observ1%sensor_id = 'dummy-sensor'
        observ1%target_id = 'dummy-target'
        observ1%name      = 'dummy-observ'
        observ1%timestamp = TIME_DEFAULT
        observ1%source    = 'dmtestobserv'
        observ1%path      = '/dev/null'

        print *, 'Adding receivers ...'
        rc = dm_observ_add_receiver(observ1, 'dummy-receiver1')
        if (dm_is_error(rc)) return

        rc = dm_observ_add_receiver(observ1, 'dummy-receiver2')
        if (dm_is_error(rc)) return

        rc = dm_observ_add_receiver(observ1, 'dummy-receiver3')
        if (dm_is_error(rc)) return

        print *, 'Creating request ...'
        request = request_type(name      = 'dummy-1', &
                               timestamp = TIME_DEFAULT, &
                               request   = 'A', &
                               response  = dm_ascii_escape('123.45' // ASCII_CR // ASCII_LF), &
                               delimiter = dm_ascii_escape(ASCII_CR // ASCII_LF), &
                               pattern   = '^(.*)$', &
                               delay     = 1000, &
                               retries   = 0, &
                               timeout   = 500, &
                               error     = 0)

        print *, 'Adding response ...'
        response = response_type('a', 'none', RESPONSE_TYPE_REAL64, E_NONE, 123.45_r8)
        rc = dm_request_add(request, response)
        if (dm_is_error(rc)) return

        print *, 'Adding request ...'
        rc = dm_observ_add_request(observ1, request)
        if (dm_is_error(rc)) return

        print *, 'Creating request ...'
        request = request_type(name      = 'dummy-2', &
                               timestamp = TIME_DEFAULT, &
                               request   = 'B', &
                               response  = dm_ascii_escape('OK' // CR_LF), &
                               delimiter = dm_ascii_escape(ASCII_CR // ASCII_LF), &
                               pattern   = '^OK', &
                               delay     = 500, &
                               retries   = 0, &
                               timeout   = 500, &
                               error     = 1)

        print *, 'Adding response ...'
        response = response_type('b', 'none', RESPONSE_TYPE_REAL64, E_NONE, 0.99_r8)
        rc = dm_request_add(request, response)
        if (dm_is_error(rc)) return

        print *, 'Adding request ...'
        rc = dm_observ_add_request(observ1, request)
        if (dm_is_error(rc)) return

        print *, 'Printing observation ...'
        print '(72("."))'
        call dm_observ_out(observ1)
        print '(72("."))'

        print *, 'Validating observation ...'
        observ2 = observ1
        if (.not. (observ1 == observ2)) return
        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer            :: rc
        integer(kind=i4)   :: ival4
        integer(kind=i8)   :: ival8
        logical            :: lval1
        real(kind=r4)      :: rval4
        real(kind=r8)      :: rval8
        type(request_type) :: request

        stat = TEST_FAILED

        ival4 = 1_i4
        ival8 = 1_i8
        lval1 = .true.
        rval4 = 1.0_r4
        rval8 = 1.0_r8

        print *, 'Setting responses ...'
        request%nresponses = 5
        call dm_request_set(request, 1, 'ival4', ival4)
        call dm_request_set(request, 2, 'ival8', ival8)
        call dm_request_set(request, 3, 'lval1', lval1)
        call dm_request_set(request, 4, 'rval4', rval4)
        call dm_request_set(request, 5, 'rval8', rval8)

        print *, 'Getting responses ...'
        call dm_request_get(request, 'ival4', ival4)
        call dm_request_get(request, 'ival8', ival8)
        call dm_request_get(request, 'lval1', lval1)
        call dm_request_get(request, 'rval4', rval4)
        call dm_request_get(request, 'rval8', rval8)

        print *, 'Validating responses ...'
        if (ival4 /= 1_i4) return
        if (ival8 /= 1_i8) return
        if (.not. lval1) return
        if (.not. dm_equals(rval4, 1.0_r4)) return
        if (.not. dm_equals(rval8, 1.0_r8)) return

        print *, 'Calling get routine ...'
        ival4 = huge(0_i4)
        call dm_request_get(request, 'invalid', ival4, status=rc)
        if (ival4 /= huge(0_i4)) return
        if (rc /= E_NOT_FOUND) return

        request%nresponses = 0
        call dm_request_get(request, 'invalid', ival4, status=rc)
        if (rc /= E_EMPTY) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        stat = TEST_PASSED

        print '(" Observation size: ", i0)', OBSERV_SIZE
    end function test03
end program dmtestobserv
