! dmtestobserv.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestobserv
    !! Test programs that validates observation data handling and JSON
    !! transformation.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS  = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestobserv%dm_test01', dm_test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
        character(len=*), parameter :: JSON = &
            '{ "id": "9273ab62f9a349b6a4da6dd274ee83e7", "node_id": "dummy-node", "sensor_id": "dummy-sensor", ' // &
            '"target_id": "dummy-target", "name": "dummy-observ", "timestamp": "1970-01-01T00:00:00.000+00:00", "path": ' // &
            '"/dev/null", "priority": 0, "error": 0, "next": 0, "nreceivers": 3, "nrequests": 2, "receivers": [ ' // &
            '"dummy-receiver1", "dummy-receiver2", "dummy-receiver3" ], "requests": [ { "timestamp": ' // &
            '"1970-01-01T00:00:00.000+00:00", "request": "A", "response": "123.45\\r\\n", "delimiter": "\\r\\n", ' // &
            '"pattern": "^(.*)$", "delay": 1000, "error": 0, "retries": 0, "state": 0, "timeout": 500, "nresponses": 1, ' // &
            '"responses": [ { "name": "a", "unit": "none", "error": 0, "value": 123.45000 } ] }, { "timestamp": ' // &
            '"1970-01-01T00:00:00.000+00:00", "request": "B", "response": "OK\\r\\n", "delimiter": "\\r\\n", "pattern": ' // &
            '"^OK", "delay": 500, "error": 1, "retries": 0, "state": 0, "timeout": 500, "nresponses": 1, "responses": [ { ' // &
            '"name": "b", "unit": "none", "error": 0, "value": 0.99000000 } ] } ] }'

        character(len=:), allocatable :: buf
        integer                       :: rc
        type(observ_type)             :: observ
        type(observ_type)             :: observs(1)
        type(request_type)            :: request
        type(response_type)           :: response

        stat = TEST_FAILED

        print *, 'Creating new observation ...'

        observ%id        = '9273ab62f9a349b6a4da6dd274ee83e7'
        observ%node_id   = 'dummy-node'
        observ%sensor_id = 'dummy-sensor'
        observ%target_id = 'dummy-target'
        observ%name      = 'dummy-observ'
        observ%path      = '/dev/null'
        observ%timestamp = TIME_DEFAULT

        print *, 'Adding receivers ...'
        rc = dm_observ_add_receiver(observ, 'dummy-receiver1')
        if (dm_is_error(rc)) return

        rc = dm_observ_add_receiver(observ, 'dummy-receiver2')
        if (dm_is_error(rc)) return

        rc = dm_observ_add_receiver(observ, 'dummy-receiver3')
        if (dm_is_error(rc)) return

        print *, 'Creating request ...'
        request = request_type(timestamp = TIME_DEFAULT, &
                               request   = 'A', &
                               response  = dm_ascii_escape('123.45' // ASCII_CR // ASCII_LF), &
                               delimiter = dm_ascii_escape(ASCII_CR // ASCII_LF), &
                               pattern   = '^(.*)$', &
                               delay     = 1000, &
                               retries   = 0, &
                               timeout   = 500, &
                               error     = 0)

        print *, 'Adding response ...'
        response = response_type('a', 'none', 0, 123.45_r8)
        rc = dm_observ_add_response(request, response)
        if (dm_is_error(rc)) return

        print *, 'Adding request ...'
        rc = dm_observ_add_request(observ, request)
        if (dm_is_error(rc)) return

        print *, 'Creating request ...'
        request = request_type(timestamp = TIME_DEFAULT, &
                               request   = 'B', &
                               response  = dm_ascii_escape('OK' // CR_LF), &
                               delimiter = dm_ascii_escape(ASCII_CR // ASCII_LF), &
                               pattern   = '^OK', &
                               delay     = 500, &
                               retries   = 0, &
                               timeout   = 500, &
                               error     = 1)

        print *, 'Adding response ...'
        response = response_type('b', 'none', 0, 0.99_r8)
        rc = dm_observ_add_response(request, response)
        if (dm_is_error(rc)) return

        print *, 'Adding request ...'
        rc = dm_observ_add_request(observ, request)
        if (dm_is_error(rc)) return

        print *, 'Printing observation ...'
        call dm_observ_out(observ)

        print *, 'Validating JSON ...'
        buf = dm_json_from(observ)
        if (buf /= JSON) return

        print *, 'Printing JSON array ...'
        observs(1) = observ
        print *, dm_json_from(observs)

        ! print *, 'Printing Namelist ...'
        ! rc = dm_nml_from(observ, str)
        ! if (dm_is_error(rc)) return
        ! print *, str

        stat = TEST_PASSED
    end function dm_test01
end program dmtestobserv

