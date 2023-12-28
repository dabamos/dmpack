! dmtestjson.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestjson
    !! Test program that tries JSON export.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 10

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('dmtestjson.test01', test01), &
        test_type('dmtestjson.test02', test02), &
        test_type('dmtestjson.test03', test03), &
        test_type('dmtestjson.test04', test04), &
        test_type('dmtestjson.test05', test05), &
        test_type('dmtestjson.test06', test06), &
        test_type('dmtestjson.test07', test07), &
        test_type('dmtestjson.test08', test08), &
        test_type('dmtestjson.test09', test09), &
        test_type('dmtestjson.test10', test10)  &
    ]

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=:), allocatable :: json
        type(observ_type)             :: observ

        stat = TEST_FAILED

        call dm_dummy_observ(observ)

        print *, 'Writing observation to JSON string ...'
        json = dm_json_from(observ)

        if (.not. allocated(json)) return
        if (len_trim(json) == 0) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=:), allocatable :: json
        type(beat_type)             :: beat

        stat = TEST_FAILED

        beat%node_id = 'dummy-node'
        beat%address = '127.0.0.1'

        print *, 'Writing beat to JSON string ...'
        json = dm_json_from(beat)

        if (.not. allocated(json)) return
        if (len_trim(json) == 0) return

        print *, 'Printing JSON string ...'
        print '(72("."))'
        print '(a)', trim(json)
        print '(72("."))'

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        character(len=:), allocatable :: json
        type(log_type)                :: log

        stat = TEST_FAILED

        call dm_dummy_log(log)

        print *, 'Writing log to JSON string ...'
        json = dm_json_from(log)

        if (.not. allocated(json)) return
        if (len_trim(json) == 0) return

        print *, 'Printing JSON string ...'
        print '(72("."))'
        print '(a)', trim(json)
        print '(72("."))'

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        character(len=:), allocatable :: json
        type(node_type)               :: node

        stat = TEST_FAILED

        call dm_dummy_node(node)

        print *, 'Writing node to JSON string ...'
        json = dm_json_from(node)

        if (.not. allocated(json)) return
        if (len_trim(json) == 0) return

        print *, 'Printing JSON string ...'
        print '(72("."))'
        print '(a)', trim(json)
        print '(72("."))'

        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        character(len=:), allocatable :: json
        type(sensor_type)             :: sensor

        stat = TEST_FAILED

        call dm_dummy_sensor(sensor)

        print *, 'Writing sensor to JSON string ...'
        json = dm_json_from(sensor)

        if (.not. allocated(json)) return
        if (len_trim(json) == 0) return

        print *, 'Printing JSON string ...'
        print '(72("."))'
        print '(a)', trim(json)
        print '(72("."))'

        stat = TEST_PASSED
    end function test05

    logical function test06() result(stat)
        character(len=:), allocatable :: json
        type(target_type)             :: target

        stat = TEST_FAILED

        call dm_dummy_target(target)

        print *, 'Writing target to JSON string ...'
        json = dm_json_from(target)

        if (.not. allocated(json)) return
        if (len_trim(json) == 0) return

        print *, 'Printing JSON string ...'
        print '(72("."))'
        print '(a)', trim(json)
        print '(72("."))'

        stat = TEST_PASSED
    end function test06

    logical function test07() result(stat)
        character(len=*), parameter :: JSON = &
            '{ "id": "9273ab62f9a349b6a4da6dd274ee83e7", "node_id": "dummy-node", "sensor_id": "dummy-sensor", ' // &
            '"target_id": "dummy-target", "name": "dummy-observ", "timestamp": "1970-01-01T00:00:00.000+00:00", "path": ' // &
            '"/dev/null", "priority": 0, "error": 0, "next": 0, "nreceivers": 3, "nrequests": 2, "receivers": [ ' // &
            '"dummy-receiver1", "dummy-receiver2", "dummy-receiver3" ], "requests": [ { "timestamp": ' // &
            '"1970-01-01T00:00:00.000+00:00", "request": "A", "response": "123.45\\r\\n", "delimiter": "\\r\\n", ' // &
            '"pattern": "^(.*)$", "delay": 1000, "error": 0, "mode": 0, "retries": 0, "state": 0, "timeout": 500, ' // &
            '"nresponses": 1, "responses": [ { "name": "a", "unit": "none", "type": 0, "error": 0, "value": ' // &
            '123.450000000 } ] }, { "timestamp": "1970-01-01T00:00:00.000+00:00", "request": "B", "response": ' // &
            '"OK\\r\\n", "delimiter": "\\r\\n", "pattern": "^OK", "delay": 500, "error": 1, "mode": 0, "retries": 0, ' // &
            '"state": 0, "timeout": 500, "nresponses": 1, "responses": [ { "name": "b", "unit": "none", "type": 0, ' // &
            '"error": 0, "value": 0.990000000000 } ] } ] }'

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
        response = response_type('a', 'none', RESPONSE_TYPE_REAL64, E_NONE, 123.45_r8)
        rc = dm_request_add(request, response)
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
        response = response_type('b', 'none', RESPONSE_TYPE_REAL64, E_NONE, 0.99_r8)
        rc = dm_request_add(request, response)
        if (dm_is_error(rc)) return

        print *, 'Adding request ...'
        rc = dm_observ_add_request(observ, request)
        if (dm_is_error(rc)) return

        print *, 'Validating JSON ...'
        buf = dm_json_from(observ)

        if (buf /= JSON) then
            print *, 'Generated JSON:'
            print '(72("."))'
            print '(a)', buf
            print '(72("."))'

            print *, 'Expected JSON:'
            print '(72("."))'
            print '(a)', JSON
            print '(72("."))'

            return
        end if

        print *, 'JSON array:'
        observs(1) = observ
        print '(72("."))'
        print '(a)', dm_json_from(observs)
        print '(72("."))'

        stat = TEST_PASSED
    end function test07

    logical function test08() result(stat)
        character(len=*), parameter :: JSON = &
            '{ "id": "dummy-node", "name": "Dummy Node", "meta": "", ' // &
            '"x": 1000.00000000, "y": 2000.00000000, "z": 10.0000000000 }'

        character(len=:), allocatable :: buf
        type(node_type)               :: node

        stat = TEST_FAILED

        node = node_type(id    = 'dummy-node', &
                         name  = 'Dummy Node', &
                         meta  = '', &
                         x     = 1000.0_r8, &
                         y     = 2000.0_r8, &
                         z     = 10.0_r8)

        buf = dm_json_from(node)

        print *, 'Generated JSON:'
        print '(72("."))'
        print '(a)', buf
        print '(72("."))'

        print *, 'Validating JSON ...'
        if (buf /= JSON) then
            print *, 'Expected JSON:'
            print '(72("."))'
            print '(a)', JSON
            print '(72("."))'
            return
        end if

        stat = TEST_PASSED
    end function test08

    logical function test09() result(stat)
        character(len=*), parameter :: JSON = &
            '{ "id": "dummy-sensor", "node_id": "dummy-node", "type": 0, "name": "Dummy Sensor", ' // &
            '"sn": "12345", "meta": "", "x": 1000.00000000, "y": 2000.00000000, "z": 10.0000000000 }'

        character(len=:), allocatable :: buf
        type(sensor_type)             :: sensor

        stat = TEST_FAILED

        sensor = sensor_type(id      = 'dummy-sensor', &
                             node_id = 'dummy-node', &
                             type    = SENSOR_TYPE_NONE, &
                             name    = 'Dummy Sensor', &
                             sn      = '12345', &
                             meta    = '', &
                             x       = 1000.0_r8, &
                             y       = 2000.0_r8, &
                             z       = 10.0_r8)

        buf = dm_json_from(sensor)

        print *, 'Generated JSON:'
        print '(72("."))'
        print '(a)', buf
        print '(72("."))'

        print *, 'Validating JSON ...'
        if (buf /= JSON) then
            print *, 'Expected JSON:'
            print '(72("."))'
            print '(a)', JSON
            print '(72("."))'
            return
        end if

        stat = TEST_PASSED
    end function test09

    logical function test10() result(stat)
        character(len=*), parameter :: JSON = &
            '{ "id": "dummy-target", "name": "Dummy Target", "meta": "", ' // &
            '"state": 1, "x": 1000.00000000, "y": 2000.00000000, "z": 10.0000000000 }'

        character(len=:), allocatable :: buf
        type(target_type)             :: target

        stat = TEST_FAILED

        target = target_type(id    = 'dummy-target', &
                             name  = 'Dummy Target', &
                             meta  = '', &
                             state = TARGET_STATE_REMOVED, &
                             x     = 1000.0_r8, &
                             y     = 2000.0_r8, &
                             z     = 10.0_r8)

        buf = dm_json_from(target)

        print *, 'Generated JSON:'
        print '(72("."))'
        print '(a)', buf
        print '(72("."))'

        print *, 'Validating JSON ...'
        if (buf /= JSON) then
            print *, 'Expected JSON:'
            print '(72("."))'
            print '(a)', JSON
            print '(72("."))'
            return
        end if

        stat = TEST_PASSED
    end function test10
end program dmtestjson
