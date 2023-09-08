! dmtestjson.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestjson
    !! Test program that tries JSON export.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 6

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestjson.test01', test01)
    tests(2) = test_type('dmtestjson.test02', test02)
    tests(3) = test_type('dmtestjson.test03', test03)
    tests(4) = test_type('dmtestjson.test04', test04)
    tests(5) = test_type('dmtestjson.test05', test05)
    tests(6) = test_type('dmtestjson.test06', test06)

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
        print '(a)', trim(json)

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
        print '(a)', trim(json)

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
        print '(a)', trim(json)

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
        print '(a)', trim(json)

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
        print '(a)', trim(json)

        stat = TEST_PASSED
    end function test06
end program dmtestjson
