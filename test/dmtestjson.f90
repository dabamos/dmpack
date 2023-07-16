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

    tests(1) = test_type('dmtestjson%dm_test01', dm_test01)
    tests(2) = test_type('dmtestjson%dm_test02', dm_test02)
    tests(3) = test_type('dmtestjson%dm_test03', dm_test03)
    tests(4) = test_type('dmtestjson%dm_test04', dm_test04)
    tests(5) = test_type('dmtestjson%dm_test05', dm_test05)
    tests(6) = test_type('dmtestjson%dm_test06', dm_test06)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
        character(len=:), allocatable :: json
        type(observ_type)             :: observ

        stat = TEST_FAILED

        call dm_dummy_observ(observ)

        print *, 'Writing observation to JSON string ...'
        json = dm_json_from(observ)

        if (.not. allocated(json)) return
        if (len_trim(json) == 0) return

        stat = TEST_PASSED
    end function dm_test01

    logical function dm_test02() result(stat)
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
    end function dm_test02

    logical function dm_test03() result(stat)
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
    end function dm_test03

    logical function dm_test04() result(stat)
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
    end function dm_test04

    logical function dm_test05() result(stat)
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
    end function dm_test05

    logical function dm_test06() result(stat)
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
    end function dm_test06
end program dmtestjson
