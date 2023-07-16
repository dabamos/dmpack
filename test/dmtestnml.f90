! dmtestnml.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestnml
    !! Test program that tries Namelist export.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 6

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestnml%dm_test01', dm_test01)
    tests(2) = test_type('dmtestnml%dm_test02', dm_test02)
    tests(3) = test_type('dmtestnml%dm_test03', dm_test03)
    tests(4) = test_type('dmtestnml%dm_test04', dm_test04)
    tests(5) = test_type('dmtestnml%dm_test05', dm_test05)
    tests(6) = test_type('dmtestnml%dm_test06', dm_test06)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
        character(len=NML_OBSERV_LEN) :: str
        integer                       :: rc
        type(observ_type)             :: observ1, observ2

        stat = TEST_FAILED

        call dm_dummy_observ(observ1)

        print *, 'Writing observation to namelist string ...'
        rc = dm_nml_from(observ1, str)

        if (dm_is_error(rc)) then
            print *, str
            call dm_perror(rc)
            return
        end if

        print *, 'Reading observation from namelist string ...'
        rc = dm_nml_to(str, observ2)

        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching observations ...'
        if (.not. (observ1 == observ2)) return

        stat = TEST_PASSED
    end function dm_test01

    logical function dm_test02() result(stat)
        character(len=NML_BEAT_LEN) :: str
        integer                     :: rc
        type(beat_type)             :: beat1, beat2

        stat = TEST_FAILED

        beat1%node_id = 'dummy-node'
        beat1%address = '127.0.0.1'

        print *, 'Writing beat to namelist string ...'
        rc = dm_nml_from(beat1, str)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading beat from namelist string ...'
        rc = dm_nml_to(str, beat2)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching beats ...'
        if (.not. (beat1 == beat2)) return

        print *, 'Printing namelist string ...'
        print '(a)', trim(str)

        stat = TEST_PASSED
    end function dm_test02

    logical function dm_test03() result(stat)
        character(len=NML_LOG_LEN) :: str
        integer                    :: rc
        type(log_type)             :: log1, log2

        stat = TEST_FAILED

        call dm_dummy_log(log1)

        print *, 'Writing log to namelist string ...'
        rc = dm_nml_from(log1, str)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading log from namelist string ...'
        rc = dm_nml_to(str, log2)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching logs ...'
        if (.not. (log1 == log2)) return

        print *, 'Printing namelist string ...'
        print '(a)', trim(str)

        stat = TEST_PASSED
    end function dm_test03

    logical function dm_test04() result(stat)
        character(len=NML_NODE_LEN) :: str
        integer                     :: rc
        type(node_type)             :: node1, node2

        stat = TEST_FAILED

        call dm_dummy_node(node1)

        print *, 'Writing node to namelist string ...'
        rc = dm_nml_from(node1, str)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading node from namelist string ...'
        rc = dm_nml_to(str, node2)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching nodes ...'
        if (.not. (node1 == node2)) return

        print *, 'Printing namelist string ...'
        print '(a)', trim(str)

        stat = TEST_PASSED
    end function dm_test04

    logical function dm_test05() result(stat)
        character(len=NML_SENSOR_LEN) :: str
        integer                       :: rc
        type(sensor_type)             :: sensor1, sensor2

        stat = TEST_FAILED

        call dm_dummy_sensor(sensor1)

        print *, 'Writing sensor to namelist string ...'
        rc = dm_nml_from(sensor1, str)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading sensor from namelist string ...'
        rc = dm_nml_to(str, sensor2)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching sensors ...'
        if (.not. (sensor1 == sensor2)) return

        print *, 'Printing namelist string ...'
        print '(a)', trim(str)

        stat = TEST_PASSED
    end function dm_test05

    logical function dm_test06() result(stat)
        character(len=NML_TARGET_LEN) :: str
        integer                       :: rc
        type(target_type)             :: target1, target2

        stat = TEST_FAILED

        call dm_dummy_target(target1)

        print *, 'Writing target to namelist string ...'
        rc = dm_nml_from(target1, str)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading target from namelist string ...'
        rc = dm_nml_to(str, target2)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching targets ...'
        if (.not. (target1 == target2)) return

        print *, 'Printing namelist string ...'
        print '(a)', trim(str)

        stat = TEST_PASSED
    end function dm_test06
end program dmtestnml
