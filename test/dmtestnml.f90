! dmtestnml.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestnml
    !! Test program that tries Namelist export.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestnml'
    integer,          parameter :: NTESTS    = 8

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04), &
        test_type('test05', test05), &
        test_type('test06', test06), &
        test_type('test07', test07), &
        test_type('test08', test08)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=NML_OBSERV_LEN) :: str
        integer                       :: rc
        type(observ_type)             :: observ1, observ2

        stat = TEST_FAILED

        call dm_test_dummy(observ1)

        print *, 'Writing observation to namelist string ...'
        rc = dm_nml_from(observ1, str)

        if (dm_is_error(rc)) then
            print *, str
            call dm_error_out(rc)
            return
        end if

        print *, 'Reading observation from namelist string ...'
        rc = dm_nml_to(str, observ2)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching observations ...'
        if (.not. (observ1 == observ2)) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=NML_BEAT_LEN) :: str
        integer                     :: rc
        type(beat_type)             :: beat1, beat2

        stat = TEST_FAILED

        beat1%node_id = 'dummy-node'
        beat1%address = '127.0.0.1'
        beat1%client  = dm_version_to_string('dmtestnml', 1, 0, 0, library=.true.)

        print *, 'Writing beat to namelist string ...'
        rc = dm_nml_from(beat1, str)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading beat from namelist string ...'
        rc = dm_nml_to(str, beat2)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching beats ...'
        if (.not. (beat1 == beat2)) return

        print *, 'Printing namelist string ...'
        print '(72("."))'
        print '(a)', trim(str)
        print '(72("."))'

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        character(len=NML_LOG_LEN) :: str
        integer                    :: rc
        type(log_type)             :: log1, log2

        stat = TEST_FAILED

        call dm_test_dummy(log1)

        print *, 'Writing log to namelist string ...'
        rc = dm_nml_from(log1, str)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading log from namelist string ...'
        rc = dm_nml_to(str, log2)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching logs ...'
        if (.not. (log1 == log2)) return

        print *, 'Printing namelist string ...'
        print '(72("."))'
        print '(a)', trim(str)
        print '(72("."))'

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        character(len=NML_NODE_LEN) :: str
        integer                     :: rc
        type(node_type)             :: node1, node2

        stat = TEST_FAILED

        call dm_test_dummy(node1)

        print *, 'Writing node to namelist string ...'
        rc = dm_nml_from(node1, str)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading node from namelist string ...'
        rc = dm_nml_to(str, node2)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching nodes ...'
        if (.not. (node1 == node2)) return

        print *, 'Printing namelist string ...'
        print '(72("."))'
        print '(a)', trim(str)
        print '(72("."))'

        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        character(len=NML_SENSOR_LEN) :: str
        integer                       :: rc
        type(sensor_type)             :: sensor1, sensor2

        stat = TEST_FAILED

        call dm_test_dummy(sensor1)

        print *, 'Writing sensor to namelist string ...'
        rc = dm_nml_from(sensor1, str)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading sensor from namelist string ...'
        rc = dm_nml_to(str, sensor2)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching sensors ...'
        if (.not. (sensor1 == sensor2)) return

        print *, 'Printing namelist string ...'
        print '(72("."))'
        print '(a)', trim(str)
        print '(72("."))'

        stat = TEST_PASSED
    end function test05

    logical function test06() result(stat)
        character(len=NML_TARGET_LEN) :: str
        integer                       :: rc
        type(target_type)             :: target1, target2

        stat = TEST_FAILED

        call dm_test_dummy(target1)

        print *, 'Writing target to namelist string ...'
        rc = dm_nml_from(target1, str)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading target from namelist string ...'
        rc = dm_nml_to(str, target2)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching targets ...'
        if (.not. (target1 == target2)) return

        print *, 'Printing namelist string ...'
        print '(72("."))'
        print '(a)', trim(str)
        print '(72("."))'

        stat = TEST_PASSED
    end function test06

    logical function test07() result(stat)
        character(len=NML_IMAGE_LEN) :: str
        integer                      :: rc
        type(image_type)             :: image1, image2

        stat = TEST_FAILED

        call dm_test_dummy(image1)

        print *, 'Writing image to namelist string ...'
        rc = dm_nml_from(image1, str)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading image from namelist string ...'
        rc = dm_nml_to(str, image2)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching images ...'
        if (.not. (image1 == image2)) return

        print *, 'Printing namelist string ...'
        print '(72("."))'
        print '(a)', trim(str)
        print '(72("."))'

        stat = TEST_PASSED
    end function test07

    logical function test08() result(stat)
        character(len=65536) :: buffer
        integer              :: n, rc

        type(beat_type)   :: beat
        type(image_type)  :: image
        type(log_type)    :: log
        type(observ_type) :: observ
        type(node_type)   :: node
        type(sensor_type) :: sensor
        type(target_type) :: target

        stat = TEST_FAILED

        buffer = ' '
        rc = dm_nml_from(beat, buffer)
        if (dm_is_error(rc)) return
        n = len_trim(buffer)
        print '(" [Beat  ] Max. length: ", i5, " Trim. length: ", i5, " Diff: ", i5)', NML_BEAT_LEN, n, NML_BEAT_LEN - n
        if (n > NML_BEAT_LEN) return

        buffer = ' '
        rc = dm_nml_from(image, buffer)
        if (dm_is_error(rc)) return
        n = len_trim(buffer)
        print '(" [Image ] Max. length: ", i5, " Trim. length: ", i5, " Diff: ", i5)', NML_IMAGE_LEN, n, NML_IMAGE_LEN - n
        if (n > NML_IMAGE_LEN) return

        buffer = ' '
        rc = dm_nml_from(log, buffer)
        if (dm_is_error(rc)) return
        n = len_trim(buffer)
        print '(" [Log   ] Max. length: ", i5, " Trim. length: ", i5, " Diff: ", i5)', NML_LOG_LEN, n, NML_LOG_LEN - n
        if (n > NML_LOG_LEN) return

        buffer = ' '
        rc = dm_nml_from(observ, buffer)
        if (dm_is_error(rc)) return
        n = len_trim(buffer)
        print '(" [Observ] Max. length: ", i5, " Trim. length: ", i5, " Diff: ", i5)', NML_OBSERV_LEN, n, NML_OBSERV_LEN - n
        if (n > NML_OBSERV_LEN) return

        buffer = ' '
        rc = dm_nml_from(node, buffer)
        if (dm_is_error(rc)) return
        n = len_trim(buffer)
        print '(" [Node  ] Max. length: ", i5, " Trim. length: ", i5, " Diff: ", i5)', NML_NODE_LEN, n, NML_NODE_LEN - n
        if (n > NML_NODE_LEN) return

        buffer = ' '
        rc = dm_nml_from(sensor, buffer)
        if (dm_is_error(rc)) return
        n = len_trim(buffer)
        print '(" [Sensor] Max. length: ", i5, " Trim. length: ", i5, " Diff: ", i5)', NML_SENSOR_LEN, n, NML_SENSOR_LEN - n
        if (n > NML_SENSOR_LEN) return

        buffer = ' '
        rc = dm_nml_from(target, buffer)
        if (dm_is_error(rc)) return
        n = len_trim(buffer)
        print '(" [Target] Max. length: ", i5, " Trim. length: ", i5, " Diff: ", i5)', NML_TARGET_LEN, n, NML_TARGET_LEN - n
        if (n > NML_TARGET_LEN) return

        stat = TEST_PASSED
    end function test08
end program dmtestnml
