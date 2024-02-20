! dmtesthdf5.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthdf5
    !! Test program for HDF5 file handling.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: FILE_PATH  = 'testobserv.hdf5'
    character(len=*), parameter :: GROUP_NAME = 'timeseries'

    integer, parameter :: NTESTS = 7

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    call dm_init()

    tests(1) = test_type('dmtesthdf5.test01', test01)
    tests(2) = test_type('dmtesthdf5.test02', test02)
    tests(3) = test_type('dmtesthdf5.test03', test03)
    tests(4) = test_type('dmtesthdf5.test04', test04)
    tests(5) = test_type('dmtesthdf5.test05', test05)
    tests(6) = test_type('dmtesthdf5.test06', test06)
    tests(7) = test_type('dmtesthdf5.test07', test07)

    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        !! Tests HDF5 library access.
        integer :: major, minor, release
        integer :: rc

        stat = TEST_FAILED

        print *, 'Initialise ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Reading HDF5 library version ...'
        rc = dm_hdf5_version(major, minor, release)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print '(" Major..: ", i0)', major
        print '(" Minor..: ", i0)', minor
        print '(" Release: ", i0)', release

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! Tests creating an HDF5 file.
        integer              :: rc
        type(hdf5_file_type) :: file

        stat = TEST_FAILED

        if (dm_file_exists(FILE_PATH)) then
            print *, 'Deleting ...'
            call dm_file_delete(FILE_PATH)
        end if

        print *, 'Initialising ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Creating HDF5 file ' // FILE_PATH // ' ...'
        rc = dm_hdf5_open(file, FILE_PATH, create=.true.)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Validating ...'
        rc = dm_hdf5_file_valid(FILE_PATH)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Closing ...'
        rc = dm_hdf5_close(file)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Deleting ...'
        if (dm_file_exists(FILE_PATH)) call dm_file_delete(FILE_PATH)

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        !! Tests writing of nodes to HDF5 file.
        integer, parameter :: N = 8

        integer                      :: i, rc
        type(hdf5_file_type)         :: file
        type(hdf5_group_type)        :: group
        type(node_type), allocatable :: input(:), output(:)

        stat = TEST_FAILED

        allocate (input(N))

        call dm_test_dummy(input)

        do i = 1, N
            write (input(i)%id, '("node-", i0)') i
        end do

        if (dm_file_exists(FILE_PATH)) then
            print *, 'Deleting ...'
            call dm_file_delete(FILE_PATH)
        end if

        print *, 'Initialising ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Creating HDF5 file ' // FILE_PATH // ' ...'
            rc = dm_hdf5_open(file, FILE_PATH, create=.true.)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating group ...'
            rc = dm_hdf5_open(file, group, GROUP_NAME, create=.true.)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Writing nodes ...'
            rc = dm_hdf5_write(group, input)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading nodes ...'
            rc = dm_hdf5_read(group, output)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating nodes ...'
            if (.not. allocated(output)) exit test_block
            if (size(input) /= size(output)) exit test_block
            if (.not. all(dm_node_equals(input, output))) exit test_block

            stat = TEST_PASSED
        end block test_block

        print *, 'Closing group ...'
        rc = dm_hdf5_close(group)

        print *, 'Closing file ...'
        rc = dm_hdf5_close(file)

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
    end function test03

    logical function test04() result(stat)
        !! Tests writing of sensors to HDF5 file.
        integer, parameter :: N = 8

        integer                        :: i, rc
        type(hdf5_file_type)           :: file
        type(hdf5_group_type)          :: group
        type(sensor_type), allocatable :: input(:), output(:)

        stat = TEST_FAILED

        allocate (input(N))

        call dm_test_dummy(input)

        do i = 1, N
            write (input(i)%id, '("sensor-", i0)') i
        end do

        print *, 'Initialising ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening HDF5 file ' // FILE_PATH // ' ...'
            rc = dm_hdf5_open(file, FILE_PATH)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening group ...'
            rc = dm_hdf5_open(file, group, GROUP_NAME)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Writing sensors ...'
            rc = dm_hdf5_write(group, input)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading sensors ...'
            rc = dm_hdf5_read(group, output)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating sensors ...'
            if (.not. allocated(output)) exit test_block
            if (size(input) /= size(output)) exit test_block
            if (.not. all(dm_sensor_equals(input, output))) exit test_block

            stat = TEST_PASSED
        end block test_block

        print *, 'Closing group ...'
        rc = dm_hdf5_close(group)

        print *, 'Closing file ...'
        rc = dm_hdf5_close(file)

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
    end function test04

    logical function test05() result(stat)
        !! Tests writing of targets to HDF5 file.
        integer, parameter :: N = 8

        integer                        :: i, rc
        type(hdf5_file_type)           :: file
        type(hdf5_group_type)          :: group
        type(target_type), allocatable :: input(:), output(:)

        stat = TEST_FAILED

        allocate (input(N))

        call dm_test_dummy(input)

        do i = 1, N
            write (input(i)%id, '("target-", i0)') i
        end do

        print *, 'Initialising ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening HDF5 file ' // FILE_PATH // ' ...'
            rc = dm_hdf5_open(file, FILE_PATH)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening group ...'
            rc = dm_hdf5_open(file, group, GROUP_NAME)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Writing targets ...'
            rc = dm_hdf5_write(group, input)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading targets ...'
            rc = dm_hdf5_read(group, output)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating targets ...'
            if (.not. allocated(output)) exit test_block
            if (size(input) /= size(output)) exit test_block
            if (.not. all(dm_target_equals(input, output))) exit test_block

            stat = TEST_PASSED
        end block test_block

        print *, 'Closing group ...'
        rc = dm_hdf5_close(group)

        print *, 'Closing file ...'
        rc = dm_hdf5_close(file)

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
    end function test05

    logical function test06() result(stat)
        !! Tests writing of targets to HDF5 file.
        integer, parameter :: N = 8

        integer                        :: i, rc
        type(hdf5_file_type)           :: file
        type(hdf5_group_type)          :: group
        type(observ_type), allocatable :: input(:), output(:)

        stat = TEST_FAILED

        allocate (input(N))

        call dm_test_dummy(input)

        do i = 1, N
            write (input(i)%name, '("observ-", i0)') i
        end do

        print *, 'Initialising ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening HDF5 file ' // FILE_PATH // ' ...'
            rc = dm_hdf5_open(file, FILE_PATH)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening group ...'
            rc = dm_hdf5_open(file, group, GROUP_NAME)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Writing observations ...'
            rc = dm_hdf5_write(group, input)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading observations ...'
            rc = dm_hdf5_read(group, output)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating observations ...'
            if (.not. allocated(output)) exit test_block
            if (size(input) /= size(output)) exit test_block
            if (.not. all(dm_observ_equals(input, output))) exit test_block

            stat = TEST_PASSED
        end block test_block

        print *, 'Closing group ...'
        rc = dm_hdf5_close(group)

        print *, 'Closing file ...'
        rc = dm_hdf5_close(file)

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
    end function test06

    logical function test07() result(stat)
        !! Tests availale filters.
        integer :: rc
        logical :: avail(4)

        stat = TEST_FAILED

        print *, 'Initialising ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Checking available filters ...'
        avail = .false.
        rc = dm_hdf5_filter_available(HDF5_FILTER_DEFLATE,    avail(HDF5_FILTER_DEFLATE));    call dm_error_out(rc)
        rc = dm_hdf5_filter_available(HDF5_FILTER_SHUFFLE,    avail(HDF5_FILTER_SHUFFLE));    call dm_error_out(rc)
        rc = dm_hdf5_filter_available(HDF5_FILTER_FLETCHER32, avail(HDF5_FILTER_FLETCHER32)); call dm_error_out(rc)
        rc = dm_hdf5_filter_available(HDF5_FILTER_SZIP,       avail(HDF5_FILTER_SZIP));       call dm_error_out(rc)

        print *, 'Deflate...: ', avail(HDF5_FILTER_DEFLATE)
        print *, 'Shuffle...: ', avail(HDF5_FILTER_SHUFFLE)
        print *, 'Fletcher32: ', avail(HDF5_FILTER_FLETCHER32)
        print *, 'SZIP......: ', avail(HDF5_FILTER_SZIP)

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test07
end program dmtesthdf5
