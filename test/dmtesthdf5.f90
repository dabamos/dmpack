! dmtesthdf5.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthdf5
    !! Test program for HDF5 file handling.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME  = 'dmtesthdf5'
    integer,          parameter :: NTESTS     = 7

    character(len=*), parameter :: FILE_PATH  = 'testobserv.hdf5'
    character(len=*), parameter :: GROUP_NAME = 'timeseries'

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    call dm_init()

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04), &
        test_type('test05', test05), &
        test_type('test06', test06), &
        test_type('test07', test07)  &
    ]

    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
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
        rc = dm_hdf5_version_number(major, minor, release)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print '(" Major..: ", i0)', major
        print '(" Minor..: ", i0)', minor
        print '(" Release: ", i0)', release
        print '(" Library: ", a)',  dm_hdf5_version(.true.)

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
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating group ...'
            rc = dm_hdf5_open(file, group, GROUP_NAME, create=.true.)
            if (dm_is_error(rc)) exit test_block
            if (.not. dm_hdf5_group_exists(file, GROUP_NAME, error=rc)) exit test_block

            print *, 'Writing nodes ...'
            rc = dm_hdf5_write(group, input)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading nodes ...'
            rc = dm_hdf5_read(group, output)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating nodes ...'
            if (.not. allocated(output)) exit test_block
            if (size(input) /= size(output)) exit test_block
            if (.not. all(dm_node_equals(input, output))) exit test_block

            stat = TEST_PASSED
        end block test_block

        call dm_error_out(rc)

        print *, 'Closing group ...'
        rc = dm_hdf5_close(group)
        call dm_error_out(rc)

        print *, 'Closing file ...'
        rc = dm_hdf5_close(file)
        call dm_error_out(rc)

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
        call dm_error_out(rc)
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
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening group ...'
            rc = dm_hdf5_open(file, group, GROUP_NAME)
            if (dm_is_error(rc)) exit test_block

            print *, 'Writing sensors ...'
            rc = dm_hdf5_write(group, input)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading sensors ...'
            rc = dm_hdf5_read(group, output)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating sensors ...'
            if (.not. allocated(output)) exit test_block
            if (size(input) /= size(output)) exit test_block
            if (.not. all(dm_sensor_equals(input, output))) exit test_block

            stat = TEST_PASSED
        end block test_block

        call dm_error_out(rc)

        print *, 'Closing group ...'
        rc = dm_hdf5_close(group)
        call dm_error_out(rc)

        print *, 'Closing file ...'
        rc = dm_hdf5_close(file)
        call dm_error_out(rc)

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
        call dm_error_out(rc)
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
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening group ...'
            rc = dm_hdf5_open(file, group, GROUP_NAME)
            if (dm_is_error(rc)) exit test_block

            print *, 'Writing targets ...'
            rc = dm_hdf5_write(group, input)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading targets ...'
            rc = dm_hdf5_read(group, output)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating targets ...'
            if (.not. allocated(output)) exit test_block
            if (size(input) /= size(output)) exit test_block
            if (.not. all(dm_target_equals(input, output))) exit test_block

            stat = TEST_PASSED
        end block test_block

        call dm_error_out(rc)

        print *, 'Closing group ...'
        rc = dm_hdf5_close(group)
        call dm_error_out(rc)

        print *, 'Closing file ...'
        rc = dm_hdf5_close(file)
        call dm_error_out(rc)

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
        call dm_error_out(rc)
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
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening group ...'
            rc = dm_hdf5_open(file, group, GROUP_NAME)
            if (dm_is_error(rc)) exit test_block

            print *, 'Writing observations ...'
            rc = dm_hdf5_write(group, input)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading observations ...'
            rc = dm_hdf5_read(group, output)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating observations ...'
            if (.not. allocated(output)) exit test_block
            if (size(input) /= size(output)) exit test_block
            if (.not. all(dm_observ_equals(input, output))) exit test_block

            stat = TEST_PASSED
        end block test_block

        call dm_error_out(rc)

        print *, 'Closing group ...'
        rc = dm_hdf5_close(group)
        call dm_error_out(rc)

        print *, 'Closing file ...'
        rc = dm_hdf5_close(file)
        call dm_error_out(rc)

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
        call dm_error_out(rc)
    end function test06

    logical function test07() result(stat)
        !! Tests availale filters.
        integer :: i, rc
        logical :: avail(4)

        stat = TEST_FAILED

        print *, 'Initialising ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Checking available filters ...'
        avail = .false.

        do i = HDF5_FILTER_DEFLATE, HDF5_FILTER_SZIP
            avail(i) = dm_hdf5_filter_available(i, error=rc)
            call dm_error_out(rc)
        end do

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
