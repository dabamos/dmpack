! dmtesthdf5.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthdf5
    !! Test program for HDF5 file handling.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: FILE_PATH_NODE = 'test_nodes.hdf5'

    integer, parameter :: NTESTS = 4

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtesthdf5.test01', test01)
    tests(2) = test_type('dmtesthdf5.test02', test02)
    tests(3) = test_type('dmtesthdf5.test03', test03)
    tests(4) = test_type('dmtesthdf5.test04', test04)

    call dm_init()
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

        if (dm_file_exists(FILE_PATH_NODE)) then
            print *, 'Deleting ...'
            call dm_file_delete(FILE_PATH_NODE)
        end if

        print *, 'Initialising ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Creating HDF5 file ' // FILE_PATH_NODE // ' ...'
        rc = dm_hdf5_open(file, FILE_PATH_NODE, create=.true.)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Validating ...'
        rc = dm_hdf5_file_valid(FILE_PATH_NODE)
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
        if (dm_file_exists(FILE_PATH_NODE)) call dm_file_delete(FILE_PATH_NODE)

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        !! Tests writing of nodes to HDF5 file.
        integer               :: i, rc
        type(hdf5_file_type)  :: file
        type(hdf5_group_type) :: group
        type(node_type)       :: nodes(10)

        stat = TEST_FAILED

        if (dm_file_exists(FILE_PATH_NODE)) then
            print *, 'Deleting ...'
            call dm_file_delete(FILE_PATH_NODE)
        end if

        print *, 'Initialising ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Creating HDF5 file ' // FILE_PATH_NODE // ' ...'
        rc = dm_hdf5_open(file, FILE_PATH_NODE, create=.true.)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Creating group ...'
        rc = dm_hdf5_open(file, group, 'nodes', create=.true.)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        call dm_dummy_node(nodes)

        do i = 1, size(nodes)
            write (nodes(i)%id, '("dummy-node-", i0)') i
        end do

        rc = dm_hdf5_insert(group, nodes)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Closing group ...'
        rc = dm_hdf5_close(group)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Closing file ...'
        rc = dm_hdf5_close(file)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        !! Tests reading of nodes from HDF5 file.
        integer                      :: i, rc
        type(hdf5_file_type)         :: file
        type(hdf5_group_type)        :: group
        type(node_type), allocatable :: nodes(:)

        stat = TEST_FAILED

        print *, 'Initialising ...'
        rc = dm_hdf5_init()
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening HDF5 file ' // FILE_PATH_NODE // ' ...'
            rc = dm_hdf5_open(file, FILE_PATH_NODE)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Opening group ...'
            rc = dm_hdf5_open(file, group, 'nodes')
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading nodes ...'
            rc = dm_hdf5_read(group, nodes)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            do i = 1, size(nodes)
                print *, i, nodes(i)%id
            end do

            stat = TEST_PASSED
        end block test_block

        print *, 'Closing group ...'
        rc = dm_hdf5_close(group)
        call dm_error_out(rc)
        if (dm_is_error(rc)) stat = TEST_FAILED

        print *, 'Closing file ...'
        rc = dm_hdf5_close(file)
        call dm_error_out(rc)
        if (dm_is_error(rc)) stat = TEST_FAILED

        print *, 'Clean-up ...'
        rc = dm_hdf5_destroy()
        call dm_error_out(rc)
        if (dm_is_error(rc)) stat = TEST_FAILED
    end function test04
end program dmtesthdf5
