! dmtesthdf5.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthdf5
    !! Test program for HDF5 file handling.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtesthdf5.test01', test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
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
end program dmtesthdf5
