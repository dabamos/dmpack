! dmtestipcmutex.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestipcmutex
    !! Test program for IPC mutex handling.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestipcmutex'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        integer              :: rc
        type(ipc_mutex_type) :: mutex

        stat = TEST_FAILED

        print *, 'Creating IPC mutex ...'
        rc = dm_ipc_mutex_create(mutex)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Locking ...'
        call dm_ipc_mutex_lock(mutex)

        print *, 'Unlocking ...'
        call dm_ipc_mutex_unlock(mutex)

        print *, 'Destroying ...'
        call dm_ipc_mutex_destroy(mutex)

        stat = TEST_PASSED
    end function test01
end program dmtestipcmutex

