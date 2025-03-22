! dmtestapi.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestapi
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestapi'
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
        !! Tests API status type handling.
        character(len=:), allocatable :: str
        integer                       :: rc
        type(api_status_type)         :: api1, api2

        stat = TEST_FAILED

        print *, 'Creating API status type ...'
        api1 = api_status_type(version = '1.0', &
                               host    = 'localhost', &
                               server  = 'test', &
                               message = 'foo', &
                               error   = E_ERROR)

        print *, 'API status type to string ...'
        str = dm_api_status_to_string(api1)

        print *, 'String to API status type ...'
        rc = dm_api_status_from_string(str, api2)
        if (dm_is_error(rc)) return

        print *, 'Matching API status types ...'
        if (.not. (api1 == api2)) return

        print *, 'Printing API status type ...'
        print *, str

        stat = TEST_PASSED
    end function test01
end program dmtestapi
