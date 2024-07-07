! dmtestversion.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestversion
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestversion'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=:), allocatable :: str

        stat = TEST_FAILED

        print '("Version major.: ", i1)', DM_VERSION_MAJOR
        if (DM_VERSION_MAJOR < 0 .or. DM_VERSION_MAJOR > 9) return

        print '("Version minor.: ", i1)', DM_VERSION_MINOR
        if (DM_VERSION_MINOR < 0 .or. DM_VERSION_MINOR > 9) return

        print '("Version patch.: ", i1)', DM_VERSION_PATCH
        if (DM_VERSION_PATCH < 0 .or. DM_VERSION_PATCH > 9) return

        print '("Version string: ", a)', DM_VERSION_STRING
        if (len(DM_VERSION_STRING) /= 5) return

        str = dm_version_to_string(TEST_NAME, 1, 0, 0)
        if (str /= (TEST_NAME // ' 1.0.0')) return

        call dm_version_out()
        call dm_version_out(TEST_NAME, 1, 0, 0)

        stat = TEST_PASSED
    end function test01
end program dmtestversion
