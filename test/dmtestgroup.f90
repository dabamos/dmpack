! dmtestgroup.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestgroup
    !! Test program for observation group.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: TEST_NAME = 'dmtestgroup'
    integer,      parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        integer, parameter :: N = 3

        integer           :: i, rc
        type(group_type)  :: group
        type(observ_type) :: observ

        stat = TEST_FAILED

        print *, 'Creating observation group ...'
        rc = dm_group_create(group, N)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Group id: ', group%id
        if (dm_group_size(group) /= size(group%observs)) return

        print *, 'Adding observations to observation group ...'

        do i = 1, N
            rc = dm_group_add(group, observ)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            if (group%observs(i)%group_id /= group%id) return
        end do

        call dm_group_destroy(group)
        if (dm_group_is_valid(group)) return

        stat = TEST_PASSED
    end function test01
end program dmtestgroup
