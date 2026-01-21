! dmtestjob.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestjob
    !! Test program that checks job processing.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestjob'
    integer,          parameter :: NTESTS    = 1

    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        integer, parameter :: MAX_SIZE = 32

        integer             :: i, j, rc
        type(group_type)    :: group
        type(job_type)      :: job
        type(job_list_type) :: job_list

        type(observ_type), allocatable :: observs(:)

        stat = TEST_FAILED

        allocate (observs(5))
        call dm_test_dummy(observs)

        print *, 'Creating job list ...'
        rc = dm_job_list_create(job_list, MAX_SIZE)
        if (dm_is_error(rc)) return

        print *, 'Checking job list size ...'
        if (dm_job_list_size(job_list) /= MAX_SIZE) return

        print *, 'Adding jobs to job list ...'
        do i = 1, size(observs)
            print *, '- Destroying group ...'
            call dm_group_destroy(group)

            print *, '- Creating group ...'
            rc = dm_group_create(group, 1)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return

            print *, '- Adding observation to group ...'
            rc = dm_group_add(group, observs(i))
            call dm_error_out(rc)
            if (dm_is_error(rc)) return

            print *, '- Setting group of job ...'
            call dm_job_set(job, group=group)

            print *, '- Adding job to job list ...'
            rc = dm_job_list_add(job_list, job)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
        end do

        print *, 'Retrieving jobs in correct order ...'
        do i = 1, 2 * size(observs)
            j = 1 + modulo(i - 1, size(observs))
            rc = dm_job_list_next(job_list, job)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            if (job%group%observs(1)%id /= observs(j)%id) return
        end do

        call dm_job_list_destroy(job_list)
        rc = dm_job_list_create(job_list, MAX_SIZE)

        print *, 'Adding disabled jobs to job list ...'
        do i = 1, size(observs)
            call dm_group_destroy(group)
            rc = dm_group_create(group, 1)
            rc = dm_group_add(group, observs(i))

            call dm_job_set(job, group=group)
            if (i > 1) call dm_job_set(job, disabled=.true.)

            rc = dm_job_list_add(job_list, job)
            if (dm_is_error(rc)) return
        end do

        print *, 'Retrieving all jobs in correct order ...'
        do i = 1, size(observs)
            rc = dm_job_list_next(job_list, job, disabled=.true.)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            if (job%group%observs(1)%id /= observs(i)%id) return
        end do

        print *, 'Retrieving enabled jobs in correct order ...'
        do i = 1, size(observs)
            rc = dm_job_list_next(job_list, job)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            if (job%group%observs(1)%id /= observs(1)%id) return
        end do

        stat = TEST_PASSED
    end function test01
end program dmtestjob
