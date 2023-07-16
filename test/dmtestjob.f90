! dmtestjob.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestjob
    !! Test program that checks job processing.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestjob%dm_test01', dm_test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
        integer, parameter :: MAX_SIZE = 32

        type(observ_type)       :: observs(5)
        type(job_type)          :: job
        type(job_list_type)     :: job_list
        integer                 :: i, j, rc

        stat = TEST_FAILED

        call dm_dummy_observ(observs)

        print *, 'Creating job list ...'
        rc = dm_job_list_init(job_list, MAX_SIZE)
        if (dm_is_error(rc)) return

        print *, 'Checking job list size ...'
        if (dm_job_list_size(job_list) /= MAX_SIZE) return

        print *, 'Adding jobs to job list ...'
        do i = 1, size(observs)
            job%observ = observs(i)
            rc = dm_job_list_add(job_list, job)
            if (dm_is_error(rc)) return
        end do

        print *, 'Retrieving jobs in correct order ...'
        do i = 1, 2 * size(observs)
            j = 1 + modulo(i - 1, size(observs))
            rc = dm_job_list_next(job_list, job)
            call dm_perror(rc)
            if (dm_is_error(rc)) return
            if (job%observ%id /= observs(j)%id) return
        end do

        call dm_job_list_destroy(job_list)
        rc = dm_job_list_init(job_list, MAX_SIZE)

        print *, 'Adding disabled jobs to job list ...'
        do i = 1, size(observs)
            job%observ = observs(i)
            if (i > 1) job%disabled = .true.
            rc = dm_job_list_add(job_list, job)
            if (dm_is_error(rc)) return
        end do

        print *, 'Retrieving all jobs in correct order ...'
        do i = 1, size(observs)
            rc = dm_job_list_next(job_list, job, disabled=.true.)
            call dm_perror(rc)
            if (dm_is_error(rc)) return
            if (job%observ%id /= observs(i)%id) return
        end do

        print *, 'Retrieving enabled jobs in correct order ...'
        do i = 1, size(observs)
            rc = dm_job_list_next(job_list, job)
            call dm_perror(rc)
            if (dm_is_error(rc)) return
            if (job%observ%id /= observs(1)%id) return
        end do

        stat = TEST_PASSED
    end function dm_test01
end program dmtestjob
