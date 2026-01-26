! Author:  Philipp Engel
! Licence: ISC
module dm_job
    !! Observation job.
    use :: dm_error
    use :: dm_group
    use :: dm_observ
    implicit none (type, external)
    private

    type, public :: job_type
        !! Job type that stores an observation for future processing.
        integer          :: delay    = 0            !! Time to wait before next job [msec].
        logical          :: disabled = .false.      !! Ignore job.
        logical          :: onetime  = .false.      !! Disable job after first execution.
        type(group_type) :: group    = group_type() !! Prototype observation(s) to be executed.
    end type job_type

    public :: dm_job_add
    public :: dm_job_count
    public :: dm_job_destroy
    public :: dm_job_group_id
    public :: dm_job_next
    public :: dm_job_set
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_job_add(job, observ) result(rc)
        !! Adds observation to job group.
        type(job_type),    intent(inout) :: job    !! Job.
        type(observ_type), intent(in)    :: observ !! Observation.

        rc = dm_group_add(job%group, observ)
    end function dm_job_add

    integer function dm_job_count(job) result(n)
        !! Returns number of observations in job.
        type(job_type), intent(inout) :: job !! Job.

        n = dm_group_size(job%group)
    end function dm_job_count

    pure elemental subroutine dm_job_destroy(job)
        type(job_type), intent(inout) :: job !! Job.

        job%delay    = 0
        job%disabled = .false.
        job%onetime  = .false.
        call dm_group_destroy(job%group)
    end subroutine dm_job_destroy

    function dm_job_group_id(job) result(id)
        !! Returns id of observation group.
        type(job_type), intent(inout) :: job !! Job.
        character(GROUP_ID_LEN)       :: id  !! Group id of observations.

        id = job%group%id
    end function dm_job_group_id

    integer function dm_job_next(job, next, observ) result(rc)
        !! Returns observation at given index in job group and increases the
        !! index by 1. If the passed index is 0, it will be set to 1 first.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_BOUNDS` if the index is out of bounds.
        !! * `E_INVALID` if the group of the job is invalid.
        !!
        type(job_type),    intent(inout) :: job    !! Job.
        integer,           intent(inout) :: next   !! Next index.
        type(observ_type), intent(out)   :: observ !! Next observation.

        rc = dm_group_next(job%group, next, observ)
    end function dm_job_next

    pure elemental subroutine dm_job_set(job, delay, disabled, onetime, group)
        !! Sets job attributes.
        type(job_type),   intent(inout)        :: job      !! Job.
        integer,          intent(in), optional :: delay    !! Time in msec to wait before next job.
        logical,          intent(in), optional :: disabled !! Ignore job.
        logical,          intent(in), optional :: onetime  !! Disable job after first execution.
        type(group_type), intent(in), optional :: group    !! Prototype observation(s).

        if (present(delay))    job%delay    = delay
        if (present(disabled)) job%disabled = disabled
        if (present(onetime))  job%onetime  = onetime
        if (present(group))    job%group    = group
    end subroutine dm_job_set
end module dm_job
