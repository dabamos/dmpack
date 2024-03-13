! Author:  Philipp Engel
! Licence: ISC
module dm_job
    !! Observation job and job list. Access to job lists is not thread-safe.
    use :: dm_error
    use :: dm_id
    use :: dm_observ
    implicit none (type, external)
    private

    type, public :: job_type
        !! Job type that stores an observation for future processing.
        integer           :: delay    = 0       !! Time in msec to wait before next job.
        logical           :: disabled = .false. !! Ignore job.
        logical           :: onetime  = .false. !! Disable job after first execution.
        logical           :: valid    = .false. !! Job has observation prototype?
        type(observ_type) :: observ             !! Prototype observation to be executed.
    end type job_type

    type, public :: job_list_type
        !! Opaque job list type.
        private
        integer                     :: cursor = 0 !! Current job.
        integer                     :: njobs  = 0 !! Number of jobs in array.
        logical,        allocatable :: mask(:)    !! Enabled mask array.
        type(job_type), allocatable :: jobs(:)    !! Job array.
    end type job_list_type

    public :: dm_job_list_add
    public :: dm_job_list_any
    public :: dm_job_list_count
    public :: dm_job_list_destroy
    public :: dm_job_list_init
    public :: dm_job_list_next
    public :: dm_job_list_size
contains
    integer function dm_job_list_add(job_list, job) result(rc)
        !! Adds job to job list at the next free index in job array.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if job list is not initialised properly.
        !! * `E_LIMIT` if memory limit of job list is reached.
        !! * `E_INVALID` if passed job is invalid.
        !!
        type(job_list_type), intent(inout) :: job_list !! Job list type.
        type(job_type),      intent(inout) :: job      !! Job type.
        integer                            :: i

        rc = E_CORRUPT
        if (.not. allocated(job_list%jobs)) return
        if (.not. allocated(job_list%mask)) return

        rc = E_LIMIT
        i = job_list%njobs + 1
        if (i > size(job_list%jobs)) return

        rc = E_INVALID
        if (job%valid) then
            if (.not. dm_observ_valid(job%observ, id=.false., timestamp=.false.)) return
            if (.not. dm_id_valid(job%observ%target_id)) return
        end if

        job_list%njobs   = i
        job_list%jobs(i) = job
        job_list%mask(i) = (.not. job_list%jobs(i)%disabled)

        rc = E_NONE
    end function dm_job_list_add

    logical function dm_job_list_any(job_list) result(has)
        !! Returns `.true.` if job list contains any enabled jobs.
        type(job_list_type), intent(inout) :: job_list !! Job list type.

        has = .false.
        if (job_list%njobs == 0) return
        if (.not. any(job_list%mask(1:job_list%njobs), 1)) return
        has = .true.
    end function dm_job_list_any

    integer function dm_job_list_count(job_list, disabled) result(n)
        !! Returns number of (enabled) jobs in job list.
        type(job_list_type), intent(inout)        :: job_list !! Job list type.
        logical,             intent(in), optional :: disabled !! Include disabled jobs.
        logical                                   :: disabled_

        n = 0
        if (.not. allocated(job_list%mask)) return

        disabled_ = .false.
        if (present(disabled)) disabled_ = disabled

        if (disabled_) then
            n = job_list%njobs
            return
        end if

        n = count(job_list%mask(1:job_list%njobs), 1)
    end function dm_job_list_count

    integer function dm_job_list_init(job_list, n) result(rc)
        !! Initialises job list. The function returns `E_ALLOC` on error.
        type(job_list_type), intent(out) :: job_list !! Job list type.
        integer,             intent(in)  :: n        !! Maximum number of jobs to hold.
        integer                          :: stat

        rc = E_ALLOC
        allocate (job_list%jobs(n), stat=stat)
        if (stat /= 0) return
        allocate (job_list%mask(n), stat=stat, source=.true.)
        if (stat /= 0) return
        rc = E_NONE
    end function dm_job_list_init

    integer function dm_job_list_next(job_list, job, index, disabled, revolved) result(rc)
        !! Returns copy of next enabled job. If `disabled` is `.true.`, the next
        !! job is returned regardless of state. One-time jobs are disabled by
        !! this functions. If the job list has been finished and restarted from
        !! the beginning, `revolved` is set to `.true.`.
        !!
        !! Call `dm_job_list_any()` or `dm_job_list_count()` beforehand to check
        !! if the job list contains any enabled jobs. Otherwise, this function
        !! return `E_EMPTY`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if job list is not initialised properly.
        !! * `E_EMPTY` if job list is empty.
        !!
        type(job_list_type), intent(inout)         :: job_list !! Job list type.
        type(job_type),      intent(out)           :: job      !! Job type.
        integer,             intent(out), optional :: index    !! Position in job list.
        logical,             intent(out), optional :: revolved !! Job list was revolved.
        logical,             intent(in),  optional :: disabled !! Return disabled job.

        integer :: i, j
        logical :: disabled_

        if (present(index)) index = 0
        if (present(revolved)) revolved = .false.

        rc = E_CORRUPT
        if (.not. allocated(job_list%jobs)) return
        if (.not. allocated(job_list%mask)) return

        rc = E_EMPTY
        if (size(job_list%jobs) == 0) return
        if (job_list%njobs == 0) return

        ! At least one enabled job in job list?
        disabled_ = .false.
        if (present(disabled)) disabled_ = disabled
        if (.not. disabled_ .and. .not. dm_job_list_any(job_list)) return

        ! Find next job (including invalid ones).
        do i = 0, job_list%njobs - 1
            j = 1 + modulo(job_list%cursor + i, job_list%njobs)
            if (.not. job_list%jobs(j)%disabled .or. disabled_) exit
        end do

        if (present(revolved) .and. j <= job_list%cursor) then
            revolved = .true.
        end if

        if (present(index)) index = j

        job = job_list%jobs(j)
        job_list%cursor = j

        ! Disable one-time job.
        if (.not. job_list%jobs(j)%disabled .and. job_list%jobs(j)%onetime) then
            job_list%jobs(j)%disabled = .true.
            job_list%mask(j) = (.not. job_list%jobs(j)%disabled)
        end if

        rc = E_NONE
    end function dm_job_list_next

    integer function dm_job_list_size(job_list, njobs) result(sz)
        !! Returns size of job list array.
        type(job_list_type), intent(inout)         :: job_list !! Job list type.
        integer,             intent(out), optional :: njobs    !! Number of jobs in job list.

        sz = 0
        if (present(njobs)) njobs = job_list%njobs
        if (.not. allocated(job_list%jobs)) return
        sz = size(job_list%jobs)
    end function dm_job_list_size

    subroutine dm_job_list_destroy(job_list)
        !! Deallocates job list.
        type(job_list_type), intent(inout) :: job_list !! Job list type.

        job_list%njobs  = 0
        job_list%cursor = 0

        if (allocated(job_list%jobs)) deallocate (job_list%jobs)
        if (allocated(job_list%mask)) deallocate (job_list%mask)
    end subroutine dm_job_list_destroy
end module dm_job
