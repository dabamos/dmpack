! Author:  Philipp Engel
! Licence: ISC
module dm_job_list
    !! Observation job list. Access to job lists is not thread-safe.
    use :: dm_error
    use :: dm_job
    implicit none (type, external)
    private

    type, public :: job_list_type
        !! Opaque job list type.
        private
        integer                     :: index = 0 !! Current job.
        integer                     :: njobs = 0 !! Number of jobs in array.
        logical,        allocatable :: mask(:)   !! Enabled mask array.
        type(job_type), allocatable :: jobs(:)   !! Job array.
    end type job_list_type

    public :: dm_job_list_add
    public :: dm_job_list_any
    public :: dm_job_list_count
    public :: dm_job_list_create
    public :: dm_job_list_destroy
    public :: dm_job_list_next
    public :: dm_job_list_reset
    public :: dm_job_list_size
contains
    integer function dm_job_list_add(job_list, job) result(rc)
        !! Adds job to job list at the next free index in job array.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if job list is not initialised properly.
        !! * `E_LIMIT` if memory limit of job list is reached.
        !! * `E_INVALID` if passed job is invalid.
        !!
        use :: dm_group
        use :: dm_id
        use :: dm_observ

        type(job_list_type), intent(inout) :: job_list !! Job list.
        type(job_type),      intent(inout) :: job      !! Job type to add to list.

        integer :: i, j

        rc = E_NULL
        if (.not. allocated(job_list%jobs) .or. .not. allocated(job_list%mask)) return

        rc = E_LIMIT
        if (job_list%njobs >= size(job_list%jobs)) return

        rc = E_INVALID
        if (.not. dm_group_is_valid(job%group)) return

        do i = 1, dm_group_count(job%group)
            associate (observ => job%group%observs(i))
                if (.not. dm_observ_is_valid(observ, id=.false., timestamp=.false.)) return
                if (.not. dm_id_is_valid(observ%target_id)) return
            end associate
        end do

        rc = E_NONE
        j = job_list%njobs + 1
        job_list%njobs   = j
        job_list%jobs(j) = job
        job_list%mask(j) = (.not. job_list%jobs(j)%disabled)
    end function dm_job_list_add

    logical function dm_job_list_any(job_list) result(has)
        !! Returns `.true.` if job list contains any enabled jobs.
        type(job_list_type), intent(inout) :: job_list !! Job list.

        has = .false.
        if (job_list%njobs == 0) return
        if (.not. any(job_list%mask(1:job_list%njobs), 1)) return
        has = .true.
    end function dm_job_list_any

    integer function dm_job_list_count(job_list, disabled) result(n)
        !! Returns number of (enabled) jobs in job list. If `disabled` is
        !! `.true.`, includes also any disabled jobs.
        use :: dm_util, only: dm_present

        type(job_list_type), intent(inout)        :: job_list !! Job list.
        logical,             intent(in), optional :: disabled !! Include disabled jobs in count.

        n = 0
        if (.not. allocated(job_list%mask)) return

        if (dm_present(disabled, .false.)) then
            n = job_list%njobs
            return
        end if

        n = count(job_list%mask(1:job_list%njobs), 1)
    end function dm_job_list_count

    integer function dm_job_list_create(job_list, size) result(rc)
        !! Initialises job list.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if given size is less than 0.
        !!
        type(job_list_type), intent(out) :: job_list !! Job list.
        integer,             intent(in)  :: size     !! Maximum number of jobs to hold.

        integer :: stat

        rc = E_INVALID
        if (size < 0) return

        rc = E_ALLOC
        allocate (job_list%jobs(size), stat=stat);                if (stat /= 0) return
        allocate (job_list%mask(size), stat=stat, source=.true.); if (stat /= 0) return

        rc = E_NONE
    end function dm_job_list_create

    pure subroutine dm_job_list_destroy(job_list)
        !! Deallocates job list.
        type(job_list_type), intent(inout) :: job_list !! Job list.

        job_list%njobs = 0
        job_list%index = 0

        if (allocated(job_list%jobs)) then
            call dm_job_destroy(job_list%jobs)
            deallocate (job_list%jobs)
        end if

        if (allocated(job_list%mask)) deallocate (job_list%mask)
    end subroutine dm_job_list_destroy

    integer function dm_job_list_next(job_list, job, index, disabled, revolved) result(rc)
        !! Returns copy of next enabled job. If `disabled` is `.true.`, the next
        !! job is returned regardless of state. One-time jobs are disabled by
        !! this function. If the job list has been finished and restarted from
        !! the beginning, `revolved` is set to `.true.`.
        !!
        !! Call `dm_job_list_any()` or `dm_job_list_count()` beforehand to check
        !! if the job list contains any enabled jobs. Otherwise, this function
        !! returns `E_EMPTY`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if job list is not initialised properly.
        !! * `E_EMPTY` if job list is empty or has no more jobs left.
        !!
        use :: dm_util, only: dm_present

        type(job_list_type), intent(inout)         :: job_list !! Job list.
        type(job_type),      intent(out)           :: job      !! Job.
        integer,             intent(out), optional :: index    !! Position in job list.
        logical,             intent(out), optional :: revolved !! Job list was revolved.
        logical,             intent(in),  optional :: disabled !! Return disabled job.

        integer :: i, j
        logical :: disabled_

        if (present(index))    index    = 0
        if (present(revolved)) revolved = .false.

        rc = E_NULL
        if (.not. allocated(job_list%jobs)) return
        if (.not. allocated(job_list%mask)) return

        rc = E_EMPTY
        if (size(job_list%jobs) == 0) return
        if (job_list%njobs == 0)      return

        ! At least one enabled job in job list?
        disabled_ = dm_present(disabled, .false.)
        if (.not. disabled_ .and. .not. dm_job_list_any(job_list)) return

        ! Find next job (including invalid ones).
        do i = 0, job_list%njobs - 1
            j = 1 + modulo(job_list%index + i, job_list%njobs)
            if (.not. job_list%jobs(j)%disabled .or. disabled_) exit
        end do

        if (present(revolved) .and. j <= job_list%index) then
            revolved = .true.
        end if

        if (present(index)) index = j

        job = job_list%jobs(j)
        job_list%index = j

        ! Disable one-time job.
        if (.not. job_list%jobs(j)%disabled .and. job_list%jobs(j)%onetime) then
            job_list%jobs(j)%disabled = .true.
            job_list%mask(j) = (.not. job_list%jobs(j)%disabled)
        end if

        rc = E_NONE
    end function dm_job_list_next

    pure subroutine dm_job_list_reset(job_list)
        !! Resets cursor of job list.
        type(job_list_type), intent(inout) :: job_list !! Job list.

        job_list%index = 0
    end subroutine dm_job_list_reset

    integer function dm_job_list_size(job_list) result(n)
        !! Returns size of job list array.
        type(job_list_type), intent(inout) :: job_list !! Job list.

        n = 0
        if (.not. allocated(job_list%jobs)) return
        n = size(job_list%jobs)
    end function dm_job_list_size
end module dm_job_list
