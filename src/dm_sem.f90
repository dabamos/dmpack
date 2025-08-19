! Author:  Philipp Engel
! Licence: ISC
module dm_sem
    !! Named and unnamed POSIX semaphores. Has to be linked with `-lpthread`.
    use :: unix
    use :: dm_error
    use :: dm_id
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: SEM_MODE     = int(o'0660') !! Permissions.
    integer, parameter, public :: SEM_NAME_LEN = ID_LEN + 1   !! Max. semaphore identifier length.

    type, public :: sem_named_type
        !! Named semaphore type.
        private
        character(len=SEM_NAME_LEN) :: name = ' '        !! Semaphore name (with leading `/`).
        type(c_ptr)                 :: ctx  = c_null_ptr !! C pointer to named semaphore.
    end type sem_named_type

    type, public :: sem_unnamed_type
        !! Unnamed semaphore type.
        private
        type(c_sem_t) :: ctx !! Allocated unnamed sempahore type.
    end type sem_unnamed_type

    interface dm_sem_post
        !! Post to named or unnamed sempahore.
        module procedure :: sem_post_named
        module procedure :: sem_post_unnamed
    end interface dm_sem_post

    interface dm_sem_value
        !! Get value of named or unnamed sempahore.
        module procedure :: sem_value_named
        module procedure :: sem_value_unnamed
    end interface dm_sem_value

    interface dm_sem_wait
        !! Wait for named or unnamed sempahore.
        module procedure :: sem_wait_named
        module procedure :: sem_wait_unnamed
    end interface dm_sem_wait

    public :: dm_sem_close
    public :: dm_sem_destroy
    public :: dm_sem_init
    public :: dm_sem_open
    public :: dm_sem_name
    public :: dm_sem_post
    public :: dm_sem_unlink
    public :: dm_sem_value
    public :: dm_sem_wait

    private :: sem_post_named
    private :: sem_post_unnamed
    private :: sem_value_named
    private :: sem_value_unnamed
    private :: sem_wait_named
    private :: sem_wait_unnamed
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_sem_init(sem, value) result(rc)
        !! Initialises unnamed semaphore. The function returns the following
        !! error codes:
        !!
        !! * `E_SYSTEM` if system call to close semaphore failed.
        !!
        type(sem_unnamed_type), intent(inout)        :: sem   !! Semaphore type.
        integer,                intent(in), optional :: value !! Initial value.

        integer :: value_

        value_ = dm_present(value, 0)

        rc = E_SYSTEM
        if (c_sem_init(sem%ctx, value_) /= 0) return

        rc = E_NONE
    end function dm_sem_init

    integer function dm_sem_open(sem, name, value, create, mode) result(rc)
        !! Opens and optionally creates named semaphore. The given name
        !! shall not start with a leading `/`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if name is empty or starts with `/`, or if value is negative.
        !! * `E_SYSTEM` if system call to open semaphore failed.
        !!
        use :: dm_c, only: dm_f_c_string

        type(sem_named_type), intent(inout)        :: sem    !! Semaphore type.
        character(len=*),     intent(in)           :: name   !! Semaphore name (without leading `/`).
        integer,              intent(in), optional :: value  !! Initial value.
        logical,              intent(in), optional :: create !! Create semaphore.
        integer,              intent(in), optional :: mode   !! Permissions.

        integer :: flag, mode_, value_
        logical :: create_

        value_  = dm_present(value,  0)
        create_ = dm_present(create, .false.)
        mode_   = dm_present(mode,   SEM_MODE)

        flag = 0
        if (create_) flag = ior(flag, O_CREAT)

        rc = E_INVALID
        if (value_ < 0)          return
        if (len_trim(name) == 0) return
        if (name(1:1) == '/')    return

        sem%name = '/' // name

        rc = E_SYSTEM
        sem%ctx = c_sem_open(name  = dm_f_c_string(sem%name), &
                             oflag = flag, &
                             mode  = int(mode_, kind=c_mode_t), &
                             value = value_)
        if (.not. c_associated(sem%ctx)) return

        rc = E_NONE
    end function dm_sem_open

    function dm_sem_name(sem) result(name)
        !! Returns the name of the semaphore.
        type(sem_named_type), intent(inout) :: sem  !! Semaphore type.
        character(len=:), allocatable       :: name !! Semaphore name.

        name = trim(sem%name)
    end function dm_sem_name

    subroutine dm_sem_close(sem, error)
        !! Closes named semaphore. On error, the routine sets argument `error`
        !! to the following error codes:
        !!
        !! * `E_NULL` if semaphore pointer is not associated.
        !! * `E_SYSTEM` if system call to close semaphore failed.
        !!
        type(sem_named_type), intent(inout) :: sem !! Semaphore type.
        integer,              intent(out), optional :: error  !! Error code.

        integer :: rc

        sem_block: block
            rc = E_NULL
            if (.not. c_associated(sem%ctx)) exit sem_block

            rc = E_SYSTEM
            if (c_sem_close(sem%ctx) == 0) rc = E_NONE
        end block sem_block

        if (present(error)) error = rc
    end subroutine dm_sem_close

    subroutine dm_sem_destroy(sem, error)
        !! Destroys unnamed semaphore. The routine sets argument `error` to
        !! `E_SYSTEM` on error.
        type(sem_unnamed_type), intent(inout)         :: sem   !! Semaphore type.
        integer,                intent(out), optional :: error !! Error code.

        if (present(error)) error = E_SYSTEM
        if (c_sem_destroy(sem%ctx) /= 0) return
        if (present(error)) error = E_NONE
    end subroutine dm_sem_destroy

    subroutine dm_sem_unlink(sem, error)
        !! Unlinks named semaphore. Sets argument `error` to `E_SYSTEM` on error.
        use :: dm_c, only: dm_f_c_string

        type(sem_named_type), intent(inout)         :: sem   !! Semaphore type.
        integer,              intent(out), optional :: error !! Error code.

        if (present(error)) error = E_SYSTEM
        if (c_sem_unlink(dm_f_c_string(sem%name)) /= 0) return
        if (present(error)) error = E_NONE
    end subroutine dm_sem_unlink

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function sem_post_named(sem) result(rc)
        !! Increases semaphore value. Returns `E_SYSTEM` on error.
        type(sem_named_type), intent(inout) :: sem !! Semaphore type.

        rc = E_SYSTEM
        if (c_sem_post(sem%ctx) /= 0) return
        rc = E_NONE
    end function sem_post_named

    integer function sem_post_unnamed(sem) result(rc)
        !! Increases semaphore value. Returns `E_SYSTEM` on error.
        type(sem_unnamed_type), target, intent(inout) :: sem !! Semaphore type.

        rc = E_SYSTEM
        if (c_sem_post(c_loc(sem%ctx)) /= 0) return
        rc = E_NONE
    end function sem_post_unnamed

    integer function sem_value_named(sem, value) result(rc)
        !! Returns the current semaphore value. Returns `E_SYSTEM` on error.
        type(sem_named_type), intent(inout) :: sem   !! Semaphore type.
        integer,              intent(out)   :: value !! Returned value.

        rc = E_SYSTEM
        if (c_sem_getvalue(sem%ctx, value) /= 0) return
        rc = E_NONE
    end function sem_value_named

    integer function sem_value_unnamed(sem, value) result(rc)
        !! Returns the current semaphore value. Returns `E_SYSTEM` on error.
        type(sem_unnamed_type), target, intent(inout) :: sem   !! Semaphore type.
        integer,                        intent(out)   :: value !! Returned value.

        rc = E_SYSTEM
        if (c_sem_getvalue(c_loc(sem%ctx), value) /= 0) return
        rc = E_NONE
    end function sem_value_unnamed

    integer function sem_wait_named(sem) result(rc)
        !! Waits for semaphore. Returns `E_SYSTEM` on error.
        type(sem_named_type), intent(inout) :: sem !! Semaphore type.

        rc = E_SYSTEM
        if (c_sem_wait(sem%ctx) /= 0) return
        rc = E_NONE
    end function sem_wait_named

    integer function sem_wait_unnamed(sem) result(rc)
        !! Waits for semaphore. Returns `E_SYSTEM` on error.
        type(sem_unnamed_type), target, intent(inout) :: sem !! Semaphore type.

        rc = E_SYSTEM
        if (c_sem_wait(c_loc(sem%ctx)) /= 0) return
        rc = E_NONE
    end function sem_wait_unnamed
end module dm_sem
