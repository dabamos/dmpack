! Author:  Philipp Engel
! Licence: ISC
module dm_posix_sem
    !! Named and unnamed POSIX semaphores. Has to be linked with `-lpthread`.
    use :: unix
    use :: dm_error
    use :: dm_id
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: POSIX_SEM_MODE     = int(o'0660') !! Permissions.
    integer, parameter, public :: POSIX_SEM_NAME_LEN = ID_LEN + 1   !! Max. semaphore identifier length.

    type, public :: posix_sem_named_type
        !! Named semaphore type.
        private
        character(POSIX_SEM_NAME_LEN) :: name    = ' '        !! Semaphore name (with leading `/`).
        type(c_ptr)                   :: context = c_null_ptr !! C pointer to named semaphore.
    end type posix_sem_named_type

    type, public :: posix_sem_unnamed_type
        !! Unnamed semaphore type.
        private
        type(c_sem_t) :: context !! Allocated unnamed sempahore type.
    end type posix_sem_unnamed_type

    interface dm_posix_sem_post
        !! Post to named or unnamed sempahore.
        module procedure :: posix_sem_post_named
        module procedure :: posix_sem_post_unnamed
    end interface dm_posix_sem_post

    interface dm_posix_sem_value
        !! Get value of named or unnamed sempahore.
        module procedure :: posix_sem_value_named
        module procedure :: posix_sem_value_unnamed
    end interface dm_posix_sem_value

    interface dm_posix_sem_wait
        !! Wait for named or unnamed sempahore.
        module procedure :: posix_sem_wait_named
        module procedure :: posix_sem_wait_unnamed
    end interface dm_posix_sem_wait

    public :: dm_posix_sem_close
    public :: dm_posix_sem_destroy
    public :: dm_posix_sem_init
    public :: dm_posix_sem_open
    public :: dm_posix_sem_name
    public :: dm_posix_sem_post
    public :: dm_posix_sem_unlink
    public :: dm_posix_sem_value
    public :: dm_posix_sem_wait

    private :: posix_sem_post_named
    private :: posix_sem_post_unnamed
    private :: posix_sem_value_named
    private :: posix_sem_value_unnamed
    private :: posix_sem_wait_named
    private :: posix_sem_wait_unnamed
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_posix_sem_init(sem, value) result(rc)
        !! Initialises unnamed semaphore. The function returns the following
        !! error codes:
        !!
        !! * `E_SYSTEM` if system call to close semaphore failed.
        !!
        type(posix_sem_unnamed_type), intent(inout)        :: sem   !! Semaphore.
        integer,                      intent(in), optional :: value !! Initial value.

        integer :: value_

        value_ = dm_present(value, 0)

        rc = E_SYSTEM
        if (c_sem_init(sem%context, value_) == 0) rc = E_NONE
    end function dm_posix_sem_init

    integer function dm_posix_sem_open(sem, name, value, create, mode) result(rc)
        !! Opens and optionally creates named semaphore. The given name
        !! shall not start with a leading `/`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if name is empty or starts with `/`, or if value is negative.
        !! * `E_SYSTEM` if system call to open semaphore failed.
        !!
        use :: dm_c, only: dm_f_c_string

        type(posix_sem_named_type), intent(inout)        :: sem    !! Semaphore.
        character(*),               intent(in)           :: name   !! Semaphore name (without leading `/`).
        integer,                    intent(in), optional :: value  !! Initial value.
        logical,                    intent(in), optional :: create !! Create semaphore.
        integer,                    intent(in), optional :: mode   !! Permissions.

        integer :: flag, mode_, value_
        logical :: create_

        value_  = dm_present(value,  0)
        create_ = dm_present(create, .false.)
        mode_   = dm_present(mode,   POSIX_SEM_MODE)

        flag = 0
        if (create_) flag = ior(flag, O_CREAT)

        rc = E_INVALID
        if (value_ < 0)          return
        if (len_trim(name) == 0) return
        if (name(1:1) == '/')    return

        sem%name = '/' // name

        rc = E_SYSTEM
        sem%context = c_sem_open(name  = dm_f_c_string(sem%name), &
                             oflag = flag, &
                             mode  = int(mode_, c_mode_t), &
                             value = value_)
        if (c_associated(sem%context)) rc = E_NONE
    end function dm_posix_sem_open

    function dm_posix_sem_name(sem) result(name)
        !! Returns the name of the semaphore without leading `/`.
        type(posix_sem_named_type), intent(inout) :: sem  !! Semaphore.
        character(:), allocatable                 :: name !! Semaphore name.

        name = trim(sem%name(2:))
    end function dm_posix_sem_name

    subroutine dm_posix_sem_close(sem, error)
        !! Closes named semaphore. On error, the routine sets argument `error`
        !! to the following error codes:
        !!
        !! * `E_NULL` if semaphore pointer is not associated.
        !! * `E_SYSTEM` if system call to close semaphore failed.
        !!
        type(posix_sem_named_type), intent(inout)         :: sem   !! Semaphore.
        integer,                    intent(out), optional :: error !! Error code.

        integer :: rc

        sem_block: block
            rc = E_NULL
            if (.not. c_associated(sem%context)) exit sem_block

            rc = E_SYSTEM
            if (c_sem_close(sem%context) == 0) rc = E_NONE
        end block sem_block

        if (present(error)) error = rc
    end subroutine dm_posix_sem_close

    subroutine dm_posix_sem_destroy(sem, error)
        !! Destroys unnamed semaphore. The routine sets argument `error` to
        !! `E_SYSTEM` on error.
        type(posix_sem_unnamed_type), intent(inout)         :: sem   !! Semaphore.
        integer,                      intent(out), optional :: error !! Error code.

        if (present(error)) error = E_SYSTEM
        if (c_sem_destroy(sem%context) /= 0) return
        if (present(error)) error = E_NONE
    end subroutine dm_posix_sem_destroy

    subroutine dm_posix_sem_unlink(sem, error)
        !! Unlinks named semaphore. Sets argument `error` to `E_SYSTEM` on error.
        use :: dm_c, only: dm_f_c_string

        type(posix_sem_named_type), intent(inout)         :: sem   !! Semaphore.
        integer,                    intent(out), optional :: error !! Error code.

        if (present(error)) error = E_SYSTEM
        if (c_sem_unlink(dm_f_c_string(sem%name)) /= 0) return
        if (present(error)) error = E_NONE
    end subroutine dm_posix_sem_unlink

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function posix_sem_post_named(sem) result(rc)
        !! Increases semaphore value. Returns `E_SYSTEM` on error.
        type(posix_sem_named_type), intent(inout) :: sem !! Semaphore.

        rc = E_SYSTEM
        if (c_sem_post(sem%context) == 0) rc = E_NONE
    end function posix_sem_post_named

    integer function posix_sem_post_unnamed(sem) result(rc)
        !! Increases semaphore value. Returns `E_SYSTEM` on error.
        type(posix_sem_unnamed_type), target, intent(inout) :: sem !! Semaphore.

        rc = E_SYSTEM
        if (c_sem_post(c_loc(sem%context)) == 0) rc = E_NONE
    end function posix_sem_post_unnamed

    integer function posix_sem_value_named(sem, value) result(rc)
        !! Returns the current semaphore value. Returns `E_SYSTEM` on error.
        type(posix_sem_named_type), intent(inout) :: sem   !! Semaphore.
        integer,                    intent(out)   :: value !! Returned value.

        rc = E_SYSTEM
        if (c_sem_getvalue(sem%context, value) == 0) rc = E_NONE
    end function posix_sem_value_named

    integer function posix_sem_value_unnamed(sem, value) result(rc)
        !! Returns the current semaphore value. Returns `E_SYSTEM` on error.
        type(posix_sem_unnamed_type), target, intent(inout) :: sem   !! Semaphore.
        integer,                              intent(out)   :: value !! Returned value.

        rc = E_SYSTEM
        if (c_sem_getvalue(c_loc(sem%context), value) == 0) rc = E_NONE
    end function posix_sem_value_unnamed

    integer function posix_sem_wait_named(sem) result(rc)
        !! Waits for semaphore. Returns `E_SYSTEM` on error.
        type(posix_sem_named_type), intent(inout) :: sem !! Semaphore.

        rc = E_SYSTEM
        if (c_sem_wait(sem%context) == 0) rc = E_NONE
    end function posix_sem_wait_named

    integer function posix_sem_wait_unnamed(sem) result(rc)
        !! Waits for semaphore. Returns `E_SYSTEM` on error.
        type(posix_sem_unnamed_type), target, intent(inout) :: sem !! Semaphore.

        rc = E_SYSTEM
        if (c_sem_wait(c_loc(sem%context)) == 0) rc = E_NONE
    end function posix_sem_wait_unnamed
end module dm_posix_sem
