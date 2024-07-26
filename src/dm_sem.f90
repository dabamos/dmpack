! Author:  Philipp Engel
! Licence: ISC
module dm_sem
    !! Named POSIX semaphores. Has to be linked with `-lpthread`.
    use :: unix
    use :: dm_error
    use :: dm_id
    implicit none (type, external)
    private

    integer, parameter, public :: SEM_MODE     = int(o'0660') !! Permissions.
    integer, parameter, public :: SEM_NAME_LEN = ID_LEN + 1   !! Max. semaphore identifier length.

    type, public :: sem_type
        !! Opaque named semaphore type.
        private
        character(len=SEM_NAME_LEN) :: name = ' '        !! Semaphore name (with leading `/`).
        type(c_ptr)                 :: ptr  = c_null_ptr !! C pointer to named semaphore.
    end type sem_type

    public :: dm_sem_close
    public :: dm_sem_open
    public :: dm_sem_name
    public :: dm_sem_post
    public :: dm_sem_unlink
    public :: dm_sem_value
    public :: dm_sem_wait
contains
    integer function dm_sem_close(sem) result(rc)
        !! Closes named semaphore. The function returns the following error
        !! codes:
        !!
        !! * `E_NULL` if semaphore pointer is not associated.
        !! * `E_SYSTEM` if system call to close semaphore failed.
        !!
        type(sem_type), intent(inout) :: sem !! Semaphore type.

        rc = E_NULL
        if (.not. c_associated(sem%ptr)) return

        rc = E_SYSTEM
        if (c_sem_close(sem%ptr) /= 0) return

        rc = E_NONE
    end function dm_sem_close

    integer function dm_sem_open(sem, name, value, create, mode) result(rc)
        !! Opens and optionally creates named semaphore. The given name
        !! shall not start with a leading `/`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if name is empty or starts with `/`, or if value is negative.
        !! * `E_SYSTEM` if system call to open semaphore failed.
        !!
        type(sem_type),   intent(inout)        :: sem    !! Semaphore type.
        character(len=*), intent(in)           :: name   !! Semaphore name (without leading `/`).
        integer,          intent(in), optional :: value  !! Initial value.
        logical,          intent(in), optional :: create !! Create semaphore.
        integer,          intent(in), optional :: mode   !! Permissions.

        integer :: flag, mode_, value_
        logical :: create_

        value_ = 0
        if (present(value)) value_ = value

        create_ = .false.
        if (present(create)) create_ = create

        mode_ = SEM_MODE
        if (present(mode)) mode_ = mode

        flag = 0
        if (create_) flag = ior(flag, O_CREAT)

        rc = E_INVALID
        if (value_ < 0)          return
        if (len_trim(name) == 0) return
        if (name(1:1) == '/')    return

        sem%name = '/' // name

        rc = E_SYSTEM
        sem%ptr = c_sem_open(name  = trim(sem%name) // c_null_char, &
                             oflag = flag, &
                             mode  = int(mode_, kind=c_mode_t), &
                             value = value_)
        if (.not. c_associated(sem%ptr)) return

        rc = E_NONE
    end function dm_sem_open

    function dm_sem_name(sem) result(name)
        !! Returns the name of the semaphore.
        type(sem_type), intent(inout) :: sem  !! Semaphore type.
        character(len=:), allocatable :: name !! Semaphore name.

        name = trim(sem%name)
    end function dm_sem_name

    integer function dm_sem_post(sem) result(rc)
        !! Increases semaphore value. Returns `E_SYSTEM` on error.
        type(sem_type), intent(inout) :: sem !! Semaphore type.

        rc = E_SYSTEM
        if (c_sem_post(sem%ptr) /= 0) return
        rc = E_NONE
    end function dm_sem_post

    integer function dm_sem_unlink(sem) result(rc)
        !! Unlinks named semaphore. Returns `E_SYSTEM` on error.
        type(sem_type), intent(inout) :: sem !! Semaphore type.

        rc = E_SYSTEM
        if (c_sem_unlink(trim(sem%name) // c_null_char) /= 0) return
        rc = E_NONE
    end function dm_sem_unlink

    integer function dm_sem_value(sem, value) result(rc)
        !! Returns the current semaphore value. Returns `E_SYSTEM` on error.
        type(sem_type), intent(inout) :: sem   !! Semaphore type.
        integer,        intent(out)   :: value !! Returned value.

        rc = E_SYSTEM
        if (c_sem_getvalue(sem%ptr, value) /= 0) return
        rc = E_NONE
    end function dm_sem_value

    integer function dm_sem_wait(sem) result(rc)
        !! Waits for semaphore. Returns `E_SYSTEM` on error.
        type(sem_type), intent(inout) :: sem !! Semaphore type.

        rc = E_SYSTEM
        if (c_sem_wait(sem%ptr) /= 0) return
        rc = E_NONE
    end function dm_sem_wait
end module dm_sem
