! Author:  Philipp Engel
! Licence: ISC
module dm_sem
    !! Named POSIX semaphores. Has to be linked with `-lpthread`.
    use, intrinsic :: iso_c_binding, only: c_null_ptr
    use :: unix
    use :: dm_error
    implicit none (type, external)
    private

    integer, parameter, public :: SEM_MODE     = int(o'0660') !! Permissions.
    integer, parameter, public :: SEM_NAME_LEN = 32           !! Max. semaphore identifier length.

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
        !! Closes named semaphore.
        type(sem_type), intent(inout) :: sem !! Semaphore type.

        rc = E_INVALID
        if (.not. c_associated(sem%ptr)) return

        rc = E_SYSTEM
        if (c_sem_close(sem%ptr) /= 0) return

        rc = E_NONE
    end function dm_sem_close

    integer function dm_sem_open(sem, name, value, create, mode) result(rc)
        !! Opens and optionally creates named semaphore. The given name
        !! shall not start with a leading `/`.
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
        if (value_ < 0) return
        if (len_trim(name) == 0) return
        if (name(1:1) == '/') return
        sem%name = '/' // name

        rc = E_SYSTEM
        sem%ptr = c_sem_open(trim(sem%name) // c_null_char, flag, mode_, value_)
        if (.not. c_associated(sem%ptr)) return
        rc = E_NONE
    end function dm_sem_open

    function dm_sem_name(sem) result(name)
        !! Returns the name of the semaphore.
        type(sem_type), intent(inout) :: sem !! Semaphore type.
        character(len=:), allocatable :: name

        name = trim(sem%name)
    end function dm_sem_name

    integer function dm_sem_post(sem) result(rc)
        !! Increases semaphore value.
        type(sem_type), intent(inout) :: sem !! Semaphore type.

        rc = E_SYSTEM
        if (c_sem_post(sem%ptr) /= 0) return
        rc = E_NONE
    end function dm_sem_post

    integer function dm_sem_unlink(sem) result(rc)
        !! Unlinks named semaphore.
        type(sem_type), intent(inout) :: sem !! Semaphore type.

        rc = E_SYSTEM
        if (c_sem_unlink(trim(sem%name) // c_null_char) /= 0) return
        rc = E_NONE
    end function dm_sem_unlink

    integer function dm_sem_value(sem, value) result(rc)
        !! Returns the current semaphore value.
        type(sem_type), intent(inout) :: sem   !! Semaphore type.
        integer,        intent(out)   :: value !! Returned value.

        rc = E_SYSTEM
        if (c_sem_getvalue(sem%ptr, value) /= 0) return
        rc = E_NONE
    end function dm_sem_value

    integer function dm_sem_wait(sem) result(rc)
        !! Waits for semaphore.
        type(sem_type), intent(inout) :: sem !! Semaphore type.

        rc = E_SYSTEM
        if (c_sem_wait(sem%ptr) /= 0) return
        rc = E_NONE
    end function dm_sem_wait
end module dm_sem
