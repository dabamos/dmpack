! Author:  Philipp Engel
! Licence: ISC
module dm_posix_mqueue
    !! Module for inter-process communication (IPC) and message passing through
    !! POSIX message queues. Has to be linked with `-lrt`.
    use :: unix
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: POSIX_MQUEUE_MODE     = int(o'0644') !! Default permissions (octal).
    integer, parameter, public :: POSIX_MQUEUE_NAME_LEN = ID_LEN + 1   !! Maximum message queue identifier length.
    integer, parameter, public :: POSIX_MQUEUE_MAX_MSG  = 10           !! Maximum number of messages in queue.

    integer, parameter, public :: POSIX_MQUEUE_RDONLY = 0 !! Read-only access.
    integer, parameter, public :: POSIX_MQUEUE_WRONLY = 1 !! Write-only access.
    integer, parameter, public :: POSIX_MQUEUE_RDWR   = 2 !! Read/write access.

    type, public :: posix_mqueue_type
        !! Opaque POSIX message queue type.
        private
        character(POSIX_MQUEUE_NAME_LEN) :: name = ' '       !! Message queue name (with leading `/`).
        integer(c_mqd_t)                 :: mqd  = 0_c_mqd_t !! C message queue descriptor.
    end type posix_mqueue_type

    interface dm_posix_mqueue_open
        !! Generic message queue open function.
        procedure :: posix_mqueue_open_raw
        procedure :: posix_mqueue_open_type
    end interface dm_posix_mqueue_open

    interface dm_posix_mqueue_read
        !! Generic message queue read function.
        procedure :: posix_mqueue_read_log
        procedure :: posix_mqueue_read_observ
        procedure :: posix_mqueue_read_raw
    end interface dm_posix_mqueue_read

    interface dm_posix_mqueue_write
        !! Generic message queue write function.
        procedure :: posix_mqueue_write_log
        procedure :: posix_mqueue_write_observ
        procedure :: posix_mqueue_write_raw
    end interface dm_posix_mqueue_write

    ! Public procedures.
    public :: dm_posix_mqueue_attributes
    public :: dm_posix_mqueue_close
    public :: dm_posix_mqueue_open
    public :: dm_posix_mqueue_name
    public :: dm_posix_mqueue_read
    public :: dm_posix_mqueue_unlink
    public :: dm_posix_mqueue_write

    ! Private procedures.
    private :: posix_mqueue_open_raw
    private :: posix_mqueue_open_type
    private :: posix_mqueue_read_log
    private :: posix_mqueue_read_observ
    private :: posix_mqueue_read_raw
    private :: posix_mqueue_write_log
    private :: posix_mqueue_write_observ
    private :: posix_mqueue_write_raw
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_posix_mqueue_attributes(mqueue, flags, max_msg, msg_size, cur_msgs) result(rc)
        !! Returns message queue attributes.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if message queue descriptor is invalid.
        !! * `E_MQUEUE` if system call to get the attributes failed.
        !!
        type(posix_mqueue_type), intent(inout)         :: mqueue   !! Message queue.
        integer(i8),             intent(out), optional :: flags    !! Flags.
        integer(i8),             intent(out), optional :: max_msg  !! Maximum number of messages in queue.
        integer(i8),             intent(out), optional :: msg_size !! Message size.
        integer(i8),             intent(out), optional :: cur_msgs !! Current number of messages in queue.

        type(c_mq_attr) :: attr

        rc = E_INVALID
        if (mqueue%mqd <= 0) return

        rc = E_MQUEUE
        if (c_mq_getattr(mqueue%mqd, attr) < 0) return

        if (present(flags))    flags    = int(attr%mq_flags,   i8)
        if (present(max_msg))  max_msg  = int(attr%mq_maxmsg,  i8)
        if (present(msg_size)) msg_size = int(attr%mq_msgsize, i8)
        if (present(cur_msgs)) cur_msgs = int(attr%mq_curmsgs, i8)

        rc = E_NONE
    end function dm_posix_mqueue_attributes

    function dm_posix_mqueue_name(mqueue) result(name)
        !! Returns message queue name without leading `/` as allocatable
        !! character string.
        type(posix_mqueue_type), intent(inout) :: mqueue !! Message queue.
        character(:), allocatable        :: name   !! Name.

        name = trim(mqueue%name(2:))
    end function dm_posix_mqueue_name

    subroutine dm_posix_mqueue_close(mqueue, error)
        !! Closes message queue.
        !!
        !! The routine returns the following error codes in `error`:
        !!
        !! * `E_INVALID` if message queue descriptor is invalid.
        !! * `E_MQUEUE` if system call to close the queue failed.
        !!
        type(posix_mqueue_type), intent(inout)         :: mqueue !! Message queue.
        integer,                 intent(out), optional :: error  !! Error code.

        integer :: rc

        mq_block: block
            rc = E_NONE
            if (mqueue%mqd == 0) exit mq_block

            rc = E_INVALID
            if (mqueue%mqd < 0) exit mq_block

            rc = E_MQUEUE
            if (c_mq_close(mqueue%mqd) == 0) rc = E_NONE
        end block mq_block

        if (present(error)) error = rc
    end subroutine dm_posix_mqueue_close

    subroutine dm_posix_mqueue_unlink(mqueue, error)
        !! Deletes POSIX message queue.
        !!
        !! The routine returns the following error codes in `error`:
        !!
        !! * `E_INVALID` if message queue has no name.
        !! * `E_MQUEUE` if system call to unlink the queue failed.
        !!
        use :: dm_c, only: dm_f_c_string

        type(posix_mqueue_type), intent(inout)         :: mqueue !! Message queue.
        integer,                 intent(out), optional :: error  !! Error code.

        integer :: rc

        mq_block: block
            rc = E_INVALID
            if (len_trim(mqueue%name) == 0) exit mq_block

            rc = E_MQUEUE
            if (c_mq_unlink(dm_f_c_string(mqueue%name)) == 0) rc = E_NONE
        end block mq_block

        if (present(error)) error = rc
    end subroutine dm_posix_mqueue_unlink

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function posix_mqueue_open_raw(mqueue, name, max_msg, msg_size, access, mode, &
                                           create, exclusive, blocking) result(rc)
        !! Opens POSIX message queue of given name.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if name is empty or starts with `/`.
        !! * `E_MQUEUE` if system call to open the queue failed.
        !!
        use :: dm_c, only: dm_f_c_string

        type(posix_mqueue_type), intent(out)          :: mqueue    !! Message queue.
        character(*),            intent(in)           :: name      !! Message queue name (without leading `/`).
        integer,                 intent(in)           :: max_msg   !! Maximum number of messages in queue.
        integer,                 intent(in)           :: msg_size  !! Message size.
        integer,                 intent(in), optional :: access    !! Access type (`POSIX_MQUEUE_RDONLY`, `POSIX_MQUEUE_WRONLY`, `POSIX_MQUEUE_RDWR`).
        integer,                 intent(in), optional :: mode      !! Access permissions.
        logical,                 intent(in), optional :: create    !! Creates message queue if true.
        logical,                 intent(in), optional :: exclusive !! Opens message queue exclusively if true.
        logical,                 intent(in), optional :: blocking  !! Blocking access if true.

        integer                 :: access_, flag, mode_
        logical                 :: create_, exclusive_, blocking_
        type(c_mq_attr), target :: attr

        ! MQ name.
        rc = E_INVALID
        if (len_trim(name) == 0) return
        if (name(1:1) == '/')    return

        access_ = dm_present(access, POSIX_MQUEUE_WRONLY)
        mode_   = dm_present(mode,   POSIX_MQUEUE_MODE)

        create_    = dm_present(create,    .false.)
        exclusive_ = dm_present(exclusive, .false.)
        blocking_  = dm_present(blocking,  .true.)

        mqueue%name = '/' // name

        ! MQ access type.
        select case (access_)
            case (POSIX_MQUEUE_RDONLY); flag = O_RDONLY
            case (POSIX_MQUEUE_WRONLY); flag = O_WRONLY
            case (POSIX_MQUEUE_RDWR);   flag = O_RDWR
            case default;         flag = O_WRONLY
        end select

        ! MQ flags.
        if (create_)         flag = ior(flag, O_CREAT)
        if (exclusive_)      flag = ior(flag, O_EXCL)
        if (.not. blocking_) flag = ior(flag, O_NONBLOCK)

        attr%mq_maxmsg  = int(max_msg, c_long)
        attr%mq_msgsize = int(msg_size, c_long)

        rc = E_MQUEUE
        mqueue%mqd = c_mq_open(name  = dm_f_c_string(mqueue%name), &
                               oflag = flag, &
                               mode  = int(mode_, c_mode_t), &
                               attr  = c_loc(attr))
        if (mqueue%mqd < 0) return

        rc = E_NONE
    end function posix_mqueue_open_raw

    integer function posix_mqueue_open_type(mqueue, type, name, access, blocking) result(rc)
        !! Opens message queue for reading/writing logs or observations (in
        !! blocking mode by default).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if type is invalid, name is empty, or name starts with `/`.
        !! * `E_MQUEUE` if system call to open the queue failed.
        !!
        use :: dm_log,    only: LOG_TYPE_SIZE
        use :: dm_observ, only: OBSERV_TYPE_SIZE
        use :: dm_type

        type(posix_mqueue_type), intent(out)          :: mqueue   !! Message queue.
        integer,                 intent(in)           :: type     !! Data type (`TYPE_LOG`, `TYPE_OBSERV`).
        character(*),            intent(in)           :: name     !! Message queue name (without leading `/`).
        integer,                 intent(in)           :: access   !! `POSIX_MQUEUE_RDONLY`, `POSIX_MQUEUE_WRONLY`, `POSIX_MQUEUE_RDWR`.
        logical,                 intent(in), optional :: blocking !! Blocking access if true.

        integer :: msg_size
        integer :: access_
        logical :: blocking_

        rc = E_INVALID
        blocking_ = dm_present(blocking, .true.)

        select case (type)
            case (TYPE_LOG);    msg_size = LOG_TYPE_SIZE
            case (TYPE_OBSERV); msg_size = OBSERV_TYPE_SIZE
            case default;       return
        end select

        select case (access)
            case (POSIX_MQUEUE_RDONLY, &
                  POSIX_MQUEUE_WRONLY, &
                  POSIX_MQUEUE_RDWR); access_ = access
            case default;             return
        end select

        rc = dm_posix_mqueue_open(mqueue   = mqueue,               & ! Message queue type.
                                  name     = trim(name),           & ! Name without slash.
                                  max_msg  = POSIX_MQUEUE_MAX_MSG, & ! Max. number of messages.
                                  msg_size = msg_size,             & ! Max. message size in bytes.
                                  access   = access_,              & ! Read-only, write-only, or read-write access.
                                  mode     = POSIX_MQUEUE_MODE,    & ! Permissions.
                                  create   = .true.,               & ! Create message queue.
                                  blocking = blocking_)              ! Access is blocking.
    end function posix_mqueue_open_type

    integer function posix_mqueue_read_log(mqueue, log, timeout) result(rc)
        !! Receives log from message queue. The received message shall not be
        !! larger than parameter `LOG_TYPE_SIZE`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the message queue is empty (non-blocking).
        !! * `E_INVALID` if buffer size is less than specified message size.
        !! * `E_LIMIT` if buffer is too small for message.
        !! * `E_MQUEUE` if system call to receive message failed.
        !! * `E_SYSTEM` if system call to get time failed.
        !! * `E_TIMEOUT` if an timeout occured.
        !!
        use :: dm_log

        type(posix_mqueue_type), intent(inout)        :: mqueue  !! Message queue.
        type(log_type),          intent(out)          :: log     !! Log.
        integer(i8),             intent(in), optional :: timeout !! Timeout in seconds.

        character(LOG_TYPE_SIZE) :: buffer

        rc = posix_mqueue_read_raw(mqueue, buffer, timeout=timeout)
        if (dm_is_error(rc)) return
        log = transfer(buffer, log)
    end function posix_mqueue_read_log

    integer function posix_mqueue_read_observ(mqueue, observ, timeout) result(rc)
        !! Receives observation from message queue. The received message shall
        !! not be larger than parameter `OBSERV_TYPE_SIZE`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the message queue is empty (non-blocking).
        !! * `E_INVALID` if buffer size is less than specified message size.
        !! * `E_LIMIT` if buffer is too small for message.
        !! * `E_MQUEUE` if system call to receive message failed.
        !! * `E_SYSTEM` if system call to get time failed.
        !! * `E_TIMEOUT` if an timeout occured.
        !!
        use :: dm_observ

        type(posix_mqueue_type), intent(inout)        :: mqueue  !! Message queue.
        type(observ_type),       intent(out)          :: observ  !! Observation.
        integer(i8),             intent(in), optional :: timeout !! Timeout in seconds.

        character(OBSERV_TYPE_SIZE) :: buffer

        rc = posix_mqueue_read_raw(mqueue, buffer, timeout=timeout)
        if (dm_is_error(rc)) return
        observ = transfer(buffer, observ)
    end function posix_mqueue_read_observ

    integer function posix_mqueue_read_raw(mqueue, buffer, priority, timeout) result(rc)
        !! Receives message from message queue and returns data in `buffer`.
        !! The buffer size must equal the message size.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_AGAIN` if the message queue is empty (non-blocking).
        !! * `E_INVALID` if buffer size is less than specified message size.
        !! * `E_LIMIT` if buffer is too small for message.
        !! * `E_MQUEUE` if system call to receive message failed.
        !! * `E_SYSTEM` if system call to get time failed.
        !! * `E_TIMEOUT` if an timeout occured.
        !!
        type(posix_mqueue_type), intent(inout)         :: mqueue   !! Message queue.
        character(*),            intent(inout)         :: buffer   !! Byte buffer.
        integer,                 intent(out), optional :: priority !! Message priority.
        integer(i8),             intent(in),  optional :: timeout  !! Timeout in seconds.

        integer           :: priority_
        integer(c_size_t) :: sz
        type(c_timespec)  :: ts

        if (present(timeout)) then
            rc = E_SYSTEM
            if (c_clock_gettime(CLOCK_REALTIME, ts) /= 0) return
            ts%tv_sec = ts%tv_sec + timeout

            sz = c_mq_timedreceive(mqueue%mqd, buffer, len(buffer, c_size_t), priority_, ts)
        else
            sz = c_mq_receive(mqueue%mqd, buffer, len(buffer, c_size_t), priority_)
        end if

        if (present(priority)) priority = priority_

        if (sz >= 0) then
            rc = E_NONE
            return
        end if

        select case (c_errno())
            case (EAGAIN);    rc = E_AGAIN
            case (ETIMEDOUT); rc = E_TIMEOUT
            case (EINVAL);    rc = E_INVALID
            case (EMSGSIZE);  rc = E_LIMIT
            case default;     rc = E_MQUEUE
        end select
    end function posix_mqueue_read_raw

    integer function posix_mqueue_write_log(mqueue, log) result(rc)
        !! Sends log message to message queue. Returns `E_MQUEUE` on error.
        use :: dm_log

        type(posix_mqueue_type), intent(inout) :: mqueue !! Message queue.
        type(log_type),          intent(inout) :: log    !! Log.

        character(LOG_TYPE_SIZE) :: buffer

        buffer = transfer(log, buffer)
        rc = posix_mqueue_write_raw(mqueue, buffer)
    end function posix_mqueue_write_log

    integer function posix_mqueue_write_observ(mqueue, observ) result(rc)
        !! Sends observation to message queue. Returns `E_MQUEUE` on error.
        use :: dm_observ

        type(posix_mqueue_type), intent(inout) :: mqueue !! Message queue.
        type(observ_type),       intent(inout) :: observ !! Observation.

        character(OBSERV_TYPE_SIZE) :: buffer

        buffer = transfer(observ, buffer)
        rc = posix_mqueue_write_raw(mqueue, buffer, priority=observ%priority)
    end function posix_mqueue_write_observ

    integer function posix_mqueue_write_raw(mqueue, buffer, priority) result(rc)
        !! Sends log to message queue. Returns `E_MQUEUE` on error.
        type(posix_mqueue_type), intent(inout)        :: mqueue   !! Message queue.
        character(*),            intent(inout)        :: buffer   !! Byte buffer
        integer,                 intent(in), optional :: priority !! Priority

        integer :: priority_

        rc = E_MQUEUE
        priority_ = dm_present(priority, 0)
        if (c_mq_send(mqueue%mqd, buffer, len(buffer, c_size_t), priority_) < 0) return
        rc = E_NONE
    end function posix_mqueue_write_raw
end module dm_posix_mqueue
