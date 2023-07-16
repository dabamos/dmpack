! Author:  Philipp Engel
! Licence: ISC
module dm_mqueue
    !! Module for inter-process communication (IPC) and message passing through
    !! POSIX message queues.
    use, intrinsic :: iso_c_binding, only: c_loc, c_null_char
    use :: unix
    use :: dm_error
    use :: dm_id
    use :: dm_log
    use :: dm_observ
    use :: dm_type
    implicit none (type, external)
    private

    integer,          parameter, public :: MQUEUE_MODE     = int(o'0644') !! Default permissions (octal).
    integer,          parameter, public :: MQUEUE_NAME_LEN = ID_LEN       !! Maximum message queue identifier length.
    integer(kind=i8), parameter, public :: MQUEUE_MAX_MSG  = 8            !! Maximum number of messages in queue.

    integer, parameter, public :: MQUEUE_RDONLY = 0 !! Read-only access.
    integer, parameter, public :: MQUEUE_WRONLY = 1 !! Write-only access.
    integer, parameter, public :: MQUEUE_RDWR   = 2 !! Read/write access.

    type, public :: mqueue_type
        !! Opaque POSIX message queue type.
        private
        character(len=MQUEUE_NAME_LEN) :: name = ' ' !! Message queue name (with leading `/`).
        integer(kind=c_mqd_t)          :: mqd  = 0   !! C message queue descriptor.
    end type mqueue_type

    interface dm_mqueue_open
        !! Generic message queue open function.
        procedure :: mqueue_open_raw
        procedure :: mqueue_open_type
    end interface

    interface dm_mqueue_read
        !! Generic message queue read function.
        procedure :: mqueue_read_log
        procedure :: mqueue_read_observ
        procedure :: mqueue_read_raw
    end interface

    interface dm_mqueue_write
        !! Generic message queue write function.
        procedure :: mqueue_write_log
        procedure :: mqueue_write_observ
        procedure :: mqueue_write_raw
    end interface

    public :: dm_mqueue_attr
    public :: dm_mqueue_close
    public :: dm_mqueue_open
    public :: dm_mqueue_read
    public :: dm_mqueue_unlink
    public :: dm_mqueue_write

    private :: mqueue_open_raw
    private :: mqueue_open_type

    private :: mqueue_read_log
    private :: mqueue_read_observ
    private :: mqueue_read_raw

    private :: mqueue_write_log
    private :: mqueue_write_observ
    private :: mqueue_write_raw
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_mqueue_attr(mqueue, flags, max_msg, msg_size, cur_msgs) result(rc)
        !! Returns message queue attributes.
        type(mqueue_type), intent(inout)         :: mqueue   !! Message queue type.
        integer(kind=i8),  intent(out), optional :: flags    !! Flags.
        integer(kind=i8),  intent(out), optional :: max_msg  !! Maximum number of messages in queue.
        integer(kind=i8),  intent(out), optional :: msg_size !! Message size.
        integer(kind=i8),  intent(out), optional :: cur_msgs !! Current number of messages in queue.
        type(c_mq_attr)                          :: attr

        rc = E_INVALID
        if (mqueue%mqd <= 0) return

        rc = E_MQUEUE
        if (c_mq_getattr(mqueue%mqd, attr) < 0) return

        if (present(flags))    flags    = attr%mq_flags
        if (present(max_msg))  max_msg  = attr%mq_maxmsg
        if (present(msg_size)) msg_size = attr%mq_msgsize
        if (present(cur_msgs)) cur_msgs = attr%mq_curmsgs

        rc = E_NONE
    end function dm_mqueue_attr

    integer function dm_mqueue_close(mqueue) result(rc)
        !! Closes message queue.
        type(mqueue_type), intent(inout) :: mqueue !! Message queue type.

        rc = E_NONE
        if (mqueue%mqd == 0) return

        rc = E_INVALID
        if (mqueue%mqd < 0) return

        rc = E_MQUEUE
        if (c_mq_close(mqueue%mqd) /= 0) return

        rc = E_NONE
    end function dm_mqueue_close

    integer function dm_mqueue_unlink(mqueue) result(rc)
        !! Deletes POSIX message queue.
        type(mqueue_type), intent(inout) :: mqueue !! Message queue type.

        rc = E_INVALID
        if (len_trim(mqueue%name) == 0) return

        rc = E_MQUEUE
        if (c_mq_unlink(trim(mqueue%name) // c_null_char) /= 0) return
        rc = E_NONE
    end function dm_mqueue_unlink

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function mqueue_open_raw(mqueue, name, max_msg, msg_size, access, mode, create, exclusive, blocking) result(rc)
        !! Opens POSIX message queue of given name.
        type(mqueue_type), intent(out)          :: mqueue    !! Message queue type.
        character(len=*),  intent(in)           :: name      !! Message queue name (without leading `/`).
        integer(kind=i8),  intent(in)           :: max_msg   !! Maximum number of messages in queue.
        integer(kind=i8),  intent(in)           :: msg_size  !! Message size.
        integer,           intent(in), optional :: access    !! Access type (`MQUEUE_RDONLY`, `MQUEUE_WRONLY`, `MQUEUE_RDWR`).
        integer,           intent(in), optional :: mode      !! Access permissions.
        logical,           intent(in), optional :: create    !! Creates message queue if true.
        logical,           intent(in), optional :: exclusive !! Opens message queue exclusively if true.
        logical,           intent(in), optional :: blocking  !! Blocking access if true.

        integer                 :: flag, mode_
        type(c_mq_attr), target :: attr

        ! MQ name.
        rc = E_INVALID
        if (len_trim(name) == 0) return
        if (name(1:1) == '/') return
        mqueue%name = '/' // name

        ! MQ access type.
        flag = O_WRONLY

        if (present(access)) then
            select case (access)
                case (MQUEUE_RDONLY)
                    flag = O_RDONLY
                case (MQUEUE_WRONLY)
                    flag = O_WRONLY
                case (MQUEUE_RDWR)
                    flag = O_RDWR
            end select
        end if

        ! MQ permissions.
        mode_ = MQUEUE_MODE
        if (present(mode)) mode_ = mode

        if (present(create)) then
            if (create) flag = ior(flag, O_CREAT)
        end if

        if (present(exclusive)) then
            if (exclusive) flag = ior(flag, O_EXCL)
        end if

        if (present(blocking)) then
            if (.not. blocking) flag = ior(flag, O_NONBLOCK)
        end if

        ! Set message queue attributes.
        attr%mq_maxmsg  = max_msg
        attr%mq_msgsize = msg_size

        ! Open message queue.
        rc = E_MQUEUE
        mqueue%mqd = c_mq_open(name  = trim(mqueue%name) // c_null_char, &
                               oflag = flag, &
                               mode  = mode_, &
                               attr  = c_loc(attr))
        if (mqueue%mqd < 0) return

        rc = E_NONE
    end function mqueue_open_raw

    integer function mqueue_open_type(mqueue, type, name, access, blocking) result(rc)
        !! Opens message queue for reading/writing logs or observations (in
        !! blocking mode by default).
        type(mqueue_type), intent(out)          :: mqueue   !! Message queue type.
        integer,           intent(in)           :: type     !! Data type (`TYPE_LOG`, `TYPE_OBSERV`).
        character(len=*),  intent(in)           :: name     !! Message queue name (without leading `/`).
        integer,           intent(in)           :: access   !! `MQUEUE_RDONLY`, `MQUEUE_WRONLY`, `MQUEUE_RDWR`.
        logical,           intent(in), optional :: blocking !! Blocking access if true.

        integer          :: access_
        integer(kind=i8) :: sz
        logical          :: blocking_

        rc = E_INVALID

        select case (type)
            case (TYPE_LOG)
                sz = LOG_SIZE
            case (TYPE_OBSERV)
                sz = OBSERV_SIZE
            case default
                return
        end select

        select case (access)
            case (MQUEUE_RDONLY, MQUEUE_WRONLY, MQUEUE_RDWR)
                access_ = access
            case default
                return
        end select

        blocking_ = .true.
        if (present(blocking)) blocking_ = blocking

        rc = dm_mqueue_open(mqueue   = mqueue, &
                            name     = trim(name), &
                            max_msg  = MQUEUE_MAX_MSG, &
                            msg_size = sz, &
                            access   = access_, &
                            mode     = MQUEUE_MODE, &
                            create   = .true., &
                            blocking = blocking_)
    end function mqueue_open_type

    integer function mqueue_read_log(mqueue, log, timeout) result(rc)
        !! Receives log from message queue.
        type(mqueue_type), intent(inout)         :: mqueue   !! Message queue type.
        type(log_type),    intent(out)           :: log      !! Log type.
        integer(kind=i8),  intent(in),  optional :: timeout  !! Timeout in seconds.
        character(len=LOG_SIZE)                  :: buffer

        if (present(timeout)) then
            rc = mqueue_read_raw(mqueue, buffer, timeout=timeout)
        else
            rc = mqueue_read_raw(mqueue, buffer)
        end if

        if (dm_is_error(rc)) return
        log = transfer(buffer, log)
    end function mqueue_read_log

    integer function mqueue_read_observ(mqueue, observ, timeout) result(rc)
        !! Receives observation from message queue.
        type(mqueue_type), intent(inout)         :: mqueue  !! Message queue type.
        type(observ_type), intent(out)           :: observ  !! Observation type.
        integer(kind=i8),  intent(in),  optional :: timeout !! Timeout in seconds.
        character(len=OBSERV_SIZE)               :: buffer

        if (present(timeout)) then
            rc = mqueue_read_raw(mqueue, buffer, timeout=timeout)
        else
            rc = mqueue_read_raw(mqueue, buffer)
        end if

        if (dm_is_error(rc)) return
        observ = transfer(buffer, observ)
    end function mqueue_read_observ

    integer function mqueue_read_raw(mqueue, buffer, priority, timeout) result(rc)
        !! Receives message from message queue and returns data in `buffer`.
        !! The buffer size must equal the message size.
        type(mqueue_type), intent(inout)         :: mqueue   !! Message queue type.
        character(len=*),  intent(inout)         :: buffer   !! Byte buffer.
        integer,           intent(out), optional :: priority !! Message priority.
        integer(kind=i8),  intent(in),  optional :: timeout  !! Timeout in seconds.

        integer          :: prio
        integer(kind=i8) :: sz
        type(c_timespec) :: timespec

        rc = E_MQUEUE

        if (present(timeout)) then
            timespec%tv_sec  = timeout
            timespec%tv_nsec = 0_i8

            sz = c_mq_timedreceive(mqueue%mqd, buffer, len(buffer, kind=i8), prio, timespec)
        else
            sz = c_mq_receive(mqueue%mqd, buffer, len(buffer, kind=i8), prio)
        end if

        if (present(priority)) priority = prio
        if (sz <= 0) return
        rc = E_NONE
    end function mqueue_read_raw

    integer function mqueue_write_log(mqueue, log) result(rc)
        !! Sends log message to message queue.
        type(mqueue_type), intent(inout) :: mqueue !! Message queue type.
        type(log_type),    intent(inout) :: log    !! Log type.
        character(len=LOG_SIZE)          :: buffer

        buffer = transfer(log, buffer)
        rc = mqueue_write_raw(mqueue, buffer)
    end function mqueue_write_log

    integer function mqueue_write_observ(mqueue, observ) result(rc)
        !! Sends observation to message queue.
        type(mqueue_type), intent(inout) :: mqueue !! Message queue type.
        type(observ_type), intent(inout) :: observ !! Observation type.
        character(len=OBSERV_SIZE)       :: buffer

        rc = E_MQUEUE
        buffer = transfer(observ, buffer)
        rc = mqueue_write_raw(mqueue, buffer, priority=observ%priority)
    end function mqueue_write_observ

    integer function mqueue_write_raw(mqueue, buffer, priority) result(rc)
        !! Sends log to message queue.
        type(mqueue_type), intent(inout)        :: mqueue   !! Message queue type.
        character(len=*),  intent(inout)        :: buffer   !! Byte buffer
        integer,           intent(in), optional :: priority !! Priority
        integer                                 :: prio

        rc = E_MQUEUE

        prio = 0
        if (present(priority)) prio = priority

        if (c_mq_send(mqueue%mqd, buffer, len(buffer, kind=i8), prio) < 0) return
        rc = E_NONE
    end function mqueue_write_raw
end module dm_mqueue
