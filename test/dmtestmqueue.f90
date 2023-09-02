! dmtestmqueue.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestmqueue
    !! Test programs that tries message passing using POSIX message queues.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: MQ_NAME    = 'dmtest'     !! Message queue name (without leading `/`).
    integer,          parameter :: MQ_MAX_MSG = 32           !! Maximum number of messages in queue.
    integer,          parameter :: MQ_MODE    = int(o'0644') !! Access permissions (octal).

    integer, parameter :: NTESTS = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestmqueue%dm_test01', dm_test01)
    tests(2) = test_type('dmtestmqueue%dm_test02', dm_test02)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
        !! Tests observation exchange using a single message queue descriptor.
        integer           :: rc
        integer(kind=i8)  :: flags, max_msg, msg_size, cur_msgs
        type(mqueue_type) :: mqueue
        type(observ_type) :: observ1, observ2

        stat = TEST_FAILED

        rc = dm_mqueue_open(mqueue    = mqueue, &
                            name      = MQ_NAME, &
                            max_msg   = MQ_MAX_MSG, &
                            msg_size  = OBSERV_SIZE, &
                            access    = MQUEUE_RDWR, &
                            mode      = MQ_MODE, &
                            create    = .true., &
                            exclusive = .false., &
                            blocking  = .true.)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'Created message queue "' // MQ_NAME // '"'

        rc = dm_mqueue_attr(mqueue, flags, max_msg, msg_size, cur_msgs)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'Got message queue attributes'

        print *, 'Flags.......: ', flags
        print *, 'Max. Msg....: ', max_msg
        print *, 'Msg. Size...: ', msg_size, ' bytes'
        print *, 'Cur. Msgs...: ', cur_msgs
        print *, 'Observ. Size: ', OBSERV_SIZE, ' bytes'

        if (msg_size /= OBSERV_SIZE) then
            print *, 'Wrong message size!'
            return
        end if

        call dm_dummy_observ(observ1)

        rc = dm_mqueue_write(mqueue, observ1)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'Message has been sent'

        rc = dm_mqueue_read(mqueue, observ2, 2_8)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'Message has been received'

        rc = dm_mqueue_close(mqueue)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'Closed message queue "' // MQ_NAME // '"'

        rc = dm_mqueue_unlink(mqueue)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'Unlinked message queue "' // MQ_NAME // '"'

        print *, 'Validating observation data ...'
        if (.not. (observ1 == observ2)) return

        stat = TEST_PASSED
    end function dm_test01

    logical function dm_test02() result(stat)
        !! Tests observation exchange using reader/writer functions (two message
        !! queue descriptors).
        integer           :: rc
        type(mqueue_type) :: mqueue1, mqueue2
        type(observ_type) :: observ1, observ2

        stat = TEST_FAILED

        rc = dm_mqueue_open(mqueue1, TYPE_OBSERV, MQ_NAME, MQUEUE_WRONLY)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'WRITER: Created message queue "' // MQ_NAME // '"'

        rc = dm_mqueue_open(mqueue2, TYPE_OBSERV, MQ_NAME, MQUEUE_RDONLY)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'READER: Created message queue "' // MQ_NAME // '"'

        call dm_dummy_observ(observ1)

        rc = dm_mqueue_write(mqueue1, observ1)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'WRITER: Sent message to queue "' // MQ_NAME // '"'

        rc = dm_mqueue_read(mqueue2, observ2)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'READER: Received message from queue "' // MQ_NAME // '"'

        rc = dm_mqueue_close(mqueue1)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'WRITER: Closed message queue "' // MQ_NAME // '"'

        rc = dm_mqueue_close(mqueue2)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'READER: Closed message queue "' // MQ_NAME // '"'

        rc = dm_mqueue_unlink(mqueue1)
        call dm_perror(rc)
        if (dm_is_error(rc)) return
        print *, 'WRITER: Unlinked message queue "' // MQ_NAME // '"'

        print *, 'Validating observation data ...'
        if (.not. (observ1 == observ2)) return

        stat = TEST_PASSED
    end function dm_test02
end program dmtestmqueue
