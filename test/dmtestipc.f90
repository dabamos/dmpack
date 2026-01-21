! dmtestipc.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestipc
    !! Test program for IPC through NNG sockets.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestipc'
    integer,          parameter :: NTESTS    = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(*), parameter :: URL = 'ipc:///tmp/dmtestipc.ipc'

        integer                :: rc
        type(ipc_context_type) :: sender, receiver

        stat = TEST_FAILED

        print *, 'Testing pair protocol ...'

        ipc_block: block
            character(8), target :: msg_recv, msg_sent

            msg_sent = 'fortran'
            msg_recv = ' '

            print *, '[RECV] Opening socket ...'
            rc = dm_ipc_open_pair(receiver)
            if (dm_is_error(rc)) exit ipc_block

            print *, '[SEND] Opening socket ...'
            rc = dm_ipc_open_pair(sender)
            if (dm_is_error(rc)) exit ipc_block

            print *, '[RECV] Listening ...'
            rc = dm_ipc_listen(receiver, URL)
            if (dm_is_error(rc)) exit ipc_block

            print *, '[SEND] Dialing ...'
            rc = dm_ipc_dial(sender, URL)
            if (dm_is_error(rc)) exit ipc_block

            print *, '[SEND] Sending message ...'
            rc = dm_ipc_send(sender, msg_sent, timeout=100)
            if (dm_is_error(rc)) exit ipc_block

            print *, '[RECV] Receiving message ...'
            rc = dm_ipc_receive(receiver, msg_recv, timeout=100)
            if (dm_is_error(rc)) exit ipc_block

            print *, '[RECV] Validating message ...'
            rc = E_INVALID
            if (msg_recv == msg_sent) rc = E_NONE
        end block ipc_block

        if (dm_is_error(rc)) then
            call dm_error_out(rc)
            print '(" [SEND] Error message: ", a)', dm_ipc_error_message(sender)
            print '(" [RECV] Error message: ", a)', dm_ipc_error_message(receiver)
        end if

        call dm_ipc_close(receiver)
        call dm_ipc_close(sender)

        if (dm_is_ok(rc)) stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer              :: rc
        type(ipc_mutex_type) :: mutex

        stat = TEST_FAILED

        print *, 'Creating IPC mutex ...'
        rc = dm_ipc_mutex_create(mutex)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Locking ...'
        call dm_ipc_mutex_lock(mutex)

        print *, 'Unlocking ...'
        call dm_ipc_mutex_unlock(mutex)

        print *, 'Destroying ...'
        call dm_ipc_mutex_destroy(mutex)

        stat = TEST_PASSED
    end function test02
end program dmtestipc
