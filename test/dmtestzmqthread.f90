! dmtestzmqthread.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestzmqthread
    !! Test program for ZMQ threads.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestzmqthread'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        integer, target       :: argument
        integer               :: rc
        type(zmq_thread_type) :: thread

        stat = TEST_FAILED
        argument  = 1

        test_block: block
            print *, 'Creating IPC thread ...'
            rc = dm_zmq_thread_create(thread, thread_callback, argument)
            if (dm_is_error(rc)) exit test_block

            print *, 'Joining IPC thread ...'
            rc = dm_zmq_thread_join(thread)
            if (dm_is_error(rc)) exit test_block
        end block test_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01

    subroutine thread_callback(argument) bind(c)
        use, intrinsic :: iso_c_binding
        type(c_ptr), intent(in), value :: argument
        integer, pointer               :: i

        if (.not. c_associated(argument)) return
        call c_f_pointer(argument, i)
        print *, 'Client data:', i
    end subroutine thread_callback
end program dmtestzmqthread
