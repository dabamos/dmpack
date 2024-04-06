! dmtestthread.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestthread
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestthread'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        integer, target   :: arg
        integer           :: rc
        type(thread_type) :: thread

        stat = TEST_FAILED
        arg  = 1

        thread_block: block
            print *, 'Creating thread ...'
            rc = dm_thread_create(thread, thread_routine, arg)
            if (dm_is_error(rc)) exit thread_block

            print *, 'Joining thread ...'
            rc = dm_thread_join(thread)
            if (dm_is_error(rc)) exit thread_block
        end block thread_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01

    subroutine thread_routine(arg) bind(c)
        use, intrinsic :: iso_c_binding
        type(c_ptr), intent(in), value :: arg
        integer, pointer               :: i

        if (.not. c_associated(arg)) return
        call c_f_pointer(arg, i)
        print *, '>>> client data:', i
    end subroutine thread_routine
end program dmtestthread
