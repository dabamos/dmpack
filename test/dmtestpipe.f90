! dmtestpipe.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestpipe
    use, intrinsic :: iso_c_binding, only: c_associated
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestpipe.test01', test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=*), parameter :: COMMAND = 'cat -n'

        character(len=4)  :: message
        character(len=32) :: buffer, error
        integer           :: rc
        type(pipe_type)   :: stdin, stdout, stderr

        stat = TEST_FAILED

        rc = dm_pipe_open2(stdin, stdout, stderr, COMMAND)
        if (rc /= E_NONE) return

        if (.not. dm_pipe_connected(stdin) .or. &
            .not. dm_pipe_connected(stdout) .or. &
            .not. dm_pipe_connected(stderr)) return

        message = 'TEST'
        print '(" Parent: ", a)', trim(message)

        ! Write to stdin.
        print '(" stdin.: ", i0, " bytes")', dm_pipe_write2(stdin, message)
        call dm_pipe_close2(stdin)

        ! Read from stdout.
        print '(" stdout: ", i0, " bytes")', dm_pipe_read(stdout, buffer)
        call dm_pipe_close2(stdout)

        ! Read from stderr.
        print '(" stderr: ", i0, " bytes")', dm_pipe_read(stderr, error)
        call dm_pipe_close2(stderr)

        if (len_trim(buffer) == 0) then
            if (len_trim(error) > 0) print '("Error: ", a)', error
            return
        end if

        print '(" Child.: ", a)', trim(buffer)

        stat = TEST_PASSED
    end function test01
end program dmtestpipe
