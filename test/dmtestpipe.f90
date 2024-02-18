! dmtestpipe.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestpipe
    use, intrinsic :: iso_c_binding, only: c_associated
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    call dm_init()
    no_color = dm_env_has('NO_COLOR')

    tests(1) = test_type('dmtestpipe.test01', test01)

    call dm_test_run(tests, stats, no_color)
contains
    logical function skip_test() result(skip)
        integer :: rc

        rc = dm_env_get('DM_PIPE_SKIP', skip)

        if (skip) then
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("dmtestpipe:")'
            print '("    Environment variable DM_PIPE_SKIP is set.")'
            print '("    This test will be skipped.")'
            call dm_ansi_reset(no_color)
        end if
    end function skip_test

    logical function test01() result(stat)
        character(len=*), parameter :: COMMAND = 'cat -n'

        character(len=4)  :: message
        character(len=32) :: buffer, error
        integer           :: rc
        type(pipe_type)   :: stdin, stdout, stderr

        stat = TEST_PASSED
        if (skip_test()) return

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
