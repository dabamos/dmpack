! dmtestposixpipe.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestposixpipe
    !! Disabled the tests of this program by setting the following environment
    !! variable:
    !!
    !!      DM_PIPE_SKIP - Skip all tests.
    !!
    !! For example:
    !!
    !!      $ export DM_PIPE_SKIP=1
    !!      $ ./dmtestposixpipe
    !!
    !! This may be necessary on test platforms where bi-directional pipes are
    !! not available.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestposixpipe'
    integer,          parameter :: NTESTS    = 2

    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=*), parameter :: COMMAND = 'cat -n'

        character(len=4)      :: message
        character(len=32)     :: buffer, error
        integer               :: rc
        integer(kind=i8)      :: n
        type(posix_pipe_type) :: stdin, stdout, stderr

        stat = TEST_PASSED
        if (dm_test_skip('DM_PIPE_SKIP')) return

        stat = TEST_FAILED
        rc = dm_posix_pipe_open2(stdin, stdout, stderr, COMMAND)
        if (rc /= E_NONE) return

        if (.not. dm_posix_pipe_is_connected(stdin)  .or. &
            .not. dm_posix_pipe_is_connected(stdout) .or. &
            .not. dm_posix_pipe_is_connected(stderr)) return

        message = 'TEST'
        print '(" Parent: ", a)', trim(message)

        ! Write to stdin.
        rc = dm_posix_pipe_write2(stdin, message, n)
        print '(" stdin.: ", i0, " bytes")', n
        call dm_posix_pipe_close2(stdin)

        ! Read from stdout.
        rc = dm_posix_pipe_read(stdout, buffer, n)
        print '(" stdout: ", i0, " bytes")', n
        call dm_posix_pipe_close2(stdout)

        ! Read from stderr.
        rc = dm_posix_pipe_read(stderr, error, n)
        print '(" stderr: ", i0, " bytes")', n
        call dm_posix_pipe_close2(stderr)

        if (len_trim(buffer) == 0) then
            if (len_trim(error) > 0) print '("Error: ", a)', error
            return
        end if

        print '(" Child.: ", a)', trim(buffer)

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=*), parameter :: COMMAND = 'df .'

        character(len=256)    :: buffers(2)
        integer               :: rc
        type(posix_pipe_type) :: pipe

        stat = TEST_PASSED
        if (dm_test_skip('DM_PIPE_SKIP')) return

        stat = TEST_FAILED
        io_block: block
            print *, 'Opening pipe ...'
            rc = dm_posix_pipe_open(pipe, COMMAND, PIPE_RDONLY)
            if (dm_is_error(rc)) exit io_block

            print *, 'Reading from pipe ...'
            buffers = ' '
            rc = dm_posix_pipe_read_line(pipe, buffers(1))
            if (dm_is_error(rc)) exit io_block

            rc = dm_posix_pipe_read_line(pipe, buffers(2))
            if (dm_is_error(rc)) exit io_block

            print '(a, /, a)', trim(buffers(1)), trim(buffers(2))
        end block io_block

        call dm_posix_pipe_close(pipe)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test02
end program dmtestposixpipe
