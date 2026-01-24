! dmtesttty.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesttty
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtesttty'
    integer,          parameter :: NTESTS    = 1
    logical,          parameter :: SKIP      = .true.

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    no_color = dm_env_has('NO_COLOR')

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        use :: unix

        character(len=128)   :: buf
        integer              :: rc
        integer(kind=i8)     :: n
        type(posix_tty_type) :: tty

        stat = TEST_PASSED

        if (skip) then
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print *, 'This test will be skipped by default!'
            call dm_ansi_reset(no_color)
            return
        end if

        stat = TEST_FAILED

        tty%path      = '/dev/ttyU0'
        tty%access    = POSIX_TTY_RDWR
        tty%baud_rate = POSIX_TTY_B9600
        tty%byte_size = POSIX_TTY_BYTE_SIZE8
        tty%stop_bits = POSIX_TTY_STOP_BITS2
        tty%parity    = POSIX_TTY_PARITY_NONE
        tty%timeout   = 50
        tty%rts       = .true.
        tty%dtr       = .true.
        tty%blocking  = .true.

        print *, 'Opening TTY ', trim(tty%path), ' ...'
        rc = dm_posix_tty_open(tty)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        rc = c_usleep(500 * 1000)
        print *, 'Writing ...'
        rc = dm_posix_tty_flush(tty, output=.false.)
        rc = dm_posix_tty_write(tty, dm_ascii_unescape('s\r'))
        call dm_error_out(rc)

        rc = c_usleep(500 * 1000)
        print *, 'Writing ...'
        rc = dm_posix_tty_flush(tty, output=.false.)
        rc = dm_posix_tty_write(tty, dm_ascii_unescape('Meter\r'))
        call dm_error_out(rc)

        print *, 'Reading ...'
        buf = ' '
        rc = dm_posix_tty_read(tty, buf, ASCII_CR, n)
        call dm_error_out(rc)
        print *, trim(dm_ascii_escape(buf))

        print *, 'Closing TTY ...'
        call dm_posix_tty_close(tty)

        stat = TEST_PASSED
    end function test01
end program dmtesttty
