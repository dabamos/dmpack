! dmtesttty.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesttty
    use :: unix
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtesttty%dm_test01', dm_test01)

    call dm_init()
    call dm_test_run(tests, stats, no_color=dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
        character(len=128) :: buf
        integer            :: rc
        integer(kind=i8)   :: n
        type(tty_type)     :: tty

        stat = TEST_FAILED

        tty%path      = '/dev/ttyU0'
        tty%access    = TTY_RDWR
        tty%baud_rate = TTY_B9600
        tty%byte_size = TTY_BYTE_SIZE8
        tty%stop_bits = TTY_STOP_BITS2
        tty%parity    = TTY_PARITY_NONE
        tty%timeout   = 5
        tty%rts       = .true.
        tty%dtr       = .true.
        tty%blocking  = .true.

        print *, 'Opening TTY ', trim(tty%path), ' ...'
        rc = dm_tty_open(tty)
        call dm_perror(rc)
        if (rc /= 0) return

        rc = c_usleep(500 * 1000)
        print *, 'Writing ...'
        rc = dm_tty_write(tty, dm_ascii_unescape('s\r\r'))
        call dm_perror(rc)

        rc = c_usleep(500 * 1000)
        print *, 'Writing ...'
        rc = dm_tty_write(tty, dm_ascii_unescape('Meter\r\r'))
        call dm_perror(rc)

        print *, 'Reading ...'
        buf = ' '
        rc = dm_tty_read(tty, buf, ASCII_LF // ASCII_LF, n)
        call dm_perror(rc)
        print *, trim(dm_ascii_escape(buf))

        print *, 'Closing TTY ...'
        call dm_tty_close(tty)

        stat = TEST_PASSED
    end function dm_test01
end program dmtesttty

