! dmtestbase64.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestbase64
    !! Test program for Base64 encoding.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: TEST_NAME = 'dmtestbase64'
    integer,      parameter :: NTESTS    = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        character(*), parameter :: INPUT = &
            'Now is the time for all good men to come to the aid of the party.'
        character(*), parameter :: ASSERT = &
            'Tm93IGlzIHRoZSB0aW1lIGZvciBhbGwgZ29vZCBtZW4gdG8gY29tZSB0byB0aGUgYWlkIG9mIHRoZSBwYXJ0eS4='

        integer                   :: rc
        character(:), allocatable :: decoded, encoded

        stat = TEST_FAILED

        print *, 'Encoding and decoding ...'
        call dm_base64_encode(INPUT,   encoded, error=rc); if (dm_is_error(rc)) return
        call dm_base64_decode(encoded, decoded, error=rc); if (dm_is_error(rc)) return

        if (encoded /= ASSERT) return
        if (decoded /= INPUT)  return

        print '(" Encoded: ", a)', encoded
        print '(" Decoded: ", a)', decoded

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer, parameter :: N = 1024 * 1024

        character(:), allocatable :: buffer, decoded, encoded
        integer                   :: code, i
        real(r8)                  :: b, dt, r
        type(timer_type)          :: timer

        stat = TEST_FAILED

        ! Fill buffer with random bytes.
        allocate (character(N) :: buffer)

        do i = 1, len(buffer)
            call random_number(r)
            code = 32 + int(r * 95)
            if (code > 126) code = 126
            buffer(i:i) = achar(code)
        end do

        call dm_timer_start(timer)
        call dm_base64_encode(buffer, encoded)
        call dm_timer_stop(timer, duration=dt)

        b = len(buffer) / dt / 1024 / 1024
        print '(" Encoded ", i0, " bytes in ", f12.10, " sec: ", f6.1, " MiB/sec")', len(buffer), dt, b

        call dm_timer_start(timer)
        call dm_base64_decode(encoded, decoded)
        call dm_timer_stop(timer, duration=dt)

        b = len(encoded) / dt / 1024 / 1024
        print '(" Decoded ", i0, " bytes in ", f12.10, " sec: ", f6.1, " MiB/sec")', len(encoded), dt, b

        stat = TEST_PASSED
    end function test02
end program dmtestbase64
