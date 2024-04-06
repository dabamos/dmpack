! dmtestascii.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestascii
    !! Test program for ASCII procedures.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestascii'
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
        character(len=:), allocatable :: str

        stat = TEST_FAILED

        print *, 'Escaping strings ...'
        str = dm_ascii_escape('ABCabc123' // ASCII_CR // ASCII_LF)
        if (str /= 'ABCabc123\r\n') return
        str = dm_ascii_escape('\')
        if (str /= '\\') return
        str = dm_ascii_escape('\\')
        if (str /= '\\\\') return
        str = dm_ascii_escape(ASCII_NUL)
        if (str /= '\x00') return
        str = dm_ascii_escape('ABC' // ASCII_DLE // 'abc' // ASCII_FS)
        if (str /= 'ABC\x10abc\x1C') return

        print *, 'Unescaping strings ...'
        str = dm_ascii_unescape('ABCabc123\r\n')
        if (str /= 'ABCabc123' // ASCII_CR // ASCII_LF) return
        str = dm_ascii_unescape('\\')
        if (str /= '\') return
        str = dm_ascii_unescape('\\\\')
        if (str /= '\\') return
        str = dm_ascii_unescape('\x00')
        if (str /= ASCII_NUL) return
        str = dm_ascii_unescape('ABC\x10abc\x1c')
        if (str /= 'ABC' // ASCII_DLE // 'abc' // ASCII_FS) return

        stat = TEST_PASSED
    end function test01
end program dmtestascii
