! dmtestatom.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestatom
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: TEST_NAME = 'dmtestatom'
    integer,      parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        type(atom_type) :: atom
        type(log_type)  :: logs(2)

        stat = TEST_FAILED

        atom%alt      = 'http://example.com/dmpack/log?log_id='
        atom%author   = 'dummy'
        atom%email    = 'mail@example.com'
        atom%id       = dm_uuid_hyphenize(dm_uuid_new())
        atom%title    = 'Dummy Title'
        atom%subtitle = 'Dummy Sub-Title'
        atom%url      = 'http://example.com/feed.xml'

        call dm_test_dummy(logs)
        call dm_atom_write(atom, logs, STDOUT)
        print *

        stat = TEST_PASSED
    end function test01
end program dmtestatom
