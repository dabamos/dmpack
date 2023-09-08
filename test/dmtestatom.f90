! dmtestatom.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestatom
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestatom.test01', test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=:), allocatable :: xml
        type(atom_type)               :: atom
        type(log_type)                :: logs(2)

        stat = TEST_FAILED

        atom%alt      = 'http://example.com/dmpack/log?log_id='
        atom%author   = 'dummy'
        atom%email    = 'mail@example.com'
        atom%id       = dm_uuid4_hyphenize(dm_uuid4())
        atom%title    = 'Dummy Title'
        atom%subtitle = 'Dummy Sub-Title'
        atom%url      = 'http://example.com/feed.xml'

        call dm_dummy_log(logs)
        call dm_atom_from_logs(atom, logs, xml)
        print '(a)', xml

        stat = TEST_PASSED
    end function test01
end program dmtestatom
