! dmtestregex.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestregex
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestregex.test01', test01)
    tests(2) = test_type('dmtestregex.test02', test02)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        integer          :: rc
        type(regex_type) :: regex

        stat = TEST_FAILED

        test_block: block
            rc = dm_regex_create(regex, '^([A-Z][a-z]+)$')
            print *, 'Created regular expression'
            if (dm_is_error(rc)) exit test_block

            rc = dm_regex_match(regex, 'Fortran')
            if (dm_is_error(rc)) exit test_block
            print *, 'Subject matches'
        end block test_block

        call dm_regex_destroy(regex)

        if (dm_is_error(rc)) then
            call dm_perror(rc)
            return
        end if

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=*), parameter :: PATTERN = '--config\s*(?:"(?<config>[^"]*|[^"]+)"(?:\s+|$))?'
        character(len=*), parameter :: SUBJECT = './dmtestregex --config "./config/test.config" --logger "dmlogger" --verbose'

        character(len=:), allocatable :: name, value
        integer                       :: rc
        type(regex_type)              :: regex

        stat = TEST_FAILED

        name = 'config'

        test_block: block
            rc = dm_regex_create(regex, PATTERN)
            if (dm_is_error(rc)) exit test_block
            print *, 'Created regular expression'

            rc = dm_regex_group(regex, SUBJECT, name, value)
            if (dm_is_error(rc)) exit test_block
            print *, 'Matched groups'

            print *, '"', name, '": ', value
        end block test_block

        call dm_regex_destroy(regex)

        if (dm_is_error(rc)) then
            call dm_perror(rc)
            return
        end if

        stat = TEST_PASSED
    end function test02
end program dmtestregex
