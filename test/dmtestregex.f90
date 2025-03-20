! dmtestregex.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestregex
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestregex'
    integer,          parameter :: NTESTS    = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'), compiler_version(), compiler_options())
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
            call dm_error_out(rc)
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
            call dm_error_out(rc)
            return
        end if

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        character(len=*), parameter :: ASSERT = 'dummy'

        character(len=:), allocatable :: string
        integer                       :: rc
        type(request_type)            :: request

        stat = TEST_FAILED

        request%response = '%R1P,0,0:0,1,100,0,"' // ASSERT // '"'
        request%pattern  = '%R1P,0,0:(?<grc>\d+),(?<imageno>\d+),(?<quality>\d+),(?<subfunc>\d+),"(?<fnprefix>.+)"'

        print *, 'Extracting response string ...'
        rc = dm_regex_response_string(request, 'fnprefix', string)

        call dm_error_out(rc)
        if (string /= ASSERT) return

        stat = TEST_PASSED
    end function test03
end program dmtestregex
