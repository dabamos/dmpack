! dmtestlua.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestlua
    !! Tests Lua abstraction layer.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestlua'
    character(len=*), parameter :: LUA_FILE  = 'test/test.lua'
    integer,          parameter :: NTESTS    = 12

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04), &
        test_type('test05', test05), &
        test_type('test06', test06), &
        test_type('test07', test07), &
        test_type('test08', test08), &
        test_type('test09', test09), &
        test_type('test10', test10), &
        test_type('test11', test11), &
        test_type('test12', test12)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        !! Reads Lua global variables from file.
        character(len=*), parameter   :: FOO   = 'bar'
        integer,          parameter   :: VALUE = 420

        character(len=32)    :: str
        integer              :: rc
        integer              :: v
        real(kind=r8)        :: r
        type(lua_state_type) :: lua

        stat = TEST_FAILED

        print '(" Library: ", a)', dm_lua_version(.true.)
        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening Lua file ...'
            rc = dm_lua_open(lua, LUA_FILE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading global string ...'
            str = ' '
            rc = dm_lua_read(lua, 'foo', str)
            if (dm_is_error(rc)) exit test_block
            if (len_trim(str) == 0) exit test_block
            print *, 'Value: ', trim(str)

            print *, 'Reading global int ...'
            rc = dm_lua_read(lua, 'value', v)
            if (dm_is_error(rc)) exit test_block
            print *, 'Value: ', v

            print *, 'Reading global float ...'
            rc = dm_lua_read(lua, 'pi', r)
            if (dm_is_error(rc)) exit test_block
            print *, 'Value: ', r

            print *, 'Validating values ...'
            if (str /= FOO) exit test_block
            if (.not. dm_equals(r, PI)) exit test_block
            if (v /= VALUE) exit test_block
        end block test_block

        call dm_lua_destroy(lua)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, dm_lua_last_error(lua))
            return
        end if

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! Reads Lua table item from file.
        character(len=32)    :: str
        integer              :: i, rc
        type(lua_state_type) :: lua

        stat = TEST_FAILED

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening Lua file ...'
            rc = dm_lua_open(lua, LUA_FILE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Loading table ...'
            rc = dm_lua_table(lua, 'dmtest')
            if (dm_is_error(rc)) exit test_block

            print *, 'Printing Lua stack dump ...'
            call dm_lua_dump_stack(lua)

            print *, 'Reading key ...'
            str = ' '
            rc = dm_lua_field(lua, 'logger', str)
            if (dm_is_error(rc)) exit test_block
            print *, 'Value: ', trim(str)
            if (str /= 'dmlogger') exit test_block

            print *, 'Reading invalid key (string) ...'
            rc = dm_lua_field(lua, 'logger1', str)
            call dm_error_out(rc)
            if (.not. dm_is_error(rc)) exit test_block

            print *, 'Reading invalid key (integer) ...'
            rc = dm_lua_field(lua, 'i', i)
            call dm_error_out(rc)
            if (.not. dm_is_error(rc)) exit test_block

            print *, 'Reading key ...'
            str = ' '
            rc = dm_lua_field(lua, 'node', str)
            if (dm_is_error(rc)) exit test_block
            print *, 'Value: ', trim(str)
            if (str /= 'dummy-node') exit test_block

            call dm_lua_pop(lua)
            rc = E_NONE
        end block test_block

        call dm_lua_destroy(lua)

        if (dm_is_error(rc)) then
            call dm_error_out(rc)
            return
        end if

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        !! Reads observation from Lua file.
        integer              :: rc
        type(lua_state_type) :: lua
        type(observ_type)    :: observ

        stat = TEST_FAILED

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening Lua file ...'
            rc = dm_lua_open(lua, LUA_FILE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading table ...'
            rc = dm_lua_read(lua, 'observ')
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading observation ...'
            rc = dm_lua_to(lua, observ)
            if (dm_is_error(rc)) exit test_block
        end block test_block

        print '(72("."))'
        call dm_observ_out(observ)
        print '(72("."))'

        call dm_error_out(rc, dm_lua_last_error(lua))
        call dm_lua_destroy(lua)

        if (dm_is_error(rc)) return

        print *, 'Validating observation ...'
        if (observ%requests(1)%delimiter /= '\n') return
        if (observ%requests(1)%response /= '123\r\n') return
        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        !! Reads observations from Lua file.
        integer                        :: rc
        type(lua_state_type)           :: lua
        type(observ_type), allocatable :: observs(:)

        stat = TEST_FAILED

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening Lua file ...'
            rc = dm_lua_open(lua, LUA_FILE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading table ...'
            rc = dm_lua_read(lua, 'observs')
            if (dm_is_error(rc)) exit test_block

            print *, 'Printing Lua stack dump ...'
            call dm_lua_dump_stack(lua)

            print *, 'Reading observations ...'
            rc = dm_lua_to(lua, observs)
            if (dm_is_error(rc)) exit test_block

            print *, 'Printing Lua stack dump ...'
            call dm_lua_dump_stack(lua)
        end block test_block

        call dm_error_out(rc, dm_lua_last_error(lua))
        call dm_lua_destroy(lua)

        print *, 'Validating observations ...'
        if (dm_is_error(rc)) return
        if (.not. allocated(observs)) return
        if (size(observs) /= 2) return
        if (.not. (observs(1) == observs(2))) return

        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        !! Reads jobs from Lua file.
        integer                     :: rc
        type(lua_state_type)        :: lua
        type(job_type)              :: job
        type(job_list_type)         :: job_list
        type(job_type), allocatable :: jobs(:)

        stat = TEST_FAILED

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening Lua file ...'
            rc = dm_lua_open(lua, LUA_FILE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading table ...'
            rc = dm_lua_read(lua, 'config')
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading field ...'
            rc = dm_lua_field(lua, 'jobs')
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading jobs ...'
            rc = dm_lua_to(lua, jobs)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading field ...'
            rc = dm_lua_field(lua, 'jobs')
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading job list ...'
            rc = dm_lua_to(lua, job_list)
            if (dm_is_error(rc)) exit test_block
        end block test_block

        call dm_error_out(rc, dm_lua_last_error(lua))
        call dm_lua_destroy(lua)

        print *, 'Validating jobs ...'
        if (size(jobs) == 0) return
        if (dm_job_list_count(job_list) == 0) return

        print *, 'Retrieving next job ...'
        rc = dm_job_list_next(job_list, job)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'delay...: ', job%delay
        print *, 'disabled: ', job%disabled
        print *, 'onetime.: ', job%onetime
        print *, 'observ..: ', job%observ%name

        stat = TEST_PASSED
    end function test05

    logical function test06() result(stat)
        !! Reads report from Lua file.
        integer              :: rc
        type(lua_state_type) :: lua
        type(report_type)    :: report

        stat = TEST_FAILED

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening Lua file ...'
            rc = dm_lua_open(lua, LUA_FILE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading table "dmreport" ...'
            rc = dm_lua_read(lua, 'dmreport')
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading report to derived type ...'
            rc = dm_lua_to(lua, report)
            if (dm_is_error(rc)) exit test_block

            print *, 'Printing Lua stack dump ...'
            call dm_lua_dump_stack(lua)

            print *, 'Validating report ...'
            rc = E_INVALID
            if (.not. dm_report_valid(report)) exit test_block

            rc = E_NONE
        end block test_block

        call dm_error_out(rc, dm_lua_last_error(lua))
        call dm_lua_destroy(lua)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test06

    logical function test07() result(stat)
        !! Moves data through the Lua stack.
        integer              :: rc
        type(lua_state_type) :: lua
        type(observ_type)    :: observ1, observ2

        stat = TEST_FAILED
        call dm_test_dummy(observ1, nrequests=2)

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Pushing observation onto Lua stack ...'
            call dm_lua_from(lua, observ1)

            print *, 'Printing Lua stack dump ...'
            call dm_lua_dump_stack(lua)

            print *, 'Pulling observation from Lua stack ...'
            rc = dm_lua_to(lua, observ2)

            print *, 'Printing Lua stack dump ...'
            call dm_lua_dump_stack(lua)

            rc = E_NONE
        end block test_block

        call dm_error_out(rc, dm_lua_last_error(lua))
        call dm_lua_destroy(lua)
        if (dm_is_error(rc)) return

        print *, 'Validating ...'
        if (.not. (observ1 == observ2)) return

        stat = TEST_PASSED
    end function test07

    logical function test08() result(stat)
        !! Test that passes a request to a Lua function and reads it back.
        character(len=*), parameter :: FUNC_NAME = 'process'

        integer              :: rc
        type(lua_state_type) :: lua
        type(request_type)   :: request1, request2

        stat = TEST_FAILED

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening Lua file ...'
            rc = dm_lua_open(lua, LUA_FILE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Loading Lua function ' // FUNC_NAME // '() ...'
            rc = dm_lua_read(lua, FUNC_NAME)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating Lua function ' // FUNC_NAME // '() ...'
            if (.not. dm_lua_is_function(lua)) exit test_block

            print *, 'Pushing request onto Lua stack ...'
            call dm_geocom_api_request_beep_alarm(request1)
            call dm_lua_from(lua, request1)

            print *, 'Calling Lua function ' // FUNC_NAME // '() ...'
            print '(72("."))'
            rc = dm_lua_call(lua, nargs=1, nresults=1)
            if (dm_is_error(rc)) exit test_block
            print '(72("."))'

            print *, 'Pulling request from Lua stack ...'
            rc = dm_lua_to(lua, request2)
            if (dm_is_error(rc)) exit test_block

            print *, 'Printing Lua stack dump ...'
            call dm_lua_dump_stack(lua)

            rc = E_NONE
        end block test_block

        call dm_error_out(rc, dm_lua_last_error(lua))
        call dm_lua_destroy(lua)
        if (dm_is_error(rc)) return

        print *, 'Validating request ...'
        if (.not. (request1 == request2)) return

        stat = TEST_PASSED
    end function test08

    logical function test09() result(stat)
        !! GeoCOM API test.
        character(len=*), parameter :: FUNC_NAME = 'geocom'

        integer              :: rc
        type(lua_state_type) :: lua
        type(request_type)   :: request1, request2

        stat = TEST_FAILED

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening Lua file ...'
            rc = dm_lua_open(lua, LUA_FILE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating GeoCOM environment ...'
            rc = dm_lua_geocom_register(lua)
            if (dm_is_error(rc)) exit test_block

            print *, 'Loading Lua function ' // FUNC_NAME // '() ...'
            rc = dm_lua_read(lua, FUNC_NAME)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating Lua function ' // FUNC_NAME // '() ...'
            if (.not. dm_lua_is_function(lua)) exit test_block

            print *, 'Calling Lua function ' // FUNC_NAME // '() ...'
            rc = dm_lua_call(lua, nargs=0, nresults=1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Pulling request from Lua stack ...'
            rc = dm_lua_to(lua, request1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Printing Lua stack dump ...'
            call dm_lua_dump_stack(lua)

            rc = E_NONE
        end block test_block

        call dm_error_out(rc, 'Lua error: ' // dm_lua_last_error(lua))
        call dm_lua_destroy(lua)
        if (dm_is_error(rc)) return

        print *, 'Validating request ...'
        call dm_geocom_api_request_beep_alarm(request2)
        if (.not. (request1 == request2)) return

        stat = TEST_PASSED
    end function test09

    logical function test10() result(stat)
        !! Lua array access test.
        integer, parameter :: ASSERT(3) = [ 1, 2, 3 ]

        integer              :: i, rc
        integer, allocatable :: values(:)
        type(lua_state_type) :: lua

        stat = TEST_FAILED

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening Lua file ...'
            rc = dm_lua_open(lua, LUA_FILE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading integer array ...'
            rc = dm_lua_read(lua, 'mylist', values)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating ...'
            rc = E_CORRUPT
            if (.not. allocated(values)) exit test_block
            if (size(values) /= size(ASSERT)) exit test_block

            rc = E_INVALID
            do i = 1, size(values)
                if (values(i) /= ASSERT(i)) exit test_block
            end do

            rc = E_NONE
        end block test_block

        call dm_error_out(rc, 'Lua error: ' // dm_lua_last_error(lua))
        call dm_lua_destroy(lua)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test10

    logical function test11() result(stat)
        !! Reads array from table.
        integer, parameter :: ASSERT(3) = [ 1, 2, 3 ]

        integer              :: i, rc
        integer, allocatable :: values(:)
        type(lua_state_type) :: lua

        stat = TEST_FAILED

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Opening Lua file ...'
            rc = dm_lua_open(lua, LUA_FILE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading table ...'
            rc = dm_lua_read(lua, 'config')
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading field ...'
            rc = dm_lua_field(lua, 'list', values)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating ...'
            rc = E_CORRUPT
            if (.not. allocated(values)) exit test_block
            if (size(values) /= size(ASSERT)) exit test_block

            rc = E_INVALID
            do i = 1, size(values)
                if (values(i) /= ASSERT(i)) exit test_block
            end do

            rc = E_NONE
        end block test_block

        call dm_error_out(rc, dm_lua_last_error(lua))
        call dm_lua_destroy(lua)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test11

    logical function test12() result(stat)
        !! Writes and reads variable.
        integer              :: rc
        integer              :: v1, v2
        type(lua_state_type) :: lua

        stat = TEST_FAILED

        print *, 'Creating new Lua state ...'
        rc = dm_lua_init(lua)
        if (dm_is_error(rc)) return

        test_block: block
            v1 = 12345

            print *, 'Writing int ...'
            rc = dm_lua_set(lua, 'foo', v1)
            if (dm_is_error(rc)) exit test_block
            print *, 'Value: ', v1

            print *, 'Reading int ...'
            rc = dm_lua_read(lua, 'foo', v2)
            if (dm_is_error(rc)) exit test_block
            print *, 'Value: ', v2

            print *, 'Validating values ...'
            if (v1 /= v2) exit test_block
        end block test_block

        call dm_lua_destroy(lua)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, dm_lua_last_error(lua))
            return
        end if

        stat = TEST_PASSED
    end function test12
end program dmtestlua
