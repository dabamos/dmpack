! dmtestlog.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestlog
    !! Test program for log handling.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestlog'
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
        stat = TEST_FAILED

        print *, 'Testing utility functions ...'

        if (dm_log_level_from_name('abc')      /= LVL_NONE)     return
        if (dm_log_level_from_name('NONE    ') /= LVL_NONE)     return
        if (dm_log_level_from_name('DEBUG   ') /= LVL_DEBUG)    return
        if (dm_log_level_from_name('INFO    ') /= LVL_INFO)     return
        if (dm_log_level_from_name('WARNING ') /= LVL_WARNING)  return
        if (dm_log_level_from_name('ERROR   ') /= LVL_ERROR)    return
        if (dm_log_level_from_name('CRITICAL') /= LVL_CRITICAL) return

        if (dm_log_valid(LVL_NONE)) return

        if (.not. dm_log_valid(LVL_DEBUG))    return
        if (.not. dm_log_valid(LVL_INFO))     return
        if (.not. dm_log_valid(LVL_WARNING))  return
        if (.not. dm_log_valid(LVL_ERROR))    return
        if (.not. dm_log_valid(LVL_CRITICAL)) return

        stat = TEST_PASSED
    end function test01
end program dmtestlog
