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

        if (dm_log_level_from_name('abc')      /= LL_NONE)     return
        if (dm_log_level_from_name('NONE')     /= LL_NONE)     return
        if (dm_log_level_from_name('DEBUG')    /= LL_DEBUG)    return
        if (dm_log_level_from_name('INFO')     /= LL_INFO)     return
        if (dm_log_level_from_name('WARNING ') /= LL_WARNING)  return
        if (dm_log_level_from_name('ERROR')    /= LL_ERROR)    return
        if (dm_log_level_from_name('CRITICAL') /= LL_CRITICAL) return
        if (dm_log_level_from_name('USER')     /= LL_USER)     return

        if (dm_log_level_is_valid(LL_NONE)) return

        if (.not. dm_log_level_is_valid(LL_DEBUG))    return
        if (.not. dm_log_level_is_valid(LL_INFO))     return
        if (.not. dm_log_level_is_valid(LL_WARNING))  return
        if (.not. dm_log_level_is_valid(LL_ERROR))    return
        if (.not. dm_log_level_is_valid(LL_CRITICAL)) return
        if (.not. dm_log_level_is_valid(LL_USER))     return

        stat = TEST_PASSED
    end function test01
end program dmtestlog
