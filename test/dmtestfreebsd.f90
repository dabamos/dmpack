! dmtestfreebsd.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestfreebsd
    !! Test program of FreeBSD API module.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestfreebsd'
    integer,          parameter :: NTESTS    = 1

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    no_color = dm_env_has('NO_COLOR')

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        integer :: rc

        stat = TEST_FAILED

        if (dm_system_type() /= SYSTEM_TYPE_FREEBSD) then
            stat = TEST_PASSED
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("> This test is for FreeBSD only and will be skipped.")'
            call dm_ansi_reset(no_color)
            return
        end if

        io_block: block
            character(len=64) :: model
            integer(kind=i8)  :: phys_mem, real_mem, user_mem
            integer           :: life, max_mqs, max_msgs, max_size

            rc = dm_freebsd_sysctl_memory(phys_mem, real_mem, user_mem)
            if (dm_is_error(rc)) exit io_block

            rc = dm_freebsd_sysctl_battery_life(life)
            rc = dm_freebsd_sysctl_hardware_model(model)
            rc = dm_freebsd_sysctl_mqueue(max_mqs, max_msgs, max_size)
            rc = E_NONE

            print '(" phys memory....: ", i0, " bytes")', phys_mem
            print '(" real memory....: ", i0, " bytes")', real_mem
            print '(" user memory....: ", i0, " bytes")', user_mem
            print '(" battery life...: ", i0, " %")',     life
            print '(" hardware model.: ", a)',            trim(model)
            print '(" mqueue max mqs.: ", i0)',           max_mqs
            print '(" mqueue max msgs: ", i0)',           max_msgs
            print '(" mqueue max size: ", i0, " bytes")', max_size
        end block io_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01
end program dmtestfreebsd
