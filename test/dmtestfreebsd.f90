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
    integer,          parameter :: NTESTS    = 2

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    no_color = dm_env_has('NO_COLOR')

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function is_freebsd(stat) result(is)
        logical, intent(out) :: stat

        is = (dm_system_type() == SYSTEM_TYPE_FREEBSD)
        stat = TEST_FAILED

        if (.not. is) then
            stat = TEST_PASSED
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("> This test is for FreeBSD only and will be skipped.")'
            call dm_ansi_reset(no_color)
            return
        end if
    end function is_freebsd

    logical function test01() result(stat)
        integer :: rc

        if (.not. is_freebsd(stat)) return

        io_block: block
            character(len=*), parameter :: PATH = '.'

            character(len=64) :: paths(2), model
            integer           :: capacity, idle, life, max_mqs, max_msgs, max_size, ncore
            integer(kind=i8)  :: available, size, used
            integer(kind=i8)  :: phys_mem, real_mem, user_mem
            real              :: avgs(3), temp

            print *, 'Reading system status ...'

            rc = dm_freebsd_sysctl_battery_life(life) ! Returns an error if the system has no battery.

            rc = dm_freebsd_disk_free(PATH, paths(1), size, used, available, capacity, paths(2))
            if (dm_is_error(rc)) exit io_block

            rc = dm_freebsd_sysctl_cpu_cores(ncore);                        if (dm_is_error(rc)) exit io_block
            rc = dm_freebsd_sysctl_cpu_model(model);                        if (dm_is_error(rc)) exit io_block
            rc = dm_freebsd_sysctl_cpu_temperature(temp);                   if (dm_is_error(rc)) exit io_block
            rc = dm_freebsd_sysctl_memory(phys_mem, real_mem, user_mem);    if (dm_is_error(rc)) exit io_block
            rc = dm_freebsd_sysctl_mqueue(max_mqs, max_msgs, max_size);     if (dm_is_error(rc)) exit io_block
            rc = dm_freebsd_uptime_load_average(avgs(1), avgs(2), avgs(3)); if (dm_is_error(rc)) exit io_block
            rc = dm_freebsd_vmstat_cpu_idle(idle);                          if (dm_is_error(rc)) exit io_block

            print '(" Path...........: ", a)',            PATH
            print '(" File system....: ", a)',            trim(paths(1))
            print '(" Mounted on.....: ", a)',            trim(paths(2))
            print '(" Size...........: ", f0.1, " GiB")', dble(size) / 1024**3
            print '(" Used...........: ", f0.1, " GiB")', dble(used) / 1024**3
            print '(" Available......: ", f0.1, " GiB")', dble(available) / 1024**3
            print '(" Capacity.......: ", i0, " %")',     capacity
            print '(" Physical memory: ", f0.1, " GiB")', dble(phys_mem) / 1024**3
            print '(" Real memory....: ", f0.1, " GiB")', dble(real_mem) / 1024**3
            print '(" User memory....: ", f0.1, " GiB")', dble(user_mem) / 1024**3
            print '(" Battery life...: ", i0, " %")',     life
            print '(" CPU model......: ", a)',            trim(model)
            print '(" CPU cores......: ", i0)',           ncore
            print '(" CPU temperature: ", f0.1, " C")',   temp
            print '(" CPU idle.......: ", i0, " %")',     idle
            print '(" CPU load.......:", 3(" ", f0.2))',  avgs
            print '(" MQ max mqs.....: ", i0)',           max_mqs
            print '(" MQ max msgs....: ", i0)',           max_msgs
            print '(" MQ max size....: ", i0, " bytes")', max_size
        end block io_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer :: rc

        if (.not. is_freebsd(stat)) return

        io_block: block
            integer(kind=i8) :: vmstat(FREEBSD_NVMSTAT)

            print *, 'Executing vmstat ...'
            rc = dm_freebsd_vmstat(vmstat)
            if (dm_is_error(rc)) exit io_block

            print '(" Threads running..: ", i0)',           vmstat(1)
            print '(" Threads blocked..: ", i0)',           vmstat(2)
            print '(" Threads swapped..: ", i0)',           vmstat(3)
            print '(" Virtual memory...: ", f0.1, " GiB")', dble(vmstat(4)) / 1024**3
            print '(" Free memory......: ", f0.1, " GiB")', dble(vmstat(5)) / 1024**3
            print '(" Page faults......: ", i0)',           vmstat(6)
            print '(" Pages reactivated: ", i0)',           vmstat(7)
            print '(" Pages paged in...: ", i0)',           vmstat(8)
            print '(" Pages paged out..: ", i0)',           vmstat(9)
            print '(" Pages freed......: ", i0)',           vmstat(10)
            print '(" Pages scanned....: ", i0)',           vmstat(11)
            print '(" Device interrupts: ", i0)',           vmstat(12)
            print '(" System calls.....: ", i0)',           vmstat(13)
            print '(" Context switches.: ", i0)',           vmstat(14)
            print '(" User time........: ", i0)',           vmstat(15)
            print '(" System time......: ", i0)',           vmstat(16)
            print '(" Idle time........: ", i0)',           vmstat(17)
        end block io_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test02
end program dmtestfreebsd
