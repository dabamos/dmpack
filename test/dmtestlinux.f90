! dmtestlinux.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestlinux
    !! Test program of Linux API module.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestlinux'
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
    logical function is_linux(stat) result(is)
        logical, intent(out) :: stat

        is = (dm_system_type() == SYSTEM_TYPE_LINUX)
        stat = TEST_FAILED

        if (.not. is) then
            stat = TEST_PASSED
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("> This test is for Linux only and will be skipped.")'
            call dm_ansi_reset(no_color)
            return
        end if
    end function is_linux

    logical function test01() result(stat)
        integer :: rc

        if (.not. is_linux(stat)) return

        io_block: block
            character(len=*), parameter :: PATH = '.'

            character(len=64) :: paths(2), model
            integer           :: capacity, idle, ncore
            integer(kind=i8)  :: available, size, used
            real              :: avgs(3), temp

            rc = dm_linux_disk_free(PATH, paths(1), size, used, available, capacity, paths(2))
            if (dm_is_error(rc)) exit io_block

            rc = dm_linux_procfs_cpu_cores(ncore);                        if (dm_is_error(rc)) exit io_block
            rc = dm_linux_procfs_cpu_idle(idle);                          if (dm_is_error(rc)) exit io_block
            rc = dm_linux_procfs_cpu_model(model);                        if (dm_is_error(rc)) exit io_block
            rc = dm_linux_procfs_load_average(avgs(1), avgs(2), avgs(3)); if (dm_is_error(rc)) exit io_block
            rc = dm_linux_sys_cpu_temperature(temp);                      if (dm_is_error(rc)) exit io_block

            print '(" Path...........: ", a)',            PATH
            print '(" File system....: ", a)',            trim(paths(1))
            print '(" Mounted on.....: ", a)',            trim(paths(2))
            print '(" Size...........: ", f0.1, " GiB")', dble(size) / 1024**3
            print '(" Used...........: ", f0.1, " GiB")', dble(used) / 1024**3
            print '(" Available......: ", f0.1, " GiB")', dble(available) / 1024**3
            print '(" Capacity.......: ", i0, " %")',     capacity
            print '(" CPU model......: ", a)',            trim(model)
            print '(" CPU cores......: ", i0)',           ncore
            print '(" CPU temperature: ", f0.1, " C")',   temp
            print '(" CPU idle.......: ", i0, " %")',     idle
            print '(" CPU load.......:", 3(1x, f0.2))',   avgs
        end block io_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01
end program dmtestlinux
