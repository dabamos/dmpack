! dmtestsystem.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestsystem
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestsystem'
    integer,          parameter :: NTESTS    = 1

    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        integer :: rc

        stat = TEST_FAILED

        io_block: block
            character(len=*), parameter :: PATH = '.'

            character(len=256) :: model, name, paths(2)
            integer            :: capacity, ncore, pid
            integer(kind=i8)   :: available, size, used
            real               :: avgs(3), temp

            call dm_system_pid(pid)

            print *, 'Reading free disk space ...'
            rc = dm_system_disk_free(PATH, paths(1), size, used, available, capacity, paths(2))
            if (dm_is_error(rc)) exit io_block

            print *, 'Reading CPU cores ...'
            rc = dm_system_cpu_cores(ncore)
            if (dm_is_error(rc)) exit io_block

            print *, 'Reading CPU model ...'
            rc = dm_system_cpu_model(model)
            if (dm_is_error(rc)) exit io_block

            print *, 'Reading CPU temperature ...'
            rc = dm_system_cpu_temperature(temp)
            if (dm_is_error(rc)) print *, 'No temperature available'

            print *, 'Reading load average ...'
            rc = dm_system_load_average(avgs(1), avgs(2), avgs(3))
            if (dm_is_error(rc)) exit io_block

            print *, 'Reading host name ...'
            rc = dm_system_host_name(name)
            if (dm_is_error(rc)) exit io_block

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
            print '(" CPU load.......:", 3(1x, f0.2))',   avgs
            print '(" Host name......: ", a)',            trim(name)
            print '(" PID............: ", i0)',           pid
        end block io_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01
end program dmtestsystem
