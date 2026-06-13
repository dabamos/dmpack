! dmtestposix.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestposix
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestposix'
    integer,          parameter :: NTESTS    = 2

    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        integer :: rc

        stat = TEST_FAILED

        test_block: block
            character(len=*), parameter :: PATH = '.'

            character(len=256) :: model, name, paths(2)
            integer            :: capacity, ncore, pid
            integer(kind=i8)   :: available, size, used
            real               :: avgs(3), temp

            call dm_posix_pid(pid)

            print *, 'Reading free disk space ...'
            rc = dm_posix_disk_free(PATH, paths(1), size, used, available, capacity, paths(2))
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading CPU cores ...'
            rc = dm_posix_cpu_cores(ncore)
            if (dm_is_error(rc)) print *, 'CPU cores not available'

            print *, 'Reading CPU model ...'
            rc = dm_posix_cpu_model(model)
            if (dm_is_error(rc)) print *, 'CPU model not available'

            print *, 'Reading CPU temperature ...'
            rc = dm_posix_cpu_temperature(temp)
            if (dm_is_error(rc)) print *, 'Temperature not available'

            print *, 'Reading load average ...'
            rc = dm_posix_load_average(avgs(1), avgs(2), avgs(3))
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading host name ...'
            rc = dm_posix_host_name(name)
            if (dm_is_error(rc)) exit test_block

            print '(" Path...........: ", a)',          PATH
            print '(" File system....: ", a)',          trim(paths(1))
            print '(" Mounted on.....: ", a)',          trim(paths(2))
            print '(" Size...........: ", a)',          dm_size_to_human(size)
            print '(" Used...........: ", a)',          dm_size_to_human(used)
            print '(" Available......: ", a)',          dm_size_to_human(available)
            print '(" Capacity.......: ", i0, " %")',   capacity
            print '(" CPU model......: ", a)',          trim(model)
            print '(" CPU cores......: ", i0)',         ncore
            print '(" CPU temperature: ", f0.1, " C")', temp
            print '(" CPU load.......:", 3(1x, f0.2))', avgs
            print '(" Host name......: ", a)',          trim(name)
            print '(" PID............: ", i0)',         pid
        end block test_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(80) :: argv(3), command, path
        integer       :: pid, rc
        logical       :: running

        stat = TEST_FAILED

        path    = '/tmp/dmpack-test-' // dm_uuid_new()
        running = .false.

        test_block: block
            print *, 'Creating temporary file ' // trim(path) // ' ...'
            call dm_file_touch(path, error=rc)
            if (dm_is_error(rc)) exit test_block

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) exit test_block

            command = '/usr/bin/tail'
            argv(1) = 'tail'
            argv(2) = '-f'
            argv(3) = path

            print *, 'Spawning process ' // trim(command) // ' ...'
            call dm_posix_spawn(pid, command, argv, error=rc)
            if (dm_is_error(rc)) exit test_block

            print '(" Searching for process ", i0, " ...")', pid
            call dm_posix_wait_pid(pid, blocking=.false., running=running, error=rc)
            if (.not. running) print '(" Process ", i0, " is not running")', pid
            if (dm_is_error(rc)) exit test_block

            print '(" Killing process ", i0, " ...")', pid
            call dm_posix_kill(pid, POSIX_SIGNAL_SIGTERM, error=rc)
            if (dm_is_error(rc)) exit test_block

            print '(" Waiting for process ", i0, " to be killed ...")', pid
            call dm_posix_msleep(10)

            print '(" Searching for process ", i0, " ...")', pid
            call dm_posix_wait_pid(pid, blocking=.false., running=running, error=rc)
            if (running) exit test_block

            print '(" Process ", i0, " killed")', pid
        end block test_block

        call dm_file_delete(path)

        if (rc == E_SYSTEM) then
            print '(" Error: ", a)', dm_posix_error_message()
        end if

        if (running) then
            print '(" Error: Process ", i0, " not killed")', pid
        end if

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test02
end program dmtestposix
