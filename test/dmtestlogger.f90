! dmtestlogger.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestlogger
    !! Test program that writes to and reads from the logger queue.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    call dm_init()
    no_color = dm_env_has('NO_COLOR')

    tests(1) = test_type('dmtestlogger.test01', test01)

    call dm_test_run(tests, stats, no_color)
contains
    logical function skip_test() result(skip)
        integer :: rc

        rc = dm_env_get('DM_MQUEUE_SKIP', skip)

        if (skip) then
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("dmtestlogger:")'
            print '("    Environment variable DM_MQUEUE_SKIP is set.")'
            print '("    This test will be skipped.")'
            call dm_ansi_reset(no_color)
        end if
    end function skip_test

    logical function test01() result(stat)
        character(len=*), parameter   :: JSON = &
            '{ "id": "f5ec2dd3870a47b5be3ae397552706fe", "level": 4, "error": 1, "timestamp": ' // &
            '"1970-01-01T00:00:00.000000+00:00", "node_id": "test-node", "sensor_id": "test-sensor", ' // &
            '"target_id": "test-target", "observ_id": "6b0ca75ae594425a8d38adfd709b11cd", "message": "test" }'
        character(len=*), parameter   :: LOGGER_NAME  = 'dmlogger'
        character(len=*), parameter   :: TEST_MESSAGE = 'test'

        character(len=:), allocatable :: buffer
        type(log_type)                :: log1, log2
        type(log_type)                :: logs(1)
        type(mqueue_type)             :: mqueue
        type(observ_type)             :: observ

        stat = TEST_PASSED
        if (skip_test()) return

        stat = TEST_FAILED
        observ%id        = '6b0ca75ae594425a8d38adfd709b11cd'
        observ%node_id   = 'test-node'
        observ%sensor_id = 'test-sensor'
        observ%target_id = 'test-target'

        log1%id        = 'f5ec2dd3870a47b5be3ae397552706fe'
        log1%level     = LOG_ERROR
        log1%message   = TEST_MESSAGE
        log1%node_id   = observ%node_id
        log1%sensor_id = observ%sensor_id
        log1%target_id = observ%target_id
        log1%observ_id = observ%id
        log1%timestamp = TIME_DEFAULT
        log1%error     = E_ERROR

        print *, 'Initialising logger ...'
        call dm_logger_init(ipc=.true., no_color=dm_env_has('NO_COLOR'))

        print *, 'Creating log message ...'
        call dm_logger_log(log1)

        print *, 'Opening log message queue ...'
        if (dm_mqueue_open(mqueue, TYPE_LOG, LOGGER_NAME, MQUEUE_RDONLY) /= E_NONE) return

        print *, 'Reading from log message queue ...'
        if (dm_mqueue_read(mqueue, log2) /= E_NONE) return

        print *, 'Printing log message ...'
        call dm_logger_out(log2)

        print *, 'Closing log message queue ...'
        if (dm_mqueue_close(mqueue) /= E_NONE) return

        print *, 'Validating log message ...'
        if (.not. dm_log_valid(log2)) return
        if (.not. (log1 == log2)) return

        print *, 'Validating JSON ...'
        buffer = dm_json_from(log2)
        if (buffer /= JSON) return

        print *, 'Printing log array ...'
        logs(1) = log2
        print *, dm_json_from(logs)

        stat = TEST_PASSED
    end function test01
end program dmtestlogger
