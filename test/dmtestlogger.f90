! dmtestlogger.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestlogger
    !! Test program that writes to and reads from the logger queue.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestlogger'
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
        !! Sends and receives log message through POSIX message queue.
        !! Validates serialisation to JSON.
        character(len=*), parameter :: JSON = &
            '{"id":"f5ec2dd3870a47b5be3ae397552706fe","level":4,"error":2,"timestamp":' // &
            '"1970-01-01T00:00:00.000000+00:00","node_id":"test-node","sensor_id":"test-sensor",' // &
            '"target_id":"test-target","observ_id":"6b0ca75ae594425a8d38adfd709b11cd",' // &
            '"message":"test message"}'
        character(len=*), parameter :: LOGGER_NAME  = 'dmtestlogger'
        character(len=*), parameter :: TEST_MESSAGE = 'test message'
        integer,          parameter :: TEST_ERROR   = E_DUMMY
        integer,          parameter :: TEST_LEVEL   = LL_INFO

        character(len=:), allocatable :: buffer
        class(logger_class), pointer  :: logger
        integer                       :: rc
        type(log_type)                :: log1, log2, log3
        type(log_type)                :: logs(1)
        type(posix_mqueue_type)       :: mqueue
        type(observ_type)             :: observ

        stat = TEST_PASSED
        if (dm_test_skip('DM_MQUEUE_SKIP')) return

        stat = TEST_FAILED
        observ%id        = '6b0ca75ae594425a8d38adfd709b11cd'
        observ%node_id   = 'test-node'
        observ%sensor_id = 'test-sensor'
        observ%target_id = 'test-target'

        log1%id        = 'f5ec2dd3870a47b5be3ae397552706fe'
        log1%level     = LL_ERROR
        log1%message   = TEST_MESSAGE
        log1%node_id   = observ%node_id
        log1%sensor_id = observ%sensor_id
        log1%target_id = observ%target_id
        log1%observ_id = observ%id
        log1%timestamp = TIME_DEFAULT
        log1%error     = TEST_ERROR

        print *, 'Initialising logger ...'
        logger => dm_logger_get_default()
        call logger%configure(name=LOGGER_NAME, debug=.true., ipc=.true., no_color=.true.)

        print *, 'Opening log message queue ...'
        if (dm_posix_mqueue_open(mqueue, TYPE_LOG, LOGGER_NAME, POSIX_MQUEUE_RDONLY) /= E_NONE) return

        print *, 'Creating log message ...'
        call logger%log(log1)

        print *, 'Reading from log message queue ...'
        if (dm_posix_mqueue_read(mqueue, log2) /= E_NONE) return

        print *, 'Creating log message ...'
        call logger%info(TEST_MESSAGE, error=TEST_ERROR, observ=observ)

        print *, 'Reading from log message queue ...'
        if (dm_posix_mqueue_read(mqueue, log3) /= E_NONE) return

        print *, 'Closing log message queue ...'
        call dm_posix_mqueue_close(mqueue, rc)
        if (dm_is_error(rc)) return

        print *, 'Unlinking log message queue ...'
        call dm_posix_mqueue_unlink(mqueue, rc)
        if (dm_is_error(rc)) return

        print *, 'Printing log message ...'
        call logger%out(log2)

        print *, 'Validating log message ...'
        if (.not. dm_log_is_valid(log2)) return
        if (.not. (log1 == log2)) return

        print *, 'Validating JSON ...'
        buffer = dm_json_from(log2)
        if (buffer /= JSON) return

        print *, 'Printing log array ...'
        logs(1) = log2
        print *, dm_json_from(logs)

        print *, 'Printing log message ...'
        call logger%out(log3)

        print *, 'Validating log message ...'
        if (.not. dm_log_is_valid(log3)) return
        if (log3%level /= TEST_LEVEL) return
        if (log3%message /= TEST_MESSAGE) return
        if (log3%error /= TEST_ERROR) return

        stat = TEST_PASSED
    end function test01
end program dmtestlogger
