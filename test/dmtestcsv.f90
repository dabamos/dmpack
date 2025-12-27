! dmtestcsv.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestcsv
    !! Test program that tries CSV writing/reading.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestcsv'
    integer,          parameter :: NTESTS    = 5

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04), &
        test_type('test05', test05)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        !! Prints nodes and observations to stdout in CSV format.
        real(kind=r8)     :: dt
        type(node_type)   :: nodes(2)
        type(observ_type) :: observs(2)
        type(timer_type)  :: timer

        stat = TEST_FAILED

        call dm_test_dummy(nodes)
        call dm_test_dummy(observs)

        print *, 'Node to CSV:'
        print '(72("."))'
        print '(a)', dm_csv_from(nodes(1))
        print '(72("."))'

        print *, 'Nodes to CSV (with header):'
        print '(72("."))'
        print '(a)', dm_csv_from(nodes, header=.true.)
        print '(72("."))'

        print *, 'Observation to CSV:'
        print '(72("."))'
        print '(a)', dm_csv_from(observs(1))
        print '(72("."))'

        print *, 'Observations to CSV:'
        print '(72("."))'
        call dm_timer_start(timer)
        print '(a)', dm_csv_from(observs, header=.false.)
        call dm_timer_stop(timer, dt)
        print '(72("."))'
        print '(a, f8.6)', ' Time: ', dt

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! Prints nodes and observations to stdout in CSV format.
        integer           :: rc
        real(kind=r8)     :: dt
        type(node_type)   :: nodes(2)
        type(observ_type) :: observs(2)
        type(timer_type)  :: timer

        stat = TEST_FAILED

        call dm_test_dummy(nodes)
        call dm_test_dummy(observs)

        print *, 'Nodes to CSV (write):'
        print '(72("."))'
        rc = dm_csv_write(nodes, header=.false.)
        print '(72("."))'

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Observations to CSV (write):'
        print '(72("."))'
        call dm_timer_start(timer)
        rc = dm_csv_write(observs, header=.false.)
        call dm_timer_stop(timer, dt)
        print '(72("."))'
        print '(a, f8.6)', ' Time: ', dt

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        !! Writes and reads observations in CSV to/from scratch file.
        integer, parameter :: N = 3

        integer             :: i, fu, rc
        type(request_type)  :: request
        type(response_type) :: response
        type(observ_type)   :: observs1(N), observs2(N)

        stat = TEST_FAILED

        print *, 'Opening scratch file ...'
        open (action='readwrite', iostat=rc, newunit=fu, status='scratch')
        if (rc /= 0) return

        call dm_test_dummy(observs1, nrequests=OBSERV_MAX_NREQUESTS)
        call dm_test_dummy(request)

        response = response_type('dummy', 'none', value=999.999_r8)
        observs1(1)%requests(OBSERV_MAX_NREQUESTS)%nresponses = REQUEST_MAX_NRESPONSES
        observs1(1)%requests(OBSERV_MAX_NREQUESTS)%responses(REQUEST_MAX_NRESPONSES) = response

        print *, 'Writing observations to scratch file ...'
        rc = dm_csv_write(observs1, unit=fu, header=.true.)
        call dm_error_out(rc)

        rewind (fu)
        print *, 'Reading observations from scratch file ...'
        i = 1

        do
            rc = E_NONE
            if (i > N) exit
            rc = dm_csv_read(observs2(i), unit=fu, separator=',', quote='"')
            if (rc == E_EOR) cycle
            if (dm_is_error(rc)) exit
            i = i + 1
        end do

        close (fu)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Validating observations ...'
        if (.not. all(observs1 == observs2)) return
        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        !! Writes and reads CSV to/from scratch file.
        integer           :: fu, rc
        type(log_type)    :: log1, log2
        type(node_type)   :: node1, node2
        type(sensor_type) :: sensor1, sensor2
        type(target_type) :: target1, target2

        stat = TEST_FAILED

        call dm_test_dummy(node1)
        call dm_test_dummy(sensor1)
        call dm_test_dummy(target1)
        call dm_test_dummy(log1)

        print *, 'Opening scratch file ...'
        open (action='readwrite', iostat=rc, newunit=fu, status='scratch')
        if (rc /= 0) return

        csv_block: block
            print *, 'Writing log to scratch file ...'
            rc = dm_csv_write(log1, unit=fu)
            if (dm_is_error(rc)) exit csv_block
            rewind (fu)
            print *, 'Reading log from scratch file ...'
            rc = dm_csv_read(log2, unit=fu, separator=',', quote='"')
            if (dm_is_error(rc)) exit csv_block
            rewind (fu)

            print *, 'Writing node to scratch file ...'
            rc = dm_csv_write(node1, unit=fu)
            if (dm_is_error(rc)) exit csv_block
            rewind (fu)
            print *, 'Reading node from scratch file ...'
            rc = dm_csv_read(node2, unit=fu, separator=',', quote='"')
            if (dm_is_error(rc)) exit csv_block
            rewind (fu)

            print *, 'Writing sensor to scratch file ...'
            rc = dm_csv_write(sensor1, unit=fu)
            if (dm_is_error(rc)) exit csv_block
            rewind (fu)
            print *, 'Reading sensor from scratch file ...'
            rc = dm_csv_read(sensor2, unit=fu, separator=',', quote='"')
            if (dm_is_error(rc)) exit csv_block
            rewind (fu)

            print *, 'Writing target to scratch file ...'
            rc = dm_csv_write(target1, unit=fu)
            if (dm_is_error(rc)) exit csv_block
            rewind (fu)
            print *, 'Reading target from scratch file ...'
            rc = dm_csv_read(target2, unit=fu, separator=',', quote='"')
            if (dm_is_error(rc)) exit csv_block
            rewind (fu)
        end block csv_block

        close (fu)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Validating log ...'
        if (.not. (log1 == log2)) return
        print *, 'Validating node ...'
        if (.not. (node1 == node2)) return
        print *, 'Validating sensor ...'
        if (.not. (sensor1 == sensor2)) return
        print *, 'Validating target ...'
        if (.not. (target1 == target2)) return

        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        !! Tests header string lengths.
        integer, parameter :: LEN_BEAT   = 66    !! Beats header length.
        integer, parameter :: LEN_DP     = 4     !! Data points header length.
        integer, parameter :: LEN_LOG    = 71    !! Logs header length.
        integer, parameter :: LEN_NODE   = 48    !! Nodes header length.
        integer, parameter :: LEN_OBSERV = 22100 !! Observations header length.
        integer, parameter :: LEN_VIEW   = 180   !! Observation views header length.
        integer, parameter :: LEN_SENSOR = 64    !! Sensors header length.
        integer, parameter :: LEN_TARGET = 54    !! Targets header length.

        integer :: n

        stat = TEST_FAILED

        print *, 'Testing CSV header lengths ...'

        print *, '-- beat'
        n = len(dm_csv_header_beat())
        if (n /= LEN_BEAT) then
            print *, dm_csv_header_beat()
            print '(" Error: expected ", i0, ", got ", i0)', LEN_BEAT, n
            return
        end if

        print *, '-- data point'
        n = len(dm_csv_header_dp())
        if (n /= LEN_DP) then
            print *, dm_csv_header_dp()
            print '(" Error: expected ", i0, ", got ", i0)', LEN_DP, n
            return
        end if

        print *, '-- log'
        n = len(dm_csv_header_log())
        if (n /= LEN_LOG) then
            print *, dm_csv_header_log()
            print '(" Error: expected ", i0, ", got ", i0)', LEN_LOG, n
            return
        end if

        print *, '-- node'
        n = len(dm_csv_header_node())
        if (n /= LEN_NODE) then
            print *, dm_csv_header_node()
            print '(" Error: expected ", i0, ", got ", i0)', LEN_NODE, n
            return
        end if

        print *, '-- observ'
        n = len(dm_csv_header_observ())
        if (n /= LEN_OBSERV) then
            print *, dm_csv_header_observ()
            print '(" Error: expected ", i0, ", got ", i0)', LEN_OBSERV, n
            return
        end if

        print *, '-- observ_view'
        n = len(dm_csv_header_observ_view())
        if (n /= LEN_VIEW) then
            print *, dm_csv_header_observ_view()
            print '(" Error: expected ", i0, ", got ", i0)', LEN_VIEW, n
            return
        end if

        print *, '-- sensor'
        n = len(dm_csv_header_sensor())
        if (n /= LEN_SENSOR) then
            print *, dm_csv_header_sensor()
            print '(" Error: expected ", i0, ", got ", i0)', LEN_SENSOR, n
            return
        end if

        print *, '-- target'
        n = len(dm_csv_header_target())
        if (n /= LEN_TARGET) then
            print *, dm_csv_header_target()
            print '(" Error: expected ", i0, ", got ", i0)', LEN_TARGET, n
            return
        end if

        stat = TEST_PASSED
    end function test05
end program dmtestcsv
