! dmtestcsv.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestcsv
    !! Test program that tries CSV writing/reading.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 5

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestcsv.test01', test01)
    tests(2) = test_type('dmtestcsv.test02', test02)
    tests(3) = test_type('dmtestcsv.test03', test03)
    tests(4) = test_type('dmtestcsv.test04', test04)
    tests(5) = test_type('dmtestcsv.test05', test05)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        real(kind=r8)     :: dt
        type(node_type)   :: nodes(2)
        type(observ_type) :: observs(2)
        type(timer_type)  :: timer

        stat = TEST_FAILED

        call dm_dummy_node(nodes)
        call dm_dummy_observ(observs)

        print *, 'Node to CSV:'
        print '(a)', dm_csv_from(nodes(1))

        print *, 'Nodes to CSV (with header):'
        print '(a)', dm_csv_from(nodes, header=.true.)

        print *, 'Observation to CSV:'
        print '(a)', dm_csv_from(observs(1))

        print *, 'Observations to CSV:'
        call dm_timer_start(timer)
        print '(a)', dm_csv_from(observs, header=.false.)
        dt = dm_timer_stop(timer)
        print '(a, f8.6)', ' Time: ', dt

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer           :: rc
        real(kind=r8)     :: dt
        type(node_type)   :: nodes(2)
        type(observ_type) :: observs(2)
        type(timer_type)  :: timer

        stat = TEST_FAILED

        call dm_dummy_node(nodes)
        call dm_dummy_observ(observs)

        print *, 'Nodes to CSV (write):'
        rc = dm_csv_write(nodes, header=.false.)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Observations to CSV (write):'
        call dm_timer_start(timer)
        rc = dm_csv_write(observs, header=.false.)
        dt = dm_timer_stop(timer)
        print '(a, f8.6)', ' Time: ', dt

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        integer, parameter :: N = 3

        integer             :: i, fu, rc
        type(request_type)  :: request
        type(response_type) :: response
        type(observ_type)   :: observs1(N), observs2(N)

        stat = TEST_FAILED

        print *, 'Opening scratch file ...'
        open (action='readwrite', iostat=rc, newunit=fu, status='scratch')
        if (rc /= 0) return

        call dm_dummy_observ(observs1, nrequests=OBSERV_MAX_NREQUESTS)
        call dm_dummy_request(request)

        response = response_type('dummy', 'none', 0, 999.999_r8)
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
        call dm_perror(rc)

        print *, 'Validating observations ...'
        if (.not. all(observs1 == observs2)) return
        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        integer           :: fu, rc
        type(log_type)    :: log1, log2
        type(node_type)   :: node1, node2
        type(sensor_type) :: sensor1, sensor2
        type(target_type) :: target1, target2

        stat = TEST_FAILED

        call dm_dummy_node(node1)
        call dm_dummy_sensor(sensor1)
        call dm_dummy_target(target1)
        call dm_dummy_log(log1)

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

        call dm_perror(rc)
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
        integer :: n

        stat = TEST_FAILED

        print *, 'Testing CSV header lengths ...'

        n = len(dm_csv_header_beat())
        if (n /= 58) return
        n = len(dm_csv_header_data_point())
        if (n /= 4) return
        n = len(dm_csv_header_log())
        if (n /= 71) return
        n = len(dm_csv_header_node())
        if (n /= 13) return
        n = len(dm_csv_header_observ())
        if (n /= 17922) return
        n = len(dm_csv_header_observ_view())
        if (n /= 153) return
        n = len(dm_csv_header_sensor())
        if (n /= 29) return
        n = len(dm_csv_header_target())
        if (n /= 13) return

        stat = TEST_PASSED
    end function test05
end program dmtestcsv
