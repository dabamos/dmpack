! dmtestdb.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestdb
    !! Tests database access using `dm_db` module.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestdb'
    integer,          parameter :: NTESTS    = 20

    character(len=*), parameter :: DB_BEAT          = 'testbeat.sqlite'
    character(len=*), parameter :: DB_LOG           = 'testlog.sqlite'
    character(len=*), parameter :: DB_OBSERV        = 'testobserv.sqlite'
    character(len=*), parameter :: DB_OBSERV_BACKUP = 'testobserv_backup.sqlite'
    character(len=*), parameter :: DB_OBSERV_VACUUM = 'testobserv_vacuum.sqlite'
    character(len=*), parameter :: DB_TRANSFER      = 'testtransfer.sqlite'

    integer, parameter :: NLOGS    = 100
    integer, parameter :: NOBSERVS = 1000

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04), &
        test_type('test05', test05), &
        test_type('test06', test06), &
        test_type('test07', test07), &
        test_type('test08', test08), &
        test_type('test09', test09), &
        test_type('test10', test10), &
        test_type('test11', test11), &
        test_type('test12', test12), &
        test_type('test13', test13), &
        test_type('test14', test14), &
        test_type('test15', test15), &
        test_type('test16', test16), &
        test_type('test17', test17), &
        test_type('test18', test18), &
        test_type('test19', test19), &
        test_type('test20', test20)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        !! Creates observation database.
        character(len=SQL_TABLE_NAME_LEN), allocatable :: tables(:)

        integer       :: error, i, rc
        type(db_type) :: db

        stat = TEST_FAILED

        print '(" Library: ", a)', dm_db_version(.true.)
        print *, 'Checking for stale database "' // DB_OBSERV // '" ...'

        if (dm_file_exists(DB_OBSERV)) then
            print *, 'Deleting stale database ...'
            call dm_file_delete(DB_OBSERV)
        end if

        print *, 'Creating database "' // DB_OBSERV // '" ...'
        rc = dm_db_open(db, DB_OBSERV, create=.true., wal=.true.)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Validating application id ...'
            rc = dm_db_validate(db)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating tables ...'
            rc = dm_db_table_create_observs(db, sync=.true.)
            if (dm_is_error(rc)) exit test_block

            rc = dm_db_table_select(db, tables)
            if (dm_is_error(rc)) exit test_block

            print *, 'Tables:'
            do i = 1, size(tables)
                print *, ' - ' // trim(tables(i))
            end do
        end block test_block

        if (dm_is_error(rc)) then
            call dm_error_out(rc)
            print *, dm_db_error_message(db)
        end if

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db, error=error)
        if (dm_is_error(rc)) return
        if (dm_is_error(error)) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! Tests writing/reading/deleting of nodes.
        integer                      :: error, i, rc
        type(db_type)                :: db
        type(node_type), allocatable :: in(:), out(:)

        stat = TEST_FAILED

        allocate (in(3))

        ! Sensor nodes.
        in = [ &
            node_type('a', 'Test Node A', 'Description of test node.'), &
            node_type('b', 'Test Node B', 'Description of test node.'), &
            node_type('z', 'Test Node Z', 'Description of test node.')  &
        ]

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        rc = dm_db_open(db, DB_OBSERV)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Adding nodes ...'
            do i = 1, size(in)
                rc = dm_db_insert_node(db, in(i))
                if (dm_is_error(rc)) exit test_block
                print *, 'Added node "' // trim(in(i)%name) // '"'
            end do

            print *, 'Selecting nodes ...'
            rc = dm_db_select_nodes(db, out)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating number of nodes ...'
            if (.not. allocated(out)) return
            if (size(in) /= size(out)) return

            print *, 'Validating nodes ...'
            rc = E_INVALID
            do i = 1, size(out)
                if (.not. (in(i) == out(i))) then
                    print *, 'Expected:'
                    print '(72("."))'
                    call dm_node_out(in(i))
                    print '(72("."))'
                    print *, 'Received:'
                    print '(72("."))'
                    call dm_node_out(out(i))
                    print '(72("."))'
                    exit test_block
                end if
            end do

            print *, 'Deleting ...'
            do i = 1, size(out)
                print *, 'Deleting "' // trim(in(i)%id) // '" ...'
                rc = dm_db_delete_node(db, in(i)%id)
                if (dm_is_error(rc)) exit test_block
            end do
            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db, error=error)
        if (dm_is_error(rc)) return
        if (dm_is_error(error)) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        !! Tests writing/reading of nodes and sensors.
        integer           :: error, rc
        type(db_type)     :: db
        type(node_type)   :: node1, node2
        type(sensor_type) :: sensor1, sensor2
        type(target_type) :: target1, target2

        stat = TEST_FAILED

        call dm_test_dummy(node1)
        call dm_test_dummy(sensor1)
        call dm_test_dummy(target1)

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        rc = dm_db_open(db, DB_OBSERV)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Adding node ...'
            rc = dm_db_insert_node(db, node1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Adding sensor ...'
            rc = dm_db_insert_sensor(db, sensor1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Adding target ...'
            rc = dm_db_insert_target(db, target1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Selecting node ...'
            rc = dm_db_select_node(db, node2, node1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Selecting sensor ...'
            rc = dm_db_select_sensor(db, sensor2, sensor1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Selecting target ...'
            rc = dm_db_select_target(db, target2, target1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Deleting target ...'
            rc = dm_db_delete_target(db, target1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Deleting sensor ...'
            rc = dm_db_delete_sensor(db, sensor1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Deleting node ...'
            rc = dm_db_delete_node(db, node1%id)
            if (dm_is_error(rc)) exit test_block

            rc = E_INVALID
            print *, 'Matching nodes ...'
            if (.not. (node1 == node2)) then
                print *, 'Expected:'
                print '(72("."))'
                call dm_node_out(node1)
                print '(72("."))'
                print *, 'Received:'
                print '(72("."))'
                call dm_node_out(node2)
                print '(72("."))'
                exit test_block
            end if

            print *, 'Matching sensors ...'
            if (.not. (sensor1 == sensor2)) then
                print *, 'Expected:'
                print '(72("."))'
                call dm_sensor_out(sensor1)
                print '(72("."))'
                print *, 'Received:'
                print '(72("."))'
                call dm_sensor_out(sensor2)
                print '(72("."))'
                exit test_block
            end if

            print *, 'Matching targets ...'
            if (.not. (target1 == target2)) then
                print *, 'Expected:'
                print '(72("."))'
                call dm_target_out(target1)
                print '(72("."))'
                print *, 'Received:'
                print '(72("."))'
                call dm_target_out(target2)
                print '(72("."))'
                exit test_block
            end if
            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db, error=error)
        if (dm_is_error(rc)) return
        if (dm_is_error(error)) return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        !! Observation sync.
        integer           :: error, rc
        type(db_type)     :: db
        type(observ_type) :: observ
        type(node_type)   :: node
        type(sensor_type) :: sensor
        type(sync_type)   :: sync1, sync2
        type(target_type) :: target

        stat = TEST_FAILED

        call dm_test_dummy(node)
        call dm_test_dummy(sensor)
        call dm_test_dummy(target)
        call dm_test_dummy(observ)

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        if (dm_db_open(db, DB_OBSERV) /= E_NONE) return

        test_block: block
            print *, 'Adding node ...'
            rc = dm_db_insert_node(db, node)
            if (dm_is_error(rc)) exit test_block

            print *, 'Adding sensor ...'
            rc = dm_db_insert_sensor(db, sensor)
            if (dm_is_error(rc)) exit test_block

            print *, 'Adding target ...'
            rc = dm_db_insert_target(db, target)
            if (dm_is_error(rc)) exit test_block

            print *, 'Adding observation ...'
            rc = dm_db_insert_observ(db, observ)
            if (dm_is_error(rc)) exit test_block

            sync1 = sync_type(type      = SYNC_TYPE_OBSERV, &
                              id        = observ%id, &
                              timestamp = dm_time_now(), &
                              code      = 500, &
                              attempts = 1)

            print *, 'Inserting sync ...'
            rc = dm_db_insert_sync_observ(db, sync1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Selecting sync ...'
            rc = dm_db_select_sync_observ(db, sync2)
            if (dm_is_error(rc)) exit test_block
        end block test_block

        call dm_error_out(rc)
        if (rc >= E_DB) print *, dm_db_error_message(db)

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db, error=error)
        if (dm_is_error(rc)) return
        if (dm_is_error(error)) return

        print *, 'Matching sync data ...'
        if (.not. (sync1 == sync2)) return

        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        !! Tests writing/reading of observation.
        character(len=ID_LEN), allocatable :: ids(:)
        integer                            :: error, i, rc
        type(db_type)                      :: db
        type(observ_type)                  :: observ1, observ2

        stat = TEST_FAILED

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        rc = dm_db_open(db, DB_OBSERV)
        if (dm_is_error(rc)) return

        call dm_test_dummy(observ1)
        observ1%priority = 100

        print *, 'Created observation "' // observ1%id // '"'

        test_block: block
            print *, 'Adding observation ...'
            rc = dm_db_insert_observ(db, observ1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading observation ...'
            rc = dm_db_select_observ(db, observ2, observ1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading observation ids ...'
            rc = dm_db_select_observ_ids(db, ids, 'dummy-node', 'dummy-sensor', 'dummy-target', &
                                         desc=.true., limit=10_i8)
            if (dm_is_error(rc)) exit test_block

            print *, 'Deleting observation ...'
            rc = dm_db_delete_observ(db, observ1%id)
            if (dm_is_error(rc)) exit test_block
        end block test_block

        call dm_error_out(rc)
        if (rc >= E_DB) print *, dm_db_error_message(db)

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db, error=error)
        if (dm_is_error(rc)) return
        if (dm_is_error(error)) return

        print *, 'Matching observations ...'
        if (.not. (observ1 == observ2)) return

        print *, 'Validating id ...'
        do i = 1, size(ids)
            print *, ids(i)
        end do

        if (size(ids) == 0) return
        if (ids(1) /= observ1%id) return

        stat = TEST_PASSED
    end function test05

    logical function test06() result(stat)
        !! Tests writing of observation.
        integer, parameter :: N = 100

        character(len=TIME_LEN) :: timestamp
        integer                 :: error, i, rc
        real(kind=r8)           :: dt, r(6)
        type(db_type)           :: db
        type(timer_type)        :: t

        type(node_type)   :: node1
        type(sensor_type) :: sensor1, sensor2
        type(target_type) :: target1, target2

        type(observ_type), allocatable :: observs1(:), observs2(:)

        stat = TEST_FAILED
        allocate (observs1(N), observs2(N))

        call dm_test_dummy(node1)
        call dm_test_dummy(sensor1)
        call dm_test_dummy(sensor2, id='dummy-sensor2', name='Sensor 2')
        call dm_test_dummy(target1)
        call dm_test_dummy(target2, id='dummy-target2', name='Target 2')

        do i = 1, N
            call random_number(r)
            timestamp = dm_time_create(year    = 2022, &
                                       month   = 1, &
                                       day     = 1 + int(r(1) * 27), &
                                       hour    = int(r(2) * 23), &
                                       minute  = int(r(3) * 60), &
                                       second  = int(r(4) * 60), &
                                       usecond = int(r(5) * 1000))
            call dm_test_dummy(observs1(i), timestamp=timestamp, response_value=sin(r(6) * PI))
        end do

        observs2 = observs1

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        rc = dm_db_open(db, DB_OBSERV)
        if (dm_is_error(rc)) return

        test_block: block
            ! Ignore the constraint errors.
            print *, 'Adding node ...'
            rc = dm_db_insert_node(db, node1)
            print *, 'Adding sensor ...'
            rc = dm_db_insert_sensor(db, sensor1)
            print *, 'Adding sensor ...'
            rc = dm_db_insert_sensor(db, sensor2)
            print *, 'Adding target ...'
            rc = dm_db_insert_target(db, target1)
            print *, 'Adding target ...'
            rc = dm_db_insert_target(db, target2)

            print *, 'Adding observations sequentially ...'
            call dm_timer_start(t)
            do i = 1, N
                rc = dm_db_insert_observ(db, observs1(i))
                if (dm_is_error(rc))  exit test_block
            end do
            call dm_timer_stop(t, dt)

            print '(1x, i0, " observations written in ", f0.3, " sec (", i0, " per sec)")', N, dt, int(1 / (dt / N))
            print *, 'Selecting and matching observations ...'

            do i = 1, N
                rc = dm_db_select_observ(db, observs2(i), observ_id=observs1(i)%id)
                if (dm_is_error(rc)) exit test_block
                rc = E_INVALID
                if (.not. (observs1(i) == observs2(i))) exit test_block
            end do
            rc = E_NONE
        end block test_block

        if (dm_is_error(rc)) call dm_error_out(dm_db_error(db))
        if (rc >= E_DB) print *, dm_db_error_message(db)

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db, error=error)
        if (dm_is_error(rc)) return
        if (dm_is_error(error)) return

        stat = TEST_PASSED
    end function test06

    logical function test07() result(stat)
        !! Benchmark reading and writing of observations.
        character(NODE_ID_LEN)   :: node_id
        character(SENSOR_ID_LEN) :: sensor_id
        character(TARGET_ID_LEN) :: target_id
        character(TIME_LEN)      :: from, timestamp

        integer          :: error, i, rc
        integer(i8)      :: nobs
        real(r8)         :: dt
        type(db_type)    :: db
        type(timer_type) :: t

        type(node_type)   :: node1
        type(sensor_type) :: sensor1
        type(target_type) :: target1

        type(observ_type),      allocatable :: observs1(:), observs2(:)
        type(observ_view_type), allocatable :: views(:)

        stat = TEST_FAILED
        allocate (observs1(NOBSERVS))

        call dm_test_dummy(node1)
        call dm_test_dummy(sensor1)
        call dm_test_dummy(target1)

        do i = 1, NOBSERVS
            timestamp = dm_time_create(year=1992, month=6, day=1, hour=10, minute=0, second=0, usecond =modulo(i, 9999))
            call dm_test_dummy(observs1(i), timestamp=timestamp, response_value=sin((1.0 / NOBSERVS) * i * PI))
        end do

        node_id   = observs1(1)%node_id
        sensor_id = observs1(1)%sensor_id
        target_id = observs1(1)%target_id
        from      = observs1(1)%timestamp

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        rc = dm_db_open(db, DB_OBSERV)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Tuning database ...'
            rc = dm_db_set_synchronous (db, DB_SYNCHRONOUS_OFF);        if (dm_is_error(rc)) exit test_block
            rc = dm_db_set_cache_size  (db, 1000000_i8);                if (dm_is_error(rc)) exit test_block
            rc = dm_db_set_locking_mode(db, DB_LOCKING_MODE_EXCLUSIVE); if (dm_is_error(rc)) exit test_block
            rc = dm_db_set_temp_store  (db, DB_TEMP_STORE_MEMORY);      if (dm_is_error(rc)) exit test_block

            print *, 'Bulk adding observations ...'
            call dm_timer_start(t)
            rc = dm_db_insert_observs(db, observs1)
            call dm_timer_stop(t, dt)
            if (dm_is_error(rc)) exit test_block
            print '(1x, i0, " observations written in ", f0.3, " sec (", i0, " per sec)")', size(observs1), dt, int(1 / (dt / size(observs1)))

            print *, 'Selecting observations ...'
            call dm_timer_start(t)
            rc = dm_db_select_observs(db, observs2, node_id=node_id, sensor_id=sensor_id, target_id=target_id, from=from, limit=int(NOBSERVS, kind=i8), nobservs=nobs)
            call dm_timer_stop(t, dt)
            if (dm_is_error(rc)) exit test_block
            print '(1x, i0, " observations read in ", f0.3, " sec (", i0, " per sec)")', nobs, dt, int(1 / (dt / nobs))
            if (size(observs2) /= size(observs1)) return

            print *, 'Selecting observations by id range ...'
            print *, 'From: ', observs1(1)%id
            print *, 'To:   ', observs1(NOBSERVS)%id

            deallocate (observs2)
            call dm_timer_start(t)
            rc = dm_db_select_observs_from(db, observs2, from_id=observs1(1)%id, to_id=observs1(NOBSERVS)%id, nobservs=nobs)
            call dm_timer_stop(t, dt)
            print '(1x, i0, " observations read in ", f0.3, " sec (", i0, " per sec)")', nobs, dt, int(1 / (dt / nobs))
            if (dm_is_error(rc)) exit test_block
            if (size(observs2) == 0) return

            print *, 'Selecting observation views ...'
            call dm_timer_start(t)
            rc = dm_db_select_observ_views(db, views, node1%id, sensor1%id, target1%id, observs1(1)%requests(1)%responses(1)%name, '1992', '1993', nviews=nobs)
            call dm_timer_stop(t, dt)
            if (dm_is_error(rc)) exit test_block
            print '(1x, i0, " observation views read in ", f0.3, " sec (", i0, " per sec)")', nobs, dt, int(1 / (dt / nobs))

            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db, error=error)
        if (dm_is_error(rc)) return
        if (dm_is_error(error)) return

        stat = TEST_PASSED
    end function test07

    logical function test08() result(stat)
        !! Tests creation of log database
        integer       :: rc
        type(db_type) :: db

        stat = TEST_FAILED

        print *, 'Checking for stale database "' // DB_LOG // '" ...'

        if (dm_file_exists(DB_LOG)) then
            call dm_file_delete(DB_LOG)
            print *, 'Deleted stale database'
        end if

        print *, 'Creating database "' // DB_LOG // '" ...'
        if (dm_db_open(db, DB_LOG, create=.true., wal=.true.) /= E_NONE) return

        test_block: block
            print *, 'Validating application id ...'
            rc = dm_db_validate(db)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating tables ...'
            rc = dm_db_table_create_logs(db, sync=.true.)
            if (dm_is_error(rc)) exit test_block
        end block test_block

        call dm_error_out(rc)
        if (rc >= E_DB) print *, dm_db_error_message(db)

        print *, 'Closing database "' // DB_LOG // '" ...'
        call dm_db_close(db)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test08

    logical function test09() result(stat)
        !! Tests writing/reading of log.
        integer          :: i, rc
        real(kind=r8)    :: dt
        type(db_type)    :: db
        type(timer_type) :: t

        type(log_type)              :: log1, log2
        type(log_type), allocatable :: logs(:)

        stat = TEST_FAILED

        print *, 'Opening database "' // DB_LOG // '" ...'
        if (dm_db_open(db, DB_LOG) /= E_NONE) return

        test_block: block
            call dm_test_dummy(log1)

            if (.not. dm_log_is_valid(log1)) then
                print *, 'Error: invalid dummy log'
                exit test_block
            end if

            print *, 'Adding log ...'
            rc = dm_db_insert_log(db, log1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Selecting log ...'
            rc = dm_db_select_log(db, log2, log1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Matching logs ...'
            if (.not. (log1 == log2)) exit test_block

            print *, 'Adding logs ...'
            allocate (logs(NLOGS), stat=rc)
            if (rc /= 0) exit test_block

            call dm_test_dummy(logs)

            call dm_timer_start(t)
            do i = 1, NLOGS
                rc = dm_db_insert_log(db, logs(i))
                if (dm_is_error(rc)) exit test_block
            end do
            call dm_timer_stop(t, dt)

            print '(1x, i0, a, f0.3, a)', NLOGS, ' logs added in ', dt, ' sec'
        end block test_block

        call dm_error_out(rc)
        if (rc >= E_DB) print *, dm_db_error_message(db)

        print *, 'Closing database "' // DB_LOG // '" ...'
        call dm_db_close(db)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test09

    logical function test10() result(stat)
        !! Tests reading of logs.
        integer          :: rc
        integer(kind=i8) :: n
        real(kind=r8)    :: dt

        type(db_type)               :: db
        type(log_type), allocatable :: logs(:)
        type(timer_type)            :: t

        stat = TEST_FAILED

        print *, 'Opening database "' // DB_LOG // '" ...'
        if (dm_db_open(db, DB_LOG) /= E_NONE) return

        test_block: block
            print *, 'Selecting log messages ...'
            call dm_timer_start(t)
            rc = dm_db_select_logs(db, logs, limit=int(NLOGS, i8), nlogs=n)
            call dm_timer_stop(t, dt)
            print '(1x, a, i0, a, f0.3, a)', 'Selected ', n, ' logs in ', dt, ' sec'
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_LOG // '" ...'
        call dm_db_close(db)
        if (dm_is_error(rc)) return
        if (n < NLOGS) return

        stat = TEST_PASSED
    end function test10

    logical function test11() result(stat)
        !! Tests database error handler.
        integer       :: rc
        type(db_type) :: db

        stat = TEST_FAILED

        ! Add error log handler.
        rc = dm_db_shutdown()
        rc = dm_db_set_log_callback(log_callback)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Opening database "' // DB_LOG // '" ...'
        if (dm_db_open(db, DB_LOG) /= E_NONE) return

        print *, 'Testing error logging ...'
        call dm_db_log(1, 'TEST LOG')

        print *, 'Closing database "' // DB_LOG // '" ...'
        call dm_db_close(db, error=rc)
        if (rc /= E_NONE) return

        stat = TEST_PASSED
    end function test11

    logical function test12() result(stat)
        !! Tests database backup.
        integer       :: rc
        type(db_type) :: db

        stat = TEST_FAILED

        print *, 'Checking for stale backup database "' // DB_OBSERV_BACKUP // '" ...'

        if (dm_file_exists(DB_OBSERV_BACKUP)) then
            call dm_file_delete(DB_OBSERV_BACKUP)
            print *, 'Deleted stale database'
        end if

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        if (dm_db_open(db, DB_OBSERV) /= E_NONE) return

        print *, 'Creating backup ...'
        rc = dm_db_backup(db         = db, &
                          path       = DB_OBSERV_BACKUP, &
                          wal        =.true., &
                          callback   = backup_callback, &
                          nsteps     = 500, &
                          sleep_time = 5)
        call dm_error_out(rc)

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test12

    logical function test13() result(stat)
        !! Tests VACUUM INTO.
        integer       :: rc
        type(db_type) :: db

        stat = TEST_FAILED

        print *, 'Checking for stale backup database "' // DB_OBSERV_VACUUM // '" ...'

        if (dm_file_exists(DB_OBSERV_VACUUM)) then
            call dm_file_delete(DB_OBSERV_VACUUM)
            print *, 'Deleted stale database'
        end if

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        if (dm_db_open(db, DB_OBSERV) /= E_NONE) return

        print *, 'Creating backup ...'
        rc = dm_db_vacuum(db, into=DB_OBSERV_VACUUM)
        call dm_error_out(rc)

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test13

    logical function test14() result(stat)
        !! Tests creation of beat database
        integer         :: rc
        type(beat_type) :: beat1, beat2
        type(beat_type) :: beats(2)
        type(db_type)   :: db

        stat = TEST_FAILED

        print *, 'Checking for stale database "' // DB_BEAT // '" ...'

        if (dm_file_exists(DB_BEAT)) then
            call dm_file_delete(DB_BEAT)
            print *, 'Deleted stale database'
        end if

        print *, 'Creating database "' // DB_BEAT // '" ...'
        if (dm_db_open(db, DB_BEAT, create=.true., wal=.true.) /= E_NONE) return

        test_block: block
            print *, 'Validating application id ...'
            rc = dm_db_validate(db)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating tables ...'
            rc = dm_db_table_create_beats(db)
            if (dm_is_error(rc)) exit test_block

            beat1%node_id = 'dummy-node'
            beat1%address = '127.0.0.1'
            beat1%client  = dm_version_to_string('dmtestdb', 1, 0, 0, library=.true.)

            print *, 'Adding beat ...'
            rc = dm_db_insert(db, beat1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Selecting beat ...'
            rc = dm_db_select(db, beat2, beat1%node_id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Adding beats ...'
            call dm_test_dummy(beats)
            beats(1)%node_id = 'dummy-node-1'
            beats(2)%node_id = 'dummy-node-2'
            rc = dm_db_insert(db, beats)
        end block test_block

        print *, 'Closing database "' // DB_BEAT // '" ...'
        call dm_db_close(db)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching beats ...'
        if (.not. (beat1 == beat2)) return

        stat = TEST_PASSED
    end function test14

    logical function test15() result(stat)
        !! Tests conflicts.
        integer           :: rc
        type(db_type)     :: db
        type(observ_type) :: observ

        stat = TEST_FAILED

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        if (dm_db_open(db, DB_OBSERV) /= E_NONE) return

        test_block: block
            call dm_test_dummy(observ)

            print *, 'Adding observation ...'
            rc = dm_db_insert_observ(db, observ)
            if (dm_is_error(rc)) exit test_block

            print *, 'Trying to add observation again ...'
            rc = dm_db_insert_observ(db, observ)
        end block test_block

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db)
        if (rc == E_NONE) return
        print *, 'Failed successfully'

        stat = TEST_PASSED
    end function test15

    logical function test16() result(stat)
        !! Tests JSON output of SQLite (logs).
        integer                        :: rc
        integer(kind=i8)               :: nlogs
        type(db_type)                  :: db
        type(string_type), allocatable :: strings(:)

        stat = TEST_FAILED

        print *, 'Opening database "' // DB_LOG // '" ...'
        if (dm_db_open(db, DB_LOG) /= E_NONE) return

        test_block: block
            print *, 'Selecting logs in JSON format ...'
            rc = dm_db_json_select_logs(db, strings, limit=1_i8, nlogs=nlogs)
            if (dm_is_error(rc)) exit test_block

            rc = E_ERROR
            if (.not. allocated(strings)) exit test_block
            if (size(strings) /= 1) exit test_block
            if (.not. allocated(strings(1)%data)) exit test_block

            print '(72("."))'
            print '(a)', strings(1)%data
            print '(72("."))'

            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_LOG // '" ...'
        call dm_db_close(db)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test16

    logical function test17() result(stat)
        !! Tests JSON output of SQLite (beats).
        integer            :: rc
        integer(kind=i8)   :: nbeats
        type(db_type)      :: db
        type(db_stmt_type) :: db_stmt

        character(len=:),  allocatable :: json
        type(string_type), allocatable :: strings(:)

        stat = TEST_FAILED

        print *, 'Opening database "' // DB_BEAT // '" ...'
        if (dm_db_open(db, DB_BEAT) /= E_NONE) return

        test_block: block
            print *, 'Selecting beats in JSON format ...'
            rc = dm_db_json_select_beats(db, strings, limit=1_i8, nbeats=nbeats)
            if (dm_is_error(rc)) exit test_block

            rc = E_ERROR
            if (.not. allocated(strings)) exit test_block
            if (size(strings) /= 1) exit test_block
            if (.not. allocated(strings(1)%data)) exit test_block

            print '(72("."))'
            print '(a)', strings(1)%data
            print '(72("."))'

            print *, 'Selecting beats in JSON format iterative ...'
            rc = dm_db_json_select_beats(db, db_stmt, json)
            if (dm_is_error(rc)) exit test_block

            print '(72("."))'
            print '(a)', json
            print '(72("."))'

            if (json /= strings(1)%data) exit test_block

            print *, 'Selecting more ...'
            rc = dm_db_json_select_beats(db, db_stmt, json)
            if (dm_is_error(rc)) exit test_block

            print '(72("."))'
            print '(a)', json
            print '(72("."))'

            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        call dm_db_finalize(db_stmt)

        print *, 'Closing database "' // DB_BEAT // '" ...'
        call dm_db_close(db)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test17

    logical function test18() result(stat)
        !! Tests JSON output of SQLite (nodes).
        integer                        :: rc
        integer(kind=i8)               :: nnodes
        type(db_type)                  :: db
        type(string_type), allocatable :: strings(:)

        stat = TEST_FAILED

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        if (dm_db_open(db, DB_OBSERV) /= E_NONE) return

        test_block: block
            print *, 'Selecting nodes in JSON format ...'
            rc = dm_db_json_select_nodes(db, strings, limit=1_i8, nnodes=nnodes)
            if (dm_is_error(rc)) exit test_block

            rc = E_ERROR
            if (.not. allocated(strings)) exit test_block
            if (size(strings) /= 1) exit test_block
            if (.not. allocated(strings(1)%data)) exit test_block

            print '(72("."))'
            print '(a)', strings(1)%data
            print '(72("."))'

            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_OBSERV // '" ...'
        call dm_db_close(db)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test18

    logical function test19() result(stat)
        !! Tests PRAGMAs.
        integer(kind=i8), parameter :: CACHE_SIZE     = 1000000_i8
        integer,          parameter :: SCHEMA_VERSION = DB_SCHEMA_VERSION

        integer          :: mode, n, rc
        integer(kind=i8) :: size
        logical          :: enabled
        type(db_type)    :: db

        stat = TEST_FAILED

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        if (dm_db_open(db, DB_OBSERV) /= E_NONE) return

        test_block: block
            print *, 'Testing foreign keys ...'
            rc = dm_db_set_foreign_keys(db, .true.)
            if (dm_is_error(rc)) exit test_block
            rc = dm_db_get_foreign_keys(db, enabled)
            if (dm_is_error(rc)) exit test_block
            if (.not. enabled) exit test_block

            print *, 'Testing application id ...'
            rc = dm_db_set_application_id(db, DB_APPLICATION_ID)
            if (dm_is_error(rc)) exit test_block
            rc = dm_db_get_application_id(db, n)
            if (dm_is_error(rc)) exit test_block
            if (n /= DB_APPLICATION_ID) exit test_block

            print *, 'Testing schema version ...'
            rc = dm_db_set_schema_version(db, SCHEMA_VERSION)
            if (dm_is_error(rc)) exit test_block
            rc = dm_db_get_schema_version(db, n)
            if (dm_is_error(rc)) exit test_block
            if (n /= SCHEMA_VERSION) exit test_block

            print *, 'Testing query-only ...'
            rc = dm_db_set_query_only(db, .true.)
            if (dm_is_error(rc)) exit test_block
            rc = dm_db_get_query_only(db, enabled)
            if (dm_is_error(rc)) exit test_block
            if (.not. enabled) exit test_block

            print *, 'Testing cache size ...'
            rc = dm_db_set_cache_size(db, CACHE_SIZE)
            if (dm_is_error(rc)) exit test_block
            rc = dm_db_get_cache_size(db, size)
            if (dm_is_error(rc)) exit test_block
            if (size /= CACHE_SIZE) exit test_block

            print *, 'Testing locking mode ...'
            rc = dm_db_set_locking_mode(db, DB_LOCKING_MODE_EXCLUSIVE)
            if (dm_is_error(rc)) exit test_block
            rc = dm_db_get_locking_mode(db, mode)
            if (dm_is_error(rc)) exit test_block
            if (mode /= DB_LOCKING_MODE_EXCLUSIVE) exit test_block

            print *, 'Testing synchronous mode ...'
            rc = dm_db_set_synchronous(db, DB_SYNCHRONOUS_EXTRA)
            if (dm_is_error(rc)) exit test_block
            rc = dm_db_get_synchronous(db, mode)
            if (dm_is_error(rc)) exit test_block
            if (mode /= DB_SYNCHRONOUS_EXTRA) exit test_block

            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_LOG // '" ...'
        call dm_db_close(db)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test19

    logical function test20() result(stat)
        !! Creates transfer database.
        integer             :: rc
        type(db_type)       :: db
        type(transfer_type) :: transfer1, transfer2

        stat = TEST_FAILED

        print *, 'Checking for stale database "' // DB_TRANSFER // '" ...'

        if (dm_file_exists(DB_TRANSFER)) then
            print *, 'Deleting stale database ...'
            call dm_file_delete(DB_TRANSFER)
        end if

        print *, 'Creating database "' // DB_TRANSFER // '" ...'
        rc = dm_db_open(db, DB_TRANSFER, create=.true., wal=.false.)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Creating table ...'
            rc = dm_db_table_create_transfers(db)
            if (dm_is_error(rc)) exit test_block

            rc = E_NOT_FOUND
            if (.not. dm_db_table_has_transfers(db)) exit test_block

            print *, 'Creating transfer ...'
            rc = dm_transfer_create(transfer1, node_id='dummy-node', type_id=dm_uuid4(), type=TRANSFER_TYPE_IMAGE, size=1024_i8)
            if (dm_is_error(rc)) exit test_block
            if (.not. dm_transfer_is_valid(transfer1)) exit test_block

            print *, 'Adding transfer ...'
            rc = dm_db_insert_transfer(db, transfer1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Selecting transfer ...'
            rc = dm_db_select_transfer(db, transfer2, transfer1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Printing transfer ...'
            print '(72("."))'
            call dm_transfer_out(transfer2)
            print '(72("."))'

            print *, 'Matching transfers ...'
            rc = E_CORRUPT
            if (.not. (transfer1 == transfer2)) exit test_block

            print *, 'Updating transfers ...'
            rc = dm_db_update_transfer(db, transfer1%id, timestamp=dm_time_now(), state=TRANSFER_STATE_ACTIVE, error=E_DUMMY)
            if (dm_is_error(rc)) exit test_block

            print *, 'Selecting transfer ...'
            rc = dm_db_select_transfer(db, transfer2, transfer1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating transfer ...'
            rc = E_CORRUPT
            if (transfer2%state /= TRANSFER_STATE_ACTIVE) exit test_block
            if (transfer2%error /= E_DUMMY) exit test_block

            print *, 'Updating transfers ...'
            rc = dm_db_update_transfer(db, transfer1%id, timestamp=dm_time_now(), state=TRANSFER_STATE_DONE)
            if (dm_is_error(rc)) exit test_block

            print *, 'Selecting transfer ...'
            rc = dm_db_select_transfer(db, transfer2, transfer1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating transfer ...'
            rc = E_CORRUPT
            if (transfer2%state /= TRANSFER_STATE_DONE) exit test_block

            rc = E_NONE
        end block test_block

        if (dm_is_error(rc)) then
            call dm_error_out(rc)
            print *, dm_db_error_message(db)
        end if

        print *, 'Closing database "' // DB_TRANSFER // '" ...'
        call dm_db_close(db)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test20

    subroutine backup_callback(remaining, page_count)
        integer, intent(in) :: remaining
        integer, intent(in) :: page_count

        print '(" *** Progress: ", f5.1, " %")', 100.0 * (page_count - remaining) / page_count
    end subroutine backup_callback

    subroutine log_callback(client_data, err_code, err_msg_ptr) bind(c)
        !! Callback for SQLite error logs.
        use, intrinsic :: iso_c_binding
        use :: sqlite3_util, only: c_f_str_ptr
        type(c_ptr),         intent(in), value :: client_data
        integer(kind=c_int), intent(in), value :: err_code
        type(c_ptr),         intent(in), value :: err_msg_ptr

        character(len=:), allocatable :: err_msg

        if (.not. c_associated(err_msg_ptr)) return
        call c_f_str_ptr(err_msg_ptr, err_msg)
        if (.not. allocated(err_msg)) return
        print '("Error ", i0, ": ", a)', err_code, err_msg
        deallocate (err_msg)
    end subroutine log_callback
end program dmtestdb
