! dmtestdb.f90
!
! Author:  Philipp Engel
! Licence: ISC
module handler
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    public :: backup_handler
    public :: log_handler
contains
    subroutine backup_handler(remaining, page_count)
        integer, intent(in) :: remaining
        integer, intent(in) :: page_count

        print '("Progress: ", f5.1, " %")', 100.0 * (page_count - remaining) / page_count
    end subroutine backup_handler

    subroutine log_handler(client_data, err_code, err_msg_ptr) bind(c)
        !! Callback for SQLite error logs.
        use :: sqlite3_util, only: c_f_str_ptr
        type(c_ptr),         intent(in), value :: client_data
        integer(kind=c_int), intent(in), value :: err_code
        type(c_ptr),         intent(in), value :: err_msg_ptr
        character(len=:), allocatable          :: err_msg

        if (.not. c_associated(err_msg_ptr)) return
        call c_f_str_ptr(err_msg_ptr, err_msg)
        if (.not. allocated(err_msg)) return
        print '("Error ", i0, ": ", a)', err_code, err_msg
        deallocate (err_msg)
    end subroutine log_handler
end module handler

program dmtestdb
    !! Tests database access using `dm_db` module.
    use, intrinsic :: iso_c_binding
    use :: dmpack
    use :: handler
    implicit none (type, external)
    character(len=*), parameter :: DB_BEAT          = 'testbeat.sqlite'
    character(len=*), parameter :: DB_LOG           = 'testlog.sqlite'
    character(len=*), parameter :: DB_OBSERV        = 'testobserv.sqlite'
    character(len=*), parameter :: DB_OBSERV_BACKUP = 'testobserv_backup.sqlite'
    character(len=*), parameter :: DB_OBSERV_VACUUM = 'testobserv_vacuum.sqlite'

    integer, parameter :: NLOGS    = 100
    integer, parameter :: NOBSERVS = 100
    integer, parameter :: NTESTS   = 15

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests( 1) = test_type('dmtestdb%dm_test01', dm_test01)
    tests( 2) = test_type('dmtestdb%dm_test02', dm_test02)
    tests( 3) = test_type('dmtestdb%dm_test03', dm_test03)
    tests( 4) = test_type('dmtestdb%dm_test04', dm_test04)
    tests( 5) = test_type('dmtestdb%dm_test05', dm_test05)
    tests( 6) = test_type('dmtestdb%dm_test06', dm_test06)
    tests( 7) = test_type('dmtestdb%dm_test07', dm_test07)
    tests( 8) = test_type('dmtestdb%dm_test08', dm_test08)
    tests( 9) = test_type('dmtestdb%dm_test09', dm_test09)
    tests(10) = test_type('dmtestdb%dm_test10', dm_test10)
    tests(11) = test_type('dmtestdb%dm_test11', dm_test11)
    tests(12) = test_type('dmtestdb%dm_test12', dm_test12)
    tests(13) = test_type('dmtestdb%dm_test13', dm_test13)
    tests(14) = test_type('dmtestdb%dm_test14', dm_test14)
    tests(15) = test_type('dmtestdb%dm_test15', dm_test15)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
        !! Creates observation database.
        character(len=SQL_TABLE_NAME_LEN), allocatable :: tables(:)

        integer       :: i, rc
        type(db_type) :: db

        stat = TEST_FAILED

        print *, 'Checking for stale database "' // DB_OBSERV // '" ...'

        if (dm_file_exists(DB_OBSERV)) then
            print *, 'Deleting old database ...'
            call dm_file_delete(DB_OBSERV)
        end if

        print *, 'Creating database "' // DB_OBSERV // '" ...'
        rc = dm_db_open(db, DB_OBSERV, create=.true., wal=.true.)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Validating application id ...'
            rc = dm_db_valid(db)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating tables ...'
            rc = dm_db_create_observs(db, sync=.true.)
            if (dm_is_error(rc)) exit test_block

            rc = dm_db_select_tables(db, tables)
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
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function dm_test01

    logical function dm_test02() result(stat)
        !! Tests writing/reading/deleting of nodes.
        integer                      :: i, rc
        type(db_type)                :: db
        type(node_type), allocatable :: in(:), out(:)

        stat = TEST_FAILED

        allocate (in(3))

        ! Sensor nodes.
        in = [ node_type('z', 'Test Node Z', 'A test node.'), &
               node_type('a', 'Test Node A', 'A test node.'), &
               node_type('b', 'Test Node B', 'A test node.') ]

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        rc = dm_db_open(db, DB_OBSERV)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        test_block: block
            do i = 1, size(in)
                rc = dm_db_insert_node(db, in(i))
                if (dm_is_error(rc)) exit test_block
                print *, 'Added node "' // trim(in(i)%name) // '"'
            end do

            rc = dm_db_select_nodes(db, out)
            if (dm_is_error(rc)) exit test_block
            print *, 'Read nodes'

            if (.not. allocated(out)) return
            if (size(in) /= size(out)) return
            print *, 'Node data match'

            rc = E_INVALID
            do i = 1, size(out)
                if (.not. (in(i) == out(i))) exit test_block
                print *, 'Deleting "' // trim(in(i)%id) // '" ...'
                if (dm_db_delete_node(db, in(i)%id) /= E_NONE) exit test_block
            end do
            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_OBSERV // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function dm_test02

    logical function dm_test03() result(stat)
        !! Tests writing/reading of nodes and sensors.
        integer           :: rc
        type(db_type)     :: db
        type(node_type)   :: node1, node2
        type(sensor_type) :: sensor1, sensor2
        type(target_type) :: target1, target2

        stat = TEST_FAILED

        call dm_dummy_node(node1)
        call dm_dummy_sensor(sensor1)
        call dm_dummy_target(target1)

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
            if (.not. (node1 == node2)) exit test_block

            print *, 'Matching sensors ...'
            if (.not. (sensor1 == sensor2)) exit test_block

            print *, 'Matching targets ...'
            if (.not. (target1 == target2)) exit test_block
            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_OBSERV // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function dm_test03

    logical function dm_test04() result(stat)
        !! Observation sync.
        integer           :: rc
        type(db_type)     :: db
        type(observ_type) :: observ
        type(node_type)   :: node
        type(sensor_type) :: sensor
        type(sync_type)   :: sync1, sync2
        type(target_type) :: target

        stat = TEST_FAILED

        call dm_dummy_node(node)
        call dm_dummy_sensor(sensor)
        call dm_dummy_target(target)
        call dm_dummy_observ(observ)

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
                              nattempts = 1)

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
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        print *, 'Matching sync data ...'
        if (.not. (sync1 == sync2)) return

        stat = TEST_PASSED
    end function dm_test04

    logical function dm_test05() result(stat)
        !! Tests writing/reading of observation.
        integer           :: rc
        type(db_type)     :: db
        type(observ_type) :: observ1, observ2

        stat = TEST_FAILED

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        rc = dm_db_open(db, DB_OBSERV)
        if (dm_is_error(rc)) return

        call dm_dummy_observ(observ1)
        observ1%priority = 100

        print *, 'Created observation "' // observ1%id // '"'

        test_block: block
            print *, 'Adding observation ...'
            rc = dm_db_insert_observ(db, observ1)
            if (dm_is_error(rc)) exit test_block

            print *, 'Reading observation ...'
            rc = dm_db_select_observ(db, observ2, observ1%id)
            if (dm_is_error(rc)) exit test_block

            print *, 'Deleting observation ...'
            rc = dm_db_delete_observ(db, observ1%id)
            if (dm_is_error(rc)) exit test_block
        end block test_block

        call dm_error_out(rc)
        if (rc >= E_DB) print *, dm_db_error_message(db)

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        print *, 'Matching observations ...'
        if (.not. (observ1 == observ2)) return

        stat = TEST_PASSED
    end function dm_test05

    logical function dm_test06() result(stat)
        !! Tests writing of observation.
        character(len=TIME_LEN) :: timestamp
        integer                 :: i, rc
        real(kind=r8)           :: dt, r(6)
        type(db_type)           :: db
        type(timer_type)        :: t

        type(node_type)   :: node1
        type(sensor_type) :: sensor1, sensor2
        type(target_type) :: target1, target2

        type(observ_type), allocatable :: observs1(:), observs2(:)

        stat = TEST_FAILED
        allocate (observs1(NOBSERVS), observs2(NOBSERVS))

        call dm_dummy_node(node1)
        call dm_dummy_sensor(sensor1)
        call dm_dummy_sensor(sensor2, id='dummy-sensor2', name='Sensor 2')
        call dm_dummy_target(target1)
        call dm_dummy_target(target2, id='dummy-target2', name='Target 2')

        do i = 1, NOBSERVS
            call random_number(r)
            timestamp = dm_time_create(year    = 2022, &
                                       month   = 1, &
                                       day     = 1 + int(r(1) * 27), &
                                       hour    = int(r(2) * 23), &
                                       minute  = int(r(3) * 60), &
                                       second  = int(r(4) * 60), &
                                       msecond = int(r(5) * 1000))
            call dm_dummy_observ(observs1(i), timestamp=timestamp, value=sin(r(6) * PI))
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
            do i = 1, NOBSERVS
                rc = dm_db_insert_observ(db, observs1(i))
                if (dm_is_error(rc))  exit test_block
            end do
            dt = dm_timer_stop(t)

            print *, NOBSERVS, ' observations written in ', dt, ' sec'
            print *, 'Selecting and matching observations ...'

            do i = 1, NOBSERVS
                rc = dm_db_select_observ(db, observs2(i), observs1(i)%id)
                if (dm_is_error(rc)) exit test_block
                rc = E_INVALID
                if (.not. (observs1(i) == observs2(i))) exit test_block
            end do
            rc = E_NONE
        end block test_block

        if (dm_is_error(rc)) call dm_error_out(dm_db_error(db))
        if (rc >= E_DB) print *, dm_db_error_message(db)

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function dm_test06

    logical function dm_test07() result(stat)
        !! Tests writing of observation.
        character(len=TIME_LEN) :: timestamp
        integer                 :: i, rc
        integer(kind=i8)        :: nobs
        real(kind=r8)           :: dt, r(5)
        type(db_type)           :: db
        type(timer_type)        :: t

        type(node_type)   :: node1
        type(sensor_type) :: sensor1
        type(target_type) :: target1

        type(observ_type),      allocatable :: observs1(:), observs2(:)
        type(observ_view_type), allocatable :: views(:)

        stat = TEST_FAILED
        allocate (observs1(NOBSERVS))

        call dm_dummy_node(node1)
        call dm_dummy_sensor(sensor1)
        call dm_dummy_target(target1)

        do i = 1, NOBSERVS
            call random_number(r)

            timestamp = dm_time_create(year    = 2022, &
                                       month   = 1, &
                                       day     = 1 + modulo(i - 1, 27), &
                                       hour    = int(r(1) * 23), &
                                       minute  = int(r(2) * 60), &
                                       second  = int(r(3) * 60), &
                                       msecond = int(r(4) * 1000))
            call dm_dummy_observ(observs1(i), timestamp=timestamp, value=sin(r(5) * PI))
        end do

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        rc = dm_db_open(db, DB_OBSERV)
        if (dm_is_error(rc)) return

        test_block: block
            print *, 'Bulk adding observations ...'
            call dm_timer_start(t)
            rc = dm_db_insert_observs(db, observs1)
            dt = dm_timer_stop(t)
            if (dm_is_error(rc)) exit test_block
            print *, NOBSERVS, ' observations written in ', dt, ' sec'

            print *, 'Selecting observations ...'
            call dm_timer_start(t)
            rc = dm_db_select_observs(db, observs2, node1%id, sensor1%id, target1%id, &
                                      '1970', '2100', limit=int(NOBSERVS, kind=i8))
            dt = dm_timer_stop(t)
            if (dm_is_error(rc)) exit test_block
            print *, size(observs2), ' observations read in ', dt, ' sec'
            if (size(observs2) /= size(observs1)) return

            print *, 'Selecting observations by id range ...'
            print *, 'After:  ', observs1(1)%id
            print *, 'Before: ', observs1(NOBSERVS)%id

            deallocate (observs2)
            call dm_timer_start(t)
            rc = dm_db_select_observs(db, observs2, after=observs1(1)%id, before=observs1(NOBSERVS)%id, &
                                      nobservs=nobs)
            dt = dm_timer_stop(t)
            print *, size(observs2), ' observations read in ', dt, ' sec'
            if (dm_is_error(rc)) exit test_block
            if (size(observs2) == 0) return

            print *, 'Selecting observation views ...'
            call dm_timer_start(t)
            rc = dm_db_select_observ_views(db, views, node1%id, sensor1%id, target1%id, &
                                           observs1(1)%requests(1)%responses(1)%name, &
                                           '1970', '2100')
            dt = dm_timer_stop(t)
            if (dm_is_error(rc)) exit test_block
            print *, size(views), ' observation views read in ', dt, ' sec'

            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_OBSERV // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function dm_test07

    logical function dm_test08() result(stat)
        !! Tests creation of log database
        integer       :: rc
        type(db_type) :: db

        stat = TEST_FAILED

        print *, 'Checking for stale database "' // DB_LOG // '" ...'

        if (dm_file_exists(DB_LOG)) then
            call dm_file_delete(DB_LOG)
            print *, 'Deleted old database'
        end if

        print *, 'Creating database "' // DB_LOG // '" ...'
        if (dm_db_open(db, DB_LOG, create=.true., wal=.true.) /= E_NONE) return

        test_block: block
            print *, 'Validating application id ...'
            rc = dm_db_valid(db)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating tables ...'
            rc = dm_db_create_logs(db, sync=.true.)
            call dm_error_out(rc)
        end block test_block

        call dm_error_out(rc)
        if (rc >= E_DB) print *, dm_db_error_message(db)

        print *, 'Closing database "' // DB_LOG // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function dm_test08

    logical function dm_test09() result(stat)
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
            call dm_dummy_log(log1)

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

            call dm_dummy_log(logs)

            call dm_timer_start(t)
            do i = 1, NLOGS
                rc = dm_db_insert_log(db, logs(i))
                if (dm_is_error(rc)) exit test_block
            end do
            dt = dm_timer_stop(t)

            print *, NLOGS, ' logs added in ', dt, ' sec'
        end block test_block

        call dm_error_out(rc)
        if (rc >= E_DB) print *, dm_db_error_message(db)

        print *, 'Closing database "' // DB_LOG // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function dm_test09

    logical function dm_test10() result(stat)
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
            rc = dm_db_select_logs(db, logs, node_id='', sensor_id='', target_id='', &
                                   source='', from='', to='', limit=int(NLOGS, i8), nlogs=n)
            dt = dm_timer_stop(t)
            print *, 'Selected ', n, ' logs in ', dt, ' sec'
        end block test_block

        call dm_error_out(rc)
        print *, 'Closing database "' // DB_LOG // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return
        if (n < NLOGS) return

        stat = TEST_PASSED
    end function dm_test10

    logical function dm_test11() result(stat)
        !! Tests database error handler.
        integer       :: rc
        type(db_type) :: db

        stat = TEST_FAILED

        ! Add error log handler.
        rc = dm_db_shutdown()
        rc = dm_db_set_log_handler(log_handler)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Opening database "' // DB_LOG // '" ...'
        if (dm_db_open(db, DB_LOG) /= E_NONE) return

        print *, 'Testing error logging ...'
        call dm_db_log(1, 'TEST LOG')

        print *, 'Closing database "' // DB_LOG // '" ...'
        if (dm_db_close(db) /= E_NONE) return

        stat = TEST_PASSED
    end function dm_test11

    logical function dm_test12() result(stat)
        !! Tests database backup.
        integer       :: rc
        type(db_type) :: db

        stat = TEST_FAILED

        print *, 'Checking for stale backup database "' // DB_OBSERV_BACKUP // '" ...'

        if (dm_file_exists(DB_OBSERV_BACKUP)) then
            call dm_file_delete(DB_OBSERV_BACKUP)
            print *, 'Deleted old database'
        end if

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        if (dm_db_open(db, DB_OBSERV) /= E_NONE) return

        print *, 'Creating backup ...'
        rc = dm_db_backup(db         = db, &
                          path       = DB_OBSERV_BACKUP, &
                          wal        =.true., &
                          callback   = backup_handler, &
                          nsteps     = 500, &
                          sleep_time = 5)
        call dm_error_out(rc)

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function dm_test12

    logical function dm_test13() result(stat)
        !! Tests VACUUM INTO.
        integer       :: rc
        type(db_type) :: db

        stat = TEST_FAILED

        print *, 'Checking for stale backup database "' // DB_OBSERV_VACUUM // '" ...'

        if (dm_file_exists(DB_OBSERV_VACUUM)) then
            call dm_file_delete(DB_OBSERV_VACUUM)
            print *, 'Deleted old database'
        end if

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        if (dm_db_open(db, DB_OBSERV) /= E_NONE) return

        print *, 'Creating backup ...'
        rc = dm_db_vacuum_into(db, DB_OBSERV_VACUUM)
        call dm_error_out(rc)

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function dm_test13

    logical function dm_test14() result(stat)
        !! Tests creation of beat database
        integer         :: rc
        type(beat_type) :: beat1, beat2
        type(db_type)   :: db

        stat = TEST_FAILED

        print *, 'Checking for stale database "' // DB_BEAT // '" ...'

        if (dm_file_exists(DB_BEAT)) then
            call dm_file_delete(DB_BEAT)
            print *, 'Deleted old database'
        end if

        print *, 'Creating database "' // DB_BEAT // '" ...'
        if (dm_db_open(db, DB_BEAT, create=.true., wal=.true.) /= E_NONE) return

        test_block: block
            print *, 'Validating application id ...'
            rc = dm_db_valid(db)
            if (dm_is_error(rc)) exit test_block

            print *, 'Creating tables ...'
            rc = dm_db_create_beats(db)
            if (dm_is_error(rc)) exit test_block

            beat1%node_id = 'dummy-node'
            beat1%address = '127.0.0.1'

            print *, 'Adding beat ...'
            rc = dm_db_insert_beat(db, beat1)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Selecting beat ...'
            rc = dm_db_select_beat(db, beat2, beat1%node_id)
            call dm_error_out(rc)
            if (dm_is_error(rc)) exit test_block
        end block test_block

        print *, 'Closing database "' // DB_BEAT // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (dm_is_error(rc)) return

        print *, 'Matching beats ...'
        if (.not. (beat1 == beat2)) return

        stat = TEST_PASSED
    end function dm_test14

    logical function dm_test15() result(stat)
        !! Tests conflicts.
        integer           :: rc
        type(db_type)     :: db
        type(observ_type) :: observ

        stat = TEST_FAILED

        print *, 'Opening database "' // DB_OBSERV // '" ...'
        if (dm_db_open(db, DB_OBSERV) /= E_NONE) return

        test_block: block
            call dm_dummy_observ(observ)

            print *, 'Adding observation ...'
            rc = dm_db_insert_observ(db, observ)
            if (dm_is_error(rc)) exit test_block

            print *, 'Trying to add observation again ...'
            rc = dm_db_insert_observ(db, observ)
        end block test_block

        print *, 'Closing database "' // DB_OBSERV // '" ...'
        if (dm_db_close(db) /= E_NONE) return
        if (rc == E_NONE) return
        print *, 'Failed successfully'

        stat = TEST_PASSED
    end function dm_test15
end program dmtestdb
