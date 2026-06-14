! dmtestmsgpack.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestmsgpack
    !! Test program for network module.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestmsgpack'
    integer,          parameter :: NTESTS    = 7

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04), &
        test_type('test05', test05), &
        test_type('test06', test06), &
        test_type('test07', test07)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        !! Packs and unpack scalar values.
        integer(i8), parameter :: BUFFER_SIZE = 512

        integer     :: rc
        integer(i4) :: i32
        integer(i8) :: i64
        logical     :: l32
        real(r4)    :: r32
        real(r8)    :: r64

        type(buffer_type)         :: buffer
        type(msgpack_packer_type) :: packer
        type(msgpack_unpack_type) :: unpack

        stat = TEST_FAILED

        i32 = 123456_i4
        i64 = 123456789123456_i8
        l32 = .true.
        r32 = 1.1234_r4
        r64 = 123.123456789_r8

        print '(" Testing packing ...")'
        print '(" int32..: ", i0)',    i32
        print '(" int64..: ", i0)',    i64
        print '(" bool...: ", l1)',    l32
        print '(" float32: ", f0.8)',  r32
        print '(" float64: ", f0.16)', r64

        call dm_buffer_init(buffer, BUFFER_SIZE)
        call dm_msgpack_init(packer, buffer)

        pack_block: block
            character(:), pointer :: bytes

            call dm_msgpack_pack(packer, i32, rc); if (dm_is_error(rc)) exit pack_block
            call dm_msgpack_pack(packer, i64, rc); if (dm_is_error(rc)) exit pack_block
            call dm_msgpack_pack(packer, l32, rc); if (dm_is_error(rc)) exit pack_block
            call dm_msgpack_pack(packer, r32, rc); if (dm_is_error(rc)) exit pack_block
            call dm_msgpack_pack(packer, r64, rc); if (dm_is_error(rc)) exit pack_block

            print '(/, " Buffer size: ", i0)', dm_buffer_size(buffer)
            print '(" Packed size: ", i0)',    dm_msgpack_packer_size(packer)

            bytes => dm_msgpack_packer_result(packer)
            call bytes_out(bytes)

            call dm_msgpack_destroy(packer)
        end block pack_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print '(/, " Testing unpacking ...")'

        associate (object => unpack%object)
            call dm_msgpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack(object, i32, rc);  if (dm_is_error(rc)) return

            call dm_msgpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack(object, i64, rc);  if (dm_is_error(rc)) return

            call dm_msgpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack(object, l32, rc);  if (dm_is_error(rc)) return

            call dm_msgpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack(object, r32, rc);  if (dm_is_error(rc)) return

            call dm_msgpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack(object, r64, rc);  if (dm_is_error(rc)) return

            call dm_msgpack_next(unpack, buffer, rc); if (dm_is_ok(rc))    return
        end associate

        print '(" int32..: ", i0)',    i32
        print '(" int64..: ", i0)',    i64
        print '(" bool...: ", l1)',    l32
        print '(" float32: ", f0.8)',  r32
        print '(" float64: ", f0.16)', r64

        call dm_msgpack_destroy(unpack)
        call dm_buffer_destroy(buffer)

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! Packs and unpacks fixarray.
        integer,     parameter :: N           = 8
        integer,     parameter :: ASSERT(N)   = [ 8, 16, 32, 64, 128, 256, 512, 1024 ]
        integer(i8), parameter :: BUFFER_SIZE = 64

        character(:), pointer     :: bytes
        type(buffer_type)         :: buffer
        type(msgpack_packer_type) :: packer
        type(msgpack_unpack_type) :: unpack

        integer :: i, sz, rc
        integer :: v(N)

        stat = TEST_FAILED

        call dm_buffer_init(buffer, BUFFER_SIZE)
        call dm_msgpack_init(packer, buffer)

        ! Pack to MessagePack buffer.
        print '(" Testing packing of fixarray ...")'
        v = ASSERT

        do i = 1, N
            print '(" v(", i0, ") = ", i0)', i, v(i)
        end do

        ! Write array header.
        call dm_msgpack_pack_fixarray(packer, N, rc); if (dm_is_error(rc)) return

        ! Write array elements.
        do i = 1, N
            call dm_msgpack_pack(packer, v(i), rc); if (dm_is_error(rc)) return
        end do

        print '(/, " Buffer size: ", i0)', dm_buffer_size(buffer)
        print '(" Packed size: ", i0)',    dm_msgpack_packer_size(packer)

        bytes => dm_msgpack_packer_result(packer)
        call dm_msgpack_destroy(packer)

        call bytes_out(bytes)

        ! Unpack from MessagePack buffer.
        print '(/, " Testing unpacking of fixarray ...")'

        associate (object => unpack%object)
            ! Read MessagePack array header + size.
            call dm_msgpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack_fixarray(object, sz, rc); if (dm_is_error(rc)) return

            ! Validate array size.
            if (sz /= N) return

            ! Read all integers from MessagePack array.
            do i = 1, sz
                ! Read header.
                call dm_msgpack_next(unpack, buffer, rc)
                if (dm_is_error(rc)) return

                ! Validate element type.
                if (object%type /= MSGPACK_INT32) return

                ! Read value.
                call dm_msgpack_unpack(object, v(i), rc)
                if (dm_is_error(rc)) return
            end do
        end associate

        do i = 1, N
            print '(" v(", i0, ") = ", i0)', i, v(i)
            if (v(i) /= ASSERT(i)) return
        end do

        call dm_msgpack_destroy(unpack)
        call dm_buffer_destroy(buffer)

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        !! Packs and unpacks strings.
        integer(i8), parameter :: BUFFER_SIZE = 1024 * 1024
        integer,     parameter :: N           = 65536 * 2

        character(:), pointer     :: bytes
        type(buffer_type)         :: buffer
        type(msgpack_packer_type) :: packer
        type(msgpack_unpack_type) :: unpack
        type(timer_type)          :: timer

        integer                   :: i, rc, sz
        character(N), allocatable :: str1(:), str2(:)

        stat = TEST_FAILED

        allocate (str1(5), str2(5))

        str1 = [ character(len(str1)) :: &
            repeat('A',   N), &
            repeat('B',  35), &
            repeat('C', 255), &
            repeat('D',  15), &
            repeat('E',   8)  &
        ]

        str2 = ' '

        call dm_buffer_init(buffer, BUFFER_SIZE)

        pack_block: block
            call dm_msgpack_init(packer, buffer)
            call dm_timer_start(timer)

            print '(" Packing array ...")'
            call dm_msgpack_pack_array(packer, size(str1), rc)
            if (dm_is_error(rc)) exit pack_block

            do i = 1, size(str1)
                call dm_msgpack_pack(packer, trim(str1(i)), rc)
                if (dm_is_error(rc)) exit pack_block
            end do

            call dm_timer_stop(timer)
            print '(" Buffer size: ", i0)',    dm_buffer_size(buffer)
            print '(" Packed size: ", i0)',    dm_msgpack_packer_size(packer)
            print '(" Time: ", f8.6, " sec")', dm_timer_result(timer)
            bytes => dm_msgpack_packer_result(packer)
        end block pack_block

        call dm_msgpack_destroy(packer)
        ! call bytes_out(bytes)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        unpack_block: block
            call dm_timer_start(timer)

            print '(" Unpacking array ...")'
            call dm_msgpack_next(unpack, buffer, rc)
            if (dm_is_error(rc)) exit unpack_block

            print '(" Reading array ...")'
            call dm_msgpack_unpack_array(unpack%object, sz, rc)
            if (dm_is_error(rc)) exit unpack_block

            print '(" Array size: ", i0)', sz
            if (sz /= size(str1)) exit unpack_block

            do i = 1, sz
                print '(" [", i0, "/", i0, "] Unpacking string ...")', i, sz
                call dm_msgpack_next(unpack, buffer, rc)
                if (dm_is_error(rc)) exit unpack_block

                print '(" [", i0, "/", i0, "] Reading string ...")', i, sz
                call dm_msgpack_unpack(unpack%object, str2(i), rc)
                if (dm_is_error(rc)) exit unpack_block

                print '(" [", i0, "/", i0, "] Validating string ...")', i, sz
                if (str1(i) /= str2(i)) then
                    rc = E_INVALID
                    exit unpack_block
                end if
            end do

            call dm_timer_stop(timer)
            print '(" Time: ", f8.6, " sec")', dm_timer_result(timer)
        end block unpack_block

        call dm_error_out(rc)
        call dm_msgpack_destroy(unpack)
        call dm_buffer_destroy(buffer)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        !! Packs and unpacks derived types.
        integer(i8), parameter :: BUFFER_SIZE = 1024

        character(:), pointer     :: bytes
        type(buffer_type)         :: buffer
        type(msgpack_packer_type) :: packer
        type(msgpack_unpack_type) :: unpack

        integer :: rc

        stat = TEST_FAILED

        beat_block: block
            type(beat_type) :: beat1, beat2

            print '(" Testing packing of beat type ...")'
            call dm_test_dummy(beat1)
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_msgpack_init(packer, buffer)

            call dm_msgpack_pack_type(packer, beat1, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            print '(" Buffer size: ", i0)', dm_buffer_size(buffer)
            print '(" Packed size: ", i0)', dm_msgpack_packer_size(packer)
            bytes => dm_msgpack_packer_result(packer)
            call dm_msgpack_destroy(packer)
            call bytes_out(bytes)

            print '(" Testing unpacking of beat type ...")'
            call dm_msgpack_unpack_type(unpack, buffer, beat2, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return

            call dm_msgpack_destroy(unpack)
            call dm_buffer_destroy(buffer)

            print '(" Validating beat ...")'
            if (.not. (beat1 == beat2)) return
        end block beat_block

        print '(72("."))'

        dp_block: block
            type(dp_type) :: dp1, dp2

            print '(" Testing packing of data point type ...")'
            dp1%x = dm_time_now()
            dp1%y = dm_random_get()

            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_msgpack_init(packer, buffer)

            call dm_msgpack_pack_type(packer, dp1, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            print '(" Buffer size: ", i0)', dm_buffer_size(buffer)
            print '(" Packed size: ", i0)', dm_msgpack_packer_size(packer)
            bytes => dm_msgpack_packer_result(packer)
            call dm_msgpack_destroy(packer)
            call bytes_out(bytes)

            print '(" Testing unpacking of data point type ...")'
            call dm_msgpack_unpack_type(unpack, buffer, dp2, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return

            call dm_msgpack_destroy(unpack)
            call dm_buffer_destroy(buffer)

            print '(" Validating data point ...")'
            if (.not. (dp1 == dp2)) return
        end block dp_block

        print '(72("."))'

        header_block: block
            type(ipc_header_type) :: header1, header2

            print '(" Testing packing of header type ...")'
            call dm_test_dummy(header1)
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_msgpack_init(packer, buffer)

            call dm_msgpack_pack_type(packer, header1, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            print '(" Buffer size: ", i0)', dm_buffer_size(buffer)
            print '(" Packed size: ", i0)', dm_msgpack_packer_size(packer)
            bytes => dm_msgpack_packer_result(packer)
            call dm_msgpack_destroy(packer)
            call bytes_out(bytes)

            print '(" Testing unpacking of header type ...")'
            call dm_msgpack_unpack_type(unpack, buffer, header2, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return

            call dm_msgpack_destroy(unpack)
            call dm_buffer_destroy(buffer)

            print '(" Validating header ...")'
            if (.not. (header1 == header2)) return
        end block header_block

        print '(72("."))'

        log_block: block
            type(log_type) :: log1, log2

            print '(" Testing packing of log type ...")'
            call dm_test_dummy(log1)
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_msgpack_init(packer, buffer)

            call dm_msgpack_pack_type(packer, log1, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            print '(" Buffer size: ", i0)', dm_buffer_size(buffer)
            print '(" Packed size: ", i0)', dm_msgpack_packer_size(packer)
            bytes => dm_msgpack_packer_result(packer)
            call dm_msgpack_destroy(packer)
            call bytes_out(bytes)

            print '(" Testing unpacking of log type ...")'
            call dm_msgpack_unpack_type(unpack, buffer, log2, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return

            call dm_msgpack_destroy(unpack)
            call dm_buffer_destroy(buffer)

            print '(" Validating log ...")'
            if (.not. (log1 == log2)) return
        end block log_block

        print '(72("."))'

        node_block: block
            type(node_type) :: node1, node2

            print '(" Testing packing of node type ...")'
            call dm_test_dummy(node1)
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_msgpack_init(packer, buffer)

            call dm_msgpack_pack_type(packer, node1, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            print '(" Buffer size: ", i0)', dm_buffer_size(buffer)
            print '(" Packed size: ", i0)', dm_msgpack_packer_size(packer)
            bytes => dm_msgpack_packer_result(packer)
            call dm_msgpack_destroy(packer)
            call bytes_out(bytes)

            print '(" Testing unpacking of node type ...")'
            call dm_msgpack_unpack_type(unpack, buffer, node2, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return

            call dm_msgpack_destroy(unpack)
            call dm_buffer_destroy(buffer)

            print '(" Validating node ...")'
            if (.not. (node1 == node2)) return
        end block node_block

        print '(72("."))'

        observ_block: block
            type(observ_type) :: observ1, observ2

            print '(" Testing packing of observation type ...")'
            call dm_test_dummy(observ1)
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_msgpack_init(packer, buffer)

            call dm_msgpack_pack_type(packer, observ1, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            print '(" Buffer size: ", i0)', dm_buffer_size(buffer)
            print '(" Packed size: ", i0)', dm_msgpack_packer_size(packer)
            bytes => dm_msgpack_packer_result(packer)
            call dm_msgpack_destroy(packer)
            call bytes_out(bytes)

            print '(" Testing unpacking of observation type ...")'
            call dm_msgpack_unpack_type(unpack, buffer, observ2, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return

            call dm_msgpack_destroy(unpack)
            call dm_buffer_destroy(buffer)

            print '(" Validating observation ...")'
            if (.not. (observ1 == observ2)) return
        end block observ_block

        print '(72("."))'

        sensor_block: block
            type(sensor_type) :: sensor1, sensor2

            print '(" Testing packing of sensor type ...")'
            call dm_test_dummy(sensor1)
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_msgpack_init(packer, buffer)

            call dm_msgpack_pack_type(packer, sensor1, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            print '(" Buffer size: ", i0)', dm_buffer_size(buffer)
            print '(" Packed size: ", i0)', dm_msgpack_packer_size(packer)
            bytes => dm_msgpack_packer_result(packer)
            call dm_msgpack_destroy(packer)
            call bytes_out(bytes)

            print '(" Testing unpacking of sensor type ...")'
            call dm_msgpack_unpack_type(unpack, buffer, sensor2, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return

            call dm_msgpack_destroy(unpack)
            call dm_buffer_destroy(buffer)

            print '(" Validating sensor ...")'
            if (.not. (sensor1 == sensor2)) return
        end block sensor_block

        print '(72("."))'

        target_block: block
            type(target_type) :: target1, target2

            print '(" Testing packing of target type ...")'
            call dm_test_dummy(target1)
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_msgpack_init(packer, buffer)

            call dm_msgpack_pack_type(packer, target1, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return
            print '(" Buffer size: ", i0)', dm_buffer_size(buffer)
            print '(" Packed size: ", i0)', dm_msgpack_packer_size(packer)
            bytes => dm_msgpack_packer_result(packer)
            call dm_msgpack_destroy(packer)
            call bytes_out(bytes)

            print '(" Testing unpacking of target type ...")'
            call dm_msgpack_unpack_type(unpack, buffer, target2, rc)
            call dm_error_out(rc)
            if (dm_is_error(rc)) return

            call dm_msgpack_destroy(unpack)
            call dm_buffer_destroy(buffer)

            print '(" Validating target ...")'
            if (.not. (target1 == target2)) return
        end block target_block

        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        !! Benchmarks packing and unpacking of derived types.
        integer(i8), parameter :: BUFFER_SIZE = 1024
        integer,     parameter :: N           = 10000

        type(buffer_type)         :: buffer
        type(msgpack_packer_type) :: packer
        type(msgpack_unpack_type) :: unpack

        integer                      :: i, rc
        type(node_type), allocatable :: nodes1(:), nodes2(:)
        type(timer_type)             :: timer

        stat = TEST_FAILED

        allocate (nodes1(N))
        allocate (nodes2(N))

        print '(" Creating nodes ...")'
        call dm_test_dummy(nodes1)

        do i = 1, N
            nodes1(i)%id        = dm_uuid_new()
            nodes1(i)%x         = dm_random_get_uniform( -2000.0_r8,  2000.0_r8)
            nodes1(i)%y         = dm_random_get_uniform( -1000.0_r8,  1000.0_r8)
            nodes1(i)%z         = dm_random_get_uniform(     0.0_r8,   100.0_r8)
            nodes1(i)%longitude = dm_random_get_uniform(-20000.0_r8, 20000.0_r8)
            nodes1(i)%latitude  = dm_random_get_uniform(-10000.0_r8, 10000.0_r8)
            nodes1(i)%elevation = dm_random_get_uniform(     0.0_r8,   100.0_r8)
        end do

        print '(" Packing and unpacking ...")'
        call dm_buffer_init(buffer, BUFFER_SIZE)
        call dm_timer_start(timer)

        do i = 1, N
            ! Reset buffer position to avoid deallocation.
            call dm_buffer_reset(buffer)

            ! Pack.
            call dm_msgpack_init(packer, buffer)
            call dm_msgpack_pack_type(packer, nodes1(i), rc)
            if (dm_is_error(rc)) exit
            call dm_msgpack_destroy(packer)

            ! Unpack.
            call dm_msgpack_unpack_type(unpack, buffer, nodes2(i), rc)
            if (dm_is_error(rc)) exit
            call dm_msgpack_destroy(unpack)
        end do

        call dm_timer_stop(timer)
        call dm_buffer_destroy(buffer)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print '(" Elapsed time: ", f8.6, " sec")', dm_timer_result(timer)
        print '(" Ops per sec.: ", i0)',           int(N / dm_timer_result(timer))

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print '(" Validating ...")'

        do i = 1, N
            if (.not. (nodes1(i) == nodes2(i))) return
        end do

        stat = TEST_PASSED
    end function test05

    logical function test06() result(stat)
        !! Packs and unpacks derived types.
        integer(i8), parameter :: BUFFER_SIZE = 1024_i8 * 10

        integer           :: rc
        type(buffer_type) :: buffer

        stat = TEST_FAILED

        beat_block: block
            integer(i8)           :: nbytes
            type(ipc_header_type) :: header1, header2
            type(beat_type)       :: beat1, beat2

            print '(" Testing packing of beat message ...")'
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_ipc_header(header1, from='dmtestmsgpack', to='dmdummy', type=TYPE_BEAT)
            call dm_test_dummy(beat1)

            call dm_msgpack_pack_message(buffer, header1, beat1, nbytes, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Buffer size: ", i0)', buffer%nbytes
            print '(" Packed size: ", i0)', nbytes
            if (buffer%nbytes /= nbytes) return

            print '(" Testing unpacking of beat message ...")'
            call dm_msgpack_unpack_message(buffer, header2, beat2, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Validating beat message ...", /, " Header:")'
            call dm_ipc_header_out(header1)
            if (.not. (header1 == header2)) return
            if (.not. (beat1 == beat2))     return
        end block beat_block

        call dm_buffer_destroy(buffer)
        print '(72("."))'

        dp_block: block
            integer(i8)           :: nbytes
            type(ipc_header_type) :: header1, header2
            type(dp_type)         :: dp1, dp2

            print '(" Testing packing of data point message ...")'
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_ipc_header(header1, from='dmtestmsgpack', to='dmdummy', type=TYPE_DP)

            dp1%x = dm_time_now()
            dp1%y = dm_random_get()

            call dm_msgpack_pack_message(buffer, header1, dp1, nbytes, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Buffer size: ", i0)', buffer%nbytes
            print '(" Packed size: ", i0)', nbytes
            if (buffer%nbytes /= nbytes) return

            print '(" Testing unpacking of data point message ...")'
            call dm_msgpack_unpack_message(buffer, header2, dp2, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Validating data point message ...", /, " Header:")'
            call dm_ipc_header_out(header1)
            if (.not. (header1 == header2)) return
            if (.not. (dp1 == dp2))         return
        end block dp_block

        call dm_buffer_destroy(buffer)
        print '(72("."))'

        log_block: block
            integer(i8)           :: nbytes
            type(ipc_header_type) :: header1, header2
            type(log_type)        :: log1, log2

            print '(" Testing packing of log message ...")'
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_ipc_header(header1, from='dmtestmsgpack', to='dmdummy', type=TYPE_LOG)
            call dm_test_dummy(log1)

            call dm_msgpack_pack_message(buffer, header1, log1, nbytes, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Buffer size: ", i0)', buffer%nbytes
            print '(" Packed size: ", i0)', nbytes
            if (buffer%nbytes /= nbytes) return

            print '(" Testing unpacking of log message ...")'
            call dm_msgpack_unpack_message(buffer, header2, log2, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Validating log message ...", /, " Header:")'
            call dm_ipc_header_out(header1)
            if (.not. (header1 == header2)) return
            if (.not. (log1 == log2))       return
        end block log_block

        call dm_buffer_destroy(buffer)
        print '(72("."))'

        node_block: block
            integer(i8)           :: nbytes
            type(ipc_header_type) :: header1, header2
            type(node_type)       :: node1, node2

            print '(" Testing packing of node message ...")'
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_ipc_header(header1, from='dmtestmsgpack', to='dmdummy', type=TYPE_NODE)
            call dm_test_dummy(node1)

            call dm_msgpack_pack_message(buffer, header1, node1, nbytes, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Buffer size: ", i0)', buffer%nbytes
            print '(" Packed size: ", i0)', nbytes
            if (buffer%nbytes /= nbytes) return

            print '(" Testing unpacking of node message ...")'
            call dm_msgpack_unpack_message(buffer, header2, node2, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Validating node message ...", /, " Header:")'
            call dm_ipc_header_out(header1)
            if (.not. (header1 == header2)) return
            if (.not. (node1 == node2))     return
        end block node_block

        call dm_buffer_destroy(buffer)
        print '(72("."))'

        observ_block: block
            integer(i8)           :: nbytes
            type(ipc_header_type) :: header1, header2
            type(observ_type)     :: observ1, observ2

            print '(" Testing packing of observation message ...")'
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_ipc_header(header1, from='dmtestmsgpack', to='dmdummy', type=TYPE_OBSERV)
            call dm_test_dummy(observ1, nresponses=OBSERV_MAX_NRESPONSES)

            call dm_msgpack_pack_message(buffer, header1, observ1, nbytes, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Buffer size: ", i0)', buffer%nbytes
            print '(" Packed size: ", i0)', nbytes
            if (buffer%nbytes /= nbytes) return

            print '(" Testing unpacking of observation message ...")'
            call dm_msgpack_unpack_message(buffer, header2, observ2, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Validating observation message ...", /, " Header:")'
            call dm_ipc_header_out(header1)
            if (.not. (header1 == header2)) return
            if (.not. (observ1 == observ2)) return
        end block observ_block

        call dm_buffer_destroy(buffer)
        print '(72("."))'

        sensor_block: block
            integer(i8)           :: nbytes
            type(ipc_header_type) :: header1, header2
            type(sensor_type)     :: sensor1, sensor2

            print '(" Testing packing of sensor message ...")'
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_ipc_header(header1, from='dmtestmsgpack', to='dmdummy', type=TYPE_SENSOR)
            call dm_test_dummy(sensor1)

            call dm_msgpack_pack_message(buffer, header1, sensor1, nbytes, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Buffer size: ", i0)', buffer%nbytes
            print '(" Packed size: ", i0)', nbytes
            if (buffer%nbytes /= nbytes) return

            print '(" Testing unpacking of sensor message ...")'
            call dm_msgpack_unpack_message(buffer, header2, sensor2, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Validating sensor message ...", /, " Header:")'
            call dm_ipc_header_out(header1)
            if (.not. (header1 == header2)) return
            if (.not. (sensor1 == sensor2)) return
        end block sensor_block

        call dm_buffer_destroy(buffer)
        print '(72("."))'

        target_block: block
            integer(i8)           :: nbytes
            type(ipc_header_type) :: header1, header2
            type(target_type)     :: target1, target2

            print '(" Testing packing of target message ...")'
            call dm_buffer_init(buffer, BUFFER_SIZE)
            call dm_ipc_header(header1, from='dmtestmsgpack', to='dmdummy', type=TYPE_TARGET)
            call dm_test_dummy(target1)

            call dm_msgpack_pack_message(buffer, header1, target1, nbytes, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Buffer size: ", i0)', buffer%nbytes
            print '(" Packed size: ", i0)', nbytes
            if (buffer%nbytes /= nbytes) return

            print '(" Testing unpacking of target message ...")'
            call dm_msgpack_unpack_message(buffer, header2, target2, rc)
            call dm_error_out(rc); if (dm_is_error(rc)) return

            print '(" Validating target message ...", /, " Header:")'
            call dm_ipc_header_out(header1)
            if (.not. (header1 == header2)) return
            if (.not. (target1 == target2)) return
        end block target_block

        call dm_buffer_destroy(buffer)

        stat = TEST_PASSED
    end function test06

    logical function test07() result(stat)
        !! Benchmark for packing and unpacking of observations.
        stat = TEST_FAILED

        pack_block: block
            integer(i8), parameter :: BUFFER_SIZE = 1024_i8 * 5
            integer,     parameter :: N           = 1000

            integer                        :: i, rc
            type(buffer_type)              :: buffer
            type(observ_type), allocatable :: observs(:)
            type(timer_type)               :: timer

            print '(" Testing packing of observation message ...")'

            allocate (observs(N))
            call dm_test_dummy(observs, nresponses=OBSERV_MAX_NRESPONSES)
            call dm_timer_start(timer)

            do i = 1, N
                call dm_buffer_init(buffer, BUFFER_SIZE)
                call dm_msgpack_pack_message(buffer, dm_ipc_header_observ('dmtestmsgpack', 'dmdummy'), observs(i), error=rc)
                call dm_error_out(rc)
                if (dm_is_error(rc)) return
                call dm_buffer_destroy(buffer)
            end do

            call dm_timer_stop(timer)

            print '(" Elapsed time: ", f8.6, " sec")', dm_timer_result(timer)
            print '(" Ops per sec.: ", i0)',           int(N / dm_timer_result(timer))
        end block pack_block

        stat = TEST_PASSED
    end function test07

    subroutine bytes_out(bytes)
        !! Outputs bytes in hex format.
        character(*), intent(in) :: bytes

        integer :: i

        write (*, '(" Encoded bytes:")')
        write (*, '(*(1x, z2.2))') (ichar(bytes(i:i)), i = 1, len(bytes))
    end subroutine bytes_out
end program dmtestmsgpack
