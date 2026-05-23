! dmtestmsgpack.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestmsgpack
    !! Test program for network module.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestmsgpack'
    integer,          parameter :: NTESTS    = 10

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
        test_type('test10', test10)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        !! Writes and reads nil.
        character(MSGPACK_SIZE_NIL) :: bytes
        integer                     :: rc

        stat = TEST_FAILED
        print '(" Testing nil ...")'

        call dm_msgpack_write_nil(bytes)
        call bytes_out(bytes)
        call dm_msgpack_read_nil(bytes, rc)

        call dm_error_out(rc)
        if (dm_is_error(rc) .or. bytes(1:1) /= char(MSGPACK_NIL)) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! Writes and reads logical.
        logical, parameter :: ASSERT = .true.

        character(MSGPACK_SIZE_BOOL) :: bytes
        integer                      :: rc
        logical                      :: v

        stat = TEST_FAILED
        print '(" Testing bool ...")'

        v = ASSERT

        call dm_msgpack_write(v, bytes)
        call bytes_out(bytes)
        call dm_msgpack_read(bytes, v, rc)

        call dm_error_out(rc)
        print '(" Decoded value: ", l1)', v
        if (dm_is_error(rc) .or. v .neqv. ASSERT) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        !! Writes and reads 4-byte real.
        real(r4), parameter :: ASSERT = 1234.123456_r4

        character(MSGPACK_SIZE_FLOAT32) :: bytes
        integer                         :: rc
        real(r4)                        :: v

        stat = TEST_FAILED
        print '(" Testing float32 ...")'

        v = ASSERT

        call dm_msgpack_write(v, bytes)
        call bytes_out(bytes)
        call dm_msgpack_read(bytes, v, rc)

        call dm_error_out(rc)
        print '(" Decoded value: ", f0.8)', v
        if (dm_is_error(rc) .or. ASSERT - v > 10e-8) return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        !! Writes and reads 8-byte real.
        real(r8), parameter :: ASSERT = 1234567890.1234567890123456789_r8

        character(MSGPACK_SIZE_FLOAT64) :: bytes
        integer                         :: rc
        real(r8)                        :: v

        stat = TEST_FAILED
        print '(" Testing float64 ...")'

        v = ASSERT

        call dm_msgpack_write(v, bytes)
        call bytes_out(bytes)
        call dm_msgpack_read(bytes, v, rc)

        call dm_error_out(rc)
        print '(" Decoded value: ", f0.20)', v
        if (dm_is_error(rc) .or. ASSERT - v > 10e-16) return

        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        !! Writes and reads 4-byte integer.
        integer(i4), parameter :: ASSERT = -123456_i4

        character(MSGPACK_SIZE_INT32) :: bytes
        integer                       :: rc
        integer(i4)                   :: v

        stat = TEST_FAILED
        print '(" Testing int32 ...")'

        v = ASSERT

        call dm_msgpack_write(v, bytes)
        call bytes_out(bytes)
        call dm_msgpack_read(bytes, v, rc)

        call dm_error_out(rc)
        print '(" Decoded value: ", i0)', v
        if (dm_is_error(rc) .or. v /= ASSERT) return

        stat = TEST_PASSED
    end function test05

    logical function test06() result(stat)
        !! Writes and reads 8-byte integer.
        integer(i8), parameter :: ASSERT = -123456789012345_i8

        character(MSGPACK_SIZE_INT64) :: bytes
        integer                       :: rc
        integer(i8)                   :: v

        stat = TEST_FAILED
        print '(" Testing int64 ...")'

        v = ASSERT

        call dm_msgpack_write(v, bytes)
        call bytes_out(bytes)
        call dm_msgpack_read(bytes, v, rc)

        call dm_error_out(rc)
        print '(" Decoded value: ", i0)', v
        if (dm_is_error(rc) .or. v /= ASSERT) return

        stat = TEST_PASSED
    end function test06

    logical function test07() result(stat)
        !! Writes and read strings.
        character(:), allocatable :: assert_fixstr
        character(:), allocatable :: assert_str8
        character(:), allocatable :: assert_str16
        character(:), allocatable :: assert_str32

        character(80 * 1024) :: bytes, v
        integer              :: n, rc

        stat = TEST_FAILED

        bytes = ' '
        v     = ' '

        assert_fixstr = repeat('A', 31)
        assert_str8   = repeat('B', 64)
        assert_str16  = repeat('C', 512)
        assert_str32  = repeat('D', 68000)

        print '(" Testing fixstr ...")'
        call dm_msgpack_write_fixstr(assert_fixstr, bytes)
        call dm_msgpack_read_string(bytes, v, n, error=rc)
        call dm_error_out(rc)

        print '(" String length: ", i0)', n
        if (n /= len(assert_fixstr)) return
        if (dm_is_error(rc) .or. v /= assert_fixstr) return

        print '(" Testing str8 ...")'
        call dm_msgpack_write_str8(assert_str8, bytes)
        call dm_msgpack_read_string(bytes, v, n, error=rc)
        call dm_error_out(rc)

        print '(" String length: ", i0)', n
        if (n /= len(assert_str8)) return
        if (dm_is_error(rc) .or. v /= assert_str8) return

        print '(" Testing str16 ...")'
        call dm_msgpack_write_str16(assert_str16, bytes)
        call dm_msgpack_read_string(bytes, v, n, error=rc)
        call dm_error_out(rc)

        print '(" String length: ", i0)', n
        if (n /= len(assert_str16)) return
        if (dm_is_error(rc) .or. v /= assert_str16) return

        print '(" Testing str32 ...")'
        call dm_msgpack_write_str32(assert_str32, bytes)
        call dm_msgpack_read_string(bytes, v, n, error=rc)
        call dm_error_out(rc)

        print '(" String length: ", i0)', n
        if (n /= len(assert_str32)) return
        if (dm_is_error(rc) .or. v /= assert_str32) return

        print '(" Testing string ...")'
        call dm_msgpack_write_string(assert_str32, bytes)
        call dm_msgpack_read_string(bytes, v, n, error=rc)
        call dm_error_out(rc)

        print '(" String length: ", i0)', n
        if (n /= len(assert_str32)) return
        if (dm_is_error(rc) .or. v /= assert_str32) return

        stat = TEST_PASSED
    end function test07

    logical function test08() result(stat)
        !! Packs and unpack scalar values.
        integer(i8), parameter :: BUFFER_SIZE = 512

        integer     :: i, rc
        integer(i4) :: i32
        integer(i8) :: i64
        logical     :: l32
        real(r4)    :: r32
        real(r8)    :: r64

        character(:), pointer     :: bytes
        type(msgpack_buffer_type) :: buffer
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

        call dm_msgpack_buffer_init(buffer, BUFFER_SIZE)
        call dm_msgpack_packer_init(packer, buffer)

        call dm_msgpack_pack(packer, i32, rc); if (dm_is_error(rc)) return
        call dm_msgpack_pack(packer, i64, rc); if (dm_is_error(rc)) return
        call dm_msgpack_pack(packer, l32, rc); if (dm_is_error(rc)) return
        call dm_msgpack_pack(packer, r32, rc); if (dm_is_error(rc)) return
        call dm_msgpack_pack(packer, r64, rc); if (dm_is_error(rc)) return

        print '(/, " Buffer Size: ", i0)', dm_msgpack_buffer_size(buffer)
        print '(" Packed Size: ", i0)',    dm_msgpack_packer_size(packer)

        bytes => dm_msgpack_packer_result(packer)
        call dm_msgpack_packer_destroy(packer)

        print '(" Encoded bytes:")'
        print '(*(1x, z2.2))', (ichar(bytes(i:i)), i = 1, len(bytes))
        print '(/, " Testing unpacking ...")'

        associate (object => unpack%object)
            call dm_msgpack_unpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack(object, i32, rc);         if (dm_is_error(rc)) return

            call dm_msgpack_unpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack(object, i64, rc);         if (dm_is_error(rc)) return

            call dm_msgpack_unpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack(object, l32, rc);         if (dm_is_error(rc)) return

            call dm_msgpack_unpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack(object, r32, rc);         if (dm_is_error(rc)) return

            call dm_msgpack_unpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack(object, r64, rc);         if (dm_is_error(rc)) return

            call dm_msgpack_unpack_next(unpack, buffer, rc); if (dm_is_ok(rc))    return
        end associate

        print '(" int32..: ", i0)',    i32
        print '(" int64..: ", i0)',    i64
        print '(" bool...: ", l1)',    l32
        print '(" float32: ", f0.8)',  r32
        print '(" float64: ", f0.16)', r64

        call dm_msgpack_unpack_destroy(unpack)
        call dm_msgpack_buffer_destroy(buffer)

        stat = TEST_PASSED
    end function test08

    logical function test09() result(stat)
        !! Very basic benchmark of serialisation/deserialisation.
        integer, parameter :: N = 1000000

        integer          :: i, rc
        real(r8)         :: value
        type(timer_type) :: timer

        character(MSGPACK_SIZE_FLOAT64) :: bytes
        real(r8), allocatable           :: values(:)

        stat = TEST_FAILED
        print '(" Running benchmark ...")'

        allocate (values(N))
        call random_number(values)
        call dm_timer_start(timer)

        do i = 1, N
            call dm_msgpack_write(values(i), bytes)
            call dm_msgpack_read(bytes, value, rc)
        end do

        call dm_timer_stop(timer)
        print '(" Elapsed time: ", f8.6, " sec")', dm_timer_result(timer)
        print '(" Ops per sec.: ", i0)',           int(N / dm_timer_result(timer))

        stat = TEST_PASSED
    end function test09

    logical function test10() result(stat)
        !! Packs and unpacks fixarray.
        integer,     parameter :: N           = 8
        integer,     parameter :: ASSERT(N)   = [ 8, 16, 32, 64, 128, 256, 512, 1024 ]
        integer(i8), parameter :: BUFFER_SIZE = 64

        character(:), pointer     :: bytes
        type(msgpack_buffer_type) :: buffer
        type(msgpack_packer_type) :: packer
        type(msgpack_unpack_type) :: unpack

        integer :: i, sz, rc
        integer :: v(N)

        stat = TEST_FAILED

        call dm_msgpack_buffer_init(buffer, BUFFER_SIZE)
        call dm_msgpack_packer_init(packer, buffer)

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

        print '(/, " Buffer Size: ", i0)', dm_msgpack_buffer_size(buffer)
        print '(" Packed Size: ", i0)',    dm_msgpack_packer_size(packer)

        bytes => dm_msgpack_packer_result(packer)
        call dm_msgpack_packer_destroy(packer)

        print '(" Encoded bytes:")'
        print '(*(1x, z2.2))', (ichar(bytes(i:i)), i = 1, len(bytes))

        ! Unpack from MessagePack buffer.
        print '(/, " Testing unpacking of fixarray ...")'

        associate (object => unpack%object)
            ! Read MessagePack array header + size.
            call dm_msgpack_unpack_next(unpack, buffer, rc); if (dm_is_error(rc)) return
            call dm_msgpack_unpack_fixarray(object, sz, rc); if (dm_is_error(rc)) return

            ! Validate array size.
            if (sz /= N) return

            ! Read all integers from MessagePack array.
            do i = 1, sz
                ! Read header.
                call dm_msgpack_unpack_next(unpack, buffer, rc)
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

        call dm_msgpack_unpack_destroy(unpack)
        call dm_msgpack_buffer_destroy(buffer)

        stat = TEST_PASSED
    end function test10

    subroutine bytes_out(bytes)
        !! Outputs bytes in hex format.
        character(*), intent(in) :: bytes

        integer :: i

        write (*, '(" Encoded bytes:")', advance='no')
        write (*, '(*(1x, z2.2))') (ichar(bytes(i:i)), i = 1, len(bytes))
    end subroutine bytes_out
end program dmtestmsgpack
