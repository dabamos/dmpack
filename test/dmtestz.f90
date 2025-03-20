! dmtestz.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestz
    !! Test program for deflate and zstd compression.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestz'
    integer,          parameter :: NTESTS    = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'), compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        stat = TEST_FAILED

        print *, 'Content encoding to compression enumerator ...'
        if (dm_z_type_from_encoding('')        /= Z_TYPE_NONE)    return
        if (dm_z_type_from_encoding('       ') /= Z_TYPE_NONE)    return
        if (dm_z_type_from_encoding('deflate') /= Z_TYPE_ZLIB)    return
        if (dm_z_type_from_encoding('zstd   ') /= Z_TYPE_ZSTD)    return
        if (dm_z_type_from_encoding('DEFLATE') /= Z_TYPE_INVALID) return
        if (dm_z_type_from_encoding('dummy')   /= Z_TYPE_INVALID) return

        print *, 'Compression enumerator to content encoding ...'
        if (dm_z_type_to_encoding(Z_TYPE_INVALID) /= '')        return
        if (dm_z_type_to_encoding(Z_TYPE_NONE)    /= '')        return
        if (dm_z_type_to_encoding(Z_TYPE_ZLIB)    /= 'deflate') return
        if (dm_z_type_to_encoding(Z_TYPE_ZSTD)    /= 'zstd')    return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=*), parameter :: FMT = &
            '(" - Compressed (", a, ") from ", i5, " bytes to ", i5, " bytes (", f4.1, " %) in ", f8.6, " sec")'

        integer :: rc

        stat = TEST_FAILED

        test_block: block
            character(len=:), allocatable :: str

            integer          :: m, n, z
            real(kind=r8)    :: p, t
            type(timer_type) :: timer

            type(beat_type)   :: beat1, beat2
            type(log_type)    :: log1, log2
            type(node_type)   :: node1, node2
            type(observ_type) :: observ1, observ2
            type(sensor_type) :: sensor1, sensor2
            type(target_type) :: target1, target2

            call dm_test_dummy(beat1)

            do z = Z_TYPE_NONE, Z_TYPE_LAST
                if (z == Z_TYPE_NONE) print *, 'Type beat ...'

                call dm_timer_start(timer)
                rc = dm_z_compress  (beat1, z, str); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, z, beat2); if (dm_is_error(rc)) exit test_block
                call dm_timer_stop(timer, t)

                m = NML_BEAT_LEN
                n = len(str)
                p = (real(n) / real(m)) * 100.0
                print FMT, Z_TYPE_NAMES(z), m, n, p, t

                rc = E_CORRUPT
                if (.not. (beat1 == beat2)) exit test_block
            end do

            call dm_test_dummy(log1)

            do z = Z_TYPE_NONE, Z_TYPE_LAST
                if (z == Z_TYPE_NONE) print *, 'Type log ...'

                call dm_timer_start(timer)
                rc = dm_z_compress  (log1, z, str); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, z, log2); if (dm_is_error(rc)) exit test_block
                call dm_timer_stop(timer, t)

                m = NML_LOG_LEN
                n = len(str)
                p = (real(n) / real(m)) * 100.0
                print FMT, Z_TYPE_NAMES(z), m, n, p, t

                rc = E_CORRUPT
                if (.not. (log1 == log2)) exit test_block
            end do

            call dm_test_dummy(node1)

            do z = Z_TYPE_NONE, Z_TYPE_LAST
                if (z == Z_TYPE_NONE) print *, 'Type node ...'

                call dm_timer_start(timer)
                rc = dm_z_compress  (node1, z, str); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, z, node2); if (dm_is_error(rc)) exit test_block
                call dm_timer_stop(timer, t)

                m = NML_NODE_LEN
                n = len(str)
                p = (real(n) / real(m)) * 100.0
                print FMT, Z_TYPE_NAMES(z), m, n, p, t

                rc = E_CORRUPT
                if (.not. (node1 == node2)) exit test_block
            end do

            call dm_test_dummy(observ1)

            do z = Z_TYPE_NONE, Z_TYPE_LAST
                if (z == Z_TYPE_NONE) print *, 'Type observation ...'

                call dm_timer_start(timer)
                rc = dm_z_compress  (observ1, z, str); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, z, observ2); if (dm_is_error(rc)) exit test_block
                call dm_timer_stop(timer, t)

                m = NML_OBSERV_LEN
                n = len(str)
                p = (real(n) / real(m)) * 100.0
                print FMT, Z_TYPE_NAMES(z), m, n, p, t

                rc = E_CORRUPT
                if (.not. (observ1 == observ2)) exit test_block
            end do

            call dm_test_dummy(sensor1)

            do z = Z_TYPE_NONE, Z_TYPE_LAST
                if (z == Z_TYPE_NONE) print *, 'Type sensor ...'

                call dm_timer_start(timer)
                rc = dm_z_compress  (sensor1, z, str); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, z, sensor2); if (dm_is_error(rc)) exit test_block
                call dm_timer_stop(timer, t)

                m = NML_SENSOR_LEN
                n = len(str)
                p = (real(n) / real(m)) * 100.0
                print FMT, Z_TYPE_NAMES(z), m, n, p, t

                rc = E_CORRUPT
                if (.not. (sensor1 == sensor2)) exit test_block
            end do

            call dm_test_dummy(target1)

            do z = Z_TYPE_NONE, Z_TYPE_LAST
                if (z == Z_TYPE_NONE) print *, 'Type target ...'

                call dm_timer_start(timer)
                rc = dm_z_compress  (target1, z, str); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, z, target2); if (dm_is_error(rc)) exit test_block
                call dm_timer_stop(timer, t)

                m = NML_TARGET_LEN
                n = len(str)
                p = (real(n) / real(m)) * 100.0
                print FMT, Z_TYPE_NAMES(z), m, n, p, t

                rc = E_CORRUPT
                if (.not. (target1 == target2)) exit test_block
            end do

            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        integer, parameter :: N = 100

        integer                        :: i, rc, rc2
        real(kind=r8)                  :: t
        type(timer_type)               :: timer
        type(zstd_context_type)        :: context
        type(observ_type), allocatable :: observs(:)
        character(len=:),  allocatable :: str
        type(string_type), allocatable :: strings(:)

        stat = TEST_FAILED

        allocate (observs(N))
        call dm_test_dummy(observs)

        ! No context.
        print *, 'Compressing without zstd context ...'
        call dm_timer_start(timer)
        do i = 1, N
            rc = dm_z_compress(observs(i), Z_TYPE_ZSTD, str)
            if (dm_is_error(rc)) exit
        end do
        call dm_timer_stop(timer, t)
        print '(" - ", i0, " observs in ", f8.6, " sec")', N, t

        print *, 'Uncompressing without zstd context ...'
        call dm_timer_start(timer)
        do i = 1, N
            rc = dm_z_uncompress(str, Z_TYPE_ZSTD, observs(i))
            if (dm_is_error(rc)) exit
        end do
        call dm_timer_stop(timer, t)
        print '(" - ", i0, " observs in ", f8.6, " sec")', N, t

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        ! Context.
        print *, 'Compressing with zstd context ...'
        call dm_timer_start(timer)
        do i = 1, N
            rc = dm_z_compress(observs(i), Z_TYPE_ZSTD, str, context=context)
            if (dm_is_error(rc)) exit
        end do
        call dm_timer_stop(timer, t)
        print '(" - ", i0, " observs in ", f8.6, " sec")', N, t

        print *, 'Uncompressing with zstd context ...'
        call dm_timer_start(timer)
        do i = 1, N
            rc = dm_z_uncompress(str, Z_TYPE_ZSTD, observs(i), context=context)
            if (dm_is_error(rc)) exit
        end do
        call dm_timer_stop(timer, t)
        print '(" - ", i0, " observs in ", f8.6, " sec")', N, t

        call dm_error_out(rc)
        rc2 = dm_zstd_destroy(context)
        call dm_error_out(rc2)
        if (dm_is_error(rc)) return

        ! Type context.
        print *, 'Compressing type with zstd context ...'
        call dm_timer_start(timer)
        do i = 1, N
            rc = dm_z_compress_type(observs(i), Z_TYPE_ZSTD, str, context=context)
            if (dm_is_error(rc)) exit
        end do
        call dm_timer_stop(timer, t)
        print '(" - ", i0, " observs in ", f8.6, " sec")', N, t

        call dm_error_out(rc)
        rc2 = dm_zstd_destroy(context)
        call dm_error_out(rc2)
        if (dm_is_error(rc)) return

        ! Types context.
        print *, 'Compressing types with zstd context ...'
        call dm_timer_start(timer)
        rc = dm_z_compress_types(observs, Z_TYPE_ZSTD, strings)
        call dm_timer_stop(timer, t)
        print '(" - ", i0, " observs in ", f8.6, " sec")', N, t

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test03
end program dmtestz
