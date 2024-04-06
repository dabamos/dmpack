! dmtestz.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestz
    !! Test program for deflate and zstd compression.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('dmtestz.test01', test01), &
        test_type('dmtestz.test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        stat = TEST_FAILED

        print *, 'Converting content encoding to compression enumerator ...'
        if (dm_z_type_from_encoding('')        /= Z_TYPE_NONE)    return
        if (dm_z_type_from_encoding('       ') /= Z_TYPE_NONE)    return
        if (dm_z_type_from_encoding('deflate') /= Z_TYPE_ZLIB)    return
        if (dm_z_type_from_encoding('zstd   ') /= Z_TYPE_ZSTD)    return
        if (dm_z_type_from_encoding('DEFLATE') /= Z_TYPE_INVALID) return
        if (dm_z_type_from_encoding('dummy')   /= Z_TYPE_INVALID) return

        print *, 'Converting compression enumerator to content encoding ...'
        if (dm_z_type_to_encoding(Z_TYPE_INVALID) /= '')        return
        if (dm_z_type_to_encoding(Z_TYPE_NONE)    /= '')        return
        if (dm_z_type_to_encoding(Z_TYPE_ZLIB)    /= 'deflate') return
        if (dm_z_type_to_encoding(Z_TYPE_ZSTD)    /= 'zstd')    return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer :: rc

        stat = TEST_FAILED

        test_block: block
            character(len=:), allocatable :: str
            integer                       :: type

            type(beat_type)   :: beat1, beat2
            type(log_type)    :: log1, log2
            type(node_type)   :: node1, node2
            type(observ_type) :: observ1, observ2
            type(sensor_type) :: sensor1, sensor2
            type(target_type) :: target1, target2

            call dm_test_dummy(beat1)

            do type = Z_TYPE_NONE, Z_TYPE_LAST
                print *, 'Compressing and uncompressing beat (' // trim(Z_TYPE_NAMES(type)) // ') ...'
                rc = dm_z_compress  (beat1, str, type); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, beat2, type); if (dm_is_error(rc)) exit test_block
                rc = E_CORRUPT
                if (.not. (beat1 == beat2)) exit test_block
            end do

            call dm_test_dummy(log1)

            do type = Z_TYPE_NONE, Z_TYPE_LAST
                print *, 'Compressing and uncompressing log (' // trim(Z_TYPE_NAMES(type)) // ') ...'
                rc = dm_z_compress  (log1, str, type); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, log2, type); if (dm_is_error(rc)) exit test_block
                rc = E_CORRUPT
                if (.not. (log1 == log2)) exit test_block
            end do

            call dm_test_dummy(node1)

            do type = Z_TYPE_NONE, Z_TYPE_LAST
                print *, 'Compressing and uncompressing node (' // trim(Z_TYPE_NAMES(type)) // ') ...'
                rc = dm_z_compress  (node1, str, type); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, node2, type); if (dm_is_error(rc)) exit test_block
                rc = E_CORRUPT
                if (.not. (node1 == node2)) exit test_block
            end do

            call dm_test_dummy(observ1)

            do type = Z_TYPE_NONE, Z_TYPE_LAST
                print *, 'Compressing and uncompressing observation (' // trim(Z_TYPE_NAMES(type)) // ') ...'
                rc = dm_z_compress  (observ1, str, type); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, observ2, type); if (dm_is_error(rc)) exit test_block
                rc = E_CORRUPT
                if (.not. (observ1 == observ2)) exit test_block
            end do

            call dm_test_dummy(sensor1)

            do type = Z_TYPE_NONE, Z_TYPE_LAST
                print *, 'Compressing and uncompressing sensor (' // trim(Z_TYPE_NAMES(type)) // ') ...'
                rc = dm_z_compress  (sensor1, str, type); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, sensor2, type); if (dm_is_error(rc)) exit test_block
                rc = E_CORRUPT
                if (.not. (sensor1 == sensor2)) exit test_block
            end do

            call dm_test_dummy(target1)

            do type = Z_TYPE_NONE, Z_TYPE_LAST
                print *, 'Compressing and uncompressing target (' // trim(Z_TYPE_NAMES(type)) // ') ...'
                rc = dm_z_compress  (target1, str, type); if (dm_is_error(rc)) exit test_block
                rc = dm_z_uncompress(str, target2, type); if (dm_is_error(rc)) exit test_block
                rc = E_CORRUPT
                if (.not. (target1 == target2)) exit test_block
            end do

            rc = E_NONE
        end block test_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test02
end program dmtestz
