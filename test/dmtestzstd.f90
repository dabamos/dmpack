! dmtestzstd.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestzstd
    !! Test program for Zstandard compression (zstd).
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestzstd'
    integer,          parameter :: NTESTS = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=NML_OBSERV_LEN) :: input, output2
        character(len=:), allocatable :: output1
        integer                       :: rc
        integer(kind=i8)              :: output_len
        type(observ_type)             :: observ1, observ2

        stat = TEST_FAILED

        print '(" Library: ", a)', dm_zstd_version(.true.)

        call dm_test_dummy(observ1)
        rc = dm_nml_from(observ1, input)

        zstd_block: block
            print *, 'Compressing ...'
            rc = dm_zstd_compress(input, output1, output_len=output_len)
            if (dm_is_error(rc)) exit zstd_block

            print *, 'Uncompressing ...'
            rc = dm_zstd_uncompress(output1, output2, input_len=output_len)
            if (dm_is_error(rc)) exit zstd_block
        end block zstd_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching ...'
        rc = dm_nml_to(output2, observ2)
        if (.not. (observ2 == observ1)) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=NML_OBSERV_LEN) :: input, output2
        character(len=:), allocatable :: output1
        integer                       :: rc
        integer(kind=i8)              :: output_len
        type(observ_type)             :: observ1, observ2
        type(zstd_context_type)       :: context

        stat = TEST_FAILED

        call dm_test_dummy(observ1)
        rc = dm_nml_from(observ1, input)

        zstd_block: block
            print *, 'Compressing with context ...'
            rc = dm_zstd_compress(context, input, output1, output_len=output_len)
            if (dm_is_error(rc)) exit zstd_block

            print *, 'Uncompressing with context ...'
            rc = dm_zstd_uncompress(context, output1, output2, input_len=output_len)
            if (dm_is_error(rc)) exit zstd_block
        end block zstd_block

        print *, 'Destroying ...'
        if (dm_is_error(dm_zstd_destroy(context))) return

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Matching ...'
        rc = dm_nml_to(output2, observ2)
        if (.not. (observ2 == observ1)) return

        stat = TEST_PASSED
    end function test02
end program dmtestzstd
