! dmtesthash.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthash
    !! Hash and hash table tests.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtesthash'
    integer,          parameter :: NTESTS    = 4

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'), compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        stat = TEST_FAILED

        print *, 'Checking hashes ...'
        if (dm_hash_djb2('?') /= 177636) return
        if (dm_hash_djb2a('?') /= 177562) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer(kind=i8) :: hashes(2)

        stat = TEST_FAILED

        print *, 'Checking collision ...'
        hashes(1) = dm_hash_fnv1('creamwove')
        hashes(2) = dm_hash_fnv1('quists')
        if (hashes(1) /= hashes(2)) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        integer(kind=i8) :: hashes(2)

        stat = TEST_FAILED

        print *, 'Checking collision ...'
        hashes(1) = dm_hash_fnv1a('costarring')
        hashes(2) = dm_hash_fnv1a('liquid')
        if (hashes(1) /= hashes(2)) return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        character(len=32), target :: values(3)
        class(*), pointer         :: ptr
        integer                   :: rc
        type(hash_table_type)     :: table

        stat = TEST_FAILED

        values = [ character(len=32) :: 'bar', 'baz', 'qux' ]

        test_block: block
            print *, 'Creating hash table ...'
            rc = dm_hash_table_create(table, size(values));  if (dm_is_error(rc)) exit test_block

            print *, 'Setting values to hash table ...'
            rc = dm_hash_table_set(table, 'foo', values(1)); if (dm_is_error(rc)) exit test_block
            rc = dm_hash_table_set(table, 'zap', values(2)); if (dm_is_error(rc)) exit test_block
            rc = dm_hash_table_set(table, 'uxn', values(3)); if (dm_is_error(rc)) exit test_block

            print *, 'Getting key from hash table ...'
            rc = dm_hash_table_get(table, 'zap', ptr);       if (dm_is_error(rc)) exit test_block

            rc = E_NULL
            if (.not. associated(ptr)) exit test_block

            rc = E_CORRUPT
            select type (value => ptr)
                type is (character(len=*))
                    print '(" zap: ", a)', trim(value)
                    if (value /= 'baz') exit test_block
                    rc = E_NONE

                class default
                    exit test_block
            end select
        end block test_block

        call dm_hash_table_destroy(table)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test04
end program dmtesthash
