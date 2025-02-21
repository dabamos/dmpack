! dmtesthashtable.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthashtable
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtesthashtable'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
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
    end function test01
end program dmtesthashtable
