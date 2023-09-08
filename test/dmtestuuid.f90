! dmtestuuid.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestuuid
    !! Test program for UUID4 generation. Change `NUUIDS` to the number of
    !! UUIDs to be generated.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 2
    integer, parameter :: NUUIDS = 1024

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestuuid.test01', test01)
    tests(2) = test_type('dmtestuuid.test02', test02)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=UUID_LEN) :: uuids(NUUIDS)
        integer                 :: i, j

        stat = TEST_FAILED

        print *, 'Generating and validating ', NUUIDS, ' UUID4s ...'

        do i = 1, NUUIDS
            uuids(i) = dm_uuid4()
            if (.not. dm_uuid4_valid(uuids(i))) then
                print *, 'Error: ', uuids(i), ' is invalid'
                return
            end if

            do j = 1, i - 1
                if (uuids(j) == uuids(i)) then
                    print *, 'Error: duplicate UUIDs ', i, ' and ', j, ' (', uuids(i), ', ', uuids(j), ')'
                    return
                end if
            end do
        end do

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=UUID_LEN + 4) :: uuid

        stat = TEST_FAILED

        print *, 'Adding hyphens to UUID4 ...'
        uuid = dm_uuid4_hyphenize(UUID_DEFAULT)
        if (uuid /= '00000000-0000-0000-0000-000000000000') return

        stat = TEST_PASSED
    end function test02
end program dmtestuuid
