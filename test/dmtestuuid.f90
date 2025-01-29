! dmtestuuid.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestuuid
    !! Test program for UUID4 generation. Change `NUUIDS` to the number of
    !! UUIDs to be generated.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestuuid'
    integer,          parameter :: NTESTS    = 2
    integer,          parameter :: NUUIDS    = 1024

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=UUID_LEN) :: uuids(NUUIDS)
        integer                 :: i, j

        stat = TEST_FAILED

        print '(" Generating and validating ", i0, " UUIDs ...")', NUUIDS

        do i = 1, NUUIDS
            uuids(i) = dm_uuid4()
            if (.not. dm_uuid4_is_valid(uuids(i))) then
                print '(" Error: UUID ", a, " is invalid")', uuids(i)
                return
            end if

            do j = 1, i - 1
                if (uuids(j) == uuids(i)) then
                    print '(" Error: duplicate UUID ", a)', uuids(i)
                    return
                end if
            end do
        end do

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=UUID_LEN + 4) :: uuid

        stat = TEST_FAILED

        print *, 'Adding hyphens to UUID ...'
        uuid = dm_uuid4_hyphenize(UUID_DEFAULT)
        if (uuid /= '00000000-0000-0000-0000-000000000000') return

        stat = TEST_PASSED
    end function test02
end program dmtestuuid
