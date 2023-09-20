! Author:  Philipp Engel
! Licence: ISC
module dm_uuid
    !! Provides a UUID4 generator. DMPACK uses 32 characters long UUID4
    !! identifiers in hexadecimal format, without hyphens.
    implicit none (type, external)
    private

    integer,          parameter, public :: UUID_LEN      = 32 !! Hex UUID4 length.
    integer,          parameter, public :: UUID_FULL_LEN = 36 !! Full UUID4 length.
    character(len=*), parameter, public :: UUID_DEFAULT  = repeat('0', UUID_LEN) !! Default ID (hex).

    character(len=*), parameter :: UUID_SET = '0123456789abcdef'

    public :: dm_uuid4
    public :: dm_uuid4_hyphens
    public :: dm_uuid4_hyphenize
    public :: dm_uuid4_valid
contains
    impure elemental function dm_uuid4() result(uuid)
        !! Generates random UUID (RFC 4122) in hexadecimal format, i.e.,
        !! without hyphens (32 characters long). The PRNG has to be seeded
        !! before the first invocation by calling `dm_init()` once.
        character(len=UUID_LEN) :: uuid

        integer :: b(32), i, j
        real    :: r(32)

        call random_number(r)

        b     = int(r * 16)
        b(13) = 4
        b(17) = ior(iand(b(17), 3), 8)

        do i = 1, UUID_LEN
            j = 1 + b(i)
            uuid(i:i) = UUID_SET(j:j)
        end do
    end function dm_uuid4

    impure elemental function dm_uuid4_hyphens() result(uuid)
        !! Returns UUID4 with hyphens (36 characters long). The PRNG has to be
        !! seeded before the first invocation by calling `dm_init()` once.
        character(len=UUID_FULL_LEN) :: uuid

        integer :: i, j, k
        integer :: b(32)
        real    :: r(32)

        call random_number(r)

        b     = int(r * 16)
        b(13) = 4
        b(17) = ior(iand(b(17), 3), 8)

        j = 1

        do i = 1, UUID_LEN + 4
            select case (i)
                case (9, 14, 19, 24)
                    uuid(i:i) = '-'
                case default
                    k = 1 + b(j)
                    uuid(i:i) = UUID_SET(k:k)
                    j = j + 1
            end select
        end do
    end function dm_uuid4_hyphens

    pure elemental function dm_uuid4_hyphenize(uuid) result(str)
        !! Returns given UUID with hyphens, i.e., turns string
        !! `00000000000000000000000000000000` into
        !! `00000000-0000-0000-0000-000000000000`.
        character(len=UUID_LEN), intent(in) :: uuid
        character(len=UUID_FULL_LEN)        :: str

        integer :: stat

        write (str, '(a8, "-", 3(a4, "-"), a12)', iostat=stat) &
            uuid(1:8), uuid(9:12), uuid(13:16), uuid(17:20), uuid(21:32)
    end function dm_uuid4_hyphenize

    pure elemental logical function dm_uuid4_valid(uuid) result(valid)
        !! Returns `.true.` if given UUID in hex format is a valid UUID4.
        character(len=*), intent(in) :: uuid !! UUID to validate.

        integer :: i

        valid = .false.

        if (len_trim(uuid) /= UUID_LEN) return

        do i = 1, UUID_LEN
            select case (i)
                case (13)
                    if (uuid(i:i) /= '4') return
                case (17)
                    select case (uuid(i:i))
                        case ('8', '9', 'a', 'b')
                            continue
                        case default
                            return
                    end select
                case default
                    select case (uuid(i:i))
                        case ('0':'9', 'a':'f')
                            continue
                        case default
                            return
                    end select
            end select
        end do

        valid = .true.
    end function dm_uuid4_valid
end module dm_uuid
