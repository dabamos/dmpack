! Author:  Philipp Engel
! Licence: ISC
module dm_uuid
    !! Provides a UUIDv4 generator. DMPACK uses 32 characters long UUIDv4
    !! identifiers in hexadecimal format, without hyphens.
    implicit none (type, external)
    private

    integer,      parameter, public :: UUID_LEN      = 32                    !! Hex UUIDv4 length.
    integer,      parameter, public :: UUID_FULL_LEN = 36                    !! Full UUIDv4 length (with hyphens).
    character(*), parameter, public :: UUID_DEFAULT  = repeat('0', UUID_LEN) !! Default ID (hex).

    character(*), parameter :: UUID_SET = '0123456789abcdef'

    public :: dm_uuid4
    public :: dm_uuid4_hyphens
    public :: dm_uuid4_hyphenize
    public :: dm_uuid4_is_valid
contains
    impure elemental function dm_uuid4() result(uuid)
        !! Generates random UUIDv4 (RFC 4122) in hexadecimal format, i.e.,
        !! without hyphens (32 characters long). The PRNG has to be seeded
        !! before the first invocation by calling `dm_init()` once.
        character(UUID_LEN) :: uuid !! UUIDv4 string.

        integer :: i, j
        integer :: b(UUID_LEN)
        real    :: r(UUID_LEN)

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
        !! Returns UUIDv4 with hyphens (36 characters long). The PRNG has to be
        !! seeded before the first invocation by calling `dm_init()` once.
        character(UUID_FULL_LEN) :: uuid !! UUIDv4 string.

        uuid = dm_uuid4_hyphenize(dm_uuid4())
    end function dm_uuid4_hyphens

    pure elemental function dm_uuid4_hyphenize(uuid) result(full)
        !! Returns given UUID with hyphens, i.e., turns string
        !! `00000000000000000000000000000000` into
        !! `00000000-0000-0000-0000-000000000000`. The function does not
        !! validate the passed indentifier.
        character(UUID_LEN), intent(in) :: uuid !! UUIDv4 input string.
        character(UUID_FULL_LEN)        :: full !! UUIDv4 output string.

        integer :: stat

        write (full, '(a8, "-", 3(a4, "-"), a12)', iostat=stat) &
            uuid(1:8), uuid(9:12), uuid(13:16), uuid(17:20), uuid(21:32)
    end function dm_uuid4_hyphenize

    pure elemental logical function dm_uuid4_is_valid(uuid) result(valid)
        !! Returns `.true.` if given UUID in hex format is a valid UUIDv4. Only
        !! lower-case letters are valid.
        character(*), intent(in) :: uuid !! UUIDv4 to validate.

        character :: a
        integer   :: i

        valid = .false.

        if (len_trim(uuid) /= UUID_LEN) return

        do i = 1, UUID_LEN
            a = uuid(i:i)
            select case (i)
                case (13)
                    if (a /= '4') return
                case (17)
                    select case (a)
                        case ('8', '9', 'a', 'b'); continue
                        case default;              return
                    end select
                case default
                    select case (a)
                        case ('0':'9', 'a':'f'); continue
                        case default;            return
                    end select
            end select
        end do

        valid = .true.
    end function dm_uuid4_is_valid
end module dm_uuid
