! Author:  Philipp Engel
! Licence: ISC
module dm_hash
    !! Non-cryptographic hash functions (DJB2, DJB2a, FNV-1, FNV-1a).
    use :: dm_kind
    use :: dm_platform
    implicit none (type, external)
    private

    integer(i8), parameter :: DJB2_OFFSET = 5381_i8
    integer(i8), parameter :: FNV1_OFFSET = int(z'811C9DC5', i8)
    integer(i8), parameter :: FNV1_PRIME  = int(z'01000193', i8)

    public :: dm_hash_djb2
    public :: dm_hash_djb2a
    public :: dm_hash_fnv1
    public :: dm_hash_fnv1a
contains
    pure elemental integer(i8) function dm_hash_djb2(input) result(hash)
        !! Dan Bernstein’s DJB2 non-cryptographic hash algorithm.
        character(*), intent(in) :: input !! Input string.

        integer :: i

        hash = DJB2_OFFSET

        do i = 1, len(input)
            hash = (ishftc(hash, 5) + hash) + iachar(input(i:i))
        end do
    end function dm_hash_djb2

    pure elemental integer(i8) function dm_hash_djb2a(input) result(hash)
        !! Dan Bernstein’s DJB2a (XOR) non-cryptographic hash algorithm.
        character(*), intent(in) :: input !! Input string.

        integer :: i

        hash = DJB2_OFFSET

        do i = 1, len(input)
            hash = ieor((ishftc(hash, 5) + hash), iachar(input(i:i), i8))
        end do
    end function dm_hash_djb2a

    pure elemental integer(i8) function dm_hash_fnv1(input) result(hash)
        !! 32-bit Fowler–Noll–Vo hash function (FNV-1). Uses a 64-bit signed
        !! integer to store the unsigned 32-bit hash.
        character(*), intent(in) :: input !! Input string.

        integer     :: i, k
        integer(i8) :: c

        hash = FNV1_OFFSET

        do i = 1, len(input)
            k = iachar(input(i:i))

            if (LITTLE_ENDIAN) then
                c = transfer([ k, 0 ], 0)
            else
                c = transfer([ 0, k ], 0)
            end if

            hash = hash * FNV1_PRIME
            hash = ieor(hash, c)
            hash = iand(hash, int(z'FFFFFFFF', i8))
        end do
    end function dm_hash_fnv1

    pure elemental integer(i8) function dm_hash_fnv1a(input) result(hash)
        !! 32-bit Fowler–Noll–Vo hash function (FNV-1a). Uses a 64-bit signed
        !! integer to store the unsigned 32-bit hash.
        !!
        !! Adapted from: http://www.isthe.com/chongo/tech/comp/fnv/fnv32.f
        character(*), intent(in) :: input !! Input string.

        integer     :: i, k
        integer(i8) :: c

        hash = FNV1_OFFSET

        do i = 1, len(input)
            k = iachar(input(i:i))

            if (LITTLE_ENDIAN) then
                c = transfer([ k, 0 ], 0)
            else
                c = transfer([ 0, k ], 0)
            end if

            hash = ieor(hash, c)
            hash = hash * FNV1_PRIME
            hash = iand(hash, int(z'FFFFFFFF', i8))
        end do
    end function dm_hash_fnv1a
end module dm_hash
