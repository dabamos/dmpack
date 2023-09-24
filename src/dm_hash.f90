! Author:  Philipp Engel
! Licence: ISC
module dm_hash
    !! Hashing algorithms.
    use :: dm_kind
    use :: dm_platform
    implicit none (type, external)
    private

    integer(kind=i8), parameter :: DJB2_OFFSET = 5381_i8
    integer(kind=i8), parameter :: FNV1_OFFSET = int(z'811C9DC5', kind=i8)
    integer(kind=i8), parameter :: FNV1_PRIME  = int(z'01000193', kind=i8)

    public :: dm_hash_djb2
    public :: dm_hash_djb2a
    public :: dm_hash_fnv1
    public :: dm_hash_fnv1a
contains
    pure elemental integer(kind=i8) function dm_hash_djb2(str) result(hash)
        !! Dan Bernstein’s DJB2 non-cryptographic hash algorithm.
        character(len=*), intent(in) :: str !! Input string.
        integer                      :: i

        hash = DJB2_OFFSET

        do i = 1, len(str)
            hash = (ishftc(hash, 5) + hash) + iachar(str(i:i))
        end do
    end function dm_hash_djb2

    pure elemental integer(kind=i8) function dm_hash_djb2a(str) result(hash)
        !! Dan Bernstein’s DJB2a (XOR) non-cryptographic hash algorithm.
        character(len=*), intent(in) :: str !! Input string.
        integer                      :: i

        hash = DJB2_OFFSET

        do i = 1, len(str)
            hash = ieor((ishftc(hash, 5) + hash), iachar(str(i:i), kind=i8))
        end do
    end function dm_hash_djb2a

    pure elemental integer(kind=i8) function dm_hash_fnv1(str) result(hash)
        !! 32-bit Fowler–Noll–Vo hash function (FNV-1). Uses a 64-bit signed
        !! integer to store the unsigned 32-bit hash.
        character(len=*), intent(in) :: str !! Input string.

        integer          :: i, k
        integer(kind=i8) :: c

        hash = FNV1_OFFSET

        do i = 1, len(str)
            k = iachar(str(i:i))

            if (LITTLE_ENDIAN) then
                c = transfer([ k, 0 ], 0)
            else
                c = transfer([ 0, k ], 0)
            end if

            hash = hash * FNV1_PRIME
            hash = ieor(hash, c)
            hash = iand(hash, int(z'FFFFFFFF', kind=i8))
        end do
    end function dm_hash_fnv1

    pure elemental integer(kind=i8) function dm_hash_fnv1a(str) result(hash)
        !! 32-bit Fowler–Noll–Vo hash function (FNV-1a). Uses a 64-bit signed
        !! integer to store the unsigned 32-bit hash.
        !!
        !! Adapted from: http://www.isthe.com/chongo/tech/comp/fnv/fnv32.f
        character(len=*), intent(in) :: str !! Input string.

        integer          :: i, k
        integer(kind=i8) :: c

        hash = FNV1_OFFSET

        do i = 1, len(str)
            k = iachar(str(i:i))

            if (LITTLE_ENDIAN) then
                c = transfer([ k, 0 ], 0)
            else
                c = transfer([ 0, k ], 0)
            end if

            hash = ieor(hash, c)
            hash = hash * FNV1_PRIME
            hash = iand(hash, int(z'FFFFFFFF', kind=i8))
        end do
    end function dm_hash_fnv1a
end module dm_hash
