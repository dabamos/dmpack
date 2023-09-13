! Author:  Philipp Engel
! Licence: ISC
module dm_hash
    !! Hashing algorithms
    use :: dm_kind
    implicit none (type, external)
    private

    public :: dm_hash_fnv1a
contains
    pure elemental integer(kind=i8) function dm_hash_fnv1a(str) result(hash)
        !! 32-bit Fowler–Noll–Vo hash function (FNV-1a). Uses a 64-bit signed
        !! integer to store the unsigned 32-bit hash. For little endian
        !! platforms only!
        !!
        !! Adapted from: http://www.isthe.com/chongo/tech/comp/fnv/fnv32.f
        integer(kind=i8), parameter :: OFFSET = int(z'811C9DC5', kind=i8)
        integer(kind=i8), parameter :: PRIME  = int(z'01000193', kind=i8)

        character(len=*), intent(in) :: str !! Input string.

        integer          :: i
        integer(kind=i8) :: c

        hash = OFFSET

        do i = 1, len(str)
            c    = transfer([ iachar(str(i:i)), 0 ], 0)
            hash = ieor(hash, c)
            hash = hash * PRIME
            hash = iand(hash, int(z'FFFFFFFF', kind=i8))
        end do
    end function dm_hash_fnv1a
end module dm_hash
