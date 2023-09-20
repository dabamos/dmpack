! Author:  Philipp Engel
! Licence: ISC
module dm_platform
    !! System platform parameters.
    implicit none (type, external)
    private

    logical, parameter, public :: BIG_ENDIAN    = (iachar(transfer(1, mold='a')) == 0) !! Big Endian platform.
    logical, parameter, public :: LITTLE_ENDIAN = .not. BIG_ENDIAN                     !! Little Endian platform.
end module dm_platform
