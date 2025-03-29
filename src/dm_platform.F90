! Author:  Philipp Engel
! Licence: ISC
module dm_platform
    !! System platform parameters.
    implicit none (type, external)
    private

    logical, parameter, public :: BIG_ENDIAN    = (iachar(transfer(1, mold='a')) == 0) !! Big Endian platform.
    logical, parameter, public :: LITTLE_ENDIAN = .not. BIG_ENDIAN                     !! Little Endian platform.

    integer, parameter, public :: PLATFORM_SYSTEM_UNKNOWN = 0 !! Unknown OS (invalid).
    integer, parameter, public :: PLATFORM_SYSTEM_FREEBSD = 1 !! FreeBSD.
    integer, parameter, public :: PLATFORM_SYSTEM_LINUX   = 2 !! Linux.
    integer, parameter, public :: PLATFORM_SYSTEM_LAST    = 2 !! Never use this.

#if defined (__FreeBSD__)
    character(len=*), parameter, public :: PLATFORM_SYSTEM_NAME = 'FreeBSD'
    integer,          parameter, public :: PLATFORM_SYSTEM      = PLATFORM_SYSTEM_FREEBSD
#elif defined (__linux__)
    character(len=*), parameter, public :: PLATFORM_SYSTEM_NAME = 'Linux'
    integer,          parameter, public :: PLATFORM_SYSTEM      = PLATFORM_SYSTEM_LINUX
#else
    character(len=*), parameter, public :: PLATFORM_SYSTEM_NAME = 'unknown'
    integer,          parameter, public :: PLATFORM_SYSTEM      = PLATFORM_SYSTEM_UNKNOWN
#endif
end module dm_platform
