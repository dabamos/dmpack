! Author:  Philipp Engel
! Licence: ISC
module dm_inet
    !! TCP/IP parameters.
    implicit none (type, external)
    private

    integer, parameter, public :: INET_IPV4_LEN = 15 !! IPv4 address length.
    integer, parameter, public :: INET_IPV6_LEN = 45 !! IPv6 address length.
end module dm_inet
