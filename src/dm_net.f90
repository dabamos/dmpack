! Author:  Philipp Engel
! Licence: ISC
module dm_net
    !! Networks.
    implicit none (type, external)
    private

    integer, parameter, public :: NET_IPV4_LEN = 15 !! IPv4 address length.
    integer, parameter, public :: NET_IPV6_LEN = 45 !! IPv6 address length.

    public :: dm_net_ipv4_is_valid
contains
    pure elemental logical function dm_net_ipv4_is_valid(address) result(is)
        !! Returns `.true.` if the argument is a valid IPv4 address.
        character(len=*), intent(in) :: address !! IPv4 address.

        character :: a
        integer   :: i, n, ndigits, ndots

        is = .false.

        n = len_trim(address)
        if (n < 7 .or. n > NET_IPV4_LEN) return

        ndigits = 0
        ndots   = 0

        do i = 1, n
            a = address(i:i)

            select case (a)
                case ('.')
                    if (ndigits < 1 .or. ndigits > 3) return
                    ndigits = 0
                    ndots   = ndots + 1
                    if (ndots > 3) return
                case ('0':'9')
                    ndigits = ndigits + 1
                    if (ndigits > 3) return
                case default
                    return
            end select
        end do

        is = .true.
    end function dm_net_ipv4_is_valid
end module dm_net
