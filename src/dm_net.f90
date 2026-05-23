! Author:  Philipp Engel
! Licence: ISC
module dm_net
    !! Networks.
    implicit none (type, external)
    private

    integer, parameter, public :: NET_IPV4_LEN = 15 !! IPv4 address length.
    integer, parameter, public :: NET_IPV6_LEN = 39 !! IPv6 address length.

    public :: dm_net_ipv4_is_valid
    public :: dm_net_ipv6_is_valid
contains
    pure elemental logical function dm_net_ipv4_is_valid(address) result(valid)
        !! Returns `.true.` if the argument is a valid IPv4 address.
        character(*), intent(in) :: address !! IPv4 address.

        character :: a
        integer   :: i, n, ndigits, ndots

        valid = .false.

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

        valid = .true.
    end function dm_net_ipv4_is_valid

    pure elemental logical function dm_net_ipv6_is_valid(address) result(valid)
        !! Returns `.true.` if the argument is a pure IPv6 address (no embedded
        !! IPv4 address!).
        character(*), intent(in) :: address !! IPv6 address.

        character :: a
        integer   :: i, j, n
        integer   :: ncolons, ndigits, ngroups
        logical   :: compressed

        valid = .false.

        n = len_trim(address)
        if (n < 2 .or. n > NET_IPV6_LEN) return

        ngroups    = 0
        ndigits    = 0
        ncolons    = 0
        compressed = .false.

        do i = 1, n
            a = address(i:i)

            select case (a)
                case ('0':'9', 'a':'f', 'A':'F')
                    ndigits = ndigits + 1
                    if (ndigits > 4) return

                case (':')
                    ncolons = ncolons + 1

                    if (i > 1) then
                        j = i - 1

                        if (address(j:j) == ':') then
                            ! "::"
                            if (compressed) return
                            compressed = .true.
                        else
                            ! End of a normal group.
                            if (ndigits > 0) ngroups = ngroups + 1
                        end if
                    end if

                    ndigits = 0

                case default
                    return
            end select
        end do

        ! Count final group.
        if (ndigits > 0) ngroups = ngroups + 1

        if (compressed) then
            ! "::" may replace one or more groups.
            valid = (ngroups <= 7)
        else
            valid = (ngroups == 8)
        end if
    end function dm_net_ipv6_is_valid
end module dm_net
