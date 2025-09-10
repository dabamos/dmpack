! Author:  Philipp Engel
! Licence: ISC
module dm_base64
    !! Base64 encoding for poor people.
    use :: dm_ascii
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    public :: dm_base64_encode
contains
    pure subroutine dm_base64_encode(input, output, error)
        !! Encodes given input string in base64.
        !!
        !! Based on implementation by
        !! [cure honey](https://qiita.com/cure_honey/items/3a4cb6742364d3907cda)
        !! that is compatible to little endian and big endian alike (and quite
        !! fast, too!).
        !!
        !! The routine returns `E_ALLOC` in optional argument `error` if the
        !! allocation of `output` failed.
        character, parameter :: b64(0:63) = &
            transfer('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/', 'a', size=64)

        character(*),              intent(in)            :: input  !! Input string.
        character(:), allocatable, intent(out)           :: output !! Base64-encoded output string.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: i, j, l, n, stat
        integer :: m1, m2, m3
        integer :: k1, k2, k3, k4

        j = 1
        n = len(input)
        l = ceiling(n / 3.0) * 4

        if (present(error)) error = E_ALLOC
        allocate (character(l) :: output, stat=stat)
        if (stat /= 0) return

        do i = 1, (n / 3) * 3, 3
            m1 = iachar(input(i:i))
            m2 = iachar(input(i + 1:i + 1))
            m3 = iachar(input(i + 2:i + 2))
            k1 = m1 / 4
            k2 = m2 / 16 + modulo(m1, 4) * 16
            k3 = m3 / 64 + modulo(m2, 16) * 4
            k4 = modulo(m3, 64)
            output(j:j + 3) = b64(k1) // b64(k2) // b64(k3) // b64(k4)
            j = j + 4
        end do

        select case (modulo(n, 3))
            case (1)
                m1 = iachar(input(i:i))
                k1 = m1 / 4
                k2 = modulo(m1, 4) * 16
                output(j:j + 3) = b64(k1) // b64(k2) // '=='
            case (2)
                m1 = iachar(input(i:i))
                m2 = iachar(input(i + 1:i + 1))
                k1 = m1 / 4
                k2 = m2 / 16 + modulo(m1, 4) * 16
                k3 = modulo(m2, 16) * 4
                output(j:j + 3) = b64(k1) // b64(k2) // b64(k3) // '='
        end select

        if (present(error)) error = E_NONE
    end subroutine dm_base64_encode
end module dm_base64
