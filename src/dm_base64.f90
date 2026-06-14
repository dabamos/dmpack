! Author:  Philipp Engel
! Licence: ISC
module dm_base64
    !! Base64 encoding and decoding for poor people.
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PRIVATE PARAMETERS
    ! **************************************************************************
    character, parameter :: B64(0:63) = &
        transfer('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/', 'a', size=64)

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_base64_decode
    public :: dm_base64_encode
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    subroutine dm_base64_decode(input, output, error)
        !! Decodes given base64-encoded input string.
        !!
        !! Based on implementation by
        !! [cure honey](https://qiita.com/cure_honey/items/3a4cb6742364d3907cda).
        !!
        !! The routine returns `E_ALLOC` in optional argument `error` if the
        !! allocation of `output` failed.
        character(*),              intent(in)            :: input  !! Base64-encoded input string.
        character(:), allocatable, intent(out)           :: output !! Output string.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: i, j, l, n, stat
        integer :: m1, m2, m3, m4
        integer :: k1, k2, k3

        n = len(input)
        l = (n / 4) * 3

        if (input(n    :n    ) == '=') l = l - 1
        if (input(n - 1:n - 1) == '=') l = l - 1

        allocate (character(l) :: output, stat=stat)

        if (present(error)) then
            error = E_NONE
            if (stat /= 0) error = E_ALLOC
        end if

        if (stat /= 0) return

        j = 1

        do i = 1, n - 4, 4
            m1 = findloc(B64, input(i    :i    ), dim=1) - 1
            m2 = findloc(B64, input(i + 1:i + 1), dim=1) - 1
            m3 = findloc(B64, input(i + 2:i + 2), dim=1) - 1
            m4 = findloc(B64, input(i + 3:i + 3), dim=1) - 1

            k1 =        m1      *  4 + m2 / 16
            k2 = modulo(m2, 16) * 16 + m3 /  4
            k3 = modulo(m3,  4) * 64 + m4

            output(j    :j    ) = achar(k1)
            output(j + 1:j + 1) = achar(k2)
            output(j + 2:j + 2) = achar(k3)

            j = j + 3
        end do

        m1 = findloc(B64, input(i    :i    ), dim=1) - 1
        m2 = findloc(B64, input(i + 1:i + 1), dim=1) - 1
        k1 = m1 * 4 + m2 / 16
        output(j:j) = achar(k1)
        if (input(i + 2:i + 2) == '=') return

        m3 = findloc(B64, input(i + 2:i + 2), dim=1) - 1
        k2 = modulo(m2, 16) * 16 + m3 / 4
        output(j + 1:j + 1) = achar(k2)
        if (input(i + 3:i + 3) == '=') return

        m4 = findloc(B64, input(i + 3:i + 3), dim=1) - 1
        k3 = modulo(m3,  4) * 64 + m4
        output(j + 2:j + 2) = achar(k3)
    end subroutine dm_base64_decode

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
        character(*),              intent(in)            :: input  !! Input string.
        character(:), allocatable, intent(out)           :: output !! Base64-encoded output string.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: i, j, l, n, stat
        integer :: m1, m2, m3
        integer :: k1, k2, k3, k4

        j = 1
        n = len(input)
        l = ceiling(n / 3.0) * 4

        allocate (character(l) :: output, stat=stat)

        if (present(error)) then
            error = E_NONE
            if (stat /= 0) error = E_ALLOC
        end if

        if (stat /= 0) return

        do i = 1, (n / 3) * 3, 3
            m1 = ichar(input(i:i))
            m2 = ichar(input(i + 1:i + 1))
            m3 = ichar(input(i + 2:i + 2))
            k1 = m1 / 4
            k2 = m2 / 16 + modulo(m1,  4) * 16
            k3 = m3 / 64 + modulo(m2, 16) * 4
            k4 = modulo(m3, 64)
            output(j:j + 3) = B64(k1) // B64(k2) // B64(k3) // B64(k4)
            j = j + 4
        end do

        select case (modulo(n, 3))
            case (1)
                m1 = ichar(input(i:i))
                k1 = m1 / 4
                k2 = modulo(m1, 4) * 16
                output(j:j + 3) = B64(k1) // B64(k2) // '=='
            case (2)
                m1 = ichar(input(i:i))
                m2 = ichar(input(i + 1:i + 1))
                k1 = m1 / 4
                k2 = m2 / 16 + modulo(m1, 4) * 16
                k3 = modulo(m2, 16) * 4
                output(j:j + 3) = B64(k1) // B64(k2) // B64(k3) // '='
        end select
    end subroutine dm_base64_encode
end module dm_base64
