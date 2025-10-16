! Author:  Philipp Engel
! Licence: ISC
module dm_filter
    !! Filter module, based on FORTRAN 77 procedures by David Harris (1990).
    use :: dm_const
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter :: FILTER_ROOT_TYPE_SP  = 1 !! Single real pole.
    integer, parameter :: FILTER_ROOT_TYPE_CP  = 2 !! Complex conjugate pole pair.
    integer, parameter :: FILTER_ROOT_TYPE_CPZ = 3 !! Complex conjugate pole and zero pairs.

    public :: dm_filter_low_pass

    private :: filter_apply
    private :: filter_bilinear
    private :: filter_butterworth
    private :: filter_cutoff
    private :: filter_design_low_pass
    private :: filter_low_pass
    private :: filter_warp
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    pure subroutine dm_filter_low_pass(data, ord, cutoff, ts, passes)
        real(r8), intent(inout) :: data(:) !! Sequence to be filtered.
        integer,  intent(in)    :: ord     !! Order (#poles), not to exceed 10.
        real(r8), intent(in)    :: cutoff  !! High-frequency cutoff of filter [Hz].
        real(r8), intent(in)    :: ts      !! Sampling interval [sec].
        logical,  intent(in)    :: passes  !! Forward filtering only if true, else forward and reverse (zero phase).

        integer  :: nsects
        real(r8) :: sn(30), sd(30)

        call filter_design_low_pass(ord, cutoff, ts, sn, sd, nsects)
        call filter_apply(data, passes, sn, sd, nsects)
    end subroutine dm_filter_low_pass

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    pure subroutine filter_apply(data, zp, sn, sd, nsects)
        !! Subroutine to apply an filter to a data sequence. The filter is
        !! assumed to be stored as second order sections. Filtering is
        !! in-place.
        real(r8), intent(inout) :: data(:) !! Data array.
        logical,  intent(in)    :: zp      !! True for zero phase filtering, false for single pass filtering.
        real(r8), intent(inout) :: sn(:)   !! Numerator polynomials for second order sections.
        real(r8), intent(inout) :: sd(:)   !! Denominator polynomials for second order sections.
        integer,  intent(in)    :: nsects  !! Number of second-order sections.

        integer  :: i, j, k, nsamps
        real(r8) :: a1, a2, b0, b1, b2
        real(r8) :: x1, x2, y1, y2
        real(r8) :: output

        nsamps = size(data)

        k = 1

        do  j = 1, nsects
            x1 = 0.0_r8
            x2 = 0.0_r8
            y1 = 0.0_r8
            y2 = 0.0_r8
            b0 = sn(k)
            b1 = sn(k + 1)
            b2 = sn(k + 2)
            a1 = sd(k + 1)
            a2 = sd(k + 2)

            do i = 1, nsamps
                output = b0 * data(i) + b1 * x1 + b2 * x2
                output = output - (a1 * y1 + a2 * y2)
                y2 = y1
                y1 = output
                x2 = x1
                x1 = data(i)
                data(i) = output
            end do

            k = k + 3
        end do

        if (zp) then
            k = 1

            do j = 1, nsects
                x1 = 0.0
                x2 = 0.0
                y1 = 0.0
                y2 = 0.0
                b0 = sn(k)
                b1 = sn(k + 1)
                b2 = sn(k + 2)
                a1 = sd(k + 1)
                a2 = sd(k + 2)

                do i = nsamps, 1, -1
                    output = b0 * data(i) + b1 * x1 + b2 * x2
                    output = output - (a1 * y1 + a2 * y2)
                    y2 = y1
                    y1 = output
                    x2 = x1
                    x1 = data(i)
                    data(i) = output
                end do

                k = k + 3
            end do
        end if
    end subroutine filter_apply

    pure subroutine filter_bilinear(sn, sd, nsects)
        !! Transforms an analog filter to a digital filter via the bilinear
        !! transformation. Assumes both are stored as second order sections.
        !! The transformation is done in-place.
        real(r8), intent(inout) :: sn(:)  !! Numerator coefficients or second-order sections packed head-to-tail.
        real(r8), intent(inout) :: sd(:)  !! Denominator coefficients of second-order sections packed head-to-tail.
        integer,  intent(in)    :: nsects !! Number of second order sections.

        integer  :: i, k
        real(r8) :: a0, a1, a2, s

        k = 1

        do i = 1, nsects
            a0 = sd(k)
            a1 = sd(k + 1)
            a2 = sd(k + 2)

            s = a2 + a1 + a0
            sd(k)   = 1.0_r8
            sd(k + 1) = (2 * (a0 - a2)) / s
            sd(k + 2) = (a2 - a1 + a0) / s

            a0 = sn(k)
            a1 = sn(k + 1)
            a2 = sn(k + 2)

            sn(k)     = (a2 + a1 + a0) / s
            sn(k + 1) = (2 * (a0 - a2)) / s
            sn(k + 2) = (a2 - a1 + a0) / s

            k = k + 3
        end do
    end subroutine filter_bilinear

    pure subroutine filter_butterworth(ord, p, rtype, dc, nsects)
        !! Subroutine to compute Butterworth poles for normalised low pass
        !! filter. Output argument `p` is a complex array containing poles:
        !! only one from each complex conjugate pair, and all real poles.
        !!
        !! Originally written by David Harris (September 7, 1990).
        integer,      intent(in)    :: ord      !! Desired filter order.
        complex(r8),  intent(inout) :: p(:)     !! Poles.
        integer,      intent(inout) :: rtype(:) !! Root type (`FILTER_ROOT_TYPE_*`).
        real(r8),     intent(out)   :: dc       !! Magnitude of filter at zero frequency.
        integer,      intent(out)   :: nsects   !! Number of second order sections.

        integer  :: half, k
        real(r8) :: angle

        half = ord / 2

        ! test for odd order, and add pole at -1
        nsects = 0

        if (2 * half < ord) then
            p(1) = cmplx(-1.0_r8, 0.0_r8, kind=r8)
            rtype(1) = FILTER_ROOT_TYPE_SP
            nsects = 1
        end if

        do k = 1, half
            angle = PI * (0.5 + real(2 * k - 1) / real(2 * ord))
            nsects = nsects + 1
            p(nsects) = cmplx(cos(angle), sin(angle), kind=r8)
            rtype(nsects) = FILTER_ROOT_TYPE_CP
        end do

        dc = 1.0_r8
    end subroutine filter_butterworth

    pure subroutine filter_cutoff(f, sn, sd, nsects)
        !! Subroutine to alter the cutoff of a filter. Assumes that the filter
        !! is structured as second order sections. Changes the cutoff of
        !! normalised low pass and highpass filters through a simple polynomial
        !! transformation.
        real(r8), intent(in)    :: f      !! New cutoff frequency.
        real(r8), intent(inout) :: sn(:)  !! Numerator polynomials for second order sections.
        real(r8), intent(inout) :: sd(:)  !! Denominator polynomials for second order sections.
        integer,  intent(inout) :: nsects !! Number of second order sections.

        integer  :: i, k
        real(r8) :: s ! scale

        s = PI2 * f
        k = 1

        do i = 1, nsects
            sn(k + 1) = sn(k + 1) / s
            sn(k + 2) = sn(k + 2) / (s * s)
            sd(k + 1) = sd(k + 1) / s
            sd(k + 2) = sd(k + 2) / (s * s)
            k = k + 3
        end do
    end subroutine filter_cutoff

    pure subroutine filter_design_low_pass(ord, cutoff, ts, sn, sd, nsects)
        !! Subroutine to design low-pass filter from analog prototypes.
        integer,  intent(in)    :: ord    !! Filter order.
        real(r8), intent(in)    :: cutoff !! High-frequency cut-off.
        real(r8), intent(in)    :: ts     !! Sampling interval [sec].
        real(r8), intent(inout) :: sn(:)  !! Numerator coefficients or second-order sections packed head-to-tail.
        real(r8), intent(inout) :: sd(:)  !! Denominator coefficients of second-order sections packed head-to-tail.
        integer,  intent(out)   :: nsects !! Number of second-order sections.

        complex(r8) :: p(10), z(10)
        real(r8)    :: dc, fhw
        integer     :: rtype(10)

        call filter_butterworth(ord, p, rtype, dc, nsects)

        ! Analog mapping.
        fhw = filter_warp(cutoff * ts / 2.0_r8, 2.0_r8)
        call filter_low_pass(p, z, rtype, dc, nsects, sn, sd)
        call filter_cutoff(fhw, sn, sd, nsects)

        ! Bilinear analog to digital transformation.
        call filter_bilinear(sn, sd, nsects)
    end subroutine filter_design_low_pass

    pure subroutine filter_low_pass(p, z, rtype, dc, nsects, sn, sd)
        !! Subroutine to generate second order section parameterization from an
        !! pole-zero description for low pass filters.
        !!
        !! Originally written by Dave Harris.
        complex(r8),  intent(inout) :: p(:)     !! Poles.
        complex(r8),  intent(inout) :: z(:)     !! Zeroes.
        integer,      intent(inout) :: rtype(:) !! Root type (`FILTER_ROOT_TYPE_*`).
        real(r8),     intent(in)    :: dc       !! Zero-frequency value of prototype filter.
        integer,      intent(in)    :: nsects   !! Number of second-order sections.
        real(r8),     intent(inout) :: sn(:)    !! Numerator polynomials for second order sections.
        real(r8),     intent(inout) :: sd(:)    !! Denominator polynomials for second order sections.

        integer  :: i, k
        real(r8) :: s ! scale

        k = 1

        do i = 1, nsects
            select case (rtype(i))
                case (FILTER_ROOT_TYPE_SP)
                    s = -1 * real(p(i))
                    sn(k)     = s
                    sn(k + 1) = 0.0_r8
                    sn(k + 2) = 0.0_r8
                    sd(k)     = -1.0_r8 * dble(p(i))
                    sd(k + 1) = 1.0_r8
                    sd(k + 2) = 0.0_r8
                    k = k + 3
                case (FILTER_ROOT_TYPE_CP)
                    s = dble(p(i) * conjg(p(i)))
                    sn(k)     = s
                    sn(k + 1) = 0.0_r8
                    sn(k + 2) = 0.0_r8
                    sd(k)     = dble(p(i) * conjg(p(i)))
                    sd(k + 1) = -2.0_r8 * dble(p(i))
                    sd(k + 2) = 1.0_r8
                    k = k + 3
                case (FILTER_ROOT_TYPE_CPZ)
                    s = dble(p(i) * conjg(p(i))) / dble(z(i) * conjg( z(i)))
                    sn(k)     = dble(z(i) * conjg(z(i))) * s
                    sn(k + 1) = -2.0_r8 * dble(z(i)) * s
                    sn(k + 2) = 1.0_r8 * s
                    sd(k)     = dble(p(i) * conjg( p(i)))
                    sd(k + 1) = -2.0_r8 * dble(p(i))
                    sd(k + 2) = 1.0_r8
                    k = k + 3
            end select
        end do

        sn(1) = dc * sn(1)
        sn(2) = dc * sn(2)
        sn(3) = dc * sn(3)
    end subroutine filter_low_pass

    pure elemental real(r8) function filter_warp(f, ts) result(warp)
        !! Function, applies tangent frequency warping to compensate for
        !! bilinear analog to digital transformation.
        real(r8), intent(in) :: f  !! Original design frequency specification [Hz].
        real(r8), intent(in) :: ts !! Sampling interval [sec].

        real(r8) :: angle

        angle = PI2 * f * ts / 2.0_r8
        warp  = 2.0_r8 * tan(angle) / ts
        warp  = warp / PI2
    end function filter_warp
end module dm_filter
