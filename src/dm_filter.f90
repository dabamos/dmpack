! Author:  Philipp Engel
! Licence: ISC
module dm_filter
    !! Filter module, based on FORTRAN 77 procedures by David Harris:
    !!
    !! * Harris, D. (1990): XAPiir. A Recursive Digital Filtering Package.
    !!   Technical Report. Lawrence Livermore National Laboratory.
    !!   DOI: doi.org/10.2172/6416972
    !!
    use :: dm_const
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter :: FILTER_PROTOTYPE_BUTTERWORTH = 1 !! Butterworth.
    integer, parameter :: FILTER_PROTOTYPE_BESSEL      = 2 !! Bessel.
    integer, parameter :: FILTER_PROTOTYPE_CHEBYSHEV1  = 3 !! Chebyshev Type I.
    integer, parameter :: FILTER_PROTOTYPE_CHEBYSHEV2  = 4 !! Chebyshev Type II.

    integer, parameter :: FILTER_ROOT_SP  = 1 !! Single real pole.
    integer, parameter :: FILTER_ROOT_CP  = 2 !! Complex conjugate pole pair.
    integer, parameter :: FILTER_ROOT_CPZ = 3 !! Complex conjugate pole and zero pairs.

    integer, parameter :: FILTER_TYPE_LP = 1 !! Low-pass.
    integer, parameter :: FILTER_TYPE_HP = 2 !! High-pass.
    integer, parameter :: FILTER_TYPE_BP = 3 !! Band-pass.
    integer, parameter :: FILTER_TYPE_BR = 4 !! Band-reject.

    public :: dm_filter_low_pass

    private :: filter_bilinear
    private :: filter_cutoff
    private :: filter_design
    private :: filter_filter
    private :: filter_low_pass
    private :: filter_low_pass_to_band_pass
    private :: filter_low_pass_to_band_reject
    private :: filter_low_pass_to_high_pass
    private :: filter_roots_bessel
    private :: filter_roots_butterworth
    private :: filter_roots_chebyshev1
    private :: filter_roots_chebyshev2
    private :: filter_roots_chebyshev_parameters
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
        logical,  intent(in)    :: passes  !! Forward filtering only if `.true.`, else forward and reverse (zero phase).

        integer  :: nsects
        real(r8) :: a, trans
        real(r8) :: sn(30), sd(30)

        call filter_design(ord, FILTER_TYPE_LP, FILTER_PROTOTYPE_BUTTERWORTH, a, trans, 0.0_r8, cutoff, ts, sn, sd, nsects)
        call filter_filter(data, passes, sn, sd, nsects)
    end subroutine dm_filter_low_pass

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    pure subroutine filter_bilinear(sn, sd, nsects)
        !! Transforms an analog filter to a digital filter via the bilinear
        !! transformation. Assumes both are stored as second order sections.
        !! The transformation is done in-place.
        real(r8), intent(inout) :: sn(:)  !! Numerator polynomial coefficients for second order sections. Packed head-to-tail.
        real(r8), intent(inout) :: sd(:)  !! Denominator polynomial coefficients for second order sections. Packed head-to-tail.
        integer,  intent(in)    :: nsects !! Number of second order sections.

        integer  :: i, k
        real(r8) :: a0, a1, a2, s

        k = 1

        do i = 1, nsects
            a0 = sd(k)
            a1 = sd(k + 1)
            a2 = sd(k + 2)

            s = a2 + a1 + a0
            sd(k)     = 1.0
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

    pure subroutine filter_cutoff(sn, sd, nsects, f)
        !! Subroutine to alter the cut-off of a filter. Assumes that the filter
        !! is structured as second order sections. Changes the cut-off of
        !! normalised low-pass and high-pass filters through a simple polynomial
        !! transformation.
        real(r8), intent(inout) :: sn(:)  !! Numerator polynomials for second order sections.
        real(r8), intent(inout) :: sd(:)  !! Denominator polynomials for second order sections.
        integer,  intent(inout) :: nsects !! Number of second order sections.
        real(r8), intent(in)    :: f      !! New cut-off frequency.

        integer  :: i, k
        real(r8) :: scale

        scale = PI2 * f
        k = 1

        do i = 1, nsects
            sn(k + 1) = sn(k + 1) / scale
            sn(k + 2) = sn(k + 2) / scale**2
            sd(k + 1) = sd(k + 1) / scale
            sd(k + 2) = sd(k + 2) / scale**2
            k = k + 3
        end do
    end subroutine filter_cutoff

    pure subroutine filter_design(ord, type, prototype, a, trans, fl, fh, ts, sn, sd, nsects)
        !! Subroutine to design IIR digital filters from analog prototypes.
        integer,  intent(in)    :: ord       !! Filter order.
        integer,  intent(in)    :: type      !! Filter type enumerator (`FILTER_TYPE_*`).
        integer,  intent(in)    :: prototype !! Analog prototype enumerator (`FILTER_PROTOTYPE_*`).
        real(r8), intent(in)    :: a         !! Chebyshev stopband attenuation factor.
        real(r8), intent(in)    :: trans     !! Chebyshev transition bandwidth (fraction of low-pass prototype pass-band width).
        real(r8), intent(in)    :: fl        !! Low-frequency cut-off.
        real(r8), intent(in)    :: fh        !! High-frequency cut-off.
        real(r8), intent(in)    :: ts        !! Sampling interval [sec].
        real(r8), intent(inout) :: sn(:)     !! Numerator coefficients or second-order sections packed head-to-tail.
        real(r8), intent(inout) :: sd(:)     !! Denominator coefficients of second-order sections packed head-to-tail.
        integer,  intent(out)   :: nsects    !! Number of 2nd order sections.

        complex(r16) :: poles(10), zeros(10)
        real(r8)     :: dc, eps, flw, fhw, omegar, ripple
        integer      :: root_type(10)

        ! Analog prototype selection.
        select case (prototype)
            case (FILTER_PROTOTYPE_BUTTERWORTH)
                call filter_roots_butterworth(poles, root_type, dc, nsects, ord)

            case (FILTER_PROTOTYPE_BESSEL)
                call filter_roots_bessel(poles, root_type, dc, nsects, ord)

            case (FILTER_PROTOTYPE_CHEBYSHEV1)
                call filter_roots_chebyshev_parameters(a, trans, ord, eps, ripple)
                call filter_roots_chebyshev1(poles, root_type, dc, nsects, ord, eps)

            case (FILTER_PROTOTYPE_CHEBYSHEV2)
                omegar = 1.0 + trans
                call filter_roots_chebyshev2(poles, zeros, root_type, dc, nsects, ord, a, omegar)
        end select

        ! Analog mapping selection.
        select case (type)
            case (FILTER_TYPE_LP)
                fhw = filter_warp(fh * ts / 2.0, 2.0_r8)
                call filter_low_pass(poles, zeros, root_type, dc, nsects, sn, sd)
                call filter_cutoff(sn, sd, nsects, fhw)

            case (FILTER_TYPE_HP)
                flw = filter_warp(fl * ts / 2.0, 2.0_r8)
                call filter_low_pass_to_high_pass(poles, zeros, root_type, dc, nsects, sn, sd)
                call filter_cutoff(sn, sd, nsects, flw)

            case (FILTER_TYPE_BP)
                flw = filter_warp(fl * ts / 2.0, 2.0_r8)
                fhw = filter_warp(fh * ts / 2.0, 2.0_r8)
                call filter_low_pass_to_band_pass(poles, zeros, root_type, dc, nsects, flw, fhw, sn, sd)

            case (FILTER_TYPE_BR)
                flw = filter_warp(fl * ts / 2.0, 2.0_r8)
                fhw = filter_warp(fh * ts / 2.0, 2.0_r8)
                call filter_low_pass_to_band_reject(poles, zeros, root_type, dc, nsects, flw, fhw, sn, sd)
        end select

        ! Bilinear analog to digital transformation.
        call filter_bilinear(sn, sd, nsects)
    end subroutine filter_design

    pure subroutine filter_filter(data, zp, sn, sd, nsects)
        !! Subroutine to apply an filter to a data sequence. The filter is
        !! assumed to be stored as second order sections. Filtering is
        !! in-place. Zero-phase (forward and reverse) is an option.
        real(r8), intent(inout) :: data(:) !! Data array.
        logical,  intent(in)    :: zp      !! Zero phase filtering if `.true.`, else single pass filtering.
        real(r8), intent(inout) :: sn(:)   !! Numerator polynomials for second order sections.
        real(r8), intent(inout) :: sd(:)   !! Denominator polynomials for second order sections.
        integer,  intent(in)    :: nsects  !! Number of 2nd order sections.

        integer  :: i, j, k, nsamps
        real(r8) :: a1, a2, b0, b1, b2
        real(r8) :: x1, x2, y1, y2
        real(r8) :: output

        nsamps = size(data)

        k = 1

        do  j = 1, nsects
            x1 = 0.0
            x2 = 0.0
            y1 = 0.0
            y2 = 0.0
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
    end subroutine filter_filter

    pure subroutine filter_low_pass(poles, zeros, root_type, dc, nsects, sn, sd)
        !! Subroutine to generate second order section parameterisation from an
        !! pole-zero description for low pass filters.
        complex(r16), intent(inout) :: poles(:)     !! Poles.
        complex(r16), intent(inout) :: zeros(:)     !! Zeroes.
        integer,      intent(inout) :: root_type(:) !! Root type (`FILTER_ROOT_*`).
        real(r8),     intent(in)    :: dc           !! Zero-frequency value of prototype filter.
        integer,      intent(in)    :: nsects       !! Number of 2nd order sections.
        real(r8),     intent(inout) :: sn(:)        !! Numerator polynomials for second order sections.
        real(r8),     intent(inout) :: sd(:)        !! Denominator polynomials for second order sections.

        integer  :: i, k
        real(r8) :: scale

        k = 1

        do i = 1, nsects
            select case (root_type(i))
                case (FILTER_ROOT_SP)
                    scale     = -1 * dble(poles(i))
                    sn(k)     = scale
                    sn(k + 1) = 0.0
                    sn(k + 2) = 0.0
                    sd(k)     = -1.0 * dble(poles(i))
                    sd(k + 1) = 1.0
                    sd(k + 2) = 0.0
                    k = k + 3
                case (FILTER_ROOT_CP)
                    scale     = dble(poles(i) * conjg(poles(i)))
                    sn(k)     = scale
                    sn(k + 1) = 0.0
                    sn(k + 2) = 0.0
                    sd(k)     = dble(poles(i) * conjg(poles(i)))
                    sd(k + 1) = -2.0 * dble(poles(i))
                    sd(k + 2) = 1.0
                    k = k + 3
                case (FILTER_ROOT_CPZ)
                    scale     = dble(poles(i) * conjg(poles(i))) / dble(zeros(i) * conjg(zeros(i)))
                    sn(k)     = dble(zeros(i) * conjg(zeros(i))) * scale
                    sn(k + 1) = -2.0 * dble(zeros(i)) * scale
                    sn(k + 2) = 1.0 * scale
                    sd(k)     = dble(poles(i) * conjg(poles(i)))
                    sd(k + 1) = -2.0 * dble(poles(i))
                    sd(k + 2) = 1.0
                    k = k + 3
            end select
        end do

        sn(1) = dc * sn(1)
        sn(2) = dc * sn(2)
        sn(3) = dc * sn(3)
    end subroutine filter_low_pass

    pure subroutine filter_low_pass_to_band_pass(poles, zeros, root_type, dc, nsects, fl, fh, sn, sd)
        !! Subroutine to convert an prototype low-pass filter to a band-pass
        !! filter via the analog polynomial transformation. The low-pass filter
        !! is described in terms of its poles and zeros (as input to this
        !! routine). The output consists of the parameters for second order
        !! sections.
        complex(r16), intent(inout) :: poles(:)     !! Array containing poles.
        complex(r16), intent(inout) :: zeros(:)     !! Array containing zeros.
        integer,      intent(inout) :: root_type(:) !! Root type (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc           !! Zero-frequency value of filter.
        integer,      intent(inout) :: nsects       !! Number of 2nd order sections upon input/output.
        real(r8),     intent(out)   :: fl           !! Low-frequency cut-off.
        real(r8),     intent(out)   :: fh           !! High-frequency cut-off.
        real(r8),     intent(inout) :: sn(:)        !! Numerator polynomials for second order sections.
        real(r8),     intent(inout) :: sd(:)        !! Denominator polynomials for second order sections.

        complex(r16) :: c, p1, p2, z1, z2, s, h
        real(r8)     :: a, b, scale
        integer      :: i, k, n

        a = PI2**2 * fl * fh
        b = PI2 * (fh - fl)

        n = nsects
        nsects = 0
        k = 1

        do i = 1, n
            select case (root_type(i))
                case (FILTER_ROOT_SP)
                    sn(k)     = 0.0
                    sn(k + 1) = b
                    sn(k + 2) = 0.0
                    sd(k)     = a
                    sd(k + 1) = -b * dble(poles(i))
                    sd(k + 2) = 1.0
                    k = k + 3
                    nsects = nsects + 1

                case (FILTER_ROOT_CP)
                    c = (b * poles(i))**2 - 4.0 * a
                    c = sqrt(c)
                    p1 = 0.5 * (b * poles(i) + c)
                    p2 = 0.5 * (b * poles(i) - c)
                    sn(k)     = 0.0
                    sn(k + 1) = b
                    sn(k + 2) = 0.0
                    sd(k)     = dble(p1 * conjg(p1))
                    sd(k + 1) = -2.0 * dble(p1)
                    sd(k + 2) = 1.0
                    k = k + 3
                    sn(k)     = 0.0
                    sn(k + 1) = b
                    sn(k + 2) = 0.0
                    sd(k)     = dble(p2 * conjg(p2))
                    sd(k + 1) = -2.0 * dble(p2)
                    sd(k + 2) = 1.0
                    k = k + 3
                    nsects = nsects + 2

                case (FILTER_ROOT_CPZ)
                    c = (b * zeros(i))**2 - 4.0 * a
                    c = sqrt(c)
                    z1 = 0.5 * (b * zeros(i) + c)
                    z2 = 0.5 * (b * zeros(i) - c)
                    c = (b * poles(i))**2 - 4.0 * a
                    c = sqrt(c)
                    p1 = 0.5 * (b * poles(i) + c)
                    p2 = 0.5 * (b * poles(i) - c)
                    sn(k)     = dble(z1 * conjg(z1))
                    sn(k + 1) = -2. * dble(z1)
                    sn(k + 2) = 1.0
                    sd(k)     = dble(p1 * conjg(p1))
                    sd(k + 1) = -2. * dble(p1)
                    sd(k + 2) = 1.0
                    k = k + 3
                    sn(k)     = dble(z2 * conjg(z2))
                    sn(k + 1) = -2.0 * dble(z2)
                    sn(k + 2) = 1.0
                    sd(k)     = dble(p2 * conjg(p2))
                    sd(k + 1) = -2.0 * dble(p2)
                    sd(k + 2) = 1.0
                    k = k + 3
                    nsects = nsects + 2
            end select
        end do

        s = cmplx(0.0_r8, sqrt(a), r8)
        h = cmplx(1.0_r8, 0.0_r8,  r8)
        k = 1

        do i = 1, nsects
            h = h * ((sn(k + 2) * s + sn(k + 1)) * s + sn(k)) / ((sd(k + 2) * s + sd(k + 1)) * s + sd(k))
            k = k + 3
        end do

        scale = dble(dc / sqrt(dble(h) * conjg(h)))

        sn(1) = sn(1) * scale
        sn(2) = sn(2) * scale
        sn(3) = sn(3) * scale
    end subroutine filter_low_pass_to_band_pass

    pure subroutine filter_low_pass_to_high_pass(poles, zeros, root_type, dc, nsects, sn, sd)
        !! Subroutine to convert a low-pass filter to a high-pass filter via an
        !! analog polynomial transformation. The low-pass filter is described
        !! in terms of its poles and zeroes (as input to this routine). The
        !! output consists of the parameters for second order sections.
        complex(r16), intent(inout) :: poles(:)     !! Array containing poles.
        complex(r16), intent(inout) :: zeros(:)     !! Array containing zeros.
        integer,      intent(inout) :: root_type(:) !! Root type (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc           !! Zero-frequency value of filter.
        integer,      intent(inout) :: nsects       !! Number of 2nd order sections upon input/output.
        real(r8),     intent(inout) :: sn(:)        !! Numerator polynomials for second order sections.
        real(r8),     intent(inout) :: sd(:)        !! Denominator polynomials for second order sections.

        real(r8) :: scale
        integer  :: i, k

        k = 1

        do i = 1, nsects
            select case (root_type(i))
                case (FILTER_ROOT_SP)
                    scale     = -dble(poles(i))
                    sn(k)     = 0.0
                    sn(k + 1) = scale
                    sn(k + 2) = 0.0
                    sd(k)     = 1.0
                    sd(k + 1) = -dble(poles(i))
                    sd(k + 2) = 0.0
                    k = k + 3

                case (FILTER_ROOT_CPZ)
                    scale     = dble(poles(i) * conjg(poles(i))) / dble(zeros(i) * conjg(zeros(i)))
                    sn(k)     = 1.0 * scale
                    sn(k + 1) = -2.0 * dble(zeros(i))  *  scale
                    sn(k + 2) = dble(zeros(i) * conjg(zeros(i)))  *  scale
                    sd(k)     = 1.0
                    sd(k + 1) = -2.0 * dble(poles(i))
                    sd(k + 2) = dble(poles(i) * conjg(poles(i)))
                    k = k + 3

                case (FILTER_ROOT_CP)
                    scale     = dble(poles(i) * conjg(poles(i)))
                    sn(k)     = 0.0
                    sn(k + 1) = 0.0
                    sn(k + 2) = scale
                    sd(k)     = 1.0
                    sd(k + 1) = -2.0 * dble(poles(i))
                    sd(k + 2) = dble(poles(i) * conjg(poles(i)))
                    k = k + 3
            end select
        end do

        sn(1) = sn(1) * dc
        sn(2) = sn(2) * dc
        sn(3) = sn(3) * dc
    end subroutine filter_low_pass_to_high_pass

    pure subroutine filter_low_pass_to_band_reject(poles, zeros, root_type, dc, nsects, fl, fh, sn, sd)
        !! Subroutine to convert a low-pass filter to a band-reject filter via
        !! an analog polynomial transformation. The low-pass filter is
        !! described in terms of its poles and zeros (as input to this
        !! routine). The output consists of the parameters for second order
        !! sections.
        complex(r16), intent(inout) :: poles(:)     !! Array containing poles.
        complex(r16), intent(inout) :: zeros(:)     !! Array containing zeros.
        integer,      intent(inout) :: root_type(:) !! Root type (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc           !! Zero-frequency value of filter.
        integer,      intent(inout) :: nsects       !! Number of 2nd order sections upon input/output.
        real(r8),     intent(out)   :: fl           !! Low-frequency cut-off.
        real(r8),     intent(out)   :: fh           !! High-frequency cut-off.
        real(r8),     intent(inout) :: sn(:)        !! Numerator polynomials for second order sections.
        real(r8),     intent(inout) :: sd(:)        !! Denominator polynomials for second order sections.

        complex(r16) :: cinv, ctemp, p1, p2, z1, z2
        real(r8)     :: a, b, scale, h
        integer      :: n, k, i

        a = PI2**2 * fl * fh
        b = PI2 * (fh - fl)

        n = nsects
        nsects = 0

        k = 1

        do i = 1, n
            select case (root_type(i))
                case (FILTER_ROOT_SP)
                    sn(k)     = a
                    sn(k + 1) = 0.0
                    sn(k + 2) = 1.0
                    sd(k)     = -a * dble(poles(i))
                    sd(k + 1) = b
                    sd(k + 2) = -dble(poles(i))
                    k = k + 3
                    nsects = nsects + 1

                case (FILTER_ROOT_CP)
                    cinv  = 1.0 / poles(i)
                    ctemp = (b * cinv)**2 - 4.0 * a
                    ctemp = sqrt(ctemp)
                    p1 = 0.5 * (b * cinv + ctemp)
                    p2 = 0.5 * (b * cinv - ctemp)
                    sn(k)     = a
                    sn(k + 1) = 0.0
                    sn(k + 2) = 1.0
                    sd(k)     = dble(p1 * conjg(p1))
                    sd(k + 1) = -2.0 * dble(p1)
                    sd(k + 2) = 1.0
                    k = k + 3
                    sn(k)     = a
                    sn(k + 1) = 0.0
                    sn(k + 2) = 1.0
                    sd(k)     = dble(p2 * conjg(p2))
                    sd(k + 1) = -2.0 * dble(p2)
                    sd(k + 2) = 1.0
                    k = k + 3
                    nsects = nsects + 2

                case (FILTER_ROOT_CPZ)
                    cinv  = 1.0 / zeros(i)
                    ctemp = (b * cinv)**2 - 4.0 * a
                    ctemp = sqrt(ctemp)
                    z1 = 0.5 * (b * cinv + ctemp)
                    z2 = 0.5 * (b * cinv - ctemp)
                    cinv  = 1.0 / poles(i)
                    ctemp = (b * cinv)**2 - 4.0 *a
                    ctemp = sqrt(ctemp)
                    p1 = 0.5 * (b * cinv + ctemp)
                    p2 = 0.5 * (b * cinv - ctemp)
                    sn(k)     = dble(z1 * conjg(z1))
                    sn(k + 1) = -2.0 * dble(z1)
                    sn(k + 2) = 1.0
                    sd(k)     = dble(p1 * conjg(p1))
                    sd(k + 1) = -2.0 * dble(p1)
                    sd(k + 2) = 1.0
                    k = k + 3
                    sn(k)     = dble(z2 * conjg(z2))
                    sn(k + 1) = -2.0 * dble(z2)
                    sn(k + 2) = 1.0
                    sd(k)     = dble(p2 * conjg(p2))
                    sd(k + 1) = -2.0 * dble(p2)
                    sd(k + 2) = 1.0
                    k = k + 3
                    nsects = nsects + 2
            end select
        end do

        h = 1.0
        k = 1

        do i = 1, nsects
            h = h * sn(k) / sd(k)
            k = k + 3
        end do

        scale = dc / abs(h)

        sn(1) = sn(1) * scale
        sn(2) = sn(2) * scale
        sn(3) = sn(3) * scale
    end subroutine filter_low_pass_to_band_reject

    pure subroutine filter_roots_bessel(poles, root_type, dc, nsects, ord)
        !! Subroutine to return bessel poles for normalised low-pass filter.
        complex(r16), intent(inout) :: poles(:)     !! Complex array containing poles. Contains only one from each complex conjugate pair, and all real poles.
        integer,      intent(inout) :: root_type(:) !! Root type (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc           !! Magnitude of filter at zero frequency.
        integer,      intent(out)   :: nsects       !! Number of second ord sections.
        integer,      intent(in)    :: ord          !! Desired filter order.

        select case (ord)
            case (1)
                poles(1) = cmplx(-1.0_r8, 0.0_r8, r16)
                root_type(1) = FILTER_ROOT_SP

            case (2)
                poles(1) = cmplx(-1.1016013_r8, 0.6360098_r8, r16)
                root_type(1) = FILTER_ROOT_CP

            case (3)
                poles(1) = cmplx(-1.0474091_r8, 0.9992645_r8, r16)
                poles(2) = cmplx(-1.3226758_r8, 0.0_r8,       r16)
                root_type(1) = FILTER_ROOT_CP
                root_type(2) = FILTER_ROOT_SP

            case (4)
                poles(1) = cmplx(-0.9952088_r8, 1.2571058_r8, r16)
                poles(2) = cmplx(-1.3700679_r8, 0.4102497_r8, r16)
                root_type(1) = FILTER_ROOT_CP
                root_type(2) = FILTER_ROOT_CP

            case (5)
                poles(1) = cmplx(-0.9576766_r8, 1.4711244_r8, r16)
                poles(2) = cmplx(-1.3808774_r8, 0.7179096_r8, r16)
                poles(3) = cmplx(-1.5023160_r8, 0.0_r8,       r16)
                root_type(1) = FILTER_ROOT_CP
                root_type(2) = FILTER_ROOT_CP
                root_type(3) = FILTER_ROOT_SP

            case (6)
                poles(1) = cmplx(-0.9306565_r8, 1.6618633_r8, r16)
                poles(2) = cmplx(-1.3818581_r8, 0.9714719_r8, r16)
                poles(3) = cmplx(-1.5714904_r8, 0.3208964_r8, r16)
                root_type(1) = FILTER_ROOT_CP
                root_type(2) = FILTER_ROOT_CP
                root_type(3) = FILTER_ROOT_CP

            case (7)
                poles(1) = cmplx(-0.9098678_r8, 1.8364514_r8, r16)
                poles(2) = cmplx(-1.3789032_r8, 1.1915667_r8, r16)
                poles(3) = cmplx(-1.6120388_r8, 0.5892445_r8, r16)
                poles(4) = cmplx(-1.6843682_r8, 0.0_r8,       r16)
                root_type(1) = FILTER_ROOT_CP
                root_type(2) = FILTER_ROOT_CP
                root_type(3) = FILTER_ROOT_CP
                root_type(4) = FILTER_ROOT_SP

            case (8)
                poles(1) = cmplx(-0.8928710_r8, 1.9983286_r8, r16)
                poles(2) = cmplx(-1.3738431_r8, 1.3883585_r8, r16)
                poles(3) = cmplx(-1.6369417_r8, 0.8227968_r8, r16)
                poles(4) = cmplx(-1.7574108_r8, 0.2728679_r8, r16)
                root_type(1) = FILTER_ROOT_CP
                root_type(2) = FILTER_ROOT_CP
                root_type(3) = FILTER_ROOT_CP
                root_type(4) = FILTER_ROOT_CP
        end select

        nsects = ord - ord / 2
        dc     = 1.0
    end subroutine filter_roots_bessel

    pure subroutine filter_roots_butterworth(poles, root_type, dc, nsects, ord)
        !! Subroutine to compute Butterworth poles for normalised low pass
        !! filter. Output argument `p` is a complex array containing poles:
        !! only one from each complex conjugate pair, and all real poles.
        complex(r16),  intent(inout) :: poles(:)     !! Poles.
        integer,       intent(inout) :: root_type(:) !! Root type (`FILTER_ROOT_*`).
        real(r8),      intent(out)   :: dc           !! Magnitude of filter at zero frequency.
        integer,       intent(out)   :: nsects       !! Number of second order sections.
        integer,       intent(in)    :: ord          !! Desired filter order.

        integer  :: half, k
        real(r8) :: angle

        half = ord / 2

        ! Test for odd order, and add pole at -1.
        nsects = 0

        if (2 * half < ord) then
            poles(1) = cmplx(-1.0_r8, 0.0_r8, r16)
            root_type(1) = FILTER_ROOT_SP
            nsects = 1
        end if

        do k = 1, half
            angle = PI * (0.5 + dble(2 * k - 1) / dble(2 * ord))
            nsects = nsects + 1
            poles(nsects) = cmplx(cos(angle), sin(angle), r16)
            root_type(nsects) = FILTER_ROOT_CP
        end do

        dc = 1.0_r8
    end subroutine filter_roots_butterworth

    pure subroutine filter_roots_chebyshev1(poles, root_type, dc, nsects, ord, eps)
        !! Subroutine to compute Chebyshev type 1 poles for normalised low-pass
        !! filter.
        complex(r16), intent(inout) :: poles(:)     !! Complex array containing poles. Contains only one from each complex conjugate pair, and all real poles.
        integer,      intent(inout) :: root_type(:) !! Root type (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc           !! Magnitude of filter at zero frequency.
        integer,      intent(out)   :: nsects       !! Number of second ord sections.
        integer,      intent(in)    :: ord          !! Desired filter order.
        real(r8),     intent(out)   :: eps          !! Chebyshev parameter related to passband ripple.

        real(r8) :: angle, c, s
        real(r8) :: gamma, sigma, omega
        integer  :: half, i

        half = ord / 2

        ! Intermediate design parameters.
        gamma = (1.0_r8 + sqrt(1.0_r8 + eps**2)) / eps
        gamma = log(gamma) / dble(ord)
        gamma = exp(gamma)

        s = 0.5 * (gamma - 1.0 / gamma)
        c = 0.5 * (gamma + 1.0 / gamma)

        ! Calculate poles.
        nsects = 0

        do i = 1, half
            root_type(i) = FILTER_ROOT_CP
            angle = dble(2 * i - 1) * PI / dble(2 * ord)
            sigma = -s * sin(angle)
            omega =  c * cos(angle)
            poles(i) = cmplx(sigma, omega, r16)
            nsects = nsects + 1
        end do

        if (2 * half < ord) then
            root_type(half + 1) = FILTER_ROOT_SP
            poles(half + 1) = cmplx(-s, 0.0_r8, r16)
            nsects = nsects + 1
            dc = 1.0_r8
        else
            dc = 1.0_r8 / sqrt(1.0 + eps**2)
        end if
    end subroutine filter_roots_chebyshev1

    pure subroutine filter_roots_chebyshev2(poles, zeros, root_type, dc, nsects, ord, a, omegar)
        !! Subroutine to compute roots for normalised low-pass Chebyshev
        !! type II filter.
        complex(r16), intent(inout) :: poles(:)     !! Complex array containing poles. Contains only one from each complex conjugate pair, and all real poles.
        complex(r16), intent(inout) :: zeros(:)     !! Complex array containing zeros. Contains only one from each complex conjugate pair, and all real poles.
        integer,      intent(inout) :: root_type(:) !! Root type (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc           !! Magnitude of filter at zero frequency.
        integer,      intent(out)   :: nsects       !! Number of second ord sections.
        integer,      intent(in)    :: ord          !! Desired filter order.
        real(r8),     intent(in)    :: a            !! Stop-band attenuation factor.
        real(r8),     intent(in)    :: omegar       !! Cut-off frequency of stop-band. Pass-band cut-off is at 1.0 Hz.

        real(r8) :: c, s
        real(r8) :: angle, denom
        real(r8) :: alpha, beta, gamma, sigma, omega
        integer  :: half, i

        half = ord / 2

        ! Intermediate design parameters.
        gamma = (a + sqrt(a * a - 1.0))
        gamma = log(gamma) / dble(ord)
        gamma = exp(gamma)

        s = 0.5 * (gamma - 1.0 / gamma)
        c = 0.5 * (gamma + 1.0 / gamma)

        nsects = 0

        do i = 1, half
            ! Calculate poles.
            root_type(i) = FILTER_ROOT_CPZ
            angle = dble(2 * i - 1) * PI / dble(2 * ord)
            alpha = -s * sin(angle)
            beta  =  c * cos(angle)
            denom = alpha**2 + beta**2
            sigma =  omegar * alpha / denom
            omega = -omegar * beta / denom
            poles(i) = cmplx(sigma, omega, r16)

            ! Calculate zeros.
            omega = omegar / cos(angle)
            zeros(i) = cmplx(0.0, omega, r16)

            nsects = nsects + 1
        end do

        ! Odd-order filters.
        if (2 * half < ord) then
            root_type(half + 1) = FILTER_ROOT_SP
            poles(half + 1) = cmplx(-omegar / s, 0.0_r8, r16)
            nsects = nsects + 1
        end if

        dc = 1.0_r8
    end subroutine filter_roots_chebyshev2

    pure subroutine filter_roots_chebyshev_parameters(a, trans, ord, eps, ripple)
        !! Calculates Chebyshev type I and II design parameters.
        real(r8), intent(in)  :: a      !! Desired stopband attenuation, i.e. max stopband amplitude is 1/attenuation.
        real(r8), intent(in)  :: trans  !! Transition bandwidth between stop- and passbands as a fraction of the passband width.
        integer,  intent(in)  :: ord    !! Filter order (number of poles).
        real(r8), intent(out) :: eps    !! Chebyshev passband parameter.
        real(r8), intent(out) :: ripple !! Passband ripple.

        real(r8) :: alpha, omegar
        real(r8) :: g

        omegar =  1. + trans
        alpha  = (omegar + sqrt(omegar**2 - 1.0))**ord
        g      = (alpha**2 + 1.0) / (2.0 * alpha)
        eps    = sqrt(a**2 - 1.0) / g
        ripple = 1.0 / sqrt(1.0 + eps**2)
    end subroutine filter_roots_chebyshev_parameters

    pure elemental real(r8) function filter_warp(f, ts) result(warp)
        !! Function, applies tangent frequency warping to compensate for
        !! bilinear analog to digital transformation.
        real(r8), intent(in) :: f  !! Original design frequency specification [Hz].
        real(r8), intent(in) :: ts !! Sampling interval [sec].

        real(r8) :: angle

        angle = PI2 * f * ts / 2.0
        warp  = 2.0 * tan(angle) / ts
        warp  = warp / PI2
    end function filter_warp
end module dm_filter
