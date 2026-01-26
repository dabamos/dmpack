! Author:  Philipp Engel
! Licence: ISC
module dm_filter
    !! IIR filter module, based on FORTRAN 77 procedures by David Harris:
    !!
    !! * Harris, D. (1990): XAPiir. A Recursive Digital Filtering Package.
    !!   Technical Report. Lawrence Livermore National Laboratory.
    !!   DOI: doi.org/10.2172/6416972
    !!
    use :: dm_const
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter :: MAX_ORDER = 10
    integer, parameter :: MAX_SIZE  = 3 * MAX_ORDER

    ! Analog prototypes.
    integer, parameter, public :: FILTER_BESSEL      = 1 !! Bessel.
    integer, parameter, public :: FILTER_BUTTERWORTH = 2 !! Butterworth.
    integer, parameter, public :: FILTER_CHEBYSHEV1  = 3 !! Chebyshev Type I.
    integer, parameter, public :: FILTER_CHEBYSHEV2  = 4 !! Chebyshev Type II.

    ! Root types.
    integer, parameter :: FILTER_ROOT_SP  = 1 !! Single real pole.
    integer, parameter :: FILTER_ROOT_CP  = 2 !! Complex conjugate pole pair.
    integer, parameter :: FILTER_ROOT_CPZ = 3 !! Complex conjugate pole and zero pairs.

    ! Filter types.
    integer, parameter :: FILTER_TYPE_LP = 1 !! Low-pass.
    integer, parameter :: FILTER_TYPE_HP = 2 !! High-pass.
    integer, parameter :: FILTER_TYPE_BP = 3 !! Band-pass.
    integer, parameter :: FILTER_TYPE_BR = 4 !! Band-reject.

    integer, parameter, public :: FILTER_MAX_ORDER = MAX_ORDER !! Max. order (number of poles).

    ! Public procedures.
    public :: dm_filter_high_pass
    public :: dm_filter_is_valid
    public :: dm_filter_low_pass

    ! Private procedures.
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
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental logical function dm_filter_is_valid(filter) result(valid)
        !! Returns `.true.` if `prototype` is a valid filter enumerator:
        !!
        !! * `FILTER_BESSEL`
        !! * `FILTER_BUTTERWORTH`
        !! * `FILTER_CHEBYSHEV1`
        !! * `FILTER_CHEBYSHEV2`
        !!
        integer, intent(in) :: filter !! Filter enumerator (`FILTER_*`).

        valid = (filter >= FILTER_BESSEL .and. filter <= FILTER_CHEBYSHEV2)
    end function dm_filter_is_valid

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    pure subroutine dm_filter_high_pass(filter, data, order, cutoff, ts, zp, attenuation, transition)
        integer,  intent(in)           :: filter      !! Filter enumerator (`FILTER_*`).
        real(r8), intent(inout)        :: data(:)     !! Sequence to be filtered.
        integer,  intent(in)           :: order       !! Order (#poles), not to exceed `FILTER_MAX_ORDER`.
        real(r8), intent(in)           :: cutoff      !! Low-frequency cut-off of filter [Hz].
        real(r8), intent(in)           :: ts          !! Sampling interval [sec].
        logical,  intent(in)           :: zp          !! Zero phase filtering if `.true.`, else single pass filtering.
        real(r8), intent(in), optional :: attenuation !! Chebyshev stopband attenuation factor.
        real(r8), intent(in), optional :: transition  !! Chebyshev transition bandwidth (fraction of low-pass prototype passband width).

        integer  :: nsects, order_
        real(r8) :: a, t
        real(r8) :: sn(MAX_SIZE), sd(MAX_SIZE)

        if (.not. dm_filter_is_valid(filter)) return

        if (present(attenuation)) a = attenuation
        if (present(transition))  t = transition

        order_ = min(MAX_ORDER, order)

        call filter_design(order_, FILTER_TYPE_HP, filter, a, t, cutoff, 0.0_r8, ts, sn, sd, nsects)
        call filter_filter(data, zp, sn, sd, nsects)
    end subroutine dm_filter_high_pass

    pure subroutine dm_filter_low_pass(filter, data, order, cutoff, ts, zp, attenuation, transition)
        integer,  intent(in)           :: filter      !! Filter enumerator (`FILTER_*`).
        real(r8), intent(inout)        :: data(:)     !! Sequence to be filtered.
        integer,  intent(in)           :: order       !! Order (#poles), not to exceed `FILTER_MAX_ORDER`.
        real(r8), intent(in)           :: cutoff      !! High-frequency cut-off of filter [Hz].
        real(r8), intent(in)           :: ts          !! Sampling interval [sec].
        logical,  intent(in)           :: zp          !! Zero phase filtering if `.true.`, else single pass filtering.
        real(r8), intent(in), optional :: attenuation !! Chebyshev stopband attenuation factor.
        real(r8), intent(in), optional :: transition  !! Chebyshev transition bandwidth (fraction of low-pass prototype passband width).

        integer  :: nsects, order_
        real(r8) :: a, t
        real(r8) :: sn(MAX_SIZE), sd(MAX_SIZE)

        if (.not. dm_filter_is_valid(filter)) return

        if (present(attenuation)) a = attenuation
        if (present(transition))  t = transition

        order_ = min(MAX_ORDER, order)

        call filter_design(order_, FILTER_TYPE_LP, filter, a, t, 0.0_r8, cutoff, ts, sn, sd, nsects)
        call filter_filter(data, zp, sn, sd, nsects)
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

            sd(k)     = 1.0_r8
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

    pure subroutine filter_design(order, type, prototype, a, trans, fl, fh, ts, sn, sd, nsects)
        !! Subroutine to design IIR digital filters from analog prototypes.
        integer,  intent(in)    :: order     !! Filter order.
        integer,  intent(in)    :: type      !! Filter type enumerator (`FILTER_TYPE_*`).
        integer,  intent(in)    :: prototype !! Analog prototype enumerator (`FILTER_*`).
        real(r8), intent(in)    :: a         !! Chebyshev stopband attenuation factor.
        real(r8), intent(in)    :: trans     !! Chebyshev transition bandwidth (fraction of low-pass prototype passband width).
        real(r8), intent(in)    :: fl        !! Low-frequency cut-off [Hz].
        real(r8), intent(in)    :: fh        !! High-frequency cut-off [Hz].
        real(r8), intent(in)    :: ts        !! Sampling interval [sec].
        real(r8), intent(inout) :: sn(:)     !! Numerator coefficients or second order sections. Packed head-to-tail.
        real(r8), intent(inout) :: sd(:)     !! Denominator coefficients of second order sections. Packed head-to-tail.
        integer,  intent(out)   :: nsects    !! Number of second order sections.

        complex(r16) :: poles(MAX_ORDER), zeros(MAX_ORDER)
        real(r8)     :: dc, eps, fhw, flw, omegar, ripple
        integer      :: roots(MAX_ORDER)

        ! Analog prototype selection.
        select case (prototype)
            case (FILTER_BESSEL)
                call filter_roots_bessel(poles, roots, dc, nsects, order)

            case (FILTER_BUTTERWORTH)
                call filter_roots_butterworth(poles, roots, dc, nsects, order)

            case (FILTER_CHEBYSHEV1)
                call filter_roots_chebyshev_parameters(a, trans, order, eps, ripple)
                call filter_roots_chebyshev1(poles, roots, dc, nsects, order, eps)

            case (FILTER_CHEBYSHEV2)
                omegar = 1.0 + trans
                call filter_roots_chebyshev2(poles, zeros, roots, dc, nsects, order, a, omegar)
        end select

        ! Analog mapping selection.
        select case (type)
            case (FILTER_TYPE_LP)
                fhw = filter_warp(fh * ts / 2.0, 2.0_r8)
                call filter_low_pass(poles, zeros, roots, dc, nsects, sn, sd)
                call filter_cutoff(sn, sd, nsects, fhw)

            case (FILTER_TYPE_HP)
                flw = filter_warp(fl * ts / 2.0, 2.0_r8)
                call filter_low_pass_to_high_pass(poles, zeros, roots, dc, nsects, sn, sd)
                call filter_cutoff(sn, sd, nsects, flw)

            case (FILTER_TYPE_BP)
                flw = filter_warp(fl * ts / 2.0, 2.0_r8)
                fhw = filter_warp(fh * ts / 2.0, 2.0_r8)
                call filter_low_pass_to_band_pass(poles, zeros, roots, dc, nsects, flw, fhw, sn, sd)

            case (FILTER_TYPE_BR)
                flw = filter_warp(fl * ts / 2.0, 2.0_r8)
                fhw = filter_warp(fh * ts / 2.0, 2.0_r8)
                call filter_low_pass_to_band_reject(poles, zeros, roots, dc, nsects, flw, fhw, sn, sd)
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
        integer,  intent(in)    :: nsects  !! Number of second order sections.

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
                x1 = 0.0_r8
                x2 = 0.0_r8
                y1 = 0.0_r8
                y2 = 0.0_r8
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

    pure subroutine filter_low_pass(poles, zeros, roots, dc, nsects, sn, sd)
        !! Subroutine to generate second order section parameterisation from an
        !! pole-zero description for low pass filters.
        complex(r16), intent(inout) :: poles(:) !! Poles.
        complex(r16), intent(inout) :: zeros(:) !! Zeroes.
        integer,      intent(inout) :: roots(:) !! Root types (`FILTER_ROOT_*`).
        real(r8),     intent(in)    :: dc       !! Zero-frequency value of prototype filter.
        integer,      intent(in)    :: nsects   !! Number of second order sections.
        real(r8),     intent(inout) :: sn(:)    !! Numerator polynomials for second order sections.
        real(r8),     intent(inout) :: sd(:)    !! Denominator polynomials for second order sections.

        integer  :: i, k
        real(r8) :: scale

        k = 1

        do i = 1, nsects
            select case (roots(i))
                case (FILTER_ROOT_SP)
                    scale     = -1 * dble(poles(i))
                    sn(k)     = scale
                    sn(k + 1) = 0.0_r8
                    sn(k + 2) = 0.0_r8
                    sd(k)     = -1.0 * dble(poles(i))
                    sd(k + 1) = 1.0_r8
                    sd(k + 2) = 0.0_r8
                    k = k + 3

                case (FILTER_ROOT_CP)
                    scale     = dble(poles(i) * conjg(poles(i)))
                    sn(k)     = scale
                    sn(k + 1) = 0.0_r8
                    sn(k + 2) = 0.0_r8
                    sd(k)     = dble(poles(i) * conjg(poles(i)))
                    sd(k + 1) = -2.0 * dble(poles(i))
                    sd(k + 2) = 1.0_r8
                    k = k + 3

                case (FILTER_ROOT_CPZ)
                    scale     = dble(poles(i) * conjg(poles(i))) / dble(zeros(i) * conjg(zeros(i)))
                    sn(k)     = dble(zeros(i) * conjg(zeros(i))) * scale
                    sn(k + 1) = -2.0 * dble(zeros(i)) * scale
                    sn(k + 2) = 1.0 * scale
                    sd(k)     = dble(poles(i) * conjg(poles(i)))
                    sd(k + 1) = -2.0 * dble(poles(i))
                    sd(k + 2) = 1.0_r8
                    k = k + 3
            end select
        end do

        sn(1) = sn(1) * dc
        sn(2) = sn(2) * dc
        sn(3) = sn(3) * dc
    end subroutine filter_low_pass

    pure subroutine filter_low_pass_to_band_pass(poles, zeros, roots, dc, nsects, fl, fh, sn, sd)
        !! Subroutine to convert an prototype low-pass filter to a band-pass
        !! filter via the analog polynomial transformation. The low-pass filter
        !! is described in terms of its poles and zeros (as input to this
        !! routine). The output consists of the parameters for second order
        !! sections.
        complex(r16), intent(inout) :: poles(:) !! Array containing poles.
        complex(r16), intent(inout) :: zeros(:) !! Array containing zeros.
        integer,      intent(inout) :: roots(:) !! Root types (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc       !! Zero-frequency value of filter.
        integer,      intent(inout) :: nsects   !! Number of second order sections upon input/output.
        real(r8),     intent(out)   :: fl       !! Low-frequency cut-off.
        real(r8),     intent(out)   :: fh       !! High-frequency cut-off.
        real(r8),     intent(inout) :: sn(:)    !! Numerator polynomials for second order sections.
        real(r8),     intent(inout) :: sd(:)    !! Denominator polynomials for second order sections.

        complex(r16) :: c, p1, p2, z1, z2, s, h
        real(r8)     :: a, b, scale
        integer      :: i, k, n

        a = PI2**2 * fl * fh
        b = PI2 * (fh - fl)

        n = nsects
        nsects = 0
        k = 1

        do i = 1, n
            select case (roots(i))
                case (FILTER_ROOT_SP)
                    sn(k)     = 0.0_r8
                    sn(k + 1) = b
                    sn(k + 2) = 0.0_r8
                    sd(k)     = a
                    sd(k + 1) = -b * dble(poles(i))
                    sd(k + 2) = 1.0_r8
                    k = k + 3
                    nsects = nsects + 1

                case (FILTER_ROOT_CP)
                    c = (b * poles(i))**2 - 4.0 * a
                    c = sqrt(c)
                    p1 = 0.5 * (b * poles(i) + c)
                    p2 = 0.5 * (b * poles(i) - c)
                    sn(k)     = 0.0_r8
                    sn(k + 1) = b
                    sn(k + 2) = 0.0_r8
                    sd(k)     = dble(p1 * conjg(p1))
                    sd(k + 1) = -2.0 * dble(p1)
                    sd(k + 2) = 1.0_r8
                    k = k + 3
                    sn(k)     = 0.0_r8
                    sn(k + 1) = b
                    sn(k + 2) = 0.0_r8
                    sd(k)     = dble(p2 * conjg(p2))
                    sd(k + 1) = -2.0 * dble(p2)
                    sd(k + 2) = 1.0_r8
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
                    sn(k + 1) = -2.0 * dble(z1)
                    sn(k + 2) = 1.0_r8
                    sd(k)     = dble(p1 * conjg(p1))
                    sd(k + 1) = -2.0 * dble(p1)
                    sd(k + 2) = 1.0_r8
                    k = k + 3
                    sn(k)     = dble(z2 * conjg(z2))
                    sn(k + 1) = -2.0 * dble(z2)
                    sn(k + 2) = 1.0_r8
                    sd(k)     = dble(p2 * conjg(p2))
                    sd(k + 1) = -2.0 * dble(p2)
                    sd(k + 2) = 1.0_r8
                    k = k + 3
                    nsects = nsects + 2
            end select
        end do

        s = cmplx(0.0_r8, sqrt(a), r16)
        h = cmplx(1.0_r8, 0.0_r8,  r16)
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

    pure subroutine filter_low_pass_to_high_pass(poles, zeros, roots, dc, nsects, sn, sd)
        !! Subroutine to convert a low-pass filter to a high-pass filter via an
        !! analog polynomial transformation. The low-pass filter is described
        !! in terms of its poles and zeroes (as input to this routine). The
        !! output consists of the parameters for second order sections.
        complex(r16), intent(inout) :: poles(:) !! Array containing poles.
        complex(r16), intent(inout) :: zeros(:) !! Array containing zeros.
        integer,      intent(inout) :: roots(:) !! Root types (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc       !! Zero-frequency value of filter.
        integer,      intent(inout) :: nsects   !! Number of second order sections upon input/output.
        real(r8),     intent(inout) :: sn(:)    !! Numerator polynomials for second order sections.
        real(r8),     intent(inout) :: sd(:)    !! Denominator polynomials for second order sections.

        real(r8) :: scale
        integer  :: i, k

        k = 1

        do i = 1, nsects
            select case (roots(i))
                case (FILTER_ROOT_SP)
                    scale     = -dble(poles(i))
                    sn(k)     = 0.0_r8
                    sn(k + 1) = scale
                    sn(k + 2) = 0.0_r8
                    sd(k)     = 1.0_r8
                    sd(k + 1) = -dble(poles(i))
                    sd(k + 2) = 0.0_r8
                    k = k + 3

                case (FILTER_ROOT_CPZ)
                    scale     = dble(poles(i) * conjg(poles(i))) / dble(zeros(i) * conjg(zeros(i)))
                    sn(k)     = 1.0 * scale
                    sn(k + 1) = -2.0 * dble(zeros(i))  *  scale
                    sn(k + 2) = dble(zeros(i) * conjg(zeros(i)))  *  scale
                    sd(k)     = 1.0_r8
                    sd(k + 1) = -2.0 * dble(poles(i))
                    sd(k + 2) = dble(poles(i) * conjg(poles(i)))
                    k = k + 3

                case (FILTER_ROOT_CP)
                    scale     = dble(poles(i) * conjg(poles(i)))
                    sn(k)     = 0.0_r8
                    sn(k + 1) = 0.0_r8
                    sn(k + 2) = scale
                    sd(k)     = 1.0_r8
                    sd(k + 1) = -2.0 * dble(poles(i))
                    sd(k + 2) = dble(poles(i) * conjg(poles(i)))
                    k = k + 3
            end select
        end do

        sn(1) = sn(1) * dc
        sn(2) = sn(2) * dc
        sn(3) = sn(3) * dc
    end subroutine filter_low_pass_to_high_pass

    pure subroutine filter_low_pass_to_band_reject(poles, zeros, roots, dc, nsects, fl, fh, sn, sd)
        !! Subroutine to convert a low-pass filter to a band-reject filter via
        !! an analog polynomial transformation. The low-pass filter is
        !! described in terms of its poles and zeros (as input to this
        !! routine). The output consists of the parameters for second order
        !! sections.
        complex(r16), intent(inout) :: poles(:) !! Array containing poles.
        complex(r16), intent(inout) :: zeros(:) !! Array containing zeros.
        integer,      intent(inout) :: roots(:) !! Root types (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc       !! Zero-frequency value of filter.
        integer,      intent(inout) :: nsects   !! Number of second order sections upon input/output.
        real(r8),     intent(out)   :: fl       !! Low-frequency cut-off.
        real(r8),     intent(out)   :: fh       !! High-frequency cut-off.
        real(r8),     intent(inout) :: sn(:)    !! Numerator polynomials for second order sections.
        real(r8),     intent(inout) :: sd(:)    !! Denominator polynomials for second order sections.

        complex(r16) :: cinv, ctemp, p1, p2, z1, z2
        real(r8)     :: a, b, scale, h
        integer      :: n, k, i

        a = PI2**2 * fl * fh
        b = PI2 * (fh - fl)

        n = nsects
        nsects = 0

        k = 1

        do i = 1, n
            select case (roots(i))
                case (FILTER_ROOT_SP)
                    sn(k)     = a
                    sn(k + 1) = 0.0_r8
                    sn(k + 2) = 1.0_r8
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
                    sn(k + 1) = 0.0_r8
                    sn(k + 2) = 1.0_r8
                    sd(k)     = dble(p1 * conjg(p1))
                    sd(k + 1) = -2.0 * dble(p1)
                    sd(k + 2) = 1.0_r8
                    k = k + 3
                    sn(k)     = a
                    sn(k + 1) = 0.0_r8
                    sn(k + 2) = 1.0_r8
                    sd(k)     = dble(p2 * conjg(p2))
                    sd(k + 1) = -2.0 * dble(p2)
                    sd(k + 2) = 1.0_r8
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
                    sn(k + 2) = 1.0_r8
                    sd(k)     = dble(p1 * conjg(p1))
                    sd(k + 1) = -2.0 * dble(p1)
                    sd(k + 2) = 1.0_r8
                    k = k + 3
                    sn(k)     = dble(z2 * conjg(z2))
                    sn(k + 1) = -2.0 * dble(z2)
                    sn(k + 2) = 1.0_r8
                    sd(k)     = dble(p2 * conjg(p2))
                    sd(k + 1) = -2.0 * dble(p2)
                    sd(k + 2) = 1.0_r8
                    k = k + 3
                    nsects = nsects + 2
            end select
        end do

        h = 1.0_r8
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

    pure subroutine filter_roots_bessel(poles, roots, dc, nsects, order)
        !! Subroutine to return frequency normalised Bessel pole locations for
        !! normalised low-pass filter. Values have been taken from
        !! http://www.crbond.com/papers/bsf2.pdf.
        complex(r16), intent(inout) :: poles(:) !! Complex array containing poles. Contains only one from each complex conjugate pair, and all real poles.
        integer,      intent(inout) :: roots(:) !! Root types (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc       !! Magnitude of filter at zero frequency.
        integer,      intent(out)   :: nsects   !! Number of second order sections.
        integer,      intent(in)    :: order    !! Desired filter order.

        select case (order)
            case (1)
                poles(1) = cmplx(-1.0000000000_r16, 0.0000000000_r16, r16); roots(1) = FILTER_ROOT_SP

            case (2)
                poles(1) = cmplx(-1.1016013306_r16, 0.6360098248_r16, r16); roots(1) = FILTER_ROOT_CP

            case (3)
                poles(2) = cmplx(-1.0474091610_r16, 0.9992644363_r16, r16); roots(1) = FILTER_ROOT_CP
                poles(2) = cmplx(-1.3226757999_r16, 0.0000000000_r16, r16); roots(2) = FILTER_ROOT_SP

            case (4)
                poles(1) = cmplx(-0.9952087644_r16, 1.2571057395_r16, r16); roots(1) = FILTER_ROOT_CP
                poles(2) = cmplx(-1.3700678306_r16, 0.4102497175_r16, r16); roots(2) = FILTER_ROOT_CP

            case (5)
                poles(1) = cmplx(-0.9576765486_r16, 1.4711243207_r16, r16); roots(1) = FILTER_ROOT_CP
                poles(2) = cmplx(-1.3808773259_r16, 0.7179095876_r16, r16); roots(2) = FILTER_ROOT_CP
                poles(3) = cmplx(-1.5023162714_r16, 0.0000000000_r16, r16); roots(3) = FILTER_ROOT_SP

            case (6)
                poles(1) = cmplx(-0.9306565229_r16, 1.6618632689_r16, r16); roots(1) = FILTER_ROOT_CP
                poles(2) = cmplx(-1.3818580976_r16, 0.9714718907_r16, r16); roots(2) = FILTER_ROOT_CP
                poles(3) = cmplx(-1.5714904036_r16, 0.3208963742_r16, r16); roots(3) = FILTER_ROOT_CP

            case (7)
                poles(1) = cmplx(-0.9098677806_r16, 1.8364513530_r16, r16); roots(1) = FILTER_ROOT_CP
                poles(2) = cmplx(-1.3789032168_r16, 1.1915667778_r16, r16); roots(2) = FILTER_ROOT_CP
                poles(3) = cmplx(-1.6120387662_r16, 0.5892445069_r16, r16); roots(3) = FILTER_ROOT_CP
                poles(4) = cmplx(-1.6843681793_r16, 0.0000000000_r16, r16); roots(4) = FILTER_ROOT_SP

            case (8)
                poles(1) = cmplx(-0.8928697188_r16, 1.9983258436_r16, r16); roots(1) = FILTER_ROOT_CP
                poles(2) = cmplx(-1.3738412176_r16, 1.3883565759_r16, r16); roots(2) = FILTER_ROOT_CP
                poles(3) = cmplx(-1.6369394181_r16, 0.8227956251_r16, r16); roots(3) = FILTER_ROOT_CP
                poles(4) = cmplx(-1.7574084004_r16, 0.2728675751_r16, r16); roots(4) = FILTER_ROOT_CP

            case (9)
                poles(1) = cmplx(-0.8783992762_r16, 2.1498005243_r16, r16); roots(1) = FILTER_ROOT_CP
                poles(2) = cmplx(-1.3675883098_r16, 1.5677337122_r16, r16); roots(2) = FILTER_ROOT_CP
                poles(3) = cmplx(-1.6523964846_r16, 1.0313895670_r16, r16); roots(3) = FILTER_ROOT_CP
                poles(4) = cmplx(-1.8071705350_r16, 0.5123837306_r16, r16); roots(4) = FILTER_ROOT_CP
                poles(5) = cmplx(-1.8566005012_r16, 0.0000000000_r16, r16); roots(5) = FILTER_ROOT_SP

            case (10)
                poles(1) = cmplx(-0.8657569017_r16, 2.2926048310_r16, r16); roots(1) = FILTER_ROOT_CP
                poles(2) = cmplx(-1.3606922784_r16, 1.7335057427_r16, r16); roots(2) = FILTER_ROOT_CP
                poles(3) = cmplx(-1.6618102414_r16, 1.2211002186_r16, r16); roots(3) = FILTER_ROOT_CP
                poles(4) = cmplx(-1.8421962445_r16, 0.7272575978_r16, r16); roots(4) = FILTER_ROOT_CP
                poles(4) = cmplx(-1.9276196914_r16, 0.2416234710_r16, r16); roots(5) = FILTER_ROOT_CP
        end select

        nsects = order - order / 2
        dc = 1.0_r8
    end subroutine filter_roots_bessel

    pure subroutine filter_roots_butterworth(poles, roots, dc, nsects, order)
        !! Subroutine to compute Butterworth poles for normalised low pass
        !! filter. Output argument `p` is a complex array containing poles:
        !! only one from each complex conjugate pair, and all real poles.
        complex(r16),  intent(inout) :: poles(:) !! Poles.
        integer,       intent(inout) :: roots(:) !! Root types (`FILTER_ROOT_*`).
        real(r8),      intent(out)   :: dc       !! Magnitude of filter at zero frequency.
        integer,       intent(out)   :: nsects   !! Number of second order sections.
        integer,       intent(in)    :: order    !! Desired filter order.

        integer  :: half, k
        real(r8) :: angle

        half = order / 2

        ! Test for odd order, and add pole at -1.
        nsects = 0

        if (2 * half < order) then
            poles(1) = cmplx(-1.0_r8, 0.0_r8, r16)
            roots(1) = FILTER_ROOT_SP
            nsects   = 1
        end if

        do k = 1, half
            angle  = PI * (0.5 + dble(2 * k - 1) / dble(2 * order))
            nsects = nsects + 1
            poles(nsects) = cmplx(cos(angle), sin(angle), r16)
            roots(nsects) = FILTER_ROOT_CP
        end do

        dc = 1.0_r8
    end subroutine filter_roots_butterworth

    pure subroutine filter_roots_chebyshev1(poles, roots, dc, nsects, order, eps)
        !! Subroutine to compute Chebyshev type I poles for normalised low-pass
        !! filter.
        complex(r16), intent(inout) :: poles(:) !! Complex array containing poles. Contains only one from each complex conjugate pair, and all real poles.
        integer,      intent(inout) :: roots(:) !! Root types (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc       !! Magnitude of filter at zero frequency.
        integer,      intent(out)   :: nsects   !! Number of second order sections.
        integer,      intent(in)    :: order    !! Desired filter order.
        real(r8),     intent(out)   :: eps      !! Chebyshev parameter related to passband ripple.

        real(r8) :: angle, c, s
        real(r8) :: gamma, sigma, omega
        integer  :: half, i

        half = order / 2

        ! Intermediate design parameters.
        gamma = (1.0_r8 + sqrt(1.0_r8 + eps**2)) / eps
        gamma = log(gamma) / dble(order)
        gamma = exp(gamma)

        s = 0.5 * (gamma - 1.0 / gamma)
        c = 0.5 * (gamma + 1.0 / gamma)

        ! Calculate poles.
        nsects = 0

        do i = 1, half
            roots(i) = FILTER_ROOT_CP
            angle    = dble(2 * i - 1) * PI / dble(2 * order)
            sigma    = -s * sin(angle)
            omega    =  c * cos(angle)
            poles(i) = cmplx(sigma, omega, r16)
            nsects   = nsects + 1
        end do

        if (2 * half < order) then
            roots(half + 1) = FILTER_ROOT_SP
            poles(half + 1) = cmplx(-s, 0.0_r8, r16)

            nsects = nsects + 1
            dc = 1.0_r8
        else
            dc = 1.0_r8 / sqrt(1.0 + eps**2)
        end if
    end subroutine filter_roots_chebyshev1

    pure subroutine filter_roots_chebyshev2(poles, zeros, roots, dc, nsects, order, a, omegar)
        !! Subroutine to compute roots for normalised low-pass Chebyshev
        !! type II filter.
        complex(r16), intent(inout) :: poles(:) !! Complex array containing poles. Contains only one from each complex conjugate pair, and all real poles.
        complex(r16), intent(inout) :: zeros(:) !! Complex array containing zeros. Contains only one from each complex conjugate pair, and all real poles.
        integer,      intent(inout) :: roots(:) !! Root types (`FILTER_ROOT_*`).
        real(r8),     intent(out)   :: dc       !! Magnitude of filter at zero frequency.
        integer,      intent(out)   :: nsects   !! Number of second order sections.
        integer,      intent(in)    :: order    !! Desired filter order.
        real(r8),     intent(in)    :: a        !! Stop-band attenuation factor.
        real(r8),     intent(in)    :: omegar   !! Cut-off frequency of stop-band. Pass-band cut-off is at 1.0 Hz.

        real(r8) :: c, s
        real(r8) :: angle, denom
        real(r8) :: alpha, beta, gamma, sigma, omega
        integer  :: half, i

        half = order / 2

        ! Intermediate design parameters.
        gamma = (a + sqrt(a * a - 1.0))
        gamma = log(gamma) / dble(order)
        gamma = exp(gamma)

        s = 0.5 * (gamma - 1.0 / gamma)
        c = 0.5 * (gamma + 1.0 / gamma)

        nsects = 0

        do i = 1, half
            ! Calculate poles.
            roots(i) = FILTER_ROOT_CPZ
            angle    = dble(2 * i - 1) * PI / dble(2 * order)
            alpha    = -s * sin(angle)
            beta     =  c * cos(angle)
            denom    = alpha**2 + beta**2
            sigma    =  omegar * alpha / denom
            omega    = -omegar * beta / denom
            poles(i) = cmplx(sigma, omega, r16)

            ! Calculate zeros.
            omega    = omegar / cos(angle)
            zeros(i) = cmplx(0.0, omega, r16)

            nsects = nsects + 1
        end do

        ! Odd-order filters.
        if (2 * half < order) then
            roots(half + 1) = FILTER_ROOT_SP
            poles(half + 1) = cmplx(-omegar / s, 0.0_r8, r16)
            nsects = nsects + 1
        end if

        dc = 1.0_r8
    end subroutine filter_roots_chebyshev2

    pure subroutine filter_roots_chebyshev_parameters(a, trans, order, eps, ripple)
        !! Calculates Chebyshev type I and II design parameters.
        real(r8), intent(in)  :: a      !! Desired stopband attenuation, i.e. max stopband amplitude is 1/attenuation.
        real(r8), intent(in)  :: trans  !! Transition bandwidth between stop- and passbands as a fraction of the passband width.
        integer,  intent(in)  :: order  !! Filter order (number of poles).
        real(r8), intent(out) :: eps    !! Chebyshev passband parameter.
        real(r8), intent(out) :: ripple !! Passband ripple.

        real(r8) :: alpha, omegar
        real(r8) :: g

        omegar = 1.0 + trans
        alpha  = (omegar + sqrt(omegar**2 - 1.0))**order
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
