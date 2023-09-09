! Author:  Philipp Engel
! Licence: ISC
module dm_transform
    !! Coordinate transformations (unfinished).
    use :: dm_const
    use :: dm_error
    use :: dm_la
    use :: dm_type
    implicit none (type, external)
    private

    public :: dm_transform_coord_2d
    public :: dm_transform_polar_3d
contains
    integer function dm_transform_coord_2d(cs, ct, observs, trans, params, residuals, variance, rotation, scale_factor) result(rc)
        integer, parameter :: NRHS = 4 !! Number of columns in matrix A.

        real(kind=r8),              intent(inout)         :: cs(:)        !! Input: Common control points in source system.
        real(kind=r8),              intent(inout)         :: ct(:)        !! Input: Common control points in target system.
        real(kind=r8),              intent(inout)         :: observs(:)   !! Input: Observations.
        real(kind=r8), allocatable, intent(out)           :: trans(:)     !! Output: Transformed observations.
        real(kind=r8), allocatable, intent(out), optional :: params(:)    !! Output: Transformation parameters.
        real(kind=r8), allocatable, intent(out), optional :: residuals(:) !! Output: Residuals.
        real(kind=r8),              intent(out), optional :: variance     !! Output: Reference variance of adjustment.
        real(kind=r8),              intent(out), optional :: rotation     !! Output: Rotation angle.
        real(kind=r8),              intent(out), optional :: scale_factor !! Output: Scale factor.

        real(kind=r8), allocatable :: a(:, :), x(:), r(:)
        real(kind=r8), allocatable :: b(:, :), l(:)
        real(kind=r8)              :: s, v, w
        integer                    :: i, k, n, stat

        ! Allocate memory.
        rc = E_ALLOC
        n = size(cs); k = size(observs)
        allocate (a(n, NRHS), b(n, NRHS), l(n), r(n), trans(k), x(NRHS), stat=stat)
        if (stat /= 0) return

        ! Fill coefficient matrix A.
        do i = 1, n, 2
            a(    i, :) = [     cs(i), -cs(i + 1), 1.0_r8, 0.0_r8 ]
            a(i + 1, :) = [ cs(i + 1),      cs(i), 0.0_r8, 1.0_r8 ]
        end do

        ! Compute minimum-norm least squares solution to AX = L.
        b = a; l = ct
        call dm_la_gels(b, l)

        x = l(1:NRHS)         ! Solution [ a, b, Te, Tn ].
        w = atan2(x(1), x(2)) ! Rotation angle.
        w = modulo(w, PI2)    ! Add 400 gon if angle is negative.
        s = x(1) / cos(w)     ! Scale factor.
        r = matmul(a, x) - ct ! Residuals of observations.

        ! Adjustment's reference variance, given
        ! n observations and NRHS degrees of freedom.
        v = dot_product(r, r) / (n - NRHS)

        ! Transform observations from source (x, y) to target (E, N) system.
        do i = 1, k, 2
            trans(i)     = (s * cos(w)) * observs(i) - (s * sin(w)) * observs(i + 1) + x(3)
            trans(i + 1) = (s * sin(w)) * observs(i) + (s * cos(w)) * observs(i + 1) + x(4)
        end do

        if (present(params)) params = x
        if (present(residuals)) residuals = r
        if (present(variance)) variance = v
        if (present(rotation)) rotation = w
        if (present(scale_factor)) scale_factor = s

        rc = E_NONE
    end function dm_transform_coord_2d

    integer function dm_transform_polar_3d(vx, vy, vz, tx, ty, hz, v, dist_hz, x, y, z, azimuth) result(rc)
        !! Calculates coordinates (x, y, z) out of horizontal direction,
        !! vertical angle, and slope distance to a target point using a
        !! 3-dimensional polar transformation.
        real(kind=r8), intent(in)           :: vx      !! View point x.
        real(kind=r8), intent(in)           :: vy      !! View point y.
        real(kind=r8), intent(in)           :: vz      !! View point z.
        real(kind=r8), intent(in)           :: tx      !! Target x.
        real(kind=r8), intent(in)           :: ty      !! Target y.
        real(kind=r8), intent(in)           :: hz      !! Horizontal direction between view point and target point.
        real(kind=r8), intent(in)           :: v       !! Vertical angle between view point and target point.
        real(kind=r8), intent(in)           :: dist_hz !! Horizontal distance between view point and target point.
        real(kind=r8), intent(out)          :: x       !! Transformed x.
        real(kind=r8), intent(out)          :: y       !! Transformed y.
        real(kind=r8), intent(out)          :: z       !! Transformed z.
        real(kind=r8), intent(in), optional :: azimuth !! Global azimuth.

        real(kind=r8) :: dx, dy, dz
        real(kind=r8) :: t

        dx = tx - vx
        dy = ty - vy

        if (dx == 0.0) then
            if (dy > 0.0) then
                z = 0.5 * PI
            else if (dy < 0.0) then
                z = 1.5 * PI
            else
                ! View point position equals azimuth position.
                rc = E_INVALID
                return
            end if
        else
            z = modulo(atan2(dx, dy), PI2)
        end if

        if (present(azimuth)) then
            ! Remove the global azimuth angle of the sensor from the
            ! calculated local azimuth.
            if (z /= 0.0) z = z - azimuth
        end if

        ! Append the measured horizontal direction to the angle.
        t = z + hz

        ! Calculate coordinates of the target point.
        dx = dist_hz * sin(v) * cos(t)
        dy = dist_hz * sin(v) * sin(t)
        dz = dist_hz * cos(v)

        x = vx + dx
        y = vy + dy
        z = vz + dz

        rc = E_NONE
    end function dm_transform_polar_3d
end module dm_transform
