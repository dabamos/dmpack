! Author:  Philipp Engel
! Licence: ISC
module dm_transform
    !! Coordinate transformations (unfinished).
    use :: dm_const
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    public :: dm_transform_coord_2d
    public :: dm_transform_polar_3d
    public :: dm_transform_cartesian_to_polar
    public :: dm_transform_polar_to_cartesian
contains
    integer function dm_transform_coord_2d(cs, ct, observs, trans, params, residuals, variance, rotation, scale_factor) result(rc)
        use :: dm_la

        integer, parameter :: NRHS = 4 !! Number of columns in matrix A.

        real(kind=r8),              intent(inout)         :: cs(:)        !! Common control points in source system.
        real(kind=r8),              intent(inout)         :: ct(:)        !! Common control points in target system.
        real(kind=r8),              intent(inout)         :: observs(:)   !! Observations.
        real(kind=r8), allocatable, intent(out)           :: trans(:)     !! Transformed observations.
        real(kind=r8), allocatable, intent(out), optional :: params(:)    !! Transformation parameters.
        real(kind=r8), allocatable, intent(out), optional :: residuals(:) !! Residuals.
        real(kind=r8),              intent(out), optional :: variance     !! Reference variance of adjustment.
        real(kind=r8),              intent(out), optional :: rotation     !! Rotation angle.
        real(kind=r8),              intent(out), optional :: scale_factor !! Scale factor.

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

        ! Adjustment's reference variance, given n observations and NRHS
        ! degrees of freedom.
        v = dot_product(r, r) / (n - NRHS)

        ! Functional model to transform observations from source (x, y) to
        ! target (E, N) system.
        do i = 1, k, 2
            trans(i)     = (s * cos(w)) * observs(i) - (s * sin(w)) * observs(i + 1) + x(3)
            trans(i + 1) = (s * sin(w)) * observs(i) + (s * cos(w)) * observs(i + 1) + x(4)
        end do

        if (present(params))       params       = x
        if (present(residuals))    residuals    = r
        if (present(variance))     variance     = v
        if (present(rotation))     rotation     = w
        if (present(scale_factor)) scale_factor = s

        rc = E_NONE
    end function dm_transform_coord_2d

    integer function dm_transform_polar_3d(vx, vy, vz, tx, ty, hz, v, hz_dist, x, y, z, azimuth) result(rc)
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
        real(kind=r8), intent(in)           :: hz_dist !! Horizontal distance between view point and target point.
        real(kind=r8), intent(out)          :: x       !! Transformed x.
        real(kind=r8), intent(out)          :: y       !! Transformed y.
        real(kind=r8), intent(out)          :: z       !! Transformed z.
        real(kind=r8), intent(in), optional :: azimuth !! Global azimuth.

        real(kind=r8) :: dx, dy, dz
        real(kind=r8) :: t

        dx = tx - vx
        dy = ty - vy

        if (dm_equals(dx, 0.0_r8)) then
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
            if (.not. dm_equals(z, 0.0_r8)) z = z - azimuth
        end if

        ! Append the measured horizontal direction to the angle.
        t = z + hz

        ! Calculate coordinates of the target point.
        call dm_transform_polar_to_cartesian(t, v, hz_dist, dx, dy, dz)

        x = vx + dx
        y = vy + dy
        z = vz + dz

        rc = E_NONE
    end function dm_transform_polar_3d

    pure elemental subroutine dm_transform_cartesian_to_polar(x, y, z, hz, v, r)
        !! Transforms cartesian coordinates to polar (spherical) coordinates.
        real(kind=r8), intent(in)  :: x  !! X coordinate.
        real(kind=r8), intent(in)  :: y  !! Y coordinate.
        real(kind=r8), intent(in)  :: z  !! Z coordinate.
        real(kind=r8), intent(out) :: hz !! Horizontal angle (phi) [rad].
        real(kind=r8), intent(out) :: v  !! Vertical angle (omega) [rad].
        real(kind=r8), intent(out) :: r  !! Radius.

        r  = sqrt(x**2 + y**2 + z**2)
        hz = atan2(y, x)
        v  = acos(z / r)
    end subroutine dm_transform_cartesian_to_polar

    pure elemental subroutine dm_transform_polar_to_cartesian(hz, v, r, x, y, z)
        !! Transforms polar (spherical) coordinates to cartesian coordinates.
        real(kind=r8), intent(in)  :: hz !! Horizontal angle (phi) [rad].
        real(kind=r8), intent(in)  :: v  !! Vertical angle (omega) [rad].
        real(kind=r8), intent(in)  :: r  !! Radius.
        real(kind=r8), intent(out) :: x  !! X coordinate.
        real(kind=r8), intent(out) :: y  !! Y coordinate.
        real(kind=r8), intent(out) :: z  !! Z coordinate.

        x = r * sin(v) * cos(hz)
        y = r * sin(v) * sin(hz)
        z = r * cos(v)
    end subroutine dm_transform_polar_to_cartesian
end module dm_transform
