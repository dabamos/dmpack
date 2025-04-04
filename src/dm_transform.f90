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

    type, public :: transform_coordinate_2d_type
        real(kind=r8) :: a        = 0_r8
        real(kind=r8) :: b        = 0_r8
        real(kind=r8) :: scale    = 0_r8
        real(kind=r8) :: variance = 0_r8
    end type transform_coordinate_2d_type

    interface dm_transform_cartesian_to_polar_3d
        module procedure :: dm_transform_cartesian_to_polar_3d_array
        module procedure :: dm_transform_cartesian_to_polar_3d_scalar
    end interface dm_transform_cartesian_to_polar_3d

    interface dm_transform_polar_to_cartesian_3d
        module procedure :: dm_transform_polar_to_cartesian_3d_array
        module procedure :: dm_transform_polar_to_cartesian_3d_scalar
    end interface dm_transform_polar_to_cartesian_3d

    public :: dm_transform_coordinate_2d
    public :: dm_transform_polar_3d
    public :: dm_transform_cartesian_to_polar_3d
    public :: dm_transform_polar_to_cartesian_3d

    private :: dm_transform_cartesian_to_polar_3d_array
    private :: dm_transform_cartesian_to_polar_3d_scalar
    private :: dm_transform_polar_to_cartesian_3d_array
    private :: dm_transform_polar_to_cartesian_3d_scalar
contains
    integer function dm_transform_coordinate_2d(cs, ct, observs, trans, params, residuals, variance, rotation, scale_factor) result(rc)
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
    end function dm_transform_coordinate_2d

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
            if (dm_equals(dy, 0.0_r8)) then
                ! View point position equals azimuth position.
                rc = E_INVALID
                return
            else if (dy > 0.0) then
                z = 0.5 * PI
            else if (dy < 0.0) then
                z = 1.5 * PI
            end if
        else
            z = modulo(atan2(dy, dx), PI2)
        end if

        if (present(azimuth)) then
            ! Remove the global azimuth angle of the sensor from the
            ! calculated local azimuth.
            if (.not. dm_equals(z, 0.0_r8)) z = z - azimuth
        end if

        ! Append the measured horizontal direction to the angle.
        t = z + hz

        ! Calculate coordinates of the target point.
        call dm_transform_polar_to_cartesian_3d(r=hz_dist, omega=v, phi=t, x=dx, y=dy, z=dz)

        x = vx + dx
        y = vy + dy
        z = vz + dz

        rc = E_NONE
    end function dm_transform_polar_3d

    pure subroutine dm_transform_cartesian_to_polar_3d_array(c, p)
        !! Transforms cartesian coordinates to polar (spherical) coordinates.
        !! Array `c` must contain `[ x, y, z ]`, array `p` will contain
        !! `[ r, omega, phi ]`.
        real(kind=r8), intent(in)  :: c(3) !! Cartesian coordinates (x, y, z).
        real(kind=r8), intent(out) :: p(3) !! Polar coordinates (r, omega, phi).

        real(kind=r8) :: r

        associate (x => c(1), y => c(2), z => c(3))
            r = norm2(c)
            p = [ r, acos(z / r), atan2(y, x) ]
        end associate
    end subroutine dm_transform_cartesian_to_polar_3d_array

    pure elemental subroutine dm_transform_cartesian_to_polar_3d_scalar(x, y, z, r, omega, phi)
        !! Transforms cartesian coordinates to polar (spherical) coordinates.
        real(kind=r8), intent(in)  :: x     !! X coordinate.
        real(kind=r8), intent(in)  :: y     !! Y coordinate.
        real(kind=r8), intent(in)  :: z     !! Z coordinate.
        real(kind=r8), intent(out) :: r     !! Radial distance.
        real(kind=r8), intent(out) :: omega !! Polar (vertical) angle [rad].
        real(kind=r8), intent(out) :: phi   !! Azimuthal (horizontal) angle [rad].

        r     = sqrt(x**2 + y**2 + z**2)
        omega = acos(z / r)
        phi   = atan2(y, x)
    end subroutine dm_transform_cartesian_to_polar_3d_scalar

    pure subroutine dm_transform_polar_to_cartesian_3d_array(p, c)
        !! Transforms polar (spherical) coordinates to cartesian coordinates.
        !! Array `p` must contain `[ r, omega, phi ]`, array `c` will contain
        !! `[ x, y, z ]`.
        real(kind=r8), intent(in)  :: p(3) !! Polar coordinates (r, omega, phi).
        real(kind=r8), intent(out) :: c(3) !! Cartesian coordinates (x, y, z).

        real(kind=r8) :: s

        associate (r => p(1), omega => p(2), phi => p(3))
            s = sin(omega)
            c = r * [ s, s, cos(omega) ] * [ cos(phi), sin(phi), 1.0_r8 ]
        end associate
    end subroutine dm_transform_polar_to_cartesian_3d_array

    pure elemental subroutine dm_transform_polar_to_cartesian_3d_scalar(r, omega, phi, x, y, z)
        !! Transforms polar (spherical) coordinates to cartesian coordinates.
        real(kind=r8), intent(in)  :: r     !! Radial distance.
        real(kind=r8), intent(in)  :: omega !! Polar (vertical) angle [rad].
        real(kind=r8), intent(in)  :: phi   !! Azimuthal (horizontal) angle [rad].
        real(kind=r8), intent(out) :: x     !! X coordinate.
        real(kind=r8), intent(out) :: y     !! Y coordinate.
        real(kind=r8), intent(out) :: z     !! Z coordinate.

        real(kind=r8) :: s

        s = sin(omega)
        x = r * s * cos(phi)
        y = r * s * sin(phi)
        z = r * cos(omega)
    end subroutine dm_transform_polar_to_cartesian_3d_scalar
end module dm_transform
