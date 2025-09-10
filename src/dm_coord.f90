! Author:  Philipp Engel
! Licence: ISC
module dm_coord
    use :: dm_kind
    implicit none (type, external)
    private

    !! Coordinate transformations.
    interface dm_coord_cartesian_to_polar_3d
        !! Generic 3D cartesian to polar cooordinate transformation routine.
        module procedure :: coord_cartesian_to_polar_3d_array
        module procedure :: coord_cartesian_to_polar_3d_scalar
    end interface dm_coord_cartesian_to_polar_3d

    interface dm_coord_polar_to_cartesian_3d
        !! Generic 3D polar to cartesian cooordinate transformation routine.
        module procedure :: coord_polar_to_cartesian_3d_array
        module procedure :: coord_polar_to_cartesian_3d_scalar
    end interface dm_coord_polar_to_cartesian_3d

    public :: dm_coord_cartesian_to_polar_3d
    public :: dm_coord_polar_to_cartesian_3d

    private :: coord_cartesian_to_polar_3d_array
    private :: coord_cartesian_to_polar_3d_scalar
    private :: coord_polar_to_cartesian_3d_array
    private :: coord_polar_to_cartesian_3d_scalar
contains
    pure subroutine coord_cartesian_to_polar_3d_array(c, p)
        !! Transforms cartesian coordinates to polar (spherical) coordinates.
        !! Array `c` must contain `[ x, y, z ]`, array `p` will contain
        !! `[ r, omega, phi ]`.
        real(r8), intent(in)  :: c(3) !! Cartesian coordinates (x, y, z).
        real(r8), intent(out) :: p(3) !! Polar coordinates (r, omega, phi).

        associate (r => norm2(c), x => c(1), y => c(2), z => c(3))
            p = [ r, acos(z / r), atan2(y, x) ]
        end associate
    end subroutine coord_cartesian_to_polar_3d_array

    pure elemental subroutine coord_cartesian_to_polar_3d_scalar(x, y, z, r, omega, phi)
        !! Transforms cartesian coordinates to polar (spherical) coordinates.
        real(r8), intent(in)  :: x     !! X coordinate.
        real(r8), intent(in)  :: y     !! Y coordinate.
        real(r8), intent(in)  :: z     !! Z coordinate.
        real(r8), intent(out) :: r     !! Radial distance.
        real(r8), intent(out) :: omega !! Polar (vertical) angle [rad].
        real(r8), intent(out) :: phi   !! Azimuthal (horizontal) angle [rad].

        r     = sqrt(x**2 + y**2 + z**2)
        omega = acos(z / r)
        phi   = atan2(y, x)
    end subroutine coord_cartesian_to_polar_3d_scalar

    pure subroutine coord_polar_to_cartesian_3d_array(p, c)
        !! Transforms polar (spherical) coordinates to cartesian coordinates.
        !! Array `p` must contain `[ r, omega, phi ]`, array `c` will contain
        !! `[ x, y, z ]`.
        real(r8), intent(in)  :: p(3) !! Polar coordinates (r, omega, phi).
        real(r8), intent(out) :: c(3) !! Cartesian coordinates (x, y, z).

        associate (r => p(1), s => sin(p(2)), omega => p(2), phi => p(3))
            c = r * [ s, s, cos(omega) ] * [ cos(phi), sin(phi), 1.0_r8 ]
        end associate
    end subroutine coord_polar_to_cartesian_3d_array

    pure elemental subroutine coord_polar_to_cartesian_3d_scalar(r, omega, phi, x, y, z)
        !! Transforms polar (spherical) coordinates to cartesian coordinates.
        real(r8), intent(in)  :: r     !! Radial distance.
        real(r8), intent(in)  :: omega !! Polar (vertical) angle [rad].
        real(r8), intent(in)  :: phi   !! Azimuthal (horizontal) angle [rad].
        real(r8), intent(out) :: x     !! X coordinate.
        real(r8), intent(out) :: y     !! Y coordinate.
        real(r8), intent(out) :: z     !! Z coordinate.

        real(r8) :: s

        s = sin(omega)
        x = r * s * cos(phi)
        y = r * s * sin(phi)
        z = r * cos(omega)
    end subroutine coord_polar_to_cartesian_3d_scalar
end module dm_coord
