! Author:  Philipp Engel
! Licence: ISC
module dm_rts
    !! Parameters and procedures for processing of measurement data obtained
    !! from a robotic total station (RTS) by Leica Geosystems.
    !!
    !! The EDM distance correction functions use the default mean refraction
    !! coefficient 0.13, unless argument `k` is passed. The earth radius is
    !! assumed to be 6.389 * 10^6 m.
    !!
    !! The following code calculates scale correction values for the measured
    !! slope distance `slope_dist` from air temperature `temperature` [°C], air
    !! pressure `pressure` [mbar], relative humidity `humidity`, and instrument
    !! EDM height `height` [m]. The horizontal distance `hz_dist` [m] is
    !! determined using the corrected slope distance `corr_dist` [m] and the
    !! vertical angle `v` [rad]:
    !!
    !! ```fortran
    !! real(kind=r8) :: temperature, pressure, humidity, height
    !! real(kind=r8) :: ppm, ppm1, ppm2
    !! real(kind=r8) :: corr_dist, hz_dist, slope_dist, v
    !!
    !! temperature = 15.0_r8   ! [°C]
    !! pressure    = 1010.0_r8 ! [mbar]
    !! humidity    = 0.65_r8   ! [none]
    !! height      = 100.0_r8  ! [m]
    !! slope_dist  = 42.0_r8   ! [m]
    !!
    !! v = dm_gon_to_rad(101.5_r8) ! [rad]
    !!
    !! ppm1 = dm_rts_correction_atmospheric(temperature, pressure, humidity)
    !! ppm2 = dm_rts_correction_sea_level(height)
    !! ppm  = ppm1 + ppm2
    !!
    !! corr_dist = dm_rts_correction_distance(slope_dist, ppm, RTS_PRISM_LEICA_STANDARD)
    !! hz_dist   = dm_rts_distance_horizontal(corr_dist, v)
    !! ```
    !!
    !! Formulas are taken from the Leica TM30/TS30 User Manual.
    use :: dm_kind
    implicit none (type, external)
    private

    ! Reflector constants for Leica total stations.
    real(kind=r8), parameter, public :: RTS_PRISM_LEICA_STANDARD      =  0.0_r8 !! Leica Standard Prism, GPR1 [mm].
    real(kind=r8), parameter, public :: RTS_PRISM_LEICA_MINI          = 17.5_r8 !! Leica Mini Prism, GMP101 [mm].
    real(kind=r8), parameter, public :: RTS_PRISM_LEICA_360           = 23.1_r8 !! Leica 360° Prism, GRZ4/GRZ122 [mm].
    real(kind=r8), parameter, public :: RTS_PRISM_LEICA_360_MINI      = 30.0_r8 !! Leica 360° Mini Prism, GRZ101 [mm].
    real(kind=r8), parameter, public :: RTS_PRISM_LEICA_TAPE          = 34.4_r8 !! Leica Reflector Tape S, M, L [mm].
    real(kind=r8), parameter, public :: RTS_PRISM_LEICA_REFLECTORLESS = 34.4_r8 !! Reflectorless [mm].
    real(kind=r8), parameter, public :: RTS_PRISM_LEICA_MACHINE_AUTO  = 28.1_r8 !! Machine Automation Power Prism, MPR122 [mm].

    real(kind=r8), parameter :: EARTH_RADIUS       = 6.378_r8 * 10e6 !! Default radius of the earth.
    real(kind=r8), parameter :: MEAN_REFRACT_COEFF = 0.13_r8         !! Default mean refraction coefficient.

    interface dm_rts_distance_std_dev
        !! Generic interface to standard deviation functions.
        module procedure :: dm_rts_distance_std_dev_array
        module procedure :: dm_rts_distance_std_dev_mean
    end interface

    public :: dm_rts_correction_atmospheric
    public :: dm_rts_correction_distance
    public :: dm_rts_correction_projection
    public :: dm_rts_correction_sea_level
    public :: dm_rts_distance_horizontal
    public :: dm_rts_distance_mean
    public :: dm_rts_distance_std_dev
    public :: dm_rts_distance_std_dev_array
    public :: dm_rts_distance_std_dev_mean
    public :: dm_rts_height_difference
contains
    pure elemental real(kind=r8) function dm_rts_correction_atmospheric(temperature, pressure, humidity) result(ppm)
        !! Returns atmospheric correction value [ppm]. Multiply by 10e-6 before
        !! applying the correction value to a slope distance [m]. If no
        !! relative humidity is passed, the default 60 % of the instrument is
        !! used instead.
        !!
        !! The slope distance displayed is correct if the scale correction in
        !! ppm, mm/km, which has been entered corresponds to the atmospheric
        !! conditions prevailing at the time of the measurement.
        !!
        !! The atmospheric correction includes:
        !!
        !! * adjustments for air pressure,
        !! * air temperature,
        !! * relative humidity.
        !!
        !! For highest precision distance measurements, the atmospheric
        !! correction should be determined with an accuracy of 1 ppm. The
        !! following parameters must be redetermined:
        !!
        !! * air temperature to 1 °C,
        !! * air pressure to 3 mbar,
        !! * relative humidity to 20 %.
        !!
        !! The air humidity influences the distance measurement if the climate
        !! is extremely hot and damp. For high precision measurements, the
        !! relative humidity must be measured and entered along with the air
        !! pressure and the temperature.
        !!
        !! If the basic value of 60 % relative humidity as used by the EDM is
        !! retained, the maximum possible error in the calculated atmospheric
        !! correction is 2 ppm, 2 mm/km.
        !!
        !! The formula for visible red laser is taken from the Leica TM30/TS30
        !! User Manual, p. 76.
        real(kind=r8), intent(in)           :: temperature !! Air temperature [°C].
        real(kind=r8), intent(in)           :: pressure    !! Air pressure [mbar, hPa].
        real(kind=r8), intent(in), optional :: humidity    !! Relative humidity from 0.0 to 1.0.

        real(kind=r8) :: a, b, c, d, x
        real(kind=r8) :: humidity_

        humidity_ = 0.6_r8
        if (present(humidity)) humidity_ = humidity

        a = 1 / 273.15_r8
        b = 1 + a * temperature
        c = (0.29525 * pressure) / b
        d = (4.126 * 10e-4 * humidity_) / b
        x = (7.5 * temperature / (237.3 + temperature)) + 0.7857

        ppm = 286.34 - (c - d * 10.0_r8**x)
    end function dm_rts_correction_atmospheric

    pure elemental real(kind=r8) function dm_rts_correction_distance(slope_dist, ppm, prism) result(corrected)
        !! Applied atmospheric correction [ppm] and prism constant [mmm] to
        !! uncorrected slope distance [m].
        real(kind=r8), intent(in) :: slope_dist !! Uncorrected slope distance [m].
        real(kind=r8), intent(in) :: ppm        !! Atmospheric scale correction [ppm, mm/km].
        real(kind=r8), intent(in) :: prism      !! Additive constant of the reflector [mm].

        corrected = slope_dist * (1 + (ppm * 10e-6)) + prism
    end function dm_rts_correction_distance

    pure elemental real(kind=r8) function dm_rts_correction_projection(east) result(ppm)
        !! The magnitude of the projection distortion is in accordance with the
        !! projection system used in a particular country, for which official
        !! tables are generally available. The used formula is valid for
        !! cylindrical projections such as that of Gauss-Krüger.
        !!
        !! In countries where the scale factor is not unity, this formula
        !! cannot be directly applied.
        !!
        !! Multiply by 10e-6 before applying the correction value to a slope
        !! distance [m].
        real(kind=r8), intent(in) :: east !! Easting, distance from projection zero line with the scale factor 1 [km].

        ppm = (east**2 / (2 * EARTH_RADIUS**2)) * 10e6
    end function dm_rts_correction_projection

    pure elemental real(kind=r8) function dm_rts_correction_sea_level(height) result(ppm)
        !! Returns sea level correction value [ppm] for an EDM of the given
        !! height above sea level [m]. The value of the result is always
        !! negative. Multiply by 10e-6 before applying the correction value
        !! to a slope distance [m].
        real(kind=r8), intent(in) :: height !! Height of EDM above sea level [m].

        ppm = -1 * (height / EARTH_RADIUS) * 10e6
    end function dm_rts_correction_sea_level

    pure elemental real(kind=r8) function dm_rts_distance_horizontal(slope_dist, v, k) result(hz_dist)
        !! Returns horizontal distance [m] from slope distance `slope_dist` [m]
        !! and vertical angle `v` [rad]. The default mean refraction
        !! coefficient `k` is 0.13.
        !!
        !! Earth curvature (1 / `EARTH_RADIUS`) and mean refraction coefficient
        !! (`k`) are automatically taken into account by Leica instruments when
        !! calculating the horizontal distance if enabed in the settings
        !! (Instrument Settings/TPS Corrections). The calculated horizontal
        !! distance relates to the instrument height and not to the reflector
        !! height.
        real(kind=r8), intent(in)           :: slope_dist !! Slope distance [m].
        real(kind=r8), intent(in)           :: v          !! Vertical cycle reading [rad].
        real(kind=r8), intent(in), optional :: k          !! Mean refraction coefficient.

        real(kind=r8) :: a, k_, x, y

        k_ = MEAN_REFRACT_COEFF
        if (present(k)) k_ = k

        a = (1 - (k_ / 2)) / EARTH_RADIUS
        x = slope_dist * cos(v)
        y = slope_dist * abs(sin(v))

        hz_dist = y - a * x * y
    end function dm_rts_distance_horizontal

    real(kind=r8) function dm_rts_distance_mean(dists) result(mean)
        !! Returns (slope) distance as arithmetic mean of all measurements.
        real(kind=r8), intent(inout) :: dists(:) !! Array of (slope) distances [m].

        mean = sum(dists) / size(dists, kind=i8)
    end function dm_rts_distance_mean

    real(kind=r8) function dm_rts_distance_std_dev_array(dists) result(std_dev)
        !! Returns standard deviation of a single slope distance measurement.
        real(kind=r8), intent(inout) :: dists(:) !! Array of (slope) distances [m].

        integer(kind=i8) :: n
        real(kind=r8)    :: a, b

        n = size(dists, kind=i8)
        a = sum(dists**2)
        b = sum(dists)**2 / n

        std_dev = sqrt((a - b) / (n - 1))
    end function dm_rts_distance_std_dev_array

    pure elemental real(kind=r8) function dm_rts_distance_std_dev_mean(s, n) result(std_dev)
        !! Returns the standard deviation of the arithmetic mean of the distance.
        real(kind=r8),    intent(in) :: s !! Standard deviation of a single measurement.
        integer(kind=i8), intent(in) :: n !! Number of measurements.

        std_dev = s / sqrt(real(n, kind=r8))
    end function dm_rts_distance_std_dev_mean

    pure elemental real(kind=r8) function dm_rts_height_difference(slope_dist, v, k) result(diff)
        !! Returns height difference [m] from slope distance `slope_dist` [m]
        !! and vertical angle `v` [rad]. The default mean refraction
        !! coefficient `k` is 0.13.
        !!
        !! Earth curvature (1 / `EARTH_RADIUS`) and mean refraction coefficient
        !! (`k`) are automatically taken into account by Leica instruments when
        !! calculating the height difference if enabled in the settings.
        real(kind=r8), intent(in)           :: slope_dist !! Slope distance [m].
        real(kind=r8), intent(in)           :: v          !! Vertical cycle reading [rad].
        real(kind=r8), intent(in), optional :: k          !! Mean refraction coefficient.

        real(kind=r8) :: b, k_, x, y

        k_ = MEAN_REFRACT_COEFF
        if (present(k)) k_ = k

        b = (1 - k_) / (2 * EARTH_RADIUS)
        x = slope_dist * cos(v)
        y = slope_dist * abs(sin(v))

        diff = x + b * y**2
    end function dm_rts_height_difference
end module dm_rts
