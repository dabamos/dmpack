! dmtestrts.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestrts
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestrts'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        real(kind=r8) :: d_corr, d_hz, d_ref, d_slope
        real(kind=r8) :: delta, diff, east, height, v
        real(kind=r8) :: hum, press, temp
        real(kind=r8) :: ppm, ppm1, ppm2, ppm3, prism

        stat = TEST_FAILED

        ! Check sea level correction value.
        ppm = dm_rts_correction_sea_level(0.0_r8)
        if (.not. dm_equals(ppm, 0.0_r8)) return

        ! Set "measured" values and constants.
        east    = 1000.0_r8                ! Easting [km].
        d_ref   = 99.98769_r8              ! Reference hz distance [m].
        d_slope = 100.0_r8                 ! Measured slope distance [m].
        height  = 10.0_r8                  ! EDM height [m].
        v       = dm_gon_to_rad(99.0_r8)   ! Vertical angle [rad].
        prism   = RTS_PRISM_LEICA_STANDARD ! Prism constant [mm].
        temp    = 12.0_r8                  ! Air temperature [deg C].
        press   = 995.45_r8                ! Air pressure [mbar].
        hum     = 0.4_r8                   ! Humidity [%].

        ! Calculate correction values. Apply only 1 and 2.
        ppm1 = dm_rts_correction_atmospheric(temp, press, hum)
        ppm2 = dm_rts_correction_sea_level(height)
        ppm3 = dm_rts_correction_projection(east)
        ppm  = ppm1 + ppm2

        ! Correct the slope distance and calculate the horizontal distance.
        d_corr = dm_rts_correction_distance(d_slope, ppm, prism)
        d_hz   = dm_rts_distance_horizontal(d_corr, v)
        diff   = dm_rts_height_difference(d_corr, v)
        delta  = d_ref - d_hz

        print '(" Slope distance........: ", f10.5, " m")',   d_slope
        print '(" EDM height............: ", f10.5, " m")',   height
        print '(" Vertical angle........: ", f10.5, " gon")', dm_rad_to_gon(v)

        print '(" Air temperature.......: ", f10.5, " deg C")', temp
        print '(" Air pressure..........: ", f10.5, " mbar")',  press
        print '(" Relative humidity.....: ", f10.5, " %")',     hum * 100

        print '(" Prism constant........: ", f10.5, " mm")',  prism
        print '(" Atmospheric correction: ", f10.5, " ppm")', ppm1
        print '(" Sea level correction..: ", f10.5, " ppm")', ppm2
        print '(" Projection distortion.: ", f10.5, " ppm")', ppm3
        print '(" Distance correction...: ", f10.5, " ppm")', ppm
        print '(" Corrected distance....: ", f10.5, " m")',   d_corr
        print '(" Height difference.....: ", f10.5, " m")',   diff
        print '(" Reference Hz distance.: ", f10.5, " m")',   d_ref
        print '(" Hz distance...........: ", f10.5, " m")',   d_hz
        print '(" Hz distance delta.....: ", f10.5, " m")',   delta

        if (abs(delta) > 0.00001_r8) return

        stat = TEST_PASSED
    end function test01
end program dmtestrts
