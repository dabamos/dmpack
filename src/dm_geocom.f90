! Author:  Philipp Engel
! Licence: ISC
module dm_geocom
    !! Object-oriented GeoCOM API for Fortran.
    !!
    !! The API provided by DMPACK does not follow the official Leica GeoCOM API
    !! for C/C++ and Visual Basic. Structured types and functions are simplified
    !! and given more memorable names. Function names do not contain a sub-system
    !! prefix.
    !!
    !! References to the official API are made in the procedure descriptions.
    !!
    !! ```fortran
    !! integer            :: rc     ! DMPACK return code.
    !! type(geocom_class) :: geocom ! GeoCOM object.
    !!
    !! call geocom%initialize()
    !! rc = geocom%open('/dev/ttyUSB0', TTY_B115200, nretries=1)
    !!
    !! if (dm_is_ok(rc)) then
    !!     rc = geocom%beep_normal()
    !!     print '(i0, ": ", a)', geocom%code(), geocom%message()
    !! end if
    !!
    !! call geocom%close()
    !! call geocom%destroy()
    !! ```
    use :: dm_error
    use :: dm_geocom_api
    use :: dm_geocom_error
    use :: dm_kind
    use :: dm_request
    use :: dm_response
    use :: dm_tty
    implicit none (type, external)
    private

    type, public :: geocom_class
        !! GeoCOM class for TTY access and GeoCOM API handling through the
        !! public methods.
        private
        integer        :: grc     = GRC_OK !! Last GeoCOM return code.
        logical        :: verbose = .true. !! Print error messages to stderr.
        type(tty_type) :: tty              !! TTY type for serial connection to sensor.
    contains
        procedure, public :: beep_alarm  => geocom_beep_alarm
        procedure, public :: beep_normal => geocom_beep_normal
        procedure, public :: beep_off    => geocom_beep_off
        procedure, public :: beep_on     => geocom_beep_on
        procedure, public :: close       => geocom_close
        procedure, public :: code        => geocom_code
        procedure, public :: destroy     => geocom_destroy
        procedure, public :: initialize  => geocom_initialize
        procedure, public :: message     => geocom_message
        procedure, public :: open        => geocom_open
        procedure, public :: send        => geocom_send
    end type geocom_class

    ! Private methods.
    private :: geocom_close
    private :: geocom_code
    private :: geocom_destroy
    private :: geocom_initialize
    private :: geocom_message
    private :: geocom_open
    private :: geocom_send

    ! Private GeoCOM methods.

    ! private :: geocom_abort_download()
    ! private :: geocom_abort_list()
    private :: geocom_beep_alarm
    private :: geocom_beep_normal
    private :: geocom_beep_off
    private :: geocom_beep_on
    ! private :: geocom_change_face(pos_mode, atr_mode)
    ! private :: geocom_delete(device_type, file_type, del_date, file_name, nfiles)
    ! private :: geocom_do_measure(command, mode)
    ! private :: geocom_do_position(hz, v, pos_mode, atr_mode)
    ! private :: geocom_download(block_number, block)
    ! private :: geocom_fine_adjust(hz, v)
    ! private :: geocom_get_angle1(angle, mode)
    ! private :: geocom_get_angle5(only_angle, mode)
    ! private :: geocom_get_angular_correction(incline, stand_axis, collimation, tilt_axis)
    ! private :: geocom_get_atmospheric_correction(atmos)
    ! private :: geocom_get_atmospheric_ppm(ppm)
    ! private :: geocom_get_atr_error(error)
    ! private :: geocom_get_atr_setting(setting)
    ! private :: geocom_get_binary_status(available)
    ! private :: geocom_get_config(auto_power, timeout)
    ! private :: geocom_get_coordinate(wait_time, coordinate, mode)
    ! private :: geocom_get_date_time(dt)
    ! private :: geocom_get_date_time_centi(year, month, day, hour, minute, second, centisecond)
    ! private :: geocom_get_device_config(device)
    ! private :: geocom_get_double_precision(ndigits)
    ! private :: geocom_get_edm_mode(mode)
    ! private :: geocom_get_egl_intensity(intensity)
    ! private :: geocom_get_face(face)
    ! private :: geocom_get_fine_adjust_mode(mode)
    ! private :: geocom_get_full_measurement(wait_time, hz, v, accuracy, cross_incl, length_incl, accuracy_incl, slope_dist, dist_time, mode)
    ! private :: geocom_get_geometric_ppm(automatic, scale_factor, offset, height, individual)
    ! private :: geocom_get_height(height)
    ! private :: geocom_get_image_config(mem_type, parameters)
    ! private :: geocom_get_incline_correction(mode)
    ! private :: geocom_get_incline_error(error)
    ! private :: geocom_get_instrument_name(name)
    ! private :: geocom_get_instrument_number(serial)
    ! private :: geocom_get_internal_temperature(temperature)
    ! private :: geocom_get_lock_status(status)
    ! private :: geocom_get_measurement_program(prog)
    ! private :: geocom_get_power(capacity, active, suggest)
    ! private :: geocom_get_prism_definition(type, definition)
    ! private :: geocom_get_prism_type(type)
    ! private :: geocom_get_prism_type2(type, name)
    ! private :: geocom_get_quick_distance(only_angle, slope_dist)
    ! private :: geocom_get_reduced_atr_fov(mode)
    ! private :: geocom_get_reflectorless_class(class)
    ! private :: geocom_get_refractive_correction(coefficient)
    ! private :: geocom_get_refractive_method(method)
    ! private :: geocom_get_search_area(area)
    ! private :: geocom_get_set_laser_pointer(mode)
    ! private :: geocom_get_signal(signal)
    ! private :: geocom_get_simple_coordinates(wait_time, e, n, h, prog)
    ! private :: geocom_get_simple_meta(wait_time, only_angle, slope_dist, mode)
    ! private :: geocom_get_slope_distance_correction(ppm_corr, prism_corr)
    ! private :: geocom_get_software_version(release, version, subversion)
    ! private :: geocom_get_software_version2(release, version, subversion)
    ! private :: geocom_get_station(station)
    ! private :: geocom_get_target_type(type)
    ! private :: geocom_get_timeout(hz, v)
    ! private :: geocom_get_tolerance(hz, v)
    ! private :: geocom_get_user_atr_mode(mode)
    ! private :: geocom_get_user_local_mode(mode)
    ! private :: geocom_get_user_prism_definition(name, add_const, type, creator)
    ! private :: geocom_get_user_spiral(hz, v)
    ! private :: geocom_list(next, last, dir_info)
    ! private :: geocom_lock_in()
    ! private :: geocom_measure_distance_angle(mode, hz, v, slope_dist)
    ! private :: geocom_null_proc()
    ! private :: geocom_ps_enable_range(enable)
    ! private :: geocom_ps_search_next(direction, swing)
    ! private :: geocom_ps_search_window()
    ! private :: geocom_ps_set_range(min_dist, max_dist)
    ! private :: geocom_search(hz, v)
    ! private :: geocom_search_target()
    ! private :: geocom_set_angle_correction(incline, stand_axis, collimation, tilt_axis)
    ! private :: geocom_set_atmospheric_correction(atmos)
    ! private :: geocom_set_atmospheric_ppm(ppm)
    ! private :: geocom_set_atr_setting(setting)
    ! private :: geocom_set_binary_available(available)
    ! private :: geocom_set_config(auto_power, timeout)
    ! private :: geocom_set_date_time(dt)
    ! private :: geocom_set_double_precision(ndigits)
    ! private :: geocom_set_edm_mode(mode)
    ! private :: geocom_set_egl_intensity(intensity)
    ! private :: geocom_set_fine_adjust_mode(mode)
    ! private :: geocom_set_geometric_ppm(automatic, scale_factor, offset, height, individual)
    ! private :: geocom_set_height(height)
    ! private :: geocom_set_image_config(mem_type, parameters)
    ! private :: geocom_set_incline_correction(mode)
    ! private :: geocom_set_measurement_program(prog)
    ! private :: geocom_set_offset(slope_dist, height, mode)
    ! private :: geocom_set_orientation(hz)
    ! private :: geocom_set_prism_correction(prism_corr)
    ! private :: geocom_set_prism_type(type)
    ! private :: geocom_set_prism_type2(type, name)
    ! private :: geocom_set_reduced_atr_fov(mode)
    ! private :: geocom_set_refractive_correction(coefficient)
    ! private :: geocom_set_refractive_method(method)
    ! private :: geocom_set_search_area(area)
    ! private :: geocom_set_station(station)
    ! private :: geocom_set_target_type(type)
    ! private :: geocom_set_timeout(hz, v)
    ! private :: geocom_set_tolerance(hz, v)
    ! private :: geocom_set_user_atr_mode(mode)
    ! private :: geocom_set_user_local_mode(mode)
    ! private :: geocom_set_user_prism_definition(name, add_const, type, creator)
    ! private :: geocom_set_user_spiral(hz, v)
    ! private :: geocom_set_velocity(omega)
    ! private :: geocom_setup_download(device_type, file_type, file_name, block_size, nblocks)
    ! private :: geocom_setup_list(device_type, file_type, search_path)
    ! private :: geocom_start_controller(mode)
    ! private :: geocom_stop_controller(mode)
    ! private :: geocom_switch_off(mode)
    ! private :: geocom_switch_on(mode)
    ! private :: geocom_take_image(mem_type, n)
contains
    ! **************************************************************************
    ! PRIVATE METHODS.
    ! **************************************************************************
    subroutine geocom_close(this)
        !! Closes TTY connection.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        if (dm_tty_connected(this%tty)) call dm_tty_close(this%tty)
    end subroutine geocom_close

    integer function geocom_code(this) result(grc)
        !! Returns last GeoCOM return code.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        grc = this%grc
    end function geocom_code

    subroutine geocom_destroy(this)
        !! Class destructor.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        if (dm_tty_connected(this%tty)) call dm_tty_close(this%tty)

        ! Reset derived type.
        this%grc = GRC_OK
        this%tty = tty_type()
    end subroutine geocom_destroy

    subroutine geocom_initialize(this)
        !! Constructor.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        return
    end subroutine geocom_initialize

    function geocom_message(this) result(str)
        !! Returns message associated with last GeoCOM return code as
        !! allocatable string.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.
        character(len=:), allocatable      :: str  !! Last return code message.

        str = dm_geocom_error_message(this%grc)
    end function geocom_message

    integer function geocom_open(this, path, baud_rate, nretries) result(rc)
        !! Opens TTY connection to robotic total station.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if baud rate is invalid.
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        character(len=*),    intent(in)           :: path      !! Path of TTY.
        integer,             intent(in)           :: baud_rate !! Baud rate enumerator.
        integer,             intent(in), optional :: nretries  !! Number of retries

        integer :: nretries_

        nretries_ = 0
        if (present(nretries)) nretries_ = nretries

        rc = E_INVALID
        if (.not. dm_tty_valid_baud_rate(baud_rate)) return

        rc = E_NONE

        !! ... TODO ...
    end function geocom_open

    integer function geocom_send(this, request) result(rc)
        !! Sends request to configured TTY.
        class(geocom_class), intent(inout) :: this    !! GeoCOM object.
        type(request_type),  intent(inout) :: request !! Request to send.

        integer :: grc

        !! Send request to sensor.
        !! ... TODO ...

        !! Get GeoCOM return code from response.
        this%grc = GRC_UNDEFINED
        call dm_request_get(request, 'grc', grc, error=rc)
        if (dm_is_ok(rc)) this%grc = grc
    end function geocom_send

    ! **************************************************************************
    ! PRIVATE GEOCOM METHODS.
    ! **************************************************************************
    integer function geocom_beep_alarm(this) result(rc)
        !! BMM_BeepAlarm
        !!
        !! Sends request to output an alarm signal (triple beep).
        class(geocom_class), intent(inout) :: this !! GeoCOM object.
        type(request_type)                 :: request

        call dm_geocom_api_request_beep_alarm(request)
        rc = this%send(request)
    end function geocom_beep_alarm

    integer function geocom_beep_normal(this) result(rc)
        !! BMM_BeepNormal
        !!
        !! Sends request to output an alarm signal (single beep).
        class(geocom_class), intent(inout) :: this !! GeoCOM object.
        type(request_type)                 :: request

        call dm_geocom_api_request_beep_normal(request)
        rc = this%send(request)
    end function geocom_beep_normal

    integer function geocom_beep_off(this) result(rc)
        !! IOS_BeepOff
        !!
        !! Sends request to stop an active beep signal.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.
        type(request_type)                 :: request

        call dm_geocom_api_request_beep_off(request)
        rc = this%send(request)
    end function geocom_beep_off

    integer function geocom_beep_on(this, intensity) result(rc)
        !! IOS_BeepOn
        !!
        !! Sends request to start a continuous beep signal of given intensity.
        !!
        !! The optional intensity must be between 0 and 100. If no intensity
        !! is passed, the default (`GEOCOM_IOS_BEEP_STDINTENS`) is used.
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        integer,             intent(in), optional :: intensity !! Intensity of beep.
        type(request_type)                        :: request

        if (present(intensity)) then
            call dm_geocom_api_request_beep_on(request, intensity)
        else
            call dm_geocom_api_request_beep_on(request)
        end if

        rc = this%send(request)
    end function geocom_beep_on

    ! geocom_get_double_precision(ndigits)
    ! geocom_set_double_precision(ndigits)
    ! geocom_get_user_atr_mode(mode)
    ! geocom_set_user_atr_mode(mode)
    ! geocom_get_user_local_mode(mode)
    ! geocom_set_user_local_mode(mode)
    ! geocom_get_tolerance(hz, v)
    ! geocom_set_tolerance(hz, v)
    ! geocom_get_timeout(hz, v)
    ! geocom_set_timeout(hz, v)
    ! geocom_do_position(hz, v, pos_mode, atr_mode)
    ! geocom_change_face(pos_mode, atr_mode)
    ! geocom_fine_adjust(hz, v)
    ! geocom_search(hz, v)
    ! geocom_get_fine_adjust_mode(mode)
    ! geocom_set_fine_adjust_mode(mode)
    ! geocom_lock_in()
    ! geocom_get_search_area(area)
    ! geocom_set_search_area(area)
    ! geocom_get_user_spiral(hz, v)
    ! geocom_set_user_spiral(hz, v)
    ! geocom_ps_enable_range(enable)
    ! geocom_ps_set_range(min_dist, max_dist)
    ! geocom_ps_search_window()
    ! geocom_ps_search_next(direction, swing)
    ! geocom_get_target_type(type)
    ! geocom_set_target_type(type)
    ! geocom_get_prism_type(type)
    ! geocom_set_prism_type(type)
    ! geocom_get_prism_type2(type, name)
    ! geocom_set_prism_type2(type, name)
    ! geocom_get_prism_definition(type, definition)
    ! geocom_get_user_prism_definition(name, add_const, type, creator)
    ! geocom_set_user_prism_definition(name, add_const, type, creator)
    ! geocom_get_measurement_program(prog)
    ! geocom_set_measurement_program(prog)
    ! geocom_measure_distance_angle(mode, hz, v, slope_dist)
    ! geocom_search_target()
    ! geocom_get_atr_setting(setting)
    ! geocom_set_atr_setting(setting)
    ! geocom_get_reduced_atr_fov(mode)
    ! geocom_set_reduced_atr_fov(mode)
    ! geocom_beep_alarm()
    ! geocom_beep_normal()
    ! geocom_beep_on()
    ! geocom_beep_off()
    ! geocom_get_software_version(release, version, subversion)
    ! geocom_switch_on(mode)
    ! geocom_switch_off(mode)
    ! geocom_null_proc()
    ! geocom_get_binary_available(available)
    ! geocom_set_binary_available(available)
    ! geocom_get_instrument_number(serial)
    ! geocom_get_instrument_name(name)
    ! geocom_get_device_config(device)
    ! geocom_get_reflectorless_class(class)
    ! geocom_get_date_time(dt)
    ! geocom_set_date_time(dt)
    ! geocom_get_software_version2(release, version, subversion)
    ! geocom_get_power(capacity, active, suggest)
    ! geocom_get_internal_temperature(temperature)
    ! geocom_get_date_time_centi(year, month, day, hour, minute, second, centisecond)
    ! geocom_get_set_laser_pointer(mode)
    ! geocom_get_egl_intensity(intensity)
    ! geocom_set_egl_intensity(intensity)
    ! geocom_setup_list(device_type, file_type, search_path)
    ! geocom_list(next, last, dir_info)
    ! geocom_abort_list()
    ! geocom_setup_download(device_type, file_type, file_name, block_size, nblocks)
    ! geocom_download(block_number, block)
    ! geocom_abort_download()
    ! geocom_delete(device_type, file_type, del_date, file_name, nfiles)
    ! geocom_get_image_config(mem_type, parameters)
    ! geocom_set_image_config(mem_type, parameters)
    ! geocom_take_image(mem_type, n)
    ! geocom_get_lock_status(status)
    ! geocom_start_controller(mode)
    ! geocom_stop_controller(mode)
    ! geocom_set_velocity(omega)
    ! geocom_get_config(auto_power, timeout)
    ! geocom_set_config(auto_power, timeout)
    ! geocom_get_coordinate(wait_time, coordinate, mode)
    ! geocom_get_simple_meta(wait_time, only_angle, slope_dist, mode)
    ! geocom_get_angle1(angle, mode)
    ! geocom_get_angle5(only_angle, mode)
    ! geocom_get_quick_distance(only_angle, slope_dist)
    ! geocom_get_full_measurement(wait_time, hz, v, accuracy, cross_incl, length_incl, accuracy_incl, slope_dist, dist_time, mode)
    ! geocom_do_measure(command, mode)
    ! geocom_set_offset(slope_dist, height, mode)
    ! geocom_get_height(height)
    ! geocom_set_height(height)
    ! geocom_get_atmospheric_correction(atmos)
    ! geocom_set_atmospheric_correction(atmos)
    ! geocom_set_orientation(hz)
    ! geocom_set_prism_correction(prism_corr)
    ! geocom_get_refractive_correction(coefficient)
    ! geocom_set_refractive_correction(coefficient)
    ! geocom_get_refractive_method(method)
    ! geocom_set_refractive_method(method)
    ! geocom_get_station(station)
    ! geocom_set_station(station)
    ! geocom_get_atmospheric_ppm(ppm)
    ! geocom_set_atmospheric_ppm(ppm)
    ! geocom_get_geometric_ppm(automatic, scale_factor, offset, height, individual)
    ! geocom_set_geometric_ppm(automatic, scale_factor, offset, height, individual)
    ! geocom_get_face(face)
    ! geocom_get_signal(signal)
    ! geocom_get_angular_correction(incline, stand_axis, collimation, tilt_axis)
    ! geocom_get_incline_correction(mode)
    ! geocom_set_incline_correction(mode)
    ! geocom_get_edm_mode(mode)
    ! geocom_set_edm_mode(mode)
    ! geocom_get_simple_coordinates(wait_time, e, n, h, prog)
    ! geocom_get_atr_error(error)
    ! geocom_get_incline_error(error)
    ! geocom_set_angle_correction(incline, stand_axis, collimation, tilt_axis)
    ! geocom_get_slope_distance_correction(ppm_corr, prism_corr)
end module dm_geocom
