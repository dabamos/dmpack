! Author:  Philipp Engel
! Licence: ISC
module dm_lua_geocom
    !! GeoCOM API for Lua.
    use, intrinsic :: iso_c_binding
    use :: dm_error
    use :: dm_geocom_api
    use :: dm_kind
    use :: dm_lua
    use :: dm_request
    implicit none (type, external)
    private

    ! Public procedures.
    public  :: dm_lua_geocom_register

    ! Private procedures.
    private :: lua_geocom_abort_download
    private :: lua_geocom_abort_list
    private :: lua_geocom_beep_alarm
    private :: lua_geocom_beep_normal
    private :: lua_geocom_beep_off
    private :: lua_geocom_beep_on
    private :: lua_geocom_change_face
    private :: lua_geocom_delete
    private :: lua_geocom_do_measure
    private :: lua_geocom_download
    private :: lua_geocom_fine_adjust
    private :: lua_geocom_get_angle
    private :: lua_geocom_get_angle_complete
    private :: lua_geocom_get_angular_correction_status
    private :: lua_geocom_get_atmospheric_correction
    private :: lua_geocom_get_atmospheric_ppm
    private :: lua_geocom_get_atr_error
    private :: lua_geocom_get_atr_setting
    private :: lua_geocom_get_binary_mode
    private :: lua_geocom_get_config
    private :: lua_geocom_get_coordinate
    private :: lua_geocom_get_date_time
    private :: lua_geocom_get_date_time_centi
    private :: lua_geocom_get_device_config
    private :: lua_geocom_get_double_precision
    private :: lua_geocom_get_edm_mode
    private :: lua_geocom_get_egl_intensity
    private :: lua_geocom_get_face
    private :: lua_geocom_get_fine_adjust_mode
    private :: lua_geocom_get_full_measurement
    private :: lua_geocom_get_geocom_version
    private :: lua_geocom_get_geometric_ppm
    private :: lua_geocom_get_height
    private :: lua_geocom_get_image_config
    private :: lua_geocom_get_inclination_correction
    private :: lua_geocom_get_inclination_error
    private :: lua_geocom_get_instrument_name
    private :: lua_geocom_get_instrument_number
    private :: lua_geocom_get_internal_temperature
    private :: lua_geocom_get_lock_status
    private :: lua_geocom_get_measurement_program
    private :: lua_geocom_get_power
    private :: lua_geocom_get_prism_constant
    private :: lua_geocom_get_prism_definition
    private :: lua_geocom_get_prism_type
    private :: lua_geocom_get_prism_type_v2
    private :: lua_geocom_get_quick_distance
    private :: lua_geocom_get_reduced_atr_fov
    private :: lua_geocom_get_reflectorless_class
    private :: lua_geocom_get_refraction_mode
    private :: lua_geocom_get_search_area
    private :: lua_geocom_get_signal
    private :: lua_geocom_get_simple_coordinates
    private :: lua_geocom_get_simple_measurement
    private :: lua_geocom_get_slope_distance_correction
    private :: lua_geocom_get_software_version
    private :: lua_geocom_get_station
    private :: lua_geocom_get_target_type
    private :: lua_geocom_get_timeout
    private :: lua_geocom_get_tolerance
    private :: lua_geocom_get_user_atr_mode
    private :: lua_geocom_get_user_lock_mode
    private :: lua_geocom_get_user_prism_definition
    private :: lua_geocom_get_user_spiral
    private :: lua_geocom_list
    private :: lua_geocom_lock_in
    private :: lua_geocom_null
    private :: lua_geocom_ps_enable_range
    private :: lua_geocom_ps_search_next
    private :: lua_geocom_ps_search_window
    private :: lua_geocom_ps_set_range
    private :: lua_geocom_search
    private :: lua_geocom_search_target
    private :: lua_geocom_set_angle_correction
    private :: lua_geocom_set_atmospheric_correction
    private :: lua_geocom_set_atmospheric_ppm
    private :: lua_geocom_set_atr_mode
    private :: lua_geocom_set_binary_mode
    private :: lua_geocom_set_config
    private :: lua_geocom_set_date_time
    private :: lua_geocom_set_distance
    private :: lua_geocom_set_double_precision
    private :: lua_geocom_set_edm_mode
    private :: lua_geocom_set_egl_intensity
    private :: lua_geocom_set_fine_adjust_mode
    private :: lua_geocom_set_geometric_ppm
    private :: lua_geocom_set_height
    private :: lua_geocom_set_image_config
    private :: lua_geocom_set_inclination_correction
    private :: lua_geocom_set_laser_pointer
    private :: lua_geocom_set_measurement_program
    private :: lua_geocom_set_orientation
    private :: lua_geocom_set_position
    private :: lua_geocom_set_positioning_timeout
    private :: lua_geocom_set_prism_constant
    private :: lua_geocom_set_prism_type
    private :: lua_geocom_set_prism_type_v2
    private :: lua_geocom_set_reduced_atr_fov
    private :: lua_geocom_set_refraction_mode
    private :: lua_geocom_set_search_area
    private :: lua_geocom_set_station
    private :: lua_geocom_set_target_type
    private :: lua_geocom_set_tolerance
    private :: lua_geocom_set_user_atr_mode
    private :: lua_geocom_set_user_lock_mode
    private :: lua_geocom_set_user_prism_definition
    private :: lua_geocom_set_user_spiral
    private :: lua_geocom_set_velocity
    private :: lua_geocom_setup_download
    private :: lua_geocom_setup_list
    private :: lua_geocom_start_controller
    private :: lua_geocom_stop_controller
    private :: lua_geocom_switch_off
    private :: lua_geocom_switch_on
    private :: lua_geocom_take_image
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_lua_geocom_register(lua) result(rc)
        !! Registers all GeoCOM API functions. Returns `E_INVALID` if the Lua
        !! interpreter is not initialised, or `E_LUA` if the registration
        !! failed.
        !!
        !! The following Lua procedures are registered:
        !!
        !! * `geocom_abort_download()`
        !! * `geocom_abort_list()`
        !! * `geocom_beep_alarm()`
        !! * `geocom_beep_normal()`
        !! * `geocom_beep_off()`
        !! * `geocom_beep_on()`
        !! * `geocom_change_face()`
        !! * `geocom_delete()`
        !! * `geocom_do_measure()`
        !! * `geocom_download()`
        !! * `geocom_fine_adjust()`
        !! * `geocom_get_angle()`
        !! * `geocom_get_angle_complete()`
        !! * `geocom_get_angular_correction_status()`
        !! * `geocom_get_atmospheric_correction()`
        !! * `geocom_get_atmospheric_ppm()`
        !! * `geocom_get_atr_error()`
        !! * `geocom_get_atr_setting()`
        !! * `geocom_get_binary_mode()`
        !! * `geocom_get_config()`
        !! * `geocom_get_coordinate()`
        !! * `geocom_get_date_time()`
        !! * `geocom_get_date_time_centi()`
        !! * `geocom_get_device_config()`
        !! * `geocom_get_double_precision()`
        !! * `geocom_get_edm_mode()`
        !! * `geocom_get_egl_intensity()`
        !! * `geocom_get_face()`
        !! * `geocom_get_fine_adjust_mode()`
        !! * `geocom_get_full_measurement()`
        !! * `geocom_get_geocom_version()`
        !! * `geocom_get_geometric_ppm()`
        !! * `geocom_get_height()`
        !! * `geocom_get_image_config()`
        !! * `geocom_get_inclination_correction()`
        !! * `geocom_get_inclination_error()`
        !! * `geocom_get_instrument_name()`
        !! * `geocom_get_instrument_number()`
        !! * `geocom_get_internal_temperature()`
        !! * `geocom_get_lock_status()`
        !! * `geocom_get_measurement_program()`
        !! * `geocom_get_power()`
        !! * `geocom_get_prism_constant()`
        !! * `geocom_get_prism_definition()`
        !! * `geocom_get_prism_type()`
        !! * `geocom_get_prism_type_v2()`
        !! * `geocom_get_quick_distance()`
        !! * `geocom_get_reduced_atr_fov()`
        !! * `geocom_get_reflectorless_class()`
        !! * `geocom_get_refraction_mode()`
        !! * `geocom_get_search_area()`
        !! * `geocom_get_signal()`
        !! * `geocom_get_simple_coordinates()`
        !! * `geocom_get_simple_measurement()`
        !! * `geocom_get_slope_distance_correction()`
        !! * `geocom_get_software_version()`
        !! * `geocom_get_station()`
        !! * `geocom_get_target_type()`
        !! * `geocom_get_timeout()`
        !! * `geocom_get_tolerance()`
        !! * `geocom_get_user_atr_mode()`
        !! * `geocom_get_user_lock_mode()`
        !! * `geocom_get_user_prism_definition()`
        !! * `geocom_get_user_spiral()`
        !! * `geocom_list()`
        !! * `geocom_lock_in()`
        !! * `geocom_null()`
        !! * `geocom_ps_enable_range()`
        !! * `geocom_ps_search_next()`
        !! * `geocom_ps_search_window()`
        !! * `geocom_ps_set_range()`
        !! * `geocom_search()`
        !! * `geocom_search_target()`
        !! * `geocom_set_angle_correction()`
        !! * `geocom_set_atmospheric_correction()`
        !! * `geocom_set_atmospheric_ppm()`
        !! * `geocom_set_atr_mode()`
        !! * `geocom_set_binary_mode()`
        !! * `geocom_set_config()`
        !! * `geocom_set_date_time()`
        !! * `geocom_set_distance()`
        !! * `geocom_set_double_precision()`
        !! * `geocom_set_edm_mode()`
        !! * `geocom_set_egl_intensity()`
        !! * `geocom_set_fine_adjust_mode()`
        !! * `geocom_set_geometric_ppm()`
        !! * `geocom_set_height()`
        !! * `geocom_set_image_config()`
        !! * `geocom_set_inclination_correction()`
        !! * `geocom_set_laser_pointer()`
        !! * `geocom_set_measurement_program()`
        !! * `geocom_set_orientation()`
        !! * `geocom_set_position()`
        !! * `geocom_set_positioning_timeout()`
        !! * `geocom_set_prism_constant()`
        !! * `geocom_set_prism_type()`
        !! * `geocom_set_prism_type_v2()`
        !! * `geocom_set_reduced_atr_fov()`
        !! * `geocom_set_refraction_mode()`
        !! * `geocom_set_search_area()`
        !! * `geocom_set_station()`
        !! * `geocom_set_target_type()`
        !! * `geocom_set_tolerance()`
        !! * `geocom_set_user_atr_mode()`
        !! * `geocom_set_user_lock_mode()`
        !! * `geocom_set_user_prism_definition()`
        !! * `geocom_set_user_spiral()`
        !! * `geocom_set_velocity()`
        !! * `geocom_setup_download()`
        !! * `geocom_setup_list()`
        !! * `geocom_start_controller()`
        !! * `geocom_stop_controller()`
        !! * `geocom_switch_off()`
        !! * `geocom_switch_on()`
        !! * `geocom_take_image()`
        !!
        use :: dm_util, only: dm_itoa
        type(lua_state_type), intent(inout) :: lua !! Lua state type.

        rc = E_INVALID
        if (.not. dm_lua_is_opened(lua)) return

        ! Add GeoCOM constants.
        rc = dm_lua_eval(lua, 'GEOCOM_AUT_NORMAL = '               // dm_itoa(GEOCOM_AUT_NORMAL));               if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_AUT_PRECISE = '              // dm_itoa(GEOCOM_AUT_PRECISE));              if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_AUT_FAST = '                 // dm_itoa(GEOCOM_AUT_FAST));                 if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_AUT_NORM_MODE = '            // dm_itoa(GEOCOM_AUT_NORM_MODE));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_AUT_POINT_MODE = '           // dm_itoa(GEOCOM_AUT_POINT_MODE));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_AUT_DEFINE_MODE = '          // dm_itoa(GEOCOM_AUT_DEFINE_MODE));          if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_AUT_POSITION = '             // dm_itoa(GEOCOM_AUT_POSITION));             if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_AUT_TARGET = '               // dm_itoa(GEOCOM_AUT_TARGET));               if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_AUT_CLOCKWISE = '            // dm_itoa(GEOCOM_AUT_CLOCKWISE));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_AUT_ANTICLOCKWISE = '        // dm_itoa(GEOCOM_AUT_ANTICLOCKWISE));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_NO_MEAS = '              // dm_itoa(GEOCOM_BAP_NO_MEAS));              if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_NO_DIST = '              // dm_itoa(GEOCOM_BAP_NO_DIST));              if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_DEF_DIST = '             // dm_itoa(GEOCOM_BAP_DEF_DIST));             if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_CLEAR_DIST = '           // dm_itoa(GEOCOM_BAP_CLEAR_DIST));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_STOP_TRK = '             // dm_itoa(GEOCOM_BAP_STOP_TRK));             if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_SINGLE_REF_STANDARD = '  // dm_itoa(GEOCOM_BAP_SINGLE_REF_STANDARD));  if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_SINGLE_REF_FAST = '      // dm_itoa(GEOCOM_BAP_SINGLE_REF_FAST));      if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_SINGLE_REF_VISIBLE = '   // dm_itoa(GEOCOM_BAP_SINGLE_REF_VISIBLE));   if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_SINGLE_RLESS_VISIBLE = ' // dm_itoa(GEOCOM_BAP_SINGLE_RLESS_VISIBLE)); if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_CONT_REF_STANDARD = '    // dm_itoa(GEOCOM_BAP_CONT_REF_STANDARD));    if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_CONT_REF_FAST = '        // dm_itoa(GEOCOM_BAP_CONT_REF_FAST));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_CONT_RLESS_VISIBLE = '   // dm_itoa(GEOCOM_BAP_CONT_RLESS_VISIBLE));   if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_AVG_REF_STANDARD = '     // dm_itoa(GEOCOM_BAP_AVG_REF_STANDARD));     if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_AVG_REF_VISIBLE = '      // dm_itoa(GEOCOM_BAP_AVG_REF_VISIBLE));      if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_AVG_RLESS_VISIBLE = '    // dm_itoa(GEOCOM_BAP_AVG_RLESS_VISIBLE));    if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_CONT_REF_SYNCHRO = '     // dm_itoa(GEOCOM_BAP_CONT_REF_SYNCHRO));     if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_SINGLE_REF_PRECISE = '   // dm_itoa(GEOCOM_BAP_SINGLE_REF_PRECISE));   if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_ROUND = '          // dm_itoa(GEOCOM_BAP_PRISM_ROUND));          if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_MINI = '           // dm_itoa(GEOCOM_BAP_PRISM_MINI));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_TAPE = '           // dm_itoa(GEOCOM_BAP_PRISM_TAPE));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_360 = '            // dm_itoa(GEOCOM_BAP_PRISM_360));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_USER1 = '          // dm_itoa(GEOCOM_BAP_PRISM_USER1));          if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_USER2 = '          // dm_itoa(GEOCOM_BAP_PRISM_USER2));          if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_USER3 = '          // dm_itoa(GEOCOM_BAP_PRISM_USER3));          if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_360_MINI = '       // dm_itoa(GEOCOM_BAP_PRISM_360_MINI));       if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_MINI_ZERO = '      // dm_itoa(GEOCOM_BAP_PRISM_MINI_ZERO));      if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_USER = '           // dm_itoa(GEOCOM_BAP_PRISM_USER));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_NDS_TAPE = '       // dm_itoa(GEOCOM_BAP_PRISM_NDS_TAPE));       if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_GRZ121_ROUND = '   // dm_itoa(GEOCOM_BAP_PRISM_GRZ121_ROUND));   if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_PRISM_MA_MPR122 = '      // dm_itoa(GEOCOM_BAP_PRISM_MA_MPR122));      if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_REFL_UNDEF = '           // dm_itoa(GEOCOM_BAP_REFL_UNDEF));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_REFL_PRISM = '           // dm_itoa(GEOCOM_BAP_REFL_PRISM));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_REFL_TAPE = '            // dm_itoa(GEOCOM_BAP_REFL_TAPE));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_REFL_USE = '             // dm_itoa(GEOCOM_BAP_REFL_USE));             if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_REFL_LESS = '            // dm_itoa(GEOCOM_BAP_REFL_LESS));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_ATRSET_NORMAL = '        // dm_itoa(GEOCOM_BAP_ATRSET_NORMAL));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_ATRSET_LOWVIS_ON = '     // dm_itoa(GEOCOM_BAP_ATRSET_LOWVIS_ON));     if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_ATRSET_LOWVIS_AON = '    // dm_itoa(GEOCOM_BAP_ATRSET_LOWVIS_AON));    if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_ATRSET_SRANGE_ON = '     // dm_itoa(GEOCOM_BAP_ATRSET_SRANGE_ON));     if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_BAP_ATRSET_SRANGE_AON = '    // dm_itoa(GEOCOM_BAP_ATRSET_SRANGE_AON));    if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_COM_ASCII = '                // dm_itoa(GEOCOM_COM_ASCII));                if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_COM_BINARY = '               // dm_itoa(GEOCOM_COM_BINARY));               if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_COM_STOP_SHUT_DOWN = '       // dm_itoa(GEOCOM_COM_STOP_SHUT_DOWN));       if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_COM_STOP_SLEEP = '           // dm_itoa(GEOCOM_COM_STOP_SLEEP));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_COM_STARTUP_LOCAL = '        // dm_itoa(GEOCOM_COM_STARTUP_LOCAL));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_COM_STARTUP_REMOTE = '       // dm_itoa(GEOCOM_COM_STARTUP_REMOTE));       if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_CSV_EXTERNAL_POWER = '       // dm_itoa(GEOCOM_CSV_EXTERNAL_POWER));       if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_CSV_INTERNAL_POWER = '       // dm_itoa(GEOCOM_CSV_INTERNAL_POWER));       if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_EGLINTEN_OFF = '         // dm_itoa(GEOCOM_EDM_EGLINTEN_OFF));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_EGLINTEN_LOW = '         // dm_itoa(GEOCOM_EDM_EGLINTEN_LOW));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_EGLINTEN_MID = '         // dm_itoa(GEOCOM_EDM_EGLINTEN_MID));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_EGLINTEN_HIGH = '        // dm_itoa(GEOCOM_EDM_EGLINTEN_HIGH));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_MODE_NOT_USED = '        // dm_itoa(GEOCOM_EDM_MODE_NOT_USED));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_SINGLE_TAPE = '          // dm_itoa(GEOCOM_EDM_SINGLE_TAPE));          if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_SINGLE_STANDARD = '      // dm_itoa(GEOCOM_EDM_SINGLE_STANDARD));      if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_SINGLE_FAST = '          // dm_itoa(GEOCOM_EDM_SINGLE_FAST));          if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_SINGLE_LRANGE = '        // dm_itoa(GEOCOM_EDM_SINGLE_LRANGE));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_SINGLE_SRANGE = '        // dm_itoa(GEOCOM_EDM_SINGLE_SRANGE));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_CONT_STANDARD = '        // dm_itoa(GEOCOM_EDM_CONT_STANDARD));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_CONT_DYNAMIC = '         // dm_itoa(GEOCOM_EDM_CONT_DYNAMIC));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_CONT_REFLESS = '         // dm_itoa(GEOCOM_EDM_CONT_REFLESS));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_CONT_FAST = '            // dm_itoa(GEOCOM_EDM_CONT_FAST));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_AVERAGE_IR = '           // dm_itoa(GEOCOM_EDM_AVERAGE_IR));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_PRECISE_IR = '           // dm_itoa(GEOCOM_EDM_PRECISE_IR));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_EDM_PRECISE_TAPE = '         // dm_itoa(GEOCOM_EDM_PRECISE_TAPE));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_FTR_DEVICE_INTERNAL = '      // dm_itoa(GEOCOM_FTR_DEVICE_INTERNAL));      if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_FTR_DEVICE_PCPARD = '        // dm_itoa(GEOCOM_FTR_DEVICE_PCPARD));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_FTR_FILE_IMAGES = '          // dm_itoa(GEOCOM_FTR_FILE_IMAGES));          if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_IMG_INTERNAL_MEMORY = '      // dm_itoa(GEOCOM_IMG_INTERNAL_MEMORY));      if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_IMG_PC_CARD = '              // dm_itoa(GEOCOM_IMG_PC_CARD));              if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_IOS_BEEP_STDINTENS = '       // dm_itoa(GEOCOM_IOS_BEEP_STDINTENS));       if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_LOCKED_OUT = '           // dm_itoa(GEOCOM_MOT_LOCKED_OUT));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_LOCKED_IN = '            // dm_itoa(GEOCOM_MOT_LOCKED_IN));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_PREDICTION = '           // dm_itoa(GEOCOM_MOT_PREDICTION));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_NORMAL = '               // dm_itoa(GEOCOM_MOT_NORMAL));               if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_SHUTDOWN = '             // dm_itoa(GEOCOM_MOT_SHUTDOWN));             if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_POSIT = '                // dm_itoa(GEOCOM_MOT_POSIT));                if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_OCONST = '               // dm_itoa(GEOCOM_MOT_OCONST));               if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_MANUPOS = '              // dm_itoa(GEOCOM_MOT_MANUPOS));              if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_LOCK = '                 // dm_itoa(GEOCOM_MOT_LOCK));                 if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_BREAK = '                // dm_itoa(GEOCOM_MOT_BREAK));                if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_MOT_TERM = '                 // dm_itoa(GEOCOM_MOT_TERM));                 if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_SUP_POWER_DISABLED = '       // dm_itoa(GEOCOM_SUP_POWER_DISABLED));       if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_SUP_POWER_OFF = '            // dm_itoa(GEOCOM_SUP_POWER_OFF));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_MEA_INC = '              // dm_itoa(GEOCOM_TMC_MEA_INC));              if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_AUTO_INC = '             // dm_itoa(GEOCOM_TMC_AUTO_INC));             if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_PLANE_INC = '            // dm_itoa(GEOCOM_TMC_PLANE_INC));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_STOP = '                 // dm_itoa(GEOCOM_TMC_STOP));                 if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_DEF_DIST = '             // dm_itoa(GEOCOM_TMC_DEF_DIST));             if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_CLEAR = '                // dm_itoa(GEOCOM_TMC_CLEAR));                if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_SIGNAL = '               // dm_itoa(GEOCOM_TMC_SIGNAL));               if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_DO_MEASURE = '           // dm_itoa(GEOCOM_TMC_DO_MEASURE));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_RTRK_DIST = '            // dm_itoa(GEOCOM_TMC_RTRK_DIST));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_RED_TRK_DIST = '         // dm_itoa(GEOCOM_TMC_RED_TRK_DIST));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_FREQUENCY = '            // dm_itoa(GEOCOM_TMC_FREQUENCY));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_FACE_NORMAL = '          // dm_itoa(GEOCOM_TMC_FACE_NORMAL));          if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_FACE_TURN = '            // dm_itoa(GEOCOM_TMC_FACE_TURN));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_FACE_1 = '               // dm_itoa(GEOCOM_TMC_FACE_1));               if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TMC_FACE_2 = '               // dm_itoa(GEOCOM_TMC_FACE_2));               if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1100 = '           // dm_itoa(GEOCOM_TPS_CLASS_1100));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1700 = '           // dm_itoa(GEOCOM_TPS_CLASS_1700));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1800 = '           // dm_itoa(GEOCOM_TPS_CLASS_1800));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_5000 = '           // dm_itoa(GEOCOM_TPS_CLASS_5000));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_6000 = '           // dm_itoa(GEOCOM_TPS_CLASS_6000));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1500 = '           // dm_itoa(GEOCOM_TPS_CLASS_1500));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_2003 = '           // dm_itoa(GEOCOM_TPS_CLASS_2003));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_5005 = '           // dm_itoa(GEOCOM_TPS_CLASS_5005));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_5100 = '           // dm_itoa(GEOCOM_TPS_CLASS_5100));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1102 = '           // dm_itoa(GEOCOM_TPS_CLASS_1102));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1103 = '           // dm_itoa(GEOCOM_TPS_CLASS_1103));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1105 = '           // dm_itoa(GEOCOM_TPS_CLASS_1105));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1101 = '           // dm_itoa(GEOCOM_TPS_CLASS_1101));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1202 = '           // dm_itoa(GEOCOM_TPS_CLASS_1202));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1203 = '           // dm_itoa(GEOCOM_TPS_CLASS_1203));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1205 = '           // dm_itoa(GEOCOM_TPS_CLASS_1205));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_1201 = '           // dm_itoa(GEOCOM_TPS_CLASS_1201));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_TX30 = '           // dm_itoa(GEOCOM_TPS_CLASS_TX30));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_CLASS_TX31 = '           // dm_itoa(GEOCOM_TPS_CLASS_TX31));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_T = '             // dm_itoa(GEOCOM_TPS_DEVICE_T));             if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_MOT = '           // dm_itoa(GEOCOM_TPS_DEVICE_MOT));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_ATR = '           // dm_itoa(GEOCOM_TPS_DEVICE_ATR));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_EGL = '           // dm_itoa(GEOCOM_TPS_DEVICE_EGL));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_DB = '            // dm_itoa(GEOCOM_TPS_DEVICE_DB));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_DL = '            // dm_itoa(GEOCOM_TPS_DEVICE_DL));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_LP = '            // dm_itoa(GEOCOM_TPS_DEVICE_LP));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_TC1 = '           // dm_itoa(GEOCOM_TPS_DEVICE_TC1));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_TC2 = '           // dm_itoa(GEOCOM_TPS_DEVICE_TC2));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_TC = '            // dm_itoa(GEOCOM_TPS_DEVICE_TC));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_TCR = '           // dm_itoa(GEOCOM_TPS_DEVICE_TCR));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_ATC = '           // dm_itoa(GEOCOM_TPS_DEVICE_ATC));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_LPNT = '          // dm_itoa(GEOCOM_TPS_DEVICE_LPNT));          if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_RL_EXT = '        // dm_itoa(GEOCOM_TPS_DEVICE_RL_EXT));        if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_PS = '            // dm_itoa(GEOCOM_TPS_DEVICE_PS));            if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_DEVICE_SIM = '           // dm_itoa(GEOCOM_TPS_DEVICE_SIM));           if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_REFLESS_NONE = '         // dm_itoa(GEOCOM_TPS_REFLESS_NONE));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_REFLESS_R100 = '         // dm_itoa(GEOCOM_TPS_REFLESS_R100));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_REFLESS_R300 = '         // dm_itoa(GEOCOM_TPS_REFLESS_R300));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_REFLESS_R400 = '         // dm_itoa(GEOCOM_TPS_REFLESS_R400));         if (dm_is_error(rc)) return
        rc = dm_lua_eval(lua, 'GEOCOM_TPS_REFLESS_R1000 = '        // dm_itoa(GEOCOM_TPS_REFLESS_R1000));        if (dm_is_error(rc)) return

        call dm_lua_register(lua, 'geocom_beep_alarm',                    lua_geocom_beep_alarm)
        call dm_lua_register(lua, 'geocom_abort_download',                lua_geocom_abort_download)
        call dm_lua_register(lua, 'geocom_abort_list',                    lua_geocom_abort_list)
        call dm_lua_register(lua, 'geocom_beep_alarm',                    lua_geocom_beep_alarm)
        call dm_lua_register(lua, 'geocom_beep_normal',                   lua_geocom_beep_normal)
        call dm_lua_register(lua, 'geocom_beep_off',                      lua_geocom_beep_off)
        call dm_lua_register(lua, 'geocom_beep_on',                       lua_geocom_beep_on)
        call dm_lua_register(lua, 'geocom_change_face',                   lua_geocom_change_face)
        call dm_lua_register(lua, 'geocom_delete',                        lua_geocom_delete)
        call dm_lua_register(lua, 'geocom_do_measure',                    lua_geocom_do_measure)
        call dm_lua_register(lua, 'geocom_download',                      lua_geocom_download)
        call dm_lua_register(lua, 'geocom_fine_adjust',                   lua_geocom_fine_adjust)
        call dm_lua_register(lua, 'geocom_get_angle',                     lua_geocom_get_angle)
        call dm_lua_register(lua, 'geocom_get_angle_complete',            lua_geocom_get_angle_complete)
        call dm_lua_register(lua, 'geocom_get_angular_correction_status', lua_geocom_get_angular_correction_status)
        call dm_lua_register(lua, 'geocom_get_atmospheric_correction',    lua_geocom_get_atmospheric_correction)
        call dm_lua_register(lua, 'geocom_get_atmospheric_ppm',           lua_geocom_get_atmospheric_ppm)
        call dm_lua_register(lua, 'geocom_get_atr_error',                 lua_geocom_get_atr_error)
        call dm_lua_register(lua, 'geocom_get_atr_setting',               lua_geocom_get_atr_setting)
        call dm_lua_register(lua, 'geocom_get_binary_mode',               lua_geocom_get_binary_mode)
        call dm_lua_register(lua, 'geocom_get_config',                    lua_geocom_get_config)
        call dm_lua_register(lua, 'geocom_get_coordinate',                lua_geocom_get_coordinate)
        call dm_lua_register(lua, 'geocom_get_date_time',                 lua_geocom_get_date_time)
        call dm_lua_register(lua, 'geocom_get_date_time_centi',           lua_geocom_get_date_time_centi)
        call dm_lua_register(lua, 'geocom_get_device_config',             lua_geocom_get_device_config)
        call dm_lua_register(lua, 'geocom_get_double_precision',          lua_geocom_get_double_precision)
        call dm_lua_register(lua, 'geocom_get_edm_mode',                  lua_geocom_get_edm_mode)
        call dm_lua_register(lua, 'geocom_get_egl_intensity',             lua_geocom_get_egl_intensity)
        call dm_lua_register(lua, 'geocom_get_face',                      lua_geocom_get_face)
        call dm_lua_register(lua, 'geocom_get_fine_adjust_mode',          lua_geocom_get_fine_adjust_mode)
        call dm_lua_register(lua, 'geocom_get_full_measurement',          lua_geocom_get_full_measurement)
        call dm_lua_register(lua, 'geocom_get_geocom_version',            lua_geocom_get_geocom_version)
        call dm_lua_register(lua, 'geocom_get_geometric_ppm',             lua_geocom_get_geometric_ppm)
        call dm_lua_register(lua, 'geocom_get_height',                    lua_geocom_get_height)
        call dm_lua_register(lua, 'geocom_get_image_config',              lua_geocom_get_image_config)
        call dm_lua_register(lua, 'geocom_get_inclination_correction',    lua_geocom_get_inclination_correction)
        call dm_lua_register(lua, 'geocom_get_inclination_error',         lua_geocom_get_inclination_error)
        call dm_lua_register(lua, 'geocom_get_instrument_name',           lua_geocom_get_instrument_name)
        call dm_lua_register(lua, 'geocom_get_instrument_number',         lua_geocom_get_instrument_number)
        call dm_lua_register(lua, 'geocom_get_internal_temperature',      lua_geocom_get_internal_temperature)
        call dm_lua_register(lua, 'geocom_get_lock_status',               lua_geocom_get_lock_status)
        call dm_lua_register(lua, 'geocom_get_measurement_program',       lua_geocom_get_measurement_program)
        call dm_lua_register(lua, 'geocom_get_power',                     lua_geocom_get_power)
        call dm_lua_register(lua, 'geocom_get_prism_constant',            lua_geocom_get_prism_constant)
        call dm_lua_register(lua, 'geocom_get_prism_definition',          lua_geocom_get_prism_definition)
        call dm_lua_register(lua, 'geocom_get_prism_type',                lua_geocom_get_prism_type)
        call dm_lua_register(lua, 'geocom_get_prism_type_v2',             lua_geocom_get_prism_type_v2)
        call dm_lua_register(lua, 'geocom_get_quick_distance',            lua_geocom_get_quick_distance)
        call dm_lua_register(lua, 'geocom_get_reduced_atr_fov',           lua_geocom_get_reduced_atr_fov)
        call dm_lua_register(lua, 'geocom_get_reflectorless_class',       lua_geocom_get_reflectorless_class)
        call dm_lua_register(lua, 'geocom_get_refraction_mode',           lua_geocom_get_refraction_mode)
        call dm_lua_register(lua, 'geocom_get_search_area',               lua_geocom_get_search_area)
        call dm_lua_register(lua, 'geocom_get_signal',                    lua_geocom_get_signal)
        call dm_lua_register(lua, 'geocom_get_simple_coordinates',        lua_geocom_get_simple_coordinates)
        call dm_lua_register(lua, 'geocom_get_simple_measurement',        lua_geocom_get_simple_measurement)
        call dm_lua_register(lua, 'geocom_get_slope_distance_correction', lua_geocom_get_slope_distance_correction)
        call dm_lua_register(lua, 'geocom_get_software_version',          lua_geocom_get_software_version)
        call dm_lua_register(lua, 'geocom_get_station',                   lua_geocom_get_station)
        call dm_lua_register(lua, 'geocom_get_target_type',               lua_geocom_get_target_type)
        call dm_lua_register(lua, 'geocom_get_timeout',                   lua_geocom_get_timeout)
        call dm_lua_register(lua, 'geocom_get_tolerance',                 lua_geocom_get_tolerance)
        call dm_lua_register(lua, 'geocom_get_user_atr_mode',             lua_geocom_get_user_atr_mode)
        call dm_lua_register(lua, 'geocom_get_user_lock_mode',            lua_geocom_get_user_lock_mode)
        call dm_lua_register(lua, 'geocom_get_user_prism_definition',     lua_geocom_get_user_prism_definition)
        call dm_lua_register(lua, 'geocom_get_user_spiral',               lua_geocom_get_user_spiral)
        call dm_lua_register(lua, 'geocom_list',                          lua_geocom_list)
        call dm_lua_register(lua, 'geocom_lock_in',                       lua_geocom_lock_in)
        call dm_lua_register(lua, 'geocom_null',                          lua_geocom_null)
        call dm_lua_register(lua, 'geocom_ps_enable_range',               lua_geocom_ps_enable_range)
        call dm_lua_register(lua, 'geocom_ps_search_next',                lua_geocom_ps_search_next)
        call dm_lua_register(lua, 'geocom_ps_search_window',              lua_geocom_ps_search_window)
        call dm_lua_register(lua, 'geocom_ps_set_range',                  lua_geocom_ps_set_range)
        call dm_lua_register(lua, 'geocom_search',                        lua_geocom_search)
        call dm_lua_register(lua, 'geocom_search_target',                 lua_geocom_search_target)
        call dm_lua_register(lua, 'geocom_set_angle_correction',          lua_geocom_set_angle_correction)
        call dm_lua_register(lua, 'geocom_set_atmospheric_correction',    lua_geocom_set_atmospheric_correction)
        call dm_lua_register(lua, 'geocom_set_atmospheric_ppm',           lua_geocom_set_atmospheric_ppm)
        call dm_lua_register(lua, 'geocom_set_atr_mode',                  lua_geocom_set_atr_mode)
        call dm_lua_register(lua, 'geocom_set_binary_mode',               lua_geocom_set_binary_mode)
        call dm_lua_register(lua, 'geocom_set_config',                    lua_geocom_set_config)
        call dm_lua_register(lua, 'geocom_set_date_time',                 lua_geocom_set_date_time)
        call dm_lua_register(lua, 'geocom_set_distance',                  lua_geocom_set_distance)
        call dm_lua_register(lua, 'geocom_set_double_precision',          lua_geocom_set_double_precision)
        call dm_lua_register(lua, 'geocom_set_edm_mode',                  lua_geocom_set_edm_mode)
        call dm_lua_register(lua, 'geocom_set_egl_intensity',             lua_geocom_set_egl_intensity)
        call dm_lua_register(lua, 'geocom_set_fine_adjust_mode',          lua_geocom_set_fine_adjust_mode)
        call dm_lua_register(lua, 'geocom_set_geometric_ppm',             lua_geocom_set_geometric_ppm)
        call dm_lua_register(lua, 'geocom_set_height',                    lua_geocom_set_height)
        call dm_lua_register(lua, 'geocom_set_image_config',              lua_geocom_set_image_config)
        call dm_lua_register(lua, 'geocom_set_inclination_correction',    lua_geocom_set_inclination_correction)
        call dm_lua_register(lua, 'geocom_set_laser_pointer',             lua_geocom_set_laser_pointer)
        call dm_lua_register(lua, 'geocom_set_measurement_program',       lua_geocom_set_measurement_program)
        call dm_lua_register(lua, 'geocom_set_orientation',               lua_geocom_set_orientation)
        call dm_lua_register(lua, 'geocom_set_position',                  lua_geocom_set_position)
        call dm_lua_register(lua, 'geocom_set_positioning_timeout',       lua_geocom_set_positioning_timeout)
        call dm_lua_register(lua, 'geocom_set_prism_constant',            lua_geocom_set_prism_constant)
        call dm_lua_register(lua, 'geocom_set_prism_type',                lua_geocom_set_prism_type)
        call dm_lua_register(lua, 'geocom_set_prism_type_v2',             lua_geocom_set_prism_type_v2)
        call dm_lua_register(lua, 'geocom_set_reduced_atr_fov',           lua_geocom_set_reduced_atr_fov)
        call dm_lua_register(lua, 'geocom_set_refraction_mode',           lua_geocom_set_refraction_mode)
        call dm_lua_register(lua, 'geocom_set_search_area',               lua_geocom_set_search_area)
        call dm_lua_register(lua, 'geocom_set_station',                   lua_geocom_set_station)
        call dm_lua_register(lua, 'geocom_set_target_type',               lua_geocom_set_target_type)
        call dm_lua_register(lua, 'geocom_set_tolerance',                 lua_geocom_set_tolerance)
        call dm_lua_register(lua, 'geocom_set_user_atr_mode',             lua_geocom_set_user_atr_mode)
        call dm_lua_register(lua, 'geocom_set_user_lock_mode',            lua_geocom_set_user_lock_mode)
        call dm_lua_register(lua, 'geocom_set_user_prism_definition',     lua_geocom_set_user_prism_definition)
        call dm_lua_register(lua, 'geocom_set_user_spiral',               lua_geocom_set_user_spiral)
        call dm_lua_register(lua, 'geocom_set_velocity',                  lua_geocom_set_velocity)
        call dm_lua_register(lua, 'geocom_setup_download',                lua_geocom_setup_download)
        call dm_lua_register(lua, 'geocom_setup_list',                    lua_geocom_setup_list)
        call dm_lua_register(lua, 'geocom_start_controller',              lua_geocom_start_controller)
        call dm_lua_register(lua, 'geocom_stop_controller',               lua_geocom_stop_controller)
        call dm_lua_register(lua, 'geocom_switch_off',                    lua_geocom_switch_off)
        call dm_lua_register(lua, 'geocom_switch_on',                     lua_geocom_switch_on)
        call dm_lua_register(lua, 'geocom_take_image',                    lua_geocom_take_image)

        rc = E_NONE
    end function dm_lua_geocom_register

    ! **************************************************************************
    ! PRIVATE LUA GEOCOM REQUEST FUNCTIONS.
    ! **************************************************************************
    integer(kind=c_int) function lua_geocom_abort_download(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_abort_download(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_abort_download

    integer(kind=c_int) function lua_geocom_abort_list(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_abort_list(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_abort_list

    integer(kind=c_int) function lua_geocom_beep_alarm(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_beep_alarm(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_beep_alarm

    integer(kind=c_int) function lua_geocom_beep_normal(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_beep_normal(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_beep_normal

    integer(kind=c_int) function lua_geocom_beep_off(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_beep_off(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_beep_off

    integer(kind=c_int) function lua_geocom_beep_on(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: intensity
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        intensity = max(0, min(100, dm_lua_to_int32(lua, 1)))
        call dm_geocom_api_request_beep_on(request, intensity)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_beep_on

    integer(kind=c_int) function lua_geocom_change_face(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: atr_mode, pos_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        pos_mode = dm_geocom_api_parameter_aut_posmode(dm_lua_to_int32(lua, 1))
        atr_mode = dm_geocom_api_parameter_aut_atrmode(dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_change_face(request, pos_mode, atr_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_change_face

    integer(kind=c_int) function lua_geocom_delete(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_delete(request, device_type, file_type, day, month, year, file_name)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_delete

    integer(kind=c_int) function lua_geocom_do_measure(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: inc_mode, prog
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        prog     = dm_geocom_api_parameter_tmc_measure_prg(dm_lua_to_int32(lua, 1))
        inc_mode = dm_geocom_api_parameter_tmc_incline_prg(dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_do_measure(request, prog, inc_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_do_measure

    integer(kind=c_int) function lua_geocom_download(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: block_number
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        block_number = max(0, min(65535, dm_lua_to_int32(lua, 1)))
        call dm_geocom_api_request_download(request, block_number)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_download

    integer(kind=c_int) function lua_geocom_fine_adjust(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: search_hz, search_v
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        search_hz = dm_lua_to_real64(lua, 1)
        search_v  = dm_lua_to_real64(lua, 2)
        call dm_geocom_api_request_fine_adjust(request, search_hz, search_v)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_fine_adjust

    integer(kind=c_int) function lua_geocom_get_angle(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: inc_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        inc_mode = dm_geocom_api_parameter_tmc_incline_prg(dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_get_angle(request, inc_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_angle

    integer(kind=c_int) function lua_geocom_get_angle_complete(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: inc_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        inc_mode = dm_geocom_api_parameter_tmc_incline_prg(dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_get_angle_complete(request, inc_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_angle_complete

    integer(kind=c_int) function lua_geocom_get_angular_correction_status(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_angular_correction_status(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_angular_correction_status

    integer(kind=c_int) function lua_geocom_get_atmospheric_correction(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_atmospheric_correction(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_atmospheric_correction

    integer(kind=c_int) function lua_geocom_get_atmospheric_ppm(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_atmospheric_ppm(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_atmospheric_ppm

    integer(kind=c_int) function lua_geocom_get_atr_error(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_atr_error(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_atr_error

    integer(kind=c_int) function lua_geocom_get_atr_setting(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_atr_setting(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_atr_setting

    integer(kind=c_int) function lua_geocom_get_binary_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_binary_mode(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_binary_mode

    integer(kind=c_int) function lua_geocom_get_config(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_config(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_config

    integer(kind=c_int) function lua_geocom_get_coordinate(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: inc_mode, wait_time
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        inc_mode  = dm_geocom_api_parameter_tmc_incline_prg(dm_lua_to_int32(lua, 1))
        wait_time = max(0, dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_get_coordinate(request, inc_mode, wait_time)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_coordinate

    integer(kind=c_int) function lua_geocom_get_date_time(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_date_time(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_date_time

    integer(kind=c_int) function lua_geocom_get_date_time_centi(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_date_time_centi(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_date_time_centi

    integer(kind=c_int) function lua_geocom_get_device_config(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_device_config(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_device_config

    integer(kind=c_int) function lua_geocom_get_double_precision(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_double_precision(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_double_precision

    integer(kind=c_int) function lua_geocom_get_edm_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_edm_mode(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_edm_mode

    integer(kind=c_int) function lua_geocom_get_egl_intensity(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_egl_intensity(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_egl_intensity

    integer(kind=c_int) function lua_geocom_get_face(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_face(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_face

    integer(kind=c_int) function lua_geocom_get_fine_adjust_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_fine_adjust_mode(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_fine_adjust_mode

    integer(kind=c_int) function lua_geocom_get_full_measurement(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: inc_mode, wait_time
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        inc_mode  = dm_geocom_api_parameter_tmc_incline_prg(dm_lua_to_int32(lua, 1))
        wait_time = max(0, dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_get_full_measurement(request, inc_mode, wait_time)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_full_measurement

    integer(kind=c_int) function lua_geocom_get_geocom_version(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_geocom_version(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_geocom_version

    integer(kind=c_int) function lua_geocom_get_geometric_ppm(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_geometric_ppm(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_geometric_ppm

    integer(kind=c_int) function lua_geocom_get_height(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_height(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_height

    integer(kind=c_int) function lua_geocom_get_image_config(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: mem_type
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        mem_type = dm_geocom_api_parameter_img_mem_type(dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_get_image_config(request, mem_type)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_image_config

    integer(kind=c_int) function lua_geocom_get_inclination_correction(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_inclination_correction(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_inclination_correction

    integer(kind=c_int) function lua_geocom_get_inclination_error(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_inclination_error(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_inclination_error

    integer(kind=c_int) function lua_geocom_get_instrument_name(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_instrument_name(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_instrument_name

    integer(kind=c_int) function lua_geocom_get_instrument_number(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_instrument_number(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_instrument_number

    integer(kind=c_int) function lua_geocom_get_internal_temperature(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_internal_temperature(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_internal_temperature

    integer(kind=c_int) function lua_geocom_get_lock_status(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_lock_status(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_lock_status

    integer(kind=c_int) function lua_geocom_get_measurement_program(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_measurement_program(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_measurement_program

    integer(kind=c_int) function lua_geocom_get_power(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_power(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_power

    integer(kind=c_int) function lua_geocom_get_prism_constant(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_prism_constant(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_prism_constant

    integer(kind=c_int) function lua_geocom_get_prism_definition(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: prism_type
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        prism_type = dm_geocom_api_parameter_bap_prismtype(dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_get_prism_definition(request, prism_type)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_prism_definition

    integer(kind=c_int) function lua_geocom_get_prism_type(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_prism_type(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_prism_type

    integer(kind=c_int) function lua_geocom_get_prism_type_v2(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_prism_type_v2(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_prism_type_v2

    integer(kind=c_int) function lua_geocom_get_quick_distance(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_quick_distance(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_quick_distance

    integer(kind=c_int) function lua_geocom_get_reduced_atr_fov(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_reduced_atr_fov(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_reduced_atr_fov

    integer(kind=c_int) function lua_geocom_get_reflectorless_class(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_reflectorless_class(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_reflectorless_class

    integer(kind=c_int) function lua_geocom_get_refraction_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_refraction_mode(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_refraction_mode

    integer(kind=c_int) function lua_geocom_get_search_area(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_search_area(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_search_area

    integer(kind=c_int) function lua_geocom_get_signal(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_signal(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_signal

    integer(kind=c_int) function lua_geocom_get_simple_coordinates(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: inc_mode, wait_time
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        inc_mode  = dm_geocom_api_parameter_tmc_incline_prg(dm_lua_to_int32(lua, 1))
        wait_time = max(0, dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_get_simple_coordinates(request, inc_mode, wait_time)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_simple_coordinates

    integer(kind=c_int) function lua_geocom_get_simple_measurement(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: inc_mode, wait_time
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        inc_mode  = dm_geocom_api_parameter_tmc_incline_prg(dm_lua_to_int32(lua, 1))
        wait_time = max(0, dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_get_simple_measurement(request, inc_mode, wait_time)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_simple_measurement

    integer(kind=c_int) function lua_geocom_get_slope_distance_correction(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_slope_distance_correction(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_slope_distance_correction

    integer(kind=c_int) function lua_geocom_get_software_version(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_software_version(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_software_version

    integer(kind=c_int) function lua_geocom_get_station(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_station(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_station

    integer(kind=c_int) function lua_geocom_get_target_type(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_target_type(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_target_type

    integer(kind=c_int) function lua_geocom_get_timeout(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_timeout(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_timeout

    integer(kind=c_int) function lua_geocom_get_tolerance(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_tolerance(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_tolerance

    integer(kind=c_int) function lua_geocom_get_user_atr_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_user_atr_mode(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_user_atr_mode

    integer(kind=c_int) function lua_geocom_get_user_lock_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_user_lock_mode(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_user_lock_mode

    integer(kind=c_int) function lua_geocom_get_user_prism_definition(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        character(len=GEOCOM_BAP_PRISMNAME_LEN) :: name

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        name = dm_lua_to_string(lua, 1)
        call dm_geocom_api_request_get_user_prism_definition(request, name)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_user_prism_definition

    integer(kind=c_int) function lua_geocom_get_user_spiral(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_user_spiral(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_user_spiral

    integer(kind=c_int) function lua_geocom_list(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        logical              :: next
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        next = dm_lua_to_logical(lua, 1)
        call dm_geocom_api_request_list(request, next)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_list

    integer(kind=c_int) function lua_geocom_lock_in(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_lock_in(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_lock_in

    integer(kind=c_int) function lua_geocom_null(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_null(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_null

    integer(kind=c_int) function lua_geocom_ps_enable_range(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        logical              :: enabled
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        enabled = dm_lua_to_logical(lua, 1)
        call dm_geocom_api_request_ps_enable_range(request, enabled)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_ps_enable_range

    integer(kind=c_int) function lua_geocom_ps_search_next(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_ps_search_next(request, direction, swing)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_ps_search_next

    integer(kind=c_int) function lua_geocom_ps_search_window(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_ps_search_window(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_ps_search_window

    integer(kind=c_int) function lua_geocom_ps_set_range(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_ps_set_range(request, min_dist, max_dist)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_ps_set_range

    integer(kind=c_int) function lua_geocom_search(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: search_hz, search_v
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        search_hz = dm_lua_to_real64(lua, 1)
        search_v  = dm_lua_to_real64(lua, 2)
        call dm_geocom_api_request_search(request, search_hz, search_v)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_search

    integer(kind=c_int) function lua_geocom_search_target(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_search_target(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_search_target

    integer(kind=c_int) function lua_geocom_set_angle_correction(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_angle_correction(request, incline, stand_axis, collimation, tilt_axis)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_angle_correction

    integer(kind=c_int) function lua_geocom_set_atmospheric_correction(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_atmospheric_correction(request, lambda, pressure, dry_temp, wet_temp)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_atmospheric_correction

    integer(kind=c_int) function lua_geocom_set_atmospheric_ppm(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_atmospheric_ppm(request, ppm)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_atmospheric_ppm

    integer(kind=c_int) function lua_geocom_set_atr_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: atr_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        atr_mode = dm_geocom_api_parameter_aut_atrmode(dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_set_atr_mode(request, atr_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_atr_mode

    integer(kind=c_int) function lua_geocom_set_binary_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        logical              :: enabled
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        enabled = dm_lua_to_logical(lua, 1)
        call dm_geocom_api_request_set_binary_mode(request, enabled)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_binary_mode

    integer(kind=c_int) function lua_geocom_set_config(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_config(request, auto_power, timeout)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_config

    integer(kind=c_int) function lua_geocom_set_date_time(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_date_time(request, year, month, day, hour, minute, second)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_date_time

    integer(kind=c_int) function lua_geocom_set_distance(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_distance(request, slope_dist, height_offset, mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_distance

    integer(kind=c_int) function lua_geocom_set_double_precision(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_double_precision(request, ndigits)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_double_precision

    integer(kind=c_int) function lua_geocom_set_edm_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_edm_mode(request, edm_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_edm_mode

    integer(kind=c_int) function lua_geocom_set_egl_intensity(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_egl_intensity(request, intensity)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_egl_intensity

    integer(kind=c_int) function lua_geocom_set_fine_adjust_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_fine_adjust_mode(request, mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_fine_adjust_mode

    integer(kind=c_int) function lua_geocom_set_geometric_ppm(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_geometric_ppm(request, enabled, scale_factor, offset, ppm_height, ppm_individual)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_geometric_ppm

    integer(kind=c_int) function lua_geocom_set_height(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_height(request, height)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_height

    integer(kind=c_int) function lua_geocom_set_image_config(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_image_config(request, mem_type, image_number, quality, sub_function, prefix)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_image_config

    integer(kind=c_int) function lua_geocom_set_inclination_correction(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        logical              :: enabled
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        enabled = dm_lua_to_logical(lua, 1)
        call dm_geocom_api_request_set_inclination_correction(request, enabled)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_inclination_correction

    integer(kind=c_int) function lua_geocom_set_laser_pointer(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        logical              :: enabled
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        enabled = dm_lua_to_logical(lua, 1)
        call dm_geocom_api_request_set_laser_pointer(request, enabled)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_laser_pointer

    integer(kind=c_int) function lua_geocom_set_measurement_program(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_measurement_program(request, prog)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_measurement_program

    integer(kind=c_int) function lua_geocom_set_orientation(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: hz
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        hz = dm_lua_to_real64(lua, 1)
        call dm_geocom_api_request_set_orientation(request, hz)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_orientation

    integer(kind=c_int) function lua_geocom_set_position(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: atr_mode, pos_mode
        real(kind=r8)        :: hz, v
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        hz = dm_lua_to_real64(lua, 1)
        v  = dm_lua_to_real64(lua, 2)
        pos_mode = dm_geocom_api_parameter_aut_posmode(dm_lua_to_int32(lua, 3))
        atr_mode = dm_geocom_api_parameter_aut_atrmode(dm_lua_to_int32(lua, 4))
        call dm_geocom_api_request_set_position(request, hz, v, pos_mode, atr_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_position

    integer(kind=c_int) function lua_geocom_set_positioning_timeout(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: hz, v
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        hz = dm_lua_to_real64(lua, 1)
        v  = dm_lua_to_real64(lua, 2)
        call dm_geocom_api_request_set_positioning_timeout(request, hz, v)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_positioning_timeout

    integer(kind=c_int) function lua_geocom_set_prism_constant(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: prism_corr
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        prism_corr = dm_lua_to_real64(lua, 1)
        call dm_geocom_api_request_set_prism_constant(request, prism_corr)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_prism_constant

    integer(kind=c_int) function lua_geocom_set_prism_type(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_prism_type(request, prism_type)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_prism_type

    integer(kind=c_int) function lua_geocom_set_prism_type_v2(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_prism_type_v2(request, prism_type, prism_name)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_prism_type_v2

    integer(kind=c_int) function lua_geocom_set_reduced_atr_fov(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        logical              :: enabled
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        enabled = dm_lua_to_logical(lua, 1)
        call dm_geocom_api_request_set_reduced_atr_fov(request, enabled)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_reduced_atr_fov

    integer(kind=c_int) function lua_geocom_set_refraction_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_refraction_mode(request, mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_refraction_mode

    integer(kind=c_int) function lua_geocom_set_search_area(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_search_area(request, center_hz, center_v, range_hz, range_v, enabled)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_search_area

    integer(kind=c_int) function lua_geocom_set_station(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_station(request, easting, northing, height, instr_height)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_station

    integer(kind=c_int) function lua_geocom_set_target_type(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_target_type(request, target_type)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_target_type

    integer(kind=c_int) function lua_geocom_set_tolerance(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: hz, v
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        hz = dm_lua_to_real64(lua, 1)
        v  = dm_lua_to_real64(lua, 2)
        call dm_geocom_api_request_set_tolerance(request, hz, v)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_tolerance

    integer(kind=c_int) function lua_geocom_set_user_atr_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        logical              :: enabled
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        enabled = dm_lua_to_logical(lua, 1)
        call dm_geocom_api_request_set_user_atr_mode(request, enabled)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_user_atr_mode

    integer(kind=c_int) function lua_geocom_set_user_lock_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        logical              :: enabled
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        enabled = dm_lua_to_logical(lua, 1)
        call dm_geocom_api_request_set_user_lock_mode(request, enabled)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_user_lock_mode

    integer(kind=c_int) function lua_geocom_set_user_prism_definition(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_set_user_prism_definition(request, prism_name, prism_const, prism_type, creator)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_user_prism_definition

    integer(kind=c_int) function lua_geocom_set_user_spiral(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: hz, v
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        hz = dm_lua_to_real64(lua, 1)
        v  = dm_lua_to_real64(lua, 2)
        call dm_geocom_api_request_set_user_spiral(request, hz, v)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_user_spiral

    integer(kind=c_int) function lua_geocom_set_velocity(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: omega_hz, omega_v
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        omega_hz = dm_lua_to_real64(lua, 1)
        omega_v  = dm_lua_to_real64(lua, 2)
        call dm_geocom_api_request_set_velocity(request, omega_hz, omega_v)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_velocity

    integer(kind=c_int) function lua_geocom_setup_download(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_setup_download(request, device_type, file_type, file_name, block_size)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_setup_download

    integer(kind=c_int) function lua_geocom_setup_list(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_setup_list(request, device_type, file_type, search_path)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_setup_list

    integer(kind=c_int) function lua_geocom_start_controller(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_start_controller(request, start_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_start_controller

    integer(kind=c_int) function lua_geocom_stop_controller(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_stop_controller(request, stop_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_stop_controller

    integer(kind=c_int) function lua_geocom_switch_off(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_switch_off(request, stop_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_switch_off

    integer(kind=c_int) function lua_geocom_switch_on(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_switch_on(request, start_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_switch_on

    integer(kind=c_int) function lua_geocom_take_image(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ! call dm_geocom_api_request_take_image(request, mem_type)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_take_image
end module dm_lua_geocom
