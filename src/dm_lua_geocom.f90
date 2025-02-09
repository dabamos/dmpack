! Author:  Philipp Engel
! Licence: ISC
module dm_lua_geocom
    !! GeoCOM API for Lua.
    use, intrinsic :: iso_c_binding
    use :: dm_error
    use :: dm_geocom_api
    use :: dm_geocom_error
    use :: dm_geocom_type
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
    private :: lua_geocom_get_angle_correction
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
    private :: lua_geocom_measure_distance_angle
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
    integer function dm_lua_geocom_register(lua, procedures, errors) result(rc)
        !! Registers all GeoCOM API procedures and type parameters in given Lua
        !! environment `lua`, unless `procedures` is `.false.`. If `errors` is
        !! passed and `.true.`, all named GeoCOM return code parameters are
        !! registered as well.
        !!
        !! The function returns `E_INVALID` if the Lua interpreter is not
        !! initialised, or `E_LUA` if the registration failed.
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
        !! * `geocom_get_angle_correction()`
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
        !! * `geocom_measure_distance_angle()`
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
        use :: dm_util, only: dm_itoa, dm_present

        type(lua_state_type), intent(inout)        :: lua        !! Lua state type.
        logical,              intent(in), optional :: procedures !! Export GeoCOM API procedures and type parameters.
        logical,              intent(in), optional :: errors     !! Export GeoCOM return codes (`GRC_*`).

        rc = E_INVALID
        if (.not. dm_lua_is_opened(lua)) return

        ! Register GeoCOM Lua API.
        if (dm_present(procedures, .true.)) then
            ! Add GeoCOM type parameters.
            rc = dm_lua_set(lua, 'GEOCOM_AUT_NORMAL',               GEOCOM_AUT_NORMAL);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_AUT_PRECISE',              GEOCOM_AUT_PRECISE);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_AUT_FAST',                 GEOCOM_AUT_FAST);                 if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_AUT_NORM_MODE',            GEOCOM_AUT_NORM_MODE);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_AUT_POINT_MODE',           GEOCOM_AUT_POINT_MODE);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_AUT_DEFINE_MODE',          GEOCOM_AUT_DEFINE_MODE);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_AUT_POSITION',             GEOCOM_AUT_POSITION);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_AUT_TARGET',               GEOCOM_AUT_TARGET);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_AUT_CLOCKWISE',            GEOCOM_AUT_CLOCKWISE);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_AUT_ANTICLOCKWISE',        GEOCOM_AUT_ANTICLOCKWISE);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_NO_MEAS',              GEOCOM_BAP_NO_MEAS);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_NO_DIST',              GEOCOM_BAP_NO_DIST);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_DEF_DIST',             GEOCOM_BAP_DEF_DIST);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_CLEAR_DIST',           GEOCOM_BAP_CLEAR_DIST);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_STOP_TRK',             GEOCOM_BAP_STOP_TRK);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_SINGLE_REF_STANDARD',  GEOCOM_BAP_SINGLE_REF_STANDARD);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_SINGLE_REF_FAST',      GEOCOM_BAP_SINGLE_REF_FAST);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_SINGLE_REF_VISIBLE',   GEOCOM_BAP_SINGLE_REF_VISIBLE);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_SINGLE_RLESS_VISIBLE', GEOCOM_BAP_SINGLE_RLESS_VISIBLE); if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_CONT_REF_STANDARD',    GEOCOM_BAP_CONT_REF_STANDARD);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_CONT_REF_FAST',        GEOCOM_BAP_CONT_REF_FAST);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_CONT_RLESS_VISIBLE',   GEOCOM_BAP_CONT_RLESS_VISIBLE);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_AVG_REF_STANDARD',     GEOCOM_BAP_AVG_REF_STANDARD);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_AVG_REF_VISIBLE',      GEOCOM_BAP_AVG_REF_VISIBLE);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_AVG_RLESS_VISIBLE',    GEOCOM_BAP_AVG_RLESS_VISIBLE);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_CONT_REF_SYNCHRO',     GEOCOM_BAP_CONT_REF_SYNCHRO);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_SINGLE_REF_PRECISE',   GEOCOM_BAP_SINGLE_REF_PRECISE);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_ROUND',          GEOCOM_BAP_PRISM_ROUND);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_MINI',           GEOCOM_BAP_PRISM_MINI);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_TAPE',           GEOCOM_BAP_PRISM_TAPE);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_360',            GEOCOM_BAP_PRISM_360);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_USER1',          GEOCOM_BAP_PRISM_USER1);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_USER2',          GEOCOM_BAP_PRISM_USER2);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_USER3',          GEOCOM_BAP_PRISM_USER3);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_360_MINI',       GEOCOM_BAP_PRISM_360_MINI);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_MINI_ZERO',      GEOCOM_BAP_PRISM_MINI_ZERO);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_USER',           GEOCOM_BAP_PRISM_USER);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_NDS_TAPE',       GEOCOM_BAP_PRISM_NDS_TAPE);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_GRZ121_ROUND',   GEOCOM_BAP_PRISM_GRZ121_ROUND);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_PRISM_MA_MPR122',      GEOCOM_BAP_PRISM_MA_MPR122);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_REFL_UNDEF',           GEOCOM_BAP_REFL_UNDEF);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_REFL_PRISM',           GEOCOM_BAP_REFL_PRISM);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_REFL_TAPE',            GEOCOM_BAP_REFL_TAPE);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_REFL_USE',             GEOCOM_BAP_REFL_USE);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_REFL_LESS',            GEOCOM_BAP_REFL_LESS);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_ATRSET_NORMAL',        GEOCOM_BAP_ATRSET_NORMAL);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_ATRSET_LOWVIS_ON',     GEOCOM_BAP_ATRSET_LOWVIS_ON);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_ATRSET_LOWVIS_AON',    GEOCOM_BAP_ATRSET_LOWVIS_AON);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_ATRSET_SRANGE_ON',     GEOCOM_BAP_ATRSET_SRANGE_ON);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_BAP_ATRSET_SRANGE_AON',    GEOCOM_BAP_ATRSET_SRANGE_AON);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_COM_ASCII',                GEOCOM_COM_ASCII);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_COM_BINARY',               GEOCOM_COM_BINARY);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_COM_STOP_SHUT_DOWN',       GEOCOM_COM_STOP_SHUT_DOWN);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_COM_STOP_SLEEP',           GEOCOM_COM_STOP_SLEEP);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_COM_STARTUP_LOCAL',        GEOCOM_COM_STARTUP_LOCAL);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_COM_STARTUP_REMOTE',       GEOCOM_COM_STARTUP_REMOTE);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_CSV_EXTERNAL_POWER',       GEOCOM_CSV_EXTERNAL_POWER);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_CSV_INTERNAL_POWER',       GEOCOM_CSV_INTERNAL_POWER);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_EGLINTEN_OFF',         GEOCOM_EDM_EGLINTEN_OFF);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_EGLINTEN_LOW',         GEOCOM_EDM_EGLINTEN_LOW);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_EGLINTEN_MID',         GEOCOM_EDM_EGLINTEN_MID);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_EGLINTEN_HIGH',        GEOCOM_EDM_EGLINTEN_HIGH);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_MODE_NOT_USED',        GEOCOM_EDM_MODE_NOT_USED);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_SINGLE_TAPE',          GEOCOM_EDM_SINGLE_TAPE);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_SINGLE_STANDARD',      GEOCOM_EDM_SINGLE_STANDARD);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_SINGLE_FAST',          GEOCOM_EDM_SINGLE_FAST);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_SINGLE_LRANGE',        GEOCOM_EDM_SINGLE_LRANGE);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_SINGLE_SRANGE',        GEOCOM_EDM_SINGLE_SRANGE);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_CONT_STANDARD',        GEOCOM_EDM_CONT_STANDARD);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_CONT_DYNAMIC',         GEOCOM_EDM_CONT_DYNAMIC);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_CONT_REFLESS',         GEOCOM_EDM_CONT_REFLESS);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_CONT_FAST',            GEOCOM_EDM_CONT_FAST);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_AVERAGE_IR',           GEOCOM_EDM_AVERAGE_IR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_PRECISE_IR',           GEOCOM_EDM_PRECISE_IR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_EDM_PRECISE_TAPE',         GEOCOM_EDM_PRECISE_TAPE);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_FTR_DEVICE_INTERNAL',      GEOCOM_FTR_DEVICE_INTERNAL);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_FTR_DEVICE_PCPARD',        GEOCOM_FTR_DEVICE_PCPARD);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_FTR_FILE_IMAGES',          GEOCOM_FTR_FILE_IMAGES);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_IMG_INTERNAL_MEMORY',      GEOCOM_IMG_INTERNAL_MEMORY);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_IMG_PC_CARD',              GEOCOM_IMG_PC_CARD);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_IOS_BEEP_STDINTENS',       GEOCOM_IOS_BEEP_STDINTENS);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_LOCKED_OUT',           GEOCOM_MOT_LOCKED_OUT);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_LOCKED_IN',            GEOCOM_MOT_LOCKED_IN);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_PREDICTION',           GEOCOM_MOT_PREDICTION);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_NORMAL',               GEOCOM_MOT_NORMAL);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_SHUTDOWN',             GEOCOM_MOT_SHUTDOWN);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_POSIT',                GEOCOM_MOT_POSIT);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_OCONST',               GEOCOM_MOT_OCONST);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_MANUPOS',              GEOCOM_MOT_MANUPOS);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_LOCK',                 GEOCOM_MOT_LOCK);                 if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_BREAK',                GEOCOM_MOT_BREAK);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_MOT_TERM',                 GEOCOM_MOT_TERM);                 if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_SUP_POWER_DISABLED',       GEOCOM_SUP_POWER_DISABLED);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_SUP_POWER_OFF',            GEOCOM_SUP_POWER_OFF);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_MEA_INC',              GEOCOM_TMC_MEA_INC);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_AUTO_INC',             GEOCOM_TMC_AUTO_INC);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_PLANE_INC',            GEOCOM_TMC_PLANE_INC);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_STOP',                 GEOCOM_TMC_STOP);                 if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_DEF_DIST',             GEOCOM_TMC_DEF_DIST);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_CLEAR',                GEOCOM_TMC_CLEAR);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_SIGNAL',               GEOCOM_TMC_SIGNAL);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_DO_MEASURE',           GEOCOM_TMC_DO_MEASURE);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_RTRK_DIST',            GEOCOM_TMC_RTRK_DIST);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_RED_TRK_DIST',         GEOCOM_TMC_RED_TRK_DIST);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_FREQUENCY',            GEOCOM_TMC_FREQUENCY);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_FACE_NORMAL',          GEOCOM_TMC_FACE_NORMAL);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_FACE_TURN',            GEOCOM_TMC_FACE_TURN);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_FACE_1',               GEOCOM_TMC_FACE_1);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TMC_FACE_2',               GEOCOM_TMC_FACE_2);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1100',           GEOCOM_TPS_CLASS_1100);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1700',           GEOCOM_TPS_CLASS_1700);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1800',           GEOCOM_TPS_CLASS_1800);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_5000',           GEOCOM_TPS_CLASS_5000);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_6000',           GEOCOM_TPS_CLASS_6000);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1500',           GEOCOM_TPS_CLASS_1500);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_2003',           GEOCOM_TPS_CLASS_2003);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_5005',           GEOCOM_TPS_CLASS_5005);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_5100',           GEOCOM_TPS_CLASS_5100);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1102',           GEOCOM_TPS_CLASS_1102);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1103',           GEOCOM_TPS_CLASS_1103);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1105',           GEOCOM_TPS_CLASS_1105);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1101',           GEOCOM_TPS_CLASS_1101);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1202',           GEOCOM_TPS_CLASS_1202);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1203',           GEOCOM_TPS_CLASS_1203);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1205',           GEOCOM_TPS_CLASS_1205);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_1201',           GEOCOM_TPS_CLASS_1201);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_TX30',           GEOCOM_TPS_CLASS_TX30);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_CLASS_TX31',           GEOCOM_TPS_CLASS_TX31);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_T',             GEOCOM_TPS_DEVICE_T);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_MOT',           GEOCOM_TPS_DEVICE_MOT);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_ATR',           GEOCOM_TPS_DEVICE_ATR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_EGL',           GEOCOM_TPS_DEVICE_EGL);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_DB',            GEOCOM_TPS_DEVICE_DB);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_DL',            GEOCOM_TPS_DEVICE_DL);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_LP',            GEOCOM_TPS_DEVICE_LP);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_TC1',           GEOCOM_TPS_DEVICE_TC1);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_TC2',           GEOCOM_TPS_DEVICE_TC2);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_TC',            GEOCOM_TPS_DEVICE_TC);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_TCR',           GEOCOM_TPS_DEVICE_TCR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_ATC',           GEOCOM_TPS_DEVICE_ATC);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_LPNT',          GEOCOM_TPS_DEVICE_LPNT);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_RL_EXT',        GEOCOM_TPS_DEVICE_RL_EXT);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_PS',            GEOCOM_TPS_DEVICE_PS);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_DEVICE_SIM',           GEOCOM_TPS_DEVICE_SIM);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_REFLESS_NONE',         GEOCOM_TPS_REFLESS_NONE);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_REFLESS_R100',         GEOCOM_TPS_REFLESS_R100);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_REFLESS_R300',         GEOCOM_TPS_REFLESS_R300);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_REFLESS_R400',         GEOCOM_TPS_REFLESS_R400);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GEOCOM_TPS_REFLESS_R1000',        GEOCOM_TPS_REFLESS_R1000);        if (dm_is_error(rc)) return

            ! Add Lua functions.
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
            call dm_lua_register(lua, 'geocom_get_angle_correction',          lua_geocom_get_angle_correction)
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
            call dm_lua_register(lua, 'geocom_measure_distance_angle',        lua_geocom_measure_distance_angle)
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
        end if

        ! Register GeoCOM return codes.
        if (dm_present(errors, .false.)) then
            rc = dm_lua_set(lua, 'GRC_OK',                         GRC_OK);                         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_UNDEFINED',                  GRC_UNDEFINED);                  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_IVPARAM',                    GRC_IVPARAM);                    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_IVRESULT',                   GRC_IVRESULT);                   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_FATAL',                      GRC_FATAL);                      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_NOT_IMPL',                   GRC_NOT_IMPL);                   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TIME_OUT',                   GRC_TIME_OUT);                   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_SET_INCOMPL',                GRC_SET_INCOMPL);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ABORT',                      GRC_ABORT);                      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_NOMEMORY',                   GRC_NOMEMORY);                   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_NOTINIT',                    GRC_NOTINIT);                    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_SHUT_DOWN',                  GRC_SHUT_DOWN);                  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_SYSBUSY',                    GRC_SYSBUSY);                    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_HWFAILURE',                  GRC_HWFAILURE);                  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ABORT_APPL',                 GRC_ABORT_APPL);                 if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_LOW_POWER',                  GRC_LOW_POWER);                  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_IVVERSION',                  GRC_IVVERSION);                  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BAT_EMPTY',                  GRC_BAT_EMPTY);                  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_NO_EVENT',                   GRC_NO_EVENT);                   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_OUT_OF_TEMP',                GRC_OUT_OF_TEMP);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_INSTRUMENT_TILT',            GRC_INSTRUMENT_TILT);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_SETTING',                GRC_COM_SETTING);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_NO_ACTION',                  GRC_NO_ACTION);                  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_SLEEP_MODE',                 GRC_SLEEP_MODE);                 if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_NOTOK',                      GRC_NOTOK);                      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_NA',                         GRC_NA);                         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_OVERFLOW',                   GRC_OVERFLOW);                   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_STOPPED',                    GRC_STOPPED);                    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG',                        GRC_ANG);                        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_ERROR',                  GRC_ANG_ERROR);                  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_INCL_ERROR',             GRC_ANG_INCL_ERROR);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_BAD_ACC',                GRC_ANG_BAD_ACC);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_BAD_ANGLE_ACC',          GRC_ANG_BAD_ANGLE_ACC);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_BAD_INCLIN_ACC',         GRC_ANG_BAD_INCLIN_ACC);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_WRITE_PROTECTED',        GRC_ANG_WRITE_PROTECTED);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_OUT_OF_RANGE',           GRC_ANG_OUT_OF_RANGE);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_IR_OCCURED',             GRC_ANG_IR_OCCURED);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_HZ_MOVED',               GRC_ANG_HZ_MOVED);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_OS_ERROR',               GRC_ANG_OS_ERROR);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_DATA_ERROR',             GRC_ANG_DATA_ERROR);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_PEAK_CNT_UFL',           GRC_ANG_PEAK_CNT_UFL);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_TIME_OUT',               GRC_ANG_TIME_OUT);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_TOO_MANY_EXPOS',         GRC_ANG_TOO_MANY_EXPOS);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_PIX_CTRL_ERR',           GRC_ANG_PIX_CTRL_ERR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_MAX_POS_SKIP',           GRC_ANG_MAX_POS_SKIP);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_MAX_NEG_SKIP',           GRC_ANG_MAX_NEG_SKIP);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_EXP_LIMIT',              GRC_ANG_EXP_LIMIT);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_UNDER_EXPOSURE',         GRC_ANG_UNDER_EXPOSURE);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_OVER_EXPOSURE',          GRC_ANG_OVER_EXPOSURE);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_TMANY_PEAKS',            GRC_ANG_TMANY_PEAKS);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_TLESS_PEAKS',            GRC_ANG_TLESS_PEAKS);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_PEAK_TOO_SLIM',          GRC_ANG_PEAK_TOO_SLIM);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_PEAK_TOO_WIDE',          GRC_ANG_PEAK_TOO_WIDE);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_BAD_PEAKDIFF',           GRC_ANG_BAD_PEAKDIFF);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_UNDER_EXP_PICT',         GRC_ANG_UNDER_EXP_PICT);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_PEAKS_INHOMOGEN',        GRC_ANG_PEAKS_INHOMOGEN);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_NO_DECOD_POSS',          GRC_ANG_NO_DECOD_POSS);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_UNSTABLE_DECOD',         GRC_ANG_UNSTABLE_DECOD);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_TLESS_FPEAKS',           GRC_ANG_TLESS_FPEAKS);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_INCL_OLD_PLANE',         GRC_ANG_INCL_OLD_PLANE);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_INCL_NO_PLANE',          GRC_ANG_INCL_NO_PLANE);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_FAST_ANG_ERR',           GRC_ANG_FAST_ANG_ERR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_FAST_ANG_ERR_5',         GRC_ANG_FAST_ANG_ERR_5);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_FAST_ANG_ERR_25',        GRC_ANG_FAST_ANG_ERR_25);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_TRANS_ERR',              GRC_ANG_TRANS_ERR);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_TRANS_ERR_5',            GRC_ANG_TRANS_ERR_5);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ANG_TRANS_ERR_25',           GRC_ANG_TRANS_ERR_25);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_NOT_READY',              GRC_ATA_NOT_READY);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_NO_RESULT',              GRC_ATA_NO_RESULT);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_SEVERAL_TARGETS',        GRC_ATA_SEVERAL_TARGETS);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_BIG_SPOT',               GRC_ATA_BIG_SPOT);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_BACKGROUND',             GRC_ATA_BACKGROUND);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_NO_TARGETS',             GRC_ATA_NO_TARGETS);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_NOT_ACCURAT',            GRC_ATA_NOT_ACCURAT);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_SPOT_ON_EDGE',           GRC_ATA_SPOT_ON_EDGE);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_BLOOMING',               GRC_ATA_BLOOMING);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_NOT_BUSY',               GRC_ATA_NOT_BUSY);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_STRANGE_LIGHT',          GRC_ATA_STRANGE_LIGHT);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_V24_FAIL',               GRC_ATA_V24_FAIL);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_DECODE_ERROR',           GRC_ATA_DECODE_ERROR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_HZ_FAIL',                GRC_ATA_HZ_FAIL);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_V_FAIL',                 GRC_ATA_V_FAIL);                 if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_HZ_STRANGE_L',           GRC_ATA_HZ_STRANGE_L);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_V_STRANGE_L',            GRC_ATA_V_STRANGE_L);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_SLDR_TRANSFER_PENDING',  GRC_ATA_SLDR_TRANSFER_PENDING);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_SLDR_TRANSFER_ILLEGAL',  GRC_ATA_SLDR_TRANSFER_ILLEGAL);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_SLDR_DATA_ERROR',        GRC_ATA_SLDR_DATA_ERROR);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_SLDR_CHK_SUM_ERROR',     GRC_ATA_SLDR_CHK_SUM_ERROR);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_SLDR_ADDRESS_ERROR',     GRC_ATA_SLDR_ADDRESS_ERROR);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_SLDR_INV_LOADFILE',      GRC_ATA_SLDR_INV_LOADFILE);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_SLDR_UNSUPPORTED',       GRC_ATA_SLDR_UNSUPPORTED);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_PS_NOT_READY',           GRC_ATA_PS_NOT_READY);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_ATA_ATR_SYSTEM_ERR',         GRC_ATA_ATR_SYSTEM_ERR);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM',                        GRC_EDM);                        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SYSTEM_ERR',             GRC_EDM_SYSTEM_ERR);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_INVALID_COMMAND',        GRC_EDM_INVALID_COMMAND);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_BOOM_ERR',               GRC_EDM_BOOM_ERR);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SIGN_LOW_ERR',           GRC_EDM_SIGN_LOW_ERR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_DIL_ERR',                GRC_EDM_DIL_ERR);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SIGN_HIGH_ERR',          GRC_EDM_SIGN_HIGH_ERR);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_TIMEOUT',                GRC_EDM_TIMEOUT);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_FLUKT_ERR',              GRC_EDM_FLUKT_ERR);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_FMOT_ERR',               GRC_EDM_FMOT_ERR);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_DEV_NOT_INSTALLED',      GRC_EDM_DEV_NOT_INSTALLED);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_NOT_FOUND',              GRC_EDM_NOT_FOUND);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_ERROR_RECEIVED',         GRC_EDM_ERROR_RECEIVED);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_MISSING_SRVPWD',         GRC_EDM_MISSING_SRVPWD);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_INVALID_ANSWER',         GRC_EDM_INVALID_ANSWER);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SEND_ERR',               GRC_EDM_SEND_ERR);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_RECEIVE_ERR',            GRC_EDM_RECEIVE_ERR);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_INTERNAL_ERR',           GRC_EDM_INTERNAL_ERR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_BUSY',                   GRC_EDM_BUSY);                   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_NO_MEASACTIVITY',        GRC_EDM_NO_MEASACTIVITY);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_CHKSUM_ERR',             GRC_EDM_CHKSUM_ERR);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_INIT_OR_STOP_ERR',       GRC_EDM_INIT_OR_STOP_ERR);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SRL_NOT_AVAILABLE',      GRC_EDM_SRL_NOT_AVAILABLE);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_MEAS_ABORTED',           GRC_EDM_MEAS_ABORTED);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SLDR_TRANSFER_PENDING',  GRC_EDM_SLDR_TRANSFER_PENDING);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SLDR_TRANSFER_ILLEGAL',  GRC_EDM_SLDR_TRANSFER_ILLEGAL);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SLDR_DATA_ERROR',        GRC_EDM_SLDR_DATA_ERROR);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SLDR_CHK_SUM_ERROR',     GRC_EDM_SLDR_CHK_SUM_ERROR);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SLDR_ADDR_ERROR',        GRC_EDM_SLDR_ADDR_ERROR);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SLDR_INV_LOADFILE',      GRC_EDM_SLDR_INV_LOADFILE);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SLDR_UNSUPPORTED',       GRC_EDM_SLDR_UNSUPPORTED);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_UNKNOW_ERR',             GRC_EDM_UNKNOW_ERR);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_DISTRANGE_ERR',          GRC_EDM_DISTRANGE_ERR);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_SIGNTONOISE_ERR',        GRC_EDM_SIGNTONOISE_ERR);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_NOISEHIGH_ERR',          GRC_EDM_NOISEHIGH_ERR);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_PWD_NOTSET',             GRC_EDM_PWD_NOTSET);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_ACTION_NO_MORE_VALID',   GRC_EDM_ACTION_NO_MORE_VALID);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_MULTRG_ERR',             GRC_EDM_MULTRG_ERR);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_MISSING_EE_CONSTS',      GRC_EDM_MISSING_EE_CONSTS);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_NOPRECISE',              GRC_EDM_NOPRECISE);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_EDM_MEAS_DIST_NOT_ALLOWED',  GRC_EDM_MEAS_DIST_NOT_ALLOWED);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF',                        GRC_GMF);                        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_WRONG_AREA_DEF',         GRC_GMF_WRONG_AREA_DEF);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_IDENTICAL_PTS',          GRC_GMF_IDENTICAL_PTS);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_PTS_IN_LINE',            GRC_GMF_PTS_IN_LINE);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_OUT_OF_RANGE',           GRC_GMF_OUT_OF_RANGE);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_PLAUSIBILITY_ERR',       GRC_GMF_PLAUSIBILITY_ERR);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_TOO_FEW_OBSERVATIONS',   GRC_GMF_TOO_FEW_OBSERVATIONS);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_NO_SOLUTION',            GRC_GMF_NO_SOLUTION);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_ONE_SOLUTION',           GRC_GMF_ONE_SOLUTION);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_TWO_SOLUTIONS',          GRC_GMF_TWO_SOLUTIONS);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_ANGLE_SMALLER_15GON',    GRC_GMF_ANGLE_SMALLER_15GON);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_INVALID_TRIANGLE_TYPE',  GRC_GMF_INVALID_TRIANGLE_TYPE);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_INVALID_ANGLE_SYSTEM',   GRC_GMF_INVALID_ANGLE_SYSTEM);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_INVALID_DIST_SYSTEM',    GRC_GMF_INVALID_DIST_SYSTEM);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_INVALID_V_SYSTEM',       GRC_GMF_INVALID_V_SYSTEM);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_INVALID_TEMP_SYSTEM',    GRC_GMF_INVALID_TEMP_SYSTEM);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_INVALID_PRES_SYSTEM',    GRC_GMF_INVALID_PRES_SYSTEM);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_RADIUS_NOT_POSSIBLE',    GRC_GMF_RADIUS_NOT_POSSIBLE);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_NO_PROVISIONAL_VALUES',  GRC_GMF_NO_PROVISIONAL_VALUES);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_SINGULAR_MATRIX',        GRC_GMF_SINGULAR_MATRIX);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_TOO_MANY_ITERATIONS',    GRC_GMF_TOO_MANY_ITERATIONS);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_IDENTICAL_TIE_POINTS',   GRC_GMF_IDENTICAL_TIE_POINTS);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_GMF_SETUP_EQUALS_TIE_POINT', GRC_GMF_SETUP_EQUALS_TIE_POINT); if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC',                        GRC_TMC);                        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC_NO_FULL_CORRECTION',     GRC_TMC_NO_FULL_CORRECTION);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC_ACCURACY_GUARANTEE',     GRC_TMC_ACCURACY_GUARANTEE);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC_ANGLE_OK',               GRC_TMC_ANGLE_OK);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC_ANGLE_NOT_FULL_CORR',    GRC_TMC_ANGLE_NOT_FULL_CORR);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC_ANGLE_NO_ACC_GUARANTY',  GRC_TMC_ANGLE_NO_ACC_GUARANTY);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC_ANGLE_ERROR',            GRC_TMC_ANGLE_ERROR);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC_DIST_PPM',               GRC_TMC_DIST_PPM);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC_DIST_ERROR',             GRC_TMC_DIST_ERROR);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC_BUSY',                   GRC_TMC_BUSY);                   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_TMC_SIGNAL_ERROR',           GRC_TMC_SIGNAL_ERROR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_UNREADY',                GRC_MOT_UNREADY);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_BUSY',                   GRC_MOT_BUSY);                   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_NOT_OCONST',             GRC_MOT_NOT_OCONST);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_NOT_CONFIG',             GRC_MOT_NOT_CONFIG);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_NOT_POSIT',              GRC_MOT_NOT_POSIT);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_NOT_SERVICE',            GRC_MOT_NOT_SERVICE);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_NOT_BUSY',               GRC_MOT_NOT_BUSY);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_NOT_LOCK',               GRC_MOT_NOT_LOCK);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_NOT_SPIRAL',             GRC_MOT_NOT_SPIRAL);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_V_ENCODER',              GRC_MOT_V_ENCODER);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_HZ_ENCODER',             GRC_MOT_HZ_ENCODER);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_MOT_HZ_V_ENCODER',           GRC_MOT_HZ_V_ENCODER);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM',                        GRC_BMM);                        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_XFER_PENDING',           GRC_BMM_XFER_PENDING);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_NO_XFER_OPEN',           GRC_BMM_NO_XFER_OPEN);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_UNKNOWN_CHARSET',        GRC_BMM_UNKNOWN_CHARSET);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_NOT_INSTALLED',          GRC_BMM_NOT_INSTALLED);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_ALREADY_EXIST',          GRC_BMM_ALREADY_EXIST);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_CANT_DELETE',            GRC_BMM_CANT_DELETE);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_MEM_ERROR',              GRC_BMM_MEM_ERROR);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_CHARSET_USED',           GRC_BMM_CHARSET_USED);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_CHARSET_SAVED',          GRC_BMM_CHARSET_SAVED);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_INVALID_ADR',            GRC_BMM_INVALID_ADR);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_CANCELANDADR_ERROR',     GRC_BMM_CANCELANDADR_ERROR);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_INVALID_SIZE',           GRC_BMM_INVALID_SIZE);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_CANCELANDINVSIZE_ERROR', GRC_BMM_CANCELANDINVSIZE_ERROR); if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_ALL_GROUP_OCC',          GRC_BMM_ALL_GROUP_OCC);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_CANT_DEL_LAYERS',        GRC_BMM_CANT_DEL_LAYERS);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_UNKNOWN_LAYER',          GRC_BMM_UNKNOWN_LAYER);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_BMM_INVALID_LAYERLEN',       GRC_BMM_INVALID_LAYERLEN);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_ERO',                    GRC_COM_ERO);                    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_CANT_ENCODE',            GRC_COM_CANT_ENCODE);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_CANT_DECODE',            GRC_COM_CANT_DECODE);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_CANT_SEND',              GRC_COM_CANT_SEND);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_CANT_RECV',              GRC_COM_CANT_RECV);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_TIMEDOUT',               GRC_COM_TIMEDOUT);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_WRONG_FORMAT',           GRC_COM_WRONG_FORMAT);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_VER_MISMATCH',           GRC_COM_VER_MISMATCH);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_CANT_DECODE_REQ',        GRC_COM_CANT_DECODE_REQ);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_PROC_UNAVAIL',           GRC_COM_PROC_UNAVAIL);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_CANT_ENCODE_REP',        GRC_COM_CANT_ENCODE_REP);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_SYSTEM_ERR',             GRC_COM_SYSTEM_ERR);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_FAILED',                 GRC_COM_FAILED);                 if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_NO_BINARY',              GRC_COM_NO_BINARY);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_INTR',                   GRC_COM_INTR);                   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_REQUIRES_8DBITS',        GRC_COM_REQUIRES_8DBITS);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_TR_ID_MISMATCH',         GRC_COM_TR_ID_MISMATCH);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_NOT_GEOCOM',             GRC_COM_NOT_GEOCOM);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_UNKNOWN_PORT',           GRC_COM_UNKNOWN_PORT);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_ERO_END',                GRC_COM_ERO_END);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_OVERRUN',                GRC_COM_OVERRUN);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_SRVR_RX_CHECKSUM_ERRR',  GRC_COM_SRVR_RX_CHECKSUM_ERRR);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_CLNT_RX_CHECKSUM_ERRR',  GRC_COM_CLNT_RX_CHECKSUM_ERRR);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_PORT_NOT_AVAILABLE',     GRC_COM_PORT_NOT_AVAILABLE);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_PORT_NOT_OPEN',          GRC_COM_PORT_NOT_OPEN);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_NO_PARTNER',             GRC_COM_NO_PARTNER);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_ERO_NOT_STARTED',        GRC_COM_ERO_NOT_STARTED);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_CONS_REQ',               GRC_COM_CONS_REQ);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_SRVR_IS_SLEEPING',       GRC_COM_SRVR_IS_SLEEPING);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_SRVR_IS_OFF',            GRC_COM_SRVR_IS_OFF);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_COM_NO_CHECKSUM',            GRC_COM_NO_CHECKSUM);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_TIMEOUT',                GRC_AUT_TIMEOUT);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_DETENT_ERROR',           GRC_AUT_DETENT_ERROR);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_ANGLE_ERROR',            GRC_AUT_ANGLE_ERROR);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_MOTOR_ERROR',            GRC_AUT_MOTOR_ERROR);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_INCACC',                 GRC_AUT_INCACC);                 if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_DEV_ERROR',              GRC_AUT_DEV_ERROR);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_NO_TARGET',              GRC_AUT_NO_TARGET);              if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_MULTIPLE_TARGETS',       GRC_AUT_MULTIPLE_TARGETS);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_BAD_ENVIRONMENT',        GRC_AUT_BAD_ENVIRONMENT);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_DETECTOR_ERROR',         GRC_AUT_DETECTOR_ERROR);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_NOT_ENABLED',            GRC_AUT_NOT_ENABLED);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_CALACC',                 GRC_AUT_CALACC);                 if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_ACCURACY',               GRC_AUT_ACCURACY);               if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_DIST_STARTED',           GRC_AUT_DIST_STARTED);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_SUPPLY_TOO_HIGH',        GRC_AUT_SUPPLY_TOO_HIGH);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_SUPPLY_TOO_LOW',         GRC_AUT_SUPPLY_TOO_LOW);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_NO_WORKING_AREA',        GRC_AUT_NO_WORKING_AREA);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_ARRAY_FULL',             GRC_AUT_ARRAY_FULL);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_AUT_NO_DATA',                GRC_AUT_NO_DATA);                if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_KDM_NOT_AVAILABLE',          GRC_KDM_NOT_AVAILABLE);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_FTR_FILEACCESS',             GRC_FTR_FILEACCESS);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_FTR_WRONGFILEBLOCKNUMBER',   GRC_FTR_WRONGFILEBLOCKNUMBER);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_FTR_NOTENOUGHSPACE',         GRC_FTR_NOTENOUGHSPACE);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_FTR_INVALIDINPUT',           GRC_FTR_INVALIDINPUT);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'GRC_FTR_MISSINGSETUP',           GRC_FTR_MISSINGSETUP);           if (dm_is_error(rc)) return
        end if

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
        pos_mode = dm_geocom_type_validated(GEOCOM_AUT_POSMODE, dm_lua_to_int32(lua, 1))
        atr_mode = dm_geocom_type_validated(GEOCOM_AUT_ATRMODE, dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_change_face(request, pos_mode, atr_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_change_face

    integer(kind=c_int) function lua_geocom_delete(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        character(len=:), allocatable :: file_name
        integer                       :: device_type, file_type
        integer                       :: day, month, year
        type(lua_state_type)          :: lua
        type(request_type)            :: request

        lua = lua_state_type(ptr)
        device_type = dm_geocom_type_validated(GEOCOM_FTR_DEVICETYPE, dm_lua_to_int32(lua, 1))
        file_type   = dm_geocom_type_validated(GEOCOM_FTR_FILETYPE,   dm_lua_to_int32(lua, 2))
        day         = dm_lua_to_int32(lua, 3)
        month       = dm_lua_to_int32(lua, 4)
        year        = dm_lua_to_int32(lua, 5)
        file_name   = dm_lua_to_string(lua, 6)
        call dm_geocom_api_request_delete(request, device_type, file_type, day, month, year, file_name)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_delete

    integer(kind=c_int) function lua_geocom_do_measure(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: inc_mode, tmc_prog
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        tmc_prog = dm_geocom_type_validated(GEOCOM_TMC_MEASURE_PRG, dm_lua_to_int32(lua, 1))
        inc_mode = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_do_measure(request, tmc_prog, inc_mode)
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
        inc_mode = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, dm_lua_to_int32(lua, 1))
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
        inc_mode = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_get_angle_complete(request, inc_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_angle_complete

    integer(kind=c_int) function lua_geocom_get_angle_correction(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_request_get_angle_correction(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_angle_correction

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
        wait_time = max(0, dm_lua_to_int32(lua, 1))
        inc_mode  = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_get_coordinate(request, wait_time, inc_mode)
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
        wait_time = max(0, dm_lua_to_int32(lua, 1))
        inc_mode  = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_get_full_measurement(request, wait_time, inc_mode)
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
        mem_type = dm_geocom_type_validated(GEOCOM_IMG_MEM_TYPE, dm_lua_to_int32(lua, 1))
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
        prism_type = dm_geocom_type_validated(GEOCOM_BAP_PRISMTYPE, dm_lua_to_int32(lua, 1))
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
        wait_time = max(0, dm_lua_to_int32(lua, 1))
        inc_mode  = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_get_simple_coordinates(request, wait_time, inc_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_get_simple_coordinates

    integer(kind=c_int) function lua_geocom_get_simple_measurement(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: inc_mode, wait_time
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        wait_time = max(0, dm_lua_to_int32(lua, 1))
        inc_mode  = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, dm_lua_to_int32(lua, 2))
        call dm_geocom_api_request_get_simple_measurement(request, wait_time, inc_mode)
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

    integer(kind=c_int) function lua_geocom_measure_distance_angle(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: dist_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        dist_mode = dm_geocom_type_validated(GEOCOM_BAP_MEASURE_PRG, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_measure_distance_angle(request, dist_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_measure_distance_angle

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

        integer              :: direction
        logical              :: swing
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        direction = max(-1, min(1, dm_lua_to_int32(lua, 1)))
        swing     = dm_lua_to_logical(lua, 2)
        call dm_geocom_api_request_ps_search_next(request, direction, swing)
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

        integer              :: max_dist, min_dist
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        min_dist = dm_lua_to_int32(lua, 1)
        max_dist = dm_lua_to_int32(lua, 2)
        call dm_geocom_api_request_ps_set_range(request, min_dist, max_dist)
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

        logical              :: collimation, incline, stand_axis, tilt_axis
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        incline     = dm_lua_to_logical(lua, 1)
        stand_axis  = dm_lua_to_logical(lua, 2)
        collimation = dm_lua_to_logical(lua, 3)
        tilt_axis   = dm_lua_to_logical(lua, 4)
        call dm_geocom_api_request_set_angle_correction(request, incline, stand_axis, collimation, tilt_axis)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_angle_correction

    integer(kind=c_int) function lua_geocom_set_atmospheric_correction(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=i8)        :: dry_temp, lambda, pressure, wet_temp
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        lambda   = dm_lua_to_real64(lua, 1)
        pressure = dm_lua_to_real64(lua, 2)
        dry_temp = dm_lua_to_real64(lua, 3)
        wet_temp = dm_lua_to_real64(lua, 4)
        call dm_geocom_api_request_set_atmospheric_correction(request, lambda, pressure, dry_temp, wet_temp)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_atmospheric_correction

    integer(kind=c_int) function lua_geocom_set_atmospheric_ppm(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: atm_ppm
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        atm_ppm = dm_lua_to_real64(lua, 1)
        call dm_geocom_api_request_set_atmospheric_ppm(request, atm_ppm)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_atmospheric_ppm

    integer(kind=c_int) function lua_geocom_set_atr_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: atr_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        atr_mode = dm_geocom_type_validated(GEOCOM_BAP_ATRSETTING, dm_lua_to_int32(lua, 1))
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

        integer              :: auto_power, timeout
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        auto_power = dm_lua_to_int32(lua, 1)
        timeout    = dm_lua_to_int32(lua, 2)
        call dm_geocom_api_request_set_config(request, auto_power, timeout)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_config

    integer(kind=c_int) function lua_geocom_set_date_time(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: year, month, day
        integer              :: hour, minute, second
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        year   = dm_lua_to_int32(lua, 1)
        month  = dm_lua_to_int32(lua, 2)
        day    = dm_lua_to_int32(lua, 3)
        hour   = dm_lua_to_int32(lua, 4)
        minute = dm_lua_to_int32(lua, 5)
        second = dm_lua_to_int32(lua, 6)
        call dm_geocom_api_request_set_date_time(request, year, month, day, hour, minute, second)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_date_time

    integer(kind=c_int) function lua_geocom_set_distance(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: inc_mode
        real(kind=r8)        :: height_offset, slope_dist
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        slope_dist    = dm_lua_to_real64(lua, 1)
        height_offset = dm_lua_to_real64(lua, 2)
        inc_mode      = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, dm_lua_to_int32(lua, 3))
        call dm_geocom_api_request_set_distance(request, slope_dist, height_offset, inc_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_distance

    integer(kind=c_int) function lua_geocom_set_double_precision(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: ndigits
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        ndigits = dm_lua_to_int32(lua, 1)
        call dm_geocom_api_request_set_double_precision(request, ndigits)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_double_precision

    integer(kind=c_int) function lua_geocom_set_edm_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: edm_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        edm_mode = dm_geocom_type_validated(GEOCOM_EDM_MODE, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_set_edm_mode(request, edm_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_edm_mode

    integer(kind=c_int) function lua_geocom_set_egl_intensity(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: intensity
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        intensity = dm_geocom_type_validated(GEOCOM_EDM_EGLINTENSITY_TYPE, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_set_egl_intensity(request, intensity)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_egl_intensity

    integer(kind=c_int) function lua_geocom_set_fine_adjust_mode(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: adj_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        adj_mode = dm_geocom_type_validated(GEOCOM_AUT_ADJMODE, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_set_fine_adjust_mode(request, adj_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_fine_adjust_mode

    integer(kind=c_int) function lua_geocom_set_geometric_ppm(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        logical              :: enabled
        real(kind=r8)        :: height_ppm, individual_ppm, offset, scale_factor
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        enabled        = dm_lua_to_logical(lua, 1)
        scale_factor   = dm_lua_to_real64(lua, 2)
        offset         = dm_lua_to_real64(lua, 3)
        height_ppm     = dm_lua_to_real64(lua, 4)
        individual_ppm = dm_lua_to_real64(lua, 5)
        call dm_geocom_api_request_set_geometric_ppm(request, enabled, scale_factor, offset, height_ppm, individual_ppm)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_geometric_ppm

    integer(kind=c_int) function lua_geocom_set_height(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: height
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        height = dm_lua_to_real64(lua, 1)
        call dm_geocom_api_request_set_height(request, height)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_height

    integer(kind=c_int) function lua_geocom_set_image_config(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        character(len=:), allocatable :: prefix
        integer                       :: image_number, mem_type, quality, sub_function
        type(lua_state_type)          :: lua
        type(request_type)            :: request

        lua = lua_state_type(ptr)
        mem_type     = dm_geocom_type_validated(GEOCOM_IMG_MEM_TYPE, dm_lua_to_int32(lua, 1))
        image_number = dm_lua_to_int32(lua, 2)
        quality      = dm_lua_to_int32(lua, 3)
        sub_function = dm_lua_to_int32(lua, 4)
        prefix       = dm_lua_to_string(lua, 5)
        call dm_geocom_api_request_set_image_config(request, mem_type, image_number, quality, sub_function, prefix)
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

        integer              :: bap_prog
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        bap_prog = dm_geocom_type_validated(GEOCOM_BAP_USER_MEASPRG, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_set_measurement_program(request, bap_prog)
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
        hz       = dm_lua_to_real64(lua, 1)
        v        = dm_lua_to_real64(lua, 2)
        pos_mode = dm_geocom_type_validated(GEOCOM_AUT_POSMODE, dm_lua_to_int32(lua, 3))
        atr_mode = dm_geocom_type_validated(GEOCOM_AUT_ATRMODE, dm_lua_to_int32(lua, 4))
        call dm_geocom_api_request_set_position(request, hz, v, pos_mode, atr_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_position

    integer(kind=c_int) function lua_geocom_set_positioning_timeout(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: time_hz, time_v
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        time_hz = dm_lua_to_real64(lua, 1)
        time_v  = dm_lua_to_real64(lua, 2)
        call dm_geocom_api_request_set_positioning_timeout(request, time_hz, time_v)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_positioning_timeout

    integer(kind=c_int) function lua_geocom_set_prism_constant(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: prism_const
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        prism_const = dm_lua_to_real64(lua, 1)
        call dm_geocom_api_request_set_prism_constant(request, prism_const)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_prism_constant

    integer(kind=c_int) function lua_geocom_set_prism_type(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: prism_type
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        prism_type = dm_geocom_type_validated(GEOCOM_BAP_PRISMTYPE, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_set_prism_type(request, prism_type)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_prism_type

    integer(kind=c_int) function lua_geocom_set_prism_type_v2(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        character(len=:), allocatable :: prism_name
        integer                       :: prism_type
        type(lua_state_type)          :: lua
        type(request_type)            :: request

        lua = lua_state_type(ptr)
        prism_type = dm_geocom_type_validated(GEOCOM_BAP_PRISMTYPE, dm_lua_to_int32(lua, 1))
        prism_name = dm_lua_to_string(lua, 2)
        call dm_geocom_api_request_set_prism_type_v2(request, prism_type, prism_name)
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

        integer              :: mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        mode = max(1, min(2, dm_lua_to_int32(lua, 1)))
        call dm_geocom_api_request_set_refraction_mode(request, mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_refraction_mode

    integer(kind=c_int) function lua_geocom_set_search_area(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        logical              :: enabled
        real(kind=r8)        :: center_hz, center_v, range_hz, range_v
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        center_hz = dm_lua_to_real64(lua, 1)
        center_v  = dm_lua_to_real64(lua, 2)
        range_hz  = dm_lua_to_real64(lua, 3)
        range_v   = dm_lua_to_real64(lua, 4)
        enabled   = dm_lua_to_logical(lua, 5)
        call dm_geocom_api_request_set_search_area(request, center_hz, center_v, range_hz, range_v, enabled)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_search_area

    integer(kind=c_int) function lua_geocom_set_station(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        real(kind=r8)        :: easting, height, instr_height, northing
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        easting      = dm_lua_to_real64(lua, 1)
        northing     = dm_lua_to_real64(lua, 2)
        height       = dm_lua_to_real64(lua, 3)
        instr_height = dm_lua_to_real64(lua, 4)
        call dm_geocom_api_request_set_station(request, easting, northing, height, instr_height)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_set_station

    integer(kind=c_int) function lua_geocom_set_target_type(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: target_type
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        target_type = dm_geocom_type_validated(GEOCOM_BAP_TARGET_TYPE, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_set_target_type(request, target_type)
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

        character(len=:), allocatable :: creator, prism_name
        integer                       :: refl_type
        real(kind=r8)                 :: prism_const
        type(lua_state_type)          :: lua
        type(request_type)            :: request

        lua = lua_state_type(ptr)
        prism_name  = dm_lua_to_string(lua, 1)
        prism_const = dm_lua_to_real64(lua, 2)
        refl_type   = dm_geocom_type_validated(GEOCOM_BAP_REFLTYPE, dm_lua_to_int32(lua, 3))
        creator     = dm_lua_to_string(lua, 4)
        call dm_geocom_api_request_set_user_prism_definition(request, prism_name, prism_const, refl_type, creator)
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

        character(len=:), allocatable :: file_name
        integer                       :: block_size, device_type, file_type
        type(lua_state_type)          :: lua
        type(request_type)            :: request

        lua = lua_state_type(ptr)
        device_type = dm_geocom_type_validated(GEOCOM_FTR_DEVICETYPE, dm_lua_to_int32(lua, 1))
        file_type   = dm_geocom_type_validated(GEOCOM_FTR_FILETYPE,   dm_lua_to_int32(lua, 2))
        file_name   = dm_lua_to_string(lua, 3)
        block_size  = dm_lua_to_int32(lua, 4)
        call dm_geocom_api_request_setup_download(request, device_type, file_type, file_name, block_size)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_setup_download

    integer(kind=c_int) function lua_geocom_setup_list(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        character(len=:), allocatable :: search_path
        integer                       :: device_type, file_type
        type(lua_state_type)          :: lua
        type(request_type)            :: request

        lua = lua_state_type(ptr)
        device_type = dm_geocom_type_validated(GEOCOM_FTR_DEVICETYPE, dm_lua_to_int32(lua, 1))
        file_type   = dm_geocom_type_validated(GEOCOM_FTR_FILETYPE,   dm_lua_to_int32(lua, 2))
        search_path = dm_lua_to_string(lua, 3)
        call dm_geocom_api_request_setup_list(request, device_type, file_type, search_path)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_setup_list

    integer(kind=c_int) function lua_geocom_start_controller(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: start_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        start_mode = dm_geocom_type_validated(GEOCOM_MOT_MODE, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_start_controller(request, start_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_start_controller

    integer(kind=c_int) function lua_geocom_stop_controller(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: stop_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        stop_mode = dm_geocom_type_validated(GEOCOM_MOT_STOPMODE, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_stop_controller(request, stop_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_stop_controller

    integer(kind=c_int) function lua_geocom_switch_off(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: stop_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        stop_mode = dm_geocom_type_validated(GEOCOM_COM_TPS_STOP_MODE, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_switch_off(request, stop_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_switch_off

    integer(kind=c_int) function lua_geocom_switch_on(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: start_mode
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        start_mode = dm_geocom_type_validated(GEOCOM_COM_TPS_STARTUP_MODE, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_switch_on(request, start_mode)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_switch_on

    integer(kind=c_int) function lua_geocom_take_image(ptr) bind(c) result(n)
        type(c_ptr), intent(in), value :: ptr !! Lua state pointer.

        integer              :: mem_type
        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        mem_type = dm_geocom_type_validated(GEOCOM_IMG_MEM_TYPE, dm_lua_to_int32(lua, 1))
        call dm_geocom_api_request_take_image(request, mem_type)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_take_image
end module dm_lua_geocom
