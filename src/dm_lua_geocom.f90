! Author:  Philipp Engel
! Licence: ISC
module dm_lua_geocom
    !! GeoCOM API for Lua.
    use, intrinsic :: iso_c_binding
    use :: dm_error
    use :: dm_geocom_api
    use :: dm_lua
    use :: dm_request
    implicit none (type, external)
    private

    ! Public procedures.
    public  :: dm_lua_geocom_register

    ! Private procedures.
    private :: lua_geocom_beep_alarm
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_lua_geocom_register(lua) result(rc)
        !! Registers all GeoCOM API functions. Returns `E_INVALID` if the Lua
        !! interpreter is not initialised.
        !!
        !! The following Lua procedures are registered:
        !!
        !! * `geocom_beep_alarm()`
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

        call dm_lua_register(lua, 'geocom_beep_alarm', lua_geocom_beep_alarm)

        rc = E_NONE
    end function dm_lua_geocom_register

    ! **************************************************************************
    ! PRIVATE LUA INTERFACE PROCEDURES.
    ! **************************************************************************
    function lua_geocom_beep_alarm(ptr) bind(c) result(n)
        !! Lua function `geocom_beep_alarm()` that pushes the request table of
        !! GeoCOM command `BMM_BeepAlarm` onto the stack.
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_api_beep_alarm(request)
        call dm_lua_from(lua, request)
        n = 1
    end function lua_geocom_beep_alarm
end module dm_lua_geocom
