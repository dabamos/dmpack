! Author:  Philipp Engel
! Licence: ISC
module dm_ve
    !! VE.Direct (TTL) protocol abstraction layer, for Maximum Power Point
    !! Tracking (MPPT) solar charge controllers and battery monitors made by
    !! Victron Energy. This module supports:
    !!
    !! * BlueSolar MPPT series,
    !! * SmartSolar MPPT series,
    !! * SmartShunt.
    !!
    !! The following VE.Direct fields are captured:
    !!
    !! | Name    | Unit    | Description                               | MPPT | Shunt |
    !! |---------|---------|-------------------------------------------|------|-------|
    !! | `Alarm` | –       | Alarm condition active (on/off).          |      |   ✓   |
    !! | `AR`    | –       | Alarm reason (decimal).                   |      |   ✓   |
    !! | `CE`    | mAh     | Consumed amp hours.                       |      |   ✓   |
    !! | `CS`    | –       | State of operation.                       |  ✓   |       |
    !! | `DM`    | ‰       | Mid-point deviation of the battery bank.  |      |   ✓   |
    !! | `ERR`   | –       | Error code.                               |  ✓   |       |
    !! | `FW`    | –       | Firmware version (16 bit).                |  ✓   |   ✓   |
    !! | `H1`    | mAh     | Depth of the deepest discharge.           |      |   ✓   |
    !! | `H2`    | mAh     | Depth of the last discharge.              |      |   ✓   |
    !! | `H3`    | mAh     | Depth of the average discharge.           |      |   ✓   |
    !! | `H4`    | –       | Number of charge cycles.                  |      |   ✓   |
    !! | `H5`    | –       | Number of full discharges.                |      |   ✓   |
    !! | `H6`    | mAh     | Cumulative amp hours drawn.               |      |   ✓   |
    !! | `H7`    | mV      | Minimum main (battery) voltage.           |      |   ✓   |
    !! | `H8`    | mV      | Maximum main (battery) voltage.           |      |   ✓   |
    !! | `H9`    | sec     | Number of seconds since last full charge. |      |   ✓   |
    !! | `H10`   | –       | Number of automatic synchronisations.     |      |   ✓   |
    !! | `H11`   | –       | Number of low main voltage alarms.        |      |   ✓   |
    !! | `H12`   | –       | Number of high main voltage alarms.       |      |   ✓   |
    !! | `H15`   | mV      | Minimum auxiliary (battery) voltage.      |      |   ✓   |
    !! | `H16`   | mV      | Maximum auxiliary (battery) voltage.      |      |   ✓   |
    !! | `H17`   | kWh/100 | Amount of produced energy.                |      |   ✓   |
    !! | `H18`   | kWh/100 | Amount of consumed energy.                |      |   ✓   |
    !! | `H19`   | kWh/100 | Yield total (user resettable counter).    |  ✓   |       |
    !! | `H20`   | kWh/100 | Yield today.                              |  ✓   |       |
    !! | `H21`   | W       | Maximum power today.                      |  ✓   |       |
    !! | `H22`   | kWh/100 | Yield yesterday.                          |  ✓   |       |
    !! | `H23`   | W       | Maximum power yesterday.                  |  ✓   |       |
    !! | `HSDS`  | –       | Day sequence number (0 to 364).           |  ✓   |       |
    !! | `I`     | mA      | Main or channel 1 battery current.        |  ✓   |   ✓   |
    !! | `IL`    | mA      | Load current.                             |  ✓   |       |
    !! | `LOAD`  | –       | Load output state (on/off).               |  ✓   |       |
    !! | `MON`   | –       | DC monitor mode.                          |      |   ✓   |
    !! | `MPPT`  | –       | Tracker operation mode.                   |  ✓   |       |
    !! | `OR`    | –       | Off reason.                               |  ✓   |       |
    !! | `P`     | W       | Instantaneous power.                      |      |   ✓   |
    !! | `PID`   | –       | Product ID.                               |  ✓   |   ✓   |
    !! | `PPV`   | W       | Panel power.                              |  ✓   |       |
    !! | `Relay` | –       | Relay state (on/off).                     |  ✓   |   ✓   |
    !! | `SOC`   | ‰       | State-of-charge.                          |      |   ✓   |
    !! | `T`     | °C      | Battery temperature.                      |      |   ✓   |
    !! | `TTG`   | min     | Time-to-go.                               |      |   ✓   |
    !! | `V`     | mV      | Main or channel 1 (battery) voltage.      |  ✓   |   ✓   |
    !! | `VM`    | mV      | Mid-point voltage of the battery bank.    |      |   ✓   |
    !! | `VPV`   | mV      | Panel voltage.                            |  ✓   |       |
    !! | `VS`    | mV      | Auxiliary (starter) voltage.              |      |   ✓   |
    !!
    !! The TTY has to be configured to these serial port parameters:
    !!
    !! | Parameter    | Value |
    !! |--------------|-------|
    !! | Baud rate    | 19200 |
    !! | Data bits    | 8     |
    !! | Parity       | –     |
    !! | Stop bits    | 1     |
    !! | Flow control | –     |
    !!
    !! The device transmits blocks of data at 1 second intervals. Each field is
    !! sent using the following format:
    !!
    !! ```
    !! <Newline><Field-Label><Tab><Field-Value>
    !! ```
    !!
    !! The identifiers are defined as:
    !!
    !! | Identifier      | Description                                                 |
    !! |-----------------|-------------------------------------------------------------|
    !! | `<Newline>`     | A carriage return followed by a line feed (`0x0D`, `0x0A`). |
    !! | `<Field-Label>` | An arbitrary length label that identifies the field.        |
    !! | `<Tab>`         | A horizontal tab (`0x09`).                                  |
    !! | `<Field-Value>` | The ASCII formatted value of this field.                    |
    !!
    !! The statistics are grouped in blocks with a checksum appended. The last
    !! field in a block will always be `Checksum`. The value is a single byte,
    !! and will not necessarily be a printable ASCII character. The modulo 256
    !! sum of all bytes in a block will equal 0 if there were no transmission
    !! errors. Multiple blocks are sent containing different fields.
    !!
    !! ## Fields
    !!
    !! Description of fields returned by MPPT chargers and SmartShunt.
    !!
    !! ## Alarm
    !!
    !! This shows the buzzer alarm state of the BMV. During normal operation,
    !! this will be `OFF`. When a buzzer alarm occurs the value will change to
    !! `ON`.
    !!
    !! This refers to the value of the alarm condition, and not the buzzer
    !! itself. This means that once a condition has occurred, the value will be
    !! `ON` until all alarm conditions have cleared; regardless of whether or
    !! not a button has been pressed to silence the buzzer.
    !!
    !! ## AR
    !!
    !! Alarm reason; this field describes the cause of the alarm. Since
    !! multiple alarm conditions can be present at the same time the values of
    !! the separate alarm conditions are added. The value total is sent in
    !! decimal notation.
    !!
    !! | Value | Description          |
    !! |-------|----------------------|
    !! | 1     | low voltage          |
    !! | 2     | high voltage         |
    !! | 4     | low SOC              |
    !! | 8     | low starter voltage  |
    !! | 16    | high starter voltage |
    !! | 32    | low temperature      |
    !! | 64    | high temperature     |
    !! | 128   | mid voltage          |
    !!
    !! ## CS
    !!
    !! The state of the MPPT operation.
    !!
    !! | Value | Description               |
    !! |-------|---------------------------|
    !! | 0     | off                       |
    !! | 2     | fault                     |
    !! | 3     | bulk                      |
    !! | 4     | absorption                |
    !! | 5     | float                     |
    !! | 7     | equalise (manual)         |
    !! | 245   | starting-up               |
    !! | 247   | auto-equalise/recondition |
    !! | 252   | external control          |
    !!
    !! ### ERR
    !!
    !! The error code of the device, relevant when the device is in fault
    !! state.
    !!
    !! Error 19 can be ignored, this condition regularly occurs during start-up
    !! or shutdown of the MPPT charger. Since version 1.15 this error will no
    !! longer be reported.
    !!
    !! Error 21 can be ignored for 5 minutes, this condition regularly occurs
    !! during start-up or shutdown of the MPPT charger. Since version 1.16 this
    !! warning will no longer be reported when it is not persistent.
    !!
    !! | Value | Description                                           |
    !! |-------|-------------------------------------------------------|
    !! | 0     | No error.                                             |
    !! | 2     | Battery voltage too high.                             |
    !! | 17    | Charger temperature too high.                         |
    !! | 18    | Charger over current.                                 |
    !! | 19    | Charger current reversed.                             |
    !! | 20    | Bulk time limit exceeded.                             |
    !! | 21    | Current sensor issue (sensor bias/sensor broken).     |
    !! | 26    | Terminals overheated.                                 |
    !! | 28    | Converter issue (dual converter models only).         |
    !! | 33    | Input voltage too high (solar panel).                 |
    !! | 34    | Input current too high (solar panel).                 |
    !! | 38    | Input shutdown (due to excessive battery voltage).    |
    !! | 39    | Input shutdown (due to current flow during off mode). |
    !! | 65    | Lost communication with one of devices.               |
    !! | 66    | Synchronised charging device configuration issue.     |
    !! | 67    | BMS connection lost.                                  |
    !! | 68    | Network misconfigured.                                |
    !! | 116   | Factory calibration data lost.                        |
    !! | 117   | Invalid/incompatible firmware.                        |
    !! | 119   | User settings invalid.                                |
    !!
    !! ### FW
    !!
    !! The firmware version of the device. The version is reported as a whole
    !! number, e.g. 208 for firmware version 2.08.
    !!
    !! ### HSDS
    !!
    !! The day sequence number in range 0 to 364. A change in this number
    !! indicates a new day. This implies that the historical data has changed.
    !!
    !! ### MPPT
    !!
    !! The tracker operation mode.
    !!
    !! | Value | Description                |
    !! |-------|----------------------------|
    !! | 0     | off                        |
    !! | 1     | voltage or current limited |
    !! | 2     | MPPT active                |
    !!
    !! ### OR
    !!
    !! The off reason of the charger. This field describes why a unit is
    !! switched off.
    !!
    !! | Value      | Description                         |
    !! |------------|-------------------------------------|
    !! | 0x00000001 | no input power                      |
    !! | 0x00000002 | switched off (power switch)         |
    !! | 0x00000004 | switched off (device mode register) |
    !! | 0x00000008 | remote input                        |
    !! | 0x00000010 | protection active                   |
    !! | 0x00000020 | pay-as-you-go (PAYGo)               |
    !! | 0x00000040 | BMS                                 |
    !! | 0x00000080 | engine shutdown detection           |
    !! | 0x00000100 | analysing input voltage             |
    !!
    !! ### Relay
    !!
    !! During normal operation of the MPPT charger, the relay is `OFF`. If
    !! there is a battery low voltage condition, the value will change to `ON`.
    !!
    !! ### SER#
    !!
    !! The serial number of the device. The notation is `LLYYMMSSSSS`, where
    !! `LL` is the location code, `YYWW` is the production date stamp (year,
    !! week), and `SSSSS` is a unique part of the serial number.
    !!
    !! The field is not supported by DMPACK and should be ignored.
    !!
    !! ## Example
    !!
    !! The snipped reads a single VE.Direct block from sequentially passed
    !! character `byte`, adds it to a frame, and converts the frame to a
    !! response type once finished:
    !!
    !! ```fortran
    !! integer             :: rc
    !! logical             :: eor, finished, valid
    !! type(ve_frame_type) :: frame
    !! type(response_type) :: response
    !!
    !! call dm_ve_frame_next(frame, byte, eor, finished, valid)
    !!
    !! if (finished) then
    !!     if (valid) then
    !!         print '("Record is valid")'
    !!     else
    !!         print '("Record is invalid")'
    !!     end if
    !!
    !!     return
    !! end if
    !!
    !! if (eor) then
    !!     call dm_ve_frame_read(frame, response)
    !!     print '("Label: ", a, " Value: ", a)',    frame%label, trim(frame%value)
    !!     print '("Name:  ", a, " Value: ", f8.1)', response%name, response%value
    !! end if
    !!
    !! call dm_ve_frame_reset(frame)
    !! ```
    !!
    !! ## References
    !!
    !! * [VE.Direct Protocol v.3.33](https://www.victronenergy.com/upload/documents/VE.Direct-Protocol-3.33.pdf) (6 June 2023)
    !!
    use :: dm_error
    use :: dm_kind
    use :: dm_response
    use :: dm_tty
    implicit none (type, external)
    private

    ! Supported Victron Energy devices.
    integer, parameter, public :: VE_DEVICE_NONE  = 0 !! No device (invalid).
    integer, parameter, public :: VE_DEVICE_MPPT  = 1 !! BlueSolar and SmartSolar MPPT.
    integer, parameter, public :: VE_DEVICE_SHUNT = 2 !! SmartShunt (SS).
    integer, parameter, public :: VE_DEVICE_LAST  = 2 !! Never use this

    integer, parameter, public :: VE_DEVICE_NAME_LEN = 5 !! Max. device name length.

    character(len=*), parameter, public :: VE_DEVICE_NAMES(VE_DEVICE_NONE:VE_DEVICE_LAST) = [ &
        character(len=VE_DEVICE_NAME_LEN) :: 'none', 'mppt', 'shunt' &
    ] !! Device names.

    ! Character lenghts.
    integer, parameter, public :: VE_LABEL_LEN = 8                 !! Max. field label length (minus newline).
    integer, parameter, public :: VE_NAME_LEN  = RESPONSE_NAME_LEN !! Max. response name length.
    integer, parameter, public :: VE_UNIT_LEN  = RESPONSE_UNIT_LEN !! Max. field unit length.
    integer, parameter, public :: VE_VALUE_LEN = 32                !! Max. field value length (minus tab).

    ! TTY parameters.
    integer, parameter, public :: VE_TTY_ACCESS    = TTY_RDONLY      !! Default TTY access mode.
    integer, parameter, public :: VE_TTY_BAUD_RATE = TTY_B19200      !! Default TTY baud rate.
    integer, parameter, public :: VE_TTY_BYTE_SIZE = TTY_BYTE_SIZE8  !! Default TTY byte size.
    integer, parameter, public :: VE_TTY_PARITY    = TTY_PARITY_NONE !! Default TTY parity.
    integer, parameter, public :: VE_TTY_STOP_BITS = TTY_STOP_BITS1  !! Default TTY stop bits.

    ! VE.Direct frame states.
    integer, parameter :: VE_STATE_IDLE     = 0 !! Idle.
    integer, parameter :: VE_STATE_BEGIN    = 1 !! Begin record.
    integer, parameter :: VE_STATE_LABEL    = 2 !! Label field.
    integer, parameter :: VE_STATE_VALUE    = 3 !! Value field.
    integer, parameter :: VE_STATE_CHECKSUM = 4 !! Checksum.

    type, public :: ve_frame_type
        !! VE.Direct frame state.
        integer                     :: state    = VE_STATE_IDLE !! Current state.
        integer                     :: checksum = 0             !! Current checksum value.
        integer                     :: cursor   = 0             !! String cursor.
        logical                     :: finished = .false.       !! Block is finished.
        character(len=VE_LABEL_LEN) :: label    = ' '           !! Label string.
        character(len=VE_VALUE_LEN) :: value    = ' '           !! Value string.
    end type ve_frame_type

    type, public :: ve_field_type
        !! VE.Direct field description.
        character(len=VE_LABEL_LEN) :: label  = ' '                 !! Field label.
        character(len=VE_NAME_LEN)  :: name   = ' '                 !! Response name (must be valid id).
        character(len=VE_UNIT_LEN)  :: unit   = ' '                 !! Field unit.
        integer                     :: type   = RESPONSE_TYPE_INT32 !! Field value type.
        character(len=VE_VALUE_LEN) :: value  = ' '                 !! Field value.
    end type ve_field_type

    ! Supported VE.Direct field types.
    integer, parameter, public :: VE_FIELD_NONE  = 0  !! None (invalid).
    integer, parameter, public :: VE_FIELD_ALARM = 1  !! [      SS ] Alarm condition active (ON/OFF).
    integer, parameter, public :: VE_FIELD_AR    = 2  !! [      SS ] Alarm reason (decimal).
    integer, parameter, public :: VE_FIELD_CE    = 3  !! [      SS ] Consumed amp hours.
    integer, parameter, public :: VE_FIELD_CS    = 4  !! [ MPPT    ] State of operation.
    integer, parameter, public :: VE_FIELD_DM    = 5  !! [      SS ] Mid-point deviation of the battery bank.
    integer, parameter, public :: VE_FIELD_ERR   = 6  !! [ MPPT    ] Error code.
    integer, parameter, public :: VE_FIELD_FW    = 7  !! [ MPPT SS ] Firmware version (16 bit).
    integer, parameter, public :: VE_FIELD_H1    = 8  !! [      SS ] Depth of the deepest discharge.
    integer, parameter, public :: VE_FIELD_H2    = 9  !! [      SS ] Depth of the last discharge.
    integer, parameter, public :: VE_FIELD_H3    = 10 !! [      SS ] Depth of the average discharge.
    integer, parameter, public :: VE_FIELD_H4    = 11 !! [      SS ] Number of charge cycles.
    integer, parameter, public :: VE_FIELD_H5    = 12 !! [      SS ] Number of full discharges.
    integer, parameter, public :: VE_FIELD_H6    = 13 !! [      SS ] Cumulative amp hours drawn
    integer, parameter, public :: VE_FIELD_H7    = 14 !! [      SS ] Minimum main (battery) voltage.
    integer, parameter, public :: VE_FIELD_H8    = 15 !! [      SS ] Maximum main (battery) voltage.
    integer, parameter, public :: VE_FIELD_H9    = 16 !! [      SS ] Number of seconds since last full charge.
    integer, parameter, public :: VE_FIELD_H10   = 17 !! [      SS ] Number of automatic synchronisations.
    integer, parameter, public :: VE_FIELD_H11   = 18 !! [      SS ] Number of low main voltage alarms.
    integer, parameter, public :: VE_FIELD_H12   = 19 !! [      SS ] Number of high main voltage alarms.
    integer, parameter, public :: VE_FIELD_H15   = 20 !! [      SS ] Minimum auxiliary (battery) voltage.
    integer, parameter, public :: VE_FIELD_H16   = 21 !! [      SS ] Maximum auxiliary (battery) voltage.
    integer, parameter, public :: VE_FIELD_H17   = 22 !! [      SS ] Amount of produced energy.
    integer, parameter, public :: VE_FIELD_H18   = 23 !! [      SS ] Amount of consumed energy.
    integer, parameter, public :: VE_FIELD_H19   = 24 !! [ MPPT    ] Yield total (user resettable counter).
    integer, parameter, public :: VE_FIELD_H20   = 25 !! [ MPPT    ] Yield today.
    integer, parameter, public :: VE_FIELD_H21   = 26 !! [ MPPT    ] Maximum power today.
    integer, parameter, public :: VE_FIELD_H22   = 27 !! [ MPPT    ] Yield yesterday.
    integer, parameter, public :: VE_FIELD_H23   = 28 !! [ MPPT    ] Maximum power yesterday.
    integer, parameter, public :: VE_FIELD_HSDS  = 29 !! [ MPPT    ] Day sequence number (0 to 364).
    integer, parameter, public :: VE_FIELD_I     = 30 !! [ MPPT SS ] Main or channel 1 battery current.
    integer, parameter, public :: VE_FIELD_IL    = 31 !! [ MPPT    ] Load current.
    integer, parameter, public :: VE_FIELD_LOAD  = 32 !! [ MPPT    ] Load output state (ON/OFF).
    integer, parameter, public :: VE_FIELD_MON   = 33 !! [      SS ] DC monitor mode.
    integer, parameter, public :: VE_FIELD_MPPT  = 34 !! [ MPPT    ] Tracker operation mode.
    integer, parameter, public :: VE_FIELD_OR    = 35 !! [ MPPT    ] Off reason (hexadecimal).
    integer, parameter, public :: VE_FIELD_P     = 36 !! [      SS ] Instantaneous power.
    integer, parameter, public :: VE_FIELD_PID   = 37 !! [ MPPT SS ] Product ID (hexadecimal).
    integer, parameter, public :: VE_FIELD_PPV   = 38 !! [ MPPT    ] Panel power.
    integer, parameter, public :: VE_FIELD_RELAY = 39 !! [ MPPT SS ] Relay state (ON/OFF).
    integer, parameter, public :: VE_FIELD_SOC   = 40 !! [      SS ] State-of-charge.
    integer, parameter, public :: VE_FIELD_T     = 41 !! [      SS ] Battery temperature.
    integer, parameter, public :: VE_FIELD_TTG   = 42 !! [      SS ] Time-to-go.
    integer, parameter, public :: VE_FIELD_V     = 43 !! [ MPPT SS ] Main or channel 1 (battery) voltage.
    integer, parameter, public :: VE_FIELD_VM    = 44 !! [      SS ] Mid-point voltage of the battery bank.
    integer, parameter, public :: VE_FIELD_VPV   = 45 !! [ MPPT    ] Panel voltage.
    integer, parameter, public :: VE_FIELD_VS    = 46 !! [      SS ] Auxiliary (starter) voltage.
    integer, parameter, public :: VE_FIELD_LAST  = 46 !! Never use this.

    ! VE.Direct default fields (MPPT only).
    integer, parameter, public :: VE_NFIELDS = VE_FIELD_LAST !! Number of supported fields.

    type(ve_field_type), parameter, public :: VE_FIELDS(VE_NFIELDS) = [    &
        ve_field_type('ALARM', 'alarm', 'none',    RESPONSE_TYPE_LOGICAL), &
        ve_field_type('AR',    'ar',    'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('CE',    'ce',    'mAh',     RESPONSE_TYPE_INT32),   &
        ve_field_type('CS',    'cs',    'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('DM',    'dm',    '%/10',    RESPONSE_TYPE_INT32),   &
        ve_field_type('ERR',   'err',   'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('FW',    'fw',    'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('H1',    'h1',    'mAh',     RESPONSE_TYPE_INT32),   &
        ve_field_type('H2',    'h2',    'mAh',     RESPONSE_TYPE_INT32),   &
        ve_field_type('H3',    'h3',    'mAh',     RESPONSE_TYPE_INT32),   &
        ve_field_type('H4',    'h4',    'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('H5',    'h5',    'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('H6',    'h6',    'mAh',     RESPONSE_TYPE_INT32),   &
        ve_field_type('H7',    'h7',    'mV',      RESPONSE_TYPE_INT32),   &
        ve_field_type('H8',    'h8',    'mV',      RESPONSE_TYPE_INT32),   &
        ve_field_type('H9',    'h9',    'sec',     RESPONSE_TYPE_INT32),   &
        ve_field_type('H10',   'h10',   'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('H11',   'h11',   'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('H12',   'h12',   'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('H15',   'h15',   'mV',      RESPONSE_TYPE_INT32),   &
        ve_field_type('H16',   'h16',   'mV',      RESPONSE_TYPE_INT32),   &
        ve_field_type('H17',   'h17',   'kWh/100', RESPONSE_TYPE_INT32),   &
        ve_field_type('H18',   'h18',   'kWh/100', RESPONSE_TYPE_INT32),   &
        ve_field_type('H19',   'h19',   'kWh/100', RESPONSE_TYPE_INT32),   &
        ve_field_type('H20',   'h20',   'kWh/100', RESPONSE_TYPE_INT32),   &
        ve_field_type('H21',   'h21',   'W',       RESPONSE_TYPE_INT32),   &
        ve_field_type('H22',   'h22',   'kWh/100', RESPONSE_TYPE_INT32),   &
        ve_field_type('H23',   'h23',   'W',       RESPONSE_TYPE_INT32),   &
        ve_field_type('HSDS',  'hsds',  'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('I',     'i',     'mA',      RESPONSE_TYPE_INT32),   &
        ve_field_type('IL',    'il',    'mA',      RESPONSE_TYPE_INT32),   &
        ve_field_type('LOAD',  'load',  'none',    RESPONSE_TYPE_LOGICAL), &
        ve_field_type('MON',   'mon',   'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('MPPT',  'mppt',  'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('OR',    'or',    'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('P',     'p',     'W',       RESPONSE_TYPE_INT32),   &
        ve_field_type('PID',   'pid',   'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('PPV',   'ppv',   'W',       RESPONSE_TYPE_INT32),   &
        ve_field_type('RELAY', 'relay', 'none',    RESPONSE_TYPE_LOGICAL), &
        ve_field_type('SOC',   'soc',   '%/10',    RESPONSE_TYPE_INT32),   &
        ve_field_type('T',     't',     'degC',    RESPONSE_TYPE_INT32),   &
        ve_field_type('TTG',   'ttg',   'min',     RESPONSE_TYPE_INT32),   &
        ve_field_type('V',     'v',     'mV',      RESPONSE_TYPE_INT32),   &
        ve_field_type('VM',    'vm',    'mV',      RESPONSE_TYPE_INT32),   &
        ve_field_type('VPV',   'vpv',   'mV',      RESPONSE_TYPE_INT32),   &
        ve_field_type('VS',    'vs',    'mV',      RESPONSE_TYPE_INT32)    &
    ] !! Predefined fields.

    public :: dm_ve_device_from_name
    public :: dm_ve_device_is_valid
    public :: dm_ve_error_message
    public :: dm_ve_field_label
    public :: dm_ve_field_type
    public :: dm_ve_frame_next
    public :: dm_ve_frame_read
    public :: dm_ve_frame_reset
    public :: dm_ve_is_error
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental integer function dm_ve_device_from_name(name) result(device)
        !! Returns device enumerator from given name. If the argument is not a
        !! valid device, the function returns `VE_DEVICE_NONE`.
        use :: dm_string, only: dm_to_lower

        character(len=*), intent(in)      :: name !! Device name.
        character(len=VE_DEVICE_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (VE_DEVICE_NAMES(VE_DEVICE_MPPT));  device = VE_DEVICE_MPPT
            case (VE_DEVICE_NAMES(VE_DEVICE_SHUNT)); device = VE_DEVICE_SHUNT
            case default;                            device = VE_DEVICE_NONE
        end select
    end function dm_ve_device_from_name

    pure elemental logical function dm_ve_device_is_valid(device) result(is)
        !! Returns `.true.` if given VE device enumerator is valid.
        integer, intent(in) :: device !! Device enumerator.

        is = (device > VE_DEVICE_NONE .and. device <= VE_DEVICE_LAST)
    end function dm_ve_device_is_valid

    pure function dm_ve_error_message(code) result(message)
        !! Returns message associated with given VE.Direct error code.
        use :: dm_util, only: dm_itoa

        integer, intent(in)           :: code    !! VE.Direct error code.
        character(len=:), allocatable :: message !! VE.Direct error code message.

        select case (code)
            case (  0);   message = 'no error'
            case (  2);   message = 'battery voltage too high'
            case ( 17);   message = 'charger temperature too high'
            case ( 18);   message = 'charger over current'
            case ( 19);   message = 'charger current reversed'
            case ( 20);   message = 'bulk time limit exceeded'
            case ( 21);   message = 'current sensor issue (sensor bias/sensor broken)'
            case ( 26);   message = 'terminals overheated'
            case ( 28);   message = 'converter issue (dual converter models only)'
            case ( 33);   message = 'input voltage too high (solar panel)'
            case ( 34);   message = 'input current too high (solar panel)'
            case ( 38);   message = 'input shutdown (due to excessive battery voltage)'
            case ( 39);   message = 'input shutdown (due to current flow during off mode)'
            case ( 65);   message = 'lost communication with one of devices'
            case ( 66);   message = 'synchronised charging device configuration issue'
            case ( 67);   message = 'BMS connection lost'
            case ( 68);   message = 'network misconfigured'
            case (116);   message = 'factory calibration data lost'
            case (117);   message = 'invalid/incompatible firmware'
            case (119);   message = 'user settings invalid'
            case default; message = 'unknown VE.Direct code (' // dm_itoa(code) // ')'
        end select
    end function dm_ve_error_message

    pure function dm_ve_field_label(type) result(label)
        !! Returns field label as allocatable string, or empty string if type
        !! is invalid.
        integer, intent(in)           :: type  !! Field type.
        character(len=:), allocatable :: label !! Field label.

        if (.not. dm_ve_is_valid_field_type(type)) then
            label = ''
            return
        end if

        label = trim(VE_FIELDS(type)%label)
    end function dm_ve_field_label

    pure integer function dm_ve_field_type(label) result(type)
        !! Returns VE.Direct field type (`VE_FIELD_*`) from label, or
        !! `VE_FIELD_NONE` on error.
        character(len=*), intent(in) :: label !! Field label.

        integer :: i, n

        type = VE_FIELD_NONE

        n = len_trim(label)
        if (n == 0) return
        n = min(n, VE_LABEL_LEN)

        do i = 1, VE_NFIELDS
            if (label(1:n) /= VE_FIELDS(i)%label) cycle
            type = i
            exit
        end do
    end function dm_ve_field_type

    pure elemental logical function dm_ve_is_error(code) result(is)
        !! Returns `.true.` if given code is a valid VE.Direct error code.
        integer, intent(in) :: code !! VE.Direct error code.

        is = .false.

        select case (code)
            case (2, 17, 18, 19, 20, 21, 26, 28, 33, 34, 38, 39, 65, 66, 67, 68, 116, 117, 119)
                is = .true.
            case default
                return
        end select
    end function dm_ve_is_error

    pure elemental logical function dm_ve_is_valid_field_type(type) result(is)
        !! Returns `.true.` if given type is a valid field enumerator
        !! (`VE_FIELD_*`). The enumerator `VE_FIELD_NONE` is invalid.
        integer, intent(in) :: type !! Field type.

        is = (type > VE_FIELD_NONE .and. type <= VE_FIELD_LAST)
    end function dm_ve_is_valid_field_type

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    pure elemental subroutine dm_ve_frame_next(frame, byte, eor, finished, valid)
        !! State machine to read VE.Direct text protocol frame. Argument `eor`
        !! is `.true.` if a single record has been read. Argument `finished`
        !! is `.true.` if a block has been read. Argument `valid` is `.true.`
        !! if the checksum of a finished block is valid.
        !!
        !! If `eor` is `.true.`, read the frame with `dm_ve_frame_read()`
        !! afterwards. If `finished` is `.true.`, reset the frame before
        !! reading the next block.
        !!
        !! The routine converts frame labels and values to upper-case.
        !!
        !! ## References
        !!
        !! * [VE.Direct Protocol FAQ](https://www.victronenergy.com/live/vedirect_protocol:faq)
        !!
        use :: dm_ascii
        use :: dm_string, only: dm_to_upper

        type(ve_frame_type), intent(inout) :: frame    !! VE.Direct protocol frame.
        character,           intent(in)    :: byte     !! Single byte to add.
        logical,             intent(out)   :: eor      !! End of frame record reached.
        logical,             intent(out)   :: finished !! Block finished.
        logical,             intent(out)   :: valid    !! Finished block is valid.

        character :: a

        eor      = .false.
        finished = frame%finished
        valid    = (frame%checksum == 0)

        if (finished) return

        frame%checksum = modulo(frame%checksum + ichar(byte), 256)
        a = dm_to_upper(byte)

        select case (frame%state)
            case (VE_STATE_IDLE)
                select case (a)
                    case (ASCII_LF)
                        frame%state = VE_STATE_BEGIN

                    case (ASCII_CR)
                        continue ! skip
                end select

            case (VE_STATE_BEGIN)
                frame%label = ' '
                frame%value = ' '
                frame%cursor = 1
                frame%label(1:1) = a
                frame%state = VE_STATE_LABEL

            case (VE_STATE_LABEL)
                select case (a)
                    case (ASCII_TAB)
                        if (frame%label == 'CHECKSUM') then
                            frame%state = VE_STATE_CHECKSUM
                        else
                            frame%cursor = 0
                            frame%state  = VE_STATE_VALUE
                        end if

                    case default
                        if (frame%cursor < VE_LABEL_LEN) then
                            frame%cursor = frame%cursor + 1
                            frame%label(frame%cursor:frame%cursor) = a
                        end if
                end select

            case (VE_STATE_VALUE)
                select case (a)
                    case (ASCII_LF)
                        eor = .true.
                        frame%state = VE_STATE_BEGIN

                    case (ASCII_CR)
                        continue ! skip

                    case default
                        if (frame%cursor < VE_VALUE_LEN) then
                            frame%cursor = frame%cursor + 1
                            frame%value(frame%cursor:frame%cursor) = a
                        end if
                end select

            case (VE_STATE_CHECKSUM)
                valid = (frame%checksum == 0)

                frame%finished = .true.
                frame%state    = VE_STATE_IDLE
        end select

        finished = frame%finished
    end subroutine dm_ve_frame_next

    pure elemental subroutine dm_ve_frame_read(frame, response, field_type, error)
        !! Parses frame field label and returns the field as a response. The
        !! frame label and value is expected to be in upper-case. Checksum
        !! frames are ignored.
        !!
        !! The routine returns the following error codes in `error`:
        !!
        !! * `E_EMPTY` if the field value is empty.
        !! * `E_EOR` if the field is a checksum.
        !! * `E_INVALID` if the field label is unsupported.
        !! * `E_TYPE` if the field value type is invalid.
        !!
        use :: dm_string, only: dm_string_to
        use :: dm_util,   only: dm_hex_to_int, dm_to_real64

        type(ve_frame_type), intent(inout)         :: frame      !! Field frame.
        type(response_type), intent(out)           :: response   !! Response of field data.
        integer,             intent(out), optional :: field_type !! VE.Direct field type (`VE_FIELD_*`).
        integer,             intent(out), optional :: error      !! Error code.

        integer             :: i, rc, type
        type(ve_field_type) :: field

        if (present(field_type)) field_type = VE_FIELD_NONE

        read_block: block
            ! Ignore checksum frame.
            rc = E_EOR
            if (frame%label == 'CHECKSUM') exit read_block

            ! Find field type by label.
            rc = E_INVALID
            type = dm_ve_field_type(frame%label)
            if (present(field_type)) field_type = type
            if (type == VE_FIELD_NONE) exit read_block

            ! Initialise response.
            field = VE_FIELDS(type)
            response = response_type(name  = field%name, &
                                     unit  = field%unit, &
                                     type  = field%type, &
                                     error = E_INCOMPLETE)

            ! Check for frame value.
            rc = E_EMPTY
            if (len_trim(frame%value) == 0) exit read_block

            ! Convert string to real.
            rc = E_TYPE
            select case (field%type)
                case (RESPONSE_TYPE_INT32)
                    if (frame%value(1:3) == '---') then
                        ! No value.
                        rc = E_NONE
                        response%value = 0.0_r8
                    else if (frame%value(1:2) == '0X') then
                        ! Convert hex string to real.
                        call dm_hex_to_int(frame%value, i, rc)
                        response%value = dm_to_real64(i)
                    else
                        ! Convert string to real.
                        call dm_string_to(frame%value, response%value, rc)
                    end if

                case (RESPONSE_TYPE_LOGICAL)
                    rc = E_NONE
                    if (frame%value == 'ON') then
                        response%value = dm_to_real64(.true.)
                    else
                        response%value = dm_to_real64(.false.)
                    end if
            end select

            response%error = rc
        end block read_block

        if (present(error)) error = rc
    end subroutine dm_ve_frame_read

    pure elemental subroutine dm_ve_frame_reset(frame)
        !! Resets the frame to be used for the next block.
        type(ve_frame_type), intent(inout) :: frame !! VE.Direct frame.

        frame = ve_frame_type()
    end subroutine dm_ve_frame_reset
end module dm_ve
