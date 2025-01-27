! Author:  Philipp Engel
! Licence: ISC
module dm_ve
    !! VE.Direct protocol abstraction layer, for MPPT devices made by Victron
    !! Energy.
    !!
    !! The following MPPT series are compatible:
    !!
    !! * BlueSolar MPPT
    !! * SmartSolar MPPT
    !!
    !! The MPPT-specific fields supported by this module:
    !!
    !! | Name    | Description                            |
    !! |---------|----------------------------------------|
    !! | `CS`    | State of operation.                    |
    !! | `ERR`   | Error code.                            |
    !! | `FW`    | Firmware version (16 bit).             |
    !! | `H19`   | Yield total (user resettable counter). |
    !! | `H20`   | Yield today.                           |
    !! | `H21`   | Maximum power today.                   |
    !! | `H22`   | Yield yesterday.                       |
    !! | `H23`   | Maximum power yesterday.               |
    !! | `HSDS`  | Day sequence number (0 to 364).        |
    !! | `I`     | Main or channel 1 battery current.     |
    !! | `IL`    | Load current.                          |
    !! | `LOAD`  | Load output state (ON/OFF).            |
    !! | `MPPT`  | Tracker operation mode.                |
    !! | `OR`    | Off reason (hexadecimal).              |
    !! | `PID`   | Product ID (hexadecimal).              |
    !! | `PPV`   | Panel power.                           |
    !! | `Relay` | Relay state (ON/OFF).                  |
    !! | `V`     | Main or channel 1 (battery) voltage.   |
    !! | `VPV`   | Panel voltage.                         |
    !!
    !! The TTY has to be configured to the following serial port parameters:
    !!
    !! | Parameter    | Value |
    !! |--------------|-------|
    !! | Baud rate    | 19200 |
    !! | Data bits    | 8     |
    !! | Parity       | none  |
    !! | Stop bits    | 1     |
    !! | Flow control | none  |
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
    !! field in a block will always be `CHECKSUM`. The value is a single byte,
    !! and will not necessarily be a printable ASCII character. The modulo 256
    !! sum of all bytes in a block will equal 0 if there were no transmission
    !! errors. Multiple blocks are sent containing different fields.
    !!
    !! ## Example
    !!
    !! The snipped reads a single VE.Direct block from passed character `byte`
    !! and converts it to a response type:
    !!
    !! ```fortran
    !! integer             :: rc
    !! logical             :: eor, finished, valid
    !! type(ve_frame_type) :: frame
    !! type(response_type) :: response
    !!
    !! do
    !!     call dm_ve_frame_next(frame, byte, eor, finished, valid)
    !!
    !!     if (finished) then
    !!         if (valid) then
    !!             print '("record is valid")'
    !!         else
    !!             print '("record is invalid")'
    !!         end if
    !!
    !!         exit
    !!     end if
    !!
    !!     if (eor) then
    !!         rc = dm_ve_frame_read(frame, response)
    !!         print '("Label: ", a, " Value: ", a)',    frame%label, trim(frame%value)
    !!         print '("Name:  ", a, " Value: ", f8.1)', response%name, response%value
    !!     end if
    !! end do
    !!
    !! call dm_ve_frame_reset(frame)
    !! ```
    !!
    !! ## References
    !!
    !! * [VE.Direct Protocol v.3.33](https://www.victronenergy.com/upload/documents/VE.Direct-Protocol-3.33.pdf) (6 June 2023)
    !!
    use :: dm_error
    use :: dm_response
    use :: dm_tty
    implicit none (type, external)
    private

    ! Character lenghts.
    integer, parameter, public :: VE_LABEL_LEN = 8                 !! Max. field label length (minus newline).
    integer, parameter, public :: VE_NAME_LEN  = RESPONSE_NAME_LEN !! Max. response name length.
    integer, parameter, public :: VE_UNIT_LEN  = RESPONSE_UNIT_LEN !! Max. field unit length.
    integer, parameter, public :: VE_VALUE_LEN = 32                !! Max. field value length (minus tab).

    ! TTY parameters.
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
    integer, parameter, public :: VE_FIELD_CS    = 1  !! State of operation.
    integer, parameter, public :: VE_FIELD_ERR   = 2  !! Error code.
    integer, parameter, public :: VE_FIELD_FW    = 3  !! Firmware version (16 bit).
    integer, parameter, public :: VE_FIELD_H19   = 4  !! Yield total (user resettable counter).
    integer, parameter, public :: VE_FIELD_H20   = 5  !! Yield today.
    integer, parameter, public :: VE_FIELD_H21   = 6  !! Maximum power today.
    integer, parameter, public :: VE_FIELD_H22   = 7  !! Yield yesterday.
    integer, parameter, public :: VE_FIELD_H23   = 8  !! Maximum power yesterday.
    integer, parameter, public :: VE_FIELD_HSDS  = 9  !! Day sequence number (0 to 364).
    integer, parameter, public :: VE_FIELD_I     = 10 !! Main or channel 1 battery current.
    integer, parameter, public :: VE_FIELD_IL    = 11 !! Load current.
    integer, parameter, public :: VE_FIELD_LOAD  = 12 !! Load output state (ON/OFF).
    integer, parameter, public :: VE_FIELD_MPPT  = 13 !! Tracker operation mode.
    integer, parameter, public :: VE_FIELD_OR    = 14 !! Off reason (hexadecimal).
    integer, parameter, public :: VE_FIELD_PID   = 15 !! Product ID (hexadecimal).
    integer, parameter, public :: VE_FIELD_PPV   = 16 !! Panel power.
    integer, parameter, public :: VE_FIELD_RELAY = 17 !! Relay state (ON/OFF).
    integer, parameter, public :: VE_FIELD_V     = 18 !! Main or channel 1 (battery) voltage.
    integer, parameter, public :: VE_FIELD_VPV   = 19 !! Panel voltage.
    integer, parameter, public :: VE_FIELD_LAST  = 19 !! Never use this.

    ! VE.Direct default fields (MPPT only).
    integer, parameter, public :: VE_NFIELDS = VE_FIELD_LAST !! Number of supported fields.

    type(ve_field_type), parameter, public :: VE_FIELDS(VE_NFIELDS) = [    &
        ve_field_type('CS',    'cs',    'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('ERR',   'err',   'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('FW',    'fw',    'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('H19',   'h19',   'kWh/100', RESPONSE_TYPE_INT32),   &
        ve_field_type('H20',   'h20',   'kWh/100', RESPONSE_TYPE_INT32),   &
        ve_field_type('H21',   'h21',   'W',       RESPONSE_TYPE_INT32),   &
        ve_field_type('H22',   'h22',   'kWh/100', RESPONSE_TYPE_INT32),   &
        ve_field_type('H23',   'h23',   'W',       RESPONSE_TYPE_INT32),   &
        ve_field_type('HSDS',  'hsds',  'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('I',     'i',     'mA',      RESPONSE_TYPE_INT32),   &
        ve_field_type('IL',    'il',    'mA',      RESPONSE_TYPE_INT32),   &
        ve_field_type('LOAD',  'load',  'none',    RESPONSE_TYPE_LOGICAL), &
        ve_field_type('MPPT',  'mppt',  'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('OR',    'or',    'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('PID',   'pid',   'none',    RESPONSE_TYPE_INT32),   &
        ve_field_type('PPV',   'ppv',   'W',       RESPONSE_TYPE_INT32),   &
        ve_field_type('RELAY', 'relay', 'none',    RESPONSE_TYPE_LOGICAL), &
        ve_field_type('V',     'v',     'mV',      RESPONSE_TYPE_INT32),   &
        ve_field_type('VPV',   'vpv',   'mV',      RESPONSE_TYPE_INT32)    &
    ] !! Predefined fields.

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
    pure function dm_ve_error_message(code) result(message)
        !! Returns message associated with given VE.Direct error code.
        use :: dm_util, only: dm_itoa

        integer, intent(in)           :: code    !! VE.Direct error code.
        character(len=:), allocatable :: message !! VE.Direct error code message.

        select case (code)
            case (0);     message = 'no error'
            case (2);     message = 'battery voltage too high'
            case (17);    message = 'charger temperature too high'
            case (18);    message = 'charger over current'
            case (19);    message = 'charger current reversed'
            case (20);    message = 'bulk time limit exceeded'
            case (21);    message = 'current sensor issue (sensor bias/sensor broken)'
            case (26);    message = 'terminals overheated'
            case (28);    message = 'converter issue (dual converter models only)'
            case (33);    message = 'input voltage too high (solar panel)'
            case (34);    message = 'input current too high (solar panel)'
            case (38);    message = 'input shutdown (due to excessive battery voltage)'
            case (39);    message = 'input shutdown (due to current flow during off mode)'
            case (65);    message = 'lost communication with one of devices'
            case (66);    message = 'synchronised charging device configuration issue'
            case (67);    message = 'BMS connection lost'
            case (68);    message = 'network misconfigured'
            case (116);   message = 'factory calibration data lost'
            case (117);   message = 'invalid/incompatible firmware'
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

    integer function dm_ve_frame_read(frame, response, field_type) result(rc)
        !! Parses frame field label and returns the field as a response.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the field value is empty.
        !! * `E_EOR` if the field is a checksum.
        !! * `E_INVALID` if the field label is unsupported.
        !! * `E_TYPE` if the field value type is invalid.
        !!
        use :: dm_string, only: dm_string_to
        use :: dm_util,   only: dm_hex_to_int, dm_to_real64

        type(ve_frame_type), intent(inout)         :: frame      !! Field frame.
        type(response_type), intent(out)           :: response   !! Parsed response.
        integer,             intent(out), optional :: field_type !! VE.Direct field type (`VE_FIELD_*`).

        integer             :: i, type
        type(ve_field_type) :: field

        ! Ignore checksum frame.
        rc = E_EOR
        if (present(field_type)) field_type = VE_FIELD_NONE
        if (frame%label == 'CHECKSUM') return

        ! Find field type by label.
        rc = E_INVALID
        type = dm_ve_field_type(frame%label)
        if (present(field_type)) field_type = type
        if (type == VE_FIELD_NONE) return

        ! Initialise response.
        field = VE_FIELDS(type)
        response = response_type(name  = field%name, &
                                 unit  = field%unit, &
                                 type  = field%type, &
                                 error = E_INCOMPLETE)

        ! Convert string to real.
        rc = E_TYPE
        select case (field%type)
            case (RESPONSE_TYPE_INT32)
                rc = E_EMPTY
                if (len_trim(frame%value) == 0) return

                if (frame%value(1:2) == '0X') then
                    ! Convert hex string to integer.
                    call dm_hex_to_int(frame%value, i, rc)
                    response%value = dm_to_real64(i)
                else
                    ! Convert string to integer.
                    call dm_string_to(frame%value, response%value, rc)
                end if

            case (RESPONSE_TYPE_LOGICAL)
                rc = E_EMPTY
                if (len_trim(frame%value) == 0) return

                rc = TYPE
                if (frame%value == 'OFF') then
                    rc = E_NONE
                    response%value = dm_to_real64(.false.)
                else if (frame%value == 'ON') then
                    rc = E_NONE
                    response%value = dm_to_real64(.true.)
                end if
        end select

        response%error = rc
    end function dm_ve_frame_read

    pure elemental logical function dm_ve_is_error(code) result(is)
        !! Returns `.true.` if given code is a valid VE.Direct error code.
        integer, intent(in) :: code !! VE.Direct error code.

        is = .false.

        select case (code)
            case (2, 17, 18, 19, 20, 21, 26, 28, 33, 34, 38, 39, 65, 66, 67, 68, 116, 117)
                is = .true.
            case default
                return
        end select
    end function dm_ve_is_error

    pure elemental logical function dm_ve_is_valid_field_type(type) result(is)
        !! Returns `.true.` if given type is a valid field enumerator (`VE_FIELD_*`).
        integer, intent(in) :: type !! Field type.

        is = (type > VE_FIELD_NONE .and. type <= VE_FIELD_LAST)
    end function dm_ve_is_valid_field_type

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    pure subroutine dm_ve_frame_next(frame, byte, eor, finished, valid)
        !! State machine to read VE.Direct Text protocol frame. Argument `eor`
        !! is `.true.`, if a single record has been read. Argument `finished`
        !! is `.true.`, if a block has been read. Argument `valid` is `.true.`
        !! if the checksum of a finished block is valid.
        !!
        !! If `eor` is `.true.`, read the frame with `dm_ve_frame_read()`
        !! afterwards. If `finished` is `.true.`, reset the frame before
        !! reading the next block.
        !!
        !! ## References
        !!
        !! * [VE.Direct Protocol FAQ](https://www.victronenergy.com/live/vedirect_protocol:faq)
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

        frame%checksum = modulo(frame%checksum + iachar(byte), 256)
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

    pure subroutine dm_ve_frame_reset(frame)
        !! Resets the frame to be used for the next block.
        type(ve_frame_type), intent(inout) :: frame !! VE.Direct frame.

        frame = ve_frame_type()
    end subroutine dm_ve_frame_reset
end module dm_ve
