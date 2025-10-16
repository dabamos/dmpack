! modbus.F90
!
! Author:  Philipp Engel
! Licence: ISC
module modbus
    !! Auto-generated Fortran 2018 interace bindings to libmodbus.
    use, intrinsic :: iso_c_binding, only: c_associated, c_f_pointer, &
                                           c_char, c_float, c_int, c_int8_t, c_int16_t, c_int32_t, &
                                           c_size_t, c_ptr, c_null_char, c_null_ptr
#if HAS_UNSIGNED

    use, intrinsic :: iso_c_binding, only: c_uint8_t, c_uint16_t, c_uint32_t, c_unsigned

#endif
    implicit none (type, external)
    private

#if HAS_UNSIGNED

    public :: c_uint8_t
    public :: c_uint16_t
    public :: c_uint32_t
    public :: c_unsigned

#else

    integer, parameter, public :: c_uint8_t  = c_int8_t
    integer, parameter, public :: c_uint16_t = c_int16_t
    integer, parameter, public :: c_uint32_t = c_int32_t
    integer, parameter, public :: c_unsigned = c_int

#endif

    public :: c_associated
    public :: c_f_pointer

    public :: c_char
    public :: c_float
    public :: c_int
    public :: c_int8_t
    public :: c_int16_t
    public :: c_int32_t
    public :: c_ptr
    public :: c_null_char
    public :: c_null_ptr

    integer(kind=c_int), parameter, public :: MODBUS_FC_READ_COILS               = int(z'01')
    integer(kind=c_int), parameter, public :: MODBUS_FC_READ_DISCRETE_INPUTS     = int(z'02')
    integer(kind=c_int), parameter, public :: MODBUS_FC_READ_HOLDING_REGISTERS   = int(z'03')
    integer(kind=c_int), parameter, public :: MODBUS_FC_READ_INPUT_REGISTERS     = int(z'04')
    integer(kind=c_int), parameter, public :: MODBUS_FC_WRITE_SINGLE_COIL        = int(z'05')
    integer(kind=c_int), parameter, public :: MODBUS_FC_WRITE_SINGLE_REGISTER    = int(z'06')
    integer(kind=c_int), parameter, public :: MODBUS_FC_READ_EXCEPTION_STATUS    = int(z'07')
    integer(kind=c_int), parameter, public :: MODBUS_FC_WRITE_MULTIPLE_COILS     = int(z'0F')
    integer(kind=c_int), parameter, public :: MODBUS_FC_WRITE_MULTIPLE_REGISTERS = int(z'10')
    integer(kind=c_int), parameter, public :: MODBUS_FC_REPORT_SLAVE_ID          = int(z'11')
    integer(kind=c_int), parameter, public :: MODBUS_FC_MASK_WRITE_REGISTER      = int(z'16')
    integer(kind=c_int), parameter, public :: MODBUS_FC_WRITE_AND_READ_REGISTERS = int(z'17')

    integer(kind=c_int), parameter, public :: MODBUS_BROADCAST_ADDRESS = 0

    integer(kind=c_int), parameter, public :: MODBUS_MAX_READ_BITS  = 2000
    integer(kind=c_int), parameter, public :: MODBUS_MAX_WRITE_BITS = 1968

    integer(kind=c_int), parameter, public :: MODBUS_MAX_READ_REGISTERS     = 125
    integer(kind=c_int), parameter, public :: MODBUS_MAX_WRITE_REGISTERS    = 123
    integer(kind=c_int), parameter, public :: MODBUS_MAX_WR_WRITE_REGISTERS = 121
    integer(kind=c_int), parameter, public :: MODBUS_MAX_WR_READ_REGISTERS  = 125

    integer(kind=c_int), parameter, public :: MODBUS_MAX_PDU_LENGTH = 253
    integer(kind=c_int), parameter, public :: MODBUS_MAX_ADU_LENGTH = 260

    integer(kind=c_int), parameter, public :: MODBUS_ENOBASE = 112345678

    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_ILLEGAL_FUNCTION        = 1
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_ILLEGAL_DATA_ADDRESS    = 2
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_ILLEGAL_DATA_VALUE      = 3
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_SLAVE_OR_SERVER_FAILURE = 4
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_ACKNOWLEDGE             = 5
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_SLAVE_OR_SERVER_BUSY    = 6
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_NEGATIVE_ACKNOWLEDGE    = 7
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_MEMORY_PARITY           = 8
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_NOT_DEFINED             = 9
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_GATEWAY_PATH            = 10
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_GATEWAY_TARGET          = 11
    integer(kind=c_int), parameter, public :: MODBUS_EXCEPTION_MAX                     = 12

    integer(kind=c_int), parameter, public :: EMBXILFUN  = MODBUS_ENOBASE + MODBUS_EXCEPTION_ILLEGAL_FUNCTION
    integer(kind=c_int), parameter, public :: EMBXILADD  = MODBUS_ENOBASE + MODBUS_EXCEPTION_ILLEGAL_DATA_ADDRESS
    integer(kind=c_int), parameter, public :: EMBXILVAL  = MODBUS_ENOBASE + MODBUS_EXCEPTION_ILLEGAL_DATA_VALUE
    integer(kind=c_int), parameter, public :: EMBXSFAIL  = MODBUS_ENOBASE + MODBUS_EXCEPTION_SLAVE_OR_SERVER_FAILURE
    integer(kind=c_int), parameter, public :: EMBXACK    = MODBUS_ENOBASE + MODBUS_EXCEPTION_ACKNOWLEDGE
    integer(kind=c_int), parameter, public :: EMBXSBUSY  = MODBUS_ENOBASE + MODBUS_EXCEPTION_SLAVE_OR_SERVER_BUSY
    integer(kind=c_int), parameter, public :: EMBXNACK   = MODBUS_ENOBASE + MODBUS_EXCEPTION_NEGATIVE_ACKNOWLEDGE
    integer(kind=c_int), parameter, public :: EMBXMEMPAR = MODBUS_ENOBASE + MODBUS_EXCEPTION_MEMORY_PARITY
    integer(kind=c_int), parameter, public :: EMBXGPATH  = MODBUS_ENOBASE + MODBUS_EXCEPTION_GATEWAY_PATH
    integer(kind=c_int), parameter, public :: EMBXGTAR   = MODBUS_ENOBASE + MODBUS_EXCEPTION_GATEWAY_TARGET

    ! Native libmodbus error codes.
    integer(kind=c_int), parameter, public :: EMBBADCRC   = EMBXGTAR + 1
    integer(kind=c_int), parameter, public :: EMBBADDATA  = EMBXGTAR + 2
    integer(kind=c_int), parameter, public :: EMBBADEXC   = EMBXGTAR + 3
    integer(kind=c_int), parameter, public :: EMBUNKEXC   = EMBXGTAR + 4
    integer(kind=c_int), parameter, public :: EMBMDATA    = EMBXGTAR + 5
    integer(kind=c_int), parameter, public :: EMBBADSLAVE = EMBXGTAR + 6

    ! modbus_error_recovery_mode
    integer(kind=c_int), parameter, public :: MODBUS_ERROR_RECOVERY_NONE     = 0
    integer(kind=c_int), parameter, public :: MODBUS_ERROR_RECOVERY_LINK     = shiftl(1, 1)
    integer(kind=c_int), parameter, public :: MODBUS_ERROR_RECOVERY_PROTOCOL = shiftl(1, 2)

    ! modbus_quirks
    integer(kind=c_int), parameter, public :: MODBUS_QUIRK_NONE               = 0
    integer(kind=c_int), parameter, public :: MODBUS_QUIRK_MAX_SLAVE          = shiftl(1, 1)
    integer(kind=c_int), parameter, public :: MODBUS_QUIRK_REPLY_TO_BROADCAST = shiftl(1, 2)
    integer(kind=c_int), parameter, public :: MODBUS_QUIRK_ALL                = int(z'FF')

    integer(kind=c_unsigned), bind(c, name='libmodbus_version_major'), public :: LIBMODBUS_VERSION_MAJOR
    integer(kind=c_unsigned), bind(c, name='libmodbus_version_minor'), public :: LIBMODBUS_VERSION_MINOR
    integer(kind=c_unsigned), bind(c, name='libmodbus_version_micro'), public :: LIBMODBUS_VERSION_MICRO

    public :: modbus_close
    public :: modbus_close_
    public :: modbus_connect
    public :: modbus_disable_quirks
    public :: modbus_enable_quirks
    public :: modbus_flush
    public :: modbus_free
    public :: modbus_free_
    public :: modbus_get_byte_from_bits
    public :: modbus_get_byte_timeout
    public :: modbus_get_float
    public :: modbus_get_float_abcd
    public :: modbus_get_float_badc
    public :: modbus_get_float_cdab
    public :: modbus_get_float_dcba
    public :: modbus_get_header_length
    public :: modbus_get_indication_timeout
    public :: modbus_get_response_timeout
    public :: modbus_get_slave
    public :: modbus_get_socket
    public :: modbus_mapping_free
    public :: modbus_mapping_new
    public :: modbus_mapping_new_start_address
    public :: modbus_mask_write_register
    public :: modbus_read_bits
    public :: modbus_read_input_bits
    public :: modbus_read_input_registers
    public :: modbus_read_registers
    public :: modbus_receive
    public :: modbus_receive_confirmation
    public :: modbus_reply
    public :: modbus_reply_exception
    public :: modbus_report_slave_id
    public :: modbus_send_raw_request
    public :: modbus_set_bits_from_byte
    public :: modbus_set_bits_from_bytes
    public :: modbus_set_byte_timeout
    public :: modbus_set_debug
    public :: modbus_set_error_recovery
    public :: modbus_set_float
    public :: modbus_set_float_abcd
    public :: modbus_set_float_badc
    public :: modbus_set_float_cdab
    public :: modbus_set_float_dcba
    public :: modbus_set_indication_timeout
    public :: modbus_set_response_timeout
    public :: modbus_set_slave
    public :: modbus_set_socket
    public :: modbus_strerror
    public :: modbus_strerror_
    public :: modbus_write_and_read_registers
    public :: modbus_write_bit
    public :: modbus_write_bits
    public :: modbus_write_register
    public :: modbus_write_registers

    private :: c_f_str_ptr

    interface
        ! void modbus_close(modbus_t *ctx)
        subroutine modbus_close_(ctx) bind(c, name='modbus_close')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
        end subroutine modbus_close_

        ! int modbus_connect(modbus_t *ctx)
        function modbus_connect(ctx) bind(c, name='modbus_connect')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            integer(kind=c_int)            :: modbus_connect
        end function modbus_connect

        ! int modbus_disable_quirks(modbus_t *ctx, unsigned int quirks_mask)
        function modbus_disable_quirks(ctx, quirks_mask) bind(c, name='modbus_disable_quirks')
            import :: c_int, c_ptr, c_unsigned
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_unsigned), intent(in), value :: quirks_mask
            integer(kind=c_int)                         :: modbus_disable_quirks
        end function modbus_disable_quirks

        ! int modbus_enable_quirks(modbus_t *ctx, unsigned int quirks_mask)
        function modbus_enable_quirks(ctx, quirks_mask) bind(c, name='modbus_enable_quirks')
            import :: c_int, c_ptr, c_unsigned
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_unsigned), intent(in), value :: quirks_mask
            integer(kind=c_int)                         :: modbus_enable_quirks
        end function modbus_enable_quirks

        ! int modbus_flush(modbus_t *ctx)
        function modbus_flush(ctx) bind(c, name='modbus_flush')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            integer(kind=c_int)            :: modbus_flush
        end function modbus_flush

        ! void modbus_free(modbus_t *ctx)
        subroutine modbus_free_(ctx) bind(c, name='modbus_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
        end subroutine modbus_free_

        ! uint8_t modbus_get_byte_from_bits(const uint8_t *src, int idx, unsigned int nb_bits)
        function modbus_get_byte_from_bits(src, idx, nb_bits) bind(c, name='modbus_get_byte_from_bits')
            import :: c_int, c_uint8_t, c_unsigned
            implicit none
            integer(kind=c_uint8_t),  intent(inout)     :: src(*)
            integer(kind=c_int),      intent(in), value :: idx
            integer(kind=c_unsigned), intent(in), value :: nb_bits
            integer(kind=c_uint8_t)                     :: modbus_get_byte_from_bits
        end function modbus_get_byte_from_bits

        ! int modbus_get_byte_timeout(modbus_t *ctx, uint32_t *to_sec, uint32_t *to_usec)
        function modbus_get_byte_timeout(ctx, to_sec, to_usec) bind(c, name='modbus_get_byte_timeout')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_uint32_t), intent(out)       :: to_sec
            integer(kind=c_uint32_t), intent(out)       :: to_usec
            integer(kind=c_int)                         :: modbus_get_byte_timeout
        end function modbus_get_byte_timeout

        ! float modbus_get_float(const uint16_t *src)
        function modbus_get_float(src) bind(c, name='modbus_get_float')
            import :: c_float, c_uint16_t
            implicit none
            integer(kind=c_uint16_t), intent(in) :: src(*)
            real(kind=c_float)                   :: modbus_get_float
        end function modbus_get_float

        ! float modbus_get_float_abcd(const uint16_t *src)
        function modbus_get_float_abcd(src) bind(c, name='modbus_get_float_abcd')
            import :: c_float, c_uint16_t
            implicit none
            integer(kind=c_uint16_t), intent(in) :: src(*)
            real(kind=c_float)                   :: modbus_get_float_abcd
        end function modbus_get_float_abcd

        ! float modbus_get_float_badc(const uint16_t *src)
        function modbus_get_float_badc(src) bind(c, name='modbus_get_float_badc')
            import :: c_float, c_uint16_t
            implicit none
            integer(kind=c_uint16_t), intent(in) :: src(*)
            real(kind=c_float)                   :: modbus_get_float_badc
        end function modbus_get_float_badc

        ! float modbus_get_float_cdab(const uint16_t *src)
        function modbus_get_float_cdab(src) bind(c, name='modbus_get_float_cdab')
            import :: c_float, c_uint16_t
            implicit none
            integer(kind=c_uint16_t), intent(in) :: src(*)
            real(kind=c_float)                   :: modbus_get_float_cdab
        end function modbus_get_float_cdab

        ! float modbus_get_float_dcba(const uint16_t *src)
        function modbus_get_float_dcba(src) bind(c, name='modbus_get_float_dcba')
            import :: c_float, c_uint16_t
            implicit none
            integer(kind=c_uint16_t), intent(in) :: src(*)
            real(kind=c_float)                   :: modbus_get_float_dcba
        end function modbus_get_float_dcba

        ! int modbus_get_header_length(modbus_t *ctx)
        function modbus_get_header_length(ctx) bind(c, name='modbus_get_header_length')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            integer(kind=c_int)            :: modbus_get_header_length
        end function modbus_get_header_length

        ! int modbus_get_indication_timeout(modbus_t *ctx, uint32_t *to_sec, uint32_t *to_usec)
        function modbus_get_indication_timeout(ctx, to_sec, to_usec) bind(c, name='modbus_get_indication_timeout')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_uint32_t), intent(out)       :: to_sec
            integer(kind=c_uint32_t), intent(out)       :: to_usec
            integer(kind=c_int)                         :: modbus_get_indication_timeout
        end function modbus_get_indication_timeout

        ! int modbus_get_response_timeout(modbus_t *ctx, uint32_t *to_sec, uint32_t *to_usec)
        function modbus_get_response_timeout(ctx, to_sec, to_usec) bind(c, name='modbus_get_response_timeout')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_uint32_t), intent(out)       :: to_sec
            integer(kind=c_uint32_t), intent(out)       :: to_usec
            integer(kind=c_int)                         :: modbus_get_response_timeout
        end function modbus_get_response_timeout

        ! int modbus_get_slave(modbus_t *ctx)
        function modbus_get_slave(ctx) bind(c, name='modbus_get_slave')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            integer(kind=c_int)            :: modbus_get_slave
        end function modbus_get_slave

        ! int modbus_get_socket(modbus_t *ctx)
        function modbus_get_socket(ctx) bind(c, name='modbus_get_socket')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            integer(kind=c_int)            :: modbus_get_socket
        end function modbus_get_socket

        ! void modbus_mapping_free(modbus_mapping_t *mb_mapping)
        subroutine modbus_mapping_free(mb_mapping) bind(c, name='modbus_mapping_free')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: mb_mapping
        end subroutine modbus_mapping_free

        ! modbus_mapping_t *modbus_mapping_new(int nb_bits, int nb_input_bits, int nb_registers, int nb_input_registers)
        function modbus_mapping_new(nb_bits, nb_input_bits, nb_registers, nb_input_registers) bind(c, name='modbus_mapping_new')
            import :: c_int, c_ptr
            implicit none
            integer(kind=c_int), intent(in), value :: nb_bits
            integer(kind=c_int), intent(in), value :: nb_input_bits
            integer(kind=c_int), intent(in), value :: nb_registers
            integer(kind=c_int), intent(in), value :: nb_input_registers
            type(c_ptr)                            :: modbus_mapping_new
        end function modbus_mapping_new

        ! modbus_mapping_t *modbus_mapping_new_start_address(unsigned int start_bits, unsigned int nb_bits, unsigned int start_input_bits, unsigned int nb_input_bits, unsigned int start_registers, unsigned int nb_registers, unsigned int start_input_registers, unsigned int nb_input_registers)
        function modbus_mapping_new_start_address(start_bits, nb_bits, start_input_bits, nb_input_bits, start_registers, &
                nb_registers, start_input_registers, nb_input_registers) bind(c, name='modbus_mapping_new_start_address')
            import :: c_ptr, c_unsigned
            implicit none
            integer(kind=c_unsigned), intent(in), value :: start_bits
            integer(kind=c_unsigned), intent(in), value :: nb_bits
            integer(kind=c_unsigned), intent(in), value :: start_input_bits
            integer(kind=c_unsigned), intent(in), value :: nb_input_bits
            integer(kind=c_unsigned), intent(in), value :: start_registers
            integer(kind=c_unsigned), intent(in), value :: nb_registers
            integer(kind=c_unsigned), intent(in), value :: start_input_registers
            integer(kind=c_unsigned), intent(in), value :: nb_input_registers
            type(c_ptr)                                 :: modbus_mapping_new_start_address
        end function modbus_mapping_new_start_address

        ! int modbus_mask_write_register(modbus_t *ctx, int addr, uint16_t and_mask, uint16_t or_mask)
        function modbus_mask_write_register(ctx, addr, and_mask, or_mask) bind(c, name='modbus_mask_write_register')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_int),      intent(in), value :: addr
            integer(kind=c_uint16_t), intent(in), value :: and_mask
            integer(kind=c_uint16_t), intent(in), value :: or_mask
            integer(kind=c_int)                         :: modbus_mask_write_register
        end function modbus_mask_write_register

        ! int modbus_read_bits(modbus_t *ctx, int addr, int nb, uint8_t *dest)
        function modbus_read_bits(ctx, addr, nb, dest) bind(c, name='modbus_read_bits')
            import :: c_int, c_ptr, c_uint8_t
            implicit none
            type(c_ptr),             intent(in), value :: ctx
            integer(kind=c_int),     intent(in), value :: addr
            integer(kind=c_int),     intent(in), value :: nb
            integer(kind=c_uint8_t), intent(inout)     :: dest(*)
            integer(kind=c_int)                        :: modbus_read_bits
        end function modbus_read_bits

        ! int modbus_read_input_bits(modbus_t *ctx, int addr, int nb, uint8_t *dest)
        function modbus_read_input_bits(ctx, addr, nb, dest) bind(c, name='modbus_read_input_bits')
            import :: c_int, c_ptr, c_uint8_t
            implicit none
            type(c_ptr),             intent(in), value :: ctx
            integer(kind=c_int),     intent(in), value :: addr
            integer(kind=c_int),     intent(in), value :: nb
            integer(kind=c_uint8_t), intent(inout)     :: dest(*)
            integer(kind=c_int)                        :: modbus_read_input_bits
        end function modbus_read_input_bits

        ! int modbus_read_input_registers(modbus_t *ctx, int addr, int nb, uint16_t *dest)
        function modbus_read_input_registers(ctx, addr, nb, dest) bind(c, name='modbus_read_input_registers')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_int),      intent(in), value :: addr
            integer(kind=c_int),      intent(in), value :: nb
            integer(kind=c_uint16_t), intent(inout)     :: dest(*)
            integer(kind=c_int)                         :: modbus_read_input_registers
        end function modbus_read_input_registers

        ! int modbus_read_registers(modbus_t *ctx, int addr, int nb, uint16_t *dest)
        function modbus_read_registers(ctx, addr, nb, dest) bind(c, name='modbus_read_registers')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_int),      intent(in), value :: addr
            integer(kind=c_int),      intent(in), value :: nb
            integer(kind=c_uint16_t), intent(inout)     :: dest(*)
            integer(kind=c_int)                         :: modbus_read_registers
        end function modbus_read_registers

        ! int modbus_receive(modbus_t *ctx, uint8_t *req)
        function modbus_receive(ctx, req) bind(c, name='modbus_receive')
            import :: c_int, c_ptr, c_uint8_t
            implicit none
            type(c_ptr),             intent(in), value :: ctx
            integer(kind=c_uint8_t), intent(inout)     :: req(*)
            integer(kind=c_int)                        :: modbus_receive
        end function modbus_receive

        ! int modbus_receive_confirmation(modbus_t *ctx, uint8_t *rsp)
        function modbus_receive_confirmation(ctx, rsp) bind(c, name='modbus_receive_confirmation')
            import :: c_int, c_ptr, c_uint8_t
            implicit none
            type(c_ptr),             intent(in), value :: ctx
            integer(kind=c_uint8_t), intent(inout)     :: rsp(*)
            integer(kind=c_int)                        :: modbus_receive_confirmation
        end function modbus_receive_confirmation

        ! int modbus_reply(modbus_t *ctx, const uint8_t *req, int req_length, modbus_mapping_t *mb_mapping)
        function modbus_reply(ctx, req, req_length, mb_mapping) bind(c, name='modbus_reply')
            import :: c_int, c_ptr, c_uint8_t
            implicit none
            type(c_ptr),             intent(in), value :: ctx
            integer(kind=c_uint8_t), intent(inout)     :: req(*)
            integer(kind=c_int),     intent(in), value :: req_length
            type(c_ptr),             intent(in), value :: mb_mapping
            integer(kind=c_int)                        :: modbus_reply
        end function modbus_reply

        ! int modbus_reply_exception(modbus_t *ctx, const uint8_t *req, unsigned int exception_code)
        function modbus_reply_exception(ctx, req, exception_code) bind(c, name='modbus_reply_exception')
            import :: c_int, c_ptr, c_uint8_t, c_unsigned
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_uint8_t),  intent(inout)     :: req(*)
            integer(kind=c_unsigned), intent(in), value :: exception_code
            integer(kind=c_int)                         :: modbus_reply_exception
        end function modbus_reply_exception

        ! int modbus_report_slave_id(modbus_t *ctx, int max_dest, uint8_t *dest)
        function modbus_report_slave_id(ctx, max_dest, dest) bind(c, name='modbus_report_slave_id')
            import :: c_int, c_ptr, c_uint8_t
            implicit none
            type(c_ptr),             intent(in), value :: ctx
            integer(kind=c_int),     intent(in), value :: max_dest
            integer(kind=c_uint8_t), intent(inout)     :: dest(*)
            integer(kind=c_int)                        :: modbus_report_slave_id
        end function modbus_report_slave_id

        ! int modbus_send_raw_request(modbus_t *ctx, const uint8_t *raw_req, int raw_req_length)
        function modbus_send_raw_request(ctx, raw_req, raw_req_length) bind(c, name='modbus_send_raw_request')
            import :: c_int, c_ptr, c_uint8_t
            implicit none
            type(c_ptr),             intent(in), value :: ctx
            integer(kind=c_uint8_t), intent(inout)     :: raw_req(*)
            integer(kind=c_int),     intent(in), value :: raw_req_length
            integer(kind=c_int)                        :: modbus_send_raw_request
        end function modbus_send_raw_request

        ! void modbus_set_bits_from_byte(uint8_t *dest, int idx, const uint8_t value)
        subroutine modbus_set_bits_from_byte(dest, idx, value) bind(c, name='modbus_set_bits_from_byte')
            import :: c_int, c_uint8_t
            implicit none
            integer(kind=c_uint8_t), intent(inout)     :: dest(*)
            integer(kind=c_int),     intent(in), value :: idx
            integer(kind=c_uint8_t), intent(in), value :: value
        end subroutine modbus_set_bits_from_byte

        ! void modbus_set_bits_from_bytes(uint8_t *dest, int idx, unsigned int nb_bits, const uint8_t *tab_byte)
        subroutine modbus_set_bits_from_bytes(dest, idx, nb_bits, tab_byte) bind(c, name='modbus_set_bits_from_bytes')
            import :: c_int, c_uint8_t, c_unsigned
            implicit none
            integer(kind=c_uint8_t),  intent(inout)     :: dest(*)
            integer(kind=c_int),      intent(in), value :: idx
            integer(kind=c_unsigned), intent(in), value :: nb_bits
            integer(kind=c_uint8_t),  intent(in)        :: tab_byte
        end subroutine modbus_set_bits_from_bytes

        ! int modbus_set_byte_timeout(modbus_t *ctx, uint32_t to_sec, uint32_t to_usec)
        function modbus_set_byte_timeout(ctx, to_sec, to_usec) bind(c, name='modbus_set_byte_timeout')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_uint32_t), intent(in), value :: to_sec
            integer(kind=c_uint32_t), intent(in), value :: to_usec
            integer(kind=c_int)                         :: modbus_set_byte_timeout
        end function modbus_set_byte_timeout

        ! int modbus_set_debug(modbus_t *ctx, int flag)
        function modbus_set_debug(ctx, flag) bind(c, name='modbus_set_debug')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(in), value :: flag
            integer(kind=c_int)                    :: modbus_set_debug
        end function modbus_set_debug

        ! int modbus_set_error_recovery(modbus_t *ctx, modbus_error_recovery_mode error_recovery)
        function modbus_set_error_recovery(ctx, error_recovery) bind(c, name='modbus_set_error_recovery')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(in), value :: error_recovery
            integer(kind=c_int)                    :: modbus_set_error_recovery
        end function modbus_set_error_recovery

        ! void modbus_set_float(float f, uint16_t *dest)
        subroutine modbus_set_float(f, dest) bind(c, name='modbus_set_float')
            import :: c_float, c_uint16_t
            implicit none
            real(kind=c_float),       intent(in), value :: f
            integer(kind=c_uint16_t), intent(inout)     :: dest(*)
        end subroutine modbus_set_float

        ! void modbus_set_float_abcd(float f, uint16_t *dest)
        subroutine modbus_set_float_abcd(f, dest) bind(c, name='modbus_set_float_abcd')
            import :: c_float, c_uint16_t
            implicit none
            real(kind=c_float),       intent(in), value :: f
            integer(kind=c_uint16_t), intent(inout)     :: dest(*)
        end subroutine modbus_set_float_abcd

        ! void modbus_set_float_badc(float f, uint16_t *dest)
        subroutine modbus_set_float_badc(f, dest) bind(c, name='modbus_set_float_badc')
            import :: c_float, c_uint16_t
            implicit none
            real(kind=c_float),       intent(in), value :: f
            integer(kind=c_uint16_t), intent(inout)     :: dest(*)
        end subroutine modbus_set_float_badc

        ! void modbus_set_float_cdab(float f, uint16_t *dest)
        subroutine modbus_set_float_cdab(f, dest) bind(c, name='modbus_set_float_cdab')
            import :: c_float, c_uint16_t
            implicit none
            real(kind=c_float),       intent(in), value :: f
            integer(kind=c_uint16_t), intent(inout)     :: dest(*)
        end subroutine modbus_set_float_cdab

        ! void modbus_set_float_dcba(float f, uint16_t *dest)
        subroutine modbus_set_float_dcba(f, dest) bind(c, name='modbus_set_float_dcba')
            import :: c_float, c_uint16_t
            implicit none
            real(kind=c_float),       intent(in), value :: f
            integer(kind=c_uint16_t), intent(inout)     :: dest(*)
        end subroutine modbus_set_float_dcba

        ! int modbus_set_indication_timeout(modbus_t *ctx, uint32_t to_sec, uint32_t to_usec)
        function modbus_set_indication_timeout(ctx, to_sec, to_usec) bind(c, name='modbus_set_indication_timeout')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_uint32_t), intent(in), value :: to_sec
            integer(kind=c_uint32_t), intent(in), value :: to_usec
            integer(kind=c_int)                         :: modbus_set_indication_timeout
        end function modbus_set_indication_timeout

        ! int modbus_set_response_timeout(modbus_t *ctx, uint32_t to_sec, uint32_t to_usec)
        function modbus_set_response_timeout(ctx, to_sec, to_usec) bind(c, name='modbus_set_response_timeout')
            import :: c_int, c_ptr, c_uint32_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_uint32_t), intent(in), value :: to_sec
            integer(kind=c_uint32_t), intent(in), value :: to_usec
            integer(kind=c_int)                         :: modbus_set_response_timeout
        end function modbus_set_response_timeout

        ! int modbus_set_slave(modbus_t *ctx, int slave)
        function modbus_set_slave(ctx, slave) bind(c, name='modbus_set_slave')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(in), value :: slave
            integer(kind=c_int)                    :: modbus_set_slave
        end function modbus_set_slave

        ! int modbus_set_socket(modbus_t *ctx, int s)
        function modbus_set_socket(ctx, s) bind(c, name='modbus_set_socket')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(in), value :: s
            integer(kind=c_int)                    :: modbus_set_socket
        end function modbus_set_socket

        ! const char *modbus_strerror(int errnum)
        function modbus_strerror_(err_num) bind(c, name='modbus_strerror')
            import :: c_int, c_ptr
            implicit none
            integer(kind=c_int), intent(in), value :: err_num
            type(c_ptr)                            :: modbus_strerror_
        end function modbus_strerror_

        ! int modbus_write_and_read_registers(modbus_t *ctx, int write_addr, int write_nb, const uint16_t *src, int read_addr, int read_nb, uint16_t *dest)
        function modbus_write_and_read_registers(ctx, write_addr, write_nb, src, read_addr, read_nb, dest) &
                bind(c, name='modbus_write_and_read_registers')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_int),      intent(in), value :: write_addr
            integer(kind=c_int),      intent(in), value :: write_nb
            integer(kind=c_uint16_t), intent(inout)     :: src(*)
            integer(kind=c_int),      intent(in), value :: read_addr
            integer(kind=c_int),      intent(in), value :: read_nb
            integer(kind=c_uint16_t), intent(inout)     :: dest(*)
            integer(kind=c_int)                         :: modbus_write_and_read_registers
        end function modbus_write_and_read_registers

        ! int modbus_write_bit(modbus_t *ctx, int coil_addr, int status)
        function modbus_write_bit(ctx, coil_addr, status) bind(c, name='modbus_write_bit')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(in), value :: coil_addr
            integer(kind=c_int), intent(in), value :: status
            integer(kind=c_int)                    :: modbus_write_bit
        end function modbus_write_bit

        ! int modbus_write_bits(modbus_t *ctx, int addr, int nb, const uint8_t *data)
        function modbus_write_bits(ctx, addr, nb, data) bind(c, name='modbus_write_bits')
            import :: c_int, c_ptr, c_uint8_t
            implicit none
            type(c_ptr),             intent(in), value :: ctx
            integer(kind=c_int),     intent(in), value :: addr
            integer(kind=c_int),     intent(in), value :: nb
            integer(kind=c_uint8_t), intent(inout)     :: data(*)
            integer(kind=c_int)                        :: modbus_write_bits
        end function modbus_write_bits

        ! int modbus_write_register(modbus_t *ctx, int reg_addr, const uint16_t value)
        function modbus_write_register(ctx, reg_addr, value) bind(c, name='modbus_write_register')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_int),      intent(in), value :: reg_addr
            integer(kind=c_uint16_t), intent(in), value :: value
            integer(kind=c_int)                         :: modbus_write_register
        end function modbus_write_register

        ! int modbus_write_registers(modbus_t *ctx, int addr, int nb, const uint16_t *data)
        function modbus_write_registers(ctx, addr, nb, data) bind(c, name='modbus_write_registers')
            import :: c_int, c_ptr, c_uint16_t
            implicit none
            type(c_ptr),              intent(in), value :: ctx
            integer(kind=c_int),      intent(in), value :: addr
            integer(kind=c_int),      intent(in), value :: nb
            integer(kind=c_uint16_t), intent(inout)     :: data(*)
            integer(kind=c_int)                         :: modbus_write_registers
        end function modbus_write_registers
    end interface
contains
    subroutine c_f_str_ptr(c_str, f_str)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
        type(c_ptr),                   intent(in)  :: c_str
        character(len=:), allocatable, intent(out) :: f_str

        character(kind=c_char), pointer :: ptrs(:)
        integer(kind=c_size_t)          :: i, sz

        interface
            ! size_t strlen(const char *str)
            function c_strlen(str) bind(c, name='strlen')
                import :: c_ptr, c_size_t
                implicit none
                type(c_ptr), intent(in), value :: str
                integer(kind=c_size_t)         :: c_strlen
            end function c_strlen
        end interface

        copy_if: if (c_associated(c_str)) then
            sz = c_strlen(c_str)
            if (sz < 0) exit copy_if
            call c_f_pointer(c_str, ptrs, [ sz ])
            allocate (character(len=sz) :: f_str)

            do i = 1, sz
                f_str(i:i) = ptrs(i)
            end do

            return
        end if copy_if

        if (.not. allocated(f_str)) f_str = ''
    end subroutine c_f_str_ptr

    ! void modbus_close(modbus_t *ctx)
    subroutine modbus_close(ctx)
        type(c_ptr), intent(inout) :: ctx

        call modbus_close_(ctx)
        ctx = c_null_ptr
    end subroutine modbus_close

    ! void modbus_free(modbus_t *ctx)
    subroutine modbus_free(ctx)
        type(c_ptr), intent(inout) :: ctx

        call modbus_free_(ctx)
        ctx = c_null_ptr
    end subroutine modbus_free

    ! const char *modbus_strerror(int errnum)
    function modbus_strerror(err_num) result(str)
        integer, intent(in)           :: err_num
        character(len=:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = modbus_strerror_(err_num)
        call c_f_str_ptr(ptr, str)
    end function modbus_strerror
end module modbus
