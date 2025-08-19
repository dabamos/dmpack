! modbus.f90
!
! Author:  Philipp Engel
! Licence: ISC
module modbus_rtu
    !! Auto-generated Fortran 2018 interace bindings to `modbus-rtu.h`.
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    ! Modbus_Application_Protocol_V1_1b.pdf Chapter 4 Section 1 Page 5
    ! RS232 / RS485 ADU = 253 bytes + slave (1 byte) + CRC (2 bytes) = 256 bytes
    integer(kind=c_int), parameter, public :: MODBUS_RTU_MAX_ADU_LENGTH = 256

    integer(kind=c_int), parameter, public :: MODBUS_RTU_RS232 = 0
    integer(kind=c_int), parameter, public :: MODBUS_RTU_RS485 = 1

    integer(kind=c_int), parameter, public :: MODBUS_RTU_RTS_NONE = 0
    integer(kind=c_int), parameter, public :: MODBUS_RTU_RTS_UP   = 1
    integer(kind=c_int), parameter, public :: MODBUS_RTU_RTS_DOWN = 2

    public :: modbus_new_rtu
    public :: modbus_new_rtu_
    public :: modbus_rtu_get_rts
    public :: modbus_rtu_get_rts_delay
    public :: modbus_rtu_get_serial_mode
    public :: modbus_rtu_set_custom_rts
    public :: modbus_rtu_set_rts
    public :: modbus_rtu_set_rts_delay
    public :: modbus_rtu_set_serial_mode

    interface
        ! modbus_t *modbus_new_rtu(const char *device, int baud, char parity, int data_bit, int stop_bit)
        function modbus_new_rtu_(device, baud, parity, data_bit, stop_bit) bind(c, name='modbus_new_rtu')
            import :: c_char, c_int, c_ptr
            implicit none
            character(kind=c_char), intent(in)        :: device
            integer(kind=c_int),    intent(in), value :: baud
            character(kind=c_char), intent(in), value :: parity
            integer(kind=c_int),    intent(in), value :: data_bit
            integer(kind=c_int),    intent(in), value :: stop_bit
            type(c_ptr)                               :: modbus_new_rtu_
        end function modbus_new_rtu_

        ! int modbus_rtu_get_rts(modbus_t *ctx)
        function modbus_rtu_get_rts(ctx) bind(c, name='modbus_rtu_get_rts')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            integer(kind=c_int)            :: modbus_rtu_get_rts
        end function modbus_rtu_get_rts

        ! int modbus_rtu_get_rts_delay(modbus_t *ctx)
        function modbus_rtu_get_rts_delay(ctx) bind(c, name='modbus_rtu_get_rts_delay')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            integer(kind=c_int)            :: modbus_rtu_get_rts_delay
        end function modbus_rtu_get_rts_delay

        ! int modbus_rtu_get_serial_mode(modbus_t *ctx)
        function modbus_rtu_get_serial_mode(ctx) bind(c, name='modbus_rtu_get_serial_mode')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ctx
            integer(kind=c_int)            :: modbus_rtu_get_serial_mode
        end function modbus_rtu_get_serial_mode

        ! int modbus_rtu_set_custom_rts(modbus_t *ctx, void (*set_rts)(modbus_t *ctx, int on))
        function modbus_rtu_set_custom_rts(ctx, funptr) bind(c, name='modbus_rtu_set_custom_rts')
            import :: c_funptr, c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: ctx
            type(c_funptr), intent(in), value :: funptr
            integer(kind=c_int)               :: modbus_rtu_set_custom_rts
        end function modbus_rtu_set_custom_rts

        ! int modbus_rtu_set_rts(modbus_t *ctx, int mode)
        function modbus_rtu_set_rts(ctx, mode) bind(c, name='modbus_rtu_set_rts')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(in), value :: mode
            integer(kind=c_int)                    :: modbus_rtu_set_rts
        end function modbus_rtu_set_rts

        ! int modbus_rtu_set_rts_delay(modbus_t *ctx, int us)
        function modbus_rtu_set_rts_delay(ctx, us) bind(c, name='modbus_rtu_set_rts_delay')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(in), value :: us
            integer(kind=c_int)                    :: modbus_rtu_set_rts_delay
        end function modbus_rtu_set_rts_delay

        ! int modbus_rtu_set_serial_mode(modbus_t *ctx, int mode)
        function modbus_rtu_set_serial_mode(ctx, mode) bind(c, name='modbus_rtu_set_serial_mode')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: ctx
            integer(kind=c_int), intent(in), value :: mode
            integer(kind=c_int)                    :: modbus_rtu_set_serial_mode
        end function modbus_rtu_set_serial_mode
    end interface
contains
    ! modbus_t *modbus_new_rtu(const char *device, int baud, char parity, int data_bit, int stop_bit)
    function modbus_new_rtu(device, baud, parity, data_bit, stop_bit) result(ctx)
        character(len=*), intent(in) :: device
        integer,          intent(in) :: baud
        character,        intent(in) :: parity
        integer,          intent(in) :: data_bit
        integer,          intent(in) :: stop_bit
        type(c_ptr)                  :: ctx

        character(kind=c_char) :: parity_

        parity_ = parity ! Workaround for GNU Fortran.
        if (parity_ >= 'a' .and. parity_ <= 'z') parity_ = achar(iachar(parity_) - 32)
        ctx = modbus_new_rtu_(trim(device) // c_null_char, baud, parity_, data_bit, stop_bit)
    end function modbus_new_rtu
end module modbus_rtu
