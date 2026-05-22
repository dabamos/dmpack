! dm_msgpack.f90
!
! Author:  Philipp Engel
! Licence: ISC
module dm_msgpack
    !! Pure Fortran 2018 module for MessagePack serialisation and
    !! deserialisation.
    !!
    !! * [Specification](https://github.com/msgpack/msgpack/blob/master/spec.md)
    !!
    use :: dm_error
    use :: dm_kind
    implicit none
    private

    ! MessagePack objects.
    integer, parameter, public :: MSGPACK_NONE     = int(z'00')
    integer, parameter, public :: MSGPACK_FIXSTR   = int(z'A0')
    integer, parameter, public :: MSGPACK_NIL      = int(z'C0')
    integer, parameter, public :: MSGPACK_FALSE    = int(z'C2')
    integer, parameter, public :: MSGPACK_TRUE     = int(z'C3')
    integer, parameter, public :: MSGPACK_BIN8     = int(z'C4')
    integer, parameter, public :: MSGPACK_BIN16    = int(z'C5')
    integer, parameter, public :: MSGPACK_BIN32    = int(z'C6')
    integer, parameter, public :: MSGPACK_EXT8     = int(z'C7')
    integer, parameter, public :: MSGPACK_EXT16    = int(z'C8')
    integer, parameter, public :: MSGPACK_EXT32    = int(z'C9')
    integer, parameter, public :: MSGPACK_FLOAT32  = int(z'CA')
    integer, parameter, public :: MSGPACK_FLOAT64  = int(z'CB')
    integer, parameter, public :: MSGPACK_UINT8    = int(z'CC')
    integer, parameter, public :: MSGPACK_UINT16   = int(z'CD')
    integer, parameter, public :: MSGPACK_UINT32   = int(z'CE')
    integer, parameter, public :: MSGPACK_UINT64   = int(z'CF')
    integer, parameter, public :: MSGPACK_INT8     = int(z'D0')
    integer, parameter, public :: MSGPACK_INT16    = int(z'D1')
    integer, parameter, public :: MSGPACK_INT32    = int(z'D2')
    integer, parameter, public :: MSGPACK_INT64    = int(z'D3')
    integer, parameter, public :: MSGPACK_FIXEXT1  = int(z'D4')
    integer, parameter, public :: MSGPACK_FIXEXT2  = int(z'D5')
    integer, parameter, public :: MSGPACK_FIXEXT4  = int(z'D6')
    integer, parameter, public :: MSGPACK_FIXEXT8  = int(z'D7')
    integer, parameter, public :: MSGPACK_FIXEXT16 = int(z'D8')
    integer, parameter, public :: MSGPACK_STR8     = int(z'D9')
    integer, parameter, public :: MSGPACK_STR16    = int(z'DA')
    integer, parameter, public :: MSGPACK_STR32    = int(z'DB')
    integer, parameter, public :: MSGPACK_ARRAY16  = int(z'DC')
    integer, parameter, public :: MSGPACK_ARRAY32  = int(z'DD')
    integer, parameter, public :: MSGPACK_MAP16    = int(z'DE')
    integer, parameter, public :: MSGPACK_MAP32    = int(z'DF')

    ! MessagePack object sizes [byte].
    integer, parameter, public :: MSGPACK_SIZE_BOOL    = 1
    integer, parameter, public :: MSGPACK_SIZE_FALSE   = 1
    integer, parameter, public :: MSGPACK_SIZE_FIXSTR  = 1
    integer, parameter, public :: MSGPACK_SIZE_FLOAT32 = 5
    integer, parameter, public :: MSGPACK_SIZE_FLOAT64 = 9
    integer, parameter, public :: MSGPACK_SIZE_INT8    = 2
    integer, parameter, public :: MSGPACK_SIZE_INT16   = 3
    integer, parameter, public :: MSGPACK_SIZE_INT32   = 5
    integer, parameter, public :: MSGPACK_SIZE_INT64   = 9
    integer, parameter, public :: MSGPACK_SIZE_NIL     = 1
    integer, parameter, public :: MSGPACK_SIZE_STR8    = 2
    integer, parameter, public :: MSGPACK_SIZE_STR16   = 3
    integer, parameter, public :: MSGPACK_SIZE_STR32   = 5
    integer, parameter, public :: MSGPACK_SIZE_TRUE    = 1

    type, public :: msgpack_buffer_type
        !! Byte buffer.
        private
        character(:), allocatable :: bytes
    end type msgpack_buffer_type

    type, public :: msgpack_object_type
        !! MessagePack object.
        integer               :: type   = MSGPACK_NONE !! Object type.
        integer               :: nbytes = 0            !! Object size (incl. payload).
        integer               :: length = 0            !! Payload size (string).
        character(:), pointer :: bytes  => null()
    end type msgpack_object_type

    type, public :: msgpack_packer_type
        !! Packer context.
        private
        type(msgpack_buffer_type), pointer :: buffer => null()
        integer(i8)                        :: index  = 0_i8
        integer(i8)                        :: size   = 0_i8
    end type msgpack_packer_type

    type, public :: msgpack_unpack_type
        !! Unpack context.
        type(msgpack_object_type) :: object = msgpack_object_type()
        integer(i8), private      :: index = 0_i8
    end type msgpack_unpack_type

    interface dm_msgpack_pack
        !! Generic pack routine.
        module procedure :: dm_msgpack_pack_bool
        module procedure :: dm_msgpack_pack_float32
        module procedure :: dm_msgpack_pack_float64
        module procedure :: dm_msgpack_pack_int32
        module procedure :: dm_msgpack_pack_int64
        module procedure :: dm_msgpack_pack_string
    end interface dm_msgpack_pack

    interface dm_msgpack_read
        !! Generic read routine.
        module procedure :: dm_msgpack_read_bool
        module procedure :: dm_msgpack_read_float32
        module procedure :: dm_msgpack_read_float64
        module procedure :: dm_msgpack_read_int32
        module procedure :: dm_msgpack_read_int64
        module procedure :: dm_msgpack_read_string
    end interface dm_msgpack_read

    interface dm_msgpack_unpack
        !! Generic unpack routine.
        module procedure :: dm_msgpack_unpack_bool
        module procedure :: dm_msgpack_unpack_float32
        module procedure :: dm_msgpack_unpack_float64
        module procedure :: dm_msgpack_unpack_int32
        module procedure :: dm_msgpack_unpack_int64
        module procedure :: dm_msgpack_unpack_string
    end interface dm_msgpack_unpack

    interface dm_msgpack_write
        !! Generic write routine.
        module procedure :: dm_msgpack_write_bool
        module procedure :: dm_msgpack_write_float32
        module procedure :: dm_msgpack_write_float64
        module procedure :: dm_msgpack_write_int32
        module procedure :: dm_msgpack_write_int64
        module procedure :: dm_msgpack_write_string
    end interface dm_msgpack_write

    public :: dm_msgpack_buffer_destroy
    public :: dm_msgpack_buffer_init
    public :: dm_msgpack_buffer_size

    public :: dm_msgpack_pack
    public :: dm_msgpack_pack_bool
    public :: dm_msgpack_pack_float32
    public :: dm_msgpack_pack_float64
    public :: dm_msgpack_pack_int32
    public :: dm_msgpack_pack_int64
    public :: dm_msgpack_pack_next
    public :: dm_msgpack_pack_nil

    public :: dm_msgpack_packer_destroy
    public :: dm_msgpack_packer_init
    public :: dm_msgpack_packer_result
    public :: dm_msgpack_packer_size

    public :: dm_msgpack_read
    public :: dm_msgpack_read_bool
    public :: dm_msgpack_read_float32
    public :: dm_msgpack_read_float64
    public :: dm_msgpack_read_int32
    public :: dm_msgpack_read_int64
    public :: dm_msgpack_read_nil
    public :: dm_msgpack_read_string

    public :: dm_msgpack_string_object
    public :: dm_msgpack_string_object_size

    public :: dm_msgpack_unpack
    public :: dm_msgpack_unpack_destroy
    public :: dm_msgpack_unpack_float32
    public :: dm_msgpack_unpack_float64
    public :: dm_msgpack_unpack_int32
    public :: dm_msgpack_unpack_int64
    public :: dm_msgpack_unpack_next

    public :: dm_msgpack_write
    public :: dm_msgpack_write_bool
    public :: dm_msgpack_write_fixstr
    public :: dm_msgpack_write_float32
    public :: dm_msgpack_write_float64
    public :: dm_msgpack_write_int32
    public :: dm_msgpack_write_int64
    public :: dm_msgpack_write_nil
    public :: dm_msgpack_write_str8
    public :: dm_msgpack_write_str16
    public :: dm_msgpack_write_str32
    public :: dm_msgpack_write_string
contains
    ! **************************************************************************
    ! MSGPACK BUFFER
    ! **************************************************************************
    pure subroutine dm_msgpack_buffer_destroy(buffer)
        type(msgpack_buffer_type), intent(inout) :: buffer !! Buffer.

        if (allocated(buffer%bytes)) deallocate (buffer%bytes)
    end subroutine dm_msgpack_buffer_destroy

    pure subroutine dm_msgpack_buffer_init(buffer, size, error)
        type(msgpack_buffer_type), intent(out)           :: buffer !! Buffer.
        integer(i8),               intent(in)            :: size   !! Max. size of buffer [byte].
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: error_

        if (present(error)) error = E_NONE

        if (size < 1) then
            if (present(error)) error = E_INVALID
            return
        end if

        allocate (character(size) :: buffer%bytes, stat=error_)

        if (error_ /= 0) then
            if (present(error)) error = E_ALLOC
            return
        end if
    end subroutine dm_msgpack_buffer_init

    pure function dm_msgpack_buffer_size(buffer) result(size)
        type(msgpack_buffer_type), intent(in) :: buffer !! Buffer.
        integer(i8)                           :: size   !! Buffer size.

        if (allocated(buffer%bytes)) then
            size = len(buffer%bytes, i8)
        else
            size = 0
        end if
    end function dm_msgpack_buffer_size

    ! **************************************************************************
    ! MSGPACK PACK
    ! **************************************************************************
    pure subroutine dm_msgpack_pack_bool(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        logical,                   intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: error_
        integer(i8) :: pos1, pos2

        call dm_msgpack_pack_next(packer, MSGPACK_SIZE_BOOL, pos1, pos2, error_)
        if (present(error)) error = error_
        if (error_ /= 0) return
        call dm_msgpack_write(value, packer%buffer%bytes(pos1:pos2))
    end subroutine dm_msgpack_pack_bool

    pure subroutine dm_msgpack_pack_float32(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        real(r4),                  intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: error_
        integer(i8) :: pos1, pos2

        call dm_msgpack_pack_next(packer, MSGPACK_SIZE_FLOAT32, pos1, pos2, error_)
        if (present(error)) error = error_
        if (error_ /= 0) return
        call dm_msgpack_write(value, packer%buffer%bytes(pos1:pos2))
    end subroutine dm_msgpack_pack_float32

    pure subroutine dm_msgpack_pack_float64(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        real(r8),                  intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: error_
        integer(i8) :: pos1, pos2

        call dm_msgpack_pack_next(packer, MSGPACK_SIZE_FLOAT64, pos1, pos2, error_)
        if (present(error)) error = error_
        if (error_ /= 0) return
        call dm_msgpack_write(value, packer%buffer%bytes(pos1:pos2))
    end subroutine dm_msgpack_pack_float64

    pure subroutine dm_msgpack_pack_int32(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer(i4),               intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: error_
        integer(i8) :: pos1, pos2

        call dm_msgpack_pack_next(packer, MSGPACK_SIZE_INT32, pos1, pos2, error_)
        if (present(error)) error = error_
        if (error_ /= 0) return
        call dm_msgpack_write(value, packer%buffer%bytes(pos1:pos2))
    end subroutine dm_msgpack_pack_int32

    pure subroutine dm_msgpack_pack_int64(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer(i8),               intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: error_
        integer(i8) :: pos1, pos2

        call dm_msgpack_pack_next(packer, MSGPACK_SIZE_INT64, pos1, pos2, error_)
        if (present(error)) error = error_
        if (error_ /= 0) return
        call dm_msgpack_write(value, packer%buffer%bytes(pos1:pos2))
    end subroutine dm_msgpack_pack_int64

    pure subroutine dm_msgpack_pack_next(packer, size, pos1, pos2, error)
        type(msgpack_packer_type), intent(inout) :: packer !! Packer.
        integer,                   intent(in)    :: size   !! Size of type [byte].
        integer(i8),               intent(out)   :: pos1   !! Position of first bytes.
        integer(i8),               intent(out)   :: pos2   !! Position of last byte..
        integer,                   intent(out)   :: error  !! Error code.

        pos1 = 0
        pos2 = 0
        error = E_NONE

        if (packer%size == 0) then
            error = E_CORRUPT
            return
        end if

        if (packer%index + size > packer%size) then
            error = E_BOUNDS
            return
        end if

        pos1 = packer%index + 1
        pos2 = packer%index + size
        packer%index = pos2
    end subroutine dm_msgpack_pack_next

    pure subroutine dm_msgpack_pack_nil(packer, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: error_
        integer(i8) :: pos1, pos2

        call dm_msgpack_pack_next(packer, MSGPACK_SIZE_NIL, pos1, pos2, error_)
        if (present(error)) error = error_
        if (error_ /= 0) return
        call dm_msgpack_write_nil(packer%buffer%bytes(pos1:pos2))
    end subroutine dm_msgpack_pack_nil

    pure subroutine dm_msgpack_pack_string(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        character(*),              intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: error_
        integer     :: n
        integer(i8) :: pos1, pos2

        n = dm_msgpack_string_object_size(len(value))
        call dm_msgpack_pack_next(packer, n, pos1, pos2, error_)
        if (present(error)) error = error_
        if (error_ /= 0) return
        call dm_msgpack_write(value, packer%buffer%bytes(pos1:pos2))
    end subroutine dm_msgpack_pack_string

    ! **************************************************************************
    ! MSGPACK PACKER
    ! **************************************************************************
    pure subroutine dm_msgpack_packer_destroy(packer)
        !! Destroy packer.
        type(msgpack_packer_type), intent(inout) :: packer !! Packer.

        packer = msgpack_packer_type()
    end subroutine dm_msgpack_packer_destroy

    pure subroutine dm_msgpack_packer_init(packer, buffer)
        !! Initialises packer and associated byte buffer.
        type(msgpack_packer_type),         intent(out)   :: packer !! Packer.
        type(msgpack_buffer_type), target, intent(inout) :: buffer !! Buffer.

        packer%buffer => buffer
        packer%size   = dm_msgpack_buffer_size(buffer)
    end subroutine dm_msgpack_packer_init

    function dm_msgpack_packer_result(packer) result(bytes)
        !! Returns pointer to packed data in byte buffer.
        type(msgpack_packer_type), intent(inout) :: packer !! Packer.
        character(:), pointer                    :: bytes  !! Pointer to packed data.

        nullify (bytes)
        if (packer%index == 0 .or. dm_msgpack_buffer_size(packer%buffer) == 0) return
        bytes => packer%buffer%bytes(1:packer%index)
    end function dm_msgpack_packer_result

    pure function dm_msgpack_packer_size(packer) result(size)
        !! Returns actual size of packed data in associated buffer as 8-byte
        !! integer [byte].
        type(msgpack_packer_type), intent(in) :: packer !! Packer.
        integer(i8)                           :: size   !! Packed data size.

        size = packer%index
    end function dm_msgpack_packer_size

    ! **************************************************************************
    ! MSGPACK READ
    ! **************************************************************************
    pure subroutine dm_msgpack_read_bool(bytes, value, error)
        !! Reads MessagePack bool from byte buffer.
        !!
        !! Format: 0xC2 or 0xC3.
        character(*), intent(in)            :: bytes !! Input buffer.
        logical,      intent(out)           :: value !! Output value.
        integer,      intent(out), optional :: error !! Error code.

        integer(i4) :: b

        value = .false.
        if (present(error)) error = E_NONE

        if (len(bytes) < MSGPACK_SIZE_BOOL) then
            if (present(error)) error = E_BOUNDS
            return
        end if

        b = ichar(bytes(1:1))

        select case (b)
            case (MSGPACK_FALSE); value = .false.
            case (MSGPACK_TRUE);  value = .true.
            case default;         if (present(error)) error = E_FORMAT
        end select
    end subroutine dm_msgpack_read_bool

    pure subroutine dm_msgpack_read_float32(bytes, value, error)
        !! Read MessagePack float32 from byte buffer.
        !!
        !! Format: 0xCA + 4-byte real (big-endian).
        character(len=*), intent(in)            :: bytes !! Input buffer.
        real(r4),         intent(out)           :: value !! Output value.
        integer,          intent(out), optional :: error !! Error code.

        integer(i4) :: b(MSGPACK_SIZE_FLOAT32)
        integer(i4) :: bits

        value = 0.0_r4
        if (present(error)) error = E_NONE

        if (len(bytes) < MSGPACK_SIZE_FLOAT32) then
            if (present(error)) error = E_BOUNDS
            return
        end if

        b(1) = ichar(bytes(1:1))

        if (b(1) /= MSGPACK_FLOAT32) then
            if (present(error)) error = E_FORMAT
            return
        end if

        b(2) = ichar(bytes(2:2))
        b(3) = ichar(bytes(3:3))
        b(4) = ichar(bytes(4:4))
        b(5) = ichar(bytes(5:5))

        bits = 0_i4
        bits = ior(bits, shiftl(iand(b(2), int(z'FF')), 24))
        bits = ior(bits, shiftl(iand(b(3), int(z'FF')), 16))
        bits = ior(bits, shiftl(iand(b(4), int(z'FF')),  8))
        bits = ior(bits,        iand(b(5), int(z'FF')))

        value = transfer(bits, value)
    end subroutine dm_msgpack_read_float32

    pure subroutine dm_msgpack_read_float64(bytes, value, error)
        !! Reads MessagePack float64 from byte buffer.
        !!
        !! Format: 0xCB + 8-byte real (big-endian).
        character(len=*), intent(in)            :: bytes !! Input buffer.
        real(r8),         intent(out)           :: value !! Output value.
        integer,          intent(out), optional :: error !! Error code.

        integer(i4) :: b(MSGPACK_SIZE_FLOAT64)
        integer(i8) :: bits

        value = 0.0_r8
        if (present(error)) error = E_NONE

        if (len(bytes) < MSGPACK_SIZE_FLOAT64) then
            if (present(error)) error = E_BOUNDS
            return
        end if

        b(1) = ichar(bytes(1:1))

        if (b(1) /= MSGPACK_FLOAT64) then
            if (present(error)) error = E_FORMAT
            return
        end if

        b(2) = ichar(bytes(2:2))
        b(3) = ichar(bytes(3:3))
        b(4) = ichar(bytes(4:4))
        b(5) = ichar(bytes(5:5))
        b(6) = ichar(bytes(6:6))
        b(7) = ichar(bytes(7:7))
        b(8) = ichar(bytes(8:8))
        b(9) = ichar(bytes(9:9))

        bits = 0_i8
        bits = ior(bits, shiftl(iand(int(b(2), i8), int(z'FF', i8)), 56))
        bits = ior(bits, shiftl(iand(int(b(3), i8), int(z'FF', i8)), 48))
        bits = ior(bits, shiftl(iand(int(b(4), i8), int(z'FF', i8)), 40))
        bits = ior(bits, shiftl(iand(int(b(5), i8), int(z'FF', i8)), 32))
        bits = ior(bits, shiftl(iand(int(b(6), i8), int(z'FF', i8)), 24))
        bits = ior(bits, shiftl(iand(int(b(7), i8), int(z'FF', i8)), 16))
        bits = ior(bits, shiftl(iand(int(b(8), i8), int(z'FF', i8)),  8))
        bits = ior(bits,        iand(int(b(9), i8), int(z'FF', i8)))

        value = transfer(bits, value)
    end subroutine dm_msgpack_read_float64

    pure subroutine dm_msgpack_read_int32(bytes, value, error)
        !! Reads MessagePack int32 from byte buffer.
        !!
        !! Format: 0xD2 + 4-byte signed integer (big-endian).
        character(*), intent(in)            :: bytes !! Input buffer.
        integer(i4),  intent(out)           :: value !! Output value.
        integer,      intent(out), optional :: error !! Error code.

        integer(i4) :: b(MSGPACK_SIZE_FLOAT32)

        value = 0
        if (present(error)) error = E_NONE

        if (len(bytes) < MSGPACK_SIZE_INT32) then
            if (present(error)) error = E_BOUNDS
            return
        end if

        b(1) = ichar(bytes(1:1))

        if (b(1) /= MSGPACK_INT32) then
            if (present(error)) error = E_FORMAT
            return
        end if

        b(2) = ichar(bytes(2:2))
        b(3) = ichar(bytes(3:3))
        b(4) = ichar(bytes(4:4))
        b(5) = ichar(bytes(5:5))

        value = ior(shiftl(b(2), 24), ior(shiftl(b(3), 16), ior(shiftl(b(4), 8), b(5))))
    end subroutine dm_msgpack_read_int32

    pure subroutine dm_msgpack_read_int64(bytes, value, error)
        !! Reads MessagePack int64 from byte buffer.
        !!
        !! Format: 0xD3 + 8-byte signed integer (big-endian).
        character(*), intent(in)            :: bytes !! Input buffer.
        integer(i8),  intent(out)           :: value !! Output value.
        integer,      intent(out), optional :: error !! Error code.

        integer     :: i
        integer(i4) :: b
        integer(i8) :: bits

        value = 0_i8
        if (present(error)) error = E_NONE

        if (len(bytes) < MSGPACK_SIZE_INT64) then
            if (present(error)) error = E_BOUNDS
            return
        end if

        b = ichar(bytes(1:1))

        if (b /= MSGPACK_INT64) then
            if (present(error)) error = E_FORMAT
            return
        end if

        do i = 2, MSGPACK_SIZE_FLOAT64
            bits  = int(ichar(bytes(i:i)), i8)
            value = ior(ishft(value, 8), bits)
        end do
    end subroutine dm_msgpack_read_int64

    pure subroutine dm_msgpack_read_nil(bytes, error)
        !! Reads MessagePack nil from byte buffer.
        !!
        !! Format: 0xC0.
        character(*), intent(in)            :: bytes !! Input buffer.
        integer,      intent(out), optional :: error !! Error code.

        if (present(error)) error = E_NONE

        if (len(bytes) < MSGPACK_SIZE_NIL) then
            if (present(error)) error = E_BOUNDS
            return
        end if

        if (ichar(bytes(1:1)) == MSGPACK_NIL) return
        if (present(error)) error = E_FORMAT
    end subroutine dm_msgpack_read_nil

    pure subroutine dm_msgpack_read_string(bytes, value, length, error)
        !! Deserialise MessagePack string (fixstr, str8, str16, str32) from byte
        !! buffer.
        !!
        !! Multi-byte lengths are decoded as big-endian.
        character(*), intent(in)            :: bytes  !! Input buffer.
        character(*), intent(inout)         :: value  !! Output value.
        integer,      intent(out), optional :: length !! String length.
        integer,      intent(out), optional :: error  !! Error code.

        integer :: pos1, pos2, rc

        type(msgpack_object_type) :: object

        read_block: block
            rc = E_FORMAT
            object = dm_msgpack_string_object(bytes)

            select case (object%type)
                case (MSGPACK_FIXSTR); pos1 = MSGPACK_SIZE_FIXSTR + 1
                case (MSGPACK_STR8);   pos1 = MSGPACK_SIZE_STR8   + 1
                case (MSGPACK_STR16);  pos1 = MSGPACK_SIZE_STR16  + 1
                case (MSGPACK_STR32);  pos1 = MSGPACK_SIZE_STR32  + 1
                case default;          exit read_block
            end select

            pos2 = object%nbytes

            rc = E_BOUNDS
            if (pos1 > len(bytes) .or. pos2 > len(bytes)) exit read_block

            rc = E_NONE
            value = bytes(pos1:pos2)
        end block read_block

        if (present(length)) length = object%length
        if (present(error))  error  = rc
    end subroutine dm_msgpack_read_string

    ! **************************************************************************
    ! MSGPACK STRING OBJECT
    ! **************************************************************************
    pure function dm_msgpack_string_object(bytes) result(object)
        character(*), intent(in)  :: bytes
        type(msgpack_object_type) :: object

        integer :: b, n

        if (len(bytes) == 0) return

        b = ichar(bytes(1:1))

        if (iand(b, int(z'E0')) == MSGPACK_FIXSTR) then
            n = iand(b, int(z'1F'))
            object = msgpack_object_type(MSGPACK_FIXSTR, MSGPACK_SIZE_FIXSTR + n, n)
            return
        end if

        select case (b)
            case (MSGPACK_STR8)
                if (len(bytes) < MSGPACK_SIZE_STR8) return
                n = ichar(bytes(2:2))
                object = msgpack_object_type(b, MSGPACK_SIZE_STR8 + n, n)

            case (MSGPACK_STR16)
                if (len(bytes) < MSGPACK_SIZE_STR16) return
                n = 0
                n = n + shiftl(ichar(bytes(2:2)), 8)
                n = n +        ichar(bytes(3:3))
                object = msgpack_object_type(b, MSGPACK_SIZE_STR16 + n, n)

            case (MSGPACK_STR32)
                if (len(bytes) < MSGPACK_SIZE_STR32) return
                n = 0
                n = n + shiftl(ichar(bytes(2:2)), 24)
                n = n + shiftl(ichar(bytes(3:3)), 16)
                n = n + shiftl(ichar(bytes(4:4)),  8)
                n = n +        ichar(bytes(5:5))
                object = msgpack_object_type(b, MSGPACK_SIZE_STR32 + n, n)
        end select
    end function dm_msgpack_string_object

    pure integer function dm_msgpack_string_object_size(n) result(size)
        !! Returns size of MessagePack string object in bytes based on the
        !! string length `n`.
        integer, intent(in) :: n !! String length.

        if (n <= 31) then
            size = MSGPACK_SIZE_FIXSTR + n
        else if (n <= 255) then
            size = MSGPACK_SIZE_STR8 + n
        else if (n <= 65535) then
            size = MSGPACK_SIZE_STR16 + n
        else
            size = MSGPACK_SIZE_STR32 + n
        end if
    end function dm_msgpack_string_object_size

    ! **************************************************************************
    ! MSGPACK UNPACK
    ! **************************************************************************
    pure subroutine dm_msgpack_unpack_destroy(unpack)
        type(msgpack_unpack_type), intent(inout) :: unpack !! Unpack type.

        unpack = msgpack_unpack_type()
    end subroutine dm_msgpack_unpack_destroy

    pure subroutine dm_msgpack_unpack_bool(object, value, error)
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        logical,                   intent(out)           :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        value = .false.

        if (.not. associated(object%bytes)) then
            if (present(error)) error = E_CORRUPT
            return
        end if

        call dm_msgpack_read(object%bytes, value, error)
    end subroutine dm_msgpack_unpack_bool

    pure subroutine dm_msgpack_unpack_float32(object, value, error)
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        real(r4),                  intent(out)           :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        value = 0.0_r4

        if (.not. associated(object%bytes)) then
            if (present(error)) error = E_CORRUPT
            return
        end if

        call dm_msgpack_read(object%bytes, value, error)
    end subroutine dm_msgpack_unpack_float32

    pure subroutine dm_msgpack_unpack_float64(object, value, error)
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        real(r8),                  intent(out)           :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        value = 0.0_r8

        if (.not. associated(object%bytes)) then
            if (present(error)) error = E_CORRUPT
            return
        end if

        call dm_msgpack_read(object%bytes, value, error)
    end subroutine dm_msgpack_unpack_float64

    pure subroutine dm_msgpack_unpack_int32(object, value, error)
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        integer(i4),               intent(out)           :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        value = 0_i4

        if (.not. associated(object%bytes)) then
            if (present(error)) error = E_CORRUPT
            return
        end if

        call dm_msgpack_read(object%bytes, value, error)
    end subroutine dm_msgpack_unpack_int32

    pure subroutine dm_msgpack_unpack_int64(object, value, error)
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        integer(i8),               intent(out)           :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        value = 0_i8

        if (.not. associated(object%bytes)) then
            if (present(error)) error = E_CORRUPT
            return
        end if

        call dm_msgpack_read(object%bytes, value, error)
    end subroutine dm_msgpack_unpack_int64

    pure subroutine dm_msgpack_unpack_next(unpack, buffer, error)
        type(msgpack_unpack_type),         intent(inout) :: unpack !! Unpack context.
        type(msgpack_buffer_type), target, intent(inout) :: buffer !! Input buffer.
        integer,                           intent(out)   :: error  !! Error code.

        integer(i4) :: b
        integer(i8) :: i, j, n

        i = unpack%index + 1
        n = dm_msgpack_buffer_size(buffer)

        error = E_BOUNDS
        if (i > n) return

        associate (bytes => buffer%bytes, object => unpack%object)
            b = ichar(bytes(i:i))

            object_select: select case (b)
                case (MSGPACK_NIL);     object = msgpack_object_type(MSGPACK_NIL,     MSGPACK_SIZE_NIL)
                case (MSGPACK_FALSE);   object = msgpack_object_type(MSGPACK_FALSE,   MSGPACK_SIZE_BOOL)
                case (MSGPACK_TRUE);    object = msgpack_object_type(MSGPACK_TRUE,    MSGPACK_SIZE_BOOL)
                case (MSGPACK_FLOAT32); object = msgpack_object_type(MSGPACK_FLOAT32, MSGPACK_SIZE_FLOAT32)
                case (MSGPACK_FLOAT64); object = msgpack_object_type(MSGPACK_FLOAT64, MSGPACK_SIZE_FLOAT64)
                case (MSGPACK_INT32);   object = msgpack_object_type(MSGPACK_INT32,   MSGPACK_SIZE_INT32)
                case (MSGPACK_INT64);   object = msgpack_object_type(MSGPACK_INT64,   MSGPACK_SIZE_INT64)
                case (MSGPACK_STR8);    object = dm_msgpack_string_object(bytes)
                case (MSGPACK_STR16);   object = dm_msgpack_string_object(bytes)
                case (MSGPACK_STR32);   object = dm_msgpack_string_object(bytes)
                case default
                    if (iand(b, int(z'E0')) == MSGPACK_FIXSTR) then
                        object = dm_msgpack_string_object(bytes)
                        exit object_select
                    end if

                    error = E_INVALID
                    return
            end select object_select

            error = E_BOUNDS
            if (unpack%index + object%nbytes > n) return

            error = E_NONE
            j = i + object%nbytes
            object%bytes => bytes(i:j)
            unpack%index = unpack%index + object%nbytes
        end associate
    end subroutine dm_msgpack_unpack_next

    pure subroutine dm_msgpack_unpack_string(object, value, error)
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        character(*),              intent(inout)         :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        if (.not. associated(object%bytes)) then
            if (present(error)) error = E_CORRUPT
            value = ''
            return
        end if

        call dm_msgpack_read(object%bytes, value, error)
    end subroutine dm_msgpack_unpack_string

    ! **************************************************************************
    ! MSGPACK WRITE
    ! **************************************************************************
    pure subroutine dm_msgpack_write_bool(value, bytes)
        !! Writes MessagePack bool into byte buffer.
        !!
        !! Format: 0xC2 or 0xC3.
        logical,                      intent(in)  :: value !! Input logical to encode.
        character(MSGPACK_SIZE_BOOL), intent(out) :: bytes !! Output buffer.

        if (value) then
            bytes(1:1) = char(MSGPACK_TRUE)
        else
            bytes(1:1) = char(MSGPACK_FALSE)
        end if
    end subroutine dm_msgpack_write_bool

    pure subroutine dm_msgpack_write_fixstr(value, bytes, nbytes)
        character(*), intent(in)            :: value  !! Input value.
        character(*), intent(inout)         :: bytes  !! Output buffer.
        integer,      intent(out), optional :: nbytes !! Number of bytes written.

        character(1) :: b
        integer      :: n

        n = min(31, len(value))
        if (present(nbytes)) nbytes = MSGPACK_SIZE_FIXSTR + n

        b = char(MSGPACK_FIXSTR + n)

        if (n > 0) then
            bytes = b // value(1:n)
        else
            bytes = b
        end if
    end subroutine dm_msgpack_write_fixstr

    pure subroutine dm_msgpack_write_float32(value, bytes)
        !! Writes MessagePack float32 to byte buffer.
        !!
        !! Format: 0xCA + 4 byte real (big-endian).
        real(r4),                        intent(in)  :: value
        character(MSGPACK_SIZE_FLOAT32), intent(out) :: bytes

        integer(i4) :: bits

        bits = transfer(value, bits)

        bytes(1:1) = char(MSGPACK_FLOAT32                   )
        bytes(2:2) = char(iand(shiftr(bits, 24), int(z'FF')))
        bytes(3:3) = char(iand(shiftr(bits, 16), int(z'FF')))
        bytes(4:4) = char(iand(shiftr(bits,  8), int(z'FF')))
        bytes(5:5) = char(iand(bits,             int(z'FF')))
    end subroutine dm_msgpack_write_float32

    pure subroutine dm_msgpack_write_float64(value, bytes)
        !! Writes MessagePack float64 to byte buffer.
        !!
        !! Format: 0xCB + 8-byte real (big-endian).
        real(r8),                        intent(in)  :: value
        character(MSGPACK_SIZE_FLOAT64), intent(out) :: bytes

        integer(i8) :: bits

        bits = transfer(value, bits)

        bytes(1:1) = char(MSGPACK_FLOAT64                       )
        bytes(2:2) = char(iand(shiftr(bits, 56), int(z'FF', i8)))
        bytes(3:3) = char(iand(shiftr(bits, 48), int(z'FF', i8)))
        bytes(4:4) = char(iand(shiftr(bits, 40), int(z'FF', i8)))
        bytes(5:5) = char(iand(shiftr(bits, 32), int(z'FF', i8)))
        bytes(6:6) = char(iand(shiftr(bits, 24), int(z'FF', i8)))
        bytes(7:7) = char(iand(shiftr(bits, 16), int(z'FF', i8)))
        bytes(8:8) = char(iand(shiftr(bits,  8), int(z'FF', i8)))
        bytes(9:9) = char(iand(bits,             int(z'FF', i8)))
    end subroutine dm_msgpack_write_float64

    pure subroutine dm_msgpack_write_int32(value, bytes)
        !! Writes MessagePack int32 into byte buffer.
        !!
        !! Format: 0xD2 + 4-byte signed integer (big-endian).
        integer(i4),                   intent(in)  :: value !! Input integer to encode.
        character(MSGPACK_SIZE_INT32), intent(out) :: bytes !! Output buffer.

        bytes(1:1) = char(MSGPACK_INT32                      )
        bytes(2:2) = char(iand(shiftr(value, 24), int(z'FF')))
        bytes(3:3) = char(iand(shiftr(value, 16), int(z'FF')))
        bytes(4:4) = char(iand(shiftr(value,  8), int(z'FF')))
        bytes(5:5) = char(iand(value,             int(z'FF')))
    end subroutine dm_msgpack_write_int32

    pure subroutine dm_msgpack_write_int64(value, bytes)
        !! Writes MessagePack int64 into byte buffer.
        !!
        !! Format: 0xD3 + 8-byte signed integer (big-endian).
        integer(i8),                   intent(in)  :: value !! Input integer to encode.
        character(MSGPACK_SIZE_INT64), intent(out) :: bytes !! Output buffer.

        integer :: b, i, j, s

        bytes(1:1) = char(MSGPACK_INT64)

        do i = 1, 8
            s = 64 - (8 * i)
            b = int(iand(shiftr(value, s), int(z'FF', i8)))
            j = i + 1
            bytes(j:j) = char(b)
        end do
    end subroutine dm_msgpack_write_int64

    pure subroutine dm_msgpack_write_nil(bytes)
        !! Writes MessagePack nil into byte buffer.
        !!
        !! Format: 0xC0.
        character(MSGPACK_SIZE_NIL), intent(out) :: bytes !! Output buffer.

        bytes(1:1) = char(MSGPACK_NIL)
    end subroutine dm_msgpack_write_nil

    pure subroutine dm_msgpack_write_str8(value, bytes, nbytes)
        !! The buffer `bytes` must be large enough to hold the MessagePack string.
        character(*), intent(in)            :: value  !! Input value.
        character(*), intent(inout)         :: bytes  !! Output buffer.
        integer,      intent(out), optional :: nbytes !! Number of bytes written.

        character(2) :: b
        integer      :: n

        n = min(255, len(value))
        if (present(nbytes)) nbytes = len(b) + n

        b(1:1) = char(MSGPACK_STR8)
        b(2:2) = char(iand(n, int(z'FF')))

        if (n > 0) then
            bytes = b // value(1:n)
        else
            bytes = b
        end if
    end subroutine dm_msgpack_write_str8

    pure subroutine dm_msgpack_write_str16(value, bytes, nbytes)
        !! The buffer `bytes` must be large enough to hold the MessagePack string.
        character(*), intent(in)            :: value  !! Input value.
        character(*), intent(inout)         :: bytes  !! Output buffer.
        integer,      intent(out), optional :: nbytes !! Number of bytes written.

        character(3) :: b
        integer      :: n

        n = min(65535, len(value))
        if (present(nbytes)) nbytes = len(b) + n

        b(1:1) = char(MSGPACK_STR16)
        b(2:2) = char(iand(shiftr(n, 8), int(z'FF')))
        b(3:3) = char(iand(n,            int(z'FF')))

        if (n > 0) then
            bytes = b // value(1:n)
        else
            bytes = b
        end if
    end subroutine dm_msgpack_write_str16

    pure subroutine dm_msgpack_write_str32(value, bytes, nbytes)
        !! The buffer `bytes` must be large enough to hold the MessagePack string.
        character(*), intent(in)            :: value  !! Input value.
        character(*), intent(inout)         :: bytes  !! Output buffer.
        integer,      intent(out), optional :: nbytes !! Number of bytes written.

        character(5) :: b
        integer      :: n

        n = min(huge(0), len(value))
        if (present(nbytes)) nbytes = len(b) + n

        b(1:1) = char(MSGPACK_STR32)
        b(2:2) = char(iand(shiftr(n, 24), int(z'FF')))
        b(3:3) = char(iand(shiftr(n, 16), int(z'FF')))
        b(4:4) = char(iand(shiftr(n,  8), int(z'FF')))
        b(5:5) = char(iand(n,             int(z'FF')))

        if (n > 0) then
            bytes = b // value(1:n)
        else
            bytes = b
        end if
    end subroutine dm_msgpack_write_str32

    pure subroutine dm_msgpack_write_string(value, bytes, nbytes)
        !! The buffer `bytes` must be large enough to hold the MessagePack string.
        character(*), intent(in)            :: value  !! Input value.
        character(*), intent(inout)         :: bytes  !! Output buffer.
        integer,      intent(out), optional :: nbytes !! Number of bytes written.

        if (present(nbytes)) nbytes = 0

        select case (len(value))
            case (    0:     31); call dm_msgpack_write_fixstr(value, bytes, nbytes)
            case (   32:    255); call dm_msgpack_write_str8  (value, bytes, nbytes)
            case (  256:  65535); call dm_msgpack_write_str16 (value, bytes, nbytes)
            case (65536:huge(0)); call dm_msgpack_write_str32 (value, bytes, nbytes)
            case default;         bytes = ' '
        end select
    end subroutine dm_msgpack_write_string
end module dm_msgpack
