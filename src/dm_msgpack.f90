! dm_msgpack.f90
!
! Author:  Philipp Engel
! Licence: ISC
module dm_msgpack
    !! Pure Fortran 2018 module for MessagePack serialisation and
    !! deserialisation (for little-endian platforms only).
    !!
    !! ## Examples
    !!
    !! The following program `pack` serialises an array consisting of a string,
    !! a 4-byte integer and a 8-byte real using the packer:
    !!
    !! ``` fortran
    !! ! pack.f90
    !! program main
    !!     use :: dmpack
    !!     implicit none (type, external)
    !!
    !!     type(msgpack_buffer_type) :: buffer
    !!     type(msgpack_packer_type) :: packer
    !!     type(msgpack_unpack_type) :: unpack
    !!
    !!     call dm_msgpack_init(buffer, 512_i8) ! Initialise buffer of 512 bytes.
    !!     call dm_msgpack_init(packer, buffer) ! Initialise packer context.
    !!
    !!     call dm_msgpack_pack_array16(packer, 3)             ! Create array.
    !!     call dm_msgpack_pack(packer, 'Hello from Fortran!') ! Add string.
    !!     call dm_msgpack_pack(packer, 42)                    ! Add 4-byte integer.
    !!     call dm_msgpack_pack(packer, 99999.99999_r8)        ! Add 8-byte real.
    !!
    !!     write (*, '(a)', advance='no') dm_msgpack_packer_result(packer)
    !!
    !!     call dm_msgpack_destroy(packer)
    !!     call dm_msgpack_destroy(buffer)
    !! end program main
    !! ```
    !!
    !! A Python script `unpack.py` may read the encoded message:
    !!
    !! ``` python
    !!  #!/usr/bin/env python3
    !!
    !!  import sys
    !!  import msgpack
    !!
    !!  def main():
    !!      if len(sys.argv) != 2:
    !!          print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
    !!          sys.exit(1)
    !!
    !!      with open(sys.argv[1], "rb") as f:
    !!          data = f.read()
    !!
    !!      # Decode the MessagePack payload.
    !!      value = msgpack.unpackb(data, raw=False)
    !!
    !!      # Expect: [string, integer, float]
    !!      if not isinstance(value, (list, tuple)) or len(value) != 3:
    !!          raise ValueError("Expected a MessagePack array containing 3 elements")
    !!
    !!      text, number, real = value
    !!
    !!      if not isinstance(text, str):
    !!          raise TypeError(f"Expected string, got {type(text).__name__}")
    !!
    !!      if not isinstance(number, int):
    !!          raise TypeError(f"Expected integer, got {type(number).__name__}")
    !!
    !!      if not isinstance(real, float):
    !!          raise TypeError(f"Expected float, got {type(real).__name__}")
    !!
    !!      print(f"String.: {text}")
    !!      print(f"Integer: {number}")
    !!      print(f"Float..: {real}")
    !!
    !!  if __name__ == "__main__":
    !!      main()
    !! ```
    !!
    !! Writing the output to file and then reading the encoded data in Python:
    !!
    !! ```
    !! $ gfortran -I/opt/include/dmpack -o pack pack.f90 /opt/lib/libdmpack.a
    !! $ ./pack > msg.bin
    !! $ ./unpack.py msg.bin
    !! String.: Hello from Fortran
    !! Integer: 42
    !! Float..: 99999.99999
    !! ```
    !!
    !! ## References
    !!
    !! * [MessagePack Specification](https://github.com/msgpack/msgpack/blob/master/spec.md)
    !!
    use :: dm_error
    use :: dm_kind
    use :: dm_platform
    use :: dm_util, only: dm_present_set
    implicit none
    private

    ! MessagePack objects.
    integer, parameter, public :: MSGPACK_NONE     = int(z'00')
    integer, parameter, public :: MSGPACK_FIXARRAY = int(z'90')
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
    integer, parameter, public :: MSGPACK_SIZE_ARRAY16  =  3 ! Header only.
    integer, parameter, public :: MSGPACK_SIZE_ARRAY32  =  5 ! Header only.
    integer, parameter, public :: MSGPACK_SIZE_BOOL     =  1 ! Header + value.
    integer, parameter, public :: MSGPACK_SIZE_FALSE    =  1 ! Header + value.
    integer, parameter, public :: MSGPACK_SIZE_FIXARRAY =  1 ! Header only.
    integer, parameter, public :: MSGPACK_SIZE_FIXSTR   =  1 ! Header only.
    integer, parameter, public :: MSGPACK_SIZE_FLOAT32  =  5 ! Header + value.
    integer, parameter, public :: MSGPACK_SIZE_FLOAT64  =  9 ! Header + value.
    integer, parameter, public :: MSGPACK_SIZE_INT16    =  3 ! Header + value.
    integer, parameter, public :: MSGPACK_SIZE_INT32    =  5 ! Header + value.
    integer, parameter, public :: MSGPACK_SIZE_INT64    =  9 ! Header + value.
    integer, parameter, public :: MSGPACK_SIZE_INT8     =  2 ! Header + value.
    integer, parameter, public :: MSGPACK_SIZE_NIL      =  1 ! Header + value.
    integer, parameter, public :: MSGPACK_SIZE_STR16    =  3 ! Header only.
    integer, parameter, public :: MSGPACK_SIZE_STR32    =  5 ! Header only.
    integer, parameter, public :: MSGPACK_SIZE_STR8     =  2 ! Header only.
    integer, parameter, public :: MSGPACK_SIZE_TRUE     =  1 ! Header + value.

    integer, parameter, public :: MSGPACK_SIZE_BEAT     =  8 ! Array size.
    integer, parameter, public :: MSGPACK_SIZE_DP       =  2 ! Array size.
    integer, parameter, public :: MSGPACK_SIZE_LOG      = 10 ! Array size.
    integer, parameter, public :: MSGPACK_SIZE_NODE     = 10 ! Array size.
    integer, parameter, public :: MSGPACK_SIZE_OBSERV   = 21 ! Array size.
    integer, parameter, public :: MSGPACK_SIZE_SENSOR   = 12 ! Array size.
    integer, parameter, public :: MSGPACK_SIZE_TARGET   = 10 ! Array size.

    type, public :: msgpack_buffer_type
        !! Byte buffer. Attribute `ptr` may point to attribute `bytes` or any
        !! other character string.
        private
        character(:), allocatable :: bytes !! Allocatable byte buffer.
        character(:), pointer     :: ptr   !! Pointer to byte buffer.
    end type msgpack_buffer_type

    type, public :: msgpack_object_type
        !! MessagePack data object that stores type within the bytes buffer.
        integer               :: type   = MSGPACK_NONE !! Object type.
        integer(i8)           :: nbytes = 0            !! Object size (incl. string size).
        integer(i8)           :: size   = 0            !! Payload size (array size, string length).
        character(:), pointer :: bytes  => null()
    end type msgpack_object_type

    type, public :: msgpack_packer_type
        !! Packer context that keeps track of the packing process.
        private
        type(msgpack_buffer_type), pointer :: buffer => null() !! Buffer type.
        integer(i8)                        :: index  = 0_i8    !! Current cursor position.
        integer(i8)                        :: size   = 0_i8    !! Buffer size.
    end type msgpack_packer_type

    type, public :: msgpack_unpack_type
        !! Unpack context that keeps track of the unpacking process.
        type(msgpack_object_type) :: object = msgpack_object_type() !! Unpacked MessagePack object.
        integer(i8), private      :: index = 0_i8                   !! Current cursor position.
    end type msgpack_unpack_type

    interface dm_msgpack_destroy
        !! Generic destruction routine for MessagePack buffer and packer.
        module procedure :: dm_msgpack_buffer_destroy
        module procedure :: dm_msgpack_packer_destroy
    end interface dm_msgpack_destroy

    interface dm_msgpack_buffer_init
        !! Generic init routine for MessagePack buffer (either allocatable
        !! string or pointer).
        module procedure :: msgpack_buffer_init_allocatable
        module procedure :: msgpack_buffer_init_pointer
    end interface dm_msgpack_buffer_init

    interface dm_msgpack_init
        !! Generic init routine for MessagePack buffer and packer.
        module procedure :: msgpack_buffer_init_allocatable
        module procedure :: msgpack_buffer_init_pointer
        module procedure :: dm_msgpack_packer_init
    end interface dm_msgpack_init

    interface dm_msgpack_next
        !! Generic pack/unpack next routine.
        module procedure :: msgpack_pack_next
        module procedure :: msgpack_unpack_next
    end interface dm_msgpack_next

    interface dm_msgpack_pack
        !! Generic pack routine. Not all types are included.
        module procedure :: dm_msgpack_pack_bool
        module procedure :: dm_msgpack_pack_float32
        module procedure :: dm_msgpack_pack_float64
        module procedure :: dm_msgpack_pack_int32
        module procedure :: dm_msgpack_pack_int64
        module procedure :: dm_msgpack_pack_string
    end interface dm_msgpack_pack

    interface dm_msgpack_pack_type
        !! Generic type pack routine.
        module procedure :: msgpack_pack_type_beat
        module procedure :: msgpack_pack_type_dp
        module procedure :: msgpack_pack_type_log
        module procedure :: msgpack_pack_type_node
        module procedure :: msgpack_pack_type_observ
        module procedure :: msgpack_pack_type_sensor
        module procedure :: msgpack_pack_type_target
    end interface dm_msgpack_pack_type

    interface dm_msgpack_unpack
        !! Generic unpack routine. Not all types are included.
        module procedure :: dm_msgpack_unpack_bool
        module procedure :: dm_msgpack_unpack_float32
        module procedure :: dm_msgpack_unpack_float64
        module procedure :: dm_msgpack_unpack_int32
        module procedure :: dm_msgpack_unpack_int64
        module procedure :: dm_msgpack_unpack_string
    end interface dm_msgpack_unpack

    interface dm_msgpack_unpack_type
        !! Generic type unpack routine.
        module procedure :: msgpack_unpack_type_beat
        module procedure :: msgpack_unpack_type_dp
        module procedure :: msgpack_unpack_type_log
        module procedure :: msgpack_unpack_type_node
        module procedure :: msgpack_unpack_type_observ
        module procedure :: msgpack_unpack_type_sensor
        module procedure :: msgpack_unpack_type_target
    end interface dm_msgpack_unpack_type

    public :: dm_msgpack_buffer_destroy
    public :: dm_msgpack_buffer_init
    public :: dm_msgpack_buffer_size

    public :: dm_msgpack_destroy
    public :: dm_msgpack_init
    public :: dm_msgpack_next

    public :: dm_msgpack_pack
    public :: dm_msgpack_pack_array
    public :: dm_msgpack_pack_array16
    public :: dm_msgpack_pack_array32
    public :: dm_msgpack_pack_bool
    public :: dm_msgpack_pack_fixarray
    public :: dm_msgpack_pack_float32
    public :: dm_msgpack_pack_float64
    public :: dm_msgpack_pack_int32
    public :: dm_msgpack_pack_int64
    public :: dm_msgpack_pack_nil
    public :: dm_msgpack_pack_string
    public :: dm_msgpack_pack_type

    public :: dm_msgpack_packer_destroy
    public :: dm_msgpack_packer_init
    public :: dm_msgpack_packer_result
    public :: dm_msgpack_packer_size

    public :: dm_msgpack_unpack
    public :: dm_msgpack_unpack_array
    public :: dm_msgpack_unpack_array16
    public :: dm_msgpack_unpack_array32
    public :: dm_msgpack_unpack_destroy
    public :: dm_msgpack_unpack_fixarray
    public :: dm_msgpack_unpack_float32
    public :: dm_msgpack_unpack_float64
    public :: dm_msgpack_unpack_int32
    public :: dm_msgpack_unpack_int64
    public :: dm_msgpack_unpack_nil
    public :: dm_msgpack_unpack_string
    public :: dm_msgpack_unpack_type

    private :: msgpack_array_object
    private :: msgpack_buffer_init_allocatable
    private :: msgpack_buffer_init_pointer
    private :: msgpack_string_object

    private :: msgpack_pack_next
    private :: msgpack_pack_type_beat
    private :: msgpack_pack_type_dp
    private :: msgpack_pack_type_log
    private :: msgpack_pack_type_node
    private :: msgpack_pack_type_observ
    private :: msgpack_pack_type_sensor
    private :: msgpack_pack_type_target

    private :: msgpack_unpack_next
    private :: msgpack_unpack_type_beat
    private :: msgpack_unpack_type_dp
    private :: msgpack_unpack_type_log
    private :: msgpack_unpack_type_node
    private :: msgpack_unpack_type_observ
    private :: msgpack_unpack_type_sensor
    private :: msgpack_unpack_type_target
contains
    ! **************************************************************************
    ! MSGPACK BUFFER
    ! **************************************************************************
    pure subroutine dm_msgpack_buffer_destroy(buffer)
        type(msgpack_buffer_type), intent(inout) :: buffer !! Buffer.

        if (allocated(buffer%bytes)) deallocate (buffer%bytes)
        if (associated(buffer%ptr))  nullify (buffer%ptr)
    end subroutine dm_msgpack_buffer_destroy

    pure function dm_msgpack_buffer_size(buffer) result(size)
        type(msgpack_buffer_type), intent(in) :: buffer !! Buffer.
        integer(i8)                           :: size   !! Buffer size.

        if (associated(buffer%ptr)) then
            size = len(buffer%ptr, i8)
        else
            size = 0
        end if
    end function dm_msgpack_buffer_size

    ! **************************************************************************
    ! MSGPACK PACK
    ! **************************************************************************
    pure subroutine dm_msgpack_pack_array(packer, size, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer,                   intent(in)            :: size   !! Input value (array size).
        integer,                   intent(out), optional :: error  !! Error code.

        select case (size)
            case (    0:       15); call dm_msgpack_pack_array16(packer, size, error)
            case (   16:2**16 - 1); call dm_msgpack_pack_array16(packer, size, error)
            case (2**16:  huge(0)); call dm_msgpack_pack_array16(packer, size, error)
            case default;           call dm_present_set(error, E_BOUNDS)
        end select
    end subroutine dm_msgpack_pack_array

    pure subroutine dm_msgpack_pack_array16(packer, size, error)
        !! Packs header of MessagePack array `array16`.
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if no buffer space is available.
        !! * `E_CORRUPT` if buffer size is invalid.
        !! * `E_PLATFORM` if platform is big-endian.
        !! *
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer,                   intent(in)            :: size   !! Input value (array size).
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: rc
        integer(i8) :: pos(2)

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_ARRAY16, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                bytes(1:1) = char(MSGPACK_ARRAY16                  )
                bytes(2:2) = char(iand(shiftr(size, 8), int(z'FF')))
                bytes(3:3) = char(iand(size,            int(z'FF')))
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_array16

    pure subroutine dm_msgpack_pack_array32(packer, size, error)
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if no buffer space is available.
        !! * `E_PLATFORM` if platform is big-endian.
        !! * `E_CORRUPT` if buffer size is invalid.
        !! *
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer,                   intent(in)            :: size   !! Input value (array size).
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: rc
        integer(i8) :: pos(2)

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_ARRAY32, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                bytes(1:1) = char(MSGPACK_ARRAY32                   )
                bytes(2:2) = char(iand(shiftr(size, 24), int(z'FF')))
                bytes(3:3) = char(iand(shiftr(size, 16), int(z'FF')))
                bytes(4:4) = char(iand(shiftr(size,  8), int(z'FF')))
                bytes(5:5) = char(iand(size,             int(z'FF')))
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_array32

    pure subroutine dm_msgpack_pack_bool(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        logical,                   intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: rc
        integer(i8) :: pos(2)

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_BOOL, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                if (value) then
                    bytes(1:1) = char(MSGPACK_TRUE)
                else
                    bytes(1:1) = char(MSGPACK_FALSE)
                end if
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_bool

    pure subroutine dm_msgpack_pack_fixarray(packer, size, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer(i4),               intent(in)            :: size   !! Input value (array size).
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: rc
        integer(i8) :: pos(2)

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            rc = E_BOUNDS
            if (size < 0 .or. size > 15) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_FIXARRAY, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
               bytes(1:1) = char(ior(MSGPACK_FIXARRAY, size))
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_fixarray

    pure subroutine dm_msgpack_pack_fixstr(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        character(*),              intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: n, rc
        integer(i8) :: pos(2)

        n = min(31, len(value))

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_FIXSTR + n, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                bytes(1:1) = char(ior(MSGPACK_FIXSTR, n))
                if (n > 0) bytes(2:) = value(1:n)
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_fixstr

    pure subroutine dm_msgpack_pack_float32(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        real(r4),                  intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: bits, rc
        integer(i8) :: pos(2)

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_FLOAT32, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                bits = transfer(value, bits)

                bytes(1:1) = char(MSGPACK_FLOAT32                   )
                bytes(2:2) = char(iand(shiftr(bits, 24), int(z'FF')))
                bytes(3:3) = char(iand(shiftr(bits, 16), int(z'FF')))
                bytes(4:4) = char(iand(shiftr(bits,  8), int(z'FF')))
                bytes(5:5) = char(iand(bits,             int(z'FF')))
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_float32

    pure subroutine dm_msgpack_pack_float64(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        real(r8),                  intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: rc
        integer(i8) :: bits, pos(2)

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_FLOAT64, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
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
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_float64

    pure subroutine dm_msgpack_pack_int32(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer(i4),               intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: rc
        integer(i8) :: pos(2)

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_INT32, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                bytes(1:1) = char(MSGPACK_INT32                      )
                bytes(2:2) = char(iand(shiftr(value, 24), int(z'FF')))
                bytes(3:3) = char(iand(shiftr(value, 16), int(z'FF')))
                bytes(4:4) = char(iand(shiftr(value,  8), int(z'FF')))
                bytes(5:5) = char(iand(value,             int(z'FF')))
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_int32

    pure subroutine dm_msgpack_pack_int64(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer(i8),               intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: b, i, j, rc, s
        integer(i8) :: pos(2)

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_INT64, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                bytes(1:1) = char(MSGPACK_INT64)

                do i = 1, 8
                    s = 64 - (8 * i)
                    b = int(iand(shiftr(value, s), int(z'FF', i8)))
                    j = i + 1
                    bytes(j:j) = char(b)
                end do
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_int64

    pure subroutine dm_msgpack_pack_nil(packer, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: rc
        integer(i8) :: pos(2)

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_NIL, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                bytes(1:1) = char(MSGPACK_NIL)
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_nil

    pure subroutine dm_msgpack_pack_str8(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        character(*),              intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: n, rc
        integer(i8) :: pos(2)

        n = min(255, len(value))

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_STR8 + n, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                bytes(1:1) = char(MSGPACK_STR8)
                bytes(2:2) = char(iand(n, int(z'FF')))
                if (n > 0) bytes(3:) = value(1:n)
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_str8

    pure subroutine dm_msgpack_pack_str16(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        character(*),              intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: n, rc
        integer(i8) :: pos(2)

        n = min(65535, len(value))

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_STR16 + n, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                bytes(1:1) = char(MSGPACK_STR16)
                bytes(2:2) = char(iand(shiftr(n, 8), int(z'FF')))
                bytes(3:3) = char(iand(n,            int(z'FF')))
                if (n > 0) bytes(4:) = value(1:n)
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_str16

    pure subroutine dm_msgpack_pack_str32(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        character(*),              intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: n, rc
        integer(i8) :: pos(2)

        n = len(value)

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            call dm_msgpack_next(packer, MSGPACK_SIZE_STR32 + n, pos, rc)
            if (rc /= E_NONE) exit pack_block

            associate (bytes => packer%buffer%ptr(pos(1):pos(2)))
                bytes(1:1) = char(MSGPACK_STR32)
                bytes(2:2) = char(iand(shiftr(n, 24), int(z'FF')))
                bytes(3:3) = char(iand(shiftr(n, 16), int(z'FF')))
                bytes(4:4) = char(iand(shiftr(n,  8), int(z'FF')))
                bytes(5:5) = char(iand(n,             int(z'FF')))
                if (n > 0) bytes(6:) = value(1:n)
            end associate
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_pack_str32

    pure subroutine dm_msgpack_pack_string(packer, value, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        character(*),              intent(in)            :: value  !! Input value.
        integer,                   intent(out), optional :: error  !! Error code.

        select case (len(value))
            case (    0:     31); call dm_msgpack_pack_fixstr(packer, value, error)
            case (   32:    255); call dm_msgpack_pack_str8  (packer, value, error)
            case (  256:  65535); call dm_msgpack_pack_str16 (packer, value, error)
            case (65536:huge(0)); call dm_msgpack_pack_str32 (packer, value, error)
            case default;         call dm_present_set(error, E_BOUNDS)
        end select
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
        bytes => packer%buffer%ptr(1:packer%index)
    end function dm_msgpack_packer_result

    pure function dm_msgpack_packer_size(packer) result(size)
        !! Returns actual size of packed data in associated buffer as 8-byte
        !! integer [byte].
        type(msgpack_packer_type), intent(in) :: packer !! Packer.
        integer(i8)                           :: size   !! Packed data size.

        size = packer%index
    end function dm_msgpack_packer_size

    ! **************************************************************************
    ! MSGPACK UNPACK
    ! **************************************************************************
    pure subroutine dm_msgpack_unpack_destroy(unpack)
        type(msgpack_unpack_type), intent(inout) :: unpack !! Unpack type.

        unpack = msgpack_unpack_type()
    end subroutine dm_msgpack_unpack_destroy

    pure subroutine dm_msgpack_unpack_array(object, size, error)
        !! Unpacks header of `fixarray`, `array16` and `array32` and returns
        !! size in argument `size`.
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not an array.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        integer,                   intent(out)           :: size   !! Output value (array size).
        integer,                   intent(out), optional :: error  !! Error code.

        size = 0

        select case (object%type)
            case (MSGPACK_FIXARRAY); call dm_msgpack_unpack_fixarray(object, size, error)
            case (MSGPACK_ARRAY16);  call dm_msgpack_unpack_array16 (object, size, error)
            case (MSGPACK_ARRAY32);  call dm_msgpack_unpack_array32 (object, size, error)
            case default;            call dm_present_set(error, E_CORRUPT)
        end select
    end subroutine dm_msgpack_unpack_array

    pure subroutine dm_msgpack_unpack_array16(object, size, error)
        !! Unpacks header of `array16` and returns size in argument `size`.
        !!
        !! Format: 0xDC + 2-byte unsigned array size (big-endian).
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not an array.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        integer,                   intent(out)           :: size   !! Output value (array size).
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        size = 0

        unpack_block: block
            integer(i4) :: b(MSGPACK_SIZE_ARRAY16)

            rc = E_PLATFORM
            if (BIG_ENDIAN) exit unpack_block

            rc = E_NULL
            if (.not. associated(object%bytes)) exit unpack_block

            rc = E_BOUNDS
            if (len(object%bytes) < MSGPACK_SIZE_ARRAY16) exit unpack_block

            rc = E_CORRUPT
            b(1) = ichar(object%bytes(1:1))
            if (b(1) /= MSGPACK_ARRAY16) exit unpack_block

            rc = E_NONE
            b(2) = ichar(object%bytes(2:2))
            b(3) = ichar(object%bytes(3:3))
            size = ior(shiftl(b(2), 8), b(3))
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_unpack_array16

    pure subroutine dm_msgpack_unpack_array32(object, size, error)
        !! Unpacks header of `array32` and returns size in argument `size`.
        !!
        !! Format: 0xDD + 4-byte unsigned array size (big-endian).
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not an array.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        integer,                   intent(out)           :: size   !! Output value (array size).
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        size = 0

        unpack_block: block
            integer(i8) :: b(MSGPACK_SIZE_ARRAY32)

            rc = E_PLATFORM
            if (BIG_ENDIAN) exit unpack_block

            rc = E_NULL
            if (.not. associated(object%bytes)) exit unpack_block

            rc = E_BOUNDS
            if (len(object%bytes) < MSGPACK_SIZE_ARRAY32) exit unpack_block

            rc = E_CORRUPT
            b(1) = ichar(object%bytes(1:1))
            if (b(1) /= MSGPACK_ARRAY32) exit unpack_block

            rc = E_NONE
            b(2) = ichar(object%bytes(2:2))
            b(3) = ichar(object%bytes(3:3))
            b(4) = ichar(object%bytes(4:4))
            b(5) = ichar(object%bytes(5:5))
            size = int(ior(shiftl(b(2), 24), ior(shiftl(b(3), 16), ior(shiftl(b(4),  8), b(5)))))
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_unpack_array32

    pure subroutine dm_msgpack_unpack_bool(object, value, error)
        !! Unpacks MessagePack `bool` from byte buffer.
        !!
        !! Format: 0xC2 or 0xC3.
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not an array.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        logical,                   intent(out)           :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        value = .false.

        unpack_block: block
            integer(i4) :: b

            rc = E_PLATFORM
            if (BIG_ENDIAN) exit unpack_block

            rc = E_NULL
            if (.not. associated(object%bytes)) exit unpack_block

            rc = E_BOUNDS
            if (len(object%bytes) < MSGPACK_SIZE_BOOL) exit unpack_block

            rc = E_CORRUPT
            b = ichar(object%bytes(1:1))
            if (b /= MSGPACK_FALSE .and. b /= MSGPACK_TRUE) exit unpack_block

            rc = E_NONE
            if (b == MSGPACK_TRUE) value = .true.
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_unpack_bool

    pure subroutine dm_msgpack_unpack_fixarray(object, size, error)
        !! Unpacks header of `fixarray` and returns size in argument `size`.
        !!
        !! Format: 0x90 .. 0x9F.
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not an array.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        integer(i4),               intent(out)           :: size   !! Output value (array size).
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        size = 0

        unpack_block: block
            integer(i4) :: b

            rc = E_PLATFORM
            if (BIG_ENDIAN) exit unpack_block

            rc = E_NULL
            if (.not. associated(object%bytes)) exit unpack_block

            rc = E_BOUNDS
            if (len(object%bytes) < MSGPACK_SIZE_FIXARRAY) exit unpack_block

            rc = E_CORRUPT
            b = ichar(object%bytes(1:1))
            if (iand(b, int(z'F0')) /= MSGPACK_FIXARRAY) exit unpack_block

            rc = E_NONE
            size = iand(b, int(z'0F'))
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_unpack_fixarray

    pure subroutine dm_msgpack_unpack_float32(object, value, error)
        !! Unpacks MessagePack `float32` from byte buffer.
        !!
        !! Format: 0xCA + 4-byte real (big-endian).
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not an array.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        real(r4),                  intent(out)           :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        value = 0.0_r4

        unpack_block: block
            integer(i4) :: b(MSGPACK_SIZE_FLOAT32), bits

            rc = E_PLATFORM
            if (BIG_ENDIAN) exit unpack_block

            rc = E_NULL
            if (.not. associated(object%bytes)) exit unpack_block

            rc = E_BOUNDS
            if (len(object%bytes) < MSGPACK_SIZE_FLOAT32) exit unpack_block

            rc = E_CORRUPT
            b(1) = ichar(object%bytes(1:1))
            if (b(1) /= MSGPACK_FLOAT32) exit unpack_block

            rc = E_NONE
            b(2) = ichar(object%bytes(2:2))
            b(3) = ichar(object%bytes(3:3))
            b(4) = ichar(object%bytes(4:4))
            b(5) = ichar(object%bytes(5:5))

            bits = 0_i4
            bits = ior(bits, shiftl(iand(b(2), int(z'FF')), 24))
            bits = ior(bits, shiftl(iand(b(3), int(z'FF')), 16))
            bits = ior(bits, shiftl(iand(b(4), int(z'FF')),  8))
            bits = ior(bits,        iand(b(5), int(z'FF')))

            value = transfer(bits, value)
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_unpack_float32

    pure subroutine dm_msgpack_unpack_float64(object, value, error)
        !! Unpacks MessagePack `float32` from byte buffer.
        !!
        !! Format: 0xCB + 8-byte real (big-endian).
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not an array.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        real(r8),                  intent(out)           :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        value = 0.0_r8

        unpack_block: block
            integer(i4) :: b(MSGPACK_SIZE_FLOAT64)
            integer(i8) :: bits

            rc = E_PLATFORM
            if (BIG_ENDIAN) exit unpack_block

            rc = E_NULL
            if (.not. associated(object%bytes)) exit unpack_block

            rc = E_BOUNDS
            if (len(object%bytes) < MSGPACK_SIZE_FLOAT64) exit unpack_block

            rc = E_CORRUPT
            b(1) = ichar(object%bytes(1:1))
            if (b(1) /= MSGPACK_FLOAT64) exit unpack_block

            rc = E_NONE
            b(2) = ichar(object%bytes(2:2))
            b(3) = ichar(object%bytes(3:3))
            b(4) = ichar(object%bytes(4:4))
            b(5) = ichar(object%bytes(5:5))
            b(6) = ichar(object%bytes(6:6))
            b(7) = ichar(object%bytes(7:7))
            b(8) = ichar(object%bytes(8:8))
            b(9) = ichar(object%bytes(9:9))

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
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_unpack_float64

    pure subroutine dm_msgpack_unpack_int32(object, value, error)
        !! Unpacks MessagePack `int32` from byte buffer.
        !!
        !! Format: 0xD2 + 4-byte signed integer (big-endian).
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not an array.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        integer(i4),               intent(out)           :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        value = 0_i4

        unpack_block: block
            integer(i4) :: b(MSGPACK_SIZE_INT32)

            rc = E_PLATFORM
            if (BIG_ENDIAN) exit unpack_block

            rc = E_NULL
            if (.not. associated(object%bytes)) exit unpack_block

            rc = E_BOUNDS
            if (len(object%bytes) < MSGPACK_SIZE_INT32) exit unpack_block

            rc = E_CORRUPT
            b(1) = ichar(object%bytes(1:1))
            if (b(1) /= MSGPACK_INT32) exit unpack_block

            rc = E_NONE
            b(2) = ichar(object%bytes(2:2))
            b(3) = ichar(object%bytes(3:3))
            b(4) = ichar(object%bytes(4:4))
            b(5) = ichar(object%bytes(5:5))

            value = ior(shiftl(b(2), 24), ior(shiftl(b(3), 16), ior(shiftl(b(4), 8), b(5))))
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_unpack_int32

    pure subroutine dm_msgpack_unpack_int64(object, value, error)
        !! Unpacks MessagePack `int64` from byte buffer.
        !!
        !! Format: 0xD3 + 8-byte signed integer (big-endian).
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not an array.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        integer(i8),               intent(out)           :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        value = 0_i8

        unpack_block: block
            integer     :: b, i
            integer(i8) :: bits

            rc = E_PLATFORM
            if (BIG_ENDIAN) exit unpack_block

            rc = E_NULL
            if (.not. associated(object%bytes)) exit unpack_block

            rc = E_BOUNDS
            if (len(object%bytes) < MSGPACK_SIZE_INT64) exit unpack_block

            rc = E_CORRUPT
            b = ichar(object%bytes(1:1))
            if (b /= MSGPACK_INT64) exit unpack_block

            rc = E_NONE
            do i = 2, MSGPACK_SIZE_FLOAT64
                bits  = int(ichar(object%bytes(i:i)), i8)
                value = ior(shiftl(value, 8), bits)
            end do
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_unpack_int64

    pure subroutine dm_msgpack_unpack_nil(object, error)
        !! Unpacks MessagePack `nil` from byte buffer.
        !!
        !! Format: 0xC0.
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not an array.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        unpack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit unpack_block

            rc = E_NULL
            if (.not. associated(object%bytes)) exit unpack_block

            rc = E_BOUNDS
            if (len(object%bytes) < MSGPACK_SIZE_NIL) exit unpack_block

            rc = E_CORRUPT
            if (ichar(object%bytes(1:1)) /= MSGPACK_NIL) exit unpack_block

            rc = E_NONE
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine dm_msgpack_unpack_nil

    pure subroutine dm_msgpack_unpack_string(object, value, error)
        !! Unpacks MessagePack `fixstr`, `str8`, `str16`, or `str32` from byte
        !! buffer.
        !!
        !! Multi-byte lengths are decoded as big-endian
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_BOUNDS` if byte range is out of bounds.
        !! * `E_CORRUPT` if input is not a string.
        !! * `E_NULL` if bytes pointer is not associated.
        !! * `E_PLATFORM` if platform is big-endian.
        !!
        type(msgpack_object_type), intent(in)            :: object !! Type object.
        character(*),              intent(inout)         :: value  !! Output value.
        integer,                   intent(out), optional :: error  !! Error code.

        integer     :: rc
        integer(i8) :: pos

        unpack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit unpack_block

            rc = E_NULL
            if (.not. associated(object%bytes)) exit unpack_block

            rc = E_CORRUPT
            select case (object%type)
                case (MSGPACK_FIXSTR); pos = 1_i8 + MSGPACK_SIZE_FIXSTR
                case (MSGPACK_STR8);   pos = 1_i8 + MSGPACK_SIZE_STR8
                case (MSGPACK_STR16);  pos = 1_i8 + MSGPACK_SIZE_STR16
                case (MSGPACK_STR32);  pos = 1_i8 + MSGPACK_SIZE_STR32
                case default;          exit unpack_block
            end select

            rc = E_BOUNDS
            if (pos > len(object%bytes, i8)) exit unpack_block

            rc = E_NONE
            value = object%bytes(pos:)
        end block unpack_block

        if (rc /= E_NONE) value = ''
        call dm_present_set(error, rc)
    end subroutine dm_msgpack_unpack_string

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    pure function msgpack_array_object(bytes) result(object)
        character(*), intent(in)  :: bytes
        type(msgpack_object_type) :: object

        integer :: b

        if (len(bytes) == 0) return

        b = ichar(bytes(1:1))

        select case (b)
            case (MSGPACK_ARRAY16)
                if (len(bytes) < MSGPACK_SIZE_ARRAY16) return
                object = msgpack_object_type(MSGPACK_ARRAY16, MSGPACK_SIZE_ARRAY16)

            case (MSGPACK_ARRAY32)
                if (len(bytes) < MSGPACK_SIZE_ARRAY32) return
                object = msgpack_object_type(MSGPACK_ARRAY32, MSGPACK_SIZE_ARRAY32)

            case default
                if (iand(b, int(z'F0')) == MSGPACK_FIXARRAY) then
                    object = msgpack_object_type(MSGPACK_FIXARRAY, MSGPACK_SIZE_FIXARRAY)
                end if
        end select
    end function msgpack_array_object

    pure subroutine msgpack_buffer_init_allocatable(buffer, size, error)
        !! Initialises MessagePack buffer using an allocatable character string
        !! with given length `size`.
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if size is < 1.
        !!
        type(msgpack_buffer_type), target, intent(out)           :: buffer !! Buffer.
        integer(i8),                       intent(in)            :: size   !! Max. size of buffer [byte].
        integer,                           intent(out), optional :: error  !! Error code.

        integer :: rc, stat

        init_block: block
            rc = E_INVALID
            if (size < 1) exit init_block

            rc = E_ALLOC
            allocate (character(size) :: buffer%bytes, stat=stat)
            if (stat /= 0) exit init_block

            rc = E_NONE
            buffer%ptr => buffer%bytes
        end block init_block

        call dm_present_set(error, rc)
    end subroutine msgpack_buffer_init_allocatable

    pure subroutine msgpack_buffer_init_pointer(buffer, bytes, error)
        !! Initialises MessagePack buffer using given string `bytes`. The
        !! variable shall not go out of scope for the life-time of buffer type
        !! `buffer`!
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_NULL` if pointer association failed.
        !!
        type(msgpack_buffer_type), intent(out)           :: buffer !! Buffer.
        character(*), target,      intent(inout)         :: bytes  !! Bytes.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        rc = E_NULL
        buffer%ptr => bytes
        if (associated(buffer%ptr)) rc = E_NONE
        call dm_present_set(error, rc)
    end subroutine msgpack_buffer_init_pointer

    pure function msgpack_string_object(bytes) result(object)
        character(*), intent(in)  :: bytes
        type(msgpack_object_type) :: object

        integer :: b, k, n

        if (len(bytes) == 0) return

        b = ichar(bytes(1:1))

        if (iand(b, int(z'E0')) == MSGPACK_FIXSTR) then
            n = iand(b, int(z'1F'))
            k = MSGPACK_SIZE_FIXSTR + n
            object = msgpack_object_type(MSGPACK_FIXSTR, k, n)
            return
        end if

        select case (b)
            case (MSGPACK_STR8)
                if (len(bytes) < MSGPACK_SIZE_STR8) return
                n = ichar(bytes(2:2))
                k = MSGPACK_SIZE_STR8 + n
                object = msgpack_object_type(MSGPACK_STR8, k, n)

            case (MSGPACK_STR16)
                if (len(bytes) < MSGPACK_SIZE_STR16) return
                n = 0
                n = n + shiftl(ichar(bytes(2:2)), 8)
                n = n +        ichar(bytes(3:3))
                k = MSGPACK_SIZE_STR16 + n
                object = msgpack_object_type(MSGPACK_STR16, k, n)

            case (MSGPACK_STR32)
                if (len(bytes) < MSGPACK_SIZE_STR32) return
                n = 0
                n = n + shiftl(ichar(bytes(2:2)), 24)
                n = n + shiftl(ichar(bytes(3:3)), 16)
                n = n + shiftl(ichar(bytes(4:4)),  8)
                n = n +        ichar(bytes(5:5))
                k = MSGPACK_SIZE_STR32 + n
                object = msgpack_object_type(MSGPACK_STR32, k, n)
        end select
    end function msgpack_string_object

    ! **************************************************************************
    ! PRIVATE PACK SUBROUTINES
    ! **************************************************************************
    pure subroutine msgpack_pack_next(packer, size, pos, error)
        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        integer,                   intent(in)            :: size   !! Size of type [byte].
        integer(i8),               intent(out)           :: pos(2) !! Position of first and last byte.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        pos = 0

        pack_block: block
            rc = E_PLATFORM
            if (BIG_ENDIAN) exit pack_block

            rc = E_CORRUPT
            if (packer%size == 0) exit pack_block

            rc = E_BOUNDS
            if (packer%size < packer%index + size) exit pack_block

            rc = E_NONE
            pos(1) = packer%index + 1
            pos(2) = packer%index + size
            packer%index = pos(2)
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_pack_next

    pure subroutine msgpack_pack_type_beat(packer, beat, error)
        use :: dm_beat

        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        type(beat_type),           intent(in)            :: beat   !! Input beat.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        pack_block: block
            call dm_msgpack_pack_array(packer, MSGPACK_SIZE_BEAT, rc)
            if (rc /= E_NONE) exit pack_block

            call dm_msgpack_pack(packer, trim(beat%node_id),   rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(beat%address),   rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(beat%client),    rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(beat%time_sent), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(beat%time_recv), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, beat%error,           rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, beat%interval,        rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, beat%uptime,          rc); if (rc /= E_NONE) exit pack_block
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_pack_type_beat

    pure subroutine msgpack_pack_type_dp(packer, dp, error)
        use :: dm_dp

        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        type(dp_type),             intent(in)            :: dp     !! Input data point.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        pack_block: block
            call dm_msgpack_pack_array(packer, MSGPACK_SIZE_DP, rc)
            if (rc /= E_NONE) exit pack_block

            call dm_msgpack_pack(packer, dp%x, rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, dp%y, rc); if (rc /= E_NONE) exit pack_block
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_pack_type_dp

    pure subroutine msgpack_pack_type_log(packer, log, error)
        use :: dm_log

        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        type(log_type),            intent(in)            :: log    !! Input log.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        pack_block: block
            call dm_msgpack_pack_array(packer, MSGPACK_SIZE_LOG, rc)
            if (rc /= E_NONE) exit pack_block

            call dm_msgpack_pack(packer, trim(log%id),        rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, log%level,           rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, log%error,           rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(log%timestamp), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(log%node_id),   rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(log%sensor_id), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(log%target_id), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(log%observ_id), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(log%source),    rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(log%message),   rc); if (rc /= E_NONE) exit pack_block
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_pack_type_log

    pure subroutine msgpack_pack_type_node(packer, node, error)
        use :: dm_node

        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        type(node_type),           intent(in)            :: node   !! Input node.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        pack_block: block
            call dm_msgpack_pack_array(packer, MSGPACK_SIZE_NODE, rc)
            if (rc /= E_NONE) exit pack_block

            call dm_msgpack_pack(packer, trim(node%id),   rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(node%name), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(node%meta), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, node%x,          rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, node%y,          rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, node%z,          rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, node%longitude,  rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, node%latitude,   rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, node%elevation,  rc); if (rc /= E_NONE) exit pack_block
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_pack_type_node

    pure subroutine msgpack_pack_type_observ(packer, observ, error)
        use :: dm_observ

        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        type(observ_type),         intent(in)            :: observ !! Input observation.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: i, rc

        pack_block: block
            call dm_msgpack_pack_array(packer, MSGPACK_SIZE_OBSERV, rc)
            if (rc /= E_NONE) exit pack_block

            call dm_msgpack_pack(packer, trim(observ%id),        rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%group_id),  rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%node_id),   rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%sensor_id), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%target_id), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%timestamp), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%name),      rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%source),    rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%device),    rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%request),   rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%response),  rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%delimiter), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(observ%pattern),   rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, observ%delay,           rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, observ%error,           rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, observ%mode,            rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, observ%retries,         rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, observ%state,           rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, observ%timeout,         rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, observ%nresponses,      rc); if (rc /= E_NONE) exit pack_block

            call dm_msgpack_pack_array(packer, observ%nresponses, rc)
            if (rc /= E_NONE) exit pack_block

            do i = 1, min(observ%nresponses, OBSERV_MAX_NRESPONSES)
                associate (response => observ%responses(i))
                    call dm_msgpack_pack(packer, response%name,  rc); if (rc /= E_NONE) exit pack_block
                    call dm_msgpack_pack(packer, response%unit,  rc); if (rc /= E_NONE) exit pack_block
                    call dm_msgpack_pack(packer, response%type,  rc); if (rc /= E_NONE) exit pack_block
                    call dm_msgpack_pack(packer, response%error, rc); if (rc /= E_NONE) exit pack_block
                    call dm_msgpack_pack(packer, response%value, rc); if (rc /= E_NONE) exit pack_block
                end associate
            end do
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_pack_type_observ

    pure subroutine msgpack_pack_type_sensor(packer, sensor, error)
        use :: dm_sensor

        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        type(sensor_type),         intent(in)            :: sensor !! Input sensor.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        pack_block: block
            call dm_msgpack_pack_array(packer, MSGPACK_SIZE_SENSOR, rc)
            if (rc /= E_NONE) exit pack_block

            call dm_msgpack_pack(packer, trim(sensor%id),      rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(sensor%node_id), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(sensor%name),    rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(sensor%sn),      rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(sensor%meta),    rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, sensor%type,          rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, sensor%x,             rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, sensor%y,             rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, sensor%z,             rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, sensor%longitude,     rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, sensor%latitude,      rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, sensor%elevation,     rc); if (rc /= E_NONE) exit pack_block
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_pack_type_sensor

    pure subroutine msgpack_pack_type_target(packer, target, error)
        use :: dm_target

        type(msgpack_packer_type), intent(inout)         :: packer !! Packer.
        type(target_type),         intent(in)            :: target !! Input target.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        pack_block: block
            call dm_msgpack_pack_array(packer, MSGPACK_SIZE_TARGET, rc)
            if (rc /= E_NONE) exit pack_block

            call dm_msgpack_pack(packer, trim(target%id),   rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(target%name), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, trim(target%meta), rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, target%state,      rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, target%x,          rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, target%y,          rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, target%z,          rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, target%longitude,  rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, target%latitude,   rc); if (rc /= E_NONE) exit pack_block
            call dm_msgpack_pack(packer, target%elevation,  rc); if (rc /= E_NONE) exit pack_block
        end block pack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_pack_type_target

    ! **************************************************************************
    ! PRIVATE UNPACK SUBROUTINES
    ! **************************************************************************
    pure subroutine msgpack_unpack_next(unpack, buffer, error)
        type(msgpack_unpack_type),         intent(inout) :: unpack !! Unpack context.
        type(msgpack_buffer_type), target, intent(inout) :: buffer !! Input buffer.
        integer,                           intent(out)   :: error  !! Error code.

        integer(i4) :: b, rc
        integer(i8) :: i, j, n

        unpack_block: block
            associate (bytes => buffer%ptr, index => unpack%index, object => unpack%object)
                i = index + 1
                n = dm_msgpack_buffer_size(buffer)

                rc = E_BOUNDS
                if (i > n) return

                b = ichar(bytes(i:i))

                object_select: select case (b)
                    case (MSGPACK_NIL);     object = msgpack_object_type(MSGPACK_NIL,     MSGPACK_SIZE_NIL)
                    case (MSGPACK_FALSE);   object = msgpack_object_type(MSGPACK_FALSE,   MSGPACK_SIZE_BOOL)
                    case (MSGPACK_TRUE);    object = msgpack_object_type(MSGPACK_TRUE,    MSGPACK_SIZE_BOOL)
                    case (MSGPACK_FLOAT32); object = msgpack_object_type(MSGPACK_FLOAT32, MSGPACK_SIZE_FLOAT32)
                    case (MSGPACK_FLOAT64); object = msgpack_object_type(MSGPACK_FLOAT64, MSGPACK_SIZE_FLOAT64)
                    case (MSGPACK_INT32);   object = msgpack_object_type(MSGPACK_INT32,   MSGPACK_SIZE_INT32)
                    case (MSGPACK_INT64);   object = msgpack_object_type(MSGPACK_INT64,   MSGPACK_SIZE_INT64)

                    case (MSGPACK_STR8);    object = msgpack_string_object(bytes(i:)) ! Add string length.
                    case (MSGPACK_STR16);   object = msgpack_string_object(bytes(i:)) ! Add string length.
                    case (MSGPACK_STR32);   object = msgpack_string_object(bytes(i:)) ! Add string length.

                    case (MSGPACK_ARRAY16); object = msgpack_array_object(bytes(i:))  ! Add array size.
                    case (MSGPACK_ARRAY32); object = msgpack_array_object(bytes(i:))  ! Add array size.

                    case default
                        if (iand(b, int(z'E0')) == MSGPACK_FIXSTR) then
                            ! Add string length.
                            object = msgpack_string_object(bytes(i:))
                            exit object_select
                        end if

                        if (iand(b, int(z'F0')) == MSGPACK_FIXARRAY) then
                            ! Add array size.
                            object = msgpack_array_object(bytes(i:))
                            exit object_select
                        end if

                        rc = E_NOT_SUPPORTED
                        exit unpack_block
                end select object_select

                rc = E_BOUNDS
                j  = index + object%nbytes
                if (n < j) exit unpack_block

                rc = E_NONE
                index = j
                object%bytes => bytes(i:j)
            end associate
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_unpack_next

    pure subroutine msgpack_unpack_type_beat(unpack, buffer, beat, error)
        use :: dm_beat

        type(msgpack_unpack_type), intent(inout)         :: unpack !! Unpack context.
        type(msgpack_buffer_type), intent(inout)         :: buffer !! Buffer.
        type(beat_type),           intent(out)           :: beat   !! Output beat.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc, size

        unpack_block: block
            associate (object => unpack%object)
                call dm_msgpack_next        (unpack, buffer, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack_array(object, size,   rc); if (rc /= E_NONE) exit unpack_block

                rc = E_CORRUPT
                if (size /= MSGPACK_SIZE_BEAT) exit unpack_block

                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, beat%node_id,   rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, beat%address,   rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, beat%client,    rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, beat%time_sent, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, beat%time_recv, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, beat%error,     rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, beat%interval,  rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, beat%uptime,    rc); if (rc /= E_NONE) exit unpack_block
            end associate
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_unpack_type_beat

    pure subroutine msgpack_unpack_type_dp(unpack, buffer, dp, error)
        use :: dm_dp

        type(msgpack_unpack_type), intent(inout)         :: unpack !! Unpack context.
        type(msgpack_buffer_type), intent(inout)         :: buffer !! Buffer.
        type(dp_type),             intent(out)           :: dp     !! Output data point.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc, size

        unpack_block: block
            associate (object => unpack%object)
                call dm_msgpack_next        (unpack, buffer, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack_array(object, size,   rc); if (rc /= E_NONE) exit unpack_block

                rc = E_CORRUPT
                if (size /= MSGPACK_SIZE_DP) exit unpack_block

                call dm_msgpack_next  (unpack, buffer, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, dp%x,   rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, dp%y,   rc); if (rc /= E_NONE) exit unpack_block
            end associate
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_unpack_type_dp

    pure subroutine msgpack_unpack_type_log(unpack, buffer, log, error)
        use :: dm_log

        type(msgpack_unpack_type), intent(inout)         :: unpack !! Unpack context.
        type(msgpack_buffer_type), intent(inout)         :: buffer !! Buffer.
        type(log_type),            intent(out)           :: log    !! Output log.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc, size

        unpack_block: block
            associate (object => unpack%object)
                call dm_msgpack_next        (unpack, buffer, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack_array(object, size,   rc); if (rc /= E_NONE) exit unpack_block

                rc = E_CORRUPT
                if (size /= MSGPACK_SIZE_LOG) exit unpack_block

                call dm_msgpack_next  (unpack, buffer,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, log%id,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, log%level,     rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, log%error,     rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, log%timestamp, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, log%node_id,   rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, log%sensor_id, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, log%target_id, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, log%observ_id, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, log%source,    rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, log%message,   rc); if (rc /= E_NONE) exit unpack_block
            end associate
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_unpack_type_log

    pure subroutine msgpack_unpack_type_node(unpack, buffer, node, error)
        use :: dm_node

        type(msgpack_unpack_type), intent(inout)         :: unpack !! Unpack context.
        type(msgpack_buffer_type), intent(inout)         :: buffer !! Buffer.
        type(node_type),           intent(out)           :: node   !! Output node.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc, size

        unpack_block: block
            associate (object => unpack%object)
                call dm_msgpack_next        (unpack, buffer, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack_array(object, size,   rc); if (rc /= E_NONE) exit unpack_block

                rc = E_CORRUPT
                if (size /= MSGPACK_SIZE_NODE) exit unpack_block

                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, node%id,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, node%name,      rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, node%meta,      rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, node%x,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, node%y,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, node%z,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, node%longitude, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, node%latitude,  rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, node%elevation, rc); if (rc /= E_NONE) exit unpack_block
            end associate
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_unpack_type_node

    pure subroutine msgpack_unpack_type_observ(unpack, buffer, observ, error)
        use :: dm_observ

        type(msgpack_unpack_type), intent(inout)         :: unpack !! Unpack context.
        type(msgpack_buffer_type), intent(inout)         :: buffer !! Buffer.
        type(observ_type),         intent(out)           :: observ !! Output observation.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: i, rc, size

        unpack_block: block
            associate (object => unpack%object)
                call dm_msgpack_next        (unpack, buffer, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack_array(object, size,   rc); if (rc /= E_NONE) exit unpack_block

                rc = E_CORRUPT
                if (size /= MSGPACK_SIZE_OBSERV) exit unpack_block

                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%id,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%group_id,   rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%node_id,    rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%sensor_id,  rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%target_id,  rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%timestamp,  rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%name,       rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%source,     rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%device,     rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%request,    rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%response,   rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%delimiter,  rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%pattern,    rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%delay,      rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%error,      rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%mode,       rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%retries,    rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%state,      rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%timeout,    rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,            rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, observ%nresponses, rc); if (rc /= E_NONE) exit unpack_block

                call dm_msgpack_next        (unpack, buffer, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack_array(object, size,   rc); if (rc /= E_NONE) exit unpack_block

                if (size /= observ%nresponses) return

                do i = 1, min(size, OBSERV_MAX_NRESPONSES)
                    associate (response => observ%responses(i))
                        call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                        call dm_msgpack_unpack(object, response%name,  rc); if (rc /= E_NONE) exit unpack_block
                        call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                        call dm_msgpack_unpack(object, response%unit,  rc); if (rc /= E_NONE) exit unpack_block
                        call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                        call dm_msgpack_unpack(object, response%type,  rc); if (rc /= E_NONE) exit unpack_block
                        call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                        call dm_msgpack_unpack(object, response%error, rc); if (rc /= E_NONE) exit unpack_block
                        call dm_msgpack_next  (unpack, buffer,         rc); if (rc /= E_NONE) exit unpack_block
                        call dm_msgpack_unpack(object, response%value, rc); if (rc /= E_NONE) exit unpack_block
                    end associate
                end do
            end associate
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_unpack_type_observ

    pure subroutine msgpack_unpack_type_sensor(unpack, buffer, sensor, error)
        use :: dm_sensor

        type(msgpack_unpack_type), intent(inout)         :: unpack !! Unpack context.
        type(msgpack_buffer_type), intent(inout)         :: buffer !! Buffer.
        type(sensor_type),         intent(out)           :: sensor !! Output sensor.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc, size

        unpack_block: block
            associate (object => unpack%object)
                call dm_msgpack_next        (unpack, buffer, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack_array(object, size,   rc); if (rc /= E_NONE) exit unpack_block

                rc = E_CORRUPT
                if (size /= MSGPACK_SIZE_SENSOR) exit unpack_block

                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%id,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%node_id,   rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%name,      rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%sn,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%meta,      rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%type,      rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%x,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%y,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%z,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%longitude, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%latitude,  rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, sensor%elevation, rc); if (rc /= E_NONE) exit unpack_block
            end associate
        end block unpack_block

        call dm_present_set(error, rc)
    end subroutine msgpack_unpack_type_sensor

    pure subroutine msgpack_unpack_type_target(unpack, buffer, target, error)
        use :: dm_target

        type(msgpack_unpack_type), intent(inout)         :: unpack !! Unpack context.
        type(msgpack_buffer_type), intent(inout)         :: buffer !! Buffer.
        type(target_type),         intent(out)           :: target !! Output target.
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc, size

        unpack_block: block
            associate (object => unpack%object)
                call dm_msgpack_next        (unpack, buffer, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack_array(object, size,   rc); if (rc /= E_NONE) exit unpack_block

                rc = E_CORRUPT
                if (size /= MSGPACK_SIZE_TARGET) exit unpack_block

                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, target%id,        rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, target%name,      rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, target%meta,      rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, target%state,     rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, target%x,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, target%y,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, target%z,         rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, target%longitude, rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, target%latitude,  rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_next  (unpack, buffer,           rc); if (rc /= E_NONE) exit unpack_block
                call dm_msgpack_unpack(object, target%elevation, rc); if (rc /= E_NONE) exit unpack_block
            end associate
        end block unpack_block
        call dm_present_set(error, rc)
    end subroutine msgpack_unpack_type_target
end module dm_msgpack
