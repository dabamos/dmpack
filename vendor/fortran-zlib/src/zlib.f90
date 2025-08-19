! zlib.f90
!
! Fortran 2018 interface bindings to zlib.
!
! Author:  Philipp Engel
! Licence: ISC
module zlib
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    integer, parameter, public :: z_byte   = c_char
    integer, parameter, public :: z_size_t = c_size_t
    integer, parameter, public :: z_uint   = c_int
    integer, parameter, public :: z_ulong  = c_long

    integer(kind=c_int), parameter, public :: Z_NO_FLUSH      = 0
    integer(kind=c_int), parameter, public :: Z_PARTIAL_FLUSH = 1
    integer(kind=c_int), parameter, public :: Z_SYNC_FLUSH    = 2
    integer(kind=c_int), parameter, public :: Z_FULL_FLUSH    = 3
    integer(kind=c_int), parameter, public :: Z_FINISH        = 4
    integer(kind=c_int), parameter, public :: Z_BLOCK         = 5
    integer(kind=c_int), parameter, public :: Z_TREES         = 6

    integer(kind=c_int), parameter, public :: Z_OK            =  0
    integer(kind=c_int), parameter, public :: Z_STREAM_END    =  1
    integer(kind=c_int), parameter, public :: Z_NEED_DICT     =  2
    integer(kind=c_int), parameter, public :: Z_ERRNO         = -1
    integer(kind=c_int), parameter, public :: Z_STREAM_ERROR  = -2
    integer(kind=c_int), parameter, public :: Z_DATA_ERROR    = -3
    integer(kind=c_int), parameter, public :: Z_MEM_ERROR     = -4
    integer(kind=c_int), parameter, public :: Z_BUF_ERROR     = -5
    integer(kind=c_int), parameter, public :: Z_VERSION_ERROR = -6

    integer(kind=c_int), parameter, public :: Z_NO_COMPRESSION      =  0
    integer(kind=c_int), parameter, public :: Z_BEST_SPEED          =  1
    integer(kind=c_int), parameter, public :: Z_BEST_COMPRESSION    =  9
    integer(kind=c_int), parameter, public :: Z_DEFAULT_COMPRESSION = -1

    integer(kind=c_int), parameter, public :: Z_FILTERED         = 1
    integer(kind=c_int), parameter, public :: Z_HUFFMAN_ONLY     = 2
    integer(kind=c_int), parameter, public :: Z_RLE              = 3
    integer(kind=c_int), parameter, public :: Z_FIXED            = 4
    integer(kind=c_int), parameter, public :: Z_DEFAULT_STRATEGY = 0

    integer(kind=c_int), parameter, public :: Z_BINARY  = 0
    integer(kind=c_int), parameter, public :: Z_TEXT    = 1
    integer(kind=c_int), parameter, public :: Z_ASCII   = Z_TEXT
    integer(kind=c_int), parameter, public :: Z_UNKNOWN = 2

    integer(kind=c_int), parameter, public :: Z_DEFLATED = 8

    type, bind(c), public :: z_stream_type
        type(c_ptr)           :: next_in   = c_null_ptr
        integer(kind=z_uint)  :: avail_in  = 0
        integer(kind=z_ulong) :: total_in  = 0
        type(c_ptr)           :: next_out  = c_null_ptr
        integer(kind=z_uint)  :: avail_out = 0
        integer(kind=z_ulong) :: total_out = 0
        type(c_ptr)           :: msg       = c_null_ptr
        type(c_ptr)           :: state     = c_null_ptr
        type(c_funptr)        :: zalloc    = c_null_funptr
        type(c_funptr)        :: zfree     = c_null_funptr
        type(c_ptr)           :: opaque    = c_null_ptr
        integer(kind=c_int)   :: data_type = 0
        integer(kind=z_ulong) :: adler     = 0
        integer(kind=z_ulong) :: reserved  = 0
    end type z_stream_type

    public :: adler32
    public :: adler32_z
    public :: compress
    public :: compress2
    public :: compress_bound
    public :: crc32
    public :: crc32_z
    public :: deflate
    public :: deflate_end
    public :: deflate_init
    public :: deflate_init2
    public :: inflate
    public :: inflate_end
    public :: inflate_init
    public :: inflate_init2
    public :: uncompress
    public :: uncompress2

    interface
        ! uLong adler32(uLong adler, const Bytef *buf, uInt len)
        function adler32(adler, buf, len) bind(c, name='adler32')
            import :: z_byte, z_uint, z_ulong
            implicit none
            integer(kind=z_ulong),  intent(in), value :: adler
            character(kind=z_byte), intent(in)        :: buf
            integer(kind=z_uint),   intent(in), value :: len
            integer(kind=z_ulong)                     :: adler32
        end function adler32

        ! uLong adler32_z(uLong adler, const Bytef *buf, z_size_t len)
        function adler32_z(adler, buf, len) bind(c, name='adler32_z')
            import :: z_byte, z_size_t, z_ulong
            implicit none
            integer(kind=z_ulong),  intent(in), value :: adler
            character(kind=z_byte), intent(in)        :: buf
            integer(kind=z_size_t), intent(in), value :: len
            integer(kind=z_ulong)                     :: adler32_z
        end function adler32_z

        ! int compress(Bytef *dest, uLongf *destLen, const Bytef *source, uLong sourceLen)
        function compress(dest, dest_len, source, source_len) bind(c, name='compress')
            import :: c_int, z_byte, z_ulong
            implicit none
            character(kind=z_byte), intent(inout)     :: dest
            integer(kind=z_ulong),  intent(inout)     :: dest_len
            character(kind=z_byte), intent(in)        :: source
            integer(kind=z_ulong),  intent(in), value :: source_len
            integer(kind=c_int)                       :: compress
        end function compress

        ! int compress2(Bytef *dest, uLongf *destLen, const Bytef *source, uLong sourceLen, int level)
        function compress2(dest, dest_len, source, source_len, level) bind(c, name='compress2')
            import :: c_int, z_byte, z_ulong
            implicit none
            character(kind=z_byte), intent(inout)     :: dest
            integer(kind=z_ulong),  intent(inout)     :: dest_len
            character(kind=z_byte), intent(in)        :: source
            integer(kind=z_ulong),  intent(in), value :: source_len
            integer(kind=c_int),    intent(in), value :: level
            integer(kind=c_int)                       :: compress2
        end function compress2

        ! uLong compressBound(uLong sourceLen)
        function compress_bound(source_len) bind(c, name='compressBound')
            import :: z_ulong
            implicit none
            integer(kind=z_ulong), intent(in), value :: source_len
            integer(kind=z_ulong)                    :: compress_bound
        end function compress_bound

        ! uLong crc32(uLong adler, const Bytef *buf, uInt len)
        function crc32(adler, buf, len) bind(c, name='crc32')
            import :: z_byte, z_uint, z_ulong
            implicit none
            integer(kind=z_ulong),  intent(in), value :: adler
            character(kind=z_byte), intent(in)        :: buf
            integer(kind=z_uint),   intent(in), value :: len
            integer(kind=z_ulong)                     :: crc32
        end function crc32

        ! uLong crc32_z(uLong adler, const Bytef *buf, z_size_t len)
        function crc32_z(adler, buf, len) bind(c, name='crc32_z')
            import :: z_byte, z_size_t, z_ulong
            implicit none
            integer(kind=z_ulong),  intent(in), value :: adler
            character(kind=z_byte), intent(in)        :: buf
            integer(kind=z_size_t), intent(in), value :: len
            integer(kind=z_ulong)                     :: crc32_z
        end function crc32_z

        ! int deflate(z_streamp strm, int flush)
        function deflate(strm, flush) bind(c, name='deflate')
            import :: c_int, z_stream_type
            implicit none
            type(z_stream_type), intent(inout)     :: strm
            integer(kind=c_int), intent(in), value :: flush
            integer(kind=c_int)                    :: deflate
        end function deflate

        ! int deflateEnd(z_streamp strm)
        function deflate_end(strm) bind(c, name='deflateEnd')
            import :: c_int, z_stream_type
            implicit none
            type(z_stream_type), intent(inout) :: strm
            integer(kind=c_int)                :: deflate_end
        end function deflate_end

        ! int deflateInit_(z_streamp strm, int level, const char *version, int stream_size)
        function deflate_init_(strm, level, version, stream_size) bind(c, name='deflateInit_')
            import :: c_int, c_ptr, z_stream_type
            implicit none
            type(z_stream_type), intent(inout)     :: strm
            integer(kind=c_int), intent(in), value :: level
            type(c_ptr),         intent(in), value :: version
            integer(kind=c_int), intent(in), value :: stream_size
            integer(kind=c_int)                    :: deflate_init_
        end function deflate_init_

        ! int deflateInit2_(z_streamp strm, int  level, int method, int windowBits, int memLevel,
        !                   int strategy, const char *version, int stream_size)
        function deflate_init2_(strm, level, method, window_bits, mem_level, strategy, &
                version, stream_size) bind(c, name='deflateInit2_')
            import :: c_int, c_ptr, z_stream_type
            implicit none
            type(z_stream_type), intent(inout)     :: strm
            integer(kind=c_int), intent(in), value :: level
            integer(kind=c_int), intent(in), value :: method
            integer(kind=c_int), intent(in), value :: window_bits
            integer(kind=c_int), intent(in), value :: mem_level
            integer(kind=c_int), intent(in), value :: strategy
            type(c_ptr),         intent(in), value :: version
            integer(kind=c_int), intent(in), value :: stream_size
            integer(kind=c_int)                    :: deflate_init2_
        end function deflate_init2_

        ! int inflate(z_streamp strm, int flush)
        function inflate(strm, flush) bind(c, name='inflate')
            import :: c_int, z_stream_type
            implicit none
            type(z_stream_type), intent(inout)     :: strm
            integer(kind=c_int), intent(in), value :: flush
            integer(kind=c_int)                    :: inflate
        end function inflate

        ! int inflateEnd(z_streamp strm)
        function inflate_end(strm) bind(c, name='inflateEnd')
            import :: c_int, z_stream_type
            implicit none
            type(z_stream_type), intent(inout) :: strm
            integer(kind=c_int)                :: inflate_end
        end function inflate_end

        ! int inflateInit_(z_streamp strm, const char *version, int stream_size)
        function inflate_init_(strm, version, stream_size) bind(c, name='inflateInit_')
            import :: c_int, c_ptr, z_stream_type
            implicit none
            type(z_stream_type), intent(inout)     :: strm
            type(c_ptr),         intent(in), value :: version
            integer(kind=c_int), intent(in), value :: stream_size
            integer(kind=c_int)                    :: inflate_init_
        end function inflate_init_

        ! int inflateInit2_(z_streamp strm, int  windowBits, const char *version, int stream_size)
        function inflate_init2_(strm, window_bits, version, stream_size) bind(c, name='inflateInit2_')
            import :: c_int, c_ptr, z_stream_type
            implicit none
            type(z_stream_type), intent(inout)     :: strm
            integer(kind=c_int), intent(in), value :: window_bits
            type(c_ptr),         intent(in), value :: version
            integer(kind=c_int), intent(in), value :: stream_size
            integer(kind=c_int)                    :: inflate_init2_
        end function inflate_init2_

        ! int uncompress(Bytef *dest, uLongf *destLen, const Bytef *source, uLong sourceLen)
        function uncompress(dest, dest_len, source, source_len) bind(c, name='uncompress')
            import :: c_int, z_byte, z_ulong
            implicit none
            character(kind=z_byte), intent(inout)     :: dest
            integer(kind=z_ulong),  intent(inout)     :: dest_len
            character(kind=z_byte), intent(in)        :: source
            integer(kind=z_ulong),  intent(in), value :: source_len
            integer(kind=c_int)                       :: uncompress
        end function uncompress

        ! int uncompress2(Bytef *dest, uLongf *destLen, const Bytef *source, uLong *sourceLen)
        function uncompress2(dest, dest_len, source, source_len) bind(c, name='uncompress2')
            import :: c_int, z_byte, z_ulong
            implicit none
            character(kind=z_byte), intent(inout) :: dest
            integer(kind=z_ulong),  intent(inout) :: dest_len
            character(kind=z_byte), intent(in)    :: source
            integer(kind=z_ulong),  intent(inout) :: source_len
            integer(kind=c_int)                   :: uncompress2
        end function uncompress2

        function zlib_version_() bind(c, name='zlibVersion')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zlib_version_
        end function zlib_version_
    end interface
contains
    ! int deflateInit(z_streamp strm, int level)
    integer function deflate_init(strm, level) result(rc)
        type(z_stream_type), intent(inout) :: strm
        integer,             intent(in)    :: level

        rc = deflate_init_(strm, level, zlib_version_(), int(c_sizeof(strm), kind=c_int))
    end function deflate_init

    ! int deflateInit2(z_streamp strm, int level, int method, int windowBits, int memLevel, int strategy)
    integer function deflate_init2(strm, level, method, window_bits, mem_level, strategy) result(rc)
        type(z_stream_type), intent(inout) :: strm
        integer,             intent(in)    :: level
        integer,             intent(in)    :: method
        integer,             intent(in)    :: window_bits
        integer,             intent(in)    :: mem_level
        integer,             intent(in)    :: strategy

        rc = deflate_init2_(strm, level, method, window_bits, mem_level, &
                            strategy, zlib_version_(), int(c_sizeof(strm), kind=c_int))
    end function deflate_init2

    ! int inflateInit(z_streamp strm)
    integer function inflate_init(strm) result(rc)
        type(z_stream_type), intent(inout) :: strm

        rc = inflate_init_(strm, zlib_version_(), int(c_sizeof(strm), kind=c_int))
    end function inflate_init

    ! int inflateInit2(z_streamp strm, int  windowBits)
    integer function inflate_init2(strm, window_bits) result(rc)
        type(z_stream_type), intent(inout) :: strm
        integer,             intent(in)    :: window_bits

        rc = inflate_init2_(strm, window_bits, zlib_version_(), int(c_sizeof(strm), kind=c_int))
    end function inflate_init2
end module zlib
