! zstd.F90
!
! Author:  Philipp Engel
! Licence: ISC
module zstd
    !! Fortran 2018 interface bindings to Zstandard (zstd).
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

#if defined (__flang__) || (defined (__GFORTRAN__) && __GNUC__ > 15) || (defined (__GFORTRAN__) && __GNUC__ == 15 && __GNUC_MINOR__ >= 2)

    public :: c_unsigned
    public :: c_unsigned_long_long

#else

    integer, parameter, public :: c_unsigned           = c_int
    integer, parameter, public :: c_unsigned_long_long = c_long_long

#endif

    integer(kind=c_int),    parameter, public :: ZSTD_BLOCKSIZELOG_MAX = 17
    integer(kind=c_size_t), parameter, public :: ZSTD_BLOCKSIZE_MAX    = shiftl(1, ZSTD_BLOCKSIZELOG_MAX)

    integer(kind=c_unsigned_long_long), parameter, public :: ZSTD_CONTENTSIZE_UNKNOWN = -1
    integer(kind=c_unsigned_long_long), parameter, public :: ZSTD_CONTENTSIZE_ERROR   = -2

    ! ZSTD_strategy
    integer(kind=c_int), parameter, public :: ZSTD_FAST     = 1
    integer(kind=c_int), parameter, public :: ZSTD_DFAST    = 2
    integer(kind=c_int), parameter, public :: ZSTD_GREEDY   = 3
    integer(kind=c_int), parameter, public :: ZSTD_LAZY     = 4
    integer(kind=c_int), parameter, public :: ZSTD_LAZY2    = 5
    integer(kind=c_int), parameter, public :: ZSTD_BTLAZY2  = 6
    integer(kind=c_int), parameter, public :: ZSTD_BTOPT    = 7
    integer(kind=c_int), parameter, public :: ZSTD_BTULTRA  = 8
    integer(kind=c_int), parameter, public :: ZSTD_BTULTRA2 = 9

    ! ZSTD_cParameter
    integer(kind=c_int), parameter, public :: ZSTD_C_COMPRESSIONLEVEL           = 100
    integer(kind=c_int), parameter, public :: ZSTD_C_WINDOWLOG                  = 101
    integer(kind=c_int), parameter, public :: ZSTD_C_HASHLOG                    = 102
    integer(kind=c_int), parameter, public :: ZSTD_C_SEARCHLOG                  = 104
    integer(kind=c_int), parameter, public :: ZSTD_C_MINMATCH                   = 105
    integer(kind=c_int), parameter, public :: ZSTD_C_TARGETLENGTH               = 106
    integer(kind=c_int), parameter, public :: ZSTD_C_STRATEGY                   = 107
    integer(kind=c_int), parameter, public :: ZSTD_C_TARGETCBLOCKSIZE           = 130
    integer(kind=c_int), parameter, public :: ZSTD_C_ENABLELONGDISTANCEMATCHING = 160
    integer(kind=c_int), parameter, public :: ZSTD_C_LDMHASHLOG                 = 161
    integer(kind=c_int), parameter, public :: ZSTD_C_LDMMINMATCH                = 162
    integer(kind=c_int), parameter, public :: ZSTD_C_LDMBUCKETSIZELOG           = 163
    integer(kind=c_int), parameter, public :: ZSTD_C_LDMHASHRATELOG             = 164
    integer(kind=c_int), parameter, public :: ZSTD_C_CONTENTSIZEFLAG            = 200
    integer(kind=c_int), parameter, public :: ZSTD_C_CHECKSUMFLAG               = 201
    integer(kind=c_int), parameter, public :: ZSTD_C_DICTIDFLAG                 = 202
    integer(kind=c_int), parameter, public :: ZSTD_C_NBWORKERS                  = 400
    integer(kind=c_int), parameter, public :: ZSTD_C_JOBSIZE                    = 401
    integer(kind=c_int), parameter, public :: ZSTD_C_OVERLAPLOG                 = 402

    ! ZSTD_dParameter
    integer(kind=c_int), parameter, public :: ZSTD_D_WINDOWLOGMAX = 100

    ! ZSTD_EndDirective
    integer(kind=c_int), parameter, public :: ZSTD_E_CONTINUE = 0
    integer(kind=c_int), parameter, public :: ZSTD_E_FLUSH    = 1
    integer(kind=c_int), parameter, public :: ZSTD_E_END      = 2

    ! ZSTD_ResetDirective
    integer(kind=c_int), parameter, public :: ZSTD_RESET_SESSION_ONLY           = 1
    integer(kind=c_int), parameter, public :: ZSTD_RESET_PARAMETERS             = 2
    integer(kind=c_int), parameter, public :: ZSTD_RESET_SESSION_AND_PARAMETERS = 3

    ! ZSTD_bounds
    type, bind(c), public :: zstd_bounds_type
        integer(kind=c_size_t) :: error       = 0
        integer(kind=c_int)    :: lower_bound = 0
        integer(kind=c_int)    :: upper_bound = 0
    end type zstd_bounds_type

    ! ZSTD_inBuffer
    type, bind(c), public :: zstd_in_buffer_type
        type(c_ptr)            :: src  = c_null_ptr
        integer(kind=c_size_t) :: size = 0
        integer(kind=c_size_t) :: pos  = 0
    end type zstd_in_buffer_type

    ! ZSTD_outBuffer
    type, bind(c), public :: zstd_out_buffer_type
        type(c_ptr)            :: dst  = c_null_ptr
        integer(kind=c_size_t) :: size = 0
        integer(kind=c_size_t) :: pos  = 0
    end type zstd_out_buffer_type

    interface
        ! size_t ZSTD_CCtx_reset(ZSTD_CCtx *cctx, ZSTD_ResetDirective reset)
        function zstd_c_ctx_reset(c_ctx, reset) bind(c, name='ZSTD_CCtx_reset')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),         intent(in), value :: c_ctx
            integer(kind=c_int), intent(in), value :: reset
            integer(kind=c_size_t)                 :: zstd_c_ctx_reset
        end function zstd_c_ctx_reset

        ! size_t ZSTD_CCtx_setParameter(ZSTD_CCtx *cctx, ZSTD_cParameter param, int value)
        function zstd_c_ctx_set_parameter(c_ctx, param, value) bind(c, name='ZSTD_CCtx_setParameter')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),         intent(in), value :: c_ctx
            integer(kind=c_int), intent(in), value :: param
            integer(kind=c_int), intent(in), value :: value
            integer(kind=c_size_t)                 :: zstd_c_ctx_set_parameter
        end function zstd_c_ctx_set_parameter

        ! size_t ZSTD_CCtx_setPledgedSrcSize(ZSTD_CCtx *cctx, unsigned long long pledgedSrcSize)
        function zstd_c_ctx_set_pledged_src_size(c_ctx, pledged_src_size) bind(c, name='ZSTD_CCtx_setPledgedSrcSize')
            import :: c_ptr, c_size_t, c_unsigned_long_long
            implicit none
            type(c_ptr),                        intent(in), value :: c_ctx
            integer(kind=c_unsigned_long_long), intent(in), value :: pledged_src_size
            integer(kind=c_size_t)                                :: zstd_c_ctx_set_pledged_src_size
        end function zstd_c_ctx_set_pledged_src_size

        ! ZSTD_bounds ZSTD_cParam_getBounds(ZSTD_cParameter cParam)
        function zstd_c_param_get_bounds(c_param) bind(c, name='ZSTD_cParam_getBounds')
            import :: c_int, zstd_bounds_type
            implicit none
            integer(kind=c_int), intent(in), value :: c_param
            type(zstd_bounds_type)                 :: zstd_c_param_get_bounds
        end function zstd_c_param_get_bounds

        ! size_t ZSTD_compress(void *dst, size_t dstCapacity, const void *src, size_t srcSize, int compressionLevel)
        function zstd_compress(dst, dst_capacity, src, src_size, compression_level) bind(c, name='ZSTD_compress')
            import :: c_int, c_size_t
            implicit none
            type(*),                intent(inout)     :: dst
            integer(kind=c_size_t), intent(in), value :: dst_capacity
            type(*),                intent(inout)     :: src
            integer(kind=c_size_t), intent(in), value :: src_size
            integer(kind=c_int),    intent(in), value :: compression_level
            integer(kind=c_size_t)                    :: zstd_compress
        end function zstd_compress

        ! size_t ZSTD_compress2(ZSTD_CCtx *cctx, void *dst, size_t dstCapacity, const void *src, size_t srcSize)
        function zstd_compress2(c_ctx, dst, dst_capacity, src, src_size) bind(c, name='ZSTD_compress2')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: c_ctx
            type(*),                intent(inout)     :: dst
            integer(kind=c_size_t), intent(in), value :: dst_capacity
            type(*),                intent(inout)     :: src
            integer(kind=c_size_t), intent(in), value :: src_size
            integer(kind=c_size_t)                    :: zstd_compress2
        end function zstd_compress2

        ! size_t ZSTD_compressBound(size_t srcSize)
        function zstd_compress_bound(src_size) bind(c, name='ZSTD_compressBound')
            import :: c_size_t
            integer(kind=c_size_t), intent(in), value :: src_size
            integer(kind=c_size_t)                    :: zstd_compress_bound
        end function zstd_compress_bound

        ! size_t ZSTD_compressCCtx(ZSTD_CCtx *cctx, void *dst, size_t dstCapacity, const void *src, size_t srcSize, int compressionLevel)
        function zstd_compress_c_ctx(c_ctx, dst, dst_capacity, src, src_size, compression_level) bind(c, name='ZSTD_compressCCtx')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: c_ctx
            type(*),                intent(inout)     :: dst
            integer(kind=c_size_t), intent(in), value :: dst_capacity
            type(*),                intent(inout)     :: src
            integer(kind=c_size_t), intent(in), value :: src_size
            integer(kind=c_int),    intent(in), value :: compression_level
            integer(kind=c_size_t)                    :: zstd_compress_c_ctx
        end function zstd_compress_c_ctx

        ! size_t ZSTD_compressStream(ZSTD_CStream *zcs, ZSTD_outBuffer *output, ZSTD_inBuffer *input)
        function zstd_compress_stream(zcs, output, input) bind(c, name='ZSTD_compressStream')
            !! Deprecated.
            import :: c_ptr, c_size_t, zstd_in_buffer_type, zstd_out_buffer_type
            implicit none
            type(c_ptr),                intent(in), value :: zcs
            type(zstd_out_buffer_type), intent(inout)     :: output
            type(zstd_in_buffer_type),  intent(inout)     :: input
            integer(kind=c_size_t)                        :: zstd_compress_stream
        end function zstd_compress_stream

        ! size_t ZSTD_compressStream2(ZSTD_CCtx *cctx, ZSTD_outBuffer *output, ZSTD_inBuffer *input, ZSTD_EndDirective endOp)
        function zstd_compress_stream2(c_ctx, output, input, end_op) bind(c, name='ZSTD_compressStream2')
            import :: c_int, c_ptr, c_size_t, zstd_in_buffer_type, zstd_out_buffer_type
            implicit none
            type(c_ptr),                intent(in), value :: c_ctx
            type(zstd_out_buffer_type), intent(inout)     :: output
            type(zstd_in_buffer_type),  intent(inout)     :: input
            integer(kind=c_int),        intent(in), value :: end_op
            integer(kind=c_size_t)                        :: zstd_compress_stream2
        end function zstd_compress_stream2

        ! ZSTD_CCtx *ZSTD_createCCtx(void)
        function zstd_create_c_ctx() bind(c, name='ZSTD_createCCtx')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zstd_create_c_ctx
        end function zstd_create_c_ctx

        ! ZSTD_CStream *ZSTD_createCStream(void)
        function zstd_create_c_stream() bind(c, name='ZSTD_createCStream')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zstd_create_c_stream
        end function zstd_create_c_stream

        ! ZSTD_DCtx *ZSTD_createDCtx(void)
        function zstd_create_d_ctx() bind(c, name='ZSTD_createDCtx')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zstd_create_d_ctx
        end function zstd_create_d_ctx

        ! ZSTD_DStream *ZSTD_createDStream(void)
        function zstd_create_d_stream() bind(c, name='ZSTD_createDStream')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zstd_create_d_stream
        end function zstd_create_d_stream

        ! size_t ZSTD_CStreamInSize(void)
        function zstd_c_stream_in_size() bind(c, name='ZSTD_CStreamInSize')
            import :: c_size_t
            integer(kind=c_size_t) :: zstd_c_stream_in_size
        end function zstd_c_stream_in_size

        ! size_t ZSTD_CStreamOutSize(void)
        function zstd_c_stream_out_size() bind(c, name='ZSTD_CStreamOutSize')
            import :: c_size_t
            integer(kind=c_size_t) :: zstd_c_stream_out_size
        end function zstd_c_stream_out_size

        ! size_t ZSTD_DCtx_reset(ZSTD_DCtx *dctx, ZSTD_ResetDirective reset)
        function zstd_d_ctx_reset(d_ctx, reset) bind(c, name='ZSTD_DCtx_reset')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),         intent(in), value :: d_ctx
            integer(kind=c_int), intent(in), value :: reset
            integer(kind=c_size_t)                 :: zstd_d_ctx_reset
        end function zstd_d_ctx_reset

        ! size_t ZSTD_DCtx_setParameter(ZSTD_DCtx *dctx, ZSTD_dParameter param, int value)
        function zstd_d_ctx_set_parameter(d_ctx, param, value) bind(c, name='ZSTD_DCtx_setParameter')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),         intent(in), value :: d_ctx
            integer(kind=c_int), intent(in), value :: param
            integer(kind=c_int), intent(in), value :: value
            integer(kind=c_size_t)                 :: zstd_d_ctx_set_parameter
        end function zstd_d_ctx_set_parameter

        ! size_t ZSTD_DStreamInSize(void)
        function zstd_d_stream_in_size() bind(c, name='ZSTD_DStreamInSize')
            import :: c_size_t
            implicit none
            integer(kind=c_size_t) :: zstd_d_stream_in_size
        end function zstd_d_stream_in_size

        ! size_t ZSTD_DStreamOutSize(void)
        function zstd_d_stream_out_size() bind(c, name='ZSTD_DStreamOutSize')
            import :: c_size_t
            implicit none
            integer(kind=c_size_t) :: zstd_d_stream_out_size
        end function zstd_d_stream_out_size

        ! size_t ZSTD_decompress(void *dst, size_t dstCapacity, const void *src, size_t compressedSize)
        function zstd_decompress(dst, dst_capacity, src, compressed_size) bind(c, name='ZSTD_decompress')
            import :: c_size_t
            implicit none
            type(*),                intent(inout)     :: dst
            integer(kind=c_size_t), intent(in), value :: dst_capacity
            type(*),                intent(inout)     :: src
            integer(kind=c_size_t), intent(in), value :: compressed_size
            integer(kind=c_size_t)                    :: zstd_decompress
        end function zstd_decompress

        ! unsigned long long ZSTD_decompressBound(const void *src, size_t srcSize)
        function zstd_decompress_bound(src, src_size) bind(c, name='ZSTD_decompressBound')
            import :: c_size_t, c_unsigned_long_long
            implicit none
            type(*),                intent(inout)     :: src
            integer(kind=c_size_t), intent(in), value :: src_size
            integer(kind=c_unsigned_long_long)        :: zstd_decompress_bound
        end function zstd_decompress_bound

        ! size_t ZSTD_decompressDCtx(ZSTD_DCtx *dctx, void *dst, size_t dstCapacity, const void *src, size_t srcSize)
        function zstd_decompress_d_ctx(d_ctx, dst, dst_capacity, src, src_size) bind(c, name='ZSTD_decompressDCtx')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: d_ctx
            type(*),                intent(inout)     :: dst
            integer(kind=c_size_t), intent(in), value :: dst_capacity
            type(*),                intent(inout)     :: src
            integer(kind=c_size_t), intent(in), value :: src_size
            integer(kind=c_size_t)                    :: zstd_decompress_d_ctx
        end function zstd_decompress_d_ctx

        ! size_t ZSTD_decompressStream(ZSTD_DStream *zds, ZSTD_outBuffer *output, ZSTD_inBuffer *input)
        function zstd_decompress_stream(zds, output, input) bind(c, name='ZSTD_decompressStream')
            import :: c_ptr, c_size_t, zstd_in_buffer_type, zstd_out_buffer_type
            implicit none
            type(c_ptr),                intent(in), value :: zds
            type(zstd_out_buffer_type), intent(inout)     :: output
            type(zstd_in_buffer_type),  intent(inout)     :: input
            integer(kind=c_size_t)                        :: zstd_decompress_stream
        end function zstd_decompress_stream

        ! int ZSTD_defaultCLevel(void)
        function zstd_default_c_level() bind(c, name='ZSTD_defaultCLevel')
            import :: c_int
            implicit none
            integer(kind=c_int) :: zstd_default_c_level
        end function zstd_default_c_level

        ! ZSTD_bounds ZSTD_dParam_getBounds(ZSTD_dParameter dParam)
        function zstd_d_param_get_bounds(d_param) bind(c, name='ZSTD_dParam_getBounds')
            import :: c_int, zstd_bounds_type
            implicit none
            integer(kind=c_int), intent(in), value :: d_param
            type(zstd_bounds_type)                 :: zstd_d_param_get_bounds
        end function zstd_d_param_get_bounds

        ! size_t ZSTD_endStream(ZSTD_CStream *zcs, ZSTD_outBuffer *output)
        function zstd_end_stream(zcs, output) bind(c, name='ZSTD_endStream')
            !! Deprecated.
            import :: c_ptr, c_size_t, zstd_out_buffer_type
            implicit none
            type(c_ptr),                intent(in), value :: zcs
            type(zstd_out_buffer_type), intent(inout)     :: output
            integer(kind=c_size_t)                        :: zstd_end_stream
        end function zstd_end_stream

        ! unsigned long long ZSTD_findDecompressedSize(const void *src, size_t srcSize)
        function zstd_find_decompressed_size(src, src_size) bind(c, name='ZSTD_findDecompressedSize')
            import :: c_size_t, c_unsigned_long_long
            implicit none
            type(*),                intent(inout)     :: src
            integer(kind=c_size_t), intent(in), value :: src_size
            integer(kind=c_unsigned_long_long)        :: zstd_find_decompressed_size
        end function zstd_find_decompressed_size

        ! size_t ZSTD_findFrameCompressedSize(const void *src, size_t srcSize)
        function zstd_find_frame_compressed_size(src, src_size) bind(c, name='ZSTD_findFrameCompressedSize')
            import :: c_size_t
            implicit none
            type(*),                intent(inout)     :: src
            integer(kind=c_size_t), intent(in), value :: src_size
            integer(kind=c_size_t)                    :: zstd_find_frame_compressed_size
        end function zstd_find_frame_compressed_size

        ! size_t ZSTD_flushStream(ZSTD_CStream *zcs, ZSTD_outBuffer *output)
        function zstd_flush_stream(zcs, output) bind(c, name='ZSTD_flushStream')
            !! Deprecated.
            import :: c_ptr, c_size_t, zstd_out_buffer_type
            implicit none
            type(c_ptr),                intent(in), value :: zcs
            type(zstd_out_buffer_type), intent(inout)     :: output
            integer(kind=c_size_t)                        :: zstd_flush_stream
        end function zstd_flush_stream

        ! size_t ZSTD_freeCCtx(ZSTD_CCtx *cctx)
        function zstd_free_c_ctx_(c_ctx) bind(c, name='ZSTD_freeCCtx')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: c_ctx
            integer(kind=c_size_t)         :: zstd_free_c_ctx_
        end function zstd_free_c_ctx_

        ! size_t ZSTD_freeCStream(ZSTD_CStream *zcs)
        function zstd_free_c_stream_(zcs) bind(c, name='ZSTD_freeCStream')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: zcs
            integer(kind=c_size_t)         :: zstd_free_c_stream_
        end function zstd_free_c_stream_

        ! size_t ZSTD_freeDCtx(ZSTD_DCtx *dctx)
        function zstd_free_d_ctx_(d_ctx) bind(c, name='ZSTD_freeDCtx')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: d_ctx
            integer(kind=c_size_t)         :: zstd_free_d_ctx_
        end function zstd_free_d_ctx_

        ! size_t ZSTD_freeDStream(ZSTD_DStream *zds)
        function zstd_free_d_stream_(zds) bind(c, name='ZSTD_freeDStream')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: zds
            integer(kind=c_size_t)         :: zstd_free_d_stream_
        end function zstd_free_d_stream_

        ! unsigned long long ZSTD_getDecompressedSize(const void *src, size_t srcSize)
        function zstd_get_decompressed_size(src, src_size) bind(c, name='ZSTD_getDecompressedSize')
            !! Deprecated.
            import :: c_size_t, c_unsigned_long_long
            implicit none
            type(*),                intent(inout)     :: src
            integer(kind=c_size_t), intent(in), value :: src_size
            integer(kind=c_unsigned_long_long)        :: zstd_get_decompressed_size
        end function zstd_get_decompressed_size

        ! const char *ZSTD_getErrorName(size_t code)
        function zstd_get_error_name_(code) bind(c, name='ZSTD_getErrorName')
            import :: c_ptr, c_size_t
            implicit none
            integer(kind=c_size_t), intent(in), value :: code
            type(c_ptr)                               :: zstd_get_error_name_
        end function zstd_get_error_name_

        ! unsigned long long ZSTD_getFrameContentSize(const void *src, size_t srcSize)
        function zstd_get_frame_content_size(src, src_size) bind(c, name='ZSTD_getFrameContentSize')
            !! Deprecated.
            import :: c_size_t, c_unsigned_long_long
            implicit none
            type(*),                intent(inout)     :: src
            integer(kind=c_size_t), intent(in), value :: src_size
            integer(kind=c_unsigned_long_long)        :: zstd_get_frame_content_size
        end function zstd_get_frame_content_size

        ! size_t ZSTD_initCStream(ZSTD_CStream *zcs, int compressionLevel)
        function zstd_init_c_stream(zcs, compression_level) bind(c, name='ZSTD_initCStream')
            !! Deprecated.
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),         intent(in), value :: zcs
            integer(kind=c_int), intent(in), value :: compression_level
            integer(kind=c_size_t)                 :: zstd_init_c_stream
        end function zstd_init_c_stream

        ! size_t ZSTD_initDStream(ZSTD_DStream *zds)
        function zstd_init_d_stream(zds) bind(c, name='ZSTD_initDStream')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: zds
            integer(kind=c_size_t)         :: zstd_init_d_stream
        end function zstd_init_d_stream

        ! unsigned ZSTD_isError(size_t code)
        function zstd_is_error_(code) bind(c, name='ZSTD_isError')
            import :: c_size_t, c_unsigned
            implicit none
            integer(kind=c_size_t), intent(in), value :: code
            integer(kind=c_unsigned)              :: zstd_is_error_
        end function zstd_is_error_

        ! int ZSTD_maxCLevel(void)
        function zstd_max_c_level() bind(c, name='ZSTD_maxCLevel')
            import :: c_int
            implicit none
            integer(kind=c_int) :: zstd_max_c_level
        end function zstd_max_c_level

        ! int ZSTD_minCLevel(void)
        function zstd_min_c_level() bind(c, name='ZSTD_minCLevel')
            import :: c_int
            implicit none
            integer(kind=c_int) :: zstd_min_c_level
        end function zstd_min_c_level

        ! unsigned ZSTD_versionNumber(void)
        function zstd_version_number() bind(c, name='ZSTD_versionNumber')
            import :: c_unsigned
            implicit none
            integer(kind=c_unsigned) :: zstd_version_number
        end function zstd_version_number

        ! const char *ZSTD_versionString(void)
        function zstd_version_string_() bind(c, name='ZSTD_versionString')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zstd_version_string_
        end function zstd_version_string_
    end interface

    public :: zstd_c_ctx_reset
    public :: zstd_c_ctx_set_parameter
    public :: zstd_c_ctx_set_pledged_src_size
    public :: zstd_c_param_get_bounds
    public :: zstd_c_stream_in_size
    public :: zstd_c_stream_out_size
    public :: zstd_compress
    public :: zstd_compress2
    public :: zstd_compress_bound
    public :: zstd_compress_c_ctx
    public :: zstd_compress_stream
    public :: zstd_compress_stream2
    public :: zstd_create_c_ctx
    public :: zstd_create_c_stream
    public :: zstd_create_d_ctx
    public :: zstd_create_d_stream
    public :: zstd_d_ctx_reset
    public :: zstd_d_ctx_set_parameter
    public :: zstd_d_stream_in_size
    public :: zstd_d_stream_out_size
    public :: zstd_decompress
    public :: zstd_decompress_bound
    public :: zstd_decompress_d_ctx
    public :: zstd_decompress_stream
    public :: zstd_default_c_level
    public :: zstd_d_param_get_bounds
    public :: zstd_end_stream
    public :: zstd_find_decompressed_size
    public :: zstd_find_frame_compressed_size
    public :: zstd_flush_stream
    public :: zstd_free_c_ctx
    public :: zstd_free_c_ctx_
    public :: zstd_free_c_stream
    public :: zstd_free_c_stream_
    public :: zstd_free_d_ctx
    public :: zstd_free_d_ctx_
    public :: zstd_free_d_stream
    public :: zstd_free_d_stream_
    public :: zstd_get_decompressed_size
    public :: zstd_get_error_name
    public :: zstd_get_error_name_
    public :: zstd_get_frame_content_size
    public :: zstd_init_c_stream
    public :: zstd_init_d_stream
    public :: zstd_is_error
    public :: zstd_is_error_
    public :: zstd_max_c_level
    public :: zstd_min_c_level
    public :: zstd_version_number
    public :: zstd_version_string
    public :: zstd_version_string_

    private :: c_f_str_ptr
contains
    function zstd_free_c_ctx(c_ctx) result(code)
        !! Wrapper function that resets C pointer on success.
        type(c_ptr), intent(inout) :: c_ctx
        integer(kind=c_size_t)     :: code

        code = zstd_free_c_ctx_(c_ctx)
        if (.not. zstd_is_error(code)) c_ctx = c_null_ptr
    end function zstd_free_c_ctx

    function zstd_free_c_stream(zcs) result(code)
        !! Wrapper function that resets C pointer on success.
        type(c_ptr), intent(inout) :: zcs
        integer(kind=c_size_t)     :: code

        code = zstd_free_c_stream_(zcs)
        if (.not. zstd_is_error(code)) zcs = c_null_ptr
    end function zstd_free_c_stream

    function zstd_free_d_ctx(d_ctx) result(code)
        !! Wrapper function that resets C pointer on success.
        type(c_ptr), intent(inout) :: d_ctx
        integer(kind=c_size_t)     :: code

        code = zstd_free_d_ctx_(d_ctx)
        if (.not. zstd_is_error(code)) d_ctx = c_null_ptr
    end function zstd_free_d_ctx

    function zstd_free_d_stream(zds) result(code)
        !! Wrapper function that resets C pointer on success.
        type(c_ptr), intent(inout) :: zds
        integer(kind=c_size_t)     :: code

        code = zstd_free_d_stream_(zds)
        if (.not. zstd_is_error(code)) zds = c_null_ptr
    end function zstd_free_d_stream

    function zstd_get_error_name(code) result(str)
        !! Wrapper function that converts C pointer to Fortran string.
        integer(kind=c_size_t), intent(in) :: code
        character(len=:), allocatable      :: str

        type(c_ptr) :: ptr

        ptr = zstd_get_error_name_(code)
        call c_f_str_ptr(ptr, str)
    end function zstd_get_error_name

    function zstd_is_error(code) result(is_error)
        !! Wrapper function that returns logical instead of integer.
        integer(kind=c_size_t), intent(in) :: code
        logical                            :: is_error

        is_error = (zstd_is_error_(code) == 1)
    end function zstd_is_error

    function zstd_version_string() result(str)
        !! Wrapper function that converts C pointer to Fortran string.
        character(len=:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zstd_version_string_()
        call c_f_str_ptr(ptr, str)
    end function zstd_version_string

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
end module zstd
