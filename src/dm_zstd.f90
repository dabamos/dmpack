! Author:  Philipp Engel
! Licence: ISC
module dm_zstd
    !! Abstraction layer over Zstandard (zstd).
    use, intrinsic :: iso_c_binding
    use :: zstd
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    type, public :: zstd_context_type
        !! Opaque Zstandard context type.
        private
        type(c_ptr) :: c = c_null_ptr !! Compression context.
        type(c_ptr) :: d = c_null_ptr !! Decompression context.
    end type zstd_context_type

    interface dm_zstd_compress
        !! Generic Zstandard compression function.
        module procedure :: zstd_compress_context
        module procedure :: zstd_compress_simple
    end interface dm_zstd_compress

    interface dm_zstd_uncompress
        !! Generic Zstandard decompression function.
        module procedure :: zstd_uncompress_context
        module procedure :: zstd_uncompress_simple
    end interface dm_zstd_uncompress

    public :: dm_zstd_compress
    public :: dm_zstd_destroy
    public :: dm_zstd_level_default
    public :: dm_zstd_level_max
    public :: dm_zstd_level_min
    public :: dm_zstd_uncompress
    public :: dm_zstd_version

    private :: zstd_compress_context
    private :: zstd_compress_simple
    private :: zstd_uncompress_context
    private :: zstd_uncompress_simple
contains
    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function dm_zstd_destroy(context) result(rc)
        !! Destroys Zstandard context created with `zstd_compress_context()` or
        !! `zstd_uncompress_context()`.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_COMPILER` if C pointer could not be nullified (compiler bug).
        !! * `E_ZSTD` on library error.
        !!
        type(zstd_context_type), intent(inout) :: context !! Zstandard context type.

        integer(c_size_t) :: stat

        rc = E_NONE

        c_if: if (c_associated(context%c)) then
            stat = zstd_free_c_ctx(context%c)
            if (zstd_is_error(stat)) then
                rc = E_ZSTD
                exit c_if
            end if
            context%c = c_null_ptr
            if (c_associated(context%c)) rc = E_COMPILER
        end if c_if

        d_if: if (c_associated(context%d)) then
            stat = zstd_free_d_ctx(context%d)
            if (zstd_is_error(stat)) then
                rc = E_ZSTD
                exit d_if
            end if
            context%d = c_null_ptr
            if (c_associated(context%d)) rc = E_COMPILER
        end if d_if
    end function dm_zstd_destroy

    integer function dm_zstd_level_default() result(level)
        !! Returns default zstd compression level.
        level = zstd_default_c_level()
    end function dm_zstd_level_default

    integer function dm_zstd_level_max() result(level)
        !! Returns maximum zstd compression level.
        level = zstd_max_c_level()
    end function dm_zstd_level_max

    integer function dm_zstd_level_min() result(level)
        !! Returns minimum zstd compression level.
        level = zstd_min_c_level()
    end function dm_zstd_level_min

    function dm_zstd_version(name) result(version)
        !! Returns zstd library version as allocatable string.
        use :: dm_util, only: dm_present

        logical, intent(in), optional :: name !! Add prefix `libzstd/`.
        character(:), allocatable     :: version

        if (dm_present(name, .false.)) then
            version = 'libzstd/' // zstd_version_string()
        else
            version = zstd_version_string()
        end if
    end function dm_zstd_version

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function zstd_compress_context(context, input, output, level, input_len, output_len) result(rc)
        !! Compresses input string using the zstd simple context function. If no
        !! compression level is passed, the Zstandard default is used. The
        !! Zstandard context `context` has to be destroy with
        !! `dm_zstd_destroy()` once finished.
        !!
        !! The string `output` may be larger than the actual length. The
        !! argument `output_len` contains the actual length.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if the allocation of the output string failed.
        !! * `E_EMPTY` if the compressed size is 0.
        !! * `E_ZSTD` if the compression failed.
        !!
        type(zstd_context_type),   intent(inout)         :: context    !! Zstandard context type.
        character(*),              intent(inout)         :: input      !! Input bytes.
        character(:), allocatable, intent(out)           :: output     !! Output bytes.
        integer,                   intent(in),  optional :: level      !! Compression level.
        integer(i8),               intent(in),  optional :: input_len  !! Actual input length.
        integer(i8),               intent(out), optional :: output_len !! Actual output length.

        integer                :: level_
        integer(c_size_t) :: in_len, out_len, stat
        integer(i8)       :: output_len_

        output_len_ = 0

        zstd_block: block
            rc = E_EMPTY
            if (len(input) == 0) exit zstd_block

            if (present(input_len)) then
                in_len = int(input_len, c_size_t)
            else
                in_len = len(input, c_size_t)
            end if

            out_len = zstd_compress_bound(in_len)

            rc = E_ALLOC
            allocate (character(out_len) :: output, stat=stat)
            if (stat /= 0) exit zstd_block

            rc = E_ZSTD
            if (.not. c_associated(context%c)) then
                context%c = zstd_create_c_ctx()
                if (.not. c_associated(context%c)) exit zstd_block
            end if

            if (present(level)) then
                level_ = level
            else
                level_ = dm_zstd_level_default()
            end if

            stat = zstd_compress_c_ctx(context%c, output, out_len, input, in_len, level_)
            if (zstd_is_error(stat)) exit zstd_block
            output_len_ = stat

            rc = E_NONE
        end block zstd_block

        if (present(output_len)) output_len = output_len_
    end function zstd_compress_context

    integer function zstd_compress_simple(input, output, level, input_len, output_len) result(rc)
        !! Compresses input string using the zstd simple function. If no
        !! compression level is passed, the Zstandard default is used.
        !!
        !! The string `output` may be larger than the actual length. The
        !! argument `output_len` contains the actual length.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if the allocation of the output string failed.
        !! * `E_EMPTY` if the compressed size is 0.
        !! * `E_ZSTD` if the compression failed.
        !!
        character(*),              intent(inout)         :: input      !! Input bytes.
        character(:), allocatable, intent(out)           :: output     !! Output bytes.
        integer,                   intent(in),  optional :: level      !! Compression level.
        integer(i8),               intent(in),  optional :: input_len  !! Actual input length.
        integer(i8),               intent(out), optional :: output_len !! Actual output length.

        integer           :: level_
        integer(c_size_t) :: in_len, out_len, stat
        integer(i8)       :: output_len_

        output_len_ = 0

        zstd_block: block
            rc = E_EMPTY
            if (len(input) == 0) exit zstd_block

            if (present(input_len)) then
                in_len = int(input_len, c_size_t)
            else
                in_len = len(input, c_size_t)
            end if

            out_len = zstd_compress_bound(in_len)

            rc = E_ALLOC
            allocate (character(out_len) :: output, stat=stat)
            if (stat /= 0) exit zstd_block

            if (present(level)) then
                level_ = level
            else
                level_ = dm_zstd_level_default()
            end if

            rc = E_ZSTD
            stat = zstd_compress(output, out_len, input, in_len, level_)
            if (zstd_is_error(stat)) exit zstd_block
            output_len_ = stat

            rc = E_NONE
        end block zstd_block

        if (present(output_len)) output_len = output_len_
    end function zstd_compress_simple

    integer function zstd_uncompress_context(context, input, output, input_len, output_len) result(rc)
        !! Uncompresses input string using the zstd simple context function. The
        !! output buffer must be large enough to hold the uncompressed result.
        !! The function returns `E_ZSTD` if the decompression failed. The
        !! Zstandard context type `context` has to be destroyed with
        !! `dm_zstd_destroy()` once finished.
        type(zstd_context_type), intent(inout)         :: context    !! Zstandard context type.
        character(*),            intent(inout)         :: input      !! Input bytes.
        character(*),            intent(inout)         :: output     !! Output bytes.
        integer(i8),             intent(in),  optional :: input_len  !! Actual input length.
        integer(i8),             intent(out), optional :: output_len !! Actual output length.

        integer(c_size_t) :: in_len, stat
        integer(i8)       :: output_len_

        output_len_ = 0

        zstd_block: block
            rc = E_ZSTD
            if (.not. c_associated(context%d)) then
                context%d = zstd_create_d_ctx()
                if (.not. c_associated(context%d)) exit zstd_block
            end if

            if (present(input_len)) then
                in_len = int(input_len, c_size_t)
            else
                in_len = len(input, c_size_t)
            end if

            stat = zstd_decompress_d_ctx(context%d, output, len(output, c_size_t), input, in_len)
            if (zstd_is_error(stat)) exit zstd_block
            output_len_ = stat

            rc = E_NONE
        end block zstd_block

        if (present(output_len)) output_len = output_len_
    end function zstd_uncompress_context

    integer function zstd_uncompress_simple(input, output, input_len, output_len) result(rc)
        !! Uncompresses input string using the zstd simple function. The output
        !! buffer must be large enough to hold the uncompressed result. The
        !! function returns `E_ZSTD` if the decompression failed.
        character(*), intent(inout)         :: input      !! Input bytes.
        character(*), intent(inout)         :: output     !! Output bytes.
        integer(i8),  intent(in),  optional :: input_len  !! Actual input length.
        integer(i8),  intent(out), optional :: output_len !! Actual output length.

        integer(c_size_t) :: in_len, stat
        integer(i8)       :: output_len_

        rc = E_ZSTD
        output_len_ = 0

        if (present(input_len)) then
            in_len = int(input_len, c_size_t)
        else
            in_len = len(input, c_size_t)
        end if

        stat = zstd_decompress(output, len(output, c_size_t), input, in_len)

        if (.not. zstd_is_error(stat)) then
            rc = E_NONE
            output_len_ = stat
        end if

        if (present(output_len)) output_len = output_len_
    end function zstd_uncompress_simple
end module dm_zstd
