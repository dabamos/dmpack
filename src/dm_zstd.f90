! Author:  Philipp Engel
! Licence: ISC
module dm_zstd
    !! Abstraction layer over Zstandard (zstd).
    use :: dm_c
    use :: dm_error
    use :: dm_kind
    use :: zstd
    implicit none (type, external)
    private

    type, public :: zstd_context_type
        !! Opaque Zstandard context type.
        private
        integer(c_size_t) :: status = 0          !! Last zstd status code.
        type(c_ptr)       :: c      = c_null_ptr !! Compression context.
        type(c_ptr)       :: d      = c_null_ptr !! Decompression context.
    end type zstd_context_type

    interface dm_zstd_compress
        !! Generic Zstandard compression function.
        module procedure :: zstd_compress_context
        module procedure :: zstd_compress_free
    end interface dm_zstd_compress

    interface dm_zstd_uncompress
        !! Generic Zstandard decompression function.
        module procedure :: zstd_uncompress_context
        module procedure :: zstd_uncompress_free
    end interface dm_zstd_uncompress

    public :: dm_zstd_compress
    public :: dm_zstd_destroy
    public :: dm_zstd_error_message
    public :: dm_zstd_is_error
    public :: dm_zstd_level_default
    public :: dm_zstd_level_max
    public :: dm_zstd_level_min
    public :: dm_zstd_uncompress
    public :: dm_zstd_version

    private :: zstd_compress_context
    private :: zstd_compress_free
    private :: zstd_uncompress_context
    private :: zstd_uncompress_free
contains
    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    subroutine dm_zstd_destroy(context, error)
        !! Destroys Zstandard context created with `zstd_compress_context()` or
        !! `zstd_uncompress_context()`.
        !!
        !! The subroutine returns the followin error codes in `error`:
        !!
        !! * `E_COMPILER` if C pointer could not be nullified (compiler bug).
        !! * `E_ZSTD` on library error.
        !!
        type(zstd_context_type), intent(inout)         :: context !! Zstandard context type.
        integer,                 intent(out), optional :: error   !! Error code.

        integer :: rc

        rc = E_NONE

        c_block: block
            if (.not. c_associated(context%c)) exit c_block
            context%status = zstd_free_c_ctx(context%c)

            rc = E_ZSTD
            if (zstd_is_error(context%status)) exit c_block

            rc = E_COMPILER
            if (.not. c_associated(context%c)) rc = E_NONE
        end block c_block

        d_block: block
            if (.not. c_associated(context%d)) exit d_block
            context%status = zstd_free_d_ctx(context%d)

            rc = E_ZSTD
            if (zstd_is_error(context%status)) exit d_block

            rc = E_COMPILER
            if (.not. c_associated(context%d)) rc = E_NONE
        end block d_block

        if (present(error)) error = rc
    end subroutine dm_zstd_destroy

    function dm_zstd_error_message(context) result(message)
        !! Returns last error message as allocatable character string. If no
        !! error occured, the result is allocated but empty.
        type(zstd_context_type), intent(inout) :: context !! Zstandard context type.
        character(:), allocatable              :: message

        if (zstd_is_error(context%status)) then
            message = zstd_get_error_name(context%status)
            return
        end if

        message = ''
    end function dm_zstd_error_message

    logical function dm_zstd_is_error(status) result(is)
        integer, intent(in) :: status !! Zstd status code.

        is = zstd_is_error(int(status, c_size_t))
    end function dm_zstd_is_error

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
    ! PRIVATE PROCEDURES
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

        integer           :: level_, stat
        integer(c_size_t) :: in_len, out_len
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

            context%status = zstd_compress_c_ctx(context%c, output, out_len, input, in_len, level_)
            if (zstd_is_error(context%status)) exit zstd_block
            output_len_ = context%status

            rc = E_NONE
        end block zstd_block

        if (present(output_len)) output_len = output_len_
    end function zstd_compress_context

    integer function zstd_compress_free(input, output, level, input_len, output_len) result(rc)
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
        integer(i8),               intent(out), optional :: output_len !! Actual output length or zstd error code.

        integer           :: level_, stat
        integer(c_size_t) :: in_len, out_len, status

        status = 0

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
            status = zstd_compress(output, out_len, input, in_len, level_)
            if (.not. zstd_is_error(status)) rc = E_NONE
        end block zstd_block

        if (present(output_len)) output_len = status
    end function zstd_compress_free

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

        integer(c_size_t) :: in_len
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

            context%status = zstd_decompress_d_ctx(context%d, output, len(output, c_size_t), input, in_len)
            if (zstd_is_error(context%status)) exit zstd_block
            output_len_ = context%status

            rc = E_NONE
        end block zstd_block

        if (present(output_len)) output_len = output_len_
    end function zstd_uncompress_context

    integer function zstd_uncompress_free(input, output, input_len, output_len) result(rc)
        !! Uncompresses input string using the zstd simple function. The output
        !! buffer must be large enough to hold the uncompressed result. The
        !! function returns `E_ZSTD` if the decompression failed.
        character(*), intent(inout)         :: input      !! Input bytes.
        character(*), intent(inout)         :: output     !! Output bytes.
        integer(i8),  intent(in),  optional :: input_len  !! Actual input length.
        integer(i8),  intent(out), optional :: output_len !! Actual output length or zstd status code.

        integer(c_size_t) :: in_len, status

        rc = E_ZSTD

        if (present(input_len)) then
            in_len = int(input_len, c_size_t)
        else
            in_len = len(input, c_size_t)
        end if

        if (present(output_len)) output_len = 0_i8

        status = zstd_decompress(output, len(output, c_size_t), input, in_len)
        if (.not. zstd_is_error(status)) rc = E_NONE
        if (present(output_len)) output_len = status
    end function zstd_uncompress_free
end module dm_zstd
