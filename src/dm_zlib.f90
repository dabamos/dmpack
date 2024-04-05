! Author:  Philipp Engel
! Licence: ISC
module dm_zlib
    !! Abstraction layer over zlib (deflate, inflate).
    use :: zlib
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    public :: dm_zlib_compress
    public :: dm_zlib_uncompress
contains
    integer function dm_zlib_compress(input, output, input_len, output_len) result(rc)
        !! Compresses input string using the zlib utility function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if the allocation of the output string failed.
        !! * `E_EMPTY` if the compressed size is 0.
        !! * `E_ZLIB` if the compression failed.
        !!
        character(len=*),              intent(inout)         :: input      !! Input bytes.
        character(len=:), allocatable, intent(out)           :: output     !! Output bytes.
        integer(kind=i8),              intent(in),  optional :: input_len  !! Actual input length.
        integer(kind=i8),              intent(out), optional :: output_len !! Actual output length.

        integer               :: stat
        integer(kind=z_ulong) :: in_len, out_len

        if (present(input_len)) then
            in_len = int(input_len, kind=z_ulong)
        else
            in_len = len(input, kind=z_ulong)
        end if

        if (present(output_len)) output_len = 0

        out_len = compress_bound(in_len)

        rc = E_ALLOC
        allocate (character(len=out_len) :: output, stat=stat)
        if (stat /= 0) return

        rc = E_EMPTY
        if (out_len == 0) return

        rc = E_ZLIB
        stat = compress(output, out_len, input, in_len)

        if (present(output_len)) output_len = int(out_len, kind=i8)
        if (stat /= Z_OK) return
        rc = E_NONE
    end function dm_zlib_compress

    integer function dm_zlib_uncompress(input, output, input_len, output_len) result(rc)
        !! Uncompresses input string using the zlib utility function. The output
        !! buffer must be large enough to hold the uncompressed result. Returns
        !! `E_ZLIB` if the decompression failed.
        character(len=*), intent(inout)         :: input      !! Input bytes.
        character(len=*), intent(inout)         :: output     !! Output bytes.
        integer(kind=i8), intent(in),  optional :: input_len  !! Actual input length.
        integer(kind=i8), intent(out), optional :: output_len !! Actual output length.

        integer               :: stat
        integer(kind=z_ulong) :: in_len, out_len

        if (present(input_len)) then
            in_len = int(input_len, kind=z_ulong)
        else
            in_len = len(input, kind=z_ulong)
        end if

        out_len = len(output, kind=z_ulong)

        rc = E_ZLIB
        stat = uncompress(output, out_len, input, in_len)

        if (present(output_len)) output_len = int(out_len, kind=i8)
        if (stat /= Z_OK) return
        rc = E_NONE
    end function dm_zlib_uncompress
end module dm_zlib
