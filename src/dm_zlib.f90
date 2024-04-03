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
    integer function dm_zlib_compress(input, output, output_len) result(rc)
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
        integer(kind=i8),              intent(out), optional :: output_len !! Actual output length.

        integer               :: stat
        integer(kind=z_ulong) :: sz

        if (present(output_len)) output_len = 0
        sz = compress_bound(len(input, kind=z_ulong))

        rc = E_ALLOC
        allocate (character(len=sz) :: output, stat=stat)
        if (stat /= 0) return

        rc = E_EMPTY
        if (sz == 0) return

        rc = E_ZLIB
        stat = compress(output, sz, input, len(input, kind=z_ulong))
        if (present(output_len)) output_len = int(sz, kind=i8)
        if (stat /= Z_OK) return

        rc = E_NONE
    end function dm_zlib_compress

    integer function dm_zlib_uncompress(input, output, output_len) result(rc)
        !! Uncompresses input string using the zlib utility function. The output
        !! buffer must be large enough to hold the uncompressed result. Returns
        !! `E_ZLIB` if the decompression failed.
        character(len=*), intent(inout)         :: input      !! Input bytes.
        character(len=*), intent(inout)         :: output     !! Output bytes.
        integer(kind=i8), intent(out), optional :: output_len !! Actual output length.

        integer               :: stat
        integer(kind=z_ulong) :: sz

        rc = E_ZLIB
        sz = len(output, kind=z_ulong)
        stat = uncompress(output, sz, input, len(input, kind=z_ulong))
        if (present(output_len)) output_len = int(sz, kind=i8)
        if (stat /= Z_OK) return
        rc = E_NONE
    end function dm_zlib_uncompress
end module dm_zlib
