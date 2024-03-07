! Author:  Philipp Engel
! Licence: ISC
module dm_z
    !! Abstraction layer over zlib (deflate, inflate).
    use, intrinsic :: iso_c_binding, only: c_loc
    use :: zlib
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    public :: dm_z_compress
    public :: dm_z_deflate
    public :: dm_z_inflate
    public :: dm_z_uncompress
contains
    integer function dm_z_compress(input, output, output_size) result(rc)
        !! Compresses input string using the zlib utility function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if the allocation of the output string failed.
        !! * `E_EMPTY` if the compressed size is 0.
        !! * `E_ZLIB` if the compression failed.
        character(len=*),              intent(inout)         :: input       !! Input bytes.
        character(len=:), allocatable, intent(out)           :: output      !! Output bytes.
        integer(kind=i8),              intent(out), optional :: output_size !! Actual output length.

        integer               :: stat
        integer(kind=z_ulong) :: sz

        if (present(output_size)) output_size = 0
        sz = compress_bound(len(input, kind=z_ulong))

        rc = E_ALLOC
        allocate (character(len=sz) :: output, stat=stat)
        if (stat /= 0) return

        rc = E_EMPTY
        if (sz == 0) return

        rc = E_ZLIB
        stat = compress(output, sz, input, len(input, kind=z_ulong))
        if (present(output_size)) output_size = int(sz, kind=i8)
        if (stat /= Z_OK) return

        rc = E_NONE
    end function dm_z_compress

    integer function dm_z_deflate(input, output) result(rc)
        !! Compresses input string. Returns `E_ZLIB` if the compression
        !! failed.
        character(len=*), target,      intent(inout) :: input  !! Input bytes.
        character(len=:), allocatable, intent(out)   :: output !! Output bytes.

        character(len=len(input)), target :: buffer
        integer                           :: have, stat
        type(z_stream)                    :: strm

        rc = E_ZLIB
        output = ''

        stat = deflate_init2(strm, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY)
        if (stat /= Z_OK) return

        def_block: block
            strm%avail_in = len(input)
            strm%total_in = strm%avail_in
            strm%next_in  = c_loc(input)

            strm%avail_out = len(buffer)
            strm%total_out = strm%avail_in
            strm%next_out  = c_loc(buffer)

            stat = deflate(strm, Z_FINISH)
            if (stat == Z_STREAM_ERROR) exit def_block
            have = len(buffer) - strm%avail_out
            output = buffer(1:have)

            if (stat /= Z_STREAM_END) exit def_block
            rc = E_NONE
        end block def_block

        stat = deflate_end(strm)
    end function dm_z_deflate

    integer function dm_z_inflate(input, output, buffer_size) result(rc)
        !! Decompresses input string. The argument `buffer_size` specifies the
        !! size of the inflate buffer and must be large enough for the buffer
        !! to hold the result. Returns `E_ZLIB` if the decompression failed.
        character(len=*), target,      intent(inout) :: input       !! Input bytes.
        character(len=:), allocatable, intent(out)   :: output      !! Output bytes.
        integer,                       intent(in)    :: buffer_size !! Buffer size.

        character(len=buffer_size), target :: buffer
        integer                            :: have, stat
        type(z_stream)                     :: strm

        rc = E_ZLIB
        output = ''

        stat = inflate_init2(strm, -15)
        if (stat /= Z_OK) return

        inf_block: block
            strm%avail_in = len(input)
            strm%total_in = strm%total_in
            strm%next_in  = c_loc(input)

            strm%avail_out = len(buffer)
            strm%total_out = strm%avail_out
            strm%next_out  = c_loc(buffer)

            stat = inflate(strm, Z_FINISH)
            if (stat == Z_STREAM_ERROR) exit inf_block
            have = len(buffer) - strm%avail_out
            output = buffer(1:have)

            if (stat /= Z_STREAM_END) exit inf_block
            rc = E_NONE
        end block inf_block

        stat = inflate_end(strm)
    end function dm_z_inflate

    integer function dm_z_uncompress(input, output, output_size) result(rc)
        !! Uncompresses input string using the zlib utility function. The output
        !! buffer must be large enough to hold the uncompressed result. Returns
        !! `E_ZLIB` if the decompression failed.
        character(len=*), intent(inout)         :: input       !! Input bytes.
        character(len=*), intent(inout)         :: output      !! Output bytes.
        integer(kind=i8), intent(out), optional :: output_size !! Actual output length.

        integer               :: stat
        integer(kind=z_ulong) :: sz

        rc = E_ZLIB
        sz = len(output, kind=z_ulong)
        stat = uncompress(output, sz, input, len(input, kind=z_ulong))
        if (present(output_size)) output_size = int(sz, kind=i8)
        if (stat /= Z_OK) return
        rc = E_NONE
    end function dm_z_uncompress
end module dm_z
