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
    public :: dm_z_deflate_mem
    public :: dm_z_inflate_mem
    public :: dm_z_uncompress
contains
    integer function dm_z_compress(input, output, output_size) result(rc)
        !! Compresses input string using the zlib utility function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if the allocation of the output string failed.
        !! * `E_EMPTY` if the compressed size is 0.
        !! * `E_ERROR` if the compression failed.
        character(len=*),              intent(inout)         :: input       !! Input bytes.
        character(len=:), allocatable, intent(out)           :: output      !! Output bytes.
        integer(kind=i8),              intent(out), optional :: output_size !! Actual output length.

        integer          :: stat
        integer(kind=i8) :: sz

        if (present(output_size)) output_size = 0
        sz = compress_bound(len(input, kind=i8))

        rc = E_ALLOC
        allocate (character(len=sz) :: output, stat=stat)
        if (stat /= 0) return

        rc = E_EMPTY
        if (sz == 0) return

        rc = E_ERROR
        stat = compress(output, sz, input, len(input, kind=i8))
        if (present(output_size)) output_size = sz
        if (stat /= Z_OK) return

        rc = E_NONE
    end function dm_z_compress

    integer function dm_z_deflate_mem(input, output) result(rc)
        !! Compresses input string. Returns `E_ERROR` if the compression
        !! failed.
        character(len=*), target,      intent(inout) :: input  !! Input bytes.
        character(len=:), allocatable, intent(out)   :: output !! Output bytes.

        character(len=len(input)), target :: buffer
        integer                           :: err, have
        type(z_stream)                    :: strm

        rc = E_ERROR
        output = ''

        if (deflate_init2(strm, Z_DEFAULT_COMPRESSION, Z_DEFLATED, &
                          -15, 8, Z_DEFAULT_STRATEGY) /= Z_OK) return

        def_block: block
            strm%avail_in = len(input)
            strm%total_in = strm%avail_in
            strm%next_in  = c_loc(input)

            strm%avail_out = len(buffer)
            strm%total_out = strm%avail_in
            strm%next_out  = c_loc(buffer)

            err = deflate(strm, Z_FINISH)
            if (err == Z_STREAM_ERROR) exit def_block
            have = len(buffer) - strm%avail_out
            output = buffer(1:have)

            if (err /= Z_STREAM_END) exit def_block
            rc = E_NONE
        end block def_block

        err = deflate_end(strm)
    end function dm_z_deflate_mem

    integer function dm_z_inflate_mem(input, output, buffer_size) result(rc)
        !! Decompresses input string. Returns `E_ERROR` if the decompression
        !! failed.
        character(len=*), target,      intent(inout) :: input       !! Input bytes.
        character(len=:), allocatable, intent(out)   :: output      !! Output bytes.
        integer,                       intent(in)    :: buffer_size !! Buffer size.

        character(len=buffer_size), target :: buffer
        integer                            :: err, have
        type(z_stream)                     :: strm

        rc = E_ERROR
        output = ''

        if (inflate_init2(strm, -15) /= Z_OK) return

        inf_block: block
            strm%avail_in = len(input)
            strm%total_in = strm%total_in
            strm%next_in  = c_loc(input)

            strm%avail_out = len(buffer)
            strm%total_out = strm%avail_out
            strm%next_out  = c_loc(buffer)

            err = inflate(strm, Z_FINISH)
            if (err == Z_STREAM_ERROR) exit inf_block
            have = len(buffer) - strm%avail_out
            output = buffer(1:have)

            if (err /= Z_STREAM_END) exit inf_block
            rc = E_NONE
        end block inf_block

        err = inflate_end(strm)
    end function dm_z_inflate_mem

    integer function dm_z_uncompress(input, output, output_size) result(rc)
        !! Uncompresses input string using the zlib utility function. Returns
        !! `E_ERROR` if the decompression failed.
        character(len=*), intent(inout)         :: input       !! Input bytes.
        character(len=*), intent(inout)         :: output      !! Output bytes.
        integer(kind=i8), intent(out), optional :: output_size !! Actual output length.

        integer          :: stat
        integer(kind=i8) :: sz

        rc = E_ERROR
        sz = len(output, kind=i8)
        stat = uncompress(output, sz, input, len(input, kind=i8))
        if (present(output_size)) output_size = sz
        if (stat /= Z_OK) return
        rc = E_NONE
    end function dm_z_uncompress
end module dm_z
