program test_zlib
    use, intrinsic :: iso_c_binding, only: c_loc
    use :: zlib
    implicit none (type, external)

    integer, parameter :: CHUNK = 16384

    character(len=*), parameter :: SRC_FILE  = 'test/test.txt'
    character(len=*), parameter :: DST_FILE  = 'test.txt.z'
    character(len=*), parameter :: DST_FILE2 = 'test2.txt'

    character(len=*), parameter :: SRC_IN1 = &
        'Now is the time for all good men to come to the aid of the party.'
    character(len=*), parameter :: SRC_IN2 = repeat(SRC_IN1 // ' ', 10)

    integer(kind=z_ulong), parameter :: SRC_A32 = int(z'E9DD1697', kind=z_ulong)
    integer(kind=z_ulong), parameter :: SRC_CRC = int(z'93D6A5C9', kind=z_ulong)

    character(len=:), allocatable :: in1, out1, out2, out3, out4
    integer                       :: rc, sz, sz1, sz2
    integer(kind=z_ulong)         :: a32, c32, sz3, sz4
    logical                       :: exists

    ! Adler32 checksum (must be initialised to 1).
    a32 = adler32(1_z_ulong, SRC_IN1, len(SRC_IN1))
    if (a32 /= SRC_A32) stop 'Error: adler32() failed'

    a32 = adler32_z(1_z_ulong, SRC_IN1, len(SRC_IN1, kind=z_ulong))
    if (a32 /= SRC_A32) stop 'Error: adler32_z() failed'

    ! CRC32 checksum (must be initialised to 0).
    c32 = crc32(0_z_ulong, SRC_IN1, len(SRC_IN1))
    if (c32 /= SRC_CRC) stop 'Error: crc32() failed'

    c32 = crc32_z(0_z_ulong, SRC_IN1, len(SRC_IN1, kind=z_ulong))
    if (c32 /= SRC_CRC) stop 'Error: crc32_z() failed'

    print '("Adler32..........: 0x", z0)', a32
    print '("CRC32............: 0x", z0)', c32

    ! Deflate/inflate file.
    inquire (exist=exists, file=SRC_FILE, size=sz)
    if (.not. exists) stop 'Error: input file not found'

    rc = z_deflate_file(SRC_FILE, DST_FILE, Z_DEFAULT_COMPRESSION)
    if (rc /= Z_OK) stop 'Error: z_deflate_file() failed'

    inquire (exist=exists, file=DST_FILE, size=sz1)
    if (.not. exists) stop 'Error: deflated file not found'

    rc = z_inflate_file(DST_FILE, DST_FILE2)
    if (rc /= Z_OK) stop 'Error: z_inflate_file() failed'

    inquire (exist=exists, file=DST_FILE2, size=sz2)
    if (.not. exists) stop 'Error: inflated file not found'

    if (sz /= sz2) stop 'Error: file sizes do not match'

    print '("input file size..: ", i0)', sz
    print '("deflate file size: ", i0)', sz1
    print '("inflate file size: ", i0)', sz2

    ! Deflate/inflate memory.
    rc = z_deflate_mem(SRC_IN2, out1, Z_DEFAULT_COMPRESSION)
    if (rc /= Z_OK) stop 'Error: z_deflate_mem() failed'

    rc = z_inflate_mem(out1, out2, len(SRC_IN2) * 2)
    if (rc /= Z_OK) stop 'Error: z_inflate_mem() failed'

    if (out2 /= SRC_IN2) stop 'Error: data mismatch'

    print '("input size.......: ", i0)', len(SRC_IN2)
    print '("deflate size.....: ", i0)', len(out1)
    print '("inflate size.....: ", i0)', len(out2)

    ! Compress.
    sz3 = compress_bound(len(SRC_IN2, kind=z_ulong))
    allocate (character(len=sz3) :: out3)
    rc = compress(out3, sz3, SRC_IN2, len(SRC_IN2, kind=z_ulong))
    if (rc /= Z_OK) stop 'Error: compress() failed'

    ! Uncompress.
    sz4 = len(SRC_IN2)
    allocate (character(len=sz4) :: out4)
    rc = uncompress(out4, sz4, out3, sz3)
    if (rc /= Z_OK) stop 'Error: uncompress() failed'

    print '("input size.......: ", i0)', len(SRC_IN2)
    print '("compress size....: ", i0)', sz3
    print '("uncompress size..: ", i0)', sz4

    if (sz4 /= len(SRC_IN2)) stop 'Error: data mismatch'
contains
    integer function z_deflate_file(source, dest, level) result(rc)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: dest
        integer,          intent(in) :: level

        character(len=CHUNK), target :: in
        character(len=CHUNK), target :: out

        character      :: byte
        integer        :: err, flush, have
        integer        :: i, n
        integer        :: in_unit, out_unit
        type(z_stream) :: strm

        rc = deflate_init(strm, level)
        if (rc /= Z_OK) return

        def_block: block
            rc = Z_ERRNO

            open (access='stream', action='read', file=source, form='unformatted', &
                  iostat=err, newunit=in_unit, status='old')
            if (err /= 0) exit def_block

            open (access='stream', action='write', file=dest, form='unformatted', &
                  iostat=err, newunit=out_unit, status='replace')
            if (err /= 0) exit def_block

            do
                n = 0
                flush = Z_NO_FLUSH

                do i = 1, CHUNK
                    read (in_unit, iostat=err) byte

                    if (is_iostat_end(err)) then
                        flush = Z_FINISH
                        exit
                    end if

                    in(i:i) = byte
                    n = n + 1
                end do

                strm%avail_in = n
                strm%next_in = c_loc(in)

                do
                    strm%avail_out = CHUNK
                    strm%next_out = c_loc(out)
                    rc = deflate(strm, flush)
                    if (rc == Z_STREAM_ERROR) exit def_block
                    have = CHUNK - strm%avail_out
                    write (out_unit, iostat=err) out(1:have)
                    if (err /= 0) exit def_block
                    if (strm%avail_out /= 0) exit
                end do

                if (strm%avail_in /= 0) exit def_block
                if (flush == Z_FINISH) exit
            end do

            if (rc /= Z_STREAM_END) exit def_block
            rc = Z_OK
        end block def_block

        err = deflate_end(strm)
        close (out_unit)
        close (in_unit)
    end function z_deflate_file

    integer function z_deflate_mem(source, dest, level) result(rc)
        character(len=*), target,      intent(in)  :: source
        character(len=:), allocatable, intent(out) :: dest
        integer,                       intent(in)  :: level

        character(len=len(source)), target :: buffer
        integer                            :: err, have
        type(z_stream)                     :: strm

        dest = ''

        rc = deflate_init(strm, level)
        if (rc /= Z_OK) return

        def_block: block
            strm%total_in = len(source)
            strm%avail_in = len(source)
            strm%next_in = c_loc(source)

            strm%total_out = len(buffer)
            strm%avail_out = len(buffer)
            strm%next_out = c_loc(buffer)

            rc = deflate(strm, Z_FINISH)
            if (rc == Z_STREAM_ERROR) exit def_block
            have = len(buffer) - strm%avail_out
            dest = buffer(1:have)

            if (rc /= Z_STREAM_END) exit def_block
            rc = Z_OK
        end block def_block

        err = deflate_end(strm)
    end function z_deflate_mem

    integer function z_inflate_file(source, dest) result(rc)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: dest

        character(len=CHUNK), target :: in
        character(len=CHUNK), target :: out

        character      :: byte
        integer        :: err, have
        integer        :: i, n
        integer        :: in_unit, out_unit
        type(z_stream) :: strm

        rc = inflate_init(strm)
        if (rc /= Z_OK) return

        inf_block: block
            rc = Z_ERRNO

            open (access='stream', action='read', file=source, form='unformatted', &
                  iostat=err, newunit=in_unit, status='old')
            if (err /= 0) exit inf_block

            open (access='stream', action='write', file=dest, form='unformatted', &
                  iostat=err, newunit=out_unit, status='replace')
            if (err /= 0) exit inf_block

            do
                n = 0

                do i = 1, CHUNK
                    read (in_unit, iostat=err) byte
                    if (is_iostat_end(err)) exit

                    in(i:i) = byte
                    n = n + 1
                end do

                strm%avail_in = n
                strm%next_in = c_loc(in)

                do
                    strm%avail_out = CHUNK
                    strm%next_out = c_loc(out)
                    rc = inflate(strm, Z_NO_FLUSH)
                    if (rc == Z_STREAM_ERROR) exit inf_block
                    if (rc == Z_NEED_DICT) exit inf_block
                    if (rc == Z_DATA_ERROR) exit inf_block
                    if (rc == Z_MEM_ERROR) exit inf_block
                    have = CHUNK - strm%avail_out
                    write (out_unit, iostat=err) out(1:have)
                    if (err /= 0) exit inf_block
                    if (strm%avail_out /= 0) exit
                end do

                if (rc == Z_STREAM_END) exit
            end do

            rc = Z_OK
        end block inf_block

        err = inflate_end(strm)
        close (out_unit)
        close (in_unit)
    end function z_inflate_file

    integer function z_inflate_mem(source, dest, buffer_size) result(rc)
        character(len=*), target,      intent(in)  :: source
        character(len=:), allocatable, intent(out) :: dest
        integer,                       intent(in)  :: buffer_size

        character(len=buffer_size), target :: buffer
        integer                            :: err, have
        type(z_stream)                     :: strm

        dest = ''

        rc = inflate_init(strm)
        if (rc /= Z_OK) return

        inf_block: block
            strm%total_in = len(source)
            strm%avail_in = len(source)
            strm%next_in = c_loc(source)

            strm%total_out = len(buffer)
            strm%avail_out = len(buffer)
            strm%next_out = c_loc(buffer)

            rc = inflate(strm, Z_FINISH)
            if (rc == Z_STREAM_ERROR) exit inf_block
            have = len(buffer) - strm%avail_out
            dest = buffer(1:have)

            if (rc /= Z_STREAM_END) exit inf_block
            rc = Z_OK
        end block inf_block

        err = inflate_end(strm)
    end function z_inflate_mem
end program test_zlib
