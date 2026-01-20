! test_zstd.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    use :: zstd
    implicit none (type, external)

    integer, parameter :: NTESTS = 3

    integer :: i
    logical :: tests(NTESTS)

    print '("zstd version number: ", i0)', zstd_version_number()
    print '("zstd version string: ", a)',  zstd_version_string()

    tests(1) = test_simple()
    tests(2) = test_simple_multi()
    tests(3) = test_stream()

    do i = 1, NTESTS
        if (.not. tests(i)) error stop
    end do
contains
    integer(kind=c_size_t) function test_read(chunk, pos, to_read, src) result(nbytes)
        !! Reads chunk from source `src` and returns chunk length.
        character(len=*),       intent(inout) :: chunk   !! Output chunk.
        integer(kind=c_size_t), intent(inout) :: pos     !! Current cursor position.
        integer(kind=c_size_t), intent(in)    :: to_read !! Max. bytes to read.
        character(len=*),       intent(inout) :: src     !! Source to read from.

        integer(kind=c_size_t) :: pos2

        nbytes = 0
        if (to_read == 0) return

        pos = pos + 1
        if (pos > len(src)) return

        pos2   = min(pos + to_read, len(src, kind=c_size_t))
        chunk  = src(pos:pos2)
        nbytes = abs(pos2 - pos) + 1
        pos    = pos2
    end function test_read

    logical function test_simple() result(success)
        !! Simple API.
        character(len=:), allocatable :: dst1, dst2, src
        integer                       :: level
        integer(kind=c_size_t)        :: dst_len, src_len
        integer(kind=c_size_t)        :: stat

        success = .false.

        print '(">>> test_simple")'

        src = repeat('Now is the time for all good men to come to the aid of the party. ', 128)

        src_len = len(src, kind=c_size_t)
        dst_len = zstd_compress_bound(src_len)

        allocate (character(len=dst_len) :: dst1)
        allocate (character(len=src_len) :: dst2)

        level = zstd_default_c_level()
        stat  = zstd_compress(dst1, dst_len, src, src_len, level)

        if (zstd_is_error(stat)) then
            print '("zstd_compress: ", a)', zstd_get_error_name(stat)
            return
        end if

        dst_len = stat

        print '("src length: ", i0)', src_len
        print '("dst length: ", i0)', dst_len

        stat = zstd_decompress(dst2, src_len, dst1, dst_len)

        if (zstd_is_error(stat)) then
            print '("zstd_decompress: ", a)', zstd_get_error_name(stat)
            return
        end if

        if (dst2 /= src) then
            print '("data mismatch")'
            return
        end if

        success = .true.
    end function test_simple

    logical function test_simple_multi() result(success)
        !! Multiple simple API.
        character(len=:), allocatable :: dst1, dst2, src
        integer                       :: level
        integer(kind=c_size_t)        :: dst_len, src_len
        integer(kind=c_size_t)        :: stat, stat2
        type(c_ptr)                   :: c_ctx, d_ctx

        success = .false.

        print '(">>> test_simple_multi")'

        src = repeat('Now is the time for all good men to come to the aid of the party. ', 128)

        src_len = len(src, kind=c_size_t)
        dst_len = zstd_compress_bound(src_len)

        allocate (character(len=dst_len) :: dst1)
        allocate (character(len=src_len) :: dst2)

        c_ctx = zstd_create_c_ctx()
        level = zstd_default_c_level()
        stat  = zstd_compress_c_ctx(c_ctx, dst1, dst_len, src, src_len, level)
        stat2 = zstd_free_c_ctx(c_ctx)

        if (zstd_is_error(stat)) then
            print '("zstd_compress: ", a)', zstd_get_error_name(stat)
            return
        end if

        dst_len = stat

        print '("src length: ", i0)', src_len
        print '("dst length: ", i0)', dst_len

        d_ctx = zstd_create_d_ctx()
        stat  = zstd_decompress_d_ctx(d_ctx, dst2, src_len, dst1, dst_len)
        stat2 = zstd_free_d_ctx(d_ctx)

        if (zstd_is_error(stat)) then
            print '("zstd_decompress: ", a)', zstd_get_error_name(stat)
            return
        end if

        if (dst2 /= src) then
            print '("data mismatch")'
            return
        end if

        success = .true.
    end function test_simple_multi

    logical function test_stream() result(success)
        !! Streaming API.
        character(len=:), allocatable :: dst, src1, src2
        integer(kind=c_size_t)        :: stat

        success = .true.

        print '(">>> test_stream")'

        src1 = repeat('Now is the time for all good men to come to the aid of the party. ', 128)
        src2 = ''
        dst  = ''

        ! Compression: src1 -> dst.
        c_block: block
            character(len=:), allocatable, target :: buf_in, buf_out

            integer                    :: level, mode
            integer(kind=c_size_t)     :: buf_in_sz, buf_out_sz
            integer(kind=c_size_t)     :: nbytes, pos, remaining, to_read
            logical                    :: finished, last_chunk
            type(c_ptr)                :: c_ctx
            type(zstd_in_buffer_type)  :: input
            type(zstd_out_buffer_type) :: output

            ! Create the input and output buffers. They may be any size, but
            ! we recommend using these functions to size them. Performance will
            ! only suffer significantly for very tiny buffers.
            buf_in_sz  = zstd_c_stream_in_size()
            buf_out_sz = zstd_c_stream_out_size()

            allocate (character(len=buf_in_sz)  :: buf_in)
            allocate (character(len=buf_out_sz) :: buf_out)

            c_ctx = zstd_create_c_ctx()

            if (.not. c_associated(c_ctx)) then
                print '("zstd_create_c_ctx")'
                success = .false.
                exit c_block
            end if

            level = zstd_default_c_level()
            stat  = zstd_c_ctx_set_parameter(c_ctx, ZSTD_C_COMPRESSIONLEVEL, level)

            if (zstd_is_error(stat)) then
                print '("zstd_c_ctx_set_parameter: ", a)', zstd_get_error_name(stat)
                success = .false.
            end if

            stat = zstd_c_ctx_set_parameter(c_ctx, ZSTD_C_CHECKSUMFLAG, 1)

            if (zstd_is_error(stat)) then
                print '("zstd_c_ctx_set_parameter: ", a)', zstd_get_error_name(stat)
                success = .false.
            end if

            pos     = 0
            to_read = buf_in_sz

            do
                nbytes = test_read(buf_in, pos, to_read, src1)
                last_chunk = (nbytes < to_read)

                ! Select the flush mode. If the read may not be finished
                ! (nbytes == to_read) we use ZSTD_E_CONTINUE. If this is the
                ! last chunk, we use ZSTD_E_END. Zstd optimises the case where
                ! the first flush mode is ZSTD_E_END, since it knows it is
                ! compressing the entire source in one pass.
                if (last_chunk) then
                    mode = ZSTD_E_END
                else
                    mode = ZSTD_E_CONTINUE
                end if

                ! Set the input buffer to what we just read. We compress until
                ! the input buffer is empty, each time flushing the output.
                input = zstd_in_buffer_type(c_loc(buf_in), nbytes, 0)
                finished = .false.

                do while (.not. finished)
                    ! Compress into the output buffer and write all of the output to
                    ! the file so we can reuse the buffer next iteration.
                    output = zstd_out_buffer_type(c_loc(buf_out), buf_out_sz, 0)

                    remaining = zstd_compress_stream2(c_ctx, output, input, mode)

                    if (zstd_is_error(remaining)) then
                        print '("zstd_compress_stream2: ", a)', zstd_get_error_name(remaining)
                        success = .false.
                        exit
                    end if

                    ! Add compressed chunk to destination string. Alternatively,
                    ! we could also write the chunk to file.
                    dst = dst // buf_out(:output%pos)

                    ! If we're on the last chunk we're finished when zstd returns 0,
                    ! which means its consumed all the input AND finished the frame.
                    ! Otherwise, we're finished when we've consumed all the input.
                    if (last_chunk) then
                        finished = (remaining == 0)
                    else
                        finished = (input%pos == input%size)
                    end if
                end do

                if (input%pos /= input%size) then
                    print '("zstd only returns 0 when the input is completely consumed")'
                    success = .false.
                    exit
                end if

                if (last_chunk) exit
            end do

            stat = zstd_free_c_ctx(c_ctx)
        end block c_block

        if (.not. success) return

        ! Decompression: dst -> src2.
        d_block: block
            character(len=:), allocatable, target :: buf_in, buf_out

            integer(kind=c_size_t)     :: buf_in_sz, buf_out_sz
            integer(kind=c_size_t)     :: last_stat, nbytes, pos, to_read
            logical                    :: is_empty
            type(c_ptr)                :: d_ctx
            type(zstd_in_buffer_type)  :: input
            type(zstd_out_buffer_type) :: output

            buf_in_sz  = zstd_d_stream_in_size()
            buf_out_sz = zstd_d_stream_out_size()

            allocate (character(len=buf_in_sz)  :: buf_in)
            allocate (character(len=buf_out_sz) :: buf_out)

            d_ctx = zstd_create_d_ctx()

            if (.not. c_associated(d_ctx)) then
                print '("zstd_create_d_ctx")'
                success = .false.
                exit d_block
            end if

            pos       = 0
            to_read   = buf_in_sz
            last_stat = 0
            is_empty  = .true.

            ! This loop assumes that the input file is one or more concatenated zstd
            ! streams. This example won't work if there is trailing non-zstd data at
            ! the end, but streaming decompression in general handles this case.
            ! zstd_decompress_stream() returns 0 exactly when the frame is completed,
            ! and doesn't consume input after the frame.
            do
                nbytes = test_read(buf_in, pos, to_read, dst)
                if (nbytes == 0) exit
                is_empty = .false.

                input = zstd_in_buffer_type(c_loc(buf_in), nbytes, 0)

                ! Given a valid frame, zstd won't consume the last byte of the frame
                ! until it has flushed all of the decompressed data of the frame.
                ! Therefore, instead of checking if the return code is 0, we can
                ! decompress just check if input%pos < input%size.
                do while (input%pos < input%size)
                    output = zstd_out_buffer_type(c_loc(buf_out), buf_out_sz, 0)

                    ! The return code is zero if the frame is complete, but there may
                    ! be multiple frames concatenated together. Zstd will automatically
                    ! reset the context when a frame is complete. Still, calling
                    ! zstd_d_ctx_reset() can be useful to reset the context to a clean
                    ! state, for instance if the last decompression call returned an
                    ! error.
                    stat = zstd_decompress_stream(d_ctx, output, input)

                    if (zstd_is_error(stat)) then
                        print '("zstd_decompress_stream: ", a)', zstd_get_error_name(stat)
                        success = .false.
                        exit
                    end if

                    src2 = src2 // buf_out(:output%pos)
                    last_stat = stat
                end do
            end do

            if (is_empty) then
                print '("input is empty")'
                success = .false.
            end if

            if (last_stat /= 0) then
                ! The last return value from zstd_decompress_stream did not end on a
                ! frame, but we reached the end of the file! We assume this is an
                ! error, and the input was truncated.
                print '("EOF before end of stream: ", i0)', last_stat
                success = .false.
            end if

            stat = zstd_free_d_ctx(d_ctx)
        end block d_block

        print '("src1 length: ", i0)', len(src1)
        print '("src2 length: ", i0)', len(src2)
        print '("dst  length: ", i0)', len(dst)

        if (src1 /= src2) then
            print '("data mismatch")'
            success = .false.
        end if
    end function test_stream
end program main
