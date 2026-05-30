program benchmark_compare
    use iso_c_binding, only: c_char, c_double, c_int32_t, c_null_char, c_size_t
    use iso_fortran_env, only: int64, output_unit, real64
    use fast_float_module, only: outcomes, outcome, parse_double, DEFAULT_PARSING, &
                                  operator(==), operator(/=)
    use fast_float_module, only: parse_double_range_sub, parse_result, parse_double_array, &
                                  parse_double_fast, parse_double_stream
    use ffc_c_bridge, only: ffc_parse_double_c, benchmark_ffc_lines_c
    use str2real_m, only: str2real
    use stdlib_str2num, only: to_num
    implicit none(type, external)

    type :: benchmark_line
        character(:), allocatable :: text
        character(kind=c_char), pointer :: c_text(:) => null()
    end type benchmark_line

    type :: benchmark_file
        character(:), allocatable :: path
    end type benchmark_file

    character(len=1024) :: filename, arg
    integer :: i, nargs, repeat_count, random_count
    logical :: has_file, has_random
    type(benchmark_file), allocatable :: files(:)
    type(benchmark_line), allocatable :: lines(:)
    character(:), allocatable :: packed_text
    character(kind=c_char), allocatable :: packed_data(:)
    integer(c_size_t), allocatable :: lengths(:), offsets(:)
    integer :: nlines, volume

    filename = ""
    has_file = .false.
    has_random = .false.
    repeat_count = 100
    random_count = 100000

    nargs = command_argument_count()
    i = 1
    do while (i <= nargs)
        call get_command_argument(i, arg)
        select case (trim(arg))
        case ("-f", "--file")
            i = i + 1
            call get_command_argument(i, filename)
            has_file = .true.
            call add_file(files, trim(filename))
        case ("-m", "--model")
            has_random = .true.
            ! skip the model name argument (only "uniform" supported)
            if (i + 1 <= nargs) then
                i = i + 1
            end if
        case ("-n", "--volume")
            i = i + 1
            call get_command_argument(i, arg)
            read(arg, *) random_count
        case ("-r", "--repeat")
            i = i + 1
            call get_command_argument(i, arg)
            read(arg, *) repeat_count
        case ("-h", "--help")
            call print_help()
            stop
        end select
        i = i + 1
    end do

    if (.not. has_file .and. .not. has_random) then
        ! Default: run random benchmark
        has_random = .true.
    end if

    if (has_random) then
        call generate_random_lines(random_count, lines, packed_text, packed_data, offsets, lengths, nlines, volume)
        call run_benchmark("random uniform [0,1)", lines, packed_text, packed_data, &
                           offsets, lengths, nlines, volume, repeat_count)
    end if

    if (has_file) then
        do i = 1, size(files)
            call load_lines(files(i)%path, lines, packed_text, packed_data, offsets, lengths, nlines, volume)
            call verify(lines)
            call run_benchmark(files(i)%path, lines, packed_text, packed_data, &
                               offsets, lengths, nlines, volume, repeat_count)
        end do
    end if

contains

    subroutine print_help()
        write(output_unit, "(a)") "Usage: fpm run benchmark_compare -- [options]"
        write(output_unit, "(a)") "  -f <file>    Benchmark a data file (repeatable)"
        write(output_unit, "(a)") "  -m uniform   Benchmark random floats in [0,1) (default if no -f)"
        write(output_unit, "(a)") "  -n <count>   Number of random floats (default: 100000)"
        write(output_unit, "(a)") "  -r <repeat>  Repeat count (default: 100)"
    end subroutine print_help

    subroutine add_file(files, path)
        type(benchmark_file), allocatable, intent(inout) :: files(:)
        character(*), intent(in) :: path
        type(benchmark_file), allocatable :: tmp(:)
        integer :: n

        if (.not. allocated(files)) then
            allocate(files(1))
            files(1)%path = path
            return
        end if

        n = size(files)
        allocate(tmp(n + 1))
        tmp(1:n) = files
        tmp(n + 1)%path = path
        call move_alloc(tmp, files)
    end subroutine add_file

    subroutine generate_random_lines(howmany, lines, packed_text, packed_data, offsets, lengths, nlines, volume)
        integer, intent(in) :: howmany
        type(benchmark_line), allocatable, intent(out) :: lines(:)
        character(:), allocatable, intent(out) :: packed_text
        character(kind=c_char), allocatable, intent(out) :: packed_data(:)
        integer(c_size_t), allocatable, intent(out) :: offsets(:), lengths(:)
        integer, intent(out) :: nlines, volume

        real(real64) :: x
        character(len=24) :: buf
        integer :: i, j, n, cursor, total

        nlines = howmany
        allocate(lines(nlines), offsets(nlines), lengths(nlines))

        ! First pass: generate strings and measure total volume
        total = 0
        call random_seed()
        do i = 1, nlines
            call random_number(x)
            write(buf, '(es23.16)') x
            buf = adjustl(buf)
            n = len_trim(buf)
            lines(i)%text = trim(buf)
            allocate(lines(i)%c_text(n))
            lines(i)%c_text = [(buf(j:j), j = 1, n)]
            total = total + n
        end do
        volume = total

        ! Build packed arrays
        allocate(character(len=max(total, 1)) :: packed_text)
        allocate(packed_data(max(total, 1)))
        cursor = 1
        do i = 1, nlines
            n = len(lines(i)%text)
            offsets(i) = int(cursor - 1, c_size_t)
            lengths(i) = int(n, c_size_t)
            packed_text(cursor:cursor+n-1) = lines(i)%text
            packed_data(cursor:cursor+n-1) = lines(i)%c_text
            cursor = cursor + n
        end do
    end subroutine generate_random_lines

    subroutine load_lines(fname, lines, packed_text, packed_data, offsets, lengths, nlines, volume)
        character(*), intent(in) :: fname
        type(benchmark_line), allocatable, intent(out) :: lines(:)
        character(:), allocatable, intent(out) :: packed_text
        character(kind=c_char), allocatable, intent(out) :: packed_data(:)
        integer(c_size_t), allocatable, intent(out) :: offsets(:), lengths(:)
        integer, intent(out) :: nlines, volume

        character(len=1024) :: line
        integer :: cursor, i, ios, j, n, unit

        open(newunit=unit, file=fname, status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "failed to open benchmark input file"

        nlines = 0
        volume = 0
        do
            read(unit, "(a)", iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            volume = volume + len_trim(line)
        end do

        allocate(lines(nlines))
        allocate(offsets(nlines), lengths(nlines))
        allocate(character(len=max(volume, 1)) :: packed_text)
        allocate(packed_data(max(volume, 1)))
        rewind(unit)

        if (volume == 0) then
            packed_text = ""
            packed_data(1) = c_null_char
        end if
        cursor = 1
        do i = 1, nlines
            read(unit, "(a)", iostat=ios) line
            if (ios /= 0) error stop "failed while reading benchmark input file"

            n = len_trim(line)
            offsets(i) = int(cursor - 1, c_size_t)
            lengths(i) = int(n, c_size_t)
            if (n == 0) then
                lines(i)%text = ""
                allocate(lines(i)%c_text(1))
                lines(i)%c_text(1) = c_null_char
            else
                lines(i)%text = line(:n)
                allocate(lines(i)%c_text(n))
                lines(i)%c_text = [(line(j:j), j=1, n)]
                packed_text(cursor:cursor+n-1) = line(:n)
                packed_data(cursor:cursor+n-1) = lines(i)%c_text
                cursor = cursor + n
            end if
        end do

        close(unit)
    end subroutine load_lines

    subroutine verify(lines)
        type(benchmark_line), intent(in) :: lines(:)

        integer :: i
        integer(c_int32_t) :: c_outcome
        real(c_double) :: c_value
        real(real64) :: f_value
        type(parse_result) :: f_result

        do i = 1, size(lines)
            f_value = parse_double(lines(i)%text, f_result)
            call ffc_parse_double_c(lines(i)%c_text, len(lines(i)%text), c_value, c_outcome)

            if (f_result%outcome%state /= int(c_outcome, kind=1)) then
                write(output_unit, "(a,i0)") "outcome mismatch at line ", i
                error stop 1
            end if

            if (f_result%outcome == outcomes%OK) then
                if (transfer(f_value, 0_int64) /= transfer(real(c_value, real64), 0_int64)) then
                    write(output_unit, "(a,i0)") "value mismatch at line ", i
                    error stop 1
                end if
            end if
        end do
    end subroutine verify

    subroutine run_benchmark(name, lines, packed_text, packed_data, offsets, lengths, nlines, volume, repeat_count)
        character(*), intent(in) :: name
        type(benchmark_line), intent(in) :: lines(:)
        character(*), intent(in) :: packed_text
        character(kind=c_char), intent(in) :: packed_data(:)
        integer(c_size_t), intent(in) :: offsets(:), lengths(:)
        integer, intent(in) :: nlines, volume, repeat_count

        ! Benchmark case indices
        integer, parameter :: B_RSUB = 1, B_LINE = 2, B_FAST = 3
        integer, parameter :: B_STRM = 4, B_STDLIB = 5
        integer, parameter :: B_S2R = 6, B_READ = 7
        integer, parameter :: B_CLOOP = 8, NCASES = 8

        character(len=40), parameter :: labels(NCASES) = [ character(len=40) :: &
            "fortran (fast_float array)", &
            "fortran (fast_float line)", &
            "fortran (fast_float fast)", &
            "fortran (fast_float stream)", &
            "fortran (stdlib to_num)", "fortran (str2real)", &
            "fortran (read *)", &
            "ffc via fortran interop" ]
        character(len=24), parameter :: cksum_labels(NCASES) = [ character(len=24) :: &
            "fast_float arr bits   = ", &
            "fast_float line bits  = ", &
            "fast_float fast bits  = ", &
            "fast_float strm bits  = ", &
            "stdlib checksum bits  = ", "str2real checksum bits= ", &
            "read * checksum bits  = ", &
            "ffc interop bits      = " ]

        real(real64) :: min_ns(NCASES), avg_ns(NCASES), checksum(NCASES)
        integer(int64) :: xor_checksum(NCASES)
        real(real64), volatile :: answer
        real(real64) :: elapsed_ns, volume_mb, x_f
        real(real64) :: x_stdlib, x_str2real, x_read
        real(c_double) :: x_c
        integer :: i, k, r, ios_read, nparse, cursor, consumed
        integer(c_int32_t) :: c_outcome
        integer(int64) :: count_end, count_rate, count_start
        type(parse_result) :: f_result
        type(outcome) :: arr_err, fast_stat
        character(len=:), pointer :: stream_ptr
        ! Pre-computed integer start/end arrays (avoid c_size_t conversion in hot loop)
        integer, allocatable :: istart(:), iend(:)
        real(real64), allocatable :: values(:)
        character(:), allocatable, target :: packed_text_ws

        volume_mb = real(volume, real64) / (1024.0_real64 * 1024.0_real64)
        min_ns = huge(1.0_real64)
        avg_ns = 0.0_real64
        checksum = 0.0_real64

        ! Pre-compute integer offsets to avoid int() conversion in timed loops
        allocate(istart(nlines), iend(nlines), values(nlines))
        do i = 1, nlines
            istart(i) = int(offsets(i)) + 1
            iend(i) = int(offsets(i) + lengths(i))
        end do

        ! Build whitespace-separated text for parse_double_array
        allocate(character(len=volume + nlines) :: packed_text_ws)
        cursor = 1
        do i = 1, nlines
            k = len(lines(i)%text)
            packed_text_ws(cursor:cursor+k-1) = lines(i)%text
            packed_text_ws(cursor+k:cursor+k) = char(10)
            cursor = cursor + k + 1
        end do

        ! ---- XOR checksums (single untimed pass per implementation) ----
        xor_checksum = 0_int64

        ! B_RSUB: parse_double_array — whitespace-separated stream, single call
        call parse_double_array(packed_text_ws, values, nparse, arr_err)
        do i = 1, nparse
            xor_checksum(B_RSUB) = ieor(xor_checksum(B_RSUB), transfer(values(i), 0_int64))
        end do

        ! B_LINE: parse_double_range_sub — per-line via elemental sub (str, first, last)
        do i = 1, nlines
            x_f = parse_double(packed_text(istart(i):iend(i)), f_result)
            xor_checksum(B_LINE) = ieor(xor_checksum(B_LINE), transfer(x_f, 0_int64))
        end do

        ! B_FAST: parse_double_fast — per-line, position-1 interface (str(pos:))
        do i = 1, nlines
            call parse_double_fast(packed_text(istart(i):iend(i)), x_f, consumed, fast_stat)
            xor_checksum(B_FAST) = ieor(xor_checksum(B_FAST), transfer(x_f, 0_int64))
        end do

        ! B_STRM: parse_double_stream — pointer-advancing stream (like stdlib)
        stream_ptr => packed_text_ws(1:len(packed_text_ws))
        do while (len(stream_ptr) > 0)
            call parse_double_stream(stream_ptr, x_f, fast_stat)
            if (fast_stat /= outcomes%OK) exit
            xor_checksum(B_STRM) = ieor(xor_checksum(B_STRM), transfer(x_f, 0_int64))
        end do

        ! B_STDLIB / B_S2R / B_READ / B_CLOOP: third-party & reference implementations
        do i = 1, nlines
            x_stdlib = 0.0_real64
            x_stdlib = to_num(lines(i)%text, x_stdlib)
            xor_checksum(B_STDLIB) = ieor(xor_checksum(B_STDLIB), transfer(x_stdlib, 0_int64))

            x_str2real = str2real(lines(i)%text)
            xor_checksum(B_S2R) = ieor(xor_checksum(B_S2R), transfer(x_str2real, 0_int64))

            read(lines(i)%text, *, iostat=ios_read) x_read
            if (ios_read == 0) &
                xor_checksum(B_READ) = ieor(xor_checksum(B_READ), transfer(x_read, 0_int64))

            call ffc_parse_double_c(lines(i)%c_text, len(lines(i)%text), x_c, c_outcome)
            if (c_outcome == outcomes%OK%state) &
                xor_checksum(B_CLOOP) = ieor(xor_checksum(B_CLOOP), transfer(real(x_c, real64), 0_int64))
        end do

        ! ---- Timed benchmark loops ----
        call system_clock(count_rate=count_rate)

        ! B_RSUB: parse_double_array — single call parses whitespace-separated stream
        do r = 1, repeat_count
            call system_clock(count=count_start)
            call parse_double_array(packed_text_ws, values, nparse, arr_err)
            answer = maxval(values(1:nparse))
            call system_clock(count=count_end)
            call tally(B_RSUB, answer, count_start, count_end, count_rate, &
                        elapsed_ns, avg_ns, min_ns, checksum)
        end do

        ! B_LINE: parse_double_range_sub — per-line elemental (str, first, last)
        do r = 1, repeat_count
            answer = 0.0_real64
            call system_clock(count=count_start)
            do i = 1, nlines
                call parse_double_range_sub(packed_text, istart(i), iend(i), x_f, f_result, DEFAULT_PARSING)
                if (x_f > answer) answer = x_f
            end do
            call system_clock(count=count_end)
            call tally(B_LINE, answer, count_start, count_end, count_rate, &
                        elapsed_ns, avg_ns, min_ns, checksum)
        end do

        ! B_FAST: parse_double_fast — per-line, position-1 pure sub
        do r = 1, repeat_count
            answer = 0.0_real64
            call system_clock(count=count_start)
            do i = 1, nlines
                call parse_double_fast(packed_text(istart(i):iend(i)), x_f, consumed, fast_stat)
                if (x_f > answer) answer = x_f
            end do
            call system_clock(count=count_end)
            call tally(B_FAST, answer, count_start, count_end, count_rate, &
                        elapsed_ns, avg_ns, min_ns, checksum)
        end do

        ! B_STRM: parse_double_stream — pointer-advancing stream interface
        do r = 1, repeat_count
            call system_clock(count=count_start)
            answer = 0.0_real64
            stream_ptr => packed_text_ws(1:len(packed_text_ws))
            do while (len(stream_ptr) > 0)
                call parse_double_stream(stream_ptr, x_f, fast_stat)
                if (fast_stat /= outcomes%OK) exit
                if (x_f > answer) answer = x_f
            end do
            call system_clock(count=count_end)
            call tally(B_STRM, answer, count_start, count_end, count_rate, &
                        elapsed_ns, avg_ns, min_ns, checksum)
        end do

        ! B_STDLIB: stdlib to_num — per-line
        do r = 1, repeat_count
            answer = 0.0_real64
            call system_clock(count=count_start)
            do i = 1, nlines
                x_stdlib = 0.0_real64
                x_stdlib = to_num(lines(i)%text, x_stdlib)
                if (x_stdlib > answer) answer = x_stdlib
            end do
            call system_clock(count=count_end)
            call tally(B_STDLIB, answer, count_start, count_end, count_rate, &
                        elapsed_ns, avg_ns, min_ns, checksum)
        end do

        ! B_S2R: str2real — per-line
        do r = 1, repeat_count
            answer = 0.0_real64
            call system_clock(count=count_start)
            do i = 1, nlines
                x_str2real = str2real(lines(i)%text)
                if (x_str2real > answer) answer = x_str2real
            end do
            call system_clock(count=count_end)
            call tally(B_S2R, answer, count_start, count_end, count_rate, &
                        elapsed_ns, avg_ns, min_ns, checksum)
        end do

        ! B_READ: Fortran intrinsic read — per-line baseline
        do r = 1, repeat_count
            answer = 0.0_real64
            call system_clock(count=count_start)
            do i = 1, nlines
                read(lines(i)%text, *, iostat=ios_read) x_read
                if (ios_read /= 0) cycle
                if (x_read > answer) answer = x_read
            end do
            call system_clock(count=count_end)
            call tally(B_READ, answer, count_start, count_end, count_rate, &
                        elapsed_ns, avg_ns, min_ns, checksum)
        end do

        ! B_CLOOP: C ffc.h via Fortran interop — reference implementation
        do r = 1, repeat_count
            call system_clock(count=count_start)
            call benchmark_ffc_lines_c(packed_data, offsets, lengths, nlines, x_c, c_outcome)
            answer = x_c
            call system_clock(count=count_end)
            call tally(B_CLOOP, answer, count_start, count_end, count_rate, &
                        elapsed_ns, avg_ns, min_ns, checksum)
        end do

        deallocate(istart, iend)
        avg_ns = avg_ns / real(repeat_count, real64)

        write(output_unit, "(a)") ""
        write(output_unit, "(a)") "# file=" // trim(name)
        write(output_unit, "(a,i0,a,f0.6,a)") "# lines=", nlines, " volume=", volume_mb, " MB"
        do k = 1, NCASES
            call print_result(labels(k), volume_mb, nlines, min_ns(k), avg_ns(k))
        end do
        do k = 1, NCASES
            write(output_unit, "(a,z16.16)") cksum_labels(k), xor_checksum(k)
        end do
    end subroutine run_benchmark

    subroutine tally(idx, cksum_val, t0, t1, rate, elapsed_ns, avg_ns, min_ns, checksum)
        integer, intent(in) :: idx
        real(real64), intent(in) :: cksum_val
        integer(int64), intent(in) :: t0, t1, rate
        real(real64), intent(out) :: elapsed_ns
        real(real64), intent(inout) :: avg_ns(:), min_ns(:), checksum(:)
        elapsed_ns = real(t1 - t0, real64) / real(rate, real64) * 1.0e9_real64
        avg_ns(idx) = avg_ns(idx) + elapsed_ns
        if (elapsed_ns < min_ns(idx)) min_ns(idx) = elapsed_ns
        checksum(idx) = cksum_val
    end subroutine tally

    subroutine print_result(name, volume_mb, nlines, min_ns, avg_ns)
        character(*), intent(in) :: name
        real(real64), intent(in) :: volume_mb, min_ns, avg_ns
        integer, intent(in) :: nlines

        write(output_unit, "(a40,a,f8.2,a,f5.1,a,f8.2,a)") &
            name, ": ", &
            volume_mb * 1.0e9_real64 / min_ns, " MB/s (+/- ", &
            (avg_ns - min_ns) * 100.0_real64 / avg_ns, " %) ", &
            real(nlines, real64) * 1.0e3_real64 / min_ns, " Mfloat/s  "
    end subroutine print_result

end program benchmark_compare
