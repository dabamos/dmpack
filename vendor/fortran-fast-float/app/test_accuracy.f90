!> @brief Accuracy tests against supplemental hex-encoded reference data.
module test_accuracy_driver
    use iso_fortran_env, only: int32, int64, real32, real64, stderr => error_unit, &
                               stdout => output_unit
    use fast_float_module, only: parse_double, parse_float, parse_result, outcome, outcomes, &
                                  operator(==), operator(/=)
    implicit none(type, external)
    private

    public :: accuracy_tests

    integer, parameter :: MAX_LINE = 2048
    integer, parameter :: MAX_FAIL_PRINT = 20

    character(*), parameter :: this_test = '[accuracy]'
    logical, parameter :: verbose = .true.

    ! Core file set (~218k lines)
    integer, parameter :: N_CORE = 7
    character(len=40), parameter :: CORE_FILES(N_CORE) = [ character(len=40) :: &
        'more-test-cases.txt',           &
        'lemire-fast-float.txt',         &
        'freetype-2-7.txt',              &
        'tencent-rapidjson.txt',         &
        'google-wuffs.txt',              &
        'lemire-fast-double-parser.txt', &
        'ibm-fpgen.txt'                  ]

    ! Additional large files for --all
    integer, parameter :: N_EXTRA = 5
    character(len=40), parameter :: EXTRA_FILES(N_EXTRA) = [ character(len=40) :: &
        'ulfjack-ryu.txt',               &
        'google-double-conversion.txt',  &
        'remyoudompheng-fptest-0.txt',   &
        'remyoudompheng-fptest-1.txt',   &
        'remyoudompheng-fptest-2.txt'    ]

contains

    subroutine accuracy_tests(nfailed, npassed)
        integer, intent(out) :: nfailed, npassed

        character(len=1024) :: data_dir, single_file
        logical :: use_dir, use_file, use_all, do_float32
        integer :: file_pass, file_fail, i

        nfailed = 0
        npassed = 0

        call parse_cli(data_dir, single_file, use_dir, use_file, use_all, do_float32)

        if (use_file) then
            call run_file_test(trim(single_file), do_float32, file_pass, file_fail)
            npassed = npassed + file_pass
            nfailed = nfailed + file_fail
        else if (use_dir) then
            do i = 1, N_CORE
                call run_file_test(trim(data_dir) // '/' // trim(CORE_FILES(i)), &
                                   do_float32, file_pass, file_fail)
                npassed = npassed + file_pass
                nfailed = nfailed + file_fail
            end do
            if (use_all) then
                do i = 1, N_EXTRA
                    call run_file_test(trim(data_dir) // '/' // trim(EXTRA_FILES(i)), &
                                       do_float32, file_pass, file_fail)
                    npassed = npassed + file_pass
                    nfailed = nfailed + file_fail
                end do
            end if
        else
            write(stderr, '(a)') 'Usage: test_accuracy -d <data_dir> [--all] [--float32]'
            write(stderr, '(a)') '       test_accuracy -f <single_file> [--float32]'
            return
        end if

    end subroutine accuracy_tests

    !> Parse command-line arguments.
    subroutine parse_cli(data_dir, single_file, use_dir, use_file, use_all, do_float32)
        character(*), intent(out) :: data_dir, single_file
        logical, intent(out) :: use_dir, use_file, use_all, do_float32

        integer :: nargs, iarg
        character(len=1024) :: arg

        data_dir = ''
        single_file = ''
        use_dir = .false.
        use_file = .false.
        use_all = .false.
        do_float32 = .false.

        nargs = command_argument_count()
        iarg = 1
        do while (iarg <= nargs)
            call get_command_argument(iarg, arg)
            select case (trim(arg))
            case ('-d')
                if (iarg + 1 > nargs) then
                    write(stderr, '(a)') 'ERROR: -d requires a directory argument'
                    error stop 1
                end if
                iarg = iarg + 1
                call get_command_argument(iarg, data_dir)
                use_dir = .true.
            case ('-f')
                if (iarg + 1 > nargs) then
                    write(stderr, '(a)') 'ERROR: -f requires a file argument'
                    error stop 1
                end if
                iarg = iarg + 1
                call get_command_argument(iarg, single_file)
                use_file = .true.
            case ('--all')
                use_all = .true.
            case ('--float32')
                do_float32 = .true.
            case default
                write(stderr, '(a,a)') 'WARNING: unknown argument: ', trim(arg)
            end select
            iarg = iarg + 1
        end do
    end subroutine parse_cli

    !> Extract the filename component from a full path.
    function basename(path) result(name)
        character(*), intent(in) :: path
        character(len=:), allocatable :: name
        integer :: k

        k = index(path, '/', back=.true.)
        if (k == 0) k = index(path, '\', back=.true.)
        if (k == 0) then
            name = trim(path)
        else
            name = trim(path(k+1:))
        end if
    end function basename

    !> Test all lines in a single data file.
    subroutine run_file_test(filepath, do_float32, file_pass, file_fail)
        character(*), intent(in) :: filepath
        logical, intent(in) :: do_float32
        integer, intent(out) :: file_pass, file_fail

        integer :: unit_num, ios, line_num, fail_printed
        character(len=MAX_LINE) :: line
        character(len=:), allocatable :: display_name

        ! Hex tokens
        character(len=4)  :: hex_f16
        character(len=8)  :: hex_f32
        character(len=16) :: hex_f64
        character(len=MAX_LINE) :: num_string

        ! Expected and actual bits
        integer(int64) :: expected_f64_bits, actual_f64_bits
        integer(int32) :: expected_f32_bits, actual_f32_bits

        ! Parsed values
        real(real64) :: dval
        real(real32) :: fval
        type(parse_result) :: res

        logical :: line_ok

        file_pass = 0
        file_fail = 0
        fail_printed = 0
        display_name = basename(filepath)

        ! Check file exists
        inquire(file=filepath, iostat=ios)
        if (ios /= 0) then
            write(stderr, '(a,a)') 'WARNING: cannot access file: ', trim(filepath)
            return
        end if

        open(newunit=unit_num, file=filepath, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write(stderr, '(a,a)') 'WARNING: cannot open file: ', trim(filepath)
            return
        end if

        write(stdout, '(a,a,a)', advance='no') 'testing ', display_name, ' ... '
        flush(stdout)

        line_num = 0
        do
            read(unit_num, '(a)', iostat=ios) line
            if (ios /= 0) exit
            line_num = line_num + 1

            if (len_trim(line) == 0) cycle

            call split_data_line(line, hex_f16, hex_f32, hex_f64, num_string, ios)
            if (ios /= 0) then
                file_fail = file_fail + 1
                if (verbose .and. fail_printed < MAX_FAIL_PRINT) then
                    write(stdout, '()')
                    write(stdout, '(a,i0,a,a)') '  FAIL line ', line_num, &
                        ': malformed line: ', trim(line)
                    fail_printed = fail_printed + 1
                end if
                cycle
            end if

            line_ok = .true.

            ! --- Test float64 ---
            read(hex_f64, '(Z16)', iostat=ios) expected_f64_bits
            if (ios /= 0) then
                file_fail = file_fail + 1
                if (verbose .and. fail_printed < MAX_FAIL_PRINT) then
                    write(stdout, '()')
                    write(stdout, '(a,i0,a,a)') '  FAIL line ', line_num, &
                        ': bad f64 hex: ', hex_f64
                    fail_printed = fail_printed + 1
                end if
                cycle
            end if

            dval = parse_double(trim(num_string), res)
            if (res%outcome /= outcomes%OK .and. res%outcome /= outcomes%OUT_OF_RANGE) then
                line_ok = .false.
                if (verbose .and. fail_printed < MAX_FAIL_PRINT) then
                    write(stdout, '()')
                    write(stdout, '(a,i0,a,a,a,i0)') '  FAIL line ', line_num, &
                        ': parse_double failed for "', trim(num_string), &
                        '" outcome=', res%outcome%state
                    fail_printed = fail_printed + 1
                end if
            else
                actual_f64_bits = transfer(dval, 0_int64)
                if (actual_f64_bits /= expected_f64_bits) then
                    line_ok = .false.
                    if (verbose .and. fail_printed < MAX_FAIL_PRINT) then
                        write(stdout, '()')
                        write(stdout, '(a,i0,a,a,a,Z16.16,a,Z16.16)') '  FAIL line ', line_num, &
                            ': "', trim(num_string), '" expected=0x', expected_f64_bits, &
                            ' got=0x', actual_f64_bits
                        fail_printed = fail_printed + 1
                    end if
                end if
            end if

            ! --- Test float32 (optional) ---
            if (do_float32 .and. line_ok) then
                read(hex_f32, '(Z8)', iostat=ios) expected_f32_bits
                if (ios /= 0) then
                    line_ok = .false.
                    if (verbose .and. fail_printed < MAX_FAIL_PRINT) then
                        write(stdout, '()')
                        write(stdout, '(a,i0,a,a)') '  FAIL line ', line_num, &
                            ': bad f32 hex: ', hex_f32
                        fail_printed = fail_printed + 1
                    end if
                else
                    fval = parse_float(trim(num_string), res)
                    if (res%outcome /= outcomes%OK .and. res%outcome /= outcomes%OUT_OF_RANGE) then
                        line_ok = .false.
                        if (verbose .and. fail_printed < MAX_FAIL_PRINT) then
                            write(stdout, '()')
                            write(stdout, '(a,i0,a,a,a,i0)') '  FAIL line ', line_num, &
                                ': parse_float failed for "', trim(num_string), &
                                '" outcome=', res%outcome%state
                            fail_printed = fail_printed + 1
                        end if
                    else
                        actual_f32_bits = transfer(fval, 0_int32)
                        if (actual_f32_bits /= expected_f32_bits) then
                            line_ok = .false.
                            if (verbose .and. fail_printed < MAX_FAIL_PRINT) then
                                write(stdout, '()')
                                write(stdout, '(a,i0,a,a,a,Z8.8,a,Z8.8)') '  FAIL line ', line_num, &
                                    ': f32 "', trim(num_string), '" expected=0x', expected_f32_bits, &
                                    ' got=0x', actual_f32_bits
                                fail_printed = fail_printed + 1
                            end if
                        end if
                    end if
                end if
            end if

            if (line_ok) then
                file_pass = file_pass + 1
            else
                file_fail = file_fail + 1
            end if
        end do

        close(unit_num)

        if (file_fail == 0) then
            write(stdout, '(i0,a,i0,a)') file_pass, '/', file_pass, ' passed'
        else
            write(stdout, '()')
            write(stdout, '(a,i0,a,i0,a,i0,a)') '  ', file_pass, '/', &
                file_pass + file_fail, ' passed (', file_fail, ' FAILED)'
            if (fail_printed >= MAX_FAIL_PRINT) &
                write(stdout, '(a,i0,a)') '  ... and ', file_fail - fail_printed, ' more failures'
        end if
    end subroutine run_file_test

    !> Split a data line into its 4 tokens: f16_hex(4), f32_hex(8), f64_hex(16), string(rest).
    subroutine split_data_line(line, hex_f16, hex_f32, hex_f64, num_string, ios)
        character(*), intent(in) :: line
        character(4), intent(out) :: hex_f16
        character(8), intent(out) :: hex_f32
        character(16), intent(out) :: hex_f64
        character(*), intent(out) :: num_string
        integer, intent(out) :: ios

        integer :: pos, line_len

        ios = 0
        line_len = len_trim(line)

        ! Minimum: 4 + 1 + 8 + 1 + 16 + 1 + 1 = 32
        if (line_len < 32) then
            ios = -1
            return
        end if

        hex_f16 = line(1:4)
        if (line(5:5) /= ' ') then; ios = -1; return; end if

        hex_f32 = line(6:13)
        if (line(14:14) /= ' ') then; ios = -1; return; end if

        hex_f64 = line(15:30)
        if (line(31:31) /= ' ') then; ios = -1; return; end if

        ! Numeric string: everything from position 32 onward (skip leading spaces)
        pos = 32
        do while (pos <= line_len .and. line(pos:pos) == ' ')
            pos = pos + 1
        end do

        if (pos > line_len) then
            ios = -1
            return
        end if

        num_string = line(pos:line_len)
    end subroutine split_data_line

end module test_accuracy_driver

program test_accuracy
    use test_accuracy_driver, only: accuracy_tests
    implicit none(type, external)

    integer :: nfailed, npassed
    character(*), parameter :: stats = "(1x,'[',a,']',t50,' has ',i0,' tests passed, ',i0,' failed.')"

    call accuracy_tests(nfailed, npassed)

    print stats, 'accuracy', npassed, nfailed

    if (nfailed > 0) error stop 1

end program test_accuracy
