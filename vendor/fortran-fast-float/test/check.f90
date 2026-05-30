!> @brief Unit tests for fast_float parsing routines.
module test_fast_float_parsing
    use iso_fortran_env, only: int32, int64, real32, real64
    use ieee_arithmetic, only: ieee_is_nan
    use fast_float_module
    implicit none(type, external)
    private

    public :: parsing_tests

    character(*), parameter :: this_test = '[fast_float]'
    logical, parameter :: verbose = .true.

contains

    subroutine parsing_tests(nfailed, npassed)
        integer, intent(out) :: nfailed, npassed

        nfailed = 0
        npassed = 0

        call add_test(test_dp_zeros(),         '[parse] dp zeros')
        call add_test(test_dp_signed_zeros(),  '[parse] dp signed zeros')
        call add_test(test_dp_infinity(),      '[parse] dp infinity')
        call add_test(test_dp_nan(),           '[parse] dp NaN')
        call add_test(test_dp_subnormals(),    '[parse] dp subnormal boundaries')
        call add_test(test_dp_max(),           '[parse] dp DBL_MAX vicinity')
        call add_test(test_dp_underflow(),     '[parse] dp underflow')
        call add_test(test_dp_extreme_exp(),   '[parse] dp extreme exponents')
        call add_test(test_dp_powers_of_10(),  '[parse] dp powers of 10')
        call add_test(test_dp_pi(),            '[parse] dp pi 100 digits')
        call add_test(test_dp_normal_values(), '[parse] dp normal values')
        call add_test(test_integers(),         '[parse] integer parsing')
        call add_test(test_fp32(),             '[parse] float32')
        call add_test(test_fortran_format(),   '[parse] Fortran d/D exponent')
        call add_test(test_json_format(),      '[parse] JSON format')
        call add_test(test_malformed(),        '[parse] malformed input')
        call add_test(test_custom_decimal(),   '[parse] custom decimal point')
        call add_test(test_dp_stream_cursor_semantics(), '[parse] dp stream cursor semantics')
        call add_test(test_dp_array_invalid(), '[parse] dp array invalid handling')
        call add_test(test_fp_array_invalid(), '[parse] fp array invalid handling')
        call add_test(test_dp_array_hard_cases(), '[parse] dp array hard-case parity')
        call add_test(test_dp_array_out_of_range(), '[parse] dp array out-of-range contract')
        call add_test(test_dp_stream_hard_cases(), '[parse] dp stream hard-case parity')

        return

    contains

        subroutine add_test(success_flag, msg)
            logical, intent(in) :: success_flag
            character(*), intent(in) :: msg
            if (success_flag) then
                npassed = npassed + 1
            else
                nfailed = nfailed + 1
                if (verbose) print '(1x,a,a,a)', this_test, ' FAILED: ', msg
            end if
        end subroutine add_test

    end subroutine parsing_tests

    ! -----------------------------------------------------------------------
    !  Helpers: return .true. if the parsed result matches the expected value
    ! -----------------------------------------------------------------------

    !> Match parse_double result against expected IEEE 754 bit pattern.
    logical function match_dp(str, expected_bits, expected_outcome) result(ok)
        character(*), intent(in) :: str
        integer(int64), intent(in) :: expected_bits
        type(outcome), intent(in) :: expected_outcome

        real(real64) :: val
        type(parse_result) :: res
        integer(int64) :: got_bits

        val = parse_double(str, res)
        got_bits = transfer(val, 0_int64)
        ok = got_bits == expected_bits .and. res%outcome == expected_outcome

        if (.not. ok .and. verbose) then
            print '(a,a,a)', '    input: "', str, '"'
            if (got_bits /= expected_bits) &
                print '(a,Z16.16,a,Z16.16)', '    bits: expected=', expected_bits, ' got=', got_bits
            if (res%outcome /= expected_outcome) &
                print '(a,I0,a,I0)', '    outcome: expected=', expected_outcome%state, ' got=', res%outcome%state
        end if
    end function match_dp

    !> Match parse_double with options against expected bit pattern.
    logical function match_dp_opts(str, opts, expected_bits, expected_outcome) result(ok)
        character(*), intent(in) :: str
        type(parse_options), intent(in) :: opts
        integer(int64), intent(in) :: expected_bits
        type(outcome), intent(in) :: expected_outcome

        real(real64) :: val
        type(parse_result) :: res
        integer(int64) :: got_bits

        val = parse_double(str, opts, res)
        got_bits = transfer(val, 0_int64)
        ok = got_bits == expected_bits .and. res%outcome == expected_outcome

        if (.not. ok .and. verbose) then
            print '(a,a,a)', '    input: "', str, '"'
            if (got_bits /= expected_bits) &
                print '(a,Z16.16,a,Z16.16)', '    bits: expected=', expected_bits, ' got=', got_bits
            if (res%outcome /= expected_outcome) &
                print '(a,I0,a,I0)', '    outcome: expected=', expected_outcome%state, ' got=', res%outcome%state
        end if
    end function match_dp_opts

    !> Match parse_float result against expected IEEE 754 bit pattern.
    logical function match_fp(str, expected_bits, expected_outcome) result(ok)
        character(*), intent(in) :: str
        integer(int32), intent(in) :: expected_bits
        type(outcome), intent(in) :: expected_outcome

        real(real32) :: val
        type(parse_result) :: res
        integer(int32) :: got_bits

        val = parse_float(str, res)
        got_bits = transfer(val, 0_int32)
        ok = got_bits == expected_bits .and. res%outcome == expected_outcome

        if (.not. ok .and. verbose) then
            print '(a,a,a)', '    input: "', str, '"'
            if (got_bits /= expected_bits) &
                print '(a,Z8.8,a,Z8.8)', '    bits: expected=', expected_bits, ' got=', got_bits
            if (res%outcome /= expected_outcome) &
                print '(a,I0,a,I0)', '    outcome: expected=', expected_outcome%state, ' got=', res%outcome%state
        end if
    end function match_fp

    !> Check parse_double returns NaN.
    logical function match_dp_nan(str) result(ok)
        character(*), intent(in) :: str

        real(real64) :: val
        type(parse_result) :: res

        val = parse_double(str, res)
        ok = ieee_is_nan(val) .and. res%outcome == outcomes%OK

        if (.not. ok .and. verbose) then
            print '(a,a,a)', '    input: "', str, '" expected NaN'
            if (.not. ieee_is_nan(val)) &
                print '(a,Z16.16)', '    got bits=', transfer(val, 0_int64)
            if (res%outcome /= outcomes%OK) &
                print '(a,I0)', '    outcome=', res%outcome%state
        end if
    end function match_dp_nan

    !> Check parse_float returns NaN.
    logical function match_fp_nan(str) result(ok)
        character(*), intent(in) :: str

        real(real32) :: val
        type(parse_result) :: res

        val = parse_float(str, res)
        ok = ieee_is_nan(val) .and. res%outcome == outcomes%OK

        if (.not. ok .and. verbose) then
            print '(a,a,a)', '    input: "', str, '" expected NaN'
            if (.not. ieee_is_nan(val)) &
                print '(a,Z8.8)', '    got bits=', transfer(val, 0_int32)
            if (res%outcome /= outcomes%OK) &
                print '(a,I0)', '    outcome=', res%outcome%state
        end if
    end function match_fp_nan

    !> Check parse_double returns outcomes%INVALID_INPUT.
    logical function match_dp_invalid(str) result(ok)
        character(*), intent(in) :: str

        real(real64) :: val
        type(parse_result) :: res

        val = parse_double(str, res)
        ok = res%outcome == outcomes%INVALID_INPUT

        if (.not. ok .and. verbose) &
            print '(a,a,a,I0)', '    input: "', str, '" expected INVALID, got outcome=', res%outcome%state
    end function match_dp_invalid

    !> Check parse_double with options returns outcomes%INVALID_INPUT.
    logical function match_dp_opts_invalid(str, opts) result(ok)
        character(*), intent(in) :: str
        type(parse_options), intent(in) :: opts

        real(real64) :: val
        type(parse_result) :: res

        val = parse_double(str, opts, res)
        ok = res%outcome == outcomes%INVALID_INPUT

        if (.not. ok .and. verbose) &
            print '(a,a,a,I0)', '    input: "', str, '" expected INVALID, got outcome=', res%outcome%state
    end function match_dp_opts_invalid

    !> Check parse_float returns outcomes%INVALID_INPUT.
    logical function match_fp_invalid(str) result(ok)
        character(*), intent(in) :: str

        real(real32) :: val
        type(parse_result) :: res

        val = parse_float(str, res)
        ok = res%outcome == outcomes%INVALID_INPUT

        if (.not. ok .and. verbose) &
            print '(a,a,a,I0)', '    input: "', str, '" expected INVALID, got outcome=', res%outcome%state
    end function match_fp_invalid

    !> Match parse_i64 against expected value.
    logical function match_i64(str, base, expected_val, expected_outcome) result(ok)
        character(*), intent(in) :: str
        integer, intent(in) :: base
        integer(int64), intent(in) :: expected_val
        type(outcome), intent(in) :: expected_outcome

        integer(int64) :: val
        type(parse_result) :: res

        val = parse_i64(str, base, res)
        ok = res%outcome == expected_outcome .and. &
             (res%outcome /= outcomes%OK .or. val == expected_val)

        if (.not. ok .and. verbose) then
            print '(a,a,a,I0,a)', '    input: "', str, '" base=', base
            if (res%outcome /= expected_outcome) &
                print '(a,I0,a,I0)', '    outcome: expected=', expected_outcome%state, ' got=', res%outcome%state
            if (res%outcome == outcomes%OK .and. val /= expected_val) &
                print '(a,I0,a,I0)', '    value: expected=', expected_val, ' got=', val
        end if
    end function match_i64

    !> Match parse_i32 against expected value.
    logical function match_i32(str, base, expected_val, expected_outcome) result(ok)
        character(*), intent(in) :: str
        integer, intent(in) :: base
        integer(int32), intent(in) :: expected_val
        type(outcome), intent(in) :: expected_outcome

        integer(int32) :: val
        type(parse_result) :: res

        val = parse_i32(str, base, res)
        ok = res%outcome == expected_outcome .and. &
             (res%outcome /= outcomes%OK .or. val == expected_val)

        if (.not. ok .and. verbose) then
            print '(a,a,a,I0,a)', '    input: "', str, '" base=', base
            if (res%outcome /= expected_outcome) &
                print '(a,I0,a,I0)', '    outcome: expected=', expected_outcome%state, ' got=', res%outcome%state
            if (res%outcome == outcomes%OK .and. val /= expected_val) &
                print '(a,I0,a,I0)', '    value: expected=', expected_val, ' got=', val
        end if
    end function match_i32

    ! -----------------------------------------------------------------------
    !  Test functions: each returns .true. if all assertions pass
    ! -----------------------------------------------------------------------

    logical function test_dp_zeros() result(success)
        success = match_dp("0",      int(z'0000000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("0.0",    int(z'0000000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp(".0",     int(z'0000000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("0.",     int(z'0000000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("00.00",  int(z'0000000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("0e0",    int(z'0000000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("0e9999", int(z'0000000000000000', int64), outcomes%OK)
    end function test_dp_zeros

    logical function test_dp_signed_zeros() result(success)
        success = match_dp("-0",   int(z'8000000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("-0.0", int(z'8000000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("-0e0", int(z'8000000000000000', int64), outcomes%OK)
    end function test_dp_signed_zeros

    logical function test_dp_infinity() result(success)
        integer(int64), parameter :: POS_INF = int(z'7FF0000000000000', int64)
        integer(int64), parameter :: NEG_INF = int(z'FFF0000000000000', int64)

        success = match_dp("inf",       POS_INF, outcomes%OK); if (.not. success) return
        success = match_dp("INF",       POS_INF, outcomes%OK); if (.not. success) return
        success = match_dp("infinity",  POS_INF, outcomes%OK); if (.not. success) return
        success = match_dp("Infinity",  POS_INF, outcomes%OK); if (.not. success) return
        success = match_dp("+inf",      POS_INF, outcomes%OK); if (.not. success) return
        success = match_dp("+infinity", POS_INF, outcomes%OK); if (.not. success) return
        success = match_dp("-inf",      NEG_INF, outcomes%OK); if (.not. success) return
        success = match_dp("-infinity", NEG_INF, outcomes%OK); if (.not. success) return
        success = match_dp("-Infinity", NEG_INF, outcomes%OK)
    end function test_dp_infinity

    logical function test_dp_nan() result(success)
        success = match_dp_nan("nan"); if (.not. success) return
        success = match_dp_nan("NaN"); if (.not. success) return
        success = match_dp_nan("NAN")
    end function test_dp_nan

    logical function test_dp_subnormals() result(success)
        ! Largest subnormal
        success = match_dp("2.2250738585072009e-308", &
            int(z'000FFFFFFFFFFFFF', int64), outcomes%OK)
        if (.not. success) return
        ! Smallest normal
        success = match_dp("2.2250738585072014e-308", &
            int(z'0010000000000000', int64), outcomes%OK)
        if (.not. success) return
        ! Smallest subnormal
        success = match_dp("5e-324", &
            int(z'0000000000000001', int64), outcomes%OK)
    end function test_dp_subnormals

    logical function test_dp_max() result(success)
        ! DBL_MAX
        success = match_dp("1.7976931348623157e308", &
            int(z'7FEFFFFFFFFFFFFF', int64), outcomes%OK)
        if (.not. success) return
        ! Overflow to +inf
        success = match_dp("1.7976931348623159e308", &
            int(z'7FF0000000000000', int64), outcomes%OUT_OF_RANGE)
    end function test_dp_max

    logical function test_dp_underflow() result(success)
        ! Underflow to zero
        success = match_dp("1e-324", &
            int(z'0000000000000000', int64), outcomes%OUT_OF_RANGE)
        if (.not. success) return
        ! Rounds to smallest subnormal
        success = match_dp("3e-324", &
            int(z'0000000000000001', int64), outcomes%OK)
    end function test_dp_underflow

    logical function test_dp_extreme_exp() result(success)
        ! Extreme underflow
        success = match_dp("1e-500", &
            int(z'0000000000000000', int64), outcomes%OUT_OF_RANGE)
        if (.not. success) return
        ! Extreme overflow
        success = match_dp("1e500", &
            int(z'7FF0000000000000', int64), outcomes%OUT_OF_RANGE)
    end function test_dp_extreme_exp

    logical function test_dp_powers_of_10() result(success)
        success = match_dp("1e0",   int(z'3FF0000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e1",   int(z'4024000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e-1",  int(z'3FB999999999999A', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e-2",  int(z'3F847AE147AE147B', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e2",   int(z'4059000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e3",   int(z'408F400000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e5",   int(z'40F86A0000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e-5",  int(z'3EE4F8B588E368F1', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e10",  int(z'4202A05F20000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e-10", int(z'3DDB7CDFD9D7BDBB', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e15",  int(z'430C6BF526340000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e20",  int(z'4415AF1D78B58C40', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e-15", int(z'3CD203AF9EE75616', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e-20", int(z'3BC79CA10C924223', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e25",  int(z'45208B2A2C280291', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e50",  int(z'4A511B0EC57E649A', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e100", int(z'54B249AD2594C37D', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e200", int(z'6974E718D7D7625A', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e300", int(z'7E37E43C8800759C', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e-100",int(z'2B2BFF2EE48E0530', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e-200",int(z'16687E92154EF7AC', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1e-300",int(z'01A56E1FC2F8F359', int64), outcomes%OK)
    end function test_dp_powers_of_10

    logical function test_dp_pi() result(success)
        success = match_dp( &
            "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679", &
            int(z'400921FB54442D18', int64), outcomes%OK)
    end function test_dp_pi

    logical function test_dp_normal_values() result(success)
        success = match_dp("1.0",      int(z'3FF0000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("-1.0",     int(z'BFF0000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("2.0",      int(z'4000000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("0.5",      int(z'3FE0000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("-0.5",     int(z'BFE0000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("0.25",     int(z'3FD0000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("-2.5",     int(z'C004000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("3.14",     int(z'40091EB851EB851F', int64), outcomes%OK); if (.not. success) return
        success = match_dp("7.0",      int(z'401C000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("10",       int(z'4024000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("-10",      int(z'C024000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("42.0",     int(z'4045000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("-42.0",    int(z'C045000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("100",      int(z'4059000000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("0.1",      int(z'3FB999999999999A', int64), outcomes%OK); if (.not. success) return
        success = match_dp("0.01",     int(z'3F847AE147AE147B', int64), outcomes%OK); if (.not. success) return
        success = match_dp("0.001",    int(z'3F50624DD2F1A9FC', int64), outcomes%OK); if (.not. success) return
        success = match_dp("123.456",  int(z'405EDD2F1A9FBE77', int64), outcomes%OK); if (.not. success) return
        success = match_dp("999.999",  int(z'408F3FFDF3B645A2', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1000000",  int(z'412E848000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1.23e4",   int(z'40C8060000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("-1.23e4",  int(z'C0C8060000000000', int64), outcomes%OK); if (.not. success) return
        success = match_dp("1.23e-4",  int(z'3F201F31F46ED246', int64), outcomes%OK)
    end function test_dp_normal_values

    logical function test_integers() result(success)
        ! Base 10
        success = match_i64("0",     10, 0_int64,     outcomes%OK); if (.not. success) return
        success = match_i64("12345", 10, 12345_int64, outcomes%OK); if (.not. success) return
        success = match_i64("-42",   10, -42_int64,   outcomes%OK); if (.not. success) return
        success = match_i64("-1",    10, -1_int64,    outcomes%OK); if (.not. success) return
        success = match_i64("1000000", 10, 1000000_int64, outcomes%OK); if (.not. success) return
        ! Base 16
        success = match_i64("FF",       16, 255_int64,        outcomes%OK); if (.not. success) return
        success = match_i64("ff",       16, 255_int64,        outcomes%OK); if (.not. success) return
        success = match_i64("7FFFFFFF", 16, 2147483647_int64, outcomes%OK); if (.not. success) return
        success = match_i64("10",       16, 16_int64,         outcomes%OK); if (.not. success) return
        ! Base 2
        success = match_i64("101010",  2, 42_int64,  outcomes%OK); if (.not. success) return
        success = match_i64("11111111",2, 255_int64, outcomes%OK); if (.not. success) return
        success = match_i64("0",       2, 0_int64,   outcomes%OK); if (.not. success) return
        ! Base 8
        success = match_i64("777", 8, 511_int64, outcomes%OK); if (.not. success) return
        success = match_i64("77",  8, 63_int64,  outcomes%OK); if (.not. success) return
        ! i32
        success = match_i32("12345",      10, 12345_int32,      outcomes%OK); if (.not. success) return
        success = match_i32("-1",         10, -1_int32,         outcomes%OK); if (.not. success) return
        success = match_i32("0",          10, 0_int32,          outcomes%OK); if (.not. success) return
        success = match_i32("2147483647", 10, 2147483647_int32, outcomes%OK); if (.not. success) return
        success = match_i32("7F",         16, 127_int32,        outcomes%OK); if (.not. success) return
        ! Overflow
        success = match_i32("DEADBEEF", 16, 0_int32, outcomes%OUT_OF_RANGE); if (.not. success) return
        success = match_i64("9999999999999999999", 10, 0_int64, outcomes%OUT_OF_RANGE); if (.not. success) return
        ! Options: PRESET_FORTRAN enables FMT_SKIP_WS and FMT_ALLOW_PLUS, so the
        ! integer parser tolerates leading whitespace and a leading '+'.
        block
            type(parse_options) :: FORT
            type(parse_result) :: res
            integer(int64) :: v64
            integer(int32) :: v32
            FORT = parse_options(PRESET_FORTRAN, '.')
            v64 = parse_i64("   +42", 10, res, FORT)
            success = res%outcome == outcomes%OK .and. v64 == 42_int64; if (.not. success) return
            v32 = parse_i32("  +1234", 10, res, FORT)
            success = res%outcome == outcomes%OK .and. v32 == 1234_int32; if (.not. success) return
            ! Without options, a leading '+' is rejected.
            v64 = parse_i64("+42", 10, res)
            success = res%outcome == outcomes%INVALID_INPUT
        end block
    end function test_integers

    logical function test_fp32() result(success)
        integer(int32), parameter :: POS_INF = int(z'7F800000', int32)
        integer(int32), parameter :: NEG_INF = int(z'FF800000', int32)

        success = match_fp("0",   int(z'00000000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("1.0", int(z'3F800000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("0.0", int(z'00000000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("-0",  int(z'80000000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("-0.0",int(z'80000000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("-1.0",int(z'BF800000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("0.5", int(z'3F000000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("0.25",int(z'3E800000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("2.0", int(z'40000000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("100.0",  int(z'42C80000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("0.1",    int(z'3DCCCCCD', int32), outcomes%OK); if (.not. success) return
        success = match_fp("123.456",int(z'42F6E979', int32), outcomes%OK); if (.not. success) return
        success = match_fp("-42.0",  int(z'C2280000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("3.14",   int(z'4048F5C3', int32), outcomes%OK); if (.not. success) return
        ! FLT_MAX, FLT_MIN, smallest subnormal
        success = match_fp("3.4028235e38",  int(z'7F7FFFFF', int32), outcomes%OK); if (.not. success) return
        success = match_fp("1.17549435e-38",int(z'00800000', int32), outcomes%OK); if (.not. success) return
        success = match_fp("1.4e-45",       int(z'00000001', int32), outcomes%OK); if (.not. success) return
        ! Inf, NaN
        success = match_fp("inf",  POS_INF, outcomes%OK); if (.not. success) return
        success = match_fp("-inf", NEG_INF, outcomes%OK); if (.not. success) return
        success = match_fp_nan("nan"); if (.not. success) return
        success = match_fp_nan("NaN"); if (.not. success) return
        ! Invalid
        success = match_fp_invalid(""); if (.not. success) return
        success = match_fp_invalid("xyz"); if (.not. success) return
        success = match_fp_invalid(".")
    end function test_fp32

    logical function test_fortran_format() result(success)
        type(parse_options), parameter :: fort_opts = parse_options(PRESET_FORTRAN, '.')

        success = match_dp_opts("1d+4",   fort_opts, int(z'40C3880000000000', int64), outcomes%OK)
        if (.not. success) return
        success = match_dp_opts("1D-3",   fort_opts, int(z'3F50624DD2F1A9FC', int64), outcomes%OK)
        if (.not. success) return
        success = match_dp_opts("1d0",    fort_opts, int(z'3FF0000000000000', int64), outcomes%OK)
        if (.not. success) return
        success = match_dp_opts("2.5d2",  fort_opts, int(z'406F400000000000', int64), outcomes%OK)
        if (.not. success) return
        success = match_dp_opts("-3.14d0",fort_opts, int(z'C0091EB851EB851F', int64), outcomes%OK)
    end function test_fortran_format

    logical function test_json_format() result(success)
        type(parse_options), parameter :: json_opts = parse_options(PRESET_JSON, '.')

        ! Valid JSON numbers
        success = match_dp_opts("0.25", json_opts, int(z'3FD0000000000000', int64), outcomes%OK)
        if (.not. success) return
        success = match_dp_opts("1.0",  json_opts, int(z'3FF0000000000000', int64), outcomes%OK)
        if (.not. success) return
        success = match_dp_opts("-1.0", json_opts, int(z'BFF0000000000000', int64), outcomes%OK)
        if (.not. success) return
        ! Invalid JSON: no leading dot, no leading +, no leading zero
        success = match_dp_opts_invalid(".25",  json_opts); if (.not. success) return
        success = match_dp_opts_invalid("+0.25",json_opts); if (.not. success) return
        success = match_dp_opts_invalid("01",   json_opts)
    end function test_json_format

    logical function test_malformed() result(success)
        success = match_dp_invalid("");    if (.not. success) return
        success = match_dp_invalid("abc"); if (.not. success) return
        success = match_dp_invalid("e5");  if (.not. success) return
        success = match_dp_invalid(".");   if (.not. success) return
        success = match_dp_invalid("   "); if (.not. success) return
        success = match_dp_invalid("--1"); if (.not. success) return
        success = match_dp_invalid("++1")
    end function test_malformed

    logical function test_custom_decimal() result(success)
        type(parse_options), parameter :: comma_opts = parse_options(PRESET_GENERAL, ',')

        success = match_dp_opts("1,5", comma_opts, int(z'3FF8000000000000', int64), outcomes%OK)
    end function test_custom_decimal

    logical function test_dp_stream_cursor_semantics() result(success)
        character(len=:), allocatable, target :: buf
        character(len=:), pointer :: p
        real(real64) :: val
        type(outcome) :: stat

        buf = "1x2"
        p => buf

        call parse_double_stream(p, val, stat)
        success = stat == outcomes%OK .and. transfer(val, 0_int64) == int(z'3FF0000000000000', int64)
        if (.not. success) return

        success = len(p) == 2 .and. p == "x2"
        if (.not. success) return

        call parse_double_stream(p, val, stat)
        success = stat == outcomes%INVALID_INPUT .and. len(p) == 2 .and. p == "x2"
    end function test_dp_stream_cursor_semantics

    logical function test_dp_array_invalid() result(success)
        character(len=:), allocatable :: stream
        real(real64) :: values(3)
        integer :: n
        type(outcome) :: err

        stream = "1.0 abc 2.0"
        values = -huge(1.0_real64)
        call parse_double_array(stream, values, n, err)

        success = err == outcomes%INVALID_INPUT .and. n == 1 .and. &
                  transfer(values(1), 0_int64) == int(z'3FF0000000000000', int64)
    end function test_dp_array_invalid

    logical function test_fp_array_invalid() result(success)
        character(len=:), allocatable :: stream
        real(real32) :: values(3)
        integer :: n
        type(outcome) :: err

        stream = "1.0 abc 2.0"
        values = -huge(1.0_real32)
        call parse_float_array(stream, values, n, err)

        success = err == outcomes%INVALID_INPUT .and. n == 1 .and. &
                  transfer(values(1), 0_int32) == int(z'3F800000', int32)
    end function test_fp_array_invalid

    logical function test_dp_array_hard_cases() result(success)
        character(len=:), allocatable :: s1, s2, s3, stream
        real(real64) :: values(3), expected(3)
        type(parse_result) :: res
        type(outcome) :: err
        integer :: n

        s1 = "9007199254740993.0"
        s2 = "8.98846567431158053656668072130502949627624141313081589739713427561540454" // &
             "154866937524136980060240969e307"
        s3 = "1.23456789012345678901234567890123456789012345678901234567890123456789e-300"
        stream = s1 // new_line('a') // s2 // new_line('a') // s3

        expected(1) = parse_double(s1, res)
        success = res%outcome == outcomes%OK
        if (.not. success) return
        expected(2) = parse_double(s2, res)
        success = res%outcome == outcomes%OK
        if (.not. success) return
        expected(3) = parse_double(s3, res)
        success = res%outcome == outcomes%OK
        if (.not. success) return

        call parse_double_array(stream, values, n, err)
        success = err == outcomes%OK .and. n == 3
        if (.not. success) return

        success = transfer(values(1), 0_int64) == transfer(expected(1), 0_int64) .and. &
                  transfer(values(2), 0_int64) == transfer(expected(2), 0_int64) .and. &
                  transfer(values(3), 0_int64) == transfer(expected(3), 0_int64)
    end function test_dp_array_hard_cases

    logical function test_dp_array_out_of_range() result(success)
        character(len=:), allocatable :: s1, s2, s3, stream
        real(real64) :: values(3), expected(2), dummy
        type(parse_result) :: res
        type(outcome) :: err
        integer :: n

        s1 = "9007199254740993.0"
        s2 = "8.98846567431158053656668072130502949627624141313081589739713427561540454" // &
             "154866937524136980060240969e307"
        s3 = "2.47032822920623272088284396434110686182529901307162382212792841250337753" // &
             "635104375932649918180817996e-324"
        stream = s1 // new_line('a') // s2 // new_line('a') // s3

        expected(1) = parse_double(s1, res)
        success = res%outcome == outcomes%OK
        if (.not. success) return
        expected(2) = parse_double(s2, res)
        success = res%outcome == outcomes%OK
        if (.not. success) return

        dummy = parse_double(s3, res)
        success = res%outcome == outcomes%OUT_OF_RANGE
        if (.not. success) return

        values = -huge(1.0_real64)
        call parse_double_array(stream, values, n, err)
        success = err == outcomes%OUT_OF_RANGE .and. n == 2 .and. &
                  transfer(values(1), 0_int64) == transfer(expected(1), 0_int64) .and. &
                  transfer(values(2), 0_int64) == transfer(expected(2), 0_int64)
    end function test_dp_array_out_of_range

    logical function test_dp_stream_hard_cases() result(success)
        character(len=:), allocatable, target :: stream
        character(len=:), pointer :: p
        character(len=:), allocatable :: s1, s2
        real(real64) :: val, expected
        type(parse_result) :: res
        type(outcome) :: stat

        s1 = "9007199254740993.0"
        s2 = "8.98846567431158053656668072130502949627624141313081589739713427561540454" // &
             "154866937524136980060240969e307"
        stream = s1 // new_line('a') // s2 // new_line('a')
        p => stream

        expected = parse_double(s1, res)
        success = res%outcome == outcomes%OK
        if (.not. success) return
        call parse_double_stream(p, val, stat)
        success = stat == outcomes%OK .and. transfer(val, 0_int64) == transfer(expected, 0_int64)
        if (.not. success) return

        success = len(p) == len(s2) + 2 .and. p == new_line('a') // s2 // new_line('a')
        if (.not. success) return

        expected = parse_double(s2, res)
        success = res%outcome == outcomes%OK
        if (.not. success) return
        call parse_double_stream(p, val, stat)
        success = stat == outcomes%OK .and. transfer(val, 0_int64) == transfer(expected, 0_int64)
        if (.not. success) return

        success = len(p) == 1 .and. p == new_line('a')
    end function test_dp_stream_hard_cases

end module test_fast_float_parsing

program check
    use test_fast_float_parsing, only: parsing_tests
    implicit none(type, external)

    integer :: nfailed, npassed
    character(*), parameter :: stats = "(1x,'[',a,']',t50,' has ',i0,' tests passed, ',i0,' failed.')"

    call parsing_tests(nfailed, npassed)

    print stats, 'fast_float', npassed, nfailed

    if (nfailed > 0) error stop 1

end program check
