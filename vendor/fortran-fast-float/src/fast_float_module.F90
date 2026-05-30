!> @brief Eisel-Lemire fast float parsing (ffc.h port).
!> @author Federico Perini <federico.perini@gmail.com>
!> @since  Mon, 10 Mar 2026
module fast_float_module
    use iso_fortran_env, only: i1 => int8, i4 => int32, i8 => int64, sp => real32, dp => real64, int32, int64, int8, real32, real64
#if defined(__INTEL_COMPILER) || defined(__INTEL_LLVM_COMPILER)
    ! Intel Fortran does not support 128-bit integers
#else
    use iso_c_binding, only: c_int128_t
#endif
    use ieee_arithmetic
    implicit none(type, external)
    private

    public :: parse_double
    public :: parse_double_range_sub
    public :: parse_double_fast, parse_double_stream
    public :: parse_double_array
    public :: parse_float
    public :: parse_float_array
    public :: parse_i64, parse_i32
    public :: parse_options, parse_result
    public :: OUTCOME
    public :: PRESET_GENERAL, PRESET_JSON
    public :: PRESET_FORTRAN, DEFAULT_PARSING

    interface parse_double
        module procedure parse_dp, parse_dp_opts, parse_dp_range, parse_dp_range_opts
        module procedure parse_dp_res, parse_dp_opts_res, parse_dp_range_res, parse_dp_range_opts_res
    end interface parse_double

    interface parse_float
        module procedure parse_fp, parse_fp_opts
        module procedure parse_fp_res, parse_fp_opts_res
    end interface parse_float

    interface parse_i64
        module procedure parse_i64_pure, parse_i64_std
    end interface parse_i64

    interface parse_i32
        module procedure parse_i32_pure, parse_i32_std
    end interface parse_i32

    ! Parsing output
    type, bind(C) :: outcome; integer(i1) :: state; end type
    type, bind(C), private :: outcomes_enum
        type(outcome) :: OK            = outcome(0)
        type(outcome) :: INVALID_INPUT = outcome(1)
        type(outcome) :: OUT_OF_RANGE  = outcome(2)
    end type outcomes_enum
    type(outcomes_enum), parameter, public :: outcomes = outcomes_enum()


    integer(i8), parameter :: FMT_SCIENTIFIC = int(b'000000001', i8)
    integer(i8), parameter :: FMT_FIXED      = int(b'000000100', i8)
    integer(i8), parameter :: FMT_NO_INFNAN  = int(b'000010000', i8)
    integer(i8), parameter :: FMT_JSON       = int(b'000100000', i8)
    integer(i8), parameter :: FMT_FORTRAN    = int(b'001000000', i8)
    integer(i8), parameter :: FMT_ALLOW_PLUS = int(b'010000000', i8)
    integer(i8), parameter :: FMT_SKIP_WS    = int(b'100000000', i8)

    integer(i8), parameter :: PRESET_GENERAL = ior(ior(FMT_FIXED, FMT_SCIENTIFIC), FMT_SKIP_WS)
    integer(i8), parameter :: PRESET_JSON    = ior(ior(FMT_JSON, PRESET_GENERAL), FMT_NO_INFNAN)
    integer(i8), parameter :: PRESET_FORTRAN = ior(ior(FMT_FORTRAN, FMT_ALLOW_PLUS), PRESET_GENERAL)

    integer(i4), parameter :: INVALID_AM = -32768_i4
    integer(i8), parameter :: SB64 = ishft(1_i8, 63)
    integer(i8), parameter :: M32 = int(z'00000000FFFFFFFF', i8)
    integer(i8), parameter :: ZERO8_U64 = int(z'3030303030303030', i8)

    ! 128-bit integer support: compile-time detection
#if defined(__INTEL_COMPILER) || defined(__INTEL_LLVM_COMPILER)
    integer, parameter :: c_int128_t = -1
#endif
    logical, parameter :: HAS_INT128 = c_int128_t > 0
    integer, parameter :: IK128 = merge(c_int128_t, i8, HAS_INT128)

    ! Compile-time endianness detection
    integer(i4), parameter :: ENDIAN_TAG = transfer([1_i1, 0_i1, 0_i1, 0_i1], 0_i4)
    logical, parameter :: LITTLE_ENDIAN = ENDIAN_TAG == 1_i4

    type :: parse_result
        integer :: pos
        type(outcome) :: outcome
    end type parse_result

    type :: parse_options
        integer(i8) :: format = PRESET_GENERAL
        character :: decimal_point = '.'
    end type parse_options

    type(parse_options), parameter :: DEFAULT_PARSING = parse_options(PRESET_GENERAL, '.')

    type :: u128
        integer(i8) :: lo
        integer(i8) :: hi
    end type u128

    type :: float_format
        integer :: mantissa_bits, min_exponent, inf_power, sign_index
        integer :: min_round_trip_exp, max_round_trip_exp
        integer :: min_fast_path, max_fast_path
        integer(i8) :: max_mantissa, exponent_mask
        integer(i8) :: mantissa_mask, hidden_bit_mask
        integer :: smallest_pow10, largest_pow10, max_digits
    end type float_format

    ! Double precision derived constants
    integer, parameter :: DOUBLE_MANTISSA_BITS = digits(1.0_dp) - 1
    integer, parameter :: DOUBLE_SIGN_INDEX    = storage_size(1.0_dp) - 1
    integer, parameter :: DOUBLE_MAX_EXPONENT  = maxexponent(1.0_dp)

    ! Float precision derived constants
    integer, parameter :: FLOAT_MANTISSA_BITS = digits(1.0_sp) - 1
    integer, parameter :: FLOAT_SIGN_INDEX    = storage_size(1.0_sp) - 1
    integer, parameter :: FLOAT_MAX_EXPONENT  = maxexponent(1.0_sp)

    ! Double precision format parameters
    type(float_format), parameter :: DOUBLE_FMT = float_format( &
        mantissa_bits = DOUBLE_MANTISSA_BITS, &
        min_exponent = 1 - DOUBLE_MAX_EXPONENT, &
        inf_power = 2*DOUBLE_MAX_EXPONENT - 1, &
        sign_index = DOUBLE_SIGN_INDEX, &
        min_round_trip_exp = -4, &
        max_round_trip_exp = 23, &
        min_fast_path = -22, &
        max_fast_path = 22, &
        max_mantissa = ishft(2_i8, DOUBLE_MANTISSA_BITS), &
        exponent_mask = iand( &
            not(ishft(1_i8, DOUBLE_MANTISSA_BITS) - 1), &
            not(ishft(1_i8, DOUBLE_SIGN_INDEX))), &
        mantissa_mask = ishft(1_i8, DOUBLE_MANTISSA_BITS) - 1, &
        hidden_bit_mask = ishft(1_i8, DOUBLE_MANTISSA_BITS), &
        smallest_pow10 = -342, &
        largest_pow10 = 308, &
        max_digits = 769)

    ! Float precision format parameters
    type(float_format), parameter :: FLOAT_FMT = float_format( &
        mantissa_bits = FLOAT_MANTISSA_BITS, &
        min_exponent = 1 - FLOAT_MAX_EXPONENT, &
        inf_power = 2*FLOAT_MAX_EXPONENT - 1, &
        sign_index = FLOAT_SIGN_INDEX, &
        min_round_trip_exp = -17, &
        max_round_trip_exp = 10, &
        min_fast_path = -10, &
        max_fast_path = 10, &
        max_mantissa = ishft(2_i8, FLOAT_MANTISSA_BITS), &
        exponent_mask = iand( &
            not(ishft(1_i8, FLOAT_MANTISSA_BITS) - 1), &
            not(ishft(1_i8, FLOAT_SIGN_INDEX))), &
        mantissa_mask = ishft(1_i8, FLOAT_MANTISSA_BITS) - 1, &
        hidden_bit_mask = ishft(1_i8, FLOAT_MANTISSA_BITS), &
        smallest_pow10 = -64, &
        largest_pow10 = 38, &
        max_digits = 114)

    ! Double-precision hot-path constants (replaces cf_f%field runtime access)
    integer, parameter :: DP_MB = DOUBLE_MANTISSA_BITS              ! 52
    integer, parameter :: DP_ME = 1 - DOUBLE_MAX_EXPONENT           ! -1023
    integer, parameter :: DP_IP = 2*DOUBLE_MAX_EXPONENT - 1         ! 2047
    integer, parameter :: DP_MFP_LO = -22, DP_MFP_HI = 22
    integer, parameter :: DP_MRT_LO = -4,  DP_MRT_HI = 23
    integer, parameter :: DP_SP10 = -342, DP_LP10 = 308
    integer(i8), parameter :: DP_MMAX = ishft(2_i8, DP_MB)         ! 2^53
    integer(i8), parameter :: DP_HB   = ishft(1_i8, DP_MB)         ! 2^52
    integer(i8), parameter :: DP_2HB  = ishft(2_i8, DP_MB)         ! 2^53
    integer(i8), parameter :: DP_CPM  = ishft(not(0_i8), -(DP_MB+3))

    ! Single-precision hot-path constants
    integer, parameter :: SP_MB = FLOAT_MANTISSA_BITS               ! 23
    integer, parameter :: SP_ME = 1 - FLOAT_MAX_EXPONENT            ! -127
    integer, parameter :: SP_IP = 2*FLOAT_MAX_EXPONENT - 1          ! 255
    integer, parameter :: SP_MFP_LO = -10, SP_MFP_HI = 10
    integer, parameter :: SP_MRT_LO = -17, SP_MRT_HI = 10
    integer, parameter :: SP_SP10 = -64, SP_LP10 = 38
    integer(i8), parameter :: SP_MMAX = ishft(2_i8, SP_MB)         ! 2^24
    integer(i8), parameter :: SP_HB   = ishft(1_i8, SP_MB)         ! 2^23
    integer(i8), parameter :: SP_2HB  = ishft(2_i8, SP_MB)         ! 2^24
    integer(i8), parameter :: SP_CPM  = ishft(not(0_i8), -(SP_MB+3))

    ! Shared inlining constants for loop_parse_eight
    integer(i8), parameter :: LPE_M  = int(z'000000FF000000FF', i8)
    integer(i8), parameter :: LPE_M1 = int(z'000F424000000064', i8)
    integer(i8), parameter :: LPE_M2 = int(z'0000271000000001', i8)
    integer(i8), parameter :: LPE_HI = int(z'8080808080808080', i8)
    integer(i8), parameter :: LPE_46 = int(z'4646464646464646', i8)
    integer(i8), parameter :: LPE_FF = int(z'00000000FFFFFFFF', i8)

    type :: parsed_number
        integer(i8) :: exponent
        integer(i8) :: mantissa
        integer :: last_idx
        logical :: negative
        logical :: valid
        logical :: too_many_digits
        integer :: int_start
        integer :: int_len
        integer :: frac_start
        integer :: frac_len
    end type parsed_number

    type :: adjusted_mantissa
        integer(i8) :: mantissa
        integer(i4) :: power2
    end type adjusted_mantissa

    integer, parameter :: LIMB_BITS   = 64
    integer, parameter :: BIGINT_BITS = 4000
    integer, parameter :: STACKVEC_CAPACITY = BIGINT_BITS / LIMB_BITS

    type :: stackvec
        integer(i8) :: d(STACKVEC_CAPACITY)
        integer :: ln
    end type stackvec

    type :: bigint
        type(stackvec) :: vec
    end type bigint

    integer, parameter :: POWER5_TABLE_SIZE = 1302
    integer(i8), parameter :: POWER5_1(200) = [ &
        int(z'eef453d6923bd65a', i8), int(z'113faa2906a13b3f', i8), int(z'9558b4661b6565f8', i8), &
        int(z'4ac7ca59a424c507', i8), int(z'baaee17fa23ebf76', i8), int(z'5d79bcf00d2df649', i8), &
        int(z'e95a99df8ace6f53', i8), int(z'f4d82c2c107973dc', i8), int(z'91d8a02bb6c10594', i8), &
        int(z'79071b9b8a4be869', i8), int(z'b64ec836a47146f9', i8), int(z'9748e2826cdee284', i8), &
        int(z'e3e27a444d8d98b7', i8), int(z'fd1b1b2308169b25', i8), int(z'8e6d8c6ab0787f72', i8), &
        int(z'fe30f0f5e50e20f7', i8), int(z'b208ef855c969f4f', i8), int(z'bdbd2d335e51a935', i8), &
        int(z'de8b2b66b3bc4723', i8), int(z'ad2c788035e61382', i8), int(z'8b16fb203055ac76', i8), &
        int(z'4c3bcb5021afcc31', i8), int(z'addcb9e83c6b1793', i8), int(z'df4abe242a1bbf3d', i8), &
        int(z'd953e8624b85dd78', i8), int(z'd71d6dad34a2af0d', i8), int(z'87d4713d6f33aa6b', i8), &
        int(z'8672648c40e5ad68', i8), int(z'a9c98d8ccb009506', i8), int(z'680efdaf511f18c2', i8), &
        int(z'd43bf0effdc0ba48', i8), int(z'212bd1b2566def2', i8), int(z'84a57695fe98746d', i8), &
        int(z'14bb630f7604b57', i8), int(z'a5ced43b7e3e9188', i8), int(z'419ea3bd35385e2d', i8), &
        int(z'cf42894a5dce35ea', i8), int(z'52064cac828675b9', i8), int(z'818995ce7aa0e1b2', i8), &
        int(z'7343efebd1940993', i8), int(z'a1ebfb4219491a1f', i8), int(z'1014ebe6c5f90bf8', i8), &
        int(z'ca66fa129f9b60a6', i8), int(z'd41a26e077774ef6', i8), int(z'fd00b897478238d0', i8), &
        int(z'8920b098955522b4', i8), int(z'9e20735e8cb16382', i8), int(z'55b46e5f5d5535b0', i8), &
        int(z'c5a890362fddbc62', i8), int(z'eb2189f734aa831d', i8), int(z'f712b443bbd52b7b', i8), &
        int(z'a5e9ec7501d523e4', i8), int(z'9a6bb0aa55653b2d', i8), int(z'47b233c92125366e', i8), &
        int(z'c1069cd4eabe89f8', i8), int(z'999ec0bb696e840a', i8), int(z'f148440a256e2c76', i8), &
        int(z'c00670ea43ca250d', i8), int(z'96cd2a865764dbca', i8), int(z'380406926a5e5728', i8), &
        int(z'bc807527ed3e12bc', i8), int(z'c605083704f5ecf2', i8), int(z'eba09271e88d976b', i8), &
        int(z'f7864a44c633682e', i8), int(z'93445b8731587ea3', i8), int(z'7ab3ee6afbe0211d', i8), &
        int(z'b8157268fdae9e4c', i8), int(z'5960ea05bad82964', i8), int(z'e61acf033d1a45df', i8), &
        int(z'6fb92487298e33bd', i8), int(z'8fd0c16206306bab', i8), int(z'a5d3b6d479f8e056', i8), &
        int(z'b3c4f1ba87bc8696', i8), int(z'8f48a4899877186c', i8), int(z'e0b62e2929aba83c', i8), &
        int(z'331acdabfe94de87', i8), int(z'8c71dcd9ba0b4925', i8), int(z'9ff0c08b7f1d0b14', i8), &
        int(z'af8e5410288e1b6f', i8), int(z'7ecf0ae5ee44dd9', i8), int(z'db71e91432b1a24a', i8), &
        int(z'c9e82cd9f69d6150', i8), int(z'892731ac9faf056e', i8), int(z'be311c083a225cd2', i8), &
        int(z'ab70fe17c79ac6ca', i8), int(z'6dbd630a48aaf406', i8), int(z'd64d3d9db981787d', i8), &
        int(z'92cbbccdad5b108', i8), int(z'85f0468293f0eb4e', i8), int(z'25bbf56008c58ea5', i8), &
        int(z'a76c582338ed2621', i8), int(z'af2af2b80af6f24e', i8), int(z'd1476e2c07286faa', i8), &
        int(z'1af5af660db4aee1', i8), int(z'82cca4db847945ca', i8), int(z'50d98d9fc890ed4d', i8), &
        int(z'a37fce126597973c', i8), int(z'e50ff107bab528a0', i8), int(z'cc5fc196fefd7d0c', i8), &
        int(z'1e53ed49a96272c8', i8), int(z'ff77b1fcbebcdc4f', i8), int(z'25e8e89c13bb0f7a', i8), &
        int(z'9faacf3df73609b1', i8), int(z'77b191618c54e9ac', i8), int(z'c795830d75038c1d', i8), &
        int(z'd59df5b9ef6a2417', i8), int(z'f97ae3d0d2446f25', i8), int(z'4b0573286b44ad1d', i8), &
        int(z'9becce62836ac577', i8), int(z'4ee367f9430aec32', i8), int(z'c2e801fb244576d5', i8), &
        int(z'229c41f793cda73f', i8), int(z'f3a20279ed56d48a', i8), int(z'6b43527578c1110f', i8), &
        int(z'9845418c345644d6', i8), int(z'830a13896b78aaa9', i8), int(z'be5691ef416bd60c', i8), &
        int(z'23cc986bc656d553', i8), int(z'edec366b11c6cb8f', i8), int(z'2cbfbe86b7ec8aa8', i8), &
        int(z'94b3a202eb1c3f39', i8), int(z'7bf7d71432f3d6a9', i8), int(z'b9e08a83a5e34f07', i8), &
        int(z'daf5ccd93fb0cc53', i8), int(z'e858ad248f5c22c9', i8), int(z'd1b3400f8f9cff68', i8), &
        int(z'91376c36d99995be', i8), int(z'23100809b9c21fa1', i8), int(z'b58547448ffffb2d', i8), &
        int(z'abd40a0c2832a78a', i8), int(z'e2e69915b3fff9f9', i8), int(z'16c90c8f323f516c', i8), &
        int(z'8dd01fad907ffc3b', i8), int(z'ae3da7d97f6792e3', i8), int(z'b1442798f49ffb4a', i8), &
        int(z'99cd11cfdf41779c', i8), int(z'dd95317f31c7fa1d', i8), int(z'40405643d711d583', i8), &
        int(z'8a7d3eef7f1cfc52', i8), int(z'482835ea666b2572', i8), int(z'ad1c8eab5ee43b66', i8), &
        int(z'da3243650005eecf', i8), int(z'd863b256369d4a40', i8), int(z'90bed43e40076a82', i8), &
        int(z'873e4f75e2224e68', i8), int(z'5a7744a6e804a291', i8), int(z'a90de3535aaae202', i8), &
        int(z'711515d0a205cb36', i8), int(z'd3515c2831559a83', i8), int(z'd5a5b44ca873e03', i8), &
        int(z'8412d9991ed58091', i8), int(z'e858790afe9486c2', i8), int(z'a5178fff668ae0b6', i8), &
        int(z'626e974dbe39a872', i8), int(z'ce5d73ff402d98e3', i8), int(z'fb0a3d212dc8128f', i8), &
        int(z'80fa687f881c7f8e', i8), int(z'7ce66634bc9d0b99', i8), int(z'a139029f6a239f72', i8), &
        int(z'1c1fffc1ebc44e80', i8), int(z'c987434744ac874e', i8), int(z'a327ffb266b56220', i8), &
        int(z'fbe9141915d7a922', i8), int(z'4bf1ff9f0062baa8', i8), int(z'9d71ac8fada6c9b5', i8), &
        int(z'6f773fc3603db4a9', i8), int(z'c4ce17b399107c22', i8), int(z'cb550fb4384d21d3', i8), &
        int(z'f6019da07f549b2b', i8), int(z'7e2a53a146606a48', i8), int(z'99c102844f94e0fb', i8), &
        int(z'2eda7444cbfc426d', i8), int(z'c0314325637a1939', i8), int(z'fa911155fefb5308', i8), &
        int(z'f03d93eebc589f88', i8), int(z'793555ab7eba27ca', i8), int(z'96267c7535b763b5', i8), &
        int(z'4bc1558b2f3458de', i8), int(z'bbb01b9283253ca2', i8), int(z'9eb1aaedfb016f16', i8), &
        int(z'ea9c227723ee8bcb', i8), int(z'465e15a979c1cadc', i8), int(z'92a1958a7675175f', i8), &
        int(z'bfacd89ec191ec9', i8), int(z'b749faed14125d36', i8), int(z'cef980ec671f667b', i8), &
        int(z'e51c79a85916f484', i8), int(z'82b7e12780e7401a', i8), int(z'8f31cc0937ae58d2', i8), &
        int(z'd1b2ecb8b0908810', i8), int(z'b2fe3f0b8599ef07', i8), int(z'861fa7e6dcb4aa15', i8), &
        int(z'dfbdcece67006ac9', i8), int(z'67a791e093e1d49a', i8), int(z'8bd6a141006042bd', i8), &
        int(z'e0c8bb2c5c6d24e0', i8), int(z'aecc49914078536d', i8), int(z'58fae9f773886e18', i8), &
        int(z'da7f5bf590966848', i8), int(z'af39a475506a899e', i8) ]
    integer(i8), parameter :: POWER5_2(200) = [ &
        int(z'888f99797a5e012d', i8), int(z'6d8406c952429603', i8), int(z'aab37fd7d8f58178', i8), &
        int(z'c8e5087ba6d33b83', i8), int(z'd5605fcdcf32e1d6', i8), int(z'fb1e4a9a90880a64', i8), &
        int(z'855c3be0a17fcd26', i8), int(z'5cf2eea09a55067f', i8), int(z'a6b34ad8c9dfc06f', i8), &
        int(z'f42faa48c0ea481e', i8), int(z'd0601d8efc57b08b', i8), int(z'f13b94daf124da26', i8), &
        int(z'823c12795db6ce57', i8), int(z'76c53d08d6b70858', i8), int(z'a2cb1717b52481ed', i8), &
        int(z'54768c4b0c64ca6e', i8), int(z'cb7ddcdda26da268', i8), int(z'a9942f5dcf7dfd09', i8), &
        int(z'fe5d54150b090b02', i8), int(z'd3f93b35435d7c4c', i8), int(z'9efa548d26e5a6e1', i8), &
        int(z'c47bc5014a1a6daf', i8), int(z'c6b8e9b0709f109a', i8), int(z'359ab6419ca1091b', i8), &
        int(z'f867241c8cc6d4c0', i8), int(z'c30163d203c94b62', i8), int(z'9b407691d7fc44f8', i8), &
        int(z'79e0de63425dcf1d', i8), int(z'c21094364dfb5636', i8), int(z'985915fc12f542e4', i8), &
        int(z'f294b943e17a2bc4', i8), int(z'3e6f5b7b17b2939d', i8), int(z'979cf3ca6cec5b5a', i8), &
        int(z'a705992ceecf9c42', i8), int(z'bd8430bd08277231', i8), int(z'50c6ff782a838353', i8), &
        int(z'ece53cec4a314ebd', i8), int(z'a4f8bf5635246428', i8), int(z'940f4613ae5ed136', i8), &
        int(z'871b7795e136be99', i8), int(z'b913179899f68584', i8), int(z'28e2557b59846e3f', i8), &
        int(z'e757dd7ec07426e5', i8), int(z'331aeada2fe589cf', i8), int(z'9096ea6f3848984f', i8), &
        int(z'3ff0d2c85def7621', i8), int(z'b4bca50b065abe63', i8), int(z'fed077a756b53a9', i8), &
        int(z'e1ebce4dc7f16dfb', i8), int(z'd3e8495912c62894', i8), int(z'8d3360f09cf6e4bd', i8), &
        int(z'64712dd7abbbd95c', i8), int(z'b080392cc4349dec', i8), int(z'bd8d794d96aacfb3', i8), &
        int(z'dca04777f541c567', i8), int(z'ecf0d7a0fc5583a0', i8), int(z'89e42caaf9491b60', i8), &
        int(z'f41686c49db57244', i8), int(z'ac5d37d5b79b6239', i8), int(z'311c2875c522ced5', i8), &
        int(z'd77485cb25823ac7', i8), int(z'7d633293366b828b', i8), int(z'86a8d39ef77164bc', i8), &
        int(z'ae5dff9c02033197', i8), int(z'a8530886b54dbdeb', i8), int(z'd9f57f830283fdfc', i8), &
        int(z'd267caa862a12d66', i8), int(z'd072df63c324fd7b', i8), int(z'8380dea93da4bc60', i8), &
        int(z'4247cb9e59f71e6d', i8), int(z'a46116538d0deb78', i8), int(z'52d9be85f074e608', i8), &
        int(z'cd795be870516656', i8), int(z'67902e276c921f8b', i8), int(z'806bd9714632dff6', i8), &
        int(z'ba1cd8a3db53b6', i8), int(z'a086cfcd97bf97f3', i8), int(z'80e8a40eccd228a4', i8), &
        int(z'c8a883c0fdaf7df0', i8), int(z'6122cd128006b2cd', i8), int(z'fad2a4b13d1b5d6c', i8), &
        int(z'796b805720085f81', i8), int(z'9cc3a6eec6311a63', i8), int(z'cbe3303674053bb0', i8), &
        int(z'c3f490aa77bd60fc', i8), int(z'bedbfc4411068a9c', i8), int(z'f4f1b4d515acb93b', i8), &
        int(z'ee92fb5515482d44', i8), int(z'991711052d8bf3c5', i8), int(z'751bdd152d4d1c4a', i8), &
        int(z'bf5cd54678eef0b6', i8), int(z'd262d45a78a0635d', i8), int(z'ef340a98172aace4', i8), &
        int(z'86fb897116c87c34', i8), int(z'9580869f0e7aac0e', i8), int(z'd45d35e6ae3d4da0', i8), &
        int(z'bae0a846d2195712', i8), int(z'8974836059cca109', i8), int(z'e998d258869facd7', i8), &
        int(z'2bd1a438703fc94b', i8), int(z'91ff83775423cc06', i8), int(z'7b6306a34627ddcf', i8), &
        int(z'b67f6455292cbf08', i8), int(z'1a3bc84c17b1d542', i8), int(z'e41f3d6a7377eeca', i8), &
        int(z'20caba5f1d9e4a93', i8), int(z'8e938662882af53e', i8), int(z'547eb47b7282ee9c', i8), &
        int(z'b23867fb2a35b28d', i8), int(z'e99e619a4f23aa43', i8), int(z'dec681f9f4c31f31', i8), &
        int(z'6405fa00e2ec94d4', i8), int(z'8b3c113c38f9f37e', i8), int(z'de83bc408dd3dd04', i8), &
        int(z'ae0b158b4738705e', i8), int(z'9624ab50b148d445', i8), int(z'd98ddaee19068c76', i8), &
        int(z'3badd624dd9b0957', i8), int(z'87f8a8d4cfa417c9', i8), int(z'e54ca5d70a80e5d6', i8), &
        int(z'a9f6d30a038d1dbc', i8), int(z'5e9fcf4ccd211f4c', i8), int(z'd47487cc8470652b', i8), &
        int(z'7647c3200069671f', i8), int(z'84c8d4dfd2c63f3b', i8), int(z'29ecd9f40041e073', i8), &
        int(z'a5fb0a17c777cf09', i8), int(z'f468107100525890', i8), int(z'cf79cc9db955c2cc', i8), &
        int(z'7182148d4066eeb4', i8), int(z'81ac1fe293d599bf', i8), int(z'c6f14cd848405530', i8), &
        int(z'a21727db38cb002f', i8), int(z'b8ada00e5a506a7c', i8), int(z'ca9cf1d206fdc03b', i8), &
        int(z'a6d90811f0e4851c', i8), int(z'fd442e4688bd304a', i8), int(z'908f4a166d1da663', i8), &
        int(z'9e4a9cec15763e2e', i8), int(z'9a598e4e043287fe', i8), int(z'c5dd44271ad3cdba', i8), &
        int(z'40eff1e1853f29fd', i8), int(z'f7549530e188c128', i8), int(z'd12bee59e68ef47c', i8), &
        int(z'9a94dd3e8cf578b9', i8), int(z'82bb74f8301958ce', i8), int(z'c13a148e3032d6e7', i8), &
        int(z'e36a52363c1faf01', i8), int(z'f18899b1bc3f8ca1', i8), int(z'dc44e6c3cb279ac1', i8), &
        int(z'96f5600f15a7b7e5', i8), int(z'29ab103a5ef8c0b9', i8), int(z'bcb2b812db11a5de', i8), &
        int(z'7415d448f6b6f0e7', i8), int(z'ebdf661791d60f56', i8), int(z'111b495b3464ad21', i8), &
        int(z'936b9fcebb25c995', i8), int(z'cab10dd900beec34', i8), int(z'b84687c269ef3bfb', i8), &
        int(z'3d5d514f40eea742', i8), int(z'e65829b3046b0afa', i8), int(z'cb4a5a3112a5112', i8), &
        int(z'8ff71a0fe2c2e6dc', i8), int(z'47f0e785eaba72ab', i8), int(z'b3f4e093db73a093', i8), &
        int(z'59ed216765690f56', i8), int(z'e0f218b8d25088b8', i8), int(z'306869c13ec3532c', i8), &
        int(z'8c974f7383725573', i8), int(z'1e414218c73a13fb', i8), int(z'afbd2350644eeacf', i8), &
        int(z'e5d1929ef90898fa', i8), int(z'dbac6c247d62a583', i8), int(z'df45f746b74abf39', i8), &
        int(z'894bc396ce5da772', i8), int(z'6b8bba8c328eb783', i8), int(z'ab9eb47c81f5114f', i8), &
        int(z'66ea92f3f326564', i8), int(z'd686619ba27255a2', i8), int(z'c80a537b0efefebd', i8), &
        int(z'8613fd0145877585', i8), int(z'bd06742ce95f5f36', i8), int(z'a798fc4196e952e7', i8), &
        int(z'2c48113823b73704', i8), int(z'd17f3b51fca3a7a0', i8), int(z'f75a15862ca504c5', i8), &
        int(z'82ef85133de648c4', i8), int(z'9a984d73dbe722fb', i8), int(z'a3ab66580d5fdaf5', i8), &
        int(z'c13e60d0d2e0ebba', i8), int(z'cc963fee10b7d1b3', i8), int(z'318df905079926a8', i8), &
        int(z'ffbbcfe994e5c61f', i8), int(z'fdf17746497f7052', i8), int(z'9fd561f1fd0f9bd3', i8), &
        int(z'feb6ea8bedefa633', i8), int(z'c7caba6e7c5382c8', i8), int(z'fe64a52ee96b8fc0', i8), &
        int(z'f9bd690a1b68637b', i8), int(z'3dfdce7aa3c673b0', i8) ]
    integer(i8), parameter :: POWER5_3(200) = [ &
        int(z'9c1661a651213e2d', i8), int(z'6bea10ca65c084e', i8), int(z'c31bfa0fe5698db8', i8), &
        int(z'486e494fcff30a62', i8), int(z'f3e2f893dec3f126', i8), int(z'5a89dba3c3efccfa', i8), &
        int(z'986ddb5c6b3a76b7', i8), int(z'f89629465a75e01c', i8), int(z'be89523386091465', i8), &
        int(z'f6bbb397f1135823', i8), int(z'ee2ba6c0678b597f', i8), int(z'746aa07ded582e2c', i8), &
        int(z'94db483840b717ef', i8), int(z'a8c2a44eb4571cdc', i8), int(z'ba121a4650e4ddeb', i8), &
        int(z'92f34d62616ce413', i8), int(z'e896a0d7e51e1566', i8), int(z'77b020baf9c81d17', i8), &
        int(z'915e2486ef32cd60', i8), int(z'ace1474dc1d122e', i8), int(z'b5b5ada8aaff80b8', i8), &
        int(z'd819992132456ba', i8), int(z'e3231912d5bf60e6', i8), int(z'10e1fff697ed6c69', i8), &
        int(z'8df5efabc5979c8f', i8), int(z'ca8d3ffa1ef463c1', i8), int(z'b1736b96b6fd83b3', i8), &
        int(z'bd308ff8a6b17cb2', i8), int(z'ddd0467c64bce4a0', i8), int(z'ac7cb3f6d05ddbde', i8), &
        int(z'8aa22c0dbef60ee4', i8), int(z'6bcdf07a423aa96b', i8), int(z'ad4ab7112eb3929d', i8), &
        int(z'86c16c98d2c953c6', i8), int(z'd89d64d57a607744', i8), int(z'e871c7bf077ba8b7', i8), &
        int(z'87625f056c7c4a8b', i8), int(z'11471cd764ad4972', i8), int(z'a93af6c6c79b5d2d', i8), &
        int(z'd598e40d3dd89bcf', i8), int(z'd389b47879823479', i8), int(z'4aff1d108d4ec2c3', i8), &
        int(z'843610cb4bf160cb', i8), int(z'cedf722a585139ba', i8), int(z'a54394fe1eedb8fe', i8), &
        int(z'c2974eb4ee658828', i8), int(z'ce947a3da6a9273e', i8), int(z'733d226229feea32', i8), &
        int(z'811ccc668829b887', i8), int(z'806357d5a3f525f', i8), int(z'a163ff802a3426a8', i8), &
        int(z'ca07c2dcb0cf26f7', i8), int(z'c9bcff6034c13052', i8), int(z'fc89b393dd02f0b5', i8), &
        int(z'fc2c3f3841f17c67', i8), int(z'bbac2078d443ace2', i8), int(z'9d9ba7832936edc0', i8), &
        int(z'd54b944b84aa4c0d', i8), int(z'c5029163f384a931', i8), int(z'a9e795e65d4df11', i8), &
        int(z'f64335bcf065d37d', i8), int(z'4d4617b5ff4a16d5', i8), int(z'99ea0196163fa42e', i8), &
        int(z'504bced1bf8e4e45', i8), int(z'c06481fb9bcf8d39', i8), int(z'e45ec2862f71e1d6', i8), &
        int(z'f07da27a82c37088', i8), int(z'5d767327bb4e5a4c', i8), int(z'964e858c91ba2655', i8), &
        int(z'3a6a07f8d510f86f', i8), int(z'bbe226efb628afea', i8), int(z'890489f70a55368b', i8), &
        int(z'eadab0aba3b2dbe5', i8), int(z'2b45ac74ccea842e', i8), int(z'92c8ae6b464fc96f', i8), &
        int(z'3b0b8bc90012929d', i8), int(z'b77ada0617e3bbcb', i8), int(z'9ce6ebb40173744', i8), &
        int(z'e55990879ddcaabd', i8), int(z'cc420a6a101d0515', i8), int(z'8f57fa54c2a9eab6', i8), &
        int(z'9fa946824a12232d', i8), int(z'b32df8e9f3546564', i8), int(z'47939822dc96abf9', i8), &
        int(z'dff9772470297ebd', i8), int(z'59787e2b93bc56f7', i8), int(z'8bfbea76c619ef36', i8), &
        int(z'57eb4edb3c55b65a', i8), int(z'aefae51477a06b03', i8), int(z'ede622920b6b23f1', i8), &
        int(z'dab99e59958885c4', i8), int(z'e95fab368e45eced', i8), int(z'88b402f7fd75539b', i8), &
        int(z'11dbcb0218ebb414', i8), int(z'aae103b5fcd2a881', i8), int(z'd652bdc29f26a119', i8), &
        int(z'd59944a37c0752a2', i8), int(z'4be76d3346f0495f', i8), int(z'857fcae62d8493a5', i8), &
        int(z'6f70a4400c562ddb', i8), int(z'a6dfbd9fb8e5b88e', i8), int(z'cb4ccd500f6bb952', i8), &
        int(z'd097ad07a71f26b2', i8), int(z'7e2000a41346a7a7', i8), int(z'825ecc24c873782f', i8), &
        int(z'8ed400668c0c28c8', i8), int(z'a2f67f2dfa90563b', i8), int(z'728900802f0f32fa', i8), &
        int(z'cbb41ef979346bca', i8), int(z'4f2b40a03ad2ffb9', i8), int(z'fea126b7d78186bc', i8), &
        int(z'e2f610c84987bfa8', i8), int(z'9f24b832e6b0f436', i8), int(z'dd9ca7d2df4d7c9', i8), &
        int(z'c6ede63fa05d3143', i8), int(z'91503d1c79720dbb', i8), int(z'f8a95fcf88747d94', i8), &
        int(z'75a44c6397ce912a', i8), int(z'9b69dbe1b548ce7c', i8), int(z'c986afbe3ee11aba', i8), &
        int(z'c24452da229b021b', i8), int(z'fbe85badce996168', i8), int(z'f2d56790ab41c2a2', i8), &
        int(z'fae27299423fb9c3', i8), int(z'97c560ba6b0919a5', i8), int(z'dccd879fc967d41a', i8), &
        int(z'bdb6b8e905cb600f', i8), int(z'5400e987bbc1c920', i8), int(z'ed246723473e3813', i8), &
        int(z'290123e9aab23b68', i8), int(z'9436c0760c86e30b', i8), int(z'f9a0b6720aaf6521', i8), &
        int(z'b94470938fa89bce', i8), int(z'f808e40e8d5b3e69', i8), int(z'e7958cb87392c2c2', i8), &
        int(z'b60b1d1230b20e04', i8), int(z'90bd77f3483bb9b9', i8), int(z'b1c6f22b5e6f48c2', i8), &
        int(z'b4ecd5f01a4aa828', i8), int(z'1e38aeb6360b1af3', i8), int(z'e2280b6c20dd5232', i8), &
        int(z'25c6da63c38de1b0', i8), int(z'8d590723948a535f', i8), int(z'579c487e5a38ad0e', i8), &
        int(z'b0af48ec79ace837', i8), int(z'2d835a9df0c6d851', i8), int(z'dcdb1b2798182244', i8), &
        int(z'f8e431456cf88e65', i8), int(z'8a08f0f8bf0f156b', i8), int(z'1b8e9ecb641b58ff', i8), &
        int(z'ac8b2d36eed2dac5', i8), int(z'e272467e3d222f3f', i8), int(z'd7adf884aa879177', i8), &
        int(z'5b0ed81dcc6abb0f', i8), int(z'86ccbb52ea94baea', i8), int(z'98e947129fc2b4e9', i8), &
        int(z'a87fea27a539e9a5', i8), int(z'3f2398d747b36224', i8), int(z'd29fe4b18e88640e', i8), &
        int(z'8eec7f0d19a03aad', i8), int(z'83a3eeeef9153e89', i8), int(z'1953cf68300424ac', i8), &
        int(z'a48ceaaab75a8e2b', i8), int(z'5fa8c3423c052dd7', i8), int(z'cdb02555653131b6', i8), &
        int(z'3792f412cb06794d', i8), int(z'808e17555f3ebf11', i8), int(z'e2bbd88bbee40bd0', i8), &
        int(z'a0b19d2ab70e6ed6', i8), int(z'5b6aceaeae9d0ec4', i8), int(z'c8de047564d20a8b', i8), &
        int(z'f245825a5a445275', i8), int(z'fb158592be068d2e', i8), int(z'eed6e2f0f0d56712', i8), &
        int(z'9ced737bb6c4183d', i8), int(z'55464dd69685606b', i8), int(z'c428d05aa4751e4c', i8), &
        int(z'aa97e14c3c26b886', i8), int(z'f53304714d9265df', i8), int(z'd53dd99f4b3066a8', i8), &
        int(z'993fe2c6d07b7fab', i8), int(z'e546a8038efe4029', i8), int(z'bf8fdb78849a5f96', i8), &
        int(z'de98520472bdd033', i8), int(z'ef73d256a5c0f77c', i8), int(z'963e66858f6d4440', i8), &
        int(z'95a8637627989aad', i8), int(z'dde7001379a44aa8', i8), int(z'bb127c53b17ec159', i8), &
        int(z'5560c018580d5d52', i8), int(z'e9d71b689dde71af', i8), int(z'aab8f01e6e10b4a6', i8), &
        int(z'9226712162ab070d', i8), int(z'cab3961304ca70e8', i8), int(z'b6b00d69bb55c8d1', i8), &
        int(z'3d607b97c5fd0d22', i8), int(z'e45c10c42a2b3b05', i8), int(z'8cb89a7db77c506a', i8), &
        int(z'8eb98a7a9a5b04e3', i8), int(z'77f3608e92adb242', i8) ]
    integer(i8), parameter :: POWER5_4(200) = [ &
        int(z'b267ed1940f1c61c', i8), int(z'55f038b237591ed3', i8), int(z'df01e85f912e37a3', i8), &
        int(z'6b6c46dec52f6688', i8), int(z'8b61313bbabce2c6', i8), int(z'2323ac4b3b3da015', i8), &
        int(z'ae397d8aa96c1b77', i8), int(z'abec975e0a0d081a', i8), int(z'd9c7dced53c72255', i8), &
        int(z'96e7bd358c904a21', i8), int(z'881cea14545c7575', i8), int(z'7e50d64177da2e54', i8), &
        int(z'aa242499697392d2', i8), int(z'dde50bd1d5d0b9e9', i8), int(z'd4ad2dbfc3d07787', i8), &
        int(z'955e4ec64b44e864', i8), int(z'84ec3c97da624ab4', i8), int(z'bd5af13bef0b113e', i8), &
        int(z'a6274bbdd0fadd61', i8), int(z'ecb1ad8aeacdd58e', i8), int(z'cfb11ead453994ba', i8), &
        int(z'67de18eda5814af2', i8), int(z'81ceb32c4b43fcf4', i8), int(z'80eacf948770ced7', i8), &
        int(z'a2425ff75e14fc31', i8), int(z'a1258379a94d028d', i8), int(z'cad2f7f5359a3b3e', i8), &
        int(z'96ee45813a04330', i8), int(z'fd87b5f28300ca0d', i8), int(z'8bca9d6e188853fc', i8), &
        int(z'9e74d1b791e07e48', i8), int(z'775ea264cf55347e', i8), int(z'c612062576589dda', i8), &
        int(z'95364afe032a819e', i8), int(z'f79687aed3eec551', i8), int(z'3a83ddbd83f52205', i8), &
        int(z'9abe14cd44753b52', i8), int(z'c4926a9672793543', i8), int(z'c16d9a0095928a27', i8), &
        int(z'75b7053c0f178294', i8), int(z'f1c90080baf72cb1', i8), int(z'5324c68b12dd6339', i8), &
        int(z'971da05074da7bee', i8), int(z'd3f6fc16ebca5e04', i8), int(z'bce5086492111aea', i8), &
        int(z'88f4bb1ca6bcf585', i8), int(z'ec1e4a7db69561a5', i8), int(z'2b31e9e3d06c32e6', i8), &
        int(z'9392ee8e921d5d07', i8), int(z'3aff322e62439fd0', i8), int(z'b877aa3236a4b449', i8), &
        int(z'9befeb9fad487c3', i8), int(z'e69594bec44de15b', i8), int(z'4c2ebe687989a9b4', i8), &
        int(z'901d7cf73ab0acd9', i8), int(z'f9d37014bf60a11', i8), int(z'b424dc35095cd80f', i8), &
        int(z'538484c19ef38c95', i8), int(z'e12e13424bb40e13', i8), int(z'2865a5f206b06fba', i8), &
        int(z'8cbccc096f5088cb', i8), int(z'f93f87b7442e45d4', i8), int(z'afebff0bcb24aafe', i8), &
        int(z'f78f69a51539d749', i8), int(z'dbe6fecebdedd5be', i8), int(z'b573440e5a884d1c', i8), &
        int(z'89705f4136b4a597', i8), int(z'31680a88f8953031', i8), int(z'abcc77118461cefc', i8), &
        int(z'fdc20d2b36ba7c3e', i8), int(z'd6bf94d5e57a42bc', i8), int(z'3d32907604691b4d', i8), &
        int(z'8637bd05af6c69b5', i8), int(z'a63f9a49c2c1b110', i8), int(z'a7c5ac471b478423', i8), &
        int(z'fcf80dc33721d54', i8), int(z'd1b71758e219652b', i8), int(z'd3c36113404ea4a9', i8), &
        int(z'83126e978d4fdf3b', i8), int(z'645a1cac083126ea', i8), int(z'a3d70a3d70a3d70a', i8), &
        int(z'3d70a3d70a3d70a4', i8), int(z'cccccccccccccccc', i8), int(z'cccccccccccccccd', i8), &
        int(z'8000000000000000', i8), int(z'0', i8), int(z'a000000000000000', i8), &
        int(z'0', i8), int(z'c800000000000000', i8), int(z'0', i8), &
        int(z'fa00000000000000', i8), int(z'0', i8), int(z'9c40000000000000', i8), &
        int(z'0', i8), int(z'c350000000000000', i8), int(z'0', i8), &
        int(z'f424000000000000', i8), int(z'0', i8), int(z'9896800000000000', i8), &
        int(z'0', i8), int(z'bebc200000000000', i8), int(z'0', i8), &
        int(z'ee6b280000000000', i8), int(z'0', i8), int(z'9502f90000000000', i8), &
        int(z'0', i8), int(z'ba43b74000000000', i8), int(z'0', i8), &
        int(z'e8d4a51000000000', i8), int(z'0', i8), int(z'9184e72a00000000', i8), &
        int(z'0', i8), int(z'b5e620f480000000', i8), int(z'0', i8), &
        int(z'e35fa931a0000000', i8), int(z'0', i8), int(z'8e1bc9bf04000000', i8), &
        int(z'0', i8), int(z'b1a2bc2ec5000000', i8), int(z'0', i8), &
        int(z'de0b6b3a76400000', i8), int(z'0', i8), int(z'8ac7230489e80000', i8), &
        int(z'0', i8), int(z'ad78ebc5ac620000', i8), int(z'0', i8), &
        int(z'd8d726b7177a8000', i8), int(z'0', i8), int(z'878678326eac9000', i8), &
        int(z'0', i8), int(z'a968163f0a57b400', i8), int(z'0', i8), &
        int(z'd3c21bcecceda100', i8), int(z'0', i8), int(z'84595161401484a0', i8), &
        int(z'0', i8), int(z'a56fa5b99019a5c8', i8), int(z'0', i8), &
        int(z'cecb8f27f4200f3a', i8), int(z'0', i8), int(z'813f3978f8940984', i8), &
        int(z'4000000000000000', i8), int(z'a18f07d736b90be5', i8), int(z'5000000000000000', i8), &
        int(z'c9f2c9cd04674ede', i8), int(z'a400000000000000', i8), int(z'fc6f7c4045812296', i8), &
        int(z'4d00000000000000', i8), int(z'9dc5ada82b70b59d', i8), int(z'f020000000000000', i8), &
        int(z'c5371912364ce305', i8), int(z'6c28000000000000', i8), int(z'f684df56c3e01bc6', i8), &
        int(z'c732000000000000', i8), int(z'9a130b963a6c115c', i8), int(z'3c7f400000000000', i8), &
        int(z'c097ce7bc90715b3', i8), int(z'4b9f100000000000', i8), int(z'f0bdc21abb48db20', i8), &
        int(z'1e86d40000000000', i8), int(z'96769950b50d88f4', i8), int(z'1314448000000000', i8), &
        int(z'bc143fa4e250eb31', i8), int(z'17d955a000000000', i8), int(z'eb194f8e1ae525fd', i8), &
        int(z'5dcfab0800000000', i8), int(z'92efd1b8d0cf37be', i8), int(z'5aa1cae500000000', i8), &
        int(z'b7abc627050305ad', i8), int(z'f14a3d9e40000000', i8), int(z'e596b7b0c643c719', i8), &
        int(z'6d9ccd05d0000000', i8), int(z'8f7e32ce7bea5c6f', i8), int(z'e4820023a2000000', i8), &
        int(z'b35dbf821ae4f38b', i8), int(z'dda2802c8a800000', i8), int(z'e0352f62a19e306e', i8), &
        int(z'd50b2037ad200000', i8), int(z'8c213d9da502de45', i8), int(z'4526f422cc340000', i8), &
        int(z'af298d050e4395d6', i8), int(z'9670b12b7f410000', i8), int(z'daf3f04651d47b4c', i8), &
        int(z'3c0cdd765f114000', i8), int(z'88d8762bf324cd0f', i8), int(z'a5880a69fb6ac800', i8), &
        int(z'ab0e93b6efee0053', i8), int(z'8eea0d047a457a00', i8), int(z'd5d238a4abe98068', i8), &
        int(z'72a4904598d6d880', i8), int(z'85a36366eb71f041', i8), int(z'47a6da2b7f864750', i8), &
        int(z'a70c3c40a64e6c51', i8), int(z'999090b65f67d924', i8), int(z'd0cf4b50cfe20765', i8), &
        int(z'fff4b4e3f741cf6d', i8), int(z'82818f1281ed449f', i8), int(z'bff8f10e7a8921a4', i8), &
        int(z'a321f2d7226895c7', i8), int(z'aff72d52192b6a0d', i8) ]
    integer(i8), parameter :: POWER5_5(200) = [ &
        int(z'cbea6f8ceb02bb39', i8), int(z'9bf4f8a69f764490', i8), int(z'fee50b7025c36a08', i8), &
        int(z'2f236d04753d5b4', i8), int(z'9f4f2726179a2245', i8), int(z'1d762422c946590', i8), &
        int(z'c722f0ef9d80aad6', i8), int(z'424d3ad2b7b97ef5', i8), int(z'f8ebad2b84e0d58b', i8), &
        int(z'd2e0898765a7deb2', i8), int(z'9b934c3b330c8577', i8), int(z'63cc55f49f88eb2f', i8), &
        int(z'c2781f49ffcfa6d5', i8), int(z'3cbf6b71c76b25fb', i8), int(z'f316271c7fc3908a', i8), &
        int(z'8bef464e3945ef7a', i8), int(z'97edd871cfda3a56', i8), int(z'97758bf0e3cbb5ac', i8), &
        int(z'bde94e8e43d0c8ec', i8), int(z'3d52eeed1cbea317', i8), int(z'ed63a231d4c4fb27', i8), &
        int(z'4ca7aaa863ee4bdd', i8), int(z'945e455f24fb1cf8', i8), int(z'8fe8caa93e74ef6a', i8), &
        int(z'b975d6b6ee39e436', i8), int(z'b3e2fd538e122b44', i8), int(z'e7d34c64a9c85d44', i8), &
        int(z'60dbbca87196b616', i8), int(z'90e40fbeea1d3a4a', i8), int(z'bc8955e946fe31cd', i8), &
        int(z'b51d13aea4a488dd', i8), int(z'6babab6398bdbe41', i8), int(z'e264589a4dcdab14', i8), &
        int(z'c696963c7eed2dd1', i8), int(z'8d7eb76070a08aec', i8), int(z'fc1e1de5cf543ca2', i8), &
        int(z'b0de65388cc8ada8', i8), int(z'3b25a55f43294bcb', i8), int(z'dd15fe86affad912', i8), &
        int(z'49ef0eb713f39ebe', i8), int(z'8a2dbf142dfcc7ab', i8), int(z'6e3569326c784337', i8), &
        int(z'acb92ed9397bf996', i8), int(z'49c2c37f07965404', i8), int(z'd7e77a8f87daf7fb', i8), &
        int(z'dc33745ec97be906', i8), int(z'86f0ac99b4e8dafd', i8), int(z'69a028bb3ded71a3', i8), &
        int(z'a8acd7c0222311bc', i8), int(z'c40832ea0d68ce0c', i8), int(z'd2d80db02aabd62b', i8), &
        int(z'f50a3fa490c30190', i8), int(z'83c7088e1aab65db', i8), int(z'792667c6da79e0fa', i8), &
        int(z'a4b8cab1a1563f52', i8), int(z'577001b891185938', i8), int(z'cde6fd5e09abcf26', i8), &
        int(z'ed4c0226b55e6f86', i8), int(z'80b05e5ac60b6178', i8), int(z'544f8158315b05b4', i8), &
        int(z'a0dc75f1778e39d6', i8), int(z'696361ae3db1c721', i8), int(z'c913936dd571c84c', i8), &
        int(z'3bc3a19cd1e38e9', i8), int(z'fb5878494ace3a5f', i8), int(z'4ab48a04065c723', i8), &
        int(z'9d174b2dcec0e47b', i8), int(z'62eb0d64283f9c76', i8), int(z'c45d1df942711d9a', i8), &
        int(z'3ba5d0bd324f8394', i8), int(z'f5746577930d6500', i8), int(z'ca8f44ec7ee36479', i8), &
        int(z'9968bf6abbe85f20', i8), int(z'7e998b13cf4e1ecb', i8), int(z'bfc2ef456ae276e8', i8), &
        int(z'9e3fedd8c321a67e', i8), int(z'efb3ab16c59b14a2', i8), int(z'c5cfe94ef3ea101e', i8), &
        int(z'95d04aee3b80ece5', i8), int(z'bba1f1d158724a12', i8), int(z'bb445da9ca61281f', i8), &
        int(z'2a8a6e45ae8edc97', i8), int(z'ea1575143cf97226', i8), int(z'f52d09d71a3293bd', i8), &
        int(z'924d692ca61be758', i8), int(z'593c2626705f9c56', i8), int(z'b6e0c377cfa2e12e', i8), &
        int(z'6f8b2fb00c77836c', i8), int(z'e498f455c38b997a', i8), int(z'b6dfb9c0f956447', i8), &
        int(z'8edf98b59a373fec', i8), int(z'4724bd4189bd5eac', i8), int(z'b2977ee300c50fe7', i8), &
        int(z'58edec91ec2cb657', i8), int(z'df3d5e9bc0f653e1', i8), int(z'2f2967b66737e3ed', i8), &
        int(z'8b865b215899f46c', i8), int(z'bd79e0d20082ee74', i8), int(z'ae67f1e9aec07187', i8), &
        int(z'ecd8590680a3aa11', i8), int(z'da01ee641a708de9', i8), int(z'e80e6f4820cc9495', i8), &
        int(z'884134fe908658b2', i8), int(z'3109058d147fdcdd', i8), int(z'aa51823e34a7eede', i8), &
        int(z'bd4b46f0599fd415', i8), int(z'd4e5e2cdc1d1ea96', i8), int(z'6c9e18ac7007c91a', i8), &
        int(z'850fadc09923329e', i8), int(z'3e2cf6bc604ddb0', i8), int(z'a6539930bf6bff45', i8), &
        int(z'84db8346b786151c', i8), int(z'cfe87f7cef46ff16', i8), int(z'e612641865679a63', i8), &
        int(z'81f14fae158c5f6e', i8), int(z'4fcb7e8f3f60c07e', i8), int(z'a26da3999aef7749', i8), &
        int(z'e3be5e330f38f09d', i8), int(z'cb090c8001ab551c', i8), int(z'5cadf5bfd3072cc5', i8), &
        int(z'fdcb4fa002162a63', i8), int(z'73d9732fc7c8f7f6', i8), int(z'9e9f11c4014dda7e', i8), &
        int(z'2867e7fddcdd9afa', i8), int(z'c646d63501a1511d', i8), int(z'b281e1fd541501b8', i8), &
        int(z'f7d88bc24209a565', i8), int(z'1f225a7ca91a4226', i8), int(z'9ae757596946075f', i8), &
        int(z'3375788de9b06958', i8), int(z'c1a12d2fc3978937', i8), int(z'52d6b1641c83ae', i8), &
        int(z'f209787bb47d6b84', i8), int(z'c0678c5dbd23a49a', i8), int(z'9745eb4d50ce6332', i8), &
        int(z'f840b7ba963646e0', i8), int(z'bd176620a501fbff', i8), int(z'b650e5a93bc3d898', i8), &
        int(z'ec5d3fa8ce427aff', i8), int(z'a3e51f138ab4cebe', i8), int(z'93ba47c980e98cdf', i8), &
        int(z'c66f336c36b10137', i8), int(z'b8a8d9bbe123f017', i8), int(z'b80b0047445d4184', i8), &
        int(z'e6d3102ad96cec1d', i8), int(z'a60dc059157491e5', i8), int(z'9043ea1ac7e41392', i8), &
        int(z'87c89837ad68db2f', i8), int(z'b454e4a179dd1877', i8), int(z'29babe4598c311fb', i8), &
        int(z'e16a1dc9d8545e94', i8), int(z'f4296dd6fef3d67a', i8), int(z'8ce2529e2734bb1d', i8), &
        int(z'1899e4a65f58660c', i8), int(z'b01ae745b101e9e4', i8), int(z'5ec05dcff72e7f8f', i8), &
        int(z'dc21a1171d42645d', i8), int(z'76707543f4fa1f73', i8), int(z'899504ae72497eba', i8), &
        int(z'6a06494a791c53a8', i8), int(z'abfa45da0edbde69', i8), int(z'487db9d17636892', i8), &
        int(z'd6f8d7509292d603', i8), int(z'45a9d2845d3c42b6', i8), int(z'865b86925b9bc5c2', i8), &
        int(z'b8a2392ba45a9b2', i8), int(z'a7f26836f282b732', i8), int(z'8e6cac7768d7141e', i8), &
        int(z'd1ef0244af2364ff', i8), int(z'3207d795430cd926', i8), int(z'8335616aed761f1f', i8), &
        int(z'7f44e6bd49e807b8', i8), int(z'a402b9c5a8d3a6e7', i8), int(z'5f16206c9c6209a6', i8), &
        int(z'cd036837130890a1', i8), int(z'36dba887c37a8c0f', i8), int(z'802221226be55a64', i8), &
        int(z'c2494954da2c9789', i8), int(z'a02aa96b06deb0fd', i8), int(z'f2db9baa10b7bd6c', i8), &
        int(z'c83553c5c8965d3d', i8), int(z'6f92829494e5acc7', i8), int(z'fa42a8b73abbf48c', i8), &
        int(z'cb772339ba1f17f9', i8), int(z'9c69a97284b578d7', i8), int(z'ff2a760414536efb', i8), &
        int(z'c38413cf25e2d70d', i8), int(z'fef5138519684aba', i8), int(z'f46518c2ef5b8cd1', i8), &
        int(z'7eb258665fc25d69', i8), int(z'98bf2f79d5993802', i8), int(z'ef2f773ffbd97a61', i8), &
        int(z'beeefb584aff8603', i8), int(z'aafb550ffacfd8fa', i8), int(z'eeaaba2e5dbf6784', i8), &
        int(z'95ba2a53f983cf38', i8), int(z'952ab45cfa97a0b2', i8), int(z'dd945a747bf26183', i8), &
        int(z'ba756174393d88df', i8), int(z'94f971119aeef9e4', i8) ]
    integer(i8), parameter :: POWER5_6(200) = [ &
        int(z'e912b9d1478ceb17', i8), int(z'7a37cd5601aab85d', i8), int(z'91abb422ccb812ee', i8), &
        int(z'ac62e055c10ab33a', i8), int(z'b616a12b7fe617aa', i8), int(z'577b986b314d6009', i8), &
        int(z'e39c49765fdf9d94', i8), int(z'ed5a7e85fda0b80b', i8), int(z'8e41ade9fbebc27d', i8), &
        int(z'14588f13be847307', i8), int(z'b1d219647ae6b31c', i8), int(z'596eb2d8ae258fc8', i8), &
        int(z'de469fbd99a05fe3', i8), int(z'6fca5f8ed9aef3bb', i8), int(z'8aec23d680043bee', i8), &
        int(z'25de7bb9480d5854', i8), int(z'ada72ccc20054ae9', i8), int(z'af561aa79a10ae6a', i8), &
        int(z'd910f7ff28069da4', i8), int(z'1b2ba1518094da04', i8), int(z'87aa9aff79042286', i8), &
        int(z'90fb44d2f05d0842', i8), int(z'a99541bf57452b28', i8), int(z'353a1607ac744a53', i8), &
        int(z'd3fa922f2d1675f2', i8), int(z'42889b8997915ce8', i8), int(z'847c9b5d7c2e09b7', i8), &
        int(z'69956135febada11', i8), int(z'a59bc234db398c25', i8), int(z'43fab9837e699095', i8), &
        int(z'cf02b2c21207ef2e', i8), int(z'94f967e45e03f4bb', i8), int(z'8161afb94b44f57d', i8), &
        int(z'1d1be0eebac278f5', i8), int(z'a1ba1ba79e1632dc', i8), int(z'6462d92a69731732', i8), &
        int(z'ca28a291859bbf93', i8), int(z'7d7b8f7503cfdcfe', i8), int(z'fcb2cb35e702af78', i8), &
        int(z'5cda735244c3d43e', i8), int(z'9defbf01b061adab', i8), int(z'3a0888136afa64a7', i8), &
        int(z'c56baec21c7a1916', i8), int(z'88aaa1845b8fdd0', i8), int(z'f6c69a72a3989f5b', i8), &
        int(z'8aad549e57273d45', i8), int(z'9a3c2087a63f6399', i8), int(z'36ac54e2f678864b', i8), &
        int(z'c0cb28a98fcf3c7f', i8), int(z'84576a1bb416a7dd', i8), int(z'f0fdf2d3f3c30b9f', i8), &
        int(z'656d44a2a11c51d5', i8), int(z'969eb7c47859e743', i8), int(z'9f644ae5a4b1b325', i8), &
        int(z'bc4665b596706114', i8), int(z'873d5d9f0dde1fee', i8), int(z'eb57ff22fc0c7959', i8), &
        int(z'a90cb506d155a7ea', i8), int(z'9316ff75dd87cbd8', i8), int(z'9a7f12442d588f2', i8), &
        int(z'b7dcbf5354e9bece', i8), int(z'c11ed6d538aeb2f', i8), int(z'e5d3ef282a242e81', i8), &
        int(z'8f1668c8a86da5fa', i8), int(z'8fa475791a569d10', i8), int(z'f96e017d694487bc', i8), &
        int(z'b38d92d760ec4455', i8), int(z'37c981dcc395a9ac', i8), int(z'e070f78d3927556a', i8), &
        int(z'85bbe253f47b1417', i8), int(z'8c469ab843b89562', i8), int(z'93956d7478ccec8e', i8), &
        int(z'af58416654a6babb', i8), int(z'387ac8d1970027b2', i8), int(z'db2e51bfe9d0696a', i8), &
        int(z'6997b05fcc0319e', i8), int(z'88fcf317f22241e2', i8), int(z'441fece3bdf81f03', i8), &
        int(z'ab3c2fddeeaad25a', i8), int(z'd527e81cad7626c3', i8), int(z'd60b3bd56a5586f1', i8), &
        int(z'8a71e223d8d3b074', i8), int(z'85c7056562757456', i8), int(z'f6872d5667844e49', i8), &
        int(z'a738c6bebb12d16c', i8), int(z'b428f8ac016561db', i8), int(z'd106f86e69d785c7', i8), &
        int(z'e13336d701beba52', i8), int(z'82a45b450226b39c', i8), int(z'ecc0024661173473', i8), &
        int(z'a34d721642b06084', i8), int(z'27f002d7f95d0190', i8), int(z'cc20ce9bd35c78a5', i8), &
        int(z'31ec038df7b441f4', i8), int(z'ff290242c83396ce', i8), int(z'7e67047175a15271', i8), &
        int(z'9f79a169bd203e41', i8), int(z'f0062c6e984d386', i8), int(z'c75809c42c684dd1', i8), &
        int(z'52c07b78a3e60868', i8), int(z'f92e0c3537826145', i8), int(z'a7709a56ccdf8a82', i8), &
        int(z'9bbcc7a142b17ccb', i8), int(z'88a66076400bb691', i8), int(z'c2abf989935ddbfe', i8), &
        int(z'6acff893d00ea435', i8), int(z'f356f7ebf83552fe', i8), int(z'583f6b8c4124d43', i8), &
        int(z'98165af37b2153de', i8), int(z'c3727a337a8b704a', i8), int(z'be1bf1b059e9a8d6', i8), &
        int(z'744f18c0592e4c5c', i8), int(z'eda2ee1c7064130c', i8), int(z'1162def06f79df73', i8), &
        int(z'9485d4d1c63e8be7', i8), int(z'8addcb5645ac2ba8', i8), int(z'b9a74a0637ce2ee1', i8), &
        int(z'6d953e2bd7173692', i8), int(z'e8111c87c5c1ba99', i8), int(z'c8fa8db6ccdd0437', i8), &
        int(z'910ab1d4db9914a0', i8), int(z'1d9c9892400a22a2', i8), int(z'b54d5e4a127f59c8', i8), &
        int(z'2503beb6d00cab4b', i8), int(z'e2a0b5dc971f303a', i8), int(z'2e44ae64840fd61d', i8), &
        int(z'8da471a9de737e24', i8), int(z'5ceaecfed289e5d2', i8), int(z'b10d8e1456105dad', i8), &
        int(z'7425a83e872c5f47', i8), int(z'dd50f1996b947518', i8), int(z'd12f124e28f77719', i8), &
        int(z'8a5296ffe33cc92f', i8), int(z'82bd6b70d99aaa6f', i8), int(z'ace73cbfdc0bfb7b', i8), &
        int(z'636cc64d1001550b', i8), int(z'd8210befd30efa5a', i8), int(z'3c47f7e05401aa4e', i8), &
        int(z'8714a775e3e95c78', i8), int(z'65acfaec34810a71', i8), int(z'a8d9d1535ce3b396', i8), &
        int(z'7f1839a741a14d0d', i8), int(z'd31045a8341ca07c', i8), int(z'1ede48111209a050', i8), &
        int(z'83ea2b892091e44d', i8), int(z'934aed0aab460432', i8), int(z'a4e4b66b68b65d60', i8), &
        int(z'f81da84d5617853f', i8), int(z'ce1de40642e3f4b9', i8), int(z'36251260ab9d668e', i8), &
        int(z'80d2ae83e9ce78f3', i8), int(z'c1d72b7c6b426019', i8), int(z'a1075a24e4421730', i8), &
        int(z'b24cf65b8612f81f', i8), int(z'c94930ae1d529cfc', i8), int(z'dee033f26797b627', i8), &
        int(z'fb9b7cd9a4a7443c', i8), int(z'169840ef017da3b1', i8), int(z'9d412e0806e88aa5', i8), &
        int(z'8e1f289560ee864e', i8), int(z'c491798a08a2ad4e', i8), int(z'f1a6f2bab92a27e2', i8), &
        int(z'f5b5d7ec8acb58a2', i8), int(z'ae10af696774b1db', i8), int(z'9991a6f3d6bf1765', i8), &
        int(z'acca6da1e0a8ef29', i8), int(z'bff610b0cc6edd3f', i8), int(z'17fd090a58d32af3', i8), &
        int(z'eff394dcff8a948e', i8), int(z'ddfc4b4cef07f5b0', i8), int(z'95f83d0a1fb69cd9', i8), &
        int(z'4abdaf101564f98e', i8), int(z'bb764c4ca7a4440f', i8), int(z'9d6d1ad41abe37f1', i8), &
        int(z'ea53df5fd18d5513', i8), int(z'84c86189216dc5ed', i8), int(z'92746b9be2f8552c', i8), &
        int(z'32fd3cf5b4e49bb4', i8), int(z'b7118682dbb66a77', i8), int(z'3fbc8c33221dc2a1', i8), &
        int(z'e4d5e82392a40515', i8), int(z'fabaf3feaa5334a', i8), int(z'8f05b1163ba6832d', i8), &
        int(z'29cb4d87f2a7400e', i8), int(z'b2c71d5bca9023f8', i8), int(z'743e20e9ef511012', i8), &
        int(z'df78e4b2bd342cf6', i8), int(z'914da9246b255416', i8), int(z'8bab8eefb6409c1a', i8), &
        int(z'1ad089b6c2f7548e', i8), int(z'ae9672aba3d0c320', i8), int(z'a184ac2473b529b1', i8), &
        int(z'da3c0f568cc4f3e8', i8), int(z'c9e5d72d90a2741e', i8), int(z'8865899617fb1871', i8), &
        int(z'7e2fa67c7a658892', i8), int(z'aa7eebfb9df9de8d', i8), int(z'ddbb901b98feeab7', i8), &
        int(z'd51ea6fa85785631', i8), int(z'552a74227f3ea565', i8) ]
    integer(i8), parameter :: POWER5_7(102) = [ &
        int(z'8533285c936b35de', i8), int(z'd53a88958f87275f', i8), int(z'a67ff273b8460356', i8), &
        int(z'8a892abaf368f137', i8), int(z'd01fef10a657842c', i8), int(z'2d2b7569b0432d85', i8), &
        int(z'8213f56a67f6b29b', i8), int(z'9c3b29620e29fc73', i8), int(z'a298f2c501f45f42', i8), &
        int(z'8349f3ba91b47b8f', i8), int(z'cb3f2f7642717713', i8), int(z'241c70a936219a73', i8), &
        int(z'fe0efb53d30dd4d7', i8), int(z'ed238cd383aa0110', i8), int(z'9ec95d1463e8a506', i8), &
        int(z'f4363804324a40aa', i8), int(z'c67bb4597ce2ce48', i8), int(z'b143c6053edcd0d5', i8), &
        int(z'f81aa16fdc1b81da', i8), int(z'dd94b7868e94050a', i8), int(z'9b10a4e5e9913128', i8), &
        int(z'ca7cf2b4191c8326', i8), int(z'c1d4ce1f63f57d72', i8), int(z'fd1c2f611f63a3f0', i8), &
        int(z'f24a01a73cf2dccf', i8), int(z'bc633b39673c8cec', i8), int(z'976e41088617ca01', i8), &
        int(z'd5be0503e085d813', i8), int(z'bd49d14aa79dbc82', i8), int(z'4b2d8644d8a74e18', i8), &
        int(z'ec9c459d51852ba2', i8), int(z'ddf8e7d60ed1219e', i8), int(z'93e1ab8252f33b45', i8), &
        int(z'cabb90e5c942b503', i8), int(z'b8da1662e7b00a17', i8), int(z'3d6a751f3b936243', i8), &
        int(z'e7109bfba19c0c9d', i8), int(z'cc512670a783ad4', i8), int(z'906a617d450187e2', i8), &
        int(z'27fb2b80668b24c5', i8), int(z'b484f9dc9641e9da', i8), int(z'b1f9f660802dedf6', i8), &
        int(z'e1a63853bbd26451', i8), int(z'5e7873f8a0396973', i8), int(z'8d07e33455637eb2', i8), &
        int(z'db0b487b6423e1e8', i8), int(z'b049dc016abc5e5f', i8), int(z'91ce1a9a3d2cda62', i8), &
        int(z'dc5c5301c56b75f7', i8), int(z'7641a140cc7810fb', i8), int(z'89b9b3e11b6329ba', i8), &
        int(z'a9e904c87fcb0a9d', i8), int(z'ac2820d9623bf429', i8), int(z'546345fa9fbdcd44', i8), &
        int(z'd732290fbacaf133', i8), int(z'a97c177947ad4095', i8), int(z'867f59a9d4bed6c0', i8), &
        int(z'49ed8eabcccc485d', i8), int(z'a81f301449ee8c70', i8), int(z'5c68f256bfff5a74', i8), &
        int(z'd226fc195c6a2f8c', i8), int(z'73832eec6fff3111', i8), int(z'83585d8fd9c25db7', i8), &
        int(z'c831fd53c5ff7eab', i8), int(z'a42e74f3d032f525', i8), int(z'ba3e7ca8b77f5e55', i8), &
        int(z'cd3a1230c43fb26f', i8), int(z'28ce1bd2e55f35eb', i8), int(z'80444b5e7aa7cf85', i8), &
        int(z'7980d163cf5b81b3', i8), int(z'a0555e361951c366', i8), int(z'd7e105bcc332621f', i8), &
        int(z'c86ab5c39fa63440', i8), int(z'8dd9472bf3fefaa7', i8), int(z'fa856334878fc150', i8), &
        int(z'b14f98f6f0feb951', i8), int(z'9c935e00d4b9d8d2', i8), int(z'6ed1bf9a569f33d3', i8), &
        int(z'c3b8358109e84f07', i8), int(z'a862f80ec4700c8', i8), int(z'f4a642e14c6262c8', i8), &
        int(z'cd27bb612758c0fa', i8), int(z'98e7e9cccfbd7dbd', i8), int(z'8038d51cb897789c', i8), &
        int(z'bf21e44003acdd2c', i8), int(z'e0470a63e6bd56c3', i8), int(z'eeea5d5004981478', i8), &
        int(z'1858ccfce06cac74', i8), int(z'95527a5202df0ccb', i8), int(z'f37801e0c43ebc8', i8), &
        int(z'baa718e68396cffd', i8), int(z'd30560258f54e6ba', i8), int(z'e950df20247c83fd', i8), &
        int(z'47c6b82ef32a2069', i8), int(z'91d28b7416cdd27e', i8), int(z'4cdc331d57fa5441', i8), &
        int(z'b6472e511c81471d', i8), int(z'e0133fe4adf8e952', i8), int(z'e3d8f9e563a198e5', i8), &
        int(z'58180fddd97723a6', i8), int(z'8e679c2f5e44ff8f', i8), int(z'570f09eaa7ea7648', i8) ]
    integer(i8), parameter :: POWER5_TABLE(POWER5_TABLE_SIZE) = &
        [ POWER5_1, POWER5_2, POWER5_3, POWER5_4, POWER5_5, POWER5_6, POWER5_7 ]

    integer, private :: i
    real(dp), parameter :: DOUBLE_POW10(-22:22) = [ (10**real(i,dp), i=-22, 22) ]
    real(sp), parameter :: FLOAT_POW10 (-10:10) = [ (10**real(i,sp), i=-10, 10) ]

    integer(i8), parameter :: DOUBLE_MAX_MANTISSA(0:23) = [ &
        [(ishft(2_i8, DOUBLE_MANTISSA_BITS) / 10_i8**i, i=0, 18)], &
        472_i8, 94_i8, 18_i8, 3_i8, 0_i8 ]

    integer(i8), parameter :: FLOAT_MAX_MANTISSA(0:11) = &
        [(ishft(2_i8, FLOAT_MANTISSA_BITS) / 10_i8**i, i=0, 11)]

    integer(i8), parameter :: POW10_U64(0:19) = &
        [ [(10_i8**i, i=0, 18)], int(z'8AC7230489E80000', i8) ]

    integer, parameter :: MAX_DIGITS_U64(2:36) = [ &
        64, 41, 32, 28, 25, 23, 22, 21, 20, 19, 18, 18, 17, 17, 16, 16, &
        16, 16, 15, 15, 15, 15, 14, 14, 14, 14, 14, 14, 14, 13, 13, 13, &
        13, 13, 13 ]

    integer(i8), parameter :: MIN_SAFE_U64(2:36) = [ &
        int(z'8000000000000000', i8), int(z'a8b8b452291fe821', i8), int(z'4000000000000000', i8), &
        int(z'6765c793fa10079d', i8), int(z'41c21cb8e1000000', i8), int(z'3642798750226111', i8), &
        int(z'8000000000000000', i8), int(z'a8b8b452291fe821', i8), int(z'8ac7230489e80000', i8), &
        int(z'4d28cb56c33fa539', i8), int(z'1eca170c00000000', i8), int(z'780c7372621bd74d', i8), &
        int(z'1e39a5057d810000', i8), int(z'5b27ac993df97701', i8), int(z'1000000000000000', i8), &
        int(z'27b95e997e21d9f1', i8), int(z'5da0e1e53c5c8000', i8), int(z'd2ae3299c1c4aedb', i8), &
        int(z'16bcc41e90000000', i8), int(z'2d04b7fdd9c0ef49', i8), int(z'5658597bcaa24000', i8), &
        int(z'a0e2073737609371', i8), int(z'0c29e98000000000', i8), int(z'14adf4b7320334b9', i8), &
        int(z'226ed36478bfa000', i8), int(z'383d9170b85ff80b', i8), int(z'5a3c23e39c000000', i8), &
        int(z'8e65137388122bcd', i8), int(z'dd41bb36d259e000', i8), int(z'0aee5720ee830681', i8), &
        int(z'1000000000000000', i8), int(z'172588ad4f5f0981', i8), int(z'211e44f7d02c1000', i8), &
        int(z'2ee56725f06e5c71', i8), int(z'41c21cb8e1000000', i8) ]

    integer, parameter :: LARGE_POWER_OF_FIVE_SIZE = 135
    integer(i8), parameter :: SMALL_POWER_OF_FIVE(0:27) = &
        [(5_i8**i, i=0, 27)]

    integer(i8), parameter :: LARGE_POWER_OF_FIVE(5) = [ &
        int(z'13a1d71cff1b172d', i8), int(z'7f682d3defa07617', i8), int(z'3f0131e7ff8c90c0', i8), &
        int(z'917b01773fdcb9fe', i8), int(z'02c06b9d16c407a7', i8) ]

    integer, parameter :: CHAR_TO_DIGIT_TABLE(0:255) = [ &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
          0,   1,   2,   3,   4,   5,   6,   7, &
          8,   9, 255, 255, 255, 255, 255, 255, &
        255,  10,  11,  12,  13,  14,  15,  16, &
         17,  18,  19,  20,  21,  22,  23,  24, &
         25,  26,  27,  28,  29,  30,  31,  32, &
         33,  34,  35, 255, 255, 255, 255, 255, &
        255,  10,  11,  12,  13,  14,  15,  16, &
         17,  18,  19,  20,  21,  22,  23,  24, &
         25,  26,  27,  28,  29,  30,  31,  32, &
         33,  34,  35, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255, &
        255, 255, 255, 255, 255, 255, 255, 255 ]

    interface from_chars
        module procedure from_chars_64
        module procedure from_chars_32
    end interface from_chars

    interface is_digit
        module procedure is_digit_char
        module procedure is_digit_int
    end interface

    public :: operator(==), operator(/=)

    interface operator(==)
        module procedure outcome_eq
    end interface

    interface operator(/=)
        module procedure outcome_ne
    end interface

contains

    ! ===== Fast path for simple numbers =====

    !> Unified fast path for integers and simple fixed-point decimals.
    pure elemental subroutine try_fast_path(first, last, str, opts, bj, a)
        integer, intent(in) :: first, last
        character(len=*), intent(in) :: str
        type(parse_options), intent(in) :: opts
        logical, intent(in) :: bj
        type(parsed_number), intent(out) :: a
        integer(i8) :: mantissa
        integer :: int_digits, frac_digits, p, ic

        a%valid = .false.
        if (first > last) return
        if (iand(opts%format, ior(FMT_FORTRAN, FMT_SKIP_WS)) /= 0 .or. &
            iand(opts%format, FMT_FIXED) == 0 .or. &
            opts%decimal_point /= '.') return

        p = first
        if (str(p:p) == '-') then
            a%negative = .true.
            p = p + 1
        else if (str(p:p) == '+') then
            if (bj .or. iand(opts%format, FMT_ALLOW_PLUS) == 0) return
            p = p + 1
        end if
        if (p > last) return

        ! Parse integer digits
        a%int_start = p
        mantissa = 0_i8
        int_digits = 0
        do while (p <= last .and. int_digits < 19)
            ic = iachar(str(p:p)) - 48
            if (ic < 0 .or. ic > 9) exit
            mantissa = 10_i8 * mantissa + int(ic, i8)
            int_digits = int_digits + 1
            p = p + 1
        end do
        if (int_digits == 0) return
        if (bj .and. int_digits > 1 .and. str(a%int_start:a%int_start) == '0') return
        a%int_len = int_digits

        ! Check what follows the integer part
        if (p > last) then
            ! Pure integer: consumed entire string
            a%mantissa = mantissa
            a%exponent = 0_i8
            a%last_idx = p
            a%valid = .true.
            return
        end if

        ! If next char is not '.', bail out
        if (str(p:p) /= '.') return
        if (iand(opts%format, FMT_SCIENTIFIC) == 0) return

        ! Parse fractional part
        p = p + 1
        if (p > last) return
        frac_digits = last - p + 1
        if (int_digits + frac_digits > 19) return
        a%frac_start = p
        call loop_parse_eight(p, last, str, mantissa)
        call loop_parse_four(p, last, str, mantissa)
        do while (p <= last)
            ic = iachar(str(p:p)) - 48
            if (ic < 0 .or. ic > 9) return
            mantissa = 10_i8 * mantissa + int(ic, i8)
            p = p + 1
        end do
        if (frac_digits == 0) return

        a%frac_len = frac_digits
        a%mantissa = mantissa
        a%exponent = -int(frac_digits, i8)
        a%last_idx = p
        a%valid = .true.
    end subroutine try_fast_path

    ! ===== Unsigned integer utilities =====

    !> Compare two unsigned 64-bit integers (a < b).
    pure elemental logical function unsigned_lt(a, b)
        integer(i8), intent(in) :: a, b
        unsigned_lt = ieor(a, SB64) < ieor(b, SB64)
    end function unsigned_lt

    !> Compare two unsigned 64-bit integers (a >= b).
    pure elemental logical function unsigned_ge(a, b)
        integer(i8), intent(in) :: a, b
        unsigned_ge = ieor(a, SB64) >= ieor(b, SB64)
    end function unsigned_ge

    !> Compare two unsigned 64-bit integers (a > b).
    pure elemental logical function unsigned_gt(a, b)
        integer(i8), intent(in) :: a, b
        unsigned_gt = ieor(a, SB64) > ieor(b, SB64)
    end function unsigned_gt

    ! ===== 128-bit multiplication =====

    !> Multiply two unsigned 64-bit integers, returning a 128-bit result.
    pure elemental subroutine mul_u64(a, b, res)
        integer(i8), intent(in) :: a, b
        type(u128), intent(out) :: res
        if (HAS_INT128) then
            block
                integer(IK128) :: za, zb, z
                if (LITTLE_ENDIAN) then
                    za = transfer([a, 0_i8], za)
                    zb = transfer([b, 0_i8], zb)
                    z  = za * zb
                    res = transfer(z, res)
                else
                    za = transfer([0_i8, a], za)
                    zb = transfer([0_i8, b], zb)
                    z  = za * zb
                    res%lo = int(z, i8)
                    res%hi = int(ishft(z, -64), i8)
                end if
            end block
        else
            block
                integer(i8) :: a0, a1, b0, b1, w0, t, w1, w2
                a0 = iand(a, M32)
                a1 = iand(ishft(a, -32), M32)
                b0 = iand(b, M32)
                b1 = iand(ishft(b, -32), M32)
                w0 = a0 * b0
                t  = a1*b0 + iand(ishft(w0, -32), M32)
                w1 = iand(t, M32)
                w2 = iand(ishft(t, -32), M32)
                w1 = w1 + a0 * b1
                res%lo = a * b
                res%hi = a1*b1 + w2 + iand(ishft(w1, -32), M32)
            end block
        end if
    end subroutine mul_u64

    ! ===== Character and digit utilities =====

    !> Count leading zero bits in a 64-bit integer.
    pure elemental integer function count_leading_zeros(x)
        integer(i8), intent(in) :: x
        count_leading_zeros = merge(64, leadz(x), x == 0)
    end function count_leading_zeros

    !> Check if a character is an ASCII digit.
    pure elemental logical function is_digit_char(c)
        character, intent(in) :: c
        integer :: ic
        ic = iachar(c)
        is_digit_char = is_digit_int(ic)
    end function is_digit_char

    ! Check if a chacter integer is an ASCII digit
    pure elemental logical function is_digit_int(ic)
        integer, intent(in) :: ic
        is_digit_int = ic >= 48 .and. ic <= 57
    end function is_digit_int

    !> Check if a character is whitespace.
    pure elemental logical function is_space(c)
        character, intent(in) :: c
        integer :: ic
        ic = iachar(c)
        is_space = (ic >= 9 .and. ic <= 13) .or. ic == 32
    end function is_space

    !> Map a character to its digit value using the lookup table.
    pure elemental integer function char_to_digit(c)
        character, intent(in) :: c
        integer :: ic
        ic = iachar(c)
        char_to_digit = 255
        if (ic >= 0 .and. ic <= 255) char_to_digit = CHAR_TO_DIGIT_TABLE(ic)
    end function char_to_digit

    !> Reinterpret 8 characters as a single i8 (little-endian).
    pure elemental integer(i8) function read8_to_u64(str)
        character(len=8), intent(in) :: str
        integer :: j
        if (LITTLE_ENDIAN) then
            read8_to_u64 = transfer(str, 0_i8)
        else
            read8_to_u64 = iachar(str(1:1), kind=i8)
            do j = 2, 8
                read8_to_u64 = ior(read8_to_u64, ishft(int(iachar(str(j:j)), i8), 8*(j-1)))
            end do
        end if
    end function read8_to_u64

    !> Reinterpret 4 characters as a single i4 (little-endian).
    pure elemental integer(i4) function read4_to_u32(str)
        character(len=4), intent(in) :: str
        integer :: j
        if (LITTLE_ENDIAN) then
            read4_to_u32 = transfer(str, 0_i4)
        else
            read4_to_u32 = iachar(str(1:1), kind=i4)
            do j = 2, 4
                read4_to_u32 = ior(read4_to_u32, ishft(int(iachar(str(j:j)), i4), 8*(j-1)))
            end do
        end if
    end function read4_to_u32

    !> Check if 8 packed bytes are all ASCII digits.
    pure elemental logical function is_eight_digits(val)
        integer(i8), intent(in) :: val
        integer(i8) :: v1, v2
        v1 = val + int(z'4646464646464646', i8)
        v2 = val - int(z'3030303030303030', i8)
        is_eight_digits = iand(ior(v1, v2), int(z'8080808080808080', i8)) == 0
    end function is_eight_digits

    !> Parse 8 ASCII digit bytes into an integer.
    pure elemental integer function parse_eight_digits(val) result(res)
        integer(i8), intent(in) :: val
        integer(i8) :: v
        integer(i8), parameter :: m  = int(z'000000FF000000FF', i8)
        integer(i8), parameter :: m1 = int(z'000F424000000064', i8)
        integer(i8), parameter :: m2 = int(z'0000271000000001', i8)
        v   = val - int(z'3030303030303030', i8)
        v   = v*10 + ishft(v, -8)
        v   = ishft(iand(v, m)*m1 + iand(ishft(v, -16), m)*m2, -32)
        res = int(iand(v, int(z'FFFFFFFF', i8)), i4)
    end function parse_eight_digits

    !> Check if 4 packed bytes are all ASCII digits.
    pure elemental logical function is_four_digits(val)
        integer(i4), intent(in) :: val
        integer(i8) :: z, v1, v2
        z = iand(int(val, i8), int(z'00000000FFFFFFFF', i8))
        v1 = z + int(z'46464646', i8)
        v2 = z - int(z'30303030', i8)
        is_four_digits = iand(ior(v1, v2), int(z'80808080', i8)) == 0
    end function is_four_digits

    !> Parse 4 ASCII digit bytes into an integer.
    pure elemental integer function parse_four_digits(val) result(res)
        integer(i4), intent(in) :: val
        integer(i8) :: v
        v = iand(int(val, i8), int(z'00000000FFFFFFFF', i8))
        v = v - int(z'30303030', i8)
        v = v*10_i8 + ishft(v, -8)
        res = int(iand(ishft(iand(v, int(z'0000000000FF00FF', i8)) * int(z'0000000000640001', i8), -16), &
                       int(z'000000000000FFFF', i8)), kind(res))
    end function parse_four_digits

    !> Parse groups of 8 digits in a loop.
    pure elemental subroutine loop_parse_eight(pos, last, str, i)
        integer, intent(in) :: last
        integer, intent(inout) :: pos
        character(len=*), intent(in) :: str
        integer(i8), intent(inout) :: i
        integer(i8) :: val
        do while (last - pos + 1 >= 8)
            val = read8_to_u64(str(pos:))
            if (.not. is_eight_digits(val)) exit
            i = i * 100000000_i8 + int(parse_eight_digits(val), i8)
            pos = pos + 8
        end do
    end subroutine loop_parse_eight

    !> Parse groups of 4 digits in a loop.
    pure elemental subroutine loop_parse_four(pos, last, str, i)
        integer, intent(in) :: last
        integer, intent(inout) :: pos
        character(len=*), intent(in) :: str
        integer(i8), intent(inout) :: i
        integer(i4) :: val
        do while (last - pos + 1 >= 4)
            val = read4_to_u32(str(pos:))
            if (.not. is_four_digits(val)) exit
            i = i * 10000_i8 + int(parse_four_digits(val), i8)
            pos = pos + 4
        end do
    end subroutine loop_parse_four

    !> Case-insensitive comparison of 3 characters.
    pure logical function strcmpi3(str, e3)
        character(3), intent(in) :: str
        character(3), intent(in) :: e3
        integer :: i
        strcmpi3 = .false.
        do i = 1, 3
            if (ior(iachar(str(i:i)), 32) /= iachar(e3(i:i))) return
        end do
        strcmpi3 = .true.
    end function strcmpi3

    !> Case-insensitive comparison of 5 characters.
    pure logical function strcmpi5(str, e5)
        character(5), intent(in) :: str
        character(5), intent(in) :: e5
        integer :: i
        strcmpi5 = .false.
        do i = 1, 5
            if (ior(iachar(str(i:i)), 32) /= iachar(e5(i:i))) return
        end do
        strcmpi5 = .true.
    end function strcmpi5

    ! ===== Number string parser =====

    !> Parse a number string into mantissa, exponent, and sign.
    elemental subroutine parse_number_string(first, last, str, opts, bj, a)
        integer, intent(in) :: first, last
        character(*), intent(in) :: str
        type(parse_options), intent(in) :: opts
        logical, intent(in) :: bj
        type(parsed_number), intent(out) :: a

        integer(i8) :: i, dc, exp, en
        integer(i8), parameter :: m19 = 1000000000000000000_i8
        integer :: p, sd, eip, bf, le, ic, ie, fe, sp
        logical :: alp, hdp, ne, hse, hed

        associate(fmt=>opts%format, dp=>opts%decimal_point)

        p = first
        a%valid = .false.
        if (p > last) then
            a%last_idx = p
            return
        end if

        a%negative = (str(p:p) == '-')
        alp = iand(fmt, FMT_ALLOW_PLUS) /= 0

        if (str(p:p) == '-' .or. (alp .and. .not. bj .and. str(p:p) == '+')) then
            p = p + 1
            if (p > last) then
                a%last_idx = p
                return
            end if
            if (bj) then
                if (.not. is_digit(str(p:p))) then
                    a%last_idx = p
                    return
                end if
            else
                if (.not. is_digit(str(p:p)) .and. str(p:p) /= dp) then
                    a%last_idx = p
                    return
                end if
            end if
        end if

        sd = p
        i = 0_i8
        do while (p <= last)
            ic = iachar(str(p:p))
            if (.not. is_digit(ic)) exit
            i = 10*i + int(ic - 48, i8)
            p = p + 1
        end do

        eip = p
        dc = int(eip - sd, i8)
        a%int_start = sd
        a%int_len = int(dc)

        if (bj) then
            if (dc == 0) then
                a%last_idx = p
                return
            end if
            if (str(sd:sd) == '0' .and. dc > 1) then
                a%last_idx = sd
                return
            end if
        end if

        exp = 0_i8
        a%frac_start = 0
        a%frac_len = 0
        hdp = .false.
        if (p <= last) hdp = (str(p:p) == dp)

        if (hdp) then
            p = p + 1
            bf = p
            call loop_parse_eight(p, last, str, i)
            call loop_parse_four(p, last, str, i)
            do while (p <= last)
                ic = iachar(str(p:p))
                if (.not. is_digit(ic)) exit
                i = i*10 + int(ic-48, i8)
                p = p + 1
            end do
            exp = int(bf - p, i8)
            a%frac_start = bf
            a%frac_len = p - bf
            dc = dc - exp
        end if

        if (bj) then
            if (hdp .and. exp == 0) then
                a%last_idx = p
                return
            end if
        else
            if (dc == 0) then
                a%last_idx = p
                return
            end if
        end if

        en = 0_i8
        hse = .false.
        if (p <= last) then
            hse = (iand(fmt, FMT_SCIENTIFIC) /= 0 .and. (str(p:p) == 'e' .or. str(p:p) == 'E')) .or. &
                  (iand(fmt, FMT_FORTRAN) /= 0 .and. (str(p:p) == '+' .or. str(p:p) == '-' .or. &
                                                     str(p:p) == 'd' .or. str(p:p) == 'D'))
        end if
        if (hse) then
            le = p
            if (str(p:p) == 'e' .or. str(p:p) == 'E' .or. str(p:p) == 'd' .or. str(p:p) == 'D') p = p + 1
            ne = .false.
            if (p <= last) then
                if (str(p:p) == '-') then
                    ne = .true.
                    p = p + 1
                else if (str(p:p) == '+') then
                    p = p + 1
                end if
            end if
            hed = .false.
            if (p <= last) hed = is_digit(str(p:p))
            if (.not. hed) then
                if (iand(fmt, FMT_FIXED) == 0) then
                    a%last_idx = p
                    return
                end if
                p = le
            else
                do while (p <= last)
                    ic = iachar(str(p:p))
                    if (.not. is_digit(ic)) exit
                    if (en < 268435456_i8) en = 10*en + int(ic-48, i8)
                    p = p + 1
                end do
                if (ne) en = -en
                exp = exp + en
            end if
        else
            if (iand(fmt, FMT_SCIENTIFIC) /= 0 .and. iand(fmt, FMT_FIXED) == 0) then
                a%last_idx = p
                return
            end if
        end if

        a%last_idx = p
        a%valid = .true.
        a%too_many_digits = .false.

        if (dc > 19) then
            sp = sd
            do while (sp <= last)
                if (str(sp:sp) /= '0' .and. str(sp:sp) /= dp) exit
                if (str(sp:sp) == '0') dc = dc - 1
                sp = sp + 1
            end do
            if (dc > 19) then
                a%too_many_digits = .true.
                i = 0
                p = a%int_start
                ie = p + a%int_len
                do while (unsigned_lt(i, m19) .and. p < ie)
                    i = i*10 + int( iachar(str(p:p)) - 48, i8)
                    p = p + 1
                end do
                if (unsigned_ge(i, m19)) then
                    exp = int(eip - p, i8) + en
                else
                    p = a%frac_start
                    fe = p + a%frac_len
                    do while (unsigned_lt(i, m19) .and. p < fe)
                        i = i*10 + int( iachar(str(p:p)) - 48, i8)
                        p = p + 1
                    end do
                    exp = int(a%frac_start - p, i8) + en
                end if
            end if
        end if
        a%exponent = exp
        a%mantissa = i

        endassociate

    end subroutine parse_number_string

    ! ===== Infinity and NaN parsing =====

    !> Parse inf/nan for single precision.
    elemental subroutine parse_infnan_32(str, p0, la, vf, res)
        character(*), intent(in) :: str
        integer, intent(in) :: p0, la
        real(sp), intent(out) :: vf
        type(parse_result), intent(out) :: res
        integer :: p, pp
        logical :: ms
        if (p0 > la) then
            res = parse_result(p0, OUTCOMES%INVALID_INPUT)
            return
        else
            res = parse_result(p0, OUTCOMES%OK)
        end if
        p = p0
        ms = str(p:p) == '-'
        if (ms .or. str(p:p) == '+') p = p + 1
        if (la - p + 1 >= 3) then
            if (strcmpi3(str(p:), 'nan')) then
                p = p + 3
                res%pos = p
                vf = ieee_value(0.0_sp, ieee_quiet_nan)
                if (ms) vf = -vf
                if (p <= la) then
                    if (str(p:p) == '(') then
                        pp = p + 1
                        do while (pp <= la)
                            if (str(pp:pp) == ')') then
                                res%pos = pp + 1
                                exit
                            end if
                            pp = pp + 1
                        end do
                    end if
                end if
                return
            end if
            if (strcmpi3(str(p:), 'inf')) then
                res%pos = p + 3
                if (la - p + 1 >= 8) then
                    if (strcmpi5(str(p+3:), 'inity')) res%pos = p + 8
                end if
                vf = ieee_value(0.0_sp, ieee_positive_inf)
                if (ms) vf = -vf
                return
            end if
        end if
        res%outcome = OUTCOMES%INVALID_INPUT
    end subroutine parse_infnan_32

    !> Parse inf/nan for double precision.
    elemental subroutine parse_infnan_64(str, p0, la, vd, res)
        character(*), intent(in) :: str
        integer, intent(in) :: p0, la
        real(dp), intent(out) :: vd
        type(parse_result), intent(out) :: res
        integer :: p, pp
        logical :: ms
        res%pos = p0
        if (p0 > la) then
            res%outcome = OUTCOMES%INVALID_INPUT
            return
        else
            res%outcome = OUTCOMES%OK
        end if
        p = p0
        ms = (str(p:p) == '-')
        if (str(p:p) == '-' .or. str(p:p) == '+') p = p + 1
        if (la - p + 1 >= 3) then
            if (strcmpi3(str(p:), 'nan')) then
                p = p + 3
                res%pos = p
                vd = ieee_value(0.0_dp, ieee_quiet_nan)
                if (ms) vd = -vd
                if (p <= la) then
                    if (str(p:p) == '(') then
                        pp = p + 1
                        do while (pp <= la)
                            if (str(pp:pp) == ')') then
                                res%pos = pp + 1
                                exit
                            end if
                            pp = pp + 1
                        end do
                    end if
                end if
                return
            end if
            if (strcmpi3(str(p:), 'inf')) then
                res%pos = p + 3
                if (la - p + 1 >= 8) then
                    if (strcmpi5(str(p+3:), 'inity')) res%pos = p + 8
                end if
                vd = ieee_value(0.0_dp, ieee_positive_inf)
                if (ms) vd = -vd
                return
            end if
        end if
        res%outcome = OUTCOMES%INVALID_INPUT
    end subroutine parse_infnan_64

    ! ===== Eisel-Lemire core algorithm =====

    !> Convert a base-10 exponent to base-2.
    pure elemental integer(i4) function b10_to_b2(q)
        integer(i4), intent(in) :: q
        b10_to_b2 = int(ishft( (152170 + 65536)*int(q, i8), -16), i4) + 63
    end function b10_to_b2

    !> Compute the product approximation for Eisel-Lemire.
    pure elemental subroutine compute_product(q, w, f, res)
        integer(i8), intent(in) :: q, w
        type(float_format), intent(in) :: f
        type(u128), intent(out) :: res
        integer(i8) :: pm, bp
        integer :: idx
        type(u128) :: sp
        bp = int(f%mantissa_bits + 3, i8)
        pm = ishft(not(0_i8), -int(bp))
        idx = 2*int(q - (-342_i8)) + 1
        call mul_u64(w, POWER5_TABLE(idx), res)
        if (iand(res%hi, pm) == pm) then
            call mul_u64(w, POWER5_TABLE(idx + 1), sp)
            res%lo = res%lo + sp%hi
            if (unsigned_lt(res%lo, sp%hi)) res%hi = res%hi + 1
        end if
    end subroutine compute_product

    !> Compute the Eisel-Lemire float approximation.
    pure elemental subroutine compute_float(q, wi, f, res)
        integer(i8), intent(in) :: q, wi
        type(float_format), intent(in) :: f
        type(adjusted_mantissa), intent(out) :: res
        integer(i8) :: w
        integer(i4) :: lz
        type(u128) :: pr
        integer :: ub, sa

        w = wi
        if (w == 0 .or. q < int(f%smallest_pow10, i8)) then
            res = adjusted_mantissa(0, 0)
            return
        end if
        if (q > int(f%largest_pow10, i8)) then
            res = adjusted_mantissa( 0, int(f%inf_power, i4))
            return
        end if
        lz = int(count_leading_zeros(w), i4)
        w = ishft(w, lz)
        call compute_product(q, w, f, pr)
        ub = int(ishft(pr%hi, -63))
        sa = ub + 64 - f%mantissa_bits - 3
        res%mantissa = ishft(pr%hi, -sa)
        res%power2 = b10_to_b2(int(q, i4)) + int(ub-lz-f%min_exponent, i4)

        if (res%power2 <= 0) then
            if (-res%power2 + 1 >= 64) then
                res = adjusted_mantissa( &
                    0_i8, 0_i4)
                return
            end if
            res%mantissa = ishft(res%mantissa, res%power2 - 1_i4)
            res%mantissa = res%mantissa + iand(res%mantissa, 1_i8)
            res%mantissa = ishft(res%mantissa, -1)
            if (unsigned_lt(res%mantissa, ishft(1_i8, f%mantissa_bits))) then
                res%power2 = 0
            else
                res%power2 = 1
            end if
            return
        end if

        if (iand(pr%lo, not(1_i8)) == 0 .and. &
            q >= int(f%min_round_trip_exp, i8) .and. q <= int(f%max_round_trip_exp, i8) .and. &
            iand(res%mantissa, 3_i8) == 1 .and. ishft(res%mantissa, sa) == pr%hi) &
                res%mantissa = iand(res%mantissa, not(1_i8))

        res%mantissa = res%mantissa + iand(res%mantissa, 1_i8)
        res%mantissa = ishft(res%mantissa, -1)

        if (unsigned_ge(res%mantissa, ishft(2_i8, f%mantissa_bits))) then
            res%mantissa = ishft(1_i8, f%mantissa_bits)
            res%power2 = res%power2 + 1
        end if

        res%mantissa = iand(res%mantissa, not(ishft(1_i8, f%mantissa_bits)))
        if (res%power2 >= int(f%inf_power, i4)) then
            res%power2 = int(f%inf_power, i4)
            res%mantissa = 0
        end if
    end subroutine compute_float

    !> Compute error-scaled adjusted mantissa.
    pure elemental subroutine compute_error_scaled(q, wi, lz, f, res)
        integer(i8), intent(in) :: q, wi
        integer(i4), intent(in) :: lz
        type(float_format), intent(in) :: f
        type(adjusted_mantissa), intent(out) :: res
        integer :: h, b

        h = int(ieor(ishft(wi, -63), 1_i8))
        b = f%mantissa_bits - f%min_exponent
        res = adjusted_mantissa(ishft(wi, h), b10_to_b2(int(q, i4)) + int(b-h-lz-62+INVALID_AM, i4))
    end subroutine compute_error_scaled

    !> Compute error adjusted mantissa.
    pure elemental subroutine compute_error(q, wi, f, res)
        integer(i8), intent(in) :: q, wi
        type(float_format), intent(in) :: f
        type(adjusted_mantissa), intent(out) :: res
        integer(i8) :: w
        integer(i4) :: lz
        type(u128) :: pr
        w = wi
        lz = int(count_leading_zeros(w), i4)
        w = ishft(w, lz)
        call compute_product(q, w, f, pr)
        call compute_error_scaled(q, pr%hi, lz, f, res)
    end subroutine compute_error

    ! ===== Clinger fast path =====

    !> Clinger fast path for double precision.
    pure elemental subroutine clinger_fast_path_64( m, e, ng, vd, f, ok)
        integer(i8), intent(in) :: m, e
        logical, intent(in) :: ng
        real(dp), intent(inout) :: vd
        type(float_format), intent(in) :: f
        logical, intent(out) :: ok

        ok = e >= int(f%min_fast_path, i8) .and. e <= int(f%max_fast_path, i8) .and. &
             m >= 0 .and. m <= f%max_mantissa
        if (.not. ok) return

        if (e < 0) then
            vd = real(m / DOUBLE_POW10(-e), dp)
        else
            vd = real(m * DOUBLE_POW10(e), dp)
        end if
        if (ng) vd = -vd

    end subroutine clinger_fast_path_64

    !> Clinger fast path for single precision.
    pure elemental subroutine clinger_fast_path_32( &
            m, e, ng, vf, f, ok)
        integer(i8), intent(in) :: m, e
        logical, intent(in) :: ng
        real(sp), intent(inout) :: vf
        type(float_format), intent(in) :: f
        logical, intent(out) :: ok

        ok = e >= int(f%min_fast_path, i8) .and. e <= int(f%max_fast_path, i8) .and. &
             m >= 0 .and. m <= f%max_mantissa
        if (.not. ok) return

        if (e < 0) then
            vf = real(m, sp) / FLOAT_POW10(-e)
        else
            vf = real(m, sp) * FLOAT_POW10(e)
        end if
        if (ng) vf = -vf

    end subroutine clinger_fast_path_32

    ! ===== Adjusted mantissa to float conversion =====

    !> Convert adjusted mantissa to double precision.
    pure elemental real(dp) function am_to_double(ng, am) result(v)
        logical, intent(in) :: ng
        type(adjusted_mantissa), intent(in) :: am
        integer(i8) :: w
        w = am%mantissa
        w = ior(w, ishft(int(am%power2, i8), 52))
        if (ng) w = ior(w, ishft(1_i8, 63))
        v = transfer(w, 0.0_dp)
    end function am_to_double

    !> Convert adjusted mantissa to single precision.
    pure elemental real(sp) function am_to_float(ng, am) result(v)
        logical, intent(in) :: ng
        type(adjusted_mantissa), intent(in) :: am
        integer(i4) :: w
        w = int(am%mantissa, i4)
        w = ior(w, ishft(am%power2, 23))
        if (ng) w = ior(w, ishft(1_i4, 31))
        v = transfer(w, 0.0_sp)
    end function am_to_float

    ! ===== Stack vector operations =====

    !> Push a value onto the stack vector.
    pure elemental subroutine sv_push(sv, v)
        type(stackvec), intent(inout) :: sv
        integer(i8), intent(in) :: v
        sv%ln = sv%ln + 1
        sv%d(sv%ln) = v
    end subroutine sv_push

    !> Try to push a value onto the stack vector.
    pure elemental subroutine sv_try_push(sv, v, ok)
        type(stackvec), intent(inout) :: sv
        integer(i8), intent(in) :: v
        logical, intent(out) :: ok
        if (sv%ln < STACKVEC_CAPACITY) then
            sv%ln = sv%ln + 1
            sv%d(sv%ln) = v
            ok = .true.
        else
            ok = .false.
        end if
    end subroutine sv_try_push

    !> Try to reserve space in the stack vector.
    pure elemental subroutine sv_try_reserve(sv, nl, ok)
        type(stackvec), intent(inout) :: sv
        integer, intent(in) :: nl
        logical, intent(out) :: ok
        integer :: i
        if (nl > STACKVEC_CAPACITY) then
            ok = .false.
            return
        end if
        if (nl > sv%ln) then
            do i = sv%ln + 1, nl
                sv%d(i) = 0
            end do
        end if
        sv%ln = nl
        ok = .true.
    end subroutine sv_try_reserve

    !> Normalize the stack vector by removing trailing zeros.
    pure elemental subroutine sv_normalize(sv)
        type(stackvec), intent(inout) :: sv
        do while (sv%ln > 0)
            if (sv%d(sv%ln) /= 0) exit
            sv%ln = sv%ln - 1
        end do
    end subroutine sv_normalize

    !> Get element at reverse index from the stack vector.
    pure elemental integer(i8) function sv_rindex(sv, i)
        type(stackvec), intent(in) :: sv
        integer, intent(in) :: i
        sv_rindex = sv%d(sv%ln - i)
    end function sv_rindex

    !> Check if any non-zero elements exist after index i.
    pure elemental logical function sv_nonzero_after(sv, i)
        type(stackvec), intent(in) :: sv
        integer, intent(in) :: i
        integer :: j
        sv_nonzero_after = .false.
        do j = i, sv%ln - 1
            if (sv%d(sv%ln - j) /= 0) then
                sv_nonzero_after = .true.
                return
            end if
        end do
    end function sv_nonzero_after

    ! ===== Big integer arithmetic =====

    !> Add two unsigned 64-bit integers with carry detection.
    pure elemental subroutine scalar_add(x, y, r, ov)
        integer(i8), intent(in) :: x, y
        integer(i8), intent(out) :: r
        logical, intent(out) :: ov
        integer(i8) :: s
        s = x + y
        ov = unsigned_lt(s, x)
        r = s
    end subroutine scalar_add

    !> Multiply with carry for big integer arithmetic.
    pure elemental subroutine scalar_mul(x, y, c, r)
        integer(i8), intent(in) :: x, y
        integer(i8), intent(inout) :: c
        integer(i8), intent(out) :: r
        type(u128) :: z
        logical :: ov
        integer(i8) :: t
        call mul_u64(x, y, z)
        call scalar_add(z%lo, c, t, ov)
        z%lo = t
        if (ov) z%hi = z%hi + 1
        c = z%hi
        r = z%lo
    end subroutine scalar_mul

    !> Add a small value to a bigint starting at a given position.
    pure elemental subroutine bigint_small_add(sv, y, st, ok)
        type(stackvec), intent(inout) :: sv
        integer(i8), intent(in) :: y
        integer, intent(in) :: st
        logical, intent(out) :: ok
        integer :: j
        integer(i8) :: c, t
        logical :: ov
        c = y
        j = st
        do while (c /= 0 .and. j <= sv%ln)
            call scalar_add(sv%d(j), c, t, ov)
            sv%d(j) = t
            c = merge(1_i8, 0_i8, ov)
            j = j + 1
        end do
        if (c /= 0) then
            call sv_try_push(sv, c, ok)
            return
        end if
        ok = .true.
    end subroutine bigint_small_add

    !> Multiply all limbs of a bigint by a scalar.
    pure elemental subroutine bigint_small_mul(sv, y, ok)
        type(stackvec), intent(inout) :: sv
        integer(i8), intent(in) :: y
        logical, intent(out) :: ok
        integer :: j
        integer(i8) :: c, t
        c = 0
        do j = 1, sv%ln
            call scalar_mul(sv%d(j), y, c, t)
            sv%d(j) = t
        end do
        if (c /= 0) then
            call sv_try_push(sv, c, ok)
            return
        end if
        ok = .true.
    end subroutine bigint_small_mul

    !> Add a large integer array to a bigint.
    pure subroutine bigint_large_add(x, yd, yl, st, bla_ok)
        type(stackvec), intent(inout) :: x
        integer(i8), intent(in) :: yd(:)
        integer, intent(in) :: yl, st
        logical, intent(out) :: bla_ok
        integer :: j
        logical :: cf, c1, c2, ok
        integer(i8) :: xi, yi, t, t2
        if (x%ln < st - 1 + yl) then
            call sv_try_reserve(x, st - 1 + yl, ok)
            if (.not. ok) then
                bla_ok = .false.
                return
            end if
        end if
        cf = .false.
        do j = 1, yl
            xi = x%d(st - 1 + j)
            yi = yd(j)
            call scalar_add(xi, yi, t, c1)
            c2 = .false.
            if (cf) then
                call scalar_add(t, 1_i8, t2, c2)
                t = t2
            end if
            x%d(st - 1 + j) = t
            cf = c1 .or. c2
        end do
        if (cf) then
            call bigint_small_add(x, 1_i8, st + yl, bla_ok)
            return
        end if
        bla_ok = .true.
    end subroutine bigint_large_add

    !> Long multiplication of a bigint by an integer array.
    pure subroutine bigint_long_mul(x, yd, yl, blm_ok)
        type(stackvec), intent(inout) :: x
        integer(i8), intent(in) :: yd(:)
        integer, intent(in) :: yl
        logical, intent(out) :: blm_ok
        type(stackvec) :: z, zi
        integer :: j
        integer(i8) :: yi
        logical :: ok
        z%ln = x%ln
        z%d(1:x%ln) = x%d(1:x%ln)
        if (yl /= 0) then
            call bigint_small_mul(x, yd(1), ok)
            if (.not. ok) then
                blm_ok = .false.
                return
            end if
            do j = 2, yl
                yi = yd(j)
                if (yi /= 0) then
                    zi%ln = z%ln
                    zi%d(1:z%ln) = z%d(1:z%ln)
                    call bigint_small_mul(zi, yi, ok)
                    if (.not. ok) then
                        blm_ok = .false.
                        return
                    end if
                    call bigint_large_add(x, zi%d, zi%ln, j, ok)
                    if (.not. ok) then
                        blm_ok = .false.
                        return
                    end if
                end if
            end do
        end if
        call sv_normalize(x)
        blm_ok = .true.
    end subroutine bigint_long_mul

    !> Large multiplication dispatch (scalar or long multiply).
    pure subroutine bigint_large_mul(x, yd, yl, ok)
        type(stackvec), intent(inout) :: x
        integer(i8), intent(in) :: yd(:)
        integer, intent(in) :: yl
        logical, intent(out) :: ok
        if (yl == 1) then
            call bigint_small_mul(x, yd(1), ok)
        else
            call bigint_long_mul(x, yd, yl, ok)
        end if
    end subroutine bigint_large_mul

    !> Shift bigint left by n bits within a limb.
    pure subroutine bigint_shl_bits(sv, n, ok)
        type(stackvec), intent(inout) :: sv
        integer, intent(in) :: n
        logical, intent(out) :: ok
        integer :: sl, sr, j
        integer(i8) :: pv, xi, cy
        sl = n
        sr = LIMB_BITS - sl
        pv = 0
        do j = 1, sv%ln
            xi = sv%d(j)
            sv%d(j) = ior(ishft(xi, sl), ishft(pv, -sr))
            pv = xi
        end do
        cy = ishft(pv, -sr)
        if (cy /= 0) then
            call sv_try_push(sv, cy, ok)
            return
        end if
        ok = .true.
    end subroutine bigint_shl_bits

    !> Shift bigint left by n whole limbs.
    pure subroutine bigint_shl_limbs(sv, n, ok)
        type(stackvec), intent(inout) :: sv
        integer, intent(in) :: n
        logical, intent(out) :: ok
        integer :: i
        if (n + sv%ln > STACKVEC_CAPACITY) then
            ok = .false.
            return
        end if
        if (sv%ln /= 0) then
            do i = sv%ln, 1, -1
                sv%d(i + n) = sv%d(i)
            end do
            do i = 1, n
                sv%d(i) = 0
            end do
            sv%ln = sv%ln + n
        end if
        ok = .true.
    end subroutine bigint_shl_limbs

    !> Shift bigint left by n bits.
    pure subroutine bigint_shl(sv, n, ok)
        type(stackvec), intent(inout) :: sv
        integer, intent(in) :: n
        logical, intent(out) :: ok
        integer :: rm, dv
        rm = mod(n, LIMB_BITS)
        dv = n / LIMB_BITS
        ok = .true.
        if (rm /= 0) then
            call bigint_shl_bits(sv, rm, ok)
            if (.not. ok) return
        end if
        if (dv /= 0) call bigint_shl_limbs(sv, dv, ok)
    end subroutine bigint_shl

    ! ===== Big integer powers =====

    !> Raise bigint to a power of 2.
    elemental pure subroutine bigint_pow2(bi, e, ok)
        type(bigint), intent(inout) :: bi
        integer, intent(in) :: e
        logical, intent(out) :: ok
        call bigint_shl(bi%vec, e, ok)
    end subroutine bigint_pow2

    !> Multiply bigint by 5^e.
    pure elemental subroutine bigint_pow5(bi, ei, bp5_ok)
        type(bigint), intent(inout) :: bi
        integer, intent(in) :: ei
        logical, intent(out) :: bp5_ok
        integer :: er
        logical :: ok
        integer, parameter :: ss = 27
        integer(i8), parameter :: mn = 7450580596923828125_i8
        er = ei
        bp5_ok = .false.
        do while (er >= LARGE_POWER_OF_FIVE_SIZE)
            call bigint_large_mul( bi%vec, LARGE_POWER_OF_FIVE, 5, ok)
            if (.not. ok) return
            er = er - LARGE_POWER_OF_FIVE_SIZE
        end do
        do while (er >= ss)
            call bigint_small_mul(bi%vec, mn, ok)
            if (.not. ok) return
            er = er - ss
        end do
        if (er /= 0) then
            call bigint_small_mul(bi%vec, SMALL_POWER_OF_FIVE(er), ok)
            if (.not. ok) return
        end if
        bp5_ok = .true.
    end subroutine bigint_pow5

    !> Multiply bigint by 10^e.
    pure elemental subroutine bigint_pow10(bi, e, ok)
        type(bigint), intent(inout) :: bi
        integer, intent(in) :: e
        logical, intent(out) :: ok
        call bigint_pow5(bi, e, ok)
        if (ok) call bigint_pow2(bi, e, ok)
    end subroutine bigint_pow10

    !> Create a bigint from a single value.
    pure elemental subroutine bigint_make(v, res)
        integer(i8), intent(in) :: v
        type(bigint), intent(out) :: res
        res%vec%ln = 0
        call sv_push(res%vec, v)
        call sv_normalize(res%vec)
    end subroutine bigint_make

    !> Create an empty bigint.
    pure elemental subroutine bigint_empty(res)
        type(bigint), intent(out) :: res
        res%vec%ln = 0
    end subroutine bigint_empty

    !> Count leading zeros of a bigint.
    pure elemental integer function bigint_ctlz(bi)
        type(bigint), intent(in) :: bi
        if (bi%vec%ln == 0) then
            bigint_ctlz = 0
        else
            bigint_ctlz = count_leading_zeros(bi%vec%d(bi%vec%ln))
        end if
    end function bigint_ctlz

    !> Compute bit length of a bigint.
    pure elemental integer function bigint_bit_length(bi)
        type(bigint), intent(in) :: bi
        bigint_bit_length = LIMB_BITS * bi%vec%ln - bigint_ctlz(bi)
    end function bigint_bit_length

    !> Extract high 64 bits from a single limb.
    pure elemental subroutine hi64_1(r0, tr, res)
        integer(i8), intent(in) :: r0
        logical, intent(out) :: tr
        integer(i8), intent(out) :: res
        tr = .false.
        res = ishft(r0, count_leading_zeros(r0))
    end subroutine hi64_1

    !> Extract high 64 bits from two limbs.
    pure elemental subroutine hi64_2(r0, r1, tr, res)
        integer(i8), intent(in) :: r0, r1
        logical, intent(out) :: tr
        integer(i8), intent(out) :: res
        integer :: s
        s = count_leading_zeros(r0)
        if (s == 0) then
            tr = r1 /= 0
            res = r0
        else
            tr = ishft(r1, s) /= 0
            res = ior(ishft(r0, s), ishft(r1, -(64 - s)))
        end if
    end subroutine hi64_2

    !> Extract the high 64 bits of a bigint.
    pure elemental subroutine bigint_hi64(bi, tr, res)
        type(bigint), intent(in) :: bi
        logical, intent(out) :: tr
        integer(i8), intent(out) :: res
        if (bi%vec%ln == 0) then
            tr = .false.
            res = 0
        else if (bi%vec%ln == 1) then
            call hi64_1(sv_rindex(bi%vec, 0), tr, res)
        else
            call hi64_2(sv_rindex(bi%vec, 0), sv_rindex(bi%vec, 1), tr, res)
            tr = tr .or. sv_nonzero_after(bi%vec, 2)
        end if
    end subroutine bigint_hi64

    !> Compare two bigints.
    pure elemental integer function bigint_compare(a, b)
        type(bigint), intent(in) :: a, b
        integer :: j
        j = a%vec%ln - b%vec%ln
        if (j/=0) then
            bigint_compare = sign(1,j)
            return
        end if
        bigint_compare = 0
        do j = a%vec%ln, 1, -1
            if (unsigned_gt(a%vec%d(j), b%vec%d(j))) then
                bigint_compare = 1
                return
            else if (unsigned_lt(a%vec%d(j), b%vec%d(j))) then
                bigint_compare = -1
                return
            end if
        end do
    end function bigint_compare

    ! ===== Digit comparison and rounding =====

    !> Scale the exponent by the number of digits in the mantissa.
    pure elemental integer(i4) function scale_exponent(mi, e)
        integer(i8), intent(in) :: mi
        integer(i4), intent(in) :: e
        integer(i8) :: m
        m = mi
        scale_exponent = e
        ! Unsigned values >= 2^63 appear negative in signed int64.
        ! These always have exactly 19 decimal digits (range [2^63, 10^19-1]).
        if (m < 0) then
            scale_exponent = e + 18
            return
        end if
        do while (m >= 10000)
            m = m / 10000
            scale_exponent = scale_exponent + 4
        end do
        do while (m >= 100)
            m = m / 100
            scale_exponent = scale_exponent + 2
        end do
        do while (m >= 10)
            m = m / 10
            scale_exponent = scale_exponent + 1
        end do
    end function scale_exponent

    !> Convert a float to its extended representation.
    pure elemental subroutine to_extended(vd, f, res)
        real(dp), intent(in) :: vd
        type(float_format), intent(in) :: f
        type(adjusted_mantissa), intent(out) :: res
        integer(i8) :: b
        integer(i4) :: bi
        bi = int(f%mantissa_bits - f%min_exponent, i4)
        b = transfer(vd, b)
        if (iand(b, f%exponent_mask) == 0) then
            res = adjusted_mantissa( iand(b, f%mantissa_mask), 1 - bi)
        else
            res = adjusted_mantissa( ior(iand(b, f%mantissa_mask), f%hidden_bit_mask), &
                                     int(ishft(iand(b, f%exponent_mask),-f%mantissa_bits), i4) - bi)
        end if
    end subroutine to_extended

    !> Convert a float to its extended halfway representation.
    pure elemental subroutine to_extended_halfway(vd, f, res)
        real(dp), intent(in) :: vd
        type(float_format), intent(in) :: f
        type(adjusted_mantissa), intent(out) :: res
        call to_extended(vd, f, res)
        res%mantissa = ishft(res%mantissa, 1) + 1
        res%power2 = res%power2 - 1
    end subroutine to_extended_halfway

    !> Round down implementation.
    pure elemental subroutine round_down_impl(am, s)
        type(adjusted_mantissa), intent(inout) :: am
        integer(i4), intent(in) :: s
        if (s == 64) then
            am%mantissa = 0
        else
            am%mantissa = ishft(am%mantissa, -s)
        end if
        am%power2 = am%power2 + s
    end subroutine round_down_impl

    !> Round to nearest with tie-even.
    pure elemental subroutine round_nearest_tie_even( am, s, tr)
        type(adjusted_mantissa), intent(inout) :: am
        integer(i4), intent(in) :: s
        logical, intent(in) :: tr
        integer(i8) :: mk, hw, tb
        logical :: ab, hf, od
        if (s == 64) then
            mk = not(0_i8)
        else
            mk = ishft(1_i8, s) - 1
        end if
        if (s == 0) then
            hw = 0
        else
            hw = ishft(1_i8, s - 1)
        end if
        tb = iand(am%mantissa, mk)
        ab = unsigned_gt(tb, hw)
        hf = tb == hw
        if (s == 64) then
            am%mantissa = 0
        else
            am%mantissa = ishft(am%mantissa, -s)
        end if
        am%power2 = am%power2 + s
        od = iand(am%mantissa, 1_i8) == 1
        if (ab .or. (hf .and. tr) .or. (od .and. hf)) am%mantissa = am%mantissa + 1
    end subroutine round_nearest_tie_even

    !> Round to nearest using comparison ordering.
    pure elemental subroutine round_nearest_cmp(am, s, ord)
        type(adjusted_mantissa), intent(inout) :: am
        integer(i4), intent(in) :: s
        integer, intent(in) :: ord
        logical :: od
        if (s == 64) then
            am%mantissa = 0
        else
            am%mantissa = ishft(am%mantissa, -s)
        end if
        am%power2 = am%power2 + s
        od = iand(am%mantissa, 1_i8) == 1
        if (ord > 0 .or. (ord == 0 .and. od)) am%mantissa = am%mantissa + 1
    end subroutine round_nearest_cmp

    !> Finish rounding: handle overflow and infinity.
    pure elemental subroutine round_finish(am, f)
        type(adjusted_mantissa), intent(inout) :: am
        type(float_format), intent(in) :: f
        if (unsigned_ge(am%mantissa, ishft(2_i8, f%mantissa_bits))) then
            am%mantissa = ishft(1_i8, f%mantissa_bits)
            am%power2 = am%power2 + 1
        end if
        am%mantissa = iand(am%mantissa, not(ishft(1_i8, f%mantissa_bits)))
        if (am%power2 >= int(f%inf_power, i4)) then
            am%power2 = int(f%inf_power, i4)
            am%mantissa = 0
        end if
    end subroutine round_finish

    !> Round down a subnormal adjusted mantissa.
    pure elemental subroutine round_down(am, f)
        type(adjusted_mantissa), intent(inout) :: am
        type(float_format), intent(in) :: f
        integer(i4) :: ms, s
        ms = int(64 - f%mantissa_bits - 1, i4)
        if (-am%power2 >= ms) then
            s = min(-am%power2 + 1, 64_i4)
            call round_down_impl(am, s)
            if (unsigned_lt(am%mantissa, ishft(1_i8, f%mantissa_bits))) then
                am%power2 = 0
            else
                am%power2 = 1
            end if
            return
        end if
        call round_down_impl(am, ms)
        call round_finish(am, f)
    end subroutine round_down

    !> Round tie-even for subnormals.
    pure elemental subroutine round_tie_even(am, tr, f)
        type(adjusted_mantissa), intent(inout) :: am
        logical, intent(in) :: tr
        type(float_format), intent(in) :: f
        integer(i4) :: ms, s
        ms = int(64 - f%mantissa_bits - 1, i4)
        if (-am%power2 >= ms) then
            s = min(-am%power2 + 1, 64_i4)
            call round_nearest_tie_even(am, s, tr)
            if (unsigned_lt(am%mantissa, ishft(1_i8, f%mantissa_bits))) then
                am%power2 = 0
            else
                am%power2 = 1
            end if
            return
        end if
        call round_nearest_tie_even(am, ms, tr)
        call round_finish(am, f)
    end subroutine round_tie_even

    !> Round tie-even using digit comparison ordering.
    pure elemental subroutine round_tie_even_cmp(am, ord, f)
        type(adjusted_mantissa), intent(inout) :: am
        integer, intent(in) :: ord
        type(float_format), intent(in) :: f
        integer(i4) :: ms, s
        ms = int(64 - f%mantissa_bits - 1, i4)
        if (-am%power2 >= ms) then
            s = min(-am%power2 + 1, 64_i4)
            call round_nearest_cmp(am, s, ord)
            if (unsigned_lt(am%mantissa, ishft(1_i8, f%mantissa_bits))) then
                am%power2 = 0
            else
                am%power2 = 1
            end if
            return
        end if
        call round_nearest_cmp(am, ms, ord)
        call round_finish(am, f)
    end subroutine round_tie_even_cmp

    ! ===== Mantissa parsing =====

    !> Check if a substring contains any non-zero digits.
    pure elemental logical function is_truncated(str, fi, la)
        character(len=*), intent(in) :: str
        integer, intent(in) :: fi, la
        integer :: p
        integer(i8) :: v

        is_truncated = .false.
        p = fi
        if (p > la) return

        do while (la - p + 1 >= 8)
            v = read8_to_u64(str(p:))
            if (v /= ZERO8_U64) then
                is_truncated = .true.
                return
            end if
            p = p + 8
        end do

        do while (p <= la)
            if (str(p:p) /= '0') then
                is_truncated = .true.
                return
            end if
            p = p + 1
        end do
    end function is_truncated

    !> Skip leading zero characters.
    pure elemental subroutine skip_zeros(str, p, la)
        character(*), intent(in) :: str
        integer, intent(inout) :: p
        integer, intent(in) :: la
        integer(i8) :: v

        do while (la - p + 1 >= 8)
            v = read8_to_u64(str(p:))
            if (v /= ZERO8_U64) exit
            p = p + 8
        end do
        do while (p <= la)
            if (str(p:p) /= '0') return
            p = p + 1
        end do
    end subroutine skip_zeros

    !> Add native: multiply and add to a bigint.
    pure elemental subroutine add_native(bi, pw, v)
        type(bigint), intent(inout) :: bi
        integer(i8), intent(in) :: pw, v
        logical :: ok
        call bigint_small_mul(bi%vec, pw, ok)
        call bigint_small_add(bi%vec, v, 1, ok)
    end subroutine add_native

    !> Round up a bigint by adding one digit.
    pure elemental subroutine round_up_bigint(bi, cn)
        type(bigint), intent(inout) :: bi
        integer, intent(inout) :: cn
        call add_native(bi, 10_i8, 1_i8)
        cn = cn + 1
    end subroutine round_up_bigint

    !> Parse mantissa digits into a bigint.
    pure elemental subroutine parse_mantissa( str, num, md, rb, dg)
        character(*), intent(in) :: str
        type(parsed_number), intent(in) :: num
        integer, intent(in) :: md
        type(bigint), intent(inout) :: rb
        integer, intent(out) :: dg
        integer :: ct, p, pe, stp
        integer(i8) :: v
        logical :: tr

        ct = 0
        dg = 0
        v = 0
        stp = 19

        if (num%int_start > 0 .and. num%int_len > 0) then
            p = num%int_start
            pe = p + num%int_len - 1
            call skip_zeros(str, p, pe)
            do while (p <= pe)
                do while (pe - p + 1 >= 8 .and. stp - ct >= 8 .and. md - dg >= 8)
                    v = v*100000000_i8 + int( parse_eight_digits(read8_to_u64(str(p:))),i8)
                    p = p + 8
                    ct = ct + 8
                    dg = dg + 8
                end do
                do while (pe - p + 1 >= 4 .and. stp - ct >= 4 .and. md - dg >= 4)
                    v = v*10000_i8 + int(parse_four_digits(read4_to_u32(str(p:))), i8)
                    p = p + 4
                    ct = ct + 4
                    dg = dg + 4
                end do
                do while (ct < stp .and. p <= pe .and. dg < md)
                    v = v*10 + int(iachar(str(p:p)) - 48, i8)
                    p = p + 1
                    ct = ct + 1
                    dg = dg + 1
                end do
                if (dg == md) then
                    call add_native(rb, POW10_U64(ct), v)
                    tr = is_truncated(str, p, pe)
                    if (num%frac_start > 0 .and. num%frac_len > 0) &
                        tr = tr .or. is_truncated(str, num%frac_start, num%frac_start + num%frac_len - 1)
                    if (tr) call round_up_bigint(rb, dg)
                    return
                else
                    call add_native(rb, POW10_U64(ct), v)
                    ct = 0
                    v = 0
                end if
            end do
        end if

        if (num%frac_start > 0 .and. num%frac_len > 0) then
            p = num%frac_start
            pe = p + num%frac_len - 1
            if (dg == 0) call skip_zeros(str, p, pe)
            do while (p <= pe)
                do while (pe - p + 1 >= 8 .and. stp - ct >= 8 .and. md - dg >= 8)
                    v = v*100000000_i8 + int(parse_eight_digits(read8_to_u64(str(p:))),i8)
                    p = p + 8
                    ct = ct + 8
                    dg = dg + 8
                end do
                do while (pe - p + 1 >= 4 .and. stp - ct >= 4 .and. md - dg >= 4)
                    v = v*10000_i8 + int(parse_four_digits(read4_to_u32(str(p:))), i8)
                    p = p + 4
                    ct = ct + 4
                    dg = dg + 4
                end do
                do while (ct < stp .and. p <= pe .and. dg < md)
                    v = v*10 + int(iachar(str(p:p)) - 48, i8)
                    p = p + 1
                    ct = ct + 1
                    dg = dg + 1
                end do
                if (dg == md) then
                    call add_native( rb, POW10_U64(ct), v)
                    tr = is_truncated(str, p, pe)
                    if (tr) call round_up_bigint(rb, dg)
                    return
                else
                    call add_native( rb, POW10_U64(ct), v)
                    ct = 0
                    v = 0
                end if
            end do
        end if
        if (ct /= 0) call add_native(rb, POW10_U64(ct), v)
    end subroutine parse_mantissa

    ! ===== Digit comparison fallback =====

    !> Positive digit comparison path.
    pure elemental subroutine positive_digit_comp( str, bm, ev, f, res)
        character(*), intent(in) :: str
        type(bigint), intent(inout) :: bm
        integer(i4), intent(in) :: ev
        type(float_format), intent(in) :: f
        type(adjusted_mantissa), intent(out) :: res
        logical :: tr, ok
        integer :: bi
        call bigint_pow10(bm, int(ev), ok)
        call bigint_hi64(bm, tr, res%mantissa)
        bi = f%mantissa_bits - f%min_exponent
        res%power2 = int( bigint_bit_length(bm) - 64 + bi, i4)
        call round_tie_even(res, tr, f)
    end subroutine positive_digit_comp

    !> Negative digit comparison path.
    pure elemental subroutine negative_digit_comp( str, bm, ai, ev, f, res)
        character(*), intent(in) :: str
        type(bigint), intent(inout) :: bm
        type(adjusted_mantissa), intent(in) :: ai
        integer(i4), intent(in) :: ev
        type(float_format), intent(in) :: f
        type(adjusted_mantissa), intent(out) :: res
        type(adjusted_mantissa) :: ab
        real(dp) :: bv
        type(adjusted_mantissa) :: th
        type(bigint) :: td
        integer(i4) :: te, p2e
        integer :: p5e, ord
        logical :: ok
        ab = ai
        call round_down(ab, f)
        bv = am_to_double(.false., ab)
        call to_extended_halfway(bv, f, th)
        call bigint_make(th%mantissa, td)
        te = th%power2
        p2e = te - ev
        p5e = int(-ev)
        if (p5e /= 0) call bigint_pow5(td, p5e, ok)
        if (p2e > 0) then
            call bigint_pow2(td, int(p2e), ok)
        else if (p2e < 0) then
            call bigint_pow2(bm, int(-p2e), ok)
        end if
        ord = bigint_compare(bm, td)
        res = ai
        call round_tie_even_cmp(res, ord, f)
    end subroutine negative_digit_comp

    !> Digit comparison fallback.
    pure elemental subroutine digit_comp(str, num, ai, f, res)
        character(*), intent(in) :: str
        type(parsed_number), intent(in) :: num
        type(adjusted_mantissa), intent(in) :: ai
        type(float_format), intent(in) :: f
        type(adjusted_mantissa), intent(out) :: res
        type(adjusted_mantissa) :: am
        integer(i4) :: se, ev
        integer :: dg
        type(bigint) :: bm
        am = ai
        am%power2 = am%power2 - INVALID_AM
        se = scale_exponent(num%mantissa, int(num%exponent, i4))
        call bigint_empty(bm)
        call parse_mantissa(str, num, f%max_digits, bm, dg)
        ev = se + 1 - int(dg, i4)
        if (ev >= 0) then
            call positive_digit_comp(str, bm, ev, f, res)
        else
            call negative_digit_comp(str, bm, am, ev, f, res)
        end if
    end subroutine digit_comp

    ! ===== Conversion entry points =====

    !> Convert parsed number string to double (from_chars path).
    pure elemental subroutine from_chars_64(str, p, vd, f, res)
        character(*), intent(in) :: str
        type(parsed_number), intent(in) :: p
        real(dp), intent(inout) :: vd
        type(float_format), intent(in) :: f
        type(parse_result), intent(out) :: res
        type(adjusted_mantissa) :: am, ap
        logical :: eq, cfok

        res%outcome = OUTCOMES%OK
        res%pos     = p%last_idx

        if (.not. p%too_many_digits) then
            call clinger_fast_path_64(p%mantissa, p%exponent, p%negative, vd, f, cfok)
            if (cfok) return
        end if

        call compute_float(p%exponent, p%mantissa, f, am)
        if (p%too_many_digits .and. am%power2 >= 0) then
            call compute_float(p%exponent, p%mantissa + 1, f, ap)
            eq = am%mantissa == ap%mantissa .and. am%power2 == ap%power2
            if (.not. eq) call compute_error(p%exponent, p%mantissa, f, am)
        end if
        if (am%power2 < 0) then
            ap = am
            call digit_comp(str, p, ap, f, am)
        end if

        vd = am_to_double(p%negative, am)

        if ((p%mantissa /= 0 .and. am%mantissa == 0 .and. am%power2 == 0) .or. &
             am%power2 == int(f%inf_power, i4)) res%outcome = OUTCOMES%OUT_OF_RANGE
    end subroutine from_chars_64

    !> Convert parsed number string to float (from_chars path).
    pure elemental subroutine from_chars_32( str, p, vf, f, res)
        character(*), intent(in) :: str
        type(parsed_number), intent(in) :: p
        real(sp), intent(inout) :: vf
        type(float_format), intent(in) :: f
        type(parse_result), intent(out) :: res
        type(adjusted_mantissa) :: am, ap
        logical :: eq, cfok

        res%outcome = OUTCOMES%OK
        res%pos     = p%last_idx

        if (.not. p%too_many_digits) then
            call clinger_fast_path_32( p%mantissa, p%exponent, p%negative, vf, f, cfok)
            if (cfok) return
        end if

        call compute_float( p%exponent, p%mantissa, f, am)
        if (p%too_many_digits .and. am%power2 >= 0) then
            call compute_float(p%exponent, p%mantissa + 1, f, ap)
            eq = am%mantissa == ap%mantissa .and. am%power2 == ap%power2
            if (.not. eq) call compute_error(p%exponent, p%mantissa, f, am)
        end if
        if (am%power2 < 0) then
            ap = am
            call digit_comp(str, p, ap, f, am)
        end if

        vf = am_to_float(p%negative, am)

        if ((p%mantissa /= 0 .and. am%mantissa == 0 .and. am%power2 == 0) .or. &
            am%power2 == int(f%inf_power, i4)) res%outcome = OUTCOMES%OUT_OF_RANGE
    end subroutine from_chars_32

    ! ===== Public API =====

    ! --- parse_double: pure elemental (no parse_result) ---

    pure elemental real(dp) function parse_dp(str) result(out)
        character(*), intent(in) :: str
        type(parse_result) :: res
        call parse_double_range_sub(str, 1, len(str), out, res, DEFAULT_PARSING)
    end function parse_dp

    pure elemental real(dp) function parse_dp_opts(str, opts) result(out)
        character(*), intent(in) :: str
        type(parse_options), intent(in) :: opts
        type(parse_result) :: res
        call parse_double_range_sub(str, 1, len(str), out, res, opts)
    end function parse_dp_opts

    pure elemental real(dp) function parse_dp_range(str, first, last) result(out)
        character(*), intent(in) :: str
        integer, intent(in) :: first, last
        type(parse_result) :: res
        call parse_double_range_sub(str, first, last, out, res, DEFAULT_PARSING)
    end function parse_dp_range

    pure elemental real(dp) function parse_dp_range_opts(str, first, last, opts) result(out)
        character(*), intent(in) :: str
        integer, intent(in) :: first, last
        type(parse_options), intent(in) :: opts
        type(parse_result) :: res
        call parse_double_range_sub(str, first, last, out, res, opts)
    end function parse_dp_range_opts

    ! --- parse_double: standard (with parse_result intent(out)) ---

    real(dp) function parse_dp_res(str, res) result(out)
        character(*), intent(in) :: str
        type(parse_result), intent(out) :: res
        call parse_double_range_sub(str, 1, len(str), out, res, DEFAULT_PARSING)
    end function parse_dp_res

    real(dp) function parse_dp_opts_res(str, opts, res) result(out)
        character(*), intent(in) :: str
        type(parse_options), intent(in) :: opts
        type(parse_result), intent(out) :: res
        call parse_double_range_sub(str, 1, len(str), out, res, opts)
    end function parse_dp_opts_res

    real(dp) function parse_dp_range_res(str, first, last, res) result(out)
        character(*), intent(in) :: str
        integer, intent(in) :: first, last
        type(parse_result), intent(out) :: res
        call parse_double_range_sub(str, first, last, out, res, DEFAULT_PARSING)
    end function parse_dp_range_res

    real(dp) function parse_dp_range_opts_res(str, first, last, opts, res) result(out)
        character(*), intent(in) :: str
        integer, intent(in) :: first, last
        type(parse_options), intent(in) :: opts
        type(parse_result), intent(out) :: res
        call parse_double_range_sub(str, first, last, out, res, opts)
    end function parse_dp_range_opts_res

    !> Parse a string range to double, subroutine form.
    !> Parse a string range to double, subroutine form.
    !> Hot-path routines manually inlined for performance without LTO.
    elemental subroutine parse_double_range_sub(str, first, last, out, res, o)
        character(len=*), intent(in) :: str
        integer, value :: first
        integer, intent(in), value :: last
        real(dp), intent(out) :: out
        type(parse_result), intent(out) :: res
        type(parse_options), intent(in) :: o
        type(parsed_number) :: p
        type(adjusted_mantissa) :: cf_am, fc_ap
        logical :: fc_eq, fc_cfok
        integer :: ic
        real(dp) :: fc_out
        ! parse_number_string inline variables
        integer :: pns_first, pns_last
        logical :: pns_bj
        type(parse_options) :: pns_opts
        integer(i8) :: pns_i, pns_dc, pns_exp, pns_en
        integer :: pns_p, pns_sd, pns_eip, pns_bf, pns_le, pns_ic, pns_ie, pns_fe, pns_sp_
        logical :: pns_alp, pns_hdp, pns_ne, pns_hse, pns_hed
        ! compute_float inline variables
        integer(i8) :: cf_q, cf_wi, cf_w
        integer(i4) :: cf_lz
        type(u128) :: cf_pr
        integer :: cf_ub, cf_sa
        integer :: cp_idx
        type(u128) :: cp_sp
        ! loop_parse_eight inline variables
        integer(i8) :: lpe_val, lpe_v, lpe_v1, lpe_v2
        ! am_to_double inline variable
        integer(i8) :: atd_w
        ! mul_u64 inline variables
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
        integer(c_int128_t) :: z128a, z128b, z128r
#else
        integer(i8) :: mu_a0, mu_a1, mu_b0, mu_b1, mu_w0, mu_t, mu_w1, mu_w2
#endif

        if (iand(o%format, FMT_SKIP_WS) /= 0) then
            do while (first <= last)
                ic = iachar(str(first:first))
                if (.not. ((ic >= 9 .and. ic <= 13) .or. ic == 32)) exit
                first = first + 1
            end do
        end if
        if (first > last) then
            res = parse_result(first, OUTCOMES%INVALID_INPUT)
            out = 0.0_dp
            return
        end if

        ! === Inlined parse_number_string ===
        pns_first = first; pns_last = last; pns_opts = o
        pns_bj = iand(o%format, FMT_JSON) /= 0

        parse_number: block
        pns_p = pns_first
        p%valid = .false.
        if (pns_p > pns_last) then
            p%last_idx = pns_p
            exit parse_number
        end if

        p%negative = (str(pns_p:pns_p) == '-')
        pns_alp = iand(pns_opts%format, FMT_ALLOW_PLUS) /= 0

        if (str(pns_p:pns_p) == '-' .or. &
            (pns_alp .and. .not. pns_bj .and. str(pns_p:pns_p) == '+')) then
            pns_p = pns_p + 1
            if (pns_p > pns_last) then
                p%last_idx = pns_p
                exit parse_number
            end if
            if (pns_bj) then
                pns_ic = iachar(str(pns_p:pns_p))
                if (pns_ic < 48 .or. pns_ic > 57) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            else
                pns_ic = iachar(str(pns_p:pns_p))
                if ((pns_ic < 48 .or. pns_ic > 57) .and. &
                    str(pns_p:pns_p) /= pns_opts%decimal_point) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            end if
        end if

        pns_sd = pns_p
        pns_i = 0_i8
        do while (pns_p <= pns_last)
            pns_ic = iachar(str(pns_p:pns_p))
            if (pns_ic < 48 .or. pns_ic > 57) exit
            pns_i = 10*pns_i + int(pns_ic - 48, i8)
            pns_p = pns_p + 1
        end do

        pns_eip = pns_p
        pns_dc = int(pns_eip - pns_sd, i8)
        p%int_start = pns_sd
        p%int_len = int(pns_dc)

        if (pns_bj) then
            if (pns_dc == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
            if (str(pns_sd:pns_sd) == '0' .and. pns_dc > 1) then
                p%last_idx = pns_sd
                exit parse_number
            end if
        end if

        pns_exp = 0_i8
        p%frac_start = 0
        p%frac_len = 0
        pns_hdp = .false.
        if (pns_p <= pns_last) pns_hdp = (str(pns_p:pns_p) == pns_opts%decimal_point)

        if (pns_hdp) then
            pns_p = pns_p + 1
            pns_bf = pns_p
            ! --- Inlined loop_parse_eight ---
            do while (pns_last - pns_p + 1 >= 8)
                lpe_val = transfer(str(pns_p:pns_p+7), 0_i8)
                lpe_v1 = lpe_val + LPE_46
                lpe_v2 = lpe_val - ZERO8_U64
                if (iand(ior(lpe_v1, lpe_v2), LPE_HI) /= 0) exit
                lpe_v = lpe_val - ZERO8_U64
                lpe_v = lpe_v*10 + ishft(lpe_v, -8)
                lpe_v = ishft(iand(lpe_v, LPE_M)*LPE_M1 + iand(ishft(lpe_v,-16), LPE_M)*LPE_M2, -32)
                pns_i = pns_i * 100000000_i8 + iand(lpe_v, LPE_FF)
                pns_p = pns_p + 8
            end do
            call loop_parse_four(pns_p, pns_last, str, pns_i)
            do while (pns_p <= pns_last)
                pns_ic = iachar(str(pns_p:pns_p))
                if (pns_ic < 48 .or. pns_ic > 57) exit
                pns_i = pns_i*10 + int(pns_ic-48, i8)
                pns_p = pns_p + 1
            end do
            pns_exp = int(pns_bf - pns_p, i8)
            p%frac_start = pns_bf
            p%frac_len = pns_p - pns_bf
            pns_dc = pns_dc - pns_exp
        end if

        if (pns_bj) then
            if (pns_hdp .and. pns_exp == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
        else
            if (pns_dc == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
        end if

        pns_en = 0_i8
        pns_hse = .false.
        if (pns_p <= pns_last) then
            pns_hse = (iand(pns_opts%format, FMT_SCIENTIFIC) /= 0 .and. &
                       (str(pns_p:pns_p) == 'e' .or. str(pns_p:pns_p) == 'E')) .or. &
                      (iand(pns_opts%format, FMT_FORTRAN) /= 0 .and. &
                       (str(pns_p:pns_p) == '+' .or. str(pns_p:pns_p) == '-' .or. &
                        str(pns_p:pns_p) == 'd' .or. str(pns_p:pns_p) == 'D'))
        end if
        if (pns_hse) then
            pns_le = pns_p
            if (str(pns_p:pns_p) == 'e' .or. str(pns_p:pns_p) == 'E' .or. &
                str(pns_p:pns_p) == 'd' .or. str(pns_p:pns_p) == 'D') pns_p = pns_p + 1
            pns_ne = .false.
            if (pns_p <= pns_last) then
                if (str(pns_p:pns_p) == '-') then
                    pns_ne = .true.
                    pns_p = pns_p + 1
                else if (str(pns_p:pns_p) == '+') then
                    pns_p = pns_p + 1
                end if
            end if
            pns_hed = .false.
            if (pns_p <= pns_last) then
                pns_ic = iachar(str(pns_p:pns_p))
                pns_hed = (pns_ic >= 48 .and. pns_ic <= 57)
            end if
            if (.not. pns_hed) then
                if (iand(pns_opts%format, FMT_FIXED) == 0) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
                pns_p = pns_le
            else
                do while (pns_p <= pns_last)
                    pns_ic = iachar(str(pns_p:pns_p))
                    if (pns_ic < 48 .or. pns_ic > 57) exit
                    if (pns_en < 268435456_i8) pns_en = 10*pns_en + int(pns_ic-48, i8)
                    pns_p = pns_p + 1
                end do
                if (pns_ne) pns_en = -pns_en
                pns_exp = pns_exp + pns_en
            end if
        else
            if (iand(pns_opts%format, FMT_SCIENTIFIC) /= 0 .and. &
                iand(pns_opts%format, FMT_FIXED) == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
        end if

        p%last_idx = pns_p
        p%valid = .true.
        p%too_many_digits = .false.

        if (pns_dc > 19) then
            pns_sp_ = pns_sd
            do while (pns_sp_ <= pns_last)
                if (str(pns_sp_:pns_sp_) /= '0' .and. &
                    str(pns_sp_:pns_sp_) /= pns_opts%decimal_point) exit
                if (str(pns_sp_:pns_sp_) == '0') pns_dc = pns_dc - 1
                pns_sp_ = pns_sp_ + 1
            end do
            if (pns_dc > 19) then
                p%too_many_digits = .true.
                pns_i = 0
                pns_p = p%int_start
                pns_ie = pns_p + p%int_len
                do while (ieor(pns_i, SB64) < ieor(1000000000000000000_i8, SB64) .and. pns_p < pns_ie)
                    pns_i = pns_i*10 + int( iachar(str(pns_p:pns_p)) - 48, i8)
                    pns_p = pns_p + 1
                end do
                if (ieor(pns_i, SB64) >= ieor(1000000000000000000_i8, SB64)) then
                    pns_exp = int(pns_eip - pns_p, i8) + pns_en
                else
                    pns_p = p%frac_start
                    pns_fe = pns_p + p%frac_len
                    do while (ieor(pns_i, SB64) < ieor(1000000000000000000_i8, SB64) .and. pns_p < pns_fe)
                        pns_i = pns_i*10 + int( iachar(str(pns_p:pns_p)) - 48, i8)
                        pns_p = pns_p + 1
                    end do
                    pns_exp = int(p%frac_start - pns_p, i8) + pns_en
                end if
            end if
        end if
        p%exponent = pns_exp
        p%mantissa = pns_i
        end block parse_number

        if (.not. p%valid) then
            if (iand(o%format, FMT_NO_INFNAN) /= 0) then
                res = parse_result(first, OUTCOMES%INVALID_INPUT)
                out = 0.0_dp
            else
                call parse_infnan_64(str, first, last, out, res)
            end if
            return
        end if

        ! --- Inlined from_chars_64 ---
        fc_out = 0.0_dp
        res%outcome = OUTCOMES%OK
        res%pos     = p%last_idx

        ! --- Inlined clinger_fast_path_64 ---
        if (.not. p%too_many_digits) then
            fc_cfok = p%exponent >= int(DP_MFP_LO, i8) .and. p%exponent <= int(DP_MFP_HI, i8) .and. &
                      p%mantissa >= 0 .and. p%mantissa <= DP_MMAX
            if (fc_cfok) then
                if (p%exponent < 0) then
                    fc_out = real(p%mantissa / DOUBLE_POW10(-p%exponent), dp)
                else
                    fc_out = real(p%mantissa * DOUBLE_POW10(p%exponent), dp)
                end if
                if (p%negative) fc_out = -fc_out
                out = fc_out
                return
            end if
        end if

        ! === Inlined compute_float (double precision) ===
        cf_q = p%exponent; cf_wi = p%mantissa
        cf_w = cf_wi
        if (cf_w == 0 .or. cf_q < int(DP_SP10, i8)) then
            cf_am = adjusted_mantissa(0, 0)
        else if (cf_q > int(DP_LP10, i8)) then
            cf_am = adjusted_mantissa(0, int(DP_IP, i4))
        else
            cf_lz = leadz(cf_w)
            cf_w = ishft(cf_w, cf_lz)

            cp_idx = 2*int(cf_q + 342_i8) + 1
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
            z128a = transfer([cf_w, 0_i8], z128a)
            z128b = transfer([POWER5_TABLE(cp_idx), 0_i8], z128b)
            z128r = z128a * z128b
            cf_pr = transfer(z128r, cf_pr)
#else
            mu_a0 = iand(cf_w, M32); mu_a1 = iand(ishft(cf_w, -32), M32)
            mu_b0 = iand(POWER5_TABLE(cp_idx), M32)
            mu_b1 = iand(ishft(POWER5_TABLE(cp_idx), -32), M32)
            mu_w0 = mu_a0 * mu_b0
            mu_t = mu_a1*mu_b0 + iand(ishft(mu_w0, -32), M32)
            mu_w1 = iand(mu_t, M32)
            mu_w2 = iand(ishft(mu_t, -32), M32)
            mu_w1 = mu_w1 + mu_a0 * mu_b1
            cf_pr%lo = cf_w * POWER5_TABLE(cp_idx)
            cf_pr%hi = mu_a1*mu_b1 + mu_w2 + iand(ishft(mu_w1, -32), M32)
#endif
            if (iand(cf_pr%hi, DP_CPM) == DP_CPM) then
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
                z128b = transfer([POWER5_TABLE(cp_idx + 1), 0_i8], z128b)
                z128r = z128a * z128b
                cp_sp = transfer(z128r, cp_sp)
#else
                mu_b0 = iand(POWER5_TABLE(cp_idx+1), M32)
                mu_b1 = iand(ishft(POWER5_TABLE(cp_idx+1), -32), M32)
                mu_w0 = mu_a0 * mu_b0
                mu_t = mu_a1*mu_b0 + iand(ishft(mu_w0, -32), M32)
                mu_w1 = iand(mu_t, M32)
                mu_w2 = iand(ishft(mu_t, -32), M32)
                mu_w1 = mu_w1 + mu_a0 * mu_b1
                cp_sp%lo = cf_w * POWER5_TABLE(cp_idx+1)
                cp_sp%hi = mu_a1*mu_b1 + mu_w2 + iand(ishft(mu_w1, -32), M32)
#endif
                cf_pr%lo = cf_pr%lo + cp_sp%hi
                if (ieor(cf_pr%lo, SB64) < ieor(cp_sp%hi, SB64)) cf_pr%hi = cf_pr%hi + 1
            end if

            cf_ub = int(ishft(cf_pr%hi, -63))
            cf_sa = cf_ub + 9
            cf_am%mantissa = ishft(cf_pr%hi, -cf_sa)
            cf_am%power2 = int(ishft(217706_i8 * int(cf_q, i8), -16), i4) + 1086_i4 &
                         + int(cf_ub, i4) - cf_lz

            if (cf_am%power2 <= 0) then
                if (-cf_am%power2 + 1 >= 64) then
                    cf_am = adjusted_mantissa(0_i8, 0_i4)
                else
                    cf_am%mantissa = ishft(cf_am%mantissa, cf_am%power2 - 1_i4)
                    cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                    cf_am%mantissa = ishft(cf_am%mantissa, -1)
                    if (cf_am%mantissa < DP_HB) then
                        cf_am%power2 = 0
                    else
                        cf_am%power2 = 1
                    end if
                end if
            else
                if (iand(cf_pr%lo, not(1_i8)) == 0 .and. &
                    cf_q >= int(DP_MRT_LO, i8) .and. &
                    cf_q <= int(DP_MRT_HI, i8) .and. &
                    iand(cf_am%mantissa, 3_i8) == 1 .and. &
                    ishft(cf_am%mantissa, cf_sa) == cf_pr%hi) &
                        cf_am%mantissa = iand(cf_am%mantissa, not(1_i8))

                cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                cf_am%mantissa = ishft(cf_am%mantissa, -1)

                if (cf_am%mantissa >= DP_2HB) then
                    cf_am%mantissa = DP_HB
                    cf_am%power2 = cf_am%power2 + 1
                end if

                cf_am%mantissa = iand(cf_am%mantissa, not(DP_HB))
                if (cf_am%power2 >= int(DP_IP, i4)) then
                    cf_am%power2 = int(DP_IP, i4)
                    cf_am%mantissa = 0
                end if
            end if
        end if
        ! === End inlined compute_float ===

        if (p%too_many_digits .and. cf_am%power2 >= 0) then
            call compute_float(p%exponent, p%mantissa + 1, DOUBLE_FMT, fc_ap)
            fc_eq = cf_am%mantissa == fc_ap%mantissa .and. cf_am%power2 == fc_ap%power2
            if (.not. fc_eq) call compute_error(p%exponent, p%mantissa, DOUBLE_FMT, cf_am)
        end if
        if (cf_am%power2 < 0) then
            fc_ap = cf_am
            call digit_comp(str, p, fc_ap, DOUBLE_FMT, cf_am)
        end if

        ! --- Inlined am_to_double ---
        atd_w = ior(cf_am%mantissa, ishft(int(cf_am%power2, i8), DP_MB))
        if (p%negative) atd_w = ior(atd_w, SB64)
        out = transfer(atd_w, 0.0_dp)

        if ((p%mantissa /= 0 .and. cf_am%mantissa == 0 .and. cf_am%power2 == 0) .or. &
             cf_am%power2 == int(DP_IP, i4)) res%outcome = OUTCOMES%OUT_OF_RANGE
    end subroutine parse_double_range_sub

    !> Parse a double from a string, position-1 based. Returns bytes consumed.
    !> Designed for stream parsing: caller passes str(pos:), gets nread back.
    !> Full hot-path inlined for maximum performance without LTO.
    pure subroutine parse_double_fast(str, val, nread, stat, o)
        character(*), intent(in) :: str
        real(dp), intent(out) :: val
        integer, intent(out) :: nread
        type(outcome), intent(out) :: stat
        type(parse_options), intent(in), optional :: o
        type(parse_options) :: opts
        type(parsed_number) :: p
        type(adjusted_mantissa) :: cf_am, fc_ap
        logical :: fc_eq, fc_cfok
        integer :: pos, slen, ic
        real(dp) :: fc_out
        ! parse_number_string inline variables
        logical :: pns_bj
        integer(i8) :: pns_i, pns_dc, pns_exp, pns_en
        integer :: pns_p, pns_sd, pns_eip, pns_bf, pns_le, pns_ic, pns_ie, pns_fe, pns_sp_
        logical :: pns_alp, pns_hdp, pns_ne, pns_hse, pns_hed
        ! compute_float inline variables
        integer(i8) :: cf_q, cf_wi, cf_w
        integer(i4) :: cf_lz
        type(u128) :: cf_pr
        integer :: cf_ub, cf_sa
        integer :: cp_idx
        type(u128) :: cp_sp
        ! loop_parse_eight inline variables
        integer(i8) :: lpe_val, lpe_v, lpe_v1, lpe_v2
        ! am_to_double inline variable
        integer(i8) :: atd_w
        ! mul_u64 inline variables
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
        integer(c_int128_t) :: z128a, z128b, z128r
#else
        integer(i8) :: mu_a0, mu_a1, mu_b0, mu_b1, mu_w0, mu_t, mu_w1, mu_w2
#endif

        opts = DEFAULT_PARSING
        if (present(o)) opts = o

        slen = len(str)
        pos = 1

        ! Whitespace skip
        if (iand(opts%format, FMT_SKIP_WS) /= 0) then
            do while (pos <= slen)
                ic = iachar(str(pos:pos))
                if (.not. ((ic >= 9 .and. ic <= 13) .or. ic == 32)) exit
                pos = pos + 1
            end do
        end if
        if (pos > slen) then
            stat = OUTCOMES%INVALID_INPUT
            val = 0.0_dp
            nread = pos
            return
        end if

        ! === Inlined parse_number_string (position-1 based) ===
        pns_bj = iand(opts%format, FMT_JSON) /= 0

        parse_number: block
        pns_p = pos
        p%valid = .false.
        if (pns_p > slen) then
            p%last_idx = pns_p
            exit parse_number
        end if

        p%negative = (str(pns_p:pns_p) == '-')
        pns_alp = iand(opts%format, FMT_ALLOW_PLUS) /= 0

        if (str(pns_p:pns_p) == '-' .or. &
            (pns_alp .and. .not. pns_bj .and. str(pns_p:pns_p) == '+')) then
            pns_p = pns_p + 1
            if (pns_p > slen) then
                p%last_idx = pns_p
                exit parse_number
            end if
            if (pns_bj) then
                pns_ic = iachar(str(pns_p:pns_p))
                if (pns_ic < 48 .or. pns_ic > 57) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            else
                pns_ic = iachar(str(pns_p:pns_p))
                if ((pns_ic < 48 .or. pns_ic > 57) .and. &
                    str(pns_p:pns_p) /= opts%decimal_point) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            end if
        end if

        pns_sd = pns_p
        pns_i = 0_i8
        do while (pns_p <= slen)
            pns_ic = iachar(str(pns_p:pns_p))
            if (pns_ic < 48 .or. pns_ic > 57) exit
            pns_i = 10*pns_i + int(pns_ic - 48, i8)
            pns_p = pns_p + 1
        end do

        pns_eip = pns_p
        pns_dc = int(pns_eip - pns_sd, i8)
        p%int_start = pns_sd
        p%int_len = int(pns_dc)

        if (pns_bj) then
            if (pns_dc == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
            if (str(pns_sd:pns_sd) == '0' .and. pns_dc > 1) then
                p%last_idx = pns_sd
                exit parse_number
            end if
        end if

        pns_exp = 0_i8
        p%frac_start = 0
        p%frac_len = 0
        pns_hdp = .false.
        if (pns_p <= slen) pns_hdp = (str(pns_p:pns_p) == opts%decimal_point)

        if (pns_hdp) then
            pns_p = pns_p + 1
            pns_bf = pns_p
            do while (slen - pns_p + 1 >= 8)
                lpe_val = transfer(str(pns_p:pns_p+7), 0_i8)
                lpe_v1 = lpe_val + LPE_46
                lpe_v2 = lpe_val - ZERO8_U64
                if (iand(ior(lpe_v1, lpe_v2), LPE_HI) /= 0) exit
                lpe_v = lpe_val - ZERO8_U64
                lpe_v = lpe_v*10 + ishft(lpe_v, -8)
                lpe_v = ishft(iand(lpe_v, LPE_M)*LPE_M1 + iand(ishft(lpe_v,-16), LPE_M)*LPE_M2, -32)
                pns_i = pns_i * 100000000_i8 + iand(lpe_v, LPE_FF)
                pns_p = pns_p + 8
            end do
            call loop_parse_four(pns_p, slen, str, pns_i)
            do while (pns_p <= slen)
                pns_ic = iachar(str(pns_p:pns_p))
                if (pns_ic < 48 .or. pns_ic > 57) exit
                pns_i = pns_i*10 + int(pns_ic-48, i8)
                pns_p = pns_p + 1
            end do
            pns_exp = int(pns_bf - pns_p, i8)
            p%frac_start = pns_bf
            p%frac_len = pns_p - pns_bf
            pns_dc = pns_dc - pns_exp
        end if

        if (pns_bj) then
            if (pns_hdp .and. pns_exp == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
        else
            if (pns_dc == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
        end if

        pns_en = 0_i8
        pns_hse = .false.
        if (pns_p <= slen) then
            pns_hse = (iand(opts%format, FMT_SCIENTIFIC) /= 0 .and. &
                       (str(pns_p:pns_p) == 'e' .or. str(pns_p:pns_p) == 'E')) .or. &
                      (iand(opts%format, FMT_FORTRAN) /= 0 .and. &
                       (str(pns_p:pns_p) == '+' .or. str(pns_p:pns_p) == '-' .or. &
                        str(pns_p:pns_p) == 'd' .or. str(pns_p:pns_p) == 'D'))
        end if
        if (pns_hse) then
            pns_le = pns_p
            if (str(pns_p:pns_p) == 'e' .or. str(pns_p:pns_p) == 'E' .or. &
                str(pns_p:pns_p) == 'd' .or. str(pns_p:pns_p) == 'D') pns_p = pns_p + 1
            pns_ne = .false.
            if (pns_p <= slen) then
                if (str(pns_p:pns_p) == '-') then
                    pns_ne = .true.
                    pns_p = pns_p + 1
                else if (str(pns_p:pns_p) == '+') then
                    pns_p = pns_p + 1
                end if
            end if
            pns_hed = .false.
            if (pns_p <= slen) then
                pns_ic = iachar(str(pns_p:pns_p))
                pns_hed = (pns_ic >= 48 .and. pns_ic <= 57)
            end if
            if (.not. pns_hed) then
                if (iand(opts%format, FMT_FIXED) == 0) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
                pns_p = pns_le
            else
                do while (pns_p <= slen)
                    pns_ic = iachar(str(pns_p:pns_p))
                    if (pns_ic < 48 .or. pns_ic > 57) exit
                    if (pns_en < 268435456_i8) pns_en = 10*pns_en + int(pns_ic-48, i8)
                    pns_p = pns_p + 1
                end do
                if (pns_ne) pns_en = -pns_en
                pns_exp = pns_exp + pns_en
            end if
        else
            if (iand(opts%format, FMT_SCIENTIFIC) /= 0 .and. &
                iand(opts%format, FMT_FIXED) == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
        end if

        p%last_idx = pns_p
        p%valid = .true.
        p%too_many_digits = .false.

        if (pns_dc > 19) then
            pns_sp_ = pns_sd
            do while (pns_sp_ <= slen)
                if (str(pns_sp_:pns_sp_) /= '0' .and. &
                    str(pns_sp_:pns_sp_) /= opts%decimal_point) exit
                if (str(pns_sp_:pns_sp_) == '0') pns_dc = pns_dc - 1
                pns_sp_ = pns_sp_ + 1
            end do
            if (pns_dc > 19) then
                p%too_many_digits = .true.
                pns_i = 0
                pns_p = p%int_start
                pns_ie = pns_p + p%int_len
                do while (ieor(pns_i, SB64) < ieor(1000000000000000000_i8, SB64) .and. pns_p < pns_ie)
                    pns_i = pns_i*10 + int( iachar(str(pns_p:pns_p)) - 48, i8)
                    pns_p = pns_p + 1
                end do
                if (ieor(pns_i, SB64) >= ieor(1000000000000000000_i8, SB64)) then
                    pns_exp = int(pns_eip - pns_p, i8) + pns_en
                else
                    pns_p = p%frac_start
                    pns_fe = pns_p + p%frac_len
                    do while (ieor(pns_i, SB64) < ieor(1000000000000000000_i8, SB64) .and. pns_p < pns_fe)
                        pns_i = pns_i*10 + int( iachar(str(pns_p:pns_p)) - 48, i8)
                        pns_p = pns_p + 1
                    end do
                    pns_exp = int(p%frac_start - pns_p, i8) + pns_en
                end if
            end if
        end if
        p%exponent = pns_exp
        p%mantissa = pns_i
        end block parse_number

        if (.not. p%valid) then
            if (iand(opts%format, FMT_NO_INFNAN) == 0) then
                block
                    type(parse_result) :: infnan_res
                    call parse_infnan_64(str, pos, slen, val, infnan_res)
                    stat = infnan_res%outcome
                    nread = infnan_res%pos
                end block
            else
                stat = OUTCOMES%INVALID_INPUT
                val = 0.0_dp
                nread = pos
            end if
            return
        end if

        ! --- Inlined from_chars_64 ---
        fc_out = 0.0_dp
        stat = OUTCOMES%OK
        nread = p%last_idx

        if (.not. p%too_many_digits) then
            fc_cfok = p%exponent >= int(DP_MFP_LO, i8) .and. p%exponent <= int(DP_MFP_HI, i8) .and. &
                      p%mantissa >= 0 .and. p%mantissa <= DP_MMAX
            if (fc_cfok) then
                if (p%exponent < 0) then
                    fc_out = real(p%mantissa / DOUBLE_POW10(-p%exponent), dp)
                else
                    fc_out = real(p%mantissa * DOUBLE_POW10(p%exponent), dp)
                end if
                if (p%negative) fc_out = -fc_out
                val = fc_out
                return
            end if
        end if

        ! === Inlined compute_float (double precision) ===
        cf_q = p%exponent; cf_wi = p%mantissa
        cf_w = cf_wi
        if (cf_w == 0 .or. cf_q < int(DP_SP10, i8)) then
            cf_am = adjusted_mantissa(0, 0)
        else if (cf_q > int(DP_LP10, i8)) then
            cf_am = adjusted_mantissa(0, int(DP_IP, i4))
        else
            cf_lz = leadz(cf_w)
            cf_w = ishft(cf_w, cf_lz)

            cp_idx = 2*int(cf_q + 342_i8) + 1
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
            z128a = transfer([cf_w, 0_i8], z128a)
            z128b = transfer([POWER5_TABLE(cp_idx), 0_i8], z128b)
            z128r = z128a * z128b
            cf_pr = transfer(z128r, cf_pr)
#else
            mu_a0 = iand(cf_w, M32); mu_a1 = iand(ishft(cf_w, -32), M32)
            mu_b0 = iand(POWER5_TABLE(cp_idx), M32)
            mu_b1 = iand(ishft(POWER5_TABLE(cp_idx), -32), M32)
            mu_w0 = mu_a0 * mu_b0
            mu_t = mu_a1*mu_b0 + iand(ishft(mu_w0, -32), M32)
            mu_w1 = iand(mu_t, M32)
            mu_w2 = iand(ishft(mu_t, -32), M32)
            mu_w1 = mu_w1 + mu_a0 * mu_b1
            cf_pr%lo = cf_w * POWER5_TABLE(cp_idx)
            cf_pr%hi = mu_a1*mu_b1 + mu_w2 + iand(ishft(mu_w1, -32), M32)
#endif
            if (iand(cf_pr%hi, DP_CPM) == DP_CPM) then
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
                z128b = transfer([POWER5_TABLE(cp_idx + 1), 0_i8], z128b)
                z128r = z128a * z128b
                cp_sp = transfer(z128r, cp_sp)
#else
                mu_b0 = iand(POWER5_TABLE(cp_idx+1), M32)
                mu_b1 = iand(ishft(POWER5_TABLE(cp_idx+1), -32), M32)
                mu_w0 = mu_a0 * mu_b0
                mu_t = mu_a1*mu_b0 + iand(ishft(mu_w0, -32), M32)
                mu_w1 = iand(mu_t, M32)
                mu_w2 = iand(ishft(mu_t, -32), M32)
                mu_w1 = mu_w1 + mu_a0 * mu_b1
                cp_sp%lo = cf_w * POWER5_TABLE(cp_idx+1)
                cp_sp%hi = mu_a1*mu_b1 + mu_w2 + iand(ishft(mu_w1, -32), M32)
#endif
                cf_pr%lo = cf_pr%lo + cp_sp%hi
                if (ieor(cf_pr%lo, SB64) < ieor(cp_sp%hi, SB64)) cf_pr%hi = cf_pr%hi + 1
            end if

            cf_ub = int(ishft(cf_pr%hi, -63))
            cf_sa = cf_ub + 9
            cf_am%mantissa = ishft(cf_pr%hi, -cf_sa)
            cf_am%power2 = int(ishft(217706_i8 * int(cf_q, i8), -16), i4) + 1086_i4 &
                         + int(cf_ub, i4) - cf_lz

            if (cf_am%power2 <= 0) then
                if (-cf_am%power2 + 1 >= 64) then
                    cf_am = adjusted_mantissa(0_i8, 0_i4)
                else
                    cf_am%mantissa = ishft(cf_am%mantissa, cf_am%power2 - 1_i4)
                    cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                    cf_am%mantissa = ishft(cf_am%mantissa, -1)
                    if (cf_am%mantissa < DP_HB) then
                        cf_am%power2 = 0
                    else
                        cf_am%power2 = 1
                    end if
                end if
            else
                if (iand(cf_pr%lo, not(1_i8)) == 0 .and. &
                    cf_q >= int(DP_MRT_LO, i8) .and. &
                    cf_q <= int(DP_MRT_HI, i8) .and. &
                    iand(cf_am%mantissa, 3_i8) == 1 .and. &
                    ishft(cf_am%mantissa, cf_sa) == cf_pr%hi) &
                        cf_am%mantissa = iand(cf_am%mantissa, not(1_i8))

                cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                cf_am%mantissa = ishft(cf_am%mantissa, -1)

                if (cf_am%mantissa >= DP_2HB) then
                    cf_am%mantissa = DP_HB
                    cf_am%power2 = cf_am%power2 + 1
                end if

                cf_am%mantissa = iand(cf_am%mantissa, not(DP_HB))
                if (cf_am%power2 >= int(DP_IP, i4)) then
                    cf_am%power2 = int(DP_IP, i4)
                    cf_am%mantissa = 0
                end if
            end if
        end if

        if (p%too_many_digits .and. cf_am%power2 >= 0) then
            call compute_float(p%exponent, p%mantissa + 1, DOUBLE_FMT, fc_ap)
            fc_eq = cf_am%mantissa == fc_ap%mantissa .and. cf_am%power2 == fc_ap%power2
            if (.not. fc_eq) call compute_error(p%exponent, p%mantissa, DOUBLE_FMT, cf_am)
        end if
        if (cf_am%power2 < 0) then
            fc_ap = cf_am
            call digit_comp(str, p, fc_ap, DOUBLE_FMT, cf_am)
        end if

        atd_w = ior(cf_am%mantissa, ishft(int(cf_am%power2, i8), DP_MB))
        if (p%negative) atd_w = ior(atd_w, SB64)
        val = transfer(atd_w, 0.0_dp)

        if ((p%mantissa /= 0 .and. cf_am%mantissa == 0 .and. cf_am%power2 == 0) .or. &
             cf_am%power2 == int(DP_IP, i4)) stat = OUTCOMES%OUT_OF_RANGE
    end subroutine parse_double_fast

    !> Parse a double from a character stream, advancing the pointer.
    !> Matches stdlib's to_num stream interface pattern.
    subroutine parse_double_stream(str, val, stat, o)
        character(len=:), pointer, intent(inout) :: str
        real(dp), intent(out) :: val
        type(outcome), intent(out) :: stat
        type(parse_options), intent(in), optional :: o
        integer :: nread
        call parse_double_fast(str, val, nread, stat, o)
        if (stat == OUTCOMES%INVALID_INPUT) return
        if (nread <= len(str)) then
            str => str(nread:)
        else
            str => str(len(str)+1:len(str))
        end if
    end subroutine parse_double_stream

    !> Parse a string range to float, subroutine form.
    !> Hot-path routines manually inlined for performance without LTO.
    elemental subroutine parse_float_range_sub(str, first, last, out, res, o)
        character(len=*), intent(in) :: str
        integer, intent(in), value :: first, last
        real(sp), intent(out) :: out
        type(parse_result), intent(out) :: res
        type(parse_options), intent(in) :: o
        type(parsed_number) :: p
        type(adjusted_mantissa) :: cf_am, fc_ap
        logical :: fc_eq, fc_cfok
        integer :: ps
        real(sp) :: fc_outf
        ! parse_number_string inline variables
        integer :: pns_first, pns_last
        logical :: pns_bj
        type(parse_options) :: pns_opts
        integer(i8) :: pns_i, pns_dc, pns_exp, pns_en
        integer :: pns_p, pns_sd, pns_eip, pns_bf, pns_le, pns_ic, pns_ie, pns_fe, pns_sp_
        logical :: pns_alp, pns_hdp, pns_ne, pns_hse, pns_hed
        ! compute_float inline variables
        integer(i8) :: cf_q, cf_wi, cf_w
        integer(i4) :: cf_lz
        type(u128) :: cf_pr
        integer :: cf_ub, cf_sa, ic
        integer :: cp_idx
        type(u128) :: cp_sp
        ! loop_parse_eight inline variables
        integer(i8) :: lpe_val, lpe_v, lpe_v1, lpe_v2
        ! am_to_float inline variable
        integer(i4) :: atf_w
        ! mul_u64 inline variables
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
        integer(c_int128_t) :: z128a, z128b, z128r
#else
        integer(i8) :: mu_a0, mu_a1, mu_b0, mu_b1, mu_w0, mu_t, mu_w1, mu_w2
#endif

        ps = first
        if (iand(o%format, FMT_SKIP_WS) /= 0) then
            do while (ps <= last)
                ic = iachar(str(ps:ps))
                if (.not. ((ic >= 9 .and. ic <= 13) .or. ic == 32)) exit
                ps = ps + 1
            end do
        end if
        if (ps > last) then
            res = parse_result(ps, OUTCOMES%INVALID_INPUT)
            out = 0.0_sp
            return
        end if

        ! === Inlined parse_number_string ===
        pns_first = ps; pns_last = last; pns_opts = o
        pns_bj = iand(o%format, FMT_JSON) /= 0

        parse_number: block
        pns_p = pns_first
        p%valid = .false.
        if (pns_p > pns_last) then
            p%last_idx = pns_p
            exit parse_number
        end if

        p%negative = (str(pns_p:pns_p) == '-')
        pns_alp = iand(pns_opts%format, FMT_ALLOW_PLUS) /= 0

        if (str(pns_p:pns_p) == '-' .or. &
            (pns_alp .and. .not. pns_bj .and. str(pns_p:pns_p) == '+')) then
            pns_p = pns_p + 1
            if (pns_p > pns_last) then
                p%last_idx = pns_p
                exit parse_number
            end if
            if (pns_bj) then
                pns_ic = iachar(str(pns_p:pns_p))
                if (pns_ic < 48 .or. pns_ic > 57) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            else
                pns_ic = iachar(str(pns_p:pns_p))
                if ((pns_ic < 48 .or. pns_ic > 57) .and. &
                    str(pns_p:pns_p) /= pns_opts%decimal_point) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            end if
        end if

        pns_sd = pns_p
        pns_i = 0_i8
        do while (pns_p <= pns_last)
            pns_ic = iachar(str(pns_p:pns_p))
            if (pns_ic < 48 .or. pns_ic > 57) exit
            pns_i = 10*pns_i + int(pns_ic - 48, i8)
            pns_p = pns_p + 1
        end do

        pns_eip = pns_p
        pns_dc = int(pns_eip - pns_sd, i8)
        p%int_start = pns_sd
        p%int_len = int(pns_dc)

        if (pns_bj) then
            if (pns_dc == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
            if (str(pns_sd:pns_sd) == '0' .and. pns_dc > 1) then
                p%last_idx = pns_sd
                exit parse_number
            end if
        end if

        pns_exp = 0_i8
        p%frac_start = 0
        p%frac_len = 0
        pns_hdp = .false.
        if (pns_p <= pns_last) pns_hdp = (str(pns_p:pns_p) == pns_opts%decimal_point)

        if (pns_hdp) then
            pns_p = pns_p + 1
            pns_bf = pns_p
            do while (pns_last - pns_p + 1 >= 8)
                lpe_val = transfer(str(pns_p:pns_p+7), 0_i8)
                lpe_v1 = lpe_val + LPE_46
                lpe_v2 = lpe_val - ZERO8_U64
                if (iand(ior(lpe_v1, lpe_v2), LPE_HI) /= 0) exit
                lpe_v = lpe_val - ZERO8_U64
                lpe_v = lpe_v*10 + ishft(lpe_v, -8)
                lpe_v = ishft(iand(lpe_v, LPE_M)*LPE_M1 + iand(ishft(lpe_v,-16), LPE_M)*LPE_M2, -32)
                pns_i = pns_i * 100000000_i8 + iand(lpe_v, LPE_FF)
                pns_p = pns_p + 8
            end do
            call loop_parse_four(pns_p, pns_last, str, pns_i)
            do while (pns_p <= pns_last)
                pns_ic = iachar(str(pns_p:pns_p))
                if (pns_ic < 48 .or. pns_ic > 57) exit
                pns_i = pns_i*10 + int(pns_ic-48, i8)
                pns_p = pns_p + 1
            end do
            pns_exp = int(pns_bf - pns_p, i8)
            p%frac_start = pns_bf
            p%frac_len = pns_p - pns_bf
            pns_dc = pns_dc - pns_exp
        end if

        if (pns_bj) then
            if (pns_hdp .and. pns_exp == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
        else
            if (pns_dc == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
        end if

        pns_en = 0_i8
        pns_hse = .false.
        if (pns_p <= pns_last) then
            pns_hse = (iand(pns_opts%format, FMT_SCIENTIFIC) /= 0 .and. &
                       (str(pns_p:pns_p) == 'e' .or. str(pns_p:pns_p) == 'E')) .or. &
                      (iand(pns_opts%format, FMT_FORTRAN) /= 0 .and. &
                       (str(pns_p:pns_p) == '+' .or. str(pns_p:pns_p) == '-' .or. &
                        str(pns_p:pns_p) == 'd' .or. str(pns_p:pns_p) == 'D'))
        end if
        if (pns_hse) then
            pns_le = pns_p
            if (str(pns_p:pns_p) == 'e' .or. str(pns_p:pns_p) == 'E' .or. &
                str(pns_p:pns_p) == 'd' .or. str(pns_p:pns_p) == 'D') pns_p = pns_p + 1
            pns_ne = .false.
            if (pns_p <= pns_last) then
                if (str(pns_p:pns_p) == '-') then
                    pns_ne = .true.
                    pns_p = pns_p + 1
                else if (str(pns_p:pns_p) == '+') then
                    pns_p = pns_p + 1
                end if
            end if
            pns_hed = .false.
            if (pns_p <= pns_last) then
                pns_ic = iachar(str(pns_p:pns_p))
                pns_hed = (pns_ic >= 48 .and. pns_ic <= 57)
            end if
            if (.not. pns_hed) then
                if (iand(pns_opts%format, FMT_FIXED) == 0) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
                pns_p = pns_le
            else
                do while (pns_p <= pns_last)
                    pns_ic = iachar(str(pns_p:pns_p))
                    if (pns_ic < 48 .or. pns_ic > 57) exit
                    if (pns_en < 268435456_i8) pns_en = 10*pns_en + int(pns_ic-48, i8)
                    pns_p = pns_p + 1
                end do
                if (pns_ne) pns_en = -pns_en
                pns_exp = pns_exp + pns_en
            end if
        else
            if (iand(pns_opts%format, FMT_SCIENTIFIC) /= 0 .and. &
                iand(pns_opts%format, FMT_FIXED) == 0) then
                p%last_idx = pns_p
                exit parse_number
            end if
        end if

        p%last_idx = pns_p
        p%valid = .true.
        p%too_many_digits = .false.

        if (pns_dc > 19) then
            pns_sp_ = pns_sd
            do while (pns_sp_ <= pns_last)
                if (str(pns_sp_:pns_sp_) /= '0' .and. &
                    str(pns_sp_:pns_sp_) /= pns_opts%decimal_point) exit
                if (str(pns_sp_:pns_sp_) == '0') pns_dc = pns_dc - 1
                pns_sp_ = pns_sp_ + 1
            end do
            if (pns_dc > 19) then
                p%too_many_digits = .true.
                pns_i = 0
                pns_p = p%int_start
                pns_ie = pns_p + p%int_len
                do while (ieor(pns_i, SB64) < ieor(1000000000000000000_i8, SB64) .and. pns_p < pns_ie)
                    pns_i = pns_i*10 + int( iachar(str(pns_p:pns_p)) - 48, i8)
                    pns_p = pns_p + 1
                end do
                if (ieor(pns_i, SB64) >= ieor(1000000000000000000_i8, SB64)) then
                    pns_exp = int(pns_eip - pns_p, i8) + pns_en
                else
                    pns_p = p%frac_start
                    pns_fe = pns_p + p%frac_len
                    do while (ieor(pns_i, SB64) < ieor(1000000000000000000_i8, SB64) .and. pns_p < pns_fe)
                        pns_i = pns_i*10 + int( iachar(str(pns_p:pns_p)) - 48, i8)
                        pns_p = pns_p + 1
                    end do
                    pns_exp = int(p%frac_start - pns_p, i8) + pns_en
                end if
            end if
        end if
        p%exponent = pns_exp
        p%mantissa = pns_i
        end block parse_number

        if (.not. p%valid) then
            if (iand(o%format, FMT_NO_INFNAN) /= 0) then
                res = parse_result(ps, OUTCOMES%INVALID_INPUT)
                out = 0.0_sp
            else
                call parse_infnan_32(str, ps, last, out, res)
            end if
            return
        end if

        ! --- Inlined from_chars_32 ---
        fc_outf = 0.0_sp
        res%outcome = OUTCOMES%OK
        res%pos     = p%last_idx

        ! --- Inlined clinger_fast_path_32 ---
        if (.not. p%too_many_digits) then
            fc_cfok = p%exponent >= int(SP_MFP_LO, i8) .and. p%exponent <= int(SP_MFP_HI, i8) .and. &
                      p%mantissa >= 0 .and. p%mantissa <= SP_MMAX
            if (fc_cfok) then
                if (p%exponent < 0) then
                    fc_outf = real(p%mantissa, sp) / FLOAT_POW10(-p%exponent)
                else
                    fc_outf = real(p%mantissa, sp) * FLOAT_POW10(p%exponent)
                end if
                if (p%negative) fc_outf = -fc_outf
                out = fc_outf
                return
            end if
        end if

        ! === Inlined compute_float (single precision) ===
        cf_q = p%exponent; cf_wi = p%mantissa
        cf_w = cf_wi
        if (cf_w == 0 .or. cf_q < int(SP_SP10, i8)) then
            cf_am = adjusted_mantissa(0, 0)
        else if (cf_q > int(SP_LP10, i8)) then
            cf_am = adjusted_mantissa(0, int(SP_IP, i4))
        else
            cf_lz = leadz(cf_w)
            cf_w = ishft(cf_w, cf_lz)

            cp_idx = 2*int(cf_q + 342_i8) + 1
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
            z128a = transfer([cf_w, 0_i8], z128a)
            z128b = transfer([POWER5_TABLE(cp_idx), 0_i8], z128b)
            z128r = z128a * z128b
            cf_pr = transfer(z128r, cf_pr)
#else
            mu_a0 = iand(cf_w, M32); mu_a1 = iand(ishft(cf_w, -32), M32)
            mu_b0 = iand(POWER5_TABLE(cp_idx), M32)
            mu_b1 = iand(ishft(POWER5_TABLE(cp_idx), -32), M32)
            mu_w0 = mu_a0 * mu_b0
            mu_t = mu_a1*mu_b0 + iand(ishft(mu_w0, -32), M32)
            mu_w1 = iand(mu_t, M32)
            mu_w2 = iand(ishft(mu_t, -32), M32)
            mu_w1 = mu_w1 + mu_a0 * mu_b1
            cf_pr%lo = cf_w * POWER5_TABLE(cp_idx)
            cf_pr%hi = mu_a1*mu_b1 + mu_w2 + iand(ishft(mu_w1, -32), M32)
#endif
            if (iand(cf_pr%hi, SP_CPM) == SP_CPM) then
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
                z128b = transfer([POWER5_TABLE(cp_idx + 1), 0_i8], z128b)
                z128r = z128a * z128b
                cp_sp = transfer(z128r, cp_sp)
#else
                mu_b0 = iand(POWER5_TABLE(cp_idx+1), M32)
                mu_b1 = iand(ishft(POWER5_TABLE(cp_idx+1), -32), M32)
                mu_w0 = mu_a0 * mu_b0
                mu_t = mu_a1*mu_b0 + iand(ishft(mu_w0, -32), M32)
                mu_w1 = iand(mu_t, M32)
                mu_w2 = iand(ishft(mu_t, -32), M32)
                mu_w1 = mu_w1 + mu_a0 * mu_b1
                cp_sp%lo = cf_w * POWER5_TABLE(cp_idx+1)
                cp_sp%hi = mu_a1*mu_b1 + mu_w2 + iand(ishft(mu_w1, -32), M32)
#endif
                cf_pr%lo = cf_pr%lo + cp_sp%hi
                if (ieor(cf_pr%lo, SB64) < ieor(cp_sp%hi, SB64)) cf_pr%hi = cf_pr%hi + 1
            end if

            cf_ub = int(ishft(cf_pr%hi, -63))
            cf_sa = cf_ub + 38
            cf_am%mantissa = ishft(cf_pr%hi, -cf_sa)
            cf_am%power2 = int(ishft(217706_i8 * int(cf_q, i8), -16), i4) + 190_i4 &
                         + int(cf_ub, i4) - cf_lz

            if (cf_am%power2 <= 0) then
                if (-cf_am%power2 + 1 >= 64) then
                    cf_am = adjusted_mantissa(0_i8, 0_i4)
                else
                    cf_am%mantissa = ishft(cf_am%mantissa, cf_am%power2 - 1_i4)
                    cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                    cf_am%mantissa = ishft(cf_am%mantissa, -1)
                    if (cf_am%mantissa < SP_HB) then
                        cf_am%power2 = 0
                    else
                        cf_am%power2 = 1
                    end if
                end if
            else
                if (iand(cf_pr%lo, not(1_i8)) == 0 .and. &
                    cf_q >= int(SP_MRT_LO, i8) .and. &
                    cf_q <= int(SP_MRT_HI, i8) .and. &
                    iand(cf_am%mantissa, 3_i8) == 1 .and. &
                    ishft(cf_am%mantissa, cf_sa) == cf_pr%hi) &
                        cf_am%mantissa = iand(cf_am%mantissa, not(1_i8))

                cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                cf_am%mantissa = ishft(cf_am%mantissa, -1)

                if (cf_am%mantissa >= SP_2HB) then
                    cf_am%mantissa = SP_HB
                    cf_am%power2 = cf_am%power2 + 1
                end if

                cf_am%mantissa = iand(cf_am%mantissa, not(SP_HB))
                if (cf_am%power2 >= int(SP_IP, i4)) then
                    cf_am%power2 = int(SP_IP, i4)
                    cf_am%mantissa = 0
                end if
            end if
        end if

        if (p%too_many_digits .and. cf_am%power2 >= 0) then
            call compute_float(p%exponent, p%mantissa + 1, FLOAT_FMT, fc_ap)
            fc_eq = cf_am%mantissa == fc_ap%mantissa .and. cf_am%power2 == fc_ap%power2
            if (.not. fc_eq) call compute_error(p%exponent, p%mantissa, FLOAT_FMT, cf_am)
        end if
        if (cf_am%power2 < 0) then
            fc_ap = cf_am
            call digit_comp(str, p, fc_ap, FLOAT_FMT, cf_am)
        end if

        ! --- Inlined am_to_float ---
        atf_w = int(cf_am%mantissa, i4)
        atf_w = ior(atf_w, ishft(cf_am%power2, SP_MB))
        if (p%negative) atf_w = ior(atf_w, ishft(1_i4, 31))
        out = transfer(atf_w, 0.0_sp)

        if ((p%mantissa /= 0 .and. cf_am%mantissa == 0 .and. cf_am%power2 == 0) .or. &
            cf_am%power2 == int(SP_IP, i4)) res%outcome = OUTCOMES%OUT_OF_RANGE
    end subroutine parse_float_range_sub

    ! --- parse_float: pure elemental (no parse_result) ---

    pure elemental real(sp) function parse_fp(str) result(out)
        character(*), intent(in) :: str
        type(parse_result) :: res
        call parse_float_range_sub(str, 1, len(str), out, res, DEFAULT_PARSING)
    end function parse_fp

    pure elemental real(sp) function parse_fp_opts(str, opts) result(out)
        character(*), intent(in) :: str
        type(parse_options), intent(in) :: opts
        type(parse_result) :: res
        call parse_float_range_sub(str, 1, len(str), out, res, opts)
    end function parse_fp_opts

    ! --- parse_float: standard (with parse_result intent(out)) ---

    real(sp) function parse_fp_res(str, res) result(out)
        character(*), intent(in) :: str
        type(parse_result), intent(out) :: res
        call parse_float_range_sub(str, 1, len(str), out, res, DEFAULT_PARSING)
    end function parse_fp_res

    real(sp) function parse_fp_opts_res(str, opts, res) result(out)
        character(*), intent(in) :: str
        type(parse_options), intent(in) :: opts
        type(parse_result), intent(out) :: res
        call parse_float_range_sub(str, 1, len(str), out, res, opts)
    end function parse_fp_opts_res

    ! ===== Integer parsing =====

    !> Parse a string to a 64-bit integer (subroutine form).
    !>
    !> When `o` is supplied, `FMT_SKIP_WS` skips leading whitespace and
    !> `FMT_ALLOW_PLUS` accepts a leading `+` sign (symmetric to `-`).
    !> Omitting `o` preserves the historical behavior: no whitespace skipping,
    !> `-` accepted, `+` rejected.
    elemental subroutine parse_i64_sub(str, base, out, res, o)
        character(*), intent(in) :: str
        integer, intent(in) :: base
        integer(i8), intent(out) :: out
        type(parse_result), intent(out) :: res
        type(parse_options), intent(in), optional :: o
        integer :: p, la, sn, sd, dc, md, d, ic
        logical :: ng, hlz, skip_ws, allow_plus
        integer(i8) :: i

        skip_ws    = .false.
        allow_plus = .false.
        if (present(o)) then
            skip_ws    = iand(o%format, FMT_SKIP_WS)    /= 0
            allow_plus = iand(o%format, FMT_ALLOW_PLUS) /= 0
        end if

        out = 0
        la = len(str)
        p = 1
        if (p > la .or. base < 2 .or. base > 36) then
            res%outcome = OUTCOMES%INVALID_INPUT
            res%pos = p
            return
        end if
        if (skip_ws) then
            do while (p <= la)
                ic = iachar(str(p:p))
                if (.not. ((ic >= 9 .and. ic <= 13) .or. ic == 32)) exit
                p = p + 1
            end do
            if (p > la) then
                res%outcome = OUTCOMES%INVALID_INPUT
                res%pos = p
                return
            end if
        end if
        ng = str(p:p) == '-'
        if (ng .or. (allow_plus .and. str(p:p) == '+')) p = p + 1
        if (p > la) then
            res%outcome = OUTCOMES%INVALID_INPUT
            res%pos = p
            return
        end if
        sn = p
        do while (p <= la)
            if (str(p:p) /= '0') exit
            p = p + 1
        end do
        hlz = p > sn
        sd = p
        i = 0
        if (base == 10) call loop_parse_eight(p, la, str, i)
        if (base == 10) call loop_parse_four(p, la, str, i)
        do while (p <= la)
            d = char_to_digit(str(p:p))
            if (d >= base) exit
            i = int(base, i8)*i + int(d, i8)
            p = p + 1
        end do
        dc = p - sd
        if (dc == 0) then
            if (hlz) then
                out = 0
                res%outcome = OUTCOMES%OK
                res%pos = p
            else
                res%outcome = OUTCOMES%INVALID_INPUT
                res%pos = 1
            end if
            return
        end if
        res%pos = p
        md = MAX_DIGITS_U64(base)
        if (dc > md) then
            res%outcome = OUTCOMES%OUT_OF_RANGE
            return
        end if
        if (dc == md .and. unsigned_lt(i, MIN_SAFE_U64(base))) then
            res%outcome = OUTCOMES%OUT_OF_RANGE
            return
        end if
        if (.not. ng) then
            if (unsigned_gt(i, int(z'7FFFFFFFFFFFFFFF', i8))) then
                res%outcome = OUTCOMES%OUT_OF_RANGE
                return
            end if
            out = i
        else
            if (unsigned_gt(i, ishft(1_i8, 63))) then
                res%outcome = OUTCOMES%OUT_OF_RANGE
                return
            end if
            out = not(i) + 1
        end if
        res%outcome = OUTCOMES%OK
    end subroutine parse_i64_sub

    !> Parse a string to a 32-bit integer (subroutine form). See parse_i64_sub
    !> for the meaning of `o`.
    elemental subroutine parse_i32_sub(str, base, out, res, o)
        character(*), intent(in) :: str
        integer, intent(in) :: base
        integer(i4), intent(out) :: out
        type(parse_result), intent(out) :: res
        type(parse_options), intent(in), optional :: o
        integer(i8) :: v
        out = 0
        call parse_i64_sub(str, base, v, res, o)
        if (.not. res%outcome == OUTCOMES%OK) return
        if (v > int(z'7FFFFFFF', i8) .or. v < int(z'FFFFFFFF80000000', i8)) then
            res%outcome = OUTCOMES%OUT_OF_RANGE
            return
        end if
        out = int(v, i4)
    end subroutine parse_i32_sub

    ! --- parse_i64: pure elemental + standard ---

    pure elemental integer(i8) function parse_i64_pure(str, base) result(out)
        character(*), intent(in) :: str
        integer, intent(in) :: base
        type(parse_result) :: res
        call parse_i64_sub(str, base, out, res)
    end function parse_i64_pure

    integer(i8) function parse_i64_std(str, base, res, o) result(out)
        character(*), intent(in) :: str
        integer, intent(in) :: base
        type(parse_result), intent(out) :: res
        type(parse_options), intent(in), optional :: o
        call parse_i64_sub(str, base, out, res, o)
    end function parse_i64_std

    ! --- parse_i32: pure elemental + standard ---

    pure elemental integer(i4) function parse_i32_pure(str, base) result(out)
        character(*), intent(in) :: str
        integer, intent(in) :: base
        type(parse_result) :: res
        call parse_i32_sub(str, base, out, res)
    end function parse_i32_pure

    integer(i4) function parse_i32_std(str, base, res, o) result(out)
        character(*), intent(in) :: str
        integer, intent(in) :: base
        type(parse_result), intent(out) :: res
        type(parse_options), intent(in), optional :: o
        call parse_i32_sub(str, base, out, res, o)
    end function parse_i32_std

    ! Outcome comparisons
    elemental logical function outcome_eq(this,that)
        type(outcome), intent(in) :: this,that
        outcome_eq = this%state==that%state
    end function outcome_eq

    elemental logical function outcome_ne(this,that)
        type(outcome), intent(in) :: this,that
        outcome_ne = this%state/=that%state
    end function outcome_ne

    !> Parse a whitespace-delimited stream of double-precision floats into an array.
    !> Hot-path routines manually inlined for performance without LTO.
    pure subroutine parse_double_array(stream, values, n, err, o)
        character(*), intent(in) :: stream
        real(dp), intent(out) :: values(:)
        integer, intent(out) :: n
        type(outcome), intent(out) :: err
        type(parse_options), intent(in), optional :: o
        type(parse_options) :: opts
        type(parse_result) :: res
        type(parsed_number) :: p
        type(adjusted_mantissa) :: cf_am, fc_ap
        logical :: fc_eq, fc_cfok
        integer :: pos, slen, ic
        real(dp) :: fc_out
        ! parse_number_string inline variables
        integer :: pns_first
        integer(i8) :: pns_i, pns_dc, pns_exp, pns_en
        integer :: pns_p, pns_sd, pns_eip, pns_bf, pns_le, pns_ic, pns_ie, pns_fe, pns_sp_, pns_fl
        logical :: pns_hdp, pns_ne, pns_hse, pns_hed
        ! Loop-invariant format flags (hoisted)
        logical :: fmt_bj, fmt_alp, fmt_sci, fmt_fort, fmt_fix
        character :: fmt_dp
        ! compute_float inline variables
        integer(i8) :: cf_q, cf_wi, cf_w
        integer(i4) :: cf_lz
        type(u128) :: cf_pr
        integer :: cf_ub, cf_sa
        integer :: cp_idx
        type(u128) :: cp_sp
        ! loop_parse_eight inline variables
        integer(i8) :: lpe_val, lpe_v, lpe_v1, lpe_v2
        ! am_to_double inline variable
        integer(i8) :: atd_w
        ! mul_u64 inline variables
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
        integer(c_int128_t) :: z128a, z128b, z128r
#else
        integer(i8) :: mu_a0, mu_a1, mu_b0, mu_b1, mu_w0, mu_t, mu_w1, mu_w2
#endif

        opts = DEFAULT_PARSING
        if (present(o)) opts = o
        opts%format = ior(opts%format, FMT_SKIP_WS)

        ! Pre-compute loop-invariant format flags
        fmt_bj   = iand(opts%format, FMT_JSON) /= 0
        fmt_alp  = iand(opts%format, FMT_ALLOW_PLUS) /= 0
        fmt_sci  = iand(opts%format, FMT_SCIENTIFIC) /= 0
        fmt_fort = iand(opts%format, FMT_FORTRAN) /= 0
        fmt_fix  = iand(opts%format, FMT_FIXED) /= 0
        fmt_dp   = opts%decimal_point

        slen = len(stream)
        n = 0
        err = OUTCOMES%OK
        pos = 1

        do while (pos <= slen .and. n < size(values))
            ! Whitespace skip
            do while (pos <= slen)
                ic = iachar(stream(pos:pos))
                if (.not. ((ic >= 9 .and. ic <= 13) .or. ic == 32)) exit
                pos = pos + 1
            end do
            if (pos > slen) exit

            ! === Inlined parse_number_string ===
            pns_first = pos

            parse_number: block
            pns_p = pns_first
            p%valid = .false.
            if (pns_p > slen) then
                p%last_idx = pns_p
                exit parse_number
            end if

            p%negative = (stream(pns_p:pns_p) == '-')

            if (stream(pns_p:pns_p) == '-' .or. &
                (fmt_alp .and. .not. fmt_bj .and. stream(pns_p:pns_p) == '+')) then
                pns_p = pns_p + 1
                if (pns_p > slen) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
                if (fmt_bj) then
                    pns_ic = iachar(stream(pns_p:pns_p))
                    if (pns_ic < 48 .or. pns_ic > 57) then
                        p%last_idx = pns_p
                        exit parse_number
                    end if
                else
                    pns_ic = iachar(stream(pns_p:pns_p))
                    if ((pns_ic < 48 .or. pns_ic > 57) .and. &
                        stream(pns_p:pns_p) /= fmt_dp) then
                        p%last_idx = pns_p
                        exit parse_number
                    end if
                end if
            end if

            pns_sd = pns_p
            pns_i = 0_i8
            do while (pns_p <= slen)
                pns_ic = iachar(stream(pns_p:pns_p))
                if (pns_ic < 48 .or. pns_ic > 57) exit
                pns_i = 10*pns_i + int(pns_ic - 48, i8)
                pns_p = pns_p + 1
            end do

            pns_eip = pns_p
            pns_dc = int(pns_eip - pns_sd, i8)
            p%int_start = pns_sd
            p%int_len = pns_eip - pns_sd

            if (fmt_bj) then
                if (pns_dc == 0) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
                if (stream(pns_sd:pns_sd) == '0' .and. pns_dc > 1) then
                    p%last_idx = pns_sd
                    exit parse_number
                end if
            end if

            pns_exp = 0_i8
            pns_fl = 0
            p%frac_start = 0
            p%frac_len = 0
            pns_hdp = .false.
            if (pns_p <= slen) pns_hdp = (stream(pns_p:pns_p) == fmt_dp)

            if (pns_hdp) then
                pns_p = pns_p + 1
                pns_bf = pns_p
                ! --- Inlined loop_parse_eight ---
                do while (slen - pns_p + 1 >= 8)
                    lpe_val = transfer(stream(pns_p:pns_p+7), 0_i8)
                    lpe_v1 = lpe_val + LPE_46
                    lpe_v2 = lpe_val - ZERO8_U64
                    if (iand(ior(lpe_v1, lpe_v2), LPE_HI) /= 0) exit
                    lpe_v = lpe_val - ZERO8_U64
                    lpe_v = lpe_v*10 + ishft(lpe_v, -8)
                    lpe_v = ishft(iand(lpe_v, LPE_M)*LPE_M1 + iand(ishft(lpe_v,-16), LPE_M)*LPE_M2, -32)
                    pns_i = pns_i * 100000000_i8 + iand(lpe_v, LPE_FF)
                    pns_p = pns_p + 8
                end do
                call loop_parse_four(pns_p, slen, stream, pns_i)
                do while (pns_p <= slen)
                    pns_ic = iachar(stream(pns_p:pns_p))
                    if (pns_ic < 48 .or. pns_ic > 57) exit
                    pns_i = pns_i*10 + int(pns_ic-48, i8)
                    pns_p = pns_p + 1
                end do
                pns_exp = int(pns_bf - pns_p, i8)
                pns_fl = pns_p - pns_bf
                p%frac_start = pns_bf
                p%frac_len = pns_fl
                pns_dc = pns_dc - pns_exp
            end if

            if (fmt_bj) then
                if (pns_hdp .and. pns_exp == 0) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            else
                if (pns_dc == 0) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            end if

            pns_en = 0_i8
            pns_hse = .false.
            if (pns_p <= slen) then
                pns_hse = (fmt_sci .and. &
                           (stream(pns_p:pns_p) == 'e' .or. stream(pns_p:pns_p) == 'E')) .or. &
                          (fmt_fort .and. &
                           (stream(pns_p:pns_p) == '+' .or. stream(pns_p:pns_p) == '-' .or. &
                            stream(pns_p:pns_p) == 'd' .or. stream(pns_p:pns_p) == 'D'))
            end if
            if (pns_hse) then
                pns_le = pns_p
                if (stream(pns_p:pns_p) == 'e' .or. stream(pns_p:pns_p) == 'E' .or. &
                    stream(pns_p:pns_p) == 'd' .or. stream(pns_p:pns_p) == 'D') pns_p = pns_p + 1
                pns_ne = .false.
                if (pns_p <= slen) then
                    if (stream(pns_p:pns_p) == '-') then
                        pns_ne = .true.
                        pns_p = pns_p + 1
                    else if (stream(pns_p:pns_p) == '+') then
                        pns_p = pns_p + 1
                    end if
                end if
                pns_hed = .false.
                if (pns_p <= slen) then
                    pns_ic = iachar(stream(pns_p:pns_p))
                    pns_hed = (pns_ic >= 48 .and. pns_ic <= 57)
                end if
                if (.not. pns_hed) then
                    if (.not. fmt_fix) then
                        p%last_idx = pns_p
                        exit parse_number
                    end if
                    pns_p = pns_le
                else
                    do while (pns_p <= slen)
                        pns_ic = iachar(stream(pns_p:pns_p))
                        if (pns_ic < 48 .or. pns_ic > 57) exit
                        if (pns_en < 268435456_i8) pns_en = 10*pns_en + int(pns_ic-48, i8)
                        pns_p = pns_p + 1
                    end do
                    if (pns_ne) pns_en = -pns_en
                    pns_exp = pns_exp + pns_en
                end if
            else
                if (fmt_sci .and. .not. fmt_fix) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            end if

            p%last_idx = pns_p
            p%valid = .true.
            p%too_many_digits = .false.

            if (pns_dc > 19) then
                pns_sp_ = pns_sd
                do while (pns_sp_ <= slen)
                    if (stream(pns_sp_:pns_sp_) /= '0' .and. &
                        stream(pns_sp_:pns_sp_) /= fmt_dp) exit
                    if (stream(pns_sp_:pns_sp_) == '0') pns_dc = pns_dc - 1
                    pns_sp_ = pns_sp_ + 1
                end do
                if (pns_dc > 19) then
                    p%too_many_digits = .true.
                    p%int_start = pns_sd
                    p%int_len = pns_eip - pns_sd
                    p%frac_start = pns_bf
                    p%frac_len = pns_fl
                    pns_i = 0
                    pns_p = p%int_start
                    pns_ie = pns_p + p%int_len
                    do while (ieor(pns_i, SB64) < ieor(1000000000000000000_i8, SB64) .and. pns_p < pns_ie)
                        pns_i = pns_i*10 + int( iachar(stream(pns_p:pns_p)) - 48, i8)
                        pns_p = pns_p + 1
                    end do
                    if (ieor(pns_i, SB64) >= ieor(1000000000000000000_i8, SB64)) then
                        pns_exp = int(pns_eip - pns_p, i8) + pns_en
                    else
                        pns_p = p%frac_start
                        pns_fe = pns_p + p%frac_len
                        do while (ieor(pns_i, SB64) < ieor(1000000000000000000_i8, SB64) .and. pns_p < pns_fe)
                            pns_i = pns_i*10 + int( iachar(stream(pns_p:pns_p)) - 48, i8)
                            pns_p = pns_p + 1
                        end do
                        pns_exp = int(p%frac_start - pns_p, i8) + pns_en
                    end if
                end if
            end if
            p%exponent = pns_exp
            p%mantissa = pns_i
            end block parse_number

            if (.not. p%valid) then
                err = OUTCOMES%INVALID_INPUT
                return
            end if

            ! --- Inlined from_chars_64 ---
            fc_out = 0.0_dp
            res%outcome = OUTCOMES%OK
            res%pos     = p%last_idx

            ! --- Inlined clinger_fast_path_64 ---
            if (.not. p%too_many_digits) then
                fc_cfok = p%exponent >= int(DP_MFP_LO, i8) .and. p%exponent <= int(DP_MFP_HI, i8) .and. &
                          p%mantissa >= 0 .and. p%mantissa <= DP_MMAX
                if (fc_cfok) then
                    if (p%exponent < 0) then
                        fc_out = real(p%mantissa / DOUBLE_POW10(-p%exponent), dp)
                    else
                        fc_out = real(p%mantissa * DOUBLE_POW10(p%exponent), dp)
                    end if
                    if (p%negative) fc_out = -fc_out
                    values(n + 1) = fc_out
                    n = n + 1
                    pos = res%pos
                    cycle
                end if
            end if

            ! === Inlined compute_float (double precision) ===
            cf_q = p%exponent; cf_wi = p%mantissa
            cf_w = cf_wi
            if (cf_w == 0 .or. cf_q < int(DP_SP10, i8)) then
                cf_am = adjusted_mantissa(0, 0)
            else if (cf_q > int(DP_LP10, i8)) then
                cf_am = adjusted_mantissa(0, int(DP_IP, i4))
            else
                cf_lz = leadz(cf_w)
                cf_w = ishft(cf_w, cf_lz)

                cp_idx = 2*int(cf_q + 342_i8) + 1
                ! Inline mul_u64 #1
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
                z128a = transfer([cf_w, 0_i8], z128a)
                z128b = transfer([POWER5_TABLE(cp_idx), 0_i8], z128b)
                z128r = z128a * z128b
                cf_pr = transfer(z128r, cf_pr)
#else
                mu_a0 = iand(cf_w, M32); mu_a1 = iand(ishft(cf_w, -32), M32)
                mu_b0 = iand(POWER5_TABLE(cp_idx), M32)
                mu_b1 = iand(ishft(POWER5_TABLE(cp_idx), -32), M32)
                mu_w0 = mu_a0 * mu_b0
                mu_t = mu_a1*mu_b0 + iand(ishft(mu_w0, -32), M32)
                mu_w1 = iand(mu_t, M32)
                mu_w2 = iand(ishft(mu_t, -32), M32)
                mu_w1 = mu_w1 + mu_a0 * mu_b1
                cf_pr%lo = cf_w * POWER5_TABLE(cp_idx)
                cf_pr%hi = mu_a1*mu_b1 + mu_w2 + iand(ishft(mu_w1, -32), M32)
#endif
                if (iand(cf_pr%hi, DP_CPM) == DP_CPM) then
                    ! Inline mul_u64 #2
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
                    z128b = transfer([POWER5_TABLE(cp_idx + 1), 0_i8], z128b)
                    z128r = z128a * z128b
                    cp_sp = transfer(z128r, cp_sp)
#else
                    mu_b0 = iand(POWER5_TABLE(cp_idx+1), M32)
                    mu_b1 = iand(ishft(POWER5_TABLE(cp_idx+1), -32), M32)
                    mu_w0 = mu_a0 * mu_b0
                    mu_t = mu_a1*mu_b0 + iand(ishft(mu_w0, -32), M32)
                    mu_w1 = iand(mu_t, M32)
                    mu_w2 = iand(ishft(mu_t, -32), M32)
                    mu_w1 = mu_w1 + mu_a0 * mu_b1
                    cp_sp%lo = cf_w * POWER5_TABLE(cp_idx+1)
                    cp_sp%hi = mu_a1*mu_b1 + mu_w2 + iand(ishft(mu_w1, -32), M32)
#endif
                    cf_pr%lo = cf_pr%lo + cp_sp%hi
                    if (ieor(cf_pr%lo, SB64) < ieor(cp_sp%hi, SB64)) cf_pr%hi = cf_pr%hi + 1
                end if

                cf_ub = int(ishft(cf_pr%hi, -63))
                cf_sa = cf_ub + 9
                cf_am%mantissa = ishft(cf_pr%hi, -cf_sa)
                cf_am%power2 = int(ishft(217706_i8 * int(cf_q, i8), -16), i4) + 1086_i4 &
                             + int(cf_ub, i4) - cf_lz

                if (cf_am%power2 <= 0) then
                    if (-cf_am%power2 + 1 >= 64) then
                        cf_am = adjusted_mantissa(0_i8, 0_i4)
                    else
                        cf_am%mantissa = ishft(cf_am%mantissa, cf_am%power2 - 1_i4)
                        cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                        cf_am%mantissa = ishft(cf_am%mantissa, -1)
                        if (cf_am%mantissa < DP_HB) then
                            cf_am%power2 = 0
                        else
                            cf_am%power2 = 1
                        end if
                    end if
                else
                    if (iand(cf_pr%lo, not(1_i8)) == 0 .and. &
                        cf_q >= int(DP_MRT_LO, i8) .and. &
                        cf_q <= int(DP_MRT_HI, i8) .and. &
                        iand(cf_am%mantissa, 3_i8) == 1 .and. &
                        ishft(cf_am%mantissa, cf_sa) == cf_pr%hi) &
                            cf_am%mantissa = iand(cf_am%mantissa, not(1_i8))

                    cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                    cf_am%mantissa = ishft(cf_am%mantissa, -1)

                    if (cf_am%mantissa >= DP_2HB) then
                        cf_am%mantissa = DP_HB
                        cf_am%power2 = cf_am%power2 + 1
                    end if

                    cf_am%mantissa = iand(cf_am%mantissa, not(DP_HB))
                    if (cf_am%power2 >= int(DP_IP, i4)) then
                        cf_am%power2 = int(DP_IP, i4)
                        cf_am%mantissa = 0
                    end if
                end if
            end if
            ! === End inlined compute_float ===

            if (p%too_many_digits .and. cf_am%power2 >= 0) then
                call compute_float(p%exponent, p%mantissa + 1, DOUBLE_FMT, fc_ap)
                fc_eq = cf_am%mantissa == fc_ap%mantissa .and. cf_am%power2 == fc_ap%power2
                if (.not. fc_eq) call compute_error(p%exponent, p%mantissa, DOUBLE_FMT, cf_am)
            end if
            if (cf_am%power2 < 0) then
                fc_ap = cf_am
                call digit_comp(stream, p, fc_ap, DOUBLE_FMT, cf_am)
            end if

            ! --- Inlined am_to_double ---
            atd_w = ior(cf_am%mantissa, ishft(int(cf_am%power2, i8), DP_MB))
            if (p%negative) atd_w = ior(atd_w, SB64)
            values(n + 1) = transfer(atd_w, 0.0_dp)

            if ((p%mantissa /= 0 .and. cf_am%mantissa == 0 .and. cf_am%power2 == 0) .or. &
                 cf_am%power2 == int(DP_IP, i4)) then
                err = OUTCOMES%OUT_OF_RANGE
                return
            end if
            n = n + 1
            pos = res%pos
        end do
    end subroutine parse_double_array

    !> Parse a whitespace-delimited stream of single-precision floats into an array.
    !> Hot-path routines manually inlined for performance without LTO.
    pure subroutine parse_float_array(stream, values, n, err, o)
        character(*), intent(in) :: stream
        real(sp), intent(out) :: values(:)
        integer, intent(out) :: n
        type(outcome), intent(out) :: err
        type(parse_options), intent(in), optional :: o
        type(parse_options) :: opts
        type(parse_result) :: res
        type(parsed_number) :: p
        type(adjusted_mantissa) :: cf_am, fc_ap
        logical :: fc_eq, fc_cfok
        integer :: pos, slen, ic
        real(sp) :: fc_outf
        ! parse_number_string inline variables
        integer :: pns_first, pns_last
        logical :: pns_bj
        type(parse_options) :: pns_opts
        integer(i8) :: pns_i, pns_dc, pns_exp, pns_en
        integer :: pns_p, pns_sd, pns_eip, pns_bf, pns_le, pns_ic, pns_ie, pns_fe, pns_sp_
        logical :: pns_alp, pns_hdp, pns_ne, pns_hse, pns_hed
        ! compute_float inline variables
        integer(i8) :: cf_q, cf_wi, cf_w
        integer(i4) :: cf_lz
        type(u128) :: cf_pr
        integer :: cf_ub, cf_sa
        integer :: cp_idx
        type(u128) :: cp_sp
        ! loop_parse_eight inline variables
        integer(i8) :: lpe_val, lpe_v, lpe_v1, lpe_v2
        ! am_to_float inline variable
        integer(i4) :: atf_w
        ! mul_u64 inline variables
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
        integer(c_int128_t) :: z128a, z128b, z128r
#else
        integer(i8) :: mu_a0, mu_a1, mu_b0, mu_b1, mu_w0, mu_t, mu_w1, mu_w2
#endif

        opts = DEFAULT_PARSING
        if (present(o)) opts = o
        opts%format = ior(opts%format, FMT_SKIP_WS)

        slen = len(stream)
        n = 0
        err = OUTCOMES%OK
        pos = 1

        do while (pos <= slen .and. n < size(values))
            do while (pos <= slen)
                ic = iachar(stream(pos:pos))
                if (.not. ((ic >= 9 .and. ic <= 13) .or. ic == 32)) exit
                pos = pos + 1
            end do
            if (pos > slen) exit

            ! === Inlined parse_number_string ===
            pns_first = pos; pns_last = slen; pns_opts = opts
            pns_bj = iand(opts%format, FMT_JSON) /= 0

            parse_number: block
            pns_p = pns_first
            p%valid = .false.
            if (pns_p > pns_last) then
                p%last_idx = pns_p
                exit parse_number
            end if

            p%negative = (stream(pns_p:pns_p) == '-')
            pns_alp = iand(pns_opts%format, FMT_ALLOW_PLUS) /= 0

            if (stream(pns_p:pns_p) == '-' .or. &
                (pns_alp .and. .not. pns_bj .and. stream(pns_p:pns_p) == '+')) then
                pns_p = pns_p + 1
                if (pns_p > pns_last) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
                if (pns_bj) then
                    pns_ic = iachar(stream(pns_p:pns_p))
                    if (pns_ic < 48 .or. pns_ic > 57) then
                        p%last_idx = pns_p
                        exit parse_number
                    end if
                else
                    pns_ic = iachar(stream(pns_p:pns_p))
                    if ((pns_ic < 48 .or. pns_ic > 57) .and. &
                        stream(pns_p:pns_p) /= pns_opts%decimal_point) then
                        p%last_idx = pns_p
                        exit parse_number
                    end if
                end if
            end if

            pns_sd = pns_p
            pns_i = 0_i8
            do while (pns_p <= pns_last)
                pns_ic = iachar(stream(pns_p:pns_p))
                if (pns_ic < 48 .or. pns_ic > 57) exit
                pns_i = 10*pns_i + int(pns_ic - 48, i8)
                pns_p = pns_p + 1
            end do

            pns_eip = pns_p
            pns_dc = int(pns_eip - pns_sd, i8)
            p%int_start = pns_sd
            p%int_len = int(pns_dc)

            if (pns_bj) then
                if (pns_dc == 0) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
                if (stream(pns_sd:pns_sd) == '0' .and. pns_dc > 1) then
                    p%last_idx = pns_sd
                    exit parse_number
                end if
            end if

            pns_exp = 0_i8
            p%frac_start = 0
            p%frac_len = 0
            pns_hdp = .false.
            if (pns_p <= pns_last) pns_hdp = (stream(pns_p:pns_p) == pns_opts%decimal_point)

            if (pns_hdp) then
                pns_p = pns_p + 1
                pns_bf = pns_p
                do while (pns_last - pns_p + 1 >= 8)
                    lpe_val = transfer(stream(pns_p:pns_p+7), 0_i8)
                    lpe_v1 = lpe_val + LPE_46
                    lpe_v2 = lpe_val - ZERO8_U64
                    if (iand(ior(lpe_v1, lpe_v2), LPE_HI) /= 0) exit
                    lpe_v = lpe_val - ZERO8_U64
                    lpe_v = lpe_v*10 + ishft(lpe_v, -8)
                    lpe_v = ishft(iand(lpe_v, LPE_M)*LPE_M1 + iand(ishft(lpe_v,-16), LPE_M)*LPE_M2, -32)
                    pns_i = pns_i * 100000000_i8 + iand(lpe_v, LPE_FF)
                    pns_p = pns_p + 8
                end do
                call loop_parse_four(pns_p, pns_last, stream, pns_i)
                do while (pns_p <= pns_last)
                    pns_ic = iachar(stream(pns_p:pns_p))
                    if (pns_ic < 48 .or. pns_ic > 57) exit
                    pns_i = pns_i*10 + int(pns_ic-48, i8)
                    pns_p = pns_p + 1
                end do
                pns_exp = int(pns_bf - pns_p, i8)
                p%frac_start = pns_bf
                p%frac_len = pns_p - pns_bf
                pns_dc = pns_dc - pns_exp
            end if

            if (pns_bj) then
                if (pns_hdp .and. pns_exp == 0) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            else
                if (pns_dc == 0) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            end if

            pns_en = 0_i8
            pns_hse = .false.
            if (pns_p <= pns_last) then
                pns_hse = (iand(pns_opts%format, FMT_SCIENTIFIC) /= 0 .and. &
                           (stream(pns_p:pns_p) == 'e' .or. stream(pns_p:pns_p) == 'E')) .or. &
                          (iand(pns_opts%format, FMT_FORTRAN) /= 0 .and. &
                           (stream(pns_p:pns_p) == '+' .or. stream(pns_p:pns_p) == '-' .or. &
                            stream(pns_p:pns_p) == 'd' .or. stream(pns_p:pns_p) == 'D'))
            end if
            if (pns_hse) then
                pns_le = pns_p
                if (stream(pns_p:pns_p) == 'e' .or. stream(pns_p:pns_p) == 'E' .or. &
                    stream(pns_p:pns_p) == 'd' .or. stream(pns_p:pns_p) == 'D') pns_p = pns_p + 1
                pns_ne = .false.
                if (pns_p <= pns_last) then
                    if (stream(pns_p:pns_p) == '-') then
                        pns_ne = .true.
                        pns_p = pns_p + 1
                    else if (stream(pns_p:pns_p) == '+') then
                        pns_p = pns_p + 1
                    end if
                end if
                pns_hed = .false.
                if (pns_p <= pns_last) then
                    pns_ic = iachar(stream(pns_p:pns_p))
                    pns_hed = (pns_ic >= 48 .and. pns_ic <= 57)
                end if
                if (.not. pns_hed) then
                    if (iand(pns_opts%format, FMT_FIXED) == 0) then
                        p%last_idx = pns_p
                        exit parse_number
                    end if
                    pns_p = pns_le
                else
                    do while (pns_p <= pns_last)
                        pns_ic = iachar(stream(pns_p:pns_p))
                        if (pns_ic < 48 .or. pns_ic > 57) exit
                        if (pns_en < 268435456_i8) pns_en = 10*pns_en + int(pns_ic-48, i8)
                        pns_p = pns_p + 1
                    end do
                    if (pns_ne) pns_en = -pns_en
                    pns_exp = pns_exp + pns_en
                end if
            else
                if (iand(pns_opts%format, FMT_SCIENTIFIC) /= 0 .and. &
                    iand(pns_opts%format, FMT_FIXED) == 0) then
                    p%last_idx = pns_p
                    exit parse_number
                end if
            end if

            p%last_idx = pns_p
            p%valid = .true.
            p%too_many_digits = .false.

            if (pns_dc > 19) then
                pns_sp_ = pns_sd
                do while (pns_sp_ <= pns_last)
                    if (stream(pns_sp_:pns_sp_) /= '0' .and. &
                        stream(pns_sp_:pns_sp_) /= pns_opts%decimal_point) exit
                    if (stream(pns_sp_:pns_sp_) == '0') pns_dc = pns_dc - 1
                    pns_sp_ = pns_sp_ + 1
                end do
                if (pns_dc > 19) then
                    p%too_many_digits = .true.
                    pns_i = 0
                    pns_p = p%int_start
                    pns_ie = pns_p + p%int_len
                    do while (ieor(pns_i, SB64) < ieor(1000000000000000000_i8, SB64) .and. pns_p < pns_ie)
                        pns_i = pns_i*10 + int( iachar(stream(pns_p:pns_p)) - 48, i8)
                        pns_p = pns_p + 1
                    end do
                    if (ieor(pns_i, SB64) >= ieor(1000000000000000000_i8, SB64)) then
                        pns_exp = int(pns_eip - pns_p, i8) + pns_en
                    else
                        pns_p = p%frac_start
                        pns_fe = pns_p + p%frac_len
                        do while (ieor(pns_i, SB64) < ieor(1000000000000000000_i8, SB64) .and. pns_p < pns_fe)
                            pns_i = pns_i*10 + int( iachar(stream(pns_p:pns_p)) - 48, i8)
                            pns_p = pns_p + 1
                        end do
                        pns_exp = int(p%frac_start - pns_p, i8) + pns_en
                    end if
                end if
            end if
            p%exponent = pns_exp
            p%mantissa = pns_i
            end block parse_number

            if (.not. p%valid) then
                err = OUTCOMES%INVALID_INPUT
                return
            end if

            ! --- Inlined from_chars_32 ---
            fc_outf = 0.0_sp
            res%outcome = OUTCOMES%OK
            res%pos     = p%last_idx

            ! --- Inlined clinger_fast_path_32 ---
            if (.not. p%too_many_digits) then
                fc_cfok = p%exponent >= int(SP_MFP_LO, i8) .and. p%exponent <= int(SP_MFP_HI, i8) .and. &
                          p%mantissa >= 0 .and. p%mantissa <= SP_MMAX
                if (fc_cfok) then
                    if (p%exponent < 0) then
                        fc_outf = real(p%mantissa, sp) / FLOAT_POW10(-p%exponent)
                    else
                        fc_outf = real(p%mantissa, sp) * FLOAT_POW10(p%exponent)
                    end if
                    if (p%negative) fc_outf = -fc_outf
                    values(n + 1) = fc_outf
                    n = n + 1
                    pos = res%pos
                    cycle
                end if
            end if

            ! === Inlined compute_float (single precision) ===
            cf_q = p%exponent; cf_wi = p%mantissa
            cf_w = cf_wi
            if (cf_w == 0 .or. cf_q < int(SP_SP10, i8)) then
                cf_am = adjusted_mantissa(0, 0)
            else if (cf_q > int(SP_LP10, i8)) then
                cf_am = adjusted_mantissa(0, int(SP_IP, i4))
            else
                cf_lz = leadz(cf_w)
                cf_w = ishft(cf_w, cf_lz)

                cp_idx = 2*int(cf_q + 342_i8) + 1
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
                z128a = transfer([cf_w, 0_i8], z128a)
                z128b = transfer([POWER5_TABLE(cp_idx), 0_i8], z128b)
                z128r = z128a * z128b
                cf_pr = transfer(z128r, cf_pr)
#else
                mu_a0 = iand(cf_w, M32); mu_a1 = iand(ishft(cf_w, -32), M32)
                mu_b0 = iand(POWER5_TABLE(cp_idx), M32)
                mu_b1 = iand(ishft(POWER5_TABLE(cp_idx), -32), M32)
                mu_w0 = mu_a0 * mu_b0
                mu_t = mu_a1*mu_b0 + iand(ishft(mu_w0, -32), M32)
                mu_w1 = iand(mu_t, M32)
                mu_w2 = iand(ishft(mu_t, -32), M32)
                mu_w1 = mu_w1 + mu_a0 * mu_b1
                cf_pr%lo = cf_w * POWER5_TABLE(cp_idx)
                cf_pr%hi = mu_a1*mu_b1 + mu_w2 + iand(ishft(mu_w1, -32), M32)
#endif
                if (iand(cf_pr%hi, SP_CPM) == SP_CPM) then
#if !defined(__INTEL_COMPILER) && !defined(__INTEL_LLVM_COMPILER)
                    z128b = transfer([POWER5_TABLE(cp_idx + 1), 0_i8], z128b)
                    z128r = z128a * z128b
                    cp_sp = transfer(z128r, cp_sp)
#else
                    mu_b0 = iand(POWER5_TABLE(cp_idx+1), M32)
                    mu_b1 = iand(ishft(POWER5_TABLE(cp_idx+1), -32), M32)
                    mu_w0 = mu_a0 * mu_b0
                    mu_t = mu_a1*mu_b0 + iand(ishft(mu_w0, -32), M32)
                    mu_w1 = iand(mu_t, M32)
                    mu_w2 = iand(ishft(mu_t, -32), M32)
                    mu_w1 = mu_w1 + mu_a0 * mu_b1
                    cp_sp%lo = cf_w * POWER5_TABLE(cp_idx+1)
                    cp_sp%hi = mu_a1*mu_b1 + mu_w2 + iand(ishft(mu_w1, -32), M32)
#endif
                    cf_pr%lo = cf_pr%lo + cp_sp%hi
                    if (ieor(cf_pr%lo, SB64) < ieor(cp_sp%hi, SB64)) cf_pr%hi = cf_pr%hi + 1
                end if

                cf_ub = int(ishft(cf_pr%hi, -63))
                cf_sa = cf_ub + 38
                cf_am%mantissa = ishft(cf_pr%hi, -cf_sa)
                cf_am%power2 = int(ishft(217706_i8 * int(cf_q, i8), -16), i4) + 190_i4 &
                             + int(cf_ub, i4) - cf_lz

                if (cf_am%power2 <= 0) then
                    if (-cf_am%power2 + 1 >= 64) then
                        cf_am = adjusted_mantissa(0_i8, 0_i4)
                    else
                        cf_am%mantissa = ishft(cf_am%mantissa, cf_am%power2 - 1_i4)
                        cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                        cf_am%mantissa = ishft(cf_am%mantissa, -1)
                        if (cf_am%mantissa < SP_HB) then
                            cf_am%power2 = 0
                        else
                            cf_am%power2 = 1
                        end if
                    end if
                else
                    if (iand(cf_pr%lo, not(1_i8)) == 0 .and. &
                        cf_q >= int(SP_MRT_LO, i8) .and. &
                        cf_q <= int(SP_MRT_HI, i8) .and. &
                        iand(cf_am%mantissa, 3_i8) == 1 .and. &
                        ishft(cf_am%mantissa, cf_sa) == cf_pr%hi) &
                            cf_am%mantissa = iand(cf_am%mantissa, not(1_i8))

                    cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                    cf_am%mantissa = ishft(cf_am%mantissa, -1)

                    if (cf_am%mantissa >= SP_2HB) then
                        cf_am%mantissa = SP_HB
                        cf_am%power2 = cf_am%power2 + 1
                    end if

                    cf_am%mantissa = iand(cf_am%mantissa, not(SP_HB))
                    if (cf_am%power2 >= int(SP_IP, i4)) then
                        cf_am%power2 = int(SP_IP, i4)
                        cf_am%mantissa = 0
                    end if
                end if
            end if

            if (p%too_many_digits .and. cf_am%power2 >= 0) then
                call compute_float(p%exponent, p%mantissa + 1, FLOAT_FMT, fc_ap)
                fc_eq = cf_am%mantissa == fc_ap%mantissa .and. cf_am%power2 == fc_ap%power2
                if (.not. fc_eq) call compute_error(p%exponent, p%mantissa, FLOAT_FMT, cf_am)
            end if
            if (cf_am%power2 < 0) then
                fc_ap = cf_am
                call digit_comp(stream, p, fc_ap, FLOAT_FMT, cf_am)
            end if

            ! --- Inlined am_to_float ---
            atf_w = int(cf_am%mantissa, i4)
            atf_w = ior(atf_w, ishft(cf_am%power2, SP_MB))
            if (p%negative) atf_w = ior(atf_w, ishft(1_i4, 31))
            values(n + 1) = transfer(atf_w, 0.0_sp)

            if ((p%mantissa /= 0 .and. cf_am%mantissa == 0 .and. cf_am%power2 == 0) .or. &
                cf_am%power2 == int(SP_IP, i4)) then
                err = OUTCOMES%OUT_OF_RANGE
                return
            end if
            n = n + 1
            pos = res%pos
        end do
    end subroutine parse_float_array

end module fast_float_module
