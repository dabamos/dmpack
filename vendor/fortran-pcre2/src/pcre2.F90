! pcre2.F90
!
! A collection of ISO_C_BINDING interfaces to PCRE2 for Fortran 2018.
!
! Author:  Philipp Engel
! Licence: ISC
module pcre2
    use, intrinsic :: iso_c_binding, only: c_associated, c_f_pointer, &
                                           c_char, c_int, c_int32_t, c_size_t, &
                                           c_ptr, c_null_char, c_null_ptr
#if HAS_UNSIGNED

    use, intrinsic :: iso_c_binding, only: c_uint32_t

#endif
    use, intrinsic :: iso_fortran_env, only: i8 => int64
    implicit none (type, external)
    private

#if HAS_UNSIGNED

    public :: c_uint32_t

#else

    integer, parameter, public :: c_uint32_t = c_int32_t

#endif

    public :: c_associated
    public :: c_f_pointer

    public :: c_char
    public :: c_int
    public :: c_int32_t
    public :: c_size_t
    public :: c_ptr
    public :: c_null_char
    public :: c_null_ptr

    integer,                  parameter, public :: pcre2_uchar           = c_char
    integer,                  parameter, public :: pcre2_sptr            = c_char
    integer,                  parameter, public :: pcre2_size            = c_size_t
    integer,                  parameter, public :: pcre2_size_max        = storage_size(int(0, kind=pcre2_size)) / 8
    integer(kind=pcre2_size), parameter, public :: pcre2_zero_terminated = not(int(0, kind=pcre2_size))
    integer(kind=pcre2_size), parameter, public :: pcre2_unset           = not(int(0, kind=pcre2_size))

    integer(kind=c_int), parameter, public :: PCRE2_ANCHORED     = int(z'80000000')
    integer(kind=c_int), parameter, public :: PCRE2_NO_UTF_CHECK = int(z'40000000')
    integer(kind=c_int), parameter, public :: PCRE2_ENDANCHORED  = int(z'20000000')

    ! The following option bits can be passed only to pcre2_compile().
    integer(kind=c_int), parameter, public :: PCRE2_ALLOW_EMPTY_CLASS   = int(z'00000001')
    integer(kind=c_int), parameter, public :: PCRE2_ALT_BSUX            = int(z'00000002')
    integer(kind=c_int), parameter, public :: PCRE2_AUTO_CALLOUT        = int(z'00000004')
    integer(kind=c_int), parameter, public :: PCRE2_CASELESS            = int(z'00000008')
    integer(kind=c_int), parameter, public :: PCRE2_DOLLAR_ENDONLY      = int(z'00000010')
    integer(kind=c_int), parameter, public :: PCRE2_DOTALL              = int(z'00000020')
    integer(kind=c_int), parameter, public :: PCRE2_DUPNAMES            = int(z'00000040')
    integer(kind=c_int), parameter, public :: PCRE2_EXTENDED            = int(z'00000080')
    integer(kind=c_int), parameter, public :: PCRE2_FIRSTLINE           = int(z'00000100')
    integer(kind=c_int), parameter, public :: PCRE2_MATCH_UNSET_BACKREF = int(z'00000200')
    integer(kind=c_int), parameter, public :: PCRE2_MULTILINE           = int(z'00000400')
    integer(kind=c_int), parameter, public :: PCRE2_NEVER_UCP           = int(z'00000800')
    integer(kind=c_int), parameter, public :: PCRE2_NEVER_UTF           = int(z'00001000')
    integer(kind=c_int), parameter, public :: PCRE2_NO_AUTO_CAPTURE     = int(z'00002000')
    integer(kind=c_int), parameter, public :: PCRE2_NO_AUTO_POSSESS     = int(z'00004000')
    integer(kind=c_int), parameter, public :: PCRE2_NO_DOTSTAR_ANCHOR   = int(z'00008000')
    integer(kind=c_int), parameter, public :: PCRE2_NO_START_OPTIMIZE   = int(z'00010000')
    integer(kind=c_int), parameter, public :: PCRE2_UCP                 = int(z'00020000')
    integer(kind=c_int), parameter, public :: PCRE2_UNGREEDY            = int(z'00040000')
    integer(kind=c_int), parameter, public :: PCRE2_UTF                 = int(z'00080000')
    integer(kind=c_int), parameter, public :: PCRE2_NEVER_BACKSLASH_C   = int(z'00100000')
    integer(kind=c_int), parameter, public :: PCRE2_ALT_CIRCUMFLEX      = int(z'00200000')
    integer(kind=c_int), parameter, public :: PCRE2_ALT_VERBNAMES       = int(z'00400000')
    integer(kind=c_int), parameter, public :: PCRE2_USE_OFFSET_LIMIT    = int(z'00800000')
    integer(kind=c_int), parameter, public :: PCRE2_EXTENDED_MORE       = int(z'01000000')
    integer(kind=c_int), parameter, public :: PCRE2_LITERAL             = int(z'02000000')
    integer(kind=c_int), parameter, public :: PCRE2_MATCH_INVALID_UTF   = int(z'04000000')

    ! An additional compile options word is available in the compile context.
    integer(kind=c_int), parameter, public :: PCRE2_EXTRA_ALLOW_SURROGATE_ESCAPES = int(z'00000001')
    integer(kind=c_int), parameter, public :: PCRE2_EXTRA_BAD_ESCAPE_IS_LITERAL   = int(z'00000002')
    integer(kind=c_int), parameter, public :: PCRE2_EXTRA_MATCH_WORD              = int(z'00000004')
    integer(kind=c_int), parameter, public :: PCRE2_EXTRA_MATCH_LINE              = int(z'00000008')
    integer(kind=c_int), parameter, public :: PCRE2_EXTRA_ESCAPED_CR_IS_LF        = int(z'00000010')
    integer(kind=c_int), parameter, public :: PCRE2_EXTRA_ALT_BSUX                = int(z'00000020')
    integer(kind=c_int), parameter, public :: PCRE2_EXTRA_ALLOW_LOOKAROUND_BSK    = int(z'00000040')

    ! These are for pcre2_jit_compile().
    integer(kind=c_int), parameter, public :: PCRE2_JIT_COMPLETE     = int(z'00000001')
    integer(kind=c_int), parameter, public :: PCRE2_JIT_PARTIAL_SOFT = int(z'00000002')
    integer(kind=c_int), parameter, public :: PCRE2_JIT_PARTIAL_HARD = int(z'00000004')
    integer(kind=c_int), parameter, public :: PCRE2_JIT_INVALID_UTF  = int(z'00000100')

    ! These are for pcre2_match(), pcre2_dfa_match(), pcre2_jit_match(), and pcre2_substitute().
    integer(kind=c_int), parameter, public :: PCRE2_NOTBOL                      = int(z'00000001')
    integer(kind=c_int), parameter, public :: PCRE2_NOTEOL                      = int(z'00000002')
    integer(kind=c_int), parameter, public :: PCRE2_NOTEMPTY                    = int(z'00000004')
    integer(kind=c_int), parameter, public :: PCRE2_NOTEMPTY_ATSTART            = int(z'00000008')
    integer(kind=c_int), parameter, public :: PCRE2_PARTIAL_SOFT                = int(z'00000010')
    integer(kind=c_int), parameter, public :: PCRE2_PARTIAL_HARD                = int(z'00000020')
    integer(kind=c_int), parameter, public :: PCRE2_DFA_RESTART                 = int(z'00000040')
    integer(kind=c_int), parameter, public :: PCRE2_DFA_SHORTEST                = int(z'00000080')
    integer(kind=c_int), parameter, public :: PCRE2_SUBSTITUTE_GLOBAL           = int(z'00000100')
    integer(kind=c_int), parameter, public :: PCRE2_SUBSTITUTE_EXTENDED         = int(z'00000200')
    integer(kind=c_int), parameter, public :: PCRE2_SUBSTITUTE_UNSET_EMPTY      = int(z'00000400')
    integer(kind=c_int), parameter, public :: PCRE2_SUBSTITUTE_UNKNOWN_UNSET    = int(z'00000800')
    integer(kind=c_int), parameter, public :: PCRE2_SUBSTITUTE_OVERFLOW_LENGTH  = int(z'00001000')
    integer(kind=c_int), parameter, public :: PCRE2_NO_JIT                      = int(z'00002000')
    integer(kind=c_int), parameter, public :: PCRE2_COPY_MATCHED_SUBJECT        = int(z'00004000')
    integer(kind=c_int), parameter, public :: PCRE2_SUBSTITUTE_LITERAL          = int(z'00008000')
    integer(kind=c_int), parameter, public :: PCRE2_SUBSTITUTE_MATCHED          = int(z'00010000')
    integer(kind=c_int), parameter, public :: PCRE2_SUBSTITUTE_REPLACEMENT_ONLY = int(z'00020000')

    ! Options for pcre2_pattern_convert().
    integer(kind=c_int), parameter, public :: PCRE2_CONVERT_UTF                    = int(z'00000001')
    integer(kind=c_int), parameter, public :: PCRE2_CONVERT_NO_UTF_CHECK           = int(z'00000002')
    integer(kind=c_int), parameter, public :: PCRE2_CONVERT_POSIX_BASIC            = int(z'00000004')
    integer(kind=c_int), parameter, public :: PCRE2_CONVERT_POSIX_EXTENDED         = int(z'00000008')
    integer(kind=c_int), parameter, public :: PCRE2_CONVERT_GLOB                   = int(z'00000010')
    integer(kind=c_int), parameter, public :: PCRE2_CONVERT_GLOB_NO_WILD_SEPARATOR = int(z'00000030')
    integer(kind=c_int), parameter, public :: PCRE2_CONVERT_GLOB_NO_STARSTAR       = int(z'00000050')

    integer(kind=c_int), parameter, public :: PCRE2_NEWLINE_CR      = 1
    integer(kind=c_int), parameter, public :: PCRE2_NEWLINE_LF      = 2
    integer(kind=c_int), parameter, public :: PCRE2_NEWLINE_CRLF    = 3
    integer(kind=c_int), parameter, public :: PCRE2_NEWLINE_ANY     = 4
    integer(kind=c_int), parameter, public :: PCRE2_NEWLINE_ANYCRLF = 5
    integer(kind=c_int), parameter, public :: PCRE2_NEWLINE_NUL     = 6

    integer(kind=c_int), parameter, public :: PCRE2_BSR_UNICODE = 1
    integer(kind=c_int), parameter, public :: PCRE2_BSR_ANYCRLF = 2

    integer(kind=c_int), parameter, public :: PCRE2_ERROR_END_BACKSLASH                       = 101
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_END_BACKSLASH_C                     = 102
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNKNOWN_ESCAPE                      = 103
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_QUANTIFIER_OUT_OF_ORDER             = 104
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_QUANTIFIER_TOO_BIG                  = 105
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MISSING_SQUARE_BRACKET              = 106
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_ESCAPE_INVALID_IN_CLASS             = 107
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CLASS_RANGE_ORDER                   = 108
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_QUANTIFIER_INVALID                  = 109
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_UNEXPECTED_REPEAT          = 110
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INVALID_AFTER_PARENS_QUERY          = 111
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_POSIX_CLASS_NOT_IN_CLASS            = 112
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_POSIX_NO_SUPPORT_COLLATING          = 113
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MISSING_CLOSING_PARENTHESIS         = 114
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BAD_SUBPATTERN_REFERENCE            = 115
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_NULL_PATTERN                        = 116
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BAD_OPTIONS                         = 117
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MISSING_COMMENT_CLOSING             = 118
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_PARENTHESES_NEST_TOO_DEEP           = 119
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_PATTERN_TOO_LARGE                   = 120
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_HEAP_FAILED                         = 121
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNMATCHED_CLOSING_PARENTHESIS       = 122
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_CODE_OVERFLOW              = 123
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MISSING_CONDITION_CLOSING           = 124
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_LOOKBEHIND_NOT_FIXED_LENGTH         = 125
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_ZERO_RELATIVE_REFERENCE             = 126
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_TOO_MANY_CONDITION_BRANCHES         = 127
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CONDITION_ASSERTION_EXPECTED        = 128
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BAD_RELATIVE_REFERENCE              = 129
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNKNOWN_POSIX_CLASS                 = 130
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_STUDY_ERROR                = 131
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNICODE_NOT_SUPPORTED               = 132
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_PARENTHESES_STACK_CHECK             = 133
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CODE_POINT_TOO_BIG                  = 134
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_LOOKBEHIND_TOO_COMPLICATED          = 135
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_LOOKBEHIND_INVALID_BACKSLASH_C      = 136
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNSUPPORTED_ESCAPE_SEQUENCE         = 137
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CALLOUT_NUMBER_TOO_BIG              = 138
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MISSING_CALLOUT_CLOSING             = 139
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_ESCAPE_INVALID_IN_VERB              = 140
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNRECOGNIZED_AFTER_QUERY_P          = 141
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MISSING_NAME_TERMINATOR             = 142
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_DUPLICATE_SUBPATTERN_NAME           = 143
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INVALID_SUBPATTERN_NAME             = 144
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNICODE_PROPERTIES_UNAVAILABLE      = 145
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MALFORMED_UNICODE_PROPERTY          = 146
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNKNOWN_UNICODE_PROPERTY            = 147
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_SUBPATTERN_NAME_TOO_LONG            = 148
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_TOO_MANY_NAMED_SUBPATTERNS          = 149
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CLASS_INVALID_RANGE                 = 150
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_OCTAL_BYTE_TOO_BIG                  = 151
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_OVERRAN_WORKSPACE          = 152
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_MISSING_SUBPATTERN         = 153
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_DEFINE_TOO_MANY_BRANCHES            = 154
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BACKSLASH_O_MISSING_BRACE           = 155
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_UNKNOWN_NEWLINE            = 156
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BACKSLASH_G_SYNTAX                  = 157
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_PARENS_QUERY_R_MISSING_CLOSING      = 158
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_VERB_ARGUMENT_NOT_ALLOWED           = 159 ! obsolete
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_VERB_UNKNOWN                        = 160
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_SUBPATTERN_NUMBER_TOO_BIG           = 161
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_SUBPATTERN_NAME_EXPECTED            = 162
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_PARSED_OVERFLOW            = 163
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INVALID_OCTAL                       = 164
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_SUBPATTERN_NAMES_MISMATCH           = 165
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MARK_MISSING_ARGUMENT               = 166
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INVALID_HEXADECIMAL                 = 167
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BACKSLASH_C_SYNTAX                  = 168
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BACKSLASH_K_SYNTAX                  = 169
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_BAD_CODE_LOOKBEHINDS       = 170
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BACKSLASH_N_IN_CLASS                = 171
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CALLOUT_STRING_TOO_LONG             = 172
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNICODE_DISALLOWED_CODE_POINT       = 173
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UTF_IS_DISABLED                     = 174
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UCP_IS_DISABLED                     = 175
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_VERB_NAME_TOO_LONG                  = 176
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BACKSLASH_U_CODE_POINT_TOO_BIG      = 177
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MISSING_OCTAL_OR_HEX_DIGITS         = 178
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_VERSION_CONDITION_SYNTAX            = 179
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_BAD_CODE_AUTO_POSSESS      = 180
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CALLOUT_NO_STRING_DELIMITER         = 181
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CALLOUT_BAD_STRING_DELIMITER        = 182
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BACKSLASH_C_CALLER_DISABLED         = 183
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_QUERY_BARJX_NEST_TOO_DEEP           = 184
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BACKSLASH_C_LIBRARY_DISABLED        = 185
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_PATTERN_TOO_COMPLICATED             = 186
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_LOOKBEHIND_TOO_LONG                 = 187
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_PATTERN_STRING_TOO_LONG             = 188
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_BAD_CODE                   = 189
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_BAD_CODE_IN_SKIP           = 190
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_NO_SURROGATES_IN_UTF16              = 191
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BAD_LITERAL_OPTIONS                 = 192
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_SUPPORTED_ONLY_IN_UNICODE           = 193
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INVALID_HYPHEN_IN_OPTIONS           = 194
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_ALPHA_ASSERTION_UNKNOWN             = 195
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_SCRIPT_RUN_NOT_AVAILABLE            = 196
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_TOO_MANY_CAPTURES                   = 197
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CONDITION_ATOMIC_ASSERTION_EXPECTED = 198
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BACKSLASH_K_IN_LOOKAROUND           = 199

    ! "Expected" matching error codes: no match and partial match.
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_NOMATCH = -1
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_PARTIAL = -2

    ! Miscellaneous error codes for pcre2[_dfa]_match().
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADDATA           = -29
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MIXEDTABLES       = -30 ! name was changed
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADMAGIC          = -31
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADMODE           = -32
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADOFFSET         = -33
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADOPTION         = -34
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADREPLACEMENT    = -35
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADUTFOFFSET      = -36
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CALLOUT           = -37 ! never used by PCRE2 itself
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_DFA_BADRESTART    = -38
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_DFA_RECURSE       = -39
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_DFA_UCOND         = -40
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_DFA_UFUNC         = -41
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_DFA_UITEM         = -42
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_DFA_WSSIZE        = -43
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL          = -44
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_JIT_BADOPTION     = -45
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_JIT_STACKLIMIT    = -46
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_MATCHLIMIT        = -47
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_NOMEMORY          = -48
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_NOSUBSTRING       = -49
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_NOUNIQUESUBSTRING = -50
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_NULL              = -51
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_RECURSELOOP       = -52
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_DEPTHLIMIT        = -53
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_RECURSIONLIMIT    = -53 ! obsolete synonym
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNAVAILABLE       = -54
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_UNSET             = -55
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADOFFSETLIMIT    = -56
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADREPESCAPE      = -57
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_REPMISSINGBRACE   = -58
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADSUBSTITUTION   = -59
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADSUBSPATTERN    = -60
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_TOOMANYREPLACE    = -61
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_BADSERIALIZEDDATA = -62
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_HEAPLIMIT         = -63
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_CONVERT_SYNTAX    = -64
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_INTERNAL_DUPMATCH = -65
    integer(kind=c_int), parameter, public :: PCRE2_ERROR_DFA_UINVALID_UTF  = -66

    ! Request types for pcre2_pattern_info()
    integer(kind=c_int), parameter, public :: PCRE2_INFO_ALLOPTIONS     = 0
    integer(kind=c_int), parameter, public :: PCRE2_INFO_ARGOPTIONS     = 1
    integer(kind=c_int), parameter, public :: PCRE2_INFO_BACKREFMAX     = 2
    integer(kind=c_int), parameter, public :: PCRE2_INFO_BSR            = 3
    integer(kind=c_int), parameter, public :: PCRE2_INFO_CAPTURECOUNT   = 4
    integer(kind=c_int), parameter, public :: PCRE2_INFO_FIRSTCODEUNIT  = 5
    integer(kind=c_int), parameter, public :: PCRE2_INFO_FIRSTCODETYPE  = 6
    integer(kind=c_int), parameter, public :: PCRE2_INFO_FIRSTBITMAP    = 7
    integer(kind=c_int), parameter, public :: PCRE2_INFO_HASCRORLF      = 8
    integer(kind=c_int), parameter, public :: PCRE2_INFO_JCHANGED       = 9
    integer(kind=c_int), parameter, public :: PCRE2_INFO_JITSIZE        = 10
    integer(kind=c_int), parameter, public :: PCRE2_INFO_LASTCODEUNIT   = 11
    integer(kind=c_int), parameter, public :: PCRE2_INFO_LASTCODETYPE   = 12
    integer(kind=c_int), parameter, public :: PCRE2_INFO_MATCHEMPTY     = 13
    integer(kind=c_int), parameter, public :: PCRE2_INFO_MATCHLIMIT     = 14
    integer(kind=c_int), parameter, public :: PCRE2_INFO_MAXLOOKBEHIND  = 15
    integer(kind=c_int), parameter, public :: PCRE2_INFO_MINLENGTH      = 16
    integer(kind=c_int), parameter, public :: PCRE2_INFO_NAMECOUNT      = 17
    integer(kind=c_int), parameter, public :: PCRE2_INFO_NAMEENTRYSIZE  = 18
    integer(kind=c_int), parameter, public :: PCRE2_INFO_NAMETABLE      = 19
    integer(kind=c_int), parameter, public :: PCRE2_INFO_NEWLINE        = 20
    integer(kind=c_int), parameter, public :: PCRE2_INFO_DEPTHLIMIT     = 21
    integer(kind=c_int), parameter, public :: PCRE2_INFO_RECURSIONLIMIT = 21 ! obsolete synonym
    integer(kind=c_int), parameter, public :: PCRE2_INFO_SIZE           = 22
    integer(kind=c_int), parameter, public :: PCRE2_INFO_HASBACKSLASHC  = 23
    integer(kind=c_int), parameter, public :: PCRE2_INFO_FRAMESIZE      = 24
    integer(kind=c_int), parameter, public :: PCRE2_INFO_HEAPLIMIT      = 25
    integer(kind=c_int), parameter, public :: PCRE2_INFO_EXTRAOPTIONS   = 26

    ! Request types for pcre2_config().
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_BSR               = 0
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_JIT               = 1
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_JITTARGET         = 2
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_LINKSIZE          = 3
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_MATCHLIMIT        = 4
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_NEWLINE           = 5
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_PARENSLIMIT       = 6
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_DEPTHLIMIT        = 7
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_RECURSIONLIMIT    = 7 ! obsolete synonym
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_STACKRECURSE      = 8 ! obsolete
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_UNICODE           = 9
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_UNICODE_VERSION   = 10
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_VERSION           = 11
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_HEAPLIMIT         = 12
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_NEVER_BACKSLASH_C = 13
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_COMPILED_WIDTHS   = 14
    integer(kind=c_int), parameter, public :: PCRE2_CONFIG_TABLES_LENGTH     = 15

    public :: pcre2_code_free
    public :: pcre2_compile
    public :: pcre2_compile_context_free
    public :: pcre2_get_error_message
    public :: pcre2_get_ovector_count
    public :: pcre2_get_ovector_pointer
    public :: pcre2_match
    public :: pcre2_match_data_create
    public :: pcre2_match_data_free
    public :: pcre2_substring_copy_byname
    public :: pcre2_substring_copy_byname_
    public :: pcre2_substring_copy_bynumber
    public :: pcre2_substring_copy_bynumber_
    public :: pcre2_substring_free
    public :: pcre2_substring_get_byname
    public :: pcre2_substring_get_byname_
    public :: pcre2_substring_get_bynumber
    public :: pcre2_substring_get_bynumber_
    public :: pcre2_substring_number_from_name

    private :: c_f_str_ptr

    interface
        ! pcre2_code *pcre2_compile(PCRE2_SPTR pattern, PCRE2_SIZE length, uint32_t options, int *errorcode, PCRE2_SIZE *erroroffset, pcre2_compile_context *ccontext)
        function pcre2_compile(pattern, length, options, errorcode, erroroffset, ccontext) bind(c, name='pcre2_compile_8')
            import :: c_uint32_t, c_int, c_ptr, pcre2_size, pcre2_sptr
            implicit none
            character(kind=pcre2_sptr), intent(in)        :: pattern
            integer(kind=pcre2_size),   intent(in), value :: length
            integer(kind=c_uint32_t),   intent(in), value :: options
            integer(kind=c_int),        intent(out)       :: errorcode
            integer(kind=pcre2_size),   intent(out)       :: erroroffset
            type(c_ptr),                intent(in), value :: ccontext
            type(c_ptr)                                   :: pcre2_compile
        end function pcre2_compile

        ! int pcre2_get_error_message(int errorcode, PCRE2_UCHAR *buffer, PCRE2_SIZE bufflen)
        function pcre2_get_error_message(errorcode, buffer, bufflen) bind(c, name='pcre2_get_error_message_8')
            import :: c_int, pcre2_size, pcre2_uchar
            implicit none
            integer(kind=c_int),         intent(in), value :: errorcode
            character(kind=pcre2_uchar), intent(inout)     :: buffer
            integer(kind=pcre2_size),    intent(in), value :: bufflen
            integer(kind=c_int)                            :: pcre2_get_error_message
        end function pcre2_get_error_message

        ! uint32_t pcre2_get_ovector_count(pcre2_match_data *match_data)
        function pcre2_get_ovector_count(match_data) bind(c, name='pcre2_get_ovector_count_8')
            import :: c_ptr, c_uint32_t
            implicit none
            type(c_ptr), intent(in), value :: match_data
            integer(kind=c_uint32_t)       :: pcre2_get_ovector_count
        end function pcre2_get_ovector_count

        ! PCRE2_SIZE *pcre2_get_ovector_pointer(pcre2_match_data *match_data)
        function pcre2_get_ovector_pointer(match_data) bind(c, name='pcre2_get_ovector_pointer_8')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: match_data
            type(c_ptr)                    :: pcre2_get_ovector_pointer
        end function pcre2_get_ovector_pointer

        ! int pcre2_match(const pcre2_code *code, PCRE2_SPTR subject, PCRE2_SIZE length, PCRE2_SIZE startoffset, uint32_t options, pcre2_match_data *match_data, pcre2_match_context *mcontext)
        function pcre2_match(code, subject, length, startoffset, options, match_data, mcontext) bind(c, name='pcre2_match_8')
            import :: c_int, c_ptr, c_uint32_t, pcre2_size, pcre2_sptr
            implicit none
            type(c_ptr),                intent(in), value :: code
            character(kind=pcre2_sptr), intent(in)        :: subject
            integer(kind=pcre2_size),   intent(in), value :: length
            integer(kind=pcre2_size),   intent(in), value :: startoffset
            integer(kind=c_uint32_t),   intent(in), value :: options
            type(c_ptr),                intent(in), value :: match_data
            type(c_ptr),                intent(in), value :: mcontext
            integer(kind=c_int)                           :: pcre2_match
        end function pcre2_match

        ! pcre2_match_data *pcre2_match_data_create(uint32_t ovecsize, pcre2_general_context *gcontext)
        function pcre2_match_data_create(ovecsize, gcontext) bind(c, name='pcre2_match_data_create_8')
            import :: c_uint32_t, c_ptr
            implicit none
            integer(kind=c_uint32_t), intent(in), value :: ovecsize
            type(c_ptr),              intent(in), value :: gcontext
            type(c_ptr)                                 :: pcre2_match_data_create
        end function pcre2_match_data_create

        ! int pcre2_substring_copy_byname(pcre2_match_data *match_data, PCRE2_SPTR name, PCRE2_UCHAR *buffer, PCRE2_SIZE *bufflen)
        function pcre2_substring_copy_byname_(match_data, name, buffer, bufflen) bind(c, name='pcre2_substring_copy_byname_8')
            import :: c_int, c_ptr, pcre2_size, pcre2_sptr, pcre2_uchar
            implicit none
            type(c_ptr),                 intent(in), value :: match_data
            character(kind=pcre2_sptr),  intent(in)        :: name
            character(kind=pcre2_uchar), intent(inout)     :: buffer
            integer(kind=pcre2_size),    intent(inout)     :: bufflen
            integer(kind=c_int)                            :: pcre2_substring_copy_byname_
        end function pcre2_substring_copy_byname_

        ! int pcre2_substring_copy_bynumber(pcre2_match_data *match_data, uint32_t number, PCRE2_UCHAR *buffer, PCRE2_SIZE *bufflen)
        function pcre2_substring_copy_bynumber_(match_data, number, buffer, bufflen) bind(c, name='pcre2_substring_copy_bynumber_8')
            import :: c_int, c_ptr, c_uint32_t, pcre2_size, pcre2_uchar
            implicit none
            type(c_ptr),                 intent(in), value :: match_data
            integer(kind=c_uint32_t),    intent(in), value :: number
            character(kind=pcre2_uchar), intent(inout)     :: buffer
            integer(kind=pcre2_size),    intent(inout)     :: bufflen
            integer(kind=c_int)                            :: pcre2_substring_copy_bynumber_
        end function pcre2_substring_copy_bynumber_

        ! int pcre2_substring_get_byname(pcre2_match_data *match_data, PCRE2_SPTR name, PCRE2_UCHAR **bufferptr, PCRE2_SIZE *bufflen)
        function pcre2_substring_get_byname_(match_data, name, bufferptr, bufflen) bind(c, name='pcre2_substring_get_byname_8')
            import :: c_int, c_ptr, pcre2_size, pcre2_sptr
            implicit none
            type(c_ptr),                 intent(in), value :: match_data
            character(kind=pcre2_sptr),  intent(in)        :: name
            type(c_ptr),                 intent(out)       :: bufferptr
            integer(kind=pcre2_size),    intent(out)       :: bufflen
            integer(kind=c_int)                            :: pcre2_substring_get_byname_
        end function pcre2_substring_get_byname_

        ! int pcre2_substring_get_bynumber(pcre2_match_data *match_data, uint32_t number, PCRE2_UCHAR **bufferptr, PCRE2_SIZE *bufflen)
        function pcre2_substring_get_bynumber_(match_data, number, bufferptr, bufflen) &
                bind(c, name='pcre2_substring_get_bynumber_8')
            import :: c_int, c_ptr, c_uint32_t, pcre2_size
            implicit none
            type(c_ptr),                 intent(in), value :: match_data
            integer(kind=c_uint32_t),    intent(in), value :: number
            type(c_ptr),                 intent(out)       :: bufferptr
            integer(kind=pcre2_size),    intent(out)       :: bufflen
            integer(kind=c_int)                            :: pcre2_substring_get_bynumber_
        end function pcre2_substring_get_bynumber_

        ! int pcre2_substring_number_from_name(const pcre2_code *code, PCRE2_SPTR name)
        function pcre2_substring_number_from_name(code, name) bind(c, name='pcre2_substring_number_from_name_8')
            import :: c_int, c_ptr, pcre2_sptr
            implicit none
            type(c_ptr),                intent(in), value :: code
            character(kind=pcre2_sptr), intent(in)        :: name
            integer(kind=c_int)                           :: pcre2_substring_number_from_name
        end function pcre2_substring_number_from_name

        ! void pcre2_code_free(pcre2_code *code)
        subroutine pcre2_code_free(code) bind(c, name='pcre2_code_free_8')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: code
        end subroutine pcre2_code_free

        ! void pcre2_compile_context_free(pcre2_compile_context *ccontext)
        subroutine pcre2_compile_context_free(ccontext) bind(c, name='pcre2_compile_context_free_8')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ccontext
        end subroutine pcre2_compile_context_free

        ! void pcre2_match_data_free(pcre2_match_data *match_data)
        subroutine pcre2_match_data_free(match_data) bind(c, name='pcre2_match_data_free_8')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: match_data
        end subroutine pcre2_match_data_free

        ! void pcre2_substring_free(PCRE2_UCHAR *buffer)
        subroutine pcre2_substring_free(buffer) bind(c, name='pcre2_substring_free_8')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: buffer
        end subroutine pcre2_substring_free
    end interface

contains
    function pcre2_substring_copy_byname(match_data, name, buffer, buff_len) result(rc)
        type(c_ptr),              intent(in)              :: match_data
        character(len=*),         intent(in)              :: name
        character(len=*),         intent(inout)           :: buffer
        integer(kind=pcre2_size), intent(inout), optional :: buff_len
        integer                                           :: rc

        integer(kind=pcre2_size) :: sz

        if (present(buff_len)) then
            sz = buff_len
        else
            sz = len(buffer, kind=pcre2_size)
        end if

        rc = pcre2_substring_copy_byname_(match_data, name // c_null_char, buffer, sz)
        if (present(buff_len)) buff_len = sz
    end function pcre2_substring_copy_byname

    function pcre2_substring_copy_bynumber(match_data, number, buffer, buff_len) result(rc)
        type(c_ptr),              intent(in)              :: match_data
        integer,                  intent(in)              :: number
        character(len=*),         intent(inout)           :: buffer
        integer(kind=pcre2_size), intent(inout), optional :: buff_len
        integer                                           :: rc

        integer(kind=pcre2_size) :: sz

        if (present(buff_len)) then
            sz = buff_len
        else
            sz = len(buffer, kind=pcre2_size)
        end if

        rc = pcre2_substring_copy_bynumber_(match_data, number, buffer, sz)
        if (present(buff_len)) buff_len = sz
    end function pcre2_substring_copy_bynumber

    function pcre2_substring_get_byname(match_data, name, buffer, buff_len) result(rc)
        type(c_ptr),                   intent(in)  :: match_data
        character(len=*),              intent(in)  :: name
        character(len=:), allocatable, intent(out) :: buffer
        integer(kind=pcre2_size),      intent(out) :: buff_len
        integer                                    :: rc

        type(c_ptr) :: ptr

        ptr = c_null_ptr
        rc = pcre2_substring_get_byname_(match_data, name // c_null_char, ptr, buff_len)
        call c_f_str_ptr(ptr, buffer)
        if (c_associated(ptr)) call pcre2_substring_free(ptr)
    end function pcre2_substring_get_byname

    function pcre2_substring_get_bynumber(match_data, number, buffer, buff_len) result(rc)
        type(c_ptr),                   intent(in)  :: match_data
        integer,                       intent(in)  :: number
        character(len=:), allocatable, intent(out) :: buffer
        integer(kind=pcre2_size),      intent(out) :: buff_len
        integer                                    :: rc

        type(c_ptr) :: ptr

        ptr = c_null_ptr
        rc = pcre2_substring_get_bynumber_(match_data, number, ptr, buff_len)
        call c_f_str_ptr(ptr, buffer)
        if (c_associated(ptr)) call pcre2_substring_free(ptr)
    end function pcre2_substring_get_bynumber

    subroutine c_f_str_ptr(c_str, f_str)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
        type(c_ptr),                   intent(in)  :: c_str
        character(len=:), allocatable, intent(out) :: f_str

        character(kind=c_char), pointer :: ptrs(:)
        integer(kind=c_size_t)          :: i, sz

        interface
            ! size_t strlen(const char *str)
            function c_strlen(str) bind(c, name='strlen')
                import :: c_ptr, c_size_t
                implicit none
                type(c_ptr), intent(in), value :: str
                integer(kind=c_size_t)         :: c_strlen
            end function c_strlen
        end interface

        copy_block: block
            if (.not. c_associated(c_str)) exit copy_block
            sz = c_strlen(c_str)
            if (sz < 0) exit copy_block
            call c_f_pointer(c_str, ptrs, [ sz ])
            allocate (character(len=sz) :: f_str)

            do i = 1, sz
                f_str(i:i) = ptrs(i)
            end do

            return
        end block copy_block

        if (.not. allocated(f_str)) f_str = ''
    end subroutine c_f_str_ptr
end module pcre2
