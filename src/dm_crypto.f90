! Author:  Philipp Engel
! Licence: ISC
module dm_crypto
    !! Cryptographic hash functions from OpenSSL:
    !!
    !! * MD4
    !! * MD5
    !! * RIPEMD-160
    !! * SHA-1
    !! * SHA-224
    !! * SHA-256
    !! * SHA-384
    !! * SHA-512
    !!
    !! The functions return hexadecimal character strings in upper-case format.
    !! Link this module against `-lcrypto`.
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    character(len=*), parameter :: HASH_FMT = '(*(Z2.2))'

    integer, parameter, public :: CRYPTO_MD4_DIGEST_LEN       = 16
    integer, parameter, public :: CRYPTO_MD5_DIGEST_LEN       = 16
    integer, parameter, public :: CRYPTO_RIPEMD160_DIGEST_LEN = 20
    integer, parameter, public :: CRYPTO_SHA1_DIGEST_LEN      = 20
    integer, parameter, public :: CRYPTO_SHA224_DIGEST_LEN    = 28
    integer, parameter, public :: CRYPTO_SHA256_DIGEST_LEN    = 32
    integer, parameter, public :: CRYPTO_SHA384_DIGEST_LEN    = 48
    integer, parameter, public :: CRYPTO_SHA512_DIGEST_LEN    = 64

    integer, parameter, public :: CRYPTO_MD4_HASH_LEN         = 2 * CRYPTO_MD4_DIGEST_LEN
    integer, parameter, public :: CRYPTO_MD5_HASH_LEN         = 2 * CRYPTO_MD5_DIGEST_LEN
    integer, parameter, public :: CRYPTO_RIPEMD160_HASH_LEN   = 2 * CRYPTO_RIPEMD160_DIGEST_LEN
    integer, parameter, public :: CRYPTO_SHA1_HASH_LEN        = 2 * CRYPTO_SHA1_DIGEST_LEN
    integer, parameter, public :: CRYPTO_SHA224_HASH_LEN      = 2 * CRYPTO_SHA224_DIGEST_LEN
    integer, parameter, public :: CRYPTO_SHA256_HASH_LEN      = 2 * CRYPTO_SHA256_DIGEST_LEN
    integer, parameter, public :: CRYPTO_SHA384_HASH_LEN      = 2 * CRYPTO_SHA384_DIGEST_LEN
    integer, parameter, public :: CRYPTO_SHA512_HASH_LEN      = 2 * CRYPTO_SHA512_DIGEST_LEN

    public :: dm_crypto_md4
    public :: dm_crypto_md5
    public :: dm_crypto_ripemd160
    public :: dm_crypto_sha1
    public :: dm_crypto_sha224
    public :: dm_crypto_sha256
    public :: dm_crypto_sha384
    public :: dm_crypto_sha512
contains
    function dm_crypto_md4(str) result(hash)
        !! MD4 cryptographic hash function with a 128 bit output.
        character(len=*), intent(in)       :: str  !! Input string.
        character(len=CRYPTO_MD4_HASH_LEN) :: hash !! Output hash.

        character(len=CRYPTO_MD4_DIGEST_LEN) :: raw
        integer                              :: i
        type(c_ptr)                          :: ptr

        interface
            ! unsigned char *MD4(const unsigned char *d, size_t n, unsigned char *md)
            function c_md4(d, n, md) bind(c, name='MD4')
                import :: c_char, c_ptr, c_size_t
                implicit none
                character(kind=c_char), intent(in)        :: d
                integer(kind=c_size_t), intent(in), value :: n
                character(kind=c_char), intent(in)        :: md
                type(c_ptr)                               :: c_md4
            end function c_md4
        end interface

        hash = ' '
        ptr  = c_md4(str, len(str, kind=c_size_t), raw)
        if (.not. c_associated(ptr)) return
        write (hash, HASH_FMT) (iachar(raw(i:i)), i = 1, len(raw))
    end function dm_crypto_md4

    function dm_crypto_md5(str) result(hash)
        !! MD5 cryptographic hash function with a 128 bit output.
        character(len=*), intent(in)       :: str  !! Input string.
        character(len=CRYPTO_MD5_HASH_LEN) :: hash !! Output hash.

        character(len=CRYPTO_MD5_DIGEST_LEN) :: raw
        integer                              :: i
        type(c_ptr)                          :: ptr

        interface
            ! unsigned char *MD5(const unsigned char *d, size_t n, unsigned char *md)
            function c_md5(d, n, md) bind(c, name='MD5')
                import :: c_char, c_ptr, c_size_t
                implicit none
                character(kind=c_char), intent(in)        :: d
                integer(kind=c_size_t), intent(in), value :: n
                character(kind=c_char), intent(in)        :: md
                type(c_ptr)                               :: c_md5
            end function c_md5
        end interface

        hash = ' '
        ptr  = c_md5(str, len(str, kind=c_size_t), raw)
        if (.not. c_associated(ptr)) return
        write (hash, HASH_FMT) (iachar(raw(i:i)), i = 1, len(raw))
    end function dm_crypto_md5

    function dm_crypto_ripemd160(str) result(hash)
        !! RIPEMD-160 cryptographic hash function with a 160 bit output.
        character(len=*), intent(in)             :: str  !! Input string.
        character(len=CRYPTO_RIPEMD160_HASH_LEN) :: hash !! Output hash.

        character(len=CRYPTO_RIPEMD160_DIGEST_LEN) :: raw
        integer                                    :: i
        type(c_ptr)                                :: ptr

        interface
            ! unsigned char *RIPEMD160(const unsigned char *d, size_t n, unsigned char *md)
            function c_ripemd160(d, n, md) bind(c, name='RIPEMD160')
                import :: c_char, c_ptr, c_size_t
                implicit none
                character(kind=c_char), intent(in)        :: d
                integer(kind=c_size_t), intent(in), value :: n
                character(kind=c_char), intent(in)        :: md
                type(c_ptr)                               :: c_ripemd160
            end function c_ripemd160
        end interface

        hash = ' '
        ptr  = c_ripemd160(str, len(str, kind=c_size_t), raw)
        if (.not. c_associated(ptr)) return
        write (hash, HASH_FMT) (iachar(raw(i:i)), i = 1, len(raw))
    end function dm_crypto_ripemd160

    function dm_crypto_sha1(str) result(hash)
        !! SHA-1 cryptographic hash function with a 160 bit output.
        character(len=*), intent(in)        :: str  !! Input string.
        character(len=CRYPTO_SHA1_HASH_LEN) :: hash !! Output hash.

        character(len=CRYPTO_SHA1_DIGEST_LEN) :: raw
        integer                               :: i
        type(c_ptr)                           :: ptr

        interface
            ! unsigned char *SHA1(const unsigned char *d, size_t n, unsigned char *md)
            function c_sha1(d, n, md) bind(c, name='SHA1')
                import :: c_char, c_ptr, c_size_t
                implicit none
                character(kind=c_char), intent(in)        :: d
                integer(kind=c_size_t), intent(in), value :: n
                character(kind=c_char), intent(in)        :: md
                type(c_ptr)                               :: c_sha1
            end function c_sha1
        end interface

        hash = ' '
        ptr  = c_sha1(str, len(str, kind=c_size_t), raw)
        if (.not. c_associated(ptr)) return
        write (hash, HASH_FMT) (iachar(raw(i:i)), i = 1, len(raw))
    end function dm_crypto_sha1

    function dm_crypto_sha224(str) result(hash)
        !! SHA-2 cryptographic hash function with a 224 bit output.
        character(len=*), intent(in)          :: str  !! Input string.
        character(len=CRYPTO_SHA224_HASH_LEN) :: hash !! Output hash.

        character(len=CRYPTO_SHA224_DIGEST_LEN) :: raw
        integer                                 :: i
        type(c_ptr)                             :: ptr

        interface
            ! unsigned char *SHA224(const unsigned char *d, size_t n, unsigned char *md)
            function c_sha224(d, n, md) bind(c, name='SHA224')
                import :: c_char, c_ptr, c_size_t
                implicit none
                character(kind=c_char), intent(in)        :: d
                integer(kind=c_size_t), intent(in), value :: n
                character(kind=c_char), intent(in)        :: md
                type(c_ptr)                               :: c_sha224
            end function c_sha224
        end interface

        hash = ' '
        ptr  = c_sha224(str, len(str, kind=c_size_t), raw)
        if (.not. c_associated(ptr)) return
        write (hash, HASH_FMT) (iachar(raw(i:i)), i = 1, len(raw))
    end function dm_crypto_sha224

    function dm_crypto_sha256(str) result(hash)
        !! SHA-2 cryptographic hash function with a 256 bit output.
        character(len=*), intent(in)          :: str  !! Input string.
        character(len=CRYPTO_SHA256_HASH_LEN) :: hash !! Output hash.

        character(len=CRYPTO_SHA256_DIGEST_LEN) :: raw
        integer                                 :: i
        type(c_ptr)                             :: ptr

        interface
            ! unsigned char *SHA256(const unsigned char *d, size_t n, unsigned char *md)
            function c_sha256(d, n, md) bind(c, name='SHA256')
                import :: c_char, c_ptr, c_size_t
                implicit none
                character(kind=c_char), intent(in)        :: d
                integer(kind=c_size_t), intent(in), value :: n
                character(kind=c_char), intent(in)        :: md
                type(c_ptr)                               :: c_sha256
            end function c_sha256
        end interface

        hash = ' '
        ptr  = c_sha256(str, len(str, kind=c_size_t), raw)
        if (.not. c_associated(ptr)) return
        write (hash, HASH_FMT) (iachar(raw(i:i)), i = 1, len(raw))
    end function dm_crypto_sha256

    function dm_crypto_sha384(str) result(hash)
        !! SHA-2 cryptographic hash function with a 384 bit output.
        character(len=*), intent(in)          :: str  !! Input string.
        character(len=CRYPTO_SHA384_HASH_LEN) :: hash !! Output hash.

        character(len=CRYPTO_SHA384_DIGEST_LEN) :: raw
        integer                                 :: i
        type(c_ptr)                             :: ptr

        interface
            ! unsigned char *SHA384(const unsigned char *d, size_t n, unsigned char *md)
            function c_sha384(d, n, md) bind(c, name='SHA384')
                import :: c_char, c_ptr, c_size_t
                implicit none
                character(kind=c_char), intent(in)        :: d
                integer(kind=c_size_t), intent(in), value :: n
                character(kind=c_char), intent(in)        :: md
                type(c_ptr)                               :: c_sha384
            end function c_sha384
        end interface

        hash = ' '
        ptr  = c_sha384(str, len(str, kind=c_size_t), raw)
        if (.not. c_associated(ptr)) return
        write (hash, HASH_FMT) (iachar(raw(i:i)), i = 1, len(raw))
    end function dm_crypto_sha384

    function dm_crypto_sha512(str) result(hash)
        !! SHA-2 cryptographic hash function with a 512 bit output.
        character(len=*), intent(in)          :: str  !! Input string.
        character(len=CRYPTO_SHA512_HASH_LEN) :: hash !! Output hash.

        character(len=CRYPTO_SHA512_DIGEST_LEN) :: raw
        integer                                 :: i
        type(c_ptr)                             :: ptr

        interface
            ! unsigned char *SHA512(const unsigned char *d, size_t n, unsigned char *md)
            function c_sha512(d, n, md) bind(c, name='SHA512')
                import :: c_char, c_ptr, c_size_t
                implicit none
                character(kind=c_char), intent(in)        :: d
                integer(kind=c_size_t), intent(in), value :: n
                character(kind=c_char), intent(in)        :: md
                type(c_ptr)                               :: c_sha512
            end function c_sha512
        end interface

        hash = ' '
        ptr  = c_sha512(str, len(str, kind=c_size_t), raw)
        if (.not. c_associated(ptr)) return
        write (hash, HASH_FMT) (iachar(raw(i:i)), i = 1, len(raw))
    end function dm_crypto_sha512
end module dm_crypto
