! dmtestcrypto.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestcrypto
    !! Test program for cryptographic hash functions. Link against `-lcrypto`.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestcrypto'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=*), parameter :: A = 'DMPACK'

        stat = TEST_FAILED

        print *, 'Testing cryptographic hash functions ...'

        print *, 'MD4'
        if (dm_crypto_md4(A) /= '2b15614e9fba67c22a46b54ddb76ce56') return

        print *, 'MD5'
        if (dm_crypto_md5(A) /= '68119f8cfc54d7e9818208b94a6b137b') return

        print *, 'RIPEMD-160'
        if (dm_crypto_ripemd160(A) /= '22ba4998dca28dafaf4ec87c75518a7b2a23e5c5') return

        print *, 'SHA-1'
        if (dm_crypto_sha1(A) /= '2e6cf625d9a0785c436f7ff4f8d558ed42a516a5') return

        print *, 'SHA-224'
        if (dm_crypto_sha224(A) /= '441729b007a7dacf616316f08fd816ab8a0f8a8d2d32bf4bfbd060fe') return

        print *, 'SHA-256'
        if (dm_crypto_sha256(A) /= '9a0921ff882824019aa2f3c0d5572dc362c092232bb2bc88fb5f96848f2ade5a') return

        print *, 'SHA-384'
        if (dm_crypto_sha384(A) /= '8c521a3797cdbf905f9fbb2f9a8dcd71d00ac3550daf5537d32c14c098f9e5a9631bcad634210662e780696b4f69e309') return

        print *, 'SHA-512'
        if (dm_crypto_sha512(A) /= '218778843752fcfca9d5c9fdc2bdc826540da0acd0612b641326ae2e605a20d5a532f7cad1d887a78e6fa7c4e3cb62d75862b5418c5da69d11ee806798df59b6') return

        stat = TEST_PASSED
    end function test01
end program dmtestcrypto
