! dmtestcrypto.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestcrypto
    !! Test program for cryptographic hash functions. Link against `-lcrypto`.
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
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=*), parameter :: A = 'DMPACK'

        stat = TEST_FAILED

        print *, 'Testing cryptographic hash functions ...'

        print *, 'MD4'
        if (dm_crypto_md4(A) /= '2B15614E9FBA67C22A46B54DDB76CE56') return

        print *, 'MD5'
        if (dm_crypto_md5(A) /= '68119F8CFC54D7E9818208B94A6B137B') return

        print *, 'RIPEMD-160'
        if (dm_crypto_ripemd160(A) /= '22BA4998DCA28DAFAF4EC87C75518A7B2A23E5C5') return

        print *, 'SHA-1'
        if (dm_crypto_sha1(A) /= '2E6CF625D9A0785C436F7FF4F8D558ED42A516A5') return

        print *, 'SHA-224'
        if (dm_crypto_sha224(A) /= '441729B007A7DACF616316F08FD816AB8A0F8A8D2D32BF4BFBD060FE') return

        print *, 'SHA-256'
        if (dm_crypto_sha256(A) /= '9A0921FF882824019AA2F3C0D5572DC362C092232BB2BC88FB5F96848F2ADE5A') return

        print *, 'SHA-384'
        if (dm_crypto_sha384(A) /= '8C521A3797CDBF905F9FBB2F9A8DCD71D00AC3550DAF5537D32C14C098F9E5A9631BCAD634210662E780696B4F69E309') return

        print *, 'SHA-512'
        if (dm_crypto_sha512(A) /= '218778843752FCFCA9D5C9FDC2BDC826540DA0ACD0612B641326AE2E605A20D5A532F7CAD1D887A78E6FA7C4E3CB62D75862B5418C5DA69D11EE806798DF59B6') return

        stat = TEST_PASSED
    end function test01
end program dmtestcrypto
