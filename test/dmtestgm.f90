! dmtestgm.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestgm
    !! GraphicsMagick test program.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestgm'
    integer,          parameter :: NTESTS    = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=*), parameter :: IMAGE_PATH   = 'testgm.png'
        integer,          parameter :: IMAGE_WIDTH  = 640
        integer,          parameter :: IMAGE_HEIGHT = 480

        character(len=:), allocatable      :: format, image1, image2, mime
        character(len=CRYPTO_MD5_HASH_LEN) :: hash1, hash2
        integer                            :: w, h, rc
        integer(kind=i8)                   :: nbytes
        type(gm_text_box_type)             :: text_box

        stat = TEST_PASSED
        if (dm_test_skip('DM_GM_SKIP')) return

        stat = TEST_FAILED
        if (dm_file_exists(IMAGE_PATH)) call dm_file_delete(IMAGE_PATH)

        print *, 'Creating image file ' // IMAGE_PATH // ' ...'
        rc = dm_gm_create(IMAGE_PATH, width=IMAGE_WIDTH, height=IMAGE_HEIGHT, color=GM_COLOR_DARK_VIOLET)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        if (.not. dm_file_exists(IMAGE_PATH)) return

        call dm_file_read(IMAGE_PATH, image1, size=nbytes)
        hash1 = dm_crypto_md5(image1)

        gm_block: block
            print *, 'Reading dimensions ...'
            rc = dm_gm_get_dimensions(IMAGE_PATH, w, h)
            if (dm_is_error(rc)) exit gm_block

            print *, 'Reading file format ...'
            rc = dm_gm_get_file_format(IMAGE_PATH, format)
            if (dm_is_error(rc)) exit gm_block

            print *, 'Reading MIME type ...'
            rc = dm_gm_get_mime(IMAGE_PATH, mime)
            if (dm_is_error(rc)) exit gm_block
        end block gm_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print '(" File size.: ", i0, " bytes")', nbytes
        print '(" MD5 hash..: ", a)',            hash1
        print '(" Dimensions: ", i0, "x", i0)',  w, h
        print '(" Format....: ", a)',            format
        print '(" MIME type.: ", a)',            mime

        if (w /= IMAGE_WIDTH .or. h /= IMAGE_HEIGHT) return
        if (format /= 'PNG')                         return
        if (mime /= MIME_PNG)                        return

        print *, 'Adding text box to image ...'
        text_box%background = GM_COLOR_DARK_VIOLET
        text_box%font       = ''

        rc = dm_gm_add_text_box(IMAGE_PATH, dm_time_now(), text_box); if (dm_is_error(rc)) return

        call dm_file_read(IMAGE_PATH, image2, size=nbytes)
        hash2 = dm_crypto_md5(image2)

        print '(" File size.: ", i0, " bytes")', nbytes
        print '(" MD5 hash..: ", a)',            hash2

        print *, 'Comparing image files ...'
        if (hash1 == hash2) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        stat = TEST_FAILED

        print *, 'Validating font names ...'

        if (dm_gm_font_is_valid(' '))              return
        if (dm_gm_font_is_valid('Helvetica Bold')) return

        if (.not. dm_gm_font_is_valid('Helvetica'))      return
        if (.not. dm_gm_font_is_valid('Helvetica-Bold')) return

        stat = TEST_PASSED
    end function test02
end program dmtestgm
