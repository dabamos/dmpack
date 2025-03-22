! dmtestve.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestve
    !! Test program for VE.Direct protocol handling.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestve'
    integer,          parameter :: NTESTS    = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=*), parameter :: BYTES = &
            CR_LF // 'PID'      // ASCII_TAB // '0x203' // &
            CR_LF // 'V'        // ASCII_TAB // '26201' // &
            CR_LF // 'I'        // ASCII_TAB // '0'     // &
            CR_LF // 'P'        // ASCII_TAB // '0'     // &
            CR_LF // 'CE'       // ASCII_TAB // '0'     // &
            CR_LF // 'SOC'      // ASCII_TAB // '1000'  // &
            CR_LF // 'TTG'      // ASCII_TAB // '-1'    // &
            CR_LF // 'Alarm'    // ASCII_TAB // 'OFF'   // &
            CR_LF // 'Relay'    // ASCII_TAB // 'OFF'   // &
            CR_LF // 'AR'       // ASCII_TAB // '0'     // &
            CR_LF // 'BMV'      // ASCII_TAB // '700'   // &
            CR_LF // 'FW'       // ASCII_TAB // '0307'  // &
            CR_LF // 'Checksum' // ASCII_TAB // char(216)

        character           :: byte
        integer             :: i
        logical             :: eor, finished, valid
        type(ve_frame_type) :: frame

        stat = TEST_FAILED

        print *, 'Reading VE.Direct protocol block into frames ...'

        do i = 1, len(BYTES)
            byte = BYTES(i:i)
            call dm_ve_frame_next(frame, byte, eor, finished, valid)

            if (finished) then
                if (valid) then
                    stat = TEST_PASSED
                    print '(" Record is valid")'
                else
                    print '(" Record is invalid")'
                end if

                exit
            end if

            if (eor) print '(" Label: ", a, " Value: ", a)', frame%label, trim(frame%value)
        end do

        call dm_ve_frame_reset(frame)

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=*), parameter :: BYTES = &
            CR_LF // 'PID'      // ASCII_TAB // '0xA068'      // &
            CR_LF // 'FW'       // ASCII_TAB // '164'         // &
            CR_LF // 'SER#'     // ASCII_TAB // 'HQ2322J6VYR' // &
            CR_LF // 'V'        // ASCII_TAB // '54290'       // &
            CR_LF // 'I'        // ASCII_TAB // '0'           // &
            CR_LF // 'VPV'      // ASCII_TAB // '135030'      // &
            CR_LF // 'PPV'      // ASCII_TAB // '4'           // &
            CR_LF // 'CS'       // ASCII_TAB // '5'           // &
            CR_LF // 'MPPT'     // ASCII_TAB // '1'           // &
            CR_LF // 'OR'       // ASCII_TAB // '0x00000000'  // &
            CR_LF // 'ERR'      // ASCII_TAB // '2'           // &
            CR_LF // 'LOAD'     // ASCII_TAB // 'ON'          // &
            CR_LF // 'Relay'    // ASCII_TAB // 'OFF'         // &
            CR_LF // 'H19'      // ASCII_TAB // '607'         // &
            CR_LF // 'H20'      // ASCII_TAB // '5'           // &
            CR_LF // 'H21'      // ASCII_TAB // '37'          // &
            CR_LF // 'H22'      // ASCII_TAB // '10'          // &
            CR_LF // 'H23'      // ASCII_TAB // '29'          // &
            CR_LF // 'HSDS'     // ASCII_TAB // '25'          // &
            CR_LF // 'Checksum' // ASCII_TAB // char(229)
        integer, parameter :: NBLOCKS = 5

        character           :: byte
        integer             :: code, i, j
        logical             :: eor, finished, valid
        type(ve_frame_type) :: frame
        type(response_type) :: response

        stat = TEST_FAILED
        code = 0

        print *, 'Converting VE.Direct frames to responses ...'

        do i = 1, NBLOCKS
            do j = 1, len(BYTES)
                byte = BYTES(j:j)

                call dm_ve_frame_next(frame, byte, eor, finished, valid)

                if (finished) then
                    if (valid) then
                        stat = TEST_PASSED
                        print '(" Record is valid")'
                    else
                        print '(" Record is invalid")'
                    end if

                    call dm_ve_frame_reset(frame)
                end if

                if (i == 1 .and. eor) then
                    ! Ignore serial number field.
                    if (frame%label == 'SER#') cycle
                    if (frame%label == 'ERR') code = dm_atoi(frame%value)

                    call dm_ve_frame_read(frame, response)
                    print '(" Name: ", a, " Value: ", f8.1)', response%name, response%value
                end if
            end do
        end do

        print '(" Associated error message: ", a, " (", i0, ")")', dm_ve_error_message(code), code
        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        character(len=VE_PRODUCT_NAME_LEN) :: name
        integer                            :: rc, type

        stat = TEST_FAILED

        print *, 'Validating product name ...'
        rc = dm_ve_product_name(int(z'0203'), name)
        if (dm_is_error(rc)) return
        if (name /= 'BMV-700') return

        print *, 'Validating field type ...'
        type = dm_ve_field_type('ALARM')
        if (type /= VE_FIELD_ALARM) return

        stat = TEST_PASSED
    end function test03
end program dmtestve
