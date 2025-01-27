! dmtestve.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestve
    !! Test program for VE.Direct protocol handling.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestve'
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
            CR_LF // 'Checksum' // ASCII_TAB // char(int(z'D8'))

        character           :: byte
        integer             :: i
        logical             :: eor, finished, valid
        type(ve_frame_type) :: frame

        stat = TEST_FAILED

        do i = 1, len(BYTES)
            byte = BYTES(i:i)
            call dm_ve_frame_next(frame, byte, eor, finished, valid)

            if (finished) then
                if (valid) then
                    stat = TEST_PASSED
                    print '("record is valid")'
                else
                    print '("record is invalid")'
                end if

                exit
            end if

            if (eor) print '("Label: ", a, " Value: ", a)', frame%label, trim(frame%value)
        end do

        call dm_ve_frame_reset(frame)

        stat = TEST_PASSED
    end function test01
end program dmtestve
