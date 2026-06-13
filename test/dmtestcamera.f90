! dmtestcamera.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestcamera
    !! Test program for data points handling.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestcamera'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        character(FILE_PATH_LEN) :: command
        integer                  :: rc
        type(camera_type)        :: camera

        stat = TEST_FAILED

        camera = camera_type(input  = '/dev/video0', &
                             device = CAMERA_DEVICE_V4L2, &
                             width  = 1280, &
                             height = 720)

        rc = dm_camera_capture(camera, '/tmp/image.jpg', dry=.true., command=command)
        print '(" Command: ", a)', trim(command)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01
end program dmtestcamera
