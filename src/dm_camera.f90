! Author:  Philipp Engel
! Licence: ISC
module dm_camera
    !! Module for taking still images from RTSP video streams and USB webcams,
    !! using FFmpeg.
    !!
    !! On Linux, install the packages `ffmpeg` and `v4l-utils`:
    !!
    !! ```
    !! $ sudo apt-get install ffmpeg v4l-utils
    !! ```
    !!
    !! List connected USB cameras:
    !!
    !! ```
    !! $ v4l2-ctl --list-devices
    !! UVC Camera (046d:0825) (usb-0000:00:1d.7-1):
    !!         /dev/video0
    !!         /dev/video1
    !!         /dev/media0
    !! ```
    !!
    !! The following example captures an image from an attached USB webcam at
    !! `/dev/video0` and adds a timestamp in ISO 8601 format to it:
    !!
    !! ```fortran
    !! character(len=*), parameter :: IMAGE_PATH = '/tmp/image.jpg'
    !!
    !! integer           :: width, height
    !! integer           :: rc
    !! type(camera_type) :: camera
    !!
    !! camera = camera_type(input='/dev/video0', device=CAMERA_DEVICE_V4L)
    !!
    !! rc = dm_camera_capture(camera, IMAGE_PATH)
    !! if (dm_is_error(rc)) call dm_error_out(rc)
    !!
    !! rc = dm_image_add_text_box(IMAGE_PATH, text=dm_time_now())
    !! if (dm_is_error(rc)) call dm_error_out(rc)
    !!
    !! rc = dm_image_get_dimensions(IMAGE_PATH, width, height)
    !! if (dm_is_error(rc)) call dm_error_out(rc)
    !!
    !! print '("image dimensions: ", i0, "x", i0)', width, height
    !! ```
    !!
    !! Change the camera type to capture an RTSP stream instead:
    !!
    !! ```fortran
    !! camera = camera_type(input='rtsp://10.10.10.15:8554/camera1', device=CAMERA_DEVICE_RTSP)
    !! ```
    !!
    !! The attribute `input` must be the URL of the stream.
    use :: dm_error
    use :: dm_file
    use :: dm_string
    implicit none (type, external)
    private

    ! FFmpeg devices/formats.
    integer, parameter, public :: CAMERA_DEVICE_NONE = 0 !! No device selected.
    integer, parameter, public :: CAMERA_DEVICE_RTSP = 1 !! RTSP stream.
    integer, parameter, public :: CAMERA_DEVICE_V4L  = 2 !! USB webcam via Video4Linux2.
    integer, parameter, public :: CAMERA_DEVICE_LAST = 2 !! Never use this.

    integer, parameter, public :: CAMERA_COMMAND_LEN = FILE_PATH_LEN !! Max. length of command string.

    character(len=*), parameter :: CAMERA_FFMPEG = 'ffmpeg' !! FFmpeg binary name.

    type, public :: camera_type
        !! Camera settings type.
        character(len=FILE_PATH_LEN) :: input  = ' '                !! Input device path (`/dev/video0` or `rtsp://10.0.0.1`).
        integer                      :: device = CAMERA_DEVICE_NONE !! Input device.
        integer                      :: width  = 0                  !! Camera stream width in pixels (optional).
        integer                      :: height = 0                  !! Camera stream height in pixels (optional).
    end type camera_type

    public :: dm_camera_capture
    public :: dm_camera_device_is_valid

    private :: camera_prepare_capture
contains
    integer function dm_camera_capture(camera, output, command) result(rc)
        !! Captures a single frame from a V4L device or RTSP stream with
        !! FFmpeg, and optionally adds a timestamp with GraphicsMagick. If the
        !! input is an RTSP stream, the URL must start with `rtsp://`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if camera input or output is empty.
        !! * `E_INVALID` if camera device or RTSP stream URL is invalid.
        !! * `E_IO` if FFmpeg command execution failed.
        !!
        type(camera_type),             intent(in)            :: camera  !! Camera type.
        character(len=*),              intent(in)            :: output  !! Output file.
        character(len=:), allocatable, intent(out), optional :: command !! Executed command.

        character(len=CAMERA_COMMAND_LEN) :: command_
        integer                           :: stat

        command_ = ' '

        io_block: block
            rc = E_EMPTY
            if (len_trim(camera%input) == 0 .or. len_trim(output) == 0) exit io_block

            rc = E_INVALID
            if (.not. dm_camera_device_is_valid(camera%device)) return
            if (camera%device == CAMERA_DEVICE_RTSP .and. .not. dm_string_starts_with(camera%input, 'rtsp://')) exit io_block

            rc = E_IO
            call camera_prepare_capture(command_, camera, output)
            call execute_command_line(trim(command_), exitstat=stat)
            if (stat /= 0 .or. .not. dm_file_exists(output)) exit io_block

            rc = E_NONE
        end block io_block

        if (present(command)) command = trim(command_)
    end function dm_camera_capture

    logical function dm_camera_device_is_valid(device) result(is)
        !! Returns `.true.` if device enumerator is valid. The device
        !! `CAMERA_DEVICE_NONE` is invalid.
        integer, intent(in) :: device !! Camera device enumerator.

        is = (device > CAMERA_DEVICE_NONE .and. device <= CAMERA_DEVICE_LAST)
    end function dm_camera_device_is_valid

    subroutine camera_prepare_capture(command, camera, output)
        !! Creates FFmpeg command to capture a single camera frame through V4L
        !! or RTSP. The function returns `E_INVALID` on error.
        character(len=CAMERA_COMMAND_LEN), intent(out) :: command !! Prepared command string.
        type(camera_type),                 intent(in)  :: camera  !! Camera type.
        character(len=*),                  intent(in)  :: output  !! Output file.

        character(len=32) :: video_size

        ! Disable logging and set output file.
        command = ' -hide_banner -loglevel fatal -nostats -y ' // output

        select case (camera%device)
            case (CAMERA_DEVICE_RTSP)
                ! Capture RTSP stream for 0.5 seconds to get key frame,
                ! overwrite output file.
                command = ' -i ' // trim(camera%input) // ' -f image2 -update 1 -t 0.5' // command

            case (CAMERA_DEVICE_V4L)
                ! Capture single frame from V4L device.
                if (camera%width > 0 .and. camera%height > 0) then
                    write (video_size, '(" -video_size ", i0, "x", i0)') camera%width, camera%height
                    command = trim(video_size) // command
                end if

                ! Format argument `-f` must be before input argument `-i`.
                command = ' -f v4l2 -i ' // trim(camera%input) // ' -frames:v 1' // command
        end select

        ! Concatenate command string.
        command = CAMERA_FFMPEG // trim(command)
    end subroutine camera_prepare_capture
end module dm_camera
