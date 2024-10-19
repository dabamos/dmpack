! Author:  Philipp Engel
! Licence: ISC
module dm_camera
    !! Module for taking still images from RTSP video streams and USB webcams,
    !! using FFmpeg.
    !!
    !! On Linux, install the packages `ffmpeg`, `graphicsmagick`, and
    !! `v4l-utils`:
    !!
    !! ```
    !! $ sudo apt-get install ffmpeg graphicsmagick v4l-utils
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
    !! GraphicsMagick is required to add text to captured camera frames. For a
    !! list of all supported font names, run:
    !!
    !! ```
    !! $ gm convert -list font
    !! Path: /usr/local/lib/GraphicsMagick/config/type-windows.mgk
    !!
    !! Name                             Family                  Style   Stretch  Weight
    !! --------------------------------------------------------------------------------
    !! Arial                            Arial                   normal  normal    400
    !! Arial-Black                      Arial                   normal  normal    900
    !! Arial-Bold                       Arial                   normal  normal    700
    !! Arial-Bold-Italic                Arial                   italic  normal    700
    !! Arial-Italic                     Arial                   italic  normal    400
    !! ...
    !! ```
    !!
    !! The default font used is `Lucida-Console` in 12 points size.
    !!
    !! The following example captures an image from an attached USB webcam at
    !! `/dev/video0` and adds a timestamp in ISO 8601 to it:
    !!
    !! ```fortran
    !! character(len=*), parameter :: IMAGE_PATH = '/tmp/image.jpg'
    !!
    !! integer           :: rc
    !! type(camera_type) :: camera
    !!
    !! camera = camera_type(input='/dev/video0', device=CAMERA_DEVICE_V4L)
    !!
    !! rc = dm_camera_capture(camera, IMAGE_PATH)
    !! if (dm_is_error(rc)) call dm_error_out(rc)
    !!
    !! rc = dm_camera_image_add_text(IMAGE_PATH, text=dm_time_now())
    !! if (dm_is_error(rc)) call dm_error_out(rc)
    !! ```
    use :: dm_error
    use :: dm_file
    use :: dm_string
    use :: dm_time
    implicit none (type, external)
    private

    ! FFmpeg devices/formats.
    integer, parameter, public :: CAMERA_DEVICE_NONE = 0 !! No device selected.
    integer, parameter, public :: CAMERA_DEVICE_RTSP = 1 !! RTSP stream.
    integer, parameter, public :: CAMERA_DEVICE_V4L  = 2 !! USB webcam via Video4Linux2.
    integer, parameter, public :: CAMERA_DEVICE_LAST = 2 !! Never use this.

    integer, parameter, public :: CAMERA_COLOR_LEN   = 32            !! Max. length of GM colour name.
    integer, parameter, public :: CAMERA_COMMAND_LEN = FILE_PATH_LEN !! Max. length of command string.
    integer, parameter, public :: CAMERA_FONT_LEN    = 64            !! Max. length of GraphicsMagick font name.
    integer, parameter, public :: CAMERA_GRAVITY_LEN = 32            !! Max. length of GM gravity.

    character(len=*), parameter :: CAMERA_FFMPEG = 'ffmpeg' !! FFmpeg binary name.
    character(len=*), parameter :: CAMERA_GM     = 'gm'     !! GraphicsMagick binary name.

    type, public :: camera_type
        !! Camera settings type.
        character(len=FILE_PATH_LEN) :: input  = ' '                !! Input device path (`/dev/video0` or `rtsp://10.0.0.1`).
        integer                      :: device = CAMERA_DEVICE_NONE !! Input device.
        integer                      :: width  = 0                  !! Camera stream width in pixels (optional).
        integer                      :: height = 0                  !! Camera stream height in pixels (optional).
    end type camera_type

    type, public :: camera_text_box_type
        !! Text box settings for drawing text onto camera frame image.
        character(len=CAMERA_GRAVITY_LEN) :: gravity    = 'SouthWest'      !! Text position.
        character(len=CAMERA_COLOR_LEN)   :: background = 'black'          !! Box colour.
        character(len=CAMERA_COLOR_LEN)   :: foreground = 'white'          !! Text colour.
        character(len=CAMERA_FONT_LEN)    :: font       = 'Lucida-Console' !! GraphicsMagick font name.
        integer                           :: font_size  = 12               !! Font size in points.
    end type camera_text_box_type

    public :: dm_camera_capture
    public :: dm_camera_device_is_valid
    public :: dm_camera_image_add_text
    public :: dm_camera_prepare_command_ffmpeg
    public :: dm_camera_prepare_command_gm
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_camera_capture(camera, output) result(rc)
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
        type(camera_type), intent(in) :: camera !! Camera type.
        character(len=*),  intent(in) :: output !! Output file.

        character(len=CAMERA_COMMAND_LEN) :: command
        integer                           :: exit_stat

        rc = E_EMPTY
        if (len_trim(camera%input) == 0 .or. len_trim(output) == 0) return

        rc = E_INVALID
        if (.not. dm_camera_device_is_valid(camera%device)) return
        if (camera%device == CAMERA_DEVICE_RTSP .and. .not. dm_string_starts_with(camera%input, 'rtsp://')) return

        rc = E_IO
        call dm_camera_prepare_command_ffmpeg(command, camera, output)
        call execute_command_line(trim(command), exitstat=exit_stat)

        if (exit_stat /= 0) return
        if (.not. dm_file_exists(output)) return

        rc = E_NONE
    end function dm_camera_capture

    logical function dm_camera_device_is_valid(device) result(is)
        !! Returns `.true.` if device enumerator is valid. The device
        !! `CAMERA_DEVICE_NONE` is invalid.
        integer, intent(in) :: device !! Camera device enumerator.

        is = (device > CAMERA_DEVICE_NONE .and. device <= CAMERA_DEVICE_LAST)
    end function dm_camera_device_is_valid

    integer function dm_camera_image_add_text(path, text, box) result(rc)
        !! Draws text onto camera image file, using GraphicsMagick. By default,
        !! the text box is drawn to the bottom-left corner of the image.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if text or image path are empty.
        !! * `E_IO` if GraphicsMagick command execution failed.
        !! * `E_NOT_FOUND` if image at given path does no exist.
        !!
        character(len=*),           intent(in)           :: path !! Image file path.
        character(len=*),           intent(in)           :: text !! Text to add.
        type(camera_text_box_type), intent(in), optional :: box  !! Camera box type.

        character(len=CAMERA_COMMAND_LEN) :: command
        integer                           :: exit_stat

        rc = E_EMPTY
        if (len_trim(path) == 0 .or. len_trim(text) == 0) return

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) return

        rc = E_IO
        call dm_camera_prepare_command_gm(command, path, text, box)
        call execute_command_line(trim(command), exitstat=exit_stat)
        if (exit_stat /= 0) return

        rc = E_NONE
    end function dm_camera_image_add_text

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_camera_prepare_command_ffmpeg(command, camera, output)
        !! Creates FFmpeg command to capture a single camera frame through V4L
        !! or RTSP. The function returns `E_INVALID` on error.
        character(len=CAMERA_COMMAND_LEN), intent(out) :: command !! Prepared command string.
        type(camera_type),                 intent(in)  :: camera  !! Camera type.
        character(len=*),                  intent(in)  :: output  !! Output file.

        character(len=32) :: video_size

        ! Disable logging and set output file.
        command = ' -hide_banner -loglevel fatal -nostats -y ' // output

        ! Format argument `-f` must be before input argument `-i`.
        select case (camera%device)
            case (CAMERA_DEVICE_RTSP)
                ! Capture RTSP stream for 0.5 seconds to get key frame,
                ! overwrite output file.
                command = ' -f image2 -i ' // trim(camera%input) // ' -update 1 -t 0.5' // command

            case (CAMERA_DEVICE_V4L)
                ! Capture single frame from V4L device.
                if (camera%width > 0 .and. camera%height > 0) then
                    write (video_size, '(" -video_size ", i0, "x", i0)') camera%width, camera%height
                    command = trim(video_size) // command
                end if

                command = ' -f v4l2 -i ' // trim(camera%input) // ' -frames:v 1' // command
        end select

        ! Concatenate command string.
        command = CAMERA_FFMPEG // command
    end subroutine dm_camera_prepare_command_ffmpeg

    subroutine dm_camera_prepare_command_gm(command, path, text, box)
        !! Prepares GraphicsMagick command to add text to image.
        character(len=CAMERA_COMMAND_LEN), intent(out)          :: command !! Prepared command string.
        character(len=*),                  intent(in)           :: path    !! Image file path.
        character(len=*),                  intent(in)           :: text    !! Text to add.
        type(camera_text_box_type),        intent(in), optional :: box     !! Camera box type.

        character(len=32)          :: point_size
        type(camera_text_box_type) :: box_

        if (present(box)) box_ = box

        write (command, '(" -gravity ", a, " -box ", a, " -fill ", a, " -draw ''text 0,0 """, a, """''", 2(1x, a))') &
            trim(box_%gravity), trim(box_%background), trim(box_%foreground), trim(text), trim(path), path

        if (box_%font_size > 0) then
            write (point_size, '(" -pointsize ", i0)') box_%font_size
            command = trim(point_size) // command
        end if

        if (len_trim(box_%font) > 0) then
            command = ' -font ' // trim(box_%font) // command
        end if

        command = CAMERA_GM // ' convert' // command
    end subroutine dm_camera_prepare_command_gm
end module dm_camera
