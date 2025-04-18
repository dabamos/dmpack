! Author:  Philipp Engel
! Licence: ISC
module dm_camera
    !! Module for taking still images from RTSP video streams and USB webcams
    !! (V4L2), using FFmpeg.
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
    !! `/dev/video0` and adds a timestamp in ISO 8601 format to it using
    !! GraphicsMagick:
    !!
    !! ```fortran
    !! character(len=*), parameter :: IMAGE_PATH = '/tmp/image.jpg'
    !!
    !! integer                :: rc
    !! type(camera_type)      :: camera
    !! type(gm_text_box_type) :: text_box
    !!
    !! camera = camera_type(input  = '/dev/video0', &
    !!                      device = CAMERA_DEVICE_V4L2, &
    !!                      width  = 1280, &
    !!                      height = 720)
    !!
    !! rc = dm_camera_capture(camera, IMAGE_PATH)
    !! if (dm_is_error(rc)) call dm_error_out(rc)
    !!
    !! text_box = gm_text_box_type(font='DroidSansMono', font_size=16)
    !! rc = dm_gm_add_text_box(IMAGE_PATH, text=dm_time_now(), text_box=text_box)
    !! if (dm_is_error(rc)) call dm_error_out(rc)
    !! ```
    !!
    !! The camera must support the resolution of 1280×720 in this case. If no
    !! resolution is specified, the camera default is used. Run _ffmpeg(1)_ to
    !! list the supported output dimensions:
    !!
    !! ```
    !! $ ffmpeg -f v4l2 -list_formats all -i /dev/video0
    !! ```
    !!
    !! RTSP streams are always captured in the stream resolution:
    !!
    !! ```fortran
    !! camera = camera_type(input='rtsp://10.10.10.15:8554/camera1', device=CAMERA_DEVICE_RTSP)
    !! ```
    !!
    !! The attribute `input` must be set to the stream URL and may include user
    !! name and password.
    use :: dm_error
    use :: dm_file
    use :: dm_string
    implicit none (type, external)
    private

    ! FFmpeg devices/formats.
    integer, parameter, public :: CAMERA_DEVICE_NONE = 0 !! No device selected.
    integer, parameter, public :: CAMERA_DEVICE_RTSP = 1 !! RTSP stream.
    integer, parameter, public :: CAMERA_DEVICE_V4L2 = 2 !! USB webcam via Video4Linux2.
    integer, parameter, public :: CAMERA_DEVICE_LAST = 2 !! Never use this.

    integer, parameter, public :: CAMERA_DEVICE_NAME_LEN = 4

    character(len=*), parameter, public :: CAMERA_DEVICE_NAMES(CAMERA_DEVICE_NONE:CAMERA_DEVICE_LAST) = [ &
        character(len=CAMERA_DEVICE_NAME_LEN) :: 'none', 'rtsp', 'v4l2' &
    ] !! Camera device names.

    character(len=*), parameter :: CAMERA_FFMPEG      = 'ffmpeg'      !! FFmpeg binary name.
    integer,          parameter :: CAMERA_COMMAND_LEN = FILE_PATH_LEN !! Max. length of command string.

    type, public :: camera_type
        !! Camera settings type.
        character(len=FILE_PATH_LEN) :: input  = ' '                !! Input device path (`/dev/video0` or `rtsp://10.0.0.1/`).
        integer                      :: device = CAMERA_DEVICE_NONE !! Input device.
        integer                      :: width  = 0                  !! Camera stream width in pixels (optional).
        integer                      :: height = 0                  !! Camera stream height in pixels (optional).
    end type camera_type

    public :: dm_camera_capture
    public :: dm_camera_device_from_name
    public :: dm_camera_device_is_valid

    private :: camera_prepare_capture
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_camera_capture(camera, path, command) result(rc)
        !! Captures a single frame from a V4L2 device or RTSP stream with
        !! FFmpeg, and optionally adds a timestamp with GraphicsMagick. If the
        !! input is an RTSP stream, the URL must start with `rtsp://`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if camera attribute input or path is empty.
        !! * `E_INVALID` if camera device or RTSP stream URL is invalid.
        !! * `E_IO` if FFmpeg command execution failed.
        !!
        type(camera_type),             intent(in)            :: camera  !! Camera type.
        character(len=*),              intent(in)            :: path    !! Output file.
        character(len=:), allocatable, intent(out), optional :: command !! Executed command.

        character(len=CAMERA_COMMAND_LEN) :: command_
        integer                           :: cmdstat, stat

        command_ = ' '

        io_block: block
            rc = E_EMPTY
            if (len_trim(camera%input) == 0 .or. len_trim(path) == 0) exit io_block

            rc = E_INVALID
            if (.not. dm_camera_device_is_valid(camera%device)) exit io_block

            if (camera%device == CAMERA_DEVICE_RTSP) then
                if (.not. dm_string_starts_with(camera%input, 'rtsp://')) exit io_block
            end if

            rc = E_IO
            call camera_prepare_capture(camera, path, command_)
            call execute_command_line(trim(command_), exitstat=stat, cmdstat=cmdstat)
            if (stat /= 0 .or. cmdstat /= 0 .or. .not. dm_file_exists(path)) exit io_block

            rc = E_NONE
        end block io_block

        if (present(command)) command = trim(command_)
    end function dm_camera_capture

    pure elemental integer function dm_camera_device_from_name(name) result(device)
        !! Returns device enumerator from name. On error, the result is
        !! `CAMERA_DEVICE_NONE`.
        character(len=*), intent(in) :: name !! Device name.

        character(len=CAMERA_DEVICE_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (CAMERA_DEVICE_NAMES(CAMERA_DEVICE_RTSP)); device = CAMERA_DEVICE_RTSP
            case (CAMERA_DEVICE_NAMES(CAMERA_DEVICE_V4L2)); device = CAMERA_DEVICE_V4L2
            case default;                                   device = CAMERA_DEVICE_NONE
        end select
    end function dm_camera_device_from_name

    pure elemental logical function dm_camera_device_is_valid(device) result(valid)
        !! Returns `.true.` if device enumerator is valid. The device
        !! `CAMERA_DEVICE_NONE` is invalid.
        integer, intent(in) :: device !! Camera device type (`CAMERA_DEVICE_*`).

        valid = (device > CAMERA_DEVICE_NONE .and. device <= CAMERA_DEVICE_LAST)
    end function dm_camera_device_is_valid

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    pure elemental subroutine camera_prepare_capture(camera, path, command)
        !! Creates FFmpeg command to capture a single camera frame through V4L2
        !! or RTSP. The function returns `E_INVALID` on error.
        type(camera_type),                 intent(in)  :: camera  !! Camera type.
        character(len=*),                  intent(in)  :: path    !! Output file.
        character(len=CAMERA_COMMAND_LEN), intent(out) :: command !! Prepared command string.

        character(len=32) :: video_size

        ! Disable logging and set output file.
        command = ' -hide_banner -loglevel quiet -nostats -y ' // path

        select case (camera%device)
            case (CAMERA_DEVICE_RTSP)
                ! Capture RTSP stream for 0.5 seconds to get key frame,
                ! overwrite output file.
                command = ' -i ' // trim(camera%input) // ' -f image2 -update 1 -t 0.5' // command

            case (CAMERA_DEVICE_V4L2)
                ! Capture single frame from V4L2 device.
                command = ' -i ' // trim(camera%input) // ' -frames:v 1' // command

                if (camera%width > 0 .and. camera%height > 0) then
                    write (video_size, '(" -video_size ", i0, "x", i0)') camera%width, camera%height
                    command = trim(video_size) // command
                end if

                ! Format argument `-f` must be before input argument `-i`.
                command = ' -f v4l2' // trim(command)
        end select

        ! Concatenate command string.
        command = CAMERA_FFMPEG // trim(command)
    end subroutine camera_prepare_capture
end module dm_camera
