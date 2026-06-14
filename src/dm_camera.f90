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
    !! ``` fortran
    !! character(*), parameter :: IMAGE_PATH = '/tmp/image.jpg'
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
    !! ``` fortran
    !! camera = camera_type(input='rtsp://10.10.10.15:8554/camera1', device=CAMERA_DEVICE_RTSP)
    !! ```
    !!
    !! The attribute `input` must be set to the stream URL and may include user
    !! name and password.
    use :: dm_buffer
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_string
    use :: dm_util
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PUBLIC PARAMETERS
    ! **************************************************************************
    ! FFmpeg devices/formats.
    integer, parameter, public :: CAMERA_DEVICE_NONE = 0 !! No device selected.
    integer, parameter, public :: CAMERA_DEVICE_RTSP = 1 !! RTSP stream.
    integer, parameter, public :: CAMERA_DEVICE_V4L2 = 2 !! USB webcam via Video4Linux2.
    integer, parameter, public :: CAMERA_DEVICE_LAST = 2 !! Never use this.

    integer, parameter, public :: CAMERA_DEVICE_NAME_LEN = 4

    character(*), parameter, public :: CAMERA_DEVICE_NAMES(CAMERA_DEVICE_NONE:CAMERA_DEVICE_LAST) = [ &
        character(CAMERA_DEVICE_NAME_LEN) :: &
        'none', & ! CAMERA_DEVICE_NONE
        'rtsp', & ! CAMERA_DEVICE_RTSP
        'v4l2'  & ! CAMERA_DEVICE_V4L2
    ] !! Camera device names.

    ! **************************************************************************
    ! PRIVATE PARAMETERS
    ! **************************************************************************
    character(*), parameter :: FFMPEG_BINARY = 'ffmpeg' !! FFmpeg binary path or name.

    ! **************************************************************************
    ! PUBLIC DERIVED TYPES
    ! **************************************************************************
    type, public :: camera_type
        !! Camera settings type.
        character(FILE_PATH_LEN) :: input  = ' '                !! Input device path (`/dev/video0` or `rtsp://10.0.0.1/`).
        integer                  :: device = CAMERA_DEVICE_NONE !! Input device.
        integer                  :: width  = 0                  !! Camera stream width in pixels (optional).
        integer                  :: height = 0                  !! Camera stream height in pixels (optional).
    end type camera_type

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_camera_capture
    public :: dm_camera_device_from_name
    public :: dm_camera_device_is_valid
    public :: dm_camera_out

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    private :: camera_prepare_capture
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    integer function dm_camera_capture(camera, path, dry, command) result(rc)
        !! Captures a single frame from a V4L2 device or RTSP stream with
        !! FFmpeg, and optionally adds a timestamp with GraphicsMagick. If the
        !! input is an RTSP stream, the URL must start with `rtsp://`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_CORRUPT` if capture command could not be created.
        !! * `E_EMPTY` if camera attribute input or path is empty.
        !! * `E_EXEC` if FFmpeg command execution failed.
        !! * `E_FORMAT` if RTSP address is invalid.
        !! * `E_INVALID` if camera device or RTSP stream URL is invalid.
        !!
        type(camera_type), intent(in)              :: camera  !! Camera type.
        character(*),      intent(in)              :: path    !! Output file.
        logical,           intent(in),    optional :: dry     !! Dry run.
        character(*),      intent(inout), optional :: command !! Executed command.

        type(buffer_type) :: buffer
        integer           :: cmdstat, stat

        if (present(command)) command = ''

        call dm_buffer_init(buffer, int(FILE_PATH_LEN, i8), rc)
        if (dm_is_error(rc)) return

        capture_block: block
            rc = E_EMPTY
            if (len_trim(camera%input) == 0 .or. len_trim(path) == 0) exit capture_block

            rc = E_INVALID
            if (.not. dm_camera_device_is_valid(camera%device)) exit capture_block

            rc = E_FORMAT
            if (camera%device == CAMERA_DEVICE_RTSP .and. .not. dm_string_starts_with(camera%input, 'rtsp://')) exit capture_block

            rc = E_CORRUPT
            call camera_prepare_capture(buffer, camera, path)
            if (buffer%nbytes == 0) exit capture_block

            if (dm_present(dry, .false.)) then
                rc = E_NONE
                exit capture_block
            end if

            rc = E_EXEC
            call execute_command_line(dm_buffer_bytes(buffer), exitstat=stat, cmdstat=cmdstat)
            if (stat == 0 .and. cmdstat == 0 .and. dm_file_exists(path)) rc = E_NONE
        end block capture_block

        if (present(command)) command = dm_buffer_copy(buffer)
        call dm_buffer_destroy(buffer)
    end function dm_camera_capture

    pure elemental integer function dm_camera_device_from_name(name) result(device)
        !! Returns device enumerator from name. On error, the result is
        !! `CAMERA_DEVICE_NONE`.
        character(*), intent(in) :: name !! Device name.

        character(CAMERA_DEVICE_NAME_LEN) :: name_

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

    subroutine dm_camera_out(camera, unit)
        !! Prints camera to standard output or given file unit.
        type(camera_type), intent(inout)        :: camera !! Camera.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("camera.input: ", a)')   trim(camera%input)
        write (unit_, '("camera.device: ", i0)') camera%device
        write (unit_, '("camera.width: ", i0)')  camera%width
        write (unit_, '("camera.height: ", i0)') camera%height
    end subroutine dm_camera_out

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    pure subroutine camera_prepare_capture(buffer, camera, path)
        !! Creates FFmpeg command to capture a single camera frame through V4L2
        !! or RTSP. The function returns `E_INVALID` on error.
        type(buffer_type), intent(inout) :: buffer !! Command buffer.
        type(camera_type), intent(in)    :: camera !! Camera type.
        character(*),      intent(in)    :: path   !! Output file.

        if (.not. dm_buffer_is_initialized(buffer)) then
            call dm_buffer_init(buffer, int(FILE_PATH_LEN, i8))
        end if

        select case (camera%device)
            case (CAMERA_DEVICE_RTSP)
                ! Capture RTSP stream for 0.5 seconds to get key frame,
                ! overwrite output file.
                call dm_buffer_append(buffer, FFMPEG_BINARY)
                call dm_buffer_append(buffer, ' -f image2 -update 1 -t 0.5')
                call dm_buffer_append(buffer, ' -i ')
                call dm_buffer_append(buffer, trim(camera%input))

            case (CAMERA_DEVICE_V4L2)
                ! Capture single frame from V4L2 device.
                call dm_buffer_append(buffer, FFMPEG_BINARY)
                call dm_buffer_append(buffer, ' -f v4l2') ! Format argument `-f` must be before input argument `-i`.

                ! Capture single frame from V4L2 device.
                if (camera%width > 0 .and. camera%height > 0) then
                    call dm_buffer_append(buffer, ' -video_size ')
                    call dm_buffer_append(buffer, dm_itoa(camera%width))
                    call dm_buffer_append(buffer, 'x')
                    call dm_buffer_append(buffer, dm_itoa(camera%height))
                end if

                call dm_buffer_append(buffer, ' -i ')
                call dm_buffer_append(buffer, trim(camera%input))
                call dm_buffer_append(buffer, ' -frames:v 1')
            case default
                return
        end select

        call dm_buffer_append(buffer, ' -hide_banner -loglevel quiet -nostats -y ')
        call dm_buffer_append(buffer, trim(path))
    end subroutine camera_prepare_capture
end module dm_camera
