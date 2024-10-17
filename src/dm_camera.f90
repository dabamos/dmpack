! Author:  Philipp Engel
! Licence: ISC
module dm_camera
    !! Module for handling IP cameras and webcams using FFmpeg.
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
    !! Capture a still image from the camera attached to `/dev/video0`, add a
    !! timestamp to the bottom right corner, and save it to `/tmp/image.jpg`:
    !!
    !! ```
    !! $ ffmpeg -f video4linux2 -i /dev/video0 -vframes 1 -video_size 640x480 \
    !!   -vf "drawtext=fontfile=DejaVuMono.ttf:fontsize=12:fontcolor=white:box=1:boxcolor=black:text='%{localtime}':x=(w-text_w):y=(h-text_h)" \
    !!   -hide_banner -loglevel error -nostats -y \
    !!   /tmp/image.jpg
    !! ```
    use :: dm_file, only: FILE_PATH_LEN
    use :: dm_mime, only: MIME_LEN, MIME_GIF, MIME_PNG, MIME_JPEG
    implicit none (type, external)
    private

    ! FFmpeg devices/formats.
    integer, parameter, public :: CAMERA_DEVICE_NONE = 0 !! No device selected.
    integer, parameter, public :: CAMERA_DEVICE_V4L  = 1 !! Video4Linux2.
    integer, parameter, public :: CAMERA_DEVICE_RTSP = 2 !! RTSP stream.

    integer, parameter, public :: CAMERA_FONT_LEN = 128  !! Max. length of font name or path.

    type, public :: camera_type
        !! Camera context type.
        integer                        :: device    = CAMERA_DEVICE_NONE !! Input device.
        character(len=FILE_PATH_LEN)   :: input     = ' '                !! Input path.
        character(len=FILE_PATH_LEN)   :: output    = ' '                !! Output path.
        character(len=MIME_LEN)        :: mime      = ' '                !! Output format (MIME type).
        character(len=CAMERA_FONT_LEN) :: font      = ' '                !! Overlay font name.
        integer                        :: font_size = 12                 !! Overlay font size
        integer                        :: width     = 0                  !! Image width in pixels.
        integer                        :: height    = 0                  !! Image size in pixels.
        logical                        :: overlay   = .false.            !! Overlay flag.
    end type camera_type
end module dm_camera
