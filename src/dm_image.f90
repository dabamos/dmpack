! Author:  Philipp Engel
! Licence: ISC
module dm_image
    !! Module for handling images of IP cameras and webcams.
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
    use :: dm_id,     only: ID_LEN
    use :: dm_mime,   only: MIME_LEN, MIME_GIF, MIME_PNG, MIME_JPEG
    use :: dm_node,   only: NODE_ID_LEN
    use :: dm_sensor, only: SENSOR_ID_LEN
    use :: dm_time
    implicit none (type, external)
    private

    integer, parameter, public :: IMAGE_DEVICE_LEN = 512            !! Max. image device path length.
    integer, parameter, public :: IMAGE_BASE64_LEN = int(z'800000') !! Max. base64-encoded image data size in bytes (8 MiB).

    type, public :: image_meta_type
        !! Image meta type.
        character(len=ID_LEN)           :: id         = ' '          !! Image id (UUIDv4).
        character(len=NODE_ID_LEN)      :: node_id    = ' '          !! Node id.
        character(len=SENSOR_ID_LEN)    :: sensor_id  = ' '          !! Sensor id.
        character(len=TIME_LEN)         :: timestamp  = TIME_DEFAULT !! Image timestamp (ISO 85601).
        character(len=IMAGE_DEVICE_LEN) :: device     = ' '          !! Physical camera device path, host, or address.
        character(len=MIME_LEN)         :: mime       = ' '          !! Image format (MIME type).
        integer                         :: width      = 0            !! Image width in pixels.
        integer                         :: height     = 0            !! Image height in pixels.
    end type image_meta_type

    type, public :: image_data_type
        !! Image data type.
        character(len=IMAGE_BASE64_LEN) :: base64 = ' ' !! Base64-encoded image data.
    end type image_data_type

    type, public :: image_type
        !! Image compound type.
        type(image_meta_type) :: meta !! Image meta data.
        type(image_data_type) :: data !! Image data.
    end type image_type
end module dm_image
