! Author:  Philipp Engel
! Licence: ISC
module dm_image
    !! Image type module.
    use :: dm_id,     only: ID_LEN
    use :: dm_mime,   only: MIME_LEN
    use :: dm_node,   only: NODE_ID_LEN
    use :: dm_sensor, only: SENSOR_ID_LEN
    use :: dm_time,   only: TIME_DEFAULT, TIME_LEN
    implicit none (type, external)
    private

    integer, parameter, public :: IMAGE_DEVICE_LEN = 512 !! Max. image device path length.

    type, public :: image_type
        !! Image type.
        character(len=ID_LEN)           :: id         = ' '          !! Image id (UUIDv4).
        character(len=NODE_ID_LEN)      :: node_id    = ' '          !! Node id.
        character(len=SENSOR_ID_LEN)    :: sensor_id  = ' '          !! Sensor id.
        character(len=TIME_LEN)         :: timestamp  = TIME_DEFAULT !! Image timestamp (ISO 85601).
        character(len=IMAGE_DEVICE_LEN) :: device     = ' '          !! Physical camera device path, host, or address.
        character(len=MIME_LEN)         :: mime       = ' '          !! Image format (MIME type).
        integer                         :: width      = 0            !! Image width in pixels.
        integer                         :: height     = 0            !! Image height in pixels.
    end type image_type
end module dm_image
