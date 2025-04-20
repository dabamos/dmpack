! Author:  Philipp Engel
! Licence: ISC
module dm_image
    !! Image type module.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_mime
    use :: dm_node,   only: NODE_ID_LEN
    use :: dm_sensor, only: SENSOR_ID_LEN
    use :: dm_target, only: TARGET_ID_LEN
    use :: dm_time
    use :: dm_uuid
    implicit none (type, external)
    private

    character(len=*), parameter :: IMAGE_FILE_SUFFIX_JPEG = '.jpg'
    character(len=*), parameter :: IMAGE_FILE_SUFFIX_PNG  = '.png'
    integer,          parameter :: IMAGE_FILE_SUFFIX_LEN  = 4

    type, public :: image_type
        !! Image type.
        character(len=UUID_LEN)      :: id         = ' '          !! Image id (UUIDv4).
        character(len=NODE_ID_LEN)   :: node_id    = ' '          !! Node id.
        character(len=SENSOR_ID_LEN) :: sensor_id  = ' '          !! Sensor id.
        character(len=TARGET_ID_LEN) :: target_id  = ' '          !! Target id.
        character(len=TIME_LEN)      :: timestamp  = TIME_DEFAULT !! Image timestamp (ISO 8601).
        character(len=MIME_LEN)      :: mime       = ' '          !! Image format (MIME type).
        integer                      :: width      = 0            !! Image width [px].
        integer                      :: height     = 0            !! Image height [px].
        integer(kind=i8)             :: size       = 0_i8         !! Image file size [byte].
    end type image_type

    public :: dm_image_is_valid
    public :: dm_image_path
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental logical function dm_image_is_valid(image) result(valid)
        !! Returns `.true.` if passed image is valid.
        type(image_type), intent(in) :: image !! Image type.

        valid = .false.

        if (.not. dm_uuid4_is_valid(image%id)     .or. &
            .not. dm_id_is_valid(image%node_id)   .or. &
            .not. dm_id_is_valid(image%sensor_id) .or. &
            .not. dm_id_is_valid(image%target_id)) return

        if (.not. dm_time_is_valid(image%timestamp, strict=.true.))     return
        if (image%mime /= MIME_JPEG .and. image%mime /= MIME_PNG)       return
        if (image%width < 0 .or. image%height < 0 .or. image%size <= 0) return

        valid = .true.
    end function dm_image_is_valid

    pure function dm_image_path(image, base) result(path)
        !! Returns path of image file from image id, MIME type, and base path:
        !! `<base>/<id>.<suffix>`. On error, the returned string is allocated
        !! but empty.
        use :: dm_path,   only: dm_path_join
        use :: dm_string, only: dm_string_is_present

        type(image_type), intent(in)           :: image !! Image type.
        character(len=*), intent(in), optional :: base  !! Image base path.
        character(len=:), allocatable          :: path  !! Image path.

        path_block: block
            character(len=IMAGE_FILE_SUFFIX_LEN) :: suffix

            if (.not. dm_uuid4_is_valid(image%id)) exit path_block

            select case (image%mime)
                case (MIME_JPEG); suffix = IMAGE_FILE_SUFFIX_JPEG
                case (MIME_PNG);  suffix = IMAGE_FILE_SUFFIX_PNG
                case default;     exit path_block
            end select

            if (dm_string_is_present(base)) then
                path = dm_path_join(base, image%id // suffix)
            else
                path = image%id // trim(suffix)
            end if
        end block path_block

        if (.not. allocated(path)) path = ''
    end function dm_image_path

    pure elemental subroutine dm_image_set(image, id, node_id, sensor_id, target_id, timestamp, mime, width, height, size)
        !! Sets image attributes.
        type(image_type),        intent(inout)        :: image     !! Image type.
        character(len=UUID_LEN), intent(in), optional :: id        !! Image id.
        character(len=*),        intent(in), optional :: node_id   !! Node id.
        character(len=*),        intent(in), optional :: sensor_id !! Sensor id.
        character(len=*),        intent(in), optional :: target_id !! Target id.
        character(len=TIME_LEN), intent(in), optional :: timestamp !! Image timestamp.
        character(len=*),        intent(in), optional :: mime      !! Image format.
        integer,                 intent(in), optional :: width     !! Image width [px].
        integer,                 intent(in), optional :: height    !! Image height [px].
        integer(kind=i8),        intent(in), optional :: size      !! Image size [byte].

        if (present(id))        image%id        = id
        if (present(node_id))   image%node_id   = node_id
        if (present(sensor_id)) image%sensor_id = sensor_id
        if (present(target_id)) image%target_id = target_id
        if (present(timestamp)) image%timestamp = timestamp
        if (present(mime))      image%mime      = mime
        if (present(width))     image%width     = width
        if (present(height))    image%height    = height
        if (present(size))      image%size      = size
    end subroutine dm_image_set
end module dm_image
