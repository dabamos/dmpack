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

    integer, parameter, public :: IMAGE_ID_LEN = UUID_LEN

    character(*), parameter :: IMAGE_FILE_SUFFIX_JPEG = '.jpg'
    character(*), parameter :: IMAGE_FILE_SUFFIX_PNG  = '.png'
    integer,      parameter :: IMAGE_FILE_SUFFIX_LEN  = 4

    type, public :: image_type
        !! Image type.
        character(IMAGE_ID_LEN)  :: id        = ' '          !! Image id (UUIDv4).
        character(NODE_ID_LEN)   :: node_id   = ' '          !! Node id.
        character(SENSOR_ID_LEN) :: sensor_id = ' '          !! Sensor id.
        character(TARGET_ID_LEN) :: target_id = ' '          !! Target id.
        character(TIME_LEN)      :: timestamp = TIME_DEFAULT !! Image timestamp (ISO 8601).
        character(MIME_LEN)      :: mime      = ' '          !! Image format (MIME type).
        integer                  :: width     = 0            !! Image width [px].
        integer                  :: height    = 0            !! Image height [px].
        integer(i8)              :: size      = 0_i8         !! Image file size [byte].
    end type image_type

    interface operator (==)
        !! Returns `.true.` if images are equal.
        module procedure :: dm_image_equals
    end interface

    public :: operator (==)

    public :: dm_image_equals
    public :: dm_image_is_valid
    public :: dm_image_out
    public :: dm_image_path
    public :: dm_image_set
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental logical function dm_image_equals(image1, image2) result(equals)
        !! Returns `.true.` if given images are equal.
        type(image_type), intent(in) :: image1 !! The first image.
        type(image_type), intent(in) :: image2 !! The second image.

        equals = (image1%id        == image2%id        .and. &
                  image1%node_id   == image2%node_id   .and. &
                  image1%sensor_id == image2%sensor_id .and. &
                  image1%target_id == image2%target_id .and. &
                  image1%timestamp == image2%timestamp .and. &
                  image1%mime      == image2%mime      .and. &
                  image1%width     == image2%width     .and. &
                  image1%height    == image2%height    .and. &
                  image1%size      == image2%size)
    end function dm_image_equals

    pure elemental logical function dm_image_is_valid(image) result(valid)
        !! Returns `.true.` if passed image is valid.
        type(image_type), intent(in) :: image !! Image.

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

        type(image_type), intent(in)           :: image !! Image.
        character(*),     intent(in), optional :: base  !! Image base path.
        character(:), allocatable              :: path  !! Image path.

        path_block: block
            character(IMAGE_FILE_SUFFIX_LEN) :: suffix

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

    subroutine dm_image_out(image, unit)
        !! Prints image to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(image_type), intent(inout)        :: image
        integer,          intent(in), optional :: unit

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("image.id: ", a)')        trim(image%id)
        write (unit_, '("image.node_id: ", a)')   trim(image%node_id)
        write (unit_, '("image.sensor_id: ", a)') trim(image%sensor_id)
        write (unit_, '("image.target_id: ", a)') trim(image%target_id)
        write (unit_, '("image.timestamp: ", a)') trim(image%timestamp)
        write (unit_, '("image.mime: ", a)')      trim(image%mime)
        write (unit_, '("image.width: ", i0)')    image%width
        write (unit_, '("image.height: ", i0)')   image%height
        write (unit_, '("image.size: ", i0)')     image%size
    end subroutine dm_image_out

    pure elemental subroutine dm_image_set(image, id, node_id, sensor_id, target_id, timestamp, mime, width, height, size)
        !! Sets image attributes.
        type(image_type),    intent(inout)        :: image     !! Image.
        character(UUID_LEN), intent(in), optional :: id        !! Image id.
        character(*),        intent(in), optional :: node_id   !! Node id.
        character(*),        intent(in), optional :: sensor_id !! Sensor id.
        character(*),        intent(in), optional :: target_id !! Target id.
        character(TIME_LEN), intent(in), optional :: timestamp !! Image timestamp.
        character(*),        intent(in), optional :: mime      !! Image format.
        integer,             intent(in), optional :: width     !! Image width [px].
        integer,             intent(in), optional :: height    !! Image height [px].
        integer(i8),         intent(in), optional :: size      !! Image size [byte].

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
