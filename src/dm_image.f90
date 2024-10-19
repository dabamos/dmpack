! Author:  Philipp Engel
! Licence: ISC
module dm_image
    !! Image type module.
    !!
    !! GraphicsMagick is required to add text to images. On Linux, install the
    !! package `graphicsmagick`:
    !!
    !! ```
    !! $ sudo apt-get install graphicsmagick
    !! ```
    !!
    !! For a list of all supported font names, run:
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
    !! The default font is `Lucida-Console` in 12 points size.
    use :: dm_error
    use :: dm_file
    use :: dm_id,     only: ID_LEN
    use :: dm_mime,   only: MIME_LEN, MIME_GIF, MIME_PNG, MIME_JPEG
    use :: dm_node,   only: NODE_ID_LEN
    use :: dm_sensor, only: SENSOR_ID_LEN
    use :: dm_time,   only: TIME_DEFAULT, TIME_LEN
    implicit none (type, external)
    private

    integer, parameter, public :: IMAGE_DEVICE_LEN = 512 !! Max. image device path length.

    integer, parameter, public :: IMAGE_COLOR_LEN   = 32            !! Max. length of GM colour name.
    integer, parameter, public :: IMAGE_COMMAND_LEN = FILE_PATH_LEN !! Max. length of command string.
    integer, parameter, public :: IMAGE_FONT_LEN    = 64            !! Max. length of GraphicsMagick font name.
    integer, parameter, public :: IMAGE_GRAVITY_LEN = 32            !! Max. length of GM gravity.

    character(len=*), parameter :: IMAGE_GM = 'gm' !! GraphicsMagick binary name.

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

    type, public :: image_text_box_type
        !! Text box settings for drawing text onto camera frame image.
        character(len=IMAGE_GRAVITY_LEN) :: gravity    = 'SouthWest'      !! Text position (GraphicsMagick).
        character(len=IMAGE_COLOR_LEN)   :: background = 'black'          !! Box colour (GraphicsMagick).
        character(len=IMAGE_COLOR_LEN)   :: foreground = 'white'          !! Text colour (GraphicsMagick).
        character(len=IMAGE_FONT_LEN)    :: font       = 'Lucida-Console' !! Font name (GraphicsMagick).
        integer                          :: font_size  = 12               !! Font size in points.
    end type image_text_box_type

    public :: dm_image_add_text_box
    public :: dm_image_get_dimensions

    private :: image_prepare_add_text_box
contains
    integer function dm_image_add_text_box(path, text, text_box, command) result(rc)
        !! Draws text camera image file, using GraphicsMagick. By default, the
        !! text box is drawn to the bottom-left corner of the image.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if text or image path are empty.
        !! * `E_IO` if GraphicsMagick command execution failed.
        !! * `E_NOT_FOUND` if image at given path does no exist.
        !!
        character(len=*),              intent(in)            :: path     !! Image file path.
        character(len=*),              intent(in)            :: text     !! Text to add.
        type(image_text_box_type),     intent(in),  optional :: text_box !! Image text box type.
        character(len=:), allocatable, intent(out), optional :: command  !! Executed command.

        character(len=IMAGE_COMMAND_LEN) :: command_
        integer                          :: stat

        command_ = ' '

        io_block: block
            rc = E_EMPTY
            if (len_trim(path) == 0 .or. len_trim(text) == 0) return

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) return

            rc = E_IO
            call image_prepare_add_text_box(command_, path, text, text_box)
            call execute_command_line(trim(command_), exitstat=stat)
            if (stat /= 0) return

            rc = E_NONE
        end block io_block

        if (present(command)) command = trim(command_)
    end function dm_image_add_text_box

    integer function dm_image_get_dimensions(path, width, height) result(rc)
        !! Uses GraphicsMagick to determine the dimensions of the image at
        !! given path.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !! * `E_SYSTEM` if execution of GraphicsMagick failed.
        !!
        use :: dm_kind
        use :: dm_pipe

        character(len=*), intent(in)  :: path   !! Image file path.
        integer,          intent(out) :: width  !! Image width.
        integer,          intent(out) :: height !! Image height.

        character(len=32) :: dimensions
        integer           :: stat
        integer(kind=i8)  :: n
        type(pipe_type)   :: pipe

        width  = 0
        height = 0

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) return

        rc = dm_pipe_open(pipe, 'gm identify -format "%w %h" ' // trim(path), PIPE_RDONLY)
        if (dm_is_error(rc)) return

        rc = E_READ
        n = dm_pipe_read(pipe, dimensions)
        call dm_pipe_close(pipe)
        if (n == 0) return

        read (dimensions, *, iostat=stat) width, height
        if (stat /= 0) return

        rc = E_NONE
    end function dm_image_get_dimensions

    subroutine image_prepare_add_text_box(command, path, text, text_box)
        !! Prepares GraphicsMagick command to add text to image. The string
        !! `text` must not contain the quote characters `'` and `"`.
        character(len=IMAGE_COMMAND_LEN), intent(out)          :: command  !! Prepared command string.
        character(len=*),                 intent(in)           :: path     !! Image file path.
        character(len=*),                 intent(in)           :: text     !! Text to add.
        type(image_text_box_type),        intent(in), optional :: text_box !! Image text box type.

        character(len=32)         :: point_size
        type(image_text_box_type) :: box

        if (present(text_box)) box = text_box

        write (command, '(" -gravity ", a, " -box ", a, " -fill ", a, " -draw ''text 0,0 """, a, """''", 2(1x, a))') &
            trim(box%gravity), trim(box%background), trim(box%foreground), trim(text), trim(path), path

        if (box%font_size > 0) then
            write (point_size, '(" -pointsize ", i0)') box%font_size
            command = trim(point_size) // command
        end if

        if (len_trim(box%font) > 0) then
            command = ' -font ' // trim(box%font) // command
        end if

        command = IMAGE_GM // ' convert' // trim(command)
    end subroutine image_prepare_add_text_box
end module dm_image
