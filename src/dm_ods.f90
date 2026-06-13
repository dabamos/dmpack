! Author:  Philipp Engel
! Licence: ISC
module dm_ods
    !! OpenDocument Spreadsheet (ODS) 1.4 generator for poor people.
    !!
    !! * [ODF Validator](https://odfvalidator.org/)
    !!
    !! ## Example
    !!
    !! The example creates a formatted spreadsheet with a single table `Table1`,
    !! then adds a header row spanning 8 columns, followed by 32 data rows.
    !!
    !! ``` fortran
    !! integer, parameter :: NCOLUMNS = 8
    !! integer, parameter :: NROWS    = 32
    !!
    !! integer              :: i, j
    !! type(ods_type)       :: ods
    !! type(ods_style_type) :: styles(2)
    !!
    !! styles(1) = ods_style_type( &
    !!     name       = 'ce1', &
    !!     family     = ODS_STYLE_FAMILY_TABLE_CELL, &
    !!     paragraph  = ods_style_paragraph_type(text_align='center'), &
    !!     table_cell = ods_style_table_cell_type(background_color='#f0f0f0'), &
    !!     text       = ods_style_text_type(font_name='Liberation Sans', font_weight='bold') &
    !! )
    !!
    !! styles(2) = ods_style_type( &
    !!     name      = 'ce2', &
    !!     family    = ODS_STYLE_FAMILY_TABLE_CELL, &
    !!     paragraph = ods_style_paragraph_type(text_align='center'), &
    !!     text      = ods_style_text_type(font_name='Liberation Sans') &
    !! )
    !!
    !! ! Create ODS document and add table.
    !! call dm_ods_init(ods, styles=styles)
    !! call dm_ods_create_table(ods, 'Table1', NCOLUMNS)
    !!
    !! ! Add header row.
    !! call dm_ods_add_row(ods)
    !!
    !! do i = 1, NCOLUMNS
    !!     call dm_ods_add_cell(ods, achar(64 + i), style_name='ce1')
    !! end do
    !!
    !! call dm_ods_finalize_row(ods)
    !!
    !! ! Add data rows.
    !! do i = 1, NROWS
    !!     call dm_ods_add_row(ods)
    !!
    !!     do j = 1, NCOLUMNS
    !!         call dm_ods_add_cell(ods, j, style_name='ce2')
    !!     end do
    !!
    !!     call dm_ods_finalize_row(ods)
    !! end do
    !!
    !! ! Finish document and write result to ODS file.
    !! call dm_ods_finalize_table(ods)
    !! call dm_ods_finalize(ods)
    !! call dm_ods_output(ods, '/tmp/dummy.ods')
    !! ```
    !!
    !! The resulting table looks like:
    !!
    !! | A   | B   | C   | D   | E   | F   | G   | H   |
    !! |-----|-----|-----|-----|-----|-----|-----|-----|
    !! | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   |
    !! | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   |
    !! | …   | …   | …   | …   | …   | …   | …   | …   |
    !! | 1   | 2   | 3   | 4   | 5   | 6   | 7   | 8   |
    !!
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_mime
    use :: dm_path
    use :: dm_util
    use :: dm_xml
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PUBLIC PARAMETERS
    ! **************************************************************************
    ! ODS states of state machine.
    integer, parameter :: ODS_STATE_NONE        = 0 !! Initial state.
    integer, parameter :: ODS_STATE_SPREADSHEET = 1 !! Spreadsheet started.
    integer, parameter :: ODS_STATE_TABLE       = 2 !! Table started.
    integer, parameter :: ODS_STATE_ROW         = 3 !! Row started.
    integer, parameter :: ODS_STATE_FINISHED    = 4 !! Spreadsheet finished.
    integer, parameter :: ODS_STATE_LAST        = 4 !! Never use this.

    ! ODS styles.
    character(*), parameter, public :: ODS_STYLE_FAMILY_TABLE_CELL   = 'table-cell'
    character(*), parameter, public :: ODS_STYLE_FAMILY_TABLE_ROW    = 'table-row'
    character(*), parameter, public :: ODS_STYLE_FAMILY_TABLE_COLUMN = 'table-column'
    character(*), parameter, public :: ODS_STYLE_FAMILY_TABLE        = 'table'
    character(*), parameter, public :: ODS_STYLE_FAMILY_PARAGRAPH    = 'paragraph'
    character(*), parameter, public :: ODS_STYLE_FAMILY_TEXT         = 'text'
    character(*), parameter, public :: ODS_STYLE_FAMILY_GRAPHIC      = 'graphic'

    ! **************************************************************************
    ! PRIVATE PARAMETERS
    ! **************************************************************************
    ! ODS value types.
    integer, parameter :: ODS_VALUE_TYPE_NONE     = 0 !! Invalid type.
    integer, parameter :: ODS_VALUE_TYPE_STRING   = 1 !! String.
    integer, parameter :: ODS_VALUE_TYPE_FLOAT    = 2 !! Float.
    integer, parameter :: ODS_VALUE_TYPE_DATE     = 3 !! Date.
    integer, parameter :: ODS_VALUE_TYPE_TIME     = 4 !! Time.
    integer, parameter :: ODS_VALUE_TYPE_BOOLEAN  = 5 !! Boolean (NIY).
    integer, parameter :: ODS_VALUE_TYPE_CURRENCY = 6 !! Currency (NIY).
    integer, parameter :: ODS_VALUE_TYPE_LAST     = 6 !! Never use this.

    integer, parameter :: ODS_VALUE_TYPE_NAME_LEN = 8

    character(*), parameter :: ODS_VALUE_TYPE_NAMES(ODS_VALUE_TYPE_NONE:ODS_VALUE_TYPE_LAST) = [ &
        character(ODS_VALUE_TYPE_NAME_LEN) :: &
        ' ',       & ! Invalid type.
        'string',  & ! String.
        'float',   & ! Float.
        'date',    & ! Date.
        'time',    & ! Time.
        'boolean', & ! Boolean.
        'currency' & ! Currency.
    ]

    ! ODS options.
    character(*), parameter :: ODS_TEMPORARY_DIR    = '/tmp'        !! Base path of temporary files (must be absolute).
    character(*), parameter :: ODS_TEMPORARY_PREFIX = 'dmpack-ods-' !! Prefix of temporary directories.

    ! ODS file names inside archive.
    character(*), parameter :: ODS_FILE_CONTENT  = 'content.xml'  !! ODS content file.
    character(*), parameter :: ODS_FILE_MANIFEST = 'manifest.xml' !! ODS manifest file.
    character(*), parameter :: ODS_FILE_META     = 'meta.xml'     !! ODS meta file.
    character(*), parameter :: ODS_FILE_MIMETYPE = 'mimetype'     !! ODS mimetype file.
    character(*), parameter :: ODS_FILE_META_INF = 'META-INF'     !! ODS meta directory.

    integer, parameter :: ODS_ATTRIBUTE_LEN = 32 !! Max. length of type attributes.

    ! **************************************************************************
    ! PUBLIC DERIVED TYPES
    ! **************************************************************************
    ! ODS context.
    type, public :: ods_type
        !! Opaque ODS context.
        private
        character(FILE_PATH_LEN) :: path  = ' '            !! Directory of temporary files.
        integer                  :: error = E_NONE         !! Last error.
        integer                  :: state = ODS_STATE_NONE !! Current state.
        integer                  :: unit  = FILE_UNIT_NONE !! Unit of current file.
        logical                  :: raw   = .false.        !! Raw output (unformatted stream).
    end type ods_type

    ! ODS styles.
    type, public :: ods_style_paragraph_type
        !! `style:paragraph-properties`
        character(ODS_ATTRIBUTE_LEN) :: line_height  = ' ' !! `fo:line-height` (`120%`)
        character(ODS_ATTRIBUTE_LEN) :: margin_left  = ' ' !! `fo:margin-left` (`0.5cm`)
        character(ODS_ATTRIBUTE_LEN) :: margin_right = ' ' !! `fo:margin-right` (`0.5cm`)
        character(ODS_ATTRIBUTE_LEN) :: text_align   = ' ' !! `fo:text-align` (`start`, `center`, `end`, `justify`)
        character(ODS_ATTRIBUTE_LEN) :: text_indent  = ' ' !! `fo:text-indent` (`0.25cm`)
    end type ods_style_paragraph_type

    type, public :: ods_style_table_cell_type
        !! `style:table-cell-properties`
        character(ODS_ATTRIBUTE_LEN) :: background_color = ' ' !! `fo:background-color` (`#ffff00`)
        character(ODS_ATTRIBUTE_LEN) :: border           = ' ' !! `fo:border` (`0.06pt solid #000000`)
        character(ODS_ATTRIBUTE_LEN) :: border_top       = ' ' !! `fo:border-top` (`0.06pt solid #000000`)
        character(ODS_ATTRIBUTE_LEN) :: border_bottom    = ' ' !! `fo:border-bottom` (`0.06pt solid #000000`)
        character(ODS_ATTRIBUTE_LEN) :: border_left      = ' ' !! `fo:border-left` (`0.06pt solid #000000`)
        character(ODS_ATTRIBUTE_LEN) :: border_right     = ' ' !! `fo:border-right` (`0.06pt solid #000000`)
        character(ODS_ATTRIBUTE_LEN) :: padding          = ' ' !! `fo:padding` (`1mm`)
        character(ODS_ATTRIBUTE_LEN) :: padding_left     = ' ' !! `fo:padding-left` (`1mm`)
        character(ODS_ATTRIBUTE_LEN) :: padding_right    = ' ' !! `fo:padding-right` (`1mm`)
        character(ODS_ATTRIBUTE_LEN) :: vertical_align   = ' ' !! `fo:vertical-align` (`top`, `middle`, `bottom`)
        character(ODS_ATTRIBUTE_LEN) :: wrap_option      = ' ' !! `fo:wrap-option` (`wrap`)
    end type ods_style_table_cell_type

    type, public :: ods_style_table_column_type
        !! `style:table-column-properties`
        character(ODS_ATTRIBUTE_LEN) :: column_width             = ' ' !! `style:colum-width` (`2.5cm`)
        character(ODS_ATTRIBUTE_LEN) :: use_optimal_column_width = ' ' !! `style:use-optimal-column-width` (`true`)
    end type ods_style_table_column_type

    type, public :: ods_style_table_row_type
        !! `style:table-row-properties`
        character(ODS_ATTRIBUTE_LEN) :: row_height             = ' ' !! `style:row-height` (`0.8cm`)
        character(ODS_ATTRIBUTE_LEN) :: use_optimal_row_height = ' ' !! `style:use-optimal-row-height` (`true`)
    end type ods_style_table_row_type

    type, public :: ods_style_text_type
        !! `style:text-properties`
        character(ODS_ATTRIBUTE_LEN) :: color                   = ' ' !! `fo:color` (`#ff0000`)
        character(ODS_ATTRIBUTE_LEN) :: font_name               = ' ' !! `style:font-name` (`Liberation Sans`)
        character(ODS_ATTRIBUTE_LEN) :: font_size               = ' ' !! `fo:font-size` (`12pt`)
        character(ODS_ATTRIBUTE_LEN) :: font_weight             = ' ' !! `fo:font-weight` (`bold`)
        character(ODS_ATTRIBUTE_LEN) :: font_style              = ' ' !! `fo:font-style` (`italic`)
        character(ODS_ATTRIBUTE_LEN) :: font_variant            = ' ' !! `fo:font-variant` (`small-caps`)
        character(ODS_ATTRIBUTE_LEN) :: text_line_through_style = ' ' !! `style:text-line-through-style` (`solid`)
        character(ODS_ATTRIBUTE_LEN) :: text_underline_color    = ' ' !! `style:text-underline-color` (`font-color`)
        character(ODS_ATTRIBUTE_LEN) :: text_underline_style    = ' ' !! `style:text-underline-style` (`solid`)
        character(ODS_ATTRIBUTE_LEN) :: text_underline_width    = ' ' !! `style:text-underline-width` (`auto*)
    end type ods_style_text_type

    type, public :: ods_style_type
        !! ODS style.
        character(ODS_ATTRIBUTE_LEN)      :: name              = ' '                           !! `style:name`
        character(ODS_ATTRIBUTE_LEN)      :: family            = ' '                           !! `style:family` (`ODS_STYLE_FAMILY_*`)
        character(ODS_ATTRIBUTE_LEN)      :: data_style_name   = ' '                           !! `style:data-style-name`
        character(ODS_ATTRIBUTE_LEN)      :: display_name      = ' '                           !! `style:display-name`
        character(ODS_ATTRIBUTE_LEN)      :: parent_style_name = ' '                           !! `style:parent-style-name`
        type(ods_style_paragraph_type)    :: paragraph         = ods_style_paragraph_type()    !! `style:paragraph-properties`
        type(ods_style_table_cell_type)   :: table_cell        = ods_style_table_cell_type()   !! `style:table-cell-properties`
        type(ods_style_table_column_type) :: table_column      = ods_style_table_column_type() !! `style:table-column-properties`
        type(ods_style_table_row_type)    :: table_row         = ods_style_table_row_type()    !! `style:table-row-properties`
        type(ods_style_text_type)         :: text              = ods_style_text_type()         !! `style:text-properties`
    end type ods_style_type

    ! **************************************************************************
    ! PUBLIC INTERFACES
    ! **************************************************************************
    public :: dm_ods_add_cell

    interface dm_ods_add_cell
        module procedure :: ods_add_cell_int32
        module procedure :: ods_add_cell_int64
        module procedure :: ods_add_cell_real32
        module procedure :: ods_add_cell_real64
        module procedure :: ods_add_cell_string
    end interface dm_ods_add_cell

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_ods_add_cell_date
    public :: dm_ods_add_cell_time
    public :: dm_ods_add_row
    public :: dm_ods_create_table
    public :: dm_ods_destroy
    public :: dm_ods_error
    public :: dm_ods_finalize
    public :: dm_ods_finalize_row
    public :: dm_ods_finalize_table
    public :: dm_ods_init
    public :: dm_ods_is_error
    public :: dm_ods_is_finalized
    public :: dm_ods_is_valid_name
    public :: dm_ods_output
    public :: dm_ods_path

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    private :: ods_add_cell
    private :: ods_add_cell_int32
    private :: ods_add_cell_int64
    private :: ods_add_cell_real32
    private :: ods_add_cell_real64
    private :: ods_add_cell_string
    private :: ods_add_style
    private :: ods_create_content
    private :: ods_create_manifest
    private :: ods_create_meta
    private :: ods_create_mimetype
    private :: ods_file_close
    private :: ods_file_open
    private :: ods_file_write
    private :: ods_file_write_attribute
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    subroutine dm_ods_add_cell_date(ods, value, text, formula, style_name)
        !! Adds date cell to row.
        type(ods_type), intent(inout)        :: ods        !! ODS context.
        character(*),   intent(in)           :: value      !! `office:value`.
        character(*),   intent(in), optional :: text       !! `text:p`.
        character(*),   intent(in), optional :: formula    !! `table:formula` (`of:=NOW()-TODAY()`).
        character(*),   intent(in), optional :: style_name !! `table:style-name`.

        call ods_add_cell(ods, ODS_VALUE_TYPE_DATE, value, text, formula, style_name)
    end subroutine dm_ods_add_cell_date

    subroutine dm_ods_add_cell_time(ods, value, text, formula, style_name)
        !! Adds time cell to row.
        type(ods_type), intent(inout)        :: ods        !! ODS context.
        character(*),   intent(in)           :: value      !! `office:time-value`.
        character(*),   intent(in), optional :: text       !! `text:p`.
        character(*),   intent(in), optional :: formula    !! `table:formula`.
        character(*),   intent(in), optional :: style_name !! `table:style-name`.

        call ods_add_cell(ods, ODS_VALUE_TYPE_TIME, value, text, formula, style_name)
    end subroutine dm_ods_add_cell_time

    subroutine dm_ods_add_row(ods, style_name)
        !! Adds row to table.
        type(ods_type), intent(inout)        :: ods        !! ODS context.
        character(*),   intent(in), optional :: style_name !! Style name.

        if (dm_ods_is_error(ods)) return

        if (ods%unit == FILE_UNIT_NONE) then
            ods%error = E_IO
            return
        end if

        call dm_ods_finalize_row(ods)

        if (ods%state /= ODS_STATE_TABLE) then
            ods%error = E_STATE
            return
        end if

        call ods_file_write(ods, '<table:table-row')
        call ods_file_write_attribute(ods, 'table:style-name', style_name)
        call ods_file_write(ods, '>')

        ods%state = ODS_STATE_ROW
    end subroutine dm_ods_add_row

    subroutine dm_ods_create_table(ods, name, ncolumns)
        !! Creates new table of given name.
        type(ods_type), intent(inout)        :: ods      !! ODS context.
        character(*),   intent(in)           :: name     !! Table name.
        integer,        intent(in), optional :: ncolumns !! Number of columns.

        integer :: ncolumns_

        ncolumns_ = dm_present(ncolumns, 0)

        if (dm_ods_is_error(ods)) return

        if (ods%unit == FILE_UNIT_NONE) then
            call ods_file_open(ods, dm_path_join(ods%path, ODS_FILE_CONTENT))
            if (dm_ods_is_error(ods)) return
        end if

        call dm_ods_finalize_table(ods)

        if (ods%state /= ODS_STATE_SPREADSHEET) then
            ods%error = E_STATE
            return
        end if

        if (.not. dm_ods_is_valid_name(name) .or. ncolumns < 0) then
            ods%error = E_INVALID
            return
        end if

        call ods_file_write(ods, '<table:table table:name="' // dm_xml_encode(name) // '">')
        if (ncolumns_ > 0) call ods_file_write(ods, '<table:table-column table:number-columns-repeated="' // dm_itoa(ncolumns) // '"/>')

        ods%state = ODS_STATE_TABLE
    end subroutine dm_ods_create_table

    subroutine dm_ods_destroy(ods)
        !! Destroys ODS context.
        type(ods_type), intent(inout) :: ods !! ODS context.

        if (.not. dm_file_exists(ods%path)) return
        call ods_file_close(ods)
        call dm_file_delete(ods%path, recursive=.true., error=ods%error)
        ods = ods_type()
    end subroutine dm_ods_destroy

    pure integer function dm_ods_error(ods) result(error)
        !! Returns last ODS error code.
        type(ods_type), intent(in) :: ods !! ODS context.

        error = ods%error
    end function dm_ods_error

    subroutine dm_ods_finalize(ods)
        !! Finishes content data output.
        type(ods_type), intent(inout) :: ods

        if (dm_ods_is_error(ods))       return
        if (dm_ods_is_finalized(ods))   return
        if (ods%unit == FILE_UNIT_NONE) return

        call dm_ods_finalize_table(ods) ! No-op if already finalised.

        if (ods%state /= ODS_STATE_SPREADSHEET) then
            ods%error = E_STATE
            return
        end if

        call ods_file_write(ods, '</office:spreadsheet>')
        call ods_file_write(ods, '</office:body>')
        call ods_file_write(ods, '</office:document-content>')

        ! Finally, close the content file.
        call ods_file_close(ods)
        ods%state = ODS_STATE_FINISHED
    end subroutine dm_ods_finalize

    subroutine dm_ods_finalize_row(ods)
        !! Finishes row (if any).
        type(ods_type), intent(inout) :: ods !! ODS context.

        if (dm_ods_is_error(ods))       return
        if (ods%unit == FILE_UNIT_NONE) return
        if (ods%state /= ODS_STATE_ROW) return

        call ods_file_write(ods, '</table:table-row>')
        ods%state = ODS_STATE_TABLE
    end subroutine dm_ods_finalize_row

    subroutine dm_ods_finalize_table(ods)
        !! Finishes table (if any).
        type(ods_type), intent(inout) :: ods !! ODS context.

        if (dm_ods_is_error(ods))         return
        if (ods%unit == FILE_UNIT_NONE)   return
        if (ods%state == ODS_STATE_ROW)   call dm_ods_finalize_row(ods)
        if (ods%state /= ODS_STATE_TABLE) return

        call ods_file_write(ods, '</table:table>')
        ods%state = ODS_STATE_SPREADSHEET
    end subroutine dm_ods_finalize_table

    subroutine dm_ods_init(ods, styles)
        !! Initialises ODS context and creates file structure.
        use :: dm_uuid, only: dm_uuid_new

        type(ods_type),       intent(out)          :: ods       !! ODS context.
        type(ods_style_type), intent(in), optional :: styles(:) !! ODS document styles.

        ! Create temporary directory.
        ods%path = dm_path_join(ODS_TEMPORARY_DIR, ODS_TEMPORARY_PREFIX // dm_uuid_new())

        call dm_file_make_directory(ods%path, error=ods%error)
        if (dm_ods_is_error(ods)) return

        call ods_create_mimetype(ods)
        call ods_create_manifest(ods)
        call ods_create_meta(ods)
        call ods_create_content(ods, styles)
    end subroutine dm_ods_init

    pure logical function dm_ods_is_error(ods) result(is)
        !! Returns `.true.` if last ODS code is an error.
        type(ods_type), intent(in) :: ods !! ODS context.

        is = dm_is_error(ods%error)
    end function dm_ods_is_error

    pure logical function dm_ods_is_finalized(ods) result(is)
        !! Returns `.true.` if ODS document is finalized.
        type(ods_type), intent(in) :: ods !! ODS context.

        is = (ods%state == ODS_STATE_FINISHED)
    end function dm_ods_is_finalized

    logical function dm_ods_is_valid_name(name) result(is)
        !! Utility function that validates given string and returns `.true.` if
        !! the name is printable and does not contain disallowed characters
        !! (`'*/:?[\]`).
        use :: dm_string, only: dm_string_is_printable

        character(*), intent(in), optional :: name !! Attribute.

        ! Empty names are valid.
        is = .true.
        if (.not. present(name)) return

        ! No characters disallowed by LibreOffice.
        is = (dm_string_is_printable(name) .and. scan(name, "'*/:?[\]") == 0)
    end function dm_ods_is_valid_name

    subroutine dm_ods_output(ods, path)
        !! Creates ODS file at `path` from directory `ods%path`:
        !!
        !! ```
        !! <ods%path>
        !! ├─ mimetype
        !! ├─ content.xml
        !! ├─ meta.xml
        !! └─ META-INF/
        !!    └─ manifest.xml
        !! ```
        !!
        !! The output file `path` must be absolute and shall have the file
        !! ending `.ods`. The output file shall not exist beforehand.
        !!
        !! This subroutine requires _zip(1)_.
        !!
        !! The ODS error code is set to:
        !!
        !! * `E_EXEC` if execution of _zip(1)_ failed.
        !! * `E_EXIST` if output file `path` exists.
        !! * `E_INVALID` if argument `path` is not an absolute path.
        !! * `E_NOT_FOUND` if ODS spreadsheet is not prepared.
        !!
        use :: dm_zip, only: dm_zip_compress

        type(ods_type), intent(inout) :: ods  !! ODS context.
        character(*),   intent(in)    :: path !! Absolute path of ODS output file (`/tmp/dummy.ods`).

        character(:), allocatable :: content, manifest, meta, mimetype

        if (dm_ods_is_error(ods)) return

        if (.not. dm_file_exists(ods%path)) then
            ods%error = E_NOT_FOUND
            return
        end if

        if (len_trim(path) == 0) then
            ods%error = E_INVALID
            return
        end if

        if (path(1:1) /= '/') then
            ods%error = E_INVALID
            return
        end if

        if (dm_file_exists(path)) then
            ods%error = E_EXIST
            return
        end if

        if (.not. dm_ods_is_finalized(ods)) call dm_ods_finalize(ods)

        mimetype = dm_path_join(ods%path, ODS_FILE_MIMETYPE) ! Must be first file in archive and uncompressed.
        content  = dm_path_join(ods%path, ODS_FILE_CONTENT)
        meta     = dm_path_join(ods%path, ODS_FILE_META)
        manifest = dm_path_join(ODS_FILE_META_INF, ODS_FILE_MANIFEST)

        call dm_zip_compress(path, mimetype, 0, junk=.true., error=ods%error); if (dm_ods_is_error(ods)) return
        call dm_zip_compress(path, content,  9, junk=.true., error=ods%error); if (dm_ods_is_error(ods)) return
        call dm_zip_compress(path, meta,     9, junk=.true., error=ods%error); if (dm_ods_is_error(ods)) return
        call dm_zip_compress(path, manifest, 9, cd=ods%path, error=ods%error); if (dm_ods_is_error(ods)) return
    end subroutine dm_ods_output

    function dm_ods_path(ods) result(path)
        !! Returns path of temporary directory in use.
        type(ods_type), intent(inout) :: ods  !! ODS context.
        character(:), allocatable     :: path !! Returned path.

        path = trim(ods%path)
    end function dm_ods_path

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    subroutine ods_add_cell(ods, value_type, value, text, formula, style_name)
        type(ods_type), intent(inout)        :: ods        !! ODS context.
        integer,        intent(in)           :: value_type !! `office:value-type`.
        character(*),   intent(in)           :: value      !! `office:value`, `office:date-value`, `office:time-value`.
        character(*),   intent(in), optional :: text       !! `text:p`.
        character(*),   intent(in), optional :: formula    !! `table:formula`.
        character(*),   intent(in), optional :: style_name !! `table:style-name`.

        if (value_type <= ODS_VALUE_TYPE_NONE .or. value_type > ODS_VALUE_TYPE_LAST) then
            ods%error = E_INVALID
            return
        end if

        call ods_file_write(ods, '<table:table-cell')
        call ods_file_write_attribute(ods, 'office:value-type', ODS_VALUE_TYPE_NAMES(value_type))

        select case (value_type)
            case (ODS_VALUE_TYPE_DATE); call ods_file_write_attribute(ods, 'office:date-value', value)
            case (ODS_VALUE_TYPE_TIME); call ods_file_write_attribute(ods, 'office:time-value', value)
            case default;               call ods_file_write_attribute(ods, 'office:value',      value)
        end select

        call ods_file_write_attribute(ods, 'table:formula',    formula)
        call ods_file_write_attribute(ods, 'table:style-name', style_name)
        call ods_file_write(ods, '>')

        if (present(text)) then
            call ods_file_write(ods, '<text:p>' // dm_xml_encode(text) // '</text:p>')
        else
            call ods_file_write(ods, '<text:p>' // dm_xml_encode(value) // '</text:p>')
        end if

        call ods_file_write(ods, '</table:table-cell>')
    end subroutine ods_add_cell

    subroutine ods_add_cell_int32(ods, value, text, formula, style_name)
        type(ods_type), intent(inout)        :: ods        !! ODS context.
        integer(i4),    intent(in)           :: value      !! `office:value`.
        character(*),   intent(in), optional :: text       !! `text:p`.
        character(*),   intent(in), optional :: formula    !! `table:formula`.
        character(*),   intent(in), optional :: style_name !! `table:style-name`.

        call ods_add_cell(ods, ODS_VALUE_TYPE_FLOAT, dm_itoa(value), text, formula, style_name)
    end subroutine ods_add_cell_int32

    subroutine ods_add_cell_int64(ods, value, text, formula, style_name)
        type(ods_type), intent(inout)        :: ods        !! ODS context.
        integer(i8),    intent(in)           :: value      !! `office:value`.
        character(*),   intent(in), optional :: text       !! `text:p`.
        character(*),   intent(in), optional :: formula    !! `table:formula`.
        character(*),   intent(in), optional :: style_name !! `table:style-name`.

        call ods_add_cell(ods, ODS_VALUE_TYPE_FLOAT, dm_itoa(value), text, formula, style_name)
    end subroutine ods_add_cell_int64

    subroutine ods_add_cell_real32(ods, value, text, formula, style_name)
        type(ods_type), intent(inout)        :: ods        !! ODS context.
        real(r4),       intent(in)           :: value      !! `office:value`.
        character(*),   intent(in), optional :: text       !! `text:p`.
        character(*),   intent(in), optional :: formula    !! `table:formula`.
        character(*),   intent(in), optional :: style_name !! `table:style-name`.

        call ods_add_cell(ods, ODS_VALUE_TYPE_FLOAT, dm_ftoa(value), text, formula, style_name)
    end subroutine ods_add_cell_real32

    subroutine ods_add_cell_real64(ods, value, text, formula, style_name)
        type(ods_type), intent(inout)        :: ods        !! ODS context.
        real(r8),       intent(in)           :: value      !! `office:value`.
        character(*),   intent(in), optional :: text       !! `text:p`.
        character(*),   intent(in), optional :: formula    !! `table:formula`.
        character(*),   intent(in), optional :: style_name !! `table:style-name`.

        call ods_add_cell(ods, ODS_VALUE_TYPE_FLOAT, dm_ftoa(value), text, formula, style_name)
    end subroutine ods_add_cell_real64

    subroutine ods_add_cell_string(ods, value, text, formula, style_name)
        type(ods_type), intent(inout)        :: ods        !! ODS context.
        character(*),   intent(in)           :: value      !! `office:value`.
        character(*),   intent(in), optional :: text       !! `text:p`.
        character(*),   intent(in), optional :: formula    !! `table:formula`.
        character(*),   intent(in), optional :: style_name !! `table:style-name`.

        call ods_add_cell(ods, ODS_VALUE_TYPE_STRING, value, text, formula, style_name)
    end subroutine ods_add_cell_string

    subroutine ods_add_style(ods, style)
        !! Writes style blocks to file currently opened.
        use :: dm_string, only: dm_string_has

        type(ods_type),       intent(inout) :: ods   !! ODS context.
        type(ods_style_type), intent(in)    :: style !! ODS style to write.

        if (dm_ods_is_error(ods)) return

        if (ods%unit == FILE_UNIT_NONE) then
            ods%error = E_INVALID
            return
        end if

        if (.not. dm_string_has(style%name)) then
            ods%error = E_CORRUPT
            return
        end if

        ! Open style element.
        call ods_file_write(ods, '<style:style')
        call ods_file_write_attribute(ods, 'style:name',              style%name)
        call ods_file_write_attribute(ods, 'style:family',            style%family)
        call ods_file_write_attribute(ods, 'style:data-style-name',   style%data_style_name)
        call ods_file_write_attribute(ods, 'style:display-name',      style%display_name)
        call ods_file_write_attribute(ods, 'style:parent-style-name', style%parent_style_name)
        call ods_file_write(ods, '>')

        ! Add paragraph properties (if any).
        associate (paragraph => style%paragraph)
            call ods_file_write(ods, '<style:paragraph-properties')
            call ods_file_write_attribute(ods, 'fo:line-height',  paragraph%line_height)
            call ods_file_write_attribute(ods, 'fo:margin-left',  paragraph%margin_left)
            call ods_file_write_attribute(ods, 'fo:margin-right', paragraph%margin_right)
            call ods_file_write_attribute(ods, 'fo:text-align',   paragraph%text_align)
            call ods_file_write_attribute(ods, 'fo:text-indent',  paragraph%text_indent)
            call ods_file_write(ods, '/>')
        end associate

        ! Add table cell properties (if any).
        associate (table_cell => style%table_cell)
            call ods_file_write(ods, '<style:table-cell-properties')
            call ods_file_write_attribute(ods, 'fo:background-color', table_cell%background_color)
            call ods_file_write_attribute(ods, 'fo:border',           table_cell%border)
            call ods_file_write_attribute(ods, 'fo:border-top',       table_cell%border_top)
            call ods_file_write_attribute(ods, 'fo:border-bottom',    table_cell%border_bottom)
            call ods_file_write_attribute(ods, 'fo:border-left',      table_cell%border_left)
            call ods_file_write_attribute(ods, 'fo:border-right',     table_cell%border_right)
            call ods_file_write_attribute(ods, 'fo:padding',          table_cell%padding)
            call ods_file_write_attribute(ods, 'fo:padding-left',     table_cell%padding_left)
            call ods_file_write_attribute(ods, 'fo:padding-left',     table_cell%padding_right)
            call ods_file_write_attribute(ods, 'fo:vertical-align',   table_cell%vertical_align)
            call ods_file_write_attribute(ods, 'fo:wrap-option',      table_cell%wrap_option)
            call ods_file_write(ods, '/>')
        end associate

        ! Add table column properties (if any).
        associate (table_column => style%table_column)
            call ods_file_write(ods, '<style:table-column-properties')
            call ods_file_write_attribute(ods, 'style:colum-width',              table_column%column_width)
            call ods_file_write_attribute(ods, 'style:use-optimal-column-width', table_column%use_optimal_column_width)
            call ods_file_write(ods, '/>')
        end associate

        ! Add table row properties (if any).
        associate (table_row => style%table_row)
            call ods_file_write(ods, '<style:table-row-properties')
            call ods_file_write_attribute(ods, 'style:row-height',             table_row%row_height)
            call ods_file_write_attribute(ods, 'style:use-optimal-row-height', table_row%use_optimal_row_height)
            call ods_file_write(ods, '/>')
        end associate

        ! Add text properties (if any).
        associate (text => style%text)
            call ods_file_write(ods, '<style:text-properties')
            call ods_file_write_attribute(ods, 'fo:color',                      text%color)
            call ods_file_write_attribute(ods, 'style:font-name',               text%font_name)
            call ods_file_write_attribute(ods, 'fo:font-size',                  text%font_size)
            call ods_file_write_attribute(ods, 'fo:font-weight',                text%font_weight)
            call ods_file_write_attribute(ods, 'fo:font-style',                 text%font_style)
            call ods_file_write_attribute(ods, 'fo:font-variant',               text%font_variant)
            call ods_file_write_attribute(ods, 'style:text-line-through-style', text%text_line_through_style)
            call ods_file_write_attribute(ods, 'style:text-underline-color',    text%text_underline_color)
            call ods_file_write_attribute(ods, 'style:text-underline-style',    text%text_underline_style)
            call ods_file_write_attribute(ods, 'style:text-underline-width',    text%text_underline_width)
            call ods_file_write(ods, '/>')
        end associate

        ! Close style element.
        call ods_file_write(ods, '</style:style>')
    end subroutine ods_add_style

    subroutine ods_create_content(ods, styles)
        !! Writes ODS content header.
        use :: dm_string, only: dm_string_has

        type(ods_type),       intent(inout)        :: ods       !! ODS context.
        type(ods_style_type), intent(in), optional :: styles(:) !! ODS styles.

        integer :: i

        if (dm_ods_is_error(ods)) return

        if (ods%state /= ODS_STATE_NONE) then
            ods%error = E_STATE
            return
        end if

        call ods_file_open(ods, dm_path_join(ods%path, ODS_FILE_CONTENT))
        if (dm_ods_is_error(ods)) return

        ! Document header.
        call ods_file_write(ods, XML_HEADER, .true.)
        call ods_file_write(ods, '<office:document-content')
        call ods_file_write(ods, ' xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0"')
        call ods_file_write(ods, ' xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0"')
        call ods_file_write(ods, ' xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"')
        call ods_file_write(ods, ' xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"')
        call ods_file_write(ods, ' xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0"')
        call ods_file_write(ods, ' xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0"')
        call ods_file_write(ods, ' xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0"')
        call ods_file_write(ods, ' xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2"')
        call ods_file_write(ods, ' xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"')
        call ods_file_write(ods, ' xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0"')
        call ods_file_write(ods, ' xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"')
        call ods_file_write(ods, ' xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"')
        call ods_file_write(ods, ' xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"')
        call ods_file_write(ods, ' xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"')
        call ods_file_write(ods, ' office:version="1.4">')

        ! Font declarations and automatic styles (if present).
        if (present(styles)) then
            call ods_file_write(ods, '<office:font-face-decls>')

            do i = 1, size(styles)
                associate (font_name => styles(i)%text%font_name)
                    if (.not. dm_string_has(font_name))        cycle
                    if (.not. dm_ods_is_valid_name(font_name)) cycle

                    call ods_file_write(ods, '<style:font-face')
                    call ods_file_write_attribute(ods, 'style:name', font_name)
                    call ods_file_write(ods, '/>')
                end associate
            end do

            call ods_file_write(ods, '</office:font-face-decls>')
            call ods_file_write(ods, '<office:automatic-styles>')

            do i = 1, size(styles)
                call ods_add_style(ods, styles(i))
            end do

            call ods_file_write(ods, '</office:automatic-styles>')
        end if

        ! Document body.
        call ods_file_write(ods, '<office:body>')
        call ods_file_write(ods, '<office:spreadsheet>')
        call ods_file_close(ods)

        ods%state = ODS_STATE_SPREADSHEET
    end subroutine ods_create_content

    subroutine ods_create_manifest(ods)
        !! Writes ODS manifest file.
        type(ods_type), intent(inout) :: ods !! ODS context.

        character(:), allocatable :: path

        if (dm_ods_is_error(ods)) return

        ! Create META-INF directory in parent directory.
        path = dm_path_join(ods%path, ODS_FILE_META_INF)
        call dm_file_make_directory(path, error=ods%error)
        if (dm_ods_is_error(ods)) return

        ! Create manifest file in META-INF.
        call ods_file_open(ods, dm_path_join(path, ODS_FILE_MANIFEST))
        if (dm_ods_is_error(ods)) return

        call ods_file_write(ods, XML_HEADER, .true.)
        call ods_file_write(ods, '<manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0" manifest:version="1.4">')
        call ods_file_write(ods, '<manifest:file-entry manifest:full-path="/" manifest:media-type="' // MIME_ODS // '" manifest:version="1.4"/>')
        call ods_file_write(ods, '<manifest:file-entry manifest:full-path="content.xml" manifest:media-type="text/xml"/>')
        call ods_file_write(ods, '<manifest:file-entry manifest:full-path="meta.xml" manifest:media-type="text/xml"/>')
        call ods_file_write(ods, '</manifest:manifest>')
        call ods_file_close(ods)
    end subroutine ods_create_manifest

    subroutine ods_create_meta(ods)
        !! Writes ODS meta file.
        use :: dm_time,    only: dm_time_now
        use :: dm_version, only: DM_VERSION_STRING

        type(ods_type), intent(inout) :: ods !! ODS context.

        if (dm_ods_is_error(ods)) return

        call ods_file_open(ods, dm_path_join(ods%path, ODS_FILE_META))
        if (dm_ods_is_error(ods)) return

        call ods_file_write(ods, XML_HEADER, .true.)
        call ods_file_write(ods, '<office:document-meta')
        call ods_file_write(ods, ' xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0"')
        call ods_file_write(ods, ' xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"')
        call ods_file_write(ods, ' office:version="1.4">')
        call ods_file_write(ods, '<office:meta>')
        call ods_file_write(ods, '<meta:creation-date>' // dm_time_now() // '</meta:creation-date>')
        call ods_file_write(ods, '<meta:generator>DMPACK ' // DM_VERSION_STRING // '</meta:generator>')
        call ods_file_write(ods, '</office:meta>')
        call ods_file_write(ods, '</office:document-meta>')
        call ods_file_close(ods)
    end subroutine ods_create_meta

    subroutine ods_create_mimetype(ods)
        !! Writes ODS mimetype file.
        type(ods_type), intent(inout) :: ods !! ODS context.

        if (dm_ods_is_error(ods)) return

        ! Do not add line termination to output.
        call ods_file_open(ods, dm_path_join(ods%path, ODS_FILE_MIMETYPE), raw=.true.)
        if (dm_ods_is_error(ods)) return

        call ods_file_write(ods, MIME_ODS)
        call ods_file_close(ods)
    end subroutine ods_create_mimetype

    ! **************************************************************************
    ! PRIVATE ODS FILE PROCEDURES
    ! **************************************************************************
    subroutine ods_file_close(ods)
        !! Closes file and resets file unit.
        type(ods_type), intent(inout) :: ods !! ODS context.

        if (ods%unit == FILE_UNIT_NONE) return
        close (ods%unit)
        ods%unit = FILE_UNIT_NONE
    end subroutine ods_file_close

    subroutine ods_file_open(ods, path, raw)
        !! Opens file for writing, either in formatted sequential or in
        !! unformatted stream mode.
        type(ods_type), intent(inout)        :: ods  !! ODS context.
        character(*),   intent(in)           :: path !! File path.
        logical,        intent(in), optional :: raw  !! Unformatted stream.

        integer :: stat

        if (len_trim(path) == 0) then
            ods%error = E_EMPTY
            return
        end if

        if (ods%unit /= FILE_UNIT_NONE) then
            ods%error = E_EXIST
            return
        end if

        ods%raw = dm_present(raw, .false.)

        ! Open unformatted.
        if (ods%raw) then
            open (access='stream', action='write', form='unformatted', file=trim(path), iostat=stat, newunit=ods%unit)
            if (stat /= 0) ods%error = E_IO
            return
        end if

        ! Open formatted.
        open (action='write', file=trim(path), iostat=stat, newunit=ods%unit, position='append')
        if (stat /= 0) ods%error = E_IO
    end subroutine ods_file_open

    subroutine ods_file_write(ods, bytes, advance)
        !! Writes to file, either formatted or unformatted, depending on the
        !! mode the file was opened with. The argument `advance` controls
        !! whether  line termination is added to the output (only for formatted
        !! output).
        !!
        !! The ODS context error is set to the following values:
        !!
        !! * `E_INVALID` if the file is closed.
        !! * `E_WRITE` if writing to file failed.
        !!
        type(ods_type), intent(inout)        :: ods     !! ODS context.
        character(*),   intent(in)           :: bytes   !! Bytes to write.
        logical,        intent(in), optional :: advance !! Advance to next record.

        integer :: stat

        if (dm_is_error(ods%error)) return

        if (ods%unit == FILE_UNIT_NONE) then
            ods%error = E_INVALID
            return
        end if

        ! Write unformatted.
        if (ods%raw) then
            write (ods%unit, iostat=stat) trim(bytes)
            if (stat /= 0) ods%error = E_WRITE
            return
        end if

        ! Write formatted (advance).
        if (dm_present(advance, .false.)) then
            write (ods%unit, '(a)', iostat=stat) trim(bytes)
            if (stat /= 0) ods%error = E_WRITE
            return
        end if

        ! Write formatted (no advance).
        write (ods%unit, '(a)', advance='no', iostat=stat) trim(bytes)
        if (stat /= 0) ods%error = E_WRITE
    end subroutine ods_file_write

    subroutine ods_file_write_attribute(ods, name, value)
        !! Writes XML attribute `name` and associated `value` if both are
        !! passed.
        !!
        !! The ODS context error is set to the following values:
        !!
        !! * `E_INVALID` if the file is closed.
        !! * `E_WRITE` if writing to file failed.
        !!
        use :: dm_string, only: dm_string_has

        type(ods_type), intent(inout) :: ods   !! ODS context.
        character(*),   intent(in)    :: name  !! Attribute name.
        character(*),   intent(in)    :: value !! Attribute value.

        if (.not. dm_string_has(name) .or. .not. dm_string_has(value)) return
        call ods_file_write(ods, ' ' // trim(name) // '="' // dm_xml_encode(value) // '"')
    end subroutine ods_file_write_attribute
end module dm_ods
