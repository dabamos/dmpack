! Author:  Philipp Engel
! Licence: ISC
module dm_roff
    !! Module for creating formatted output in GNU roff (with _ms_ macro
    !! package by default).
    !!
    !! In order to create PostScript and PDF files, _groff(1)_ must be
    !! installed. On Linux, run:
    !!
    !! ```
    !! $ sudo apt-get install ghostscript groff
    !! ```
    !!
    !! Create a PDF report from _ms_ markup:
    !!
    !! ```fortran
    !! character(:), allocatable :: roff
    !! integer                   :: rc
    !!
    !! ! Generate markup with macro package -ms.
    !! roff = dm_roff_ms_header(title='Test Report', author='Sensor Node 1', &
    !!                          institution='University of Elbonia', &
    !!                          font_family=ROFF_FONT_HELVETICA, &
    !!                          left_footer=dm_time_date(), &
    !!                          right_footer='DMPACK ' // DM_VERSION_STRING)
    !! roff = roff // dm_roff_ms_sh(2, 'Results')
    !! roff = roff // dm_roff_ms_lp('First paragraph.')
    !!
    !! ! Create PDF from markup.
    !! rc = dm_roff_to_pdf(roff, '/tmp/report.pdf', macro=ROFF_MACRO_MS)
    !! ```
    use :: dm_ascii, only: NL => ASCII_LF, TAB => ASCII_TAB
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! Output devices.
    integer, parameter, public :: ROFF_DEVICE_NONE = 0 !! Invalid device.
    integer, parameter, public :: ROFF_DEVICE_UTF8 = 1 !! Plain text (UTF-8).
    integer, parameter, public :: ROFF_DEVICE_PS   = 2 !! PostScript.
    integer, parameter, public :: ROFF_DEVICE_PDF  = 3 !! PDF.
    integer, parameter, public :: ROFF_DEVICE_HTML = 4 !! HTML 4.01.
    integer, parameter, public :: ROFF_DEVICE_LAST = 4 !! Never use this.

    integer, parameter, public :: ROFF_DEVICE_NAME_LEN = 4 !! Max. device name length.

    character(*), parameter, public :: ROFF_DEVICE_NAMES(ROFF_DEVICE_NONE:ROFF_DEVICE_LAST) = [ &
        character(ROFF_DEVICE_NAME_LEN) :: 'none', 'utf8', 'ps', 'pdf', 'html' &
    ] !! Device names.

    ! Macro packages.
    integer, parameter, public :: ROFF_MACRO_NONE = 0 !! No macro package.
    integer, parameter, public :: ROFF_MACRO_MS   = 1 !! Macro package -ms.
    integer, parameter, public :: ROFF_MACRO_LAST = 1 !! Never use this.

    ! Font families.
    integer, parameter, public :: ROFF_FONT_NONE                   = 0 !! Default font (Times Roman).
    integer, parameter, public :: ROFF_FONT_AVANT_GARDE            = 1 !! Avant Garde.
    integer, parameter, public :: ROFF_FONT_BOOKMAN                = 2 !! Bookman.
    integer, parameter, public :: ROFF_FONT_HELVETICA              = 3 !! Helvetica.
    integer, parameter, public :: ROFF_FONT_HELVETICA_NARROW       = 4 !! Helvetica Narrow.
    integer, parameter, public :: ROFF_FONT_NEW_CENTURY_SCHOOLBOOK = 5 !! New Century Schoolbook.
    integer, parameter, public :: ROFF_FONT_PALATINO               = 6 !! Palatino.
    integer, parameter, public :: ROFF_FONT_TIMES_ROMAN            = 7 !! Times Roman.
    integer, parameter, public :: ROFF_FONT_ZAPF_CHANCERY          = 8 !! Zapf Chancery (italic only).
    integer, parameter, public :: ROFF_FONT_LAST                   = 8 !! Never use this.

    integer, parameter, public :: ROFF_FONT_NAME_LEN = 3 !! Max. font name length.

    character(*), parameter, public :: ROFF_FONT_NAMES(ROFF_FONT_NONE:ROFF_FONT_LAST) = [ &
        character(ROFF_FONT_NAME_LEN) :: 'T', 'A', 'BM', 'H', 'HN', 'N', 'P', 'T', 'ZCM' &
    ] !! Font family names.

    ! Groff and ms requests.
    character(*), parameter, public :: ROFF_REQUEST_BP    = '.bp' // NL !! Page break.
    character(*), parameter, public :: ROFF_REQUEST_BR    = '.br' // NL !! Line break.
    character(*), parameter, public :: ROFF_REQUEST_MS_P1 = '.P1' // NL !! Typeset header on page 1 (ms).

    ! Escape sequences, see groff_char(7).
    character(*), parameter, public :: ROFF_ESC_DUMMY  = '\&'         !! Interpolate a dummy character.
    character(*), parameter, public :: ROFF_ESC_EMDASH = '\[em]'      !! em dash.
    character(*), parameter, public :: ROFF_ESC_ENDASH = '\[en]'      !! en dash.
    character(*), parameter, public :: ROFF_ESC_HR     = "\l'\n(.lu'" !! Horizontal rule.
    character(*), parameter, public :: ROFF_ESC_HYPHEN = '\[hy]'      !! Hyphen.
    character(*), parameter, public :: ROFF_ESC_MVUP   = '\u'         !! Move ½ em up.
    character(*), parameter, public :: ROFF_ESC_NBSP   = '\~'         !! None-breaking space.

    character(*), parameter, public :: ROFF_ENCODING_UTF8 = '.\" -*- mode: troff; coding: utf-8 -*-' // NL !! UTF-8 encoding for preconv.

    ! Executables.
    character(*), parameter :: GROFF_BINARY  = 'groff'
    character(*), parameter :: PS2PDF_BINARY = 'ps2pdf'

    interface dm_roff_ms_nr
        !! Generic macro function to set register value.
        module procedure :: roff_ms_nr_int32
        module procedure :: roff_ms_nr_real32
    end interface dm_roff_ms_nr

    ! Public procedures.
    public :: dm_roff_device_is_valid
    public :: dm_roff_macro_is_valid
    public :: dm_roff_ps_to_pdf
    public :: dm_roff_to_pdf
    public :: dm_roff_to_ps
    public :: dm_roff_version

    ! Public high-level macros.
    public :: dm_roff_ms_header
    public :: dm_roff_tbl

    ! Public low-level macros.
    public :: dm_roff_defcolor  ! Define colour.
    public :: dm_roff_pspic     ! Include EPS picture.
    public :: dm_roff_m         ! Change stroke colour.
    public :: dm_roff_s         ! Change font size.
    public :: dm_roff_sp        ! Add vertical space.
    public :: dm_roff_tbl_block ! Text block.

    public :: dm_roff_ms_ai ! Author institution.
    public :: dm_roff_ms_au ! Author name.
    public :: dm_roff_ms_bx ! Box around text.
    public :: dm_roff_ms_ds ! Define string.
    public :: dm_roff_ms_lp ! Paragraph without indent.
    public :: dm_roff_ms_nh ! Numbered heading.
    public :: dm_roff_ms_nr ! Set register value.
    public :: dm_roff_ms_pp ! Standard paragraph with indent.
    public :: dm_roff_ms_sh ! Section heading (without number).
    public :: dm_roff_ms_tl ! Title.

    ! Private functions.
    private :: roff_make

    ! Private low-level macros.
    private :: roff_ms_nr_int32
    private :: roff_ms_nr_real32
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    pure elemental logical function dm_roff_device_is_valid(device) result(is)
        !! Returns `.true.` if argument `device` is a valid device enumerator.
        integer, intent(in) :: device !! Device type (`ROFF_MACRO_*`).

        is = (device > ROFF_DEVICE_NONE .and. device <= ROFF_DEVICE_LAST)
    end function dm_roff_device_is_valid

    pure elemental logical function dm_roff_macro_is_valid(macro) result(is)
        !! Returns `.true.` if argument `macro` is a valid macro enumerator.
        integer, intent(in) :: macro !! Macro type (`ROFF_MACRO_*`).

        is = (macro >= ROFF_MACRO_NONE .and. macro <= ROFF_MACRO_LAST)
    end function dm_roff_macro_is_valid

    integer function dm_roff_ps_to_pdf(input, output) result(rc)
        !! Converts PostScript file `input` to PDF file `output` by executing
        !! _ps2pdf(1)_. On error, an empty PDF file may be created. This
        !! function requires Ghostscript to be installed locally.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ERROR` if execution of _ps2pdf(1)_ failed.
        !! * `E_IO` if output file could not be created.
        !! * `E_NOT_FOUND` if input file does not exist.
        !!
        use :: dm_file, only: dm_file_exists, dm_file_touch

        character(*), intent(in) :: input  !! Path of PostScript file.
        character(*), intent(in) :: output !! Path of PDF file.

        integer :: cmdstat, stat

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(input)) return

        call dm_file_touch(output, error=rc)
        if (dm_is_error(rc)) return

        call execute_command_line(PS2PDF_BINARY // ' ' // trim(input) // ' ' // trim(output), exitstat=stat, cmdstat=cmdstat)
        if (stat /= 0 .or. cmdstat /= 0) rc = E_ERROR
    end function dm_roff_ps_to_pdf

    integer function dm_roff_to_pdf(roff, path, macro, pic, preconv, tbl) result(rc)
        !! Passes the markup string `roff` to _groff(1)_ to create a PDF file
        !! that is written to `path`. An existing file will not be replaced. On
        !! error, an empty file may still be created. By default, this function
        !! uses macro package _ms_, unless argument `macro` is passed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ERROR` if execution of _groff(1)_ failed.
        !! * `E_EXIST` if output file at `path` exists.
        !! * `E_INVALID` if `macro` is invalid.
        !! * `E_IO` if output file could not be created.
        !! * `E_SYSTEM` if system call failed.
        !! * `E_WRITE` if writing failed.
        !!
        character(*), intent(inout)        :: roff    !! Markup string.
        character(*), intent(in)           :: path    !! Path of output file.
        integer,      intent(in), optional :: macro   !! Macro package to use (`ROFF_MACRO_*`).
        logical,      intent(in), optional :: pic     !! Run pic preprocessor.
        logical,      intent(in), optional :: preconv !! Run preconv preprocessor.
        logical,      intent(in), optional :: tbl     !! Run tbl preprocessor.

        rc = roff_make(ROFF_DEVICE_PDF, roff, path, macro, pic, preconv, tbl)
    end function dm_roff_to_pdf

    integer function dm_roff_to_ps(roff, path, macro, pic, preconv, tbl) result(rc)
        !! Passes the markup string `roff` to _groff(1)_ to create a PostScript
        !! file that is written to `path`. An existing file will not be
        !! replaced. On error, an empty file may still be created. By default,
        !! this function uses macro package _ms_, unless argument `macro` is
        !! passed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ERROR` if execution of _groff(1)_ failed.
        !! * `E_EXIST` if output file at `path` exists.
        !! * `E_INVALID` if `macro` is invalid.
        !! * `E_IO` if output file could not be created.
        !! * `E_SYSTEM` if system call failed.
        !! * `E_WRITE` if writing failed.
        !!
        character(*), intent(inout)        :: roff    !! Markup string.
        character(*), intent(in)           :: path    !! Path of output file.
        integer,      intent(in), optional :: macro   !! Macro package to use (`ROFF_MACRO_*`).
        logical,      intent(in), optional :: pic     !! Run pic preprocessor.
        logical,      intent(in), optional :: preconv !! Run preconv preprocessor.
        logical,      intent(in), optional :: tbl     !! Run tbl preprocessor.

        rc = roff_make(ROFF_DEVICE_PS, roff, path, macro, pic, preconv, tbl)
    end function dm_roff_to_ps

    function dm_roff_version(name, found) result(version)
        !! Returns GNU roff version as allocatable string.
        use :: dm_pipe

        character(*), parameter :: NAME_STR = 'groff'

        logical, intent(in),  optional :: name    !! Add prefix `groff/`.
        logical, intent(out), optional :: found   !! Returns `.true.` if groff has been found.
        character(:), allocatable      :: version !! Version string.

        character(8)    :: a(3), v
        character(32)   :: buffer
        integer         :: stat, rc
        type(pipe_type) :: pipe

        if (present(found)) found = .false.

        rc = dm_pipe_open(pipe, GROFF_BINARY // ' --version', PIPE_RDONLY)
        v  = '0.0.0'

        if (dm_is_ok(rc)) then
            rc = dm_pipe_read_line(pipe, buffer)

            if (len_trim(buffer) > 0) then
                read (buffer, *, iostat=stat) a, v
                if (present(found) .and. stat == 0) found = .true.
            end if
        end if

        call dm_pipe_close(pipe)

        if (dm_present(name, .false.)) then
            version = NAME_STR // '/' // trim(v)
        else
            version = trim(v)
        end if
    end function dm_roff_version

    ! **************************************************************************
    ! PUBLIC HIGH-LEVEL MACROS.
    ! **************************************************************************
    pure function dm_roff_ms_header(title, author, institution, font_family, font_size, &
                                    left_header, center_header, right_header, &
                                    left_footer, center_footer, right_footer, &
                                    page_one) result(roff)
        !! Creates a new GNU roff document with macro package _ms_. The result
        !! has to be piped to _groff(1)_.
        !!
        !! The font family must be one of the following types:
        !!
        !! * `ROFF_FONT_AVANT_GARDE`
        !! * `ROFF_FONT_BOOKMAN`
        !! * `ROFF_FONT_HELVETICA`
        !! * `ROFF_FONT_HELVETICA_NARROW`
        !! * `ROFF_FONT_NEW_CENTURY_SCHOOLBOOK`
        !! * `ROFF_FONT_PALATINO`
        !! * `ROFF_FONT_TIMES_ROMAN`
        !!
        !! If no font family and size is provided, Times Roman in 10 pt is used
        !! by default. To set a fractional font size, multiply the size by 1000.
        !! For instance, pass `10500` in argument `font_size` to select a size
        !! of 10.5 pt.
        !!
        !! The return value is newline-terminated. Append a request or macro to
        !! the output of this function to create a valid _groff(1)_ document.
        use :: dm_string, only: dm_string_is_present

        character(*), intent(in), optional :: title         !! Document title.
        character(*), intent(in), optional :: author        !! Author name.
        character(*), intent(in), optional :: institution   !! Institution name.
        integer,      intent(in), optional :: font_family   !! Font family enumerator (`ROFF_FONT_*`).
        integer,      intent(in), optional :: font_size     !! Font size in pt.
        character(*), intent(in), optional :: left_header   !! Left header content.
        character(*), intent(in), optional :: center_header !! Center header content.
        character(*), intent(in), optional :: right_header  !! Right header content.
        character(*), intent(in), optional :: left_footer   !! Left footer content.
        character(*), intent(in), optional :: center_footer !! Center footer content.
        character(*), intent(in), optional :: right_footer  !! Right footer content.
        logical,      intent(in), optional :: page_one      !! Enable header on page 1.
        character(:), allocatable          :: roff          !! Output string.

        character(3) :: fam
        integer      :: ff, ps
        logical      :: p1

        ff = dm_present(font_family, ROFF_FONT_NONE)
        ps = dm_present(font_size,   10)
        p1 = dm_present(page_one,    .false.)

        if (ff < ROFF_FONT_NONE .or. ff > ROFF_FONT_LAST) ff = ROFF_FONT_NONE
        fam = ROFF_FONT_NAMES(ff)

        roff = ROFF_ENCODING_UTF8                // & ! Encoding for preconv (groff argument -k).
               dm_roff_ms_ds('FAM',    fam)      // & ! Font family.
               dm_roff_ms_nr('PS',     ps)       // & ! Font size [pt].
               dm_roff_ms_nr('GROWPS', 3)        // & ! Heading depth.
               dm_roff_ms_nr('PSINCR', 1.5, 'p') // & ! Heading increment [pt].
               dm_roff_ms_nr('HM',     2,   'c') // & ! Header margin [cm].
               dm_roff_ms_nr('FM',     3,   'c') // & ! Footer margin [cm].
               dm_roff_ms_nr('PO',     2,   'c') // & ! Left margin [cm].
               dm_roff_ms_nr('LL',     17,  'c')      ! Line length [cm].

        if (p1) roff = roff // ROFF_REQUEST_MS_P1 ! Header on page 1.

        ! Header.
        if (present(left_header))  roff = roff // dm_roff_ms_ds('LH', left_header)
        if (present(right_header)) roff = roff // dm_roff_ms_ds('RH', right_header)

        roff = roff // dm_roff_ms_ds('CH', center_header)

        ! Footer.
        if (present(left_footer))  roff = roff // dm_roff_ms_ds('LF', left_footer)
        if (present(right_footer)) roff = roff // dm_roff_ms_ds('RF', right_footer)

        if (present(center_footer)) then
            roff = roff // dm_roff_ms_ds('CF', center_footer)
        else
            roff = roff // dm_roff_ms_ds('CF', '%')
        end if

        if (dm_string_is_present(title))       roff = roff // dm_roff_ms_tl(title)
        if (dm_string_is_present(author))      roff = roff // dm_roff_ms_au(author)
        if (dm_string_is_present(institution)) roff = roff // dm_roff_ms_ai(institution)
    end function dm_roff_ms_header

    function dm_roff_tbl(format, data, all_box, box, double_box, center, expand, no_spaces) result(roff)
        !! Returns table markup for _tbl(1)_.
        use :: dm_string, only: dm_string_append

        character(*), intent(inout)        :: format(:, :) !! Table format.
        character(*), intent(inout)        :: data(:, :)   !! Table data.
        logical,      intent(in), optional :: all_box      !! Encloses each item of the table in a box.
        logical,      intent(in), optional :: box          !! Encloses the table in a box.
        logical,      intent(in), optional :: double_box   !! Encloses the table in a double box.
        logical,      intent(in), optional :: center       !! Centers the table (default is left-justified).
        logical,      intent(in), optional :: expand       !! Makes the table as wide as the current line length.
        logical,      intent(in), optional :: no_spaces    !! Ignore leading and trailing spaces in data items.
        character(:), allocatable          :: roff         !! Output string.

        character(64) :: options
        integer       :: i, j, ncol, nrow

        options = ' '

        if (dm_present(all_box,    .false.)) options = dm_string_append(options, ' allbox')
        if (dm_present(box,        .false.)) options = dm_string_append(options, ' box')
        if (dm_present(double_box, .false.)) options = dm_string_append(options, ' doublebox')
        if (dm_present(center,     .false.)) options = dm_string_append(options, ' center')
        if (dm_present(expand,     .false.)) options = dm_string_append(options, ' expand')
        if (dm_present(no_spaces,  .false.)) options = dm_string_append(options, ' nospaces')

        if (len_trim(options) > 0) then
            roff = '.TS' // NL // trim(adjustl(options)) // ';' // NL
        else
            roff = '.TS' // NL
        end if

        ncol = size(format, 1)
        nrow = size(format, 2)

        ! Table format.
        do j = 1, nrow
            do i = 1, ncol
                if (i == ncol) then
                    roff = roff // trim(format(i, j))
                else
                    roff = roff // trim(format(i, j)) // ' '
                end if
            end do

            if (j == nrow) then
                roff = roff // '.' // NL
            else
                roff = roff // NL
            end if
        end do

        ! Table data.
        ncol = size(data, 1)
        nrow = size(data, 2)

        do j = 1, nrow
            do i = 1, ncol
                if (i == ncol) then
                    roff = roff // trim(data(i, j)) // NL
                else
                    roff = roff // trim(data(i, j)) // TAB
                end if
            end do
        end do

        roff = roff // '.TE' // NL
    end function dm_roff_tbl

    ! **************************************************************************
    ! PUBLIC LOW-LEVEL REQUESTS, ESCAPE SEQUENCES, AND MACROS.
    ! **************************************************************************
    pure function dm_roff_defcolor(ident, r, g, b) result(roff)
        !! Returns request to define a named stroke colour.
        character(*), intent(in)  :: ident !! Colour identifier.
        integer,      intent(in)  :: r     !! Red channel.
        integer,      intent(in)  :: g     !! Green channel.
        integer,      intent(in)  :: b     !! Blue channel.
        character(:), allocatable :: roff  !! Output string.

        character(7) :: hex

        call dm_rgb_to_hex(r, g, b, hex)
        roff = '.defcolor ' // trim(ident) // ' rgb ' // hex // NL
    end function dm_roff_defcolor

    pure function dm_roff_m(ident, text) result(roff)
        !! Returns escape sequence to change stroke colour of `text` to
        !! identifier `ident`. The identifier has to be defined with
        !! `dm_roff_defcolor()` beforehand. The result is not new-line
        !! terminated!
        character(*), intent(in)  :: ident !! Colour identifier.
        character(*), intent(in)  :: text  !! Text.
        character(:), allocatable :: roff  !! Output string.

        roff = '\m[' // trim(ident) // ']' // trim(text) // '\m[]'
    end function dm_roff_m

    pure function dm_roff_pspic(path, align, width, height) result(roff)
        !! Returns `.PSPIC` macro to add image in Encapsulated PostScript (EPS)
        !! format. Argument `align` must be either `L` (left), `R` (right), or
        !! `C` (center). If not passed, the image will be centered.
        !!
        !! The arguments `width` and `height` give the desired width and height
        !! of the image. If neither a width nor a height argument is specified,
        !! the image’s natural width (as given in the file’s bounding box) or
        !! the current line length is used as the width, whatever is smaller.
        use :: dm_string, only: dm_upper

        character(*), intent(in)           :: path   !! Path to EPS file.
        character,    intent(in), optional :: align  !! Image alignment (`L`, `R`, `C`).
        real,         intent(in), optional :: width  !! Image width [cm].
        real,         intent(in), optional :: height !! Image height [cm].
        character(:), allocatable          :: roff   !! Output string.

        character(16) :: d
        character     :: a
        real          :: w, h

        a = dm_present(align,  'C')
        w = dm_present(width,  0.0)
        h = dm_present(height, 0.0)

        call dm_upper(a)
        if (a /= 'L' .and. a /= 'R' .and. a /= 'C') a = 'C'

        if (w > 0.0 .and. h > 0.0) then
            write (d, '(1x, f0.1, "c ", f0.1, "c")') w, h
        else if (w > 0.0) then
            write (d, '(1x, f0.1, "c")') w
        else
            d = ' '
        end if

        roff = '.PSPIC -' // a // ' ' // trim(path) // trim(d) // NL
    end function dm_roff_pspic

    pure function dm_roff_s(n, text, rel) result(roff)
        !! Returns escape sequence to change type size. Argument `n` must be
        !! single digit. The result is not new-line terminated!
        integer,      intent(in)           :: n    !! Absolute or relative size [pt].
        character(*), intent(in)           :: text !! Text.
        character,    intent(in), optional :: rel  !! `+` or `-`.
        character(:), allocatable          :: roff !! Output string.

        character :: rel_
        integer   :: n_

        rel_ = dm_present(rel, ' ')
        n_   = max(1, min(9, n))

        select case (rel_)
            case ('+', '-'); roff = '\s' // rel_ // dm_itoa(n_) // trim(text) // '\s0'
            case default;    roff = '\s' //         dm_itoa(n_) // trim(text) // '\s0'
        end select
    end function dm_roff_s

    pure function dm_roff_sp(distance, unit) result(roff)
        !! Returns request to add vertical spacing.
        integer,   intent(in), optional :: distance !! Vertical space.
        character, intent(in), optional :: unit     !! Distance unit. Uses [vee] is not passed.
        character(:), allocatable       :: roff     !! Output string.

        character :: unit_

        unit_ = dm_present(unit, 'v')

        if (present(distance)) then
            roff = '.sp ' // dm_itoa(distance) // unit_ // NL
        else
            roff = '.sp' // NL
        end if
    end function dm_roff_sp

    pure function dm_roff_tbl_block(text) result(roff)
        !! Returns argument `text` between `T{` and `T}` to create a text block.
        character(*), intent(in)  :: text !! Text.
        character(:), allocatable :: roff !! Output string.

        roff = 'T{' // NL // trim(text) // NL // 'T}'
    end function dm_roff_tbl_block

    ! **************************************************************************
    ! PUBLIC LOW-LEVEL MS MACROS.
    ! **************************************************************************
    pure function dm_roff_ms_ai(institution) result(roff)
        !! Returns macro to add institution of author(s). Multiple institutions
        !! have to be separated by new line.
        character(*), intent(in)  :: institution !! Institution name.
        character(:), allocatable :: roff        !! Output string.

        roff = '.AI' // NL // trim(institution) // NL
    end function dm_roff_ms_ai

    pure function dm_roff_ms_au(author) result(roff)
        !! Returns macro to add author. Multiple authors have to be separated
        !! by new line.
        character(*), intent(in)  :: author !! Author name.
        character(:), allocatable :: roff

        roff = '.AU' // NL // trim(author) // NL
    end function dm_roff_ms_au

    pure function dm_roff_ms_bx(text) result(roff)
        !! Returns macro to draw a box around `text`. This function does not
        !! append a new-line character!
        character(*), intent(in)  :: text !! Text.
        character(:), allocatable :: roff

        roff = '.BX "' // trim(text) // '"'
    end function dm_roff_ms_bx

    pure function dm_roff_ms_ds(name, string) result(roff)
        !! Returns macro to define a string.
        character(*), intent(in)           :: name   !! String name.
        character(*), intent(in), optional :: string !! String contents.
        character(:), allocatable          :: roff   !! Output string.

        if (present(string)) then
            roff = '.ds ' // trim(name) // ' ' // trim(string) // NL
        else
            roff = '.ds ' // trim(name) // NL
        end if
    end function dm_roff_ms_ds

    pure function dm_roff_ms_lp(text) result(roff)
        !! Returns macro to add paragraph (without indent).
        use :: dm_string, only: dm_string_is_present

        character(*), intent(in), optional :: text !! Paragraph text.
        character(:), allocatable          :: roff !! Output string.

        if (dm_string_is_present(text)) then
            roff = '.LP' // NL // trim(text) // NL
        else
            roff = '.LP' // NL // ROFF_ESC_DUMMY // NL
        end if
    end function dm_roff_ms_lp

    pure function dm_roff_ms_nh(level, text) result(roff)
        !! Returns macro to set section heading macro (without number).
        integer,      intent(in)  :: level !! Heading level (depth).
        character(*), intent(in)  :: text  !! Heading text.
        character(:), allocatable :: roff  !! Output string.

        roff = '.NH ' // dm_itoa(level) // NL // trim(text) // NL
    end function dm_roff_ms_nh

    pure function dm_roff_ms_pp(text) result(roff)
        !! Returns macro to add standard paragraph.
        use :: dm_string, only: dm_string_is_present

        character(*), intent(in), optional :: text !! Paragraph text.
        character(:), allocatable          :: roff !! Output string.

        if (dm_string_is_present(text)) then
            roff = '.LP' // NL // trim(text) // NL
        else
            roff = '.LP' // NL // ROFF_ESC_DUMMY // NL
        end if
    end function dm_roff_ms_pp

    pure function dm_roff_ms_sh(level, text) result(roff)
        !! Returns macro to add section heading (without number).
        integer,      intent(in)  :: level !! Heading level (depth).
        character(*), intent(in)  :: text  !! Heading text.
        character(:), allocatable :: roff  !! Output string.

        roff = '.SH ' // dm_itoa(level) // NL // trim(text) // NL
    end function dm_roff_ms_sh

    pure function dm_roff_ms_tl(title) result(roff)
        !! Returns macro to add title.
        character(*), intent(in)  :: title !! Title.
        character(:), allocatable :: roff  !! Output string.

        roff = '.TL' // NL // trim(title) // NL
    end function dm_roff_ms_tl

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function roff_make(device, roff, path, macro, pic, preconv, tbl) result(rc)
        !! Passes the markup string `roff` to _groff(1)_ to create a PDF or PS
        !! file in A4 paper size that is written to `path`. On error, an empty
        !! file may be created.
        !!
        !! By default, this function uses macro package _ms_, unless argument
        !! `macro` is passed. Set `macro` to `ROFF_MACRO_NONE` to use plain
        !! groff.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ERROR` if execution of _groff(1)_ failed.
        !! * `E_INVALID` if `device` or `macro` is invalid.
        !! * `E_IO` if output file could not be created.
        !! * `E_SYSTEM` if system call failed.
        !! * `E_WRITE` if writing failed.
        !!
        use :: dm_file
        use :: dm_pipe
        use :: dm_string

        integer,      intent(in)           :: device  !! Output device (`ROFF_DEVICE_*`).
        character(*), intent(inout)        :: roff    !! Markup string.
        character(*), intent(in)           :: path    !! Path of output file.
        integer,      intent(in), optional :: macro   !! Macro package to use (`ROFF_MACRO_*`).
        logical,      intent(in), optional :: pic     !! Run pic preprocessor.
        logical,      intent(in), optional :: preconv !! Run preconv preprocessor.
        logical,      intent(in), optional :: tbl     !! Run tbl preprocessor.

        character(256)  :: command
        integer         :: macro_, stat
        logical         :: pic_, preconv_, tbl_
        type(pipe_type) :: pipe

        macro_   = dm_present(macro,    ROFF_MACRO_MS)
        pic_     = dm_present(pic,     .false.)
        preconv_ = dm_present(preconv, .false.)
        tbl_     = dm_present(tbl,     .false.)

        rc = E_INVALID
        if (.not. dm_roff_device_is_valid(device)) return
        if (.not. dm_roff_macro_is_valid(macro_))  return

        call dm_file_touch(path, error=rc)
        if (dm_is_error(rc)) return

        command = GROFF_BINARY // ' -dpaper=a4 -T' // ROFF_DEVICE_NAMES(device)

        select case (macro_)
            case (ROFF_MACRO_MS); command = dm_string_append(command, ' -ms')
        end select

        if (pic_)     command = dm_string_append(command, ' -p')
        if (preconv_) command = dm_string_append(command, ' -k')
        if (tbl_)     command = dm_string_append(command, ' -t')

        rc = dm_pipe_open(pipe, trim(command) // ' > ' // trim(path), PIPE_WRONLY)
        if (dm_is_error(rc)) return

        rc = dm_pipe_write(pipe, roff, newline=.false.)
        call dm_pipe_close(pipe, exit_stat=stat)

        if (stat /= 0 .and. dm_is_ok(rc)) rc = E_ERROR
    end function roff_make

    ! **************************************************************************
    ! PRIVATE LOW-LEVEL MS MACROS.
    ! **************************************************************************
    pure function roff_ms_nr_int32(register, value, unit) result(roff)
        !! Returns macro to set register value (4-byte integer).
        character(*), intent(in)           :: register !! Register name.
        integer(i4),  intent(in)           :: value    !! Register value.
        character(*), intent(in), optional :: unit     !! Optional unit.
        character(:), allocatable          :: roff     !! Output string.

        if (present(unit)) then
            roff = '.nr ' // trim(register) // ' ' // dm_itoa(value) // trim(unit) // NL
        else
            roff = '.nr ' // trim(register) // ' ' // dm_itoa(value) // NL
        end if
    end function roff_ms_nr_int32

    pure function roff_ms_nr_real32(register, value, unit) result(roff)
        !! Returns macro to set register value (4-byte real).
        character(*), intent(in)           :: register !! Register name.
        real(r4),     intent(in)           :: value    !! Register value.
        character(*), intent(in), optional :: unit     !! Optional unit.
        character(:), allocatable          :: roff     !! Output string.

        if (present(unit)) then
            roff = '.nr ' // trim(register) // ' ' // dm_ftoa(value, 1) // trim(unit) // NL
        else
            roff = '.nr ' // trim(register) // ' ' // dm_ftoa(value, 1) // NL
        end if
    end function roff_ms_nr_real32
end module dm_roff
