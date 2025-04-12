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
    !! character(len=:), allocatable :: roff
    !! integer                       :: rc
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
    use :: dm_ascii, only: NL => ASCII_LF
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

    character(len=*), parameter :: ROFF_DEVICE_NAMES(ROFF_DEVICE_NONE:ROFF_DEVICE_LAST) = [ &
        character(len=ROFF_DEVICE_NAME_LEN) :: ' ', 'utf8', 'ps', 'pdf', 'html' &
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

    character(len=*), parameter :: ROFF_FONT_NAMES(ROFF_FONT_NONE:ROFF_FONT_LAST) = [ &
        character(len=ROFF_FONT_NAME_LEN) :: 'T', 'A', 'BM', 'H', 'HN', 'N', 'P', 'T', 'ZCM' &
    ] !! Font family names.

    ! Groff requests.
    character(len=*), parameter, public :: ROFF_REQUEST_BP = '.bp' // NL !! Break page.
    character(len=*), parameter, public :: ROFF_REQUEST_P1 = '.P1' // NL !! Typeset header on page 1 (ms).

    character(len=*), parameter, public :: ROFF_ENCODING_UTF8 = '.\" -*- mode: troff; coding: utf-8 -*-' // NL !! UTF-8 encoding for preconv.

    ! Executables.
    character(len=*), parameter :: GROFF_BINARY  = 'groff'
    character(len=*), parameter :: PS2PDF_BINARY = 'ps2pdf'

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

    ! Public high-level macros.
    public :: dm_roff_ms_header

    ! Public low-level macros.
    public :: dm_roff_pspic ! EPS picture.

    public :: dm_roff_ms_ai ! Author institution.
    public :: dm_roff_ms_au ! Author name.
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
        !! * `E_ERROR` if executing _ps2pdf(1)_ failed.
        !! * `E_IO` if output file could not be created.
        !! * `E_NOT_FOUND` if input file does not exist.
        !!
        use :: dm_file, only: dm_file_exists, dm_file_touch

        character(len=*), intent(in) :: input  !! Path of PostScript file.
        character(len=*), intent(in) :: output !! Path of PDF file.

        integer :: stat

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(input)) return

        call dm_file_touch(output, error=rc)
        if (dm_is_error(rc)) return

        call execute_command_line(PS2PDF_BINARY // ' ' // trim(input) // ' ' // trim(output), exitstat=stat)
        if (stat /= 0) rc = E_ERROR
    end function dm_roff_ps_to_pdf

    integer function dm_roff_to_pdf(roff, path, macro, pic, preconv) result(rc)
        !! Passes the markup string `roff` to _groff(1)_ to create a PDF file
        !! that is written to `path`. An existing file will not be replaced. On
        !! error, an empty file may still be created. By default, this function
        !! uses macro package _ms_, unless argument `macro` is passed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ERROR` if executing _groff(1)_ failed.
        !! * `E_EXIST` if output file at `path` exists.
        !! * `E_INVALID` if `macro` is invalid.
        !! * `E_IO` if output file could not be created.
        !! * `E_SYSTEM` if system call failed.
        !! * `E_WRITE` if writing failed.
        !!
        character(len=*), intent(inout)        :: roff    !! Markup string.
        character(len=*), intent(in)           :: path    !! Path of output file.
        integer,          intent(in), optional :: macro   !! Macro package to use (`ROFF_MACRO_*`).
        logical,          intent(in), optional :: pic     !! Run pic preprocessor.
        logical,          intent(in), optional :: preconv !! Run preconv preprocessor.

        rc = roff_make(ROFF_DEVICE_PDF, roff, path, macro, pic, preconv)
    end function dm_roff_to_pdf

    integer function dm_roff_to_ps(roff, path, macro, pic, preconv) result(rc)
        !! Passes the markup string `roff` to _groff(1)_ to create a PostScript
        !! file that is written to `path`. An existing file will not be
        !! replaced. On error, an empty file may still be created. By default,
        !! this function uses macro package _ms_, unless argument `macro` is
        !! passed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ERROR` if executing _groff(1)_ failed.
        !! * `E_EXIST` if output file at `path` exists.
        !! * `E_INVALID` if `macro` is invalid.
        !! * `E_IO` if output file could not be created.
        !! * `E_SYSTEM` if system call failed.
        !! * `E_WRITE` if writing failed.
        !!
        character(len=*), intent(inout)        :: roff    !! Markup string.
        character(len=*), intent(in)           :: path    !! Path of output file.
        integer,          intent(in), optional :: macro   !! Macro package to use (`ROFF_MACRO_*`).
        logical,          intent(in), optional :: pic     !! Run pic preprocessor.
        logical,          intent(in), optional :: preconv !! Run preconv preprocessor.

        rc = roff_make(ROFF_DEVICE_PS, roff, path, macro, pic, preconv)
    end function dm_roff_to_ps

    ! **************************************************************************
    ! PUBLIC HIGH-LEVEL MACROS.
    ! **************************************************************************
    pure function dm_roff_ms_header(title, author, institution, font_family, font_size, left_header, &
                                    center_header, right_header, left_footer, center_footer, right_footer, &
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
        character(len=*), intent(in), optional :: title         !! Document title.
        character(len=*), intent(in), optional :: author        !! Author name.
        character(len=*), intent(in), optional :: institution   !! Institution name.
        integer,          intent(in), optional :: font_family   !! Font family enumerator (`ROFF_FONT_*`).
        integer,          intent(in), optional :: font_size     !! Font size in pt.
        character(len=*), intent(in), optional :: left_header   !! Left header content.
        character(len=*), intent(in), optional :: center_header !! Center header content.
        character(len=*), intent(in), optional :: right_header  !! Right header content.
        character(len=*), intent(in), optional :: left_footer   !! Left footer content.
        character(len=*), intent(in), optional :: center_footer !! Center footer content.
        character(len=*), intent(in), optional :: right_footer  !! Right footer content.
        logical,          intent(in), optional :: page_one      !! Enable header on page 1.
        character(len=:), allocatable          :: roff          !! Output string.

        character(len=3) :: fam
        integer          :: ff, ps
        logical          :: p1

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
               dm_roff_ms_nr('HM',     1,   'c') // & ! Header margin [cm].
               dm_roff_ms_nr('FM',     3,   'c') // & ! Footer margin [cm].
               dm_roff_ms_nr('PO',     2,   'c') // & ! Left margin [cm].
               dm_roff_ms_nr('LL',     17,  'c')      ! Line length [cm].

        if (p1) roff = roff // ROFF_REQUEST_P1 ! Header on page 1.

        ! Header.
        if (present(left_header))  roff = roff // dm_roff_ms_ds('LH', left_header)
        if (present(right_header)) roff = roff // dm_roff_ms_ds('RH', right_header)

        if (present(center_header)) then
            roff = roff // dm_roff_ms_ds('CH', center_header)
        else
            roff = roff // dm_roff_ms_ds('CH')
        end if

        ! Footer.
        if (present(left_footer))  roff = roff // dm_roff_ms_ds('LF', left_footer)
        if (present(right_footer)) roff = roff // dm_roff_ms_ds('RF', right_footer)

        if (present(center_footer)) then
            roff = roff // dm_roff_ms_ds('CF', center_footer)
        else
            roff = roff // dm_roff_ms_ds('CF', '%')
        end if

        if (present(title))       roff = roff // dm_roff_ms_tl(title)
        if (present(author))      roff = roff // dm_roff_ms_au(author)
        if (present(institution)) roff = roff // dm_roff_ms_ai(institution)
    end function dm_roff_ms_header

    ! **************************************************************************
    ! PUBLIC LOW-LEVEL MACROS.
    ! **************************************************************************
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

        character(len=*), intent(in)           :: path   !! Path to EPS file.
        character,        intent(in), optional :: align  !! Image alignment (`L`, `R`, `C`).
        real,             intent(in), optional :: width  !! Image width [cm].
        real,             intent(in), optional :: height !! Image height [cm].
        character(len=:), allocatable          :: roff   !! Output string.

        character(len=16) :: d
        character         :: a
        real              :: w, h

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

    ! **************************************************************************
    ! PUBLIC LOW-LEVEL MS MACROS.
    ! **************************************************************************
    pure function dm_roff_ms_ai(institution) result(roff)
        !! Returns macro to set institution (of author).
        character(len=*), intent(in)  :: institution !! Institution name.
        character(len=:), allocatable :: roff        !! Output string.

        roff = '.AI' // NL // trim(institution) // NL
    end function dm_roff_ms_ai

    pure function dm_roff_ms_au(author) result(roff)
        !! Returns macro to set author.
        character(len=*), intent(in)  :: author !! Author name.
        character(len=:), allocatable :: roff

        roff = '.AU' // NL // trim(author) // NL
    end function dm_roff_ms_au

    pure function dm_roff_ms_ds(name, string) result(roff)
        !! Returns command to define a string.
        character(len=*), intent(in)           :: name   !! String name.
        character(len=*), intent(in), optional :: string !! String contents.
        character(len=:), allocatable          :: roff   !! Output string.

        if (present(string)) then
            roff = '.ds ' // trim(name) // ' ' // trim(string) // NL
        else
            roff = '.ds ' // trim(name) // NL
        end if
    end function dm_roff_ms_ds

    pure function dm_roff_ms_lp(text) result(roff)
        !! Returns command to add paragraph (without indent).
        character(len=*), intent(in), optional :: text !! Paragraph text.
        character(len=:), allocatable          :: roff !! Output string.

        if (present(text)) then
            roff = '.LP' // NL // trim(text) // NL
        else
            roff = '.LP' // NL
        end if
    end function dm_roff_ms_lp

    pure function dm_roff_ms_nh(level, text) result(roff)
        !! Returns command to set section heading macro (without number).
        integer,          intent(in)  :: level !! Heading level (depth).
        character(len=*), intent(in)  :: text  !! Heading text.
        character(len=:), allocatable :: roff  !! Output string.

        roff = '.NH ' // dm_itoa(level) // NL // trim(text) // NL
    end function dm_roff_ms_nh

    pure function dm_roff_ms_pp(text) result(roff)
        !! Returns command to add standard paragraph.
        character(len=*), intent(in), optional :: text !! Paragraph text.
        character(len=:), allocatable          :: roff !! Output string.

        if (present(text)) then
            roff = '.LP' // NL // trim(text) // NL
        else
            roff = '.LP' // NL
        end if
    end function dm_roff_ms_pp

    pure function dm_roff_ms_sh(level, text) result(roff)
        !! Returns command to set section heading macro (without number).
        integer,          intent(in)  :: level !! Heading level (depth).
        character(len=*), intent(in)  :: text  !! Heading text.
        character(len=:), allocatable :: roff  !! Output string.

        roff = '.SH ' // dm_itoa(level) // NL // trim(text) // NL
    end function dm_roff_ms_sh

    pure function dm_roff_ms_tl(title) result(roff)
        !! Returns command to set title.
        character(len=*), intent(in)  :: title !! Title.
        character(len=:), allocatable :: roff  !! Output string.

        roff = '.TL' // NL // trim(title) // NL
    end function dm_roff_ms_tl

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function roff_make(device, roff, path, macro, pic, preconv) result(rc)
        !! Passes the markup string `roff` to _groff(1)_ to create a PDF or PS
        !! file in A4 paper size that is written to `path`. An existing file
        !! will not be replaced. On error, an empty file may still be created.
        !!
        !! By default, this function uses macro package _ms_, unless argument
        !! `macro` is passed. Set `macro` to `ROFF_MACRO_NONE` to use plain
        !! groff.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ERROR` if executing _groff(1)_ failed.
        !! * `E_EXIST` if output file at `path` exists.
        !! * `E_INVALID` if `device` or `macro` is invalid.
        !! * `E_IO` if output file could not be created.
        !! * `E_SYSTEM` if system call failed.
        !! * `E_WRITE` if writing failed.
        !!
        use :: dm_file
        use :: dm_pipe
        use :: dm_string

        integer,          intent(in)           :: device  !! Output device (`ROFF_DEVICE_*`).
        character(len=*), intent(inout)        :: roff    !! Markup string.
        character(len=*), intent(in)           :: path    !! Path of output file.
        integer,          intent(in), optional :: macro   !! Macro package to use (`ROFF_MACRO_*`).
        logical,          intent(in), optional :: pic     !! Run pic preprocessor.
        logical,          intent(in), optional :: preconv !! Run preconv preprocessor.

        character(len=256) :: command
        integer            :: macro_, stat
        logical            :: pic_, preconv_
        type(pipe_type)    :: pipe

        macro_   = dm_present(macro,    ROFF_MACRO_MS)
        pic_     = dm_present(pic,     .false.)
        preconv_ = dm_present(preconv, .false.)

        rc = E_INVALID
        if (.not. dm_roff_device_is_valid(device)) return
        if (.not. dm_roff_macro_is_valid(macro_))  return

        rc = E_EXIST
        if (dm_file_exists(path)) return

        call dm_file_touch(path, error=rc)
        if (dm_is_error(rc)) return

        command = GROFF_BINARY // ' -dpaper=a4 -T' // ROFF_DEVICE_NAMES(device)

        select case (macro_)
            case (ROFF_MACRO_MS); command = dm_string_append(command, ' -ms')
        end select

        if (pic_)     command = dm_string_append(command, ' -p')
        if (preconv_) command = dm_string_append(command, ' -k')

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
        !! Returns command to set register value (4-byte integer).
        character(len=*), intent(in)           :: register !! Register name.
        integer(kind=i4), intent(in)           :: value    !! Register value.
        character(len=*), intent(in), optional :: unit     !! Optional unit.
        character(len=:), allocatable          :: roff     !! Output string.

        if (present(unit)) then
            roff = '.nr ' // trim(register) // ' ' // dm_itoa(value) // trim(unit) // NL
        else
            roff = '.nr ' // trim(register) // ' ' // dm_itoa(value) // NL
        end if
    end function roff_ms_nr_int32

    pure function roff_ms_nr_real32(register, value, unit) result(roff)
        !! Returns command to set register value (4-byte real).
        character(len=*), intent(in)           :: register !! Register name.
        real(kind=r4),    intent(in)           :: value    !! Register value.
        character(len=*), intent(in), optional :: unit     !! Optional unit.
        character(len=:), allocatable          :: roff     !! Output string.

        character(len=8) :: string

        write (string, '(f0.1)') value

        if (present(unit)) then
            roff = '.nr ' // trim(register) // ' ' // trim(string) // trim(unit) // NL
        else
            roff = '.nr ' // trim(register) // ' ' // trim(string) // NL
        end if
    end function roff_ms_nr_real32
end module dm_roff
