! Author:  Philipp Engel
! Licence: ISC
module dm_roff
    !! Module for creating formatted output in GNU roff (with macro package
    !! _ms_).
    use :: dm_ascii, only: NL => ASCII_LF
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! Encoding comment for preconv.
    character(len=*), parameter, public :: ROFF_ENCODING_UTF8 = '.\" -*- mode: troff; coding: utf-8 -*-'

    ! Font families.
    integer, parameter, public :: ROFF_FONT_NONE                   = 0 !! Default font.
    integer, parameter, public :: ROFF_FONT_AVANT_GARDE            = 1 !! Avant Garde.
    integer, parameter, public :: ROFF_FONT_BOOKMAN                = 2 !! Bookman.
    integer, parameter, public :: ROFF_FONT_HELVETICA              = 3 !! Helvetica.
    integer, parameter, public :: ROFF_FONT_HELVETICA_NARROW       = 4 !! Helvetica Narrow.
    integer, parameter, public :: ROFF_FONT_NEW_CENTURY_SCHOOLBOOK = 5 !! New Century Schoolbook.
    integer, parameter, public :: ROFF_FONT_PALATINO               = 6 !! Palatino.
    integer, parameter, public :: ROFF_FONT_TIMES_ROMAN            = 7 !! Times Roman (default font).
    integer, parameter, public :: ROFF_FONT_ZAPF_CHANCERY          = 8 !! Zapf Chancery (italic only).
    integer, parameter, public :: ROFF_FONT_LAST                   = 8 !! Never use this.

    character(len=3), parameter :: ROFF_FONT_NAMES(ROFF_FONT_NONE:ROFF_FONT_LAST) = [ &
        character(len=3) :: 'T', 'A', 'BM', 'H', 'HN', 'N', 'P', 'T', 'ZCM' &
    ] !! Font families.

    interface dm_roff_nr
        !! Generic function to set register value.
        module procedure :: roff_nr_int32
        module procedure :: roff_nr_real32
    end interface dm_roff_nr

    ! Public high-level functions.
    public :: dm_roff_header

    ! Public low-level functions.
    public :: dm_roff_ai ! Author institution.
    public :: dm_roff_au ! Author name.
    public :: dm_roff_ds ! Define string.
    public :: dm_roff_lp ! Paragraph without indent.
    public :: dm_roff_nh ! Numbered heading.
    public :: dm_roff_nr ! Set register value.
    public :: dm_roff_pp ! Standard paragraph with indent.
    public :: dm_roff_sh ! Section heading (without number).
    public :: dm_roff_tl ! Title.

    ! Private low-level functions.
    private :: roff_nr_int32
    private :: roff_nr_real32
contains
    ! **************************************************************************
    ! PUBLIC HIGH-LEVEL FUNCTIONS.
    ! **************************************************************************
    pure function dm_roff_header(title, author, institution, font_family, font_size) result(roff)
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
        !! The return value is not newline-terminated. Append a macro (heading,
        !! paragraph) to the output of this function to create a valid
        !! _groff(1)_ document.
        character(len=*), intent(in), optional :: title       !! Document title.
        character(len=*), intent(in), optional :: author      !! Author name.
        character(len=*), intent(in), optional :: institution !! Institution name.
        integer,          intent(in), optional :: font_family !! Font family enumerator (`ROFF_FONT_*`).
        integer,          intent(in), optional :: font_size   !! Font size in pt.
        character(len=:), allocatable          :: roff        !! Output string.

        character(len=3) :: font
        integer          :: ff, fs

        ff = dm_present(font_family, ROFF_FONT_NONE)
        fs = dm_present(font_size,   10)

        if (ff < ROFF_FONT_NONE .or. ff > ROFF_FONT_LAST) ff = ROFF_FONT_NONE
        font = ROFF_FONT_NAMES(ff)

        roff = ROFF_ENCODING_UTF8                   // & ! Encoding for preconv (groff argument -k).
               NL // dm_roff_ds('FAM',    font)     // & ! Font family.
               NL // dm_roff_nr('PS',     fs)       // & ! Font size [pt].
               NL // dm_roff_nr('GROWPS', 3)        // & ! Heading depth.
               NL // dm_roff_nr('PSINCR', 1.5, 'p') // & ! Heading increment [pt].
               NL // dm_roff_nr('HM',     1,   'c') // & ! Header margin [cm].
               NL // dm_roff_nr('FM',     3,   'c') // & ! Footer margin [cm].
               NL // dm_roff_nr('PO',     2,   'c') // & ! Left margin [cm].
               NL // dm_roff_nr('LL',     17,  'c') // & ! Line length [cm].
               NL // dm_roff_ds('CH')               // & ! Center header.
               NL // dm_roff_ds('CF',     '%')           ! Center footer.

        if (present(title))       roff = roff // NL // dm_roff_tl(trim(title))       ! Title.
        if (present(author))      roff = roff // NL // dm_roff_au(trim(author))      ! Author.
        if (present(institution)) roff = roff // NL // dm_roff_ai(trim(institution)) ! Institution.
    end function dm_roff_header

    ! **************************************************************************
    ! PUBLIC LOW-LEVEL FUNCTIONS.
    ! **************************************************************************
    pure function dm_roff_ai(institution) result(roff)
        !! Returns macro to set institution (of author).
        character(len=*), intent(in)  :: institution !! Institution name.
        character(len=:), allocatable :: roff        !! Output string.

        roff = '.AI' // NL // trim(institution)
    end function dm_roff_ai

    pure function dm_roff_au(author) result(roff)
        !! Returns macro to set author.
        character(len=*), intent(in)  :: author !! Author name.
        character(len=:), allocatable :: roff

        roff = '.AU' // NL // trim(author)
    end function dm_roff_au

    pure function dm_roff_ds(name, string) result(roff)
        !! Returns command to define a string.
        character(len=*), intent(in)           :: name   !! String name.
        character(len=*), intent(in), optional :: string !! String contents.
        character(len=:), allocatable          :: roff   !! Output string.

        if (present(string)) then
            roff = '.ds ' // trim(name) // ' ' // trim(string)
        else
            roff = '.ds ' // trim(name)
        end if
    end function dm_roff_ds

    pure function dm_roff_lp(text) result(roff)
        !! Returns command to add paragraph (without indent).
        character(len=*), intent(in), optional :: text !! Paragraph text.
        character(len=:), allocatable          :: roff !! Output string.

        if (present(text)) then
            roff = '.LP' // NL // trim(text)
        else
            roff = '.LP'
        end if
    end function dm_roff_lp

    pure function dm_roff_nh(level, text) result(roff)
        !! Returns command to set section heading macro (without number).
        integer,          intent(in)  :: level !! Heading level (depth).
        character(len=*), intent(in)  :: text  !! Heading text.
        character(len=:), allocatable :: roff  !! Output string.

        roff = '.NH' // dm_itoa(level) // NL // trim(text)
    end function dm_roff_nh

    pure function dm_roff_pp(text) result(roff)
        !! Returns command to add standard paragraph.
        character(len=*), intent(in), optional :: text !! Paragraph text.
        character(len=:), allocatable          :: roff !! Output string.

        if (present(text)) then
            roff = '.LP' // NL // trim(text)
        else
            roff = '.LP'
        end if
    end function dm_roff_pp

    pure function dm_roff_sh(level, text) result(roff)
        !! Returns command to set section heading macro (without number).
        integer,          intent(in)  :: level !! Heading level (depth).
        character(len=*), intent(in)  :: text  !! Heading text.
        character(len=:), allocatable :: roff  !! Output string.

        roff = '.SH' // dm_itoa(level) // NL // trim(text)
    end function dm_roff_sh

    pure function dm_roff_tl(title) result(roff)
        !! Returns command to set title.
        character(len=*), intent(in)  :: title !! Title.
        character(len=:), allocatable :: roff  !! Output string.

        roff = '.TL' // NL // trim(title)
    end function dm_roff_tl

    ! **************************************************************************
    ! PRIVATE LOW-LEVEL FUNCTIONS.
    ! **************************************************************************
    pure function roff_nr_int32(register, value, unit) result(roff)
        !! Returns command to set register value (4-byte integer).
        character(len=*), intent(in)           :: register !! Register name.
        integer(kind=i4), intent(in)           :: value    !! Register value.
        character(len=*), intent(in), optional :: unit     !! Optional unit.
        character(len=:), allocatable          :: roff     !! Output string.

        if (present(unit)) then
            roff = '.nr ' // trim(register) // ' ' // dm_itoa(value) // trim(unit)
        else
            roff = '.nr ' // trim(register) // ' ' // dm_itoa(value)
        end if
    end function roff_nr_int32

    pure function roff_nr_real32(register, value, unit) result(roff)
        !! Returns command to set register value (4-byte real).
        character(len=*), intent(in)           :: register !! Register name.
        real(kind=r4),    intent(in)           :: value    !! Register value.
        character(len=*), intent(in), optional :: unit     !! Optional unit.
        character(len=:), allocatable          :: roff     !! Output string.

        character(len=8) :: string

        write (string, '(f0.1)') value

        if (present(unit)) then
            roff = '.nr ' // trim(register) // ' ' // trim(string) // trim(unit)
        else
            roff = '.nr ' // trim(register) // ' ' // trim(string)
        end if
    end function roff_nr_real32
end module dm_roff
