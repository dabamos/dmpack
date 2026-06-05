! Author:  Philipp Engel
! Licence: ISC
module dm_atom
    !! Generator for the Atom Syndication Format (RFC 4287), to create a
    !! web feed of log messages in XML format, with optional XSLT style sheet.
    use :: dm_ascii, only: NL => ASCII_LF
    use :: dm_kind
    use :: dm_log
    use :: dm_time
    use :: dm_util
    use :: dm_uuid
    use :: dm_version
    use :: dm_xml
    implicit none (type, external)
    private

    ! Default values.
    integer,      parameter :: ATOM_ID_LEN           = 36
    character(*), parameter :: ATOM_ID_DEFAULT       = '00000000-0000-0000-0000-000000000000'
    character(*), parameter :: ATOM_TITLE_DEFAULT    = 'DMPACK Logs'
    character(*), parameter :: ATOM_SUBTITLE_DEFAULT = 'Log Messages Feed'

    ! Atom Syndication Format syntax.
    character(*), parameter :: A_AUTHOR        = '<author>' // NL
    character(*), parameter :: A_AUTHOR_END    = '</author>' // NL
    character(*), parameter :: A_CONTENT       = '<content>' // NL
    character(*), parameter :: A_CONTENT_END   = '</content>' // NL
    character(*), parameter :: A_CONTENT_XHTML = '<content type="xhtml">' // NL
    character(*), parameter :: A_DIV           = '<div xmlns="http://www.w3.org/1999/xhtml">' // NL
    character(*), parameter :: A_DIV_END       = '</div>' // NL
    character(*), parameter :: A_EMAIL         = '<email>'
    character(*), parameter :: A_EMAIL_END     = '</email>' // NL
    character(*), parameter :: A_ENTRY         = '<entry>' // NL
    character(*), parameter :: A_ENTRY_END     = '</entry>' // NL
    character(*), parameter :: A_FEED          = '<feed xmlns="http://www.w3.org/2005/Atom">' // NL
    character(*), parameter :: A_FEED_END      = '</feed>'
    character(*), parameter :: A_GENERATOR     = '<generator version="' // DM_VERSION_STRING // '">DMPACK</generator>' // NL
    character(*), parameter :: A_ID            = '<id>'
    character(*), parameter :: A_ID_END        = '</id>' // NL
    character(*), parameter :: A_NAME          = '<name>'
    character(*), parameter :: A_NAME_END      = '</name>' // NL
    character(*), parameter :: A_PUBLISHED     = '<published>'
    character(*), parameter :: A_PUBLISHED_END = '</published>' // NL
    character(*), parameter :: A_SUBTITLE      = '<subtitle>'
    character(*), parameter :: A_SUBTITLE_END  = '</subtitle>' // NL
    character(*), parameter :: A_SUMMARY       = '<summary>'
    character(*), parameter :: A_SUMMARY_END   = '</summary>' // NL
    character(*), parameter :: A_TITLE         = '<title>'
    character(*), parameter :: A_TITLE_END     = '</title>' // NL
    character(*), parameter :: A_UPDATED       = '<updated>'
    character(*), parameter :: A_UPDATED_END   = '</updated>' // NL

    type, public :: atom_type
        !! Atom feed attributes.
        character(256)         :: alt      = ' '          !! Alternate content link.
        character(256)         :: author   = ' '          !! Author name.
        character(256)         :: email    = ' '          !! Author e-mail.
        character(ATOM_ID_LEN) :: id       = ' '          !! Feed id.
        character(TIME_LEN)    :: updated  = TIME_DEFAULT !! Feed time stamp.
        character(256)         :: title    = ' '          !! Feed title.
        character(256)         :: subtitle = ' '          !! Feed sub-title.
        character(512)         :: url      = ' '          !! Feed URL.
        character(512)         :: xsl      = ' '          !! Path or URL of XSLT style sheet.
    end type atom_type

    interface atom_write_entry
        !! Generic XML entry writer.
        module procedure :: atom_write_entry_log
    end interface atom_write_entry

    interface dm_atom_write
        !! XML writer procedure.
        module procedure :: dm_atom_write_logs
    end interface dm_atom_write

    public :: dm_atom_write
    public :: dm_atom_write_logs

    private :: atom_link
    private :: atom_style_sheet
    private :: atom_write_entry
    private :: atom_write_entry_log
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    subroutine dm_atom_write_logs(atom, logs, unit)
        !! Writes log messages in Atom Syndication Format (RFC 4287) to file or
        !! standard output.
        !!
        !! The `atom%alt` value shall be of the form `http://www.example.com/dmpack/log?log_id=`.
        !! The particular log id will be appended to the URL. The feed identification `atom%id`
        !! shall be a valid UUID of the form `00000000-0000-0000-0000-000000000000`. The
        !! parameter `atom%url` shall be the public URL of the Atom feed.
        type(atom_type), intent(in)           :: atom    !! Atom type.
        type(log_type),  intent(in)           :: logs(:) !! Log array.
        integer,         intent(in), optional :: unit    !! Output unit.

        integer :: alt_len, author_len, email_len
        integer :: i, unit_

        unit_ = dm_present(unit, STDOUT)

        ! Feed header.
        write (unit_, '(a)') XML_HEADER

        ! Add link to XSLT style sheet.
        if (len_trim(atom%xsl) > 0) write (unit_, '(a)', advance='no') atom_style_sheet(atom%xsl)

        ! Start of feed.
        write (unit_, '(2a)', advance='no') A_FEED, A_GENERATOR

        ! Feed title.
        if (len_trim(atom%title) > 0) then
            write (unit_, '(3a)', advance='no') A_TITLE, dm_xml_encode(atom%title), A_TITLE_END
        else
            write (unit_, '(3a)', advance='no') A_TITLE, ATOM_TITLE_DEFAULT, A_TITLE_END
        end if

        ! Feed subtitle.
        if (len_trim(atom%subtitle) > 0) then
            write (unit_, '(3a)', advance='no') A_SUBTITLE, dm_xml_encode(atom%subtitle), A_SUBTITLE_END
        else
            write (unit_, '(3a)', advance='no') A_SUBTITLE, ATOM_SUBTITLE_DEFAULT, A_SUBTITLE_END
        end if

        ! Feed URL.
        if (len_trim(atom%url) > 0) write (unit_, '(a)', advance='no') atom_link(atom%url, rel='self')

        ! Feed ID and time stamp.
        if (len_trim(atom%id) == 0) then
            write (unit_, '(a, "urn:uuid:", 2a)', advance='no') A_ID, ATOM_ID_DEFAULT, A_ID_END
        else
            write (unit_, '(a, "urn:uuid:", 2a)', advance='no') A_ID, dm_xml_encode(atom%id), A_ID_END
        end if

        ! Feed time stamp
        if (len_trim(atom%updated) == 0 .or. atom%updated == TIME_DEFAULT) then
            write (unit_, '(3a)', advance='no') A_UPDATED, dm_time_strip(dm_time_now()), A_UPDATED_END
        else
            write (unit_, '(3a)', advance='no') A_UPDATED, dm_time_strip(atom%updated), A_UPDATED_END
        end if

        ! Feed author
        author_len = len_trim(atom%author)
        email_len  = len_trim(atom%email)

        if (author_len > 0 .or. email_len > 0) then
            write (unit_, '(a)', advance='no') A_AUTHOR
            if (author_len > 0) write (unit_, '(3a)', advance='no') A_NAME, dm_xml_encode(atom%author), A_NAME_END
            if (email_len  > 0) write (unit_, '(3a)', advance='no') A_EMAIL, dm_xml_encode(atom%email), A_EMAIL_END
            write (unit_, '(a)', advance='no') A_AUTHOR_END
        end if

        ! Feed entries.
        alt_len = len_trim(atom%alt)

        do i = 1, size(logs)
            if (alt_len > 0) then
                call atom_write_entry(logs(i), alt=trim(atom%alt) // trim(logs(i)%id), unit=unit_)
            else
                call atom_write_entry(logs(i), unit=unit_)
            end if
        end do

        ! Feed footer.
        write (unit_, '(a)', advance='no') A_FEED_END
    end subroutine dm_atom_write_logs

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    pure function atom_link(href, rel, type) result(xml)
        !! Returns an Atom link, for example:
        !!
        !! ``` html
        !! <link href="http://example.org/feed/" rel="self" />
        !! <link href="http://example.org/" type="text/html" />
        !! ```
        character(*), intent(in)           :: href !! Link attribute `href` (URL).
        character(*), intent(in), optional :: rel  !! Link attribute `rel` (`self`).
        character(*), intent(in), optional :: type !! Link attribute `type` (MIME type).
        character(:), allocatable          :: xml  !! Atom XML string.

        xml = '<link href="' // dm_xml_encode(href) // '"'
        if (present(rel))  xml = xml // ' rel="'  // dm_xml_encode(rel)  // '"'
        if (present(type)) xml = xml // ' type="' // dm_xml_encode(type) // '"'
        xml = xml // ' />' // NL
    end function atom_link

    function atom_style_sheet(path) result(xml)
        !! Returns `xml-stylesheet` tag with link to XSLT template.
        character(*), intent(in)  :: path !! Path to XSLT style sheet.
        character(:), allocatable :: xml  !! Atom XML string.

        xml = '<?xml-stylesheet href="' // dm_xml_encode(path) // '" type="text/xsl"?>' // NL
    end function atom_style_sheet

    subroutine atom_write_entry_log(log, alt, unit)
        !! Returns an Atom entry from given log message.
        use :: dm_html, only: dm_html_log

        type(log_type), intent(in)           :: log  !! Log type.
        character(*),   intent(in), optional :: alt  !! URL to alternate content (HTML of log).
        integer,        intent(in), optional :: unit !! Output unit.

        integer :: level, unit_

        unit_ = dm_present(unit, STDOUT)

        level = max(LL_NONE, min(LL_LAST, log%level))

        ! Atom entry.
        write (unit_, '(6a)', advance='no')              &
            A_ENTRY, A_TITLE,                            &
            dm_xml_encode(LOG_LEVEL_NAMES(level)), ': ', &
            dm_xml_encode(log%message),                  &
            A_TITLE_END

        ! Alternate link.
        if (present(alt)) write (unit_, '(a)', advance='no') atom_link(alt, rel='alternate', type='text/html')

        ! Atom entry content.
        write (unit_, '(26a)', advance='no')                                               &
            A_ID,        'urn:uuid:', dm_uuid_hyphenize(log%id), A_ID_END,                 &
            A_PUBLISHED, dm_time_strip(log%timestamp),           A_PUBLISHED_END,          &
            A_UPDATED,   dm_time_strip(log%timestamp),           A_UPDATED_END,            &
            A_SUMMARY,                                                                     &
                dm_xml_encode(LOG_LEVEL_NAMES(level)), ': ',                               &
                dm_xml_encode(log%message),                                                &
            A_SUMMARY_END,                                                                 &
            A_CONTENT_XHTML, A_DIV,  dm_html_log(log),          A_DIV_END,  A_CONTENT_END, &
            A_AUTHOR,        A_NAME, dm_xml_encode(log%source), A_NAME_END, A_AUTHOR_END,  &
            A_ENTRY_END
    end subroutine atom_write_entry_log
end module dm_atom
