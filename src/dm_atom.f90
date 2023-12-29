! Author:  Philipp Engel
! Licence: ISC
module dm_atom
    !! Generator for the Atom Syndication Format (RFC 4287), to create a
    !! web feed of log messages in XML format, with optional XSLT style sheet.
    use :: dm_ascii, only: NL => ASCII_LF
    use :: dm_html
    use :: dm_log
    use :: dm_time
    use :: dm_uuid
    use :: dm_version
    implicit none (type, external)
    private

    ! Default values.
    integer,          parameter :: ATOM_ID_LEN           = 36
    character(len=*), parameter :: ATOM_ID_DEFAULT       = '00000000-0000-0000-0000-000000000000'
    character(len=*), parameter :: ATOM_TITLE_DEFAULT    = 'DMPACK Logs'
    character(len=*), parameter :: ATOM_SUBTITLE_DEFAULT = 'Log Messages Feed'

    ! Atom Syndication Format syntax.
    character(len=*), parameter :: A_AUTHOR        = '<author>' // NL
    character(len=*), parameter :: A_AUTHOR_END    = '</author>' // NL
    character(len=*), parameter :: A_CONTENT       = '<content>' // NL
    character(len=*), parameter :: A_CONTENT_END   = '</content>' // NL
    character(len=*), parameter :: A_CONTENT_XHTML = '<content type="xhtml">' // NL
    character(len=*), parameter :: A_DIV           = '<div xmlns="http://www.w3.org/1999/xhtml">' // NL
    character(len=*), parameter :: A_DIV_END       = '</div>' // NL
    character(len=*), parameter :: A_EMAIL         = '<email>'
    character(len=*), parameter :: A_EMAIL_END     = '</email>' // NL
    character(len=*), parameter :: A_ENTRY         = '<entry>' // NL
    character(len=*), parameter :: A_ENTRY_END     = '</entry>' // NL
    character(len=*), parameter :: A_FEED          = '<feed xmlns="http://www.w3.org/2005/Atom">' // NL
    character(len=*), parameter :: A_FEED_END      = '</feed>'
    character(len=*), parameter :: A_GENERATOR     = '<generator version="' // DM_VERSION_STRING // '">DMPACK</generator>' // NL
    character(len=*), parameter :: A_ID            = '<id>'
    character(len=*), parameter :: A_ID_END        = '</id>' // NL
    character(len=*), parameter :: A_NAME          = '<name>'
    character(len=*), parameter :: A_NAME_END      = '</name>' // NL
    character(len=*), parameter :: A_PUBLISHED     = '<published>'
    character(len=*), parameter :: A_PUBLISHED_END = '</published>' // NL
    character(len=*), parameter :: A_SUBTITLE      = '<subtitle>'
    character(len=*), parameter :: A_SUBTITLE_END  = '</subtitle>' // NL
    character(len=*), parameter :: A_SUMMARY       = '<summary>'
    character(len=*), parameter :: A_SUMMARY_END   = '</summary>' // NL
    character(len=*), parameter :: A_TITLE         = '<title>'
    character(len=*), parameter :: A_TITLE_END     = '</title>' // NL
    character(len=*), parameter :: A_UPDATED       = '<updated>'
    character(len=*), parameter :: A_UPDATED_END   = '</updated>' // NL
    character(len=*), parameter :: A_XML           = '<?xml version="1.0" encoding="utf-8"?>' // NL

    type, public :: atom_type
        !! Atom feed attributes.
        character(len=256)         :: alt      = ' ' !! Alternate content link.
        character(len=256)         :: author   = ' ' !! Author name.
        character(len=256)         :: email    = ' ' !! Author e-mail.
        character(len=ATOM_ID_LEN) :: id       = ' ' !! Feed id.
        character(len=256)         :: title    = ' ' !! Feed title.
        character(len=256)         :: subtitle = ' ' !! Feed sub-title.
        character(len=512)         :: url      = ' ' !! Feed URL.
        character(len=512)         :: xsl      = ' ' !! Path or URL of XSLT style sheet.
    end type atom_type

    interface atom_entry
        !! Generic XML entry generator.
        module procedure :: atom_entry_log
    end interface

    public :: dm_atom_from_logs

    private :: atom_entry
    private :: atom_entry_log
    private :: atom_link
    private :: atom_style_sheet
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    subroutine dm_atom_from_logs(atom, logs, xml)
        !! Returns log messages in Atom Syndication Format (RFC 4287).
        !!
        !! The `atom%alt` value shall be of the form `http://www.example.com/dmpack/log?log_id=`.
        !! The particular log id will be appended to the URL. The feed identification `atom%id`
        !! shall be a valid UUID of the form `00000000-0000-0000-0000-000000000000`. The
        !! parameter `atom%url` shall be the public URL of the Atom feed.
        type(atom_type),               intent(inout) :: atom    !! Atom type.
        type(log_type),                intent(inout) :: logs(:) !! Log array.
        character(len=:), allocatable, intent(out)   :: xml     !! Returned Atom XML string.

        integer :: i, m, n

        ! Feed header.
        xml = A_XML

        ! Add link to XSLT style sheet.
        if (len_trim(atom%xsl) > 0) then
            xml = xml // atom_style_sheet(atom%xsl)
        end if

        ! Start of feed.
        xml = xml // A_FEED // A_GENERATOR

        ! Feed title.
        if (len_trim(atom%title) > 0) then
            xml = xml // A_TITLE // dm_html_encode(atom%title) // A_TITLE_END
        else
            xml = xml // A_TITLE // ATOM_TITLE_DEFAULT // A_TITLE_END
        end if

        ! Feed subtitle.
        if (len_trim(atom%subtitle) > 0) then
            xml = xml // A_SUBTITLE // dm_html_encode(atom%subtitle) // A_SUBTITLE_END
        else
            xml = xml // A_SUBTITLE // ATOM_SUBTITLE_DEFAULT // A_SUBTITLE_END
        end if

        ! Feed URL.
        if (len_trim(atom%url) > 0) then
            xml = xml // atom_link(atom%url, rel='self')
        end if

        ! Feed ID.
        xml = xml // A_ID // 'urn:uuid:'

        if (len_trim(atom%id) > 0) then
            xml = xml // dm_html_encode(atom%id) // A_ID_END
        else
            xml = xml // dm_uuid4_hyphenize(UUID_DEFAULT) // A_ID_END
        end if

        ! Feed timestamp
        xml = xml // A_UPDATED // dm_time_now() // A_UPDATED_END

        ! Feed author
        m = len_trim(atom%author)
        n = len_trim(atom%email)

        if (m > 0 .or. n > 0) then
            xml = xml // A_AUTHOR
            if (m > 0) xml = xml // A_NAME // dm_html_encode(atom%author) // A_NAME_END
            if (n > 0) xml = xml // A_EMAIL // dm_html_encode(atom%email) // A_EMAIL_END
            xml = xml // A_AUTHOR_END
        end if

        ! Feed entries.
        m = len_trim(atom%alt)

        do i = 1, size(logs)
            if (m > 0) then
                xml = xml // atom_entry(logs(i), alt=trim(atom%alt) // trim(logs(i)%id))
            else
                xml = xml // atom_entry(logs(i))
            end if
        end do

        ! Feed footer.
        xml = xml // A_FEED_END
    end subroutine dm_atom_from_logs

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    pure function atom_link(href, rel, type) result(xml)
        !! Returns an Atom link, for example:
        !!
        !! ```html
        !! <link href="http://example.org/feed/" rel="self" />
        !! <link href="http://example.org/" type="text/html" />
        !! ```
        character(len=*), intent(in)           :: href !! Link attribute `href` (URL).
        character(len=*), intent(in), optional :: rel  !! Link attribute `rel` (`self`).
        character(len=*), intent(in), optional :: type !! Link attribute `type` (MIME type).
        character(len=:), allocatable          :: xml  !! Atom XML string.

        xml = '<link href="' // dm_html_encode(href) // '"'
        if (present(rel))  xml = xml // ' rel="'  // dm_html_encode(rel)  // '"'
        if (present(type)) xml = xml // ' type="' // dm_html_encode(type) // '"'
        xml = xml // ' />' // NL
    end function atom_link

    function atom_entry_log(log, alt) result(xml)
        !! Returns an Atom entry from given log message.
        type(log_type),   intent(inout)        :: log  !! Log type.
        character(len=*), intent(in), optional :: alt  !! URL to alternate content (HTML of log).
        character(len=:), allocatable          :: xml  !! Atom XML string.

        integer :: level

        level = max(min(LOG_NLEVEL, log%level), LOG_NONE)

        ! Atom entry.
        xml = A_ENTRY // &
              A_TITLE // dm_html_encode(LOG_LEVEL_NAMES(level)) // ': ' // &
                         dm_html_encode(log%message) // A_TITLE_END

        ! Alternate link.
        if (present(alt)) then
            xml = xml // atom_link(alt, rel='alternate', type='text/html')
        end if

        ! Atom entry content.
        xml = xml // &
              A_ID            // 'urn:uuid:' // dm_uuid4_hyphenize(log%id) // A_ID_END // &
              A_PUBLISHED     // dm_html_encode(log%timestamp)             // A_PUBLISHED_END // &
              A_UPDATED       // dm_html_encode(log%timestamp)             // A_UPDATED_END // &
              A_SUMMARY       // dm_html_encode(LOG_LEVEL_NAMES(level)) // ': ' // &
                                 dm_html_encode(log%message) // A_SUMMARY_END // &
              A_CONTENT_XHTML // A_DIV  // dm_html_log(log)                // A_DIV_END  // A_CONTENT_END // &
              A_AUTHOR        // A_NAME // dm_html_encode(log%source)      // A_NAME_END // A_AUTHOR_END // &
              A_ENTRY_END
    end function atom_entry_log

    function atom_style_sheet(path) result(xml)
        !! Returns `xml-stylesheet` tag with link to XSLT template.
        character(len=*), intent(in)  :: path !! Path to XSLT style sheet.
        character(len=:), allocatable :: xml  !! Atom XML string.

        xml = '<?xml-stylesheet href="' // dm_html_encode(path) // '" type="text/xsl"?>' // NL
    end function atom_style_sheet
end module dm_atom
