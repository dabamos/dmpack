! Author:  Philipp Engel
! Licence: ISC
module dm_html
    !! HyperText Markup Language (HTML) generator procedures for (mostly)
    !! classless HTML5 syntax.
    use :: dm_ascii, only: NL => ASCII_LF
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    use :: dm_version
    implicit none (type, external)
    private

    ! HTML button types.
    integer, parameter, public :: HTML_BUTTON_TYPE_BUTTON  = 1 !! Default button.
    integer, parameter, public :: HTML_BUTTON_TYPE_RESET   = 2 !! Reset button.
    integer, parameter, public :: HTML_BUTTON_TYPE_SUBMIT  = 3 !! Submit button.

    ! HTML input types.
    integer, parameter, public :: HTML_INPUT_TYPE_BUTTON         = 1  !! Button.
    integer, parameter, public :: HTML_INPUT_TYPE_CHECKBOX       = 2  !! Checkbox.
    integer, parameter, public :: HTML_INPUT_TYPE_COLOR          = 3  !! Color.
    integer, parameter, public :: HTML_INPUT_TYPE_DATE           = 4  !! Date.
    integer, parameter, public :: HTML_INPUT_TYPE_DATETIME_LOCAL = 5  !! Date and time.
    integer, parameter, public :: HTML_INPUT_TYPE_EMAIL          = 6  !! E-mail.
    integer, parameter, public :: HTML_INPUT_TYPE_FILE           = 7  !! File.
    integer, parameter, public :: HTML_INPUT_TYPE_HIDDEN         = 8  !! Hidden.
    integer, parameter, public :: HTML_INPUT_TYPE_IMAGE          = 9  !! Image.
    integer, parameter, public :: HTML_INPUT_TYPE_MONTH          = 10 !! Month.
    integer, parameter, public :: HTML_INPUT_TYPE_NUMBER         = 11 !! Number.
    integer, parameter, public :: HTML_INPUT_TYPE_PASSWORD       = 12 !! Password.
    integer, parameter, public :: HTML_INPUT_TYPE_RADIO          = 13 !! Radio button.
    integer, parameter, public :: HTML_INPUT_TYPE_RANGE          = 14 !! Range selector.
    integer, parameter, public :: HTML_INPUT_TYPE_RESET          = 15 !! Reset.
    integer, parameter, public :: HTML_INPUT_TYPE_SEARCH         = 16 !! Search.
    integer, parameter, public :: HTML_INPUT_TYPE_SUBMIT         = 17 !! Submit.
    integer, parameter, public :: HTML_INPUT_TYPE_TEL            = 18 !! Phone.
    integer, parameter, public :: HTML_INPUT_TYPE_TEXT           = 19 !! Text.
    integer, parameter, public :: HTML_INPUT_TYPE_TIME           = 20 !! Time.
    integer, parameter, public :: HTML_INPUT_TYPE_URL            = 21 !! URL.
    integer, parameter, public :: HTML_INPUT_TYPE_WEEK           = 22 !! Week.

    ! Selection of HTML tags.
    character(len=*), parameter, public :: H_BLOCKQUOTE     = '<blockquote>'
    character(len=*), parameter, public :: H_BLOCKQUOTE_END = '</blockquote>'
    character(len=*), parameter, public :: H_BODY           = '<body>' // NL
    character(len=*), parameter, public :: H_BODY_END       = '</body>' // NL
    character(len=*), parameter, public :: H_BR             = '<br>' // NL
    character(len=*), parameter, public :: H_CODE           = '<code>'
    character(len=*), parameter, public :: H_CODE_END       = '</code>'
    character(len=*), parameter, public :: H_COMMENT        = '<!-- '
    character(len=*), parameter, public :: H_COMMENT_END    = ' //-->'
    character(len=*), parameter, public :: H_DETAILS        = '<details>' // NL
    character(len=*), parameter, public :: H_DETAILS_END    = '</details>' // NL
    character(len=*), parameter, public :: H_DIV            = '<div>' // NL
    character(len=*), parameter, public :: H_DIV_COL        = '<div class="col">' // NL
    character(len=*), parameter, public :: H_DIV_END        = '</div>' // NL
    character(len=*), parameter, public :: H_DIV_ROW        = '<div class="row">' // NL
    character(len=*), parameter, public :: H_DOCTYPE        = '<!DOCTYPE html>' // NL
    character(len=*), parameter, public :: H_EM             = '<em>'
    character(len=*), parameter, public :: H_EM_END         = '</em>'
    character(len=*), parameter, public :: H_FIELDSET       = '<fieldset>' // NL
    character(len=*), parameter, public :: H_FIELDSET_END   = '</fieldset>' // NL
    character(len=*), parameter, public :: H_FIGCAPTION     = '<figcaption>'
    character(len=*), parameter, public :: H_FIGCAPTION_END = '</figcaption>' // NL
    character(len=*), parameter, public :: H_FIGURE         = '<figure>' // NL
    character(len=*), parameter, public :: H_FIGURE_END     = '</figure>' // NL
    character(len=*), parameter, public :: H_FORM           = '<form>' // NL
    character(len=*), parameter, public :: H_FORM_END       = '</form>' // NL
    character(len=*), parameter, public :: H_FORM_POST      = '<form method="post">' // NL
    character(len=*), parameter, public :: H_FOOTER         = '<footer>' // NL
    character(len=*), parameter, public :: H_FOOTER_END     = '</footer>' // NL
    character(len=*), parameter, public :: H_H1             = '<h1>'
    character(len=*), parameter, public :: H_H1_END         = '</h1>' // NL
    character(len=*), parameter, public :: H_H2             = '<h2>'
    character(len=*), parameter, public :: H_H2_END         = '</h2>' // NL
    character(len=*), parameter, public :: H_H3             = '<h3>'
    character(len=*), parameter, public :: H_H3_END         = '</h3>' // NL
    character(len=*), parameter, public :: H_H4             = '<h4>'
    character(len=*), parameter, public :: H_H4_END         = '</h4>' // NL
    character(len=*), parameter, public :: H_HEAD           = '<head>' // NL
    character(len=*), parameter, public :: H_HEAD_END       = '</head>' // NL
    character(len=*), parameter, public :: H_HEADER         = '<header>' // NL
    character(len=*), parameter, public :: H_HEADER_END     = '</header>' // NL
    character(len=*), parameter, public :: H_HR             = '<hr>' // NL
    character(len=*), parameter, public :: H_HTML           = '<html lang="en">' // NL
    character(len=*), parameter, public :: H_HTML_END       = '</html>'
    character(len=*), parameter, public :: H_LI             = '<li>'
    character(len=*), parameter, public :: H_LI_END         = '</li>' // NL
    character(len=*), parameter, public :: H_MAIN           = '<main>' // NL
    character(len=*), parameter, public :: H_MAIN_END       = '</main>' // NL
    character(len=*), parameter, public :: H_MARK           = '<mark>'
    character(len=*), parameter, public :: H_MARK_END       = '</mark>'
    character(len=*), parameter, public :: H_META_CHARSET   = '<meta charset="utf-8">' // NL
    character(len=*), parameter, public :: H_META_GENERATOR = &
        '<meta name="generator" content="DMPACK ' // DM_VERSION_STRING // '">' // NL
    character(len=*), parameter, public :: H_META_VIEWPORT  = &
        '<meta name="viewport" content="width=device-width, initial-scale=1.0">' // NL
    character(len=*), parameter, public :: H_NAV            = '<nav>' // NL
    character(len=*), parameter, public :: H_NAV_END        = '</nav>' // NL
    character(len=*), parameter, public :: H_OPTION         = '<option>'
    character(len=*), parameter, public :: H_OPTION_END     = '</option>'
    character(len=*), parameter, public :: H_P              = '<p>'
    character(len=*), parameter, public :: H_P_END          = '</p>' // NL
    character(len=*), parameter, public :: H_PRE            = '<pre>'
    character(len=*), parameter, public :: H_PRE_END        = '</pre>'
    character(len=*), parameter, public :: H_SECTION        = '<section>' // NL
    character(len=*), parameter, public :: H_SECTION_END    = '</section>' // NL
    character(len=*), parameter, public :: H_SMALL          = '<small>'
    character(len=*), parameter, public :: H_SMALL_END      = '</small>'
    character(len=*), parameter, public :: H_SPAN           = '<small>'
    character(len=*), parameter, public :: H_SPAN_END       = '</span>'
    character(len=*), parameter, public :: H_STRONG         = '<strong>'
    character(len=*), parameter, public :: H_STRONG_END     = '</strong>'
    character(len=*), parameter, public :: H_SUMMARY        = '<summary>'
    character(len=*), parameter, public :: H_SUMMARY_END    = '</summary>' // NL
    character(len=*), parameter, public :: H_STYLE          = '<style>' // NL
    character(len=*), parameter, public :: H_STYLE_END      = '</style>' // NL
    character(len=*), parameter, public :: H_TABLE          = '<table>' // NL
    character(len=*), parameter, public :: H_TABLE_END      = '</table>' // NL
    character(len=*), parameter, public :: H_TBODY          = '<tbody>' // NL
    character(len=*), parameter, public :: H_TBODY_END      = '</tbody>' // NL
    character(len=*), parameter, public :: H_TD             = '<td>'
    character(len=*), parameter, public :: H_TD_END         = '</td>'
    character(len=*), parameter, public :: H_TH             = '<th>'
    character(len=*), parameter, public :: H_THEAD          = '<thead>' // NL
    character(len=*), parameter, public :: H_THEAD_END      = '</thead>' // NL
    character(len=*), parameter, public :: H_TH_END         = '</th>'
    character(len=*), parameter, public :: H_TITLE          = '<title>'
    character(len=*), parameter, public :: H_TITLE_END      = '</title>' // NL
    character(len=*), parameter, public :: H_TR             = '<tr>'
    character(len=*), parameter, public :: H_TR_END         = '</tr>' // NL
    character(len=*), parameter, public :: H_UL             = '<ul>' // NL
    character(len=*), parameter, public :: H_UL_END         = '</ul>' // NL

    type, public :: anchor_type
        !! HTML anchor type.
        character(len=256) :: link = ' ' !! URL.
        character(len=256) :: text = ' ' !! Link text.
    end type anchor_type

    type, public :: select_type
        !! HTML select type. The length if option names and values is limited
        !! to 32 characters. Initialise this derived type with subroutine
        !! `dm_html_select_create()`. Free the allocated memory with
        !! `dm_html_select_destroy()`.
        character(len=32), allocatable :: options(:) !! Option names.
        character(len=32), allocatable :: values(:)  !! Option values.
    end type select_type

    public :: dm_html_anchor
    public :: dm_html_beat
    public :: dm_html_beats
    public :: dm_html_button
    public :: dm_html_cgi_env
    public :: dm_html_comment
    public :: dm_html_data_uri
    public :: dm_html_decode
    public :: dm_html_error
    public :: dm_html_encode
    public :: dm_html_figure
    public :: dm_html_footer
    public :: dm_html_header
    public :: dm_html_heading
    public :: dm_html_image
    public :: dm_html_input
    public :: dm_html_label
    public :: dm_html_link
    public :: dm_html_log
    public :: dm_html_logs
    public :: dm_html_nav
    public :: dm_html_node
    public :: dm_html_nodes
    public :: dm_html_observ
    public :: dm_html_observs
    public :: dm_html_p
    public :: dm_html_pre
    public :: dm_html_request
    public :: dm_html_responses
    public :: dm_html_select
    public :: dm_html_select_create
    public :: dm_html_select_destroy
    public :: dm_html_select_set
    public :: dm_html_sensor
    public :: dm_html_sensors
    public :: dm_html_small
    public :: dm_html_span
    public :: dm_html_target
    public :: dm_html_targets
    public :: dm_html_td
    public :: dm_html_th
contains
    pure function dm_html_anchor(anchor) result(html)
        !! Returns HTML anchor tag.
        type(anchor_type), intent(in) :: anchor !! Anchor type.
        character(len=:), allocatable :: html   !! Generated HTML.

        html = '<a href="' // dm_html_encode(anchor%link) // '">' // &
                              dm_html_encode(anchor%text) // '</a>'
    end function dm_html_anchor

    function dm_html_beat(beat, delta, prefix) result(html)
        !! Returns table of single beat in HTML format.
        use :: dm_beat
        use :: dm_time
        type(beat_type),  intent(inout)        :: beat   !! Beat type.
        integer(kind=i8), intent(in), optional :: delta  !! Time delta.
        character(len=*), intent(in), optional :: prefix !! GET argument name.
        character(len=:), allocatable          :: html   !! Generated HTML.

        character(len=8)              :: beats_now, beats_sent, beats_recv
        character(len=:), allocatable :: nid
        character(len=TIME_LEN)       :: now
        integer                       :: rc
        integer(kind=i8)              :: delta_
        type(anchor_type)             :: anchor
        type(time_delta_type)         :: time, time_delta, time_inter

        delta_ = huge(0_i8)
        if (present(delta)) delta_ = delta

        if (present(prefix)) then
            ! Turn node id into link to `prefix`.
            anchor = anchor_type(link=prefix // beat%node_id, text=beat%node_id)
            nid = dm_html_anchor(anchor)
        else
            nid = H_CODE // dm_html_encode(beat%node_id) // H_CODE_END
        end if

        call dm_time_delta_from_seconds(time, int(beat%uptime, kind=i8))
        call dm_time_delta_from_seconds(time_inter, int(beat%interval, kind=i8))
        call dm_time_delta_from_seconds(time_delta, delta_)

        rc = dm_time_to_beats(beat%time_sent, beats_sent)
        rc = dm_time_to_beats(beat%time_recv, beats_recv)

        now = dm_time_now()
        rc = dm_time_to_beats(now, beats_now)

        html = H_TABLE // H_TBODY // &
               H_TR // H_TH // 'Node ID' // H_TH_END // &
               H_TD // nid // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Address' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(beat%address) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Version' // H_TH_END // &
               H_TD // dm_html_encode(beat%version) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Time Sent' // H_TH_END // &
               H_TD // dm_html_encode(beat%time_sent // ' (' // trim(beats_sent) // ')') // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Time Received' // H_TH_END // &
               H_TD // dm_html_encode(beat%time_recv // ' (' // trim(beats_recv) // ')') // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Time Now' // H_TH_END // &
               H_TD // dm_html_encode(now // ' (' // trim(beats_now) // ')') // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Time Delta' // H_TH_END // &
               H_TD // dm_time_delta_to_string(time_delta) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Interval' // H_TH_END // &
               H_TD // dm_time_delta_to_string(time_inter) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Uptime' // H_TH_END // &
               H_TD // dm_time_delta_to_string(time) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Last Error' // H_TH_END // &
               H_TD // dm_error_message(beat%error) // ' (' // dm_itoa(beat%error) // ')' // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Status' // H_TH_END // H_TD

        if (delta_ <= int(beat%interval, kind=i8)) then
            html = html // dm_html_mark('on-time', class='debug')
        else
            html = html // dm_html_mark('overdue', class='warning')
        end if

        html = html // H_TD_END // H_TR_END // H_TBODY_END // H_TABLE_END
    end function dm_html_beat

    function dm_html_beats(beats, deltas, prefix) result(html)
        !! Returns table of heartbeats in HTML format. If argument `prefix` is
        !! passed, the node ids are enclosed in HTML anchors, with the link
        !! set to `prefix`.
        use :: dm_beat
        use :: dm_time
        type(beat_type),   intent(inout)           :: beats(:)  !! Beat types.
        integer(kind=i8),  intent(inout), optional :: deltas(:) !! Time deltas.
        character(len=*),  intent(in),    optional :: prefix    !! GET argument name.
        character(len=:), allocatable              :: html      !! Generated HTML.

        integer               :: i
        integer(kind=i8)      :: delta
        logical               :: is_anchor
        type(anchor_type)     :: anchor
        type(time_delta_type) :: time_delta

        is_anchor = .false.
        if (present(prefix)) is_anchor = .true.

        html = H_TABLE // H_THEAD // H_TR // &
               H_TH // '#'           // H_TH_END // &
               H_TH // 'Node ID'     // H_TH_END // &
               H_TH // 'Address'     // H_TH_END // &
               H_TH // 'Last Signal' // H_TH_END // &
               H_TH // 'Error'       // H_TH_END // &
               H_TH // 'Interval'    // H_TH_END // &
               H_TH // 'Uptime'      // H_TH_END // &
               H_TH // 'Status'      // H_TH_END // &
               H_TR_END // H_THEAD_END // H_TBODY

        do i = 1, size(beats)
            if (present(deltas)) then
                if (i <= size(deltas)) delta = deltas(i)
            end if

            call dm_time_delta_from_seconds(time_delta, int(beats(i)%uptime, kind=i8))
            html = html // H_TR // H_TD // dm_itoa(i) // H_TD_END

            if (is_anchor) then
                ! Turn node id into link to `prefix`.
                anchor%link = prefix // beats(i)%node_id
                anchor%text = beats(i)%node_id
                html = html // H_TD // dm_html_anchor(anchor) // H_TD_END
            else
                html = html // H_TD // H_CODE // dm_html_encode(beats(i)%node_id) // H_CODE_END // H_TD_END
            end if

            html = html // &
                   H_TD // H_CODE // dm_html_encode(beats(i)%address) // H_CODE_END // H_TD_END // &
                   H_TD // dm_html_encode(beats(i)%time_recv) // H_TD_END // &
                   H_TD // dm_itoa(beats(i)%error) // H_TD_END // &
                   H_TD // dm_itoa(beats(i)%interval) // ' secs' // H_TD_END // &
                   H_TD // dm_time_delta_to_string(time_delta, hours=.false., minutes=.false., seconds=.false.) // H_TD_END // &
                   H_TD

            if (delta <= int(beats(i)%interval, kind=i8)) then
                html = html // dm_html_mark('on-time', class='debug')
            else
                html = html // dm_html_mark('overdue', class='warning')
            end if

            html = html // H_TD_END // H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_beats

    pure function dm_html_button(type, text, disabled) result(html)
        !! Returns HTML button element. This function does not encode the
        !! argument.
        character(len=*), parameter :: BUTTON_TYPES(3) = [ &
            character(len=6) :: 'button', 'reset', 'submit' ] ! Button type names.

        integer,          intent(in)           :: type     !! Button type enumerator.
        character(len=*), intent(in)           :: text     !! Button text.
        logical,          intent(in), optional :: disabled !! Disabled flag.
        character(len=:), allocatable          :: html     !! Generated HTML.

        integer :: type_
        logical :: disabled_

        disabled_ = .false.
        if (present(disabled)) disabled_ = disabled

        type_ = HTML_BUTTON_TYPE_BUTTON

        if (type >= HTML_BUTTON_TYPE_BUTTON .and. &
            type <= HTML_BUTTON_TYPE_SUBMIT) type_ = type

        if (disabled_) then
            html = '<button type="' // trim(BUTTON_TYPES(type_)) // '" disabled="disabled">' // &
                   trim(text) // &
                   '</button>' // NL
        else
            html = '<button type="' // trim(BUTTON_TYPES(type_)) // '">' // &
                   trim(text) // &
                   '</button>' // NL
        end if
    end function dm_html_button

    function dm_html_cgi_env(env) result(html)
        !! Returns HTML table of CGI environment variables.
        use :: dm_cgi
        type(cgi_env_type), intent(inout) :: env  !! CGI environment variables.
        character(len=:), allocatable     :: html !! Generated HTML.

        html = H_TABLE // H_THEAD // &
               H_TR // H_TH // 'Variable' // H_TH_END // &
               H_TH // 'Value' // H_TH_END // H_TR_END // &
               H_THEAD_END // H_TBODY // &
               H_TR // H_TD // 'AUTH_TYPE' // H_TD_END // &
               H_TD // dm_html_encode(env%auth_type) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'CONTENT_LENGTH' // H_TD_END // &
               H_TD // dm_itoa(env%content_length) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'CONTENT_TYPE' // H_TD_END // &
               H_TD // dm_html_encode(env%content_type) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'DOCUMENT_ROOT' // H_TD_END // &
               H_TD // dm_html_encode(env%document_root) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'GATEWAY_INTERFACE' // H_TD_END // &
               H_TD // dm_html_encode(env%gateway_interface) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'HTTP_ACCEPT' // H_TD_END // &
               H_TD // dm_html_encode(env%http_accept) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'HTTP_CONTENT_ENCODING' // H_TD_END // &
               H_TD // dm_html_encode(env%http_content_encoding) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'HTTP_COOKIE' // H_TD_END // &
               H_TD // dm_html_encode(env%http_cookie) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'HTTP_FROM' // H_TD_END // &
               H_TD // dm_html_encode(env%http_from) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'HTTP_REFERER' // H_TD_END // &
               H_TD // dm_html_encode(env%http_referer) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'HTTP_USER_AGENT' // H_TD_END // &
               H_TD // dm_html_encode(env%http_user_agent) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'PATH_INFO' // H_TD_END // &
               H_TD // dm_html_encode(env%path_info) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'PATH_TRANSLATED' // H_TD_END // &
               H_TD // dm_html_encode(env%path_translated) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'QUERY_STRING' // H_TD_END // &
               H_TD // dm_html_encode(env%query_string) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'REMOTE_ADDR' // H_TD_END // &
               H_TD // dm_html_encode(env%remote_addr) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'REMOTE_HOST' // H_TD_END // &
               H_TD // dm_html_encode(env%remote_host) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'REMOTE_IDENT' // H_TD_END // &
               H_TD // dm_html_encode(env%remote_ident) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'REMOTE_USER' // H_TD_END // &
               H_TD // dm_html_encode(env%remote_user) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'REQUEST_METHOD' // H_TD_END // &
               H_TD // dm_html_encode(env%request_method) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'REQUEST_URI' // H_TD_END // &
               H_TD // dm_html_encode(env%request_uri) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'SCRIPT_NAME' // H_TD_END // &
               H_TD // dm_html_encode(env%script_name) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'SERVER_NAME' // H_TD_END // &
               H_TD // dm_html_encode(env%server_name) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'SERVER_PORT' // H_TD_END // &
               H_TD // dm_itoa(env%server_port) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'SERVER_PROTOCOL' // H_TD_END // &
               H_TD // dm_html_encode(env%server_protocol) // H_TD_END // H_TR_END // &
               H_TR // H_TD // 'SERVER_SOFTWARE' // H_TD_END // &
               H_TD // dm_html_encode(env%server_software) // H_TD_END // H_TR_END // &
               H_TBODY_END // H_TABLE_END
    end function dm_html_cgi_env

    pure function dm_html_comment(str) result(html)
        !! Returns HTML comment. This function does not encode the argument.
        character(len=*), intent(in)  :: str  !! Comment string.
        character(len=:), allocatable :: html !! Generated HTML.

        html = H_COMMENT // str // H_COMMENT_END
    end function dm_html_comment

    function dm_html_data_uri(data, mime) result(uri)
        !! Returns base64-encoded data URI of given data and MIME type.
        use :: dm_base64
        character(len=*), intent(inout) :: data !! Raw data.
        character(len=*), intent(in)    :: mime !! MIME type.
        character(len=:), allocatable   :: uri  !! Data URI.

        character(len=:), allocatable :: base64

        call dm_base64_encode(data, base64)
        uri = 'data:' // trim(mime) // ';base64,' // base64
    end function dm_html_data_uri

    pure function dm_html_decode(input) result(output)
        !! Reverses HTML encoding.
        character(len=*), intent(in)  :: input  !! String to decode.
        character(len=:), allocatable :: output !! Decoded string.

        integer :: i, n

        output = ''
        n = len_trim(input)
        if (n == 0) return
        i = 1

        do
            if (i > n) exit
            if (input(i:i) == '&' .and. i + 4 <= n) then
                if (input(i:i + 4) == 'amp;') then
                    output = output // '&'
                    i = i + 5
                    cycle
                end if
            end if

            output = output // input(i:i)
            i = i + 1
        end do
    end function dm_html_decode

    pure function dm_html_encode(input) result(output)
        !! Returns encoded input string, with some HTML special characters
        !! replaced (`"`, `&`, `'`, `<`, `>`).
        !!
        !! It may be faster to count the number of occurences of special
        !! characters, and only allocate the output string once at start.
        character(len=*), intent(in)  :: input  !! Input string.
        character(len=:), allocatable :: output !! Encoded string.

        integer :: i

        output = ''

        do i = 1, len_trim(input)
            select case (input(i:i))
                case ('"')
                    output = output // '&quot;'
                case ('&')
                    output = output // '&amp;'
                case ("'")
                    output = output // '&apos;'
                case ('<')
                    output = output // '&lt;'
                case ('>')
                    output = output // '&gt;'
                case default
                    output = output // input(i:i)
            end select
        end do
    end function dm_html_encode

    pure function dm_html_error(error_code, message) result(html)
        !! Returns HTML of (encoded) error description.
        integer,          intent(in)           :: error_code !! DMPACK error code.
        character(len=*), intent(in), optional :: message    !! Error message.
        character(len=:), allocatable          :: html       !! Generated HTML.

        if (present(message)) then
            html = H_P // 'Error: ' // dm_html_encode(message) // ' (' // &
                   dm_itoa(error_code) // ')' // H_P_END
            return
        end if

        html = H_P // 'Error: ' // dm_error_message(error_code) // &
               ' (' // dm_itoa(error_code) // ')' // H_P_END
    end function dm_html_error

    pure function dm_html_figure(content, caption) result(html)
        !! Returns HTML figure. The content should end with a newline. The
        !! caption will be HTML encoded.
        character(len=*), intent(in)           :: content !! Figure content.
        character(len=*), intent(in), optional :: caption !! Figure caption.
        character(len=:), allocatable          :: html    !! Generated HTML.

        html = H_FIGURE // trim(content)

        if (present(caption)) then
            if (len_trim(caption) > 0) &
                html = html // H_FIGCAPTION // dm_html_encode(caption) // H_FIGCAPTION_END
        end if

        html = html // H_FIGURE_END
    end function dm_html_figure

    pure function dm_html_footer(content) result(html)
        !! Returns HTML footer. The content will not be HTML encoded.
        character(len=*), intent(in), optional :: content !! Optional footer content.
        character(len=:), allocatable          :: html    !! Generated HTML.

        if (present(content)) then
            html = H_MAIN_END // H_FOOTER // trim(content) // H_FOOTER_END // &
                   H_DIV_END // H_BODY_END // H_HTML_END
            return
        end if

        html = H_MAIN_END // H_DIV_END // H_BODY_END // H_HTML_END
    end function dm_html_footer

    function dm_html_header(title, subtitle, style, internal_style, brand, nav, mask) result(html)
        !! Returns HTML header with DOCTYPE and optional CSS. A link to the
        !! style sheet file and internal CSS can be added.
        !!
        !! The first heading will be set to the page title. The heading is
        !! shown only if no navigation array is passed.
        !!
        !! The given title and sub-title are encoded by this function.
        character(len=*),  intent(in)              :: title          !! HTML page title and first heading.
        character(len=*),  intent(in),    optional :: subtitle       !! Subtitle.
        character(len=*),  intent(in),    optional :: style          !! Path to CSS file.
        character(len=*),  intent(in),    optional :: internal_style !! Additional CSS (inline).
        character(len=*),  intent(in),    optional :: brand          !! Brand title.
        type(anchor_type), intent(inout), optional :: nav(:)         !! Navigation anchors.
        logical,           intent(inout), optional :: mask(:)        !! Navigation anchors mask.
        character(len=:), allocatable              :: html           !! Generated HTML.

        logical :: has_internal, has_style, has_subtitle

        has_subtitle = .false.
        if (present(subtitle)) has_subtitle = (len_trim(subtitle) > 0)

        has_style = .false.
        if (present(style)) has_style = (len_trim(style) > 0)

        has_internal = .false.
        if (present(internal_style)) has_internal = (len_trim(internal_style) > 0)

        html = H_DOCTYPE // H_HTML // H_HEAD // &
               H_TITLE // dm_html_encode(title) // H_TITLE_END // &
               H_META_CHARSET // H_META_GENERATOR // H_META_VIEWPORT

        if (has_style)    html = html // dm_html_link('stylesheet', style) // NL
        if (has_internal) html = html // H_STYLE // internal_style // H_STYLE_END

        html = html // H_HEAD_END // H_BODY // H_HEADER

        ! Brand title (will be encoded).
        if (present(brand)) then
            html = html // dm_html_heading(3, brand)
        end if

        ! Sidebar navigation.
        if (present(nav) .and. present(mask)) then
            html = html // dm_html_nav(nav, mask)
        else if (present(nav)) then
            html = html // dm_html_nav(nav)
        else
            if (has_subtitle) then
                html = html // dm_html_heading(1, title, subtitle)
            else
                html = html // dm_html_heading(1, title)
            end if
        end if

        html = html // H_HEADER_END // H_DIV // H_MAIN
    end function dm_html_header

    pure function dm_html_heading(level, str, small) result(html)
        !! Returns HTML heading of given level `level` and string `str`, with
        !! optional `<small>` child in `small`.
        !!
        !! Valid levels are 1, 2, 3, and 4. Any other is replaced by level 1.
        !!
        !! All input data will be trimmed and encoded.
        integer,          intent(in)           :: level !! Heading level.
        character(len=*), intent(in)           :: str   !! Heading string.
        character(len=*), intent(in), optional :: small !! Sub-heading string.
        character(len=:), allocatable          :: html  !! Generated HTML.

        logical :: has_small

        has_small = .false.
        if (present(small)) has_small = (len_trim(small) > 0)

        select case (level)
            case (2)
                ! H2
                if (has_small) then
                    html = H_H2 // dm_html_encode(str) // dm_html_small(small) // H_H2_END
                else
                    html = H_H2 // dm_html_encode(str) // H_H2_END
                end if
            case (3)
                ! H3
                if (has_small) then
                    html = H_H3 // dm_html_encode(str) // dm_html_small(small) // H_H3_END
                else
                    html = H_H3 // dm_html_encode(str) // H_H3_END
                end if
            case (4)
                ! H4
                if (has_small) then
                    html = H_H4 // dm_html_encode(str) // dm_html_small(small) // H_H4_END
                else
                    html = H_H4 // dm_html_encode(str) // H_H4_END
                end if
            case default
                ! H1
                if (has_small) then
                    html = H_H1 // dm_html_encode(str) // dm_html_small(small) // H_H1_END
                else
                    html = H_H1 // dm_html_encode(str) // H_H1_END
                end if
        end select
    end function dm_html_heading

    pure function dm_html_image(src, alt) result(html)
        !! Returns HTML image tag. This function does not encode the
        !! arguments.
        character(len=*), intent(in)  :: src  !! Image source.
        character(len=*), intent(in)  :: alt  !! Image alt tag.
        character(len=:), allocatable :: html !! Generated HTML.

        html = '<img src="' // src // '" alt="' // trim(alt) // '">' // NL
    end function dm_html_image

    pure function dm_html_input(type, checked, disabled, id, max, max_length, min, min_length, &
                                name, pattern, placeholder, read_only, required, size, value) result(html)
        !! Returns HTML input element.
        character(len=*), parameter :: INPUT_TYPES(22) = [ character(len=14) :: &
            'button', 'checkbox', 'color', 'date', 'datetime-local', 'email', &
            'file', 'hidden', 'image', 'month', 'number', 'password', 'radio', &
            'range', 'reset', 'search', 'submit', 'tel', 'text', 'time', 'url', &
            'week' ] ! Input type names.

        integer,          intent(in)           :: type        !! HTML input type.
        logical,          intent(in), optional :: checked     !! Input is checked.
        logical,          intent(in), optional :: disabled    !! Input is disabled.
        character(len=*), intent(in), optional :: id          !! Input id.
        integer,          intent(in), optional :: max         !! Input max. value.
        integer,          intent(in), optional :: max_length  !! Input max. length.
        integer,          intent(in), optional :: min         !! Input min. value.
        integer,          intent(in), optional :: min_length  !! Input min. length.
        character(len=*), intent(in), optional :: name        !! Input name.
        character(len=*), intent(in), optional :: pattern     !! Input regular expression pattern.
        character(len=*), intent(in), optional :: placeholder !! Input placeholder.
        logical,          intent(in), optional :: read_only   !! Input is read-only.
        logical,          intent(in), optional :: required    !! Input is required.
        integer,          intent(in), optional :: size        !! Input size.
        character(len=*), intent(in), optional :: value       !! Input value.
        character(len=:), allocatable          :: html        !! Generated HTML.

        integer :: input_type

        input_type = HTML_INPUT_TYPE_TEXT

        if (type >= HTML_INPUT_TYPE_BUTTON .and. &
            type <= HTML_INPUT_TYPE_WEEK) input_type = type

        html = '<input type="' // trim(INPUT_TYPES(input_type)) // '"'

        if (present(checked)) then
            if (checked) html = html // ' checked="checked"'
        end if

        if (present(disabled)) then
            if (disabled) html = html // ' disabled="disabled"'
        end if

        if (present(id)) then
            html = html // ' id="' // trim(id) // '"'
        end if

        if (present(max)) then
            html = html // ' max="' // dm_itoa(max) // '"'
        end if

        if (present(max_length)) then
            html = html // ' maxlength="' // dm_itoa(max_length) // '"'
        end if

        if (present(min)) then
            html = html // ' min="' // dm_itoa(min) // '"'
        end if

        if (present(min_length)) then
            html = html // ' minlength="' // dm_itoa(min_length) // '"'
        end if

        if (present(name)) then
            html = html // ' name="' // trim(name) // '"'
        end if

        if (present(pattern)) then
            html = html // ' pattern="' // trim(pattern) // '"'
        end if

        if (present(placeholder)) then
            html = html // ' placeholder="' // trim(placeholder) // '"'
        end if

        if (present(read_only)) then
            html = html // ' readonly="readonly"'
        end if

        if (present(required)) then
            html = html // ' required="required"'
        end if

        if (present(size)) then
            html = html // ' size="' // dm_itoa(size) // '"'
        end if

        if (present(value)) then
            html = html // ' value="' // trim(value) // '"'
        end if

        html = html // '>' // NL
    end function dm_html_input

    pure function dm_html_label(str, for) result(html)
        !! Returns HTML label element. This function does not encode the
        !! arguments.
        character(len=*), intent(in)           :: str  !! Label string.
        character(len=*), intent(in), optional :: for  !! Label for attribute.
        character(len=:), allocatable          :: html !! Generated HTML.

        if (present(for)) then
            html = '<label for="' // trim(for) // '">' // trim(str) // '</label>' // NL
        else
            html = '<label>' // trim(str) // '</label>' // NL
        end if
    end function dm_html_label

    pure function dm_html_link(rel, href) result(html)
        !! Returns link element. Link address and text will be trimmed and
        !! encoded.
        character(len=*), intent(in)  :: rel  !! The rel attribute.
        character(len=*), intent(in)  :: href !! The href attribute.
        character(len=:), allocatable :: html !! Generated HTML.

        html = '<link rel="' // dm_html_encode(rel) // '" href="' // dm_html_encode(href) // '">'
    end function dm_html_link

    function dm_html_log(log, prefix_node, prefix_sensor, prefix_target, prefix_observ) result(html)
        !! Returns log as HTML table. The input data will be trimmed and
        !! encoded.
        use :: dm_log
        type(log_type),   intent(inout)        :: log           !! Log type.
        character(len=*), intent(in), optional :: prefix_node   !! Node link prefix.
        character(len=*), intent(in), optional :: prefix_sensor !! Sensor link prefix.
        character(len=*), intent(in), optional :: prefix_target !! Target link prefix.
        character(len=*), intent(in), optional :: prefix_observ !! Observation link prefix.
        character(len=:), allocatable          :: html          !! Generated HTML.

        character(len=:), allocatable :: nid ! Node id.
        character(len=:), allocatable :: sid ! Sensor id.
        character(len=:), allocatable :: tid ! Target id.
        character(len=:), allocatable :: oid ! Observation id.

        integer           :: level
        type(anchor_type) :: anchor

        level = max(LOG_NONE, min(LOG_NLEVEL, log%level))

        ! Node id.
        if (present(prefix_node) .and. len_trim(log%node_id) > 0) then
            anchor%link = prefix_node // dm_html_encode(log%node_id)
            anchor%text = dm_html_encode(log%node_id)
            nid = dm_html_anchor(anchor)
        else
            nid = dm_html_encode(log%node_id)
        end if

        ! Sensor id.
        if (present(prefix_sensor) .and. len_trim(log%sensor_id) > 0) then
            anchor%link = prefix_sensor // dm_html_encode(log%sensor_id)
            anchor%text = dm_html_encode(log%sensor_id)
            sid = dm_html_anchor(anchor)
        else
            sid = dm_html_encode(log%sensor_id)
        end if

        ! Target id.
        if (present(prefix_target) .and. len_trim(log%target_id) > 0) then
            anchor%link = prefix_target // dm_html_encode(log%target_id)
            anchor%text = dm_html_encode(log%target_id)
            tid = dm_html_anchor(anchor)
        else
            tid = dm_html_encode(log%target_id)
        end if

        ! Observation id.
        if (present(prefix_observ) .and. len_trim(log%observ_id) > 0) then
            anchor%link = prefix_observ // dm_html_encode(log%observ_id)
            anchor%text = dm_html_encode(log%observ_id)
            oid = dm_html_anchor(anchor)
        else
            oid = dm_html_encode(log%observ_id)
        end if

        html = H_TABLE // H_TBODY // &
               H_TR // H_TH // 'ID' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(log%id) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Timestamp' // H_TH_END // &
               H_TD // dm_html_encode(log%timestamp) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Level' // H_TH_END // &
               H_TD // dm_html_mark(LOG_LEVEL_NAMES(level), class=LOG_LEVEL_NAMES_LOWER(level)) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Error' // H_TH_END // &
               H_TD // dm_error_message(log%error) // ' (' // dm_itoa(log%error) // ')' // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Node ID' // H_TH_END // &
               H_TD // nid // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Sensor ID' // H_TH_END // &
               H_TD // sid // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Target ID' // H_TH_END // &
               H_TD // tid // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Observation ID' // H_TH_END // &
               H_TD // oid // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Source' // H_TH_END // &
               H_TD // dm_html_encode(log%source) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Message' // H_TH_END // &
               H_TD // dm_html_encode(log%message) // H_TD_END // H_TR_END // &
               H_TBODY_END // H_TABLE_END
    end function dm_html_log

    function dm_html_logs(logs, prefix, node, max_len) result(html)
        !! Returns table of logs in HTML format. If argument `prefix` is
        !! passed, the log timestamps are enclosed in HTML anchors, with the
        !! link set to `prefix`. By default, a node id column is added to the
        !! table. Optionally, a maximum log message length can be set. The
        !! input data will be trimmed and encoded.
        use :: dm_log
        type(log_type),   intent(inout)        :: logs(:) !! Log types.
        logical,          intent(in), optional :: node    !! Show node id column.
        character(len=*), intent(in), optional :: prefix  !! Link address prefix.
        integer,          intent(in), optional :: max_len !! Max. log message length.
        character(len=:), allocatable          :: html    !! Generated HTML.

        character(len=LOG_MESSAGE_LEN) :: message
        integer                        :: i, level, max_len_, min_len
        logical                        :: is_anchor, node_
        type(anchor_type)              :: anchor

        node_ = .true.
        if (present(node)) node_ = node

        is_anchor = .false.
        if (present(prefix)) is_anchor = .true.

        html = H_TABLE // H_THEAD // H_TR // &
               H_TH // '#'         // H_TH_END // &
               H_TH // 'Timestamp' // H_TH_END

        if (node_) html = html // H_TH // 'Node ID' // H_TH_END

        html = html // H_TH // 'Source' // H_TH_END // &
               H_TH // 'Level'   // H_TH_END // &
               H_TH // 'Error'   // H_TH_END // &
               H_TH // 'Message' // H_TH_END // &
               H_TR_END // H_THEAD_END // H_TBODY

        do i = 1, size(logs)
            html = html // H_TR // H_TD // dm_itoa(i) // H_TD_END

            if (is_anchor) then
                ! Turn timestamp into link to `prefix`.
                anchor%link = prefix // dm_html_encode(logs(i)%id)
                anchor%text = dm_html_encode(logs(i)%timestamp)
                html = html // H_TD // dm_html_anchor(anchor) // H_TD_END
            else
                html = html // H_TD // dm_html_encode(logs(i)%timestamp) // H_TD_END
            end if

            if (node_) html = html // H_TD // dm_html_encode(logs(i)%node_id) // H_TD_END

            level = max(LOG_NONE, min(LOG_NLEVEL, logs(i)%level))

            min_len  = len_trim(logs(i)%message)
            max_len_ = min_len
            if (present(max_len)) max_len_ = max(3, max_len)

            if (min_len > max_len_) then
                message = logs(i)%message(1:max_len_ - 3) // '...'
            else
                message = logs(i)%message
            end if

            html = html // H_TD // dm_html_encode(logs(i)%source) // H_TD_END // &
                   H_TD // dm_html_mark(LOG_LEVEL_NAMES(level), class=LOG_LEVEL_NAMES_LOWER(level)) // H_TD_END // &
                   H_TD // dm_itoa(logs(i)%error) // H_TD_END // &
                   H_TD // dm_html_encode(message) // H_TD_END // H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_logs

    pure function dm_html_mark(str, class) result(html)
        !! Returns `<mark>` element of optional class, with encoded `str`
        !! enclosed. This function encodes the passed string.
        character(len=*), intent(in)           :: str   !! Element content.
        character(len=*), intent(in), optional :: class !! Element class.
        character(len=:), allocatable          :: html  !! Generated HTML.

        if (present(class)) then
            html = '<mark class="' // trim(class) // '">' // dm_html_encode(str) // H_MARK_END
        else
            html = H_MARK // dm_html_encode(str) // H_MARK_END
        end if
    end function dm_html_mark

    function dm_html_nav(anchors, mask) result(html)
        !! Returns HTML navigation element with unordered list of links.
        !! If optional array `mask` is passed, anchors are added according to
        !! their mask state.
        type(anchor_type), intent(inout)           :: anchors(:) !! Anchor types.
        logical,           intent(inout), optional :: mask(:)    !! Mask.
        character(len=:), allocatable              :: html       !! Generated HTML.

        integer :: i, n

        n = 0
        if (present(mask)) n = size(mask)

        html = H_NAV // H_UL

        do i = 1, size(anchors)
            if (n > 0 .and. i <= n) then
                if (.not. mask(i)) cycle
            end if

            html = html // H_LI // dm_html_anchor(anchors(i)) // H_LI_END
        end do

        html = html // H_UL_END // H_NAV_END
    end function dm_html_nav

    function dm_html_node(node) result(html)
        !! Returns sensor node as HTML table. Input data will be trimmed and
        !! encoded.
        use :: dm_node
        type(node_type), intent(inout) :: node !! Node type.
        character(len=:), allocatable  :: html !! Generated HTML.

        html = H_TABLE // H_TBODY // &
               H_TR // H_TH // 'ID' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(node%id) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Name' // H_TH_END // &
               H_TD // dm_html_encode(node%name) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Meta' // H_TH_END // &
               H_TD // dm_html_encode(node%meta) // H_TD_END // H_TR_END // &
               H_TBODY_END // H_TABLE_END
    end function dm_html_node

    function dm_html_nodes(nodes, prefix) result(html)
        !! Returns sensor nodes as HTML table. Input data will be trimmed and
        !! encoded.
        use :: dm_node
        type(node_type),  intent(inout)        :: nodes(:) !! Node types.
        character(len=*), intent(in), optional :: prefix   !! Link address prefix.
        character(len=:), allocatable          :: html     !! Generated HTML.

        integer           :: i
        logical           :: is_anchor
        type(anchor_type) :: anchor

        is_anchor = .false.
        if (present(prefix)) is_anchor = .true.

        html = H_TABLE // H_THEAD // H_TR // &
               H_TH // '#'    // H_TH_END // &
               H_TH // 'ID'   // H_TH_END // &
               H_TH // 'Name' // H_TH_END // &
               H_TH // 'Meta' // H_TH_END // &
               H_TR_END // H_THEAD_END // H_TBODY

        do i = 1, size(nodes)
            html = html // H_TR // H_TD // dm_itoa(i) // H_TD_END

            if (is_anchor) then
                ! Turn node name into link to `prefix`.
                anchor%link = prefix // dm_html_encode(nodes(i)%id)
                anchor%text = dm_html_encode(nodes(i)%id)
                html = html // H_TD // dm_html_anchor(anchor) // H_TD_END
            else
                html = html // H_TD // dm_html_encode(nodes(i)%id) // H_TD_END
            end if

            html = html // H_TD // dm_html_encode(nodes(i)%name) // H_TD_END // &
                           H_TD // dm_html_encode(nodes(i)%meta) // H_TD_END //H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_nodes

    function dm_html_observ(observ, prefix_node, prefix_sensor, prefix_target) result(html)
        !! Returns observation as HTML table. Input data will be trimmed and
        !! encoded.
        use :: dm_observ
        type(observ_type), intent(inout)        :: observ        !! Observation type.
        character(len=*),  intent(in), optional :: prefix_node   !! Node link prefix.
        character(len=*),  intent(in), optional :: prefix_sensor !! Sensor link prefix.
        character(len=*),  intent(in), optional :: prefix_target !! Target link prefix.
        character(len=:), allocatable           :: html          !! Generated HTML.

        character(len=:), allocatable :: nid
        character(len=:), allocatable :: sid
        character(len=:), allocatable :: tid

        integer           :: i, n
        type(anchor_type) :: anchor

        ! Node id.
        if (present(prefix_node) .and. len_trim(observ%node_id) > 0) then
            anchor%link = prefix_node // dm_html_encode(observ%node_id)
            anchor%text = dm_html_encode(observ%node_id)
            nid = dm_html_anchor(anchor)
        else
            nid = dm_html_encode(observ%node_id)
        end if

        ! Sensor id.
        if (present(prefix_sensor) .and. len_trim(observ%sensor_id) > 0) then
            anchor%link = prefix_sensor // dm_html_encode(observ%sensor_id)
            anchor%text = dm_html_encode(observ%sensor_id)
            sid = dm_html_anchor(anchor)
        else
            sid = dm_html_encode(observ%sensor_id)
        end if

        ! Target id.
        if (present(prefix_target) .and. len_trim(observ%target_id) > 0) then
            anchor%link = prefix_target // dm_html_encode(observ%target_id)
            anchor%text = dm_html_encode(observ%target_id)
            tid = dm_html_anchor(anchor)
        else
            tid = dm_html_encode(observ%target_id)
        end if

        html = H_TABLE // H_TBODY // &
               H_TR // H_TH // 'ID' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(observ%id) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Node ID' // H_TH_END // &
               H_TD // nid // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Sensor ID' // H_TH_END // &
               H_TD // sid // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Target ID' // H_TH_END // &
               H_TD // tid // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Name' // H_TH_END // &
               H_TD // dm_html_encode(observ%name) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Timestamp' // H_TH_END // &
               H_TD // dm_html_encode(observ%timestamp) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Path' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(observ%path) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Priority' // H_TH_END // &
               H_TD // dm_itoa(observ%priority) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Error' // H_TH_END // &
               H_TD // dm_error_message(observ%error) // ' (' // dm_itoa(observ%error) // ')' // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Next' // H_TH_END // &
               H_TD // dm_itoa(observ%next) // H_TD_END // H_TR_END // &
               H_TR // H_TH // '#Receivers' // H_TH_END // &
               H_TD // dm_itoa(observ%nreceivers) // H_TD_END // H_TR_END // &
               H_TR // H_TH // '#Requests' // H_TH_END // &
               H_TD // dm_itoa(observ%nrequests) // H_TD_END // H_TR_END

        ! Receivers.
        n = observ%nreceivers

        if (n > 0) then
            html = html // H_TR // dm_html_th('Receivers', row_span=n) // &
                   H_TD // H_CODE // dm_html_encode(observ%receivers(1)) // &
                   H_CODE_END // H_TD_END // H_TR_END

            do i = 2, n
                html = html // H_TR // H_TD // H_CODE // &
                       dm_html_encode(observ%receivers(i)) // H_CODE_END // &
                       H_TD_END // H_TR_END
            end do
        end if

        html = html // H_TBODY_END // H_TABLE_END // dm_html_heading(2, 'Requests')

        ! Requests.
        if (observ%nrequests == 0) then
            html = html // H_P // 'No associated requests found.' // H_P_END
            return
        end if

        do i = 1, observ%nrequests
            html = html // H_DETAILS // &
                   H_SUMMARY // 'Request ' // dm_itoa(i) // H_SUMMARY_END // &
                   dm_html_request(observ%requests(i))
            n = observ%requests(i)%nresponses
            if (n > 0) html = html // dm_html_responses(observ%requests(i)%responses(1:n))
            html = html // H_DETAILS_END
        end do
    end function dm_html_observ

    function dm_html_observs(observs, prefix, id, node_id, sensor_id, target_id, name, error) result(html)
        !! Returns table of observs in HTML format. If argument `prefix` is
        !! passed, the observation names are enclosed in HTML anchors, with the
        !! link set to `prefix`. The table always contains index and timestamp.
        !! The columns id, node id, sensor id, target id, name, and error are
        !! optional. Input data will be trimmed and encoded.
        use :: dm_observ
        type(observ_type), intent(inout)        :: observs(:) !! Observation types.
        character(len=*),  intent(in), optional :: prefix     !! Link address prefix.
        logical,           intent(in), optional :: id         !! Show observation ids.
        logical,           intent(in), optional :: node_id    !! Show node ids.
        logical,           intent(in), optional :: sensor_id  !! Show sensor ids.
        logical,           intent(in), optional :: target_id  !! Show target ids.
        logical,           intent(in), optional :: name       !! Show observation names.
        logical,           intent(in), optional :: error      !! Show erros.
        character(len=:), allocatable           :: html       !! Generated HTML.

        integer           :: i
        logical           :: is_anchor
        logical           :: id_, node_id_, sensor_id_, target_id_, name_, error_
        type(anchor_type) :: anchor

        is_anchor = .false.
        if (present(prefix)) is_anchor = .true.

        id_        = .false.
        node_id_   = .false.
        sensor_id_ = .false.
        target_id_ = .false.
        name_      = .false.
        error_     = .false.

        if (present(id))        id_        = id
        if (present(node_id))   node_id_   = node_id
        if (present(sensor_id)) sensor_id_ = sensor_id
        if (present(target_id)) target_id_ = target_id
        if (present(name))      name_      = name
        if (present(error))     error_     = error

        html = H_TABLE // H_THEAD // H_TR // &
               H_TH // '#'         // H_TH_END // &
               H_TH // 'Timestamp' // H_TH_END

        if (id_)        html = html // H_TH // 'ID' // H_TH_END
        if (node_id_)   html = html // H_TH // 'Node ID' // H_TH_END
        if (sensor_id_) html = html // H_TH // 'Sensor ID' // H_TH_END
        if (target_id_) html = html // H_TH // 'Target ID' // H_TH_END
        if (name_)      html = html // H_TH // 'Name' // H_TH_END
        if (error_)     html = html // H_TH // 'Error' // H_TH_END

        html = html // H_TR_END // H_THEAD_END // H_TBODY

        do i = 1, size(observs)
            html = html // H_TR // H_TD // dm_itoa(i) // H_TD_END

            if (is_anchor) then
                ! Turn timestamp into link to `prefix`.
                anchor%link = prefix // dm_html_encode(observs(i)%id)
                anchor%text = dm_html_encode(observs(i)%timestamp)
                html = html // H_TD // dm_html_anchor(anchor) // H_TD_END
            else
                html = html // H_TD // dm_html_encode(observs(i)%timestamp) // H_TD_END
            end if

            if (id_)        html = html // H_TD // H_CODE // dm_html_encode(observs(i)%id) // H_CODE_END // H_TD_END
            if (node_id_)   html = html // H_TD // dm_html_encode(observs(i)%node_id) // H_TD_END
            if (sensor_id_) html = html // H_TD // dm_html_encode(observs(i)%sensor_id) // H_TD_END
            if (target_id_) html = html // H_TD // dm_html_encode(observs(i)%target_id) // H_TD_END
            if (name_)      html = html // H_TD // dm_html_encode(observs(i)%name) // H_TD_END
            if (error_)     html = html // H_TD // dm_itoa(observs(i)%error) // H_TD_END

            html = html // H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_observs

    pure function dm_html_p(str, encode) result(html)
        !! Returns HTML paragraph. This function does not encode the argument
        !! `str` unless `encode` is `.true.`.
        character(len=*), intent(in)           :: str    !! Paragraph string.
        logical,          intent(in), optional :: encode !! HTML-encode string.
        character(len=:), allocatable          :: html   !! Generated HTML.

        logical :: encode_

        encode_ = .false.
        if (present(encode)) encode_ = encode

        if (encode_) then
            html = H_P // dm_html_encode(str) // H_P_END
        else
            html = H_P // str // H_P_END
        end if
    end function dm_html_p

    pure function dm_html_pre(str, code) result(html)
        !! Returns HTML preformatted text, with optional `<code>` tag.
        !! This function does not encode the argument `str`.
        character(len=*), intent(in)           :: str  !! Content string.
        logical,          intent(in), optional :: code !! Additional code tag?
        character(len=:), allocatable          :: html !! Generated HTML.

        logical :: code_

        code_ = .false.
        if (present(code)) code_ = code

        if (code_) then
            html = H_PRE // H_CODE // str // H_CODE_END // H_PRE_END
        else
            html = H_PRE // str // H_PRE_END
        end if
    end function dm_html_pre

    function dm_html_request(request) result(html)
        !! Returns request as HTML table. Input data will be trimmed and
        !! encoded.
        use :: dm_request
        type(request_type), intent(inout) :: request !! Observation request type.
        character(len=:), allocatable     :: html    !! Generated HTML.

        html = H_TABLE // H_TBODY // &
               H_TR // H_TH // 'Timestamp' // H_TH_END // &
               H_TD // dm_html_encode(request%timestamp) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Request' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(request%request) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Response' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(request%response) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Delimiter' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(request%delimiter) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Pattern' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(request%pattern) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Delay' // H_TH_END // &
               H_TD // dm_itoa(request%delay) // ' ms' // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Error' // H_TH_END // &
               H_TD // dm_error_message(request%error) // ' (' // dm_itoa(request%error) // ')' // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Mode' // H_TH_END // &
               H_TD // dm_itoa(request%mode) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Retries' // H_TH_END // &
               H_TD // dm_itoa(request%retries) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'State' // H_TH_END // &
               H_TD // dm_itoa(request%state) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Timeout' // H_TH_END // &
               H_TD // dm_itoa(request%timeout) // ' ms' // H_TD_END // H_TR_END // &
               H_TR // H_TH // '#Responses' // H_TH_END // &
               H_TD // dm_itoa(request%nresponses) // H_TD_END // H_TR_END // &
               H_TBODY_END // H_TABLE_END
    end function dm_html_request

    function dm_html_responses(responses) result(html)
        !! Returns responses as HTML table. Input data will be trimmed and
        !! encoded.
        use :: dm_response
        type(response_type), intent(inout) :: responses(:) !! Observation response type.
        character(len=:), allocatable      :: html         !! Generated HTML.

        integer :: i

        html = H_TABLE // H_THEAD // H_TR // &
               H_TH // '#' // H_TH_END // &
               H_TH // 'Response Name' // H_TH_END // &
               H_TH // 'Value'         // H_TH_END // &
               H_TH // 'Unit'          // H_TH_END // &
               H_TH // 'Type'          // H_TH_END // &
               H_TH // 'Error'         // H_TH_END // &
               H_THEAD_END // H_TBODY

        do i = 1, size(responses)
            html = html // H_TR // &
                   H_TD // dm_itoa(i)                               // H_TD_END // &
                   H_TD // dm_html_encode(responses(i)%name)        // H_TD_END // &
                   H_TD // dm_ftoa(responses(i)%value)              // H_TD_END // &
                   H_TD // dm_html_encode(responses(i)%unit)        // H_TD_END // &
                   H_TD // dm_response_type_name(responses(i)%type) // H_TD_END // &
                   H_TD // dm_error_message(responses(i)%error) // &
                           ' (' // dm_itoa(responses(i)%error) // ')' // H_TD_END // &
                   H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_responses

    function dm_html_select(select, id, name, selected, disabled) result(html)
        !! Returns HTML select element with option values. This function does
        !! not encode or trim the arguments.
        type(select_type), intent(inout)        :: select   !! HTML select type.
        character(len=*),  intent(in)           :: id       !! Select id.
        character(len=*),  intent(in)           :: name     !! Select name.
        character(len=*),  intent(in)           :: selected !! Element selected by default.
        logical,           intent(in), optional :: disabled !! Disable element.
        character(len=:), allocatable           :: html     !! Generated HTML.

        integer :: i, n
        logical :: disabled_

        disabled_ = .false.
        if (present(disabled)) disabled_ = disabled

        html = '<select id="' // id // '" name="' // name // '">' // NL

        n = min(size(select%options), size(select%values))

        do i = 1, n
            html = html // '<option value="' // trim(select%values(i)) // '"'
            if (disabled_)                    html = html // ' disabled="disabled"'
            if (select%values(i) == selected) html = html // ' selected="selected"'
            html = html // '>' // trim(select%options(i)) // '</option>' // NL
        end do

        html = html // '</select>' // NL
    end function dm_html_select

    function dm_html_sensor(sensor) result(html)
        !! Returns sensor as HTML table. Input data will be trimmed and
        !! encoded.
        use :: dm_sensor
        type(sensor_type), intent(inout) :: sensor !! Sensor type.
        character(len=:), allocatable    :: html   !! Generated HTML.

        integer :: type

        type = sensor%type
        if (type < 0 .or. type > SENSOR_NTYPES - 1) type = 0

        html = H_TABLE // H_TBODY // &
               H_TR // H_TH // 'ID' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(sensor%id) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Node ID' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(sensor%node_id) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Type' // H_TH_END // &
               H_TD // trim(SENSOR_TYPE_NAMES(type)) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Name' // H_TH_END // &
               H_TD // dm_html_encode(sensor%name) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Serial Number' // H_TH_END // &
               H_TD // dm_html_encode(sensor%sn) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Meta' // H_TH_END // &
               H_TD // dm_html_encode(sensor%meta) // H_TD_END // H_TR_END // &
               H_TBODY_END // H_TABLE_END
    end function dm_html_sensor

    function dm_html_sensors(sensors, prefix) result(html)
        !! Returns table of sensors in HTML format. If argument `prefix` is
        !! passed, the sensor names are enclosed in HTML anchors, with the link
        !! set to `prefix`. Input data will be trimmed and encoded.
        use :: dm_sensor
        type(sensor_type), intent(inout)        :: sensors(:) !! Sensor types.
        character(len=*),  intent(in), optional :: prefix     !! Link address prefix.
        character(len=:), allocatable           :: html       !! Generated HTML.

        integer           :: i, t
        logical           :: is_anchor
        type(anchor_type) :: anchor

        is_anchor = .false.
        if (present(prefix)) is_anchor = .true.

        html = H_TABLE // H_THEAD // H_TR // &
               H_TH // '#'       // H_TH_END // &
               H_TH // 'ID'      // H_TH_END // &
               H_TH // 'Node ID' // H_TH_END // &
               H_TH // 'Name'    // H_TH_END // &
               H_TH // 'Type'    // H_TH_END // &
               H_TH // 'S/N'     // H_TH_END // &
               H_TH // 'Meta'    // H_TH_END // &
               H_TR_END // H_THEAD_END // H_TBODY

        do i = 1, size(sensors)
            t = sensors(i)%type
            if (t < 0 .or. t > SENSOR_NTYPES - 1) t = 0

            html = html // H_TR // H_TD // dm_itoa(i) // H_TD_END

            if (is_anchor) then
                ! Turn sensor name into link to `prefix`.
                anchor%link = prefix // dm_html_encode(sensors(i)%id)
                anchor%text = dm_html_encode(sensors(i)%id)
                html = html // H_TD // dm_html_anchor(anchor) // H_TD_END
            else
                html = html // H_TD // dm_html_encode(sensors(i)%id) // H_TD_END
            end if

            html = html // H_TD // dm_html_encode(sensors(i)%node_id) // H_TD_END // &
                   H_TD // dm_html_encode(sensors(i)%name) // H_TD_END // &
                   H_TD // trim(SENSOR_TYPE_NAMES(t)) // H_TD_END // &
                   H_TD // dm_html_encode(sensors(i)%sn) // H_TD_END // &
                   H_TD // dm_html_encode(sensors(i)%meta) // H_TD_END // H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_sensors

    pure function dm_html_small(str) result(html)
        !! Returns `<small>` element with encoded `str` enclosed.
        character(len=*), intent(in)  :: str  !! Element content.
        character(len=:), allocatable :: html !! Generated HTML.

        html = H_SMALL // dm_html_encode(str) // H_SMALL_END
    end function dm_html_small

    pure function dm_html_span(str, class) result(html)
        !! Returns `<span>` element of optional class, with encoded `str`
        !! enclosed.
        character(len=*), intent(in)           :: str   !! Element content.
        character(len=*), intent(in), optional :: class !! Element class.
        character(len=:), allocatable          :: html  !! Generated HTML.

        if (present(class)) then
            html = '<span class="' // trim(class) // '">' // dm_html_encode(str) // H_SPAN_END
        else
            html = H_SPAN // dm_html_encode(str) // H_SPAN_END
        end if
    end function dm_html_span

    function dm_html_target(target) result(html)
        !! Returns target as HTML table.
        use :: dm_target
        type(target_type), intent(inout) :: target !! Target type.
        character(len=:), allocatable    :: html   !! Generated HTML.

        html = H_TABLE // H_TBODY // &
               H_TR // H_TH // 'ID' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(target%id) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Name' // H_TH_END // &
               H_TD // dm_html_encode(target%name) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Meta' // H_TH_END // &
               H_TD // dm_html_encode(target%meta) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'State' // H_TH_END // &
               H_TD // dm_target_state_name(target%state) // ' (' // dm_itoa(target%state) // ')' // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'X' // H_TH_END // &
               H_TD // dm_ftoa(target%x) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Y' // H_TH_END // &
               H_TD // dm_ftoa(target%y) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Z' // H_TH_END // &
               H_TD // dm_ftoa(target%z) // H_TD_END // H_TR_END // &
               H_TBODY_END // H_TABLE_END
    end function dm_html_target

    function dm_html_targets(targets, prefix) result(html)
        !! Returns table of targets in HTML format. If argument `prefix` is
        !! passed, the target names are enclosed in HTML anchors, with the link
        !! set to `prefix`. Input data will be trimmed and encoded.
        use :: dm_target
        type(target_type), intent(inout)        :: targets(:) !! Target types.
        character(len=*),  intent(in), optional :: prefix     !! Link address prefix.
        character(len=:), allocatable           :: html       !! Generated HTML.

        integer           :: i
        logical           :: is_anchor
        type(anchor_type) :: anchor

        is_anchor = .false.
        if (present(prefix)) is_anchor = .true.

        html = H_TABLE // H_THEAD // H_TR // &
               H_TH // '#'     // H_TH_END // &
               H_TH // 'ID'    // H_TH_END // &
               H_TH // 'Name'  // H_TH_END // &
               H_TH // 'Meta'  // H_TH_END // &
               H_TH // 'State' // H_TH_END // &
               H_TH // 'X'     // H_TH_END // &
               H_TH // 'Y'     // H_TH_END // &
               H_TH // 'Z'     // H_TH_END // &
               H_TR_END // H_THEAD_END // H_TBODY

        do i = 1, size(targets)
            html = html // H_TR // H_TD // dm_itoa(i) // H_TD_END

            if (is_anchor) then
                ! Turn target name into link to `prefix`.
                anchor%link = prefix // dm_html_encode(targets(i)%id)
                anchor%text = dm_html_encode(targets(i)%id)
                html = html // H_TD // dm_html_anchor(anchor) // H_TD_END
            else
                html = html // H_TD // dm_html_encode(targets(i)%id) // H_TD_END
            end if

            html = html // H_TD // dm_html_encode(targets(i)%name)        // H_TD_END // &
                           H_TD // dm_html_encode(targets(i)%meta)        // H_TD_END // &
                           H_TD // dm_target_state_name(targets(i)%state) // H_TD_END // &
                           H_TD // dm_ftoa(targets(i)%x)                  // H_TD_END // &
                           H_TD // dm_ftoa(targets(i)%y)                  // H_TD_END // &
                           H_TD // dm_ftoa(targets(i)%z)                  // H_TD_END // H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_targets

    pure function dm_html_td(str, col_span, row_span) result(html)
        !! Returns `str` enclosed by `<td>` tag, with optional column or row
        !! span. The passed string will not be encoded or trimmed.
        character(len=*), intent(in)           :: str      !! Input string.
        integer,          intent(in), optional :: col_span !! Column span.
        integer,          intent(in), optional :: row_span !! Row span.
        character(len=:), allocatable          :: html     !! Generated HTML.

        html = '<td'
        if (present(col_span)) html = html // ' colspan="' // dm_itoa(col_span) // '"'
        if (present(row_span)) html = html // ' rowspan="' // dm_itoa(row_span) // '"'
        html = html // '>' // str // H_TD_END
    end function dm_html_td

    pure function dm_html_th(str, col_span, row_span) result(html)
        !! Returns `str` enclosed by `<th>` tag, with optional column or row
        !! span. The passed string will not be encoded or trimmed.
        character(len=*), intent(in)           :: str      !! Input string.
        integer,          intent(in), optional :: col_span !! Column span.
        integer,          intent(in), optional :: row_span !! Row span.
        character(len=:), allocatable          :: html     !! Generated HTML.

        html = '<th'
        if (present(col_span)) html = html // ' colspan="' // dm_itoa(col_span) // '"'
        if (present(row_span)) html = html // ' rowspan="' // dm_itoa(row_span) // '"'
        html = html // '>' // str // H_TH_END
    end function dm_html_th

    pure subroutine dm_html_select_create(select, size, error)
        !! Allocates memory for arrays in select type. Returns `E_ALLOC` on
        !! error.
        type(select_type), intent(out)           :: select !! Select type.
        integer,           intent(in)            :: size   !! Array size.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: stat

        if (present(error)) error = E_ALLOC
        allocate (select%options(size), select%values(size), stat=stat)
        if (stat /= 0) return
        if (present(error)) error = E_NONE
    end subroutine dm_html_select_create

    subroutine dm_html_select_destroy(select)
        !! Deallocates arrays in select type.
        type(select_type), intent(inout) :: select !! Select type.

        if (allocated(select%options)) deallocate (select%options)
        if (allocated(select%values))  deallocate (select%values)
    end subroutine dm_html_select_destroy

    subroutine dm_html_select_set(select, index, option, value, error)
        !! Sets option name and value in select type.
        type(select_type), intent(inout)         :: select !! Select type.
        integer,           intent(in)            :: index  !! Array index.
        character(len=*),  intent(in)            :: option !! Option name.
        character(len=*),  intent(in)            :: value  !! Option value.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: rc

        set_block: block
            rc = E_ALLOC
            if (.not. allocated(select%options) .or. &
                .not. allocated(select%values)) exit set_block

            rc = E_BOUNDS
            if (index > min(size(select%options), size(select%values))) &
                exit set_block

            select%options(index) = option
            select%values(index)  = value

            rc = E_NONE
        end block set_block

        if (present(error)) error = rc
    end subroutine dm_html_select_set
end module dm_html
