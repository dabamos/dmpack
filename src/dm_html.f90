! Author:  Philipp Engel
! Licence: ISC
module dm_html
    !! HyperText Markup Language (HTML) generator procedures. Classless HTML5
    !! synax is used.
    use :: dm_ascii, only: NL => ASCII_LF
    use :: dm_base64
    use :: dm_beat
    use :: dm_cgi
    use :: dm_error
    use :: dm_http
    use :: dm_kind
    use :: dm_log
    use :: dm_node
    use :: dm_observ
    use :: dm_request
    use :: dm_response
    use :: dm_sensor
    use :: dm_system
    use :: dm_target
    use :: dm_time
    use :: dm_util
    use :: dm_version
    implicit none (type, external)
    private

    ! HTML button types.
    integer, parameter, public :: HTML_BUTTON_TYPE_BUTTON  = 1
    integer, parameter, public :: HTML_BUTTON_TYPE_RESET   = 2
    integer, parameter, public :: HTML_BUTTON_TYPE_SUBMIT  = 3

    ! HTML input types.
    integer, parameter, public :: HTML_INPUT_TYPE_BUTTON         = 1
    integer, parameter, public :: HTML_INPUT_TYPE_CHECKBOX       = 2
    integer, parameter, public :: HTML_INPUT_TYPE_COLOR          = 3
    integer, parameter, public :: HTML_INPUT_TYPE_DATE           = 4
    integer, parameter, public :: HTML_INPUT_TYPE_DATETIME_LOCAL = 5
    integer, parameter, public :: HTML_INPUT_TYPE_EMAIL          = 6
    integer, parameter, public :: HTML_INPUT_TYPE_FILE           = 7
    integer, parameter, public :: HTML_INPUT_TYPE_HIDDEN         = 8
    integer, parameter, public :: HTML_INPUT_TYPE_IMAGE          = 9
    integer, parameter, public :: HTML_INPUT_TYPE_MONTH          = 10
    integer, parameter, public :: HTML_INPUT_TYPE_NUMBER         = 11
    integer, parameter, public :: HTML_INPUT_TYPE_PASSWORD       = 12
    integer, parameter, public :: HTML_INPUT_TYPE_RADIO          = 13
    integer, parameter, public :: HTML_INPUT_TYPE_RANGE          = 14
    integer, parameter, public :: HTML_INPUT_TYPE_RESET          = 15
    integer, parameter, public :: HTML_INPUT_TYPE_SEARCH         = 16
    integer, parameter, public :: HTML_INPUT_TYPE_SUBMIT         = 17
    integer, parameter, public :: HTML_INPUT_TYPE_TEL            = 18
    integer, parameter, public :: HTML_INPUT_TYPE_TEXT           = 19
    integer, parameter, public :: HTML_INPUT_TYPE_TIME           = 20
    integer, parameter, public :: HTML_INPUT_TYPE_URL            = 21
    integer, parameter, public :: HTML_INPUT_TYPE_WEEK           = 22

    ! Selection of HTML tags.
    character(len=*), parameter, public :: H_BODY           = '<body>' // NL
    character(len=*), parameter, public :: H_BODY_END       = '</body>' // NL
    character(len=*), parameter, public :: H_BR             = '<br>' // NL
    character(len=*), parameter, public :: H_CODE           = '<code>'
    character(len=*), parameter, public :: H_CODE_END       = '</code>'
    character(len=*), parameter, public :: H_COMMENT        = '<!-- '
    character(len=*), parameter, public :: H_COMMENT_END    = ' //-->'
    character(len=*), parameter, public :: H_DETAILS        = '<details>' // NL
    character(len=*), parameter, public :: H_DETAILS_END    = '</details>' // NL
    character(len=*), parameter, public :: H_DIV_CARD       = '<div class="card">' // NL
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
    character(len=*), parameter, public :: H_META_CHARSET   = '<meta charset="utf-8">' // NL
    character(len=*), parameter, public :: H_META_GENERATOR = '<meta name="generator" content="DMPACK">' // NL
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
        !! HTML select type.
        character(len=32), allocatable :: options(:)
        character(len=32), allocatable :: values(:)
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
    public :: dm_html_target
    public :: dm_html_targets
    public :: dm_html_td
    public :: dm_html_th
contains
    pure function dm_html_anchor(anchor) result(html)
        !! Returns HTML anchor tag.
        type(anchor_type), intent(in) :: anchor !! Anchor type.
        character(len=:), allocatable :: html   !! HTML.

        html = '<a href="' // dm_html_encode(anchor%link) // '">' // &
                              dm_html_encode(anchor%text) // '</a>'
    end function dm_html_anchor

    function dm_html_beat(beat, delta) result(html)
        !! Returns table of single beat in HTML format.
        type(beat_type),  intent(inout)        :: beat  !! Beat type.
        integer(kind=i8), intent(in), optional :: delta !! Time delta.
        character(len=:), allocatable          :: html  !! HTML.

        character(len=8)        :: beats_now, beats_sent, beats_recv
        character(len=TIME_LEN) :: now
        integer                 :: rc
        integer(kind=i8)        :: delta_
        type(time_delta_type)   :: time, time_delta, time_inter

        delta_ = huge(0_i8)
        if (present(delta)) delta_ = delta

        call dm_time_delta_from_seconds(time, int(beat%uptime, kind=i8))
        call dm_time_delta_from_seconds(time_inter, int(beat%interval, kind=i8))
        call dm_time_delta_from_seconds(time_delta, delta_)

        rc = dm_time_to_beats(beat%time_sent, beats_sent)
        rc = dm_time_to_beats(beat%time_recv, beats_recv)

        now = dm_time_now()
        rc = dm_time_to_beats(now, beats_now)

        html = H_TABLE // H_TBODY // &
               H_TR // H_TH // 'Node ID' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(beat%node_id) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Address' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(beat%address) // H_CODE_END // H_TD_END // H_TR_END // &
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
               H_TR // H_TH // 'Error' // H_TH_END // &
               H_TD // dm_error_message(beat%error) // ' (' // dm_itoa(beat%error) // ')' // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Status' // H_TH_END // H_TD

        if (delta_ <= int(beat%interval, kind=i8)) then
            html = html // H_EM // 'on-time' // H_EM_END
        else
            html = html // H_STRONG // 'overdue' // H_STRONG_END
        end if

        html = html // H_TD_END // H_TR_END // H_TBODY_END // H_TABLE_END
    end function dm_html_beat

    function dm_html_beats(beats, deltas, prefix) result(html)
        !! Returns table of heartbeats in HTML format. If argument `prefix` is
        !! passed, the node ids are enclosed in HTML anchors, with the link
        !! set to `prefix`.
        type(beat_type),   intent(inout)           :: beats(:)  !! Beat types.
        integer(kind=i8),  intent(inout), optional :: deltas(:) !! Time deltas.
        character(len=*),  intent(in),    optional :: prefix    !! GET argument name.
        character(len=:), allocatable              :: html      !! HTML.

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
                html = html // H_EM // 'on-time' // H_EM_END
            else
                html = html // H_STRONG // 'overdue' // H_STRONG_END
            end if

            html = html // H_TD_END // H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_beats

    pure function dm_html_button(type, str, disabled) result(html)
        !! Returns HTML button element. This function does not encode the
        !! argument.
        character(len=*), parameter :: BUTTON_TYPES(3) = [ &
            character(len=6) :: 'button', 'reset', 'submit' ]

        integer,          intent(in)           :: type     !! Button type enumerator.
        character(len=*), intent(in)           :: str      !! Label.
        logical,          intent(in), optional :: disabled !! Disabled flag.
        character(len=:), allocatable          :: html     !! HTML.

        integer :: button_type

        button_type = HTML_BUTTON_TYPE_BUTTON

        if (type >= HTML_BUTTON_TYPE_BUTTON .and. &
            type <= HTML_BUTTON_TYPE_SUBMIT) button_type = type

        html = '<button type="' // trim(BUTTON_TYPES(button_type)) // '"'

        if (present(disabled)) then
            html = html // ' disabled="disabled"'
        end if

        html = html // '>' // trim(str) // '</button>' // NL
    end function dm_html_button

    function dm_html_cgi_env(env) result(html)
        !! Returns HTML table of CGI environment variables.
        type(cgi_env_type), intent(inout) :: env  !! CGI environment variables.
        character(len=:), allocatable     :: html !! HTML.

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
        character(len=:), allocatable :: html !! HTML.

        html = H_COMMENT // str // H_COMMENT_END
    end function dm_html_comment

    function dm_html_data_uri(data, mime) result(uri)
        !! Returns base64-encoded data URI of given data and MIME type.
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
        !! replaced (", &, <, >).
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
        !! Returns HTML of error description.
        integer,          intent(in)           :: error_code !! DMPACK error code.
        character(len=*), intent(in), optional :: message    !! Error message.
        character(len=:), allocatable          :: html       !! HTML.

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
        character(len=:), allocatable          :: html    !! HTML.

        html = H_FIGURE // trim(content)

        if (present(caption)) then
            if (len_trim(caption) > 0) &
                html = html // H_FIGCAPTION // dm_html_encode(caption) // H_FIGCAPTION_END
        end if

        html = html // H_FIGURE_END
    end function dm_html_figure

    pure function dm_html_footer() result(html)
        !! Returns HTML footer.
        character(len=:), allocatable :: html !! HTML.

        html = H_MAIN_END // H_BODY_END // H_HTML_END
    end function dm_html_footer

    function dm_html_header(title, subtitle, style, internal_style, brand, navigation) result(html)
        !! Returns HTML header with DOCTYPE and optional CSS. A link to the
        !! style sheet file and internal CSS can be added. The page title
        !! matches the first heading. No heading will be added if a navigation
        !! array is passed. Instead, the title is set as navigation brand.
        character(len=*),  intent(in)              :: title          !! HTML page title and first heading.
        character(len=*),  intent(in),    optional :: subtitle       !! Subtitle.
        character(len=*),  intent(in),    optional :: style          !! Path to CSS file.
        character(len=*),  intent(in),    optional :: internal_style !! Additional CSS (inline).
        character(len=*),  intent(in),    optional :: brand          !! Brand title.
        type(anchor_type), intent(inout), optional :: navigation(:)  !! Navigation anchors.
        character(len=:), allocatable              :: html           !! HTML.

        integer :: i
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

        html = html // H_HEAD_END // H_BODY // H_MAIN // H_HEADER

        if (present(navigation)) then
            ! Brand.
            html = html // H_NAV // H_UL

            if (present(brand)) then
                html = html // H_LI // dm_html_encode(brand) // H_LI_END
            end if

            ! Navigation elements.
            do i = 1, size(navigation)
                html = html // H_LI // dm_html_anchor(navigation(i)) // H_LI_END
            end do

            html = html // H_UL_END // H_NAV_END
        else
            if (has_subtitle) then
                html = html // dm_html_heading(1, title, subtitle)
            else
                html = html // dm_html_heading(1, title)
            end if
        end if

        html = html // H_HEADER_END
    end function dm_html_header

    pure function dm_html_heading(level, str, small) result(html)
        !! Returns HTML heading of given level. Adds horizontal line
        !! after H1 and H2.
        integer,          intent(in)           :: level !! Heading level.
        character(len=*), intent(in)           :: str   !! Heading string.
        character(len=*), intent(in), optional :: small !! Sub-heading string.
        character(len=:), allocatable          :: html  !! HTML.

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
                    html = H_H1 // dm_html_encode(str) // dm_html_small(small) // H_H1_END // H_HR
                else
                    html = H_H1 // dm_html_encode(str) // H_H1_END // H_HR
                end if
        end select
    end function dm_html_heading

    pure function dm_html_image(src, alt) result(html)
        !! Returns HTML image tag. This function does not encode the
        !! arguments.
        character(len=*), intent(in)  :: src  !! Image source.
        character(len=*), intent(in)  :: alt  !! Image alt tag.
        character(len=:), allocatable :: html !! HTML.

        html = '<img src="' // src // '" alt="' // trim(alt) // '">' // NL
    end function dm_html_image

    pure function dm_html_input(type, checked, disabled, id, max, max_length, min, min_length, &
            name, pattern, placeholder, read_only, required, size, value) result(html)
        !! Returns HTML input element.
        character(len=*), parameter :: INPUT_TYPES(22) = [ character(len=14) :: &
            'button', 'checkbox', 'color', 'date', 'datetime-local', 'email', &
            'file', 'hidden', 'image', 'month', 'number', 'password', 'radio', &
            'range', 'reset', 'search', 'submit', 'tel', 'text', 'time', 'url', &
            'week' ]

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
        character(len=:), allocatable          :: html        !! HTML.

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
        character(len=:), allocatable          :: html !! HTML.

        if (present(for)) then
            html = '<label for="' // trim(for) // '">' // trim(str) // '</label>' // NL
        else
            html = '<label>' // trim(str) // '</label>' // NL
        end if
    end function dm_html_label

    pure function dm_html_link(rel, href) result(html)
        !! Returns link element.
        character(len=*), intent(in)  :: rel  !! The rel attribute.
        character(len=*), intent(in)  :: href !! The href attribute.
        character(len=:), allocatable :: html !! HTML.

        html = '<link rel="' // dm_html_encode(rel) // '" href="' // dm_html_encode(href) // '">'
    end function dm_html_link

    function dm_html_log(log, prefix_node, prefix_sensor, prefix_target, prefix_observ) result(html)
        !! Returns log as HTML table.
        type(log_type),   intent(inout)        :: log           !! Log type.
        character(len=*), intent(in), optional :: prefix_node   !! Node link prefix.
        character(len=*), intent(in), optional :: prefix_sensor !! Sensor link prefix.
        character(len=*), intent(in), optional :: prefix_target !! Target link prefix.
        character(len=*), intent(in), optional :: prefix_observ !! Observation link prefix.
        character(len=:), allocatable          :: html          !! HTML.

        character(len=:), allocatable :: nid
        character(len=:), allocatable :: sid
        character(len=:), allocatable :: tid
        character(len=:), allocatable :: oid

        integer           :: level
        type(anchor_type) :: anchor

        level = max(min(LOG_NLEVEL, log%level), LOG_NONE)

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
               H_TD // trim(LOG_LEVEL_NAMES(level)) // ' (' // dm_itoa(level) // ')' // H_TD_END // H_TR_END // &
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
        !! table. Optionally, a maximum log message length can be set.
        type(log_type),   intent(inout)        :: logs(:) !! Log types.
        logical,          intent(in), optional :: node    !! Show node id column.
        character(len=*), intent(in), optional :: prefix  !! Link address prefix.
        integer,          intent(in), optional :: max_len !! Max. log message length.
        character(len=:), allocatable          :: html    !! HTML.

        integer           :: i, level, max_len_
        logical           :: is_anchor, node_
        type(anchor_type) :: anchor

        node_ = .true.
        if (present(node)) node_ = node

        is_anchor = .false.
        if (present(prefix)) is_anchor = .true.

        html = H_TABLE // H_THEAD // H_TR // &
               H_TH // '#' // H_TH_END // &
               H_TH // 'Timestamp' // H_TH_END

        if (node_) html = html // H_TH // 'Node ID' // H_TH_END

        html = html // H_TH // 'Source' // H_TH_END // &
               H_TH // 'Level' // H_TH_END // &
               H_TH // 'Error' // H_TH_END // &
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

            level = max(min(LOG_NLEVEL, logs(i)%level), LOG_NONE)

            max_len_ = len_trim(logs(i)%message)
            if (present(max_len)) max_len_ = min(max_len_, max_len)

            html = html // H_TD // dm_html_encode(logs(i)%source) // H_TD_END // &
                   H_TD // trim(LOG_LEVEL_NAMES(level)) // H_TD_END // &
                   H_TD // dm_itoa(logs(i)%error) // H_TD_END // &
                   H_TD // dm_html_encode(logs(i)%message(1:max_len_)) // H_TD_END // H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_logs

    function dm_html_nav(anchors) result(html)
        !! Returns HTML navigation element with unordered list of links.
        type(anchor_type), intent(inout) :: anchors(:) !! Anchor types.
        character(len=:), allocatable    :: html       !! HTML.

        integer :: i

        html = H_NAV // H_UL

        do i = 1, size(anchors)
            html = html // H_LI // dm_html_anchor(anchors(i)) // H_LI_END
        end do

        html = html // H_UL_END // H_NAV_END
    end function dm_html_nav

    function dm_html_node(node) result(html)
        !! Returns sensor node as HTML table.
        type(node_type), intent(inout) :: node !! Node type.
        character(len=:), allocatable  :: html !! HTML.

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
        !! Returns sensor nodes as HTML table.
        type(node_type),  intent(inout)        :: nodes(:) !! Node types.
        character(len=*), intent(in), optional :: prefix   !! Link address prefix.
        character(len=:), allocatable          :: html     !! HTML.

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
        !! Returns observation as HTML table.
        type(observ_type), intent(inout)        :: observ        !! Observation type.
        character(len=*),  intent(in), optional :: prefix_node   !! Node link prefix.
        character(len=*),  intent(in), optional :: prefix_sensor !! Sensor link prefix.
        character(len=*),  intent(in), optional :: prefix_target !! Target link prefix.
        character(len=:), allocatable           :: html          !! HTML.

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
               H_TD // H_CODE // dm_html_encode(observ%name) // H_CODE_END //  H_TD_END // H_TR_END // &
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

        html = html // H_TBODY_END // H_TABLE_END // dm_html_heading(3, 'Requests')

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
        !! optional.
        type(observ_type), intent(inout)        :: observs(:) !! Observation types.
        character(len=*),  intent(in), optional :: prefix     !! Link address prefix.
        logical,           intent(in), optional :: id         !! Show observation ids.
        logical,           intent(in), optional :: node_id    !! Show node ids.
        logical,           intent(in), optional :: sensor_id  !! Show sensor ids.
        logical,           intent(in), optional :: target_id  !! Show target ids.
        logical,           intent(in), optional :: name       !! Show observation names.
        logical,           intent(in), optional :: error      !! Show erros.
        character(len=:), allocatable           :: html       !! HTML.

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

    pure function dm_html_p(str) result(html)
        !! Returns HTML paragraph. This function does not encode the argument.
        character(len=*), intent(in)  :: str  !! Paragraph string.
        character(len=:), allocatable :: html !! HTML.

        html = H_P // str // H_P_END
    end function dm_html_p

    pure function dm_html_pre(str, code) result(html)
        !! Returns HTML preformatted text, with optional `<code>` tag.
        !! This function does not encode the argument.
        character(len=*), intent(in)           :: str  !! Content string.
        logical,          intent(in), optional :: code !! Additional code tag?
        character(len=:), allocatable          :: html !! HTML.

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
        !! Returns request as HTML table.
        type(request_type), intent(inout) :: request !! Observation request type.
        character(len=:), allocatable     :: html    !! HTML.

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
        !! Returns responses as HTML table.
        type(response_type), intent(inout) :: responses(:) !! Observation response type.
        character(len=:), allocatable      :: html         !! HTML.

        integer :: i

        html = H_TABLE // H_THEAD // H_TR // H_TH // '#' // H_TH_END // &
               H_TH // 'Response Name' // H_TH_END // H_TH // 'Value' // H_TH_END // &
               H_TH // 'Unit' // H_TH_END // H_TH // 'Error' // H_TH_END // H_THEAD_END // H_TBODY

        do i = 1, size(responses)
            html = html // H_TR // H_TD // dm_itoa(i) // H_TD_END // &
                   H_TD // dm_html_encode(responses(i)%name) // H_TD_END // &
                   H_TD // dm_ftoa(responses(i)%value) // H_TD_END // &
                   H_TD // dm_html_encode(responses(i)%unit) // H_TD_END // &
                   H_TD // dm_error_message(responses(i)%error) // &
                           ' (' // dm_itoa(responses(i)%error) // ')' // H_TD_END // H_TR_END // &
                   H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_responses

    function dm_html_select(select, id, name, selected) result(html)
        !! Returns HTML select element with option values.  This function does
        !! not encode the arguments.
        type(select_type), intent(inout) :: select   !! HTML select type.
        character(len=*),  intent(in)    :: id       !! Select id.
        character(len=*),  intent(in)    :: name     !! Select name.
        character(len=*),  intent(in)    :: selected !! Selected element.
        character(len=:), allocatable    :: html     !! HTML.

        integer :: i, n

        html = '<select id="' // id // '" name="' // name // '">' // NL

        n = min(size(select%options), size(select%values))

        do i = 1, n
            html = html // '<option value="' // trim(select%values(i)) // '"'
            if (select%values(i) == selected) html = html // ' selected="selected"'
            html = html // '>' // trim(select%options(i)) // '</option>' // NL
        end do

        html = html // '</select>' // NL
    end function dm_html_select

    function dm_html_sensor(sensor) result(html)
        !! Returns sensor as HTML table.
        type(sensor_type), intent(inout) :: sensor !! Sensor type.
        character(len=:), allocatable    :: html   !! HTML.

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
        !! set to `prefix`.
        type(sensor_type), intent(inout)        :: sensors(:) !! Sensor types.
        character(len=*),  intent(in), optional :: prefix     !! Link address prefix.
        character(len=:), allocatable           :: html       !! HTML.

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
        !! Returns `<small>` element with given string.
        character(len=*), intent(in)  :: str  !! Element content.
        character(len=:), allocatable :: html !! HTML.

        html = H_SMALL // dm_html_encode(str) // H_SMALL_END
    end function dm_html_small

    function dm_html_target(target) result(html)
        !! Returns target as HTML table.
        type(target_type), intent(inout) :: target !! Target type.
        character(len=:), allocatable    :: html   !! HTML.

        html = H_TABLE // H_TBODY // &
               H_TR // H_TH // 'ID' // H_TH_END // &
               H_TD // H_CODE // dm_html_encode(target%id) // H_CODE_END // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Name' // H_TH_END // &
               H_TD // dm_html_encode(target%name) // H_TD_END // H_TR_END // &
               H_TR // H_TH // 'Meta' // H_TH_END // &
               H_TD // dm_html_encode(target%meta) // H_TD_END // H_TR_END // &
               H_TBODY_END // H_TABLE_END
    end function dm_html_target

    function dm_html_targets(targets, prefix) result(html)
        !! Returns table of targets in HTML format. If argument `prefix` is
        !! passed, the target names are enclosed in HTML anchors, with the link
        !! set to `prefix`.
        type(target_type), intent(inout)        :: targets(:) !! Target types.
        character(len=*),  intent(in), optional :: prefix     !! Link address prefix.
        character(len=:), allocatable           :: html       !! HTML.

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

            html = html // H_TD // dm_html_encode(targets(i)%name) // H_TD_END // &
                           H_TD // dm_html_encode(targets(i)%meta) // H_TD_END // H_TR_END
        end do

        html = html // H_TBODY_END // H_TABLE_END
    end function dm_html_targets

    pure function dm_html_td(str, col_span, row_span) result(html)
        !! Returns `str` enclosed by `<td>` tag, with optional column or row span.
        character(len=*), intent(in)           :: str      !! Input string.
        integer,          intent(in), optional :: col_span !! Column span.
        integer,          intent(in), optional :: row_span !! Row span.
        character(len=:), allocatable          :: html     !! HTML.

        html = '<td'
        if (present(col_span)) html = html // ' colspan="' // dm_itoa(col_span) // '"'
        if (present(row_span)) html = html // ' rowspan="' // dm_itoa(row_span) // '"'
        html = html // '>' // str // H_TD_END
    end function dm_html_td

    pure function dm_html_th(str, col_span, row_span) result(html)
        !! Returns `str` enclosed by `<th>` tag, with optional column or row span.
        character(len=*), intent(in)           :: str      !! Input string.
        integer,          intent(in), optional :: col_span !! Column span.
        integer,          intent(in), optional :: row_span !! Row span.
        character(len=:), allocatable          :: html     !! HTML.

        html = '<th'
        if (present(col_span)) html = html // ' colspan="' // dm_itoa(col_span) // '"'
        if (present(row_span)) html = html // ' rowspan="' // dm_itoa(row_span) // '"'
        html = html // '>' // str // H_TH_END
    end function dm_html_th

    pure subroutine dm_html_select_create(select, size, stat)
        !! Allocates memory for arrays in select type.
        type(select_type), intent(out)           :: select !! Select type.
        integer,           intent(in)            :: size   !! Array size.
        integer,           intent(out), optional :: stat   !! Allocation status.

        integer :: rc

        if (present(stat)) stat = E_ALLOC
        allocate (select%options(size), select%values(size), stat=rc)
        if (rc /= 0) return
        if (present(stat)) stat = E_NONE
    end subroutine dm_html_select_create

    subroutine dm_html_select_destroy(select)
        !! Deallocates array in select type.
        type(select_type), intent(inout) :: select !! Select type.

        if (allocated(select%options)) deallocate (select%options)
    end subroutine dm_html_select_destroy

    subroutine dm_html_select_set(select, index, option, value, stat)
        !! Sets option name and value in select type.
        type(select_type), intent(inout)         :: select !! Select type.
        integer,           intent(in)            :: index  !! Array index.
        character(len=*),  intent(in)            :: option !! Option name.
        character(len=*),  intent(in)            :: value  !! Option value.
        integer,           intent(out), optional :: stat   !! Status.

        if (present(stat)) stat = E_ALLOC
        if (.not. allocated(select%options) .or. &
            .not. allocated(select%values)) return

        if (present(stat)) stat = E_BOUNDS
        if (index > min(size(select%options), size(select%values))) return

        select%options(index) = option
        select%values(index)  = value

        if (present(stat)) stat = E_NONE
    end subroutine dm_html_select_set
end module dm_html
