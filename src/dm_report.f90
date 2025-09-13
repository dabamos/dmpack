! Author:  Philipp Engel
! Licence: ISC
module dm_report
    !! Derived types for report generation.
    use :: dm_file,     only: FILE_PATH_LEN
    use :: dm_id
    use :: dm_kind
    use :: dm_log
    use :: dm_node,     only: NODE_ID_LEN
    use :: dm_plot
    use :: dm_response, only: RESPONSE_NAME_LEN, RESPONSE_UNIT_LEN
    use :: dm_sensor,   only: SENSOR_ID_LEN
    use :: dm_target,   only: TARGET_ID_LEN
    use :: dm_time,     only: TIME_LEN
    implicit none (type, external)
    private

    integer, parameter, public :: REPORT_FORMAT_LEN = PLOT_TERMINAL_NAME_LEN !! Max. plot format name length.
    integer, parameter, public :: REPORT_META_LEN   = 4096                   !! Max. plot meta description length.
    integer, parameter, public :: REPORT_TITLE_LEN  = 256                    !! Max. plot title length.

    integer, parameter, public :: REPORT_FORMAT_NONE = 0 !! Invalid format.
    integer, parameter, public :: REPORT_FORMAT_HTML = 1 !! HTML5.
    integer, parameter, public :: REPORT_FORMAT_PDF  = 2 !! PDF.
    integer, parameter, public :: REPORT_FORMAT_PS   = 3 !! PostScript.
    integer, parameter, public :: REPORT_FORMAT_LAST = 3 !! Never use this.

    integer, parameter, public :: REPORT_FORMAT_NAME_LEN = 4 !! Max. report format name.

    character(*), parameter, public :: REPORT_FORMAT_NAMES(REPORT_FORMAT_NONE:REPORT_FORMAT_LAST) = [ &
        character(REPORT_FORMAT_NAME_LEN) :: 'none', 'html', 'pdf', 'ps' &
    ] !! Report format names.

    type, public :: report_observ_type
        !! Single plot of observations.
        character(REPORT_FORMAT_LEN) :: format    = PLOT_TERMINAL_NAMES(PLOT_TERMINAL_SVG) !! Plot format.
        character(SENSOR_ID_LEN)     :: sensor    = ' '     !! Sensor id.
        character(TARGET_ID_LEN)     :: target    = ' '     !! Target id.
        character(RESPONSE_NAME_LEN) :: response  = ' '     !! Response name.
        character(RESPONSE_UNIT_LEN) :: unit      = ' '     !! Response unit.
        character(REPORT_TITLE_LEN)  :: title     = ' '     !! Plot title.
        character(REPORT_TITLE_LEN)  :: subtitle  = ' '     !! Plot sub-title.
        character(REPORT_META_LEN)   :: meta      = ' '     !! Plot description.
        character(8)                 :: color     = ' '     !! Foreground colour.
        integer                      :: width     = 1000    !! Plot width in pixels.
        integer                      :: height    = 400     !! Plot height in pixels.
        logical                      :: pagebreak = .false. !! Add page break behind (for PDF output only).
        logical                      :: disabled  = .false. !! Disable plot.
        real(kind=r8)                :: scale     = 1.0_r8  !! Scale factor for respone value (optional).
    end type report_observ_type

    type, public :: report_plot_type
        !! Section plots of report.
        character(FILE_PATH_LEN)              :: database = ' '     !! Path to observation database (required).
        character(REPORT_TITLE_LEN)           :: title    = 'Plots' !! Section title.
        character(REPORT_META_LEN)            :: meta     = ' '     !! Description text.
        logical                               :: disabled = .false. !! Generate plots.
        type(report_observ_type), allocatable :: observs(:)         !! Plots to generate.
    end type report_plot_type

    type, public :: report_log_type
        !! Section logs of report.
        character(FILE_PATH_LEN)    :: database  = ' '         !! Path to observation database (required).
        character(REPORT_TITLE_LEN) :: title     = 'Logs'      !! Section title.
        character(REPORT_META_LEN)  :: meta      = ' '         !! Description text.
        integer                     :: min_level = LL_WARNING  !! Minimum log level.
        integer                     :: max_level = LL_CRITICAL !! Maximum log level.
        logical                     :: disabled  = .false.     !! Generate plots.
    end type report_log_type

    type, public :: report_type
        !! Report type with plot and log settings.
        character(NODE_ID_LEN)      :: node     = ' '                !! Node id.
        character(TIME_LEN)         :: from     = ' '                !! Timestamp (ISO 8601).
        character(TIME_LEN)         :: to       = ' '                !! Timestamp (ISO 8601).
        character(FILE_PATH_LEN)    :: output   = ' '                !! Path of output file.
        character(FILE_PATH_LEN)    :: style    = ' '                !! Path to CSS file to inline (HTML only).
        character(REPORT_TITLE_LEN) :: title    = 'Report'           !! Report title.
        character(REPORT_TITLE_LEN) :: subtitle = ' '                !! Report sub-title.
        character(REPORT_META_LEN)  :: meta     = ' '                !! Report description text.
        integer                     :: format   = REPORT_FORMAT_NONE !! Format (HTML, PDF, PS).
        logical                     :: verbose  = .true.             !! Include warnings, errors, and empty plot sections.
        type(report_plot_type)      :: plot                          !! Plots section.
        type(report_log_type)       :: log                           !! Logs sections.
    end type report_type

    public :: dm_report_format_from_name
    public :: dm_report_format_is_valid
    public :: dm_report_is_valid
contains
    pure elemental integer function dm_report_format_from_name(name) result(format)
        !! Returns report format enumerator from name. On error, the result is
        !! `REPORT_FORMAT_NONE`.
        use :: dm_string, only: dm_to_lower

        character(*), intent(in) :: name !! Format name.

        character(REPORT_FORMAT_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (REPORT_FORMAT_NAMES(REPORT_FORMAT_HTML)); format = REPORT_FORMAT_HTML
            case (REPORT_FORMAT_NAMES(REPORT_FORMAT_PDF));  format = REPORT_FORMAT_PDF
            case (REPORT_FORMAT_NAMES(REPORT_FORMAT_PS));   format = REPORT_FORMAT_PS
            case default;                                   format = REPORT_FORMAT_NONE
        end select
    end function dm_report_format_from_name

    pure elemental logical function dm_report_format_is_valid(format) result(valid)
        !! Returns `.true.` if format enumerator is valid. The format
        !! `REPORT_FORMAT_NONE` is invalid.
        integer, intent(in) :: format !! Report format type (`REPORT_FORMAT_*`).

        valid = (format > REPORT_FORMAT_NONE .and. format <= REPORT_FORMAT_LAST)
    end function dm_report_format_is_valid

    logical function dm_report_is_valid(report) result(valid)
        !! Returns `.true.` if given report type is valid, else `.false.`. The
        !! attributes `plot` and `log` are only validated if enabled.
        type(report_type), intent(inout) :: report !! Report type.

        integer :: i, n, terminal

        valid = .false.

        if (.not. dm_id_is_valid(report%node))              return
        if (len_trim(report%from) == 0)                     return
        if (len_trim(report%to) == 0)                       return
        if (.not. dm_report_format_is_valid(report%format)) return

        if (.not. report%plot%disabled) then
            if (len_trim(report%plot%database) == 0) return

            n = 0
            if (allocated(report%plot%observs)) n = size(report%plot%observs)

            do i = 1, n
                terminal = dm_plot_terminal_from_name(report%plot%observs(i)%format)

                select case (report%format)
                    case (REPORT_FORMAT_HTML)
                        if (terminal /= PLOT_TERMINAL_GIF      .and. &
                            terminal /= PLOT_TERMINAL_PNG      .and. &
                            terminal /= PLOT_TERMINAL_PNGCAIRO .and. &
                            terminal /= PLOT_TERMINAL_SVG) return

                    case (REPORT_FORMAT_PDF, REPORT_FORMAT_PS)
                        if (terminal /= PLOT_TERMINAL_POSTSCRIPT) return
                end select

                if (.not. dm_id_is_valid(report%plot%observs(i)%sensor)) return
                if (.not. dm_id_is_valid(report%plot%observs(i)%target)) return
                if (len_trim(report%plot%observs(i)%response) == 0)      return
                if (report%plot%observs(i)%width <= 0)                   return
                if (report%plot%observs(i)%height <= 0)                  return
            end do
        end if

        if (.not. report%log%disabled) then
            if (len_trim(report%log%database) == 0)                return
            if (.not. dm_log_level_is_valid(report%log%min_level)) return
            if (.not. dm_log_level_is_valid(report%log%max_level)) return
            if (report%log%min_level > report%log%max_level)       return
        end if

        valid = .true.
    end function dm_report_is_valid
end module dm_report
