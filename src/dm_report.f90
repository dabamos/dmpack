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

    integer, parameter, public :: REPORT_FORMAT_LEN = PLOT_TERMINAL_NAME_LEN !! Plot format string length.
    integer, parameter, public :: REPORT_META_LEN   = 4096                   !! Plot meta description string length.
    integer, parameter, public :: REPORT_TITLE_LEN  = 256                    !! Plot title string length.

    type, public :: report_observ_type
        !! Single plot of observations.
        character(len=REPORT_FORMAT_LEN) :: format   = PLOT_TERMINAL_NAMES(PLOT_TERMINAL_SVG) !! Plot format.
        character(len=SENSOR_ID_LEN)     :: sensor   = ' '     !! Sensor id.
        character(len=TARGET_ID_LEN)     :: target   = ' '     !! Target id.
        character(len=RESPONSE_NAME_LEN) :: response = ' '     !! Response name.
        character(len=RESPONSE_UNIT_LEN) :: unit     = ' '     !! Response unit.
        character(len=REPORT_TITLE_LEN)  :: title    = ' '     !! Plot title.
        character(len=REPORT_TITLE_LEN)  :: subtitle = ' '     !! Plot sub-title.
        character(len=REPORT_META_LEN)   :: meta     = ' '     !! Plot description.
        character(len=8)                 :: color    = ' '     !! Foreground colour.
        integer                          :: width    = 1000    !! Plot width in pixels.
        integer                          :: height   = 400     !! Plot height in pixels.
        logical                          :: break    = .false. !! Add page break behind (for PDF output only).
        logical                          :: disabled = .false. !! Disable plot.
        real(kind=r8)                    :: scale    = 1.0_r8  !! Scale factor for respone value (optional).
    end type report_observ_type

    type, public :: report_plot_type
        !! Section plots of report.
        character(len=FILE_PATH_LEN)          :: database = ' '     !! Path to observation database (required).
        character(len=REPORT_TITLE_LEN)       :: title    = 'Plots' !! Section title.
        character(len=REPORT_META_LEN)        :: meta     = ' '     !! Description text.
        logical                               :: disabled = .false. !! Generate plots.
        type(report_observ_type), allocatable :: observs(:)         !! Plots to generate.
    end type report_plot_type

    type, public :: report_log_type
        !! Section logs of report.
        character(len=FILE_PATH_LEN)    :: database  = ' '         !! Path to observation database (required).
        character(len=REPORT_TITLE_LEN) :: title     = 'Logs'      !! Section title.
        character(len=REPORT_META_LEN)  :: meta      = ' '         !! Description text.
        integer                         :: min_level = LL_WARNING  !! Minimum log level.
        integer                         :: max_level = LL_CRITICAL !! Maximum log level.
        logical                         :: disabled  = .false.     !! Generate plots.
    end type report_log_type

    type, public :: report_type
        !! Report type with plot and log settings.
        character(len=NODE_ID_LEN)      :: node     = ' '      !! Node id.
        character(len=TIME_LEN)         :: from     = ' '      !! Timestamp (ISO 8601).
        character(len=TIME_LEN)         :: to       = ' '      !! Timestamp (ISO 8601).
        character(len=FILE_PATH_LEN)    :: output   = ' '      !! Path of output file.
        character(len=FILE_PATH_LEN)    :: style    = ' '      !! Path to CSS file that will be included into the report.
        character(len=REPORT_TITLE_LEN) :: title    = 'Report' !! Report title.
        character(len=REPORT_TITLE_LEN) :: subtitle = ' '      !! Report sub-title.
        character(len=REPORT_META_LEN)  :: meta     = ' '      !! Report description text.
        logical                         :: verbose  = .true.   !! Show warning, errors, and empty plot sections.
        type(report_plot_type)          :: plot                !! Plots section.
        type(report_log_type)           :: log                 !! Logs sections.
    end type report_type

    public :: dm_report_is_valid
contains
    logical function dm_report_is_valid(report) result(valid)
        !! Returns `.true.` if given report type is valid, else `.false.`.
        type(report_type), intent(inout) :: report !! Report type.

        integer :: i, n, terminal

        valid = .false.

        if (.not. dm_id_is_valid(report%node)) return
        if (len_trim(report%from) == 0)        return
        if (len_trim(report%to) == 0)          return

        if (.not. report%plot%disabled) then
            if (len_trim(report%plot%database) == 0) return

            n = 0
            if (allocated(report%plot%observs)) n = size(report%plot%observs)

            do i = 1, n
                terminal = dm_plot_terminal_from_name(report%plot%observs(i)%format)

                if (terminal /= PLOT_TERMINAL_GIF      .and. &
                    terminal /= PLOT_TERMINAL_PNG      .and. &
                    terminal /= PLOT_TERMINAL_PNGCAIRO .and. &
                    terminal /= PLOT_TERMINAL_SVG) return

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
