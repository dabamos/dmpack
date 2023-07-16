! Author:  Philipp Engel
! Licence: ISC
module dm_report
    !! Derived types for report generation.
    use :: dm_id
    use :: dm_log
    use :: dm_node
    use :: dm_observ
    use :: dm_plot
    use :: dm_sensor
    use :: dm_target
    use :: dm_time
    use :: dm_type
    implicit none (type, external)
    private

    integer, parameter, public :: REPORT_FORMAT_LEN = 8
    integer, parameter, public :: REPORT_META_LEN   = 512
    integer, parameter, public :: REPORT_PATH_LEN   = 4096
    integer, parameter, public :: REPORT_TITLE_LEN  = 80

    type, public :: report_observ_type
        !! Single plot of observations.
        character(len=REPORT_FORMAT_LEN) :: format   = 'svg' !! Plot format.
        character(len=SENSOR_ID_LEN)     :: sensor   = ' '   !! Sensor id.
        character(len=TARGET_ID_LEN)     :: target   = ' '   !! Target id.
        character(len=RESPONSE_NAME_LEN) :: response = ' '   !! Response name.
        character(len=RESPONSE_UNIT_LEN) :: unit     = ' '   !! Response unit.
        character(len=REPORT_TITLE_LEN)  :: title    = ' '   !! Plot title.
        character(len=REPORT_TITLE_LEN)  :: subtitle = ' '   !! Plot sub-title.
        character(len=REPORT_META_LEN)   :: meta     = ' '   !! Plot description.
        character(len=8)                 :: color    = ' '   !! Foreground colour.
        integer                          :: width    = 1000  !! Plot width in pixels.
        integer                          :: height   = 400   !! Plot height in pixels.
    end type report_observ_type

    type, public :: report_plot_type
        !! Section plots of report.
        logical                               :: disabled = .false. !! Generate plots.
        character(len=REPORT_PATH_LEN)        :: database = ' '     !! Path to observation database (required).
        character(len=REPORT_META_LEN)        :: meta     = ' '     !! Description text.
        character(len=REPORT_TITLE_LEN)       :: title    = 'Plots' !! Section title.
        type(report_observ_type), allocatable :: observs(:)         !! Plots to generate.
    end type report_plot_type

    type, public :: report_log_type
        !! Section logs of report.
        logical                         :: disabled  = .false.      !! Generate plots.
        integer                         :: min_level = LOG_WARNING  !! Minimum log level.
        integer                         :: max_level = LOG_CRITICAL !! Maximum log level.
        character(len=REPORT_PATH_LEN)  :: database  = ' '          !! Path to observation database (required).
        character(len=REPORT_META_LEN)  :: meta      = ' '          !! Description text.
        character(len=REPORT_TITLE_LEN) :: title     = 'Logs'       !! Section title.
    end type report_log_type

    type, public :: report_type
        !! Report type with plot and log settings.
        character(len=NODE_ID_LEN)      :: node     = ' '      !! Node id.
        character(len=TIME_LEN)         :: from     = ' '      !! Timestamp (ISO 8601).
        character(len=TIME_LEN)         :: to       = ' '      !! Timestamp (ISO 8601).
        character(len=REPORT_META_LEN)  :: meta     = ' '      !! Report description text.
        character(len=REPORT_PATH_LEN)  :: output   = ' '      !! Path of output file.
        character(len=REPORT_TITLE_LEN) :: title    = 'Report' !! Report title.
        character(len=REPORT_TITLE_LEN) :: subtitle = ' '      !! Report sub-title.
        character(len=REPORT_PATH_LEN)  :: style    = ' '      !! Path to CSS file that will be included into the report.
        type(report_plot_type)          :: plot                !! Plots section.
        type(report_log_type)           :: log                 !! Logs sections.
    end type report_type

    public :: dm_report_valid
contains
    logical function dm_report_valid(report) result(valid)
        !! Returns whether given report type is valid or not.
        type(report_type), intent(inout) :: report !! Report type.
        integer                          :: i, n

        valid = .false.

        if (.not. dm_id_valid(report%node)) return
        if (len_trim(report%from) == 0) return
        if (len_trim(report%to) == 0) return

        if (.not. report%plot%disabled) then
            if (len_trim(report%plot%database) == 0) return

            n = 0
            if (allocated(report%plot%observs)) n = size(report%plot%observs)

            do i = 1, n
                if (report%plot%observs(i)%format /= 'gif' .and. &
                    report%plot%observs(i)%format /= 'png' .and. &
                    report%plot%observs(i)%format /= 'pngcairo' .and. &
                    report%plot%observs(i)%format /= 'svg') return

                if (.not. dm_id_valid(report%plot%observs(i)%sensor)) return
                if (.not. dm_id_valid(report%plot%observs(i)%target)) return
                if (len_trim(report%plot%observs(i)%response) == 0) return
                if (report%plot%observs(i)%width <= 0) return
                if (report%plot%observs(i)%height <= 0) return
            end do
        end if

        if (.not. report%log%disabled) then
            if (report%log%min_level < LOG_NONE) return
            if (report%log%min_level > LOG_CRITICAL) return
            if (report%log%max_level < LOG_NONE) return
            if (report%log%max_level > LOG_CRITICAL) return
            if (report%log%min_level > report%log%max_level) return
            if (len_trim(report%log%database) == 0) return
        end if

        valid = .true.
    end function dm_report_valid
end module dm_report
