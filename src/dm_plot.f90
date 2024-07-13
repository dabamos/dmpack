! Author:  Philipp Engel
! Licence: ISC
module dm_plot
    !! Abstraction layer over Gnuplot.
    use, intrinsic :: iso_c_binding
    use :: dm_dp
    use :: dm_error
    use :: dm_kind
    use :: dm_pipe
    use :: dm_string
    use :: dm_time
    implicit none (type, external)
    private

    integer(kind=i8), parameter :: PLOT_BUFFER_LEN = 512 !! Line buffer length.

    ! Line styles.
    integer, parameter, public :: PLOT_STYLE_NONE        = 0 !! Invalid style.
    integer, parameter, public :: PLOT_STYLE_LINES       = 1 !! Lines.
    integer, parameter, public :: PLOT_STYLE_LINESPOINTS = 2 !! Lines with symbols.
    integer, parameter, public :: PLOT_STYLE_DOTS        = 3 !! Dots.
    integer, parameter, public :: PLOT_STYLE_POINTS      = 4 !! Points.
    integer, parameter, public :: PLOT_STYLE_LAST        = 4 !! Never use this.

    ! Gnuplot terminals, see:
    ! http://gnuplot.info/docs_5.5/Terminals.html
    integer, parameter, public :: PLOT_TERM_NONE      = 0 !! Invalid terminal.
    integer, parameter, public :: PLOT_TERM_ANSI      = 1 !! ASCII with ANSI colours (dumb).
    integer, parameter, public :: PLOT_TERM_ASCII     = 2 !! ASCII (dumb).
    integer, parameter, public :: PLOT_TERM_GIF       = 3 !! GIF (libgd).
    integer, parameter, public :: PLOT_TERM_PNG       = 4 !! PNG (libgd).
    integer, parameter, public :: PLOT_TERM_PNG_CAIRO = 5 !! PNG (libcairo).
    integer, parameter, public :: PLOT_TERM_SIXEL     = 6 !! Sixel (libgd).
    integer, parameter, public :: PLOT_TERM_SVG       = 7 !! SVG.
    integer, parameter, public :: PLOT_TERM_X11       = 8 !! X11.
    integer, parameter, public :: PLOT_TERM_LAST      = 8 !! Never use this.

    integer, parameter, public :: PLOT_TERM_NAME_LEN  = 8 !! Max. terminal name length.

    character(len=*), parameter, public :: PLOT_TERM_NAMES(PLOT_TERM_NONE:PLOT_TERM_LAST) = [ &
        character(len=PLOT_TERM_NAME_LEN) :: 'none', 'ansi', 'ascii', 'gif', 'png', &
        'pngcairo', 'sixelgd', 'svg', 'x11' ] !! Gnuplot terminal names.

    character(len=*), parameter, public :: PLOT_GNUPLOT     = 'gnuplot'           !! Gnuplot binary.
    character(len=*), parameter, public :: PLOT_TIME_FORMAT = '%Y-%m-%dT%H:%M:%S' !! Datetime format.

    type, public :: plot_type
        !! Plot type of plot settings.
        integer                  :: term       = PLOT_TERM_NONE   !! Output terminal.
        integer                  :: style      = PLOT_STYLE_LINES !! Plot line style.
        integer                  :: width      = 800              !! Plot width.
        integer                  :: height     = 300              !! Plot height.
        character(len=1024)      :: output     = ' '              !! Output file name.
        character(len=8)         :: background = ' '              !! Background colour (optional).
        character(len=8)         :: foreground = '#3b4cc0'        !! Foreground colour (optional).
        character(len=8)         :: graph      = '#ffffff'        !! Graph background colour.
        character(len=1024)      :: font       = ' '              !! Font name or file path (optional).
        character(len=128)       :: title      = ' '              !! Plot title (optional).
        character(len=128)       :: xlabel     = ' '              !! X label (optional).
        character(len=128)       :: ylabel     = ' '              !! Y label (optional).
        character(len=TIME_LEN)  :: xrange(2)  = ' '              !! X axis range.
        real(kind=r8)            :: yrange(2)  = 0.0_r8           !! Y axis range.
        logical                  :: bidirect   = .false.          !! Bi-directional anonymous pipe.
        logical                  :: persist    = .false.          !! Persistent Gnuplot process (use only with X11).
        logical                  :: xautoscale = .true.           !! Auto-scale X axis.
        logical                  :: yautoscale = .true.           !! Auto-scale Y axis.
        logical                  :: grid       = .true.           !! Show grid.
        logical                  :: legend     = .false.          !! Show legend.
        type(pipe_type), private :: stdin                         !! Gnuplot’s standard input.
        type(pipe_type), private :: stdout                        !! Gnuplot’s standard output.
        type(pipe_type), private :: stderr                        !! Gnuplot’s standard error.
    end type plot_type

    public :: dm_plot_error
    public :: dm_plot_lines
    public :: dm_plot_read
    public :: dm_plot_term_from_name
    public :: dm_plot_term_valid
    public :: dm_plot_version

    private :: plot_output
    private :: plot_set_graph
    private :: plot_set_grid
    private :: plot_set_label
    private :: plot_set_legend
    private :: plot_set_term
    private :: plot_set_title
    private :: plot_set_xaxis
    private :: plot_set_yaxis
    private :: plot_write
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer(kind=i8) function dm_plot_error(plot, bytes) result(n)
        !! Returns Gnuplot's standard error output in allocatable character
        !! string `bytes`. The result is allocated but empty if no output to
        !! standard error has been made.
        type(plot_type),               intent(inout) :: plot  !! Plot settings.
        character(len=:), allocatable, intent(out)   :: bytes !! Bytes returned by Gnuplot.

        character(len=PLOT_BUFFER_LEN) :: buffer
        integer(kind=i8)               :: sz

        n = 0_i8
        bytes = ''

        do
            sz = dm_pipe_read(plot%stderr, buffer)
            if (sz <= 0) exit
            bytes = bytes // buffer(1:sz)
            n = n + sz
            if (sz < PLOT_BUFFER_LEN) exit
        end do

        call dm_pipe_close2(plot%stderr)
    end function dm_plot_error

    integer function dm_plot_lines(plot, dps) result(rc)
        !! Plots XY data points as line chart.
        type(plot_type), intent(inout) :: plot   !! Plot settings.
        type(dp_type),   intent(inout) :: dps(:) !! Data points array.

        rc = E_INVALID
        if (plot%term <= PLOT_TERM_NONE .or. plot%term > PLOT_TERM_LAST) return

        if (.not. plot%bidirect) then
            rc = dm_pipe_open(plot%stdin, PLOT_GNUPLOT, PIPE_WRONLY)
        else
            rc = dm_pipe_open2(plot%stdin, plot%stdout, plot%stderr, PLOT_GNUPLOT)
        end if

        if (dm_is_error(rc)) return

        plot_block: block
            rc = plot_set_term(plot);   if (dm_is_error(rc)) exit plot_block
            rc = plot_set_title(plot);  if (dm_is_error(rc)) exit plot_block
            rc = plot_set_xaxis(plot);  if (dm_is_error(rc)) exit plot_block
            rc = plot_set_yaxis(plot);  if (dm_is_error(rc)) exit plot_block
            rc = plot_set_graph(plot);  if (dm_is_error(rc)) exit plot_block
            rc = plot_set_legend(plot); if (dm_is_error(rc)) exit plot_block
            rc = plot_set_grid(plot);   if (dm_is_error(rc)) exit plot_block
            rc = plot_set_label(plot);  if (dm_is_error(rc)) exit plot_block

            rc = plot_output(plot, dps)
        end block plot_block

        if (.not. plot%bidirect) then
            call dm_pipe_close(plot%stdin)
        else
            call dm_pipe_close2(plot%stdin)
        end if
    end function dm_plot_lines

    integer(kind=i8) function dm_plot_read(plot, bytes) result(n)
        !! Returns number of bytes read from Gnuplot, and plot data in `bytes`.
        type(plot_type),               intent(inout) :: plot  !! Plot settings.
        character(len=:), allocatable, intent(out)   :: bytes !! Bytes returned by Gnuplot.

        character(len=PLOT_BUFFER_LEN) :: buffer
        integer(kind=i8)               :: sz

        n = 0_i8
        bytes = ''

        do
            sz = dm_pipe_read(plot%stdout, buffer)
            if (sz <= 0) exit
            bytes = bytes // buffer(1:sz)
            n = n + sz
            if (sz < PLOT_BUFFER_LEN) exit
        end do

        call dm_pipe_close2(plot%stdout)
    end function dm_plot_read

    pure elemental integer function dm_plot_term_from_name(name) result(term)
        !! Returns Gnuplot terminal backend of given name.
        character(len=*), intent(in) :: name

        character(len=PLOT_TERM_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_lower(name)

        select case (name_)
            case (PLOT_TERM_NAMES(PLOT_TERM_ANSI))
                ! ANSI
                term = PLOT_TERM_ANSI
            case (PLOT_TERM_NAMES(PLOT_TERM_ASCII))
                ! ASCII
                term = PLOT_TERM_ASCII
            case (PLOT_TERM_NAMES(PLOT_TERM_GIF))
                ! GIF
                term = PLOT_TERM_GIF
            case (PLOT_TERM_NAMES(PLOT_TERM_PNG))
                ! PNG
                term = PLOT_TERM_PNG
            case (PLOT_TERM_NAMES(PLOT_TERM_PNG_CAIRO))
                ! PNG Cairo
                term = PLOT_TERM_PNG_CAIRO
            case (PLOT_TERM_NAMES(PLOT_TERM_SIXEL))
                ! Sixel
                term = PLOT_TERM_SIXEL
            case (PLOT_TERM_NAMES(PLOT_TERM_SVG))
                ! SVG
                term = PLOT_TERM_SVG
            case (PLOT_TERM_NAMES(PLOT_TERM_X11))
                ! X11
                term = PLOT_TERM_X11
            case default
                ! none
                term = PLOT_TERM_NONE
        end select
    end function dm_plot_term_from_name

    pure elemental logical function dm_plot_term_valid(term) result(valid)
        !! Returns `.true.` if the given terminal is valid. `PLOT_TERM_NONE` is
        !! an invalid terminal.
        integer, intent(in) :: term !! Terminal type enumerator.

        valid = (term > PLOT_TERM_NONE .and. term <= PLOT_TERM_LAST)
    end function dm_plot_term_valid

    function dm_plot_version() result(version)
        !! Returns Gnuplot version as allocatable string.
        character(len=:), allocatable :: version

        character(len=32) :: buffer
        integer           :: rc
        integer(kind=i8)  :: sz
        type(pipe_type)   :: pipe

        rc = dm_pipe_open(pipe, PLOT_GNUPLOT // ' --version', PIPE_RDONLY)

        if (dm_is_ok(rc)) then
            sz = dm_pipe_read(pipe, buffer)
            if (sz > 10) version = 'gnuplot/' // buffer(9:11)
        end if

        call dm_pipe_close(pipe)
        if (.not. allocated(version)) version = 'gnuplot/0.0'
    end function dm_plot_version

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function plot_output(plot, dps) result(rc)
        !! Plots array of dp data in X, Y format by calling the Gnuplot
        !! `plot` command and appending the values line by line.
        type(plot_type), intent(inout) :: plot   !! Plot settings.
        type(dp_type),   intent(inout) :: dps(:) !! XY plot data array.

        character(len=80) :: line, style
        integer           :: i

        rc = E_INVALID
        if (size(dps) == 0) return

        select case (plot%style)
            case (PLOT_STYLE_LINES)
                style = 'lines'
            case (PLOT_STYLE_LINESPOINTS)
                style = 'linespoints'
            case (PLOT_STYLE_DOTS)
                style = 'dots'
            case (PLOT_STYLE_POINTS)
                style = 'points'
            case default
                style = 'lines'
        end select

        if (len_trim(plot%foreground) > 0) then
            style = trim(style) // ' lc rgb "' // trim(plot%foreground) // '"'
        end if

        rc = plot_write(plot, 'plot "-" using 1:2 with ' // trim(style))
        if (dm_is_error(rc)) return

        do i = 1, size(dps)
            write (line, '(a32, 1x, f20.5)') dps(i)%x, dps(i)%y
            rc = plot_write(plot, trim(line))
            if (dm_is_error(rc)) exit
        end do

        rc = plot_write(plot, 'e')
    end function plot_output

    integer function plot_set_graph(plot) result(rc)
        !! Sets graph colour.
        type(plot_type), intent(inout) :: plot !! Plot settings.

        integer :: n

        rc = E_NONE
        n = len_trim(plot%graph)

        if (n > 0) then
            rc = plot_write(plot, 'set object 1 rect from graph 0,0 ' // &
                                  'to graph 1,1 fillcolor rgb "' // &
                                  plot%graph(1:n) // '" behind')
        end if
    end function plot_set_graph

    integer function plot_set_grid(plot) result(rc)
        !! Enables grid.
        type(plot_type), intent(inout) :: plot !! Plot settings.

        rc = E_NONE
        if (plot%grid) rc = plot_write(plot, 'set grid')
    end function plot_set_grid

    integer function plot_set_label(plot) result(rc)
        ! Set X, Y axis labels.
        type(plot_type),  intent(inout) :: plot !! Plot settings.

        integer :: n

        rc = E_NONE
        n = len_trim(plot%xlabel)
        if (n > 0) rc = plot_write(plot, 'set xlabel "' // plot%xlabel(1:n) // '"')

        n = len_trim(plot%ylabel)
        if (n > 0) rc = plot_write(plot, 'set ylabel "' // plot%ylabel(1:n) // '"')
    end function plot_set_label

    integer function plot_set_legend(plot) result(rc)
        !! Disables legend (as the legend is enabled by default).
        type(plot_type), intent(inout) :: plot !! Plot settings.

        rc = E_NONE
        if (.not. plot%legend) rc = plot_write(plot, 'set nokey')
    end function plot_set_legend

    integer function plot_set_term(plot) result(rc)
        !! Configures the Gnuplot term.
        type(plot_type), intent(inout) :: plot !! Plot settings.

        character(len=2048) :: args
        integer             :: n

        rc = E_INVALID

        ! Set output size.
        write (args, '("size ", i0, ", ", i0)') plot%width, plot%height

        select case (plot%term)
            case (PLOT_TERM_ANSI)
                ! Dumb terminal with ANSI colours.
                rc = plot_write(plot, 'set term dumb ansi ' // trim(args))
                if (dm_is_error(rc)) return

            case (PLOT_TERM_ASCII)
                ! Dumb terminal, output to stdout or file.
                rc = plot_write(plot, 'set term dumb mono ' // trim(args))
                if (dm_is_error(rc)) return

                ! Set output file path (if present).
                n = len_trim(plot%output)
                if (n == 0) return
                rc = plot_write(plot, 'set output "' // plot%output(1:n) // '"')

            case (PLOT_TERM_GIF, PLOT_TERM_PNG, PLOT_TERM_PNG_CAIRO, PLOT_TERM_SIXEL, PLOT_TERM_SVG)
                ! Background colour.
                n = len_trim(plot%background)
                if (n > 0) args = 'background rgb "' // plot%background(1:n) // '" ' // trim(args)

                ! Set font.
                n = len_trim(plot%font)
                if (n > 0) args = 'font "' // plot%font(1:n) // '" ' // trim(args)

                ! Set terminal type with additional arguments.
                rc = plot_write(plot, 'set term ' // &
                                      trim(PLOT_TERM_NAMES(plot%term)) // ' ' // &
                                      trim(args))
                if (dm_is_error(rc)) return

                ! Set output file path (if present).
                n = len_trim(plot%output)
                if (n == 0) return
                rc = plot_write(plot, 'set output "' // plot%output(1:n) // '"')

            case (PLOT_TERM_X11)
                ! Background colour.
                n = len_trim(plot%background)
                if (n > 0) args = 'background rgb "' // plot%background(1:n) // '" ' // trim(args)

                ! Window title.
                n = len_trim(plot%title)
                if (n > 0) args = 'title "' // plot%title(1:n) // '" ' // trim(args)

                ! Persistent window.
                if (plot%persist) args = 'persist ' // trim(args)

                ! Set term type with additional arguments.
                rc = plot_write(plot, 'set term ' // &
                                      trim(PLOT_TERM_NAMES(plot%term)) // ' ' // &
                                      trim(args))

            case default
                return
        end select

        rc = E_NONE
    end function plot_set_term

    integer function plot_set_title(plot) result(rc)
        !! Sets plot title.
        type(plot_type), intent(inout) :: plot !! Plot settings.

        rc = E_NONE
        if (len_trim(plot%title) == 0) return
        rc = plot_write(plot, 'set title "' // trim(plot%title) // '"')
    end function plot_set_title

    integer function plot_set_xaxis(plot) result(rc)
        !! Configures X axis. The format is set to date and time in ISO 8601.
        type(plot_type), intent(inout) :: plot !! Plot settings.

        rc = plot_write(plot, 'set timefmt "' // PLOT_TIME_FORMAT // '"')
        if (dm_is_error(rc)) return

        rc = plot_write(plot, 'set xdata time')
        if (dm_is_error(rc)) return

        rc = plot_write(plot, 'set xtics rotate by 45 right')
        if (dm_is_error(rc)) return

        if (plot%xautoscale) then
            rc = plot_write(plot, 'set autoscale x')
        else
            rc = plot_write(plot, 'set xrange ["' // trim(plot%xrange(1)) // '":"' // &
                                                     trim(plot%xrange(2)) // '"]')
        end if
    end function plot_set_xaxis

    integer function plot_set_yaxis(plot) result(rc)
        !! Configures Y axis.
        type(plot_type), intent(inout) :: plot !! Plot settings.

        rc = E_NONE
        if (plot%yautoscale) rc = plot_write(plot, 'set autoscale y')
    end function plot_set_yaxis

    integer function plot_write(plot, str) result(rc)
        type(plot_type),  intent(inout) :: plot !! Plot settings.
        character(len=*), intent(in)    :: str  !! Bytes to send through pipe.

        integer(kind=i8) :: sz

        if (plot%bidirect) then
            rc = E_IO
            sz = dm_pipe_write2(plot%stdin, str // c_new_line)
            if (sz == len(str, kind=i8) + 1) rc = E_NONE
            return
        end if

        rc = dm_pipe_write(plot%stdin, str)
    end function plot_write
end module dm_plot
