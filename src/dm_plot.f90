! Author:  Philipp Engel
! Licence: ISC
module dm_plot
    !! Abstraction layer over Gnuplot.
    !!
    !! In order to use terminal `PLOT_TERMINAL_GPIC` on Linux, Gnuplot must have
    !! been compiled with option _gpic_ enabled. You can list all installed
    !! Gnuplot terminals by running:
    !!
    !! ```
    !! $ gnuplot -e "set terminal"
    !! ```
    !!
    !! If you build Gnuplot from source, explicitly set option `--with-gpic`
    !! for PIC preprocessor format, for instance:
    !!
    !! ```
    !! $ ./configure --prefix=/opt --with-gpic
    !! ```
    !!
    !! ## Example
    !!
    !! Plot data points in SVG format to string `svg`:
    !!
    !! ```fortran
    !! character(len=:), allocatable :: svg
    !! character(len=TIME_LEN)       :: ts
    !!
    !! integer         :: i, rc
    !! type(plot_type) :: plot
    !! type(dp_type)   :: dps(60)
    !!
    !! do i = 1, size(dps)
    !!     write (ts, '("2025-01-01T00:", i0.2, ":00.000000+00:00")') i
    !!     dps(i) = dp_type(ts, sin(i * 0.1_r8))
    !! end do
    !!
    !! call dm_plot_set(plot, terminal=PLOT_TERMINAL_SVG, title='Sample Plot', &
    !!                  bidirect=.true., background='white')
    !! rc = dm_plot_lines(plot, dps)
    !! rc = dm_plot_read(plot, svg)
    !! call dm_plot_close(plot)
    !! ```
    use, intrinsic :: iso_c_binding
    use :: dm_dp
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_pipe
    use :: dm_string
    use :: dm_time
    implicit none (type, external)
    private

    ! Line styles.
    integer, parameter, public :: PLOT_STYLE_NONE        = 0 !! Invalid style.
    integer, parameter, public :: PLOT_STYLE_LINES       = 1 !! Lines.
    integer, parameter, public :: PLOT_STYLE_LINESPOINTS = 2 !! Lines with symbols.
    integer, parameter, public :: PLOT_STYLE_DOTS        = 3 !! Dots.
    integer, parameter, public :: PLOT_STYLE_POINTS      = 4 !! Points.
    integer, parameter, public :: PLOT_STYLE_LAST        = 4 !! Never use this.

    ! Gnuplot terminals, see:
    ! http://gnuplot.info/docs_6.0/Terminals.html
    integer, parameter, public :: PLOT_TERMINAL_NONE       =  0 !! Invalid terminal.
    integer, parameter, public :: PLOT_TERMINAL_ANSI       =  1 !! ASCII with ANSI colours (dumb).
    integer, parameter, public :: PLOT_TERMINAL_ASCII      =  2 !! ASCII (dumb).
    integer, parameter, public :: PLOT_TERMINAL_GIF        =  3 !! GIF (libgd).
    integer, parameter, public :: PLOT_TERMINAL_GPIC       =  4 !! PIC preprocessor for GNU roff.
    integer, parameter, public :: PLOT_TERMINAL_PNG        =  5 !! PNG (libgd).
    integer, parameter, public :: PLOT_TERMINAL_PNGCAIRO   =  6 !! PNG (libcairo).
    integer, parameter, public :: PLOT_TERMINAL_POSTSCRIPT =  7 !! PNG (libcairo).
    integer, parameter, public :: PLOT_TERMINAL_SIXELGD    =  8 !! Sixel (libgd).
    integer, parameter, public :: PLOT_TERMINAL_SIXELTEK   =  9 !! Sixel (bitmap graphics).
    integer, parameter, public :: PLOT_TERMINAL_SVG        = 10 !! SVG.
    integer, parameter, public :: PLOT_TERMINAL_X11        = 11 !! X11.
    integer, parameter, public :: PLOT_TERMINAL_LAST       = 11 !! Never use this.

    integer, parameter, public :: PLOT_TERMINAL_NAME_LEN = 10 !! Max. terminal name length.

    character(len=*), parameter, public :: PLOT_TIME_FORMAT = '%Y-%m-%dT%H:%M:%S' !! Datetime format.
    character(len=*), parameter, public :: PLOT_TERMINAL_NAMES(PLOT_TERMINAL_NONE:PLOT_TERMINAL_LAST) = [ &
        character(len=PLOT_TERMINAL_NAME_LEN) :: 'none', 'ansi', 'ascii', 'gif', 'gpic', 'png', &
        'pngcairo', 'postscript', 'sixelgd', 'sixeltek', 'svg', 'x11' &
    ] !! Gnuplot terminal names.

    character(len=*), parameter :: GNUPLOT_BINARY  = 'gnuplot' !! Gnuplot binary name.
    integer(kind=i8), parameter :: PLOT_BUFFER_LEN = 16384     !! Input buffer length.

    type, public :: plot_type
        !! Plot context type.
        integer                      :: terminal   = PLOT_TERMINAL_NONE !! Output terminal.
        integer                      :: style      = PLOT_STYLE_LINES   !! Plot line style.
        integer                      :: width      = 800                !! Plot width [px, cm].
        integer                      :: height     = 300                !! Plot height [px, cm].
        character(len=FILE_PATH_LEN) :: output     = ' '                !! Output file name.
        character(len=8)             :: background = ' '                !! Background colour (optional).
        character(len=8)             :: foreground = '#3b4cc0'          !! Foreground colour (optional).
        character(len=8)             :: graph      = '#ffffff'          !! Graph background colour.
        character(len=FILE_PATH_LEN) :: font       = ' '                !! Font name or file path (optional).
        character(len=128)           :: title      = ' '                !! Plot title (optional).
        character(len=128)           :: xlabel     = ' '                !! X label (optional).
        character(len=128)           :: ylabel     = ' '                !! Y label (optional).
        character(len=TIME_LEN)      :: xrange(2)  = ' '                !! X axis range.
        real(kind=r8)                :: yrange(2)  = 0.0_r8             !! Y axis range.
        logical                      :: bidirect   = .false.            !! Bi-directional anonymous pipe.
        logical                      :: monochrome = .false.            !! Black and white drawing (PostScript only).
        logical                      :: persist    = .false.            !! Persistent Gnuplot process (use only with X11).
        logical                      :: xautoscale = .true.             !! Auto-scale X axis.
        logical                      :: yautoscale = .true.             !! Auto-scale Y axis.
        logical                      :: grid       = .true.             !! Show grid.
        logical                      :: legend     = .false.            !! Show legend.
        type(pipe_type), private     :: stdin                           !! Gnuplot’s standard input.
        type(pipe_type), private     :: stdout                          !! Gnuplot’s standard output.
        type(pipe_type), private     :: stderr                          !! Gnuplot’s standard error.
    end type plot_type

    public :: dm_plot_close
    public :: dm_plot_error
    public :: dm_plot_lines
    public :: dm_plot_read
    public :: dm_plot_set
    public :: dm_plot_terminal_from_name
    public :: dm_plot_terminal_is_valid
    public :: dm_plot_version

    private :: plot_output
    private :: plot_set_graph
    private :: plot_set_grid
    private :: plot_set_label
    private :: plot_set_legend
    private :: plot_set_terminal
    private :: plot_set_title
    private :: plot_set_xaxis
    private :: plot_set_yaxis
    private :: plot_write
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_plot_error(plot, output, n) result(rc)
        !! Returns Gnuplot's standard error output in allocatable character
        !! string `output`. The result is an empty string of length 1 if no
        !! output to standard error has been made.
        type(plot_type),               intent(inout)         :: plot   !! Plot type.
        character(len=:), allocatable, intent(out)           :: output !! Bytes returned by Gnuplot.
        integer(kind=i8),              intent(out), optional :: n      !! Bytes read.

        character(len=PLOT_BUFFER_LEN) :: buffer
        integer                        :: i
        integer(kind=i8)               :: n1, n2

        if (present(n)) n = 0_i8

        output = ''
        n2     = 0_i8

        do
            rc = dm_pipe_read(plot%stderr, buffer, n1)
            if (dm_is_error(rc)) exit
            if (n1 == 0) exit
            output = output // buffer(1:n1)
            n2 = n2 + n1
            if (n1 < PLOT_BUFFER_LEN) exit
        end do

        call dm_pipe_close2(plot%stderr)
        if (present(n)) n = n2

        ! Remove null-termination.
        i = index(output, c_null_char)
        if (i > 0) output(i:i) = ' '

        if (dm_is_error(rc)) return
        if (len_trim(output) == 0) rc = E_EMPTY
    end function dm_plot_error

    integer function dm_plot_lines(plot, dps) result(rc)
        !! Plots XY data points as line chart.
        type(plot_type), intent(inout) :: plot   !! Plot type.
        type(dp_type),   intent(inout) :: dps(:) !! Data points array.

        rc = E_INVALID
        if (.not. dm_plot_terminal_is_valid(plot%terminal)) return

        if (.not. plot%bidirect) then
            rc = dm_pipe_open(plot%stdin, GNUPLOT_BINARY, PIPE_WRONLY)
        else
            rc = dm_pipe_open2(plot%stdin, plot%stdout, plot%stderr, GNUPLOT_BINARY)
        end if

        if (dm_is_error(rc)) return

        plot_block: block
            rc = plot_set_terminal(plot); if (dm_is_error(rc)) exit plot_block
            rc = plot_set_title(plot);    if (dm_is_error(rc)) exit plot_block
            rc = plot_set_xaxis(plot);    if (dm_is_error(rc)) exit plot_block
            rc = plot_set_yaxis(plot);    if (dm_is_error(rc)) exit plot_block
            rc = plot_set_graph(plot);    if (dm_is_error(rc)) exit plot_block
            rc = plot_set_legend(plot);   if (dm_is_error(rc)) exit plot_block
            rc = plot_set_grid(plot);     if (dm_is_error(rc)) exit plot_block
            rc = plot_set_label(plot);    if (dm_is_error(rc)) exit plot_block

            rc = plot_output(plot, dps)
        end block plot_block

        if (.not. plot%bidirect) then
            call dm_pipe_close(plot%stdin)
        else
            call dm_pipe_close2(plot%stdin)
        end if
    end function dm_plot_lines

    integer function dm_plot_read(plot, output, n) result(rc)
        !! Returns number of bytes read from Gnuplot, and plot data in `n`. The
        !! output is an empty string of length 1 if no bytes have been returned.
        type(plot_type),               intent(inout)         :: plot   !! Plot type.
        character(len=:), allocatable, intent(out)           :: output !! Bytes returned by Gnuplot.
        integer(kind=i8),              intent(out), optional :: n      !! Bytes read.

        character(len=PLOT_BUFFER_LEN) :: buffer
        integer                        :: i
        integer(kind=i8)               :: n1, n2

        if (present(n)) n = 0_i8

        output = ''
        n2     = 0_i8

        do
            rc = dm_pipe_read(plot%stdout, buffer, n1)
            if (n1 == 0) exit
            output = output // buffer(1:n1)
            n2 = n2 + n1
            if (n1 < PLOT_BUFFER_LEN) exit
        end do

        call dm_pipe_close2(plot%stdout)
        if (present(n)) n = n2

        ! Remove null-termination.
        i = index(output, c_null_char)
        if (i > 0) output(i:i) = ' '

        if (dm_is_error(rc)) return
        if (len_trim(output) == 0) rc = E_EMPTY
    end function dm_plot_read

    pure elemental integer function dm_plot_terminal_from_name(name) result(terminal)
        !! Returns Gnuplot terminal backend of given name.
        character(len=*), intent(in) :: name !! Terminal name.

        character(len=PLOT_TERMINAL_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_ANSI));       terminal = PLOT_TERMINAL_ANSI       ! ANSI
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_ASCII));      terminal = PLOT_TERMINAL_ASCII      ! ASCII
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_GIF));        terminal = PLOT_TERMINAL_GIF        ! GIF
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_GPIC));       terminal = PLOT_TERMINAL_GPIC       ! PIC (groff)
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_PNG));        terminal = PLOT_TERMINAL_PNG        ! PNG
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_PNGCAIRO));   terminal = PLOT_TERMINAL_PNGCAIRO   ! PNG (libcairo)
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_POSTSCRIPT)); terminal = PLOT_TERMINAL_POSTSCRIPT ! PostScript (EPS)
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_SIXELGD));    terminal = PLOT_TERMINAL_SIXELGD    ! Sixel (libgd)
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_SIXELTEK));   terminal = PLOT_TERMINAL_SIXELTEK   ! Sixel (bitmap)
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_SVG));        terminal = PLOT_TERMINAL_SVG        ! SVG
            case (PLOT_TERMINAL_NAMES(PLOT_TERMINAL_X11));        terminal = PLOT_TERMINAL_X11        ! X11
            case default;                                         terminal = PLOT_TERMINAL_NONE       ! none
        end select
    end function dm_plot_terminal_from_name

    pure elemental logical function dm_plot_terminal_is_valid(terminal) result(valid)
        !! Returns `.true.` if the given terminal is valid. `PLOT_TERMINAL_NONE`
        !! is an invalid terminal.
        integer, intent(in) :: terminal !! Terminal type enumerator.

        valid = (terminal > PLOT_TERMINAL_NONE .and. terminal <= PLOT_TERMINAL_LAST)
    end function dm_plot_terminal_is_valid

    function dm_plot_version(name, found) result(version)
        !! Returns Gnuplot version as allocatable string. This function is quite
        !! slow as Gnuplot has to be started in a new process.
        use :: dm_util, only: dm_present

        character(len=*), parameter :: NAME_STR = 'gnuplot'

        logical, intent(in),  optional :: name    !! Add prefix `gnuplot/`.
        logical, intent(out), optional :: found   !! Returns `.true.` if Gnuplot has been found.
        character(len=:), allocatable  :: version !! Version string.

        character(len=3)  :: v
        character(len=32) :: buffer
        integer           :: rc
        integer(kind=i8)  :: n
        type(pipe_type)   :: pipe

        if (present(found)) found = .false.

        rc = dm_pipe_open(pipe, GNUPLOT_BINARY // ' --version', PIPE_RDONLY)
        v  = '0.0'

        if (dm_is_ok(rc)) then
            rc = dm_pipe_read(pipe, buffer, n)
            if (n > 11) v = buffer(9:11)
            if (present(found) .and. buffer(1:7) == NAME_STR) found = .true.
        end if

        call dm_pipe_close(pipe)

        if (dm_present(name, .false.)) then
            version = NAME_STR // '/' // v
        else
            version = v
        end if
    end function dm_plot_version

    subroutine dm_plot_close(plot)
        !! Closes pipe connected to standard error if in bidrectional mode.
        type(plot_type), intent(inout) :: plot !! Plot type.

        if (plot%bidirect) call dm_pipe_close(plot%stderr)
    end subroutine dm_plot_close

    subroutine dm_plot_set(plot, terminal, style, width, height, output, background, foreground, &
                           graph, font, title, xlabel, ylabel, xrange, yrange, bidirect, persist, &
                           xautoscale, yautoscale, grid, legend, monochrome)
        !! Sets plot attributes.
        type(plot_type),  intent(inout)        :: plot       !! Plot type.
        integer,          intent(in), optional :: terminal   !! Output terminal.
        integer,          intent(in), optional :: style      !! Plot line style.
        integer,          intent(in), optional :: width      !! Plot width.
        integer,          intent(in), optional :: height     !! Plot height.
        character(len=*), intent(in), optional :: output     !! Output file name.
        character(len=*), intent(in), optional :: background !! Background colour.
        character(len=*), intent(in), optional :: foreground !! Foreground colour.
        character(len=*), intent(in), optional :: graph      !! Graph background colour.
        character(len=*), intent(in), optional :: font       !! Font name or file path.
        character(len=*), intent(in), optional :: title      !! Plot title.
        character(len=*), intent(in), optional :: xlabel     !! X axis label.
        character(len=*), intent(in), optional :: ylabel     !! Y axis label.
        character(len=*), intent(in), optional :: xrange(2)  !! X axis range.
        real(kind=r8),    intent(in), optional :: yrange(2)  !! Y axis range.
        logical,          intent(in), optional :: bidirect   !! Bi-directional anonymous pipe.
        logical,          intent(in), optional :: persist    !! Persistent Gnuplot process (use only with X11).
        logical,          intent(in), optional :: xautoscale !! Auto-scale X axis.
        logical,          intent(in), optional :: yautoscale !! Auto-scale Y axis.
        logical,          intent(in), optional :: grid       !! Show grid.
        logical,          intent(in), optional :: legend     !! Show legend.
        logical,          intent(in), optional :: monochrome !! No colour.

        if (present(terminal))   plot%terminal   = terminal
        if (present(style))      plot%style      = style
        if (present(width))      plot%width      = width
        if (present(height))     plot%height     = height
        if (present(output))     plot%output     = output
        if (present(background)) plot%background = background
        if (present(foreground)) plot%foreground = foreground
        if (present(graph))      plot%graph      = graph
        if (present(font))       plot%font       = font
        if (present(title))      plot%title      = title
        if (present(xlabel))     plot%xlabel     = xlabel
        if (present(ylabel))     plot%ylabel     = ylabel
        if (present(xrange))     plot%xrange     = xrange
        if (present(yrange))     plot%yrange     = yrange
        if (present(bidirect))   plot%bidirect   = bidirect
        if (present(persist))    plot%persist    = persist
        if (present(xautoscale)) plot%xautoscale = xautoscale
        if (present(yautoscale)) plot%yautoscale = yautoscale
        if (present(grid))       plot%grid       = grid
        if (present(legend))     plot%legend     = legend
        if (present(monochrome)) plot%monochrome = monochrome
    end subroutine dm_plot_set

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function plot_output(plot, dps) result(rc)
        !! Plots array of dp data in X, Y format by calling the Gnuplot
        !! `plot` command and appending the values line by line.
        type(plot_type), intent(inout) :: plot   !! Plot type.
        type(dp_type),   intent(inout) :: dps(:) !! XY plot data array.

        character(len=80) :: line, style
        integer           :: i

        rc = E_INVALID
        if (size(dps) == 0) return

        select case (plot%style)
            case (PLOT_STYLE_LINES);       style = 'lines'
            case (PLOT_STYLE_LINESPOINTS); style = 'linespoints'
            case (PLOT_STYLE_DOTS);        style = 'dots'
            case (PLOT_STYLE_POINTS);      style = 'points'
            case default;                  style = 'lines'
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
        type(plot_type), intent(inout) :: plot !! Plot type.

        integer :: n

        rc = E_NONE
        n = len_trim(plot%graph)
        if (n > 0) rc = plot_write(plot, 'set object 1 rect from graph 0,0 to graph 1,1 fillcolor rgb "' // plot%graph(1:n) // '" behind')
    end function plot_set_graph

    integer function plot_set_grid(plot) result(rc)
        !! Enables grid.
        type(plot_type), intent(inout) :: plot !! Plot type.

        rc = E_NONE
        if (plot%grid) rc = plot_write(plot, 'set grid')
    end function plot_set_grid

    integer function plot_set_label(plot) result(rc)
        ! Set X, Y axis labels with attribute `noenhanced`.
        type(plot_type),  intent(inout) :: plot !! Plot type.

        integer :: n

        rc = E_NONE
        n = len_trim(plot%xlabel)
        if (n > 0) rc = plot_write(plot, 'set xlabel "' // plot%xlabel(1:n) // '" noenhanced')

        n = len_trim(plot%ylabel)
        if (n > 0) rc = plot_write(plot, 'set ylabel "' // plot%ylabel(1:n) // '" noenhanced')
    end function plot_set_label

    integer function plot_set_legend(plot) result(rc)
        !! Disables legend (as the legend is enabled by default).
        type(plot_type), intent(inout) :: plot !! Plot type.

        rc = E_NONE
        if (.not. plot%legend) rc = plot_write(plot, 'set nokey')
    end function plot_set_legend

    integer function plot_set_terminal(plot) result(rc)
        !! Configures the Gnuplot term.
        type(plot_type), intent(inout) :: plot !! Plot type.

        character(len=2048) :: args
        integer             :: n

        rc = E_INVALID

        select case (plot%terminal)
            case (PLOT_TERMINAL_ANSI)
                ! Dumb terminal with ANSI colours.
                write (args, '("size ", i0, ", ", i0)') plot%width, plot%height
                rc = plot_write(plot, 'set terminal dumb ansi ' // trim(args))

            case (PLOT_TERMINAL_ASCII)
                ! Dumb terminal, output to stdout or file.
                write (args, '("size ", i0, ", ", i0)') plot%width, plot%height
                rc = plot_write(plot, 'set terminal dumb mono ' // trim(args))
                if (dm_is_error(rc)) return

                ! Set output file path (if present).
                n = len_trim(plot%output)
                if (n == 0) return
                rc = plot_write(plot, 'set output "' // plot%output(1:n) // '"')

            case (PLOT_TERMINAL_GIF,      &
                  PLOT_TERMINAL_PNG,      &
                  PLOT_TERMINAL_PNGCAIRO, &
                  PLOT_TERMINAL_SIXELGD,  &
                  PLOT_TERMINAL_SIXELTEK, &
                  PLOT_TERMINAL_SVG)
                write (args, '("size ", i0, ", ", i0)') plot%width, plot%height

                ! Set background colour.
                n = len_trim(plot%background)
                if (n > 0) args = 'background rgb "' // plot%background(1:n) // '" ' // trim(args)

                ! Set font.
                n = len_trim(plot%font)
                if (n > 0) args = 'font "' // plot%font(1:n) // '" ' // trim(args)

                ! Set terminal type with additional arguments.
                rc = plot_write(plot, 'set terminal ' // trim(PLOT_TERMINAL_NAMES(plot%terminal)) // ' ' // trim(args))
                if (dm_is_error(rc)) return

                ! Set output file path (if present).
                n = len_trim(plot%output)
                if (n == 0) return
                rc = plot_write(plot, 'set output "' // plot%output(1:n) // '"')

            case (PLOT_TERMINAL_GPIC)
                ! PIC preprocessor for GNU roff.
                rc = plot_write(plot, 'set terminal gpic') ! No size.
                if (dm_is_error(rc)) return

                ! Set output file path (if present).
                n = len_trim(plot%output)
                if (n == 0) return
                rc = plot_write(plot, 'set output "' // plot%output(1:n) // '"')

            case (PLOT_TERMINAL_POSTSCRIPT)
                ! PostScript (EPS).
                write (args, '("size ", i0, "cm, ", i0, "cm")') plot%width, plot%height ! Size in cm.

                ! Set colour mode.
                if (plot%monochrome) then
                    args = 'monochrome ' // trim(args)
                else
                    args = 'color ' // trim(args)
                end if

                ! Set font.
                n = len_trim(plot%font)
                if (n > 0) args = 'font "' // plot%font(1:n) // '" ' // trim(args)

                ! Set terminal type with additional arguments.
                rc = plot_write(plot, 'set terminal ' // trim(PLOT_TERMINAL_NAMES(plot%terminal)) // ' eps ' // trim(args))
                if (dm_is_error(rc)) return

                ! Set output file path (if present).
                n = len_trim(plot%output)
                if (n == 0) return
                rc = plot_write(plot, 'set output "' // plot%output(1:n) // '"')

            case (PLOT_TERMINAL_X11)
                ! X11 window.
                write (args, '("size ", i0, ", ", i0)') plot%width, plot%height

                ! Set background colour.
                n = len_trim(plot%background)
                if (n > 0) args = 'background rgb "' // plot%background(1:n) // '" ' // trim(args)

                ! Set window title.
                n = len_trim(plot%title)
                if (n > 0) args = 'title "' // plot%title(1:n) // '" ' // trim(args)

                ! Set persistent window.
                if (plot%persist) args = 'persist ' // trim(args)

                ! Set terminal type with additional arguments.
                rc = plot_write(plot, 'set terminal ' // trim(PLOT_TERMINAL_NAMES(plot%terminal)) // ' ' // trim(args))

            case default
                return
        end select
    end function plot_set_terminal

    integer function plot_set_title(plot) result(rc)
        !! Sets plot title.
        type(plot_type), intent(inout) :: plot !! Plot type.

        rc = E_NONE
        if (len_trim(plot%title) == 0) return
        rc = plot_write(plot, 'set title "' // trim(plot%title) // '"')
    end function plot_set_title

    integer function plot_set_xaxis(plot) result(rc)
        !! Configures X axis. The format is set to date and time in ISO 8601.
        type(plot_type), intent(inout) :: plot !! Plot type.

        rc = plot_write(plot, 'set timefmt "' // PLOT_TIME_FORMAT // '"')
        if (dm_is_error(rc)) return

        rc = plot_write(plot, 'set xdata time')
        if (dm_is_error(rc)) return

        rc = plot_write(plot, 'set xtics rotate by 45 right')
        if (dm_is_error(rc)) return

        if (plot%xautoscale) then
            rc = plot_write(plot, 'set autoscale x')
        else
            rc = plot_write(plot, 'set xrange ["' // trim(plot%xrange(1)) // '":"' // trim(plot%xrange(2)) // '"]')
        end if
    end function plot_set_xaxis

    integer function plot_set_yaxis(plot) result(rc)
        !! Configures Y axis.
        type(plot_type), intent(inout) :: plot !! Plot type.

        rc = E_NONE
        if (plot%yautoscale) rc = plot_write(plot, 'set autoscale y')
    end function plot_set_yaxis

    integer function plot_write(plot, input) result(rc)
        type(plot_type),  intent(inout) :: plot  !! Plot type.
        character(len=*), intent(in)    :: input !! Bytes to write to pipe.

        integer(kind=i8) :: n

        if (plot%bidirect) then
            rc = dm_pipe_write2(plot%stdin, input // c_new_line, n)
            if (n == len(input, kind=i8) + 1) rc = E_NONE
            return
        end if

        rc = dm_pipe_write(plot%stdin, trim(input))
    end function plot_write
end module dm_plot
