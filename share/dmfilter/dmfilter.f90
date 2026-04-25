! dmfilter.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! Example program for low-pass filtering of input data. The input should be
    !! TAB-separated. The output is always TAB-separated.
    !!
    !! If DMPACK is installed to `/opt`, build the program by running:
    !!
    !! ```
    !! $ gfortran -I/opt/include/dmpack -o dmfilter dmfilter.f90 /opt/lib/libdmpack.a
    !! ```
    !!
    !! 4th-order low-pass filtering of 1000 real values with a sampling rate of
    !! 1000 Hz and a cut-off frequency of 50 Hz:
    !!
    !! ```
    !! $ ./dmfilter --order 4 --freq 1000.0 --cutoff 50.0 --count 1000 \
    !!   --input input.tsv --output output.tsv
    !! ```
    !!
    !! The values are read from file `input.tsv` and the filtered values are
    !! written to file `output.tsv`. The line format of the input is expected to
    !! be of the form `<ISO 8601><TAB><VALUE>`, for instance:
    !!
    !! ```
    !! 2025-12-08T14:30:33.488500+00:00	-.047437649222
    !! 2025-12-08T14:30:33.489000+00:00	-.047262658183
    !! 2025-12-08T14:30:33.489500+00:00	-.047080435022
    !! ```
    !!
    !! If the output file exists, it will be replaced.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmfilter'
    integer,      parameter :: APP_MAJOR = 1
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    type :: app_type
        !! App settings.
        character(FILE_PATH_LEN) :: input    = '-'     !! Path of input file or "-" for stdin.
        character(FILE_PATH_LEN) :: output   = '-'     !! Path of output file or "-" for stdout.
        integer                  :: count    = 0       !! Max. number of values to filter.
        integer                  :: order    = 1       !! Filter order.
        real(r8)                 :: cutoff   = 0.0_r8  !! Cut-off frequency [Hz].
        real(r8)                 :: freq     = 0.0_r8  !! Sampling frequency [Hz].
        logical                  :: verbose  = .false. !! Print debug messages to stderr.
    end type app_type

    integer        :: rc
    type(app_type) :: app

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    call filter(app)
contains
    subroutine filter(app)
        type(app_type), intent(inout) :: app

        integer  :: i, j, n, stat, unit
        logical  :: is_file
        real(r8) :: freq

        character(TIME_LEN), allocatable :: t(:)
        real(r8),            allocatable :: v(:)

        n = app%count

        ! Allocate memory.
        allocate (t(n), stat=stat); if (stat /= 0) return
        allocate (v(n), stat=stat); if (stat /= 0) return

        unit    = STDIN
        is_file = (app%input /= '-')

        ! Open input file.
        if (is_file) then
            open (action='read', file=trim(app%input), iostat=stat, newunit=unit, status='old')
            if (stat /= 0) return
        end if

        j = 0

        ! Read input.
        do i = 1, n
            read (unit, *, iostat=stat) t(i), v(i)
            if (stat /= 0) exit
            j = i
        end do

        if (is_file) close (unit)
        if (j == 0) return

        ! Filter.
        freq = 1.0_r8 / app%freq
        call dm_filter_low_pass(FILTER_BUTTERWORTH, v(1:j), app%order, app%cutoff, freq, .false.)

        unit    = STDOUT
        is_file = (app%output /= '-')

        ! Open output file.
        if (is_file) then
            open (action='write', file=trim(app%output), iostat=stat, newunit=unit, status='replace')
            if (stat /= 0) return
        end if

        ! Write output.
        do i = 1, j
            write (unit, '(a, a1, f0.12)', iostat=stat) t(i), achar(9), v(i)
            if (stat /= 0) exit
        end do

        if (is_file) close (unit)
    end subroutine filter

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(inout) :: app !! App type.

        type(arg_class) :: arg

        ! Required and optional command-line arguments.
        call arg%create()
        call arg%add('input',   short='i', type=ARG_TYPE_FILE)                     ! -i, --input <path>
        call arg%add('output',  short='o', type=ARG_TYPE_FILE)                     ! -o, --output <path>
        call arg%add('count',   short='n', type=ARG_TYPE_INTEGER, required=.true.) ! -n, --count <n>
        call arg%add('order',   short='O', type=ARG_TYPE_INTEGER)                  ! -O, --order <n>
        call arg%add('cutoff',  short='c', type=ARG_TYPE_REAL,    required=.true.) ! -c, --cutoff <freq>
        call arg%add('freq',    short='F', type=ARG_TYPE_REAL,    required=.true.) ! -F, --freq <freq>
        call arg%add('verbose', short='V', type=ARG_TYPE_LOGICAL)                  ! -V, --verbose

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('input',   app%input)
        call arg%get('output',  app%output)
        call arg%get('count',   app%count)
        call arg%get('order',   app%order)
        call arg%get('cutoff',  app%cutoff)
        call arg%get('freq',    app%freq)
        call arg%get('verbose', app%verbose)

        rc = E_INVALID

        if (app%input /= '-' .and. .not. dm_file_exists(app%input)) then
            call dm_error_out(rc, 'input file not found')
            return
        end if

        if (app%count <= 0) then
            call dm_error_out(rc, 'count must be > 0')
            return
        end if

        if (app%order < 1 .or. app%order > FILTER_MAX_ORDER) then
            call dm_error_out(rc, 'invalid filter order')
            return
        end if

        if (app%freq <= 0.0_r8) then
            call dm_error_out(rc, 'invalid sampling interval')
            return
        end if

        rc = E_NONE
    end function read_args

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
    end subroutine version_callback
end program main
