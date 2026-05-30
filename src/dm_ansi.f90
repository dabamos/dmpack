! Author:  Philipp Engel
! Licence: ISC
module dm_ansi
    !! ANSI escape sequences for coloured terminal output.
    !!
    !! Add ANSI parameters directly to print statements, for instance:
    !!
    !! ``` fortran
    !! print '(2a)', ANSI_CLEAR_SCREEN, 'Hello, World!'
    !! ```
    !!
    !! The foreground colour can be changed through routine
    !! `dm_ansi_color()`.
    !!
    !! You may want to disable ANSI colour output depending on whether
    !! environment variable `NO_COLOR` is set, for example:
    !!
    !! ``` fortran
    !! logical :: no_color
    !! no_color = dm_env_has('NO_COLOR')
    !! call dm_ansi_color(COLOR_RED, no_color)
    !! ```
    use :: dm_ascii
    use :: dm_kind
    implicit none (type, external)
    private

    ! ANSI escape codes.
    character(*), parameter, public :: ANSI_CLEAR_SCREEN = ASCII_ESC // '[2J'   !! Clear screen.
    character(*), parameter, public :: ANSI_RESET_CURSOR = ASCII_ESC // '[0;0H' !! Reset cursor.
    character(*), parameter, public :: ANSI_HIDE_CURSOR  = ASCII_ESC // '[?25l' !! Hide cursor.
    character(*), parameter, public :: ANSI_SHOW_CURSOR  = ASCII_ESC // '[?25h' !! Show cursor.

    ! ANSI colour codes (foreground and background).
    integer, parameter, public :: COLOR_BLACK   = 0 !! Black.
    integer, parameter, public :: COLOR_RED     = 1 !! Red.
    integer, parameter, public :: COLOR_GREEN   = 2 !! Green.
    integer, parameter, public :: COLOR_YELLOW  = 3 !! Yellow.
    integer, parameter, public :: COLOR_BLUE    = 4 !! Blue.
    integer, parameter, public :: COLOR_MAGENTA = 5 !! Magenta.
    integer, parameter, public :: COLOR_CYAN    = 6 !! Cyan.
    integer, parameter, public :: COLOR_WHITE   = 7 !! White.
    integer, parameter, public :: COLOR_RESET   = 9 !! Reset colour.

    public :: dm_ansi_color
    public :: dm_ansi_reset
contains
    subroutine dm_ansi_color(color, no_color, unit)
        !! Changes foreground to `color`. Writes to `stdout` if no unit is passed.
        integer, intent(in)           :: color    !! Colour.
        logical, intent(in), optional :: no_color !! No output if `.true.`.
        integer, intent(in), optional :: unit     !! File unit.

        integer :: unit_

        if (present(no_color)) then
            if (no_color) return
        end if

        if (present(unit)) then
            unit_ = unit
        else
            unit_ = stdout
        end if

        write (*, '(a, "[", i2, "m")', advance='no') ASCII_ESC, 30 + color
    end subroutine dm_ansi_color

    subroutine dm_ansi_reset(no_color, unit)
        !! Resets colours. Writes to `stdout` if unit is passed.
        logical, intent(in), optional :: no_color !! No output if `.true.`.
        integer, intent(in), optional :: unit     !! File unit.

        integer :: unit_
        logical :: no_color_

        no_color_ = .false.
        unit_     = stdout

        if (present(no_color)) no_color_ = no_color
        if (present(unit))     unit_     = unit

        if (no_color_) return

        write (unit_, '(a, "[0m")', advance='no') ASCII_ESC
    end subroutine dm_ansi_reset
end module dm_ansi
