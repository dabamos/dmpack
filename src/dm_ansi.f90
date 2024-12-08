! Author:  Philipp Engel
! Licence: ISC
module dm_ansi
    !! ANSI escape sequences for coloured terminal output.
    !!
    !! Add ANSI parameters directly to print statements, for instance:
    !!
    !! ```fortran
    !! print '(2a)', ANSI_CLEAR_SCREEN, 'Hello, World!'
    !! ```
    !!
    !! The foreground colour can be changed through routine
    !! `dm_ansi_color()`.
    !!
    !! You may want to disable ANSI colour output depending on whether
    !! environment variable `NO_COLOR` is set, for example:
    !!
    !! ```fortran
    !! logical :: no_color
    !! no_color = dm_env_has('NO_COLOR')
    !! call dm_ansi_color(COLOR_RED, no_color)
    !! ```
    use :: dm_ascii
    implicit none (type, external)
    private

    character(len=*), parameter, public :: ANSI_CLEAR_SCREEN = ASCII_ESC // '[2J'
    character(len=*), parameter, public :: ANSI_RESET_CURSOR = ASCII_ESC // '[0;0H'
    character(len=*), parameter, public :: ANSI_HIDE_CURSOR  = ASCII_ESC // '[?25l'
    character(len=*), parameter, public :: ANSI_SHOW_CURSOR  = ASCII_ESC // '[?25h'

    integer, parameter, public :: COLOR_BLACK   = 0
    integer, parameter, public :: COLOR_RED     = 1
    integer, parameter, public :: COLOR_GREEN   = 2
    integer, parameter, public :: COLOR_YELLOW  = 3
    integer, parameter, public :: COLOR_BLUE    = 4
    integer, parameter, public :: COLOR_MAGENTA = 5
    integer, parameter, public :: COLOR_CYAN    = 6
    integer, parameter, public :: COLOR_WHITE   = 7
    integer, parameter, public :: COLOR_RESET   = 9

    public :: dm_ansi_color
    public :: dm_ansi_reset
contains
    subroutine dm_ansi_color(color, no_color)
        !! Changes foreground to given colour.
        integer, intent(in)           :: color    !! Colour.
        logical, intent(in), optional :: no_color !! No output if `.true.`.

        if (present(no_color)) then
            if (no_color) return
        end if

        write (*, '(a, "[", i2, "m")', advance='no') ASCII_ESC, 30 + color
    end subroutine dm_ansi_color

    subroutine dm_ansi_reset(no_color)
        !! Resets colours.
        logical, intent(in), optional :: no_color

        if (present(no_color)) then
            if (no_color) return
        end if

        write (*, '(a, "[0m")', advance='no') ASCII_ESC
    end subroutine dm_ansi_reset
end module dm_ansi
