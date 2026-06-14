! Author:  Philipp Engel
! Licence: ISC
module dm_posix_regex
    !! Abstraction layer over POSIX regular expressions.
    use :: dm_c
    use :: dm_error
    use :: unix, only: c_regex_t
    implicit none (type, external)
    private

    type, public :: posix_regex_type
        private
        type(c_regex_t) :: context = c_regex_t()
    end type posix_regex_type

    public :: dm_posix_regex_create
    public :: dm_posix_regex_destroy
    public :: dm_posix_regex_match
contains
    integer function dm_posix_regex_create(regex, pattern, extended, error_message) result(rc)
        use :: dm_util, only: dm_present
        use :: unix,    only: REG_EXTENDED, c_regcomp, c_regerror

        type(posix_regex_type), intent(out)             :: regex         !! Regex context.
        character(*),           intent(in)              :: pattern       !! Regular expression pattern.
        logical,                intent(in),    optional :: extended      !! Extended syntax (ERE).
        character(*), target,   intent(inout), optional :: error_message !! Error message.

        integer           :: code, flags
        integer(c_size_t) :: nbytes

        flags = 0
        if (dm_present(extended, .false.)) flags = REG_EXTENDED

        ! Compile regular expression.
        code = c_regcomp(regex%context, dm_f_c_string(pattern), flags)

        rc = E_SYSTEM
        if (code == 0) rc = E_NONE

        if (present(error_message)) then
            error_message = ' '

            if (code /= 0) then
                nbytes = c_regerror(code, regex%context, c_loc(error_message), len(error_message, c_size_t))
                if (nbytes <= 0) error_message = 'regcomp()'
            end if
        end if
    end function dm_posix_regex_create

    subroutine dm_posix_regex_destroy(regex)
        use :: unix, only: c_regfree

        type(posix_regex_type), intent(inout) :: regex !! Regex context.

        call c_regfree(regex%context)
    end subroutine dm_posix_regex_destroy

    logical function dm_posix_regex_match(regex, subject) result(match)
        use :: unix, only: c_regexec

        type(posix_regex_type), intent(inout) :: regex   !! Regex context.
        character(*),           intent(in)    :: subject !! Subject.

        match = (c_regexec(regex%context, dm_f_c_string(subject), 0_c_size_t, c_null_ptr, 0) == 0)
    end function dm_posix_regex_match
end module dm_posix_regex
