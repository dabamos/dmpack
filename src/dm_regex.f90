! Author:  Philipp Engel
! Licence: ISC
module dm_regex
    !! Abstraction layer over PCRE2.
    use, intrinsic :: iso_c_binding, only: c_associated, c_null_ptr, c_ptr
    use :: pcre2
    use :: dm_convert
    use :: dm_error
    use :: dm_kind
    use :: dm_request
    implicit none (type, external)
    private

    integer, parameter, public :: REGEX_OVEC_SIZE = 150 !! PCRE2 `O` vector size (multiple of 3).

    type, public :: regex_type
        !! Opaque regular expression type.
        private
        type(c_ptr) :: ptr = c_null_ptr !! C pointer to PCRE2.
    end type regex_type

    public :: dm_regex_create
    public :: dm_regex_destroy
    public :: dm_regex_group
    public :: dm_regex_match
    public :: dm_regex_request
contains
    integer function dm_regex_create(regex, pattern, err_msg, err_offset) result(rc)
        !! Creates new regular expression type from given pattern.
        type(regex_type),              intent(out)           :: regex      !! Regular expression type.
        character(len=*),              intent(in)            :: pattern    !! Pattern to compile.
        character(len=:), allocatable, intent(out), optional :: err_msg    !! Error message.
        integer(kind=i8),              intent(out), optional :: err_offset !! Error offset in pattern.

        character(len=128) :: buffer ! PCRE2 error message buffer.
        integer            :: code, rc2
        integer(kind=i8)   :: offset

        rc = E_REGEX_COMPILE
        regex%ptr = pcre2_compile(pattern     = pattern, &
                                  length      = len(pattern, kind=PCRE2_SIZE), &
                                  options     = 0, &
                                  errorcode   = code, &
                                  erroroffset = offset, &
                                  ccontext    = c_null_ptr)

        if (.not. c_associated(regex%ptr)) then
            if (present(err_msg)) then
                buffer = ' '
                rc2 = pcre2_get_error_message(code, buffer, len(buffer, kind=PCRE2_SIZE))
                err_msg = trim(buffer)
            end if

            if (present(err_offset)) err_offset = offset
            return
        end if

        rc = E_NONE
    end function dm_regex_create

    integer function dm_regex_group(regex, subject, name, value) result(rc)
        !! Returns group value in given subject from compiled regular
        !! expression.
        type(regex_type),              intent(inout) :: regex   !! Regular expression type.
        character(len=*),              intent(in)    :: subject !! Input string.
        character(len=*),              intent(in)    :: name    !! Group name.
        character(len=:), allocatable, intent(out)   :: value   !! Group value.

        integer                  :: match
        integer(kind=PCRE2_SIZE) :: n
        type(c_ptr)              :: match_data

        rc = E_INVALID
        if (.not. c_associated(regex%ptr)) return

        pcre_block: block
            match_data = pcre2_match_data_create(REGEX_OVEC_SIZE, c_null_ptr)
            match = pcre2_match(code        = regex%ptr, &
                                subject     = subject, &
                                length      = len(subject, kind=PCRE2_SIZE), &
                                startoffset = int(0, kind=PCRE2_SIZE), &
                                options     = 0, &
                                match_data  = match_data, &
                                mcontext    = c_null_ptr)

            rc = E_REGEX_EXCEEDED
            if (match == 0) exit pcre_block

            rc = E_REGEX_NO_MATCH
            if (match == PCRE2_ERROR_NOMATCH) exit pcre_block

            rc = E_REGEX
            if (match < 0) exit pcre_block

            rc = E_REGEX_NO_GROUP
            if (pcre2_substring_get_byname(match_data, name, value, n) /= 0) &
                exit pcre_block

            rc = E_NONE
        end block pcre_block

        if (c_associated(match_data)) call pcre2_match_data_free(match_data)
    end function dm_regex_group

    integer function dm_regex_match(regex, subject) result(rc)
        !! Returns `E_NONE` if given subject matches the compiled regular
        !! expression.
        type(regex_type), intent(inout) :: regex   !! Regular expression type.
        character(len=*), intent(in)    :: subject !! Input string to match against.

        type(c_ptr) :: match_data
        integer     :: match

        rc = E_INVALID
        if (.not. c_associated(regex%ptr)) return

        match_data = pcre2_match_data_create(REGEX_OVEC_SIZE, c_null_ptr)
        match = pcre2_match(code        = regex%ptr, &
                            subject     = subject, &
                            length      = len(subject, kind=PCRE2_SIZE), &
                            startoffset = int(0, kind=PCRE2_SIZE), &
                            options     = 0, &
                            match_data  = match_data, &
                            mcontext    = c_null_ptr)
        call pcre2_match_data_free(match_data)

        rc = E_REGEX_EXCEEDED
        if (match == 0) return

        rc = E_REGEX_NO_MATCH
        if (match == PCRE2_ERROR_NOMATCH) return

        rc = E_REGEX
        if (match < 0) return

        rc = E_NONE
    end function dm_regex_match

    integer function dm_regex_request(request) result(rc)
        !! Extracts all values by group from raw response in given
        !! request type. The regular expression is compiled and
        !! destroyed by this function. The response error is set to
        !! any occuring error code.
        type(request_type), intent(inout) :: request !! Request type.

        character(len=:), allocatable :: buffer
        integer                       :: i, match, stat
        integer(kind=PCRE2_SIZE)      :: n
        type(c_ptr)                   :: match_data
        type(regex_type)              :: regex

        pcre_block: block
            rc = dm_regex_create(regex, trim(request%pattern))
            if (dm_is_error(rc)) exit pcre_block

            match_data = pcre2_match_data_create(REGEX_OVEC_SIZE, c_null_ptr)

            match = pcre2_match(code        = regex%ptr, &
                                subject     = request%response, &
                                length      = len_trim(request%response, kind=PCRE2_SIZE), &
                                startoffset = int(0, kind=PCRE2_SIZE), &
                                options     = 0, &
                                match_data  = match_data, &
                                mcontext    = c_null_ptr)

            rc = E_REGEX_EXCEEDED
            if (match == 0) exit pcre_block

            rc = E_REGEX_NO_MATCH
            if (match == PCRE2_ERROR_NOMATCH) exit pcre_block

            rc = E_REGEX
            if (match < 0) exit pcre_block

            do i = 1, request%nresponses
                request%responses(i)%error = E_REGEX_NO_GROUP

                stat = pcre2_substring_get_byname(match_data = match_data, &
                                                  name       = trim(request%responses(i)%name), &
                                                  buffer     = buffer, &
                                                  buff_len   = n)
                if (stat /= 0) cycle

                request%responses(i)%error = E_EMPTY
                if (n == 0) cycle

                call dm_convert_to(buffer, request%responses(i)%value, rc)
                request%responses(i)%error = rc
            end do

            rc = E_NONE
        end block pcre_block

        request%error = rc
        if (c_associated(match_data)) call pcre2_match_data_free(match_data)
        call dm_regex_destroy(regex)
    end function dm_regex_request

    subroutine dm_regex_destroy(regex)
        !! Destroys compiled regular expression.
        type(regex_type), intent(inout) :: regex !! Regular expression type.

        if (.not. c_associated(regex%ptr)) return
        call pcre2_code_free(regex%ptr)
        regex%ptr = c_null_ptr
    end subroutine dm_regex_destroy
end module dm_regex
