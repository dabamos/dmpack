! Author:  Philipp Engel
! Licence: ISC
module dm_regex
    !! Abstraction layer over PCRE2.
    use, intrinsic :: iso_c_binding
    use :: pcre2
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter :: REGEX_VECTOR_SIZE = 150 !! PCRE2 O-vector size (must be multiple of 3).

    type, public :: regex_type
        !! Opaque regular expression type.
        private
        type(c_ptr) :: context = c_null_ptr !! C pointer to PCRE2.
    end type regex_type

    public :: dm_regex_create
    public :: dm_regex_destroy
    public :: dm_regex_group
    public :: dm_regex_match
    public :: dm_regex_observ
    public :: dm_regex_response_string
contains
    integer function dm_regex_create(regex, pattern, error_message, error_offset) result(rc)
        !! Creates new regular expression type from given pattern. Returns
        !! `E_REGEX_COMPILE` on error.
        type(regex_type),          intent(out)           :: regex         !! Regular expression type.
        character(*),              intent(in)            :: pattern       !! Pattern to compile.
        character(:), allocatable, intent(out), optional :: error_message !! Error message.
        integer(i8),               intent(out), optional :: error_offset  !! Error offset in pattern.

        character(128) :: buffer ! PCRE2 error message buffer.
        integer        :: code, stat
        integer(i8)    :: offset

        rc = E_REGEX_COMPILE
        regex%context = pcre2_compile(pattern     = pattern, &
                                      length      = len(pattern, pcre2_size), &
                                      options     = 0, &
                                      errorcode   = code, &
                                      erroroffset = offset, &
                                      ccontext    = c_null_ptr)

        if (.not. c_associated(regex%context)) then
            if (present(error_message)) then
                buffer = ' '
                stat   = pcre2_get_error_message(code, buffer, len(buffer, pcre2_size))

                error_message = trim(buffer)
            end if

            if (present(error_offset)) error_offset = offset
            return
        end if

        rc = E_NONE
    end function dm_regex_create

    integer function dm_regex_group(regex, subject, name, value) result(rc)
        !! Returns group value in given subject from compiled regular
        !! expression.
        !!
        !! The functions returns the following error codes:
        !!
        !! * `E_NULL` if regular expression context is not associated.
        !! * `E_REGEX` if a PCRE2 library error occured.
        !! * `E_REGEX_EXCEEDED` if the number of matches exceeds the O vector size.
        !! * `E_REGEX_NO_GROUP` if no group matches.
        !! * `E_REGEX_NO_MATCH` if the pattern does not match.
        !!
        type(regex_type),          intent(inout) :: regex   !! Regular expression type.
        character(*),              intent(in)    :: subject !! Input string.
        character(*),              intent(in)    :: name    !! Group name.
        character(:), allocatable, intent(out)   :: value   !! Group value.

        integer             :: match
        integer(pcre2_size) :: n
        type(c_ptr)         :: match_data

        rc = E_NULL
        if (.not. c_associated(regex%context)) return

        pcre_block: block
            match_data = pcre2_match_data_create(REGEX_VECTOR_SIZE, c_null_ptr)

            match = pcre2_match(code        = regex%context, &
                                subject     = subject, &
                                length      = len(subject, pcre2_size), &
                                startoffset = 0_pcre2_size, &
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
            if (pcre2_substring_get_byname(match_data, name, value, n) /= 0) exit pcre_block

            rc = E_NONE
        end block pcre_block

        if (c_associated(match_data)) call pcre2_match_data_free(match_data)
    end function dm_regex_group

    integer function dm_regex_match(regex, subject) result(rc)
        !! Returns `E_NONE` if given subject matches the compiled regular
        !! expression.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_NULL` if regular expression context is not associated.
        !! * `E_REGEX` if a PCRE2 library error occured.
        !! * `E_REGEX_EXCEEDED` if the number of matches exceeds the O vector size.
        !! * `E_REGEX_NO_MATCH` if the pattern does not match.
        !!
        type(regex_type), intent(inout) :: regex   !! Regular expression type.
        character(*),     intent(in)    :: subject !! Input string to match against.

        type(c_ptr) :: match_data
        integer     :: match

        rc = E_NULL
        if (.not. c_associated(regex%context)) return

        match_data = pcre2_match_data_create(REGEX_VECTOR_SIZE, c_null_ptr)

        match = pcre2_match(code        = regex%context, &
                            subject     = subject, &
                            length      = len(subject, pcre2_size), &
                            startoffset = 0_pcre2_size, &
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

    integer function dm_regex_observ(observ) result(rc)
        !! Extracts all values by group from raw response in given observ type.
        !! The regular expression is compiled and destroyed by this function.
        !! The response error is set to any occuring error code.
        !!
        !! The function sets the following request error codes and returns:
        !!
        !! * `E_INCOMPLETE` if the observation contains no pattern or no responses.
        !! * `E_INVALID` if the regular expression is invalid.
        !! * `E_REGEX` if a PCRE2 library error occured.
        !! * `E_REGEX_COMPILE` if the pattern failed to compile.
        !! * `E_REGEX_EXCEEDED` if the number of matches exceeds the O vector size.
        !! * `E_REGEX_NO_MATCH` if the pattern does not match.
        !!
        !! The function sets the following response error codes:
        !!
        !! * `E_EMPTY` if response string of regular expression group is empty.
        !! * `E_INCOMPLETE` if response could not be extracted.
        !! * `E_REGEX_NO_GROUP` if no regular expression group matches.
        !!
        use :: dm_observ
        use :: dm_response
        use :: dm_string
        use :: dm_util

        type(observ_type), intent(inout) :: observ !! Observation.

        character(:), allocatable :: buffer
        integer                   :: i, ibyte, match, stat
        integer(pcre2_size)       :: n
        type(c_ptr)               :: match_data
        type(regex_type)          :: regex

        ! Nothing to extract.
        rc = E_INCOMPLETE
        if (observ%nresponses == 0) return
        observ%responses(1:observ%nresponses)%error = rc

        if (len_trim(observ%pattern) == 0) return

        pcre_block: block
            ! Create regular expression.
            rc = dm_regex_create(regex, trim(observ%pattern))
            if (dm_is_error(rc)) exit pcre_block

            ! Match regular expression.
            match_data = pcre2_match_data_create(REGEX_VECTOR_SIZE, c_null_ptr)

            match = pcre2_match(code        = regex%context, &
                                subject     = observ%response, &
                                length      = len_trim(observ%response, pcre2_size), &
                                startoffset = 0_pcre2_size, &
                                options     = 0, &
                                match_data  = match_data, &
                                mcontext    = c_null_ptr)

            ! Validate result.
            rc = E_REGEX_EXCEEDED
            if (match == 0) exit pcre_block

            rc = E_REGEX_NO_MATCH
            if (match == PCRE2_ERROR_NOMATCH) exit pcre_block

            rc = E_REGEX
            if (match < 0) exit pcre_block

            ! Copy sub-strings to responses.
            rc = E_NONE

            do i = 1, observ%nresponses
                associate (response => observ%responses(i))
                    response_block: block
                        ! Get sub-string by name.
                        rc   = E_REGEX_NO_GROUP
                        stat = pcre2_substring_get_byname(match_data = match_data, &
                                                          name       = trim(response%name), &
                                                          buffer     = buffer, &
                                                          buff_len   = n)
                        if (stat /= 0) exit response_block

                        ! Check string length.
                        rc = E_EMPTY
                        if (n == 0) exit response_block

                        ! Convert string to real.
                        rc = E_TYPE
                        select case (response%type)
                            ! Convert string to real.
                            case (RESPONSE_TYPE_REAL64, RESPONSE_TYPE_REAL32, &
                                  RESPONSE_TYPE_INT64,  RESPONSE_TYPE_INT32)
                                call dm_string_to(buffer, response%value, rc)

                            ! Convert byte to real.
                            case (RESPONSE_TYPE_BYTE)
                                read (buffer, '(z2)', iostat=stat) ibyte

                                if (stat == 0) then
                                    rc = E_NONE
                                    response%value = dm_to_real64(ibyte)
                                end if

                            ! Do not extract strings.
                            case (RESPONSE_TYPE_STRING)
                                rc = E_NONE

                            ! Invalid (type error).
                            case default
                                continue
                        end select
                    end block response_block

                    response%error = rc
                end associate
            end do
        end block pcre_block

        if (c_associated(match_data)) call pcre2_match_data_free(match_data)
        call dm_regex_destroy(regex)
    end function dm_regex_observ

    integer function dm_regex_response_string(observ, name, string, pattern) result(rc)
        !! Returns response string from raw response, extracted by group name
        !! `name`. If `pattern` is passed, it is used as the regular expression
        !! pattern instead of the request pattern.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the response string of the group is empty.
        !! * `E_INCOMPLETE` if the observation contains no pattern.
        !! * `E_INVALID` if the regular expression is invalid.
        !! * `E_REGEX` if a PCRE2 library error occured.
        !! * `E_REGEX_COMPILE` if the pattern failed to compile.
        !! * `E_REGEX_EXCEEDED` if the number of matches exceeds the O vector size.
        !! * `E_REGEX_NO_GROUP` if `name` does not match any group.
        !! * `E_REGEX_NO_MATCH` if the pattern does not match.
        !!
        !! On error, the group string is allocated, but may be empty.
        use :: dm_observ

        type(observ_type),         intent(inout)        :: observ  !! Observation.
        character(*),              intent(in)           :: name    !! Response name or regular expression group.
        character(:), allocatable, intent(out)          :: string  !! String extracted from group `name`.
        character(*),              intent(in), optional :: pattern !! Pattern to use instead of the observation pattern.

        integer             :: match, stat
        integer(pcre2_size) :: n
        type(c_ptr)         :: match_data
        type(regex_type)    :: regex

        rc = E_INCOMPLETE
        if (present(pattern)) then
            if (len_trim(pattern) == 0) return
        else
            if (len_trim(observ%pattern) == 0) return
        end if

        pcre_block: block
            ! Create regular expression.
            if (present(pattern)) then
                rc = dm_regex_create(regex, pattern)
            else
                rc = dm_regex_create(regex, trim(observ%pattern))
            end if

            if (dm_is_error(rc)) exit pcre_block

            ! Match regular expression.
            match_data = pcre2_match_data_create(REGEX_VECTOR_SIZE, c_null_ptr)

            match = pcre2_match(code        = regex%context, &
                                subject     = observ%response, &
                                length      = len_trim(observ%response, pcre2_size), &
                                startoffset = 0_pcre2_size, &
                                options     = 0, &
                                match_data  = match_data, &
                                mcontext    = c_null_ptr)

            ! Validate result.
            rc = E_REGEX_EXCEEDED
            if (match == 0) exit pcre_block

            rc = E_REGEX_NO_MATCH
            if (match == PCRE2_ERROR_NOMATCH) exit pcre_block

            rc = E_REGEX
            if (match < 0) exit pcre_block

            ! Get sub-string by name.
            stat = pcre2_substring_get_byname(match_data = match_data, &
                                              name       = trim(name), &
                                              buffer     = string, &
                                              buff_len   = n)
            rc = E_REGEX_NO_GROUP
            if (stat /= 0) exit pcre_block

            ! Check string length.
            rc = E_EMPTY
            if (n == 0) exit pcre_block

            rc = E_NONE
        end block pcre_block

        if (.not. allocated(string)) string = ''
        if (c_associated(match_data)) call pcre2_match_data_free(match_data)
        call dm_regex_destroy(regex)
    end function dm_regex_response_string

    subroutine dm_regex_destroy(regex)
        !! Destroys compiled regular expression.
        type(regex_type), intent(inout) :: regex !! Regular expression type.

        if (.not. c_associated(regex%context)) return
        call pcre2_code_free(regex%context)
        regex%context = c_null_ptr
    end subroutine dm_regex_destroy
end module dm_regex
