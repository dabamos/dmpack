! test_pcre2.f90
program test_pcre2
    use :: pcre2
    implicit none (type, external)
    integer, parameter            :: OVECSIZE = 30
    character(len=:), allocatable :: pattern, subject

    pattern = '^(?<first>[A-Z][a-z]+) (?<second>[A-Z][a-z]+)$'
    subject = 'Fortran Forever'

    print '("Pattern (len: ", i0,"): ", a)', len(pattern), pattern
    print '("Subject (len: ", i0,"): ", a)', len(subject), subject

    call test(pattern, subject)
contains
    subroutine test(pattern, subject)
        character(len=*), intent(in)      :: pattern
        character(len=*), intent(in)      :: subject
        character(len=128)                :: buffer
        character(len=:), allocatable     :: substring
        integer                           :: i, rc, rc2
        integer                           :: err_code, oveccount
        integer(kind=pcre2_size)          :: buffer_length, err_offset
        integer(kind=pcre2_size)          :: substring_length, substring_start
        integer(kind=pcre2_size), pointer :: ovector(:)
        type(c_ptr)                       :: match_data, ptr, re

        ! Compile regular expression.
        re = pcre2_compile(pattern, len(pattern, kind=pcre2_size), 0, err_code, err_offset, c_null_ptr)

        if (.not. c_associated(re)) then
            buffer = ' '
            rc = pcre2_get_error_message(err_code, buffer, len(buffer, kind=pcre2_size))
            print '("Error ", i0, ": ", a)', err_code, trim(buffer)
            return
        end if

        ! Execute regular expression.
        match_data = pcre2_match_data_create(OVECSIZE, c_null_ptr)
        rc = pcre2_match(code        = re, &
                         subject     = subject, &
                         length      = len(subject, kind=pcre2_size), &
                         startoffset = int(0, kind=pcre2_size), &
                         options     = 0, &
                         match_data  = match_data, &
                         mcontext    = c_null_ptr)

        if (rc < 0) then
            ! Matching failed.
            select case (rc)
                case (PCRE2_ERROR_NOMATCH)
                    print '("No match")'
                case default
                    print '("Matching error ", i0)', rc
            end select

            call pcre2_match_data_free(match_data)
            call pcre2_code_free(re)
            return
        end if

        if (rc == 0) then
            ! The output vector wasn't big enough.
            print '("ovector too small: ", i0)', rc
        end if

        oveccount = pcre2_get_ovector_count(match_data)
        print '("ovector count: ", i0)', oveccount

        ! Match succeeded.
        ptr = pcre2_get_ovector_pointer(match_data)
        call c_f_pointer(ptr, ovector, [ OVECSIZE ])

        ! Output substring positions and lengths.
        do i = 0, rc - 1
            substring_start  = 1 + ovector(2 * i + 1)
            substring_length = ovector(2 * i + 2) - ovector(2 * i + 1)
            print '("start: ", i0, " length: ", i0)', substring_start, substring_length
        end do

        ! Output substrings by number.
        print '(/, "--- get substrings by number")'

        do i = 0, rc - 1
            rc2 = pcre2_substring_get_bynumber(match_data, i, substring, substring_length)
            print '("substring ", i0, ": ", a)', i, substring
            deallocate (substring)
        end do

        print '(/, "--- copy substrings by number")'

        do i = 0, rc - 1
            buffer = ' '
            rc2 = pcre2_substring_copy_bynumber(match_data, i, buffer)
            print '("substring ", i0, ": ", a)', i, trim(buffer)
        end do

        ! Output substrings by name.
        print '(/, "--- get substrings by name")'

        rc2 = pcre2_substring_get_byname(match_data, 'first', substring, substring_length)
        print '("substring ''", a, "'': ", a)', 'first', substring
        deallocate (substring)

        rc2 = pcre2_substring_get_byname(match_data, 'second', substring, substring_length)
        print '("substring ''", a, "'': ", a)', 'second', substring
        deallocate (substring)

        print '(/, "--- copy substrings by name")'

        buffer = ' '
        buffer_length = len(buffer)
        rc2 = pcre2_substring_copy_byname(match_data, 'first', buffer, buffer_length)
        print '("substring ''", a, "'': ", a)', 'first', buffer(1:buffer_length)

        buffer = ' '
        rc2 = pcre2_substring_copy_byname(match_data, 'second', buffer)
        print '("substring ''", a, "'': ", a)', 'second', trim(buffer)

        call pcre2_match_data_free(match_data)
        call pcre2_code_free(re)
    end subroutine test
end program test_pcre2
