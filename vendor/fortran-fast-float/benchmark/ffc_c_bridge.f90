module ffc_c_bridge
    use iso_c_binding, only: c_char, c_double, c_int32_t, c_ptr, c_size_t, c_loc
    implicit none(type, external)
    private

    public :: benchmark_ffc_lines_c
    public :: ffc_parse_double_c

    type, bind(c) :: c_ffc_result
        type(c_ptr) :: ptr
        integer(c_int32_t) :: outcome
    end type c_ffc_result

    interface
        function c_ffc_parse_double(len, input, out) bind(c, name="ffc_parse_double_ws")
            import :: c_ffc_result, c_ptr, c_size_t
            integer(c_size_t), value :: len
            type(c_ptr), value :: input
            type(c_ptr), value :: out
            type(c_ffc_result) :: c_ffc_parse_double
        end function c_ffc_parse_double

        function c_benchmark_ffc_lines(data, offsets, lengths, nlines, outcome) bind(c, name="benchmark_ffc_lines")
            import :: c_double, c_ptr, c_size_t
            type(c_ptr), value :: data
            type(c_ptr), value :: offsets
            type(c_ptr), value :: lengths
            integer(c_size_t), value :: nlines
            type(c_ptr), value :: outcome
            real(c_double) :: c_benchmark_ffc_lines
        end function c_benchmark_ffc_lines
    end interface

contains

    subroutine ffc_parse_double_c(chars, n, out, outcome)
        character(kind=c_char), intent(in), target :: chars(*)
        integer, intent(in) :: n
        real(c_double), intent(out), target :: out
        integer(c_int32_t), intent(out) :: outcome
        type(c_ffc_result) :: res

        if (n == 0) then
            out = 0.0_c_double
            outcome = 1_c_int32_t
            return
        end if

        res = c_ffc_parse_double(int(n, c_size_t), c_loc(chars(1)), c_loc(out))
        outcome = res%outcome
    end subroutine ffc_parse_double_c

    subroutine benchmark_ffc_lines_c(data, offsets, lengths, nlines, answer, outcome)
        character(kind=c_char), intent(in), target :: data(*)
        integer(c_size_t), intent(in), target :: offsets(*)
        integer(c_size_t), intent(in), target :: lengths(*)
        integer, intent(in) :: nlines
        real(c_double), intent(out) :: answer
        integer(c_int32_t), intent(out), target :: outcome

        if (nlines == 0) then
            answer = 0.0_c_double
            outcome = 1_c_int32_t
            return
        end if

        answer = c_benchmark_ffc_lines( &
            c_loc(data(1)), &
            c_loc(offsets(1)), &
            c_loc(lengths(1)), &
            int(nlines, c_size_t), &
            c_loc(outcome))
    end subroutine benchmark_ffc_lines_c

end module ffc_c_bridge
