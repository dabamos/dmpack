! Author:  Philipp Engel
! Licence: ISC
module dm_random
    use :: dm_kind
    implicit none (type, external)
    private

    interface dm_random_get_uniform
        !! Generic function to return a uniformly distributed random
        !! number in the inclusive interval [a, b].
        module procedure :: random_get_uniform_int32
        module procedure :: random_get_uniform_int64
        module procedure :: random_get_uniform_real32
        module procedure :: random_get_uniform_real64
    end interface dm_random_get_uniform

    interface dm_random_uniform
        !! Generic subroutine to generate uniformly distributed random
        !! numbers in the inclusive interval [a, b].
        module procedure :: random_uniform_int32
        module procedure :: random_uniform_int64
        module procedure :: random_uniform_real32
        module procedure :: random_uniform_real64
    end interface dm_random_uniform

    public :: dm_random_init
    public :: dm_random_get
    public :: dm_random_get_uniform
    public :: dm_random_uniform

    private :: random_get_uniform_int32
    private :: random_get_uniform_int64
    private :: random_get_uniform_real32
    private :: random_get_uniform_real64
    private :: random_uniform_int32
    private :: random_uniform_int64
    private :: random_uniform_real32
    private :: random_uniform_real64
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    function dm_random_get() result(x)
        !! Returns 8-byte pseudo-random number.
        real(r8) :: x

        call random_number(x)
    end function dm_random_get

    subroutine dm_random_init()
        !! Initialises PRNG.
        call random_init(repeatable=.false., image_distinct=.false.)
    end subroutine dm_random_init

    ! **************************************************************************
    ! PRIVATE FUNCTIONS
    ! **************************************************************************
    function random_get_uniform_int32(a, b) result(x)
        !! Returns 4-byte uniform pseudo-random integer in interval [a, b].
        integer(i4), intent(in) :: a
        integer(i4), intent(in) :: b
        integer(i4)             :: x

        real(r8) :: u

        call random_number(u)
        x = a + int((int(b, i8) - int(a, i8) + 1_i8) * u, i4)
    end function random_get_uniform_int32

    function random_get_uniform_int64(a, b) result(x)
        !! Returns 8-byte uniform pseudo-random integer in interval [a, b].
        !!
        !! This int64 implementation is statistically correct only when the
        !! interval length (b - a + 1) can be represented accurately by the
        !! floating-point type used (real64). Since real64 has only 53 bits of
        !! precision, it cannot represent all 64-bit integers exactly.
        !!
        !! If a uniform distribution over arbitrary 64-bit integer ranges is
        !! needed (especially ranges larger than 253253), a floating-point
        !! scaling method is insufficient. In that case, a rejection-sampling
        !! algorithm based on random bits should be used instead. For typical
        !! applications involving moderate integer ranges, this implementation
        !! seems to be adequate.
        integer(i8), intent(in) :: a
        integer(i8), intent(in) :: b
        integer(i8)             :: x

        real(r8) :: u

        call random_number(u)
        x = a + int((b - a + 1_i8) * u, i8)
    end function random_get_uniform_int64

    function random_get_uniform_real32(a, b) result(x)
        !! Returns 4-byte uniform pseudo-random real in interval [a, b].
        real(r4), intent(in) :: a
        real(r4), intent(in) :: b
        real(r4)             :: x

        call random_number(x)
        x = a + (b - a) * x
    end function random_get_uniform_real32

    function random_get_uniform_real64(a, b) result(x)
        !! Returns 8-byte uniform pseudo-random real in interval [a, b].
        real(r8), intent(in) :: a
        real(r8), intent(in) :: b
        real(r8)             :: x

        call random_number(x)
        x = a + (b - a) * x
    end function random_get_uniform_real64

    ! **************************************************************************
    ! PRIVATE SUBROUTINES
    ! **************************************************************************
    impure elemental subroutine random_uniform_int32(x, a, b)
        integer(i4), intent(out) :: x
        integer(i4), intent(in)  :: a
        integer(i4), intent(in)  :: b

        real(r8) :: u

        call random_number(u)
        x = a + int((int(b, i8) - int(a, i8) + 1_i8) * u, i4)
    end subroutine random_uniform_int32

    impure elemental subroutine random_uniform_int64(x, a, b)
        !! This int64 implementation is statistically correct only when the
        !! interval length (b - a + 1) can be represented accurately by the
        !! floating-point type used (real64). Since real64 has only 53 bits of
        !! precision, it cannot represent all 64-bit integers exactly.
        !!
        !! If a uniform distribution over arbitrary 64-bit integer ranges is
        !! needed (especially ranges larger than 253253), a floating-point
        !! scaling method is insufficient. In that case, a rejection-sampling
        !! algorithm based on random bits should be used instead. For typical
        !! applications involving moderate integer ranges, this implementation
        !! seems to be adequate.
        integer(i8), intent(out) :: x
        integer(i8), intent(in)  :: a
        integer(i8), intent(in)  :: b

        real(r8) :: u

        call random_number(u)
        x = a + int((b - a + 1_i8) * u, i8)
    end subroutine random_uniform_int64

    impure elemental subroutine random_uniform_real32(x, a, b)
        real(r4), intent(out) :: x
        real(r4), intent(in)  :: a
        real(r4), intent(in)  :: b

        call random_number(x)
        x = a + (b - a) * x
    end subroutine random_uniform_real32

    impure elemental subroutine random_uniform_real64(x, a, b)
        real(r8), intent(out) :: x
        real(r8), intent(in)  :: a
        real(r8), intent(in)  :: b

        call random_number(x)
        x = a + (b - a) * x
    end subroutine random_uniform_real64
end module dm_random
