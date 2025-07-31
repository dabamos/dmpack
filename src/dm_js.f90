! Author:  Philipp Engel
! Licence: ISC
module dm_js
    !! Module for JavaScript syntax generation.
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    interface dm_js_const
        !! Generic function that returns JavaScript constant declaration from
        !! given name and value as allocatable string.
        module procedure :: js_const_int32
        module procedure :: js_const_int64
        module procedure :: js_const_real32
        module procedure :: js_const_real64
        module procedure :: js_const_string
    end interface dm_js_const

    public :: dm_js_const

    private :: js_const_int32
    private :: js_const_int64
    private :: js_const_real32
    private :: js_const_real64
    private :: js_const_string
    private :: js_const_type
contains
    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    pure function js_const_int32(name, value) result(js)
        !! Returns JavaScript constant declaration of 4-byte integer.
        character(len=*), intent(in)  :: name  !! Constant name.
        integer(kind=i4), intent(in)  :: value !! Constant value.
        character(len=:), allocatable :: js    !! JavaScript constant.

        js = js_const_type(name, dm_itoa(value))
    end function js_const_int32

    pure function js_const_int64(name, value) result(js)
        !! Returns JavaScript constant declaration of 8-byte integer.
        character(len=*), intent(in)  :: name  !! Constant name.
        integer(kind=i8), intent(in)  :: value !! Constant value.
        character(len=:), allocatable :: js    !! JavaScript constant.

        js = js_const_type(name, dm_itoa(value))
    end function js_const_int64

    pure function js_const_real32(name, value) result(js)
        !! Returns JavaScript constant declaration of 4-byte real.
        character(len=*), intent(in)  :: name  !! Constant name.
        real(kind=r4),    intent(in)  :: value !! Constant value.
        character(len=:), allocatable :: js    !! JavaScript constant.

        js = js_const_type(name, dm_ftoa(value))
    end function js_const_real32

    pure function js_const_real64(name, value) result(js)
        !! Returns JavaScript constant declaration of 8-byte real.
        character(len=*), intent(in)  :: name  !! Constant name.
        real(kind=r8),    intent(in)  :: value !! Constant value.
        character(len=:), allocatable :: js    !! JavaScript constant.

        js = js_const_type(name, dm_ftoa(value))
    end function js_const_real64

    pure function js_const_string(name, value) result(js)
        !! Returns JavaScript constant declaration of string.
        character(len=*), intent(in)  :: name  !! Constant name.
        character(len=*), intent(in)  :: value !! Constant value.
        character(len=:), allocatable :: js    !! JavaScript constant.

        js = js_const_type(name, '"' // trim(value) // '"')
    end function js_const_string

    pure function js_const_type(name, value) result(js)
        !! Creates JavaScript constant declaration of given name and value.
        character(len=*), intent(in)  :: name  !! Constant name.
        character(len=*), intent(in)  :: value !! Constant value as string.
        character(len=:), allocatable :: js    !! JavaScript constant.

        js = 'const ' // trim(name) // ' = ' // value // ';'
    end function js_const_type
end module dm_js
