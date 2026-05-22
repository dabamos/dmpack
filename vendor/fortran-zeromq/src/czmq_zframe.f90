! czmq_zframe.f90
!
! Author:  Philipp Engel
! Licence: ISC
module czmq_zframe
    !! Auto-generated Fortran 2018 interface bindings to libczmq 4.
    use :: zmq_util
    implicit none (type, external)
    private

    integer(c_int), parameter, public :: CZMQ_ZFRAME_MORE     = 1
    integer(c_int), parameter, public :: CZMQ_ZFRAME_REUSE    = 2
    integer(c_int), parameter, public :: CZMQ_ZFRAME_DONTWAIT = 4

    public :: zframe_data
    public :: zframe_destroy
    public :: zframe_dup
    public :: zframe_eq
    public :: zframe_from
    public :: zframe_from_
    public :: zframe_is
    public :: zframe_meta
    public :: zframe_meta_
    public :: zframe_more
    public :: zframe_new
    public :: zframe_new_empty
    public :: zframe_print
    public :: zframe_print_
    public :: zframe_recv
    public :: zframe_reset
    public :: zframe_send
    public :: zframe_set_more
    public :: zframe_size
    public :: zframe_strdup
    public :: zframe_strdup_
    public :: zframe_streq
    public :: zframe_streq_
    public :: zframe_strhex

   interface
        ! byte *zframe_data(zframe_t *self)
        function zframe_data(self) bind(c, name='zframe_data')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zframe_data
        end function zframe_data

        ! void zframe_destroy(zframe_t **self_p)
        subroutine zframe_destroy(self_p) bind(c, name='zframe_destroy')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(inout) :: self_p
        end subroutine zframe_destroy

        ! zframe_t *zframe_dup(zframe_t *self)
        function zframe_dup(self) bind(c, name='zframe_dup')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zframe_dup
        end function zframe_dup

        ! bool zframe_eq(zframe_t *self, zframe_t *other)
        function zframe_eq(self, other) bind(c, name='zframe_eq')
            import :: c_bool, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(in), value :: other
            logical(c_bool)                :: zframe_eq
        end function zframe_eq

        ! zframe_t *zframe_from(const char *string)
        function zframe_from_(string) bind(c, name='zframe_from')
            import :: c_char, c_ptr
            implicit none
            character(c_char), intent(in) :: string
            type(c_ptr)                   :: zframe_from_
        end function zframe_from_

        ! bool zframe_is(void *self)
        function zframe_is(self) bind(c, name='zframe_is')
            import :: c_bool, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            logical(c_bool)                :: zframe_is
        end function zframe_is

        ! const char *zframe_meta(zframe_t *self, const char *property)
        function zframe_meta_(self, property) bind(c, name='zframe_meta')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: property
            type(c_ptr)                          :: zframe_meta_
        end function zframe_meta_

        ! int zframe_more(zframe_t *self)
        function zframe_more(self) bind(c, name='zframe_more')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zframe_more
        end function zframe_more

        ! zframe_t *zframe_new(const void *data, size_t size)
        function zframe_new(data, size) bind(c, name='zframe_new')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: data
            integer(c_size_t), intent(in), value :: size
            type(c_ptr)                          :: zframe_new
        end function zframe_new

        ! zframe_t *zframe_new_empty(void)
        function zframe_new_empty() bind(c, name='zframe_new_empty')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zframe_new_empty
        end function zframe_new_empty

        ! void zframe_print(zframe_t *self, const char *prefix)
        subroutine zframe_print_(self, prefix) bind(c, name='zframe_print')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: prefix
        end subroutine zframe_print_

        ! zframe_t *zframe_recv(void *source)
        function zframe_recv(source) bind(c, name='zframe_recv')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: source
            type(c_ptr)                    :: zframe_recv
        end function zframe_recv

        ! void zframe_reset(zframe_t *self, const void *data, size_t size)
        subroutine zframe_reset(self, data, size) bind(c, name='zframe_reset')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: self
            type(c_ptr),       intent(in), value :: data
            integer(c_size_t), intent(in), value :: size
        end subroutine zframe_reset

        ! int zframe_send(zframe_t **self_p, void *dest, int flags)
        function zframe_send(self_p, dest, flags) bind(c, name='zframe_send')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(inout)     :: self_p
            type(c_ptr),    intent(in), value :: dest
            integer(c_int), intent(in), value :: flags
            integer(c_int)                    :: zframe_send
        end function zframe_send

        ! void zframe_set_more(zframe_t *self, int more)
        subroutine zframe_set_more(self, more) bind(c, name='zframe_set_more')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),    intent(in), value :: self
            integer(c_int), intent(in), value :: more
        end subroutine zframe_set_more

        ! size_t zframe_size(zframe_t *self)
        function zframe_size(self) bind(c, name='zframe_size')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_size_t)              :: zframe_size
        end function zframe_size

        ! char *zframe_strdup(zframe_t *self)
        function zframe_strdup_(self) bind(c, name='zframe_strdup')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zframe_strdup_
        end function zframe_strdup_

        ! bool zframe_streq(zframe_t *self, const char *string)
        function zframe_streq_(self, string) bind(c, name='zframe_streq')
            import :: c_bool, c_char, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: string
            logical(c_bool)                      :: zframe_streq_
        end function zframe_streq_

        ! char *zframe_strhex(zframe_t *self)
        function zframe_strhex(self) bind(c, name='zframe_strhex')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zframe_strhex
        end function zframe_strhex
    end interface
contains
    ! zframe_t *zframe_from(const char *string)
    function zframe_from(string) result(ptr)
        character(*), intent(in) :: string
        type(c_ptr)              :: ptr

        ptr = zframe_from_(f_c_str(string))
    end function zframe_from

    ! const char *zframe_meta(zframe_t *self, const char *property)
    function zframe_meta(self, property) result(str)
        type(c_ptr),  intent(in)  :: self
        character(*), intent(in)  :: property
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zframe_meta_(self, f_c_str(property))
        call c_f_str_ptr(ptr, str)
    end function zframe_meta

    ! void zframe_print(zframe_t *self, const char *prefix)
    subroutine zframe_print(self, prefix)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: prefix

        call zframe_print_(self, f_c_str(prefix))
    end subroutine zframe_print

    ! char *zframe_strdup(zframe_t *self)
    function zframe_strdup(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zframe_strdup_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zframe_strdup

    ! bool zframe_streq(zframe_t *self, const char *string)
    logical function zframe_streq(self, string) result(equals)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: string

        equals = zframe_streq_(self, f_c_str(string))
    end function zframe_streq
end module czmq_zframe
