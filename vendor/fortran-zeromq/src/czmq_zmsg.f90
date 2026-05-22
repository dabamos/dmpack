! czmq_zmsg.f90
!
! Author:  Philipp Engel
! Licence: ISC
module czmq_zmsg
    !! Auto-generated Fortran 2018 interface bindings to libczmq 4.
    use :: zmq_util
    implicit none (type, external)
    private

    public :: zmsg_add
    public :: zmsg_addmem
    public :: zmsg_addmsg
    public :: zmsg_addstr
    public :: zmsg_addstr_
    public :: zmsg_append
    public :: zmsg_content_size
    public :: zmsg_decode
    public :: zmsg_destroy
    public :: zmsg_dup
    public :: zmsg_encode
    public :: zmsg_eq
    public :: zmsg_first
    public :: zmsg_fprint
    public :: zmsg_is
    public :: zmsg_last
    public :: zmsg_load
    public :: zmsg_new
    public :: zmsg_new_signal
    public :: zmsg_next
    public :: zmsg_pop
    public :: zmsg_popmsg
    public :: zmsg_popstr
    public :: zmsg_popstr_
    public :: zmsg_prepend
    public :: zmsg_print
    public :: zmsg_push
    public :: zmsg_pushmem
    public :: zmsg_pushstr
    public :: zmsg_pushstr_
    public :: zmsg_recv
    public :: zmsg_recv_nowait
    public :: zmsg_remove
    public :: zmsg_save
    public :: zmsg_send
    public :: zmsg_sendm
    public :: zmsg_signal
    public :: zmsg_size
    public :: zmsg_test
    public :: zmsg_unwrap
    public :: zmsg_wrap

    interface
        ! int zmsg_add(zmsg_t *self, zframe_t *frame)
        function zmsg_add(self, frame) bind(c, name='zmsg_add')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(in), value :: frame
            integer(c_int)                 :: zmsg_add
        end function zmsg_add

        ! int zmsg_addmem(zmsg_t *self, const void *data, size_t size)
        function zmsg_addmem(self, data, size) bind(c, name='zmsg_addmem')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: self
            type(c_ptr),       intent(in), value :: data
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: zmsg_addmem
        end function zmsg_addmem

        ! int zmsg_addmsg(zmsg_t *self, zmsg_t **msg_p)
        function zmsg_addmsg(self, msg_p) bind(c, name='zmsg_addmsg')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(inout)     :: msg_p
            integer(c_int)                 :: zmsg_addmsg
        end function zmsg_addmsg

        ! int zmsg_addstr(zmsg_t *self, const char *string)
        function zmsg_addstr_(self, string) bind(c, name='zmsg_addstr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: string
            integer(c_int)                       :: zmsg_addstr_
        end function zmsg_addstr_

        ! int zmsg_append(zmsg_t *self, zframe_t **frame_p)
        function zmsg_append(self, frame_p) bind(c, name='zmsg_append')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(inout)     :: frame_p
            integer(c_int)                 :: zmsg_append
        end function zmsg_append

        ! size_t zmsg_content_size(zmsg_t *self)
        function zmsg_content_size(self) bind(c, name='zmsg_content_size')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_size_t)              :: zmsg_content_size
        end function zmsg_content_size

        ! zmsg_t *zmsg_decode(zframe_t *frame)
        function zmsg_decode(frame) bind(c, name='zmsg_decode')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: frame
            type(c_ptr)                    :: zmsg_decode
        end function zmsg_decode

        ! void zmsg_destroy(zmsg_t **self_p)
        subroutine zmsg_destroy(self_p) bind(c, name='zmsg_destroy')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(inout) :: self_p
        end subroutine zmsg_destroy

        ! zmsg_t *zmsg_dup(zmsg_t *self)
        function zmsg_dup(self) bind(c, name='zmsg_dup')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zmsg_dup
        end function zmsg_dup

        ! zframe_t *zmsg_encode(zmsg_t *self)
        function zmsg_encode(self) bind(c, name='zmsg_encode')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zmsg_encode
        end function zmsg_encode

        ! bool zmsg_eq(zmsg_t *self, zmsg_t *other)
        function zmsg_eq(self, other) bind(c, name='zmsg_eq')
            import :: c_bool, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(in), value :: other
            logical(c_bool)                :: zmsg_eq
        end function zmsg_eq

        ! zframe_t *zmsg_first(zmsg_t *self)
        function zmsg_first(self) bind(c, name='zmsg_first')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zmsg_first
        end function zmsg_first

        ! void zmsg_fprint(zmsg_t *self, FILE *file)
        subroutine zmsg_fprint(self, file) bind(c, name='zmsg_fprint')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(in), value :: file
        end subroutine zmsg_fprint

        ! bool zmsg_is(void *self)
        function zmsg_is(self) bind(c, name='zmsg_is')
            import :: c_bool, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            logical(c_bool)                :: zmsg_is
        end function zmsg_is

        ! zframe_t *zmsg_last(zmsg_t *self)
        function zmsg_last(self) bind(c, name='zmsg_last')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zmsg_last
        end function zmsg_last

        ! zmsg_t *zmsg_load(FILE *file)
        function zmsg_load(file) bind(c, name='zmsg_load')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: file
            type(c_ptr)                    :: zmsg_load
        end function zmsg_load

        ! zmsg_t *zmsg_new(void)
        function zmsg_new() bind(c, name='zmsg_new')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zmsg_new
        end function zmsg_new

        ! zmsg_t *zmsg_new_signal(byte status)
        function zmsg_new_signal(status) bind(c, name='zmsg_new_signal')
            import :: c_byte, c_ptr
            implicit none
            integer(c_byte), intent(in), value :: status
            type(c_ptr)                        :: zmsg_new_signal
        end function zmsg_new_signal

        ! zframe_t *zmsg_next(zmsg_t *self)
        function zmsg_next(self) bind(c, name='zmsg_next')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zmsg_next
        end function zmsg_next

        ! zframe_t *zmsg_pop(zmsg_t *self)
        function zmsg_pop(self) bind(c, name='zmsg_pop')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zmsg_pop
        end function zmsg_pop

        ! zmsg_t *zmsg_popmsg(zmsg_t *self)
        function zmsg_popmsg(self) bind(c, name='zmsg_popmsg')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zmsg_popmsg
        end function zmsg_popmsg

        ! char *zmsg_popstr(zmsg_t *self)
        function zmsg_popstr_(self) bind(c, name='zmsg_popstr')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zmsg_popstr_
        end function zmsg_popstr_

        ! int zmsg_prepend(zmsg_t *self, zframe_t **frame_p)
        function zmsg_prepend(self, frame_p) bind(c, name='zmsg_prepend')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(inout)     :: frame_p
            integer(c_int)                 :: zmsg_prepend
        end function zmsg_prepend

        ! void zmsg_print(zmsg_t *self)
        subroutine zmsg_print(self) bind(c, name='zmsg_print')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
        end subroutine zmsg_print

        ! int zmsg_push(zmsg_t *self, zframe_t *frame)
        function zmsg_push(self, frame) bind(c, name='zmsg_push')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(in), value :: frame
            integer(c_int)                 :: zmsg_push
        end function zmsg_push

        ! int zmsg_pushmem(zmsg_t *self, const void *data, size_t size)
        function zmsg_pushmem(self, data, size) bind(c, name='zmsg_pushmem')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: self
            type(c_ptr),       intent(in), value :: data
            integer(c_size_t), intent(in), value :: size
            integer(c_int)                       :: zmsg_pushmem
        end function zmsg_pushmem

        ! int zmsg_pushstr(zmsg_t *self, const char *string)
        function zmsg_pushstr_(self, string) bind(c, name='zmsg_pushstr')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),       intent(in), value :: self
            character(c_char), intent(in)        :: string
            integer(c_int)                       :: zmsg_pushstr_
        end function zmsg_pushstr_

        ! zmsg_t *zmsg_recv(void *source)
        function zmsg_recv(source) bind(c, name='zmsg_recv')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: source
            type(c_ptr)                    :: zmsg_recv
        end function zmsg_recv

        ! zmsg_t *zmsg_recv_nowait(void *source)
        function zmsg_recv_nowait(source) bind(c, name='zmsg_recv_nowait')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: source
            type(c_ptr)                    :: zmsg_recv_nowait
        end function zmsg_recv_nowait

        ! void zmsg_remove(zmsg_t *self, zframe_t *frame)
        subroutine zmsg_remove(self, frame) bind(c, name='zmsg_remove')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(in), value :: frame
        end subroutine zmsg_remove

        ! int zmsg_save(zmsg_t *self, FILE *file)
        function zmsg_save(self, file) bind(c, name='zmsg_save')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(in), value :: file
            integer(c_int)                 :: zmsg_save
        end function zmsg_save

        ! int zmsg_send(zmsg_t **self_p, void *dest)
        function zmsg_send(self_p, dest) bind(c, name='zmsg_send')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(inout)     :: self_p
            type(c_ptr), intent(in), value :: dest
            integer(c_int)                 :: zmsg_send
        end function zmsg_send

        ! int zmsg_sendm(zmsg_t **self_p, void *dest)
        function zmsg_sendm(self_p, dest) bind(c, name='zmsg_sendm')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(inout)     :: self_p
            type(c_ptr), intent(in), value :: dest
            integer(c_int)                 :: zmsg_sendm
        end function zmsg_sendm

        ! int zmsg_signal(zmsg_t *self)
        function zmsg_signal(self) bind(c, name='zmsg_signal')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_int)                 :: zmsg_signal
        end function zmsg_signal

        ! size_t zmsg_size(zmsg_t *self)
        function zmsg_size(self) bind(c, name='zmsg_size')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: self
            integer(c_size_t)              :: zmsg_size
        end function zmsg_size

        ! void zmsg_test(bool verbose)
        subroutine zmsg_test(verbose) bind(c, name='zmsg_test')
            import :: c_bool
            implicit none
            logical(c_bool), intent(in), value :: verbose
        end subroutine zmsg_test

        ! zframe_t *zmsg_unwrap(zmsg_t *self)
        function zmsg_unwrap(self) bind(c, name='zmsg_unwrap')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr)                    :: zmsg_unwrap
        end function zmsg_unwrap

        ! void zmsg_wrap(zmsg_t *self, zframe_t *frame)
        subroutine zmsg_wrap(self, frame) bind(c, name='zmsg_wrap')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: self
            type(c_ptr), intent(in), value :: frame
        end subroutine zmsg_wrap
    end interface
contains
    ! int zmsg_addstr(zmsg_t *self, const char *string)
    integer function zmsg_addstr(self, string) result(rc)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: string

        rc = zmsg_addstr_(self, f_c_str(string))
    end function zmsg_addstr

    ! char *zmsg_popstr(zmsg_t *self)
    function zmsg_popstr(self) result(str)
        type(c_ptr), intent(in)   :: self
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zmsg_popstr_(self)
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zmsg_popstr

    ! int zmsg_pushstr(zmsg_t *self, const char *string)
    integer function zmsg_pushstr(self, string) result(rc)
        type(c_ptr),  intent(in) :: self
        character(*), intent(in) :: string

        rc = zmsg_pushstr_(self, f_c_str(string))
    end function zmsg_pushstr
end module czmq_zmsg
