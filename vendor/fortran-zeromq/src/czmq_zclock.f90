! czmq_zclock.f90
!
! Author:  Philipp Engel
! Licence: ISC
module czmq_zclock
    !! Auto-generated Fortran 2018 interface bindings to libczmq 4.
    use :: zmq_util
    implicit none (type, external)
    private

    public :: zclock_mono
    public :: zclock_sleep
    public :: zclock_time
    public :: zclock_timestr
    public :: zclock_timestr_
    public :: zclock_usecs

    interface
        ! int64_t zclock_mono(void)
        function zclock_mono() bind(c, name='zclock_mono')
            import :: c_int64_t
            implicit none
            integer(c_int64_t) :: zclock_mono
        end function zclock_mono

        ! void zclock_sleep(int msecs)
        subroutine zclock_sleep(msecs) bind(c, name='zclock_sleep')
            import :: c_int
            implicit none
            integer(c_int), intent(in), value :: msecs
        end subroutine zclock_sleep

        ! int64_t zclock_time(void)
        function zclock_time() bind(c, name='zclock_time')
            import :: c_int64_t
            implicit none
            integer(c_int64_t) :: zclock_time
        end function zclock_time

        ! char *zclock_timestr(void)
        function zclock_timestr_() bind(c, name='zclock_timestr')
            import :: c_ptr
            implicit none
            type(c_ptr) :: zclock_timestr_
        end function zclock_timestr_

        ! int64_t zclock_usecs(void)
        function zclock_usecs() bind(c, name='zclock_usecs')
            import :: c_int64_t
            implicit none
            integer(c_int64_t) :: zclock_usecs
        end function zclock_usecs
    end interface
contains
    ! char *zclock_timestr(void)
    function zclock_timestr() result(str)
        character(:), allocatable :: str

        type(c_ptr) :: ptr

        ptr = zclock_timestr_()
        call c_f_str_ptr(ptr, str)
        call c_free(ptr)
    end function zclock_timestr
end module czmq_zclock
