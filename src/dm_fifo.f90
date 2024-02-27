! Author:  Philipp Engel
! Licence: ISC
module dm_fifo
    !! Abstraction layer for named pipe (FIFO) access on Unix.
    use, intrinsic :: iso_c_binding
    use :: unix
    use :: dm_error
    use :: dm_file
    implicit none (type, external)
    private

    integer, parameter, public :: FIFO_PERM = int(o'0644') !! Default permissions.

    type, public :: fifo_type
        !! Opaque named pipe (FIFO) type.
        private
        character(len=:), allocatable :: path                !! Path to named pipe.
        integer                       :: perm   = FIFO_PERM  !! Access permissions.
        integer                       :: fd     = -1         !! File descriptor.
        type(c_ptr)                   :: stream = c_null_ptr !! Stream pointer.
    end type fifo_type

    public :: dm_fifo_close
    public :: dm_fifo_create
    public :: dm_fifo_open
    public :: dm_fifo_read
contains
    integer function dm_fifo_read(fifo, str, error) result(n)
        !! Reads from named pipe and returns number of lines read. The read
        !! string is returned in `str`. Argument `error` is set to `E_INVALID`
        !! if `fifo` is not connected.
        type(fifo_type),               intent(inout)         :: fifo  !! FIFO type.
        character(len=:), allocatable, intent(out)           :: str   !! Result.
        integer,                       intent(out), optional :: error !! Error code.

        integer :: i

        n   = 0
        str = ''

        if (present(error)) error = E_INVALID
        if (.not. c_associated(fifo%stream)) return

        do
            i = c_fgetc(fifo%stream)
            if (i == EOF) exit
            str = str // achar(i)
            n = i
        end do

        if (present(error)) error = E_NONE
    end function dm_fifo_read

    subroutine dm_fifo_close(fifo)
        !! Closes named pipe.
        type(fifo_type), intent(inout) :: fifo !! FIFO type.
        integer                        :: rc

        if (allocated(fifo%path)) deallocate (fifo%path)

        if (c_associated(fifo%stream)) rc = c_fclose(fifo%stream)
        fifo%stream = c_null_ptr

        if (fifo%fd > -1) rc = c_close(fifo%fd)
        fifo%fd = -1
    end subroutine dm_fifo_close

    subroutine dm_fifo_create(fifo, file_path, perm, error)
        !! Creates new named pipe.
        type(fifo_type),  intent(inout)         :: fifo      !! FIFO type.
        character(len=*), intent(in)            :: file_path !! Path of FIFO.
        integer,          intent(in),  optional :: perm      !! File permissions.
        integer,          intent(out), optional :: error      !! Error code.
        integer                                 :: rc

        if (present(error)) error = E_EXIST
        if (dm_file_exists(file_path)) return

        fifo%path = file_path
        if (present(perm)) fifo%perm = perm

        if (present(error)) error = E_IO
        rc = c_mkfifo(trim(fifo%path) // c_null_char, int(fifo%perm, kind=c_mode_t))
        if (rc < 0) return
        if (present(error)) error = E_NONE
    end subroutine dm_fifo_create

    subroutine dm_fifo_open(fifo, error)
        !! Opens named pipe.
        type(fifo_type),  intent(inout)         :: fifo  !! FIFO type.
        integer,          intent(out), optional :: error !! Error code.

        if (present(error)) error = E_IO
        if (fifo%fd > -1) return
        if (c_associated(fifo%stream)) return
        if (.not. allocated(fifo%path)) return
        if (len_trim(fifo%path) == 0) return

        fifo%fd = c_open(fifo%path // c_null_char, O_RDONLY, int(S_IRUSR, kind=c_mode_t))
        if (fifo%fd < 0) return
        fifo%stream = c_fdopen(fifo%fd, 'r' // c_null_char)
        if (.not. c_associated(fifo%stream)) return
        if (present(error)) error = E_NONE
    end subroutine dm_fifo_open
end module dm_fifo
