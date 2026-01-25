! Author:  Philipp Engel
! Licence: ISC
module dm_posix_fifo
    !! Abstraction layer for named pipe (FIFO) access on Unix.
    use :: unix
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: POSIX_FIFO_PERM = int(o'0644') !! Default permissions.

    type, public :: posix_fifo_type
        !! Opaque named pipe (FIFO) type.
        private
        character(:), allocatable :: path                     !! Path to named pipe.
        integer                   :: perm   = POSIX_FIFO_PERM !! Access permissions.
        integer                   :: fd     = -1              !! File descriptor.
        type(c_ptr)               :: stream = c_null_ptr      !! Stream pointer.
    end type posix_fifo_type

    public :: dm_posix_fifo_close
    public :: dm_posix_fifo_create
    public :: dm_posix_fifo_open
    public :: dm_posix_fifo_read
contains
    integer(i8) function dm_posix_fifo_read(fifo, bytes, error) result(nbytes)
        !! Reads from named pipe and returns number of bytes read. The read
        !! string is returned in `bytes`. Argument `error` is set to
        !! `E_NULL` if `fifo` is not connected.
        type(posix_fifo_type),     intent(inout)         :: fifo  !! FIFO type.
        character(:), allocatable, intent(out)           :: bytes !! Bytes read from FIFO.
        integer,                   intent(out), optional :: error !! Error code.

        integer :: i, rc

        nbytes = 0_i8
        bytes  = ''

        fifo_block: block
            rc = E_NULL
            if (.not. c_associated(fifo%stream)) exit fifo_block

            rc = E_NONE
            do
                i = c_fgetc(fifo%stream)
                if (i == EOF) exit
                bytes  = bytes // achar(i)
                nbytes = nbytes + 1
            end do
        end block fifo_block

        if (present(error)) error = rc
    end function dm_posix_fifo_read

    subroutine dm_posix_fifo_close(fifo)
        !! Closes named pipe.
        type(posix_fifo_type), intent(inout) :: fifo !! FIFO type.

        integer :: stat

        if (allocated(fifo%path)) deallocate (fifo%path)

        if (c_associated(fifo%stream)) stat = c_fclose(fifo%stream)
        fifo%stream = c_null_ptr

        if (fifo%fd > -1) stat = c_close(fifo%fd)
        fifo%fd = -1
    end subroutine dm_posix_fifo_close

    subroutine dm_posix_fifo_create(fifo, file_path, perm, error)
        !! Creates new named pipe.
        !!
        !! The routine returns the following error codes in `error`:
        !!
        !! * `E_EXIST` if the FIFO already exists.
        !! * `E_IO` if the system call to create the FIFO failed.
        !!
        use :: dm_c, only: dm_f_c_string

        type(posix_fifo_type), intent(inout)         :: fifo      !! FIFO type.
        character(*),          intent(in)            :: file_path !! Path of FIFO.
        integer,               intent(in),  optional :: perm      !! File permissions.
        integer,               intent(out), optional :: error     !! Error code.

        integer :: rc, stat

        fifo_block: block
            rc = E_EXIST
            if (dm_file_exists(file_path)) exit fifo_block

            fifo%path = file_path
            if (present(perm)) fifo%perm = perm

            rc = E_IO
            stat = c_mkfifo(dm_f_c_string(fifo%path), int(fifo%perm, kind=c_mode_t))
            if (stat < 0) exit fifo_block
            rc = E_NONE
        end block fifo_block

        if (present(error)) error = rc
    end subroutine dm_posix_fifo_create

    subroutine dm_posix_fifo_open(fifo, error)
        !! Opens named pipe.
        !!
        !! The routine returns the following error codes in `error`:
        !!
        !! * `E_INVALID` if the FIFO is already opened, or the path is invalid.
        !! * `E_IO` if the system call to open the FIFO failed.
        !!
        use :: dm_c, only: dm_f_c_string

        type(posix_fifo_type), intent(inout)         :: fifo  !! FIFO type.
        integer,               intent(out), optional :: error !! Error code.

        integer :: rc

        fifo_block: block
            rc = E_INVALID
            if (fifo%fd > -1)               exit fifo_block
            if (c_associated(fifo%stream))  exit fifo_block
            if (.not. allocated(fifo%path)) exit fifo_block
            if (len_trim(fifo%path) == 0)   exit fifo_block

            rc = E_IO
            fifo%fd = c_open(dm_f_c_string(fifo%path), O_RDONLY, int(S_IRUSR, kind=c_mode_t))
            if (fifo%fd < 0) exit fifo_block

            fifo%stream = c_fdopen(fifo%fd, dm_f_c_string('r'))
            if (.not. c_associated(fifo%stream)) exit fifo_block
            rc = E_NONE
        end block fifo_block

        if (present(error)) error = rc
    end subroutine dm_posix_fifo_open
end module dm_posix_fifo
