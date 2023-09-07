! Author:  Philipp Engel
! Licence: ISC
module dm_file
    !! File access utility routines.
    use :: dm_error
    use :: dm_type
    implicit none (type, external)
    private

    integer, parameter, public :: FILE_PATH_LEN = 2048 !! Maximum file path length.

    public :: dm_file_exists
    public :: dm_file_delete
    public :: dm_file_line_count
    public :: dm_file_read
    public :: dm_file_size
    public :: dm_file_touch
    public :: dm_file_write
contains
    logical function dm_file_exists(path) result(file_exists)
        !! Returns `.true.` if file at given file path exists.
        character(len=*), intent(in) :: path

        inquire (exist=file_exists, file=trim(path))
    end function dm_file_exists

    integer(kind=i8) function dm_file_line_count(path, error) result(n)
        !! Returns number of lines in given file by counting new lines.
        character(len=*), intent(in)            :: path  !! File path.
        integer,          intent(out), optional :: error !! Error code.

        character :: buffer
        integer   :: fu, rc, stat

        rc = E_IO
        n  = 0_i8

        if (present(error)) error = rc

        open (action='read', file=trim(path), iostat=stat, newunit=fu, status='old')
        if (stat /= 0) return

        rc = E_EMPTY

        do
            read (fu, *, iostat=stat) buffer
            if (is_iostat_end(stat)) exit
            n = n + 1
        end do

        close (fu)

        if (n > 0) rc = E_NONE
        if (present(error)) error = rc
    end function dm_file_line_count

    integer(kind=i8) function dm_file_size(path, error) result(sz)
        !! Returns file size in file storage units (usually, bytes). On error,
        !! size is -1.
        character(len=*), intent(in)            :: path  !! Path to file.
        integer,          intent(out), optional :: error !! Error code.
        logical                                 :: file_exists

        if (present(error)) error = E_NOT_FOUND
        sz = 0_i8
        inquire (exist=file_exists, file=trim(path), size=sz)
        if (.not. file_exists) return
        if (present(error)) error = E_NONE
    end function dm_file_size

    subroutine dm_file_delete(path, error)
        !! Deletes file at given file path.
        character(len=*), intent(in)            :: path
        integer,          intent(out), optional :: error
        integer                                 :: fu, stat

        if (present(error)) error = E_IO
        open (action='write', file=trim(path), iostat=stat, newunit=fu, status='old')
        if (stat /= 0) return
        close (fu, iostat=stat, status='delete')
        if (stat == 0 .and. present(error)) error = E_NONE
    end subroutine dm_file_delete

    subroutine dm_file_touch(path, error)
        !! Creates empty file at given file path.
        character(len=*), intent(in)            :: path
        integer,          intent(out), optional :: error
        integer                                 :: fu, stat

        if (present(error)) error = E_IO
        open (action='write', file=trim(path), iostat=stat, newunit=fu, status='unknown')
        if (stat /= 0) return
        close (fu, iostat=stat)
        if (stat == 0 .and. present(error)) error = E_NONE
    end subroutine dm_file_touch

    subroutine dm_file_read(path, content, n, error)
        !! Reads file contents as byte stream into allocatable character
        !! string.
        character(len=*),              intent(in)            :: path    !! File path.
        character(len=:), allocatable, intent(out)           :: content !! Byte string.
        integer(kind=i8),              intent(out), optional :: n       !! Content size.
        integer,                       intent(out), optional :: error   !! Error code.

        integer          :: fu, rc, stat
        integer(kind=i8) :: sz

        fu = -1
        sz = -1

        read_block: block
            ! Open file for reading.
            rc = E_IO
            open (access  = 'stream', &
                  action  = 'read', &
                  file    = trim(path), &
                  form    = 'unformatted', &
                  iostat  = stat, &
                  newunit = fu)
            if (stat /= 0) exit read_block

            ! Get content size.
            inquire (unit=fu, size=sz)
            if (sz < 0) exit read_block

            ! Allocate memory.
            rc = E_ALLOC
            allocate (character(len=sz) :: content, stat=stat)
            if (stat /= 0) exit read_block

            ! Read bytes.
            if (sz > 0) then
                rc = E_READ
                read (fu, iostat=stat) content
                if (stat /= 0) exit read_block
            end if

            rc = E_NONE
        end block read_block

        if (fu > -1) close (fu)
        if (.not. allocated(content)) content = ''
        if (present(n)) n = sz
        if (present(error)) error = rc
    end subroutine dm_file_read

    subroutine dm_file_write(path, content, raw, error)
        !! Writes content to given file (ASCII or binary).
        character(len=*), intent(in)            :: path    !! Output file path.
        character(len=*), intent(in)            :: content !! Bytes to write.
        logical,          intent(in),  optional :: raw     !! Unformatted output if true.
        integer,          intent(out), optional :: error   !! Error code.

        integer :: fu, rc, stat
        logical :: raw_

        raw_ = .false.
        if (present(raw)) raw_ = raw

        fu = -1
        rc = E_IO

        write_block: block
            ! Unformatted output.
            if (raw_) then
                open (access  = 'stream', &
                      action  = 'write', &
                      file    = trim(path), &
                      form    = 'unformatted', &
                      iostat  = stat, &
                      newunit = fu, &
                      status  = 'replace')
                if (stat /= 0) exit write_block

                rc = E_WRITE
                write (fu, iostat=stat) content
                if (stat /= 0) exit write_block

                rc = E_NONE
                exit write_block
            end if

            ! Formatted output.
            open (action  = 'write', &
                  file    = trim(path), &
                  iostat  = stat, &
                  newunit = fu, &
                  status  = 'replace')
            if (stat /= 0) exit write_block

            rc = E_WRITE
            write (fu, '(a)', iostat=stat) content
            if (stat /= 0) exit write_block

            rc = E_NONE
        end block write_block

        if (fu > -1) close (fu)
        if (present(error)) error = rc
    end subroutine dm_file_write
end module dm_file
