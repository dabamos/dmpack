! Author:  Philipp Engel
! Licence: ISC
module dm_zip
    !! Module to compress files with _zip(1)_ by Info-ZIP.
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PRIVATE PARAMETERS
    ! **************************************************************************
    character(*), parameter :: ZIP_BINARY = 'zip' !! Name of executable.

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_zip_compress
contains
    subroutine dm_zip_compress(archive, file, compression, junk, cd, error)
        !! Adds file `file` to ZIP archive `archive` with optional compression.
        !! The argument `archive` must be an absolute path. If argument
        !! `compression` is 0, no compression will be applied for _this_ file.
        !!
        !! Make sure to only pass sanitised or parameterised arguments to this
        !! subroutine as shell injections are possible otherwise.
        !!
        !! Compress file `dummy.txt` to archive `/tmp/dummy.zip`:
        !!
        !! ``` fortran
        !! call dm_zip_compress('/tmp/dummy.zip', '/tmp/dummy.txt', 9, junk=.true.)
        !! ```
        !!
        !! If argument `junk` is `.false.`, the directory `tmp/` will be added
        !! as well (i.e., the file in the archive will be `tmp/dummy.txt`).
        !!
        !! Add file `dummy/dummy.txt` (incl. the relative directory) to the
        !! archive:
        !!
        !! ``` fortran
        !! call dm_zip_compress('/tmp/dummy.zip', 'dummy/dummy.txt', 9, cd='/tmp')
        !! ```
        !!
        !! The subroutine returns the following error codes in `error`:
        !!
        !! * `E_EXEC` if execution of _zip(1)_ failed.
        !! * `E_INVALID` if `archive` or `file` is not an absolute path.
        !! * `E_NOT_FOUND` if path `cd` does not exist.
        !!
        use :: dm_buffer
        use :: dm_file, only: FILE_PATH_LEN, dm_file_exists
        use :: dm_util, only: dm_itoa, dm_present, dm_present_set

        character(*), intent(in)            :: archive     !! Absolute path of archive.
        character(*), intent(in)            :: file        !! Path of file to compress/append to archive.
        integer,      intent(in),  optional :: compression !! Compression level (0 to 9, default is 6).
        logical,      intent(in),  optional :: junk        !! Junk the file path.
        character(*), intent(in),  optional :: cd          !! Directory to change to before compression (to add relative paths).
        integer,      intent(out), optional :: error       !! Error code.

        integer           :: compression_, rc
        type(buffer_type) :: buffer

        compression_ = max(0, min(9, dm_present(compression, 6)))

        zip_block: block
            integer :: cmdstat, stat

            call dm_buffer_init(buffer, int(FILE_PATH_LEN, i8), rc)
            if (dm_is_error(rc)) exit zip_block

            rc = E_INVALID
            if (len_trim(archive) == 0 .or. len_trim(file) == 0) exit zip_block
            if (archive(1:1) /= '/') exit zip_block

            if (present(cd)) then
                rc = E_NOT_FOUND
                if (.not. dm_file_exists(cd)) exit zip_block

                call dm_buffer_append(buffer, 'cd ' // trim(cd), rc); if (dm_is_error(rc)) exit zip_block
                call dm_buffer_append(buffer, ' && ',            rc); if (dm_is_error(rc)) exit zip_block
            else
                rc = E_INVALID
                if (file(1:1) /= '/') exit zip_block
            end if

            call dm_buffer_append(buffer, ZIP_BINARY,            rc); if (dm_is_error(rc)) exit zip_block
            call dm_buffer_append(buffer, ' -q -X -',            rc); if (dm_is_error(rc)) exit zip_block
            call dm_buffer_append(buffer, dm_itoa(compression_), rc); if (dm_is_error(rc)) exit zip_block

            if (dm_present(junk, .false.)) then
                call dm_buffer_append(buffer, ' -j', rc); if (dm_is_error(rc)) exit zip_block
            end if

            call dm_buffer_append(buffer, ' ' // trim(archive), rc); if (dm_is_error(rc)) exit zip_block
            call dm_buffer_append(buffer, ' ' // trim(file),    rc); if (dm_is_error(rc)) exit zip_block

            rc = E_EXEC
            call execute_command_line(dm_buffer_bytes(buffer), exitstat=stat, cmdstat=cmdstat)
            if (stat == 0 .and. cmdstat == 0) rc = E_NONE
        end block zip_block

        call dm_buffer_destroy(buffer)
        call dm_present_set(error, rc)
    end subroutine dm_zip_compress
end module dm_zip
