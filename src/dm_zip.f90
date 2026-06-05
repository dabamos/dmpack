! Author:  Philipp Engel
! Licence: ISC
module dm_zip
    !! Module to compress files with _zip(1)_ by Info-ZIP.
    use :: dm_error
    implicit none (type, external)
    private

    character(*), parameter :: ZIP_BIN = 'zip'

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
        use :: dm_file, only: dm_file_exists
        use :: dm_util, only: dm_itoa, dm_present, dm_present_set

        character(*), intent(in)            :: archive     !! Absolute path of archive.
        character(*), intent(in)            :: file        !! Path of file to compress/append to archive.
        integer,      intent(in),  optional :: compression !! Compression level (0 to 9, default is 6).
        logical,      intent(in),  optional :: junk        !! Junk the file path.
        character(*), intent(in),  optional :: cd          !! Directory to change to before compression (to add relative paths).
        integer,      intent(out), optional :: error       !! Error code.

        integer :: compression_, rc

        compression_ = max(0, min(9, dm_present(compression, 6)))

        zip_block: block
            character(:), allocatable :: cmd
            integer                   :: cmdstat, stat

            rc = E_INVALID
            if (len_trim(archive) == 0 .or. len_trim(file) == 0) exit zip_block
            if (archive(1:1) /= '/') exit zip_block

            if (present(cd)) then
                rc = E_NOT_FOUND
                if (.not. dm_file_exists(cd)) return
                cmd = 'cd ' // trim(cd) // ' && '
            else
                rc = E_INVALID
                if (file(1:1) /= '/') return
                cmd = ''
            end if

            cmd = cmd // ZIP_BIN // ' -q -X -' // dm_itoa(compression_)
            if (dm_present(junk, .false.)) cmd = cmd // ' -j'
            cmd = cmd // ' ' // trim(archive) // ' ' // trim(file)

            rc = E_EXEC
            call execute_command_line(cmd, exitstat=stat, cmdstat=cmdstat)
            if (stat /= 0 .or. cmdstat /= 0) exit zip_block

            rc = E_NONE
        end block zip_block

        call dm_present_set(error, rc)
    end subroutine dm_zip_compress
end module dm_zip
