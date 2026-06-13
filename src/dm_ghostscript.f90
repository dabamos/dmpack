! Author:  Philipp Engel
! Licence: ISC
module dm_ghostscript
    !! Wrapper module around Ghostscript, for PostScript and PDF processing.
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PRIVATE PARAMETERS
    ! **************************************************************************
    character(*), parameter :: GS_BINARY     = 'gs'     !! Name of executable.
    character(*), parameter :: PS2PDF_BINARY = 'ps2pdf' !! Part of Ghostscript package.

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_ghostscript_add_meta_data
    public :: dm_ghostscript_ps_to_pdf
    public :: dm_ghostscript_version
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS
    ! **************************************************************************
    integer function dm_ghostscript_add_meta_data(input, output, title, author, subject, creator, producer) result(rc)
        !! Reads PDF file `input`, adds meta data using _pdfmark_, and writes
        !! result to `output`. The output path must be different from the input
        !! path. The output document will be in PDF 1.4 format and printing
        !! quality. List the meta data with _pdfinfo(1)_:
        !!
        !! ```
        !! $ pdfinfo output.pdf
        !! ```
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXEC` if execution of _gs(1)_ failed.
        !! * `E_INVALID` if input file equals output file.
        !! * `E_NOT_FOUND` if input file does not exist.
        !!
        use :: dm_buffer
        use :: dm_file, only: FILE_PATH_LEN, dm_file_exists

        character(*), intent(in)           :: input    !! Path of PDF input file.
        character(*), intent(in)           :: output   !! Path of PDF output file.
        character(*), intent(in), optional :: title    !! Document title.
        character(*), intent(in), optional :: author   !! Document author.
        character(*), intent(in), optional :: subject  !! Document subject.
        character(*), intent(in), optional :: creator  !! Document creator.
        character(*), intent(in), optional :: producer !! Document producer.

        type(buffer_type) :: buffer
        integer           :: cmdstat, stat

        call dm_buffer_init(buffer, int(FILE_PATH_LEN, i8), rc)
        if (dm_is_error(rc)) return

        meta_block: block
            rc = E_NOT_FOUND
            if (.not. dm_file_exists(input)) exit meta_block

            rc = E_INVALID
            if (input == output) exit meta_block

            call dm_buffer_append(buffer, GS_BINARY,                                           rc); if (dm_is_error(rc)) exit meta_block
            call dm_buffer_append(buffer, ' -dBATCH -dNOPAUSE -dQUIET -dPDFSETTINGS=/printer', rc); if (dm_is_error(rc)) exit meta_block
            call dm_buffer_append(buffer, ' -dCompatibilityLevel=1.4 -sDEVICE=pdfwrite',       rc); if (dm_is_error(rc)) exit meta_block
            call dm_buffer_append(buffer, ' -sOutputFile=' // trim(output),                    rc); if (dm_is_error(rc)) exit meta_block
            call dm_buffer_append(buffer, ' -f ' // trim(input),                               rc); if (dm_is_error(rc)) exit meta_block
            call dm_buffer_append(buffer, ' -c "[',                                            rc); if (dm_is_error(rc)) exit meta_block

            if (present(title))    call dm_buffer_append(buffer, ' /Title ('    // trim(title)    // ')', rc); if (dm_is_error(rc)) exit meta_block
            if (present(author))   call dm_buffer_append(buffer, ' /Author ('   // trim(author)   // ')', rc); if (dm_is_error(rc)) exit meta_block
            if (present(subject))  call dm_buffer_append(buffer, ' /Subject ('  // trim(subject)  // ')', rc); if (dm_is_error(rc)) exit meta_block
            if (present(creator))  call dm_buffer_append(buffer, ' /Creator ('  // trim(creator)  // ')', rc); if (dm_is_error(rc)) exit meta_block
            if (present(producer)) call dm_buffer_append(buffer, ' /Producer (' // trim(producer) // ')', rc); if (dm_is_error(rc)) exit meta_block

            call dm_buffer_append(buffer, ' /DOCINFO pdfmark"', rc); if (dm_is_error(rc)) exit meta_block

            rc = E_EXEC
            call execute_command_line(dm_buffer_bytes(buffer), exitstat=stat, cmdstat=cmdstat)
            if (stat == 0 .and. cmdstat == 0) rc = E_NONE
        end block meta_block

        call dm_buffer_destroy(buffer)
    end function dm_ghostscript_add_meta_data

    integer function dm_ghostscript_ps_to_pdf(input, output) result(rc)
        !! Converts PostScript file `input` to PDF file `output` by executing
        !! _ps2pdf(1)_. On error, an empty PDF file may be created. This
        !! function requires Ghostscript to be installed locally.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXEC` if execution of _ps2pdf(1)_ failed.
        !! * `E_IO` if output file could not be created.
        !! * `E_NOT_FOUND` if input file does not exist.
        !!
        use :: dm_file, only: dm_file_exists, dm_file_touch

        character(*), intent(in) :: input  !! Path of PostScript file.
        character(*), intent(in) :: output !! Path of PDF file.

        integer :: cmdstat, stat

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(input)) return

        call dm_file_touch(output, error=rc)
        if (dm_is_error(rc)) return

        rc = E_EXEC
        call execute_command_line(PS2PDF_BINARY // ' ' // trim(input) // ' ' // trim(output), exitstat=stat, cmdstat=cmdstat)
        if (stat == 0 .and. cmdstat == 0) rc = E_NONE
    end function dm_ghostscript_ps_to_pdf

    function dm_ghostscript_version(name, found) result(version)
        !! Returns Ghostscript version as allocatable string.
        use :: dm_posix_pipe

        character(*), parameter :: NAME_STR = 'ghostscript'

        logical, intent(in),  optional :: name    !! Add prefix `ghostscript/`.
        logical, intent(out), optional :: found   !! Returns `.true.` if ghostscript has been found.
        character(:), allocatable      :: version !! Version string.

        character(8)          :: buffer, v
        integer               :: rc
        type(posix_pipe_type) :: pipe

        if (present(found)) found = .false.

        rc = dm_posix_pipe_open(pipe, GS_BINARY // ' --version', PIPE_RDONLY)
        v  = '0.0.0'

        if (dm_is_ok(rc)) then
            rc = dm_posix_pipe_read_line(pipe, buffer)

            if (len_trim(buffer) > 0) then
                v = buffer
                if (present(found)) found = .true.
            end if
        end if

        call dm_posix_pipe_close(pipe)

        if (dm_present(name, .false.)) then
            version = NAME_STR // '/' // trim(v)
        else
            version = trim(v)
        end if
    end function dm_ghostscript_version
end module dm_ghostscript
