! Author:  Philipp Engel
! Licence: ISC
module dm_ghostscript
    !! Wrapper module around Ghostscript, for PostScript and PDF processing.
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! Executables.
    character(*), parameter :: GS_BINARY     = 'gs'
    character(*), parameter :: PS2PDF_BINARY = 'ps2pdf'

    ! Public procedures.
    public :: dm_ghostscript_ps_to_pdf
    public :: dm_ghostscript_set_pdf_meta
    public :: dm_ghostscript_version
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_ghostscript_ps_to_pdf(input, output) result(rc)
        !! Converts PostScript file `input` to PDF file `output` by executing
        !! _ps2pdf(1)_. On error, an empty PDF file may be created. This
        !! function requires Ghostscript to be installed locally.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ERROR` if execution of _ps2pdf(1)_ failed.
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

        rc = E_ERROR
        call execute_command_line(PS2PDF_BINARY // ' ' // trim(input) // ' ' // trim(output), exitstat=stat, cmdstat=cmdstat)
        if (stat == 0 .and. cmdstat == 0) rc = E_NONE
    end function dm_ghostscript_ps_to_pdf

    integer function dm_ghostscript_set_pdf_meta(input, output, title, author, subject, creator, producer) result(rc)
        !! Reads PDF file `input`, adds meta data using pdfmark, and writes
        !! result to `output`. The output path must be different from the input
        !! path. The output document will be a PDF 1.4 format in printing
        !! quality.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ERROR` if execution of _gs(1)_ failed.
        !! * `E_INVALID` if input file equals output file.
        !! * `E_NOT_FOUND` if input file does not exist.
        !!
        use :: dm_file, only: dm_file_exists

        character(*), intent(in)           :: input    !! Path of PDF input file.
        character(*), intent(in)           :: output   !! Path of PDF output file.
        character(*), intent(in), optional :: title    !! Document title.
        character(*), intent(in), optional :: author   !! Document author.
        character(*), intent(in), optional :: subject  !! Document subject.
        character(*), intent(in), optional :: creator  !! Document creator.
        character(*), intent(in), optional :: producer !! Document producer.

        character(2048) :: command
        integer         :: cmdstat, stat

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(input)) return

        rc = E_INVALID
        if (input == output) return

        rc = E_ERROR
        command = ' /DOCINFO pdfmark"'

        if (present(title))    command = ' /Title ('    // trim(title)    // ')' // trim(command)
        if (present(author))   command = ' /Author ('   // trim(author)   // ')' // trim(command)
        if (present(subject))  command = ' /Subject ('  // trim(subject)  // ')' // trim(command)
        if (present(creator))  command = ' /Creator ('  // trim(creator)  // ')' // trim(command)
        if (present(producer)) command = ' /Producer (' // trim(producer) // ')' // trim(command)

        command = GS_BINARY // ' -dBATCH -dNOPAUSE -dQUIET -dPDFSETTINGS=/printer -dCompatibilityLevel=1.4' // &
                               ' -sDEVICE=pdfwrite -sOutputFile=' // trim(output) // ' -f ' // trim(input) // &
                               ' -c "[' // trim(command)

        call execute_command_line(trim(command), exitstat=stat, cmdstat=cmdstat)
        if (stat == 0 .and. cmdstat == 0) rc = E_NONE
    end function dm_ghostscript_set_pdf_meta

    function dm_ghostscript_version(name, found) result(version)
        !! Returns Ghostscript version as allocatable string.
        use :: dm_pipe

        character(*), parameter :: NAME_STR = 'ghostscript'

        logical, intent(in),  optional :: name    !! Add prefix `ghostscript/`.
        logical, intent(out), optional :: found   !! Returns `.true.` if ghostscript has been found.
        character(:), allocatable      :: version !! Version string.

        character(8)    :: buffer, v
        integer         :: stat, rc
        type(pipe_type) :: pipe

        if (present(found)) found = .false.

        rc = dm_pipe_open(pipe, GS_BINARY // ' --version', PIPE_RDONLY)
        v  = '0.0.0'

        if (dm_is_ok(rc)) then
            rc = dm_pipe_read_line(pipe, buffer)

            if (len_trim(buffer) > 0) then
                v = buffer
                if (present(found)) found = .true.
            end if
        end if

        call dm_pipe_close(pipe)

        if (dm_present(name, .false.)) then
            version = NAME_STR // '/' // trim(v)
        else
            version = trim(v)
        end if
    end function dm_ghostscript_version
end module dm_ghostscript
