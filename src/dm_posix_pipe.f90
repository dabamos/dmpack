! Author:  Philipp Engel
! Licence: ISC
module dm_posix_pipe
    !! Module for basic subprocess management on Unix. Procedures with
    !! name postfix `2` are for bi-directional IPC, all other for
    !! uni-directional only.
    use, intrinsic :: iso_c_binding, only: c_new_line
    use :: unix
    use :: dm_error
    use :: dm_kind
    use :: dm_util, only: dm_present, dm_present_set
    implicit none (type, external)
    private

    integer, parameter, public :: PIPE_NONE   = 0 !! No access.
    integer, parameter, public :: PIPE_RDONLY = 1 !! Read-only access.
    integer, parameter, public :: PIPE_WRONLY = 2 !! Write-only access.

    type, public :: posix_pipe_type
        !! Opaque pipe type. Stores the C pointer of uni-directional pipe.
        private
        integer     :: access = PIPE_NONE  !! `PIPE_RDONLY` or `PIPE_WRONLY`.
        type(c_ptr) :: fp     = c_null_ptr !! File pointer.
    end type posix_pipe_type

    public :: dm_posix_pipe_access
    public :: dm_posix_pipe_close
    public :: dm_posix_pipe_close2
    public :: dm_posix_pipe_execute
    public :: dm_posix_pipe_is_connected
    public :: dm_posix_pipe_open
    public :: dm_posix_pipe_open2
    public :: dm_posix_pipe_read
    public :: dm_posix_pipe_read_line
    public :: dm_posix_pipe_write
    public :: dm_posix_pipe_write2
contains
    pure integer function dm_posix_pipe_access(pipe) result(access)
        !! Returns access type of pipe (`PIPE_NONE`, `PIPE_RDONLY`, or
        !! `PIPE_WRONLY`).
        type(posix_pipe_type), intent(in) :: pipe !! Pipe.

        access = pipe%access
    end function dm_posix_pipe_access

    pure logical function dm_posix_pipe_is_connected(pipe) result(connected)
        !! Returns `.true.` if pipe is connected.
        type(posix_pipe_type), intent(in) :: pipe !! Pipe.

        connected = c_associated(pipe%fp)
    end function dm_posix_pipe_is_connected

    integer function dm_posix_pipe_execute(command, bytes, nbytes) result(rc)
        !! Utility function that reads output from pipe. The output must be at
        !! least the length of the expected output + 1, due to the returned
        !! null-termination. The null character at the end will be removed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(*), intent(in)            :: command !! Command.
        character(*), intent(inout)         :: bytes   !! Output string.
        integer(i8),  intent(out), optional :: nbytes  !! String length.

        type(posix_pipe_type) :: pipe

        call dm_present_set(nbytes, 0_i8)

        rc = dm_posix_pipe_open(pipe, command, PIPE_RDONLY)
        if (dm_is_error(rc)) return

        rc = dm_posix_pipe_read(pipe, bytes, nbytes)
        call dm_posix_pipe_close(pipe)
    end function dm_posix_pipe_execute

    integer function dm_posix_pipe_open(pipe, command, access) result(rc)
        !! Opens a process by creating a pipe, forking, and invoking the shell.
        !! Access mode has to be either `PIPE_RDONLY` or `PIPE_WRONLY`. The
        !! command string will not be trimmed by this function.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if pipe is already connected.
        !! * `E_INVALID` if access mode is invalid.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_c, only: dm_f_c_string

        type(posix_pipe_type), intent(inout) :: pipe    !! Pipe.
        character(*),          intent(in)    :: command !! Name or path of binary to open.
        integer,               intent(in)    :: access  !! Open pipe for reading or writing.

        character :: a

        rc = E_EXIST
        if (dm_posix_pipe_is_connected(pipe)) return

        rc = E_INVALID
        select case (access)
            case (PIPE_RDONLY); a = 'r'
            case (PIPE_WRONLY); a = 'w'
            case default;       return
        end select

        rc = E_SYSTEM
        pipe%fp = c_popen(dm_f_c_string(command), dm_f_c_string(a))
        if (.not. c_associated(pipe%fp)) return

        rc = E_NONE
        pipe%access = access
    end function dm_posix_pipe_open

    integer function dm_posix_pipe_open2(stdin, stdout, stderr, command) result(rc)
        !! Creates three anonymous pipes for bidirectional IPC (`stdin`,
        !! `stdout`, `stderr`).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if one or more pipe is already conncted.
        !! * `E_SYSTEM` if opening pipes failed.
        !!
        use :: dm_c, only: dm_f_c_string

        type(posix_pipe_type), intent(out) :: stdin   !! Standard input handle.
        type(posix_pipe_type), intent(out) :: stdout  !! Standard output handle.
        type(posix_pipe_type), intent(out) :: stderr  !! Standard error handle.
        character(*),          intent(in)  :: command !! Program to invoke.

        integer :: p1(2), p2(2), p3(2), pid, stat

        rc = E_EXIST
        if (dm_posix_pipe_is_connected(stdin))  return
        if (dm_posix_pipe_is_connected(stdout)) return
        if (dm_posix_pipe_is_connected(stderr)) return

        rc = E_SYSTEM

        stdin%access  = PIPE_WRONLY
        stdout%access = PIPE_RDONLY
        stderr%access = PIPE_RDONLY

        stat = c_pipe(p1)
        stat = c_pipe(p2)
        stat = c_pipe(p3)

        pid = c_fork()

        if (pid < 0) then
            ! Fork error.
            return
        else if (pid > 0) then
            ! Parent process.
            stat = c_close(p1(1))
            stat = c_close(p2(2))
            stat = c_close(p3(2))

            stdin%fp  = c_fdopen(p1(2), dm_f_c_string('w'))
            stdout%fp = c_fdopen(p2(1), dm_f_c_string('r'))
            stderr%fp = c_fdopen(p3(1), dm_f_c_string('r'))

            if (.not. dm_posix_pipe_is_connected(stdin))  return
            if (.not. dm_posix_pipe_is_connected(stdout)) return
            if (.not. dm_posix_pipe_is_connected(stderr)) return

            rc = E_NONE
            return
        else if (pid == 0) then
            ! Child process.
            stat = c_close(p1(2))
            stat = c_close(p2(1))
            stat = c_close(p3(1))

            stat = c_dup2(p1(1), STDIN_FILENO)
            stat = c_dup2(p2(2), STDOUT_FILENO)
            stat = c_dup2(p3(2), STDERR_FILENO)

            stat = c_execl(dm_f_c_string('/bin/sh'), &
                           dm_f_c_string('/bin/sh'), &
                           dm_f_c_string('-c'),      &
                           dm_f_c_string(command),   &
                           c_null_ptr)

            call c_exit(EXIT_SUCCESS)
        end if
    end function dm_posix_pipe_open2

    integer function dm_posix_pipe_read(pipe, bytes, nbytes) result(rc)
        !! Reads from pipe to buffer `output` (binary) and returns number of
        !! bytes read from buffer.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the pipe is not connected or write-only.
        !! * `E_READ` if no bytes were returned.
        !!
        type(posix_pipe_type), intent(inout)         :: pipe   !! Bi-directional pipe.
        character(*), target,  intent(inout)         :: bytes  !! Output buffer.
        integer(i8),           intent(out), optional :: nbytes !! Bytes read.

        integer(i8) :: nbytes_

        call dm_present_set(nbytes, 0_i8)
        bytes = ' '

        rc = E_INVALID
        if (pipe%access == PIPE_WRONLY) return
        if (.not. dm_posix_pipe_is_connected(pipe)) return

        rc = E_READ
        nbytes_ = c_fread(c_loc(bytes), 1_c_size_t, len(bytes, c_size_t), pipe%fp)
        if (nbytes_ < 0) return

        rc = E_NONE
        call dm_present_set(nbytes, nbytes_)
    end function dm_posix_pipe_read

    integer function dm_posix_pipe_read_line(pipe, bytes, nbytes) result(rc)
        !! Reads line string from pipe to buffer `bytes` and removes new-line
        !! and null-termination.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if pipe is not connected or write-only.
        !! * `E_READ` if reading from pipe failed.
        !!
        type(posix_pipe_type), intent(inout)         :: pipe   !! Bi-directional pipe.
        character(*),          intent(inout)         :: bytes  !! Output buffer.
        integer,               intent(out), optional :: nbytes !! Bytes read.

        integer     :: i
        type(c_ptr) :: ptr

        call dm_present_set(nbytes, 0)
        bytes = ' '

        rc = E_INVALID
        if (pipe%access == PIPE_WRONLY) return
        if (.not. dm_posix_pipe_is_connected(pipe)) return

        rc = E_READ
        ptr = c_fgets(bytes, len(bytes, c_int), pipe%fp)
        if (.not. c_associated(ptr)) return

        ! Remove new-line and null-termination.
        i = index(bytes, c_null_char)
        if (i == 0) return

        rc = E_NONE
        if (bytes(i - 1:i - 1) == c_new_line) i = i - 1
        bytes(i:min(len(bytes), i + 1)) = ' '
        call dm_present_set(nbytes, i)
    end function dm_posix_pipe_read_line

    integer function dm_posix_pipe_write(pipe, bytes, newline) result(rc)
        !! Writes bytes to pipe and adds new-line character if `newline` is not
        !! `.false.`. The input string will not be trimmed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if pipe is not connected or read-only.
        !! * `E_WRITE` if writing failed.
        !!
        type(posix_pipe_type), intent(inout)        :: pipe    !! Pipe.
        character(*),          intent(in)           :: bytes   !! Bytes to write to the pipe.
        logical,               intent(in), optional :: newline !! Add new-line character.

        integer :: stat

        rc = E_INVALID
        if (pipe%access == PIPE_RDONLY) return
        if (.not. dm_posix_pipe_is_connected(pipe)) return

        rc = E_NONE
        if (dm_present(newline, .true.)) then
            stat = c_fputs(bytes // c_new_line // c_null_char, pipe%fp)
        else
            stat = c_fputs(bytes // c_null_char, pipe%fp)
        end if
        if (stat < 0) rc = E_WRITE
    end function dm_posix_pipe_write

    integer function dm_posix_pipe_write2(pipe, bytes, nbytes) result(rc)
        !! Writes to pipe (binary) and returns the number of bytes written in
        !! `n`. The input string is not trimmed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if pipe is not connected or read-only.
        !! * `E_WRITE` if writing failed.
        !!
        type(posix_pipe_type), intent(inout)         :: pipe   !! Bi-directional pipe.
        character(*), target,  intent(in)            :: bytes  !! Bytes to write to the pipe.
        integer(i8),           intent(out), optional :: nbytes !! Bytes written.

        integer(i8) :: nbytes_

        call dm_present_set(nbytes, 0_i8)

        rc = E_INVALID
        if (pipe%access == PIPE_RDONLY) return
        if (.not. dm_posix_pipe_is_connected(pipe)) return

        rc = E_WRITE
        nbytes_ = c_fwrite(c_loc(bytes), 1_c_size_t, len(bytes, c_size_t), pipe%fp)
        if (nbytes_ < 0) return

        rc = E_NONE
        call dm_present_set(nbytes, nbytes_)
    end function dm_posix_pipe_write2

    subroutine dm_posix_pipe_close(pipe, exit_stat)
        !! Closes pipe to process.
        type(posix_pipe_type), intent(inout)         :: pipe      !! Pipe.
        integer,               intent(out), optional :: exit_stat !! Exit status.

        integer :: stat

        if (present(exit_stat)) exit_stat = 0
        if (.not. dm_posix_pipe_is_connected(pipe)) return
        stat = c_pclose(pipe%fp)
        pipe%fp = c_null_ptr
        if (present(exit_stat)) exit_stat = stat / 256
    end subroutine dm_posix_pipe_close

    subroutine dm_posix_pipe_close2(pipe, exit_stat)
        !! Closes pipe to process (binary).
        type(posix_pipe_type), intent(inout)         :: pipe      !! Pipe.
        integer,               intent(out), optional :: exit_stat !! Exit status.

        integer :: stat

        if (present(exit_stat)) exit_stat = 0
        if (.not. dm_posix_pipe_is_connected(pipe)) return
        stat = c_fclose(pipe%fp)
        pipe%fp = c_null_ptr
        if (present(exit_stat)) exit_stat = stat
    end subroutine dm_posix_pipe_close2
end module dm_posix_pipe
