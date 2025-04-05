! Author:  Philipp Engel
! Licence: ISC
module dm_pipe
    !! Module for basic subprocess management on Unix. Procedures with
    !! name postfix `2` are for bi-directional IPC, all other for
    !! uni-directional only.
    use :: unix
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: PIPE_RDONLY = 1 !! Read-only access.
    integer, parameter, public :: PIPE_WRONLY = 2 !! Write-only access.

    type, public :: pipe_type
        !! Opaque pipe type. Stores the C pointer of uni-directional pipe.
        private
        integer     :: access = 0          !! `PIPE_RDONLY` or `PIPE_WRONLY`.
        type(c_ptr) :: fp     = c_null_ptr !! File pointer.
    end type pipe_type

    public :: dm_pipe_close
    public :: dm_pipe_close2
    public :: dm_pipe_execute
    public :: dm_pipe_is_connected
    public :: dm_pipe_open
    public :: dm_pipe_open2
    public :: dm_pipe_read
    public :: dm_pipe_read_line
    public :: dm_pipe_write
    public :: dm_pipe_write2
contains
    logical function dm_pipe_is_connected(pipe) result(connected)
        !! Returns `.true.` if pipe is connected.
        type(pipe_type), intent(inout) :: pipe !! Pipe type.

        connected = c_associated(pipe%fp)
    end function dm_pipe_is_connected

    integer function dm_pipe_execute(command, output, n) result(rc)
        !! Utility function that reads output from pipe. The output must be at
        !! least the length of the expected output + 1, due to the returned
        !! null-termination. The null character at the end will be removed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)            :: command !! Command.
        character(len=*), intent(inout)         :: output  !! Output string.
        integer(kind=i8), intent(out), optional :: n       !! String length.

        type(pipe_type) :: pipe

        if (present(n)) n = 0_i8

        rc = dm_pipe_open(pipe, command, PIPE_RDONLY)
        if (dm_is_error(rc)) return

        rc = dm_pipe_read(pipe, output, n)
        call dm_pipe_close(pipe)
    end function dm_pipe_execute

    integer function dm_pipe_open(pipe, command, access) result(rc)
        !! Opens a process by creating a pipe, forking, and invoking the shell.
        !! Access mode has to be either `PIPE_RDONLY` or `PIPE_WRONLY`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if pipe is already connected.
        !! * `E_INVALID` if access mode is invalid.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_c, only: dm_f_c_string

        type(pipe_type),  intent(inout) :: pipe    !! Pipe type.
        character(len=*), intent(in)    :: command !! Name or path of binary to open.
        integer,          intent(in)    :: access  !! Open pipe for reading or writing.

        character :: a

        rc = E_EXIST
        if (dm_pipe_is_connected(pipe)) return

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
    end function dm_pipe_open

    integer function dm_pipe_open2(stdin, stdout, stderr, command) result(rc)
        !! Creates three anonymous pipes for bidirectional IPC (`stdin`,
        !! `stdout`, `stderr`).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if one or more pipe is already conncted.
        !! * `E_SYSTEM` if opening pipes failed.
        !!
        use :: dm_c, only: dm_f_c_string

        type(pipe_type),  intent(out) :: stdin   !! Standard input handle.
        type(pipe_type),  intent(out) :: stdout  !! Standard output handle.
        type(pipe_type),  intent(out) :: stderr  !! Standard error handle.
        character(len=*), intent(in)  :: command !! Program to invoke.

        integer :: p1(2), p2(2), p3(2), pid, stat

        rc = E_EXIST
        if (dm_pipe_is_connected(stdin))  return
        if (dm_pipe_is_connected(stdout)) return
        if (dm_pipe_is_connected(stderr)) return

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

            if (.not. dm_pipe_is_connected(stdin))  return
            if (.not. dm_pipe_is_connected(stdout)) return
            if (.not. dm_pipe_is_connected(stderr)) return

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
    end function dm_pipe_open2

    integer function dm_pipe_read(pipe, output, n) result(rc)
        !! Reads from pipe to buffer `output` (binary) and returns number of
        !! bytes read from buffer.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the pipe is not connected or write-only.
        !! * `E_READ` if no bytes were returned.
        !!
        type(pipe_type),          intent(inout)         :: pipe   !! Bi-directional pipe.
        character(len=*), target, intent(inout)         :: output !! Output buffer.
        integer(kind=i8),         intent(out), optional :: n      !! Bytes read.

        integer(kind=i8) :: nbyte

        output = ' '
        if (present(n)) n = 0_i8

        rc = E_INVALID
        if (pipe%access == PIPE_WRONLY) return
        if (.not. dm_pipe_is_connected(pipe)) return

        nbyte = c_fread(c_loc(output), 1_c_size_t, len(output, kind=c_size_t), pipe%fp)

        if (present(n)) n = nbyte
        rc = E_NONE
    end function dm_pipe_read

    integer function dm_pipe_read_line(pipe, output, n) result(rc)
        !! Reads line string from pipe to buffer `output` and removes new-line
        !! and null-termination.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if pipe is not connected or write-only.
        !! * `E_READ` if reading from pipe failed.
        !!
        type(pipe_type),  intent(inout)         :: pipe   !! Bi-directional pipe.
        character(len=*), intent(inout)         :: output !! Output buffer.
        integer,          intent(out), optional :: n      !! Bytes read.

        integer     :: i
        type(c_ptr) :: ptr

        if (present(n)) n = 0_i8
        output = ' '

        rc = E_INVALID
        if (pipe%access == PIPE_WRONLY) return
        if (.not. dm_pipe_is_connected(pipe)) return

        rc = E_READ
        ptr = c_fgets(output, len(output, kind=c_int), pipe%fp)
        if (.not. c_associated(ptr)) return

        ! Remove new-line and null-termination.
        i = index(output, c_null_char)
        if (i == 0) return

        if (output(i - 1:i - 1) == c_new_line) i = i - 1
        output(i:min(len(output), i + 1)) = ' '
        if (present(n)) n = i
        rc = E_NONE
    end function dm_pipe_read_line

    integer function dm_pipe_write(pipe, input) result(rc)
        !! Writes bytes to pipe, adds new line and null-termination. The input
        !! string is not trimmed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if pipe is not connected or read-only.
        !! * `E_WRITE` if writing failed.
        !!
        type(pipe_type),  intent(inout) :: pipe  !! Pipe type.
        character(len=*), intent(in)    :: input !! Bytes to write to the pipe.

        rc = E_INVALID
        if (pipe%access == PIPE_RDONLY) return
        if (.not. dm_pipe_is_connected(pipe)) return

        rc = E_WRITE
        if (c_fputs(input // c_new_line // c_null_char, pipe%fp) < 0) return

        rc = E_NONE
    end function dm_pipe_write

    integer function dm_pipe_write2(pipe, input, n) result(rc)
        !! Writes to pipe (binary) and returns the number of bytes written in
        !! `n`. The input string is not trimmed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if pipe is not connected or read-only.
        !! * `E_WRITE` if writing failed.
        !!
        type(pipe_type),          intent(inout)         :: pipe  !! Bi-directional pipe.
        character(len=*), target, intent(in)            :: input !! Bytes to write to the pipe.
        integer(kind=i8),         intent(out), optional :: n     !! Bytes written.

        integer(kind=i8) :: n_

        if (present(n)) n = 0_i8

        rc = E_INVALID
        if (pipe%access == PIPE_RDONLY) return
        if (.not. dm_pipe_is_connected(pipe)) return

        rc = E_WRITE
        n_ = c_fwrite(c_loc(input), 1_c_size_t, len(input, kind=c_size_t), pipe%fp)
        if (n_ <= 0) return

        if (present(n)) n = n_
        rc = E_NONE
    end function dm_pipe_write2

    subroutine dm_pipe_close(pipe, exit_stat)
        !! Closes pipe to process.
        type(pipe_type), intent(inout)         :: pipe      !! Pipe type.
        integer,         intent(out), optional :: exit_stat !! Exit status.

        integer :: stat

        if (present(exit_stat)) exit_stat = 0
        if (.not. dm_pipe_is_connected(pipe)) return
        stat = c_pclose(pipe%fp)
        pipe%fp = c_null_ptr
        if (present(exit_stat)) exit_stat = stat / 256
    end subroutine dm_pipe_close

    subroutine dm_pipe_close2(pipe, exit_stat)
        !! Closes pipe to process (binary).
        type(pipe_type), intent(inout)         :: pipe      !! Pipe type.
        integer,         intent(out), optional :: exit_stat !! Exit status.

        integer :: stat

        if (present(exit_stat)) exit_stat = 0
        if (.not. dm_pipe_is_connected(pipe)) return
        stat = c_fclose(pipe%fp)
        pipe%fp = c_null_ptr
        if (present(exit_stat)) exit_stat = stat
    end subroutine dm_pipe_close2
end module dm_pipe
