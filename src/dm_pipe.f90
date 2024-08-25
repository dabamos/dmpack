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
        type(c_ptr) :: ptr    = c_null_ptr !! Pointer of pipe.
    end type pipe_type

    public :: dm_pipe_connected
    public :: dm_pipe_close
    public :: dm_pipe_close2
    public :: dm_pipe_open
    public :: dm_pipe_open2
    public :: dm_pipe_read
    public :: dm_pipe_write
    public :: dm_pipe_write2
contains
    logical function dm_pipe_connected(pipe) result(connected)
        !! Returns `.true.` if pipe is connected.
        type(pipe_type), intent(inout) :: pipe !! Pipe type.

        connected = c_associated(pipe%ptr)
    end function dm_pipe_connected

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
        type(pipe_type),  intent(inout) :: pipe    !! Pipe type.
        character(len=*), intent(in)    :: command !! Name or path of binary to open.
        integer,          intent(in)    :: access  !! Open pipe for reading or writing.

        character(len=2) :: a

        rc = E_EXIST
        if (dm_pipe_connected(pipe)) return

        rc = E_INVALID
        select case (access)
            case (PIPE_RDONLY)
                a = 'r' // c_null_char
            case (PIPE_WRONLY)
                a = 'w' // c_null_char
            case default
                return
        end select

        rc = E_SYSTEM
        pipe%ptr = c_popen(trim(command) // c_null_char, a)
        if (.not. c_associated(pipe%ptr)) return

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
        type(pipe_type),  intent(out) :: stdin   !! Standard input handle.
        type(pipe_type),  intent(out) :: stdout  !! Standard output handle.
        type(pipe_type),  intent(out) :: stderr  !! Standard error handle.
        character(len=*), intent(in)  :: command !! Program to invoke.

        integer :: p1(2), p2(2), p3(2), pid, stat

        rc = E_EXIST
        if (dm_pipe_connected(stdin))  return
        if (dm_pipe_connected(stdout)) return
        if (dm_pipe_connected(stderr)) return

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

            stdin%ptr  = c_fdopen(p1(2), 'w' // c_null_char)
            stdout%ptr = c_fdopen(p2(1), 'r' // c_null_char)
            stderr%ptr = c_fdopen(p3(1), 'r' // c_null_char)

            if (.not. dm_pipe_connected(stdin))  return
            if (.not. dm_pipe_connected(stdout)) return
            if (.not. dm_pipe_connected(stderr)) return

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

            stat = c_execl('/bin/sh'     // c_null_char, &
                           '/bin/sh'     // c_null_char, &
                           '-c'          // c_null_char, &
                           trim(command) // c_null_char, &
                           c_null_ptr)

            call c_exit(EXIT_SUCCESS)
        end if
    end function dm_pipe_open2

    integer(kind=i8) function dm_pipe_read(pipe, bytes) result(sz)
        !! Reads from pipe to buffer `bytes` (binary) and returns number of
        !! bytes written to buffer.
        type(pipe_type),          intent(inout) :: pipe  !! Bi-directional pipe.
        character(len=*), target, intent(inout) :: bytes !! Output buffer.

        sz = 0_i8
        bytes = ' '
        if (pipe%access == PIPE_WRONLY) return
        sz = c_fread(c_loc(bytes), 1_c_size_t, len(bytes, kind=c_size_t), pipe%ptr)
    end function dm_pipe_read

    integer function dm_pipe_write(pipe, str) result(rc)
        !! Writes to pipe. Trims the string, adds new line and
        !! null-termination.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if pipe is not associated or access mode is not `PIPE_RDONLY`.
        !! * `E_SYSTEM` if system call failed.
        !!
        type(pipe_type),  intent(inout) :: pipe !! Pipe type.
        character(len=*), intent(in)    :: str  !! String to write to the pipe.

        rc = E_INVALID
        if (pipe%access == PIPE_RDONLY) return
        if (.not. dm_pipe_connected(pipe)) return

        rc = E_SYSTEM
        if (c_fputs(trim(str) // c_new_line // c_null_char, pipe%ptr) < 0) return

        rc = E_NONE
    end function dm_pipe_write

    integer(kind=i8) function dm_pipe_write2(pipe, bytes) result(sz)
        !! Writes to pipe (binary) and returns the number of bytes written.
        type(pipe_type),          intent(inout) :: pipe  !! Bi-directional pipe.
        character(len=*), target, intent(in)    :: bytes !! Bytes to write to the pipe.

        sz = 0_i8
        if (pipe%access == PIPE_RDONLY) return
        sz = c_fwrite(c_loc(bytes), 1_c_size_t, len(bytes, kind=c_size_t), pipe%ptr)
    end function dm_pipe_write2

    subroutine dm_pipe_close(pipe)
        !! Closes pipe to process.
        type(pipe_type), intent(inout) :: pipe !! Pipe type.

        integer :: stat

        if (dm_pipe_connected(pipe)) stat = c_pclose(pipe%ptr)
        if (stat == 0) pipe%ptr = c_null_ptr
    end subroutine dm_pipe_close

    subroutine dm_pipe_close2(pipe)
        !! Closes pipe to process (binary).
        type(pipe_type), intent(inout) :: pipe !! Pipe type.

        integer :: stat

        if (dm_pipe_connected(pipe)) stat = c_fclose(pipe%ptr)
        if (stat == 0) pipe%ptr = c_null_ptr
    end subroutine dm_pipe_close2
end module dm_pipe
