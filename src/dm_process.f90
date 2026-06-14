! dm_process.f90
!
! Author:  Philipp Engel
! Licence: ISC
module dm_process
    !! Abstraction layer around POSIX spawning API to run programs as child
    !! processes. Only DMPACK programs are supported.
    !!
    !! ## Examples
    !!
    !! Start and stop the DMPACK program `/opt/bin/dmdb`:
    !!
    !! ``` fortran
    !! type(process_type) :: process
    !!
    !! call dm_process_init(process, 'dmdb', '/opt/bin/dmdb', config='/opt/config/dmdb.conf')
    !! call dm_process_start(process)
    !! call dm_posix_sleep(30)
    !! call dm_process_stop(process)
    !! call dm_process_destroy(process)
    !! ```
    !!
    !! The spawner will run `/opt/bin/dmdb -n dmdb -c /opt/config/dmdb.conf`.
    use :: dm_error
    use :: dm_file
    use :: dm_id
    use :: dm_kind
    use :: dm_posix
    use :: dm_zmq, only: ZMQ_ADDRESS_LEN
    implicit none
    private

    character(*), parameter :: PROCESS_ARG_CONFIG  = '-c' ! --config <path>
    character(*), parameter :: PROCESS_ARG_DEBUG   = '-D' ! --debug <T|F>
    character(*), parameter :: PROCESS_ARG_NAME    = '-n' ! --name <name>
    character(*), parameter :: PROCESS_ARG_VERBOSE = '-V' ! --verbose <T|F>

    type, public :: process_type
        !! Opaque process context.
        private
        character(ID_LEN)          :: name     = ' '     !! Name of DMPACK process (`-0-9A-Z_a-z`).
        character(FILE_PATH_LEN)   :: path     = ' '     !! Absolute path of DMPACK executable (required).
        character(FILE_PATH_LEN)   :: config   = ' '     !! Empty, character "*", or absolute path to DMPACK configuration file.
        character(ZMQ_ADDRESS_LEN) :: mqueue   = ' '     !! ZeroMQ pub/sub socket address (optional).
        character(ZMQ_ADDRESS_LEN) :: pipeline = ' '     !! ZeroMQ pipeline socket address (optional).
        integer                    :: error    = E_NONE  !! Last process error.
        integer                    :: pid      = 0       !! Process ID.
        logical                    :: debug    = .false. !! Process sends debug messages.
        logical                    :: verbose  = .false. !! Process is verbose.
    end type process_type

    public :: dm_process_destroy
    public :: dm_process_init
    public :: dm_process_error
    public :: dm_process_is_running
    public :: dm_process_is_valid
    public :: dm_process_out
    public :: dm_process_pid
    public :: dm_process_start
    public :: dm_process_stop
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS
    ! **************************************************************************
    pure elemental integer function dm_process_error(process) result(error)
        !! Returns error code of process context.
        type(process_type), intent(in) :: process !! Process context.

        error = process%error
    end function dm_process_error

    impure elemental logical function dm_process_is_running(process) result(running)
        type(process_type), intent(in) :: process !! Process context.

        running = .false.
        if (.not. dm_process_is_valid(process) .or. process%pid == 0) return
        call dm_posix_wait_pid(process%pid, blocking=.false., running=running)
    end function dm_process_is_running

    impure elemental logical function dm_process_is_valid(process) result(valid)
        !! Returns `.true.` if process is valid.
        type(process_type), intent(in) :: process !! Process context.

        valid = (dm_id_is_valid(process%name)     .and. &
                 dm_file_exists(process%path)     .and. &
                 dm_file_exists(process%config)   .and. &
                 dm_error_is_valid(process%error) .and. &
                 process%pid >= 0)
    end function dm_process_is_valid

    pure integer function dm_process_pid(process) result(pid)
        !! Returns PID of process or 0.
        type(process_type), intent(in) :: process !! Process context.

        pid = process%pid
    end function dm_process_pid

    ! **************************************************************************
    ! PUBLIC SUBROUTINES
    ! **************************************************************************
    pure elemental subroutine dm_process_destroy(process)
        !! This subroutine does not kill a running process.
        type(process_type), intent(inout) :: process !! Process context.

        process = process_type()
    end subroutine dm_process_destroy

    pure subroutine dm_process_init(process, name, path, config, mqueue, pipeline, debug, verbose)
        !! Initialises the process. This subroutine does not validate the input
        !! values.
        type(process_type), intent(out)          :: process  !! Process context to create.
        character(*),       intent(in)           :: name     !! Name of process.
        character(*),       intent(in)           :: path     !! Absolute path to executable.
        character(*),       intent(in), optional :: config   !! Absolute path of configuration file.
        character(*),       intent(in), optional :: mqueue   !! ZeroMQ pub/sub socket address.
        character(*),       intent(in), optional :: pipeline !! ZeroMQ pipeline socket address.
        logical,            intent(in), optional :: debug    !! Process sends debug messages.
        logical,            intent(in), optional :: verbose  !! Process is verbose.

        process%name = name
        process%path = path

        if (present(config))   process%config   = config
        if (present(mqueue))   process%mqueue   = mqueue
        if (present(pipeline)) process%pipeline = pipeline
        if (present(debug))    process%debug    = debug
        if (present(verbose))  process%verbose  = verbose
    end subroutine dm_process_init

    subroutine dm_process_out(process, unit)
        !! Prints process context to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(process_type), intent(in)           :: process !! Process context.
        integer,            intent(in), optional :: unit    !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("process.name: ", a)')     trim(process%name)
        write (unit_, '("process.path: ", a)')     trim(process%path)
        write (unit_, '("process.config: ", a)')   trim(process%config)
        write (unit_, '("process.mqueue: ", a)')   trim(process%mqueue)
        write (unit_, '("process.pipeline: ", a)') trim(process%pipeline)
        write (unit_, '("process.error: ", i0)')   process%error
        write (unit_, '("process.pid: ", i0)')     process%pid
        write (unit_, '("process.debug: ", l1)')   process%debug
        write (unit_, '("process.verbose: ", l1)') process%verbose
    end subroutine dm_process_out

    subroutine dm_process_start(process, error)
        use :: dm_path, only: dm_path_name
        use :: dm_util, only: dm_present_set

        type(process_type), intent(inout)         :: process !! Process context.
        integer,            intent(out), optional :: error   !! Error code.

        spawn_block: block
            integer, parameter       :: MAX_ARGS = 9
            character(FILE_PATH_LEN) :: argv(MAX_ARGS)
            integer                  :: n

            process%error = E_INVALID
            if (.not. dm_process_is_valid(process)) exit spawn_block

            n = 0
            n = n + 1; argv(n) = dm_path_name(process%path)
            n = n + 1; argv(n) = PROCESS_ARG_NAME
            n = n + 1; argv(n) = process%name

            if (len_trim(process%config) > 0) then
                n = n + 1; argv(n) = PROCESS_ARG_CONFIG
                n = n + 1; argv(n) = process%config
            end if

            n = n + 1; argv(n) = PROCESS_ARG_DEBUG
            n = n + 1; argv(n) = merge('T', 'F', process%debug)
            n = n + 1; argv(n) = PROCESS_ARG_VERBOSE
            n = n + 1; argv(n) = merge('T', 'F', process%verbose)

            call dm_posix_spawn(process%pid, process%path, argv(1:n), error=process%error)
        end block spawn_block

        call dm_present_set(error, process%error)
    end subroutine dm_process_start

    subroutine dm_process_stop(process, stopped, error)
        !! Sends signal `SIGTERM` to process.
        use :: dm_posix_signal, only: POSIX_SIGNAL_SIGTERM
        use :: dm_util,         only: dm_present_set

        type(process_type), intent(inout)         :: process !! Process context.
        logical,            intent(out), optional :: stopped !! Process has been stopped.
        integer,            intent(out), optional :: error   !! Error code.

        logical :: running

        running = .false.

        kill_block: block
            ! Only stop valid process.
            process%error = E_INVALID
            if (.not. dm_process_is_valid(process)) exit kill_block

            ! Process must be running.
            process%error = E_NONE
            running = dm_process_is_running(process)
            if (.not. running) exit kill_block

            ! Send SIGTERM.
            call dm_posix_kill(process%pid, POSIX_SIGNAL_SIGTERM, error=process%error)
            if (dm_is_ok(process%error)) running = .false.
        end block kill_block

        call dm_present_set(stopped, .not. running)
        call dm_present_set(error,   process%error)
    end subroutine dm_process_stop
end module dm_process
