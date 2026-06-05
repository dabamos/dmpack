! Author:  Philipp Engel
! Licence: ISC
module dm_test
    !! Basic testing framework.
    use :: dm_ansi
    use :: dm_ascii
    use :: dm_error
    use :: dm_kind
    use :: dm_random
    use :: dm_time
    use :: dm_util
    use :: dm_uuid
    implicit none (type, external)
    private

    integer, parameter, public :: TEST_LINE_LEN = 72
    integer, parameter, public :: TEST_NAME_LEN = 32

    ! States of a test.
    integer, parameter, public :: TEST_STATE_UNKNOWN = 0
    integer, parameter, public :: TEST_STATE_RUNNING = 1
    integer, parameter, public :: TEST_STATE_PASSED  = 2
    integer, parameter, public :: TEST_STATE_FAILED  = 3

    ! Test results.
    logical, parameter, public :: TEST_PASSED = .true.
    logical, parameter, public :: TEST_FAILED = .false.

    character(*), parameter :: TEST_STATES(0:3) = [ 'UNKNOWN', 'RUNNING', 'PASSED ', 'FAILED ' ]
    integer,      parameter :: TEST_COLORS(0:3) = [ COLOR_WHITE, COLOR_YELLOW, COLOR_GREEN, COLOR_RED ]

    abstract interface
        logical function dm_test_callback()
            !! Logical test function that either returns `TEST_PASSED` or
            !! `TEST_FAILED`.
        end function dm_test_callback
    end interface

    type, public :: test_type
        !! Test type.
        character(TEST_NAME_LEN)                     :: name = 'N/A'   !! Test name.
        procedure(dm_test_callback), pointer, nopass :: proc => null() !! Test procedure.
    end type test_type

    interface dm_test_dummy
        !! Generic dummy type generator.
        module procedure :: dm_test_dummy_beat
        module procedure :: dm_test_dummy_header
        module procedure :: dm_test_dummy_image
        module procedure :: dm_test_dummy_log
        module procedure :: dm_test_dummy_node
        module procedure :: dm_test_dummy_observ
        module procedure :: dm_test_dummy_sensor
        module procedure :: dm_test_dummy_target
    end interface dm_test_dummy

    public :: dm_test_callback

    public :: dm_test_dummy
    public :: dm_test_dummy_beat
    public :: dm_test_dummy_header
    public :: dm_test_dummy_image
    public :: dm_test_dummy_log
    public :: dm_test_dummy_node
    public :: dm_test_dummy_observ
    public :: dm_test_dummy_sensor
    public :: dm_test_dummy_target
    public :: dm_test_run
    public :: dm_test_skip
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    logical function dm_test_skip(env_var) result(skip)
        !! Returns `.true.` and outputs a debug message if environment variable
        !! of name `env_var` is set to 1.
        use :: dm_env, only: dm_env_get, dm_env_has

        character(*), intent(in) :: env_var !! Name of the environment variable.

        integer :: rc
        logical :: no_color

        no_color = dm_env_has('NO_COLOR')
        rc = dm_env_get(env_var, skip, .false.)

        if (skip) then
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("> Environment variable ", a, " is set.")', trim(env_var)
            print '("> This test will be skipped.")'
            call dm_ansi_reset(no_color)
        end if
    end function dm_test_skip

    impure elemental subroutine dm_test_dummy_beat(beat)
        !! Generates dummy beat data type.
        use :: dm_beat
        use :: dm_version

        type(beat_type), intent(out) :: beat !! Beat.

        beat = beat_type(node_id   = 'dummy-node', &
                         address   = '127.0.0.1', &
                         client    = dm_version_to_string('dmtest', 1, 0, 0, library=.true.), &
                         time_sent = dm_time_now(), &
                         time_recv = dm_time_now(), &
                         interval  = 60, &
                         uptime    = 3600)
    end subroutine dm_test_dummy_beat

    impure elemental subroutine dm_test_dummy_image(image, id)
        !! Generates dummy image data type.
        use :: dm_image
        use :: dm_mime

        type(image_type), intent(out)          :: image !! Image.
        character(*),     intent(in), optional :: id    !! Image id.

        if (present(id)) then
            image%id = id
        else
            image%id = dm_uuid_new()
        end if

        image%node_id   = 'dummy-node'
        image%sensor_id = 'dummy-sensor'
        image%target_id = 'dummy-target'
        image%timestamp = dm_time_now()
        image%mime      = MIME_PNG
        image%width     = 640
        image%height    = 480
        image%size      = 100_i8
    end subroutine dm_test_dummy_image

    impure elemental subroutine dm_test_dummy_header(header)
        !! Generates dummy header data type.
        use :: dm_message
        use :: dm_type

        type(message_header_type), intent(out) :: header !! Header.

        header = message_header_type(id    = dm_uuid_new(),                               &
                                     from  = 'dmdummy1',                                  &
                                     to    = 'dmdummy2',                                  &
                                     type  = dm_random_get_uniform(TYPE_NONE, TYPE_LAST), &
                                     size  = dm_random_get_uniform(0, 4096),              &
                                     error = dm_random_get_uniform(0, E_LAST))
    end subroutine dm_test_dummy_header

    impure elemental subroutine dm_test_dummy_log(log, timestamp)
        !! Generates dummy log data type.
        use :: dm_log

        type(log_type), intent(out)          :: log       !! Log.
        character(*),   intent(in), optional :: timestamp !! Log timestamp (ISO 8601).

        log = log_type(id        = dm_uuid_new(),                     &
                       level     = dm_random_get_uniform(1, LL_LAST), &
                       error     = dm_random_get_uniform(0, E_LAST),  &
                       timestamp = dm_time_now(),                     &
                       node_id   = 'dummy-node',                      &
                       sensor_id = 'dummy-sensor',                    &
                       target_id = 'dummy-target',                    &
                       observ_id = dm_uuid_new(),                     &
                       source    = 'dummy',                           &
                       message   = 'dummy log message')

        if (present(timestamp)) log%timestamp = timestamp
    end subroutine dm_test_dummy_log

    impure elemental subroutine dm_test_dummy_node(node, id, name)
        !! Generates dummy sensor node data type.
        use :: dm_node

        type(node_type), intent(out)          :: node !! Node.
        character(*),    intent(in), optional :: id   !! Node id.
        character(*),    intent(in), optional :: name !! Node name.

        node%id        = 'dummy-node'
        node%name      = 'Dummy Node'
        node%meta      = 'dummy description'
        node%x         = dm_random_get_uniform(       0.0_r8,     1000.0_r8)
        node%y         = dm_random_get_uniform(    2000.0_r8,     3000.0_r8)
        node%z         = dm_random_get_uniform(     -10.0_r8,      100.0_r8)
        node%longitude = dm_random_get_uniform(10.4541194_r8, 10.4600000_r8)
        node%latitude  = dm_random_get_uniform(51.1642292_r8, 51.1700000_r8)
        node%elevation = dm_random_get_uniform(     -10.0_r8,      100.0_r8)

        if (present(id))   node%id   = id
        if (present(name)) node%name = name
    end subroutine dm_test_dummy_node

    impure elemental subroutine dm_test_dummy_observ(observ, id, node_id, sensor_id, target_id, &
                                                     name, timestamp, nresponses, response_value)
        !! Generates dummy observation data type.
        use :: dm_observ

        type(observ_type), intent(out)          :: observ         !! Observation.
        character(*),      intent(in), optional :: id             !! Observation id.
        character(*),      intent(in), optional :: node_id        !! Node id.
        character(*),      intent(in), optional :: sensor_id      !! Sensor id.
        character(*),      intent(in), optional :: target_id      !! Target id.
        character(*),      intent(in), optional :: name           !! Observation name.
        character(*),      intent(in), optional :: timestamp      !! Observation and request timestamp (ISO 8601).
        integer,           intent(in), optional :: nresponses     !! Number of responses.
        real(r8),          intent(in), optional :: response_value !! Response value.

        integer  :: i, n, rc
        real(r8) :: v

        observ%id        = dm_uuid_new()
        observ%group_id  = dm_uuid_new()
        observ%node_id   = 'dummy-node'
        observ%sensor_id = 'dummy-sensor'
        observ%target_id = 'dummy-target'
        observ%timestamp = dm_time_now()
        observ%name      = 'dummy-observ'
        observ%source    = 'dmdummy'
        observ%device    = '/dev/null'

        if (present(id))        observ%id        = id
        if (present(node_id))   observ%node_id   = node_id
        if (present(sensor_id)) observ%sensor_id = sensor_id
        if (present(target_id)) observ%target_id = target_id
        if (present(name))      observ%name      = name
        if (present(timestamp)) observ%timestamp = timestamp

        observ%request   = 'dummy'
        observ%response  = dm_ascii_escape('999.99' // ASCII_CR // ASCII_LF)
        observ%delimiter = dm_ascii_escape(ASCII_CR // ASCII_LF)
        observ%pattern   = '^(?<dummy>.*)$'
        observ%delay     = dm_random_get_uniform(0, 10000)
        observ%error     = E_NONE
        observ%retries   = 1
        observ%timeout   = 500

        n = max(0, min(dm_present(nresponses, 1), OBSERV_MAX_NRESPONSES))

        do i = 1, n
            v  = dm_present(response_value, dm_random_get_uniform(0.0_r8, 1000.0_r8))
            rc = dm_observ_add_response(observ, name='dummy-' // dm_itoa(i), unit='none', value=v)
        end do
    end subroutine dm_test_dummy_observ

    impure elemental subroutine dm_test_dummy_sensor(sensor, node_id, id, name)
        !! Generates dummy sensor data type.
        use :: dm_sensor

        type(sensor_type), intent(out)          :: sensor  !! Sensor.
        character(*),      intent(in), optional :: node_id !! Node id.
        character(*),      intent(in), optional :: id      !! Sensor id.
        character(*),      intent(in), optional :: name    !! Sensor name.

        if (present(node_id)) then
            sensor%node_id = node_id
        else
            sensor%node_id = 'dummy-node'
        end if

        if (present(id)) then
            sensor%id = id
        else
            sensor%id = 'dummy-sensor'
        end if

        if (present(name)) then
            sensor%name = name
        else
            sensor%name = 'Dummy Sensor'
        end if

        sensor%type = dm_random_get_uniform(0, SENSOR_TYPE_LAST)
        sensor%sn   = dm_itoa(dm_random_get_uniform(0, 9999999))
        sensor%meta = 'dummy description'
        sensor%x    = dm_random_get_uniform(   0.0_r8, 1000.0_r8)
        sensor%y    = dm_random_get_uniform(2000.0_r8, 3000.0_r8)
        sensor%z    = dm_random_get_uniform( -10.0_r8,  100.0_r8)
    end subroutine dm_test_dummy_sensor

    impure elemental subroutine dm_test_dummy_target(target, id, name)
        !! Generates dummy target data type.
        use :: dm_target

        type(target_type), intent(out)          :: target !! Target.
        character(*),      intent(in), optional :: id     !! Target id.
        character(*),      intent(in), optional :: name   !! Target name.

        if (present(id)) then
            target%id = id
        else
            target%id = 'dummy-target'
        end if

        if (present(name)) then
            target%name = name
        else
            target%name = 'Dummy Target'
        end if

        target%meta = 'dummy description'
        target%x    = dm_random_get_uniform(   0.0_r8, 1000.0_r8)
        target%y    = dm_random_get_uniform(2000.0_r8, 3000.0_r8)
        target%z    = dm_random_get_uniform( -10.0_r8,  100.0_r8)
    end subroutine dm_test_dummy_target

    subroutine dm_test_run(name, tests, stats)
        !! Runs all tests in given array `tests`, returns test states in array
        !! `stats`.
        use :: dm_env, only: dm_env_has
        use :: dm_posix
        use :: dm_time
        use :: dm_timer
        use :: dm_version

        character(*),    intent(in)    :: name               !! Test name.
        type(test_type), intent(inout) :: tests(:)           !! Test types.
        logical,         intent(out)   :: stats(size(tests)) !! `TEST_FAILED` or `TEST_PASSED`.

        character(TEST_NAME_LEN) :: test_name
        integer                  :: i, n, nfail, npass, state
        logical                  :: no_color
        real(r8)                 :: time, total_time

        type(timer_type)       :: timer
        type(posix_uname_type) :: uname

        n = size(tests)
        no_color = dm_env_has('NO_COLOR')

        call dm_posix_uname(uname)
        call dm_ansi_color(COLOR_GREEN, no_color)
        call test_title('TEST SESSION STARTS', TEST_LINE_LEN)
        call dm_ansi_reset(no_color)

        print '("Name....: ", a)',                      trim(name)
        print '("Time....: ", a)',                      dm_time_strip(dm_time_now())
        print '("System..: ", a, 1x, a, " (", a, ")")', trim(uname%system_name), trim(uname%release), trim(uname%machine)
        print '("DMPACK..: ", a, " (", a, ")")',        DM_VERSION_STRING, DM_LIBRARY_DATE
        print '("Compiler: ", a)',                      DM_LIBRARY_COMPILER
        print '("Options.: ", a, /)',                   DM_LIBRARY_OPTIONS
        print '("Running ", i0, 1x, a, " ...")',        n, dm_btoa((n == 1), 'test', 'tests')

        total_time = 0.0

        do i = 1, n
            test_name = trim(name) // '.' // trim(tests(i)%name)

            call test_title('TEST OUTPUT', TEST_LINE_LEN, '-')
            call test_print(i, n, test_name, TEST_STATE_RUNNING, no_color=no_color)

            stats(i) = associated(tests(i)%proc)

            if (.not. stats(i)) then
                call dm_ansi_color(COLOR_RED, no_color)
                print '("[ERROR] no procedure provided for test ", a)', trim(test_name)
                call dm_ansi_reset(no_color)
                cycle
            end if

            call dm_timer_start(timer)
            stats(i) = tests(i)%proc()
            call dm_timer_stop(timer, time)
            total_time = total_time + time

            state = dm_btoi(stats(i), true=TEST_STATE_PASSED, false=TEST_STATE_FAILED)
            call test_print(i, n, test_name, state, time, no_color=no_color)
        end do

        call test_title('TEST SUMMARY', TEST_LINE_LEN, '-')
        npass = count(stats)
        call dm_ansi_color(COLOR_GREEN, no_color)
        print '(i0, 1x, a, " passed")', npass, dm_btoa((npass == 1), 'test', 'tests')
        call dm_ansi_reset(no_color)

        nfail = n - npass
        if (nfail > 0) call dm_ansi_color(COLOR_RED, no_color)
        print '(i0, 1x, a, " failed")', nfail, dm_btoa((nfail == 1), 'test', 'tests')
        call dm_ansi_reset(no_color)

        print '("Total execution time: ", f8.4, " sec")', total_time

        call dm_ansi_color(COLOR_GREEN, no_color)
        call test_title('TEST SESSION FINISHED', TEST_LINE_LEN, '-')
        call dm_ansi_reset(no_color)

        print *
        if (nfail > 0) call dm_stop(STOP_FAILURE)
    end subroutine dm_test_run

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    subroutine test_print(index, ntests, name, state, time, no_color)
        !! Outputs test states.
        character(*), parameter :: FMT_STATE = '("[TEST ", i2, "/", i2, "] ", a, 20x, a)'
        character(*), parameter :: FMT_TIME  = '("[TEST ", i2, "/", i2, "] ", a, " in ", f8.4, " sec", 4x, a)'

        integer,      intent(in)           :: index    !! Test number.
        integer,      intent(in)           :: ntests   !! Number of tests.
        character(*), intent(in)           :: name     !! Test name.
        integer,      intent(in)           :: state    !! Test state.
        real(r8),     intent(in), optional :: time     !! Test duration.
        logical,      intent(in), optional :: no_color !! No ANSI colours.

        logical :: no_color_

        no_color_ = dm_present(no_color, .false.)

        call dm_ansi_color(TEST_COLORS(state), no_color_)

        if (present(time)) then
            write (*, FMT_TIME)  index, ntests, name, time, adjustr(TEST_STATES(state))
        else
            write (*, FMT_STATE) index, ntests, name, adjustr(TEST_STATES(state))
        end if

        call dm_ansi_reset(no_color_)
    end subroutine test_print

    subroutine test_title(text, length, glyph)
        !! Prints a header with given `test` and line length `len`.
        character(*), intent(in)           :: text   !! Title text.
        integer,      intent(in)           :: length !! Line length.
        character,    intent(in), optional :: glyph  !! Optional line character.

        character :: a
        integer   :: i, j, k

        a = dm_present(glyph, '*')
        i = length - len_trim(text) - 2
        j = i / 2
        k = modulo(i, 2)

        write (*, '(a, 1x)', advance='no') repeat(a, j)
        write (*, '(a, 1x)', advance='no') trim(text)
        write (*, '(a)')                   repeat(a, j + k)
    end subroutine test_title
end module dm_test
