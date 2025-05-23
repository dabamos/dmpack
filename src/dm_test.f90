! Author:  Philipp Engel
! Licence: ISC
module dm_test
    !! Basic testing framework.
    use :: dm_ansi
    use :: dm_ascii
    use :: dm_error
    use :: dm_kind
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

    character(len=*), parameter :: TEST_STATES(0:3) = [ 'UNKNOWN', 'RUNNING', ' PASSED', ' FAILED' ]
    integer,          parameter :: TEST_COLORS(0:3) = [ COLOR_WHITE, COLOR_YELLOW, COLOR_GREEN, COLOR_RED ]

    abstract interface
        logical function dm_test_callback()
            !! Logical test function that either returns `TEST_PASSED` or
            !! `TEST_FAILED`.
        end function dm_test_callback
    end interface

    type, public :: test_type
        !! Test type.
        character(len=TEST_NAME_LEN)                 :: name = 'N/A'   !! Test name.
        procedure(dm_test_callback), pointer, nopass :: proc => null() !! Test procedure.
    end type test_type

    interface dm_test_dummy
        !! Generic dummy type generator.
        module procedure :: dm_test_dummy_beat
        module procedure :: dm_test_dummy_image
        module procedure :: dm_test_dummy_log
        module procedure :: dm_test_dummy_node
        module procedure :: dm_test_dummy_observ
        module procedure :: dm_test_dummy_request
        module procedure :: dm_test_dummy_sensor
        module procedure :: dm_test_dummy_target
    end interface dm_test_dummy

    public :: dm_test_callback

    public :: dm_test_dummy
    public :: dm_test_dummy_beat
    public :: dm_test_dummy_image
    public :: dm_test_dummy_log
    public :: dm_test_dummy_node
    public :: dm_test_dummy_observ
    public :: dm_test_dummy_request
    public :: dm_test_dummy_sensor
    public :: dm_test_dummy_target
    public :: dm_test_run
    public :: dm_test_skip
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    logical function dm_test_skip(env_var) result(skip)
        !! Returns `.true.` and outputs a debug message if environment variable
        !! of name `env_var` is set to 1.
        use :: dm_env, only: dm_env_get, dm_env_has

        character(len=*), intent(in) :: env_var !! Name of the environment variable.

        integer :: rc
        logical :: no_color

        rc = dm_env_get(env_var, skip, .false.)
        no_color = dm_env_has('NO_COLOR')

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

        type(beat_type), intent(out) :: beat !! Beat type.

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

        type(image_type), intent(out)          :: image !! Image type.
        character(len=*), intent(in), optional :: id    !! Image id.

        if (present(id)) then
            image%id = id
        else
            image%id = dm_uuid4()
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

    impure elemental subroutine dm_test_dummy_log(log, timestamp)
        !! Generates dummy log data type.
        use :: dm_log

        type(log_type),   intent(out)          :: log       !! Log type.
        character(len=*), intent(in), optional :: timestamp !! Log timestamp (ISO 8601).
        real                                   :: r(2)

        call random_number(r)

        log = log_type(id        = dm_uuid4(), &
                       level     = 1 + int(LL_ERROR * r(1)), &
                       error     = int(E_READ_ONLY * r(2)), &
                       timestamp = dm_time_now(), &
                       node_id   = 'dummy-node', &
                       sensor_id = 'dummy-sensor', &
                       target_id = 'dummy-target', &
                       observ_id = dm_uuid4(), &
                       source    = 'dummy', &
                       message   = 'dummy log message')

        if (present(timestamp)) log%timestamp = timestamp
    end subroutine dm_test_dummy_log

    pure elemental subroutine dm_test_dummy_node(node, id, name)
        !! Generates dummy sensor node data type.
        use :: dm_node

        type(node_type),  intent(out)          :: node !! Node type.
        character(len=*), intent(in), optional :: id   !! Node id.
        character(len=*), intent(in), optional :: name !! Node name.

        node%id        = 'dummy-node'
        node%name      = 'Dummy Node'
        node%meta      = 'dummy description'
        node%x         = 100.0_r8
        node%y         = 200.0_r8
        node%z         = 10.0_r8
        node%longitude = 10.4541194_r8
        node%latitude  = 51.1642292_r8
        node%elevation = 100.0_r8

        if (present(id))   node%id   = id
        if (present(name)) node%name = name
    end subroutine dm_test_dummy_node

    impure elemental subroutine dm_test_dummy_observ(observ, id, node_id, sensor_id, target_id, &
                                                     name, timestamp, nrequests, response_value)
        !! Generates dummy observation data type.
        use :: dm_observ
        use :: dm_request

        type(observ_type), intent(out)          :: observ         !! Observation type.
        character(len=*),  intent(in), optional :: id             !! Observation id.
        character(len=*),  intent(in), optional :: node_id        !! Node id.
        character(len=*),  intent(in), optional :: sensor_id      !! Sensor id.
        character(len=*),  intent(in), optional :: target_id      !! Target id.
        character(len=*),  intent(in), optional :: name           !! Observation name.
        character(len=*),  intent(in), optional :: timestamp      !! Observation and request timestamp (ISO 8601).
        integer,           intent(in), optional :: nrequests      !! Number of requests.
        real(kind=r8),     intent(in), optional :: response_value !! Response value.

        integer             :: i, n, rc
        type(request_type)  :: request

        observ%id        = dm_uuid4()
        observ%node_id   = 'dummy-node'
        observ%sensor_id = 'dummy-sensor'
        observ%target_id = 'dummy-target'
        observ%name      = 'dummy-observ'
        observ%timestamp = dm_time_now()
        observ%source    = 'dmdummy'
        observ%device    = '/dev/null'

        if (present(id))        observ%id        = id
        if (present(node_id))   observ%node_id   = node_id
        if (present(sensor_id)) observ%sensor_id = sensor_id
        if (present(target_id)) observ%target_id = target_id
        if (present(name))      observ%name      = name
        if (present(timestamp)) observ%timestamp = timestamp

        rc = dm_observ_add_receiver(observ, 'dummy-receiver-1')
        rc = dm_observ_add_receiver(observ, 'dummy-receiver-2')
        rc = dm_observ_add_receiver(observ, 'dummy-receiver-3')

        n = dm_present(nrequests, 1)

        do i = 1, n
            if (present(response_value)) then
                call dm_test_dummy_request(request, 'dummy-' // dm_itoa(i), observ%timestamp, &
                                           response_name='dummy-' // dm_itoa(i), &
                                           response_value=response_value)
            else
                call dm_test_dummy_request(request, 'dummy-' // dm_itoa(i), observ%timestamp, &
                                           nresponses=REQUEST_MAX_NRESPONSES)
            end if

            rc = dm_observ_add_request(observ, request)
        end do
    end subroutine dm_test_dummy_observ

    impure elemental subroutine dm_test_dummy_request(request, name, timestamp, nresponses, &
                                                      response_name, response_value)
        !! Generates dummy request data type.
        use :: dm_request
        use :: dm_response

        type(request_type), intent(out)          :: request        !! Request type.
        character(len=*),   intent(in), optional :: name           !! Request name.
        character(len=*),   intent(in), optional :: timestamp      !! Request timestamp (ISO 8601).
        integer,            intent(in), optional :: nresponses     !! Number of responses.
        character(len=*),   intent(in), optional :: response_name  !! Response name.
        real(kind=r8),      intent(in), optional :: response_value !! Response value.

        integer             :: i, n, rc
        type(response_type) :: response

        n = 1
        request = request_type(name      = 'dummy-request', &
                               timestamp = dm_time_now(), &
                               request   = 'dummy', &
                               response  = dm_ascii_escape('999.99' // ASCII_CR // ASCII_LF), &
                               delimiter = dm_ascii_escape(ASCII_CR // ASCII_LF), &
                               pattern   = '^(?<dummy>.*)$', &
                               delay     = 1000, &
                               retries   = 0, &
                               timeout   = 500, &
                               error     = 0)

        if (present(name))       request%name      = name
        if (present(timestamp))  request%timestamp = timestamp
        if (present(nresponses)) n = max(0, min(REQUEST_MAX_NRESPONSES, nresponses))

        do i = 1, n
            response = response_type('dummy-' // dm_itoa(i), 'none', RESPONSE_TYPE_REAL64, E_NONE, 999.99_r8)
            if (present(response_name))  response%name  = response_name
            if (present(response_value)) response%value = response_value
            rc = dm_request_add(request, response)
        end do
    end subroutine dm_test_dummy_request

    pure elemental subroutine dm_test_dummy_sensor(sensor, node_id, id, name)
        !! Generates dummy sensor data type.
        use :: dm_sensor

        type(sensor_type), intent(out)          :: sensor  !! Sensor type.
        character(len=*),  intent(in), optional :: node_id !! Node id.
        character(len=*),  intent(in), optional :: id      !! Sensor id.
        character(len=*),  intent(in), optional :: name    !! Sensor name.

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

        sensor%type = SENSOR_TYPE_VIRTUAL
        sensor%sn   = '12345'
        sensor%meta = 'dummy description'
        sensor%x    = 1000.0_r8
        sensor%y    = 2000.0_r8
        sensor%z    = 100.0_r8
    end subroutine dm_test_dummy_sensor

    pure elemental subroutine dm_test_dummy_target(target, id, name)
        !! Generates dummy target data type.
        use :: dm_target

        type(target_type), intent(out)          :: target !! Target type.
        character(len=*),  intent(in), optional :: id     !! Target id.
        character(len=*),  intent(in), optional :: name   !! Target name.

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
        target%x    = 100.0_r8
        target%y    = 200.0_r8
        target%z    = 10.0_r8
    end subroutine dm_test_dummy_target

    subroutine dm_test_run(name, tests, stats, version, options)
        !! Runs all tests in given array `tests`, returns test states in array
        !! `stats`.
        use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
        use :: dm_env, only: dm_env_has
        use :: dm_system
        use :: dm_time
        use :: dm_timer
        use :: dm_version

        character(len=*), intent(in)           :: name     !! Test name.
        type(test_type),  intent(inout)        :: tests(:) !! Test types.
        logical,          intent(out)          :: stats(:) !! `TEST_FAILED` or `TEST_PASSED`.
        character(len=*), intent(in), optional :: version  !! Compiler version.
        character(len=*), intent(in), optional :: options  !! Compiler options.

        character(len=:), allocatable :: options_, version_
        character(len=TEST_NAME_LEN)  :: test_name

        integer          :: i, n, nfail, npass, state
        logical          :: no_color
        real(kind=r8)    :: time, total_time
        type(timer_type) :: timer
        type(uname_type) :: uname

        n = size(tests)
        no_color = dm_env_has('NO_COLOR')

        if (present(version)) then
            version_ = version
        else
            version_ = compiler_version()
        end if

        if (present(options)) then
            options_ = options
        else
            options_ = compiler_options()
        end if

        call dm_system_uname(uname)
        call dm_ansi_color(COLOR_GREEN, no_color)
        call test_title('TEST SESSION STARTS', TEST_LINE_LEN)
        call dm_ansi_reset(no_color)

        print '("Name....: ", a)', trim(name)
        print '("Time....: ", a)', dm_time_strip_useconds(dm_time_now())
        print '("System..: ", a, 1x, a, " (", a, ")")', trim(uname%system_name), &
                                                        trim(uname%release), &
                                                        trim(uname%machine)
        print '("Compiler: ", a)', version_
        print '("Options.: ", a)', options_
        print '("DMPACK..: ", a, " (", a, ")", /)', DM_VERSION_STRING, DM_BUILD_DATE

        print '("Running ", i0, 1x, a, " ...")', n, dm_btoa((n == 1), 'test', 'tests')

        total_time = 0.0

        do i = 1, n
            test_name = trim(name) // '.' // trim(tests(i)%name)

            print '(a)', repeat('-', TEST_LINE_LEN)
            call test_print(i, n, test_name, TEST_STATE_RUNNING, no_color=no_color)

            stats(i) = associated(tests(i)%proc)

            if (.not. stats(i)) then
                call dm_ansi_color(COLOR_RED, no_color)
                print '("[ERROR} no procedure provided for test ", a)', trim(test_name)
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
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    subroutine test_print(index, ntests, name, state, time, no_color)
        !! Outputs test states.
        character(len=*), parameter :: FMT_STATE = '("[TEST ", i2, "/", i2, "] ", a, 20x, a)'
        character(len=*), parameter :: FMT_TIME  = '("[TEST ", i2, "/", i2, "] ", a, " in ", f8.4, " sec", 3x, a)'

        integer,          intent(in)           :: index    !! Test number.
        integer,          intent(in)           :: ntests   !! Number of tests.
        character(len=*), intent(in)           :: name     !! Test name.
        integer,          intent(in)           :: state    !! Test state.
        real(kind=r8),    intent(in), optional :: time     !! Test duration.
        logical,          intent(in), optional :: no_color !! No ANSI colours.

        logical :: no_color_

        no_color_ = dm_present(no_color, .false.)

        call dm_ansi_color(TEST_COLORS(state), no_color_)

        if (present(time)) then
            write (*, FMT_TIME)  index, ntests, name, time, TEST_STATES(state)
        else
            write (*, FMT_STATE) index, ntests, name, TEST_STATES(state)
        end if

        call dm_ansi_reset(no_color_)
    end subroutine test_print

    subroutine test_title(text, length, glyph)
        !! Prints a header with given `test` and line length `len`.
        character(len=*), intent(in)           :: text   !! Title text.
        integer,          intent(in)           :: length !! Line length.
        character,        intent(in), optional :: glyph  !! Optional line character.

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
