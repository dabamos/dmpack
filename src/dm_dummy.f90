! Author:  Philipp Engel
! Licence: ISC
module dm_dummy
    !! Dummy type generators.
    use :: dm_ascii
    use :: dm_error
    use :: dm_kind
    use :: dm_log
    use :: dm_node
    use :: dm_observ
    use :: dm_request
    use :: dm_response
    use :: dm_sensor
    use :: dm_target
    use :: dm_time
    use :: dm_uuid
    use :: dm_util
    implicit none (type, external)
    private

    public :: dm_dummy_log
    public :: dm_dummy_node
    public :: dm_dummy_observ
    public :: dm_dummy_request
    public :: dm_dummy_sensor
    public :: dm_dummy_target
contains
    impure elemental subroutine dm_dummy_log(log, timestamp)
        !! Generates dummy log data type.
        type(log_type),   intent(out)          :: log       !! Log type.
        character(len=*), intent(in), optional :: timestamp !! Log timestamp (ISO 8601).
        real                                   :: r(2)

        call random_number(r)

        log = log_type(id        = dm_uuid4(), &
                       level     = 1 + int(LOG_ERROR * r(1)), &
                       error     = int(E_READ_ONLY * r(2)), &
                       timestamp = dm_time_now(), &
                       node_id   = 'dummy-node', &
                       sensor_id = 'dummy-sensor', &
                       target_id = 'dummy-target', &
                       observ_id = dm_uuid4(), &
                       source    = 'dummy', &
                       message   = 'dummy log message')

        if (present(timestamp)) log%timestamp = timestamp
    end subroutine dm_dummy_log

    pure elemental subroutine dm_dummy_node(node, id, name)
        !! Generates dummy sensor node data type.
        type(node_type),  intent(out)          :: node !! Node type.
        character(len=*), intent(in), optional :: id   !! Node id.
        character(len=*), intent(in), optional :: name !! Node name.

        node%id   = 'dummy-node'
        node%name = 'Dummy Node'
        node%meta = 'a dummy node'

        if (present(id)) node%id = id
        if (present(name)) node%name = name
    end subroutine dm_dummy_node

    impure elemental subroutine dm_dummy_observ(observ, id, node_id, sensor_id, target_id, &
                                                name, timestamp, nrequests, value)
        !! Generates dummy observation data type.
        type(observ_type), intent(out)          :: observ    !! Observation type.
        character(len=*),  intent(in), optional :: id        !! Observation id.
        character(len=*),  intent(in), optional :: node_id   !! Node id.
        character(len=*),  intent(in), optional :: sensor_id !! Sensor id.
        character(len=*),  intent(in), optional :: target_id !! Target id.
        character(len=*),  intent(in), optional :: name      !! Observation name.
        character(len=*),  intent(in), optional :: timestamp !! Observation and request timestamp (ISO 8601).
        integer,           intent(in), optional :: nrequests !! Number of requests.
        real(kind=r8),     intent(in), optional :: value     !! Response value.

        integer             :: i, n, rc
        type(request_type)  :: request

        observ%id        = dm_uuid4()
        observ%node_id   = 'dummy-node'
        observ%sensor_id = 'dummy-sensor'
        observ%target_id = 'dummy-target'
        observ%name      = 'dummy-observ'
        observ%timestamp = dm_time_now()
        observ%path      = '/dev/null'

        if (present(id))        observ%id        = id
        if (present(node_id))   observ%node_id   = node_id
        if (present(sensor_id)) observ%sensor_id = sensor_id
        if (present(target_id)) observ%target_id = target_id
        if (present(name))      observ%name      = name
        if (present(timestamp)) observ%timestamp = timestamp

        rc = dm_observ_add_receiver(observ, 'dummy-receiver-1')
        rc = dm_observ_add_receiver(observ, 'dummy-receiver-2')
        rc = dm_observ_add_receiver(observ, 'dummy-receiver-3')

        n = 1
        if (present(nrequests)) n = nrequests

        do i = 1, n
            if (present(value)) then
                call dm_dummy_request(request, observ%timestamp, name='dummy-' // dm_itoa(i), value=value)
            else
                call dm_dummy_request(request, observ%timestamp, nresponses=REQUEST_MAX_NRESPONSES)
            end if

            rc = dm_observ_add_request(observ, request)
        end do
    end subroutine dm_dummy_observ

    impure elemental subroutine dm_dummy_request(request, timestamp, nresponses, name, value)
        !! Generates dummy request data type.
        type(request_type), intent(out)          :: request    !! Request type.
        character(len=*),   intent(in), optional :: timestamp  !! Request timestamp (ISO 8601).
        integer,            intent(in), optional :: nresponses !! Number of responses.
        character(len=*),   intent(in), optional :: name       !! Response name.
        real(kind=r8),      intent(in), optional :: value      !! Response value.

        integer             :: i, n, rc
        type(response_type) :: response

        n = 1
        request = request_type(timestamp  = dm_time_now(), &
                               request    = 'dummy', &
                               response   = dm_ascii_escape('999.999' // ASCII_CR // ASCII_LF), &
                               delimiter  = dm_ascii_escape(ASCII_CR // ASCII_LF), &
                               pattern    = '^(?<dummy>.*)$', &
                               delay      = 1000, &
                               retries    = 0, &
                               timeout    = 500, &
                               error      = 0)

        if (present(timestamp)) request%timestamp = timestamp
        if (present(nresponses)) n = max(0, min(REQUEST_MAX_NRESPONSES, nresponses))

        do i = 1, n
            response = response_type('dummy-' // dm_itoa(i), 'none', RESPONSE_TYPE_REAL64, E_NONE, 999.999_r8)
            if (present(name)) response%name = name
            if (present(value)) response%value = value
            rc = dm_request_add(request, response)
        end do
    end subroutine dm_dummy_request

    pure elemental subroutine dm_dummy_sensor(sensor, node_id, id, name)
        !! Generates dummy sensor data type.
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
        sensor%sn   = '00000'
        sensor%meta = 'a dummy sensor'
    end subroutine dm_dummy_sensor

    pure elemental subroutine dm_dummy_target(target, id, name)
        !! Generates dummy target data type.
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
    end subroutine dm_dummy_target
end module dm_dummy
