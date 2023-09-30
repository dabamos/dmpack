! Author:  Philipp Engel
! Licence: ISC
module dm_mqtt
    !! Module for publishing messages via MQTT, using libcurl.
    !!
    !! Limitations of libcurl:
    !!
    !! * Only QoS level 0 is implemented for publish.
    !! * No way to set retain flag for publish.
    !! * No TLS (mqtts) support.
    !! * Naive EAGAIN handling will not handle split messages.
    !!
    !! An MQTT server must be running, such as Mosquitto. On FreeBSD install
    !! Mosquitto with:
    !!
    !! ```
    !! # pkg install net/mosquitto
    !! ```
    !!
    !! Start the service locally:
    !!
    !! ```
    !! # service mosquitto onestart
    !! ```
    !!
    !! Subscribe topic `/fortran`:
    !!
    !! ```
    !! # mosquitto_sub -h 127.0.0.1 -t /fortran
    !! ```
    !!
    !! In Fortran, we can then create the URL of the topic, and publish a
    !! message to it:
    !!
    !! ```fortran
    !! character(len=:), allocatable :: url
    !! integer :: rc
    !!
    !! rc  = dm_mqtt_init()
    !! url = dm_mqtt_url(host='127.0.0.1', topic='/fortran', port=1883)
    !! rc  = dm_mqtt_publish(url, 'Hello, from Fortran!')
    !! call dm_mqtt_destroy()
    !! ```
    !!
    !! Any client that has subscribed topic `/fortran` will receive the
    !! message.
    !!
    !! The procedures `dm_mqtt_init()` and `dm_mqtt_destroy()` have to be called
    !! once per process and only if either the RPC or the mail backend was not
    !! initialised already.
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    public :: dm_mqtt_destroy
    public :: dm_mqtt_init
    public :: dm_mqtt_publish
    public :: dm_mqtt_url
contains
    integer function dm_mqtt_init() result(rc)
        !! Initialises libcurl backend.
        rc = E_MQTT
        if (curl_global_init(CURL_GLOBAL_DEFAULT) /= CURLE_OK) return
        rc = E_NONE
    end function dm_mqtt_init

    integer function dm_mqtt_publish(url, message, timeout, error_message, error_curl) result(rc)
        !! Sends HTTP request by calling libcurl.
        character(len=*),              intent(in)            :: url           !! URL to MQTT server/topic.
        character(len=*), target,      intent(in)            :: message       !! Message to publish.
        integer,                       intent(in),  optional :: timeout       !! Connection timeout.
        character(len=:), allocatable, intent(out), optional :: error_message !! cURL error message.
        integer,                       intent(out), optional :: error_curl    !! cURL error code.

        integer     :: er
        type(c_ptr) :: curl_ptr

        rc = E_IO

        if (present(error_message)) error_message = ''
        if (present(error_curl)) error_curl = CURLE_OK

        curl_ptr = curl_easy_init()
        if (.not. c_associated(curl_ptr)) return

        curl_block: block
            ! Prepare request.
            rc = E_INVALID

            ! Set URL.
            er = curl_easy_setopt(curl_ptr, CURLOPT_URL, url)
            if (er /= CURLE_OK) exit curl_block

            ! Enable POST.
            er = curl_easy_setopt(curl_ptr, CURLOPT_POST, 1)
            if (er /= CURLE_OK) exit curl_block

            ! Pass POST data directly.
            er = curl_easy_setopt(curl_ptr, CURLOPT_POSTFIELDSIZE, len(message, kind=i8))
            if (er /= CURLE_OK) exit curl_block

            er = curl_easy_setopt(curl_ptr, CURLOPT_POSTFIELDS, c_loc(message))
            if (er /= CURLE_OK) exit curl_block

            ! Set connection timeout.
            if (present(timeout)) then
                er = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT, timeout)
                if (er /= CURLE_OK) exit curl_block
            end if

            ! No output.
            er = curl_easy_setopt(curl_ptr, CURLOPT_NOSIGNAL, 1)
            if (er /= CURLE_OK) exit curl_block

            er = curl_easy_setopt(curl_ptr, CURLOPT_NOPROGRESS, 1)
            if (er /= CURLE_OK) exit curl_block

            ! Send request.
            rc = E_MQTT
            er = curl_easy_perform(curl_ptr)
            if (er /= CURLE_OK) exit curl_block

            rc = E_NONE
        end block curl_block

        ! Get error message.
        if (present(error_message) .and. er /= CURLE_OK) then
            error_message = curl_easy_strerror(er)
        end if

        if (present(error_curl)) error_curl = er

        call curl_easy_cleanup(curl_ptr)
    end function dm_mqtt_publish

    function dm_mqtt_url(host, topic, port) result(url)
        !! Returns allocatable string of URL to MQTT server. Uses the URL API
        !! of libcurl to create the URL. If `port` is `0`, the default port
        !! will be used. The topic must start with a `/`.
        !!
        !! On error, an empty string is returned.
        character(len=*), intent(in)           :: host  !! IP or FQDN of MQTT server.
        character(len=*), intent(in)           :: topic !! MQTT topic.
        integer,          intent(in), optional :: port  !! MQTT server port (1883 by default).
        character(len=:), allocatable          :: url   !! Created URL.

        character(len=5) :: str
        integer          :: port_
        integer          :: stat
        type(c_ptr)      :: ptr

        port_ = 0
        if (present(port)) port_ = port

        url_block: block
            ptr = curl_url()
            if (.not. c_associated(ptr)) exit url_block

            ! URL scheme.
            stat = curl_url_set(ptr, CURLUPART_SCHEME, 'mqtt')
            if (stat /= CURLUE_OK) exit url_block

            ! URL host.
            stat = curl_url_set(ptr, CURLUPART_HOST, trim(host))
            if (stat /= CURLUE_OK) exit url_block

            ! URL port.
            if (port_ > 0) then
                write (str, '(i0)', iostat=stat) port_
                stat = curl_url_set(ptr, CURLUPART_PORT, trim(str))
                if (stat /= CURLUE_OK) exit url_block
            end if

            ! URL topic.
            stat = curl_url_set(ptr, CURLUPART_PATH, trim(topic))
            if (stat /= CURLUE_OK) exit url_block

            ! Get full URL.
            stat = curl_url_get(ptr, CURLUPART_URL, url)
        end block url_block

        if (c_associated(ptr)) call curl_url_cleanup(ptr)
        if (.not. allocated(url)) url = ''
    end function dm_mqtt_url

    subroutine dm_mqtt_destroy()
        !! Cleans-up libcurl.
        call curl_global_cleanup()
    end subroutine dm_mqtt_destroy
end module dm_mqtt
