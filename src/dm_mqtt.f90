! Author:  Philipp Engel
! Licence: ISC
module dm_mqtt
    !! Module for publishing messages via MQTT, using libcurl. The libcurl
    !! library must have been built with the MQTT option enabled.
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
    !! In Fortran, we then create the URL of the topic `/fortran` on host
    !! `127.0.0.1`, and publish the message:
    !!
    !! ```fortran
    !! character(len=:), allocatable :: url
    !! integer :: rc
    !!
    !! rc  = dm_mqtt_init()
    !! url = dm_mqtt_url(host='127.0.0.1', topic='/fortran', port=1883)
    !! rc  = dm_mqtt_publish(url, 'Hello, from Fortran!')
    !! call dm_mqtt_shutdown()
    !! ```
    !!
    !! Any client subscribing topic `/fortran` will receive the message.
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    public :: dm_mqtt_init
    public :: dm_mqtt_publish
    public :: dm_mqtt_url
    public :: dm_mqtt_shutdown
contains
    integer function dm_mqtt_init() result(rc)
        !! Initialises MQTT backend.
        rc = E_MQTT
        if (curl_global_init(CURL_GLOBAL_DEFAULT) == CURLE_OK) rc = E_NONE
    end function dm_mqtt_init

    integer function dm_mqtt_publish(url, message, timeout, error_message, error_curl) result(rc)
        !! Publishes MQTT message `message` on topic with address `url` by
        !! calling libcurl.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_COMPILER` if C pointer could not be nullified (compiler bug).
        !! * `E_INVALID` if URL is invalid.
        !! * `E_MQTT` if publishing the message failed.
        !!
        character(len=*),              intent(in)            :: url           !! URL to MQTT server/topic.
        character(len=*), target,      intent(in)            :: message       !! Message to publish.
        integer,                       intent(in),  optional :: timeout       !! Connection timeout.
        character(len=:), allocatable, intent(out), optional :: error_message !! cURL error message.
        integer,                       intent(out), optional :: error_curl    !! cURL error code.

        integer     :: stat
        type(c_ptr) :: curl_ctx

        if (present(error_message)) error_message = ''
        if (present(error_curl))    error_curl    = CURLE_OK

        rc = E_MQTT
        curl_ctx = curl_easy_init()
        if (.not. c_associated(curl_ctx)) return

        rc = E_INVALID
        if (len_trim(url) == 0) return

        curl_block: block
            integer(kind=i8) :: n
            type(c_ptr)      :: ptr

            n   = len(message, kind=i8)
            ptr = c_loc(message)

            ! Prepare request.
            stat = curl_easy_setopt(curl_ctx, CURLOPT_URL,           url); if (stat /= CURLE_OK) exit curl_block ! Set URL.
            stat = curl_easy_setopt(curl_ctx, CURLOPT_POST,          1);   if (stat /= CURLE_OK) exit curl_block ! Enable POST.
            stat = curl_easy_setopt(curl_ctx, CURLOPT_POSTFIELDSIZE, n);   if (stat /= CURLE_OK) exit curl_block ! Payload size.
            stat = curl_easy_setopt(curl_ctx, CURLOPT_POSTFIELDS,    ptr); if (stat /= CURLE_OK) exit curl_block ! Pass payload directly.

            ! Set connection timeout.
            if (present(timeout)) then
                stat = curl_easy_setopt(curl_ctx, CURLOPT_CONNECTTIMEOUT, timeout)
                if (stat /= CURLE_OK) exit curl_block
            end if

            ! Set verbosity.
            stat = curl_easy_setopt(curl_ctx, CURLOPT_NOSIGNAL,   1); if (stat /= CURLE_OK) exit curl_block ! No output.
            stat = curl_easy_setopt(curl_ctx, CURLOPT_NOPROGRESS, 1); if (stat /= CURLE_OK) exit curl_block ! No progress output.

            ! Send request.
            rc = E_MQTT
            stat = curl_easy_perform(curl_ctx); if (stat /= CURLE_OK) exit curl_block

            rc = E_NONE
        end block curl_block

        ! Get error message.
        if (present(error_message) .and. stat /= CURLE_OK) then
            error_message = curl_easy_strerror(stat)
        end if
        if (present(error_curl)) error_curl = stat

        call curl_easy_cleanup(curl_ctx)
        if (dm_is_error(rc)) return
        if (c_associated(curl_ctx)) rc = E_COMPILER
    end function dm_mqtt_publish

    function dm_mqtt_url(host, topic, port) result(url)
        !! Returns allocatable string of URL to MQTT server. Uses the URL API
        !! of libcurl to create the URL. If `port` is `0`, the default port
        !! will be used. The topic must start with character `/`.
        !!
        !! On error, an empty string is returned.
        character(len=*), intent(in)           :: host  !! IP or FQDN of MQTT server.
        character(len=*), intent(in)           :: topic !! MQTT topic.
        integer,          intent(in), optional :: port  !! MQTT server port (1883 by default).
        character(len=:), allocatable          :: url   !! Created URL.

        integer     :: port_
        integer     :: stat
        type(c_ptr) :: ptr

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
                stat = curl_url_set(ptr, CURLUPART_PORT, dm_itoa(port_))
                if (stat /= CURLUE_OK) exit url_block
            end if

            ! URL topic.
            stat = curl_url_set(ptr, CURLUPART_PATH, trim(topic))
            if (stat /= CURLUE_OK) exit url_block

            ! Get full URL.
            stat = curl_url_get(ptr, CURLUPART_URL, url)
        end block url_block

        call curl_url_cleanup(ptr)
        if (.not. allocated(url)) url = ''
    end function dm_mqtt_url

    subroutine dm_mqtt_shutdown()
        !! Cleans up MQTT backend.
        call curl_global_cleanup()
    end subroutine dm_mqtt_shutdown
end module dm_mqtt
