! Author:  Philipp Engel
! Licence: ISC
module dm_mqtt
    !! Module for publishing messages via MQTT, using cURL.
    !!
    !! Limitations of cURL:
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
    !! In Fortran, we first have to create the URL of the topic we want to send
    !! a message to:
    !!
    !! ```fortran
    !! character(len=:), allocatable :: url
    !! integer :: rc
    !!
    !! url = dm_mqtt_url(host='127.0.0.1', port=1883, topic='/fortran')
    !! rc = dm_mqtt_publish(url, 'Hello, from Fortran!')
    !! ```
    !!
    !! Any client that has subscribed topic `/fortran` will receive the
    !! message.
    use, intrinsic :: iso_c_binding
    use :: curl
    use :: dm_error
    use :: dm_type
    use :: dm_util
    implicit none (type, external)
    private

    public :: dm_mqtt_publish
    public :: dm_mqtt_url
contains
    integer function dm_mqtt_publish(url, message, timeout, error_message, error_curl) result(rc)
        !! Sends HTTP request by calling libcurl.
        character(len=*),              intent(in)            :: url           !! URL to MQTT server/topic.
        character(len=*), target,      intent(in)            :: message       !! Message to publish.
        integer,                       intent(in),  optional :: timeout       !! Connection timeout.
        character(len=:), allocatable, intent(out), optional :: error_message !! cURL error message.
        integer,                       intent(out), optional :: error_curl    !! cURL error code.

        integer     :: ce
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
            ce = curl_easy_setopt(curl_ptr, CURLOPT_URL, url)
            if (ce /= CURLE_OK) exit curl_block

            ! Enable POST.
            ce = curl_easy_setopt(curl_ptr, CURLOPT_POST, 1)
            if (ce /= CURLE_OK) exit curl_block

            ! Pass POST data directly.
            ce = curl_easy_setopt(curl_ptr, CURLOPT_POSTFIELDSIZE, len(message))
            if (ce /= CURLE_OK) exit curl_block

            ce = curl_easy_setopt(curl_ptr, CURLOPT_POSTFIELDS, c_loc(message))
            if (ce /= CURLE_OK) exit curl_block

            ! Set connection timeout.
            if (present(timeout)) then
                ce = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT, timeout)
                if (ce /= CURLE_OK) exit curl_block
            end if

            ! No output.
            ce = curl_easy_setopt(curl_ptr, CURLOPT_NOSIGNAL, 1)
            if (ce /= CURLE_OK) exit curl_block

            ce = curl_easy_setopt(curl_ptr, CURLOPT_NOPROGRESS, 1)
            if (ce /= CURLE_OK) exit curl_block

            ! Send request.
            ce = curl_easy_perform(curl_ptr)
            if (ce /= CURLE_OK) exit curl_block

            rc = E_NONE
        end block curl_block

        ! Get error message.
        if (present(error_message) .and. ce /= CURLE_OK) then
            error_message = curl_easy_strerror(ce)
        end if

        if (present(error_curl)) error_curl = ce

        call curl_easy_cleanup(curl_ptr)
    end function dm_mqtt_publish

    pure function dm_mqtt_url(host, port, topic) result(url)
        !! Returns allocatable string of URL to MQTT server.
        !! If `port` is `0`, the default port will be used.
        character(len=*), intent(in)  :: host  !! IP or FQDN of MQTT server.
        integer,          intent(in)  :: port  !! MQTT server port (1883 by default).
        character(len=*), intent(in)  :: topic !! MQTT topic.
        character(len=:), allocatable :: url   !! Created URL.

        integer :: n, m

        url = 'mqtt://'
        n = len_trim(host)
        if (n == 0) return

        url = 'mqtt://' // host(1:n)
        if (port > 0) url = url // ':' // dm_itoa(port)

        m = len_trim(topic)
        if (m == 0) return

        if (host(n:n) /= '/' .and. topic(1:1) /= '/') url = url // '/'
        url = url // topic(1:m)
    end function dm_mqtt_url
end module dm_mqtt
