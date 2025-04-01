! Author:  Philipp Engel
! Licence: ISC
module dm_api_status
    !! HTTP-RPC API status message.
    use :: dm_ascii, NL => ASCII_LF
    use :: dm_error
    use :: dm_time
    use :: dm_util
    use :: dm_version
    implicit none (type, external)
    private

    integer, parameter, public :: API_STATUS_NKEYS = 7  !! Number of attributes in derived type.
    integer, parameter, public :: API_STATUS_LEN   = 32 !! Max. length of attribute values.

    type, public :: api_status_type
        !! API status type that stores an HTTP-RPC API response.
        character(len=API_STATUS_LEN) :: version   = ' '               !! Server application version.
        character(len=API_STATUS_LEN) :: dmpack    = DM_VERSION_STRING !! Server library version.
        character(len=API_STATUS_LEN) :: host      = ' '               !! Server host name.
        character(len=API_STATUS_LEN) :: server    = ' '               !! Server software (web server).
        character(len=API_STATUS_LEN) :: timestamp = TIME_DEFAULT      !! Server date and time in ISO 8601.
        character(len=API_STATUS_LEN) :: message   = ' '               !! Status message.
        integer                       :: error     = E_NONE            !! Error code.
    end type api_status_type

    interface operator (==)
        !! Returns whether api status types are equal.
        module procedure :: dm_api_status_equals
    end interface

    public :: operator (==)

    public :: dm_api_status_from_string
    public :: dm_api_status_equals
    public :: dm_api_status_set
    public :: dm_api_status_to_string
contains
    integer function dm_api_status_from_string(string, status) result(rc)
        !! Reads API status type from given string. Only keys found in the
        !! string are overwritten in the derived type. No error is returned if
        !! the string does not contain any of the keys.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the string is empty.
        !! * `E_FORMAT` if the string format is invalid.
        !!
        use :: dm_string

        integer, parameter :: LINE_LEN = 1 + (API_STATUS_LEN * 2)

        character(len=*),      intent(in)  :: string !! String representation of API status.
        type(api_status_type), intent(out) :: status !! Result.

        integer                       :: i, nlines, npairs
        character(len=LINE_LEN)       :: lines(API_STATUS_NKEYS)
        character(len=API_STATUS_LEN) :: pairs(2), key, value

        rc = E_EMPTY
        if (len_trim(string) == 0) return

        rc = E_FORMAT
        call dm_string_split(string, lines, del=NL, n=nlines)
        if (nlines == 0) return

        do i = 1, nlines
            call dm_string_split(lines(i), pairs, del='=', n=npairs)
            if (npairs /= 2) exit

            key   = adjustl(pairs(1))
            value = adjustl(pairs(2))

            call dm_lower(key)

            select case (key)
                case ('version');   status%version   = dm_ascii_escape(value)
                case ('dmpack');    status%dmpack    = dm_ascii_escape(value)
                case ('host');      status%host      = dm_ascii_escape(value)
                case ('server');    status%server    = dm_ascii_escape(value)
                case ('timestamp'); status%timestamp = dm_ascii_escape(value)
                case ('message');   status%message   = dm_ascii_escape(value)
                case ('error');     status%error     = dm_atoi(value)
                case default;       cycle
            end select
        end do

        rc = E_NONE
    end function dm_api_status_from_string

    pure elemental logical function dm_api_status_equals(status1, status2) result(equals)
        !! Returns `.true.` if given API status types are equal.
        type(api_status_type), intent(in) :: status1 !! The first status type.
        type(api_status_type), intent(in) :: status2 !! The second status type.

        equals = .false.
        if (status1%version   /= status2%version)   return
        if (status1%dmpack    /= status2%dmpack)    return
        if (status1%host      /= status2%host)      return
        if (status1%server    /= status2%server)    return
        if (status1%timestamp /= status2%timestamp) return
        if (status1%message   /= status2%message)   return
        if (status1%error     /= status2%error)     return
        equals = .true.
    end function dm_api_status_equals

    function dm_api_status_to_string(status) result(string)
        !! Returns string representation of given API status type. The string
        !! contains new-line characters.
        type(api_status_type), intent(inout) :: status !! API status type.
        character(len=:), allocatable        :: string !! String representation.

        string = 'version='   // trim(status%version) // NL // &
                 'dmpack='    // trim(status%dmpack)  // NL // &
                 'host='      // trim(status%host)    // NL // &
                 'server='    // trim(status%server)  // NL // &
                 'timestamp=' // trim(status%timestamp)

        if (len_trim(status%message) > 0) then
            string = string // NL // 'message=' // trim(status%message)
        end if

        string = string // NL // 'error=' // dm_itoa(status%error)
    end function dm_api_status_to_string

    subroutine dm_api_status_set(status, version, dmpack, host, server, timestamp, message, error)
        !! Sets attributes of API status type. This routine does not validate
        !! the arguments.
        type(api_status_type), intent(inout)   :: status    !! API status type.
        character(len=*), intent(in), optional :: version   !! Server application version.
        character(len=*), intent(in), optional :: dmpack    !! Server library version.
        character(len=*), intent(in), optional :: host      !! Server host name.
        character(len=*), intent(in), optional :: server    !! Server software (web server).
        character(len=*), intent(in), optional :: timestamp !! Server date and time in ISO 8601.
        character(len=*), intent(in), optional :: message   !! Status message.
        integer,          intent(in), optional :: error     !! Error code.

        if (present(version))   status%version   = version
        if (present(dmpack))    status%dmpack    = dmpack
        if (present(host))      status%host      = host
        if (present(server))    status%server    = server
        if (present(timestamp)) status%timestamp = timestamp
        if (present(message))   status%message   = message
        if (present(error))     status%error     = error
    end subroutine dm_api_status_set
end module dm_api_status
