! Author:  Philipp Engel
! Licence: ISC
module dm_api
    !! HTTP-RPC API status message.
    use :: dm_ascii, NL => ASCII_LF
    use :: dm_error
    use :: dm_string
    use :: dm_time
    use :: dm_util
    use :: dm_version
    implicit none (type, external)
    private

    integer, parameter, public :: API_STATUS_NKEYS = 7  !! Number of elements in derived type.
    integer, parameter, public :: API_STATUS_LEN   = 32 !! Max. length of element values.

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
    public :: dm_api_status_to_string
contains
    integer function dm_api_status_from_string(api, string) result(rc)
        !! Reads API status type from given string. Only keys found in the
        !! string are overwritten in the derived type. No error is returned if
        !! the string does not contain any of the keys. The function return
        !! `E_EMPTY` if the passed string is empty.
        integer, parameter :: LINE_LEN = 1 + (API_STATUS_LEN * 2)

        type(api_status_type), intent(out) :: api    !! Result.
        character(len=*),      intent(in)  :: string !! String representation of API status.

        integer                       :: i, n, nlines
        character(len=LINE_LEN)       :: lines(API_STATUS_NKEYS)
        character(len=API_STATUS_LEN) :: pair(2)

        rc = E_EMPTY
        if (len_trim(string) == 0) return

        call dm_string_split(string, lines, del=NL, n=nlines)
        if (nlines == 0) return

        do i = 1, nlines
            call dm_string_split(lines(i), pair, del='=', n=n)
            if (n /= 2) exit

            select case (pair(1))
                case ('version')
                    api%version = dm_ascii_escape(pair(2))
                case ('dmpack')
                    api%dmpack = dm_ascii_escape(pair(2))
                case ('host')
                    api%host = dm_ascii_escape(pair(2))
                case ('server')
                    api%server = dm_ascii_escape(pair(2))
                case ('timestamp')
                    api%timestamp = dm_ascii_escape(pair(2))
                case ('message')
                    api%message= dm_ascii_escape(pair(2))
                case ('error')
                    api%error = dm_atoi(pair(2))
                case default
                    cycle
            end select
        end do

        rc = E_NONE
    end function dm_api_status_from_string

    pure elemental logical function dm_api_status_equals(api1, api2) result(equals)
        !! Returns `.true.` if given API status types are equal.
        type(api_status_type), intent(in) :: api1 !! The first status type.
        type(api_status_type), intent(in) :: api2 !! The second status type.

        equals = .false.
        if (api1%version   /= api2%version)   return
        if (api1%dmpack    /= api2%dmpack)    return
        if (api1%host      /= api2%host)      return
        if (api1%server    /= api2%server)    return
        if (api1%timestamp /= api2%timestamp) return
        if (api1%message   /= api2%message)   return
        if (api1%error     /= api2%error)     return
        equals = .true.
    end function dm_api_status_equals

    function dm_api_status_to_string(api) result(str)
        !! Returns string representation of given API status type.
        type(api_status_type), intent(inout) :: api !! API status type.
        character(len=:), allocatable        :: str !! String representation.

        str = 'version='   // trim(api%version) // NL // &
              'dmpack='    // trim(api%dmpack)  // NL // &
              'host='      // trim(api%host)    // NL // &
              'server='    // trim(api%server)  // NL // &
              'timestamp=' // trim(api%timestamp)

        if (len_trim(api%message) > 0) then
            str = str // NL // 'message=' // trim(api%message)
        end if

        str = str // NL // 'error=' // dm_itoa(api%error)
    end function dm_api_status_to_string
end module dm_api
