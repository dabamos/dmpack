! Author:  Philipp Engel
! Licence: ISC
module dm_time
    !! Date and time functions. ISO 8601/RFC 3339 is used as the universal
    !! datetime format.
    use :: dm_ascii
    use :: dm_error
    use :: dm_util
    use :: dm_type
    implicit none (type, external)
    private

    integer,          parameter, public :: TIME_LEN     = 29
    character(len=*), parameter, public :: TIME_DEFAULT = '1970-01-01T00:00:00.000+00:00'

    type, public :: time_delta_type
        !! Time delta type.
        integer :: days = 0
        integer :: hrs  = 0
        integer :: mins = 0
        integer :: secs = 0
    end type time_delta_type

    public :: dm_time_create
    public :: dm_time_delta
    public :: dm_time_from_seconds
    public :: dm_time_mseconds
    public :: dm_time_now
    public :: dm_time_rfc2822
    public :: dm_time_to_seconds
    public :: dm_time_to_string
    public :: dm_time_valid
    public :: dm_time_zone
contains
    pure character(len=TIME_LEN) &
    function dm_time_create(year, month, day, hour, minute, second, msecond, zone) result(str)
        !! Returns a timestamp in ISO 8601/RFC 3339 of the form
        !! `1970-01-01T00:00:00.000+00:00`. Optional argument `zone` sets the
        !! time zone and has to be of the form `[+|-]hh:mm`, for example,
        !! `+00:00` or `-01:00`.
        character(len=*), parameter :: FMT_ISO = &
            '(i4, 2("-", i2.2), "T", 2(i0.2, ":"), i0.2, ".", i0.3, a)'

        integer,          intent(in), optional :: year     !! Year (YYYY).
        integer,          intent(in), optional :: month    !! Month (MM).
        integer,          intent(in), optional :: day      !! Day (DD).
        integer,          intent(in), optional :: hour     !! Hour (hh).
        integer,          intent(in), optional :: minute   !! Minute (mm).
        integer,          intent(in), optional :: second   !! Second (ss).
        integer,          intent(in), optional :: msecond  !! Millisecond (fff).
        character(len=6), intent(in), optional :: zone     !! Timezone ([+|-]hh:mm).

        integer          :: year_, month_, day_
        integer          :: hour_, minute_, second_, msecond_
        character(len=6) :: zone_

        year_    = 1970
        month_   = 1
        day_     = 1
        hour_    = 0
        minute_  = 0
        second_  = 0
        msecond_ = 0
        zone_    = '+00:00'

        if (present(year))    year_    = year
        if (present(month))   month_   = month
        if (present(day))     day_     = day
        if (present(hour))    hour_    = hour
        if (present(minute))  minute_  = minute
        if (present(second))  second_  = second
        if (present(msecond)) msecond_ = msecond
        if (present(zone))    zone_    = zone

        write (str, FMT_ISO) year_, month_, day_, hour_, minute_, second_, msecond_, zone_
    end function dm_time_create

    impure elemental integer function dm_time_delta(time1, time2, delta) result(rc)
        !! Returns the time delta between `time1` and `time2` as `delta` in
        !! seconds.
        character(len=*), intent(in)  :: time1 !! ISO 8601 timestamp.
        character(len=*), intent(in)  :: time2 !! ISO 8601 timestamp.
        integer(kind=i8), intent(out) :: delta !! Time delta in seconds.

        integer          :: m1, m2
        integer          :: tz1, tz2
        integer(kind=i8) :: t1, t2

        delta = 0_i8

        rc = dm_time_to_seconds(time1, t1, m1, tz1)
        if (dm_is_error(rc)) return

        rc = dm_time_to_seconds(time2, t2, m2, tz2)
        if (dm_is_error(rc)) return

        delta = abs((t2 - tz2) - (t1 - tz1) + int((m2 - m1) / 1000.0, kind=i8))
    end function dm_time_delta

    integer(kind=i8) function dm_time_mseconds() result(t)
        !! Returns current time in mseconds as 8-byte integer (Unix epoch).
        use :: unix
        type(c_timespec) :: tp

        t = 0
        if (c_clock_gettime(CLOCK_REALTIME, tp) /= 0) return
        t = (tp%tv_sec * 1000_i8) + (tp%tv_nsec / 1000000_i8)
    end function dm_time_mseconds

    character(len=TIME_LEN) function dm_time_now() result(str)
        !! Returns a timestamp in ISO 8601/RFC 3339 of the form
        !! `1970-01-01T00:00:00.000+00:00`.
        character(len=*), parameter :: FMT_ISO = &
            '(i4, 2("-", i2.2), "T", 2(i0.2, ":"), i0.2, ".", i0.3, a, ":", a)'

        character(len=5) :: zone
        integer          :: dt(8)

        call date_and_time(zone=zone, values=dt)
        write (str, FMT_ISO) dt(1), dt(2), dt(3), dt(5), dt(6), dt(7), &
                             dt(8), zone(1:3), zone(4:5)
    end function dm_time_now

    character(len=31) function dm_time_rfc2822() result(str)
        !! Returns current date and time in RFC 2822 format:
        !!     https://www.ietf.org/rfc/rfc2822.txt
        character(len=3), parameter :: DAYS(7) = &
            [ 'Sun', 'Mon', 'Thu', 'Wed', 'Thu', 'Fri', 'Sat' ]
        character(len=3), parameter :: MONTHS(12)  = &
            [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ]
        character(len=*), parameter :: RFC2822 = &
            '(a, ", " , i0.2, 1x, a, 1x, i4, 1x, i0.2, ":", i0.2, ":", i0.2, 1x, a)'

        character(len=5) :: z
        integer(kind=i8) :: d, dt(8)

        call date_and_time(values=dt, zone=z)
        d = 1 + modulo(dt(1) + int((dt(1) - 1) / 4) - int((dt(1) - 1) / 100) + int((dt(1) - 1) / 400), 7_i8)
        write (str, RFC2822) DAYS(d), dt(3), MONTHS(dt(2)), dt(1), dt(5), dt(6), dt(7), z
    end function dm_time_rfc2822

    integer function dm_time_to_seconds(time, seconds, mseconds, timezone) result(rc)
        !! Converts ISO 8601/RFC 3339 timestamp to Unix timestamp.
        use :: unix, only: c_mktime, c_tm
        character(len=*), parameter :: FMT_ISO = '(i4, 5(1x, i2), 1x, i3, i3, 1x, i2)'

        character(len=*), intent(in)            :: time     !! ISO 8601 timestamp.
        integer(kind=i8), intent(out)           :: seconds  !! Unix timestamp.
        integer,          intent(out), optional :: mseconds !! Milliseconds.
        integer,          intent(out), optional :: timezone !! TZ delta in seconds.

        ! For whatever reason, `m` cannot be named `msec` or `msecs` when using
        ! GNU Fortran. In such a case, `mseconds` will always be 8.
        integer    :: stat
        integer    :: year, month, day
        integer    :: hrs, mins, secs, m
        integer    :: tz, tz_hrs, tz_mins
        type(c_tm) :: tm

        rc = E_INVALID
        seconds = 0_i8

        read (time, FMT_ISO, iostat=stat) year, month, day, &
                                          hrs, mins, secs, m, &
                                          tz_hrs, tz_mins
        if (stat /= 0) return

        tm = c_tm(tm_sec   = secs, &
                  tm_min   = mins, &
                  tm_hour  = hrs, &
                  tm_mday  = day, &
                  tm_mon   = month, &
                  tm_year  = year - 1900, &
                  tm_wday  = 0, &
                  tm_yday  = 0, &
                  tm_isdst = 0)

        seconds = c_mktime(tm)
        tz = (tz_hrs * 3600) + (tz_mins * 60)
        if (present(timezone)) timezone = tz
        if (present(mseconds)) mseconds = m
        rc = E_NONE
    end function dm_time_to_seconds

    function dm_time_to_string(time_delta, days, hrs, mins, secs) result(str)
        !! Converts `time_delta_type` to string of format
        !! `[d days ][h hours ][m mins ][s secs]`.
        type(time_delta_type), intent(inout)        :: time_delta !! Time delta type.
        logical,               intent(in), optional :: days       !! Write days (default: true).
        logical,               intent(in), optional :: hrs        !! Write hours (default: true).
        logical,               intent(in), optional :: mins       !! Write minutes (default: true).
        logical,               intent(in), optional :: secs       !! Write seconds (default: true).

        character(len=:), allocatable :: str
        logical                       :: days_, hrs_, mins_, secs_

        days_ = .true.
        hrs_  = .true.
        mins_ = .true.
        secs_ = .true.

        if (present(days)) days_ = days
        if (present(hrs))  hrs_  = hrs
        if (present(mins)) mins_ = mins
        if (present(secs)) secs_ = secs

        str = ''
        if (days_) str = str // dm_itoa(time_delta%days) // ' days '
        if (hrs_)  str = str // dm_itoa(time_delta%hrs)  // ' hours '
        if (mins_) str = str // dm_itoa(time_delta%mins) // ' mins '
        if (secs_) str = str // dm_itoa(time_delta%secs) // ' secs'
    end function dm_time_to_string

    pure logical function dm_time_valid(time) result(valid)
        !! Returns `.true.` if given timestamp follows the form of ISO 8601. The
        !! timestamp does not have to be complete to be valid. The minimum
        !! length of a timestamp to be valid is 4.
        character(len=*), intent(in) :: time !! Timestamp to validate.

        character :: a
        integer   :: i, n

        valid = .false.

        n = len_trim(time)
        if (n < 4 .or. n > 29) return

        do i = 1, n
            a = time(i:i)

            select case (i)
                case (1:4)
                case (6:7)
                case (9:10)
                case (12:13)
                case (15:16)
                case (18:19)
                case (21:23)
                case (25:26)
                case (28:29)
                    if (.not. dm_ascii_is_digit(a)) return
                case (5)
                case (8)
                    if (a /= '-') return
                case (11)
                    if (a /= 'T') return
                case (14)
                case (17)
                case (27)
                    if (a /= ':') return
                case (20)
                    if (a /= '.') return
                case (24)
                    if (a /= '+' .and. a /= '-') return
            end select
        end do

        valid = .true.
    end function dm_time_valid

    character(len=5) function dm_time_zone() result(zone)
        !! Returns current time zone.
        call date_and_time(zone=zone)
    end function dm_time_zone

    pure elemental subroutine dm_time_from_seconds(time_delta, seconds)
        !! Returns time delta type from Unix timestamp in seconds.
        type(time_delta_type), intent(out) :: time_delta !! Time delta type.
        integer(kind=i8),      intent(in)  :: seconds    !! Unix timestamp.
        integer(kind=i8)                   :: t

        t = seconds
        time_delta%days = int(t / 86400)
        t = modulo(t, 86400_i8)
        time_delta%hrs  = int(t / 3600)
        t = modulo(t, 3600_i8)
        time_delta%mins = int(t / 60)
        time_delta%secs = int(modulo(t, 60_i8))
    end subroutine dm_time_from_seconds
end module dm_time
