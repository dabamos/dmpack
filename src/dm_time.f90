! Author:  Philipp Engel
! Licence: ISC
module dm_time
    !! Date and time functions. ISO 8601/RFC 3339 is used as the universal
    !! datetime format.
    use :: dm_error
    use :: dm_util
    use :: dm_type
    implicit none (type, external)
    private

    integer,          parameter, public :: TIME_LEN     = 29                              !! ISO 8601 length.
    character(len=*), parameter, public :: TIME_DEFAULT = '1970-01-01T00:00:00.000+00:00' !! Default ISO 8601.

    type, public :: time_delta_type
        !! Time delta type to store elapsed time.
        integer :: days    = 0 !! Bygone days.
        integer :: hours   = 0 !! Bygone hours.
        integer :: minutes = 0 !! Bygone minutes.
        integer :: seconds = 0 !! Bygone seconds.
    end type time_delta_type

    public :: dm_time_create
    public :: dm_time_delta_from_seconds
    public :: dm_time_delta_to_string
    public :: dm_time_diff
    public :: dm_time_from_unix
    public :: dm_time_mseconds
    public :: dm_time_now
    public :: dm_time_rfc2822
    public :: dm_time_strings
    public :: dm_time_to_beats
    public :: dm_time_to_unix
    public :: dm_time_valid
    public :: dm_time_zone
contains
    pure elemental character(len=TIME_LEN) &
    function dm_time_create(year, month, day, hour, minute, second, msecond, zone) result(str)
        !! Returns a timestamp in ISO 8601/RFC 3339 of the form
        !! `1970-01-01T00:00:00.000+00:00`. Optional argument `zone` sets the
        !! time zone and has to be of the form `[+|-]hh:mm`, for example,
        !! `+00:00` or `-01:00`.
        character(len=*), parameter :: FMT_ISO = &
            '(i4, 2("-", i0.2), "T", 2(i0.2, ":"), i0.2, ".", i0.3, a)'

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

    pure elemental subroutine dm_time_delta_from_seconds(time_delta, seconds)
        !! Returns time delta type from Unix timestamp in seconds.
        type(time_delta_type), intent(out) :: time_delta !! Time delta type.
        integer(kind=i8),      intent(in)  :: seconds    !! Unix timestamp.

        integer(kind=i8) :: t

        t = seconds
        time_delta%days = int(t / 86400)
        t = modulo(t, 86400_i8)
        time_delta%hours  = int(t / 3600)
        t = modulo(t, 3600_i8)
        time_delta%minutes = int(t / 60)
        time_delta%seconds = int(modulo(t, 60_i8))
    end subroutine dm_time_delta_from_seconds

    function dm_time_delta_to_string(time_delta, days, hours, minutes, seconds) result(str)
        !! Converts `time_delta_type` to string of format
        !! `[d days ][h hours ][m mins ][s secs]`.
        type(time_delta_type), intent(inout)        :: time_delta !! Time delta type.
        logical,               intent(in), optional :: days       !! Write days (`.true.` by default).
        logical,               intent(in), optional :: hours      !! Write hours (`.true.` by default).
        logical,               intent(in), optional :: minutes    !! Write minutes (`.true.` by default).
        logical,               intent(in), optional :: seconds    !! Write seconds (`.true.` by default).

        character(len=:), allocatable :: str
        logical                       :: days_, hours_, minutes_, seconds_

        days_    = .true.
        hours_   = .true.
        minutes_ = .true.
        seconds_ = .true.

        if (present(days))    days_    = days
        if (present(hours))   hours_   = hours
        if (present(minutes)) minutes_ = minutes
        if (present(seconds)) seconds_ = seconds

        str = ''
        if (days_)    str = str // dm_itoa(time_delta%days) // ' days '
        if (hours_)   str = str // dm_itoa(time_delta%hours) // ' hours '
        if (minutes_) str = str // dm_itoa(time_delta%minutes) // ' mins '
        if (seconds_) str = str // dm_itoa(time_delta%seconds) // ' secs'
    end function dm_time_delta_to_string

    impure elemental integer function dm_time_diff(time1, time2, diff) result(rc)
        !! Returns the time delta between `time1` and `time2` as `diff` in
        !! seconds.
        character(len=*), intent(in)  :: time1 !! ISO 8601 timestamp.
        character(len=*), intent(in)  :: time2 !! ISO 8601 timestamp.
        integer(kind=i8), intent(out) :: diff  !! Time delta in seconds.

        integer          :: m1, m2
        integer(kind=i8) :: t1, t2

        diff = 0_i8

        rc = dm_time_to_unix(time1, t1, m1)
        if (dm_is_error(rc)) return

        rc = dm_time_to_unix(time2, t2, m2)
        if (dm_is_error(rc)) return

        diff = abs(t2 - t1) + int((m2 - m1) / 1000.0, kind=i8)
    end function dm_time_diff

    impure elemental integer function dm_time_from_unix(epoch, year, month, day, hour, minute, second) result(rc)
        !! Converts the calendar time `epoch` in UTC to broken-down time
        !! representation.
        !!
        !! The argument `epoch` is the number of seconds elapsed since the
        !! Epoch, 1970-01-01 00:00:00 +0000 (UTC).
        !!
        !! The function calls `gmtime_r()` internally (SUSv2).
        use :: unix
        integer(kind=i8), intent(in)            :: epoch  !! Unix timestamp in seconds (UTC).
        integer,          intent(out), optional :: year   !! Year part of timestamp.
        integer,          intent(out), optional :: month  !! Month part of timestamp.
        integer,          intent(out), optional :: day    !! Day part of timestamp.
        integer,          intent(out), optional :: hour   !! Hour part of timestamp.
        integer,          intent(out), optional :: minute !! Minute part of timestamp.
        integer,          intent(out), optional :: second !! Second part of timestamp.

        type(c_ptr) :: ptr
        type(c_tm)  :: tm

        rc = E_SYSTEM

        ptr = c_gmtime_r(int(epoch, kind=c_time_t), tm)
        if (.not. c_associated(ptr)) return

        if (present(year))   year   = tm%tm_year + 1900
        if (present(month))  month  = tm%tm_mon + 1
        if (present(day))    day    = tm%tm_mday
        if (present(hour))   hour   = tm%tm_hour
        if (present(minute)) minute = tm%tm_min
        if (present(second)) second = tm%tm_sec

        rc = E_NONE
    end function dm_time_from_unix

    integer(kind=i8) function dm_time_mseconds() result(mseconds)
        !! Returns current time in mseconds as 8-byte integer (Unix Epoch).
        use :: unix, only: CLOCK_REALTIME, c_clock_gettime, c_timespec
        type(c_timespec) :: tp

        mseconds = 0
        if (c_clock_gettime(CLOCK_REALTIME, tp) /= 0) return
        mseconds = (tp%tv_sec * 1000_i8) + (tp%tv_nsec / 1000000_i8)
    end function dm_time_mseconds

    character(len=TIME_LEN) function dm_time_now() result(str)
        !! Returns current date and time in ISO 8601/RFC 3339 format
        !! (`1970-01-01T00:00:00.000+00:00`).
        character(len=*), parameter :: FMT_ISO = &
            '(i0.4, 2("-", i0.2), "T", 2(i0.2, ":"), i0.2, ".", i0.3, a, ":", a)'

        character(len=5) :: zone
        integer          :: dt(8)

        call date_and_time(zone=zone, values=dt)
        write (str, FMT_ISO) dt(1), dt(2), dt(3), dt(5), dt(6), dt(7), &
                             dt(8), zone(1:3), zone(4:5)
    end function dm_time_now

    character(len=31) function dm_time_rfc2822() result(str)
        !! Returns current date and time in
        !! [RFC 2822](https://www.ietf.org/rfc/rfc2822.txt) format.
        character(len=3), parameter :: &
            DAYS(7) = [ 'Sun', 'Mon', 'Thu', 'Wed', 'Thu', 'Fri', 'Sat' ]
        character(len=3), parameter :: &
            MONTHS(12) = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ]
        character(len=*), parameter :: &
            RFC2822 = '(a, ", " , i0.2, 1x, a, 1x, i4, 1x, i0.2, ":", i0.2, ":", i0.2, 1x, a)'

        character(len=5) :: z
        integer(kind=i8) :: d, dt(8)

        call date_and_time(values=dt, zone=z)
        d = 1 + modulo(dt(1) + int((dt(1) - 1) / 4) - int((dt(1) - 1) / 100) + int((dt(1) - 1) / 400), 7_i8)
        write (str, RFC2822) DAYS(d), dt(3), MONTHS(dt(2)), dt(1), dt(5), dt(6), dt(7), z
    end function dm_time_rfc2822

    impure elemental integer function dm_time_to_beats(time, beats) result(rc)
        !! Converts ISO 8601 timestamp `time` into
        !! [Swatch Internet Time](https://en.wikipedia.org/wiki/Swatch_Internet_Time)
        !! (_.beat time_) in the form `@1000.00` in `beats`. One beat is
        !! equivalent to one decimal minute in the French decimal time (1 min
        !! 26.4 secs in Solar time).
        character(len=*), intent(in)  :: time  !! ISO 8601 timestamp.
        character(len=8), intent(out) :: beats !! Timestamp converted to _.beat_.

        integer          :: hour, minute, second
        integer(kind=i8) :: bmt, utc
        real             :: b

        beats = ' '
        rc = dm_time_to_unix(time, utc)
        if (dm_is_error(rc)) return
        bmt = utc + 3600_i8
        rc = dm_time_from_unix(bmt, hour=hour, minute=minute, second=second)
        b = (hour * 3600.0 + minute * 60.0 + second) * (1000.0 / 86400.0)
        write (beats, '("@", f0.2)') b
    end function dm_time_to_beats

    impure elemental integer function dm_time_to_unix(time, epoch, mseconds) result(rc)
        !! Converts ISO 8601/RFC 3339 timestamp to Unix timestamp (Epoch). The
        !! function calls `timegm()` internally (not in POSIX, but available
        !! since 4.3BSD), and then removes the time zone offset. The returned
        !! Epoch is always in UTC.
        !!
        !! The function returns `E_INVALID` if the passed ISO 8601 timestamp is
        !! invalid. If the call to `timegm()` fails, `E_SYSTEM` is returned.
        use :: unix, only: c_timegm, c_tm
        character(len=*), parameter :: FMT_ISO = '(i4, 2(1x, i2), 1x, 3(i2, 1x), i3, i3, 1x, i2)'

        character(len=*), intent(in)            :: time     !! ISO 8601 timestamp.
        integer(kind=i8), intent(out)           :: epoch    !! Unix timestamp.
        integer,          intent(out), optional :: mseconds !! Additional milliseconds.

        integer    :: stat
        integer    :: year, month, day
        integer    :: hour, minute, second, msecond
        integer    :: tz, tz_hour, tz_minute
        type(c_tm) :: tm

        rc = E_INVALID

        epoch = 0_i8
        read (time, FMT_ISO, iostat=stat) year, month, day, &
                                          hour, minute, second, msecond, &
                                          tz_hour, tz_minute
        if (stat /= 0) return

        tm = c_tm(tm_sec  = second, &
                  tm_min  = minute, &
                  tm_hour = hour, &
                  tm_mday = day, &
                  tm_mon  = month - 1, &
                  tm_year = year - 1900)

        tz = (tz_hour * 3600) + (tz_minute * 60)
        epoch = c_timegm(tm) - tz

        rc = E_SYSTEM
        if (epoch < 0) return

        if (present(mseconds)) mseconds = msecond
        rc = E_NONE
    end function dm_time_to_unix

    pure logical function dm_time_valid(time) result(valid)
        !! Returns `.true.` if given timestamp follows the form of ISO 8601. The
        !! timestamp does not have to be complete to be valid. The minimum
        !! length of a timestamp to be valid is 4.
        use :: dm_ascii, only: dm_ascii_is_digit

        character(len=*), intent(in) :: time !! ISO 8601 timestamp to validate.

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
        !! Returns current time zone as five characters long string.
        call date_and_time(zone=zone)
    end function dm_time_zone

    subroutine dm_time_strings(year, month, day, hour, minute, second, msecond, zone)
        !! Returns current date and time values as strings in given dummy
        !! arguments.
        character(len=4), intent(out), optional :: year    !! Current year.
        character(len=2), intent(out), optional :: month   !! Current month.
        character(len=2), intent(out), optional :: day     !! Current day.
        character(len=2), intent(out), optional :: hour    !! Current hour.
        character(len=2), intent(out), optional :: minute  !! Current minute.
        character(len=2), intent(out), optional :: second  !! Current second.
        character(len=3), intent(out), optional :: msecond !! Current msecond.
        character(len=5), intent(out), optional :: zone    !! Current time zone.

        character(len=5) :: tz
        integer          :: dt(8)

        call date_and_time(zone=tz, values=dt)

        if (present(year))    write (year,    '(i0.4)') dt(1)
        if (present(month))   write (month,   '(i0.2)') dt(2)
        if (present(day))     write (day,     '(i0.2)') dt(3)
        if (present(hour))    write (hour,    '(i0.2)') dt(5)
        if (present(minute))  write (minute,  '(i0.2)') dt(6)
        if (present(second))  write (second,  '(i0.2)') dt(7)
        if (present(msecond)) write (msecond, '(i0.3)') dt(8)
        if (present(zone))    zone = tz
    end subroutine dm_time_strings
end module dm_time
