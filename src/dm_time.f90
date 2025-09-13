! Author:  Philipp Engel
! Licence: ISC
module dm_time
    !! Date and time functions.
    !!
    !! ISO 8601/RFC 3339 is used as the universal date and time format. Date and
    !! time are separated by character `T`. The time has microsecond resolution,
    !! a time zone is mandatory. A valid timestamp must contain at least the
    !! full year, and be between 4 and 32 characters long. For example, the
    !! string `1970-01-01T00:00:00.000000+00:00` is a valid DMPACK time stamp.
    use :: unix
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! The parameter `TIME_LEN` is the length of a character string that stores a
    ! time stamp. Make sure that hard-coded edit descriptors match the length,
    ! for instance, in modules `dm_block`, `dm_dp`, and `dm_plot`.
    character(*), parameter, public :: TIME_DEFAULT   = '1970-01-01T00:00:00.000000+00:00' !! Default ISO 8601 time stamp with microseconds.
    integer,      parameter, public :: TIME_BEATS_LEN = 8  !! Length of beats string.
    integer,      parameter, public :: TIME_DATE_LEN  = 10 !! Length of date string.
    integer,      parameter, public :: TIME_LEN       = 32 !! Length of ISO 8601 time stamp.

    type, public :: time_delta_type
        !! Time delta type to store elapsed time.
        integer :: days    = 0 !! Passed days.
        integer :: hours   = 0 !! Passed hours.
        integer :: minutes = 0 !! Passed minutes.
        integer :: seconds = 0 !! Passed seconds.
    end type time_delta_type

    interface dm_time_from_unix
        !! Converts Unix epoch to integer or string representation.
        module procedure :: time_from_unix_integer
        module procedure :: time_from_unix_string
    end interface dm_time_from_unix

    public :: dm_time_create
    public :: dm_time_date
    public :: dm_time_delta_from_seconds
    public :: dm_time_delta_to_string
    public :: dm_time_diff
    public :: dm_time_from_unix
    public :: dm_time_is_valid
    public :: dm_time_now
    public :: dm_time_parse_string
    public :: dm_time_rfc2822
    public :: dm_time_strings
    public :: dm_time_strip_useconds
    public :: dm_time_to_beats
    public :: dm_time_to_human
    public :: dm_time_to_unix
    public :: dm_time_to_utc
    public :: dm_time_unix
    public :: dm_time_unix_mseconds
    public :: dm_time_zone
    public :: dm_time_zone_iso

    private :: time_from_unix_integer
    private :: time_from_unix_string
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    pure elemental character(TIME_LEN) function dm_time_create(year, month, day, hour, minute, second, usecond, zone) result(string)
        !! Returns 32-characters long time stamp string in ISO 8601/RFC 3339 of
        !! the form `1970-01-01T00:00:00.000000+00:00`. Optional argument
        !! `zone` sets the time zone and has to be of the form `[+|-]hh:mm`,
        !! for example, `+00:00` or `-01:00`.
        character(*), parameter :: FMT_ISO = '(i4, 2("-", i0.2), "T", 2(i0.2, ":"), i0.2, ".", i0.6, a)'

        integer,      intent(in), optional :: year    !! Year (`YYYY`).
        integer,      intent(in), optional :: month   !! Month (`MM`).
        integer,      intent(in), optional :: day     !! Day of month (`DD`).
        integer,      intent(in), optional :: hour    !! Hour (`hh`).
        integer,      intent(in), optional :: minute  !! Minute (`mm`).
        integer,      intent(in), optional :: second  !! Second (`ss`).
        integer,      intent(in), optional :: usecond !! Microsecond (`ffffff`).
        character(6), intent(in), optional :: zone    !! Timezone (`[+|-]hh:mm`).

        integer      :: year_, month_, day_
        integer      :: hour_, minute_, second_, usecond_
        character(6) :: zone_

        year_    = dm_present(year,    1970)
        month_   = dm_present(month,   1)
        day_     = dm_present(day,     1)
        hour_    = dm_present(hour,    0)
        minute_  = dm_present(minute,  0)
        second_  = dm_present(second,  0)
        usecond_ = dm_present(usecond, 0)

        if (present(zone)) then
            zone_ = zone
        else
            zone_ = '+00:00'
        end if

        write (string, FMT_ISO) year_, month_, day_, hour_, minute_, second_, usecond_, zone_
    end function dm_time_create

    character(TIME_DATE_LEN) function dm_time_date() result(string)
        !! Returns date as string in the form `YYYY-MM-DD`.
        character(8) :: date

        call date_and_time(date=date)
        string = date(1:4) // '-' // date(5:6) // '-' // date(7:8)
    end function dm_time_date

    pure elemental subroutine dm_time_delta_from_seconds(time_delta, seconds)
        !! Returns time delta type `time_delta` from passed time `seconds`.
        type(time_delta_type), intent(out) :: time_delta !! Time delta type.
        integer(i8),           intent(in)  :: seconds    !! Time in seconds.

        integer(i8) :: t

        t = seconds
        time_delta%days = int(t / 86400)
        t = modulo(t, 86400_i8)
        time_delta%hours  = int(t / 3600)
        t = modulo(t, 3600_i8)
        time_delta%minutes = int(t / 60)
        time_delta%seconds = int(modulo(t, 60_i8))
    end subroutine dm_time_delta_from_seconds

    function dm_time_delta_to_string(time_delta, days, hours, minutes, seconds) result(string)
        !! Converts `time_delta_type` to string of format
        !! `[d days ][h hours ][m mins ][s secs]`.
        type(time_delta_type), intent(inout)        :: time_delta !! Time delta type.
        logical,               intent(in), optional :: days       !! Write days, `.true.` by default.
        logical,               intent(in), optional :: hours      !! Write hours, `.true.` by default.
        logical,               intent(in), optional :: minutes    !! Write minutes, `.true.` by default.
        logical,               intent(in), optional :: seconds    !! Write seconds, `.true.` by default.

        character(:), allocatable :: string

        string = ''

        if (dm_present(days,    .true.)) string = string // dm_itoa(time_delta%days)    // ' days '
        if (dm_present(hours,   .true.)) string = string // dm_itoa(time_delta%hours)   // ' hours '
        if (dm_present(minutes, .true.)) string = string // dm_itoa(time_delta%minutes) // ' mins '
        if (dm_present(seconds, .true.)) string = string // dm_itoa(time_delta%seconds) // ' secs'
    end function dm_time_delta_to_string

    impure elemental integer function dm_time_diff(time1, time2, seconds) result(rc)
        !! Returns the time difference between `time1` and `time2` as 8-byte
        !! integer `seconds` [sec]. The function does not validate the time
        !! stamps. Make sure to only pass valid values. On error, the result
        !! `seconds` is `huge(0_i8)`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if one time stamp or both are invalid.
        !! * `E_SYSTEM` if the system call failed.
        !!
        character(TIME_LEN), intent(in)  :: time1   !! ISO 8601 time stamp.
        character(TIME_LEN), intent(in)  :: time2   !! ISO 8601 time stamp.
        integer(i8),         intent(out) :: seconds !! Time delta [sec].

        integer     :: u1, u2 ! Microseconds.
        integer(i8) :: t1, t2 ! Seconds.

        seconds = huge(0_i8)

        rc = dm_time_to_unix(time1, t1, u1); if (dm_is_error(rc)) return
        rc = dm_time_to_unix(time2, t2, u2); if (dm_is_error(rc)) return

        seconds = abs(t2 - t1) + int((u2 - u1) / 10e6, kind=i8)
    end function dm_time_diff

    pure elemental logical function dm_time_is_valid(time, strict) result(valid)
        !! Returns `.true.` if given time stamp follows the form of ISO 8601. The
        !! time stamp does not have to be complete to be valid, unless `strict`
        !! is `.true.`. Then, argument `time` must be 32-characters long.
        !! Otherwise, the minimum length of a time stamp to be valid is 4
        !! characters, the maximum is 32 characters.
        use :: dm_ascii, only: dm_ascii_is_digit

        character(*), intent(in)           :: time   !! ISO 8601 time stamp to validate.
        logical,      intent(in), optional :: strict !! Validate length (must be 32 characters).

        character :: a
        integer   :: i, n

        valid = .false.

        n = len_trim(time)

        if (dm_present(strict, .false.)) then
            if (n /= TIME_LEN) return
        else
            if (n < 4 .or. n > TIME_LEN) return
        end if

        do i = 1, n
            a = time(i:i)

            select case (i)
                case (1:4, 6:7, 9:10, 12:13, 15:16, 18:19, 21:26, 28:29, 31:32)
                    if (.not. dm_ascii_is_digit(a)) return
                case (5, 8)
                    if (a /= '-') return
                case (11)
                    if (a /= 'T') return
                case (14, 17, 30)
                    if (a /= ':') return
                case (20)
                    if (a /= '.') return
                case (27)
                    if (a /= '+' .and. a /= '-') return
            end select
        end do

        valid = .true.
    end function dm_time_is_valid

    impure elemental character(TIME_LEN) function dm_time_now() result(string)
        !! Returns current date and time as 32-characters long string in ISO
        !! 8601/RFC 3339 format (`1970-01-01T00:00:00.000000+00:00`), and
        !! microsecond resolution.
        character(*), parameter :: FMT_ISO = &
            '(i0.4, 2("-", i0.2), "T", 2(i0.2, ":"), i0.2, ".", i0.6, sp, i0.2, ss, ":", i0.2)'

        integer          :: rc
        integer          :: tz_hour, tz_minute
        type(c_ptr)      :: ptr
        type(c_timeval)  :: tv
        type(c_timezone) :: tz
        type(c_tm)       :: tm

        rc  = c_gettimeofday(tv, tz)
        ptr = c_localtime_r(tv%tv_sec, tm)

        tz_hour   = int(tm%tm_gmtoff) / 3600
        tz_minute = modulo(int(tm%tm_gmtoff) / 60, 60)

        write (string, FMT_ISO) tm%tm_year + 1900, & ! Year.
                                tm%tm_mon + 1,     & ! Month
                                tm%tm_mday,        & ! Day of month.
                                tm%tm_hour,        & ! Hour.
                                tm%tm_min,         & ! Minute.
                                tm%tm_sec,         & ! Second.
                                tv%tv_usec,        & ! Microsecond.
                                tz_hour,           & ! Zone offset (hour).
                                tz_minute            ! Zone offset (minute).
    end function dm_time_now

    function dm_time_parse_string(string) result(parsed)
        !! Returns the passed string with formated descriptors replaced with
        !! their current values. The following format descriptors are allowed:
        !!
        !! | Format | Description  |
        !! |--------|--------------|
        !! | `%Y`   | year         |
        !! | `%M`   | month        |
        !! | `%D`   | day of month |
        !! | `%h`   | hour         |
        !! | `%m`   | minute       |
        !! | `%s`   | second       |
        !!
        character(*), intent(in)  :: string !! Input string.
        character(:), allocatable :: parsed !! Output string.

        character     :: a
        character(8)  :: date
        character(10) :: time
        logical       :: flag
        integer       :: i, n

        parsed = ''

        n = len_trim(string)
        if (n == 0) return

        call date_and_time(date, time)
        flag = .false.

        do i = 1, n
            a = string(i:i)

            if (.not. flag) then
                if (a == '%') then
                    flag = .true.
                else
                    parsed = parsed // a
                end if

                cycle
            end if

            select case (a)
                case ('D');   parsed = parsed // date(7:8)
                case ('M');   parsed = parsed // date(5:6)
                case ('Y');   parsed = parsed // date(3:4)
                case ('h');   parsed = parsed // time(1:2)
                case ('m');   parsed = parsed // time(3:4)
                case ('s');   parsed = parsed // time(5:6)
                case default; parsed = parsed // '%' // a
            end select

            flag = .false.
        end do
    end function dm_time_parse_string

    impure elemental character(31) function dm_time_rfc2822() result(string)
        !! Returns current date and time as 31-characters long string in
        !! [RFC 2822](https://www.ietf.org/rfc/rfc2822.txt) format.
        character(3), parameter :: DAYS(7) = &
            [ 'Sun', 'Mon', 'Thu', 'Wed', 'Thu', 'Fri', 'Sat' ]
        character(3), parameter :: MONTHS(12) = &
            [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ]
        character(*), parameter :: RFC_FMT = &
            '(a, ", " , i0.2, 1x, a, 1x, i4, 1x, i0.2, ":", i0.2, ":", i0.2, 1x, a)'

        character(5) :: z
        integer(i8)  :: d, dt(8)

        call date_and_time(values=dt, zone=z)
        d = 1 + modulo(dt(1) + ((dt(1) - 1) / 4) - ((dt(1) - 1) / 100) + ((dt(1) - 1) / 400), 7_i8)
        write (string, RFC_FMT) DAYS(d), dt(3), MONTHS(dt(2)), dt(1), dt(5), dt(6), dt(7), z
    end function dm_time_rfc2822

    pure elemental character(25) function dm_time_strip_useconds(time) result(string)
        !! Strips the microseconds part of the given ISO 8601 time stamp and
        !! returns a 25-characters long string. The function does not validate
        !! the time stamp for performance reasons. Make sure that only a valid
        !! 32-characters long ISO 8601 time stamp is passed in `time`.
        character(TIME_LEN), intent(in) :: time !! ISO 8601 time stamp.

        write (string, '(a19, a6)') time(1:19), time(27:32)
    end function dm_time_strip_useconds

    impure elemental integer function dm_time_to_beats(time, beats) result(rc)
        !! Converts ISO 8601 time stamp `time` into
        !! [Swatch Internet Time](https://en.wikipedia.org/wiki/Swatch_Internet_Time)
        !! (_.beat time_) in the form `@1000.00` in `beats`. One beat is
        !! equivalent to one decimal minute in the French decimal time (1 min
        !! 26.4 sec in Solar time).
        character(TIME_LEN),       intent(in)  :: time  !! ISO 8601 time stamp.
        character(TIME_BEATS_LEN), intent(out) :: beats !! Timestamp converted to _.beat_.

        integer     :: hour, minute, second
        integer(i8) :: bmt, utc
        real        :: b

        beats = ' '
        rc = dm_time_to_unix(time, utc)
        if (dm_is_error(rc)) return
        bmt = utc + 3600_i8
        rc = dm_time_from_unix(bmt, hour=hour, minute=minute, second=second)
        if (dm_is_error(rc)) return
        b = (hour * 3600.0 + minute * 60.0 + second) * (1000.0 / 86400.0)
        write (beats, '("@", f0.2)') b
    end function dm_time_to_beats

    pure function dm_time_to_human(time, zone) result(human)
        !! Returns time stamp as allocatable string in human-readable format.
        !! Converts the given ISO 8601 time stamp `time` in format
        !! `1970-01-01T00:00:00.000000+00:00` to format
        !! `1970-01-01 00:00:00 +00:00`. The argument `time` is not validated.
        !! Make sure to only pass valid values. If `zone` is `.false.`, the
        !! time zone is omitted.
        !!
        !! This function does not turn a time stamp into a human being.
        character(TIME_LEN), intent(in)           :: time  !! ISO 8601 time stamp.
        logical,             intent(in), optional :: zone  !! No time zone if `.false.`.
        character(:), allocatable                 :: human !! Human-readable time stamp.

        if (dm_present(zone, .true.)) then
            human = time(1:10) // ' ' // time(12:19) // ' ' // time(27:32)
        else
            human = time(1:10) // ' ' // time(12:19)
        end if
    end function dm_time_to_human

    impure elemental integer function dm_time_to_unix(time, epoch, useconds) result(rc)
        !! Converts 32-characters long ISO 8601/RFC 3339 time stamp to Unix
        !! time stamp (Epoch). The function calls `timegm()` internally (not
        !! in POSIX, but available since 4.3BSD), and then removes the time
        !! zone offset. The returned Epoch is always in UTC. The output does
        !! not contain microseconds.
        !!
        !! The function returns `E_INVALID` if the given ISO 8601 time stamp
        !! `time` is invalid. However, this function does not perform a full
        !! validation of the time stamp for performance reasons. Make sure to
        !! only pass valid time stamps. If the call to `timegm()` fails,
        !! `E_SYSTEM` is returned.
        character(*), parameter :: FMT_ISO = '(i4, 2(1x, i2), 1x, 3(i2, 1x), i6, i3, 1x, i2)'

        character(TIME_LEN), intent(in)            :: time     !! ISO 8601 time stamp.
        integer(i8),         intent(out)           :: epoch    !! Unix time stamp.
        integer,             intent(out), optional :: useconds !! Additional microseconds.

        integer    :: stat
        integer    :: tm_year, tm_mon, tm_mday
        integer    :: tm_hour, tm_min, tm_sec, tm_usec
        integer    :: tm_tz
        integer    :: tz_hour, tz_min
        type(c_tm) :: tm

        rc = E_INVALID

        epoch = 0_i8
        read (time, FMT_ISO, iostat=stat) tm_year, tm_mon, tm_mday, &
                                          tm_hour, tm_min, tm_sec, tm_usec, &
                                          tz_hour, tz_min
        if (stat /= 0) return

        tm = c_tm(tm_sec  = tm_sec,     &
                  tm_min  = tm_min,     &
                  tm_hour = tm_hour,    &
                  tm_mday = tm_mday,    &
                  tm_mon  = tm_mon - 1, &
                  tm_year = tm_year - 1900)

        tm_tz = (tz_hour * 3600) + (tz_min * 60)
        epoch = c_timegm(tm) - tm_tz

        rc = E_SYSTEM
        if (epoch < 0) return

        if (present(useconds)) useconds = tm_usec
        rc = E_NONE
    end function dm_time_to_unix

    impure elemental integer function dm_time_to_utc(time, utc) result(rc)
        !! Converts ISO 8601 time stamp `time` with time zone to ISO 8601 time
        !! stamp in UTC. On error, `utc` may be `1970-01-01T00:00:00.000000+00:00`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if `time` is invalid.
        !! * `E_SYSTEM ` if system call failed.
        !!
        character(TIME_LEN), intent(in)  :: time !! ISO 8601 time stamp.
        character(TIME_LEN), intent(out) :: utc  !! ISO 8601 time stamp in UTC.

        integer     :: useconds
        integer(i8) :: epoch

        utc = TIME_DEFAULT

        rc = E_INVALID
        if (.not. dm_time_is_valid(time, strict=.true.)) return

        rc = E_NONE
        if (time(28:32) == '00:00') then
            utc = time
            return
        end if

        rc = dm_time_to_unix(time, epoch, useconds)
        if (dm_is_error(rc)) return

        rc = dm_time_from_unix(epoch, utc, useconds)
    end function dm_time_to_utc

    integer(i8) function dm_time_unix() result(sec)
        !! Returns current time in seconds as 8-byte integer (Unix Epoch). On
        !! error, the result is 0.
        type(c_timespec) :: tp

        sec = 0_i8
        if (c_clock_gettime(CLOCK_REALTIME, tp) /= 0) return
        sec = tp%tv_sec
    end function dm_time_unix

    integer(i8) function dm_time_unix_mseconds() result(msec)
        !! Returns current time in mseconds as 8-byte integer (Unix Epoch). On
        !! error, the result is 0.
        type(c_timespec) :: tp

        msec = 0_i8
        if (c_clock_gettime(CLOCK_REALTIME, tp) /= 0) return
        msec = (tp%tv_sec * 1000_i8) + (tp%tv_nsec / 1000000_i8)
    end function dm_time_unix_mseconds

    character(5) function dm_time_zone() result(zone)
        !! Returns current time zone as five characters long string, for
        !! example, `+0000`.
        call date_and_time(zone=zone)
    end function dm_time_zone

    character(6) function dm_time_zone_iso() result(zone)
        !! Returns current time zone as six characters long string (ISO format),
        !! for example, `+00:00`.
        character(5) :: z

        call date_and_time(zone=z)
        write (zone, '(a3, ":", a2)') z(1:3), z(4:5)
    end function dm_time_zone_iso

    subroutine dm_time_strings(year, month, day, hour, minute, second, usecond, zone_hour, zone_minute, zone)
        !! Returns current date and time values as strings in given dummy
        !! arguments.
        character(4), intent(out), optional :: year        !! Current year (`YYYY`).
        character(2), intent(out), optional :: month       !! Current month (`MM`).
        character(2), intent(out), optional :: day         !! Current day of month (`DD`).
        character(2), intent(out), optional :: hour        !! Current hour (`hh`).
        character(2), intent(out), optional :: minute      !! Current minute (`mm`).
        character(2), intent(out), optional :: second      !! Current second (`ss`).
        character(6), intent(out), optional :: usecond     !! Current usecond (`ffffff`).
        character(3), intent(out), optional :: zone_hour   !! Current time zone (`[+|-]hh`).
        character(2), intent(out), optional :: zone_minute !! Current time zone (`mm`).
        character(6), intent(out), optional :: zone        !! Current time zone (`[+|-]hh:mm`).

        integer          :: rc, tz_hour, tz_minute
        type(c_ptr)      :: ptr
        type(c_timeval)  :: tv
        type(c_timezone) :: tz
        type(c_tm)       :: tm

        rc  = c_gettimeofday(tv, tz)
        ptr = c_localtime_r(tv%tv_sec, tm)

        tz_hour   = int(tm%tm_gmtoff) / 3600
        tz_minute = modulo(int(tm%tm_gmtoff) / 60, 60)

        if (present(year))        write (year,        '(i0.4)') tm%tm_year + 1900
        if (present(month))       write (month,       '(i0.2)') tm%tm_mon + 1
        if (present(day))         write (day,         '(i0.2)') tm%tm_mday
        if (present(hour))        write (hour,        '(i0.2)') tm%tm_hour
        if (present(minute))      write (minute,      '(i0.2)') tm%tm_min
        if (present(second))      write (second,      '(i0.2)') tm%tm_sec
        if (present(usecond))     write (usecond,     '(i0.6)') tv%tv_usec
        if (present(zone_hour))   write (zone_hour,   '(sp, i0.2, ss)') tz_hour
        if (present(zone_minute)) write (zone_minute, '(i0.2)')         tz_minute
        if (present(zone))        write (zone,        '(sp, i0.2, ss, ":", i0.2)') tz_hour, tz_minute
    end subroutine dm_time_strings

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    impure elemental integer function time_from_unix_integer(epoch, year, month, day, hour, minute, second) result(rc)
        !! Converts the 8-byte calendar time `epoch` in UTC to broken-down time
        !! representation. The argument `epoch` is the number of seconds
        !! elapsed since the Epoch, 1970-01-01 00:00:00 +0000 (UTC). The
        !! function calls `gmtime_r()` internally (SUSv2).
        !!
        !! Returns `E_SYSTEM` if the system call failed.
        integer(i8), intent(in)            :: epoch  !! Unix time stamp in seconds (UTC).
        integer,     intent(out), optional :: year   !! Year part of time stamp.
        integer,     intent(out), optional :: month  !! Month part of time stamp.
        integer,     intent(out), optional :: day    !! Day of month part of time stamp.
        integer,     intent(out), optional :: hour   !! Hour part of time stamp.
        integer,     intent(out), optional :: minute !! Minute part of time stamp.
        integer,     intent(out), optional :: second !! Second part of time stamp.

        type(c_ptr) :: ptr
        type(c_tm)  :: tm

        rc = E_SYSTEM
        ptr = c_gmtime_r(int(epoch, c_time_t), tm)

        if (.not. c_associated(ptr)) return

        if (present(year))   year   = tm%tm_year + 1900
        if (present(month))  month  = tm%tm_mon + 1
        if (present(day))    day    = tm%tm_mday
        if (present(hour))   hour   = tm%tm_hour
        if (present(minute)) minute = tm%tm_min
        if (present(second)) second = tm%tm_sec

        rc = E_NONE
    end function time_from_unix_integer

    impure elemental integer function time_from_unix_string(epoch, time, useconds) result(rc)
        !! Converts the 8-byte calendar time `epoch` in UTC to ISO 8601
        !! representation. The argument `epoch` is the number of seconds
        !! elapsed since the Epoch, 1970-01-01 00:00:00 +0000 (UTC). The
        !! function calls `gmtime_r()` internally (SUSv2).
        !!
        !! Returns `E_SYSTEM` if the system call failed.
        character(*), parameter :: FMT_ISO = &
            '(i0.4, 2("-", i0.2), "T", 2(i0.2, ":"), i0.2, ".", i0.6, "+00:00")'

        integer(i8),         intent(in)           :: epoch    !! Unix time stamp in seconds (UTC).
        character(TIME_LEN), intent(out)          :: time     !! ISO 8601 representation.
        integer,             intent(in), optional :: useconds !! Additional microseconds.

        integer     :: useconds_
        type(c_ptr) :: ptr
        type(c_tm)  :: tm

        rc = E_SYSTEM
        ptr = c_gmtime_r(int(epoch, kind=c_time_t), tm)

        if (.not. c_associated(ptr)) then
            time = TIME_DEFAULT
            return
        end if

        useconds_ = dm_present(useconds, 0)
        write (time, FMT_ISO) tm%tm_year + 1900, &
                              tm%tm_mon + 1,     &
                              tm%tm_mday,        &
                              tm%tm_hour,        &
                              tm%tm_min,         &
                              tm%tm_sec,         &
                              useconds_
        rc = E_NONE
    end function time_from_unix_string
end module dm_time
