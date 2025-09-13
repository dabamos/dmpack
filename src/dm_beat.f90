! Author:  Philipp Engel
! Licence: ISC
module dm_beat
    !! Heartbeat message type.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_net
    use :: dm_node
    use :: dm_time
    implicit none (type, external)
    private

    integer, parameter, public :: BEAT_CLIENT_LEN = 32 !! Client software name and version length.

    type, public :: beat_type
        !! Status message (heartbeat) type.
        character(NODE_ID_LEN)     :: node_id   = ' '          !! Node id (`-0-9A-Z_a-z`).
        character(NET_IPV6_LEN)    :: address   = ' '          !! Client IP address (IPv4, IPv6).
        character(BEAT_CLIENT_LEN) :: client    = ' '          !! Client software name and version.
        character(TIME_LEN)        :: time_sent = TIME_DEFAULT !! Time heartbeat was sent.
        character(TIME_LEN)        :: time_recv = TIME_DEFAULT !! Time heartbeat was received.
        integer                    :: error     = E_NONE       !! Last client error.
        integer                    :: interval  = 0            !! Transmission interval in seconds.
        integer                    :: uptime    = 0            !! System uptime in seconds.
    end type beat_type

    integer, parameter, public :: BEAT_TYPE_SIZE = storage_size(beat_type()) / 8 !! Size of `beat_type` in bytes.

    interface operator (==)
        !! Returns `.true.` if heartbeats are equal.
        module procedure :: dm_beat_equals
    end interface

    public :: operator (==)

    public :: dm_beat_equals
    public :: dm_beat_is_valid
    public :: dm_beat_out
    public :: dm_beat_set
contains
    pure elemental logical function dm_beat_equals(beat1, beat2) result(equals)
        !! Returns `.true.` if given beats are equal.
        type(beat_type), intent(in) :: beat1 !! The first beat.
        type(beat_type), intent(in) :: beat2 !! The second beat.

        equals = .false.
        if (beat1%node_id   /= beat2%node_id)   return
        if (beat1%address   /= beat2%address)   return
        if (beat1%client    /= beat2%client)    return
        if (beat1%time_sent /= beat2%time_sent) return
        if (beat1%time_recv /= beat2%time_recv) return
        if (beat1%error     /= beat2%error)     return
        if (beat1%interval  /= beat2%interval)  return
        if (beat1%uptime    /= beat2%uptime)    return
        equals= .true.
    end function dm_beat_equals

    pure elemental logical function dm_beat_is_valid(beat) result(valid)
        !! Returns `.true.` if given beat type elements are valid.
        use :: dm_string, only: dm_string_is_printable

        type(beat_type), intent(in) :: beat !! Beat type.

        valid = .false.
        if (.not. dm_id_is_valid(beat%node_id))        return
        if (.not. dm_string_is_printable(beat%client)) return
        if (.not. dm_time_is_valid(beat%time_sent))    return
        if (.not. dm_time_is_valid(beat%time_recv))    return
        if (beat%interval < 0) return
        valid = .true.
    end function dm_beat_is_valid

    subroutine dm_beat_out(beat, unit)
        !! Prints beat to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(beat_type), intent(inout)        :: beat !! Beat type.
        integer,         intent(in), optional :: unit !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("beat.node_id: ", a)')   trim(beat%node_id)
        write (unit_, '("beat.address: ", a)')   trim(beat%address)
        write (unit_, '("beat.client: ", a)')    trim(beat%client)
        write (unit_, '("beat.time_sent: ", a)') trim(beat%time_sent)
        write (unit_, '("beat.time_recv: ", a)') trim(beat%time_recv)
        write (unit_, '("beat.error: ", i0)')    beat%error
        write (unit_, '("beat.interval: ", i0)') beat%interval
        write (unit_, '("beat.uptime: ", i0)')   beat%uptime
    end subroutine dm_beat_out

    pure elemental subroutine dm_beat_set(beat, node_id, address, client, time_sent, time_recv, error, interval, uptime)
        !! Sets attributes of beat type. This routine does not validate the
        !! arguments.
        type(beat_type), intent(inout)        :: beat      !! Beat type.
        character(*),    intent(in), optional :: node_id   !! Node id.
        character(*),    intent(in), optional :: address   !! IP address.
        character(*),    intent(in), optional :: client    !! Client software.
        character(*),    intent(in), optional :: time_sent !! Time beat was sent.
        character(*),    intent(in), optional :: time_recv !! Time beat was received.
        integer,         intent(in), optional :: error     !! Last error code.
        integer,         intent(in), optional :: interval  !! Beat interval.
        integer,         intent(in), optional :: uptime    !! Client uptime.

        if (present(node_id))   beat%node_id   = node_id
        if (present(address))   beat%address   = address
        if (present(client))    beat%client    = client
        if (present(time_sent)) beat%time_sent = time_sent
        if (present(time_recv)) beat%time_recv = time_recv
        if (present(error))     beat%error     = error
        if (present(interval))  beat%interval  = interval
        if (present(uptime))    beat%uptime    = uptime
    end subroutine dm_beat_set
end module dm_beat
