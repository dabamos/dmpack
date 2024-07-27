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
        character(len=NODE_ID_LEN)     :: node_id   = ' '          !! Node id (`-0-9A-Z_a-z`).
        character(len=NET_IPV6_LEN)    :: address   = ' '          !! Client IP address (IPv4, IPv6).
        character(len=BEAT_CLIENT_LEN) :: client    = ' '          !! Client software name and version.
        character(len=TIME_LEN)        :: time_sent = TIME_DEFAULT !! Time heartbeat was sent.
        character(len=TIME_LEN)        :: time_recv = TIME_DEFAULT !! Time heartbeat was received.
        integer                        :: error     = E_NONE       !! Last client error.
        integer                        :: interval  = 0            !! Transmission interval in seconds.
        integer                        :: uptime    = 0            !! System uptime in seconds.
    end type beat_type

    integer, parameter, public :: BEAT_SIZE = storage_size(beat_type()) / 8 !! Size of `beat_type` in bytes.

    interface operator (==)
        !! Returns whether heartbeats are equal.
        module procedure :: dm_beat_equals
    end interface

    public :: operator (==)

    public :: dm_beat_equals
    public :: dm_beat_out
    public :: dm_beat_valid
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

    pure elemental logical function dm_beat_valid(beat) result(valid)
        !! Returns `.true.` if given beat type elements are valid.
        use :: dm_string, only: dm_string_is_printable

        type(beat_type), intent(in) :: beat !! Beat type.

        valid = .false.
        if (.not. dm_id_valid(beat%node_id))            return
        if (.not. dm_string_is_printable(beat%client))  return
        if (.not. dm_time_valid(beat%time_sent))        return
        if (.not. dm_time_valid(beat%time_recv))        return
        if (beat%interval < 0_i8) return
        valid = .true.
    end function dm_beat_valid

    subroutine dm_beat_out(beat, unit)
        !! Prints beat to standard output or given file unit.
        type(beat_type), intent(inout)        :: beat !! Beat type.
        integer,         intent(in), optional :: unit !! File unit.

        integer :: unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        write (unit_, '("beat.node_id: ", a)')   trim(beat%node_id)
        write (unit_, '("beat.address: ", a)')   trim(beat%address)
        write (unit_, '("beat.client: ", a)')    trim(beat%client)
        write (unit_, '("beat.time_sent: ", a)') trim(beat%time_sent)
        write (unit_, '("beat.time_recv: ", a)') trim(beat%time_recv)
        write (unit_, '("beat.error: ", i0)')    beat%error
        write (unit_, '("beat.interval: ", i0)') beat%interval
        write (unit_, '("beat.uptime: ", i0)')   beat%uptime
    end subroutine dm_beat_out
end module dm_beat
