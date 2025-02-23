! Author:  Philipp Engel
! Licence: ISC
module dm_dwd
    !! I/O module for various APIs of Deutsche Wetterdienst (DWD).
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    implicit none (type, external)
    private

    character(len=*), parameter :: DWD_MOSMIX_STATION_FMT = '(a5, 1x, a4, 1x, a20, 1x, f6.2, 1x, f7.2, 1x, i5)'

    integer, parameter, public :: DWD_MOSMIX_STATION_ID_LEN   = 5
    integer, parameter, public :: DWD_MOSMIX_STATION_ICAO_LEN = 4
    integer, parameter, public :: DWD_MOSMIX_STATION_NAME_LEN = 20

    type, public :: dwd_mosmix_station_type
        !! MOSMIX weather station.
        character(len=DWD_MOSMIX_STATION_ID_LEN)   :: id   = ' '    !! Station id.
        character(len=DWD_MOSMIX_STATION_ICAO_LEN) :: icao = '----' !! ICAO code.
        character(len=DWD_MOSMIX_STATION_NAME_LEN) :: name = ' '    !! Station name.
        real                                       :: lat  = 0.0    !! Latitude.
        real                                       :: lon  = 0.0    !! Longitude.
        integer                                    :: elev = 0      !! Elevation.
    end type dwd_mosmix_station_type

    public :: dm_dwd_mosmix_station_catalog_read
    public :: dm_dwd_mosmix_station_catalog_write
    public :: dm_dwd_mosmix_station_find
    public :: dm_dwd_mosmix_station_out
contains
    integer function dm_dwd_mosmix_station_catalog_read(stations, unit, header) result(rc)
        !! Reads MOSMIX stations from CFG catalog file. On error, the array is
        !! allocated but of size 0. Pass the file unit to the function:
        !!
        !! ```fortran
        !! integer :: stat, unit
        !! type(dwd_mosmix_station_type), allocatable :: stations(:)
        !!
        !! open (action='read', file='catalog.cfg', iostat=stat, newunit=unit, status='old')
        !! if (stat /= 0) error stop
        !! rc = dm_dwd_mosmix_station_catalog_read(stations, unit)
        !! close (unit)
        !! ```
        !!
        !! The MOSMIX station catalog has the following format:
        !!
        !! ```text
        !! ID    ICAO NAME                 LAT    LON     ELEV
        !! ----- ---- -------------------- -----  ------- -----
        !! 01001 ENJA JAN MAYEN             70.56   -8.40    10
        !! 01008 ENSB SVALBARD              78.15   15.28    29
        !! 01025 ---- TROMSOE               69.41   18.55    10
        !! ```
        !!
        !! The catalog file can be downloaded from:
        !!
        !! * [MOSMIX Stationskatalog](https://www.dwd.de/DE/leistungen/met_verfahren_mosmix/mosmix_stationskatalog.cfg?view=nasPublication&nn=16102)
        !!
        !! Or, use file `share/dmdwd/catalog.cfg`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_FORMAT` if line format is invalid.
        !! * `E_READ` if file reading failed.
        !!
        use :: dm_util, only: dm_present

        type(dwd_mosmix_station_type), allocatable, intent(out)          :: stations(:) !! Stations read from file.
        integer,                                    intent(in)           :: unit        !! File unit.
        logical,                                    intent(in), optional :: header      !! File contains header.

        character(len=52) :: line
        integer           :: nlines, nstations, stat
        logical           :: header_

        nlines    = 0
        nstations = 0
        header_   = dm_present(header, .true.)

        read_block: block
            rc = E_ALLOC
            allocate (stations(0), stat=stat)
            if (stat /= 0) exit read_block

            read_loop: do
                block
                    type(dwd_mosmix_station_type), allocatable :: buffer(:)

                    nlines = nlines + 1

                    rc = E_READ
                    read (unit, '(a)', iostat=stat) line

                    if (header_ .and. nlines < 3) cycle read_loop
                    if (is_iostat_end(stat)) exit read_loop
                    if (stat /= 0) exit read_block

                    nstations = nstations + 1

                    rc = E_ALLOC
                    allocate (buffer(nstations), stat=stat)
                    if (stat /= 0) exit read_block

                    buffer(1:size(stations)) = stations

                    rc = E_FORMAT
                    read (line, DWD_MOSMIX_STATION_FMT, iostat=stat) buffer(nstations)
                    if (stat /= 0) exit read_block

                    call move_alloc(from=buffer, to=stations)
                end block
            end do read_loop

            rc = E_NONE
        end block read_block

        if (.not. allocated(stations)) allocate (stations(0))
    end function dm_dwd_mosmix_station_catalog_read

    integer function dm_dwd_mosmix_station_find(stations, id, station, found) result(rc)
        !! Returns station of `id` from array `stations` in argument `station`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if array is empty.
        !! * `E_NOT_FOUND` if station id was not found.
        !!
        type(dwd_mosmix_station_type),            intent(inout)         :: stations(:) !! MOSMIX stations.
        character(len=DWD_MOSMIX_STATION_ID_LEN), intent(in)            :: id          !! Station id.
        type(dwd_mosmix_station_type),            intent(out)           :: station     !! MOSMIX station of id.
        logical,                                  intent(out), optional :: found       !! Station found.

        integer :: i, loc

        if (present(found)) found = .false.

        rc = E_EMPTY
        if (size(stations) == 0) return

        loc = 0

        do i = 1, size(stations)
            if (stations(i)%id == id) then
                loc = i
                exit
            end if
        end do

        rc = E_NOT_FOUND
        if (loc == 0) return

        rc = E_NONE
        station = stations(loc)

        if (present(found)) found = .true.
    end function dm_dwd_mosmix_station_find

    subroutine dm_dwd_mosmix_station_catalog_write(stations, unit, header)
        !! Writes MOSMIX station catalog to standard output or file unit.
        use :: dm_util, only: dm_present

        type(dwd_mosmix_station_type), intent(inout)        :: stations(:) !! MOSMIX stations.
        integer,                       intent(in), optional :: unit        !! File unit.
        logical,                       intent(in), optional :: header      !! Write header.

        integer :: i, unit_
        logical :: header_

        unit_   = dm_present(unit, stdout)
        header_ = dm_present(header, .true.)

        if (header_) then
            write (unit_, '("ID", 4x, "ICAO", 1x, "NAME", 17x, "LAT", 4x, "LON", 5x, "ELEV")')
            write (unit_, '(5("-"), 1x, 4("-"), 1x, 20("-"), 1x, 5("-"), 2x, 7("-"), 1x, 5("-"))')
        end if

        write (unit_, DWD_MOSMIX_STATION_FMT) [ (stations(i), i = 1, size(stations)) ]
    end subroutine dm_dwd_mosmix_station_catalog_write

    subroutine dm_dwd_mosmix_station_out(station, unit)
        !! Prints MOSMIX station to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(dwd_mosmix_station_type), intent(in)           :: station !! MOSMIX station.
        integer,                       intent(in), optional :: unit    !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, stdout)

        write (unit_, '("dwd_mosmix_station.id: ", a)')     trim(station%id)
        write (unit_, '("dwd_mosmix_station.icao: ", a)')   trim(station%icao)
        write (unit_, '("dwd_mosmix_station.name: ", a)')   trim(station%name)
        write (unit_, '("dwd_mosmix_station.lat: ", f0.2)') station%lat
        write (unit_, '("dwd_mosmix_station.lon: ", f0.2)') station%lon
        write (unit_, '("dwd_mosmix_station.elev: ", i0)')  station%elev
    end subroutine dm_dwd_mosmix_station_out
end module dm_dwd
