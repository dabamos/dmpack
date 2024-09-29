! Author:  Philipp Engel
! Licence: ISC
module dm_format
    !! Serialisation format definitions.
    implicit none (type, external)
    private

    integer, parameter, public :: FORMAT_NONE     = 0 !! Invalid format.
    integer, parameter, public :: FORMAT_BLOCK    = 1 !! ASCII block.
    integer, parameter, public :: FORMAT_CSV      = 2 !! CSV.
    integer, parameter, public :: FORMAT_GEOJSON  = 3 !! GeoJSON.
    integer, parameter, public :: FORMAT_JSON     = 4 !! JSON.
    integer, parameter, public :: FORMAT_JSONL    = 5 !! JSON Lines, NDJSON.
    integer, parameter, public :: FORMAT_NML      = 6 !! Fortran 95 Namelist.
    integer, parameter, public :: FORMAT_LAST     = 6 !! Never use this.

    integer, parameter, public :: FORMAT_NAME_LEN = 7 !! Max. length of format name.

    character(len=*), parameter, public :: FORMAT_NAMES(FORMAT_NONE:FORMAT_LAST) = [ &
        character(len=FORMAT_NAME_LEN) :: 'none', 'block', 'csv', 'geojson', 'json', 'jsonl', 'nml' &
    ] !! Format names array.

    public :: dm_format_from_name
    public :: dm_format_valid
contains
    pure elemental integer function dm_format_from_name(name) result(format)
        !! Returns format enumerator from given name. If the argument is not a
        !! valid format, the function returns `FORMAT_NONE`.
        use :: dm_string, only: dm_to_lower

        character(len=*), intent(in)   :: name !! Format name.
        character(len=FORMAT_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (FORMAT_NAMES(FORMAT_BLOCK))
                format = FORMAT_BLOCK
            case (FORMAT_NAMES(FORMAT_CSV))
                format = FORMAT_CSV
            case (FORMAT_NAMES(FORMAT_GEOJSON))
                format = FORMAT_GEOJSON
            case (FORMAT_NAMES(FORMAT_JSON))
                format = FORMAT_JSON
            case (FORMAT_NAMES(FORMAT_JSONL))
                format = FORMAT_JSONL
            case (FORMAT_NAMES(FORMAT_NML))
                format = FORMAT_NML
            case default
                format = FORMAT_NONE
        end select
    end function dm_format_from_name

    pure elemental logical function dm_format_valid(format) result(valid)
        !! Returns `.true.` if given format is valid. `FORMAT_NONE` is an
        !! invalid format.
        integer, intent(in) :: format !! Format enumerator.

        valid = (format > FORMAT_NONE .and. format <= FORMAT_LAST)
    end function dm_format_valid
end module dm_format
