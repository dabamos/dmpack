! Author:  Philipp Engel
! Licence: ISC
module dm_format
    !! Serialisation formats.
    use :: dm_string
    implicit none (type, external)
    private

    integer, parameter, public :: FORMAT_NONE  = 0 !! Invalid format.
    integer, parameter, public :: FORMAT_BLOCK = 1 !! ASCII block.
    integer, parameter, public :: FORMAT_CSV   = 2 !! CSV.
    integer, parameter, public :: FORMAT_JSON  = 3 !! JSON.
    integer, parameter, public :: FORMAT_JSONL = 4 !! JSON Lines, NDJSON.
    integer, parameter, public :: FORMAT_NML   = 5 !! Fortran 95 Namelist.
    integer, parameter, public :: FORMAT_LAST  = 5 !! Never use this.

    integer, parameter, public :: FORMAT_NAME_LEN = 5 !! Max. length of format name.

    character(len=*), parameter, public :: FORMAT_NAMES(FORMAT_NONE:FORMAT_LAST) = [ &
        character(len=FORMAT_NAME_LEN) :: 'none', 'block', 'csv', 'json', 'jsonl', 'nml' ] !! Format names array.

    public :: dm_format_from_name
    public :: dm_format_valid
contains
    pure elemental integer function dm_format_from_name(name) result(format)
        !! Returns format enumerator from given name.
        character(len=*), intent(in) :: name !! Format name.

        character(len=FORMAT_NAME_LEN) :: name_

        ! Normalised name.
        name_ = dm_lower(name)

        select case (name_)
            case (FORMAT_NAMES(FORMAT_BLOCK))
                format = FORMAT_BLOCK
            case (FORMAT_NAMES(FORMAT_CSV))
                format = FORMAT_CSV
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

        valid = .false.
        if (format <= FORMAT_NONE .or. format > FORMAT_LAST) return
        valid = .true.
    end function dm_format_valid
end module dm_format
