! Author:  Philipp Engel
! Licence: ISC
module dm_z
    !! Utility module for type-based compression and decompression (zlib, zstd).
    use :: dm_error
    use :: dm_kind
    use :: dm_nml
    use :: dm_zlib
    use :: dm_zstd
    implicit none (type, external)
    private

    ! Encoding types.
    integer, parameter, public :: Z_TYPE_INVALID = -1 !! Invalid or unknown encoding.
    integer, parameter, public :: Z_TYPE_NONE    = 0  !! No compression.
    integer, parameter, public :: Z_TYPE_ZLIB    = 1  !! Deflate compression.
    integer, parameter, public :: Z_TYPE_ZSTD    = 2  !! Zstandard compression.
    integer, parameter, public :: Z_TYPE_LAST    = 2  !! Never use this.

    integer, parameter, public :: Z_TYPE_NAME_LEN = 4 !! Max. encoding type name length.

    character(len=*), parameter, public :: Z_TYPE_NAMES(0:Z_TYPE_LAST) = [ &
        character(len=4) :: 'none', 'zlib', 'zstd' &
    ] !! Encoding type names.

    interface dm_z_uncompress
        module procedure :: z_uncompress
        module procedure :: z_uncompress_beat
    end interface

    ! Public procedures.
    public :: dm_z_type_from_name
    public :: dm_z_type_to_name
    public :: dm_z_uncompress
    public :: dm_z_valid

    ! Private procedures.
    private :: z_uncompress
    private :: z_uncompress_beat
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    pure elemental integer function dm_z_type_from_name(name) result(type)
        !! Returns encoding type from name. The function returns
        !! `Z_TYPE_INVALID` if the name is not a valid type name, and
        !! `Z_TYPE_NONE` if the string is empty or `none`.
        use :: dm_string, only: dm_lower

        character(len=*), intent(in)   :: name !! Compression type name.
        character(len=Z_TYPE_NAME_LEN) :: name_

        ! Normalise type name.
        name_ = dm_lower(name)

        select case (name_)
            case (' ', Z_TYPE_NAMES(Z_TYPE_NONE))
                type = Z_TYPE_NONE
            case (Z_TYPE_NAMES(Z_TYPE_ZLIB))
                type = Z_TYPE_ZLIB
            case (Z_TYPE_NAMES(Z_TYPE_ZSTD))
                type = Z_TYPE_ZSTD
            case default
                type = Z_TYPE_INVALID
        end select
    end function dm_z_type_from_name

    pure elemental character(len=Z_TYPE_NAME_LEN) function dm_z_type_to_name(type) result(name)
        !! Returns encoding type name from given type, for example,
        !! `Z_TYPE_NONE`, `Z_TYPE_ZLIB`, or `Z_TYPE_ZSTD`. If an invalid type
        !! is passed, the function returns `none`.
        integer, intent(in) :: type !! Compression type.

        if (.not. dm_z_valid(type)) then
            name = Z_TYPE_NAMES(Z_TYPE_NONE)
            return
        end if

        name = Z_TYPE_NAMES(type)
    end function dm_z_type_to_name

    pure elemental logical function dm_z_valid(type) result(valid)
        !! Returns `.true.` if the given compression type `type` is valid. The
        !! type `Z_TYPE_NONE` is a valid type, and `Z_TYPE_INVALID` is invalid.
        integer, intent(in) :: type !! Compression type.

        valid = (type >= Z_TYPE_NONE .and. type <= Z_TYPE_LAST)
    end function dm_z_valid

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function z_uncompress(input, output, type, input_len, output_len) result(rc)
        character(len=*), intent(inout)         :: input      !! Compressed data.
        character(len=*), intent(inout)         :: output     !! Uncompressed data.
        integer,          intent(in)            :: type       !! Input encoding type (`Z_TYPE_*`).
        integer(kind=i8), intent(in),  optional :: input_len  !! Actual input length.
        integer(kind=i8), intent(out), optional :: output_len !! Actual output length.

        rc = E_INVALID
        if (.not. dm_z_valid(type)) return

        rc = E_NONE
        select case (type)
            case (Z_TYPE_NONE)
                if (present(input_len)) then
                    output = input(:input_len)
                else
                    output = input
                end if

            case (Z_TYPE_ZLIB)
                rc = dm_zlib_uncompress(input, output, input_len=input_len, output_len=output_len)

            case (Z_TYPE_ZSTD)
                rc = dm_zstd_uncompress(input, output, input_len=input_len, output_len=output_len)
        end select
    end function z_uncompress

    integer function z_uncompress_beat(input, beat, type, input_len) result(rc)
        use :: dm_beat

        character(len=*), intent(inout)        :: input     !! Compressed and Namelist-serialised beat.
        type(beat_type),  intent(out)          :: beat      !! Beat type to uncompress and deserialise.
        integer,          intent(in)           :: type      !! Input encoding type (`Z_TYPE_*`).
        integer(kind=i8), intent(in), optional :: input_len !! Actual input length.

        character(len=NML_BEAT_LEN) :: output

        rc = z_uncompress(input, output, type, input_len=input_len)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, beat)
    end function z_uncompress_beat
end module dm_z
