! Author:  Philipp Engel
! Licence: ISC
module dm_z
    !! Utility module for (de-)serialisation and (de-)compression (zlib, zstd)
    !! of derived types. Namelist is the only serialisation format supported.
    use :: dm_error
    use :: dm_kind
    use :: dm_nml
    use :: dm_zlib
    use :: dm_zstd
    implicit none (type, external)
    private

    ! Compression type enumerators.
    integer, parameter, public :: Z_TYPE_INVALID = -1 !! Invalid or unknown type.
    integer, parameter, public :: Z_TYPE_NONE    = 0  !! No compression.
    integer, parameter, public :: Z_TYPE_ZLIB    = 1  !! Deflate compression.
    integer, parameter, public :: Z_TYPE_ZSTD    = 2  !! Zstandard compression.
    integer, parameter, public :: Z_TYPE_LAST    = 2  !! Never use this.

    integer, parameter, public :: Z_TYPE_NAME_LEN = 4 !! Max. type enumerator name length.

    character(len=*), parameter, public :: Z_TYPE_NAMES(0:Z_TYPE_LAST) = [ &
        character(len=Z_TYPE_NAME_LEN) :: 'none', 'zlib', 'zstd' &
    ] !! Compression type enumerator names.

    interface dm_z_compress
        !! Generic serialisation and compression function.
        module procedure :: z_compress
        module procedure :: z_compress_beat
        module procedure :: z_compress_log
        module procedure :: z_compress_node
        module procedure :: z_compress_observ
        module procedure :: z_compress_sensor
        module procedure :: z_compress_target
    end interface dm_z_compress

    interface dm_z_uncompress
        !! Generic deserialisation and decompression function.
        module procedure :: z_uncompress
        module procedure :: z_uncompress_beat
        module procedure :: z_uncompress_log
        module procedure :: z_uncompress_node
        module procedure :: z_uncompress_observ
        module procedure :: z_uncompress_sensor
        module procedure :: z_uncompress_target
    end interface dm_z_uncompress

    interface dm_z_is_valid
        !! Generic validation function.
        module procedure :: dm_z_type_is_valid
    end interface dm_z_is_valid

    ! Public procedures.
    public :: dm_z_compress
    public :: dm_z_compress_type
    public :: dm_z_compress_types
    public :: dm_z_is_valid
    public :: dm_z_type_from_encoding
    public :: dm_z_type_from_name
    public :: dm_z_type_is_valid
    public :: dm_z_type_name
    public :: dm_z_type_to_encoding
    public :: dm_z_uncompress

    ! Private procedures.
    private :: z_compress
    private :: z_compress_beat
    private :: z_compress_log
    private :: z_compress_node
    private :: z_compress_observ
    private :: z_compress_sensor
    private :: z_compress_target

    private :: z_uncompress
    private :: z_uncompress_beat
    private :: z_uncompress_log
    private :: z_uncompress_node
    private :: z_uncompress_observ
    private :: z_uncompress_sensor
    private :: z_uncompress_target
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_z_compress_type(type, z, output, output_len, context) result(rc)
        !! Serialises derived `type` to namelist format and compresses it
        !! depending on compression type `z`. The serialised and compressed
        !! result is returned in `output`. The argument `output_len` will equal
        !! the length of `output`. The function returns `E_INVALID` if `type`
        !! is unsupported.
        use :: dm_beat,   only: beat_type
        use :: dm_log,    only: log_type
        use :: dm_node,   only: node_type
        use :: dm_observ, only: observ_type
        use :: dm_sensor, only: sensor_type
        use :: dm_target, only: target_type

        class(*),                      intent(inout)           :: type       !! Derived type to serialise and compress.
        integer,                       intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(len=:), allocatable, intent(out)             :: output     !! Serialised and compressed type.
        integer(kind=i8),              intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),       intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        rc = E_INVALID

        select type (t => type)
            type is (beat_type);   rc = z_compress_beat  (t, z, output, output_len, context)
            type is (log_type);    rc = z_compress_log   (t, z, output, output_len, context)
            type is (node_type);   rc = z_compress_node  (t, z, output, output_len, context)
            type is (observ_type); rc = z_compress_observ(t, z, output, output_len, context)
            type is (sensor_type); rc = z_compress_sensor(t, z, output, output_len, context)
            type is (target_type); rc = z_compress_target(t, z, output, output_len, context)
            class default
                if (present(output_len)) output_len = 0_i8
                output = ''
        end select
    end function dm_z_compress_type

    integer function dm_z_compress_types(types, z, output) result(rc)
        !! Serialises derived types `types` to namelist format and compresses
        !! them depending on compression type `z`. If `z` is `Z_TYPE_ZSTD`, the
        !! function uses a Zstandard compression context. The serialised and
        !! compressed results are returned in `output`. The function returns
        !! `E_INVALID` if `types` is unsupported. On error, not all strings of
        !! `output` may be allocated.
        use :: dm_string, only: string_type

        class(*),                       intent(inout) :: types(:)  !! Derived types to serialise and compress.
        integer,                        intent(in)    :: z         !! Output compression enumerator (`Z_TYPE_*`).
        type(string_type), allocatable, intent(out)   :: output(:) !! Serialised and compressed types.

        integer                 :: i, n, stat
        type(zstd_context_type) :: context

        n = size(types)

        rc = E_ALLOC
        allocate (output(n), stat=stat)
        if (stat /= 0) return

        rc = E_INVALID
        if (.not. dm_z_is_valid(z)) return

        do i = 1, n
            if (z == Z_TYPE_ZSTD) then
                ! Use Zstandard compression context.
                rc = dm_z_compress_type(types(i), z, output(i)%data, context=context)
            else
                rc = dm_z_compress_type(types(i), z, output(i)%data)
            end if

            if (dm_is_error(rc)) exit
        end do

        if (z == Z_TYPE_ZSTD) stat = dm_zstd_destroy(context)
    end function dm_z_compress_types

    pure elemental integer function dm_z_type_from_encoding(encoding) result(z)
        !! Returns compression type enumerator from HTTP content encoding:
        !!
        !! * `Z_TYPE_NONE`    – If trimmed encoding string is empty.
        !! * `Z_TYPE_ZLIB`    – On `deflate`.
        !! * `Z_TYPE_ZSTD`    – On `zstd`.
        !! * `Z_TYPE_INVALID` – On any other encoding.
        !!
        character(len=*), intent(in) :: encoding !! Content encoding name.

        select case (trim(encoding))
            case ('');        z = Z_TYPE_NONE
            case ('deflate'); z = Z_TYPE_ZLIB
            case ('zstd');    z = Z_TYPE_ZSTD
            case default;     z = Z_TYPE_INVALID
        end select
    end function dm_z_type_from_encoding

    pure elemental integer function dm_z_type_from_name(name) result(z)
        !! Returns compression type enumerator from name. The function returns
        !! `Z_TYPE_INVALID` if the name is not a valid type name.
        use :: dm_string, only: dm_to_lower

        character(len=*), intent(in)   :: name !! Compression enumerator name.
        character(len=Z_TYPE_NAME_LEN) :: name_

        ! Normalise type name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (Z_TYPE_NAMES(Z_TYPE_NONE)); z = Z_TYPE_NONE
            case (Z_TYPE_NAMES(Z_TYPE_ZLIB)); z = Z_TYPE_ZLIB
            case (Z_TYPE_NAMES(Z_TYPE_ZSTD)); z = Z_TYPE_ZSTD
            case default;                     z = Z_TYPE_INVALID
        end select
    end function dm_z_type_from_name

    pure elemental logical function dm_z_type_is_valid(z) result(valid)
        !! Returns `.true.` if the given compression enumerator `z` is
        !! valid. The type `Z_TYPE_NONE` is a valid type, and `Z_TYPE_INVALID`
        !! is invalid.
        integer, intent(in) :: z !! Compression enumerator.

        valid = (z >= Z_TYPE_NONE .and. z <= Z_TYPE_LAST)
    end function dm_z_type_is_valid

    pure function dm_z_type_name(z) result(name)
        !! Returns compression type name as allocatable string.
        integer, intent(in)           :: z    !! Compression enumerator.
        character(len=:), allocatable :: name !! Compression type name.

        if (.not. dm_z_is_valid(z)) then
            name = 'invalid'
            return
        end if

        name = trim(Z_TYPE_NAMES(z))
    end function dm_z_type_name

    pure function dm_z_type_to_encoding(z) result(encoding)
        !! Returns allocatable HTTP content type string from compression type
        !! enumerator. The function returns an empty string for types
        !! `Z_TYPE_INVALID` and `Z_TYPE_NONE`.
        integer, intent(in)           :: z        !! Compression enumerator.
        character(len=:), allocatable :: encoding !! Content encoding string.

        select case (z)
            case (Z_TYPE_ZLIB); encoding = 'deflate'
            case (Z_TYPE_ZSTD); encoding = 'zstd'
            case default;       encoding = ''
        end select
    end function dm_z_type_to_encoding

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function z_compress(input, z, output, input_len, output_len, context) result(rc)
        !! Compresses given input and returns the result in allocatable string
        !! `output`. The actual length may be smaller than the string length
        !! and is returned in `output_len`.
        !!
        !! The following compression types are supported:
        !!
        !! * `Z_TYPE_NONE` – No compression (output equals input).
        !! * `Z_TYPE_ZLIB` – Deflate compression.
        !! * `Z_TYPE_ZSTD` – Zstandard compression.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_ZLIB` if zlib library call failed.
        !! * `E_ZSTD` if zstd library call failed.
        !!
        character(len=*),              intent(inout)           :: input      !! Uncompressed data.
        integer,                       intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(len=:), allocatable, intent(out)             :: output     !! Compressed data.
        integer(kind=i8),              intent(in),    optional :: input_len  !! Actual input length.
        integer(kind=i8),              intent(out),   optional :: output_len !! Actual output length.
        type(zstd_context_type),       intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        integer :: level

        if (present(output_len)) output_len = 0_i8

        rc = E_INVALID
        if (.not. dm_z_is_valid(z)) then
            output = ''
            return
        end if

        select case (z)
            case (Z_TYPE_NONE)
                rc = E_NONE
                if (present(input_len)) then
                    output = input(:input_len)
                else
                    output = input
                end if

                if (present(output_len)) output_len = len(output, kind=i8)

            case (Z_TYPE_ZLIB)
                rc = dm_zlib_compress(input, output, input_len=input_len, output_len=output_len)

            case (Z_TYPE_ZSTD)
                level = dm_zstd_level_default()

                if (present(context)) then
                    ! Use Zstandard compression context.
                    rc = dm_zstd_compress(context, input, output, level, input_len, output_len)
                else
                    rc = dm_zstd_compress(input, output, level, input_len, output_len)
                end if
        end select
    end function z_compress

    integer function z_compress_beat(beat, z, output, output_len, context) result(rc)
        !! Serialises beat `beat` to namelist format and compresses it
        !! depending on `z`. The serialised and compressed result is returned
        !! in `output`. The argument `output_len` will equal the length of
        !! `output`.
        use :: dm_beat, only: beat_type

        type(beat_type),               intent(inout)           :: beat       !! Beat type to serialise and compress.
        integer,                       intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(len=:), allocatable, intent(out)             :: output     !! Serialised and compressed beat.
        integer(kind=i8),              intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),       intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_BEAT_LEN) :: input
        integer(kind=i8)            :: output_len_

        rc = dm_nml_from(beat, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, kind=i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_beat

    integer function z_compress_log(log, z, output, output_len, context) result(rc)
        !! Serialises log `log` to namelist format and compresses it depending
        !! on `z`. The serialised and compressed result is returned in
        !! `output`. The argument `output_len` will equal the length of
        !! `output`.
        use :: dm_log, only: log_type

        type(log_type),                intent(inout)           :: log        !! Log type to serialise and compress.
        integer,                       intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(len=:), allocatable, intent(out)             :: output     !! Serialised and compressed log.
        integer(kind=i8),              intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),       intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_LOG_LEN) :: input
        integer(kind=i8)           :: output_len_

        rc = dm_nml_from(log, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, kind=i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_log

    integer function z_compress_node(node, z, output, output_len, context) result(rc)
        !! Serialises node `node` to namelist format and compresses it
        !! depending on `z`. The serialised and compressed result is
        !! returned in `output`. The argument `output_len` will equal the
        !! length of `output`.
        use :: dm_node, only: node_type

        type(node_type),               intent(inout)           :: node       !! Node type to serialise and compress.
        integer,                       intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(len=:), allocatable, intent(out)             :: output     !! Serialised and compressed node.
        integer(kind=i8),              intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),       intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_NODE_LEN) :: input
        integer(kind=i8)            :: output_len_

        rc = dm_nml_from(node, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, kind=i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_node

    integer function z_compress_observ(observ, z, output, output_len, context) result(rc)
        !! Serialises observation `observ` to namelist format and compresses it
        !! depending on `z`. The serialised and compressed result is returned
        !! in `output`. The argument `output_len` will equal the length of
        !! `output`.
        use :: dm_observ, only: observ_type

        type(observ_type),             intent(inout)           :: observ     !! Observation type to serialise and compress.
        integer,                       intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(len=:), allocatable, intent(out)             :: output     !! Serialised and compressed observation.
        integer(kind=i8),              intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),       intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_OBSERV_LEN) :: input
        integer(kind=i8)              :: output_len_

        rc = dm_nml_from(observ, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, kind=i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_observ

    integer function z_compress_sensor(sensor, z, output, output_len, context) result(rc)
        !! Serialises sensor `sensor` to namelist format and compresses it
        !! depending on `z`. The serialised and compressed result is
        !! returned in `output`. The argument `output_len` will equal the
        !! length of `output`.
        use :: dm_sensor, only: sensor_type

        type(sensor_type),             intent(inout)           :: sensor     !! Sensor type to serialise and compress.
        integer,                       intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(len=:), allocatable, intent(out)             :: output     !! Serialised and compressed sensor.
        integer(kind=i8),              intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),       intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_SENSOR_LEN) :: input
        integer(kind=i8)              :: output_len_

        rc = dm_nml_from(sensor, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, kind=i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_sensor

    integer function z_compress_target(target, z, output, output_len, context) result(rc)
        !! Serialises target `target` to namelist format and compresses it
        !! depending on `z`. The serialised and compressed result is
        !! returned in `output`. The argument `output_len` will equal the
        !! length of `output`.
        use :: dm_target, only: target_type

        type(target_type),             intent(inout)           :: target     !! Target type to serialise and compress.
        integer,                       intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(len=:), allocatable, intent(out)             :: output     !! Serialised and compressed target.
        integer(kind=i8),              intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),       intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_TARGET_LEN) :: input
        integer(kind=i8)              :: output_len_

        rc = dm_nml_from(target, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, kind=i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_target

    integer function z_uncompress(input, z, output, input_len, output_len, context) result(rc)
        !! Uncompresses given input and returns the result in string `output`.
        !! The string must be allocated and large enough to hold the
        !! uncompressed data. The actual length may be smaller than the output
        !! length and is returned in `output_len`.
        !!
        !! The following compression types are supported:
        !!
        !! * `Z_TYPE_NONE` – No compression (output equals input).
        !! * `Z_TYPE_ZLIB` – Deflate compression.
        !! * `Z_TYPE_ZSTD` – Zstandard compression.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if compression type is invalid.
        !! * `E_ZLIB` if zlib library call failed.
        !! * `E_ZSTD` if zstd library call failed.
        !!
        character(len=*),        intent(inout)           :: input      !! Compressed data.
        integer,                 intent(in)              :: z          !! Input compression enumerator (`Z_TYPE_*`).
        character(len=*),        intent(inout)           :: output     !! Uncompressed data.
        integer(kind=i8),        intent(in),    optional :: input_len  !! Actual input length.
        integer(kind=i8),        intent(out),   optional :: output_len !! Actual output length.
        type(zstd_context_type), intent(inout), optional :: context    !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        if (present(output_len)) output_len = 0_i8

        rc = E_INVALID
        if (.not. dm_z_is_valid(z)) return

        rc = E_NONE
        select case (z)
            case (Z_TYPE_NONE)
                if (present(input_len)) then
                    output = input(:input_len)
                else
                    output = input
                end if

                if (present(output_len)) output_len = len(output, kind=i8)

            case (Z_TYPE_ZLIB)
                rc = dm_zlib_uncompress(input, output, input_len=input_len, output_len=output_len)

            case (Z_TYPE_ZSTD)
                if (present(context)) then
                    rc = dm_zstd_uncompress(context, input, output, input_len=input_len, output_len=output_len)
                else
                    rc = dm_zstd_uncompress(input, output, input_len=input_len, output_len=output_len)
                end if
        end select
    end function z_uncompress

    integer function z_uncompress_beat(input, z, beat, input_len, context) result(rc)
        !! Uncompressed compressed beat namelist `input` and returns
        !! deserialised type in `beat`.
        use :: dm_beat, only: beat_type

        character(len=*),        intent(inout)           :: input     !! Compressed and Namelist-serialised beat.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(beat_type),         intent(out)             :: beat      !! Uncompressed and deserialised beat.
        integer(kind=i8),        intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_BEAT_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, beat)
    end function z_uncompress_beat

    integer function z_uncompress_log(input, z, log, input_len, context) result(rc)
        !! Uncompressed compressed log namelist `input` and returns
        !! deserialised type in `log`.
        use :: dm_log, only: log_type

        character(len=*),        intent(inout)           :: input     !! Compressed and Namelist-serialised log.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(log_type),          intent(out)             :: log       !! Uncompressed and deserialised log.
        integer(kind=i8),        intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_LOG_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, log)
    end function z_uncompress_log

    integer function z_uncompress_node(input, z, node, input_len, context) result(rc)
        !! Uncompressed compressed node namelist `input` and returns
        !! deserialised type in `node`.
        use :: dm_node, only: node_type

        character(len=*),        intent(inout)           :: input     !! Compressed and Namelist-serialised node.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(node_type),         intent(out)             :: node      !! Uncompressed and deserialised node.
        integer(kind=i8),        intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_NODE_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, node)
    end function z_uncompress_node

    integer function z_uncompress_observ(input, z, observ, input_len, context) result(rc)
        !! Uncompressed compressed observation namelist `input` and returns
        !! deserialised z in `observ`.
        use :: dm_observ, only: observ_type

        character(len=*),        intent(inout)           :: input     !! Compressed and Namelist-serialised observation.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(observ_type),       intent(out)             :: observ    !! Uncompressed and deserialised observation.
        integer(kind=i8),        intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_OBSERV_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, observ)
    end function z_uncompress_observ

    integer function z_uncompress_sensor(input, z, sensor, input_len, context) result(rc)
        !! Uncompressed compressed sensor namelist `input` and returns
        !! deserialised type in `sensor`.
        use :: dm_sensor, only: sensor_type

        character(len=*),        intent(inout)           :: input     !! Compressed and Namelist-serialised sensor.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(sensor_type),       intent(out)             :: sensor    !! Uncompressed and deserialised sensor.
        integer(kind=i8),        intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_SENSOR_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, sensor)
    end function z_uncompress_sensor

    integer function z_uncompress_target(input, z, target, input_len, context) result(rc)
        !! Uncompressed compressed target namelist `input` and returns
        !! deserialised type in `target`.
        use :: dm_target, only: target_type

        character(len=*),        intent(inout)           :: input     !! Compressed and Namelist-serialised target.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(target_type),       intent(out)             :: target    !! Uncompressed and deserialised target.
        integer(kind=i8),        intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(len=NML_TARGET_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, target)
    end function z_uncompress_target
end module dm_z
