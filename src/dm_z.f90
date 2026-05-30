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

    character(*), parameter, public :: Z_TYPE_NAMES(Z_TYPE_NONE:Z_TYPE_LAST) = [ &
        character(Z_TYPE_NAME_LEN) :: 'none', 'zlib', 'zstd' &
    ] !! Compression type enumerator names.

    interface dm_z_compress
        !! Generic serialisation and compression function.
        module procedure :: z_compress
        module procedure :: z_compress_beat
        module procedure :: z_compress_beats
        module procedure :: z_compress_image
        module procedure :: z_compress_images
        module procedure :: z_compress_log
        module procedure :: z_compress_logs
        module procedure :: z_compress_node
        module procedure :: z_compress_nodes
        module procedure :: z_compress_observ
        module procedure :: z_compress_observs
        module procedure :: z_compress_sensor
        module procedure :: z_compress_sensors
        module procedure :: z_compress_target
        module procedure :: z_compress_targets
    end interface dm_z_compress

    interface dm_z_uncompress
        !! Generic deserialisation and decompression function.
        module procedure :: z_uncompress
        module procedure :: z_uncompress_beat
        module procedure :: z_uncompress_image
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
    private :: z_compress_beats
    private :: z_compress_image
    private :: z_compress_images
    private :: z_compress_log
    private :: z_compress_logs
    private :: z_compress_node
    private :: z_compress_nodes
    private :: z_compress_observ
    private :: z_compress_observs
    private :: z_compress_sensor
    private :: z_compress_sensors
    private :: z_compress_target
    private :: z_compress_targets

    private :: z_uncompress
    private :: z_uncompress_beat
    private :: z_uncompress_image
    private :: z_uncompress_log
    private :: z_uncompress_node
    private :: z_uncompress_observ
    private :: z_uncompress_sensor
    private :: z_uncompress_target
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    pure elemental integer function dm_z_type_from_encoding(encoding) result(z)
        !! Returns compression type enumerator from HTTP content encoding:
        !!
        !! * `Z_TYPE_NONE`    – If trimmed encoding string is empty.
        !! * `Z_TYPE_ZLIB`    – On `deflate`.
        !! * `Z_TYPE_ZSTD`    – On `zstd`.
        !! * `Z_TYPE_INVALID` – On any other encoding.
        !!
        character(*), intent(in) :: encoding !! Content encoding name.

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

        character(*), intent(in) :: name !! Compression enumerator name.

        character(Z_TYPE_NAME_LEN) :: name_

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
        integer, intent(in)       :: z    !! Compression enumerator.
        character(:), allocatable :: name !! Compression type name.

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
        integer, intent(in)       :: z        !! Compression enumerator.
        character(:), allocatable :: encoding !! Content encoding string.

        select case (z)
            case (Z_TYPE_ZLIB); encoding = 'deflate'
            case (Z_TYPE_ZSTD); encoding = 'zstd'
            case default;       encoding = ''
        end select
    end function dm_z_type_to_encoding

    ! **************************************************************************
    ! PRIVATE PROCEDURES
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
        character(*),              intent(inout)           :: input      !! Uncompressed data.
        integer,                   intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(:), allocatable, intent(out)             :: output     !! Compressed data.
        integer(i8),               intent(in),    optional :: input_len  !! Actual input length.
        integer(i8),               intent(out),   optional :: output_len !! Actual output length.
        type(zstd_context_type),   intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

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

                if (present(output_len)) output_len = len(output, i8)

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

        type(beat_type),           intent(inout)           :: beat       !! Beat type to serialise and compress.
        integer,                   intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(:), allocatable, intent(out)             :: output     !! Serialised and compressed beat.
        integer(i8),               intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),   intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(NML_BEAT_LEN) :: input
        integer(i8)            :: output_len_

        rc = dm_nml_from(beat, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_beat

    integer function z_compress_beats(beats, z, output) result(rc)
        !! Serialises beats to namelist format and compresses them
        !! depending on compression type `z`. If `z` is `Z_TYPE_ZSTD`, the
        !! function uses a Zstandard compression context. The serialised and
        !! compressed results are returned in `output`. On error, not all
        !! strings of `output` may be allocated.
        use :: dm_beat
        use :: dm_string, only: string_type

        type(beat_type),                intent(inout) :: beats(:)  !! Beats.
        integer,                        intent(in)    :: z         !! Output compression enumerator (`Z_TYPE_*`).
        type(string_type), allocatable, intent(out)   :: output(:) !! Serialised and compressed beats.

        integer                 :: i, n, stat
        type(zstd_context_type) :: context

        n = size(beats)

        rc = E_ALLOC
        allocate (output(n), stat=stat)
        if (stat /= 0) return

        rc = E_INVALID
        if (.not. dm_z_is_valid(z)) return

        do i = 1, n
            if (z == Z_TYPE_ZSTD) then
                ! Use Zstandard compression context.
                rc = dm_z_compress(beats(i), z, output(i)%data, context=context)
            else
                rc = dm_z_compress(beats(i), z, output(i)%data)
            end if

            if (dm_is_error(rc)) exit
        end do

        if (z == Z_TYPE_ZSTD) call dm_zstd_destroy(context)
    end function z_compress_beats

    integer function z_compress_image(image, z, output, output_len, context) result(rc)
        !! Serialises image `image` to namelist format and compresses it
        !! depending on `z`. The serialised and compressed result is returned
        !! in `output`. The argument `output_len` will equal the length of
        !! `output`.
        use :: dm_image, only: image_type

        type(image_type),          intent(inout)           :: image      !! Image type to serialise and compress.
        integer,                   intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(:), allocatable, intent(out)             :: output     !! Serialised and compressed image.
        integer(i8),               intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),   intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(NML_IMAGE_LEN) :: input
        integer(i8)             :: output_len_

        rc = dm_nml_from(image, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_image

    integer function z_compress_images(images, z, output) result(rc)
        !! Serialises images to namelist format and compresses them
        !! depending on compression type `z`. If `z` is `Z_TYPE_ZSTD`, the
        !! function uses a Zstandard compression context. The serialised and
        !! compressed results are returned in `output`. On error, not all
        !! strings of `output` may be allocated.
        use :: dm_image
        use :: dm_string, only: string_type

        type(image_type),               intent(inout) :: images(:) !! Images.
        integer,                        intent(in)    :: z         !! Output compression enumerator (`Z_TYPE_*`).
        type(string_type), allocatable, intent(out)   :: output(:) !! Serialised and compressed images.

        integer                 :: i, n, stat
        type(zstd_context_type) :: context

        n = size(images)

        rc = E_ALLOC
        allocate (output(n), stat=stat)
        if (stat /= 0) return

        rc = E_INVALID
        if (.not. dm_z_is_valid(z)) return

        do i = 1, n
            if (z == Z_TYPE_ZSTD) then
                ! Use Zstandard compression context.
                rc = dm_z_compress(images(i), z, output(i)%data, context=context)
            else
                rc = dm_z_compress(images(i), z, output(i)%data)
            end if

            if (dm_is_error(rc)) exit
        end do

        if (z == Z_TYPE_ZSTD) call dm_zstd_destroy(context)
    end function z_compress_images

    integer function z_compress_log(log, z, output, output_len, context) result(rc)
        !! Serialises log `log` to namelist format and compresses it depending
        !! on `z`. The serialised and compressed result is returned in
        !! `output`. The argument `output_len` will equal the length of
        !! `output`.
        use :: dm_log, only: log_type

        type(log_type),            intent(inout)           :: log        !! Log type to serialise and compress.
        integer,                   intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(:), allocatable, intent(out)             :: output     !! Serialised and compressed log.
        integer(i8),               intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),   intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(NML_LOG_LEN) :: input
        integer(i8)           :: output_len_

        rc = dm_nml_from(log, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_log

    integer function z_compress_logs(logs, z, output) result(rc)
        !! Serialises logs to namelist format and compresses them
        !! depending on compression type `z`. If `z` is `Z_TYPE_ZSTD`, the
        !! function uses a Zstandard compression context. The serialised and
        !! compressed results are returned in `output`. On error, not all
        !! strings of `output` may be allocated.
        use :: dm_log
        use :: dm_string, only: string_type

        type(log_type),                 intent(inout) :: logs(:)   !! Logs.
        integer,                        intent(in)    :: z         !! Output compression enumerator (`Z_TYPE_*`).
        type(string_type), allocatable, intent(out)   :: output(:) !! Serialised and compressed logs.

        integer                 :: i, n, stat
        type(zstd_context_type) :: context

        n = size(logs)

        rc = E_ALLOC
        allocate (output(n), stat=stat)
        if (stat /= 0) return

        rc = E_INVALID
        if (.not. dm_z_is_valid(z)) return

        do i = 1, n
            if (z == Z_TYPE_ZSTD) then
                ! Use Zstandard compression context.
                rc = dm_z_compress(logs(i), z, output(i)%data, context=context)
            else
                rc = dm_z_compress(logs(i), z, output(i)%data)
            end if

            if (dm_is_error(rc)) exit
        end do

        if (z == Z_TYPE_ZSTD) call dm_zstd_destroy(context)
    end function z_compress_logs

    integer function z_compress_node(node, z, output, output_len, context) result(rc)
        !! Serialises node `node` to namelist format and compresses it
        !! depending on `z`. The serialised and compressed result is
        !! returned in `output`. The argument `output_len` will equal the
        !! length of `output`.
        use :: dm_node, only: node_type

        type(node_type),           intent(inout)           :: node       !! Node type to serialise and compress.
        integer,                   intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(:), allocatable, intent(out)             :: output     !! Serialised and compressed node.
        integer(i8),               intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),   intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(NML_NODE_LEN) :: input
        integer(i8)            :: output_len_

        rc = dm_nml_from(node, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_node

    integer function z_compress_nodes(nodes, z, output) result(rc)
        !! Serialises nodes to namelist format and compresses them
        !! depending on compression type `z`. If `z` is `Z_TYPE_ZSTD`, the
        !! function uses a Zstandard compression context. The serialised and
        !! compressed results are returned in `output`. On error, not all
        !! strings of `output` may be allocated.
        use :: dm_node
        use :: dm_string, only: string_type

        type(node_type),                intent(inout) :: nodes(:)  !! Nodes.
        integer,                        intent(in)    :: z         !! Output compression enumerator (`Z_TYPE_*`).
        type(string_type), allocatable, intent(out)   :: output(:) !! Serialised and compressed nodes.

        integer                 :: i, n, stat
        type(zstd_context_type) :: context

        n = size(nodes)

        rc = E_ALLOC
        allocate (output(n), stat=stat)
        if (stat /= 0) return

        rc = E_INVALID
        if (.not. dm_z_is_valid(z)) return

        do i = 1, n
            if (z == Z_TYPE_ZSTD) then
                ! Use Zstandard compression context.
                rc = dm_z_compress(nodes(i), z, output(i)%data, context=context)
            else
                rc = dm_z_compress(nodes(i), z, output(i)%data)
            end if

            if (dm_is_error(rc)) exit
        end do

        if (z == Z_TYPE_ZSTD) call dm_zstd_destroy(context)
    end function z_compress_nodes

    integer function z_compress_observ(observ, z, output, output_len, context) result(rc)
        !! Serialises observation `observ` to namelist format and compresses it
        !! depending on `z`. The serialised and compressed result is returned
        !! in `output`. The argument `output_len` will equal the length of
        !! `output`.
        use :: dm_observ, only: observ_type

        type(observ_type),         intent(inout)           :: observ     !! Observation type to serialise and compress.
        integer,                   intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(:), allocatable, intent(out)             :: output     !! Serialised and compressed observation.
        integer(i8),               intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),   intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(NML_OBSERV_LEN) :: input
        integer(i8)              :: output_len_

        rc = dm_nml_from(observ, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_observ

    integer function z_compress_observs(observs, z, output) result(rc)
        !! Serialises observations to namelist format and compresses them
        !! depending on compression type `z`. If `z` is `Z_TYPE_ZSTD`, the
        !! function uses a Zstandard compression context. The serialised and
        !! compressed results are returned in `output`. On error, not all
        !! strings of `output` may be allocated.
        use :: dm_observ
        use :: dm_string, only: string_type

        type(observ_type),              intent(inout) :: observs(:) !! Observations.
        integer,                        intent(in)    :: z          !! Output compression enumerator (`Z_TYPE_*`).
        type(string_type), allocatable, intent(out)   :: output(:)  !! Serialised and compressed observations.

        integer                 :: i, n, stat
        type(zstd_context_type) :: context

        n = size(observs)

        rc = E_ALLOC
        allocate (output(n), stat=stat)
        if (stat /= 0) return

        rc = E_INVALID
        if (.not. dm_z_is_valid(z)) return

        do i = 1, n
            if (z == Z_TYPE_ZSTD) then
                ! Use Zstandard compression context.
                rc = dm_z_compress(observs(i), z, output(i)%data, context=context)
            else
                rc = dm_z_compress(observs(i), z, output(i)%data)
            end if

            if (dm_is_error(rc)) exit
        end do

        if (z == Z_TYPE_ZSTD) call dm_zstd_destroy(context)
    end function z_compress_observs

    integer function z_compress_sensor(sensor, z, output, output_len, context) result(rc)
        !! Serialises sensor `sensor` to namelist format and compresses it
        !! depending on `z`. The serialised and compressed result is
        !! returned in `output`. The argument `output_len` will equal the
        !! length of `output`.
        use :: dm_sensor, only: sensor_type

        type(sensor_type),         intent(inout)           :: sensor     !! Sensor type to serialise and compress.
        integer,                   intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(:), allocatable, intent(out)             :: output     !! Serialised and compressed sensor.
        integer(i8),               intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),   intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(NML_SENSOR_LEN) :: input
        integer(i8)              :: output_len_

        rc = dm_nml_from(sensor, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_sensor

    integer function z_compress_sensors(sensors, z, output) result(rc)
        !! Serialises sensors to namelist format and compresses them
        !! depending on compression type `z`. If `z` is `Z_TYPE_ZSTD`, the
        !! function uses a Zstandard compression context. The serialised and
        !! compressed results are returned in `output`. On error, not all
        !! strings of `output` may be allocated.
        use :: dm_sensor
        use :: dm_string, only: string_type

        type(sensor_type),              intent(inout) :: sensors(:) !! Sensors.
        integer,                        intent(in)    :: z          !! Output compression enumerator (`Z_TYPE_*`).
        type(string_type), allocatable, intent(out)   :: output(:)  !! Serialised and compressed sensors.

        integer                 :: i, n, stat
        type(zstd_context_type) :: context

        n = size(sensors)

        rc = E_ALLOC
        allocate (output(n), stat=stat)
        if (stat /= 0) return

        rc = E_INVALID
        if (.not. dm_z_is_valid(z)) return

        do i = 1, n
            if (z == Z_TYPE_ZSTD) then
                ! Use Zstandard compression context.
                rc = dm_z_compress(sensors(i), z, output(i)%data, context=context)
            else
                rc = dm_z_compress(sensors(i), z, output(i)%data)
            end if

            if (dm_is_error(rc)) exit
        end do

        if (z == Z_TYPE_ZSTD) call dm_zstd_destroy(context)
    end function z_compress_sensors

    integer function z_compress_target(target, z, output, output_len, context) result(rc)
        !! Serialises target `target` to namelist format and compresses it
        !! depending on `z`. The serialised and compressed result is
        !! returned in `output`. The argument `output_len` will equal the
        !! length of `output`.
        use :: dm_target, only: target_type

        type(target_type),         intent(inout)           :: target     !! Target type to serialise and compress.
        integer,                   intent(in)              :: z          !! Output compression enumerator (`Z_TYPE_*`).
        character(:), allocatable, intent(out)             :: output     !! Serialised and compressed target.
        integer(i8),               intent(out),   optional :: output_len !! Output length.
        type(zstd_context_type),   intent(inout), optional :: context    !! Zstandard compression context to use with type `Z_TYPE_ZSTD`.

        character(NML_TARGET_LEN) :: input
        integer(i8)              :: output_len_

        rc = dm_nml_from(target, input)
        if (dm_is_error(rc)) return
        rc = z_compress(input, z, output, input_len=len_trim(input, i8), output_len=output_len_, context=context)
        if (len(output) /= output_len_) output = output(:output_len_)
        if (present(output_len)) output_len = output_len_
    end function z_compress_target

    integer function z_compress_targets(targets, z, output) result(rc)
        !! Serialises targets to namelist format and compresses them
        !! depending on compression type `z`. If `z` is `Z_TYPE_ZSTD`, the
        !! function uses a Zstandard compression context. The serialised and
        !! compressed results are returned in `output`. On error, not all
        !! strings of `output` may be allocated.
        use :: dm_target
        use :: dm_string, only: string_type

        type(target_type),              intent(inout) :: targets(:) !! Targets.
        integer,                        intent(in)    :: z          !! Output compression enumerator (`Z_TYPE_*`).
        type(string_type), allocatable, intent(out)   :: output(:)  !! Serialised and compressed targets.

        integer                 :: i, n, stat
        type(zstd_context_type) :: context

        n = size(targets)

        rc = E_ALLOC
        allocate (output(n), stat=stat)
        if (stat /= 0) return

        rc = E_INVALID
        if (.not. dm_z_is_valid(z)) return

        do i = 1, n
            if (z == Z_TYPE_ZSTD) then
                ! Use Zstandard compression context.
                rc = dm_z_compress(targets(i), z, output(i)%data, context=context)
            else
                rc = dm_z_compress(targets(i), z, output(i)%data)
            end if

            if (dm_is_error(rc)) exit
        end do

        if (z == Z_TYPE_ZSTD) call dm_zstd_destroy(context)
    end function z_compress_targets

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
        character(*),            intent(inout)           :: input      !! Compressed data.
        integer,                 intent(in)              :: z          !! Input compression enumerator (`Z_TYPE_*`).
        character(*),            intent(inout)           :: output     !! Uncompressed data.
        integer(i8),             intent(in),    optional :: input_len  !! Actual input length.
        integer(i8),             intent(out),   optional :: output_len !! Actual output length.
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

                if (present(output_len)) output_len = len(output, i8)

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

        character(*),            intent(inout)           :: input     !! Compressed and Namelist-serialised beat.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(beat_type),         intent(out)             :: beat      !! Uncompressed and deserialised beat.
        integer(i8),             intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(NML_BEAT_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, beat)
    end function z_uncompress_beat

    integer function z_uncompress_image(input, z, image, input_len, context) result(rc)
        !! Uncompressed compressed image namelist `input` and returns
        !! deserialised type in `image`.
        use :: dm_image, only: image_type

        character(*),            intent(inout)           :: input     !! Compressed and Namelist-serialised image.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(image_type),        intent(out)             :: image     !! Uncompressed and deserialised image.
        integer(i8),             intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(NML_IMAGE_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, image)
    end function z_uncompress_image

    integer function z_uncompress_log(input, z, log, input_len, context) result(rc)
        !! Uncompressed compressed log namelist `input` and returns
        !! deserialised type in `log`.
        use :: dm_log, only: log_type

        character(*),            intent(inout)           :: input     !! Compressed and Namelist-serialised log.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(log_type),          intent(out)             :: log       !! Uncompressed and deserialised log.
        integer(i8),             intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(NML_LOG_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, log)
    end function z_uncompress_log

    integer function z_uncompress_node(input, z, node, input_len, context) result(rc)
        !! Uncompressed compressed node namelist `input` and returns
        !! deserialised type in `node`.
        use :: dm_node, only: node_type

        character(*),            intent(inout)           :: input     !! Compressed and Namelist-serialised node.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(node_type),         intent(out)             :: node      !! Uncompressed and deserialised node.
        integer(i8),             intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(NML_NODE_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, node)
    end function z_uncompress_node

    integer function z_uncompress_observ(input, z, observ, input_len, context) result(rc)
        !! Uncompressed compressed observation namelist `input` and returns
        !! deserialised z in `observ`.
        use :: dm_observ, only: observ_type

        character(*),            intent(inout)           :: input     !! Compressed and Namelist-serialised observation.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(observ_type),       intent(out)             :: observ    !! Uncompressed and deserialised observation.
        integer(i8),             intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(NML_OBSERV_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, observ)
    end function z_uncompress_observ

    integer function z_uncompress_sensor(input, z, sensor, input_len, context) result(rc)
        !! Uncompressed compressed sensor namelist `input` and returns
        !! deserialised type in `sensor`.
        use :: dm_sensor, only: sensor_type

        character(*),            intent(inout)           :: input     !! Compressed and Namelist-serialised sensor.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(sensor_type),       intent(out)             :: sensor    !! Uncompressed and deserialised sensor.
        integer(i8),             intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(NML_SENSOR_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, sensor)
    end function z_uncompress_sensor

    integer function z_uncompress_target(input, z, target, input_len, context) result(rc)
        !! Uncompressed compressed target namelist `input` and returns
        !! deserialised type in `target`.
        use :: dm_target, only: target_type

        character(*),            intent(inout)           :: input     !! Compressed and Namelist-serialised target.
        integer,                 intent(in)              :: z         !! Input compression enumerator (`Z_TYPE_*`).
        type(target_type),       intent(out)             :: target    !! Uncompressed and deserialised target.
        integer(i8),             intent(in),    optional :: input_len !! Actual input length.
        type(zstd_context_type), intent(inout), optional :: context   !! Zstandard decompression context to use with type `Z_TYPE_ZSTD`.

        character(NML_TARGET_LEN) :: output

        rc = z_uncompress(input, z, output, input_len=input_len, context=context)
        if (dm_is_error(rc)) return
        rc = dm_nml_to(output, target)
    end function z_uncompress_target
end module dm_z
