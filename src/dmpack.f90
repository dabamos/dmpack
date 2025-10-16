! Author:  Philipp Engel
! Licence: ISC
module dmpack
    !! The DMPACK library module. Import this module to access any DMPACK
    !! procedures:
    !!
    !! ```fortran
    !! ! app.f90
    !! program main
    !!     use :: dmpack
    !!     implicit none (type, external)
    !!
    !!     ! Initialise DMPACK.
    !!     call dm_init()
    !!
    !!     ! Call any DMPACK procedures here.
    !!     call dm_version_out()
    !! end program main
    !! ```
    !!
    !! Link the program against static library `libdmpack.a`:
    !!
    !! ```
    !! $ gfortran -I/usr/local/include/dmpack -o app app.f90 /usr/local/lib/libdmpack.a
    !! ```
    !!
    !! Change `/usr/local` to the installation prefix.
    use :: dm_ansi
    use :: dm_api_status
    use :: dm_arg
    use :: dm_ascii
    use :: dm_atom
    use :: dm_base64
    use :: dm_beat
    use :: dm_block
    use :: dm_c
    use :: dm_camera
    use :: dm_cgi
    use :: dm_cgi_router
    use :: dm_config
    use :: dm_const
    use :: dm_coord
    use :: dm_crypto
    use :: dm_csv
    use :: dm_db
    use :: dm_db_api
    use :: dm_db_count
    use :: dm_db_json
    use :: dm_db_pragma
    use :: dm_db_query
    use :: dm_db_row
    use :: dm_db_table
    use :: dm_dp
    use :: dm_dwd
    use :: dm_dwd_api
    use :: dm_env
    use :: dm_error
    use :: dm_fcgi
    use :: dm_fifo
    use :: dm_file
    use :: dm_filter
    use :: dm_format
    use :: dm_freebsd
    use :: dm_ftp
    use :: dm_geocom
    use :: dm_geocom_api
    use :: dm_geocom_error
    use :: dm_geocom_type
    use :: dm_geojson
    use :: dm_gm
    use :: dm_hash
    use :: dm_hash_table
    use :: dm_hdf5
    use :: dm_html
    use :: dm_http
    use :: dm_id
    use :: dm_im
    use :: dm_image
    use :: dm_job
    use :: dm_js
    use :: dm_json
    use :: dm_jsonl
    use :: dm_kind
    use :: dm_la
    use :: dm_linux
    use :: dm_log
    use :: dm_logger
    use :: dm_lua
    use :: dm_lua_api
    use :: dm_lua_geocom
    use :: dm_lua_lib
    use :: dm_mail
    use :: dm_mime
    use :: dm_modbus
    use :: dm_modbus_register
    use :: dm_modbus_type
    use :: dm_mqtt
    use :: dm_mqueue
    use :: dm_mqueue_util
    use :: dm_mutex
    use :: dm_net
    use :: dm_netstring
    use :: dm_nml
    use :: dm_node
    use :: dm_observ
    use :: dm_path
    use :: dm_person
    use :: dm_pipe
    use :: dm_platform
    use :: dm_plot
    use :: dm_regex
    use :: dm_report
    use :: dm_request
    use :: dm_response
    use :: dm_roff
    use :: dm_rpc
    use :: dm_rts
    use :: dm_sem
    use :: dm_sensor
    use :: dm_serial
    use :: dm_signal
    use :: dm_sql
    use :: dm_string
    use :: dm_sync
    use :: dm_system
    use :: dm_target
    use :: dm_test
    use :: dm_thread
    use :: dm_time
    use :: dm_timer
    use :: dm_transfer
    use :: dm_transform
    use :: dm_tty
    use :: dm_type
    use :: dm_unit
    use :: dm_util
    use :: dm_uuid
    use :: dm_ve
    use :: dm_version
    use :: dm_z
    use :: dm_zlib
    use :: dm_zstd
    implicit none (type, external)
    public

    public :: dm_init
contains
    subroutine dm_init()
        !! Initialises DMPACK. Shall be executed once before any DMPACK
        !! routines are called to initialise the PRNG.

        call random_init(repeatable=.false., image_distinct=.false.)
    end subroutine dm_init
end module dmpack
