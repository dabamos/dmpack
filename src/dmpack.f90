! dmpack.f90
!
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
    !!     ! ...
    !! end program main
    !! ```
    !!
    !! Link the program against `libdmpack.a`:
    !!
    !! ```
    !! $ gfortran -o app app.f90 libdmpack.a
    !! ```
    use :: dm_ansi
    use :: dm_api
    use :: dm_app
    use :: dm_arg
    use :: dm_ascii
    use :: dm_atom
    use :: dm_base64
    use :: dm_beat
    use :: dm_block
    use :: dm_cgi
    use :: dm_config
    use :: dm_const
    use :: dm_convert
    use :: dm_csv
    use :: dm_db
    use :: dm_dp
    use :: dm_dummy
    use :: dm_env
    use :: dm_error
    use :: dm_fcgi
    use :: dm_fifo
    use :: dm_file
    use :: dm_format
    use :: dm_hash
    use :: dm_hash_table
    use :: dm_html
    use :: dm_http
    use :: dm_id
    use :: dm_job
    use :: dm_json
    use :: dm_jsonl
    use :: dm_la
    use :: dm_log
    use :: dm_logger
    use :: dm_lua
    use :: dm_mail
    use :: dm_mime
    use :: dm_mqtt
    use :: dm_mqueue
    use :: dm_mutex
    use :: dm_nml
    use :: dm_node
    use :: dm_observ
    use :: dm_path
    use :: dm_person
    use :: dm_pipe
    use :: dm_plot
    use :: dm_regex
    use :: dm_report
    use :: dm_router
    use :: dm_rpc
    use :: dm_sem
    use :: dm_sensor
    use :: dm_signal
    use :: dm_sql
    use :: dm_string
    use :: dm_sync
    use :: dm_system
    use :: dm_target
    use :: dm_test
    use :: dm_time
    use :: dm_timer
    use :: dm_transform
    use :: dm_tty
    use :: dm_type
    use :: dm_unit
    use :: dm_util
    use :: dm_uuid
    use :: dm_version
    use :: dm_z
    implicit none (type, external)
    public

    public :: dm_init
contains
    subroutine dm_init()
        !! Initialises DMPACK. Shall be executed once before any DMPACK
        !! routines are called to initialise the PRNG.

        call random_init(.false., .false.)
    end subroutine dm_init
end module dmpack
