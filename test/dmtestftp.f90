! dmtestftp.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestftp
    !! Test program for FTP file upload and download.
    !!
    !! To run this program, set the following environment variables
    !! first:
    !!
    !!      DM_FTP_HOST     - FTP host.
    !!      DM_FTP_PORT     - FTP port (optional).
    !!      DM_FTP_PATH     - FTP test directory.
    !!      DM_FTP_USERNAME - FTP user name.
    !!      DM_FTP_PASSWORD - FTP password.
    !!
    !! Some tests may be skipped if these are not set.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestftp'
    integer,          parameter :: NTESTS    = 1

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    no_color = dm_env_has('NO_COLOR')

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function get_env(host, port, username, password, path) result(has)
        character(len=:), allocatable, intent(out) :: host
        integer,                       intent(out) :: port
        character(len=:), allocatable, intent(out) :: username
        character(len=:), allocatable, intent(out) :: password
        character(len=:), allocatable, intent(out) :: path

        integer :: rc

        has = .false.

        if (dm_is_error(dm_env_get('DM_FTP_HOST',     host))     .or. &
            dm_is_error(dm_env_get('DM_FTP_USERNAME', username)) .or. &
            dm_is_error(dm_env_get('DM_FTP_PASSWORD', password)) .or. &
            dm_is_error(dm_env_get('DM_FTP_PATH',     path))) then

            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("> Set environment variables DM_FTP_HOST, DM_FTP_USERNAME,")'
            print '("> DM_FTP_PASSWORD, and DM_FTP_PATH. This test will be skipped.")'
            call dm_ansi_reset(no_color)
            return
        end if

        rc  = dm_env_get('DM_FTP_PORT', port, default=0)
        has = .true.
    end function get_env

    logical function test01() result(stat)
        !! File upload via FTP, using parameters passed through environment variables.
        character(len=*), parameter :: LOCAL_PATH  = 'testobserv.csv' !! Path of local file.
        character(len=*), parameter :: REMOTE_FILE = 'testobserv.tmp' !! Remote file name.
        character(len=*), parameter :: RENAME_TO   = 'testobserv.csv' !! Name after upload.
        logical,          parameter :: ACTIVE      = .true.           !! Enable active mode.
        logical,          parameter :: DEBUG       = .false.          !! Enable cURL debug output.

        character(len=:),  allocatable :: host, username, password, path, remote_path
        character(len=:),  allocatable :: error_message
        type(observ_type), allocatable :: observs(:)
        integer                        :: error_curl, iostat, port, rc, unit

        integer(kind=i8)      :: nbytes, nbytes2
        type(ftp_server_type) :: server
        type(timer_type)      :: timer

        stat = TEST_PASSED
        if (.not. get_env(host, port, username, password, path)) return

        stat = TEST_FAILED
        allocate (observs(1000))
        call dm_test_dummy(observs)

        print '(" Creating local file ", a, " ...")', LOCAL_PATH

        open (action='write', file=LOCAL_PATH, iostat=iostat, newunit=unit, status='replace')
        if (iostat /= 0) return
        rc = dm_csv_write(observs, unit=unit, header=.true.)
        close (unit)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        deallocate (observs)
        nbytes = dm_file_size(LOCAL_PATH)

        ftp_block: block
            character(len=:), allocatable :: url

            print '(" Starting FTP backend ...")'
            rc = dm_ftp_init()
            if (dm_is_error(rc)) exit ftp_block

            call dm_ftp_server_set(server, host=host, port=port, username=username, password=password, active=ACTIVE)
            remote_path = dm_path_join('/', dm_path_join(path, REMOTE_FILE)) ! absolute path
            url         = dm_ftp_url(host, port=port, path=remote_path)

            print '(" Host.......: ", a)',          host
            print '(" Port.......: ", i0)',         port
            print '(" User name..: ", a)',          username
            print '(" Password...: ", a)',          password
            print '(" Local path.: ", a)',          LOCAL_PATH
            print '(" Remote file: ", a)',          REMOTE_FILE
            print '(" Path.......: ", a)',          path
            print '(" Remote path: ", a)',          remote_path
            print '(" Rename to..: ", a)',          RENAME_TO
            print '(" URL........: ", a)',          url
            print '(" File size..: ", i0, " KiB")', dm_file_size(LOCAL_PATH) / 1024

            ! ******************************************************************
            ! UPLOAD
            ! ******************************************************************
            print '(" Uploading ", a, " to ", a, " ...")', LOCAL_PATH, url
            call dm_timer_start(timer)
            rc = dm_ftp_upload(server, LOCAL_PATH, remote_path, RENAME_TO, error_message=error_message, error_curl=error_curl, debug=DEBUG)
            call dm_timer_stop(timer)

            if (dm_is_error(rc)) then
                print '("cURL error ", i0, ": ", a)', error_curl, error_message
                exit ftp_block
            end if

            print '(" Finished upload in ", f0.2, " sec")', dm_timer_result(timer)
            print '(" Renamed remote file ", a, " to ", a)', REMOTE_FILE, RENAME_TO

            print '(" Deleting local file ", a, " ...")', LOCAL_PATH
            call dm_file_delete(LOCAL_PATH)

            ! ******************************************************************
            ! DOWNLOAD
            ! ******************************************************************
            remote_path = dm_path_join('/', dm_path_join(path, RENAME_TO)) ! absolute path
            url         = dm_ftp_url(host, port=port, path=remote_path)

            print '(" Remote path: ", a)', remote_path
            print '(" URL........: ", a)', url

            print '(" Downloading ", a, " to ", a, " ...")', url, LOCAL_PATH
            call dm_timer_start(timer)
            rc = dm_ftp_download(server, remote_path, LOCAL_PATH, error_message=error_message, error_curl=error_curl, debug=DEBUG)
            call dm_timer_stop(timer)

            if (dm_is_error(rc)) then
                print '("cURL error ", i0, ": ", a)', error_curl, error_message
                exit ftp_block
            end if

            print '(" Finished download in ", f0.2, " sec")', dm_timer_result(timer)
            nbytes2 = dm_file_size(LOCAL_PATH)

            if (nbytes2 /= nbytes) then
                print '(" File sizes do not match: expected ", i0, " bytes, got ", i0, " bytes")', nbytes, nbytes2
                exit ftp_block
            end if

            stat = TEST_PASSED
        end block ftp_block

        call dm_error_out(rc)
        call dm_ftp_shutdown()

        if (dm_file_exists(LOCAL_PATH)) then
            print '(" Deleting local file ", a, " ...")', LOCAL_PATH
            call dm_file_delete(LOCAL_PATH)
        end if

        call dm_ansi_color(COLOR_YELLOW, no_color)
        print '(" >>> DELETE REMOTE FILE ", a, " ON THE SERVER MANUALLY! <<<")', RENAME_TO
        call dm_ansi_reset(no_color)
    end function test01
end program dmtestftp
