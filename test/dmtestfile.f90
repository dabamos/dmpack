! dmtestfile.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestfile
    !! Test program for file system access.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestfile'
    integer,          parameter :: NTESTS    = 5

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04), &
        test_type('test05', test05)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=*), parameter :: FILE_PATH = 'Makefile'
        integer,          parameter :: FILE_TYPE = FILE_TYPE_FILE

        character(len=TIME_LEN) :: atime, mtime, ctime
        integer                 :: rc
        integer(kind=i8)        :: nbytes, nlines
        type(file_status_type)  :: file_status

        stat = TEST_FAILED

        print *, 'Testing if file exists ...'
        if (.not. dm_file_exists(FILE_PATH)) return

        print *, 'Testing file status reading ...'
        rc = dm_file_status(FILE_PATH, file_status)
        if (dm_is_error(rc)) return

        print '(" Path.......: ", a)',  FILE_PATH
        print '(" Mode.......: ", i0)', file_status%mode
        print '(" Size.......: ", i0)', file_status%size

        write (*, '(" Type.......: ")', advance='no')

        select case (file_status%type)
            case (FILE_TYPE_BLOCK)
                print '("block device")'
            case (FILE_TYPE_CHAR)
                print '("character device")'
            case (FILE_TYPE_DIR)
                print '("directory")'
            case (FILE_TYPE_FIFO)
                print '("fifo")'
            case (FILE_TYPE_LINK)
                print '("symlink")'
            case (FILE_TYPE_FILE)
                print '("file")'
            case (FILE_TYPE_SOCKET)
                print '("socket")'
            case default
                print '("unknown")'
        end select

        if (file_status%type /= FILE_TYPE) return

        rc = dm_time_from_unix(file_status%a_time, atime)
        rc = dm_time_from_unix(file_status%m_time, mtime)
        rc = dm_time_from_unix(file_status%c_time, ctime)

        print '(" Last access: ", a)',  atime
        print '(" Last modify: ", a)',  mtime
        print '(" Last change: ", a)',  ctime

        if (file_status%type == FILE_TYPE_FILE) then
            print *, 'Testing file size reading ...'
            nbytes = dm_file_size(FILE_PATH, error=rc)
            if (dm_is_error(rc) .or. nbytes == 0) return

            print *, 'Testing if file is empty ...'
            nlines = dm_file_line_count(FILE_PATH)
            if (nlines == 0) return

            print '(" #Bytes.....: ", i0)', nbytes
            print '(" #Lines.....: ", i0)', nlines
        end if

        call dm_file_touch('/tmp/dmtestfile.tmp', modified=TIME_DEFAULT, error=rc)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        rc = dm_file_status(FILE_PATH, file_status)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        stat = TEST_FAILED

        print *, 'Testing if file is a directory ...'
        if (.not. dm_file_is_directory('/tmp')) return

        print *, 'Testing if file is not a directory ...'
        if (dm_file_is_directory('/etc/profile')) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        character(len=*), parameter :: FILE = '/bin/sh'

        stat = TEST_FAILED

        print *, 'File exists ...'
        if (.not. dm_file_exists(FILE)) return

        print *, 'File is executable ...'
        if (.not. dm_file_is_executable(FILE)) return

        print *, 'File is readable ...'
        if (.not. dm_file_is_readable(FILE)) return

        print *, 'File is writeable ...'
        if (.not. dm_file_is_readable('/tmp')) return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        character(len=*), parameter :: PATH = '/bin'

        integer          :: rc
        integer(kind=i8) :: nbyte

        stat = TEST_FAILED

        print *, 'Reading file tree size of ' // PATH // ' ...'
        rc = dm_file_tree_size(PATH, nbyte)

        print '(" File tree size: ", a)', dm_size_to_human(nbyte)
        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        if (nbyte == 0) then
            print *, 'File tree ' // PATH // ' is empty'
            return
        end if

        stat = TEST_PASSED
    end function test04

    logical function test05() result(stat)
        character(len=*), parameter :: FILE_PATH = '/tmp/dmtestfile.tmp'

        character(len=TIME_LEN) :: mtime
        integer                 :: rc
        type(file_status_type)  :: file_status

        stat = TEST_FAILED

        test_block: block
            print *, 'Touching file ' // FILE_PATH // ' ...'
            call dm_file_touch(FILE_PATH, modified=TIME_DEFAULT, error=rc)
            if (dm_is_error(rc)) exit test_block

            print *, 'Validating modification date/time ...'
            rc = dm_file_status(FILE_PATH, file_status)
            if (dm_is_error(rc)) exit test_block

            rc = dm_time_from_unix(file_status%m_time, mtime)
            if (dm_is_error(rc)) exit test_block

            if (mtime /= TIME_DEFAULT) rc = E_INVALID
            print *, mtime
        end block test_block

        call dm_file_delete(FILE_PATH)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test05
end program dmtestfile
