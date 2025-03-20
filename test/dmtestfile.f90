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
    integer,          parameter :: NTESTS    = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'), compiler_version(), compiler_options())
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

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        stat = TEST_FAILED

        print *, 'Testing if file is a directory ...'
        if (.not. dm_file_is_directory('test')) return

        print *, 'Testing if file is not a directory ...'
        if (dm_file_is_directory('Makefile')) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        stat = TEST_FAILED

        print *, 'Testing file permissions ...'
        if (.not. dm_file_is_executable(TEST_NAME)) return
        if (.not. dm_file_is_readable(TEST_NAME))   return
        if (.not. dm_file_is_writeable(TEST_NAME))  return

        stat = TEST_PASSED
    end function test03
end program dmtestfile
