! dmuuid.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmuuid
    !! Generates pseudo-random UUIDs. By default, DMPACK uses UUIDv4 in
    !! hexadecimal format, i.e., without hyphens. By using the command-line
    !! argument `--hyphens`, these are added.
    !!
    !! The command-line flag `--convert` expects UUIDs to be passed via
    !! standard input. Invalid UUIDs will be replaced with the default UUIDv4.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmuuid'
    integer,      parameter :: APP_MAJOR = 0
    integer,      parameter :: APP_MINOR = 9
    integer,      parameter :: APP_PATCH = 9

    type :: app_type
        !! Command-line arguments.
        integer :: count   = 1       !! Number of UUIDs to generate (>= 1).
        logical :: convert = .false. !! Add hyphens to 32 characters long UUIDv4.
        logical :: hyphens = .false. !! Add hyphens to generated UUIDs.
    end type app_type

    integer        :: i, rc
    type(app_type) :: app

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Read UUIDv4s from stdin and add hyphens.
    if (app%convert) then
        call convert()
        call dm_stop(STOP_SUCCESS)
    end if

    ! Generate and output UUIDs.
    do i = 1, app%count
        if (app%hyphens) then
            print '(a36)', dm_uuid4_hyphens()
        else
            print '(a32)', dm_uuid4()
        end if
    end do
contains
    subroutine convert()
        !! Reads UUIDs from standard input, adds hyphens, and prints the result
        !! to standard output.
        character(UUID_LEN) :: uuid
        integer             :: stat

        do
            read (*, '(a)', iostat=stat) uuid
            if (is_iostat_end(rc)) exit
            if (stat /= 0 .or. .not. dm_uuid4_is_valid(uuid)) uuid = UUID_DEFAULT
            print '(a)', dm_uuid4_hyphenize(uuid)
        end do
    end subroutine convert

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app !! App type

        type(arg_class) :: arg

        call arg%create()
        call arg%add('convert', short='c', type=ARG_TYPE_LOGICAL) ! -c, --convert
        call arg%add('count',   short='n', type=ARG_TYPE_INTEGER) ! -n, --count
        call arg%add('hyphens', short='p', type=ARG_TYPE_LOGICAL) ! -p, --hyphens

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('convert', app%convert)
        call arg%get('count',   app%count)
        call arg%get('hyphens', app%hyphens)
        call arg%destroy()

        rc = validate(app)
    end function read_args

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (app%convert .and. app%hyphens) then
            call dm_error_out(rc, 'argument --convert conflicts with --hyphens')
            return
        end if

        if (app%convert .and. app%count /= 1) then
            call dm_error_out(rc, 'argument --convert conflicts with --count')
            return
        end if

        if (app%count <= 0) then
            call dm_error_out(rc, 'invalid count')
            return
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
    end subroutine version_callback
end program dmuuid
