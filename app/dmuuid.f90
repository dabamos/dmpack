! dmuuid.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmuuid
    !! Generates pseudo-random UUID4s. By default, DMPACK uses UUID4s in
    !! hexadecimal format, i.e., without hyphens. By using the command-line
    !! argument `--hyphens`, these are added.
    !!
    !! The command-line flag `--convert` expects UUID4s to be passed via
    !! standard input. Invalid UUID4s will be replaced with the default UUID4.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmuuid'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 0

    type :: app_type
        !! Command-line arguments.
        integer :: count   = 1       !! Number of UUID4s to generate (>= 1).
        logical :: convert = .false. !! Convert 32 characters long UUID4 to 36 characters (add hyphens).
        logical :: hyphens = .false. !! Add hyphens to generated UUID4s.
    end type app_type

    integer        :: i, rc
    type(app_type) :: app

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Read UUID4s from stdin and add hyphens.
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
    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app
        type(arg_type)              :: args(3)

        rc = E_NONE

        args = [ &
            arg_type('convert', short='C', type=ARG_TYPE_LOGICAL), & ! -C, --convert
            arg_type('count',   short='n', type=ARG_TYPE_INTEGER), & ! -n, --count
            arg_type('hyphens', short='H', type=ARG_TYPE_LOGICAL)  & ! -H, --hyphens
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%convert)
        rc = dm_arg_get(args(2), app%count)
        rc = dm_arg_get(args(3), app%hyphens)

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
    end function read_args

    subroutine convert()
        !! Reads UUID4s from standard input, adds hyphens, and prints the result
        !! to standard output.
        character(len=UUID_LEN) :: uuid
        integer                 :: rc

        do
            uuid = ' '
            read (*, '(a)', iostat=rc) uuid

            if (is_iostat_end(rc)) exit
            if (rc /= 0 .or. .not. dm_uuid4_valid(uuid)) uuid = UUID_DEFAULT

            print '(a)', dm_uuid4_hyphenize(uuid)
        end do
    end subroutine convert
end program dmuuid
