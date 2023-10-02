! Author:  Philipp Engel
! Licence: ISC
module dm_app
    !! Utility procedures for applications based on the DMPACK library.
    use :: dm_id
    use :: dm_version
    implicit none (type, external)
    private

    integer, parameter, public :: APP_NAME_LEN = ID_LEN !! Max. app name length.

    public :: dm_app_out
contains
    subroutine dm_app_out(name, major, minor, patch)
        !! Prints DMPACK and application version to standard output.
        character(len=*), intent(in) :: name  !! Application name.
        integer,          intent(in) :: major !! Major version number.
        integer,          intent(in) :: minor !! Minor version number.
        integer,          intent(in) :: patch !! Patch level.

        print '(a, 1x, i1, 2(".", i1), " (DMPACK ", a, ")")', &
            name, major, minor, patch, DM_VERSION_STRING
    end subroutine dm_app_out
end module dm_app

