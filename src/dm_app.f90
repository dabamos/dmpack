! Author:  Philipp Engel
! Licence: ISC
module dm_app
    !! Utility procedures for applications based on the DMPACK library.
    use :: dm_id
    use :: dm_version
    implicit none (type, external)
    private

    integer, parameter, public :: APP_NAME_LEN = ID_LEN !! Max. app name length.

    public :: dm_app_version
contains
    subroutine dm_app_version(name, major, minor)
        !! Prints DMPACK and application version to standard output.
        character(len=*), intent(in) :: name  !! Application name.
        integer,          intent(in) :: major !! Major version number.
        integer,          intent(in) :: minor !! Minor version number.

        print '(a, 1x, i1, ".", i1, " (DMPACK ", a, ")")', name, major, minor, DM_VERSION_STRING
    end subroutine dm_app_version
end module dm_app
