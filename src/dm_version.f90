! Author:  Philipp Engel
! Licence: ISC
module dm_version
    !! Current DMPACK version.
    implicit none (type, external)
    private

    integer, parameter, public :: DM_VERSION_MAJOR = 0 !! DMPACK major version, from 0 to 9.
    integer, parameter, public :: DM_VERSION_MINOR = 9 !! DMPACK minor version, from 0 to 9.

    character(len=*), parameter, public :: DM_VERSION_STRING = &
        achar(DM_VERSION_MAJOR + 48) // '.' // achar(DM_VERSION_MINOR + 48) !! DMPACK version as string.

    public :: dm_version_to_string
contains
    pure elemental character(len=3) function dm_version_to_string(major, minor) result(str)
        !! Utility function that returns a three characters long version string.
        integer, intent(in) :: major !! Major version number.
        integer, intent(in) :: minor !! Minor version number.

        write (str, '(i1, ".", i1)') major, minor
    end function dm_version_to_string
end module dm_version
