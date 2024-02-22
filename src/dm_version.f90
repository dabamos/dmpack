! Author:  Philipp Engel
! Licence: ISC
module dm_version
    !! DMPACK version and auxiliary procedures.
    implicit none (type, external)
    private

    character(len=*), parameter, public :: DM_COPYRIGHT = 'Copyright (c) 2024, Philipp Engel' !! DMPACK copyright string.

    integer, parameter, public :: DM_VERSION_MAJOR = 0 !! DMPACK major version, from 0 to 9.
    integer, parameter, public :: DM_VERSION_MINOR = 9 !! DMPACK minor version, from 0 to 9.
    integer, parameter, public :: DM_VERSION_PATCH = 3 !! DMPACK patch level, from 0 to 9.

    character(len=*), parameter, public :: DM_VERSION_STRING = achar(DM_VERSION_MAJOR + 48) // '.' // &
                                                               achar(DM_VERSION_MINOR + 48) // '.' // &
                                                               achar(DM_VERSION_PATCH + 48) !! DMPACK version as string.

    interface dm_version_to_string
        !! Generic DMPACK version formatters.
        module procedure :: dm_version_to_string_app
        module procedure :: dm_version_to_string_long
        module procedure :: dm_version_to_string_short
    end interface

    public :: dm_version_out
    public :: dm_version_to_string

    private :: dm_version_to_string_app
    private :: dm_version_to_string_long
    private :: dm_version_to_string_short
contains
    pure function dm_version_to_string_app(app_name, app_major, app_minor, app_patch, library) result(str)
        !! Returns allocatable string of application version, with optional
        !! DMPACK library version appended if `library` is `.true.`.
        character(len=*), intent(in)           :: app_name  !! App name.
        integer,          intent(in)           :: app_major !! App major version.
        integer,          intent(in)           :: app_minor !! App minor version.
        integer,          intent(in)           :: app_patch !! App patch version.
        logical,          intent(in), optional :: library   !! Append DMPACK library version.
        character(len=:), allocatable          :: str       !! App and library version string.

        logical :: library_

        library_ = .false.
        if (present(library)) library_ = library

        if (library_) then
            str = trim(app_name) // ' ' // dm_version_to_string(app_major, app_minor, app_patch) // &
                  ' (DMPACK ' // DM_VERSION_STRING // ')'
        else
            str = trim(app_name) // ' ' // dm_version_to_string(app_major, app_minor, app_patch)
        end if
    end function dm_version_to_string_app

    pure elemental function dm_version_to_string_long(major, minor, patch) result(str)
        !! Utility function that returns a five characters long version string
        !! with patch level, for example, `1.0.0`.
        integer, intent(in) :: major !! Major version number.
        integer, intent(in) :: minor !! Minor version number.
        integer, intent(in) :: patch !! Patch level.
        character(len=5)    :: str   !! Output string.

        write (str, '(i1, 2(".", i1))') major, minor, patch
    end function dm_version_to_string_long

    pure elemental function dm_version_to_string_short(major, minor) result(str)
        !! Utility function that returns a three characters long version
        !! string, for example, `1.0`.
        integer, intent(in) :: major !! Major version number.
        integer, intent(in) :: minor !! Minor version number.
        character(len=3)    :: str   !! Output string.

        write (str, '(i1, ".", i1)') major, minor
    end function dm_version_to_string_short

    subroutine dm_version_out(name, major, minor, patch)
        !! Prints DMPACK and application version to standard output.
        character(len=*), intent(in) :: name  !! Application name.
        integer,          intent(in) :: major !! Major version number of application.
        integer,          intent(in) :: minor !! Minor version number of application.
        integer,          intent(in) :: patch !! Patch level of application.

        print '(a, 1x, i1, 2(".", i1), " (DMPACK ", a, ")")', &
            trim(name), major, minor, patch, DM_VERSION_STRING
    end subroutine dm_version_out
end module dm_version
