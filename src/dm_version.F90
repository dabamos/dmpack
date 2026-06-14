! Author:  Philipp Engel
! Licence: ISC
module dm_version
    !! DMPACK version and auxiliary procedures.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PREPROCESSOR MACROS
    ! **************************************************************************
#if defined (__DATE__)
#define _BUILD_DATE_ __DATE__
#else
#define _BUILD_DATE_ "??? ?? ????"
#endif

    ! **************************************************************************
    ! PUBLIC PARAMETERS
    ! **************************************************************************
    character(*), parameter, public :: DM_COPYRIGHT        = 'Copyright (c) 2026, Philipp Engel' !! DMPACK copyright string.
    character(*), parameter, public :: DM_LIBRARY_COMPILER = compiler_version()                  !! Library compiler.
    character(*), parameter, public :: DM_LIBRARY_DATE     = _BUILD_DATE_                        !! Library build date (`??? ?? ????` if unavailable).
    character(*), parameter, public :: DM_LIBRARY_OPTIONS  = compiler_options()                  !! Library build options.

    integer, parameter, public :: DM_VERSION_MAJOR = 2 !! DMPACK major version, from 0 to 9.
    integer, parameter, public :: DM_VERSION_MINOR = 0 !! DMPACK minor version, from 0 to 9.
    integer, parameter, public :: DM_VERSION_PATCH = 0 !! DMPACK patch level, from 0 to 9.

    character(*), parameter, public :: DM_VERSION_STRING = achar(DM_VERSION_MAJOR + 48) // '.' // &
                                                           achar(DM_VERSION_MINOR + 48) // '.' // &
                                                           achar(DM_VERSION_PATCH + 48) !! DMPACK version as string.

    ! **************************************************************************
    ! PUBLIC INTERFACES
    ! **************************************************************************
    public :: dm_version_out
    public :: dm_version_to_string

    interface dm_version_out
        !! Generic DMPACK version output routine.
        module procedure :: version_out_app
        module procedure :: version_out_lib
    end interface dm_version_out

    interface dm_version_to_string
        !! Generic DMPACK version formatters.
        module procedure :: version_to_string_app
        module procedure :: version_to_string_long
        module procedure :: version_to_string_short
    end interface dm_version_to_string

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    private :: version_out_app
    private :: version_out_lib
    private :: version_to_string_app
    private :: version_to_string_long
    private :: version_to_string_short
contains
    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    pure function version_to_string_app(name, major, minor, patch, library) result(string)
        !! Returns allocatable string of application version, with optional
        !! DMPACK library version appended if `library` is `.true.`.
        character(*), intent(in)           :: name    !! App name.
        integer,      intent(in)           :: major   !! App major version.
        integer,      intent(in)           :: minor   !! App minor version.
        integer,      intent(in)           :: patch   !! App patch version.
        logical,      intent(in), optional :: library !! Append DMPACK library version.
        character(:), allocatable          :: string  !! App and library version string.

        logical :: library_

        library_ = .false.
        if (present(library)) library_ = library

        string = trim(name) // ' ' // dm_version_to_string(major, minor, patch)
        if (library_) string = string // ' (DMPACK ' // DM_VERSION_STRING // ')'
    end function version_to_string_app

    pure elemental function version_to_string_long(major, minor, patch) result(string)
        !! Utility function that returns a five characters long version string
        !! with patch level, for example, `1.0.0`.
        integer, intent(in) :: major  !! Major version number.
        integer, intent(in) :: minor  !! Minor version number.
        integer, intent(in) :: patch  !! Patch level.
        character(5)        :: string !! Output string.

        write (string, '(i1, 2(".", i1))') major, minor, patch
    end function version_to_string_long

    pure elemental function version_to_string_short(major, minor) result(string)
        !! Utility function that returns a three characters long version
        !! string, for example, `1.0`.
        integer, intent(in) :: major  !! Major version number.
        integer, intent(in) :: minor  !! Minor version number.
        character(3)        :: string !! Output string.

        write (string, '(i1, ".", i1)') major, minor
    end function version_to_string_short

    subroutine version_out_app(name, major, minor, patch)
        !! Prints application and DMPACK library version to standard output.
        character(*), intent(in) :: name  !! Application name.
        integer,      intent(in) :: major !! Major version number of application.
        integer,      intent(in) :: minor !! Minor version number of application.
        integer,      intent(in) :: patch !! Patch level of application.

        print '(a, 1x, i1, 2(".", i1), " (DMPACK ", a, ")")', trim(name), major, minor, patch, DM_VERSION_STRING
    end subroutine version_out_app

    subroutine version_out_lib()
        !! Prints DMPACK library version to standard output.
        print '("DMPACK ", a)', DM_VERSION_STRING
    end subroutine version_out_lib
end module dm_version
