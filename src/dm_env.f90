! Author:  Philipp Engel
! Licence: ISC
module dm_env
    !! Module for reading environment variables.
    use :: dm_error
    use :: dm_kind
    use :: dm_string
    implicit none (type, external)
    private

    integer, parameter :: ENV_BUFFER_LEN = 2048 !! Input buffer length.

    interface dm_env_get
        !! Generic environment variable access.
        module procedure :: env_get_allocatable
        module procedure :: env_get_int32
        module procedure :: env_get_int64
        module procedure :: env_get_logical
        module procedure :: env_get_real32
        module procedure :: env_get_real64
        module procedure :: env_get_string
    end interface dm_env_get

    public :: dm_env_get
    public :: dm_env_has

    private :: env_get_allocatable
    private :: env_get_int32
    private :: env_get_int64
    private :: env_get_logical
    private :: env_get_real32
    private :: env_get_real64
    private :: env_get_string
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    logical function dm_env_has(name) result(has)
        !! Returns `.true.` if the environment variable of the given name
        !! exists and has a value.
        character(len=*), intent(in) :: name !! Variable name.

        character :: a
        integer   :: n, stat

        call get_environment_variable(name, a, length=n, status=stat)
        has = (stat == 0 .and. n > 0)
    end function dm_env_has

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function env_get_allocatable(name, value, default, exists) result(rc)
        !! Returns environment variable as allocatable string in `value`, with
        !! optional default value from `default` if the variable does not exist.
        character(len=*),              intent(in)            :: name    !! Variable name.
        character(len=:), allocatable, intent(out)           :: value   !! Variable value.
        character(len=*),              intent(in),  optional :: default !! Default value.
        logical,                       intent(out), optional :: exists  !! Variable exists.

        character(len=ENV_BUFFER_LEN) :: buffer
        integer                       :: n, stat

        rc = E_EMPTY
        buffer = ' '

        if (present(exists)) exists = .false.

        call get_environment_variable(name, buffer, length=n, status=stat)

        if (stat /= 0 .or. n == 0) then
            if (present(default)) then
                value = trim(default)
            else
                value = ''
            end if

            return
        end if

        if (present(exists)) exists = .true.

        rc = E_NONE
        value = trim(buffer)
    end function env_get_allocatable

    integer function env_get_int32(name, value, default, exists) result(rc)
        !! Returns environment variable as 4-byte integer in `value`, with
        !! optional default value from `default` if the variable does not exist.
        character(len=*), intent(in)            :: name    !! Variable name.
        integer(kind=i4), intent(out)           :: value   !! Variable value.
        integer(kind=i4), intent(in),  optional :: default !! Default value.
        logical,          intent(out), optional :: exists  !! Variable exists.

        character(len=20) :: buffer
        integer           :: i, n, stat

        rc = E_EMPTY
        value = 0

        if (present(default)) value  = default
        if (present(exists))  exists = .false.

        call get_environment_variable(name, buffer, length=n, status=stat)
        if (stat /= 0 .or. n == 0) return
        if (present(exists)) exists = .true.
        call dm_string_to(buffer, i, rc)
        if (dm_is_error(rc)) return

        value = i
        rc = E_NONE
    end function env_get_int32

    integer function env_get_int64(name, value, default, exists) result(rc)
        !! Returns environment variable as 8-byte integer in `value`, with
        !! optional default value from `default` if the variable does not exist.
        character(len=*), intent(in)            :: name    !! Variable name.
        integer(kind=i8), intent(out)           :: value   !! Variable value.
        integer(kind=i8), intent(in),  optional :: default !! Default value.
        logical,          intent(out), optional :: exists  !! Variable exists.

        character(len=20) :: buffer
        integer           :: n, stat
        integer(kind=i8)  :: i

        rc = E_EMPTY
        value = 0

        if (present(default)) value  = default
        if (present(exists))  exists = .false.

        call get_environment_variable(name, buffer, length=n, status=stat)
        if (stat /= 0 .or. n == 0) return
        if (present(exists)) exists = .true.
        call dm_string_to(buffer, i, rc)
        if (dm_is_error(rc)) return

        value = i
        rc = E_NONE
    end function env_get_int64

    integer function env_get_logical(name, value, default, exists) result(rc)
        !! Returns environment variable as logical in `value`, with optional
        !! default value from `default` if the variable does not exist. An
        !! integer value greater 0 is interpreted as `.true.`, else `.false.`.
        character(len=*), intent(in)            :: name    !! Variable name.
        logical,          intent(out)           :: value   !! Variable value.
        logical,          intent(in),  optional :: default !! Default value.
        logical,          intent(out), optional :: exists  !! Variable exists.

        integer :: i

        value = .false.

        if (present(default)) value  = default
        if (present(exists))  exists = .false.

        rc = dm_env_get(name, i)
        if (dm_is_error(rc)) return
        if (present(exists)) exists = .true.

        value = (i > 0)
        rc = E_NONE
    end function env_get_logical

    integer function env_get_real32(name, value, default, exists) result(rc)
        !! Returns environment variable as 4-byte real in `value`, with optional
        !! default value from `default` if the variable does not exist.
        character(len=*), intent(in)            :: name    !! Variable name.
        real(kind=r4),    intent(out)           :: value   !! Variable value.
        real(kind=r4),    intent(in),  optional :: default !! Default value.
        logical,          intent(out), optional :: exists  !! Variable exists.

        character(len=20) :: buffer
        integer           :: n, stat
        real(kind=r4)     :: f

        rc = E_EMPTY
        value = 0

        if (present(default)) value  = default
        if (present(exists))  exists = .false.

        call get_environment_variable(name, buffer, length=n, status=stat)
        if (stat /= 0 .or. n == 0) return
        if (present(exists)) exists = .true.
        call dm_string_to(buffer, f, rc)
        if (dm_is_error(rc)) return

        value = f
        rc = E_NONE
    end function env_get_real32

    integer function env_get_real64(name, value, default, exists) result(rc)
        !! Returns environment variable as 8-byte real in `value`, with optional
        !! default value from `default` if the variable does not exist.
        character(len=*), intent(in)            :: name    !! Variable name.
        real(kind=r8),    intent(out)           :: value   !! Variable value.
        real(kind=r8),    intent(in),  optional :: default !! Default value.
        logical,          intent(out), optional :: exists  !! Variable exists.

        character(len=20) :: buffer
        integer           :: n, stat
        real(kind=r8)     :: f

        rc = E_EMPTY
        value = 0

        if (present(default)) value  = default
        if (present(exists))  exists = .false.

        call get_environment_variable(name, buffer, length=n, status=stat)
        if (stat /= 0 .or. n == 0) return
        if (present(exists)) exists = .true.
        call dm_string_to(buffer, f, rc)
        if (dm_is_error(rc)) return

        value = f
        rc = E_NONE
    end function env_get_real64

    integer function env_get_string(name, value, n, exists) result(rc)
        !! Returns environment variable as string in `value` and string
        !! length in `n`.
        character(len=*), intent(in)            :: name   !! Variable name.
        character(len=*), intent(inout)         :: value  !! Variable value.
        integer,          intent(out)           :: n      !! Actual length of string.
        logical,          intent(out), optional :: exists !! Variable exists.

        integer :: stat

        rc = E_EMPTY
        value = ' '

        if (present(exists)) exists = .false.

        call get_environment_variable(name, value, length=n, status=stat)
        if (stat /= 0 .or. n == 0) return
        if (present(exists)) exists = .true.
        rc = E_NONE
    end function env_get_string
end module dm_env
