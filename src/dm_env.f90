! Author:  Philipp Engel
! Licence: ISC
module dm_env
    !! Environment variable access.
    use :: dm_convert
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter :: ENV_BUFFER_LEN = 2048 !! Read buffer length.

    interface dm_env_get
        !! Generic environment variable access.
        module procedure :: env_get_a
        module procedure :: env_get_a_alloc
        module procedure :: env_get_i4
        module procedure :: env_get_i8
        module procedure :: env_get_l
        module procedure :: env_get_r4
        module procedure :: env_get_r8
    end interface

    public :: dm_env_get
    public :: dm_env_has

    private :: env_get_a
    private :: env_get_a_alloc
    private :: env_get_l
    private :: env_get_i4
    private :: env_get_i8
    private :: env_get_r4
    private :: env_get_r8
contains
    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function env_get_a(name, value, n) result(rc)
        !! Returns environment variable as string in `value` and string
        !! length in `n`.
        character(len=*), intent(in)    :: name  !! Variable name.
        character(len=*), intent(inout) :: value !! Variable value.
        integer,          intent(out)   :: n     !! Length of string.
        integer                         :: stat

        rc = E_EMPTY
        value = ' '
        call get_environment_variable(name, value, length=n, status=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function env_get_a

    integer function env_get_a_alloc(name, value, default) result(rc)
        !! Returns environment variable as allocatable string in `value`, with
        !! optional default value from `default` if the variable does not exist.
        character(len=*),              intent(in)            :: name    !! Variable name.
        character(len=:), allocatable, intent(out)           :: value   !! Variable value.
        character(len=*),              intent(in),  optional :: default !! Default value.

        character(len=ENV_BUFFER_LEN) :: buffer
        integer                       :: stat

        rc = E_EMPTY
        buffer = ' '
        call get_environment_variable(name, buffer, status=stat)

        if (stat /= 0) then
            if (present(default)) then
                value = trim(default)
            else
                value = ''
            end if

            return
        end if

        value = trim(buffer)
        rc = E_NONE
    end function env_get_a_alloc

    integer function env_get_i4(name, value, default) result(rc)
        !! Returns environment variable as 4-byte integer in `value`, with
        !! optional default value from `default` if the variable does not exist.
        character(len=*), intent(in)           :: name    !! Variable name.
        integer(kind=i4), intent(out)          :: value   !! Variable value.
        integer(kind=i4), intent(in), optional :: default !! Default value.

        character(len=20) :: buffer
        integer           :: i, stat

        rc = E_EMPTY
        value = 0
        if (present(default)) value = default

        call get_environment_variable(name, buffer, status=stat)
        if (stat /= 0) return
        call dm_convert_to(buffer, i, rc)
        if (rc /= E_NONE) return

        value = i
        rc = E_NONE
    end function env_get_i4

    integer function env_get_i8(name, value, default) result(rc)
        !! Returns environment variable as 8-byte integer in `value`, with
        !! optional default value from `default` if the variable does not exist.
        character(len=*), intent(in)           :: name    !! Variable name.
        integer(kind=i8), intent(out)          :: value   !! Variable value.
        integer(kind=i8), intent(in), optional :: default !! Default value.

        character(len=20) :: buffer
        integer           :: stat
        integer(kind=i8)  :: i

        rc = E_EMPTY
        value = 0
        if (present(default)) value = default

        call get_environment_variable(name, buffer, status=stat)
        if (stat /= 0) return
        call dm_convert_to(buffer, i, rc)
        if (rc /= E_NONE) return

        value = i
        rc = E_NONE
    end function env_get_i8

    integer function env_get_l(name, value, default) result(rc)
        !! Returns environment variable as logical in `value`, with optional
        !! default value from `default` if the variable does not exist. An
        !! integer value greater 0 is interpreted as `.true.`, else `.false.`.
        character(len=*), intent(in)           :: name    !! Variable name.
        logical,          intent(out)          :: value   !! Variable value.
        logical,          intent(in), optional :: default !! Default value.
        integer                                :: i

        value = default
        rc = dm_env_get(name, i)
        if (rc /= E_NONE) return
        value = (i > 0)
        rc = E_NONE
    end function env_get_l

    integer function env_get_r4(name, value, default) result(rc)
        !! Returns environment variable as 4-byte real in `value`, with optional
        !! default value from `default` if the variable does not exist.
        character(len=*), intent(in)           :: name    !! Variable name.
        real(kind=r4),    intent(out)          :: value   !! Variable value.
        real(kind=r4),    intent(in), optional :: default !! Default value.

        character(len=20) :: buffer
        integer           :: stat
        real(kind=r4)     :: f

        rc = E_EMPTY
        value = 0
        if (present(default)) value = default

        call get_environment_variable(name, buffer, status=stat)
        if (stat /= 0) return
        call dm_convert_to(buffer, f, rc)
        if (rc /= E_NONE) return

        value = f
        rc = E_NONE
    end function env_get_r4

    integer function env_get_r8(name, value, default) result(rc)
        !! Returns environment variable as 8-byte real in `value`, with optional
        !! default value from `default` if the variable does not exist.
        character(len=*), intent(in)           :: name    !! Variable name.
        real(kind=r8),    intent(out)          :: value   !! Variable value.
        real(kind=r8),    intent(in), optional :: default !! Default value.

        character(len=20) :: buffer
        integer           :: stat
        real(kind=r8)     :: f

        rc = E_EMPTY
        value = 0
        if (present(default)) value = default

        call get_environment_variable(name, buffer, status=stat)
        if (stat /= 0) return
        call dm_convert_to(buffer, f, rc)
        if (rc /= E_NONE) return

        value = f
        rc = E_NONE
    end function env_get_r8

    logical function dm_env_has(name) result(has)
        !! Returns `.true.` if the environment variable of the given name
        !! exists.
        character(len=*), intent(in) :: name  !! Variable name.

        character :: a
        integer   :: stat

        has = .false.
        call get_environment_variable(name, a, status=stat)
        if (stat /= 0) return
        has = .true.
    end function dm_env_has
end module dm_env
