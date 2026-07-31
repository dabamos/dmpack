! dm_buffer.f90
!
! Author:  Philipp Engel
! Licence: ISC
module dm_buffer
    !! Byte buffer.
    use :: dm_error
    use :: dm_kind
    implicit none
    private

    ! **************************************************************************
    ! PUBLIC DERIVED TYPES
    ! **************************************************************************
    type, public :: buffer_type
        !! Byte buffer.
        character(:), pointer              :: bytes  => null() !! Pointer to byte buffer.
        integer(i8)                        :: nbytes = 0_i8    !! Number of bytes used.
        character(:), allocatable, private :: buffer           !! Allocatable byte buffer.
        integer(i8),               private :: size   = 0_i8    !! Max. buffer space.
    end type buffer_type

    ! **************************************************************************
    ! PUBLIC INTERFACES
    ! **************************************************************************
    public :: dm_buffer_init

    interface dm_buffer_init
        !! Generic init routine for MessagePack buffer (either allocatable
        !! string or pointer).
        module procedure :: buffer_init_allocatable
        module procedure :: buffer_init_c_pointer
        module procedure :: buffer_init_pointer
    end interface dm_buffer_init

    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    public :: dm_buffer_append
    public :: dm_buffer_bytes
    public :: dm_buffer_copy
    public :: dm_buffer_destroy
    public :: dm_buffer_is_initialized
    public :: dm_buffer_reset
    public :: dm_buffer_size

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    private :: buffer_init_allocatable
    private :: buffer_init_c_pointer
    private :: buffer_init_pointer
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES
    ! **************************************************************************
    pure subroutine dm_buffer_append(buffer, bytes, error)
        use :: dm_util, only: dm_present_set

        type(buffer_type), intent(inout)         :: buffer !! Buffer.
        character(*),      intent(in)            :: bytes  !! Bytes to append.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: rc

        buffer_block: block
            integer(i8) :: i, j, k, n

            rc = E_NULL
            if (.not. associated(buffer%bytes)) exit buffer_block

            i = buffer%nbytes
            j = i + 1
            n = len(bytes, i8)
            k = i + n

            rc = E_LIMIT
            if (k > buffer%size) exit buffer_block

            rc = E_NONE
            if (n == 0_i8) exit buffer_block

            buffer%bytes(j:) = bytes
            buffer%nbytes    = k
        end block buffer_block

        call dm_present_set(error, rc)
    end subroutine dm_buffer_append

    function dm_buffer_bytes(buffer) result(bytes)
        type(buffer_type), target, intent(inout) :: buffer !! Buffer.
        character(:), pointer                    :: bytes  !! Result.

        nullify (bytes)

        if (.not. associated(buffer%bytes))           return
        if (buffer%nbytes == 0 .or. buffer%size == 0) return

        bytes => buffer%bytes(1:buffer%nbytes)
    end function dm_buffer_bytes

    function dm_buffer_copy(buffer) result(bytes)
        type(buffer_type), target, intent(inout) :: buffer !! Buffer.
        character(:), allocatable                :: bytes  !! Result.

        copy_block: block
            if (.not. associated(buffer%bytes))           exit copy_block
            if (buffer%nbytes == 0 .or. buffer%size == 0) exit copy_block
            bytes = buffer%bytes(1:buffer%nbytes)
            return
        end block copy_block

        if (.not. allocated(bytes)) bytes = ''
    end function dm_buffer_copy

    pure subroutine dm_buffer_destroy(buffer)
        type(buffer_type), intent(inout) :: buffer !! Buffer.

        if (allocated(buffer%buffer)) deallocate (buffer%buffer)
        if (associated(buffer%bytes)) nullify (buffer%bytes)

        buffer%nbytes = 0_i8
        buffer%size   = 0_i8
    end subroutine dm_buffer_destroy

    pure logical function dm_buffer_is_initialized(buffer) result(is)
        type(buffer_type), intent(in) :: buffer !! Buffer.

        is = (associated(buffer%bytes) .and. buffer%size > 0)
    end function dm_buffer_is_initialized

    pure subroutine dm_buffer_reset(buffer)
        type(buffer_type), intent(inout) :: buffer !! Buffer.

        buffer%nbytes = 0_i8
    end subroutine dm_buffer_reset

    pure integer(i8) function dm_buffer_size(buffer) result(size)
        type(buffer_type), intent(in) :: buffer !! Buffer.

        if (associated(buffer%bytes)) then
            size = buffer%size
        else
            size = 0_i8
        end if
    end function dm_buffer_size

    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    pure subroutine buffer_init_allocatable(buffer, nbytes, error)
        !! Initialises buffer using an allocatable character string with given
        !! length `size`.
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if size is < 1.
        !!
        use :: dm_util, only: dm_present_set

        type(buffer_type), target, intent(inout)         :: buffer !! Buffer.
        integer(i8),               intent(in)            :: nbytes !! Max. buffer size [byte].
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc, stat

        init_block: block
            rc = E_INVALID
            if (nbytes < 0_i8) exit init_block

            rc = E_ALLOC
            allocate (character(nbytes) :: buffer%buffer, stat=stat)
            if (stat /= 0) exit init_block

            rc = E_NONE
            buffer%bytes => buffer%buffer
            buffer%size  =  nbytes
        end block init_block

        call dm_present_set(error, rc)
    end subroutine buffer_init_allocatable

    subroutine buffer_init_c_pointer(buffer, ptr, nbytes, error)
        use :: dm_c,    only: c_associated, c_f_pointer, c_ptr
        use :: dm_util, only: dm_present_set

        type(buffer_type), target, intent(out)           :: buffer !! Buffer.
        type(c_ptr),               intent(in)            :: ptr    !! C pointer to bytes.
        integer(i8),               intent(in)            :: nbytes !! Size of C pointer data [byte].
        integer,                   intent(out), optional :: error  !! Error code.

        integer :: rc

        init_block: block
            character(nbytes), pointer :: bytes

            rc = E_NULL
            if (.not. c_associated(ptr)) exit init_block
            call c_f_pointer(ptr, bytes)

            rc = E_NONE
            buffer%buffer =  bytes
            buffer%bytes  => buffer%buffer
            buffer%size   =  nbytes
        end block init_block

        call dm_present_set(error, rc)
    end subroutine buffer_init_c_pointer

    pure subroutine buffer_init_pointer(buffer, bytes, nbytes, error)
        !! Initialises buffer using given string `bytes`. The variable shall not
        !! go out of scope for the life-time of buffer type `buffer`!
        !!
        !! The subroutine sets argument `error` to:
        !!
        !! * `E_NULL` if pointer association failed.
        !!
        use :: dm_util, only: dm_present, dm_present_set

        type(buffer_type),    intent(out)           :: buffer !! Buffer.
        character(*), target, intent(inout)         :: bytes  !! Bytes.
        integer(i8),          intent(in),  optional :: nbytes !! Max. buffer size [byte].
        integer,              intent(out), optional :: error  !! Error code.

        integer :: rc

        buffer%size = min(len(bytes, i8), max(0_i8, dm_present(nbytes, 0_i8)))

        if (buffer%size > 0) then
            buffer%bytes => bytes(1_i8:buffer%size)
        else
            buffer%bytes => bytes
        end if

        rc = E_NULL
        if (associated(buffer%bytes)) rc = E_NONE
        call dm_present_set(error, rc)
    end subroutine buffer_init_pointer
end module dm_buffer
