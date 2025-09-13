! Author:  Philipp Engel
! Licence: ISC
module dm_node
    !! Sensor node declaration.
    use :: dm_id
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: NODE_ID_LEN   = ID_LEN !! Max. node id length.
    integer, parameter, public :: NODE_NAME_LEN = 32     !! Max. node name length.
    integer, parameter, public :: NODE_META_LEN = 32     !! Max. node meta description length.

    type, public :: node_type
        !! Sensor node type. Uses lon-lat order.
        character(len=NODE_ID_LEN)   :: id        = ' '    !! Node id (`-0-9A-Z_a-z`).
        character(len=NODE_NAME_LEN) :: name      = ' '    !! Node name.
        character(len=NODE_META_LEN) :: meta      = ' '    !! Additional description text (optional).
        real(kind=r8)                :: x         = 0.0_r8 !! Local x or easting, usually in metres (optional).
        real(kind=r8)                :: y         = 0.0_r8 !! Local y or northing, usually in metres (optional).
        real(kind=r8)                :: z         = 0.0_r8 !! Local z or elevation, usually in metres (optional).
        real(kind=r8)                :: longitude = 0.0_r8 !! Longitude in degrees (optional).
        real(kind=r8)                :: latitude  = 0.0_r8 !! Latitude in degrees (optional).
        real(kind=r8)                :: elevation = 0.0_r8 !! Elevation in metres (optional).
    end type node_type

    integer, parameter, public :: NODE_TYPE_SIZE = storage_size(node_type()) / 8 !! Size of `node_type` in bytes.

    interface operator (==)
        !! Returns `.true.` if nodes are equal.
        module procedure :: dm_node_equals
    end interface

    public :: operator (==)

    public :: dm_node_equals
    public :: dm_node_is_valid
    public :: dm_node_out
contains
    pure elemental logical function dm_node_equals(node1, node2) result(equals)
        !! Returns `.true.` if given nodes are equal.
        use :: dm_util, only: dm_equals

        type(node_type), intent(in) :: node1 !! The first node.
        type(node_type), intent(in) :: node2 !! The second node.

        equals = (node1%id   == node2%id                      .and. &
                  node1%name == node2%name                    .and. &
                  node1%meta == node2%meta                    .and. &
                  dm_equals(node1%x,         node2%x)         .and. &
                  dm_equals(node1%y,         node2%y)         .and. &
                  dm_equals(node1%z,         node2%z)         .and. &
                  dm_equals(node1%longitude, node2%longitude) .and. &
                  dm_equals(node1%latitude,  node2%latitude)  .and. &
                  dm_equals(node1%elevation, node2%elevation))
    end function dm_node_equals

    pure elemental logical function dm_node_is_valid(node) result(valid)
        !! Returns `.true.` if given node type elements are valid.
        use :: dm_string, only: dm_string_is_printable

        type(node_type), intent(in) :: node

        valid = (dm_id_is_valid(node%id)           .and. &
                 len_trim(node%name) > 0           .and. &
                 dm_string_is_printable(node%name) .and. &
                 dm_string_is_printable(node%meta))
    end function dm_node_is_valid

    subroutine dm_node_out(node, unit)
        !! Prints node to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(node_type), intent(inout)        :: node
        integer,         intent(in), optional :: unit

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("node.id: ", a)')              trim(node%id)
        write (unit_, '("node.name: ", a)')            trim(node%name)
        write (unit_, '("node.meta: ", a)')            trim(node%meta)
        write (unit_, '("node.x: ", 1pg0.12)')         node%x
        write (unit_, '("node.y: ", 1pg0.12)')         node%y
        write (unit_, '("node.z: ", 1pg0.12)')         node%z
        write (unit_, '("node.longitude: ", 1pg0.12)') node%longitude
        write (unit_, '("node.latitude: ", 1pg0.12)')  node%latitude
        write (unit_, '("node.elevation: ", 1pg0.12)') node%elevation
    end subroutine dm_node_out
end module dm_node
