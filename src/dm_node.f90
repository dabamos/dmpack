! Author:  Philipp Engel
! Licence: ISC
module dm_node
    !! Sensor node declaration.
    use :: dm_id
    use :: dm_type
    implicit none (type, external)
    private

    integer, parameter, public :: NODE_ID_LEN   = ID_LEN
    integer, parameter, public :: NODE_NAME_LEN = 32
    integer, parameter, public :: NODE_META_LEN = 32

    type, public :: node_type
        !! Node type.
        character(len=NODE_ID_LEN)   :: id   = ' ' !! Node id (-0-9A-Za-z).
        character(len=NODE_NAME_LEN) :: name = ' ' !! Note name.
        character(len=NODE_META_LEN) :: meta = ' ' !! Additional description text.
    end type node_type

    interface operator (==)
        !! Returns whether nodes are equal.
        module procedure :: dm_node_equals
    end interface

    public :: operator (==)

    public :: dm_node_equals
    public :: dm_node_out
    public :: dm_node_valid
contains
    pure elemental logical function dm_node_equals(node1, node2) result(equals)
        !! Returns `.true.` if given nodes are equal.
        type(node_type), intent(in) :: node1 !! The first node.
        type(node_type), intent(in) :: node2 !! The second node.

        equals = .false.
        if (node1%id   /= node2%id)   return
        if (node1%name /= node2%name) return
        if (node1%meta /= node2%meta) return
        equals= .true.
    end function dm_node_equals

    pure elemental logical function dm_node_valid(node) result(valid)
        !! Returns `.true.` if given node type elements are valid.
        type(node_type), intent(in) :: node

        valid = .false.
        if (.not. dm_id_valid(node%id)) return
        if (len_trim(node%name) == 0) return
        valid = .true.
    end function dm_node_valid

    subroutine dm_node_out(node, unit)
        !! Prints node to standard output or given file unit.
        type(node_type), intent(inout)        :: node
        integer,         intent(in), optional :: unit

        integer :: unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        write (unit_, '("node.id: ", a)')   trim(node%id)
        write (unit_, '("node.name: ", a)') trim(node%name)
        write (unit_, '("node.meta: ", a)') trim(node%meta)
    end subroutine dm_node_out
end module dm_node
