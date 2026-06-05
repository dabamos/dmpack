! Author:  Philipp Engel
! Licence: ISC
module dm_xml
    !! XML syntax and utilities.
    implicit none (type, external)
    private

    character(*), parameter, public :: XML_HEADER = '<?xml version="1.0" encoding="UTF-8"?>'

    public :: dm_xml_encode
contains
    pure function dm_xml_encode(input) result(output)
        !! Returns encoded input string, with some XML special characters
        !! replaced (`"`, `&`, `'`, `<`, `>`).
        character(*), intent(in)  :: input  !! Input string.
        character(:), allocatable :: output !! Encoded string.

        integer :: i

        output = ''

        do i = 1, len_trim(input)
            select case (input(i:i))
                case ('"');   output = output // '&quot;'
                case ('&');   output = output // '&amp;'
                case ("'");   output = output // '&apos;'
                case ('<');   output = output // '&lt;'
                case ('>');   output = output // '&gt;'
                case default; output = output // input(i:i)
            end select
        end do
    end function dm_xml_encode
end module dm_xml
