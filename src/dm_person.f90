! Author:  Philipp Engel
! Licence: ISC
module dm_person
    !! Module for handling natural persons.
    implicit none (type, external)
    private

    integer, parameter, public :: PERSON_NAME_LEN  = 80 !! Max. name length.
    integer, parameter, public :: PERSON_MAIL_LEN  = 80 !! Max. address length.
    integer, parameter, public :: PERSON_PHONE_LEN = 80 !! Max. phone number length

    type, public :: person_type
        !! Person type to store name and contact details.
        character(PERSON_NAME_LEN)  :: name  = ' ' !! Person name.
        character(PERSON_MAIL_LEN)  :: mail  = ' ' !! Person e-mail address.
        character(PERSON_PHONE_LEN) :: phone = ' ' !! Person phone number.
    end type person_type

    interface operator (==)
        !! Returns `.true.` if persons are equal.
        module procedure :: dm_person_equals
    end interface

    public :: operator (==)

    public :: dm_person_equals
    public :: dm_person_has_mail
    public :: dm_person_has_name
    public :: dm_person_out
contains
    pure elemental logical function dm_person_equals(person1, person2) result(equals)
        !! Returns `.true.` if both persons are equal.
        type(person_type), intent(in) :: person1 !! First person.
        type(person_type), intent(in) :: person2 !! Second person.

        equals = (person1%name  == person2%name .and. &
                  person1%mail  == person2%mail .and. &
                  person1%phone == person2%phone)
    end function dm_person_equals

    pure elemental logical function dm_person_has_mail(person) result(has)
        !! Returns `.true.` if the person has a e-mail address.
        type(person_type), intent(in) :: person !! Person.

        has = (len_trim(person%mail) > 0)
    end function dm_person_has_mail

    pure elemental logical function dm_person_has_name(person) result(has)
        !! Returns `.true.` if the person has a name.
        type(person_type), intent(in) :: person !! Person.

        has = (len_trim(person%name) > 0)
    end function dm_person_has_name

    subroutine dm_person_out(person, unit)
        !! Prints person to standard output or given file unit.
        use :: dm_kind, only: STDOUT
        use :: dm_util, only: dm_present

        type(person_type), intent(in)           :: person !! Person.
        integer,           intent(in), optional :: unit !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, STDOUT)

        write (unit_, '("person.name: ", a)')  trim(person%name)
        write (unit_, '("person.mail: ", a)')  trim(person%mail)
        write (unit_, '("person.phone: ", a)') trim(person%phone)
    end subroutine dm_person_out
end module dm_person
