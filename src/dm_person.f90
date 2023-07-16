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
        character(len=PERSON_NAME_LEN)  :: name  = ' ' !! Person name.
        character(len=PERSON_MAIL_LEN)  :: mail  = ' ' !! Person e-mail address.
        character(len=PERSON_PHONE_LEN) :: phone = ' ' !! Person phone number.
    end type person_type

    interface operator (==)
        !! Returns whether two persons are equal.
        module procedure :: dm_person_equals
    end interface

    public :: operator (==)

    public :: dm_person_equals
    public :: dm_person_has_mail
    public :: dm_person_has_name
contains
    pure elemental logical function dm_person_equals(person1, person2) result(equals)
        !! Returns `.true.` if both persons are equal.
        type(person_type), intent(in) :: person1 !! First person.
        type(person_type), intent(in) :: person2 !! Second person.

        equals = .false.
        if (person1%name /= person2%name) return
        if (person1%mail /= person2%mail) return
        if (person1%phone /= person2%phone) return
        equals = .true.
    end function dm_person_equals

    pure elemental logical function dm_person_has_mail(person) result(has)
        !! Returns `.true.` if the person has a e-mail address.
        type(person_type), intent(in) :: person !! Person type.

        has = .false.
        if (len_trim(person%mail) == 0) return
        has = .true.
    end function dm_person_has_mail

    pure elemental logical function dm_person_has_name(person) result(has)
        !! Returns `.true.` if the person has a name.
        type(person_type), intent(in) :: person !! Person type.

        has = .false.
        if (len_trim(person%mail) == 0) return
        has = .true.
    end function dm_person_has_name
end module dm_person
