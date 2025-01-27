! Author:  Philipp Engel
! Licence: ISC
module dm_const
    !! Definitions of mathematical constants.
    use :: dm_kind
    implicit none (type, external)
    private

    real(kind=r8), parameter, public :: PI  = acos(-1.0_r8) !! Pi.
    real(kind=r8), parameter, public :: PI2 = 2 * PI        !! 2 Pi.
end module dm_const
