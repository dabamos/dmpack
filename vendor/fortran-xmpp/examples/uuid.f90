! uuid.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    !! UUID example program, based on the `uuid.c`.
    use :: xmpp
    implicit none (type, external)

    character(len=:), allocatable :: uuid
    type(c_ptr)                   :: ctx

    ctx = xmpp_ctx_new(c_null_ptr, c_null_ptr)

    uuid = xmpp_uuid_gen(ctx)

    if (len(uuid) == 0) then
        print '("Could not allocate memory")'
        stop
    end if

    print '("UUID: ", a)', uuid

    call xmpp_ctx_free(ctx)
end program main
