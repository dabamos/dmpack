! Author:  Philipp Engel
! Licence: ISC
module dm_la
    !! LAPACK95 wrapper routines around LAPACK (double precision).
    use :: dm_type
    implicit none (type, external)
    private

    ! Fortran 90 interfaces to LAPACK.
    interface
        function ilaenv(ispec, name, opts, n1, n2, n3, n4)
            implicit none
            integer,          intent(in) :: ispec
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: opts
            integer,          intent(in) :: n1
            integer,          intent(in) :: n2
            integer,          intent(in) :: n3
            integer,          intent(in) :: n4
            integer                      :: ilaenv
        end function ilaenv

        subroutine dgels(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
            import :: r8
            implicit none
            character,     intent(in)    :: trans
            integer,       intent(in)    :: m
            integer,       intent(in)    :: n
            integer,       intent(in)    :: nrhs
            integer,       intent(in)    :: lda
            real(kind=r8), intent(inout) :: a(lda, *)
            integer,       intent(in)    :: ldb
            real(kind=r8), intent(inout) :: b(ldb, *)
            real(kind=r8), intent(out)   :: work(*)
            integer,       intent(in)    :: lwork
            integer,       intent(out)   :: info
        end subroutine dgels

        subroutine dgelsd(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, lwork, iwork, info)
            import :: r8
            implicit none
            integer,       intent(in)    :: m
            integer,       intent(in)    :: n
            integer,       intent(in)    :: nrhs
            integer,       intent(in)    :: lda
            real(kind=r8), intent(inout) :: a(lda, *)
            integer,       intent(in)    :: ldb
            real(kind=r8), intent(inout) :: b(ldb, *)
            real(kind=r8), intent(out)   :: s(*)
            real(kind=r8), intent(in)    :: rcond
            integer,       intent(out)   :: rank
            real(kind=r8), intent(out)   :: work(*)
            integer,       intent(in)    :: lwork
            integer,       intent(inout) :: iwork(*)
            integer,       intent(out)   :: info
        end subroutine dgelsd
    end interface

    interface dm_la_gels
        module procedure :: dm_la_dgels
        module procedure :: dm_la_dgels1
    end interface

    interface dm_la_gelsd
        module procedure :: dm_la_dgelsd
        module procedure :: dm_la_dgelsd1
    end interface

    public :: dm_la_gels
    public :: dm_la_gelsd

    public :: dm_la_dgels
    public :: dm_la_dgels1
    public :: dm_la_dgelsd
    public :: dm_la_dgelsd1

    private :: la_erinfo
    private :: la_same
    private :: la_ws_gels
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    subroutine dm_la_dgels(a, b, trans, info)
        !! LA_GELS computes the minimum-norm least squares solution to one or
        !! more real or complex linear systems of the form A*x = b, A^T*x = b or
        !! A^H*x = b using a QR or LQ factorization of A. Matrix A is
        !! rectangular assumed to be of full rank. The vectors b and
        !! corresponding solution vectors x are the columns of matrices denoted
        !! B and X, respectively.
        !!
        !! LAPACK95 interface driver routine (version 3.0)
        !! UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
        !! September, 2000
        character(len=*), parameter :: SRNAME = 'LA_GELS'
        character(len=*), parameter :: VER    = 'D'

        real(kind=r8), intent(inout)         :: a(:, :)
        real(kind=r8), intent(inout)         :: b(:, :)
        character,     intent(in),  optional :: trans
        integer,       intent(out), optional :: info

        character :: ltrans
        integer   :: linfo, istat, istat1, lwork, n, m, nrhs

        real(kind=r8), pointer :: work(:)

        linfo = 0
        istat = 0

        m    = size(a, 1)
        n    = size(a, 2)
        nrhs = size(b, 2)

        if (present(trans)) then
            ltrans = trans
        else
            ltrans = 'n'
        end if

        if (m < 0 .or. n < 0) then
            linfo = -1
        else if (size(b, 1) /= max(1, m, n) .or. nrhs < 0) then
            linfo = -2
        else if (.not. (la_same(ltrans, 'N') .or. la_same(ltrans, 'T'))) then
            linfo = -3
        else
            lwork = la_ws_gels(VER, m, n, nrhs, ltrans)
            allocate (work(lwork), stat=istat)

            if (istat /= 0) then
                deallocate (work, stat=istat1)
                work = min(m, n) + max(1, m, n, nrhs)

                allocate (work(lwork), stat=istat)
                if (istat /= 0) call la_erinfo(-200, SRNAME, linfo)
            end if

            if (istat == 0) then
                call dgels(ltrans, m, n, nrhs, a, max(1, m), b, max(1, m, n), &
                           work, lwork, linfo)
            else
                linfo = -100
            end if

            deallocate (work, stat=istat1)
        end if

        call la_erinfo(linfo, SRNAME, info, istat)
    end subroutine dm_la_dgels

    subroutine dm_la_dgels1(a, b, trans, info)
        !! LAPACK95 interface driver routine (version 3.0) --
        !! UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
        !! September, 2000
        character(len=*), parameter :: SRNAME = 'LA_GELS'
        character(len=*), parameter :: VER    = 'D'

        real(kind=r8), intent(inout)         :: a(:, :)
        real(kind=r8), intent(inout)         :: b(:)
        character,     intent(in),  optional :: trans
        integer,       intent(out), optional :: info

        character :: ltrans
        integer   :: linfo, istat, istat1, lwork, n, m

        real(kind=r8), pointer :: work(:)

        linfo = 0
        istat = 0

        m = size(a, 1)
        n = size(a, 2)

        if (present(trans)) then
            ltrans = trans
        else
            ltrans = 'N'
        end if

        if (m < 0 .or. n < 0) then
            linfo = -1
        else if (size(b) /= max(1, m, n)) then
            linfo = -2
        else if (.not.(la_same(ltrans, 'N') .or. la_same(ltrans, 'T'))) then
            linfo = -3
        else
            lwork = la_ws_gels(ver, m, n, 1, ltrans)
            allocate (work(lwork), stat=istat)

            if (istat /= 0) then
                deallocate (work, stat=istat1)
                lwork = min(m, n) + max(1, m, n)
                allocate (work(lwork), stat=istat)
                if (istat /= 0) call la_erinfo(-200, SRNAME, linfo)
            end if

            if (istat == 0) then
                call dgels(ltrans, m, n, 1, a, max(1, m), b, max(1, m, n), &
                           work, lwork, linfo)
            else
                linfo = -100
            end if

            deallocate (work, stat=istat1)
        end if

        call la_erinfo(linfo, SRNAME, info, istat)
    end subroutine dm_la_dgels1

    subroutine dm_la_dgelsd(a, b, rank, s, rcond, info)
        !! LA_GELSS and LA_GELSD compute the minimum-norm least squares solution
        !! to one or more real or complex linear systems A*x = b using the
        !! singular value decomposition of A. Matrix A is rectangular and may be
        !! rank-deficient. The vectors b and corresponding solution vectors x
        !! are the columns of matrices denoted B and X, respectively.
        !!
        !! The effective rank of A is determined by treating as zero those
        !! singular values which are less than RCOND times the largest singular
        !! value. In addition to X, the routines also return the right singular
        !! vectors and, optionally, the rank and singular values of A.
        !!
        !! LA_GELSD combines the singular value decomposition with a divide and
        !! conquer technique. For large matrices it is often much faster than
        !! LA_GELSS but uses more workspace.
        !!
        !! LAPACK95 interface driver routine (version 3.0) --
        !! UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
        !! September, 2000
        character(len=*), parameter :: SRNAME = 'LA_GELSD'

        real(kind=r8),         intent(inout)         :: a(:, :)
        real(kind=r8),         intent(inout)         :: b(:, :)
        integer,               intent(out), optional :: rank
        real(kind=r8), target, intent(out), optional :: s(:)
        real(kind=r8),         intent(in),  optional :: rcond
        integer,               intent(out), optional :: info

        integer :: linfo, istat, lwork, n, m, mn, nrhs
        integer :: lrank, ss, liwork, smlsiz, nlvl

        integer       :: iworkmin(1)
        real(kind=r8) :: lrcond
        real(kind=r8) :: workmin(1)

        integer,       pointer :: iwork(:)
        real(kind=r8), pointer :: work(:)
        real(kind=r8), pointer :: ls(:)

        linfo = 0
        istat = 0

        m    = size(a, 1)
        n    = size(a, 2)
        nrhs = size(b, 2)

        mn     = min(m,n)
        smlsiz = ilaenv(9, 'DGELSD', ' ', 0, 0, 0, 0)
        nlvl   = int(log(dble(max(1, mn)) / dble(smlsiz + 1)) / log(2.0_r8))
        liwork = 3 * max(m, n) * (3 * nlvl + 11)

        if (present(rcond)) then
            lrcond = rcond
        else
            lrcond = 100 * epsilon(1.0_r8)
        end if

        if (present(s)) then
            ss = size(s)
        else
            ss = mn
        end if

        if (m < 0 .or. n < 0) then
            linfo = -1
        else if (size(b, 1) /= max(1, m, n) .or. nrhs < 0) then
            linfo = -2
        else if (ss /= mn) then
            linfo = -4
        else if (lrcond <= 0.0_r8) then
            linfo = -5
        else
            if (present(s)) then
                ls => s
            else
                allocate (ls(mn), stat=istat)

                if (istat /= 0) then
                    linfo = -100
                    goto 100
                end if
            end if

            lwork = -1
            call dgelsd(m, n, nrhs, a, max(1, m), b, max(1, m, n), &
                        ls, lrcond, lrank, workmin, lwork, iworkmin,  linfo)
            lwork = int(workmin(1))

            allocate (work(lwork), stat=istat)

            if (istat /= 0) then
                linfo = -100
                goto 200
            end if

            allocate (iwork(liwork), stat=istat)

            if (istat /= 0) then
                linfo = -100
                goto 250
            end if

            call dgelsd(m, n, nrhs, a, max(1, m), b, size(b, 1), &
                        ls, lrcond, lrank, work, lwork, iwork, linfo)
            if (present(rank)) rank = lrank

            deallocate (iwork)
            250 deallocate (work)
            200 if (.not. present(s)) deallocate (ls)
        end if

        100 call la_erinfo(linfo, SRNAME, info, istat)
    end subroutine dm_la_dgelsd

    subroutine dm_la_dgelsd1(a, b, rank, s, rcond, info)
        character(len=*), parameter :: SRNAME = 'LA_GELSD'

        real(kind=r8),         intent(inout)         :: a(:, :)
        real(kind=r8),         intent(inout)         :: b(:)
        integer,               intent(out), optional :: rank
        real(kind=r8), target, intent(out), optional :: s(:)
        real(kind=r8),         intent(in),  optional :: rcond
        integer,               intent(out), optional :: info

        real(kind=r8) :: lrcond
        real(kind=r8) :: workmin(1)
        integer       :: iworkmin(1)
        integer       :: linfo, istat, lwork, n, m, mn
        integer       :: lrank, ss, liwork, smlsiz, nlvl

        real(kind=r8), pointer :: work(:)
        real(kind=r8), pointer :: ls(:)
        integer,       pointer :: iwork(:)

        linfo = 0
        istat = 0

        m  = size(a, 1)
        n  = size(a, 2)
        mn = min(m, n)

        smlsiz = ilaenv(9, 'DGELSD', ' ', 0, 0, 0, 0)
        nlvl   = int(log(dble(max(1, mn)) / dble(smlsiz + 1)) / log(2.0_r8))
        liwork = 3 * max(m, n) * (3 * nlvl + 11)

        if (present(rcond)) then
            lrcond = rcond
        else
            lrcond = 100 * epsilon(1.0_r8)
        end if

        if (present(s)) then
            ss = size(s)
        else
            ss = mn
        end if

        if (m < 0 .or. n < 0) then
            linfo = -1
        else if (size(b, 1) /= max(1, m, n)) then
            linfo = -2
        else if (ss /= mn) then
            linfo = -4
        else if (lrcond <= 0.0_r8) then
            linfo = -5
        else
            if (present(s)) then
                ls => s
            else
                allocate (ls(mn), stat=istat)

                if (istat /= 0) then
                    linfo = -100
                    goto 100
                end if
            end if

            lwork = -1
            call dgelsd(m, n, 1, a, max(1, m), b, max(1, m, n), &
                        ls, lrcond, lrank, workmin, lwork, iworkmin, linfo)
            lwork = int(workmin(1))

            allocate (work(lwork), stat=istat)

            if (istat /= 0) then
                linfo = -100
                goto 200
            end if

            allocate (iwork(liwork), stat=istat)

            if (istat /= 0) then
                linfo = -100
                goto 250
            end if

            call dgelsd(m, n, 1, a, max(1, m), b, size(b, 1), &
                        ls, lrcond, lrank, work, lwork, iwork, linfo)

            if (present(rank)) rank = lrank

            deallocate (iwork)
            250 deallocate (work)
            200 if (.not. present(s)) deallocate (ls)
        end if

        100 call la_erinfo(linfo, SRNAME, info, istat)
    end subroutine dm_la_dgelsd1

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    logical function la_same(ca, cb) result(same)
        !! LAPACK95 auxility routine thats tests if ca is the same letter as cb
        !! regardless of case.
        character, intent(in) :: ca
        character, intent(in) :: cb

        integer :: a, b, zcode

        same = (ca == cb)

        if (.not. same) then
            zcode = ichar('z')

            a = ichar(ca)
            b = ichar(cb)

            if (zcode == 90 .or. zcode == 122) then
                if (a >= 97 .and. a <= 122) a = a - 32
                if (b >= 97 .and. b <= 122) b = b - 32
            else if (zcode == 233 .or. zcode == 169) then
                if (a >= 129 .and. a <= 137 .or. &
!                   a >= 145 .and. a <= 153 .or. &
                    a >= 162 .and. a <= 169) a = a + 64
                if (b >= 129 .and. b <= 137 .or. &
                    b >= 145 .and. b <= 153 .or. &
                    b >= 162 .and. b <= 169) b = b + 64
            else if (zcode == 218 .or. zcode == 250) then
                if (a >= 225 .and. a <= 250) a = a - 32
                if (b >= 225 .and. b <= 250) b = b - 32
            end if

            same = (a == b)
        end if
    end function la_same

    integer function la_ws_gels(ver, m, n, nrhs, trans)
        !! LAPACK95 interface driver routine (version 3.0)
        !! UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
        !! September, 2000
        character(len=*), parameter :: NAME1 = 'GEQRF'
        character(len=*), parameter :: NAME2 = 'ORMQR'
        character(len=*), parameter :: NAME3 = 'ORMLQ'
        character(len=*), parameter :: NAME4 = 'GELQF'

        character, intent(in) :: ver
        integer,   intent(in) :: m
        integer,   intent(in) :: n
        integer,   intent(in) :: nrhs
        character, intent(in) :: trans

        integer :: nb, mn
        logical :: tpsd

        mn = min(m, n)

        if (la_same(trans, 'N')) then
            tpsd = .false.
        else
            tpsd = .true.
        end if

        if (m >= n) then
            nb = ilaenv(1, ver // NAME1, ' ', m, n, -1, -1)

            if (tpsd) then
                nb = max(nb, ilaenv(1, ver // NAME2, 'LN', m, nrhs, n, -1))
            else
                nb = max(nb, ilaenv(1, ver // NAME2, 'LT', m, nrhs, n, -1))
            end if
        else
            nb = ilaenv(1, ver // name4, ' ', m, n, -1, -1)

            if (tpsd) then
                nb = max(nb, ilaenv(1, ver // NAME3, 'LT', n, nrhs, m, -1))
            else
                nb = max(nb, ilaenv(1, ver // NAME3, 'LN', n, nrhs, m, -1))
            end if
        end if

        la_ws_gels = mn + max(m, n, nrhs) * nb
    end function la_ws_gels

    subroutine la_erinfo(linfo, srname, info, istat)
        character(len=*), intent(in)            :: srname
        integer,          intent(in)            :: linfo
        integer,          intent(out), optional :: info
        integer,          intent(in),  optional :: istat

        ! if ((linfo < 0 .and. linfo > -200) .or. &
        !     (linfo > 0 .and. .not. present(info))) then

        if (((linfo < 0 .and. linfo > -200) .or. linfo > 0) .and. .not. present(info)) then
            print *, 'Program terminated in LAPACK95 subroutine ', srname
            print *, 'Error indicator, INFO = ', linfo

            if (present(istat)) then
                if (istat /= 0) then
                    if (linfo == -100) then
                        print *, 'The statement ALLOCATE causes STATUS = ', istat
                    else
                        print *, 'LINFO = ', linfo, ' not expected'
                    end if
                end if
            end if

            stop
        else if (linfo <= -200) then
            print *, '++++++++++++++++++++++++++++++++++++++++++++++++'
            print *, '*** WARNING, INFO = ', linfo, ' WARNING ***'

            if (linfo == -200) then
                print *, 'Could not allocate sufficient workspace for the optimum'
                print *, 'blocksize, hence the routine may not have performed as'
                print *, 'efficiently as possible'
            else
                print *, 'Unexpected warning'
            end if

            print *, '++++++++++++++++++++++++++++++++++++++++++++++++'
        end if

        if (present(info)) info = linfo
    end subroutine la_erinfo
end module dm_la
