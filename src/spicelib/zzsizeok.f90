!$Procedure      ZZSIZEOK ( Determine if the size of a segment is ok )

      SUBROUTINE ZZSIZEOK ( SIZE, PSIZE, DSIZE, OFFSET, OK, N )

! $ Abstract
!
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine exists to determine whether or not the type of
!     a DAF segment is compatible with the sizes allowed for SPK
!     type 01 segments or CK type 02 segments.  However, more generally
!     it determines whether or not the integer equation:
!
!         SIZE = PSIZE*N + (N-OFFSET)/DSIZE
!
!     can be satisfied for some value of N.  Moreover, if such
!     an N exists (there can be only one) it returns that value.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!     NUMERIC
!
! $ Declarations

      IMPLICIT NONE
      INTEGER               SIZE
      INTEGER               PSIZE
      INTEGER               DSIZE
      INTEGER               OFFSET
      LOGICAL               OK
      INTEGER               N

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SIZE       I   Left hand side of the equation in the abstract
!     PSIZE      I   Coefficient of N (packet size).
!     DSIZE      I   Divisor of N-OFFSET (directory size).
!     OFFSET     I   Offset used in computation of number of directories
!     OK         O   TRUE if a solution for N exists.
!     N          O   Value of N if there is a solution, 0 otherwise.
!
! $ Detailed_Input
!
!     SIZE       Constant terms in the equation given in the abstract.
!     PSIZE
!     DSIZE
!
!     OFFSET     Constant term in the equation above.  It should be
!                1 or 0.
!
! $ Detailed_Output
!
!     OK         is TRUE if an integer solution for N exists.  Otherwise
!                it is returned FALSE.
!
!     N          is the solution to the equation in the abstract
!                if such a solution exists.  Otherwise it is returned
!                with the value zero.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1)  If SIZE, PSIZE, or DSIZE is less than 1, OK is set to FALSE
!         N is set to zero and no attempt is made at finding a
!         solution.
!
! $ Particulars
!
!     This routine determines whether or not the integer arithmetic
!     equation
!
!         SIZE = PSIZE*N + (N-1)/DSIZE
!
!     has a solution for N and if so returns the value of N.
!
!     The routine is intended for checking the sizes of segments
!     for SPK type 01 and CK type 02.  For SPK type 01,
!
!        SIZE   = segment size - 1
!        PSIZE  = 72
!        DSIZE  = 100
!        OFFSET = 0
!
!
!     for CK type 02,
!
!        SIZE   = segment size
!        PSIZE  = 10
!        DSIZE  = 100
!        OFFSET = 1
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 02-DEC-1999 (WLT)
!
!
!-&

!
!     Here's the scoop.
!
!     Suppose N is a solution to SIZE = PSIZE*N + (N-OFFSET)/DSIZE
!     N can be represented uniquely as
!
!         N = q*DSIZE + r
!
!     where OFFSET <= r <= DSIZE+OFFSET-1.  Therefore there must
!     be values q and r such that
!
!        SIZE = PSIZE*(q*DSIZE + r ) + ( q*DSIZE + r - 1 ) / DSIZE
!
!             = PSIZE*DSIZE*q + q + PSIZE*r
!
!             = (PSIZE*DSIZE+1)*q + PSIZE*r
!
!     But SIZE can be represented uniquely as
!
!           SIZE = (PSIZE*DSIZE+1)*k + a
!
!     where  0 <= a < (PSIZE*DSIZE+1).
!
!     But   PSIZE*OFFSET < PSIZE*r < (PSIZE*DSIZE+OFFSET-1),
!    therefore  it must be that
!
!              SIZE mod(PSIZE*DSIZE+1) = PSIZE*r
!     and                            q = k
!
!     Hence, there is a solution to our equation if and only if
!
!          PSIZE divides SIZE mod(PSIZE*DSIZE+1)
!     and  OFFSET*PSIZE <= SIZE mod(PSIZE*DSIZE+1)
!

      INTEGER               Q
      INTEGER               PD1
      INTEGER               R
      INTEGER               A


!
!     Handle the exceptional case first.
!
      IF (       SIZE .LE. 0 &
           .OR. DSIZE .LE. 0 &
           .OR. PSIZE .LE. 0 ) THEN
         N  = 0
         OK = .FALSE.
         RETURN
      END IF

      PD1 = PSIZE*DSIZE + 1

      CALL RMAINI ( SIZE, PD1, Q, A )

      IF ( OFFSET*PSIZE .GT. A ) THEN
         N  = 0
         OK = .FALSE.
         RETURN
      END IF

      IF ( A .EQ. (A/PSIZE)*PSIZE ) THEN

         R  = A/PSIZE
         N  = DSIZE*Q + R
         OK = .TRUE.

      ELSE

         OK = .FALSE.
         N  =  0

      END IF

      RETURN
      END
