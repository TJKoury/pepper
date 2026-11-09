!$Procedure  T_PD2DT ( Polynomial derivatives to difference table )

      SUBROUTINE T_PD2DT ( DEGP, PDERIV, WORK, DIFTAB )
      IMPLICIT NONE

! $ Abstract
!
!     Convert a polynomial and its derivatives, evaluated at a point,
!     to an MDA "difference table."
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     SPK
!
! $ Declarations

      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 35 )

      INTEGER               DEGP
      DOUBLE PRECISION      PDERIV ( 0 : DEGP                )
      DOUBLE PRECISION      WORK   ( 0 : DEGP-1,  0 : DEGP-1 )
      DOUBLE PRECISION      DIFTAB ( 0 : DEGP                )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     MAXDEG     I   Maximum degree of internal work space buffer.
!     DEGP       I   Polynomial degree.
!     PDERIV    I-O  Array of polynomial derivatives.
!     WORK      I-O  Work space array.
!     DIFTAB     O   Difference table.
!
! $ Detailed_Input
!
!     DEGP           is the degree of a polynomial P(x) to be mapped to
!                    a difference table.
!
!     PDERIV         is an array of derivatives of P(x), evaluated at
!                    some point.  Some computations are done in place
!                    in this array, so this is an in-out argument.
!                    The element of PDERIV at index 0 is the function
!                    value itself.
!
!                    In normal use within SPKNIO, PDERIV will be the
!                    derivatives of a polynomial representing one
!                    component (x-, y-, or z-) of the acceleration of
!                    an ephemeris object.
!
!     WORK           is a work space array.
!
! $ Detailed_Output
!
!     PDERIV         is an array of derivatives of P(x), evaluated at
!                    some point.  Some computations are done in place
!                    in this array, so this is an in-out argument.
!                    On output, the jth element of PDERIV has been
!                    divided by (j+1)!.
!
!     WORK           is the work space array, filled in with a matrix
!                    used to produce the difference table.
!
!     DIFTAB         is a difference table.  This is a vector that
!                    represents the input polynomial and its
!                    derivatives. It is in a form such that, when the
!                    input polynomial derivative vector represents a
!                    component of the acceleration of some object,
!                    DIFTAB is amenable to integration by the SPICELIB
!                    type 1 SPK evaluator SPKE01.
!
!                    The relationship of DIFTAB to the input polynomial
!                    derivative vector is given by the equations below.
!                    The matrix labeled M below is upper-triangular,
!                    with zeros below the main diagonal.
!
!
!     DIFTAB(0)       =                             PDERIV(0)
!
!
!   +-            -+     +-                    -+ +-                 -+
!   | DIFTAB(1)    |     | \      k-j   k!      | | PDERIV(1)   /1!   |
!   | DIFTAB(2)    |     |  \ (-1)   ---------  | | PDERIV(2)   /2!   |
!   |       .      |  =  |   \       (k-j)! j!  | |       .           |
!   |       .      |     |    \                 | |       .           |
!   |       .      |     | 0   \                | |       .           |
!   | DIFTAB(PDEG) |     |      \               | | PDERIV(PDEG)/PDEG!|
!   +-            -+     +-                    -+ +-                 -+
!
!                                   (M)
!
!
!
! $ Parameters
!
!     MAXDEG        is the maximum polynomial degree supported by
!                   the internal work space buffer.  Difference tables
!                   having higher degree than MAXDEG are handled by
!                   this routine, but the run-time efficiency will be
!                   lower, because the matrix M**-1 will have to
!                   be re-computed on each call.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) This routine is intended for use only within the SPKNIO
!        program.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 2.0.1, 24-NOV-2021 (NJB)
!
!        This routine has been moved to the TESTUTIL library.
!
!-    TSPICE Version 2.0.0, 19-OCT-2012 (NJB)
!
!        MAXDEG was increased to 35. Routine was renamed for
!        use in TSPICE. Call to CHOOSE was replaced with call
!        to T_CHOOSE.
!
!-    SPKNIO Version 1.1.0, 02-MAR-2003 (NJB)
!
!        Now buffers the matrix
!
!            -1
!           M
!
!        and re-uses this matrix when the difference table degree
!        matches that from the previous call.
!
!-    SPKNIO Version 1.0.0, 11-APR-2000 (NJB)
!
!-&

!

!
!     Non-SPICELIB functions
!
      INTEGER               T_CHOOSE

!
!     Local variables
!
      DOUBLE PRECISION      FACT
      DOUBLE PRECISION      MINV   ( MAXDEG, MAXDEG )

      INTEGER               DEGPSQ
      INTEGER               J
      INTEGER               K
      INTEGER               PRVDEG
      INTEGER               SIGN

      LOGICAL               USEMNV

!
!     Saved variables
!
      SAVE                  MINV
      SAVE                  PRVDEG

!
!     Initial values
!
      DATA                  PRVDEG  / -1 /

!
!     The first element of the difference table is just the
!     constant term from the polynomial.
!
      DIFTAB(0) = PDERIV(0)

!
!     Use the work space array to build the mystery matrix
!
!         -1
!        M
!
!
!     The (j,k) entry of the mystery matrix is
!
!
!            k-j
!        (-1)    * ( <k choose j> )
!
!
!     for k >= j.  The lower triangular portion of the matrix is
!     identically zero.
!
      IF (       ( DEGP .EQ. PRVDEG ) &
           .AND. ( DEGP .LE. MAXDEG )  ) THEN
!
!        We can use the previously computed matrix MINV.
!
         USEMNV = .TRUE.

      ELSE

         DEGPSQ = DEGP * DEGP

         CALL CLEARD ( DEGPSQ, WORK )


         DO J = 0, DEGP-1

            SIGN   =   1

            DO K = J, DEGP-1

               WORK(J,K) = SIGN * T_CHOOSE( K, J )

               SIGN = -SIGN

            END DO

         END DO

         IF ( DEGP .LE. MAXDEG ) THEN
            CALL MOVED ( WORK, DEGPSQ, MINV )
         END IF


         USEMNV = .FALSE.

      END IF


!
!     Divide the jth element of PDERIV by j!.
!
      FACT = 1

      DO J = 1, DEGP

         FACT = J*FACT

         PDERIV(J) = PDERIV(J) / FACT

      END DO

!
!     Now left-multiply our vector of polynomial derivatives by
!     the mystery matrix.
!
      IF ( USEMNV ) THEN

         CALL MXVG ( MINV, PDERIV(1), DEGP, DEGP, DIFTAB(1) )

      ELSE

         CALL MXVG ( WORK, PDERIV(1), DEGP, DEGP, DIFTAB(1) )

      END IF

      PRVDEG = DEGP

      END
