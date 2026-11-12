!$Procedure      T_TAYHRM ( Taylor expansions of Hermite polynomials )

      SUBROUTINE T_TAYHRM ( N,  NEQ,  MBUFF,  RSYS, DERIVS, FOUND )
      use pepper_legacy

! $ Abstract
!
!     Find the Taylor expansions about a specified point of a set of
!     polynomials defined by Hermite-style constraints.  Return the
!     derivatives of the expansions.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     SPKNIO
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               N
      INTEGER               NEQ
      DOUBLE PRECISION      MBUFF  ( 4 * N * N )
      DOUBLE PRECISION      RSYS   ( 2 * N,       NEQ )
      DOUBLE PRECISION      DERIVS ( 2 * N,       NEQ )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     N          I   Number of abscissa points.
!     NEQ        I   Number of equations.
!     MBUFF      I   Buffer containing matrix of Hermite constraints.
!     RSYS       I   Array of RHS vectors of equations.
!     DERIVS     O   Array of derivatives of Taylor expansions.
!     FOUND      O   Flag indicating whether solution was found.
!
! $ Detailed_Input
!
!     N              is the size of a grid of abscissa points on
!                    which a set of polynomials is defined.
!
!     NEQ            is the number of matrix equations defining the set
!                    of polynomials.  There is one matrix equation
!                    for each polynomial.
!
!     MBUFF          is a buffer into which are packed the columns of a
!                    constraint matrix M. Let XVALs be a set of
!                    abscissa points, and let X be the central point of
!                    a Taylor expansion. Then the matrix M has the form
!
!  +-                                                                 -+
!  | 1 [XVALS(1)-X]   [XVALS(1)-X]**2 ...         [XVALS(1)-X]**(2*N-1)|
!  | 1 [XVALS(2)-X]   [XVALS(2)-X]**2 ...         [XVALS(2)-X]**(2*N-1)|
!  |                          .                                        |
!  |                          .                                        |
!  |                          .                                        |
!  | 1 [XVALS(N)-X]   [XVALS(N)-X]**2 ...         [XVALS(N)-X]**(2*N-1)|
!  | 0      1       2*[XVALS(1)-X]**1 ... (2*N-1)*[XVALS(1)-X]**(2*N-2)|
!  | 0      1       2*[XVALS(2)-X]**1 ... (2*N-1)*[XVALS(2)-X]**(2*N-2)|
!  |                          .                                        |
!  |                          .                                        |
!  |                          .                                        |
!  | 0      1       2*[XVALS(N)-X]**1 ... (2*N-1)*[XVALS(N)-X]**(2*N-2)|
!  +-                                                                 -+
!
!                    The matrix M is packed into MBUFF in column-major
!                    order: the first 2*N elements of MBUFF contain the
!                    first column of the matrix, the next 2*N elements
!                    contain the second column of the matrix, and so
!                    on.
!
!     RSYS           is an array of right-hand-sides of the NEQ matrix
!                    equations that this routine is meant to solve. Let
!                    M be the coefficient matrix described above and
!                    let C(*,i) be the ith array of Taylor coefficients
!                    which constitutes the solution of the ith
!                    equation. The ith column of RSYS is the RHS of the
!                    ith equation:
!
!                       M C(*,i) = RSYS(*,i),  i = 1, ..., NEQ
!
! $ Detailed_Output
!
!     DERIVS         is an array containing the derivatives of the
!                    polynomial expansions defined by MBUFF and RSYS,
!                    all evaluated at X.  DERIVS(I,J) contains the
!                    (I-1)st derivative of the Jth expansion.
!
!     FOUND          is a logical flag which has the value .TRUE. if
!                    DERIVS was computed and is .FALSE. otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If a solution cannot be found due to singularity of
!        the input matrix, the FOUND flag is set to .FALSE.
!        This case is not considered an error.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See the routine TAYMAT for a description of the type of
!     matrix equation this routine solves.  Since this routine is
!     typically used to find derivatives of a position vector, there
!     is normally one equation for each position component.
!
! $ Examples
!
!     See the SPKNIO routine T13MDA.
!
! $ Restrictions
!
!     1) This routine is for use only within the program TSPICE.
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
!        Corrected typo in description of input matrix MBUFF.
!        This routine has been moved to the TESTUTIL library.
!
!-    TSPICE Version 2.0.0, 19-OCT-2012 (NJB)
!
!        Renamed for use in TSPICE. Inputs XVALS and X, which were
!        included only for consistency with the routine TAYMAT, were
!        deleted. Unused local variable DEG was deleted.
!
!-    SPKNIO Version 1.0.0, 24-MAR-2003 (NJB)
!
!-&


!
!     SPICELIB functions
!

!
!     Local variables
!
      DOUBLE PRECISION      FACT

      INTEGER               I
      INTEGER               J


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_TAYHRM' )

!
!     Solve the systems of equations
!
!
!        M * COEFFS = RSYS(1,*)
!
!     where M is the coefficient matrix packed into MBUFF.
!

      CALL T_SOLVEG_2 ( RSYS, 2*N, NEQ, MBUFF, DERIVS, FOUND )

      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'T_TAYHRM' )
         RETURN
      END IF

!
!     Compute the derivatives of the Taylor expansion at X.
!
      FACT = 1.D0

      DO I = 1, 2*N

         IF ( I .GT. 2 ) THEN
            FACT = FACT * (I-1)
         END IF

         DO J = 1, NEQ
            DERIVS(I,J) = DERIVS(I,J) * FACT
         END DO

      END DO

      CALL CHKOUT ( 'T_TAYHRM' )
      RETURN
      END
