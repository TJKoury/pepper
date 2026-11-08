!$Procedure      T_SOLVEG_2 ( Solve a system of algebraic equations )

      SUBROUTINE T_SOLVEG_2 ( B, DIM, NEQ, A, X, FOUND )
      IMPLICIT NONE

! $ Abstract
!
!     Solve the matrix equation  B(i) = A X(i), i = 1, ... , n,
!     where A is a DIM by DIM matrix, B(i) is a set of n constant
!     vectors, and X(i) is a set of n vectors having dimension DIM.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     MATRIX
!     ROOT
!
! $ Declarations

      INTEGER               DIM
      INTEGER               NEQ
      DOUBLE PRECISION      B    ( DIM, NEQ )
      DOUBLE PRECISION      A    ( DIM, DIM )
      DOUBLE PRECISION      X    ( DIM, NEQ )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DIM        I   Dimension of the constant and solution vectors.
!     NEQ        I   Number of equations.
!     B         I/O  Array of constant vectors of the matrix equation.
!     A         I/O  Coefficient matrix
!     X          O   Array of solution vectors.
!     FOUND      O   Found flag.
!
! $ Detailed_Input
!
!     DIM       Dimension of the constant vectors and solution vectors.
!
!     NEQ       Number of equations.
!
!     B         Array of constant vectors of the matrix equation B =
!               Ax.  The input is destroyed by the process of solving
!               the system of equations for the vector X.
!
!     A         A DIM by DIM matrix of coefficients for the system of
!               equations.  The coefficient matrix is destroyed by the
!               solution process.  Note: It is assumed that A is
!               declared as:
!
!                  DOUBLE PRECISION      A(DIM,DIM)
!
!               If this is not the case A should be packed so that
!               in it can be treated as if it had been declared
!               as above.
!
!               (That is in physical memory, data should be arranged
!               as
!
!                A(1,1) ... A(n,1), A(2,1), ... A(n,2), A(1,3) ... etc.
!
! $ Detailed_Output
!
!     X         is the solution to the system of equations
!
!                  B(i) = A * X(i), i = 1, ... , NEQ
!
!     FOUND     is a logical flag indicating whether a solution was
!               found.  FOUND will be set to .FALSE. if the coefficient
!               matrix A is singular.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1) If the routine detects that the matrix is singular, it will
!        return with FOUND set to .FALSE.
!
!     2) This routine takes no action regarding matrices that are
!        nearly singular.  The user should read particulars and
!        determine if his/her situation warrants special consideration
!        for nearly singular matrices.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The problem of solving a system of N equations and N
!     unknowns can run into many numerical difficulties.  However, there
!     are many situations in which the system to be solved is small
!     and known to be well conditioned.  Under such situations, one
!     need not resort to the use of sophisticated black box routines
!     but may rely on techniques learned in elementary linear algebra
!     courses.
!
!     This routine is designed for accurate and stable solution of
!     systems of DIM equations in DIM variables where DIM is a small
!     integer (say less than 100). It uses the method of Gaussian
!     elimination with partial pivots. This method, while not the
!     fastest known method, is stable. Moreover it is simple and
!     easy to maintain.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     This routine does not handle problems of nearly singular matrices.
!     If you suspect your coefficient matrix is close to singular, you
!     should consider alternate methods.
!
! $ Literature_References
!
!     "Numerical Recipes --- The Art of Scientific Computing" by William
!     Press, Brian Flannery, Saul Teukolsky, William Vetterling.
!     Cambridge University Press 1986.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    TESTUTIL Version 2.0.1, 24-NOV-2021 (NJB)
!
!        Corrected spelling error.
!
!        This routine has been moved to the TESTUTIL library.
!
!-    TSPICE Version 2.0.0, 19-OCT-2012 (NJB)
!
!        Renamed for use in TSPICE. Several misspellings in
!        comments were corrected.
!
!-    Beta Version 1.0.0, 24-MAR-2003 (WLT) (NJB)
!
!-&


!
!     SPICELIB Functions
!
      LOGICAL               RETURN
!
!     Local parameters
!

!
!     Local Variables
!
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      MAXMAG
      DOUBLE PRECISION      PART
      DOUBLE PRECISION      TEMP

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               MOVE
      INTEGER               START


!
!     Standard Error Handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Nothing found yet.
!
      FOUND = .FALSE.

      START = 1

      DO WHILE ( START .LE. DIM )

!
!        First normalize each row of the matrix.
!
         DO I = START, DIM

            MAXMAG = 0
            MAG    = 0

            DO J = START,DIM
               IF ( DABS(A(I,J)) .GT. MAXMAG ) THEN
                  MAXMAG = DABS( A(I,J) )
               END IF
            END DO

            IF ( MAXMAG .EQ. 0 ) THEN
!
!              The found flag is already set.
!
               RETURN
            END IF

            DO J = START,DIM
               TEMP = A(I,J)/MAXMAG
               MAG  = MAG + TEMP*TEMP
            END DO

            MAG = MAXMAG*DSQRT(MAG)

            DO J = START, DIM
               A(I,J) = A(I,J)/MAG
            END DO

            DO K = 1, NEQ
               B(I,K) = B(I,K) / MAG
            END DO

         END DO

!
!        Find the row with the maximum first entry.
!
         MOVE    = START
         MAXMAG  = DABS( A(START,START) )

         DO I = START+1, DIM

            IF ( DABS(A(I,START)) .GT. MAXMAG ) THEN

               MAXMAG = DABS(A(I,START))
               MOVE   = I

            END IF

         END DO

!
!        If the first row is not the one with the largest
!        component of interest swap with the one that does.
!
         IF ( MOVE .NE. START ) THEN
            DO I = START, DIM
               CALL SWAPD ( A(MOVE,I), A(START,I) )
            END DO

            DO K = 1, NEQ
               CALL SWAPD ( B(MOVE,K), B(START,K) )
            END DO
         END IF

!
!        Now normalize the START row so that it begins with a 1.
!
         MAG = A(START,START)

         DO K = 1, NEQ
            B(START,K) = B(START,K)/MAG
         END DO

         DO I = START + 1, DIM
            A(START,I) = A(START,I)/MAG
         END DO

         DO I = START + 1, DIM

            DO K = 1, NEQ
               B(I,K) = B(I,K) - A(I,START)*B(START,K)
            END DO

            DO J = START + 1, DIM
               A(I,J) = A(I,J) - A(I,START)*A(START,J)
            END DO

         END DO

         START = START + 1

      END DO

!
!     Back substitute to get the solution.
!
      DO K = 1, NEQ

         DO I = DIM, 1, -1

            PART = 0

            DO J = DIM, I+1, -1

               PART = PART  +  A(I,J) * X(J,K)

            END DO

            X(I,K) = B(I,K) - PART

         END DO

      END DO

!
!     We've got a solution if we made it this far.
!
      FOUND = .TRUE.

      RETURN
      END
