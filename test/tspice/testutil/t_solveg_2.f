C$Procedure      T_SOLVEG_2 ( Solve a system of algebraic equations )

      SUBROUTINE T_SOLVEG_2 ( B, DIM, NEQ, A, X, FOUND )
      IMPLICIT NONE

C$ Abstract
C
C     Solve the matrix equation  B(i) = A X(i), i = 1, ... , n,
C     where A is a DIM by DIM matrix, B(i) is a set of n constant
C     vectors, and X(i) is a set of n vectors having dimension DIM.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     MATH
C     MATRIX
C     ROOT
C
C$ Declarations

      INTEGER               DIM
      INTEGER               NEQ
      DOUBLE PRECISION      B    ( DIM, NEQ )
      DOUBLE PRECISION      A    ( DIM, DIM )
      DOUBLE PRECISION      X    ( DIM, NEQ )
      LOGICAL               FOUND

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     DIM        I   Dimension of the constant and solution vectors.
C     NEQ        I   Number of equations.
C     B         I/O  Array of constant vectors of the matrix equation.
C     A         I/O  Coefficient matrix
C     X          O   Array of solution vectors.
C     FOUND      O   Found flag.
C
C$ Detailed_Input
C
C     DIM       Dimension of the constant vectors and solution vectors.
C
C     NEQ       Number of equations.
C
C     B         Array of constant vectors of the matrix equation B =
C               Ax.  The input is destroyed by the process of solving
C               the system of equations for the vector X.
C
C     A         A DIM by DIM matrix of coefficients for the system of
C               equations.  The coefficient matrix is destroyed by the
C               solution process.  Note: It is assumed that A is
C               declared as:
C
C                  DOUBLE PRECISION      A(DIM,DIM)
C
C               If this is not the case A should be packed so that
C               in it can be treated as if it had been declared
C               as above.
C
C               (That is in physical memory, data should be arranged
C               as
C
C                A(1,1) ... A(n,1), A(2,1), ... A(n,2), A(1,3) ... etc.
C
C$ Detailed_Output
C
C     X         is the solution to the system of equations
C
C                  B(i) = A * X(i), i = 1, ... , NEQ
C
C     FOUND     is a logical flag indicating whether a solution was
C               found.  FOUND will be set to .FALSE. if the coefficient
C               matrix A is singular.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the routine detects that the matrix is singular, it will
C        return with FOUND set to .FALSE.
C
C     2) This routine takes no action regarding matrices that are
C        nearly singular.  The user should read particulars and
C        determine if his/her situation warrants special consideration
C        for nearly singular matrices.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The problem of solving a system of N equations and N
C     unknowns can run into many numerical difficulties.  However, there
C     are many situations in which the system to be solved is small
C     and known to be well conditioned.  Under such situations, one
C     need not resort to the use of sophisticated black box routines
C     but may rely on techniques learned in elementary linear algebra
C     courses.
C
C     This routine is designed for accurate and stable solution of
C     systems of DIM equations in DIM variables where DIM is a small
C     integer (say less than 100). It uses the method of Gaussian
C     elimination with partial pivots. This method, while not the
C     fastest known method, is stable. Moreover it is simple and
C     easy to maintain.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     This routine does not handle problems of nearly singular matrices.
C     If you suspect your coefficient matrix is close to singular, you
C     should consider alternate methods.
C
C$ Literature_References
C
C     "Numerical Recipes --- The Art of Scientific Computing" by William
C     Press, Brian Flannery, Saul Teukolsky, William Vetterling.
C     Cambridge University Press 1986.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    TESTUTIL Version 2.0.1, 24-NOV-2021 (NJB)
C
C        Corrected spelling error.
C 
C        This routine has been moved to the TESTUTIL library.
C
C-    TSPICE Version 2.0.0, 19-OCT-2012 (NJB)
C
C        Renamed for use in TSPICE. Several misspellings in
C        comments were corrected.
C
C-    Beta Version 1.0.0, 24-MAR-2003 (WLT) (NJB)
C
C-&


C
C     SPICELIB Functions
C
      LOGICAL               RETURN
C
C     Local parameters
C

C
C     Local Variables
C
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      MAXMAG
      DOUBLE PRECISION      PART
      DOUBLE PRECISION      TEMP

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               MOVE
      INTEGER               START


C
C     Standard Error Handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     Nothing found yet.
C
      FOUND = .FALSE.

      START = 1

      DO WHILE ( START .LE. DIM )

C
C        First normalize each row of the matrix.
C
         DO I = START, DIM

            MAXMAG = 0
            MAG    = 0

            DO J = START,DIM
               IF ( DABS(A(I,J)) .GT. MAXMAG ) THEN
                  MAXMAG = DABS( A(I,J) )
               END IF
            END DO

            IF ( MAXMAG .EQ. 0 ) THEN
C
C              The found flag is already set.
C
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

C
C        Find the row with the maximum first entry.
C
         MOVE    = START
         MAXMAG  = DABS( A(START,START) )

         DO I = START+1, DIM

            IF ( DABS(A(I,START)) .GT. MAXMAG ) THEN

               MAXMAG = DABS(A(I,START))
               MOVE   = I

            END IF

         END DO

C
C        If the first row is not the one with the largest
C        component of interest swap with the one that does.
C
         IF ( MOVE .NE. START ) THEN
            DO I = START, DIM
               CALL SWAPD ( A(MOVE,I), A(START,I) )
            END DO

            DO K = 1, NEQ
               CALL SWAPD ( B(MOVE,K), B(START,K) )
            END DO
         END IF

C
C        Now normalize the START row so that it begins with a 1.
C
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

C
C     Back substitute to get the solution.
C
      DO K = 1, NEQ

         DO I = DIM, 1, -1

            PART = 0

            DO J = DIM, I+1, -1

               PART = PART  +  A(I,J) * X(J,K)

            END DO

            X(I,K) = B(I,K) - PART

         END DO

      END DO

C
C     We've got a solution if we made it this far.
C
      FOUND = .TRUE.

      RETURN
      END
