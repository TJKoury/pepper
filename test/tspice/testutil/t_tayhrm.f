C$Procedure      T_TAYHRM ( Taylor expansions of Hermite polynomials )

      SUBROUTINE T_TAYHRM ( N,  NEQ,  MBUFF,  RSYS, DERIVS, FOUND )

C$ Abstract
C
C     Find the Taylor expansions about a specified point of a set of
C     polynomials defined by Hermite-style constraints.  Return the
C     derivatives of the expansions.
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
C     SPKNIO
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               N
      INTEGER               NEQ
      DOUBLE PRECISION      MBUFF  ( 4 * N * N )
      DOUBLE PRECISION      RSYS   ( 2 * N,       NEQ )
      DOUBLE PRECISION      DERIVS ( 2 * N,       NEQ )
      LOGICAL               FOUND

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of abscissa points.
C     NEQ        I   Number of equations.
C     MBUFF      I   Buffer containing matrix of Hermite constraints.
C     RSYS       I   Array of RHS vectors of equations.
C     DERIVS     O   Array of derivatives of Taylor expansions.
C     FOUND      O   Flag indicating whether solution was found.
C
C$ Detailed_Input
C
C     N              is the size of a grid of abscissa points on
C                    which a set of polynomials is defined.
C
C     NEQ            is the number of matrix equations defining the set
C                    of polynomials.  There is one matrix equation
C                    for each polynomial.
C
C     MBUFF          is a buffer into which are packed the columns of a
C                    constraint matrix M. Let XVALs be a set of
C                    abscissa points, and let X be the central point of
C                    a Taylor expansion. Then the matrix M has the form
C
C  +-                                                                 -+
C  | 1 [XVALS(1)-X]   [XVALS(1)-X]**2 ...         [XVALS(1)-X]**(2*N-1)|
C  | 1 [XVALS(2)-X]   [XVALS(2)-X]**2 ...         [XVALS(2)-X]**(2*N-1)|
C  |                          .                                        |
C  |                          .                                        |
C  |                          .                                        |
C  | 1 [XVALS(N)-X]   [XVALS(N)-X]**2 ...         [XVALS(N)-X]**(2*N-1)|
C  | 0      1       2*[XVALS(1)-X]**1 ... (2*N-1)*[XVALS(1)-X]**(2*N-2)|
C  | 0      1       2*[XVALS(2)-X]**1 ... (2*N-1)*[XVALS(2)-X]**(2*N-2)|
C  |                          .                                        |
C  |                          .                                        |
C  |                          .                                        |
C  | 0      1       2*[XVALS(N)-X]**1 ... (2*N-1)*[XVALS(N)-X]**(2*N-2)|
C  +-                                                                 -+
C
C                    The matrix M is packed into MBUFF in column-major
C                    order: the first 2*N elements of MBUFF contain the
C                    first column of the matrix, the next 2*N elements
C                    contain the second column of the matrix, and so
C                    on.
C
C     RSYS           is an array of right-hand-sides of the NEQ matrix
C                    equations that this routine is meant to solve. Let
C                    M be the coefficient matrix described above and
C                    let C(*,i) be the ith array of Taylor coefficients
C                    which constitutes the solution of the ith
C                    equation. The ith column of RSYS is the RHS of the
C                    ith equation:
C
C                       M C(*,i) = RSYS(*,i),  i = 1, ..., NEQ
C
C$ Detailed_Output
C
C     DERIVS         is an array containing the derivatives of the
C                    polynomial expansions defined by MBUFF and RSYS,
C                    all evaluated at X.  DERIVS(I,J) contains the
C                    (I-1)st derivative of the Jth expansion.
C
C     FOUND          is a logical flag which has the value .TRUE. if
C                    DERIVS was computed and is .FALSE. otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If a solution cannot be found due to singularity of
C        the input matrix, the FOUND flag is set to .FALSE.
C        This case is not considered an error.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See the routine TAYMAT for a description of the type of
C     matrix equation this routine solves.  Since this routine is
C     typically used to find derivatives of a position vector, there
C     is normally one equation for each position component.
C
C$ Examples
C
C     See the SPKNIO routine T13MDA.
C
C$ Restrictions
C
C     1) This routine is for use only within the program TSPICE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 2.0.1, 24-NOV-2021 (NJB)
C
C        Corrected typo in description of input matrix MBUFF.
C        This routine has been moved to the TESTUTIL library.
C
C-    TSPICE Version 2.0.0, 19-OCT-2012 (NJB)
C
C        Renamed for use in TSPICE. Inputs XVALS and X, which were
C        included only for consistency with the routine TAYMAT, were
C        deleted. Unused local variable DEG was deleted.
C
C-    SPKNIO Version 1.0.0, 24-MAR-2003 (NJB)
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      FACT

      INTEGER               I
      INTEGER               J


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_TAYHRM' )

C
C     Solve the systems of equations
C
C
C        M * COEFFS = RSYS(1,*)
C
C     where M is the coefficient matrix packed into MBUFF.
C

      CALL T_SOLVEG_2 ( RSYS, 2*N, NEQ, MBUFF, DERIVS, FOUND )

      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'T_TAYHRM' )
         RETURN
      END IF

C
C     Compute the derivatives of the Taylor expansion at X.
C
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
