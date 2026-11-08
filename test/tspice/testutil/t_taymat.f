C$Procedure T_TAYMAT ( Taylor expansion coefficient matrix )

      SUBROUTINE T_TAYMAT ( N, XVALS, X, MBUFF )
      IMPLICIT NONE

C$ Abstract
C
C     Find the coefficient matrix required to find the Taylor expansion
C     about a specified point of a polynomial defined by Hermite-style
C     constraints.
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

      INTEGER               N
      DOUBLE PRECISION      XVALS  ( N )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      MBUFF  ( 4*N*N )

      INTEGER               MAXN
      PARAMETER           ( MAXN   = 50 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of abscissa points.
C     XVALS      I   Array of abscissa points.
C     X          I   Central point of the expansion.
C     MBUFF      O   Buffer containing packed coefficient matrix.
C     MAXN       P   Parameter giving maximum grid size.
C
C$ Detailed_Input
C
C     N              is the size of a grid of abscissa points on
C                    which a set of polynomials is defined.
C
C     XVALS          is the set of abscissa points.
C
C     X              is the central point of a Taylor expansion.
C
C$ Detailed_Output
C
C     MBUFF          is a buffer into which is packed a coefficient
C                    matrix M defined by the input grid. The
C                    coefficient matrix M is
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
C                    M is packed into MBUFF in column-major order:
C                    the first 2*N elements of MBUFF contain the first
C                    column of M, the next 2*N elements contain the
C                    second column of M, and so on.
C
C
C$ Parameters
C
C     MAXN           is the maximum size of the input grid.
C
C$ Exceptions
C
C     1)  If the input grid size N exceeds MAXN/2, the error
C         SPICE(OVERFLOW) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     If a Hermite polynomial H is defined by the constraints
C
C        H (x(i))  =   a(i)
C        H'(x(i))  =   b(i)
C
C     for i = 1, ..., N, and if T(y-x) is the Taylor expansion
C     of H about x, evaluated at y, then T must satisfy the
C     2*N constraints
C
C
C                      2*N-1
C                       ___
C                       \               K
C        T(x(i)-x) =    /     C (x(i)-x)      =   a(i), i = 1,...,N
C                       ---    K
C                      K = 0
C
C
C                      2*N-1
C                       ___
C                       \                  K-1
C        T'(x(i)-x) =   /    K * C (x(i)-x)    =   b(i), i = 1,...,N
C                       ---       K
C                      K = 1
C
C
C     where the coefficients
C
C        C ,  K = 0, ..., 2*N-1
C         K
C
C     are the Taylor coefficients comprising T.  The set of equations
C     above can be written as a matrix equation
C
C
C        M C = B
C
C     where M is the coefficient matrix described in the Detailed_Output
C     section above, C is the vector of Taylor coefficients, and B is
C     the constraint vector consisting of the values
C
C        a , ..., a , b , ..., b
C         1        n   1        b
C
C     The output buffer MBUFF contains the matrix M in a convenient
C     form for solving the matrix equation.
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
C        Corrected typo in description of output matrix MBUFF.
C        Corrected spelling errors.
C 
C        This routine has been moved to the TESTUTIL library.
C
C-    TSPICE Version 2.0.0, 19-OCT-2012 (NJB)
C
C        Renamed for use in TSPICE. Typo in Particulars header section
C        was corrected: lower bound on summation for T' is now 1.
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
      DOUBLE PRECISION      M      ( 1 : MAXN, 0 : MAXN-1 )

      INTEGER               DEG
      INTEGER               I
      INTEGER               J

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_TAYMAT' )

      IF ( N .GT. MAXN/2 ) THEN

         CALL SETMSG ( 'Grid size # exceeds maximum allowed value #.' )
         CALL ERRINT ( '#',  N                                        )
         CALL ERRINT ( '#',  MAXN/2                                   )
         CALL SIGERR ( 'SPICE(OVERFLOW)'                              )
         CALL CHKOUT ( 'T_TAYMAT'                                     )
         RETURN

      END IF

C
C     Let DEG be the Hermite polynomial degree.
C
      DEG = 2*N - 1

C
C     Fill in the top half of M.
C
      DO I = 1, N

         M(I,0) = 1.D0

         DO J = 1, DEG

            M(I,J) = ( XVALS(I) - X ) * M(I,J-1)

         END DO

      END DO

C
C     Fill in the bottom half of M.
C
      DO I = N+1, 2*N

         M(I,0) = 0.D0
         M(I,1) = 1.D0

         DO J = 2, DEG

            M(I,J) = M( I-N, J-1 )  *  J

         END DO

      END DO



C
C     Pack the significant portion of M into a buffer of
C     contiguous elements.
C
      J = 1

      DO I = 0, DEG

         CALL MOVED ( M(1,I), 2*N, MBUFF(J) )
         J = J + 2*N

      END DO

      CALL CHKOUT ( 'T_TAYMAT' )
      RETURN
      END
