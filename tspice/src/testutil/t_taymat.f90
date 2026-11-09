!$Procedure T_TAYMAT ( Taylor expansion coefficient matrix )

      SUBROUTINE T_TAYMAT ( N, XVALS, X, MBUFF )
      IMPLICIT NONE

! $ Abstract
!
!     Find the coefficient matrix required to find the Taylor expansion
!     about a specified point of a polynomial defined by Hermite-style
!     constraints.
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

      INTEGER               N
      DOUBLE PRECISION      XVALS  ( N )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      MBUFF  ( 4*N*N )

      INTEGER               MAXN
      PARAMETER           ( MAXN   = 50 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     N          I   Number of abscissa points.
!     XVALS      I   Array of abscissa points.
!     X          I   Central point of the expansion.
!     MBUFF      O   Buffer containing packed coefficient matrix.
!     MAXN       P   Parameter giving maximum grid size.
!
! $ Detailed_Input
!
!     N              is the size of a grid of abscissa points on
!                    which a set of polynomials is defined.
!
!     XVALS          is the set of abscissa points.
!
!     X              is the central point of a Taylor expansion.
!
! $ Detailed_Output
!
!     MBUFF          is a buffer into which is packed a coefficient
!                    matrix M defined by the input grid. The
!                    coefficient matrix M is
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
!                    M is packed into MBUFF in column-major order:
!                    the first 2*N elements of MBUFF contain the first
!                    column of M, the next 2*N elements contain the
!                    second column of M, and so on.
!
!
! $ Parameters
!
!     MAXN           is the maximum size of the input grid.
!
! $ Exceptions
!
!     1)  If the input grid size N exceeds MAXN/2, the error
!         SPICE(OVERFLOW) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If a Hermite polynomial H is defined by the constraints
!
!        H (x(i))  =   a(i)
!        H'(x(i))  =   b(i)
!
!     for i = 1, ..., N, and if T(y-x) is the Taylor expansion
!     of H about x, evaluated at y, then T must satisfy the
!     2*N constraints
!
!
!                      2*N-1
!                       ___
!                       \               K
!        T(x(i)-x) =    /     C (x(i)-x)      =   a(i), i = 1,...,N
!                       ---    K
!                      K = 0
!
!
!                      2*N-1
!                       ___
!                       \                  K-1
!        T'(x(i)-x) =   /    K * C (x(i)-x)    =   b(i), i = 1,...,N
!                       ---       K
!                      K = 1
!
!
!     where the coefficients
!
!        C ,  K = 0, ..., 2*N-1
!         K
!
!     are the Taylor coefficients comprising T.  The set of equations
!     above can be written as a matrix equation
!
!
!        M C = B
!
!     where M is the coefficient matrix described in the Detailed_Output
!     section above, C is the vector of Taylor coefficients, and B is
!     the constraint vector consisting of the values
!
!        a , ..., a , b , ..., b
!         1        n   1        b
!
!     The output buffer MBUFF contains the matrix M in a convenient
!     form for solving the matrix equation.
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
!        Corrected typo in description of output matrix MBUFF.
!        Corrected spelling errors.
!
!        This routine has been moved to the TESTUTIL library.
!
!-    TSPICE Version 2.0.0, 19-OCT-2012 (NJB)
!
!        Renamed for use in TSPICE. Typo in Particulars header section
!        was corrected: lower bound on summation for T' is now 1.
!
!-    SPKNIO Version 1.0.0, 24-MAR-2003 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
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

!
!     Let DEG be the Hermite polynomial degree.
!
      DEG = 2*N - 1

!
!     Fill in the top half of M.
!
      DO I = 1, N

         M(I,0) = 1.D0

         DO J = 1, DEG

            M(I,J) = ( XVALS(I) - X ) * M(I,J-1)

         END DO

      END DO

!
!     Fill in the bottom half of M.
!
      DO I = N+1, 2*N

         M(I,0) = 0.D0
         M(I,1) = 1.D0

         DO J = 2, DEG

            M(I,J) = M( I-N, J-1 )  *  J

         END DO

      END DO



!
!     Pack the significant portion of M into a buffer of
!     contiguous elements.
!
      J = 1

      DO I = 0, DEG

         CALL MOVED ( M(1,I), 2*N, MBUFF(J) )
         J = J + 2*N

      END DO

      CALL CHKOUT ( 'T_TAYMAT' )
      RETURN
      END
