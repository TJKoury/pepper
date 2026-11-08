C$Procedure T_CHBIGR ( Test utility: integrate Chebyshev expansion  )

      SUBROUTINE T_CHBIGR ( NDIM, DEGP, CP, X2S, X, WORK, P, ITGRLP )

C$ Abstract
C
C     This is a test utility that provides an alternate method of
C     computation for Chebyshev expansion integration.
C
C     Evaluate an indefinite integral of a set of Chebyshev expansions
C     at a specified point. The constants of integration are selected
C     to make each integral zero at the abscissa value X2S(1). Return
C     the values of the input expansions as well.
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
C     CHEBYSHEV
C     EPHEMERIS
C     MATH
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               NDIM
      INTEGER               DEGP
      DOUBLE PRECISION      CP     ( DEGP+1, NDIM )
      DOUBLE PRECISION      X2S    ( 2 )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      WORK   ( DEGP+2, 2 )
      DOUBLE PRECISION      P      ( NDIM )
      DOUBLE PRECISION      ITGRLP ( NDIM )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NDIM       I   Dimension of input vector-valued function.
C     DEGP       I   Degree of input Chebyshev expansions.
C     CP         I   Chebyshev coefficients of input expansions.
C     X2S        I   Transformation parameters.
C     X          I   Abscissa value of evaluation.
C     WORK      I-O  Workspace.
C     P          O   Vector-valued function at X.
C     ITGRLP     O   Vector-valued integral at X.
C
C$ Detailed_Input
C
C     NDIM     is the number of input expansions.
C
C     DEGP     is the degree of the input Chebyshev expansion.
C
C     CP       is an array containing the coefficients of the input
C              Chebyshev expansions. For the Jth dimension, the
C              coefficient of the Ith Chebyshev polynomial is
C              contained in element CP(I-1,J), for I = 1 : DEGP+1.
C
C     X2S      is an array containing the "transformation parameters"
C              of the domain of the expansion. Element X2S(1)
C              contains the midpoint of the interval on which the
C              input is defined; X2S(2) is one-half of the length of
C              this interval; this value is called the interval's
C              "radius."
C
C              The Jth input expansion defines a function f(X) on the
C              interval
C
C                 [ X2S(1)-X2S(2),  X2S(1)+X2S(2) ]
C
C              as follows:
C
C                 Define S = ( X - X2S(1) ) / X2S(2)
C
C
C                              DEGP+1
C                              __
C                              \
C                 f(X) = g(S)  = /  CP(k,J)  T   (S)
C                              --         k-1
C                              k=1
C
C
C     X        is the abscissa value at which the functions defined by
C              the input expansions and their integrals are to be
C              evaluated. Normally X should lie in the closed
C              interval
C
C                 [ X2S(1)-X2S(2),  X2S(1)+X2S(2) ]
C
C              See the Restrictions section below.
C
C
C     WORK     is a workspace array. WORK should be declared
C              with dimension (DEGP+2,2).
C
C$ Detailed_Output
C
C     WORK     is the workspace array, modified by this routine.
C
C     P        is an array containing the results of evaluating at X
C              the functions represented by the input expansions. The
C              Jth element of P is the expansion CP(*,J) evaluated at
C              S, where.
C
C                 S = ( X - X2S(1) ) / X2S(2)
C
C     ITGRLP   is an array containing the results of evaluating at X
C              the functions represented by the indefinite integrals
C              of the input expansions.
C
C              Constants of integration are chosen so that each
C              integral is zero at X = X2S(1).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides an alternate algorithm for computing
C     the results produced by the SPICELIB routine CHBIGR.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-APR-2020 (JDR)
C
C        Updated header to comply with NAIF standard.
C
C-    Testing Utilities Version 1.0.0, 23-NOV-2013 (NJB)
C
C-&


C$ Index_Entries
C
C     integral of chebyshev_polynomial_expansion
C     integrate chebyshev_polynomial_expansion
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
C
C     Local variables
C
      DOUBLE PRECISION      MIDPNT
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      X0

      INTEGER               DIX
      INTEGER               I
      INTEGER               N
      INTEGER               NTERMS

      IF ( RETURN() ) THEN
         RETURN
      END IF



      NTERMS = DEGP + 1

C
C     Transform the independent variable X to the interval
C
C        [-1, 1]
C
C     Call the result X0.
C

      MIDPNT = X2S(1)
      RADIUS = X2S(2)

      X0     = ( X - MIDPNT ) / RADIUS

C
C     Calculate the values of the Chebyshev polynomials
C
C        T  ... T
C         0      DEGP
C
C     at X0. Store these values in WORK(*,1). Also evaluate these
C     polynomials at 0 and store the values in WORK(*,2).
C

      WORK(1,1) = 1.D0
      WORK(2,1) = X0

      WORK(1,2) = 1.D0
      WORK(2,2) = 0.D0


      DO I = 3, NTERMS+1

C
C        Compute
C
C           T   ( X0 )
C            I-1
C
C        and
C
C           T
C            I-1 ( 0 )
C
C
         WORK( I, 1 ) =  - WORK( I-2, 1 ) + ( 2 * X0 * WORK( I-1, 1 ) )

         WORK( I, 2 ) =  - WORK( I-2, 2 )

      END DO

C
C     Compute the definite integral for each expansion. Evaluate each
C     expansion as well.
C
      DO DIX = 1, NDIM

         ITGRLP( DIX ) = 0.D0
         P     ( DIX ) = 0.D0
C
C        The integrals of the remaining terms are obtained from the
C        formula
C
C                                     1             1
C           [I (T )](x) =  (1/2) * ( --- T   (x) - --- T   (x) )
C                n                   n+1  n+1      n-1  n-1
C
C                                    1              1
C                        - (1/2) * ( --- T   (0) - --- T   (0) )
C                                    n+1  n+1      n-1  n-1
C
C
         DO I = NTERMS, 3, -1
C
C           Add on the next term of the expansion.
C
            P( DIX ) = P( DIX ) + CP(I,DIX) * WORK(I,1)

C
C           Add on the next term of the integral.
C
            N = I - 1

            ITGRLP( DIX ) =       ITGRLP( DIX )
     .                      + ( CP(I,DIX) *      (
     .                      + (   (  0.5D0 / (N+1)     )
     .                          * ( WORK( N+2, 1 ) - WORK( N+2, 2 ) )  )
     .                      - (   (  0.5D0 / (N-1)     )
     .                          * ( WORK( N,   1 ) - WORK( N,   2 ) )  )
     .                      ) )
         END DO

         IF ( NTERMS .GE. 2 ) THEN
C
C           Add on the first two terms of the expansion.
C
            P( DIX ) = P( DIX ) + CP(1,DIX) + CP(2,DIX) * X0

C
C           Add together the first two terms of the integral.
C
            ITGRLP( DIX ) =   ITGRLP( DIX )
     .                      + X0 *  ( CP(1,DIX) + ( CP(2,DIX) * X0/2 ) )
         ELSE
C
C           The integrand is constant.
C
            P     ( DIX ) = P( DIX ) + CP(1,DIX)

            ITGRLP( DIX ) = ITGRLP( DIX )  +  ( X0 * CP(1,DIX) )

         END IF

      END DO

C
C     Scale the integral to account for the change of variables
C     (from the original domain to [1-,1]).
C
      DO I = 1, NDIM
         ITGRLP(I) = RADIUS * ITGRLP(I)
      END DO

      RETURN
      END

