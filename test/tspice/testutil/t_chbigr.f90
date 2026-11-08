!$Procedure T_CHBIGR ( Test utility: integrate Chebyshev expansion  )

      SUBROUTINE T_CHBIGR ( NDIM, DEGP, CP, X2S, X, WORK, P, ITGRLP )

! $ Abstract
!
!     This is a test utility that provides an alternate method of
!     computation for Chebyshev expansion integration.
!
!     Evaluate an indefinite integral of a set of Chebyshev expansions
!     at a specified point. The constants of integration are selected
!     to make each integral zero at the abscissa value X2S(1). Return
!     the values of the input expansions as well.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHEBYSHEV
!     EPHEMERIS
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               NDIM
      INTEGER               DEGP
      DOUBLE PRECISION      CP     ( DEGP+1, NDIM )
      DOUBLE PRECISION      X2S    ( 2 )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      WORK   ( DEGP+2, 2 )
      DOUBLE PRECISION      P      ( NDIM )
      DOUBLE PRECISION      ITGRLP ( NDIM )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NDIM       I   Dimension of input vector-valued function.
!     DEGP       I   Degree of input Chebyshev expansions.
!     CP         I   Chebyshev coefficients of input expansions.
!     X2S        I   Transformation parameters.
!     X          I   Abscissa value of evaluation.
!     WORK      I-O  Workspace.
!     P          O   Vector-valued function at X.
!     ITGRLP     O   Vector-valued integral at X.
!
! $ Detailed_Input
!
!     NDIM     is the number of input expansions.
!
!     DEGP     is the degree of the input Chebyshev expansion.
!
!     CP       is an array containing the coefficients of the input
!              Chebyshev expansions. For the Jth dimension, the
!              coefficient of the Ith Chebyshev polynomial is
!              contained in element CP(I-1,J), for I = 1 : DEGP+1.
!
!     X2S      is an array containing the "transformation parameters"
!              of the domain of the expansion. Element X2S(1)
!              contains the midpoint of the interval on which the
!              input is defined; X2S(2) is one-half of the length of
!              this interval; this value is called the interval's
!              "radius."
!
!              The Jth input expansion defines a function f(X) on the
!              interval
!
!                 [ X2S(1)-X2S(2),  X2S(1)+X2S(2) ]
!
!              as follows:
!
!                 Define S = ( X - X2S(1) ) / X2S(2)
!
!
!                              DEGP+1
!                              __
!                              \
!                 f(X) = g(S)  = /  CP(k,J)  T   (S)
!                              --         k-1
!                              k=1
!
!
!     X        is the abscissa value at which the functions defined by
!              the input expansions and their integrals are to be
!              evaluated. Normally X should lie in the closed
!              interval
!
!                 [ X2S(1)-X2S(2),  X2S(1)+X2S(2) ]
!
!              See the Restrictions section below.
!
!
!     WORK     is a workspace array. WORK should be declared
!              with dimension (DEGP+2,2).
!
! $ Detailed_Output
!
!     WORK     is the workspace array, modified by this routine.
!
!     P        is an array containing the results of evaluating at X
!              the functions represented by the input expansions. The
!              Jth element of P is the expansion CP(*,J) evaluated at
!              S, where.
!
!                 S = ( X - X2S(1) ) / X2S(2)
!
!     ITGRLP   is an array containing the results of evaluating at X
!              the functions represented by the indefinite integrals
!              of the input expansions.
!
!              Constants of integration are chosen so that each
!              integral is zero at X = X2S(1).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides an alternate algorithm for computing
!     the results produced by the SPICELIB routine CHBIGR.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-APR-2020 (JDR)
!
!        Updated header to comply with NAIF standard.
!
!-    Testing Utilities Version 1.0.0, 23-NOV-2013 (NJB)
!
!-&


! $ Index_Entries
!
!     integral of chebyshev_polynomial_expansion
!     integrate chebyshev_polynomial_expansion
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
!
!     Local variables
!
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

!
!     Transform the independent variable X to the interval
!
!        [-1, 1]
!
!     Call the result X0.
!

      MIDPNT = X2S(1)
      RADIUS = X2S(2)

      X0     = ( X - MIDPNT ) / RADIUS

!
!     Calculate the values of the Chebyshev polynomials
!
!        T  ... T
!         0      DEGP
!
!     at X0. Store these values in WORK(*,1). Also evaluate these
!     polynomials at 0 and store the values in WORK(*,2).
!

      WORK(1,1) = 1.D0
      WORK(2,1) = X0

      WORK(1,2) = 1.D0
      WORK(2,2) = 0.D0


      DO I = 3, NTERMS+1

!
!        Compute
!
!           T   ( X0 )
!            I-1
!
!        and
!
!           T
!            I-1 ( 0 )
!
!
         WORK( I, 1 ) =  - WORK( I-2, 1 ) + ( 2 * X0 * WORK( I-1, 1 ) )

         WORK( I, 2 ) =  - WORK( I-2, 2 )

      END DO

!
!     Compute the definite integral for each expansion. Evaluate each
!     expansion as well.
!
      DO DIX = 1, NDIM

         ITGRLP( DIX ) = 0.D0
         P     ( DIX ) = 0.D0
!
!        The integrals of the remaining terms are obtained from the
!        formula
!
!                                     1             1
!           [I (T )](x) =  (1/2) * ( --- T   (x) - --- T   (x) )
!                n                   n+1  n+1      n-1  n-1
!
!                                    1              1
!                        - (1/2) * ( --- T   (0) - --- T   (0) )
!                                    n+1  n+1      n-1  n-1
!
!
         DO I = NTERMS, 3, -1
!
!           Add on the next term of the expansion.
!
            P( DIX ) = P( DIX ) + CP(I,DIX) * WORK(I,1)

!
!           Add on the next term of the integral.
!
            N = I - 1

            ITGRLP( DIX ) =       ITGRLP( DIX ) &
                            + ( CP(I,DIX) *      ( &
                            + (   (  0.5D0 / (N+1)     ) &
                                * ( WORK( N+2, 1 ) - WORK( N+2, 2 ) )  ) &
                            - (   (  0.5D0 / (N-1)     ) &
                                * ( WORK( N,   1 ) - WORK( N,   2 ) )  ) &
                            ) )
         END DO

         IF ( NTERMS .GE. 2 ) THEN
!
!           Add on the first two terms of the expansion.
!
            P( DIX ) = P( DIX ) + CP(1,DIX) + CP(2,DIX) * X0

!
!           Add together the first two terms of the integral.
!
            ITGRLP( DIX ) =   ITGRLP( DIX ) &
                            + X0 *  ( CP(1,DIX) + ( CP(2,DIX) * X0/2 ) )
         ELSE
!
!           The integrand is constant.
!
            P     ( DIX ) = P( DIX ) + CP(1,DIX)

            ITGRLP( DIX ) = ITGRLP( DIX )  +  ( X0 * CP(1,DIX) )

         END IF

      END DO

!
!     Scale the integral to account for the change of variables
!     (from the original domain to [1-,1]).
!
      DO I = 1, NDIM
         ITGRLP(I) = RADIUS * ITGRLP(I)
      END DO

      RETURN
      END

