!$Procedure  ZZCNQUAD ( Solve quadratic equation for cone intercept )

      SUBROUTINE ZZCNQUAD ( A, B, C, UB, N, R1, R2 )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Solve a quadratic equation using an upper bound for the absolute
!     value of the roots. Only real roots are computed. This routine
!     addresses the case of a small, non-zero leading coefficient.
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
!     EQUATION
!     MATH
!     QUADRATIC
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      UB
      INTEGER               N
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     A,
!     B,
!     C          I   Coefficients of quadratic equation.
!     UB         I   Upper bound on magnitude of roots.
!     N          O   Number of real roots within bound.
!     R1         O   Root of smaller absolute value.
!     R2         O   Root of larger absolute value.
!     BIG        P   Limit on magnitude of coefficients and UB.
!
! $ Detailed_Input
!
!     A,
!     B,
!     C          are the real coefficients of a quadratic equation.
!                The equation is
!
!                      2
!                   A X   +  B X  +  C  =  0
!
!
!     UB         is an upper bound on the absolute value of real
!                roots to be found by this routine. Roots having
!                absolute value larger than UB are not returned.
!
! $ Detailed_Output
!
!
!     N          is the number of real roots found that satisfy the
!                magnitude bound constraint.
!
!                Degenerate cases: if A = B = 0, then
!
!                   If C  = 0, N is set to -1; R1 and R2 are set to
!                   zero.
!
!                   If C != 0, N is set to -2; R1 and R2 are set to
!                   zero.
!
!     R1,
!     R2        are roots returned in increasing order of absolute
!               value. If there is one real root of multiplicity 2,
!               both R1 and R2 are set to that root, provided that
!               the absolute value of the root does not exceed UB.
!
!               If the absolute value of a root exceeds UB the
!               corresponding output argument is set to zero. If the
!               roots are complex, both R1 and R2 are set to zero.
!
! $ Parameters
!
!     BIG            is a limit on the absolute value of the input
!                    coefficients and on the magnitude limit for the
!                    roots. BIG is set to
!
!                       SQRT( DPMAX() ) / 100
!
! $ Exceptions
!
!     1)  If any of the input coefficients have absolute value larger
!         than the parameter BIG, the error is diagnosed by a routine
!         in the call tree of this routine.
!
!     2)  If UB is non-positive or larger than the parameter BIG, the
!         error is diagnosed by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports INCNSG.
!
!     This routine makes use of ZZBQUAD; it calls ZZBQUAD for
!     cases that can be handled accurately by that routine.
!     Other cases are handled in-line.
!
! $ Examples
!
!     See usage in INCNSG.
!
! $ Restrictions
!
!     1)  This is a private routine; it should not be called by
!         non-SPICELIB code.
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
!-    SPICELIB Version 1.0.0, 23-JAN-2017 (NJB)
!
!        Previous version 22-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     solve quadratic equation for cone intercept
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      TOUCHD

      LOGICAL               RETURN

!
!     Local parameters
!
      DOUBLE PRECISION      SMALL
      PARAMETER           ( SMALL = 1.D-8 )

!
!     Local variables
!
      DOUBLE PRECISION      COEFFS ( 3 )
      DOUBLE PRECISION      INV1
      DOUBLE PRECISION      INV2
      DOUBLE PRECISION      INVUB
      DOUBLE PRECISION      MAXMAG

      INTEGER               I
      INTEGER               MAXIX
      INTEGER               NX

      LOGICAL               FIRST

!
!     Saved values
!
      SAVE                  FIRST
      SAVE                  INVUB

!
!     Initial values
!
      DATA                  FIRST / .TRUE. /
      DATA                  INVUB / -1.D0  /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZCNQUAD' )

!
!     On the first pass, set the upper bound for the reciprocal
!     solution.
!
      IF ( FIRST ) THEN

         INVUB = SQRT( DPMAX() ) / 200.D0

         FIRST = .FALSE.

      END IF

!
!     Handle the degenerate cases first.
!
      IF ( ( A .EQ. 0.D0 ) .AND. ( B .EQ. 0.D0 )  ) THEN

         R1 = 0.D0
         R2 = 0.D0

         IF ( C .EQ. 0.D0 ) THEN

            N = -1
         ELSE
            N = -2
         END IF

         CALL CHKOUT ( 'ZZCNQUAD' )
         RETURN

      END IF

!
!     Scale the input coefficients.
!
      MAXMAG = MAX( ABS(A), ABS(B), ABS(C) )

      COEFFS(1) = TOUCHD(  A / MAXMAG )
      COEFFS(2) = TOUCHD(  B / MAXMAG )
      COEFFS(3) = TOUCHD(  C / MAXMAG )

!
!     Identify the coefficient of largest magnitude.
!
      MAXIX = 1

      DO I = 2, 3

         IF ( ABS(COEFFS(I)) .GT. ABS(COEFFS(MAXIX)) ) THEN
!
!           Record the index of the maximum magnitude.
!
            MAXIX  = I

         END IF

      END DO

!
!     Make sure the value of maximum magnitude is +/- 1.
!
      COEFFS(MAXIX) = SIGN( 1.D0, COEFFS(MAXIX) )

!
!     Find roots in a manner suited to the coefficients we have.
!
      IF (       (  ABS( COEFFS(1) ) .GE.  SMALL  ) &
           .OR.  (       COEFFS(1)   .EQ.  0.D0   )  ) THEN
!
!        This is a numerically well-behaved case. Delegate the
!        job to ZZBQUAD.
!
         CALL ZZBQUAD ( COEFFS(1), COEFFS(2), COEFFS(3), UB, &
                        N,         NX,        R1,        R2  )



      ELSE IF (  ABS( COEFFS(3) ) .GE.  SMALL  ) THEN

!
!        The zero-order coefficient has magnitude >= SMALL.
!
!        The original equation
!
!              2
!           a x  + b x + c = 0
!
!        can be replaced by
!
!              2
!           c y  + b y + a = 0
!
!        where
!
!           y = 1/x
!
!        Here
!
!          |c| >= SMALL
!          |c| <= 1
!
!          |a|  < SMALL
!
!
!        Because the quadratic coefficient is bounded away from zero,
!        the roots of the reciprocal equation are not in danger of
!        overflowing. So we can safely solve for 1/x. We might have
!        complex roots; these are rejected.
!
!        The roots of the transformed equation don't have a maximum
!        magnitude restriction imposed by UB. We set the upper bound
!        to a value that ZZBQUAD will allow.
!
         CALL ZZBQUAD ( COEFFS(3), COEFFS(2), COEFFS(1), INVUB, &
                        N,         NX,        INV1,      INV2  )

         IF ( N .EQ. 1 ) THEN
!
!           We have one real root. Make sure we can invert it.
!
            IF (  ABS( INV1 * UB )  .GE.  1.D0  ) THEN
!
!
!              |1/INV1| <= UB
!
!
               R1 = 1.D0 / INV1

            ELSE
!
!              There are no real roots having magnitude within the
!              bound.
!
               N  = 0

            END IF

!
!           There is no second root.
!
            R2 = 0.D0

         ELSE IF ( N .EQ. 2 ) THEN
!
!           We have two real roots. The one of larger magnitude is
!           the second one. The reciprocal of this root will be
!           the smaller root of the original equation, as long
!           as the reciprocal is within bounds.
!
            IF (  ABS( INV2 * UB )  .GE.  1.D0  ) THEN
!
!
!              |1/INV2| <= UB
!
!
               R1 = 1.D0 / INV2

!
!              Proceed to the first root of the transformed equation.
!
               IF (  ABS( INV1 * UB )  .GE.  1.D0  ) THEN
!
!
!                 |1/INV1| <= UB
!
!
                  R2 = 1.D0 / INV1

               ELSE
!
!                 Only the second root qualifies for inversion.
!
                  N  = 1

                  R2 = 0.D0

               END IF


            ELSE
!
!              The reciprocal of the larger root is too big; the
!              reciprocal of the smaller root will be even larger.
!              There are no real roots having magnitude within the
!              bound.
!
               N  = 0

               R1 = 0.D0
               R2 = 0.D0

            END IF


         ELSE
!
!           We have no viable roots of the transformed equation, so
!           we have no viable roots of the original one.
!
            N  = 0

            R1 = 0.D0
            R2 = 0.D0

         END IF



      ELSE

!
!        The linear coefficient B has the greatest magnitude, which
!        is 1. The quadratic coefficient A is "small":  0 < |A| < 1.D-8.
!        The zero-order coefficient is "small" as well.
!
!        It will be convenient to make B equal to 1; do this now.
!
         IF ( B .LT. 0.D0 ) THEN

            COEFFS(1) = -COEFFS(1)
            COEFFS(2) = -COEFFS(2)
            COEFFS(3) = -COEFFS(3)

         END IF

!
!        In this case we use a low-order Taylor expansion about
!        x = 0 for the square root term of the formula for the roots:
!
!                                  inf
!                                  __
!                           1/2    \    (k)     k
!           T(x) = ( 1 + x )    =  /_  f   (0) x / (k!)
!
!                                  k=0
!
!
!                              2      3         4
!                =  1 + x/2 - x /8 + x /16 + O(x )
!
!
!        Apply this formula to that for the solution having the
!        positive square root term. Here let `x' be
!
!
!                   2
!           -4ac / b
!
!        which equals
!
!           -4ac
!
!        since we've set b = 1.
!
!
!        Then the root is
!
!
!                  -1 + sqrt( 1 - 4ac )
!           x  =   --------------------
!            1             2a
!
!
!                                      2 2          3
!                  -1 + ( 1 - 2ac - 16a c /8 + O((ac)) )
!              =   -------------------------------------
!                                   2a
!
!        Discarding the high-order terms in a, we have
!
!
!           x  ~=  ( -1 + 1 - 2ac ) / 2a  =  -c
!            1
!
!        Similarly, we have
!
!
!           x  ~=  ( -1 - 1 + 2ac ) / 2a  =  ( ac - 1 )/a  = c - 1/a
!            2
!
!
!        Based on the conditions that got us here, we know
!
!           |c| < 1
!
!           |c - 1/a| ~= |1/a| > 1.e8
!

         N  = 0
         R1 = 0.D0
         R2 = 0.D0

         IF ( ABS( COEFFS(3) ) .LE. UB ) THEN

            R1 = -COEFFS(3)

            N  = 1

            IF (        ABS( COEFFS(1)*COEFFS(3) - 1.D0 ) &
                  .LT.  ABS( COEFFS(1)*UB               )  ) THEN

               R2 = COEFFS(3) - 1.D0/COEFFS(1)

               N  = 2

            END IF

         END IF

      END IF

      CALL CHKOUT ( 'ZZCNQUAD' )
      RETURN
      END
