!$Procedure  ZZCNQUAD ( Solve quadratic equation for cone intercept )

      subroutine zzcnquad ( a, b, c, ub, n, r1, r2 )
      use pepper_kinds, only: ip, wp

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

      implicit none

      real(wp)              a
      real(wp)              b
      real(wp)              c
      real(wp)              ub
      integer(ip)           n
      real(wp)              r1
      real(wp)              r2

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
!     UB         is an upper bound on the absolute value of real
!                roots to be found by this routine. Roots having
!                absolute value larger than UB are not returned.
!
! $ Detailed_Output
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
      real(wp)              dpmax
      real(wp)              touchd

      logical               return

!
!     Local parameters
!
      real(wp), parameter :: small = 1.0e-8_wp

!
!     Local variables
!
      real(wp)              coeffs ( 3 )
      real(wp)              inv1
      real(wp)              inv2
      real(wp)              invub
      real(wp)              maxmag

      integer(ip)           i
      integer(ip)           maxix
      integer(ip)           nx

      logical               first

!
!     Saved values
!
      save                  first
      save                  invub

!
!     Initial values
!
      data                  first / .true. /
      data                  invub / -1.0_wp  /

      if ( return() ) return

!
!     On the first pass, set the upper bound for the reciprocal
!     solution.
!
      if ( first ) then

         invub = sqrt( dpmax() ) / 200.0_wp

         first = .false.

      end if

!
!     Handle the degenerate cases first.
!
      if ( ( a == 0.0_wp ) .and. ( b == 0.0_wp )  ) then

         r1 = 0.0_wp
         r2 = 0.0_wp

         if ( c == 0.0_wp ) then

            n = -1
         else
            n = -2
         end if

         return

      end if

!
!     Scale the input coefficients.
!
      maxmag = max( abs(a), abs(b), abs(c) )

      coeffs(1) = touchd(  a / maxmag )
      coeffs(2) = touchd(  b / maxmag )
      coeffs(3) = touchd(  c / maxmag )

!
!     Identify the coefficient of largest magnitude.
!
      maxix = 1

      do i = 2, 3

         if ( abs(coeffs(i)) > abs(coeffs(maxix)) ) then
!
!           Record the index of the maximum magnitude.
!
            maxix  = i

         end if

      end do

!
!     Make sure the value of maximum magnitude is +/- 1.
!
      coeffs(maxix) = sign( 1.0_wp, coeffs(maxix) )

!
!     Find roots in a manner suited to the coefficients we have.
!
      if (       (  abs( coeffs(1) ) >=  small  ) &
           .or.  (       coeffs(1)   ==  0.0_wp   )  ) then
!
!        This is a numerically well-behaved case. Delegate the
!        job to ZZBQUAD.
!
         call zzbquad ( coeffs(1), coeffs(2), coeffs(3), ub, &
                        n,         nx,        r1,        r2  )

      else if (  abs( coeffs(3) ) >=  small  ) then

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
!        Because the quadratic coefficient is bounded away from zero,
!        the roots of the reciprocal equation are not in danger of
!        overflowing. So we can safely solve for 1/x. We might have
!        complex roots; these are rejected.
!
!        The roots of the transformed equation don't have a maximum
!        magnitude restriction imposed by UB. We set the upper bound
!        to a value that ZZBQUAD will allow.
!
         call zzbquad ( coeffs(3), coeffs(2), coeffs(1), invub, &
                        n,         nx,        inv1,      inv2  )

         if ( n == 1 ) then
!
!           We have one real root. Make sure we can invert it.
!
            if (  abs( inv1 * ub )  >=  1.0_wp  ) then
!
!              |1/INV1| <= UB
!
               r1 = 1.0_wp / inv1

            else
!
!              There are no real roots having magnitude within the
!              bound.
!
               n  = 0

            end if

!
!           There is no second root.
!
            r2 = 0.0_wp

         else if ( n == 2 ) then
!
!           We have two real roots. The one of larger magnitude is
!           the second one. The reciprocal of this root will be
!           the smaller root of the original equation, as long
!           as the reciprocal is within bounds.
!
            if (  abs( inv2 * ub )  >=  1.0_wp  ) then
!
!              |1/INV2| <= UB
!
               r1 = 1.0_wp / inv2

!
!              Proceed to the first root of the transformed equation.
!
               if (  abs( inv1 * ub )  >=  1.0_wp  ) then
!
!                 |1/INV1| <= UB
!
                  r2 = 1.0_wp / inv1

               else
!
!                 Only the second root qualifies for inversion.
!
                  n  = 1

                  r2 = 0.0_wp

               end if

            else
!
!              The reciprocal of the larger root is too big; the
!              reciprocal of the smaller root will be even larger.
!              There are no real roots having magnitude within the
!              bound.
!
               n  = 0

               r1 = 0.0_wp
               r2 = 0.0_wp

            end if

         else
!
!           We have no viable roots of the transformed equation, so
!           we have no viable roots of the original one.
!
            n  = 0

            r1 = 0.0_wp
            r2 = 0.0_wp

         end if

      else

!
!        The linear coefficient B has the greatest magnitude, which
!        is 1. The quadratic coefficient A is "small":  0 < |A| < 1.D-8.
!        The zero-order coefficient is "small" as well.
!
!        It will be convenient to make B equal to 1; do this now.
!
         if ( b < 0.0_wp ) then

            coeffs(1) = -coeffs(1)
            coeffs(2) = -coeffs(2)
            coeffs(3) = -coeffs(3)

         end if

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
!                              2      3         4
!                =  1 + x/2 - x /8 + x /16 + O(x )
!
!        Apply this formula to that for the solution having the
!        positive square root term. Here let `x' be
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
!        Then the root is
!
!                  -1 + sqrt( 1 - 4ac )
!           x  =   --------------------
!            1             2a
!
!                                      2 2          3
!                  -1 + ( 1 - 2ac - 16a c /8 + O((ac)) )
!              =   -------------------------------------
!                                   2a
!
!        Discarding the high-order terms in a, we have
!
!           x  ~=  ( -1 + 1 - 2ac ) / 2a  =  -c
!            1
!
!        Similarly, we have
!
!           x  ~=  ( -1 - 1 + 2ac ) / 2a  =  ( ac - 1 )/a  = c - 1/a
!            2
!
!        Based on the conditions that got us here, we know
!
!           |c| < 1
!
!           |c - 1/a| ~= |1/a| > 1.e8
!

         n  = 0
         r1 = 0.0_wp
         r2 = 0.0_wp

         if ( abs( coeffs(3) ) <= ub ) then

            r1 = -coeffs(3)

            n  = 1

            if (        abs( coeffs(1)*coeffs(3) - 1.0_wp ) &
                  <  abs( coeffs(1)*ub               )  ) then

               r2 = coeffs(3) - 1.0_wp/coeffs(1)

               n  = 2

            end if

         end if

      end if

      return
      end
