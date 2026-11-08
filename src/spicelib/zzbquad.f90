!$Procedure  ZZBQUAD ( Solve quadratic equation with bounds )

      subroutine zzbquad ( a, b, c, ub, n, nx, r1, r2 )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Solve a quadratic equation using an upper bound for the absolute
!     value of the roots. Only real roots are computed.
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

      double precision      a
      double precision      b
      double precision      c
      double precision      ub
      integer               n
      integer               nx
      double precision      r1
      double precision      r2

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     A,
!     B,
!     C          I   Coefficients of quadratic equation.
!     UB         I   Upper bound on magnitude of roots.
!     N          O   Number of real roots within bound.
!     NX         O   Number of real roots exceeding bound.
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
!                   If C  = 0, N is set to -1; NX is set to 0; R1 and
!                   R2 are set to zero.
!
!                   If C != 0, N is set to -2; NX is set to 0; R1 and
!                   R2 are set to zero.
!
!
!     NX        is the number of real roots having absolute values that
!               exceed the bound.
!
!               If the roots are complex, both N and NX are set to zero.
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
!         than the parameter BIG, the error SPICE(VALUEOUTOFRANGE) is
!         signaled.
!
!     2)  If UB is non-positive or larger than the parameter BIG, the
!         error SPICE(VALUEOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See ZZCNQUAD for a more robust implementation that makes use
!     of this one.
!
! $ Examples
!
!     See usage in ZZCNQUAD
!
! $ Restrictions
!
!     1)  This routine may suffer from loss of precision for coefficient
!         sets having small, non-zero leading coefficients. See ZZCNQUAD
!         for a more robust implementation.
!
!     2)  This is a private routine; it should not be called by
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
!-    SPICELIB Version 1.0.0, 21-JUN-2016 (NJB)
!
!        Made minor updates to header comments.
!
!        24-SEP-2014 (NJB)
!
!           Original version.
!
!-&

! $ Index_Entries
!
!     solve quadratic equation with bounds on roots
!
!-&

! $ Revisions
!
!     None.
!
!-&

!
!     SPICELIB functions
!
      double precision      dpmax
      double precision      touchd

      logical               return

!
!     Local variables
!
      double precision      big
      double precision      denom
      double precision      dscrim
      double precision      num1
      double precision      num2
      double precision      sqdisc

      logical               first

!
!     Saved variables
!
      save                  big
      save                  first

!
!     Initial values
!
      data                  big   / 0.d0   /
      data                  first / .true. /

!
!     Use discovery check-in.
!
      if ( return() ) then
         return
      end if

      if ( first ) then

         big   = sqrt( dpmax() ) / 100
         first = .false.

      end if

!
!     Set invalid counts to start out. Initialize R1 and R2.
!
      n   = -3
      nx  = -3
      r1  = 0.d0
      r2  = 0.d0

!
!     Reject all large magnitude coefficients.
!
      if (     ( abs(a) .gt. big ) &
          .or. ( abs(b) .gt. big ) &
          .or. ( abs(c) .gt. big ) ) then

         call chkin  ( 'ZZBQUAD'                                      )
         call setmsg ( 'Coefficients must have magnitude less than ' &
         //            'or equal to #, but were A = #; B = #; C = #.' )
         call errdp  ( '#',  big                                      )
         call errdp  ( '#',  a                                        )
         call errdp  ( '#',  b                                        )
         call errdp  ( '#',  c                                        )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         call chkout ( 'ZZBQUAD'                                      )
         return

      end if

!
!     Reject large magnitude upper bounds as well.
!
      if ( abs(ub) .gt. big ) then

         call chkin  ( 'ZZBQUAD'                                      )
         call setmsg ( 'Upper bounds must have magnitude less than ' &
         //            'or equal to #, but was #.'                    )
         call errdp  ( '#',  big                                      )
         call errdp  ( '#',  ub                                       )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         call chkout ( 'ZZBQUAD'                                      )
         return

      end if

!
!     The upper bound must be positive.
!
      if ( ub .le. 0.d0 ) then

         call chkin  ( 'ZZBQUAD'                                 )
         call setmsg ( 'Upper bound must be positive but was #.' )
         call errdp  ( '#',  ub                                  )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                  )
         call chkout ( 'ZZBQUAD'                                 )
         return

      end if

!
!     Handle the degenerate cases first.
!
      if ( a .eq. 0.d0 ) then

         if ( b .eq. 0.d0 ) then
!
!           The equation is of the form
!
!              C = 0
!
            if ( c .eq. 0.d0 ) then
!
!              The equation is satisfied for all real numbers.
!
               n  = -1
               nx =  0

            else
!
!              There are no solutions.
!
               n  = -2
               nx =  0

            end if

         else
!
!           The equation is first-order:
!
!              B*X + C = 0
!
!           In this branch, B is non-zero.
!
            if ( abs(c) .le. abs(ub * b) ) then

               n   =   1
               nx  =   0

               r1  =  -c / b
               r2  =   r1

            else
!
!              The magnitude of the solution is too large.
!
               n   =  0
               nx  =  1

            end if

         end if


      else
!
!        The leading coefficient of the equation is non-zero.
!
!        We can safely compute the discriminant now, due the
!        check we've already performed.
!
         dscrim = touchd(  ( b*b ) - ( 4*a*c )  )

         if ( dscrim .lt. 0.d0 ) then
!
!           We have complex roots, so we're done.
!
            n  = 0
            nx = 0

         else if ( dscrim .eq. 0.d0 ) then
!
!           We have a single real root of multiplicity 2.
!
!           Compare the magnitude of the root to the upper bound.
!
            num1   =  -b
            denom  =   2 * a

            if ( abs(num1) .ge. abs(denom * ub) ) then
!
!              The root is too large; we won't compute it.
!
               n   =  0
               nx  =  1

            else
!
!              Set both roots to the same value. In this branch,
!              A is non-zero.
!
               n   =  1
               nx  =  0

               r1  =  ( num1 / a ) / 2
               r2  =  r1

            end if


         else
!
!           We have two nominally distinct real roots. Whether
!           they're distinct double precision numbers depends
!           on the relative magnitudes of A and DSCRIM.
!
            denom  = 2 * a
            sqdisc = sqrt( dscrim )

            if ( b .gt. 0.d0 ) then

               num2 =  -b - sqdisc
               num1 =  -b + sqdisc
            else
               num2 =  -b + sqdisc
               num1 =  -b - sqdisc
            end if

!
!           See whether the root of larger magnitude is computable.
!
            if ( abs(num2) .le. abs(ub * denom) ) then
!
!              The root is computable.

               n  = 2
               nx = 0
!
!              In this branch, A is non-zero.
!
               r2 = ( num2 / a ) / 2

               if ( abs(r2) .gt. 0.d0 ) then
!
!                 Compute R1 using R2 and C; this avoids loss
!                 of precision that may occur when NUM1 is computed.
!
!                 We know R1 has smaller magnitude than R2 and R2
!                 is computable, and we know A is non-zero, so R1
!                 can be computed without a divide-by-zero error,
!                 and it is computable as long as no intermediate
!                 results overflow. The bounds on A and R2 ensure
!                 that A*R2 is computable.
!
                  r1 =  c / ( a * r2 )

               else
!
!                 The root of larger magnitude has magnitude 0. This
!                 doesn't leave many possible values for the root of
!                 smaller magnitude.
!
                  r1 = 0.d0

               end if


            else
!
!              The root of larger magnitude is not computable.
!              Check the root of smaller magnitude.
!
               if ( abs(num1) .le. abs(ub * denom) ) then
!
!                 The root is computable.
!
                  n  = 1
                  nx = 1

                  r1 = ( num1 / a ) / 2

               else
!
!                 Neither root is computable.
!
                  n  = 0
                  nx = 2

               end if

            end if

         end if

      end if

      return
      end
