!$Procedure  ZZBQUAD ( Solve quadratic equation with bounds )

      SUBROUTINE ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )

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

      IMPLICIT NONE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      UB
      INTEGER               N
      INTEGER               NX
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
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      TOUCHD

      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      BIG
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      DSCRIM
      DOUBLE PRECISION      NUM1
      DOUBLE PRECISION      NUM2
      DOUBLE PRECISION      SQDISC

      LOGICAL               FIRST

!
!     Saved variables
!
      SAVE                  BIG
      SAVE                  FIRST

!
!     Initial values
!
      DATA                  BIG   / 0.D0   /
      DATA                  FIRST / .TRUE. /

!
!     Use discovery check-in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      IF ( FIRST ) THEN

         BIG   = SQRT( DPMAX() ) / 100
         FIRST = .FALSE.

      END IF

!
!     Set invalid counts to start out. Initialize R1 and R2.
!
      N   = -3
      NX  = -3
      R1  = 0.D0
      R2  = 0.D0

!
!     Reject all large magnitude coefficients.
!
      IF (     ( ABS(A) .GT. BIG ) &
          .OR. ( ABS(B) .GT. BIG ) &
          .OR. ( ABS(C) .GT. BIG ) ) THEN

         CALL CHKIN  ( 'ZZBQUAD'                                      )
         CALL SETMSG ( 'Coefficients must have magnitude less than ' &
         //            'or equal to #, but were A = #; B = #; C = #.' )
         CALL ERRDP  ( '#',  BIG                                      )
         CALL ERRDP  ( '#',  A                                        )
         CALL ERRDP  ( '#',  B                                        )
         CALL ERRDP  ( '#',  C                                        )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZBQUAD'                                      )
         RETURN

      END IF

!
!     Reject large magnitude upper bounds as well.
!
      IF ( ABS(UB) .GT. BIG ) THEN

         CALL CHKIN  ( 'ZZBQUAD'                                      )
         CALL SETMSG ( 'Upper bounds must have magnitude less than ' &
         //            'or equal to #, but was #.'                    )
         CALL ERRDP  ( '#',  BIG                                      )
         CALL ERRDP  ( '#',  UB                                       )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZBQUAD'                                      )
         RETURN

      END IF

!
!     The upper bound must be positive.
!
      IF ( UB .LE. 0.D0 ) THEN

         CALL CHKIN  ( 'ZZBQUAD'                                 )
         CALL SETMSG ( 'Upper bound must be positive but was #.' )
         CALL ERRDP  ( '#',  UB                                  )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                  )
         CALL CHKOUT ( 'ZZBQUAD'                                 )
         RETURN

      END IF

!
!     Handle the degenerate cases first.
!
      IF ( A .EQ. 0.D0 ) THEN

         IF ( B .EQ. 0.D0 ) THEN
!
!           The equation is of the form
!
!              C = 0
!
            IF ( C .EQ. 0.D0 ) THEN
!
!              The equation is satisfied for all real numbers.
!
               N  = -1
               NX =  0

            ELSE
!
!              There are no solutions.
!
               N  = -2
               NX =  0

            END IF

         ELSE
!
!           The equation is first-order:
!
!              B*X + C = 0
!
!           In this branch, B is non-zero.
!
            IF ( ABS(C) .LE. ABS(UB * B) ) THEN

               N   =   1
               NX  =   0

               R1  =  -C / B
               R2  =   R1

            ELSE
!
!              The magnitude of the solution is too large.
!
               N   =  0
               NX  =  1

            END IF

         END IF


      ELSE
!
!        The leading coefficient of the equation is non-zero.
!
!        We can safely compute the discriminant now, due the
!        check we've already performed.
!
         DSCRIM = TOUCHD(  ( B*B ) - ( 4*A*C )  )

         IF ( DSCRIM .LT. 0.D0 ) THEN
!
!           We have complex roots, so we're done.
!
            N  = 0
            NX = 0

         ELSE IF ( DSCRIM .EQ. 0.D0 ) THEN
!
!           We have a single real root of multiplicity 2.
!
!           Compare the magnitude of the root to the upper bound.
!
            NUM1   =  -B
            DENOM  =   2 * A

            IF ( ABS(NUM1) .GE. ABS(DENOM * UB) ) THEN
!
!              The root is too large; we won't compute it.
!
               N   =  0
               NX  =  1

            ELSE
!
!              Set both roots to the same value. In this branch,
!              A is non-zero.
!
               N   =  1
               NX  =  0

               R1  =  ( NUM1 / A ) / 2
               R2  =  R1

            END IF


         ELSE
!
!           We have two nominally distinct real roots. Whether
!           they're distinct double precision numbers depends
!           on the relative magnitudes of A and DSCRIM.
!
            DENOM  = 2 * A
            SQDISC = SQRT( DSCRIM )

            IF ( B .GT. 0.D0 ) THEN

               NUM2 =  -B - SQDISC
               NUM1 =  -B + SQDISC
            ELSE
               NUM2 =  -B + SQDISC
               NUM1 =  -B - SQDISC
            END IF

!
!           See whether the root of larger magnitude is computable.
!
            IF ( ABS(NUM2) .LE. ABS(UB * DENOM) ) THEN
!
!              The root is computable.

               N  = 2
               NX = 0
!
!              In this branch, A is non-zero.
!
               R2 = ( NUM2 / A ) / 2

               IF ( ABS(R2) .GT. 0.D0 ) THEN
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
                  R1 =  C / ( A * R2 )

               ELSE
!
!                 The root of larger magnitude has magnitude 0. This
!                 doesn't leave many possible values for the root of
!                 smaller magnitude.
!
                  R1 = 0.D0

               END IF


            ELSE
!
!              The root of larger magnitude is not computable.
!              Check the root of smaller magnitude.
!
               IF ( ABS(NUM1) .LE. ABS(UB * DENOM) ) THEN
!
!                 The root is computable.
!
                  N  = 1
                  NX = 1

                  R1 = ( NUM1 / A ) / 2

               ELSE
!
!                 Neither root is computable.
!
                  N  = 0
                  NX = 2

               END IF

            END IF

         END IF

      END IF

      RETURN
      END
