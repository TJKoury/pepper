!$Procedure F_CHBIGR ( CHBIGR tests )

      SUBROUTINE F_CHBIGR ( OK )

! $ Abstract
!
!     Exercise the SPICELIB Chebyshev expansion integrator
!     routine CHBIGR.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routine CHBIGR.
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
!-    TSPICE Version 1.0.1, 23-MAR-2020 (JDR)
!
!        Updated header and comments to comply with NAIF standard.
!
!-    TSPICE Version 1.0.0, 19-MAY-2014 (NJB)
!
!        Bug fix: code for construction of quantity titles
!        for some test utility calls was missing, causing
!        use of wrong titles in those calls.
!
!        Bug fix: relative error test for the evaluated input Cheby
!        expansion was changed to absolute for a case where the
!        expansion has a zero crossing.
!
!        Bug fix: work space bounds were increased for consistency
!        with the declaration in T_CHBIGR. This update was done
!        to help prevent errors if this code is modified later.
!        The previous bounds were large enough to allow proper
!        operation of this routine.
!
!
!        Last update was 29-MAR-2014 (NJB)
!
!           Test cases that use multiple samples across
!           the domain have been corrected so that both halves
!           of the domain are sampled.
!
!           Relative error tests for values near zero have been
!           changed to absolute error tests.
!
!           Tolerances have been tightened for some relative
!           error tests.
!
!        Previous update was 23-NOV-2013.
!
!-&


! $ Index_Entries
!
!     test Chebyshev expansion integrator
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX


!
!     Other functions
!


!
!     Local Parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      MEDTOL
      PARAMETER           ( MEDTOL = 1.D-10 )


      INTEGER               MAXCOF
      PARAMETER           ( MAXCOF = 100 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )


!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      CP     ( MAXCOF )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      ITGRLP
      DOUBLE PRECISION      P
      DOUBLE PRECISION      WORK   ( 2 * (MAXCOF+1) )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      X2S    ( 2 )
      DOUBLE PRECISION      XITGRL
      DOUBLE PRECISION      XP

      INTEGER               DEGP
      INTEGER               I
      INTEGER               J
      INTEGER               NSAMP


!
!     Saved variables
!


!
!     Initial values
!


!
!     Open the test family.
!
      CALL TOPEN ( 'F_CHBIGR' )

!
!     *****************************************************************
!
!     Error cases
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Exception: expansion degree is negative.' )

      CALL CLEARD ( MAXCOF, CP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DEGP   = -1
      CP(1)  = 0.D0
      X      = 0.D0
      X2S(1) = 0.D0
      X2S(2) = 1.D0
      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Exception: interval radius is non-positive.' )

      CALL CLEARD ( MAXCOF, CP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DEGP   = 0
      CP(1)  = 0.D0
      X      = 0.D0
      X2S(1) = 0.D0
      X2S(2) = 0.D0
      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )



!
!     *****************************************************************
!
!     Normal cases
!
!     *****************************************************************
!
!     The following cases use input expansions having results that
!     can be verified by inspection.
!
!     Initialize the coefficient array with numbers that will cause
!     problems if they are incorrectly accessed.
!
      CALL FILLD ( DPMAX(), MAXCOF, CP )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial is f(x) = 6x**5; interval is ' &
      //           '[10, 30]; X = 30.'                        )

!
!     Let our domain be the interval [10, 30].
!
      X2S(1) = 20.D0
      X2S(2) = 10.D0

      DEGP  = 5

      CP(1) = 0.D0
      CP(2) = 3.75D0
      CP(3) = 0.D0
      CP(4) = 1.875D0
      CP(5) = 0.D0
      CP(6) = 0.375D0

!
!     Evaluate the function and its integral at X = 30.
!
      X = 30.D0

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The indefinite integral is F(x) = x**6, defined on
!     the interval [-1, 1]. The result, scaled to account
!     for the change of variables, should be 10.
!
      XITGRL = 10.D0
      XP     =  6.D0

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial is f(x) = 6x**5; interval is ' &
      //           '[10, 30]; X = 20.'                        )

!
!     Let our domain be the interval [10, 30].
!
      X2S(1) = 20.D0
      X2S(2) = 10.D0

      DEGP  = 5

      CP(1) = 0.D0
      CP(2) = 3.75D0
      CP(3) = 0.D0
      CP(4) = 1.875D0
      CP(5) = 0.D0
      CP(6) = 0.375D0

!
!     Evaluate the function and its integral at X = 20.
!
      X = 20.D0

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The indefinite integral is F(x) = x**6, defined on
!     the interval [-1, 1]. The result should be 0.
!
      XITGRL =  0.D0
      XP     =  0.D0

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     The following cases use T_CHBIGR to produce expected results.
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 0; radius is 10000, ' &
      //           'X = midpoint + radius/2.'                  )


      DEGP   = 0

      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      CP(1)  = 3.D0


      X = X2S(1) + X2S(2)/2

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Continue previous degree 0 case. ' &
      //           'Sample along domain interval.'    )

      NSAMP = 101
      DELTA = 2 * X2S(2) / (NSAMP-1)

      DO J = 1, NSAMP

         X = X2S(1) - X2S(2) + (J-1)*DELTA

         CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'P at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, TIGHT,  OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, TIGHT,  OK )
         END IF

      END DO


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 1; radius is 10000, ' &
      //           'X = midpoint + radius/2.'                  )

      DEGP   = 1


      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      CP(1)  = 3.D0
      CP(2)  = 5.D0

      X = X2S(1) + X2S(2)/2

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Continue previous degree 1 case. ' &
      //           'Sample along domain interval.'    )

      NSAMP = 101
      DELTA = 2 * X2S(2) / (NSAMP-1)

      DO J = 1, NSAMP

         X = X2S(1) - X2S(2) + (J-1)*DELTA

         CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'P at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Note that in this case, the function passes through 0
!        at X = 94000.
!
         IF ( J .EQ. 21 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, TIGHT,  OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, TIGHT,  OK )
         END IF

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 2; radius is 10000, ' &
      //           'X = midpoint + radius/2.'                  )

      DEGP   = 2


      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      CP(1)  = 3.D0
      CP(2)  = 5.D0
      CP(3)  = 7.D0

      X = X2S(1) + X2S(2)/2

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )



!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Continue previous degree 2 case. ' &
      //           'Sample along domain interval.'    )

      NSAMP = 101
      DELTA = 2 * X2S(2) / (NSAMP-1)

      DO J = 1, NSAMP

         X = X2S(1) - X2S(2) + (J-1)*DELTA

         CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'P at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, MEDTOL, OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 3; radius is 10000, ' &
      //           'X = midpoint + radius/2.'                  )

      DEGP   = 3


      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      CP(1)  = 3.D0
      CP(2)  = 5.D0
      CP(3)  = 7.D0
      CP(4)  = 9.D0

      X = X2S(1) + X2S(2)/2

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Continue previous degree 3 case. ' &
      //           'Sample along domain interval.'    )

      NSAMP = 101
      DELTA = 2 * X2S(2) / (NSAMP-1)

      DO J = 1, NSAMP

         X = X2S(1) - X2S(2) + (J-1)*DELTA

         CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'P at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, TIGHT,  OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 4; radius is 10000, ' &
      //           'X = midpoint - radius/2.'                  )

      DEGP   = 4

!
!     Use a negative value for the midpoint this time.
!
      X2S(1) = -100000.D0
      X2S(2) =  10000.D0

      CP(1)  = 3.D0
      CP(2)  = 5.D0
      CP(3)  = 7.D0
      CP(4)  = 9.D0
      CP(5)  = 11.D0

      X = X2S(1) - X2S(2)/2

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Continue previous degree 4 case. ' &
      //           'Sample along domain interval.'    )

      NSAMP = 101
      DELTA = 2 * X2S(2) / (NSAMP-1)

      DO J = 1, NSAMP

         X = X2S(1) - X2S(2) + (J-1)*DELTA

         CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'P at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, MEDTOL, OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 5; radius is 10000, ' &
      //           'X = midpoint - radius/2.'                  )

      DEGP   = 5

!
!     Use a negative value for the midpoint this time.
!
      X2S(1) = -100000.D0
      X2S(2) =  10000.D0

      CP(1)  = 3.D0
      CP(2)  = 5.D0
      CP(3)  = 7.D0
      CP(4)  = 9.D0
      CP(5)  = 11.D0
      CP(5)  = -13.D0

      X = X2S(1) - X2S(2)/2

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )



!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Continue previous degree 5 case. ' &
      //           'Sample along domain interval.'    )

      NSAMP = 101
      DELTA = 2 * X2S(2) / (NSAMP-1)

      DO J = 1, NSAMP

         X = X2S(1) - X2S(2) + (J-1)*DELTA

         CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TITLE = 'P at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, MEDTOL, OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 15; radius is 10000, ' &
      //           'X = midpoint + radius/2.'                  )


      DEGP   = 15

      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

      X = X2S(1) + X2S(2)/2

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 15; radius is 10000, ' &
      //           'X = midpoint.'                             )


      DEGP   = 15

      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

      X = X2S(1)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Use absolute error tests since results should be close to 0.
!
      CALL CHCKSD ( 'P',      P,      '~', XP,     MEDTOL, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~', XITGRL, MEDTOL, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 30; radius is 10000, ' &
      //           'X = midpoint + radius/2.'                  )


      DEGP   = 30

      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

      X = X2S(1) + X2S(2)/2

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )



!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 60; radius is 10000, ' &
      //           'X = midpoint + radius/2.'                  )


      DEGP   = 60

      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

      X = X2S(1) + X2S(2)/2

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 98; radius is 10000, ' &
      //           'X = midpoint + radius/2.'                  )


      DEGP   = 98

      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

      X = X2S(1) + X2S(2)/2

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 98; radius is 10000, ' &
      //           'X = midpoint + radius.'                  )


      DEGP   = 98

      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

!
!     Note the abscissa is at the right endpoint of the domain.
!
      X = X2S(1) + X2S(2)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 98; radius is 10000, ' &
      //           'X = midpoint - radius.'                  )


      DEGP   = 98

      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

!
!     Note the abscissa is at the left endpoint of the domain.
!
      X = X2S(1) - X2S(2)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 98; radius is 1000000, ' &
      //           'X = midpoint + radius.'                     )


      DEGP   = 98

      X2S(1) = 1000000.D0
      X2S(2) = 1000000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

!
!     Note the abscissa is at the right endpoint of the domain.
!
      X = X2S(1) + X2S(2)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )



!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 98; radius is 1000000, ' &
      //           'X = midpoint - radius.'                     )


      DEGP   = 98

      X2S(1) = 1000000.D0
      X2S(2) = 1000000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

!
!     Note the abscissa is at the left endpoint of the domain.
!
      X = X2S(1) - X2S(2)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 98; radius is 1000000, ' &
      //           'X = midpoint.'                               )


      DEGP   = 98

      X2S(1) = 1000000.D0
      X2S(2) = 1000000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

!
!     Note the abscissa is at the left endpoint of the domain.
!
      X = X2S(1)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Use absolute error tests since results should be close to 0.
!
      CALL CHCKSD ( 'P',      P,      '~', XP,     MEDTOL, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~', XITGRL, MEDTOL, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 15; radius is 1000000, ' &
      //           'sample results along the domain.'            )


      DEGP   = 15

      X2S(1) = 1000000.D0
      X2S(2) = 1000000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

      NSAMP = 10001
      DELTA = 2 * X2S(2) / (NSAMP-1)

      DO J = 1, NSAMP

         X = X2S(1) - X2S(2) + (J-1)*DELTA

         CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
         CALL CHCKXC ( .FALSE., ' ', OK )


         TITLE = 'P at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, MEDTOL, OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Polynomial degree is 98; radius is 1000000, ' &
      //           'Sample results along the domain.'            )



      DEGP   = 98

      X2S(1) = 1000000.D0
      X2S(2) = 1000000.D0


      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

      NSAMP = 10001
      DELTA = 2 * X2S(2) / (NSAMP-1)

      DO J = 1, NSAMP

         X = X2S(1) - X2S(2) + (J-1)*DELTA

         CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
         CALL CHCKXC ( .FALSE., ' ', OK )


         TITLE = 'P at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, MEDTOL, OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

!
!           Use absolute error tests since results should be close to
!           0.
!
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
