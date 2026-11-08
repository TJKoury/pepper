C$Procedure F_CHBIGR ( CHBIGR tests )

      SUBROUTINE F_CHBIGR ( OK )

C$ Abstract
C
C     Exercise the SPICELIB Chebyshev expansion integrator
C     routine CHBIGR.
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
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB routine CHBIGR.
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
C-    TSPICE Version 1.0.1, 23-MAR-2020 (JDR)
C
C        Updated header and comments to comply with NAIF standard.
C
C-    TSPICE Version 1.0.0, 19-MAY-2014 (NJB)
C
C        Bug fix: code for construction of quantity titles
C        for some test utility calls was missing, causing
C        use of wrong titles in those calls.
C
C        Bug fix: relative error test for the evaluated input Cheby
C        expansion was changed to absolute for a case where the
C        expansion has a zero crossing.
C
C        Bug fix: work space bounds were increased for consistency
C        with the declaration in T_CHBIGR. This update was done
C        to help prevent errors if this code is modified later.
C        The previous bounds were large enough to allow proper
C        operation of this routine.
C
C
C        Last update was 29-MAR-2014 (NJB)
C
C           Test cases that use multiple samples across
C           the domain have been corrected so that both halves
C           of the domain are sampled.
C
C           Relative error tests for values near zero have been
C           changed to absolute error tests.
C
C           Tolerances have been tightened for some relative
C           error tests.
C
C        Previous update was 23-NOV-2013.
C
C-&


C$ Index_Entries
C
C     test Chebyshev expansion integrator
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX


C
C     Other functions
C


C
C     Local Parameters
C
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


C
C     Local Variables
C
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


C
C     Saved variables
C


C
C     Initial values
C


C
C     Open the test family.
C
      CALL TOPEN ( 'F_CHBIGR' )

C
C     *****************************************************************
C
C     Error cases
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
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

C
C     --- Case: -------------------------------------------------------
C
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



C
C     *****************************************************************
C
C     Normal cases
C
C     *****************************************************************
C
C     The following cases use input expansions having results that
C     can be verified by inspection.
C
C     Initialize the coefficient array with numbers that will cause
C     problems if they are incorrectly accessed.
C
      CALL FILLD ( DPMAX(), MAXCOF, CP )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial is f(x) = 6x**5; interval is '
     .//           '[10, 30]; X = 30.'                        )

C
C     Let our domain be the interval [10, 30].
C
      X2S(1) = 20.D0
      X2S(2) = 10.D0

      DEGP  = 5

      CP(1) = 0.D0
      CP(2) = 3.75D0
      CP(3) = 0.D0
      CP(4) = 1.875D0
      CP(5) = 0.D0
      CP(6) = 0.375D0

C
C     Evaluate the function and its integral at X = 30.
C
      X = 30.D0

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The indefinite integral is F(x) = x**6, defined on
C     the interval [-1, 1]. The result, scaled to account
C     for the change of variables, should be 10.
C
      XITGRL = 10.D0
      XP     =  6.D0

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial is f(x) = 6x**5; interval is '
     .//           '[10, 30]; X = 20.'                        )

C
C     Let our domain be the interval [10, 30].
C
      X2S(1) = 20.D0
      X2S(2) = 10.D0

      DEGP  = 5

      CP(1) = 0.D0
      CP(2) = 3.75D0
      CP(3) = 0.D0
      CP(4) = 1.875D0
      CP(5) = 0.D0
      CP(6) = 0.375D0

C
C     Evaluate the function and its integral at X = 20.
C
      X = 20.D0

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The indefinite integral is F(x) = x**6, defined on
C     the interval [-1, 1]. The result should be 0.
C
      XITGRL =  0.D0
      XP     =  0.D0

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


C
C     The following cases use T_CHBIGR to produce expected results.
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 0; radius is 10000, '
     .//           'X = midpoint + radius/2.'                  )


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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Continue previous degree 0 case. '
     .//           'Sample along domain interval.'    )

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

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, TIGHT,  OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, TIGHT,  OK )
         END IF

      END DO


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 1; radius is 10000, '
     .//           'X = midpoint + radius/2.'                  )

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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Continue previous degree 1 case. '
     .//           'Sample along domain interval.'    )

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

C
C        Note that in this case, the function passes through 0
C        at X = 94000.
C
         IF ( J .EQ. 21 ) THEN

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, TIGHT,  OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, TIGHT,  OK )
         END IF

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 2; radius is 10000, '
     .//           'X = midpoint + radius/2.'                  )

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



C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Continue previous degree 2 case. '
     .//           'Sample along domain interval.'    )

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

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, MEDTOL, OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 3; radius is 10000, '
     .//           'X = midpoint + radius/2.'                  )

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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Continue previous degree 3 case. '
     .//           'Sample along domain interval.'    )

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

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, TIGHT,  OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 4; radius is 10000, '
     .//           'X = midpoint - radius/2.'                  )

      DEGP   = 4

C
C     Use a negative value for the midpoint this time.
C
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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Continue previous degree 4 case. '
     .//           'Sample along domain interval.'    )

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

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, MEDTOL, OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 5; radius is 10000, '
     .//           'X = midpoint - radius/2.'                  )

      DEGP   = 5

C
C     Use a negative value for the midpoint this time.
C
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



C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Continue previous degree 5 case. '
     .//           'Sample along domain interval.'    )

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

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, MEDTOL, OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 15; radius is 10000, '
     .//           'X = midpoint + radius/2.'                  )


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

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 15; radius is 10000, '
     .//           'X = midpoint.'                             )


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

C
C     Use absolute error tests since results should be close to 0.
C
      CALL CHCKSD ( 'P',      P,      '~', XP,     MEDTOL, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~', XITGRL, MEDTOL, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 30; radius is 10000, '
     .//           'X = midpoint + radius/2.'                  )


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



C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 60; radius is 10000, '
     .//           'X = midpoint + radius/2.'                  )


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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 98; radius is 10000, '
     .//           'X = midpoint + radius/2.'                  )


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


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 98; radius is 10000, '
     .//           'X = midpoint + radius.'                  )


      DEGP   = 98

      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

C
C     Note the abscissa is at the right endpoint of the domain.
C
      X = X2S(1) + X2S(2)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 98; radius is 10000, '
     .//           'X = midpoint - radius.'                  )


      DEGP   = 98

      X2S(1) = 100000.D0
      X2S(2) =  10000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

C
C     Note the abscissa is at the left endpoint of the domain.
C
      X = X2S(1) - X2S(2)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 98; radius is 1000000, '
     .//           'X = midpoint + radius.'                     )


      DEGP   = 98

      X2S(1) = 1000000.D0
      X2S(2) = 1000000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

C
C     Note the abscissa is at the right endpoint of the domain.
C
      X = X2S(1) + X2S(2)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )



C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 98; radius is 1000000, '
     .//           'X = midpoint - radius.'                     )


      DEGP   = 98

      X2S(1) = 1000000.D0
      X2S(2) = 1000000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

C
C     Note the abscissa is at the left endpoint of the domain.
C
      X = X2S(1) - X2S(2)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'P',      P,      '~/', XP,     VTIGHT, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~/', XITGRL, VTIGHT, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 98; radius is 1000000, '
     .//           'X = midpoint.'                               )


      DEGP   = 98

      X2S(1) = 1000000.D0
      X2S(2) = 1000000.D0

      DO I = 1, DEGP+1
         CP(I) = DBLE( (DEGP/2)-I )
      END DO

C
C     Note the abscissa is at the left endpoint of the domain.
C
      X = X2S(1)

      CALL CHBIGR ( DEGP, CP, X2S, X, P, ITGRLP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_CHBIGR ( 1, DEGP, CP, X2S, X, WORK, XP, XITGRL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Use absolute error tests since results should be close to 0.
C
      CALL CHCKSD ( 'P',      P,      '~', XP,     MEDTOL, OK )
      CALL CHCKSD ( 'ITGRLP', ITGRLP, '~', XITGRL, MEDTOL, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 15; radius is 1000000, '
     .//           'sample results along the domain.'            )


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

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, MEDTOL, OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree is 98; radius is 1000000, '
     .//           'Sample results along the domain.'            )



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

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE,  P, '~',  XP, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE,  P, '~/', XP, MEDTOL, OK )
         END IF


         TITLE = 'ITGRLP at @'
         CALL REPMD  ( TITLE, '@', X, 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( J .EQ. (NSAMP/2)+1 ) THEN

C
C           Use absolute error tests since results should be close to
C           0.
C
            CALL CHCKSD ( TITLE, ITGRLP, '~',  XITGRL, MEDTOL, OK )
         ELSE
            CALL CHCKSD ( TITLE, ITGRLP, '~/', XITGRL, MEDTOL, OK )
         END IF

      END DO

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
