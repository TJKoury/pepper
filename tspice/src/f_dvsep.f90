!$Procedure F_DVSEP ( DVSEP family tests )

      SUBROUTINE F_DVSEP ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        DVSEP
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
!     OK         is a logical that indicates the test status to the
!                caller.
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
!     None.
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
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 01-APR-2009 (EDW)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      T_RANDD


!
!     Local parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-12 )

      DOUBLE PRECISION      VAL
      DOUBLE PRECISION      COLAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MAG_LOG
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      VEL   ( 3 )
      DOUBLE PRECISION      REC1  ( 3 )
      DOUBLE PRECISION      REC2  ( 3 )
      DOUBLE PRECISION      CRSS1 ( 3 )
      DOUBLE PRECISION      CRSS2 ( 3 )
      DOUBLE PRECISION      S1    ( 6 )
      DOUBLE PRECISION      S2    ( 6 )
      DOUBLE PRECISION      S1_T  ( 6 )
      DOUBLE PRECISION      S2_T  ( 6 )
      DOUBLE PRECISION      TRANS ( 6,6 )

      INTEGER               I
      INTEGER               SEED1
      INTEGER               SEED2

      CHARACTER*(128)        TXT

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'dvsep.pck' )


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DVSEP' )

!
!     Create a PCK, load using FURNSH.
!
      CALL T_PCK08 ( PCK, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Case 1
!
      CALL TCASE ( 'DVSEP overflow.' )

!
!     Two state vectors, one rather odd.
!
      S1(1) = 3.D0
      S1(2) = 4.D0
      S1(3) = 0.D0
      S1(4) = -9.D0
      S1(5) = 2.D0
      S1(6) = 0.1D0

!
!     S2 has a very high derivative of the unit vector.
!
      S2(1) = 1.D0
      S2(2) = 0.D0
      S2(3) = 1.D0
      S2(4) = 1.D0
      S2(5) = 0.9D0*DPMAX()
      S2(6) = 0.D0

      VAL = DVSEP( S1, S2)
      CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )



!
!     Case 2
!
      CALL TCASE ( 'DVSEP S1xS2 = 0' )

      S1(1) = 25.D0
      S1(2) = 0.D0
      S1(3) = 0.D0
      S1(4) = 2.D0
      S1(5) =-1.D0
      S1(6) = 1.D0

      S2(1) = -S1(1)
      S2(2) = -S1(2)
      S2(3) = -S1(3)
      S2(4) = -S1(4)
      S2(5) = -S1(5)
      S2(6) = -S1(6)

      VAL = DVSEP( S1, S2)
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Expect a separation angle rate of zero.
!
      CALL CHCKSD ( 'DVSEP S1xS2 = 0 (1)', VAL, '=', 0.D0, TIGHT, OK )


!
!     Case 3
!


!
!     Create two random vectors in R3, REC1 and REC2, each with
!     magnitude MAG. Use the cross product operations
!
!        CRSS1 = REC1  x REC2
!        CRSS2 = CRSS1 x REC1
!
!     to construct a velocity unit vector orthogonal to REC1 in the
!     plane of REC1 and REC2 oriented such that REC1 rotates towards
!     REC2 with REC2 constant.
!
!     In this case
!
!            d(theta)
!        v = -------- * || REC1 ||
!            dt
!
!     with v = 1, then
!
!        d(theta)     -1           -1
!        -------- =  ______    =   ---
!        dt
!                  || REC1 ||      MAG
!

      SEED1  = -82763653
      SEED2  = -273661


!
!     Define an arbitrary rotation from J2000 to something
!     not referenced against earth rotation at an arbitrary
!     ephemeris time, ET = 0 in this case.
!
      CALL SXFORM( 'J2000', 'IAU_MOON', 0.D0, TRANS )

!
!     Note, 5000 is also an arbitrary value.
!
      DO I = 1, 5000

         TXT = 'S1, S2 random test #, MAG_LOG #'

         CALL REPMI ( TXT, '#', I, TXT )

         MAG_LOG = T_RANDD( -2.D0, 25.D0, SEED1 )
         MAG     = 10**MAG_LOG

         CALL REPMD ( TXT, '#', MAG_LOG, 8, TXT )

         CALL TCASE ( TXT )

         COLAT = T_RANDD( 0.D0, PI(),    SEED1 )
         LON   = T_RANDD( 0.D0, TWOPI(), SEED2 )

         CALL SPHREC ( MAG, COLAT, LON, REC1 )

         COLAT = T_RANDD( 0.D0, PI(),    SEED1 )
         LON   = T_RANDD( 0.D0, TWOPI(), SEED2 )

         CALL SPHREC ( MAG, COLAT, LON, REC2 )

         CALL VCRSS ( REC1, REC2, CRSS1)
         CALL VCRSS ( CRSS1, REC1, CRSS2 )

         CALL VHAT ( CRSS2, VEL )

         CALL VPACK ( REC1(1),  REC1(2),  REC1(3), S1(1) )
         CALL VPACK ( VEL(1),   VEL(2),   VEL(3),  S1(4) )

         CALL VPACK ( REC2(1), REC2(2),  REC2(3),  S2(1) )
         CALL VPACK ( 0.D0,    0.D0,     0.D0,     S2(4) )

!
!        Apply the transformation, TRANS, to S1 and S2, creating new
!        vectors without or with fewer zero components, particularly
!        the S2 velocity components.
!
         CALL MXVG ( TRANS, S1, 6, 6, S1_T )
         CALL MXVG ( TRANS, S2, 6, 6, S2_T )

         VAL = DVSEP( S1_T, S2_T )

         CALL CHCKSD ( TXT, VAL, '~', -1.D0/MAG, TIGHT, OK )

      END DO

!
! Case n
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END





