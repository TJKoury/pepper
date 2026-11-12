!$Procedure F_SPKE15  (  Test routine SPKE15  )

      SUBROUTINE F_SPKE15 ( OK )
      use pepper_legacy, pepper_legacy_pos => pos

! $ Abstract
!
!     This test family checks the behavior of the SPICE routine
!     SPKE15 --- the evaluator for type 15 SPK segments.
!
!     It firsts exercises all of the listed exceptions for SPKE15.
!
!     Next it compares the results of the type 15 evaluator against
!     a modified type 53 evaluator provided by MASL.
!
!     Finally a few simple consistency checks are performed to check
!     that precession of the lines of nodes and apsides move at the
!     correct rate,  Angular momentum is constant and that discrete
!     integration of the initial state approximately yields the
!     state one second later.
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
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
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
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 05-JUN-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Changed output argument name PASS to OK for consistency with
!        other test families.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.1, 14-DEC-2001 (EDW)
!
!        Altered Hyperbolic Angular Momentum test case
!        to us a vector relative comparison between the
!        experimental and theoretical angular momentum vectors.
!        The previous version of the test performed relative
!        error comparison on the vector components.
!
!-    SPICELIB Version 1.1.0, 20-OCT-1999 (WLT)
!
!        Declared PI to be an EXTERNAL Functions.
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     Spicelib Functions
!

!
!     Check out the error handling first.
!

      INTEGER               EPOCH
      INTEGER               TP
      INTEGER               PA
      INTEGER               P
      INTEGER               ECC
      INTEGER               TZERO
      INTEGER               J2FLG
      INTEGER               PV
      INTEGER               GM
      INTEGER               OJ2
      INTEGER               RPL

      PARAMETER           ( EPOCH = 1 )
      PARAMETER           ( TP    = 2 )
      PARAMETER           ( PA    = 3 )
      PARAMETER           ( P     = 4 )
      PARAMETER           ( ECC   = 5 )
      PARAMETER           ( TZERO = 6 )
      PARAMETER           ( J2FLG = 7 )
      PARAMETER           ( PV    = 8 )
      PARAMETER           ( GM    = 9 )
      PARAMETER           ( OJ2   = 10 )
      PARAMETER           ( RPL   = 11 )

      INTEGER               TYPE15
      PARAMETER           ( TYPE15 = 1 )

      INTEGER               TYPE53
      PARAMETER           ( TYPE53 = 2 )

      INTEGER               ITEM ( RPL, TYPE53 )




      INTEGER               I

      DOUBLE PRECISION      MYJ2

      DOUBLE PRECISION      MYP
      DOUBLE PRECISION      MYGM
      DOUBLE PRECISION      MYECC
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      EDPERI
      DOUBLE PRECISION      ADPERI


      DOUBLE PRECISION      RECORD ( 17, 2 )


      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE2 ( 6 )
      DOUBLE PRECISION      ADNODE
      DOUBLE PRECISION      EDNODE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      SMA
      DOUBLE PRECISION      DMDT
      DOUBLE PRECISION      PERIOD
      DOUBLE PRECISION      ELTS0 ( 8 )
      DOUBLE PRECISION      ELTS1 ( 8 )

      DOUBLE PRECISION      ANGMO1 ( 3 )
      DOUBLE PRECISION      ANGMO2 ( 3 )
      DOUBLE PRECISION      POS    ( 3 )

!
!     Open the family of tests for SPKE15.
!
      CALL TOPEN   (  'F_SPKE15'  )


      ITEM( EPOCH, TYPE15 ) = 1
      ITEM( TP,    TYPE15 ) = 2
      ITEM( PA,    TYPE15 ) = 5
      ITEM( P,     TYPE15 ) = 8
      ITEM( ECC,   TYPE15 ) = 9
      ITEM( J2FLG, TYPE15 ) = 10
      ITEM( PV,    TYPE15 ) = 11
      ITEM( GM,    TYPE15 ) = 14
      ITEM( OJ2,   TYPE15 ) = 15
      ITEM( RPL,   TYPE15 ) = 16
      ITEM( TZERO, TYPE15 ) = 17


      ITEM( EPOCH, TYPE53 ) = 1
      ITEM( TP,    TYPE53 ) = 2
      ITEM( PA,    TYPE53 ) = 5
      ITEM( P,     TYPE53 ) = 8
      ITEM( ECC,   TYPE53 ) = 9
      ITEM( TZERO, TYPE53 ) = 10
      ITEM( J2FLG, TYPE53 ) = 11
      ITEM( PV,    TYPE53 ) = 12
      ITEM( GM,    TYPE53 ) = 15
      ITEM( OJ2,   TYPE53 ) = 16
      ITEM( RPL,   TYPE53 ) = 17

      DO I = 1, 17
         RECORD ( I, TYPE15 ) = 0.0D0
         RECORD ( I, TYPE53 ) = 0.0D0
      END DO


      ET = 7200.0D0


!
!     Here's the basic strategy for each test case.
!
!        We set up conditions in the input
!        records that should yield exceptions.
!
!        We check for the exception
!
!        Finally set the exceptional item to
!        something realistic so that future tests
!        won't be tripped up by this exceptional
!        value.
!
!     Note: we set the values for both the type 53 record and
!     type 15 record at the same time so that they always
!     represent the same orbit.  We do this even when the type 53
!     record is not used.
!


!
!     The semi-latus rectum is supposed to be positive.  Start
!     out at zero and then set it to something reasonable.
!
      CALL TCASE ( 'Semi-latus rectum exception' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(P,I),   I ) =  0.0D0
         END DO

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),       STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADLATUSRECTUM)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(P,I),   I ) =  20000.0D0
         END DO


!
!     Negative eccentricities should produce exceptions.  After
!     checking that this is so set the eccentricity to something
!     yielding a periodic orbit.
!
      CALL TCASE ( 'Eccentricity Exception' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(ECC,I), I ) = -1.0D0
         END DO

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),       STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADECCENTRICITY)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(ECC,I), I ) = 0.1D0
         END DO


!
!     The central mass must be positive.  Zero or less should
!     trigger an exception. Try zero and -1.  After that we
!     use the mass of the earth.
!
      CALL TCASE ( 'Central Mass Exception --- mass 0' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(GM,I),   I ) =  0.0D0
         END DO

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),        STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEMASS)', OK    )

      CALL TCASE ( 'Central Mass Exception --- mass -1' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(GM,I),   I ) = -1.0D0
         END DO

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),        STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEMASS)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(GM,I),   I ) =  398600.447703261138D0
         END DO


!
!     Only a zero trajectory pole can produce a problem.  By
!     construction we already have one.
!
      CALL TCASE ( 'Trajectory Pole Exception' )

         CALL SPKE15 ( ET,      RECORD(1,TYPE15),  STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADVECTOR)', OK    )

!
!        Set the trajectory pole to 45 degree inclination
!
         DO I = TYPE15, TYPE53
            RECORD( ITEM(TP,I) + 1, I ) = DCOS( PI()/4.0D0 )
            RECORD( ITEM(TP,I) + 2, I ) = DSIN( PI()/4.0D0 )
         END DO


!
!     Only a zero periapsis vector yields an exception.  We
!     already have this by construction.  After testing make
!     a periapsis vector that is orthogonal to the trajectory
!     pole vector.
!
      CALL TCASE ( 'Periapsis Vector Exception' )

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),  STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADVECTOR)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(PA,I) + 1, I ) =  DSIN( PI()/4.0D0 )
            RECORD( ITEM(PA,I) + 2, I ) = -DCOS( PI()/4.0D0 )
         END DO


!
!     Only a zero central body pole vector can yield an exception.
!     We have such a situation by construction.  After checking
!     this, align the pole with the Z axis.
!
      CALL TCASE ( 'Pole Vector Exception' )

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),  STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADVECTOR)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(PV,I) + 2, I ) = 1.0D0
         END DO


!
!     Anything less than zero should trigger an exception.  After
!     checking, set the equatorial radius to that of the earth.
!
      CALL TCASE ( 'Equatorial Radius Exception' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(RPL,I),   I ) =  -1.0D0
         END DO

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),  STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUS)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(RPL,I),   I ) =  6378.184D0
         END DO



!
!     If the periapse is not nearly perpendicular to the
!     trajectory pole, we should get an exception.  Create
!     a vector that isn't perpendicular to the trajectory pole
!     by messing up the sign on the z-component.
!
      CALL TCASE ( 'Bad Initial Conditions' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(PA,I) + 1, I ) =  1.0D0
            RECORD( ITEM(PA,I) + 2, I ) =  0.0D0
         END DO

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),     STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADINITSTATE)', OK    )


         DO I = TYPE15, TYPE53
            RECORD( ITEM(PA,I) + 1, I ) =  DSIN( PI()/4.0D0 )
            RECORD( ITEM(PA,I) + 2, I ) = -DCOS( PI()/4.0D0 )
         END DO


!
!     That takes care of all the exception tests.  Next see if
!     we get the same results with the two different implementations.
!
!     Check to make sure that type 53 and type 15 agree
!     when J2 is zero.
!
      CALL TCASE ( 'Elliptic Orbit, J2 = 0' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(OJ2,I), I ) = 0.0D0
         END DO

         CALL SPKE53 ( ET, RECORD(1,TYPE53),  STATE2 )
         CALL SPKE15 ( ET, RECORD(1,TYPE15),  STATE  )

         CALL CHCKXC ( .FALSE.,        ' ',                      OK )
         CALL CHCKAD ( 'STATE', STATE, '~/', STATE2, 6, 1.0D-12, OK )


!
!     Check to make sure that type 53 and type 15 agree for
!     J2 non-zero.  More specifically when J2 is that of the
!     earth.
!
      CALL TCASE ( 'Eliptic Orbit, J2 of Earth' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(OJ2,I),   I ) = 1.082616D-3
            RECORD( ITEM(J2FLG,I), I ) = 0.0D0
         END DO

         CALL SPKE53   ( ET, RECORD(1,TYPE53), STATE  )
         CALL SPKE15   ( ET, RECORD(1,TYPE15), STATE2 )

         CALL CHCKXC ( .FALSE.,        ' ',                      OK )
         CALL CHCKAD ( 'STATE', STATE, '~/', STATE2, 6, 1.0D-13, OK )



!
!     Check that the precession is what is predicted from the
!     equations.  We use oscelt to determine how far the node
!     moves in one orbit.
!
      CALL TCASE ( 'Check Precession' )

         MYP    = RECORD( ITEM(P,  TYPE15), TYPE15 )
         MYGM   = RECORD( ITEM(GM, TYPE15), TYPE15 )
         MYECC  = RECORD( ITEM(ECC,TYPE15), TYPE15 )
         MYJ2   = RECORD( ITEM(OJ2,TYPE15), TYPE15 )
         RADIUS = RECORD( ITEM(RPL,TYPE15), TYPE15 )

         SMA    = MYP / ( 1 - MYECC**2 )
         DMDT   = SQRT ( MYGM/SMA**3 )
         PERIOD = TWOPI()/ DMDT

         CALL SPKE15 ( ET,    RECORD(1,TYPE15), STATE )
         CALL OSCELT ( STATE, ET, MYGM,         ELTS0 )

         ET = ET + PERIOD

         CALL SPKE15 ( ET,     RECORD(1,TYPE15),  STATE2 )
         CALL OSCELT ( STATE2, ET , MYGM,         ELTS1  )


!
!        ELTS        are equivalent conic elements describing the orbit
!                    of the body around its primary. The elements are,
!                    in order:
!
!                          RP      Perifocal distance.
!                          ECC     Eccentricity.
!                          INC     Inclination.
!                          LNODE   Longitude of the ascending node.
!                          ARGP    Argument of periapsis.
!                          M0      Mean anomaly at epoch.
!                          T0      Epoch.
!                          MU      Gravitational parameter.
!

         EDNODE = -TWOPI()*1.5D0*MYJ2*DCOS(ELTS0(3))*( RADIUS/MYP )**2
         ADNODE =  ELTS1(4) - ELTS0(4)

         CALL CHCKXC ( .FALSE.,        ' ',                    OK )
         CALL CHCKSD ( 'DNODE', ADNODE, '~/', EDNODE, 1.0D-11, OK )

!
!        Next Check to see that periapse has moved by the "right"
!        amount.
!
         EDPERI =   TWOPI()*1.5D0*MYJ2 &
                * ( 2.5D0*DCOS(ELTS0(3))**2 - 0.5D0 ) &
                *  (RADIUS/MYP)**2
         ADPERI = ELTS1(5) - ELTS0(5)

         CALL CHCKSD ( 'DPERI', ADPERI, '~/', EDPERI, 1.0D-10, OK )



!
!     See that we get the same results for the hyperbolic case.
!
      CALL TCASE ( 'Hyperbolic Orbit' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(ECC,I), I ) = 2.0D0
         END DO

         ET          = 7200.0D0

         CALL SPKE53 ( ET, RECORD(1,TYPE53), STATE  )
         CALL SPKE15 ( ET, RECORD(1,TYPE15), STATE2 )

         CALL CHCKXC ( .FALSE.,        ' ',                      OK )
         CALL CHCKAD ( 'STATE', STATE, '~/', STATE2, 6, 1.0D-12, OK )


!
!     Make sure the angular momentum is constant.
!
      CALL TCASE ( 'Hyperbolic Angular Momentum' )

         RECORD( ITEM(ECC,TYPE15), TYPE15 ) = 2.0D0

         ET          = 0.0D0

         CALL SPKE15   ( ET,    RECORD(1,TYPE15),   STATE  )
         CALL VCRSS    ( STATE, STATE(4),           ANGMO1 )

         ET          = ET + 72000.0D0

         CALL SPKE15   ( ET,    RECORD(1,TYPE15),   STATE  )
         CALL VCRSS    ( STATE, STATE(4),           ANGMO2 )

         CALL CHCKXC ( .FALSE.,          ' ',                     OK )
         CALL CHCKAD ( 'ANGMO', ANGMO1, '~~/', ANGMO2, 3, 1.0D-12, OK )



!
!     In this case we simply check that adding the velocity times
!     a delta time gives approximately the position.
!
      CALL TCASE ( 'Discrete Hyperbolic Integration' )

         ET          = 0.0D0

         CALL SPKE15   ( ET,       RECORD(1,TYPE15), STATE  )
         CALL SPKE15   ( ET+1.0D0, RECORD(1,TYPE15), STATE2 )

         CALL VADD ( STATE(1), STATE(4), POS )
         CALL CHCKXC ( .FALSE.,          ' ',                   OK )
         CALL CHCKAD ( 'Position', POS, '~/', STATE2, 3, 1.0D-4, OK )


      CALL T_SUCCESS(OK)
      RETURN
      END
