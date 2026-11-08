C$Procedure F_SPKE15  (  Test routine SPKE15  )

      SUBROUTINE F_SPKE15 ( OK )

C$ Abstract
C
C     This test family checks the behavior of the SPICE routine
C     SPKE15 --- the evaluator for type 15 SPK segments.
C
C     It firsts exercises all of the listed exceptions for SPKE15.
C
C     Next it compares the results of the type 15 evaluator against
C     a modified type 53 evaluator provided by MASL.
C
C     Finally a few simple consistency checks are performed to check
C     that precession of the lines of nodes and apsides move at the
C     correct rate,  Angular momentum is constant and that discrete
C     integration of the initial state approximately yields the
C     state one second later.
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
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 05-JUN-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Changed output argument name PASS to OK for consistency with
C        other test families.
C
C        Edited the header to comply with NAIF standard.
C
C-    SPICELIB Version 1.1.1, 14-DEC-2001 (EDW)
C
C        Altered Hyperbolic Angular Momentum test case
C        to us a vector relative comparison between the
C        experimental and theoretical angular momentum vectors.
C        The previous version of the test performed relative
C        error comparison on the vector components.
C
C-    SPICELIB Version 1.1.0, 20-OCT-1999 (WLT)
C
C        Declared PI to be an EXTERNAL Functions.
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     Spicelib Functions
C
      EXTERNAL              PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      PI

C
C     Check out the error handling first.
C

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

C
C     Open the family of tests for SPKE15.
C
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


C
C     Here's the basic strategy for each test case.
C
C        We set up conditions in the input
C        records that should yield exceptions.
C
C        We check for the exception
C
C        Finally set the exceptional item to
C        something realistic so that future tests
C        won't be tripped up by this exceptional
C        value.
C
C     Note: we set the values for both the type 53 record and
C     type 15 record at the same time so that they always
C     represent the same orbit.  We do this even when the type 53
C     record is not used.
C


C
C     The semi-latus rectum is supposed to be positive.  Start
C     out at zero and then set it to something reasonable.
C
      CALL TCASE ( 'Semi-latus rectum exception' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(P,I),   I ) =  0.0D0
         END DO

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),       STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADLATUSRECTUM)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(P,I),   I ) =  20000.0D0
         END DO


C
C     Negative eccentricities should produce exceptions.  After
C     checking that this is so set the eccentricity to something
C     yielding a periodic orbit.
C
      CALL TCASE ( 'Eccentricity Exception' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(ECC,I), I ) = -1.0D0
         END DO

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),       STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADECCENTRICITY)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(ECC,I), I ) = 0.1D0
         END DO


C
C     The central mass must be positive.  Zero or less should
C     trigger an exception. Try zero and -1.  After that we
C     use the mass of the earth.
C
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


C
C     Only a zero trajectory pole can produce a problem.  By
C     construction we already have one.
C
      CALL TCASE ( 'Trajectory Pole Exception' )

         CALL SPKE15 ( ET,      RECORD(1,TYPE15),  STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADVECTOR)', OK    )

C
C        Set the trajectory pole to 45 degree inclination
C
         DO I = TYPE15, TYPE53
            RECORD( ITEM(TP,I) + 1, I ) = DCOS( PI()/4.0D0 )
            RECORD( ITEM(TP,I) + 2, I ) = DSIN( PI()/4.0D0 )
         END DO


C
C     Only a zero periapsis vector yields an exception.  We
C     already have this by construction.  After testing make
C     a periapsis vector that is orthogonal to the trajectory
C     pole vector.
C
      CALL TCASE ( 'Periapsis Vector Exception' )

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),  STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADVECTOR)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(PA,I) + 1, I ) =  DSIN( PI()/4.0D0 )
            RECORD( ITEM(PA,I) + 2, I ) = -DCOS( PI()/4.0D0 )
         END DO


C
C     Only a zero central body pole vector can yield an exception.
C     We have such a situation by construction.  After checking
C     this, align the pole with the Z axis.
C
      CALL TCASE ( 'Pole Vector Exception' )

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),  STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADVECTOR)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(PV,I) + 2, I ) = 1.0D0
         END DO


C
C     Anything less than zero should trigger an exception.  After
C     checking, set the equatorial radius to that of the earth.
C
      CALL TCASE ( 'Equatorial Radius Exception' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(RPL,I),   I ) =  -1.0D0
         END DO

         CALL SPKE15 (  ET,     RECORD(1,TYPE15),  STATE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUS)', OK    )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(RPL,I),   I ) =  6378.184D0
         END DO



C
C     If the periapse is not nearly perpendicular to the
C     trajectory pole, we should get an exception.  Create
C     a vector that isn't perpendicular to the trajectory pole
C     by messing up the sign on the z-component.
C
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


C
C     That takes care of all the exception tests.  Next see if
C     we get the same results with the two different implementations.
C
C     Check to make sure that type 53 and type 15 agree
C     when J2 is zero.
C
      CALL TCASE ( 'Elliptic Orbit, J2 = 0' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(OJ2,I), I ) = 0.0D0
         END DO

         CALL SPKE53 ( ET, RECORD(1,TYPE53),  STATE2 )
         CALL SPKE15 ( ET, RECORD(1,TYPE15),  STATE  )

         CALL CHCKXC ( .FALSE.,        ' ',                      OK )
         CALL CHCKAD ( 'STATE', STATE, '~/', STATE2, 6, 1.0D-12, OK )


C
C     Check to make sure that type 53 and type 15 agree for
C     J2 non-zero.  More specifically when J2 is that of the
C     earth.
C
      CALL TCASE ( 'Eliptic Orbit, J2 of Earth' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(OJ2,I),   I ) = 1.082616D-3
            RECORD( ITEM(J2FLG,I), I ) = 0.0D0
         END DO

         CALL SPKE53   ( ET, RECORD(1,TYPE53), STATE  )
         CALL SPKE15   ( ET, RECORD(1,TYPE15), STATE2 )

         CALL CHCKXC ( .FALSE.,        ' ',                      OK )
         CALL CHCKAD ( 'STATE', STATE, '~/', STATE2, 6, 1.0D-13, OK )



C
C     Check that the precession is what is predicted from the
C     equations.  We use oscelt to determine how far the node
C     moves in one orbit.
C
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


C
C        ELTS        are equivalent conic elements describing the orbit
C                    of the body around its primary. The elements are,
C                    in order:
C
C                          RP      Perifocal distance.
C                          ECC     Eccentricity.
C                          INC     Inclination.
C                          LNODE   Longitude of the ascending node.
C                          ARGP    Argument of periapsis.
C                          M0      Mean anomaly at epoch.
C                          T0      Epoch.
C                          MU      Gravitational parameter.
C

         EDNODE = -TWOPI()*1.5D0*MYJ2*DCOS(ELTS0(3))*( RADIUS/MYP )**2
         ADNODE =  ELTS1(4) - ELTS0(4)

         CALL CHCKXC ( .FALSE.,        ' ',                    OK )
         CALL CHCKSD ( 'DNODE', ADNODE, '~/', EDNODE, 1.0D-11, OK )

C
C        Next Check to see that periapse has moved by the "right"
C        amount.
C
         EDPERI =   TWOPI()*1.5D0*MYJ2
     .          * ( 2.5D0*DCOS(ELTS0(3))**2 - 0.5D0 )
     .          *  (RADIUS/MYP)**2
         ADPERI = ELTS1(5) - ELTS0(5)

         CALL CHCKSD ( 'DPERI', ADPERI, '~/', EDPERI, 1.0D-10, OK )



C
C     See that we get the same results for the hyperbolic case.
C
      CALL TCASE ( 'Hyperbolic Orbit' )

         DO I = TYPE15, TYPE53
            RECORD( ITEM(ECC,I), I ) = 2.0D0
         END DO

         ET          = 7200.0D0

         CALL SPKE53 ( ET, RECORD(1,TYPE53), STATE  )
         CALL SPKE15 ( ET, RECORD(1,TYPE15), STATE2 )

         CALL CHCKXC ( .FALSE.,        ' ',                      OK )
         CALL CHCKAD ( 'STATE', STATE, '~/', STATE2, 6, 1.0D-12, OK )


C
C     Make sure the angular momentum is constant.
C
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



C
C     In this case we simply check that adding the velocity times
C     a delta time gives approximately the position.
C
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
