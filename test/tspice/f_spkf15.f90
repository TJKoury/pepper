!$Procedure      F_SPKF15 ( Family of tests for SPKW15 and SPKR15 )

      SUBROUTINE F_SPKF15 ( OK )

! $ Abstract
!
!     This routine test the cited exceptions of SPKW15 and then
!     creates an SPK file containing a single segment of type 15.
!
!     This segment is read using SPKSFS to find the segment and
!     read its contents via SPKR15.
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
!     This routine tests the SPICELIB routine SPKW15 and SPKR15.
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
!     W.L. Taber        (JPL)
!     B.V. Semenov      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 04-FEB-2014 (BVS)
!
!        Initialized everything passed into SPKW15 unfront to
!        prevent floating point exceptions due to uninitilized
!        values.
!
!-    SPICELIB Version 1.1.0, 20-OCT-1999 (WLT)
!
!        Declared PI to be an EXTERNAL Functions.
!
!-&
!
!     Spicelib Functions
!
      EXTERNAL              PI
      DOUBLE PRECISION      PI

!
!     Local Variables.
!

      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )

      INTEGER               NS
      PARAMETER           ( NS = 5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )



      CHARACTER*(8)         FRAME
      CHARACTER*(LNSIZE)    SEGID

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      J2FLG
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      TP ( 3 )
      DOUBLE PRECISION      PA ( 3 )
      DOUBLE PRECISION      PV ( 3 )
      DOUBLE PRECISION      GM
      DOUBLE PRECISION      P
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      J2
      DOUBLE PRECISION      RADIUS

      DOUBLE PRECISION      RECORD ( 20 )
      DOUBLE PRECISION      MYREC  ( 16 )

      DOUBLE PRECISION      DESCR  ( NS )
      DOUBLE PRECISION      DPS    ( ND )

      INTEGER               NUMS  ( NI )
      INTEGER               HANDLE
      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               I
      INTEGER               TYPE
      INTEGER               SPKHAN

      LOGICAL               FOUND



      CALL TOPEN ( 'F_SPKF15' )

!
!     delete any existing test SPK file.
!
      CALL KILFIL ( 't_spkw15.bsp'                    )
      CALL SPCOPN ( 't_spkw15.bsp', 'testSPK', HANDLE )

!
!     Set up a bunch of initial values.
!

      BODY   = -1000
      CENTER = 399
      FRAME  = 'J2000'
      FIRST  =       0.0D0
      LAST   =  100000.0D0
      SEGID  = ' '
      EPOCH  =    1000.0D0
      J2FLG  =       0.0D0
      J2     =       1.082616D-3

      P      =   10000.0D0
      ECC    =       0.1D0
      GM     =  398600.447703261138D0

      TP(1)  =       0.0D0
      TP(2)  =  DCOS(PI()/6.0D0)
      TP(3)  =  DSIN(PI()/6.0D0)

      PA(1)  =       0.0D0
      PA(2)  =  DSIN(PI()/6.0D0)
      PA(3)  = -DCOS(PI()/6.0D0)

      PV(1)  =       0.0D0
      PV(2)  =       0.0D0
      PV(3)  =       1.0D0

      RADIUS =    6378.184D0

      DO I = 1, 20
         RECORD(I) = 1.0D0
      END DO


!
!     The semi-latus rectum is supposed to be positive.  Start
!     out at zero and then set it to something reasonable.
!
      CALL TCASE ( 'Semi-latus rectum exception' )

         P = 0.0D0

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(BADLATUSRECTUM)',       OK   )

         P = 10000.0D0

!
!     Negative eccentricities should produce exceptions.  After
!     checking that this is so set the eccentricity to something
!     yielding a periodic orbit.
!
      CALL TCASE ( 'Eccentricity Exception' )

         ECC = -1.0D0

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(BADECCENTRICITY)',      OK   )

         ECC = 0.1D0

!
!     The central mass must be positive.  Zero or less should
!     trigger an exception. Try zero and -1.  After that we
!     use the mass of the earth.
!
      CALL TCASE ( 'Central Mass Exception --- mass 0' )

         GM = 0.0D0

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(NONPOSITIVEMASS)',      OK   )

      CALL TCASE ( 'Central Mass Exception --- mass -1' )

         GM = -1.0D0

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(NONPOSITIVEMASS)',      OK   )

         GM =  398600.447703261138D0


!
!     Only a zero trajectory pole can produce a problem.  By
!     construction we already have one.
!
      CALL TCASE ( 'Trajectory Pole Exception' )


         TP(1) = 0.0D0
         TP(2) = 0.0D0
         TP(3) = 0.0D0


         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(BADVECTOR)',            OK   )

!
!        Set the trajectory pole to 45 degree inclination
!
         TP(1) = 0.0D0
         TP(2) = DCOS(PI()/6.0D0)
         TP(3) = DSIN(PI()/6.0D0)

!
!     Only a zero periapsis vector yields an exception.  We
!     already have this by construction.  After testing make
!     a periapsis vector that is orthogonal to the trajectory
!     pole vector.
!
      CALL TCASE ( 'Periapsis Vector Exception' )

         PA(1) = 0.0D0
         PA(2) = 0.0D0
         PA(3) = 0.0D0

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(BADVECTOR)',            OK   )


         PA(1) = 0.0D0
         PA(2) = DSIN(PI()/6.0D0)
         PA(3) =-DCOS(PI()/6.0D0)


!
!     Only a zero central body pole vector can yield an exception.
!     We have such a situation by construction.  After checking
!     this, align the pole with the Z axis.
!
      CALL TCASE ( 'Pole Vector Exception' )

         PV(1) = 0.0D0
         PV(2) = 0.0D0
         PV(3) = 0.0D0

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(BADVECTOR)',            OK   )

         PV(3) = 1.0D0


!
!     Anything less than zero should trigger an exception.  After
!     checking, set the equatorial radius to that of the earth.
!
      CALL TCASE ( 'Equatorial Radius Exception' )

         RADIUS = -1.0D0

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(BADRADIUS)',            OK   )

         RADIUS = 6378.184D0



!
!     If the periapse is not nearly perpepndicular to the
!     trajectory pole, we should get an exception.  Create
!     a vector that isn't perpendicular to the trajectory pole
!     by messing up the sign on the z-component.
!
      CALL TCASE ( 'Bad Initial Conditions' )


         PA(1) = 0.0D0
         PA(2) = 1.0D0
         PA(3) = 0.0D0


         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(BADINITSTATE)',         OK   )


         PA(1) = 0.0D0
         PA(2) = DSIN(PI()/6.0D0)
         PA(3) =-DCOS(PI()/6.0D0)


      CALL TCASE ( 'Segment Identifier too long' )

         SEGID = 'This is a very, very, very long segment identifier '


         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(SEGIDTOOLONG)',         OK   )


      CALL TCASE ( 'Non-Printing Characters Exception' )

         SEGID = 'This is a ' // CHAR(9) // 'test segment'

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )
         CALL CHCKXC (.TRUE., 'SPICE(NONPRINTABLECHARS)',    OK   )

         SEGID = 'Test segment'

!
!     That takes care of all noted excpetions in  SPKW15.
!     Write a legitimate segment and close the SPK file.
!

      MYREC( 1) = EPOCH

      MYREC( 2) = TP(1)
      MYREC( 3) = TP(2)
      MYREC( 4) = TP(3)

      MYREC( 5) = PA(1)
      MYREC( 6) = PA(2)
      MYREC( 7) = PA(3)
      MYREC( 8) = P
      MYREC( 9) = ECC

      MYREC(10) = J2FLG

      MYREC(11) = PV(1)
      MYREC(12) = PV(2)
      MYREC(13) = PV(3)
      MYREC(14) = GM
      MYREC(15) = J2
      MYREC(16) = RADIUS


      CALL TCASE ( 'Writing a segment' )

         FIRST =   1000.0D0
         LAST  = 100000.0D0

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )

!
!        In addition we write a bogus segment with the wrong amount
!        of data in it and call it type 15.
!
         FIRST = -100000.0D0
         LAST  =   -1000.0D0
         TYPE  =      15

         CALL SPKPDS ( BODY,  CENTER, FRAME, TYPE, FIRST, LAST, &
                       DESCR        )

         CALL DAFBNA ( HANDLE, DESCR, 'Bogus Segment' )
         CALL DAFADA ( RECORD, 17                     )
         CALL DAFENA
         CALL DAFCLS ( HANDLE )

         CALL CHCKXC (.FALSE., ' ',   OK )

!
!     Corrupt the descriptor in the type component of the segment
!     and make sure that SPKR15 properly diagnoses the problem.
!
      CALL TCASE ( 'SPKR15 bad type exception.' )

         ET = 2000.0D0

         CALL SPKLEF ( 't_spkw15.bsp', SPKHAN )
         CALL SPKSFS ( BODY, ET, HANDLE, DESCR, SEGID, FOUND )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         IF ( OK ) THEN
            CALL DAFUS  ( DESCR, ND, NI, DPS, NUMS        )
            NUMS(4) = 14
            CALL DAFPS  (        ND, NI, DPS, NUMS, DESCR )

            CALL SPKR15 ( HANDLE, DESCR, ET, RECORD )
            CALL CHCKXC (.TRUE., 'SPICE(WRONGSPKTYPE)', OK )
         END IF
!
!     Recall that the second segment we wrote had too much data.
!     and had time bounds from -100000 to -1000.  We find that
!     segment next and make sure that the badly formed segment
!     is handled properly.
!

      CALL TCASE ( 'SPKR15 bad segment exception.' )

         ET = -2000.0D0

         CALL SPKSFS ( BODY, ET, HANDLE, DESCR, SEGID, FOUND  )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,         OK     )

         IF ( OK ) THEN
            CALL SPKR15 ( HANDLE, DESCR, ET,    RECORD )
            CALL CHCKXC (.TRUE., 'SPICE(MALFORMEDSEGMENT)', OK )
         END IF

      CALL TCASE ( 'SPKR15 checking segment values.' )

         ET = 2000.0D0
         CALL SPKSFS ( BODY, ET, HANDLE, DESCR, SEGID, FOUND )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,         OK     )

         IF ( OK ) THEN

            CALL SPKR15 ( HANDLE, DESCR, ET, RECORD )
            CALL CHCKXC ( .FALSE.,          ' ',                    OK )
            CALL CHCKAD ( 'RECORD', RECORD, '=', MYREC, 16, 1.0D-14,OK )

         END IF







!
!     When we've finished all tests, delete the .bsp file we created.
!
      CALL SPKUEF ( SPKHAN )
      CALL KILFIL ( 't_spkw15.bsp' )
      CALL T_SUCCESS( OK )

      RETURN
      END
