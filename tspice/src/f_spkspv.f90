!$Procedure F_SPKSPV ( Family of tests for SPKS15 )

      SUBROUTINE F_SPKSPV ( OK )
      use pepper_legacy, pepper_legacy_frame => frame

! $ Abstract
!
!     This routine creates an SPK file containing a single SPK type 15
!     segment. From this it extracts a subsegment. This subsegment
!     is examined for correctness and a state is extracted from
!     and compared with an expected state.
!
!     Routines exercised:
!
!        SPKE15
!        SPKR15
!        SPKS15
!        SPKSUB
!        SPKW15
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
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 05-JUN-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
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

      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE2 ( 6 )

      INTEGER               NUMS  ( NI )
      INTEGER               HANDLE
      INTEGER               OLDH
      INTEGER               NEWH

      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               I

      LOGICAL               FOUND



      CALL TOPEN ( 'F_SPKSPV' )

!
!     Set up a bunch of initial values.
!

      BODY   = -1000
      CENTER = 399
      FRAME  = 'J2000'
      FIRST  = 0.0D0
      LAST   = 100000.0D0
      SEGID  = ' '
      EPOCH  = 1000.0D0
      J2FLG  = 0.0D0
      J2     = 1.082616D-3


      DO I = 1, 20
         RECORD(I) = 1.0D0
      END DO


!
!     Set up the data needed for creating a legitimate segment.
!
      BODY   = -1000
      CENTER =   399

      FRAME  = 'J2000'

      FIRST  =      0.0D0
      LAST   = 100000.0D0

      SEGID  = ' '

      EPOCH  =   1000.0D0
      J2FLG  =      0.0D0
      J2     =     1.082616D-3

      P     =  10000.0D0
      ECC   =      0.1D0
      GM    = 398600.447703261138D0

      TP(1) =       0.0D0
      TP(2) = DCOS(PI()/6.0D0)
      TP(3) = DSIN(PI()/6.0D0)

      PA(1) =       0.0D0
      PA(2) = DSIN(PI()/6.0D0)
      PA(3) =-DCOS(PI()/6.0D0)

      PV(1) =      0.0D0
      PV(2) =      0.0D0
      PV(3) =      1.0D0
      RADIUS =  6378.184D0



      SEGID  = 'Test segment'

!
!     Copy the record we expect to be in the SPK file.
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


      CALL TCASE ( 'Bad Time range exception.' )


!
!        delete any existing test SPK file.
!
         CALL KILFIL ( 't_spks15.bsp'                    )
         CALL SPCOPN ( 't_spks15.bsp', 'testSPK', HANDLE )

         FIRST =   1000.0D0
         LAST  = 100000.0D0
         ET    = ( FIRST + LAST ) / 2.0D0

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, &
                       SEGID,  EPOCH, TP,     PA,     P,     ECC, &
                       J2FLG,  PV,    GM,     J2,     RADIUS      )

         CALL DAFCLS ( HANDLE            )
         CALL CHCKXC (.FALSE., ' ',   OK )



         CALL SPKLEF ( 't_spks15.bsp', OLDH )
         CALL SPKSFS ( BODY, ET, OLDH, DESCR, SEGID, FOUND )

         CALL KILFIL ( 'spksub15.bsp'                      )
         CALL SPCOPN ( 'spksub15.bsp', 'testSPK',     NEWH )

         FIRST = FIRST - 1000.0D0
         LAST  = LAST  + 1000.0D0

         CALL SPKSUB ( OLDH, DESCR, SEGID, FIRST, LAST, NEWH )
         CALL CHCKXC (.TRUE., 'SPICE(SPKNOTASUBSET)',   OK   )

      CALL TCASE ( 'Extracting a subsegment' )

         FIRST = FIRST + 2000.0D0
         LAST  = LAST  - 2000.0D0

         CALL SPKSUB ( OLDH, DESCR, SEGID, FIRST, LAST, NEWH )
         CALL DAFCLS ( NEWH )
!
!        No errors should have been signaled so far.
!
         CALL CHCKXC (.FALSE., ' ',                     OK   )
!
!        Load the subsetted ephemeris.
!
         CALL SPKUEF (                                   OLDH )
         CALL SPKLEF ( 'spksub15.bsp',                   NEWH )
!
!        Look for data for this item.
!


         CALL SPKSFS (  BODY, ET, HANDLE, DESCR, SEGID,  FOUND  )
         CALL SPKR15 ( HANDLE, DESCR,  ET,               RECORD )
!
!        The data in the record should be identical to the one we
!        stored earlier.
!
         CALL CHCKAD ( 'RECORD', RECORD, '=', MYREC, 16, 0.0D0, OK )

!
!        The stop and end times should be what we set them to earlier.
!
         CALL DAFUS  ( DESCR, ND, NI, DPS, NUMS )

         CALL CHCKSD ( 'FIRST', FIRST, '=', DPS(1), 0.0D0, OK )
         CALL CHCKSD ( 'LAST',  LAST,  '=', DPS(2), 0.0D0, OK )

      CALL TCASE ( 'State Evaluation using SPKPV' )

         CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE, CENTER )
         CALL SPKE15( ET, RECORD, STATE2                         )

         CALL CHCKAD( 'STATE', STATE, '=', STATE2, 6, 0.0D0, OK )

!
!     When we've finished all tests, delete the .bsp files we created.
!
      CALL SPKUEF (  NEWH )
      CALL KILFIL ( 't_spks15.bsp' )
      CALL KILFIL ( 'spksub15.bsp' )
      CALL T_SUCCESS( OK )

      RETURN
      END
