C$Procedure F_SPKSPV ( Family of tests for SPKS15 )

      SUBROUTINE F_SPKSPV ( OK )

C$ Abstract
C
C     This routine creates an SPK file containing a single SPK type 15
C     segment. From this it extracts a subsegment. This subsegment
C     is examined for correctness and a state is extracted from
C     and compared with an expected state.
C
C     Routines exercised:
C
C        SPKE15
C        SPKR15
C        SPKS15
C        SPKSUB
C        SPKW15
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
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 05-JUN-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
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
      DOUBLE PRECISION      PI
C
C     Local Variables.
C

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

C
C     Set up a bunch of initial values.
C

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


C
C     Set up the data needed for creating a legitimate segment.
C
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

C
C     Copy the record we expect to be in the SPK file.
C
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


C
C        delete any existing test SPK file.
C
         CALL KILFIL ( 't_spks15.bsp'                    )
         CALL SPCOPN ( 't_spks15.bsp', 'testSPK', HANDLE )

         FIRST =   1000.0D0
         LAST  = 100000.0D0
         ET    = ( FIRST + LAST ) / 2.0D0

         CALL SPKW15 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST,
     .                 SEGID,  EPOCH, TP,     PA,     P,     ECC,
     .                 J2FLG,  PV,    GM,     J2,     RADIUS      )

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
C
C        No errors should have been signaled so far.
C
         CALL CHCKXC (.FALSE., ' ',                     OK   )
C
C        Load the subsetted ephemeris.
C
         CALL SPKUEF (                                   OLDH )
         CALL SPKLEF ( 'spksub15.bsp',                   NEWH )
C
C        Look for data for this item.
C


         CALL SPKSFS (  BODY, ET, HANDLE, DESCR, SEGID,  FOUND  )
         CALL SPKR15 ( HANDLE, DESCR,  ET,               RECORD )
C
C        The data in the record should be identical to the one we
C        stored earlier.
C
         CALL CHCKAD ( 'RECORD', RECORD, '=', MYREC, 16, 0.0D0, OK )

C
C        The stop and end times should be what we set them to earlier.
C
         CALL DAFUS  ( DESCR, ND, NI, DPS, NUMS )

         CALL CHCKSD ( 'FIRST', FIRST, '=', DPS(1), 0.0D0, OK )
         CALL CHCKSD ( 'LAST',  LAST,  '=', DPS(2), 0.0D0, OK )

      CALL TCASE ( 'State Evaluation using SPKPV' )

         CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE, CENTER )
         CALL SPKE15( ET, RECORD, STATE2                         )

         CALL CHCKAD( 'STATE', STATE, '=', STATE2, 6, 0.0D0, OK )

C
C     When we've finished all tests, delete the .bsp files we created.
C
      CALL SPKUEF (  NEWH )
      CALL KILFIL ( 't_spks15.bsp' )
      CALL KILFIL ( 'spksub15.bsp' )
      CALL T_SUCCESS( OK )

      RETURN
      END
