C$Procedure F_SPK18 ( SPK data type 18 tests )

      SUBROUTINE F_SPK18 ( OK )

C$ Abstract
C
C     Exercise routines associated with SPK data type 18.
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

      INCLUDE 'spk18.inc'

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
C     OK         is a logical that indicates the test status to the
C                caller.
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
C     This routine tests routines that write and read type 18 SPK
C     data.
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.2.0, 13-OCT-2021 (NJB)
C
C        Bug fix: now correctly sets bogus velocity and acceleration
C        data derived from states in array DSCSTS.
C
C-    TSPICE Version 1.1.0, 21-DEC-2012 (NJB) (EDW)
C
C        Bug fix: now tests for correct number of states in output
C        segment for small segments. A new SPK file is created for
C        use as an input file by SPKS18; this file contains small
C        segments suitable for testing pad creation logic for small
C        segments.
C
C        An SPKW18 error test was added for the case of the packet
C        count being 1.
C
C        Unused variable and parameter declarations were deleted.
C
C        References to SPKS03 in test case titles have been corrected.
C
C        All variables to SAVE for f2c'd version compiled with Borland.
C        Implemented a proper SPICE header.
C
C-    TSPICE Version 1.0.0, 07-OCT-2005 (NJB)
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Local Parameters
C

      CHARACTER*(*)         SPK18E
      PARAMETER           ( SPK18E = 'test18err.bsp' )

      CHARACTER*(*)         SP18T0
      PARAMETER           ( SP18T0  = 'sp18t0.bsp' )

      CHARACTER*(*)         SP18T1
      PARAMETER           ( SP18T1  = 'sp18t1.bsp' )

      CHARACTER*(*)         SP18B0
      PARAMETER           ( SP18B0 = 'sp18big0.bsp' )

      CHARACTER*(*)         SP18B1
      PARAMETER           ( SP18B1 = 'sp18big1.bsp' )

      CHARACTER*(*)         SP18SM
      PARAMETER           ( SP18SM = 'sp18small.bsp' )

      CHARACTER*(*)         SP18S0
      PARAMETER           ( SP18S0 = 'sp18sub0.bsp' )

      CHARACTER*(*)         SP18S1
      PARAMETER           ( SP18S1 = 'sp18sub1.bsp' )


      DOUBLE PRECISION      BIGSTP
      PARAMETER           ( BIGSTP = 1.D1 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


      INTEGER               BIGN
      PARAMETER           ( BIGN   = 10101 )

      INTEGER               BIGID
      PARAMETER           ( BIGID  = -10000 )

      INTEGER               BIGCTR
      PARAMETER           ( BIGCTR = 5 )

      INTEGER               BIGDEG
      PARAMETER           ( BIGDEG = 3 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NDSCRT
      PARAMETER           ( NDSCRT = 9 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               POLDEG
      PARAMETER           ( POLDEG = 3 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 60 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Local Variables
C
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(SIDLEN)    SEGID2
      CHARACTER*(FRNMLN)    XREF

      DOUBLE PRECISION      BEPLST (     BIGN   )
      DOUBLE PRECISION      BT0LST ( 12, BIGN   )
      DOUBLE PRECISION      BT1LST ( 6,  BIGN   )
      DOUBLE PRECISION      DC     (     ND     )
      DOUBLE PRECISION      DESCR  (     DSCSIZ )
      DOUBLE PRECISION      DPVAL
      DOUBLE PRECISION      DSCEPC (     NDSCRT )
      DOUBLE PRECISION      DSCSTS ( 6,  NDSCRT )
      DOUBLE PRECISION      DSCPAK ( 12, NDSCRT )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               DEGREE
      INTEGER               EADDR
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               N
      INTEGER               NEWH
      INTEGER               XBODY
      INTEGER               XCENTR

      LOGICAL               FOUND

C
C     Saved values
C
      SAVE


C
C     Initial values
C


C
C     Epochs and states:
C
      DATA  DSCEPC / 100.D0, 200.D0, 300.D0, 400.D0, 500.D0,
     .               600.D0, 700.D0, 800.D0, 900.D0         /

      DATA  DSCSTS /
     .              101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0,
     .              102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0,
     .              103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0,
     .              104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0,
     .              105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0,
     .              106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0,
     .              107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0,
     .              108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0,
     .              109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0  /



C
C     Open the test family.
C
      CALL TOPEN ( 'F_SPK18' )

C
C     Test SPKW18:  start out with error handling.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 error case: bad frame name.'   )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Create a segment identifier.
C
      SEGID = 'SPK type 18 test segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SPK18E) ) THEN
         CALL DELFIL(SPK18E)
      END IF

      CALL SPKOPN( SPK18E, 'Type 18 SPK internal file name', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )


      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              'SPUD',
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC            )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 error case: SEGID too long.'   )

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              'X                                               X',
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC              )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 error case: unprintable SEGID characters.' )

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              CHAR(7),
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC           )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 error case: polynomial degree too high.' )

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              25,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC         )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 error case: polynomial degree too low.' )

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              0,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC         )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 error case:  odd window size.' )


      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              4,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 error case: too few states' )

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(1),
     .              SEGID,
     .              POLDEG,
     .              0,
     .              DSCSTS,
     .              DSCEPC         )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWSTATES)', OK )

      CALL TCASE ( 'SPKW18 error case: too few states' )

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(1),
     .              SEGID,
     .              POLDEG,
     .              1,
     .              DSCSTS,
     .              DSCEPC         )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWSTATES)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 error case: descriptor times swapped.' )

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(NDSCRT),
     .              DSCEPC(1),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC           )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 error case: descriptor start time is ' //
     .             'too early.'                                   )

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1) - 1.D0,
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC          )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 error case: descriptor end time is ' //
     .             'too late.'                                  )

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT) + 1.D0,
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC               )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )


C
C     Close the SPK file at the DAF level; SPKCLS won't close
C     a file without segments.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Enough with the error cases; write a segment already.
C
C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Create a type 18 subtype 1 segment.' )


      CALL SPKOPN ( SP18T1, 'Type 18 SPK internal file name', 4,
     .              HANDLE                                       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC           )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SP18T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up states for each epoch in our list.  Compare.
C
      DO I = 1, NDSCRT

         CALL SPKGEO ( XBODY, DSCEPC(I), XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'type 18 state',
     .                 STATE,
     .                 '~',
     .                 DSCSTS(1,I),
     .                 6,
     .                 TIGHT,
     .                 OK             )
      END DO


C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Repeat this test using subtype 0.
C

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Create a type 18 subtype 0 segment.' )


      CALL SPKOPN ( SP18T0, 'Type 18 SPK internal file name', 4,
     .              HANDLE                                       )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fill in the packets using the discrete state set.  Make the
C     velocity/acceleration data 10x the position/velocity data.  Note
C     that these two data sets are independent; realistic
C     correspondence between them is not required.
C
      DO I = 1, NDSCRT

         CALL MOVED (        DSCSTS(1,I), 6, DSCPAK(1,I) )

         CALL VSCLG ( 10.D0, DSCSTS(1,I), 6, DSCPAK(7,I) )

      END DO

      CALL SPKW18 ( HANDLE,
     .              S18TP0,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCPAK,
     .              DSCEPC           )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SP18T0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up states for each epoch in our list.  Compare.
C
      DO I = 1, NDSCRT

         CALL SPKGEO ( XBODY, DSCEPC(I), XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL MOVED (        DSCSTS(1,I), 3, XSTATE(1) )
         CALL VSCL  ( 10.D0, DSCSTS(1,I),    XSTATE(4) )


         CALL CHCKAD ( 'type 18 state',
     .                 STATE,
     .                 '~',
     .                 XSTATE,
     .                 6,
     .                 TIGHT,
     .                 OK             )
      END DO


C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 test:  create a large subtype 1 ' //
     .             'segment with multiple directories.' )

C
C     Create the state and epoch values we'll use.
C
      DO I = 1, BIGN

         DO J = 1, 6

            BT1LST(J,I) = BIGSTP*I + J

         END DO

         BEPLST(I) = 10.D0 * I

      END DO

C
C     Open a new type 18 SPK file.
C
      IF ( EXISTS(SP18B1) ) THEN
         CALL DELFIL ( SP18B1 )
      END IF

      CALL SPKOPN ( SP18B1, 'Type 18 SPK internal file name', 0,
     .              HANDLE                                      )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              BIGID,
     .              BIGCTR,
     .              XREF,
     .              BEPLST(1),
     .              BEPLST(BIGN),
     .              SEGID,
     .              BIGDEG,
     .              BIGN,
     .              BT1LST,
     .              BEPLST         )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SP18B1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up states for each midpoint of adjacent epochs in our list.
C     Compare.
C
      DO I = 1, BIGN-1

         CALL SPKGEO ( BIGID,
     .                 BEPLST(I) + BIGSTP/2,
     .                 XREF,
     .                 BIGCTR,
     .                 STATE,
     .                 LT                   )

         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Set up the expected state vector.
C
         CALL MOVED ( BT1LST(1,I), 6, XSTATE )

         DO J = 1, 6

            XSTATE(J) = XSTATE(J) + BIGSTP / 2

         END DO

         CALL CHCKAD ( 'type 18 position',
     .                 STATE,
     .                 '~',
     .                 XSTATE,
     .                 3,
     .                 TIGHT,
     .                 OK             )

         CALL CHCKAD ( 'type 18 velocity',
     .                 STATE(4),
     .                 '~',
     .                 XSTATE(4),
     .                 3,
     .                 TIGHT,
     .                 OK             )
      END DO


C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW18 test:  create a large subtype 0 ' //
     .             'segment with multiple directories.' )

C
C     Create the state and epoch values we'll use. We're going to set
C     all velocities to zero to create a rounded stair-step sort of
C     pattern in the position components. This will ensure that the
C     correct states cannot be obtained without selecting the correct
C     window of states in the reader.
C
C     For velocity and acceleration, we'll use the same idea, but we'll
C     scale the values to distinguish them.
C
      DO I = 1, BIGN

         DO J = 1, 3

            BT0LST(J,  I) = BIGSTP*I + J
            BT0LST(J+3,I) = 0.D0
            BT0LST(J+6,I) = 1.D6 * BT0LST(J,I)
            BT0LST(J+9,I) = 0.D0

         END DO

         BEPLST(I) = 10.D0 * I

      END DO

C
C     Open a new type 18 SPK file.
C
      IF ( EXISTS(SP18B0) ) THEN
         CALL DELFIL ( SP18B0)
      END IF

      CALL SPKOPN ( SP18B0, 'Type 18 SPK internal file name', 0,
     .              HANDLE                                       )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKW18 ( HANDLE,
     .              S18TP0,
     .              BIGID,
     .              BIGCTR,
     .              XREF,
     .              BEPLST(1),
     .              BEPLST(BIGN),
     .              SEGID,
     .              BIGDEG,
     .              BIGN,
     .              BT0LST,
     .              BEPLST         )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SP18B0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up states for each midpoint of adjacent epochs in our list.
C     Compare.
C
      DO I = 1, BIGN-1

         CALL SPKGEO ( BIGID,
     .                 BEPLST(I) + BIGSTP/2,
     .                 XREF,
     .                 BIGCTR,
     .                 STATE,
     .                 LT                   )

         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Set up the expected state vector.
C
         CALL MOVED ( BT0LST(1,I), 3, XSTATE )

         DO J = 1, 3

            XSTATE(J  ) = XSTATE(J) + BIGSTP / 2
            XSTATE(J+3) = 1.D6 * XSTATE(J)

         END DO

         CALL CHCKAD ( 'type 18 position',
     .                 STATE,
     .                 '~',
     .                 XSTATE,
     .                 3,
     .                 TIGHT,
     .                 OK             )

         CALL CHCKAD ( 'type 18 velocity',
     .                 STATE(4),
     .                 '~',
     .                 XSTATE(4),
     .                 3,
     .                 TIGHT,
     .                 OK             )
      END DO


C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
C
C     Create an SPK file suitable for use as an input file
C     for SPKS18. This file will contain segments of subtypes
C     0 and 1.
C
      CALL TCASE ( 'Create a type 18 SPK for subset testing. '
     .//           'This file contains segments of subtypes '
     .//           '0 and 1.'                                 )


      CALL SPKOPN ( SP18SM, 'Type 18 SPK internal file name', 4,
     .              HANDLE                                       )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add first segment, which has subtype 1 and degree 15.
C
      DEGREE = 15

      CALL SPKW18 ( HANDLE,
     .              S18TP1,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              DEGREE,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC           )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add second segment, which has subtype 0 and degree 15.
C     This file contains only 5 packets.
C
C     We must use a different body for this segment.
C
C     Note the use of DSCPAK rather than DSKSTS. DSKPAK
C     was initialized earlier.
C
      XBODY = 4

      CALL SPKW18 ( HANDLE,
     .              S18TP0,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(5),
     .              DSCEPC(7),
     .              SEGID,
     .              DEGREE,
     .              5,
     .              DSCPAK(1,4),
     .              DSCEPC(4)        )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS18: write new file having a subtype 1 '
     .//           'segment created by subsetting small segment '
     .//           'from SP18SM.'                                )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Create a segment identifier.
C
      SEGID = 'SPK type 18 test subset segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SP18S1) ) THEN
         CALL DELFIL(SP18S1)
      END IF

      CALL SPKOPN ( SP18S1, 'Type 18 SPK internal file name', 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Open SP18T1 and extract segment descriptor and ID of first
C     segment.
C
      CALL DAFOPR ( SP18SM, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a type 18 segment in new file.  Shorten the time
C     coverage by knocking off the coverage contributed by
C     the first and last packets of the source segment.
C
      CALL SPKSUB ( HANDLE,
     .              DESCR,
     .              SEGID,
     .              DSCEPC(2),
     .              DSCEPC(NDSCRT-1),
     .              NEWH               )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the new SPK file.
C
      CALL SPKCLS ( NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the old SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS18: check descriptor bounds on '
     .//           'subsetted file (subtype 1).'               )

C
C     Open SP18S1 and extract segment descriptor and ID of first
C     segment.
C
      CALL DAFOPR ( SP18S1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the time bounds.
C
      I = NDSCRT-1

      CALL CHCKSD ( 'Segment start', DC(1), '=', DSCEPC(2), 0.D0, OK )
      CALL CHCKSD ( 'Segment end',   DC(2), '=', DSCEPC(I), 0.D0, OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS18: check state count in subsetted '
     .//           'file (subtype 1).' )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SP18S1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get descriptor of first segment.
C
      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      EADDR = IC(6)

      CALL DAFGDA ( HANDLE, EADDR, EADDR, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = NINT(DPVAL )

      CALL CHCKSI ( 'N', N, '=', NDSCRT, 0, OK )

C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS18: read states from subsetted file '
     .//           '(subtype 1).'                                )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SP18S1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Look up states for each epoch in our list.  Compare.
C
      DO I = 4, 8

         CALL SPKGEO ( XBODY, DSCEPC(I), XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'type 18 position',
     .                 STATE,
     .                 '~',
     .                 DSCSTS(1,I),
     .                 3,
     .                 TIGHT,
     .                 OK             )

         CALL CHCKAD ( 'type 18 velocity',
     .                 STATE(4),
     .                 '~',
     .                 DSCSTS(4,I),
     .                 3,
     .                 TIGHT,
     .                 OK             )

      END DO



C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Repeat subsetting tests for a subtype 0 segment.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS18: write new file having a segment '   //
     .             'created by subsetting small segment from SP18SM.'//
     .             'The output segment has subtype 0.'               )


      XBODY  = 4
      XCENTR = 10
      XREF   = 'J2000'

C
C     Create a segment identifier.
C
      SEGID = 'SPK type 18 test subset segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SP18S0) ) THEN
         CALL DELFIL(SP18S0)
      END IF

      CALL SPKOPN ( SP18S0, 'Type 18 SPK internal file name', 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Open SP18SM and extract segment descriptor and ID of second
C     segment.
C
      CALL DAFOPR ( SP18SM, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, 2
         CALL DAFFNA ( FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a type 18 segment in new file.  Shorten the time
C     coverage by knocking off the coverage contributed by
C     the first and last packets of the source segment.
C
      CALL SPKSUB ( HANDLE,
     .              DESCR,
     .              SEGID,
     .              DSCEPC(5),
     .              DSCEPC(7),
     .              NEWH               )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the new SPK file.
C
      CALL SPKCLS ( NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the old SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS18: check descriptor bounds on ' //
     .             'subsetted file (subtype 0).'              )

C
C     Open SP18S0 and extract segment descriptor and ID of first
C     segment.
C
      CALL DAFOPR ( SP18S0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the time bounds.
C
      CALL CHCKSD ( 'Segment start', DC(1), '=', DSCEPC(5), 0.D0, OK )
      CALL CHCKSD ( 'Segment end',   DC(2), '=', DSCEPC(7), 0.D0, OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS18: check state count in subsetted '
     .//           'file (subtype 0).' )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SP18S0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get descriptor of first segment.
C
      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      EADDR = IC(6)

      CALL DAFGDA ( HANDLE, EADDR, EADDR, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = NINT(DPVAL )

      CALL CHCKSI ( 'N', N, '=', 5, 0, OK )

C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS18: read states from subsetted file '
     .//           '(subtype 0).'                                 )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SP18SM, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 4
      XCENTR = 10
      XREF   = 'J2000'

C
C     Look up states for each epoch in our list.  Compare.
C
      DO I = 5, 7

         CALL SPKGEO ( XBODY, DSCEPC(I), XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'type 18 position',
     .                 STATE,
     .                 '~',
     .                 DSCPAK(1,I),
     .                 3,
     .                 TIGHT,
     .                 OK             )

         CALL CHCKAD ( 'type 18 velocity',
     .                 STATE(4),
     .                 '~',
     .                 DSCPAK(7,I),
     .                 3,
     .                 TIGHT,
     .                 OK             )
      END DO



C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Clean up:  delete SPK files.' )

      CALL DELFIL ( SPK18E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SP18T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SP18T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SP18B0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SP18B1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SP18S0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SP18S1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SP18SM )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
