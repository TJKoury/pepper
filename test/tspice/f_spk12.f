C$Procedure F_SPK12 ( SPK data type 12 tests )
 
      SUBROUTINE F_SPK12 ( OK )
 
C$ Abstract
C
C     Exercise routines associated with SPK data type 12.
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
C     This routine tests routines that write and read type 12 SPK 
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
C-    TSPICE Version 2.0.0, 18-JAN-2014 (NJB)
C
C        Updated to use new short error messages for coverage gaps.
C        Added test cases for acceptable coverage gaps. Deleted
C        declarations of unused variables. Re-ordered header sections.
C
C-    TSPICE Version 1.0.0, 29-SEP-2005 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Local Parameters
C
      CHARACTER*(*)         SPK12E
      PARAMETER           ( SPK12E = 'test12err.bsp' )

      CHARACTER*(*)         SPK12
      PARAMETER           ( SPK12  = 'test12.bsp' )

      CHARACTER*(*)         SPK12B
      PARAMETER           ( SPK12B = 'test12big.bsp' )

      CHARACTER*(*)         SPK12S
      PARAMETER           ( SPK12S = 'test12sub.bsp' )


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

      DOUBLE PRECISION      BEPLST (    BIGN   )
      DOUBLE PRECISION      BSTLST ( 6, BIGN   )
      DOUBLE PRECISION      DC     (    ND     )
      DOUBLE PRECISION      DESCR  (    DSCSIZ )
      DOUBLE PRECISION      DSCEPC (    NDSCRT )
      DOUBLE PRECISION      DSCSTS ( 6, NDSCRT )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               NEWH
      INTEGER               XBODY
      INTEGER               XCENTR

      LOGICAL               FOUND


C
C     Saved values
C
      SAVE                  DSCEPC
      SAVE                  DSCSTS


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
      CALL TOPEN ( 'F_SPK12' )

C
C     Test SPKW12:  start out with error handling.
C     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: bad frame name.'   )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Create a segment identifier.
C
      SEGID = 'SPK type 12 test segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SPK12E) ) THEN
         CALL DELFIL(SPK12E)
      END IF

      CALL SPKOPN( SPK12E, 'Type 12 SPK internal file name', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )


      STEP = DSCEPC(2) - DSCEPC(1)



      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              'SPUD',
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1),
     .              STEP             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: SEGID too long.'   )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              'X                                               X',
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1),
     .              STEP             )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: unprintable SEGID characters.' )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              CHAR(7),
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1),
     .              STEP             )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: polynomial degree too high.' )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              29,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1),
     .              STEP             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: polynomial degree too low.' )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              0,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1),
     .              STEP             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: even polynomial degree.' )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              4,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1),
     .              STEP             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: too few states' )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              1,
     .              DSCSTS,
     .              DSCEPC(1),
     .              STEP             )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWSTATES)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: descriptor times swapped.' )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(NDSCRT),
     .              DSCEPC(1),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1),
     .              STEP             )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: descriptor start time is ' //
     .             'too early.'                                   )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1) + 1.D0,
     .              STEP             )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1) * ( 1.D0 + 1.D-14 ),
     .              STEP             )

      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: descriptor end time is ' //
     .             'too late.'                                  )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT) + 1.D0,
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1),
     .              STEP             )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1) * ( 1.D0 - 1.D-14 ),
     .              STEP             )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 error case: bad step size.' )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1),
     .              -1.D0             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEPSIZE)', OK )

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

      CALL TCASE ( 'Create a type 12 segment.' )


      CALL SPKOPN ( SPK12, 'Type 12 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKW12 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NDSCRT),
     .              SEGID,
     .              POLDEG,
     .              NDSCRT,
     .              DSCSTS,
     .              DSCEPC(1),
     .              STEP             )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SPK12, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up states for each epoch in our list.  Compare.
C
      DO I = 1, NDSCRT

         CALL SPKGEO ( XBODY, DSCEPC(I), XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'type 12 state',
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
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW12 test:  create a large segment with ' //
     .             'multiple directories.'                      )

C
C     Create the state and epoch values we'll use. We're going to set
C     all velocities to zero to create a rounded stair-step sort of
C     pattern in the position components. This will ensure that the
C     correct states cannot be obtained without selecting the correct
C     window of states in the reader.
C
      DO I = 1, BIGN

         DO J = 1, 3 

            BSTLST(J,  I) = BIGSTP*I + J
            BSTLST(J+3,I) = 0.D0

         END DO

         BEPLST(I) = 10.D0 * I

      END DO

C
C     Open a new type 12 SPK file.
C
      IF ( EXISTS(SPK12B) ) THEN
         CALL DELFIL ( SPK12B )
      END IF

      CALL SPKOPN ( SPK12B, 'Type 12 SPK internal file name', 0,
     .              HANDLE                                      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      
      CALL SPKW12 ( HANDLE,
     .              BIGID,
     .              BIGCTR,
     .              XREF,
     .              BEPLST(1),
     .              BEPLST(BIGN),
     .              SEGID,
     .              BIGDEG,
     .              BIGN,
     .              BSTLST,
     .              BEPLST(1),
     .              BIGSTP             )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
              
C
C     Load the SPK file.
C
      CALL SPKLEF ( SPK12B, HANDLE )
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
C        Set up the expected state vector. The derivative 1.5 was
C        obtained by differentiating the cubic -2x**3 +3x**2 at x =
C        0.5; this cubic has the same shape (but smaller scale) as the
C        cubic that actually fits the state data.
C
         CALL MOVED ( BSTLST(1,I), 6, XSTATE )

         DO J = 1, 3

            XSTATE(J  ) = XSTATE(J) + BIGSTP / 2
            XSTATE(J+3) = 1.5D0

         END DO

         CALL CHCKAD ( 'type 12 state',
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
      CALL TCASE ( 'Test SPKS12: write new file having a segment '   // 
     .             'created by subsetting small segment from SPK12.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Create a segment identifier.
C
      SEGID = 'SPK type 12 test subset segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SPK12S) ) THEN
         CALL DELFIL(SPK12S)
      END IF

      CALL SPKOPN ( SPK12S, 'Type 12 SPK internal file name', 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Open SPK12 and extract segment descriptor and ID of first segment.
C
      CALL DAFOPR ( SPK12, HANDLE )
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
C     Create a type 12 segment in new file.  Shorten the time
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
      CALL TCASE ( 'Test SPKS12: check descriptor bounds on ' //
     .             'subsetted file.'                          )

C
C     Open SPK12S and extract segment descriptor and ID of first
C     segment.
C
      CALL DAFOPR ( SPK12S, HANDLE )
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
      CALL TCASE ( 'Test SPKS03: read states from subsetted file.' )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SPK12S, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Look up states for each epoch in our list.  Compare.
C
      DO I = 2, NDSCRT-1

         CALL SPKGEO ( XBODY, DSCEPC(I), XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'type 12 state',
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
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Clean up:  delete SPK files.' )

      CALL DELFIL ( SPK12E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK12 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK12B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK12S )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


