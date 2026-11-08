!$Procedure F_SPK05 ( SPK data type 05 tests )

      SUBROUTINE F_SPK05 ( OK )

! $ Abstract
!
!     Exercise routines associated with SPK data type 05.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
! $ Files
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
! $ Particulars
!
!     This routine tests routines that write and read type 05 SPK
!     data.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 09-NOV-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI
      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         UTC1
      PARAMETER           ( UTC1   = '1999 JUL 1' )

      CHARACTER*(*)         REF1
      PARAMETER           ( REF1   = 'J2000' )


      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'test.bsp' )

      CHARACTER*(*)         SPK05E
      PARAMETER           ( SPK05E = 'test05err.bsp' )

      CHARACTER*(*)         SPK05
      PARAMETER           ( SPK05  = 'test05.bsp' )

      CHARACTER*(*)         SPK05B
      PARAMETER           ( SPK05B = 'test05big.bsp' )

      CHARACTER*(*)         SPK05S
      PARAMETER           ( SPK05S = 'test05sub.bsp' )


      DOUBLE PRECISION      BIGSTP
      PARAMETER           ( BIGSTP = 1.D2 )

!
!     This GM value is from the test utility TSTST.
!
      DOUBLE PRECISION      GMSUN
      PARAMETER           ( GMSUN  = 132712439812.232D0 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-11 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-3 )

      DOUBLE PRECISION      SLOPPY
      PARAMETER           ( SLOPPY = 1.D-1 )



      INTEGER               BIGN
      PARAMETER           ( BIGN   = 10101 )

      INTEGER               BIGID
      PARAMETER           ( BIGID  = 399 )

      INTEGER               BIGCTR
      PARAMETER           ( BIGCTR = 10 )

      INTEGER               BIGDEG
      PARAMETER           ( BIGDEG = 3 )

      INTEGER               CHBDEG
      PARAMETER           ( CHBDEG = 2 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NDSCRT
      PARAMETER           ( NDSCRT = 9 )

      INTEGER               NCHBRC
      PARAMETER           ( NCHBRC = 4 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 60 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local Variables
!
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(SIDLEN)    SEGID2
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(FRNMLN)    XREF

      DOUBLE PRECISION      BADEPC (    NDSCRT )
      DOUBLE PRECISION      BEPLST (    BIGN+1 )
      DOUBLE PRECISION      BSTLST ( 6, BIGN+1 )
      DOUBLE PRECISION      DC     (    ND     )
      DOUBLE PRECISION      DESCR  (    DSCSIZ )
      DOUBLE PRECISION      DSCEPC (    NDSCRT )
      DOUBLE PRECISION      DSCSTS ( 6, NDSCRT )
      DOUBLE PRECISION      DT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      PCOMP  ( 3 )
      DOUBLE PRECISION      PROPST ( 6, 2 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      VCOMP  ( 3 )
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               NEWH
      INTEGER               XBODY
      INTEGER               XCENTR

      LOGICAL               FOUND

!
!     Saved values
!
      SAVE                  BADEPC
      SAVE                  DSCEPC
      SAVE                  DSCSTS


!
!     Initial values
!


!
!     Epochs and states:
!
      DATA  BADEPC / 100.D0, 200.D0,  300.D0, 400.D0, 500.D0, &
                     600.D0, 700.D0, -800.D0, 900.D0         /

      DATA  DSCEPC / 100.D0, 200.D0, 300.D0, 400.D0, 500.D0, &
                     600.D0, 700.D0, 800.D0, 900.D0         /

      DATA  DSCSTS / &
                    101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0, &
                    102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0, &
                    103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0, &
                    104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0, &
                    105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0, &
                    106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0, &
                    107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0, &
                    108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0, &
                    109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0  /



!
!     Open the test family.
!
      CALL TOPEN ( 'F_SPK05' )

!
!     Test SPKW05:  start out with error handling.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW05 error case: bad frame name.'   )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 05 test segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK05E) ) THEN
         CALL DELFIL(SPK05E)
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN( SPK05E, 'Type 05 SPK internal file name', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )


      STEP = DSCEPC(2) - DSCEPC(1)



      CALL SPKW05 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    'SPUD', &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    GMSUN, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC            )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW05 error case: SEGID too long.'   )

      CALL SPKW05 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    'X                                               X', &
                    GMSUN, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC              )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW05 error case: unprintable SEGID characters.' )

      CALL SPKW05 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    CHAR(7), &
                    GMSUN, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC           )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW05 error case: zero GM' )

      CALL SPKW05 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    0.D0, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC         )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEMASS)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW05 error case: negative number of states' )

      CALL SPKW05 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    GMSUN, &
                    -1, &
                    DSCSTS, &
                    DSCEPC         )

      CALL CHCKXC ( .TRUE., 'SPICE(NUMSTATESNOTPOS)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW05 error case: descriptor times swapped.' )

      CALL SPKW05 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(NDSCRT), &
                    DSCEPC(1), &
                    SEGID, &
                    GMSUN, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC           )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW05 error case: epochs non-increasing.' )

      CALL SPKW05 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1) - 1.D0, &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    GMSUN, &
                    NDSCRT, &
                    DSCSTS, &
                    BADEPC          )

      CALL CHCKXC ( .TRUE., 'SPICE(UNORDEREDTIMES)', OK )




!
!     Close the SPK file at the DAF level; SPKCLS won't close
!     a file without segments.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Enough with the error cases; write a segment already.
!
!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Create a type 05 segment.' )


      CALL SPKOPN ( SPK05, 'Type 05 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKW05 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    GMSUN, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC           )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load the SPK file.
!
      CALL SPKLEF ( SPK05, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up states for each epoch in our list.  Compare.
!
      DO I = 1, NDSCRT

         CALL SPKGEO ( XBODY, DSCEPC(I), XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'type 05 state', &
                       STATE, &
                       '~', &
                       DSCSTS(1,I), &
                       6, &
                       TIGHT, &
                       OK             )
      END DO


!
!     Unload the SPK file.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW05 test:  create a large segment with ' // &
                   'multiple directories.'                      )

!
!     Create the state and epoch values we'll use.  Use the
!     standard test SPK file to generate reasonable states.
!
      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL ( SPK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTSPK ( SPK1, .TRUE., HANDLE )

      DO I = 1, BIGN+1

         BEPLST(I) = BIGSTP * I

         CALL SPKGEO ( BIGID, BEPLST(I), 'J2000', BIGCTR, BSTLST(1,I), &
                       LT                                              )

      END DO

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Open a new type 05 SPK file.
!
      IF ( EXISTS(SPK05B) ) THEN
         CALL DELFIL ( SPK05B )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK05B, 'Type 05 SPK internal file name', 0, &
                    HANDLE                                      )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKW05 ( HANDLE, &
                    BIGID, &
                    BIGCTR, &
                    XREF, &
                    BEPLST(1), &
                    BEPLST(BIGN), &
                    SEGID, &
                    GMSUN, &
                    BIGN, &
                    BSTLST, &
                    BEPLST         )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load the SPK file.
!
      CALL SPKLEF ( SPK05B, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up states for each midpoint of adjacent epochs in our list.
!     Compare.
!
      DO I = 1, BIGN-1

         CALL SPKGEO ( BIGID, &
                       BEPLST(I) + BIGSTP/2, &
                       XREF, &
                       BIGCTR, &
                       STATE, &
                       LT                   )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Set up the expected state vector.  Start out by propagating
!        the state at epoch I forward to the midpoint between epoch I
!        and epoch I+1.
!
         DT = BIGSTP / 2

         CALL PROP2B ( GMSUN, BSTLST(1,I),    DT, PROPST(1,1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Propagate the trajectory backward from epoch I+1:
!
         CALL PROP2B ( GMSUN, BSTLST(1,I+1), -DT, PROPST(1,2) )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Combine the positions:
!
         CALL VLCOM ( 0.5D0, PROPST(1,1), &
                      0.5D0, PROPST(1,2), XSTATE )

         CALL CHCKAD ( 'type 05 position', &
                       STATE, &
                       '~~/', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )

!
!        Combine the velocities.  At the midpoint t between the
!        epochs, the velocity should be
!
!           V (t) + V (t)     P (t) - P (t)
!            1       2         2       1
!           -------------  +  ------------ * Pi
!
!                  2           2 * BIGSTP
!

         CALL VLCOM ( 0.5D0,                 PROPST(4,1), &
                      0.5D0,                 PROPST(4,2), VCOMP )

         CALL VLCOM (  5.D-1 * PI()/BIGSTP,  PROPST(1,2), &
                      -5.D-1 * PI()/BIGSTP,  PROPST(1,1), PCOMP )

         CALL VADD  ( VCOMP, PCOMP, XSTATE(4) )

         TITLE = 'type 05 velocity #'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKAD ( TITLE, &
                       STATE(4), &
                       '~~/', &
                       XSTATE(4), &
                       3, &
                       MEDIUM, &
                       OK             )

      END DO


!
!     Unload the SPK file.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS05: write new file having a segment '   // &
                   'created by subsetting small segment from SPK05.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 05 test subset segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK05S) ) THEN
         CALL DELFIL(SPK05S)
      END IF

      CALL SPKOPN ( SPK05S, 'Type 05 SPK internal file name', 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open SPK05 and extract segment descriptor and ID of first segment.
!
      CALL DAFOPR ( SPK05, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a type 05 segment in new file.  Shorten the time
!     coverage by knocking off the coverage contributed by
!     the first and last packets of the source segment.
!
      CALL SPKSUB ( HANDLE, &
                    DESCR, &
                    SEGID, &
                    DSCEPC(2), &
                    DSCEPC(NDSCRT-1), &
                    NEWH               )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the new SPK file.
!
      CALL SPKCLS ( NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the old SPK file.
!
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS05: check descriptor bounds on ' // &
                   'subsetted file.'                          )

!
!     Open SPK05S and extract segment descriptor and ID of first
!     segment.
!
      CALL DAFOPR ( SPK05S, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the time bounds.
!
      I = NDSCRT-1

      CALL CHCKSD ( 'Segment start', DC(1), '=', DSCEPC(2), 0.D0, OK )
      CALL CHCKSD ( 'Segment end',   DC(2), '=', DSCEPC(I), 0.D0, OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test SPKS03: read states from subsetted file.' )

!
!     Load the SPK file.
!
      CALL SPKLEF ( SPK05S, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Look up states for each epoch in our list.  Compare.
!
      DO I = 2, NDSCRT-1

         CALL SPKGEO ( XBODY, DSCEPC(I), XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'type 05 state', &
                       STATE, &
                       '~', &
                       DSCSTS(1,I), &
                       6, &
                       TIGHT, &
                       OK             )
      END DO



!
!     Unload the SPK file.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Clean up:  delete SPK files.' )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK05E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK05 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK05B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK05S )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


