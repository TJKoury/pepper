!$Procedure F_SPK13 ( SPK data type 13 tests )

      SUBROUTINE F_SPK13 ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise routines associated with SPK data type 13.
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
!     This routine tests routines that write and read type 13 SPK
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
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 24-DEC-2013 (NJB)
!
!        Modified SPKW13 test case for degree out
!        of range. This was done to accommodate an
!        increase in the maximum allowed interpolation
!        degree.
!
!        Deleted unnecessary variable declarations and
!        references.
!
!        Re-ordered header sections.
!
!-    TSPICE Version 1.0.0, 07-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      CHARACTER*(*)         SPK13E
      PARAMETER           ( SPK13E = 'test13err.bsp' )

      CHARACTER*(*)         SPK13
      PARAMETER           ( SPK13  = 'test13.bsp' )

      CHARACTER*(*)         SPK13B
      PARAMETER           ( SPK13B = 'test13big.bsp' )

      CHARACTER*(*)         SPK13S
      PARAMETER           ( SPK13S = 'test13sub.bsp' )


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

!
!     Local Variables
!
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
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               NEWH
      INTEGER               XBODY
      INTEGER               XCENTR

      LOGICAL               FOUND

!
!     Saved values
!
      SAVE                  DSCEPC
      SAVE                  DSCSTS


!
!     Initial values
!


!
!     Epochs and states:
!
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
      CALL TOPEN ( 'F_SPK13' )

!
!     Test SPKW13:  start out with error handling.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW13 error case: bad frame name.'   )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 13 test segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK13E) ) THEN
         CALL DELFIL(SPK13E)
      END IF

      CALL SPKOPN( SPK13E, 'Type 13 SPK internal file name', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )


      CALL SPKW13 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    'SPUD', &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    POLDEG, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC            )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW13 error case: SEGID too long.'   )

      CALL SPKW13 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    'X                                               X', &
                    POLDEG, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC              )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW13 error case: unprintable SEGID characters.' )

      CALL SPKW13 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    CHAR(7), &
                    POLDEG, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC           )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW13 error case: polynomial degree too high.' )

      CALL SPKW13 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    31, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC         )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW13 error case: even polynomial degree.' )

      CALL SPKW13 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    4, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW13 error case: too few states' )

      CALL SPKW13 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    POLDEG, &
                    1, &
                    DSCSTS, &
                    DSCEPC         )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWSTATES)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW13 error case: descriptor times swapped.' )

      CALL SPKW13 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(NDSCRT), &
                    DSCEPC(1), &
                    SEGID, &
                    POLDEG, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC           )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW13 error case: descriptor start time is ' // &
                   'too early.'                                   )

      CALL SPKW13 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1) - 1.D0, &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    POLDEG, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC          )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW13 error case: descriptor end time is ' // &
                   'too late.'                                  )

      CALL SPKW13 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT) + 1.D0, &
                    SEGID, &
                    POLDEG, &
                    NDSCRT, &
                    DSCSTS, &
                    DSCEPC               )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )


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

      CALL TCASE ( 'Create a type 13 segment.' )


      CALL SPKOPN ( SPK13, 'Type 13 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKW13 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    DSCEPC(1), &
                    DSCEPC(NDSCRT), &
                    SEGID, &
                    POLDEG, &
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
      CALL SPKLEF ( SPK13, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up states for each epoch in our list.  Compare.
!
      DO I = 1, NDSCRT

         CALL SPKGEO ( XBODY, DSCEPC(I), XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'type 13 state', &
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
      CALL TCASE ( 'SPKW13 test:  create a large segment with ' // &
                   'multiple directories.'                      )

!
!     Create the state and epoch values we'll use. We're going to set
!     all velocities to zero to create a rounded stair-step sort of
!     pattern in the position components. This will ensure that the
!     correct states cannot be obtained without selecting the correct
!     window of states in the reader.
!
      DO I = 1, BIGN

         DO J = 1, 3

            BSTLST(J,  I) = BIGSTP*I + J
            BSTLST(J+3,I) = 0.D0

         END DO

         BEPLST(I) = 10.D0 * I

      END DO

!
!     Open a new type 13 SPK file.
!
      IF ( EXISTS(SPK13B) ) THEN
         CALL DELFIL ( SPK13B )
      END IF

      CALL SPKOPN ( SPK13B, 'Type 13 SPK internal file name', 0, &
                    HANDLE                                      )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKW13 ( HANDLE, &
                    BIGID, &
                    BIGCTR, &
                    XREF, &
                    BEPLST(1), &
                    BEPLST(BIGN), &
                    SEGID, &
                    BIGDEG, &
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
      CALL SPKLEF ( SPK13B, HANDLE )
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
!        Set up the expected state vector. The derivative 1.5 was
!        obtained by differentiating the cubic -2x**3 +3x**2 at x =
!        0.5; this cubic has the same shape (but smaller scale) as the
!        cubic that actually fits the state data.
!
         CALL MOVED ( BSTLST(1,I), 6, XSTATE )

         DO J = 1, 3

            XSTATE(J  ) = XSTATE(J) + BIGSTP / 2
            XSTATE(J+3) = 1.5D0

         END DO

         CALL CHCKAD ( 'type 13 position', &
                       STATE, &
                       '~', &
                       XSTATE, &
                       3, &
                       TIGHT, &
                       OK             )

         CALL CHCKAD ( 'type 13 velocity', &
                       STATE(4), &
                       '~', &
                       XSTATE(4), &
                       3, &
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
      CALL TCASE ( 'Test SPKS13: write new file having a segment '   // &
                   'created by subsetting small segment from SPK13.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Create a segment identifier.
!
      SEGID = 'SPK type 13 test subset segment'

!
!     Open a new SPK file.
!
      IF ( EXISTS(SPK13S) ) THEN
         CALL DELFIL(SPK13S)
      END IF

      CALL SPKOPN ( SPK13S, 'Type 13 SPK internal file name', 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open SPK13 and extract segment descriptor and ID of first segment.
!
      CALL DAFOPR ( SPK13, HANDLE )
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
!     Create a type 13 segment in new file.  Shorten the time
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
      CALL TCASE ( 'Test SPKS13: check descriptor bounds on ' // &
                   'subsetted file.'                          )

!
!     Open SPK13S and extract segment descriptor and ID of first
!     segment.
!
      CALL DAFOPR ( SPK13S, HANDLE )
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
      CALL SPKLEF ( SPK13S, HANDLE )
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

         CALL CHCKAD ( 'type 13 state', &
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

      CALL DELFIL ( SPK13E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK13 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK13B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK13S )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


